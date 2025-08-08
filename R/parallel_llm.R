#' Helper: extract model-level error object from JSON (top-level or first
#' choice)
#' @noRd
.json_extract_error <- function(json) {
  if (!is.list(json)) return(NULL)
  `%||%` <- rlang::`%||%`

  # Helper to extract error from first choice if available
  choices_first <- function(j) {
    ch <- tryCatch(j$choices, error = \(e) NULL)
    if (rlang::is_empty(ch)) return(NULL)
    tryCatch(ch[[1]]$error, error = \(e) NULL)
  }

  # Try top-level error first, then first choice error
  err <- (json$error %||% choices_first(json))
  if (!rlang::is_empty(err)) err else NULL
}

# Extract a simple error code/marker for diagnostics (character)
.json_error_code <- function(json) {
  # Extract error object from JSON response
  err <- .json_extract_error(json)
  if (is.null(err)) return(NA_character_)
  `%||%` <- rlang::`%||%`

  # Try to get error code from standard location or metadata
  code <- purrr::pluck(err, "code") %||%
    purrr::pluck(err, "metadata", "raw", "code")

  # Return specific code if found, otherwise generic "error"
  if (rlang::is_string(code) && nzchar(code)) return(code)
  "error"
}


#' Perform LLM turns concurrently using promises
#'
#' Builds requests via `ellmer:::chat_request()`, applies request throttling at
#' `rpm` tokens per minute, executes via `req_perform_parallel_promises()`, and
#' converts each JSON result into an `ellmer::Turn`.
#'
#' @param provider Provider object from an ellmer chat instance.
#' @param conversations List of conversation lists (turns per item).
#' @param tools Tools list to pass to the request builder.
#' @param type Optional type for the request builder.
#' @param rpm Requests per minute capacity for throttling.
#' @param cache_dir Directory where per-conversation cache JSON files are
#'   stored. Set to `NULL` or `FALSE` to disable cache reads and writes.
#' @param backoff_base Initial backoff delay in seconds when retrying
#'   408/429/5xx.
#' @param backoff_cap Maximum backoff delay in seconds. Also acts as the retry
#'   cut-off: retries continue only while the current backoff delay is strictly
#'   less than this cap. Any server-provided wait (e.g., `Retry-After`) is
#'   capped at this value as well.
#'
#' @return List of `ellmer::Turn` objects.
#'
#' @noRd
parallel_turns_promises <- function(
  provider,
  conversations,
  tools,
  type = NULL,
  rpm = 60,
  cache_dir = "parallel_cache_dir",
  backoff_base = 5,
  backoff_cap = 20
) {
  # Build HTTP requests for each conversation
  reqs <- purrr::map(conversations, function(turns) {
    ellmer:::chat_request(
      provider = provider,
      turns = turns,
      type = type,
      tools = tools,
      stream = FALSE
    )
  })

  # Request ids (hash of each conversation)
  conv_hashes <- purrr::map_chr(conversations, rlang::hash)
  names(conversations) <- conv_hashes

  # Compute ids used by the transport cache (method|url|body hash)
  req_ids <- purrr::map_chr(reqs, .hash_request_key)

  # Compute global cache path if caching enabled
  global_cache_path <- if (isFALSE(cache_dir) || is.null(cache_dir)) {
    NULL
  } else {
    provider_name <- provider@name
    model_name <- provider@model
    system_prompt <- conversations[[1]][[1]]@text
    system_prompt_hash <- rlang::hash(system_prompt)
    safe_provider <- gsub("/", ".", provider_name)
    safe_model <- gsub("/", ".", model_name)
    file.path(
      cache_dir,
      sprintf(
        "%s_%s_%s_cache.rds",
        safe_provider,
        safe_model,
        system_prompt_hash
      )
    )
  }

  # Execute all requests through the generic engine
  results <- req_perform_parallel_promises(
    reqs = reqs,
    global_cache_path = global_cache_path,
    temp_cache_dir = NULL,
    rpm = rpm,
    backoff_base = backoff_base,
    backoff_cap = backoff_cap
  )

  # If user aborted, propagate a graceful NULL to caller
  if (isTRUE(attr(results, "aborted"))) return(NULL)

  # Prepare to convert JSON responses
  turns <- vector("list", length(results))

  # Track JSON error codes for diagnostics
  json_error_codes <- character(0)
  key_to_purge <- character(0)
  

  # Analize request results and convert to turns
  for (i in seq_along(results)) {
    res <- results[[i]]

    # If the request failed, mark the turn as NULL to indicate failure
    if (!is.list(res) || is.null(res$kind) || res$kind != "ok") {
      turns[[i]] <- NULL
      next
    }

    # Extract the JSON response
    json <- res$json
    err_obj <- .json_extract_error(json)

    # If there's an error object in the JSON, purge the entry from the cache
    # and track the error code for diagnostics
    if (!is.null(err_obj)) {
      key_to_purge <- c(key_to_purge, req_ids[[i]])
      
      # Track the error code for diagnostics
      code <- .json_error_code(json)
      if (!is.na(code)) json_error_codes <- c(json_error_codes, code)
      
      # Mark the turn as NULL to indicate failure
      turns[[i]] <- NULL
      
      next
    }
    turns[[i]] <- ellmer:::value_turn(provider, json, has_type = !is.null(type))
  }
  # Purge all bad cache entries in one go (if any)
  if (
    !rlang::is_empty(key_to_purge) &&
    rlang::is_string(global_cache_path) &&
    fs::file_exists(global_cache_path)
  ) {
    keys <- unique(key_to_purge)
    gc <- tryCatch(readr::read_rds(global_cache_path), error = \(e) NULL)
    if (rlang::is_list(gc)) {
      nm <- names(gc)

      to_drop <- nm %in% keys
      removed <- sum(to_drop)

      if (removed > 0L) {
        gc <- gc[!to_drop]
        tryCatch(readr::write_rds(gc, global_cache_path), error = \(e) NULL)
        cli::cli_alert_info("Purged {removed} JSON-error entries from cache")
      }
    }
  }
  if (!rlang::is_empty(json_error_codes)) {
    tab <- sort(table(json_error_codes), decreasing = TRUE)
    fmt <- paste(sprintf("%s=%d", names(tab), as.integer(tab)), collapse = ", ")
    cli::cli_alert_warning("Model-level JSON errors detected (not HTTP): {fmt}")
  }
  turns
}


#' Parallel chat orchestrator using promises engine
#'
#' Replicates the behaviour of `ellmer::parallel_chat()` while delegating the
#' transport layer to `parallel_turns_promises()`.
#'
#' @param chat Base ellmer chat object.
#' @param prompts Character vector of user prompts.
#' @param max_active Not used; present for compatibility only.
#' @param rpm Requests per minute capacity for throttling.
#' @param cache_dir Directory to store conversation-level cache files.
#' @param backoff_base Initial backoff delay in seconds when retrying
#'   408/429/5xx.
#' @param backoff_cap Maximum backoff delay in seconds. Also acts as the retry
#'   cut-off: retries continue only while the current backoff delay is strictly
#'   less than this cap. Any server-provided wait (e.g., `Retry-After`) is
#'   capped at this value as well.
#'
#' @return A list of chat objects, one per conversation, with populated turns.
#'
#' @export
parallel_chat_promises <- function(
  chat,
  prompts,
  max_active = 10,
  rpm = 500,
  cache_dir = "parallel_cache_dir",
  backoff_base = 5,
  backoff_cap = 20,
  max_retry = 2
) {
  # Validate input chat object
  ellmer:::check_chat(chat)

  # Create closure that captures our parallel implementation
  my_parallel_turns <- function(conversations) {
    parallel_turns_promises(
      provider = chat$get_provider(),
      conversations = conversations,
      tools = chat$get_tools(),
      type = NULL,
      rpm = rpm,
      cache_dir = cache_dir,
      backoff_base = backoff_base,
      backoff_cap = backoff_cap
    )
  }

  # Convert prompts to user turns and prepare initial conversations
  user_turns <- ellmer:::as_user_turns(prompts)
  existing <- chat$get_turns(include_system_prompt = TRUE)
  conversations <- ellmer:::append_turns(list(existing), user_turns)

  attempt <- 0L
  repeat {
    attempt <- attempt + 1L
    assistant_turns <- my_parallel_turns(conversations)

    # Handle complete failure of parallel processing
    # Return chat objects with only the original turns (no assistant responses)
    if (is.null(assistant_turns)) {
      return(purrr::map(
        conversations,
        function(turns) chat$clone()$set_turns(turns)
      ))
    }

    # If any failed (NULL), retry the whole batch up to max_retry, otherwise
    # proceed
    if (any(purrr::map_lgl(assistant_turns, is.null)) && attempt <= max_retry) {
      next
    }
    break
  }
  # Replace any remaining failures with a placeholder assistant turn
  if (any(purrr::map_lgl(assistant_turns, is.null))) {
    failed_idx <- which(purrr::map_lgl(assistant_turns, is.null))
    assistant_turns[failed_idx] <- lapply(
      failed_idx,
      function(i) ellmer:::assistant_turn("ERROR: request failed")
    )
  }
  conversations <- ellmer:::append_turns(conversations, assistant_turns)

  # Handle tool calling loop
  repeat {
    # Match any tool calls in assistant responses
    assistant_turns <- purrr::map(
      assistant_turns,
      function(turn) ellmer:::match_tools(turn, tools = chat$get_tools())
    )
    # Execute tool calls and collect results
    tool_results <- purrr::map(
      assistant_turns,
      function(turn) coro::collect(ellmer:::invoke_tools(turn))
    )
    # Convert tool results to user turns
    user_turns <- purrr::map(tool_results, ellmer:::tool_results_as_turn)
    # Check which conversations need another iteration
    needs_iter <- !purrr::map_lgl(user_turns, is.null)
    # Exit loop if no conversations need tool result processing
    if (!any(needs_iter)) break

    # Add tool results to conversations and get next assistant responses
    conversations <- ellmer:::append_turns(conversations, user_turns)
    assistant_turns <- vector("list", length(user_turns))
    # Only process conversations that need iteration
    next_batch <- my_parallel_turns(conversations[needs_iter])
    if (is.null(next_batch)) break
    # Retry tool sub-batch if failures present
    if (any(purrr::map_lgl(next_batch, is.null))) {
      sub_attempt <- 1L
      while (
        sub_attempt <= max_retry && any(purrr::map_lgl(next_batch, is.null))
      ) {
        next_batch <- my_parallel_turns(conversations[needs_iter])
        if (is.null(next_batch)) break
        sub_attempt <- sub_attempt + 1L
      }
    }
    # Replace any remaining tool-call failures with placeholder assistant turns
    if (any(purrr::map_lgl(next_batch, is.null))) {
      bad <- which(purrr::map_lgl(next_batch, is.null))
      next_batch[bad] <- lapply(
        bad,
        function(i) ellmer:::assistant_turn("ERROR: tool request failed")
      )
    }
    assistant_turns[needs_iter] <- next_batch
    conversations <- ellmer:::append_turns(conversations, assistant_turns)
  }

  # Return cloned chat objects with populated conversation turns
  purrr::map(conversations, function(turns) chat$clone()$set_turns(turns))
}

#' Purge JSON-error entries from LLM cache files in a directory
#'
#' Scans a directory for `*_cache.rds` files produced by the LLM layer and
#' removes entries whose JSON payload contains a model-level error
#' (top-level `error` or `choices[[1]]$error`).
#'
#' The function performs in-place cleanup and prints a brief summary.
#'
#' @param cache_dir Character path to a directory containing cache files.
#'
#' @return Invisibly returns a data frame with per-file purge counts.
#'
#' @export
purge_llm_cache_errors <- function(cache_dir) {
  # Validate input directory (no default allowed)
  if (missing(cache_dir) || !rlang::is_string(cache_dir) || !fs::dir_exists(cache_dir)) {
    stop("cache_dir must be an existing directory path", call. = FALSE)
  }

  # Helper: extract model-level error object from JSON result
  json_extract_error <- function(json) {
    if (!is.list(json)) return(NULL)
    `%||%` <- rlang::`%||%`
    # First choice error if present
    choices_first <- function(j) {
      ch <- tryCatch(j$choices, error = \(e) NULL)
      if (rlang::is_empty(ch)) return(NULL)
      tryCatch(ch[[1]]$error, error = \(e) NULL)
    }
    err <- (json$error %||% choices_first(json))
    if (!rlang::is_empty(err)) err else NULL
  }

  # Discover cache files (recursive) matching *_cache.rds
  files <- fs::dir_ls(cache_dir, recurse = TRUE, glob = "*_cache.rds")
  if (rlang::is_empty(files)) {
    cli::cli_alert_info("No cache files found under: {cache_dir}")
    return(invisible(tibble::tibble(file = character(), removed = integer())))
  }

  per_file <- purrr::map_df(files, function(path) {
    gc <- tryCatch(readr::read_rds(path), error = \(e) NULL)
    if (!rlang::is_list(gc) || rlang::is_empty(gc)) {
      return(tibble::tibble(file = path, removed = 0L))
    }
    nm <- names(gc)
    if (is.null(nm) || length(nm) == 0L) {
      return(tibble::tibble(file = path, removed = 0L))
    }
    # Identify entries to purge
    to_drop <- purrr::map_lgl(gc, function(json) !is.null(json_extract_error(json)))
    removed <- sum(to_drop)
    if (removed > 0L) {
      gc <- gc[!to_drop]
      # Write back
      tryCatch(readr::write_rds(gc, path), error = \(e) NULL)
    }
    tibble::tibble(file = path, removed = removed)
  })

  total <- sum(per_file$removed)
  if (total > 0L) {
    cli::cli_alert_success("Purged {total} JSON-error entries across {nrow(per_file)} files")
  } else {
    cli::cli_alert_info("No JSON-error entries found to purge under: {cache_dir}")
  }
  invisible(per_file)
}