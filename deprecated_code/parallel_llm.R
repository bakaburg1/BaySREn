#' Helper: extract model-level error object from JSON (top-level or first
#' choice)
#' @noRd
.json_extract_error <- function(json) {
  if (!is.list(json)) {
    return(NULL)
  }
  # Helper to extract error from first choice if available
  choices_first <- function(j) {
    ch <- tryCatch(j$choices, error = \(e) NULL)
    if (rlang::is_empty(ch)) {
      return(NULL)
    }
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
  if (is.null(err)) {
    return(NA_character_)
  }
  # Try to get error code from standard location or metadata
  code <- purrr::pluck(err, "code") %||%
    purrr::pluck(err, "metadata", "raw", "code")

  # Return specific code if found, otherwise generic "error"
  if (rlang::is_string(code) && nzchar(code)) {
    return(code)
  }
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
#' @param max_concurrency Optional positive integer cap on concurrently active
#'   HTTP requests. Set to `NULL` for no client-side concurrency limit.
#' @param cache_dir Directory where per-conversation cache JSON files are
#'   stored. Set to `NULL` or `FALSE` to disable cache reads and writes.
#' @param backoff_base Initial backoff delay in seconds when retrying
#'   408/429/5xx.
#' @param backoff_cap Maximum backoff delay in seconds. Also acts as the retry
#'   cut-off: retries continue only while the current backoff delay is strictly
#'   less than this cap. Any server-provided wait (e.g., `Retry-After`) is
#'   capped at this value as well.
#' @param halve_rpm_on_retry When `TRUE`, halves the effective RPM on each
#'   retryable failure to ease pressure on the provider.
#'
#' @return List of `ellmer::Turn` objects.
#'
#' @noRd
parallel_turns_promises <- function(
  provider,
  conversations,
  tools,
  type = NULL,
  rpm = 1000,
  max_concurrency = NULL,
  cache_dir = "parallel_cache_dir",
  backoff_base = 5,
  backoff_cap = 120,
  halve_rpm_on_retry = FALSE
) {
  if (!is.null(max_concurrency)) {
    if (!is.finite(max_concurrency) || max_concurrency <= 0) {
      max_concurrency <- NULL
    } else {
      max_concurrency <- as.integer(max_concurrency)
    }
  }
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

  # Tag duplicates with a per-occurrence URL fragment so cache keys differ.
  # Compute pre-fragment transport hashes to detect identical payloads.
  base_ids <- vapply(reqs, .hash_request_key, FUN.VALUE = character(1))
  ord <- seq_along(base_ids)
  dup_pos <- ave(ord, base_ids, FUN = seq_along)
  reqs <- purrr::map2(
    reqs,
    as.integer(dup_pos),
    \(req, pos) {
      # Always set a fragment (rep_001 for singletons) to keep keys consistent.
      url_chr <- tryCatch(as.character(req$url), error = function(e) "")
      if (rlang::is_string(url_chr) && nzchar(url_chr)) {
        frag <- sprintf("rep_%03d", pos)
        tagged <- paste0(url_chr, "#", frag)
        req <- httr2::req_url(req, tagged)
      }
      req
    }
  )

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
    backoff_cap = backoff_cap,
    max_concurrency = max_concurrency,
    halve_rpm_on_retry = halve_rpm_on_retry
  )

  # If user aborted, propagate a graceful NULL to caller
  if (isTRUE(attr(results, "aborted"))) {
    return(NULL)
  }

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
      if (!is.na(code)) {
        json_error_codes <- c(json_error_codes, code)
      }

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
#' @param max_active Optional positive integer cap on concurrently active
#'   conversations. Set to `NULL` for no client-side concurrency limit.
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
  max_active = NULL,
  rpm = 1000,
  cache_dir = "parallel_cache_dir",
  backoff_base = 5,
  backoff_cap = 120,
  max_retry = 5,
  halve_rpm_on_retry = FALSE
) {
  # Validate input chat object
  ellmer:::check_chat(chat)

  if (!is.null(max_active)) {
    if (!is.numeric(max_active) || length(max_active) != 1L || max_active <= 0) {
      cli::cli_abort("`max_active` must be NULL or a positive number")
    }
    max_active <- as.integer(max_active)
  }

  # Create closure that captures our parallel implementation
  my_parallel_turns <- function(conversations) {
    parallel_turns_promises(
      provider = chat$get_provider(),
      conversations = conversations,
      tools = chat$get_tools(),
      type = NULL,
      rpm = rpm,
      max_concurrency = max_active,
      cache_dir = cache_dir,
      backoff_base = backoff_base,
      backoff_cap = backoff_cap,
      halve_rpm_on_retry = halve_rpm_on_retry
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
    if (!any(needs_iter)) {
      break
    }

    # Add tool results to conversations and get next assistant responses
    conversations <- ellmer:::append_turns(conversations, user_turns)
    assistant_turns <- vector("list", length(user_turns))
    # Only process conversations that need iteration
    next_batch <- my_parallel_turns(conversations[needs_iter])
    if (is.null(next_batch)) {
      break
    }
    # Retry tool sub-batch if failures present
    if (any(purrr::map_lgl(next_batch, is.null))) {
      sub_attempt <- 1L
      while (
        sub_attempt <= max_retry && any(purrr::map_lgl(next_batch, is.null))
      ) {
        next_batch <- my_parallel_turns(conversations[needs_iter])
        if (is.null(next_batch)) {
          break
        }
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
  if (
    missing(cache_dir) ||
      !rlang::is_string(cache_dir) ||
      !fs::dir_exists(cache_dir)
  ) {
    stop("cache_dir must be an existing directory path", call. = FALSE)
  }

  # Helper: extract model-level error object from JSON result
  json_extract_error <- function(json) {
    if (!is.list(json)) {
      return(NULL)
    }
    # First choice error if present
    choices_first <- function(j) {
      ch <- tryCatch(j$choices, error = \(e) NULL)
      if (rlang::is_empty(ch)) {
        return(NULL)
      }
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
    to_drop <- purrr::map_lgl(gc, function(json) {
      !is.null(json_extract_error(json))
    })
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
    cli::cli_alert_success(
      "Purged {total} JSON-error entries across {nrow(per_file)} files"
    )
  } else {
    cli::cli_alert_info(
      "No JSON-error entries found to purge under: {cache_dir}"
    )
  }
  invisible(per_file)
}

#' Solve vector of prompts with optional schema, cache, retries, and repetitions
#'
#' Wraps native `ellmer` parallel chat helpers with an outer loop that caches
#' completed rows on disk, retries only unresolved rows, and supports repeated
#' solves of the same prompt via `reps`.
#'
#' @param prompts Character vector or list of prompt texts. Each input prompt
#'   becomes one logical row before `reps` expansion.
#' @param chat Ellmer chat object used as a template. The provider settings,
#'   model, extra arguments, and system prompt are read from this object, and a
#'   clone is created for each cache chunk so chunk solves do not share mutable
#'   chat state.
#' @param schema Optional ellmer schema. When `NULL`, the solver runs in text
#'   mode and returns a `text` column. When supplied, the solver runs in
#'   structured mode and returns one column per schema field. The schema is also
#'   hashed into the cache key, so changing it invalidates old cached results.
#' @param cache_dir Optional directory for `result_journal.jsonl` and
#'   `result_index.rds`. When a path is supplied, completed rows are cached to
#'   disk and reused across reruns. When `NULL`, disk caching is disabled, but
#'   identical prompt rows are still deduplicated within the current call via an
#'   in-memory index.
#' @param reps Positive integer number of intentional repeated solves per
#'   prompt. Each prompt is expanded to `rep = 1:reps`, and `rep` is included in
#'   the cache key, so repeated solves do not collapse onto the same cached row.
#' @param max_attempts Positive integer retry budget applied only to unresolved
#'   rows. Rows that still fail after the final attempt are returned with `NA`
#'   payload fields plus `has_error = TRUE` and `error_message`.
#' @param max_active Optional positive integer cap on concurrently active
#'   requests within one cache chunk. Lower values reduce pressure on the
#'   provider; higher values increase throughput if the provider tolerates it.
#'   The default is `100L`. When `NULL`, the solver removes the concurrency cap
#'   and allows up to all rows in the current chunk to run, so request pacing is
#'   controlled only by `rpm`. The effective value per chunk is always capped at
#'   the number of rows in that chunk.
#' @param rpm Optional requests-per-minute budget for the underlying ellmer
#'   parallel call. Lower values make the solver more conservative; higher
#'   values let it issue requests faster, subject to any stricter provider-side
#'   rate limits. When `NULL`, the wrapper omits explicit RPM throttling and
#'   relies on `max_active` alone. `rpm` and `max_active` cannot both be
#'   `NULL`.
#' @param cache_batch_size Positive integer number of unique unresolved cache
#'   keys to solve before forcing a cache flush. Smaller values improve crash
#'   resilience by persisting results more often; larger values reduce overhead
#'   but increase the amount of completed work that can remain unflushed inside
#'   the currently running chunk.
#'
#' @return Tibble with one row per prompt-repetition. Always includes
#'   `row_id`, `rep`, `cache_key`, `from_cache`, `attempts_used`, `has_error`,
#'   and `error_message`. Text mode adds `text`; structured mode adds one
#'   column per schema field.
#'
#' @export
llm_solver <- function(
  prompts,
  chat,
  schema = NULL,
  cache_dir = NULL,
  reps = 1L,
  max_attempts = 20L,
  max_active = 100L,
  rpm = 1000,
  cache_batch_size = 50L
) {
  if (!rlang::is_scalar_integerish(reps, finite = TRUE) || reps < 1) {
    cli::cli_abort("`reps` must be a positive number.")
  }
  reps <- as.integer(reps)

  if (!rlang::is_scalar_integerish(max_attempts, finite = TRUE) || max_attempts < 1) {
    cli::cli_abort("`max_attempts` must be a positive number.")
  }
  max_attempts <- as.integer(max_attempts)

  if (
    !rlang::is_scalar_integerish(cache_batch_size, finite = TRUE) ||
      cache_batch_size < 1
  ) {
    cli::cli_abort("`cache_batch_size` must be a positive number.")
  }
  cache_batch_size <- as.integer(cache_batch_size)

  if (!is.null(max_active)) {
    if (
      !rlang::is_scalar_integerish(max_active, finite = TRUE) ||
        max_active < 1
    ) {
      cli::cli_abort("`max_active` must be NULL or a positive number.")
    }
    max_active <- as.integer(max_active)
  }

  if (!is.null(rpm)) {
    if (!rlang::is_scalar_integerish(rpm, finite = TRUE) || rpm <= 0) {
      cli::cli_abort("`rpm` must be NULL or a positive number.")
    }
    rpm <- as.integer(rpm)
  }

  if (is.null(max_active) && is.null(rpm)) {
    cli::cli_abort("`max_active` and `rpm` cannot both be NULL.")
  }

  prompts_list <- if (is.list(prompts)) {
    prompts
  } else {
    as.list(as.character(prompts))
  }
  prompts_chr <- unlist(prompts_list, use.names = FALSE)
  if (!length(prompts_chr)) {
    base_cols <- list(
      row_id = integer(),
      rep = integer(),
      cache_key = character(),
      from_cache = logical(),
      attempts_used = integer(),
      has_error = logical(),
      error_message = character()
    )

    if (is.null(schema)) {
      return(tibble::as_tibble(c(base_cols, list(text = character()))))
    }

    empty_payload <- ellmer:::convert_from_type(list(), ellmer:::type_array(schema))
    return(
      tibble::as_tibble(base_cols) |>
        dplyr::bind_cols(empty_payload)
    )
  }

  provider <- tryCatch(chat$get_provider(), error = function(e) NULL)
  provider_obj <- tryCatch(unclass(provider), error = function(e) NULL)
  provider_name <- tryCatch(attr(provider_obj, "name"), error = function(e) NULL)
  model_name <- tryCatch(attr(provider_obj, "model"), error = function(e) NULL)
  api_args <- tryCatch(attr(provider_obj, "extra_args"), error = function(e) NULL)

  system_turns <- tryCatch(
    chat$get_turns(include_system_prompt = TRUE),
    error = function(e) NULL
  )
  system_prompt <- ""
  if (!is.null(system_turns) && length(system_turns) >= 1L) {
    system_prompt <- tryCatch(system_turns[[1]]@text, error = function(e) "")
  }

  schema_hash <- if (is.null(schema)) {
    NULL
  } else {
    rlang::hash(serialize(schema, NULL, version = 2))
  }

  build_cache_key <- function(prompt, rep) {
    rlang::hash(list(
      provider = provider_name %||% NA_character_,
      model = model_name %||% NA_character_,
      api_args = api_args %||% list(),
      system_prompt = system_prompt %||% "",
      prompt = prompt,
      mode = if (is.null(schema)) "text" else "structured",
      schema = schema_hash,
      rep = rep
    ))
  }

  journal_path <- if (!is.null(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    file.path(cache_dir, "result_journal.jsonl")
  } else {
    NULL
  }
  index_path <- if (!is.null(cache_dir)) {
    file.path(cache_dir, "result_index.rds")
  } else {
    NULL
  }

  load_result_index <- function() {
    if (is.null(cache_dir)) {
      return(NULL)
    }

    if (file.exists(index_path)) {
      cached <- tryCatch(readr::read_rds(index_path), error = function(e) NULL)
      if (inherits(cached, "data.frame")) {
        return(tibble::as_tibble(cached))
      }
    }

    if (!file.exists(journal_path)) {
      return(NULL)
    }

    journal_lines <- readLines(journal_path, warn = FALSE)
    if (!length(journal_lines)) {
      return(NULL)
    }

    cached <- purrr::map_dfr(
      journal_lines,
      function(line) {
        tibble::as_tibble(jsonlite::fromJSON(line, simplifyVector = TRUE))
      }
    )
    if (!nrow(cached)) {
      return(NULL)
    }

    cached <- cached |>
      dplyr::distinct(.data$cache_key, .keep_all = TRUE)

    readr::write_rds(cached, index_path)
    cached
  }

  append_result_journal <- function(rows) {
    if (is.null(cache_dir) || !nrow(rows)) {
      return(invisible(NULL))
    }

    row_lists <- purrr::transpose(as.list(rows))
    json_lines <- vapply(
      row_lists,
      function(row) {
        jsonlite::toJSON(
          row,
          auto_unbox = TRUE,
          null = "null",
          na = "null"
        )
      },
      character(1)
    )
    cat(paste0(json_lines, "\n"), file = journal_path, append = TRUE)
    invisible(NULL)
  }

  refresh_result_index <- function(index_tbl, rows) {
    if (!nrow(rows)) {
      return(index_tbl)
    }

    updated <- dplyr::bind_rows(index_tbl, rows)
    updated <- updated |>
      dplyr::distinct(.data$cache_key, .keep_all = TRUE)
    if (!is.null(cache_dir)) {
      readr::write_rds(updated, index_path)
    }
    updated
  }

  coerce_chunk_result <- function(raw_result, chunk_meta, attempt_idx) {
    if (is.null(schema)) {
      completed <- chunk_meta |>
        dplyr::mutate(
          text = unname(raw_result),
          attempts_used = attempt_idx
        ) |>
        dplyr::filter(!is.na(.data$text)) |>
        dplyr::select("cache_key", "attempts_used", "text")

      unresolved <- chunk_meta |>
        dplyr::mutate(
          error_message = ifelse(
            is.na(raw_result),
            "No text returned by ellmer::parallel_chat_text().",
            NA_character_
          )
        ) |>
        dplyr::filter(!is.na(.data$error_message)) |>
        dplyr::select("cache_key", "error_message")

      return(list(completed = completed, unresolved = unresolved))
    }

    raw_result <- tibble::as_tibble(raw_result)
    error_messages <- rep(NA_character_, nrow(raw_result))
    if (".error" %in% names(raw_result)) {
      error_messages <- vapply(
        raw_result$.error,
        function(err) {
          if (is.null(err)) {
            return(NA_character_)
          }
          if (inherits(err, "condition")) {
            return(conditionMessage(err))
          }
          if (is.character(err) && length(err) == 1L) {
            return(err)
          }
          "Structured response failed."
        },
        character(1)
      )
    }

    payload_cols <- setdiff(names(raw_result), ".error")
    completed <- raw_result |>
      dplyr::bind_cols(chunk_meta |>
        dplyr::select("cache_key")) |>
      dplyr::mutate(
        attempts_used = attempt_idx,
        .before = 1
      )

    if ("final_decision" %in% payload_cols) {
      is_completed <- !is.na(raw_result$final_decision)
    } else {
      is_completed <- !is.na(raw_result[[payload_cols[[1]]]])
    }

    unresolved <- tibble::tibble(
      cache_key = chunk_meta$cache_key,
      error_message = ifelse(
        is_completed,
        NA_character_,
        ifelse(
          !is.na(error_messages),
          error_messages,
          "Structured response failed."
        )
      )
    ) |>
      dplyr::filter(!is.na(.data$error_message))

    completed <- completed[is_completed, , drop = FALSE] |>
      dplyr::select("cache_key", "attempts_used", dplyr::all_of(payload_cols))

    list(completed = completed, unresolved = unresolved)
  }

  solve_chunk <- function(chunk_meta, attempt_idx) {
    chunk_chat <- chat$clone()
    chunk_prompts <- as.list(chunk_meta$prompt)
    effective_max_active <- if (is.null(max_active)) {
      length(chunk_prompts)
    } else {
      min(max_active, length(chunk_prompts))
    }

    if (is.null(schema)) {
      text_args <- list(
        chat = chunk_chat,
        prompts = chunk_prompts,
        max_active = effective_max_active,
        on_error = "continue"
      )
      if (!is.null(rpm)) {
        text_args$rpm <- rpm
      }
      raw_result <- do.call(ellmer::parallel_chat_text, text_args)
    } else {
      structured_args <- list(
        chat = chunk_chat,
        prompts = chunk_prompts,
        type = schema,
        max_active = effective_max_active,
        on_error = "continue"
      )
      if (!is.null(rpm)) {
        structured_args$rpm <- rpm
      }
      raw_result <- do.call(ellmer::parallel_chat_structured, structured_args)
    }

    coerce_chunk_result(raw_result, chunk_meta, attempt_idx)
  }

  prompt_tbl <- tibble::tibble(
    row_id = seq_along(prompts_chr),
    prompt = as.character(prompts_chr)
  ) |>
    tidyr::crossing(rep = seq_len(reps)) |>
    dplyr::mutate(
      cache_key = purrr::map2_chr(
        .data$prompt,
        .data$rep,
        build_cache_key
      )
    ) |>
    dplyr::select("row_id", "rep", "prompt", "cache_key")

  result_index <- load_result_index()
  if (is.null(result_index)) {
    result_index <- tibble::tibble(cache_key = character(), attempts_used = integer())
  }

  if (is.null(schema)) {
    if (!"text" %in% names(result_index)) {
      result_index$text <- character()
    }
  } else {
    payload_template <- ellmer:::convert_from_type(list(NULL), ellmer:::type_array(schema))
    for (col in names(payload_template)) {
      if (!col %in% names(result_index)) {
        result_index[[col]] <- payload_template[[col]][NA_integer_]
      }
    }
  }

  prefills <- prompt_tbl |>
    dplyr::left_join(result_index, by = "cache_key")

  if (is.null(schema)) {
    cached_keys <- prefills |>
      dplyr::filter(!is.na(.data$text)) |>
      dplyr::pull(.data$cache_key) |>
      unique()
    unresolved_keys <- prefills |>
      dplyr::filter(is.na(.data$text)) |>
      dplyr::distinct(.data$cache_key, .data$prompt)
  } else {
    payload_cols <- names(payload_template)
    cached_keys <- prefills |>
      dplyr::filter(!is.na(.data[[payload_cols[[1]]]])) |>
      dplyr::pull(.data$cache_key) |>
      unique()
    unresolved_keys <- prefills |>
      dplyr::filter(is.na(.data[[payload_cols[[1]]]])) |>
      dplyr::distinct(.data$cache_key, .data$prompt)
  }

  last_errors <- prompt_tbl |>
    dplyr::distinct(.data$cache_key) |>
    dplyr::mutate(error_message = NA_character_)
  last_attempts <- prompt_tbl |>
    dplyr::distinct(.data$cache_key) |>
    dplyr::mutate(attempts_used_live = NA_integer_)

  if (nrow(unresolved_keys)) {
    for (attempt_idx in seq_len(max_attempts)) {
      if (!nrow(unresolved_keys)) {
        break
      }

      chunk_ids <- ceiling(seq_len(nrow(unresolved_keys)) / cache_batch_size)
      chunks <- split(unresolved_keys, chunk_ids)

      for (chunk_meta in chunks) {
        solved <- solve_chunk(chunk_meta, attempt_idx)

        if (nrow(solved$completed)) {
          append_result_journal(solved$completed)
          result_index <- refresh_result_index(result_index, solved$completed)
        }

        if (nrow(solved$unresolved)) {
          last_errors <- last_errors |>
            dplyr::rows_upsert(solved$unresolved, by = "cache_key")
          last_attempts <- last_attempts |>
            dplyr::rows_upsert(
              solved$unresolved |>
                dplyr::mutate(attempts_used_live = attempt_idx) |>
                dplyr::select("cache_key", "attempts_used_live"),
              by = "cache_key"
            )
        }

        unresolved_keys <- unresolved_keys |>
          dplyr::filter(!(.data$cache_key %in% solved$completed$cache_key))
      }
    }
  }

  final_tbl <- prompt_tbl |>
    dplyr::left_join(result_index, by = "cache_key") |>
    dplyr::left_join(last_attempts, by = "cache_key") |>
    dplyr::left_join(last_errors, by = "cache_key") |>
    dplyr::mutate(
      attempts_used = dplyr::coalesce(.data$attempts_used, .data$attempts_used_live),
      from_cache = .data$cache_key %in% cached_keys
    ) |>
    dplyr::select(-"attempts_used_live")

  if (is.null(schema)) {
    final_tbl <- final_tbl |>
      dplyr::mutate(
        has_error = is.na(.data$text),
        error_message = dplyr::if_else(
          !.data$has_error,
          NA_character_,
          ifelse(
            is.na(.data$error_message),
            "No text returned after max_attempts.",
            .data$error_message
          )
        )
      ) |>
      dplyr::select(
        "row_id",
        "rep",
        "cache_key",
        "from_cache",
        "attempts_used",
        "has_error",
        "error_message",
        "text"
      ) |>
      dplyr::arrange(.data$row_id, .data$rep)

    return(final_tbl)
  }

  payload_cols <- names(payload_template)

  if (!all(payload_cols %in% names(final_tbl))) {
    missing_cols <- setdiff(payload_cols, names(final_tbl))
    for (col in missing_cols) {
      final_tbl[[col]] <- payload_template[[col]][NA_integer_]
    }
  }

  payload_missing <- if ("final_decision" %in% names(final_tbl)) {
    is.na(final_tbl$final_decision)
  } else {
    is.na(final_tbl[[payload_cols[[1]]]])
  }

  final_tbl |>
    dplyr::mutate(
      has_error = payload_missing,
      error_message = dplyr::if_else(
        !.data$has_error,
        NA_character_,
        ifelse(
          is.na(.data$error_message),
          "Structured response unavailable after max_attempts.",
          .data$error_message
        )
      )
    ) |>
    dplyr::select(
      "row_id",
      "rep",
      "cache_key",
      "from_cache",
      "attempts_used",
      "has_error",
      "error_message",
      dplyr::all_of(payload_cols)
    ) |>
    dplyr::arrange(.data$row_id, .data$rep)
}
