#' Resolve a solver chat object
#'
#' Accepts either a chat object directly or a zero-argument factory that
#' returns one.
#'
#' @param solver_chat A chat object or a zero-argument function.
#'
#' @return A resolved chat object.
#'
#' @keywords internal
.llm_resolve_chat <- function(solver_chat) {
  if (is.function(solver_chat)) {
    solver_chat <- solver_chat()
  }

  if (!is.list(solver_chat) && !inherits(solver_chat, "Chat")) {
    cli::cli_abort(
      "{.arg solver_chat} must be a Chat object or zero-argument factory."
    )
  }

  solver_chat
}

#' Clone a chat object when the backend supports it
#'
#' @param chat A resolved chat object.
#'
#' @return A cloned chat object when possible, otherwise `chat`.
#'
#' @keywords internal
.llm_clone_chat <- function(chat) {
  clone_fun <- tryCatch(chat$clone, error = function(e) NULL)
  if (is.function(clone_fun)) {
    return(tryCatch(clone_fun(deep = TRUE), error = function(e) chat))
  }
  chat
}

#' Split row indices into batches
#'
#' @param indices Integer vector of row indices.
#' @param batch_size Batch size or `NULL` for a single batch.
#'
#' @return A list of integer vectors.
#'
#' @keywords internal
.llm_split_batches <- function(indices, batch_size) {
  if (is.null(batch_size)) {
    return(list(indices))
  }

  split(indices, ceiling(seq_along(indices) / batch_size))
}

#' Extract a scalar text result from an unstructured response
#'
#' @param value A batch result element.
#'
#' @return A single character string or `NULL`.
#'
#' @keywords internal
.llm_extract_text <- function(value) {
  if (is.null(value)) {
    return(NULL)
  }

  if (inherits(value, "condition")) {
    return(NULL)
  }

  if (is.character(value) && length(value) >= 1L) {
    text <- value[[1]]
    if (!is.na(text) && nzchar(text)) {
      return(as.character(text))
    }
    return(NULL)
  }

  # Chat-like objects may expose `$last_turn()` without the list-style fallbacks.
  last_turn <- tryCatch(value$last_turn(), error = function(e) NULL)
  if (!is.null(last_turn)) {
    turn_text <- tryCatch(last_turn@text, error = function(e) NULL)
    if (is.null(turn_text)) {
      turn_text <- tryCatch(last_turn$text, error = function(e) NULL)
    }
    if (!is.null(turn_text) && nzchar(as.character(turn_text)[1])) {
      return(as.character(turn_text)[1])
    }
  }

  if (is.list(value)) {
    if (!is.null(value$text)) {
      return(.llm_extract_text(value$text))
    }
    if (!is.null(value$result)) {
      return(.llm_extract_text(value$result))
    }
  }

  NULL
}

#' Extract a row payload from a structured response element
#'
#' @param value A batch result element.
#'
#' @return A list payload or `NULL`.
#'
#' @keywords internal
.llm_extract_structured <- function(value) {
  if (is.null(value) || inherits(value, "condition")) {
    return(NULL)
  }

  if (is.data.frame(value)) {
    if (nrow(value) < 1L) {
      return(NULL)
    }
    return(purrr::map(value[1, , drop = FALSE], \(column) column[[1]]))
  }

  if (is.list(value)) {
    if (!is.null(value$.error)) {
      return(NULL)
    }
    if (!is.null(attr(value, "error"))) {
      return(NULL)
    }
    if (!is.null(value$error) && inherits(value$error, "condition")) {
      return(NULL)
    }
    return(value)
  }

  list(result = value)
}

#' Derive a result status from a parsed payload
#'
#' @param payload Parsed row payload.
#'
#' @return `TRUE` when the payload is usable, otherwise `FALSE`.
#'
#' @keywords internal
.llm_payload_ok <- function(payload) {
  if (is.null(payload)) {
    return(FALSE)
  }
  if (is.character(payload) && length(payload) == 1L) {
    return(!is.na(payload) && nzchar(payload))
  }
  if (is.list(payload)) {
    return(length(payload) > 0L)
  }
  TRUE
}

#' Turn a batch response into row-level pieces
#'
#' @param batch_result Response returned by an ellmer parallel helper.
#' @param batch_size Expected number of rows in the batch.
#' @param structured Whether the helper ran in structured mode.
#'
#' @return A list with `result`, `solver_chat`, `payload`, and `status`.
#'
#' @keywords internal
.llm_batch_rows <- function(batch_result, batch_size, structured) {
  result <- vector("list", batch_size)
  solver_chat <- vector("list", batch_size)
  payload <- vector("list", batch_size)
  ok <- rep(FALSE, batch_size)

  if (structured) {
    if (is.data.frame(batch_result)) {
      if (nrow(batch_result) != batch_size) {
        cli::cli_abort("Structured batch returned the wrong number of rows.")
      }
      if (".error" %in% names(batch_result)) {
        error_col <- batch_result$.error
      } else {
        error_col <- rep(list(NULL), batch_size)
      }

      for (i in seq_len(batch_size)) {
        row <- batch_result[i, , drop = FALSE]
        row_payload <- .llm_extract_structured(row)
        if (!is.null(error_col[[i]])) {
          row_payload <- NULL
        }
        payload[i] <- list(row_payload)
        result[i] <- list(row_payload)
        solver_chat[i] <- list(row)
        ok[[i]] <- .llm_payload_ok(row_payload)
      }
      return(list(
        result = result,
        solver_chat = solver_chat,
        payload = payload,
        ok = ok
      ))
    }

    if (is.list(batch_result) && length(batch_result) == batch_size) {
      for (i in seq_len(batch_size)) {
        row_payload <- .llm_extract_structured(batch_result[[i]])
        payload[i] <- list(row_payload)
        result[i] <- list(row_payload)
        solver_chat[i] <- list(batch_result[[i]])
        ok[[i]] <- .llm_payload_ok(row_payload)
      }
      return(list(
        result = result,
        solver_chat = solver_chat,
        payload = payload,
        ok = ok
      ))
    }
  } else {
    if (is.character(batch_result) && length(batch_result) == batch_size) {
      for (i in seq_len(batch_size)) {
        row_payload <- .llm_extract_text(batch_result[[i]])
        payload[i] <- list(row_payload)
        result[i] <- list(row_payload)
        solver_chat[i] <- list(batch_result[[i]])
        ok[[i]] <- .llm_payload_ok(row_payload)
      }
      return(list(
        result = result,
        solver_chat = solver_chat,
        payload = payload,
        ok = ok
      ))
    }

    if (is.list(batch_result) && length(batch_result) == batch_size) {
      for (i in seq_len(batch_size)) {
        row_payload <- .llm_extract_text(batch_result[[i]])
        payload[i] <- list(row_payload)
        result[i] <- list(row_payload)
        solver_chat[i] <- list(batch_result[[i]])
        ok[[i]] <- .llm_payload_ok(row_payload)
      }
      return(list(
        result = result,
        solver_chat = solver_chat,
        payload = payload,
        ok = ok
      ))
    }
  }

  cli::cli_abort("Batch helper returned an unsupported result shape.")
}

#' Build an append-only solver diagnostic record
#'
#' @param diagnostics_path Diagnostics file path, or `NULL`.
#' @param event Event label.
#' @param payload Event payload.
#'
#' @return Invisible `NULL`.
#'
#' @keywords internal
.llm_append_diagnostic <- function(diagnostics_path, event, payload) {
  if (is.null(diagnostics_path)) {
    return(invisible(NULL))
  }

  record <- c(
    list(
      event = event,
      timestamp = as.character(Sys.time())
    ),
    payload
  )
  fs::dir_create(fs::path_dir(diagnostics_path))
  line <- jsonlite::toJSON(record, auto_unbox = TRUE, null = "null", digits = NA)
  cat(line, "\n", file = diagnostics_path, append = TRUE)
  invisible(NULL)
}

#' Run a BaySREn LLM solver over rendered prompt strings
#'
#' The solver restores cache hits first, executes missing rows in batches, and
#' persists successful or terminal-failure rows back to the selected cache
#' root.
#'
#' @param inputs Character vector of rendered prompts.
#' @param ... Additional solver arguments forwarded to the ellmer parallel
#'   helper.
#' @param solver_chat Either a chat object or a zero-argument factory.
#' @param type `NULL` for unstructured output, or an ellmer structured type.
#' @param batch_size Positive whole number giving the batch size for batched
#'   execution. `NULL` runs one batch over all pending rows.
#' @param cache_mode Cache mode: `official`, `interim`, or `none`.
#' @param cache_root Official cache root.
#' @param cache_overlay_root Interim cache root.
#' @param cache_bucket Cache bucket label such as `llm`.
#' @param cache_family Cache family label.
#' @param cache_schema_version Cache schema version.
#' @param cache_failure Logical flag controlling whether terminal failures are
#'   cached.
#' @param diagnostics_path Optional append-only JSONL diagnostics path.
#' @param max_retries Number of retry-wave passes to attempt for failed rows.
#' @param parallelize_batches Logical flag retained for future batch-parallel
#'   execution.
#'
#' @return A tibble with row-level solver outputs and telemetry.
#'
#' @keywords internal
llm_solver <- function(
  inputs,
  ...,
  solver_chat,
  type = NULL,
  batch_size = 500L,
  cache_mode = c("official", "interim", "none"),
  cache_root = "cache",
  cache_overlay_root = file.path("experiments", "cache"),
  cache_bucket = "llm",
  cache_family = "method_llm_solver",
  cache_schema_version = 1L,
  cache_failure = TRUE,
  diagnostics_path = NULL,
  max_retries = 5L,
  parallelize_batches = TRUE
) {
  cache_mode <- match.arg(cache_mode)

  # Validate the prompt vector early so the task contract stays explicit.
  if (!is.character(inputs)) {
    cli::cli_abort("{.arg inputs} must be a character vector of rendered prompts.")
  }

  # Empty input is an upstream construction error, not a solvable task.
  if (!length(inputs)) {
    cli::cli_abort("{.arg inputs} must contain at least one rendered prompt.")
  }

  # Resolve the chat factory once so cache metadata and live calls share one
  # stable solver identity.
  chat <- .llm_resolve_chat(solver_chat)

  # Collect the remaining solver arguments once so they can be reused in any
  # batching mode without touching `...` again.
  dots <- rlang::list2(...)

  # Validate the batch size contract up front so the solver fails early on
  # malformed configuration instead of deeper inside the batch loop.
  if (!is.null(batch_size)) {
    if (!is.numeric(batch_size) || length(batch_size) != 1L || is.na(batch_size) || batch_size < 1) {
      cli::cli_abort("{.arg batch_size} must be `NULL` or a positive whole number.")
    }
    batch_size <- as.integer(batch_size)
  }

  # Validate max_retries so retry-wave behaviour is explicit.
  if (!is.numeric(max_retries) || length(max_retries) != 1L || is.na(max_retries) || max_retries < 1) {
    cli::cli_abort("{.arg max_retries} must be a positive whole number.")
  }
  max_retries <- as.integer(max_retries)

  # Accept NULL as the explicit diagnostics opt-out; otherwise require a
  # single non-empty path string.
  if (!is.null(diagnostics_path) && (!rlang::is_string(diagnostics_path) || !nzchar(diagnostics_path))) {
    cli::cli_abort("{.arg diagnostics_path} must be `NULL` or a single non-empty character path.")
  }

  # Capture the cache identity once so repeated rows can reuse a single
  # namespace payload.
  cache_spec <- method_cache_spec(
    chat = chat,
    type = type,
    dots = dots,
    cache_family = cache_family,
    cache_schema_version = cache_schema_version,
    cache_failure = cache_failure
  )
  namespace_hash <- method_cache_namespace_hash(cache_spec)

  # Restore the stable cache namespace before any live calls happen.
  cache_state <- method_cache_read(
    cache_spec = cache_spec,
    cache_mode = cache_mode,
    cache_root = cache_root,
    cache_overlay_root = cache_overlay_root,
    cache_bucket = cache_bucket,
    cache_schema_version = cache_schema_version
  )
  cache_payload <- if (is.null(cache_state)) {
    method_cache_payload(
      cache_spec = cache_spec,
      namespace_hash = namespace_hash,
      rows = list(),
      cache_schema_version = cache_schema_version
    )
  } else {
    cache_state$payload
  }
  if (is.null(cache_payload$rows)) {
    cache_payload$rows <- list()
  }

  # Keep row identity explicit even though the solver only receives rendered
  # prompt strings.
  n <- length(inputs)
  row_indices <- seq_len(n)
  row_keys <- names(inputs)
  if (is.null(row_keys)) {
    row_keys <- as.character(row_indices)
  } else {
    missing_row_keys <- is.na(row_keys) | !nzchar(row_keys)
    row_keys[missing_row_keys] <- as.character(row_indices[missing_row_keys])
  }
  input_hashes <- vapply(inputs, rlang::hash, character(1))
  cache_keys <- vapply(
    seq_len(n),
    function(i) method_cache_row_key(
      namespace_hash = namespace_hash,
      input_hash = input_hashes[[i]],
      row_key = row_keys[[i]],
      row_index = row_indices[[i]]
    ),
    character(1)
  )

  # Prepare the per-row result accumulators so output length always matches
  # inputs.
  result <- vector("list", n)
  solver_chat_list <- vector("list", n)
  solver_metadata_list <- vector("list", n)
  cache_hit <- rep(FALSE, n)
  cache_source <- rep(NA_character_, n)
  attempt_count <- rep(0L, n)
  failure_count <- rep(0L, n)
  failure_reason <- vector("list", n)
  status <- rep("pending", n)

  # Hydrate any cached rows before the first live batch starts.
  if (length(cache_payload$rows)) {
    for (i in seq_len(n)) {
      cached_row <- cache_payload$rows[[cache_keys[[i]]]]
      if (is.null(cached_row) || !is.list(cached_row)) {
        next
      }
      if (!identical(cached_row$cache_key, cache_keys[[i]])) {
        next
      }

      result[[i]] <- cached_row$result
      solver_chat_list[[i]] <- cached_row$solver_chat
      solver_metadata_list[[i]] <- cached_row$solver_metadata
      cache_hit[[i]] <- TRUE
      if (
        !is.null(cached_row$cache_source) &&
          length(cached_row$cache_source) == 1L &&
          !is.na(cached_row$cache_source)
      ) {
        cache_source[[i]] <- cached_row$cache_source
      } else if (!is.null(cache_state$source)) {
        cache_source[[i]] <- cache_state$source
      } else {
        cache_source[[i]] <- "cache"
      }
      if (
        !is.null(cached_row$attempt_count) &&
          length(cached_row$attempt_count) == 1L &&
          !is.na(cached_row$attempt_count)
      ) {
        attempt_count[[i]] <- cached_row$attempt_count
      }
      if (
        !is.null(cached_row$failure_count) &&
          length(cached_row$failure_count) == 1L &&
          !is.na(cached_row$failure_count)
      ) {
        failure_count[[i]] <- cached_row$failure_count
      }
      if (!is.null(cached_row$failure_reason)) {
        failure_reason[[i]] <- cached_row$failure_reason
      }
      if (
        !is.null(cached_row$status) &&
          length(cached_row$status) == 1L &&
          !is.na(cached_row$status)
      ) {
        status[[i]] <- cached_row$status
      } else {
        status[[i]] <- "success"
      }
    }
  }

  # Keep track of which rows still need solving.
  pending <- which(!cache_hit)
  iterations <- 0L

  # Write diagnostics without disturbing solver execution.
  .llm_append_diagnostic(
    diagnostics_path,
    "call_started",
    list(
      total_inputs = as.integer(n),
      batch_size = if (is.null(batch_size)) NULL else as.integer(batch_size),
      max_retries = as.integer(max_retries),
      cache_enabled = !identical(cache_mode, "none"),
      parallelize_batches = isTRUE(parallelize_batches)
    )
  )

  on.exit(
    {
      if (length(pending)) {
        .llm_append_diagnostic(
          diagnostics_path,
          "call_aborted",
          list(
            iterations_completed = as.integer(iterations),
            unresolved_rows = as.integer(length(pending))
          )
        )
      }
    },
    add = TRUE
  )

  # Retry waves keep only the rows that still need a live call.
  while (length(pending) && iterations < max_retries) {
    iterations <- iterations + 1L
    batch_groups <- .llm_split_batches(pending, batch_size)

    for (batch_rows in batch_groups) {
      batch_rows <- as.integer(batch_rows)
      batch_prompts <- as.list(inputs[batch_rows])
      batch_chat <- .llm_clone_chat(chat)

      # Keep the call shape explicit so mocked ellmer helpers can see the same
      # arguments as the live solver.
      batch_result <- tryCatch(
        if (is.null(type)) {
          ellmer::parallel_chat(
            chat = batch_chat,
            prompts = batch_prompts,
            ...
          )
        } else {
          ellmer::parallel_chat_structured(
            chat = batch_chat,
            prompts = batch_prompts,
            type = type,
            ...
          )
        },
        error = function(e) e
      )

      if (inherits(batch_result, "condition")) {
        failure_msg <- conditionMessage(batch_result)
        for (row in batch_rows) {
          attempt_count[[row]] <- attempt_count[[row]] + 1L
          failure_count[[row]] <- failure_count[[row]] + 1L
          failure_reason[[row]] <- c(failure_reason[[row]], failure_msg)
          status[[row]] <- if (iterations < max_retries) {
            "failed_retryable"
          } else {
            "failed_final"
          }
          if (identical(status[[row]], "failed_final") && isTRUE(cache_failure)) {
            result[[row]] <- NA_character_
            cache_payload$rows[[cache_keys[[row]]]] <- method_cache_row_record(
              input = inputs[[row]],
              row_index = row_indices[[row]],
              row_key = row_keys[[row]],
              input_hash = input_hashes[[row]],
              cache_key = cache_keys[[row]],
              result = NA_character_,
              solver_chat = NULL,
              solver_metadata = list(
                payload_kind = if (is.null(type)) "text" else "structured",
                payload = NULL,
                execution = list(
                  status = status[[row]],
                  attempt_count = attempt_count[[row]],
                  failure_count = failure_count[[row]],
                  failure_reason = failure_reason[[row]]
                )
              ),
              attempt_count = attempt_count[[row]],
              failure_count = failure_count[[row]],
              failure_reason = failure_reason[[row]],
              status = status[[row]],
              cache_hit = FALSE,
              cache_source = if (!identical(cache_mode, "none")) cache_mode else NA_character_
            )
          }
        }
        next
      }

      parsed <- .llm_batch_rows(
        batch_result = batch_result,
        batch_size = length(batch_rows),
        structured = !is.null(type)
      )

      for (j in seq_along(batch_rows)) {
        row <- batch_rows[[j]]
        attempt_count[[row]] <- attempt_count[[row]] + 1L

        if (parsed$ok[[j]]) {
          result[[row]] <- parsed$result[[j]]
          solver_chat_list[[row]] <- parsed$solver_chat[[j]]
          status[[row]] <- "success"
          solver_metadata_list[[row]] <- list(
            payload_kind = if (is.null(type)) "text" else "structured",
            payload = parsed$payload[[j]],
            execution = list(
              status = "success",
              attempt_count = attempt_count[[row]],
              failure_count = failure_count[[row]],
              failure_reason = failure_reason[[row]]
            )
          )
          if (!identical(cache_mode, "none")) {
            cache_payload$rows[[cache_keys[[row]]]] <- method_cache_row_record(
              input = inputs[[row]],
              row_index = row_indices[[row]],
              row_key = row_keys[[row]],
              input_hash = input_hashes[[row]],
              cache_key = cache_keys[[row]],
              result = result[[row]],
              solver_chat = solver_chat_list[[row]],
              solver_metadata = solver_metadata_list[[row]],
              attempt_count = attempt_count[[row]],
              failure_count = failure_count[[row]],
              failure_reason = failure_reason[[row]],
              status = "success",
              cache_hit = FALSE,
              cache_source = cache_mode
            )
          }
          next
        }

        failure_count[[row]] <- failure_count[[row]] + 1L
        failure_reason[[row]] <- c(
          failure_reason[[row]],
          "parsed response was empty or malformed"
        )
        status[[row]] <- if (iterations < max_retries) {
          "failed_retryable"
        } else {
          "failed_final"
        }
        solver_metadata_list[[row]] <- list(
          payload_kind = if (is.null(type)) "text" else "structured",
          payload = parsed$payload[[j]],
          execution = list(
            status = status[[row]],
            attempt_count = attempt_count[[row]],
            failure_count = failure_count[[row]],
            failure_reason = failure_reason[[row]]
          )
        )

        if (identical(status[[row]], "failed_final") && isTRUE(cache_failure) && !identical(cache_mode, "none")) {
          result[[row]] <- NA_character_
          cache_payload$rows[[cache_keys[[row]]]] <- method_cache_row_record(
            input = inputs[[row]],
            row_index = row_indices[[row]],
            row_key = row_keys[[row]],
            input_hash = input_hashes[[row]],
            cache_key = cache_keys[[row]],
            result = NA_character_,
            solver_chat = NULL,
            solver_metadata = solver_metadata_list[[row]],
            attempt_count = attempt_count[[row]],
            failure_count = failure_count[[row]],
            failure_reason = failure_reason[[row]],
            status = "failed_final",
            cache_hit = FALSE,
            cache_source = cache_mode
          )
        }
      }

      # Remove resolved rows from the pending set and persist the namespace.
      pending <- setdiff(pending, batch_rows[parsed$ok])
      if (!identical(cache_mode, "none")) {
        cache_payload$updated_at <- Sys.time()
        method_cache_write(
          cache_payload = cache_payload,
          cache_spec = cache_spec,
          cache_mode = cache_mode,
          cache_root = cache_root,
          cache_overlay_root = cache_overlay_root,
          cache_bucket = cache_bucket
        )
      }

      .llm_append_diagnostic(
        diagnostics_path,
        "batch_completed",
        list(
          rows = as.integer(batch_rows),
          success = as.integer(sum(parsed$ok)),
          failed = as.integer(sum(!parsed$ok))
        )
      )
    }
  }

  # Mark any still-pending rows as terminal failures once retries are exhausted.
  if (length(pending)) {
    for (row in pending) {
      if (status[[row]] != "success") {
        status[[row]] <- "failed_final"
        failure_count[[row]] <- max(1L, failure_count[[row]])
        if (is.null(failure_reason[[row]]) || !length(failure_reason[[row]])) {
          failure_reason[[row]] <- "retry budget exhausted"
        }
        if (isTRUE(cache_failure) && !identical(cache_mode, "none")) {
          result[[row]] <- NA_character_
          cache_payload$rows[[cache_keys[[row]]]] <- method_cache_row_record(
            input = inputs[[row]],
            row_index = row_indices[[row]],
            row_key = row_keys[[row]],
            input_hash = input_hashes[[row]],
            cache_key = cache_keys[[row]],
            result = NA_character_,
            solver_chat = NULL,
            solver_metadata = list(
              payload_kind = if (is.null(type)) "text" else "structured",
              payload = NULL,
              execution = list(
                status = "failed_final",
                attempt_count = attempt_count[[row]],
                failure_count = failure_count[[row]],
                failure_reason = failure_reason[[row]]
              )
            ),
            attempt_count = attempt_count[[row]],
            failure_count = failure_count[[row]],
            failure_reason = failure_reason[[row]],
            status = "failed_final",
            cache_hit = FALSE,
            cache_source = cache_mode
          )
        }
      }
    }
    if (!identical(cache_mode, "none")) {
      cache_payload$updated_at <- Sys.time()
      method_cache_write(
        cache_payload = cache_payload,
        cache_spec = cache_spec,
        cache_mode = cache_mode,
        cache_root = cache_root,
        cache_overlay_root = cache_overlay_root,
        cache_bucket = cache_bucket
      )
    }
  }

  # Assemble a tibble so downstream targets can inspect telemetry row by row.
  tibble::tibble(
    input = inputs,
    row_index = row_indices,
    row_key = row_keys,
    input_hash = input_hashes,
    cache_key = cache_keys,
    result = result,
    solver_chat = solver_chat_list,
    solver_metadata = solver_metadata_list,
    cache_hit = cache_hit,
    cache_source = cache_source,
    attempt_count = attempt_count,
    failure_count = failure_count,
    failure_reason = failure_reason,
    status = status
  )
}
