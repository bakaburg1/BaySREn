#' Run an ellmer chat solver
#'
#' This solver takes already-rendered prompt strings from the dataset `input`
#' column. It selects a local unstructured or structured parallel helper based
#' on `type`, then either runs the helper once or splits the work into
#' batches, with optional mirai-backed batch parallelism.
#'
#' When `cache_dir` is set, results are cached within a stable model/parameter
#' namespace and resolved in memory one batch at a time. Completed batches are
#' merged into the stable namespace cache after each batch and written
#' atomically on the host. Retry waves (controlled by `max_retries`) rerun only
#' the rows that failed in prior iterations, preserving successful results from
#' cache.
#'
#' @param inputs Character vector of rendered prompts.
#' @param ... Additional solver arguments accepted by `vitals::Task$eval()`.
#'   These are forwarded to `ellmer::parallel_chat()` in unstructured mode and
#'   to `ellmer::parallel_chat_structured()` in structured mode.
#' @param solver_chat Either an `ellmer` `Chat` object or a zero-argument
#'   factory that returns one.
#' @param type `NULL` for unstructured chat output, or an ellmer structured
#'   type object for structured conversion.
#' @param batch_size Positive whole number giving the batch size for batched
#'   execution. `NULL` disables batching and runs the solver once over the
#'   full input vector. The default is `500`.
#' @param parallelize_batches Logical flag controlling whether batched work
#'   should use `mirai` when it is available and a daemon pool is already set.
#' @param cache_dir Directory path for the persistent cache. `NULL` or a
#'   zero-length cache-directory value disables caching entirely. The default
#'   is `cache/llm` under the current `here` project root.
#' @param cache_failure Logical flag controlling whether terminal failures are
#'   cached. Defaults to `TRUE`.
#' @param log_dir Directory path for append-only solver logs. Defaults to
#'   `.solver_log`. Set to `NULL` to disable solver logging.
#' @param max_retries Number of retry-wave passes to attempt for failed
#'   rows. The default is `5L`. Set to a higher value
#'   to re-run failed rows up to that many additional times.
#' @param retry_on Character vector of failure conditions that should trigger a
#'   retry. Supported values are `"no_response"`, `"not_parsable"`, and
#'   `"empty_response"`.
#'
#' @return A list with `result`, `solver_chat`, and `solver_metadata` elements
#'   whose lengths match `length(inputs)`. `solver_metadata` is a per-row
#'   telemetry envelope with a `payload` field for the raw solver response and
#'   an `execution` field for summary telemetry. Each element may contain a
#'   failure marker (`failure_count` and `failure_reason`) when the
#'   corresponding row could not be resolved after all retry waves. The
#'   `failure_reason` field stores the full retry history as a character
#'   vector. Deterministic provider or request-construction errors are handled
#'   separately from retryable LLM failures: cacheable rows are still flushed,
#'   but the solver aborts after the run to stop the pipeline.
#'
#' @export
llm_solver <- function(
  inputs,
  ...,
  solver_chat,
  type = NULL,
  batch_size = 500L,
  parallelize_batches = TRUE,
  cache_dir = here::here("cache", "llm"),
  cache_failure = TRUE,
  log_dir = ".solver_log",
  max_retries = 5L,
  retry_on = c("no_response", "not_parsable", "empty_response")
) {
  # Validate the prompt vector early so the task contract stays explicit.
  if (!rlang::is_character(inputs)) {
    cli::cli_abort(
      "{.arg inputs} must be a character vector of rendered prompts."
    )
  }

  # Empty input is an upstream construction error, not a solvable task.
  if (rlang::is_empty(inputs)) {
    cli::cli_abort(
      "{.arg inputs} must contain at least one rendered prompt."
    )
  }

  # Resolve the solver chat once for cache metadata and sequential execution.
  # When batch work is parallelized, we still want each worker to build its
  # own chat from the user-supplied factory instead of shipping a
  chat <- .llm_solver_resolve_chat(solver_chat)

  # Collect the remaining solver arguments once so they can be reused in any
  # batching mode without touching `...` again.
  dots <- rlang::list2(...)

  # Normalize cache-dir sentinels so direct calls can use NULL and the
  # wrapper path can still disable caching with an empty string.
  if (!rlang::is_null(cache_dir) && !rlang::is_string(cache_dir)) {
    cli::cli_abort(
      "{.arg cache_dir} must be `NULL` or a single character path."
    )
  }

  # If cache_dir is an empty string, set it to NULL.
  if (rlang::is_string(cache_dir) && !nzchar(cache_dir)) {
    cache_dir <- NULL
  }

  # Accept NULL as the explicit logging opt-out; otherwise require one
  # non-empty path string.
  if (
    !rlang::is_null(log_dir) && (!rlang::is_string(log_dir) || !nzchar(log_dir))
  ) {
    cli::cli_abort(
      "{.arg log_dir} must be `NULL` or a single non-empty character path."
    )
  }

  # Validate the cache-failure flag before any cache lookup happens.
  if (!rlang::is_bool(cache_failure)) {
    cli::cli_abort("{.arg cache_failure} must be a single logical value.")
  }

  # Validate the batch size contract up front so the solver fails early on
  # malformed configuration instead of deeper inside the batch loop.
  if (!rlang::is_null(batch_size)) {
    if (!rlang::is_scalar_integerish(batch_size) || batch_size < 1L) {
      cli::cli_abort(
        "{.arg batch_size} must be `NULL` or a positive whole number."
      )
    }
    batch_size <- as.integer(batch_size)
  }

  # Validate max_retries so retry-wave behaviour is explicit.
  if (!rlang::is_scalar_integerish(max_retries) || max_retries < 1L) {
    cli::cli_abort(
      "{.arg max_retries} must be a positive whole number."
    )
  }
  max_retries <- as.integer(max_retries)

  # Normalize the retry contract once so row classification can stay local to
  # the batch loop.
  retry_on <- .llm_solver_validate_retry_on(retry_on)

  # Capture the selected helper set once so local and mirai-backed batch work
  # can reuse the same self-contained call shape.
  .parallel_helper <- if (is.null(type)) {
    .llm_solver_parallel_unstructured
  } else {
    .llm_solver_parallel_structured
  }
  .direct_fallback <- if (is.null(type)) {
    .llm_solver_direct_fallback_unstructured
  } else {
    .llm_solver_direct_fallback_structured
  }
  .failure_chat <- .llm_solver_failure_chat
  .last_text <- .llm_solver_last_text
  .json_string <- .llm_solver_json_string
  # Forward every worker dependency explicitly so remote daemons do not need
  # to resolve package-local helper names at execution time.
  .parallel_chat_solver <- function(
    chat,
    prompts,
    ...,
    type = NULL,
    log_context = NULL
  ) {
    rlang::exec(
      .parallel_helper,
      chat,
      prompts,
      ...,
      type = type,
      log_context = log_context,
      direct_fallback = .direct_fallback,
      failure_chat = .failure_chat,
      last_text = .last_text,
      json_string = .json_string
    )
  }

  # Resolve the stable cache namespace once so repeated rows can reuse a
  # single in-memory cache object and the solver avoids per-row filesystem I/O.
  cache_spec <- NULL
  cache_key <- NULL
  stable_cache <- NULL
  if (!is.null(cache_dir)) {
    # Build the stable namespace payload from the resolved solver config.
    cache_spec <- .caching_spec(chat, type, dots, cache_failure = cache_failure)
    cache_key <- .caching_namespace_hash(cache_spec)

    # Load the stable cache once so the hot path stays in memory.
    stable_cache <- .caching_read(
      cache_dir = cache_dir,
      cache_spec = cache_spec,
      namespace_hash = cache_key
    )
    if (!.caching_validate(stable_cache, cache_spec, cache_key)) {
      # Fall back to a fresh empty namespace when the on-disk payload is stale.
      stable_cache <- .caching_payload(
        cache_spec = cache_spec,
        namespace_hash = cache_key,
        rows = list()
      )
    } else if (is.null(stable_cache$rows)) {
      stable_cache$rows <- list()
    }
  }

  # Keep row identity explicit even though `vitals` only forwards the prompt
  # vector here. `Task$solve()` expands epochs into `self$get_samples()` and
  # then calls the solver with `self$get_samples()$input`, so the solver never
  # sees an `epoch` column directly. We therefore derive a stable row identity
  # from the prompt vector itself and disambiguate duplicate prompt hashes.
  n <- length(inputs)
  row_ids <- names(inputs)
  row_payload_kind <- if (is.null(type)) "text" else "structured"
  .row_id_for <- function(i) {
    # Preserve a human-readable row label when the caller supplied one.
    row_id <- if (rlang::is_null(row_ids) || length(row_ids) < i) {
      NULL
    } else {
      row_ids[[i]]
    }
    if (!is.null(row_id) && (!rlang::is_string(row_id) || !nzchar(row_id))) {
      row_id <- NULL
    }
    row_id
  }
  row_base_hashes <- purrr::map_chr(
    seq_len(n),
    function(i) {
      # The solver receives only the prompt vector, not the expanded sample
      # table, so row-specific bookkeeping has to be reconstructed from the
      # vector itself.
      rlang::hash(list(
        input = inputs[[i]],
        row_id = .row_id_for(i)
      ))
    }
  )
  row_duplicate_index <- ave(
    seq_along(row_base_hashes),
    row_base_hashes,
    FUN = seq_along
  )
  .row_key_for <- function(i) {
    # Count duplicate prompt hashes and fold the duplicate occurrence back
    # into the final row key so repeated epoch replicas remain distinct even
    # though `vitals` never forwards an `epoch` column to the solver.
    rlang::hash(list(
      row_hash = row_base_hashes[[i]],
      duplicate_index = as.integer(row_duplicate_index[[i]])
    ))
  }

  # Prepare the per-row result accumulators so output length always matches
  # inputs.
  result <- rep(NA_character_, n)
  solver_chat_list <- vector("list", n)
  solver_metadata_list <- vector("list", n)
  attempt_count <- rep(0L, n)
  failure_count <- rep(0L, n)
  failure_reason <- rep(list(character()), n)
  technical_failures <- list()
  parallel_log_state <- new.env(parent = emptyenv())
  parallel_log_state$context <- NULL
  parallel_log_state$completed <- FALSE

  # Start the append-only solver log as soon as logging is enabled so callers
  # can tail the file immediately, even before batch execution details are
  # known.
  if (!is.null(log_dir)) {
    parallel_log_state$context <- .llm_solver_create_log_context(
      chat = chat,
      type = type,
      dots = dots,
      log_dir = log_dir
    )
    .llm_solver_append_log(
      parallel_log_state$context,
      "call_started",
      list(
        total_inputs = as.integer(n),
        batch_size = if (is.null(batch_size)) NULL else as.integer(batch_size),
        max_retries = as.integer(max_retries),
        cache_enabled = !is.null(cache_dir),
        parallelize_batches = isTRUE(parallelize_batches)
      )
    )
  }

  # Track which rows still need solving (initially all).
  pending <- seq_len(n)
  iterations <- 0L

  # Mark unfinished parallel log files explicitly when the solver aborts.
  on.exit(
    {
      # Only mark the run as aborted when no normal completion/failure path has
      # already finalized the log file.
      if (
        !is.null(parallel_log_state$context) &&
          !isTRUE(parallel_log_state$completed)
      ) {
        # Record how far the solver got before the interruption so the final log
        # still explains the abandoned state.
        .llm_solver_append_log(
          parallel_log_state$context,
          "call_aborted",
          list(
            iterations_completed = as.integer(iterations),
            unresolved_rows = as.integer(length(pending))
          )
        )
        # Rename the live file out of the running namespace once the abort is
        # confirmed.
        .llm_solver_finalize_log(
          parallel_log_state$context,
          status = "aborted"
        )
      }
    },
    add = TRUE
  )

  # Retry-wave loop: each iteration re-runs only the rows that failed.
  for (iteration in seq_len(max_retries)) {
    abort_after_iteration <- FALSE

    # Exit if all rows have been resolved.
    if (length(pending) == 0L) {
      break
    }

    # Track iteration count.
    iterations <- iteration
    incoming_rows <- length(pending)

    # Restore any rows already present in the stable cache before solving.
    cached_indices <- integer(0)
    uncached_indices <- integer(0)
    if (!is.null(stable_cache)) {
      for (i in pending) {
        row_key <- .row_key_for(i)
        cached <- if (rlang::is_null(stable_cache$rows)) {
          NULL
        } else {
          stable_cache$rows[[row_key]]
        }
        if (
          !is.null(cached) &&
            (identical(cached$status, "success") ||
              (isTRUE(cache_failure) &&
                identical(cached$status, "failed_final")))
        ) {
          result[i] <- cached$result
          solver_chat_list[[i]] <- cached$solver_chat
          attempt_count[i] <- if (
            !is.null(cached$solver_metadata) &&
              !is.null(cached$solver_metadata$execution) &&
              !is.null(cached$solver_metadata$execution$attempt_count)
          ) {
            as.integer(cached$solver_metadata$execution$attempt_count)
          } else {
            0L
          }
          failure_count[i] <- if (is.null(cached$failure_count)) {
            0L
          } else {
            cached$failure_count
          }
          failure_reason[[i]] <- .normalize_failure_reason_history(
            cached$failure_reason
          )
          if (!is.null(solver_metadata_list)) {
            solver_metadata_list[[i]] <- .llm_solver_mark_cached_envelope(
              cached$solver_metadata
            )
          }
          cached_indices <- c(cached_indices, i)
        } else {
          uncached_indices <- c(uncached_indices, i)
        }
      }

      if (length(cached_indices) > 0L) {
        .llm_solver_emit_message(
          "info",
          paste0(
            "solver: ",
            length(cached_indices),
            " rows restored from cache"
          ),
          parallel_log_state$context
        )
      }

      pending <- uncached_indices
    }

    # Nothing left to solve after cache restore.
    if (length(pending) == 0L) {
      break
    }

    # Split the remaining rows into execution batches for this iteration.
    pending_inputs <- inputs[pending]
    batch_inputs <- .split_solver_batches(pending_inputs, batch_size)
    prior_failures <- if (iteration == 1L) 0L else incoming_rows

    # Emit an iteration summary before any new work starts so the logs show the
    # incoming batch count, cache hits, and retry pressure for this wave.
    .llm_solver_emit_message(
      "info",
      paste0(
        "solver: iteration ",
        iteration,
        "/",
        max_retries,
        "; ",
        length(batch_inputs),
        " batch(es); ",
        length(cached_indices),
        " cached; ",
        length(pending),
        " to process; ",
        prior_failures,
        " prior failures"
      ),
      parallel_log_state$context
    )
    .llm_solver_append_log(
      parallel_log_state$context,
      "iteration_started",
      list(
        iteration = as.integer(iteration),
        max_retries = as.integer(max_retries),
        batch_count = as.integer(length(batch_inputs)),
        cached_rows = as.integer(length(cached_indices)),
        rows_to_process = as.integer(length(pending)),
        prior_failures = as.integer(prior_failures)
      )
    )
    .llm_solver_append_log(
      parallel_log_state$context,
      "progress",
      list(
        line = .llm_solver_progress_line(
          completed_batches = 0L,
          total_batches = length(batch_inputs),
          iteration = iteration,
          max_retries = max_retries,
          success_count = 0L,
          failure_count = 0L
        )
      )
    )

    # Execute all batches, optionally through mirai when daemons are active.
    batch_results <- .dispatch_batches(
      batch_inputs,
      parallelize_batches,
      parallel_chat_solver = .parallel_chat_solver,
      chat = chat,
      type = type,
      dots = dots,
      log_context = parallel_log_state$context,
      iteration = iteration,
      max_retries = max_retries
    )

    # Reset the per-iteration bookkeeping.
    failed_this_wave <- integer(0)
    failures_this_wave <- 0L
    successes_this_wave <- 0L
    pending_offset <- 0L

    for (batch_index in seq_along(batch_inputs)) {
      # Pull out the current batch and align its results back to row indices.
      batch_in <- batch_inputs[[batch_index]]
      batch_out <- batch_results[[batch_index]]
      batch_indices <- pending[pending_offset + seq_along(batch_in)]
      batch_keys <- purrr::map_chr(
        batch_indices,
        .row_key_for
      )
      batch_dirty <- FALSE
      batch_failures <- 0L
      batch_successes <- 0L
      batch_has_global_technical <- FALSE

      # Materialize row-level records from the batch response so success and
      # failure metadata can be merged back into the stable namespace.
      for (j in seq_along(batch_in)) {
        # Handle one row at a time so success, retry, and failure metadata stay
        # aligned.
        i <- batch_indices[[j]]
        res_val <- batch_out$result[[j]]
        chat_val <- batch_out$solver_chat[[j]]
        row_metadata <- if (is.null(type)) {
          NULL
        } else {
          batch_out$solver_metadata[[j]]
        }
        row_key <- batch_keys[[j]]
        attempt_count[i] <- attempt_count[i] + 1L
        batch_used_direct_fallback <- isTRUE(batch_out$used_direct_fallback)
        row_failure <- .llm_solver_classify_failure(
          result = res_val,
          row_metadata = row_metadata,
          chat = chat_val,
          type = type
        )
        # Separate deterministic technical faults from ordinary LLM/runtime
        # failures before deciding whether the row can be retried.
        row_failed <- !is.null(row_failure$condition)
        row_is_technical <- identical(
          row_failure$failure_class,
          "technical_error"
        )
        row_retryable <- row_failed &&
          !row_is_technical &&
          row_failure$condition %in% retry_on &&
          iteration < max_retries
        row_status <- if (!row_failed) {
          "success"
        } else if (row_is_technical) {
          "failed_technical"
        } else if (row_retryable) {
          "failed_retryable"
        } else {
          "failed_final"
        }

        # Classify row-level failures and carry only unresolved rows forward.
        if (row_failed) {
          failures_this_wave <- failures_this_wave + 1L
          batch_failures <- batch_failures + 1L
          failure_count[i] <- iteration
          failure_reason[[i]] <- c(failure_reason[[i]], row_failure$reason)
          result[i] <- NA_character_
          solver_chat_list[[i]] <- .llm_solver_failure_chat(
            chat,
            batch_in[[j]],
            row_failure$reason
          )
          row_payload <- if (identical(row_payload_kind, "structured")) {
            row_metadata
          } else {
            NULL
          }

          # Keep technical failures in a side channel so the solver can finish
          # salvaging reusable rows and then abort the full call afterwards.
          if (row_is_technical) {
            technical_failures[[length(technical_failures) + 1L]] <- list(
              row_index = i,
              iteration = iteration,
              prompt = batch_in[[j]],
              scope = row_failure$technical_scope,
              reason = row_failure$reason
            )
            batch_has_global_technical <- batch_has_global_technical ||
              identical(row_failure$technical_scope, "global")
          }

          if (row_status == "failed_retryable") {
            failed_this_wave <- c(failed_this_wave, i)
          }
        } else {
          # Clear failure state and keep the successful payload for reuse.
          failure_count[i] <- 0L
          result[i] <- res_val
          solver_chat_list[[i]] <- chat_val
          row_payload <- if (row_payload_kind == "text") {
            res_val
          } else {
            row_metadata
          }
          successes_this_wave <- successes_this_wave + 1L
          batch_successes <- batch_successes + 1L
        }

        # Build the telemetry envelope after the row has been classified.
        solver_metadata_list[[i]] <- .llm_solver_telemetry_envelope(
          payload_kind = row_payload_kind,
          payload = row_payload,
          chat = solver_chat_list[[i]],
          status = row_status,
          attempt_count = attempt_count[i],
          cache_hit = FALSE,
          restored_from_cache = FALSE,
          used_direct_fallback = batch_used_direct_fallback,
          failure_reason = failure_reason[[i]],
          failure_class = row_failure$failure_class,
          technical_scope = row_failure$technical_scope
        )

        # Persist successful rows and optionally terminal failures.
        if (
          !is.null(stable_cache) &&
            (identical(row_status, "success") ||
              (isTRUE(cache_failure) && identical(row_status, "failed_final")))
        ) {
          stable_cache$rows[[row_key]] <- .caching_row_record(
            input = batch_in[[j]],
            row_index = i,
            row_id = .row_id_for(i),
            result = if (row_failed) NA_character_ else res_val,
            solver_chat = solver_chat_list[[i]],
            solver_metadata = solver_metadata_list[[i]],
            failure_count = failure_count[i],
            failure_reason = failure_reason[[i]],
            status = row_status
          )
          batch_dirty <- TRUE
        }
      }

      # Write the stable cache immediately after each finished batch so the
      # host keeps only results that are already reusable.
      if (!is.null(cache_dir) && batch_dirty) {
        stable_cache$updated_at <- Sys.time()
        .caching_write(
          cache_dir = cache_dir,
          cache_spec = cache_spec,
          namespace_hash = cache_key,
          cache_payload = stable_cache
        )
      }

      if (batch_has_global_technical) {
        abort_after_iteration <- TRUE
      }

      # Record the host-side outcome after the batch has been re-aligned to
      # row-level successes and failures.
      .llm_solver_append_log(
        parallel_log_state$context,
        "batch_collected",
        list(
          iteration = as.integer(iteration),
          batch_index = as.integer(batch_index),
          batch_size = as.integer(length(batch_in)),
          success_count = as.integer(batch_successes),
          failure_count = as.integer(batch_failures),
          used_direct_fallback = isTRUE(batch_out$used_direct_fallback)
        )
      )
      .llm_solver_append_log(
        parallel_log_state$context,
        "progress",
        list(
          line = .llm_solver_progress_line(
            completed_batches = batch_index,
            total_batches = length(batch_inputs),
            iteration = iteration,
            max_retries = max_retries,
            success_count = successes_this_wave,
            failure_count = failures_this_wave
          )
        )
      )

      pending_offset <- pending_offset + length(batch_in)

      if (abort_after_iteration) {
        break
      }
    }

    .llm_solver_emit_message(
      "info",
      paste0(
        "solver: iteration ",
        iteration,
        "/",
        max_retries,
        " completed; ",
        successes_this_wave,
        " success(es); ",
        failures_this_wave,
        " failure(s); ",
        length(failed_this_wave),
        " unresolved"
      ),
      parallel_log_state$context
    )
    .llm_solver_append_log(
      parallel_log_state$context,
      "iteration_completed",
      list(
        iteration = as.integer(iteration),
        max_retries = as.integer(max_retries),
        success_count = as.integer(successes_this_wave),
        failure_count = as.integer(failures_this_wave),
        unresolved_rows = as.integer(length(failed_this_wave))
      )
    )

    # Carry only the unresolved rows into the next retry wave.
    pending <- failed_this_wave

    if (abort_after_iteration) {
      break
    }
  }

  if (length(technical_failures) > 0L) {
    # Record the technical-failure summary before aborting so the append-only
    # solver log still captures why the pipeline stopped.
    .llm_solver_append_log(
      parallel_log_state$context,
      "call_failed_technical",
      list(
        iterations_completed = as.integer(iterations),
        technical_failure_count = as.integer(length(technical_failures)),
        global_technical_count = as.integer(sum(
          purrr::map_chr(technical_failures, "scope") == "global"
        )),
        row_technical_count = as.integer(sum(
          purrr::map_chr(technical_failures, "scope") == "row"
        ))
      )
    )
    .llm_solver_finalize_log(
      parallel_log_state$context,
      status = "failed"
    )
    parallel_log_state$completed <- TRUE
    .llm_solver_abort_technical_failures(technical_failures)
  }

  # Attach row-level failure metadata to the result vector so downstream
  # scoring can see the final retry outcome without opening cache files.
  .llm_solver_emit_message(
    "info",
    paste0("solver: completed ", iterations, " iteration(s)"),
    parallel_log_state$context
  )
  .llm_solver_append_log(
    parallel_log_state$context,
    "call_completed",
    list(
      iterations_completed = as.integer(iterations),
      final_failure_count = as.integer(sum(failure_count > 0L)),
      final_success_count = as.integer(sum(!is.na(result)))
    )
  )
  .llm_solver_finalize_log(
    parallel_log_state$context,
    status = "done"
  )
  parallel_log_state$completed <- TRUE

  # Attach failure metadata on the main result vector for downstream scoring.
  attr(result, "failure_count") <- failure_count
  attr(result, "failure_reason") <- failure_reason

  # Return the standardized vitals-compatible solver output.
  list(
    result = result,
    solver_chat = solver_chat_list,
    solver_metadata = solver_metadata_list
  )
}

#' Dispatch a list of batches with optional mirai parallelism
#'
#' @param batch_inputs List of character vectors.
#' @param parallelize_batches Logical controlling mirai use when available.
#' @param parallel_chat_solver Selected solver helper for one batch.
#' @param chat Resolved chat object for the batch call.
#' @param type Structured type object or `NULL`.
#' @param dots Named list of forwarded solver parameters.
#'
#' @return A list of solver outputs, one per batch.
#'
#' @keywords internal
.dispatch_batches <- function(
  batch_inputs,
  parallelize_batches,
  parallel_chat_solver,
  chat,
  type,
  dots,
  log_context = NULL,
  iteration = NULL,
  max_retries = NULL
) {
  # Wrap one batch call so local and mirai-backed execution share the same body.
  .run_batch <- function(
    batch_job,
    parallel_chat_solver,
    chat,
    type,
    dots,
    log_context,
    append_log,
    append_collection_progress,
    capture_console_output
  ) {
    prompt_inputs <- batch_job$prompts
    batch_index <- batch_job$batch_index
    iteration <- batch_job$iteration
    total_batches <- batch_job$total_batches
    max_retries <- batch_job$max_retries

    # Append worker-side lifecycle lines so the host can tail the file while
    # daemon jobs are still running.
    append_log(
      log_context,
      "batch_started",
      list(
        iteration = as.integer(iteration),
        batch_index = as.integer(batch_index),
        batch_size = as.integer(length(prompt_inputs)),
        worker_pid = as.integer(Sys.getpid())
      )
    )

    # Capture raw worker console output in the same append-only log file so the
    # host log can include forwarded httr2 progress bars as well.
    batch_out <- tryCatch(
      capture_console_output(
        log_context = log_context,
        expr = rlang::exec(
          parallel_chat_solver,
          chat,
          as.list(prompt_inputs),
          type = type,
          log_context = log_context,
          !!!dots
        )
      ),
      error = function(e) e
    )

    # Persist worker-visible failures before letting the error propagate.
    if (inherits(batch_out, "error")) {
      append_log(
        log_context,
        "batch_failed",
        list(
          iteration = as.integer(iteration),
          batch_index = as.integer(batch_index),
          batch_size = as.integer(length(prompt_inputs)),
          worker_pid = as.integer(Sys.getpid()),
          message = conditionMessage(batch_out)
        )
      )
      append_collection_progress(
        log_context = log_context,
        iteration = iteration,
        max_retries = max_retries,
        total_batches = total_batches
      )
      stop(batch_out)
    }

    # Mark successful worker completion before returning the batch payload.
    append_log(
      log_context,
      "batch_finished",
      list(
        iteration = as.integer(iteration),
        batch_index = as.integer(batch_index),
        batch_size = as.integer(length(prompt_inputs)),
        worker_pid = as.integer(Sys.getpid()),
        used_direct_fallback = isTRUE(batch_out$used_direct_fallback)
      )
    )
    append_collection_progress(
      log_context = log_context,
      iteration = iteration,
      max_retries = max_retries,
      total_batches = total_batches
    )

    batch_out
  }

  # Wrap the prompt batches with stable metadata so local and mirai-backed
  # execution can share the same worker body and log fields.
  batch_jobs <- purrr::imap(
    batch_inputs,
    function(prompts, batch_index) {
      list(
        prompts = prompts,
        batch_index = as.integer(batch_index),
        iteration = as.integer(iteration),
        total_batches = as.integer(length(batch_inputs)),
        max_retries = as.integer(max_retries)
      )
    }
  )

  # Nothing to dispatch when there are no batches.
  if (rlang::is_empty(batch_jobs)) {
    return(list())
  }

  # Use mirai only when the caller asked for it and a daemon pool exists.
  if (
    length(batch_jobs) == 1L ||
      !isTRUE(parallelize_batches) ||
      !rlang::is_installed("mirai") ||
      !mirai::daemons_set()
  ) {
    return(purrr::map(
      batch_jobs,
      .run_batch,
      parallel_chat_solver = parallel_chat_solver,
      chat = chat,
      type = type,
      dots = dots,
      log_context = log_context,
      append_log = .llm_solver_append_log,
      append_collection_progress = .llm_solver_append_collection_progress,
      capture_console_output = .llm_solver_capture_console_output,
      .progress = TRUE
    ))
  }

  # Dispatch the shared worker body to mirai with only explicit helper inputs.
  jobs <- mirai::mirai_map(
    batch_jobs,
    .run_batch,
    .args = list(
      parallel_chat_solver = parallel_chat_solver,
      chat = chat,
      type = type,
      dots = dots,
      log_context = log_context,
      append_log = .llm_solver_append_log,
      append_collection_progress = .llm_solver_append_collection_progress,
      capture_console_output = .llm_solver_capture_console_output
    )
  )
  jobs[mirai::.progress]
}

#' Append raw stdout output and `cli` progress events to the active solver log
#' while preserving live console updates
#'
#' @param log_context Optional log context from
#'   `.llm_solver_create_log_context()`.
#' @param expr Expression to evaluate while console output is mirrored.
#'
#' @return Returns the value of `expr`.
#'
#' @keywords internal
.llm_solver_capture_console_output <- function(log_context, expr) {
  # Exit immediately when logging is disabled or the active file is unavailable.
  if (
    is.null(log_context) ||
      !is.list(log_context) ||
      is.null(log_context$path) ||
      !fs::file_exists(log_context$path)
  ) {
    return(force(expr))
  }

  # Override cli's plain-text logger temporarily so progress events written to
  # the solver log use a compact percentage-plus-bar format instead of raw
  # `current/total` counters.
  cli_ns <- asNamespace("cli")
  old_logger_out <- get("logger_out", envir = cli_ns, inherits = FALSE)
  logger_binding_locked <- bindingIsLocked("logger_out", cli_ns)
  if (logger_binding_locked) {
    unlockBinding("logger_out", cli_ns)
  }
  assign(
    "logger_out",
    function(bar, event) {
      # Read the current and total counters defensively because cli progress
      # bars can vary by type and provider.
      current <- tryCatch(as.numeric(bar$current), error = function(e) NA_real_)
      total <- tryCatch(as.numeric(bar$total), error = function(e) NA_real_)

      # Convert the counters into a bounded ratio so the file output stays
      # stable even if a provider reports slightly inconsistent values.
      if (is.finite(current) && is.finite(total) && total > 0) {
        ratio <- min(max(current / total, 0), 1)
      } else {
        ratio <- NA_real_
      }

      # Build a fixed-width ASCII bar so the file can be tailed comfortably in
      # plain text.
      width <- 30L
      filled <- if (is.na(ratio)) 0L else as.integer(round(ratio * width))
      progress_bar <- paste0(
        "[",
        strrep("#", filled),
        strrep("-", width - filled),
        "]"
      )
      percent <- if (is.na(ratio)) {
        " ??%"
      } else {
        sprintf("%3d%%", as.integer(round(ratio * 100)))
      }

      # Reuse cli's timestamp formatter so these lines stay aligned with the
      # rest of the logger output.
      timestamp <- get("format_iso_8601", envir = cli_ns, inherits = FALSE)(
        Sys.time()
      )
      cat(
        sep = "",
        timestamp,
        " ",
        bar$id,
        " ",
        percent,
        " ",
        progress_bar,
        " ",
        event,
        "\n"
      )
    },
    envir = cli_ns
  )
  on.exit(
    {
      # Restore cli's original logger implementation so the customization stays
      # scoped to this capture helper.
      assign("logger_out", old_logger_out, envir = cli_ns)
      if (logger_binding_locked) {
        lockBinding("logger_out", cli_ns)
      }
    },
    add = TRUE
  )

  # Extend the active cli progress-handler stack with the plain-text logger so
  # progress updates are emitted as ordinary lines that can be mirrored into
  # the same solver log file.
  old_progress_handlers_force <- getOption("cli.progress_handlers_force")
  progress_handlers_force <- if (is.null(old_progress_handlers_force)) {
    "logger"
  } else {
    unique(c(as.character(old_progress_handlers_force), "logger"))
  }
  options(cli.progress_handlers_force = progress_handlers_force)
  on.exit(
    {
      # Restore the caller's cli progress-handler preference after the wrapped
      # expression finishes so this helper does not leak logging behavior.
      options(cli.progress_handlers_force = old_progress_handlers_force)
    },
    add = TRUE
  )

  # Open one append-mode connection so stdout stays visible in the console
  # while still being mirrored into the log file.
  out_con <- file(log_context$path, open = "at")
  # Remember the current output-sink depth so cleanup only unwinds the sinks
  # created by this helper.
  output_sink_count <- sink.number(type = "output")
  sink(out_con, type = "output", split = TRUE)
  on.exit(
    {
      # Remove only the output sinks added by this helper so outer testthat or
      # caller-managed sinks stay intact.
      while (sink.number(type = "output") > output_sink_count) {
        sink(type = "output")
      }

      # Close the append-mode connection after the sink stack has been
      # restored.
      try(close(out_con), silent = TRUE)
    },
    add = TRUE
  )

  force(expr)
}

#' Emit one solver message to both `cli` and the append-only log
#'
#' @param level Message severity. Supported values are `"info"`,
#'   `"warning"`, and `"danger"`.
#' @param message Preformatted message string.
#' @param log_context Optional log context from
#'   `.llm_solver_create_log_context()`.
#'
#' @return Invisibly returns `message`.
#'
#' @keywords internal
.llm_solver_emit_message <- function(
  level = c("info", "warning", "danger"),
  message,
  log_context = NULL
) {
  # Validate the emitted message once so console and file output stay aligned.
  level <- rlang::arg_match(level)
  if (!rlang::is_string(message) || !nzchar(message)) {
    cli::cli_abort("{.arg message} must be a non-empty string.")
  }

  # Mirror the message to the console with the matching `cli` severity.
  switch(
    level,
    info = cli::cli_alert_info(message),
    warning = cli::cli_alert_warning(message),
    danger = cli::cli_alert_danger(message)
  )

  # Append the same message to the readable solver log when logging is active.
  .llm_solver_append_log(
    log_context,
    "message",
    list(
      level = level,
      message = message
    )
  )

  invisible(message)
}

#' Build a readable batch-progress line for the solver log
#'
#' @param completed_batches Number of batches collected so far.
#' @param total_batches Total batches in the current iteration.
#' @param iteration Current retry-wave iteration.
#' @param max_retries Maximum retry-wave count.
#' @param success_count Successful rows collected so far in this iteration.
#' @param failure_count Failed rows collected so far in this iteration.
#'
#' @return A single progress-line string.
#'
#' @keywords internal
.llm_solver_progress_line <- function(
  completed_batches,
  total_batches,
  iteration,
  max_retries,
  success_count = NULL,
  failure_count = NULL
) {
  # Convert the batch position into a fixed-width ASCII bar for tail-friendly
  # logs.
  width <- 30L
  ratio <- if (total_batches <= 0L) 1 else completed_batches / total_batches
  ratio <- min(max(ratio, 0), 1)
  filled <- as.integer(round(ratio * width))
  progress_bar <- paste0(
    "[",
    strrep("#", filled),
    strrep("-", width - filled),
    "]"
  )
  percent <- sprintf("%3d%%", as.integer(round(ratio * 100)))

  line <- paste0(
    "iteration ",
    iteration,
    "/",
    max_retries,
    " | batches ",
    completed_batches,
    "/",
    total_batches,
    " | ",
    progress_bar,
    " ",
    percent
  )

  if (!is.null(success_count)) {
    line <- paste0(line, " | successes=", success_count)
  }

  if (!is.null(failure_count)) {
    line <- paste0(line, " | failures=", failure_count)
  }

  line
}

#' Append a live collection-progress snapshot to the solver log
#'
#' @param log_context Log context from `.llm_solver_create_log_context()`.
#' @param iteration Current retry-wave iteration.
#' @param max_retries Maximum retry-wave count.
#' @param total_batches Total batches in the current iteration.
#'
#' @return Invisible logical indicating whether the append succeeded.
#'
#' @keywords internal
.llm_solver_append_collection_progress <- function(
  log_context,
  iteration,
  max_retries,
  total_batches
) {
  if (
    is.null(log_context) ||
      !is.list(log_context) ||
      is.null(log_context$path) ||
      !fs::file_exists(log_context$path)
  ) {
    return(invisible(FALSE))
  }

  # Count already-completed batches from the append-only log so the next
  # progress snapshot can advance while dispatch is still running.
  log_lines <- tryCatch(
    readLines(log_context$path, warn = FALSE),
    error = function(e) character()
  )
  finished_pattern <- paste0(
    "[event] batch finished | iteration=",
    iteration,
    " |"
  )
  failed_pattern <- paste0(
    "[event] batch failed | iteration=",
    iteration,
    " |"
  )
  completed_batches <- sum(grepl(finished_pattern, log_lines, fixed = TRUE)) +
    sum(grepl(failed_pattern, log_lines, fixed = TRUE))

  .llm_solver_append_log(
    log_context,
    "progress",
    list(
      line = .llm_solver_progress_line(
        completed_batches = completed_batches,
        total_batches = total_batches,
        iteration = iteration,
        max_retries = max_retries
      )
    )
  )
}

#' Create the append-only solver log context
#'
#' @param chat Resolved `Chat` object for the solver call.
#' @param type Structured type object or `NULL`.
#' @param dots Named list of forwarded solver arguments.
#' @param log_dir Logging directory passed to `llm_solver()`.
#'
#' @return A list with log path and solver identity fields.
#'
#' @keywords internal
.llm_solver_create_log_context <- function(chat, type, dots, log_dir) {
  # Reuse the stable cache-spec inputs so log-file identity stays tied to the
  # same provider/model/solver namespace as caching and retries.
  cache_spec <- .caching_spec(chat, type, dots)
  labels <- .caching_namespace_labels(cache_spec)
  namespace_hash <- .caching_namespace_hash(cache_spec)

  # Build one readable stem that every lifecycle filename can share.
  stem <- paste0(
    labels$provider,
    "_",
    labels$model,
    "_",
    namespace_hash
  )

  # Materialize the full running/done/failed/aborted path set once so later
  # helpers only need to pick a status rather than rebuild names.
  running_path <- fs::path(log_dir, paste0("running.", stem, ".log"))
  done_path <- fs::path(log_dir, paste0("done.", stem, ".log"))
  failed_path <- fs::path(log_dir, paste0("failed.", stem, ".log"))
  aborted_path <- fs::path(log_dir, paste0("aborted.", stem, ".log"))

  # Keep only one current log per solver namespace by clearing stale status
  # variants before the new run starts.
  unlink(c(running_path, done_path, failed_path, aborted_path), force = TRUE)

  # Return one compact context object so later helpers can reuse the same
  # resolved paths and identity labels.
  list(
    path = running_path,
    stem = stem,
    running_path = running_path,
    done_path = done_path,
    failed_path = failed_path,
    aborted_path = aborted_path,
    provider = labels$provider,
    model = labels$model,
    namespace_hash = namespace_hash
  )
}

#' Rename a running solver log to its final status path
#'
#' @param log_context Log context from `.llm_solver_create_log_context()`.
#' @param status Final log status. Supported values are `"done"`,
#'   `"failed"`, and `"aborted"`.
#'
#' @return Invisible logical indicating whether the rename succeeded.
#'
#' @keywords internal
.llm_solver_finalize_log <- function(
  log_context,
  status = c("done", "failed", "aborted")
) {
  # Normalize the requested terminal status before touching the filesystem.
  status <- rlang::arg_match(status)

  # Refuse to rename when the caller did not hand over a live log context.
  if (
    is.null(log_context) ||
      !is.list(log_context) ||
      is.null(log_context$path)
  ) {
    return(invisible(FALSE))
  }

  # Resolve the destination path directly from the terminal status.
  target_path <- switch(
    status,
    done = log_context$done_path,
    failed = log_context$failed_path,
    aborted = log_context$aborted_path
  )

  # Guard the full rename sequence so callers only need to handle a simple
  # success or failure flag.
  # Rename defensively: confirm the running file still exists, clear any stale
  # target file, then move the running file into its terminal slot.
  ok <- tryCatch(
    {
      # Another cleanup path may already have moved or removed the running log.
      if (!fs::file_exists(log_context$path)) {
        return(FALSE)
      }
      # Remove any stale terminal file before replacing it with the current run.
      if (fs::file_exists(target_path)) {
        file.remove(target_path)
      }
      # Promote the running log into its terminal status filename.
      file.rename(log_context$path, target_path)
    },
    error = function(e) FALSE
  )

  # Keep the in-memory context aligned with the file location after a
  # successful rename.
  if (isTRUE(ok)) {
    log_context$path <- target_path
  }

  invisible(ok)
}

#' Format one human-readable solver-log line
#'
#' @param timestamp Timestamp string for the current append.
#' @param log_context Log context from `.llm_solver_create_log_context()`.
#' @param event Event label for the emitted line.
#' @param fields Named list of event-specific fields.
#'
#' @return A single plain-text log line.
#'
#' @keywords internal
.llm_solver_format_log_line <- function(timestamp, log_context, event, fields) {
  # Render named fields consistently so the text log stays scan-friendly.
  .format_fields <- function(values) {
    # Drop missing fields so event-specific payloads can stay sparse.
    values <- Filter(Negate(is.null), values)
    if (length(values) == 0L) {
      return("")
    }

    # Normalize each field to one readable scalar before joining the final
    # `name=value` pieces.
    parts <- purrr::imap_chr(
      values,
      function(value, name) {
        scalar <- if (length(value) == 0L) {
          "[]"
        } else if (length(value) == 1L) {
          as.character(value[[1]])
        } else {
          paste(as.character(value), collapse = ",")
        }
        paste0(name, "=", scalar)
      }
    )

    # Use a fixed separator so the resulting lines stay grep-friendly.
    paste(parts, collapse = " | ")
  }

  # Keep free-form message lines compact while preserving their severity.
  if (identical(event, "message")) {
    level <- if (!is.null(fields$level)) fields$level else "info"
    message <- if (!is.null(fields$message)) fields$message else ""
    return(paste0(timestamp, " [", level, "] ", message))
  }

  # Keep progress lines readable so the file can be tailed during execution.
  if (identical(event, "progress")) {
    line <- if (!is.null(fields$line)) fields$line else ""
    return(paste0(timestamp, " [progress] ", line))
  }

  # Render the remaining lifecycle records as labeled text rather than JSON.
  label <- gsub("_", " ", event, fixed = TRUE)

  # Attach the stable solver identity only to the first lifecycle line so later
  # entries stay compact.
  base_fields <- switch(
    event,
    call_started = list(
      provider = log_context$provider,
      model = log_context$model,
      namespace_hash = log_context$namespace_hash
    ),
    list()
  )

  # Combine any event-specific fields with the optional base identity fields.
  field_text <- .format_fields(c(base_fields, fields))

  # Emit either a bare event label or a labeled payload, depending on whether
  # the current event carries extra fields.
  if (nzchar(field_text)) {
    paste0(timestamp, " [event] ", label, " | ", field_text)
  } else {
    paste0(timestamp, " [event] ", label)
  }
}

#' Append one readable line to the solver log
#'
#' @param log_context Log context from `.llm_solver_create_log_context()`.
#' @param event Event label for the emitted record.
#' @param fields Named list of event-specific fields.
#'
#' @return Invisible logical indicating whether the append succeeded.
#'
#' @keywords internal
.llm_solver_append_log <- function(log_context, event, fields = list()) {
  # Skip silently when solver logging is disabled or the context was never
  # initialized for this call.
  if (
    is.null(log_context) ||
      !is.list(log_context) ||
      is.null(log_context$path)
  ) {
    return(invisible(FALSE))
  }

  # Format the event as a plain-text line so users can tail the file directly.
  timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")
  line <- .llm_solver_format_log_line(
    timestamp = timestamp,
    log_context = log_context,
    event = event,
    fields = fields
  )
  # Refuse malformed lines so the log stays one record per physical line.
  if (!rlang::is_string(line) || !nzchar(line)) {
    return(invisible(FALSE))
  }

  # Serialize appends through a lightweight lock directory so concurrent
  # workers cannot interleave partial writes into one log file.
  ok <- tryCatch(
    {
      # Recreate the parent directory defensively in case another process
      # removed it after log initialization.
      fs::dir_create(fs::path_dir(log_context$path))
      lock_dir <- paste0(log_context$path, ".lock")
      lock_acquired <- FALSE
      # Poll briefly for the per-log lock rather than blocking forever.
      for (attempt in seq_len(500L)) {
        if (dir.create(lock_dir, showWarnings = FALSE, recursive = FALSE)) {
          lock_acquired <- TRUE
          break
        }
        Sys.sleep(0.01)
      }
      # Give up cleanly if another writer holds the lock for too long.
      if (!lock_acquired) {
        return(FALSE)
      }
      on.exit(
        {
          # Drop the lock directory on every exit path so future append attempts
          # do not inherit a stale file lock.
          unlink(lock_dir, recursive = TRUE, force = TRUE)
        },
        add = TRUE
      )

      # Append one full line once the lock is held.
      cat(
        as.character(line),
        "\n",
        file = log_context$path,
        append = TRUE,
        sep = ""
      )
      TRUE
    },
    error = function(e) FALSE
  )

  invisible(ok)
}

#' Split solver inputs into batches
#'
#' @param inputs Character vector of rendered prompts.
#' @param batch_size Batch size or `NULL`.
#'
#' @return A list of character vectors.
#'
#' @keywords internal
.split_solver_batches <- function(inputs, batch_size) {
  # A NULL batch size means "run everything in one batch".
  if (rlang::is_null(batch_size) || length(inputs) <= batch_size) {
    return(list(inputs))
  }

  # Split the prompt vector into stable execution chunks.
  split(inputs, ceiling(seq_along(inputs) / batch_size))
}

#' Run unstructured solver requests
#'
#' This helper adapts the repo-local `llm_solver()` entry point to
#' `ellmer::parallel_chat()` while preserving the `vitals` return shape.
#'
#' @param chat A resolved `ellmer` chat object.
#' @param prompts A list of rendered prompts aligned to the dataset rows.
#' @param ... Named solver arguments collected from `...`.
#' @param type Ignored in unstructured mode so the helper matches the structured
#'   dispatch interface.
#' @param log_context Optional log context from
#'   `.llm_solver_create_log_context()`.
#' @param emit_message Optional message helper. Defaults to
#'   `.llm_solver_emit_message()`.
#'
#' @return A list with `result`, `solver_chat`, and `solver_metadata = NULL`.
#'
#' @keywords internal
.llm_solver_parallel_unstructured <- function(
  chat,
  prompts,
  ...,
  type = NULL,
  log_context = NULL,
  direct_fallback = NULL,
  failure_chat = NULL,
  last_text = NULL,
  json_string = NULL,
  emit_message = NULL
) {
  # Allow direct internal calls to omit the fallback. The orchestration layer
  # passes one explicitly, but standalone helper calls do not need to.
  if (is.null(direct_fallback)) {
    direct_fallback <- .llm_solver_direct_fallback_unstructured
  }

  # Default helper dependencies locally so direct calls and worker calls share
  # one execution path without extra branching below.
  if (is.null(failure_chat)) {
    failure_chat <- .llm_solver_failure_chat
  }
  if (is.null(last_text)) {
    last_text <- .llm_solver_last_text
  }
  if (is.null(emit_message)) {
    emit_message <- .llm_solver_emit_message
  }

  # Force row-level failures to stay inside the batch response.
  dots <- rlang::list2(...)
  dots$on_error <- "continue"

  # Extract the final assistant turn text locally so the worker only needs
  # explicit arguments and namespaced dependencies.
  .last_text <- function(row_chat) {
    # Reject missing chat objects before attempting turn extraction.
    if (is.null(row_chat)) {
      return(NA_character_)
    }

    # Retrieve the final turn from the chat history while handling empty
    # histories.
    last_turn <- tryCatch(row_chat$last_turn(), error = function(e) NULL)
    if (is.null(last_turn)) {
      return(NA_character_)
    }

    # Safe-access the text payload from the last turn object.
    txt <- tryCatch(last_turn@text, error = function(e) NA_character_)
    if (!is.character(txt) || length(txt) == 0L) {
      return(NA_character_)
    }

    # Validate that the resulting text is a non-empty string.
    txt <- txt[[1]]
    if (!is.character(txt) || length(txt) != 1L || !nzchar(trimws(txt))) {
      return(NA_character_)
    }

    txt
  }

  # Attempt the batched parallel call; on catastrophic failure fall back to
  # direct per-row chat calls so a single bad batch does not poison all rows.
  chats <- tryCatch(
    rlang::exec(
      ellmer::parallel_chat,
      chat,
      prompts,
      !!!dots
    ),
    error = function(e) e
  )

  if (inherits(chats, "error")) {
    emit_message(
      "warning",
      "Batch failed, retrying row by row",
      log_context
    )
    return(direct_fallback(
      chat,
      prompts,
      failure_chat = failure_chat,
      last_text = last_text,
      log_context = log_context,
      emit_message = emit_message
    ))
  }

  # Extract the final assistant turn text for each completed chat.
  result <- purrr::map_chr(chats, .last_text)

  # Return the normalized solver output shape expected by vitals.
  list(
    result = result,
    solver_chat = chats,
    solver_metadata = NULL,
    used_direct_fallback = FALSE
  )
}

#' Run structured solver requests with synthetic logging chats
#'
#' This helper adapts the upstream `vitals` structured-solver pattern for the
#' repo-local `llm_solver()` entry point so later cache and retry layers can
#' stay local to the package.
#'
#' @param chat A resolved `ellmer` chat object.
#' @param prompts A list of rendered prompts aligned to the dataset rows.
#' @param type An ellmer structured type object.
#' @param ... Named solver arguments collected from `...`.
#' @param log_context Optional log context from
#'   `.llm_solver_create_log_context()`.
#' @param emit_message Optional message helper. Defaults to
#'   `.llm_solver_emit_message()`.
#'
#' @return A list with `result`, `solver_chat`, and `solver_metadata`.
#'
#' @details Portions of this helper are adapted from `tidyverse/vitals`
#'   `R/solver.R` on `main` at commit `6a464c7`, from an implementation authored
#'   by Simon Couch (<https://github.com/simonpcouch>).
#'
#' @keywords internal
.llm_solver_parallel_structured <- function(
  chat,
  prompts,
  ...,
  type = NULL,
  log_context = NULL,
  direct_fallback = NULL,
  failure_chat = NULL,
  last_text = NULL,
  json_string = NULL,
  emit_message = NULL
) {
  # Structured dispatch requires an ellmer type object.
  if (is.null(type)) {
    cli::cli_abort(
      "{.arg type} must be supplied for structured solver requests."
    )
  }

  # Allow direct internal calls to omit the fallback. The orchestration layer
  # passes one explicitly, but standalone helper calls do not need to.
  if (is.null(direct_fallback)) {
    direct_fallback <- .llm_solver_direct_fallback_structured
  }

  # Default helper dependencies locally so row serialization and fallback stay
  # available in both direct and mirai-backed execution.
  if (is.null(failure_chat)) {
    failure_chat <- .llm_solver_failure_chat
  }
  if (is.null(json_string)) {
    json_string <- .llm_solver_json_string
  }
  if (is.null(emit_message)) {
    emit_message <- .llm_solver_emit_message
  }

  # Force row-level failures to stay inside the batch response.
  dots <- rlang::list2(...)
  dots$on_error <- "continue"

  # Extract row payload values locally so the worker only needs explicit
  # arguments and namespaced dependencies.
  .row_turn_value <- function(result, candidates) {
    # Return a missing value when the row payload is absent.
    if (is.null(result)) {
      return(NA_real_)
    }

    # Search the candidate fields in order so providers can expose equivalent
    # values under different names.
    for (candidate in candidates) {
      value <- NULL
      if (inherits(result, "data.frame") && candidate %in% names(result)) {
        value <- result[[candidate]]
      } else if (
        is.list(result) &&
          !is.null(names(result)) &&
          candidate %in% names(result)
      ) {
        value <- result[[candidate]]
      }

      # Normalize the first usable field value to a numeric scalar.
      if (!is.null(value)) {
        value <- suppressWarnings(as.numeric(value))
        if (length(value) == 0L || all(is.na(value))) {
          return(NA_real_)
        }
        return(value[[1]])
      }
    }

    NA_real_
  }

  # Rebuild one row-level logging chat so the structured path remains
  # self-contained inside the worker process.
  .build_row_chat <- function(input, result, base_chat) {
    # Clone the base chat so each row keeps an independent log object.
    chat <- base_chat$clone()

    # Rebuild the user turn so the synthetic chat mirrors the original prompt.
    user_turn <- ellmer::UserTurn(
      contents = list(ellmer::ContentText(as.character(input)))
    )

    # Build a failure assistant turn when the provider returned a row error.
    if (inherits(result, "condition")) {
      assistant_turn <- ellmer::AssistantTurn(
        contents = list(ellmer::ContentText(
          paste0("FAILURE: ", conditionMessage(result))
        )),
        tokens = c(0, 0, 0)
      )
    } else {
      # Build a JSON assistant turn for successful structured rows.
      assistant_turn <- ellmer::AssistantTurn(
        contents = list(ellmer::ContentText(
          json_string(result)
        )),
        json = list(result),
        tokens = c(
          input = .row_turn_value(result, c("input_tokens", "input")),
          output = .row_turn_value(result, c("output_tokens", "output")),
          cached_input = .row_turn_value(
            result,
            c("cached_input_tokens", "cached_input")
          )
        ),
        cost = .row_turn_value(result, c("cost")),
        duration = .row_turn_value(result, c("duration"))
      )
    }

    # Store the synthetic turns so downstream logging sees a standard chat.
    chat$set_turns(list(user_turn, assistant_turn))
    chat
  }

  # Normalize the batch response into one structured row per prompt. Only
  # clearly row-aligned shapes are allowed to proceed to logging and
  # serialization; ambiguous top-level structures must fall back to direct
  # row-by-row requests instead of being guessed.
  .normalize_batch_result <- function(result, prompts) {
    # Split data-frame results row-wise only when the row count matches the
    # prompt count exactly.
    if (inherits(result, "data.frame")) {
      if (nrow(result) != length(prompts)) {
        return(NULL)
      }

      return(purrr::map(
        seq_len(nrow(result)),
        function(i) result[i, , drop = FALSE]
      ))
    }

    if (is.list(result)) {
      result_names <- names(result)

      # Treat named top-level lists in multi-prompt batches as ambiguous
      # single objects, not as row-aligned collections.
      if (
        length(prompts) > 1L &&
          !rlang::is_empty(result_names) &&
          any(nzchar(result_names))
      ) {
        return(NULL)
      }

      # Preserve named single-prompt objects as one structured row instead of
      # flattening them into a one-element row list.
      if (
        length(prompts) == 1L &&
          !rlang::is_empty(result_names) &&
          any(nzchar(result_names))
      ) {
        return(list(result))
      }

      # Keep only unnamed top-level lists whose element count matches the
      # prompt count. This is the unambiguous "one element per prompt" shape.
      if (length(result) == length(prompts)) {
        return(result)
      }
    }

    # Accept scalar structured payloads only when the batch contains exactly
    # one prompt.
    if (length(prompts) == 1L) {
      return(list(result))
    }

    NULL
  }

  # Submit one structured request wave through ellmer's public interface.
  # If the provider aborts the whole batch, fall back to one-row requests so a
  # single bad row does not poison the rest of the batch.
  res <- tryCatch(
    rlang::exec(
      ellmer::parallel_chat_structured,
      chat = chat,
      prompts = prompts,
      type = type,
      !!!dots
    ),
    error = function(e) e
  )

  if (inherits(res, "error")) {
    batch_failure_reason <- conditionMessage(res)
    batch_failure_body <- if (inherits(res, "condition")) {
      .llm_solver_condition_error_body(res)
    } else {
      NA_character_
    }

    emit_message(
      "warning",
      "Batch failed, retrying row by row",
      log_context
    )
    .llm_solver_append_log(
      log_context,
      "batch_failed",
      list(
        reason = .llm_solver_compact_text(batch_failure_reason),
        status_code = .llm_solver_http_status_code(batch_failure_reason),
        provider_body = .llm_solver_compact_text(batch_failure_body)
      )
    )
    return(direct_fallback(
      chat,
      prompts,
      type,
      dots,
      failure_chat = failure_chat,
      json_string = json_string,
      log_context = log_context,
      emit_message = emit_message
    ))
  }

  # Validate the top-level shape before proceeding. Named lists with the same
  # field count as the batch are ambiguous in structured mode, so only clearly
  # row-aligned collections are accepted here.
  res_per_input <- .normalize_batch_result(res, prompts)

  # Fall back immediately when the batch payload cannot be matched one-to-one
  # to the prompt vector without guessing.
  if (is.null(res_per_input)) {
    emit_message(
      "warning",
      paste0(
        "Structured batch result did not align to the input count; ",
        "retrying row by row"
      ),
      log_context
    )
    return(direct_fallback(
      chat,
      prompts,
      type,
      dots,
      failure_chat = failure_chat,
      json_string = json_string,
      log_context = log_context,
      emit_message = emit_message
    ))
  }

  # Mark row-level transport failures explicitly so a single errored request
  # does not crash the whole structured batch.
  row_failed <- purrr::map_lgl(res_per_input, inherits, what = "condition")

  # Build synthetic Chat objects for each response row. Failed rows get a
  # failure chat instead of a JSON serialization attempt.
  solver_chat <- purrr::map2(
    prompts,
    res_per_input,
    function(prompt, result) {
      .build_row_chat(prompt, result, chat)
    }
  )

  # Serialize the structured result rows into character strings for
  # compatibility with downstream parsers and scorers that expect a result
  # column. Transport failures stay as explicit `NA` rows.
  result_strings <- purrr::map_chr(
    seq_along(res_per_input),
    function(i) {
      # Keep row-level transport failures explicit in the serialized result.
      if (row_failed[[i]]) {
        return(NA_character_)
      }

      # Serialize successful structured rows into the standard character shape.
      json_string(res_per_input[[i]])
    }
  )

  # Return the serialized results alongside the internal logging chats and
  # original structured metadata.
  list(
    result = result_strings,
    solver_chat = solver_chat,
    solver_metadata = res_per_input,
    used_direct_fallback = FALSE
  )
}

#' Convert a structured solver payload to JSON
#'
#' @param x A structured solver payload.
#'
#' @return A JSON string.
#'
#' @keywords internal
.llm_solver_json_string <- function(x) {
  sanitize <- function(value) {
    if (inherits(value, "condition")) {
      return(conditionMessage(value))
    }

    if (is.data.frame(value)) {
      value[] <- lapply(value, sanitize)
      return(value)
    }

    if (is.list(value)) {
      value <- lapply(value, sanitize)
      return(value)
    }

    value
  }

  as.character(
    jsonlite::toJSON(
      sanitize(x),
      auto_unbox = TRUE
    )
  )
}

#' Extract execution telemetry from a chat
#'
#' @param chat A resolved `ellmer` chat object.
#'
#' @return A named list of execution telemetry fields.
#' @keywords internal
.llm_solver_turn_metrics <- function(chat) {
  # Default to missing values so the caller can distinguish "not available"
  # from zero-valued usage.
  metrics <- list(
    input_tokens = NA_real_,
    output_tokens = NA_real_,
    cached_input_tokens = NA_real_,
    reasoning_tokens = NA_real_,
    cost = NA_real_,
    elapsed_seconds = NA_real_
  )

  if (is.null(chat)) {
    return(metrics)
  }

  # Use the public token table when ellmer exposes it.
  tokens <- tryCatch(chat$get_tokens(), error = function(e) NULL)
  if (is.data.frame(tokens) && nrow(tokens) > 0L) {
    metrics$input_tokens <- .llm_solver_sum_numeric_column(
      tokens,
      c("input_tokens", "input")
    )
    metrics$output_tokens <- .llm_solver_sum_numeric_column(
      tokens,
      c("output_tokens", "output")
    )
    metrics$cached_input_tokens <- .llm_solver_sum_numeric_column(
      tokens,
      c("cached_input_tokens", "cached_input")
    )
    metrics$cost <- .llm_solver_sum_numeric_column(tokens, c("cost"))
  }

  # Fall back to the assistant turn for duration and provider-specific JSON.
  last_turn <- tryCatch(chat$last_turn(), error = function(e) NULL)
  if (!is.null(last_turn)) {
    metrics$elapsed_seconds <- tryCatch(
      as.numeric(last_turn@duration),
      error = function(e) NA_real_
    )

    turn_cost <- tryCatch(
      as.numeric(last_turn@cost),
      error = function(e) NA_real_
    )
    if (!is.na(turn_cost)) {
      metrics$cost <- turn_cost
    }

    metrics$reasoning_tokens <- .llm_solver_reasoning_tokens_from_turn(
      last_turn
    )
  }

  metrics
}

#' Sum a numeric column if present
#'
#' @param data A data frame-like object.
#' @param candidates Candidate column names in priority order.
#'
#' @return A numeric scalar or `NA_real_`.
#' @keywords internal
.llm_solver_sum_numeric_column <- function(data, candidates) {
  for (candidate in candidates) {
    if (candidate %in% names(data)) {
      value <- suppressWarnings(as.numeric(data[[candidate]]))
      if (length(value) == 0L || all(is.na(value))) {
        return(NA_real_)
      }
      return(sum(value, na.rm = TRUE))
    }
  }
  NA_real_
}

#' Extract reasoning tokens from an assistant turn
#'
#' @param turn An `ellmer` assistant turn.
#'
#' @return A numeric scalar or `NA_real_`.
#' @keywords internal
.llm_solver_reasoning_tokens_from_turn <- function(turn) {
  # The JSON payload is provider-specific, so we walk the common path and fall
  # back to `NA` when the provider omits reasoning usage details.
  turn_json <- tryCatch(turn@json, error = function(e) NULL)
  if (is.null(turn_json) || !is.list(turn_json)) {
    return(NA_real_)
  }

  usage <- turn_json$usage
  if (is.null(usage) || !is.list(usage)) {
    return(NA_real_)
  }

  output_details <- usage$output_tokens_details
  if (is.null(output_details) || !is.list(output_details)) {
    return(NA_real_)
  }

  reasoning_tokens <- output_details$reasoning_tokens
  if (is.null(reasoning_tokens)) {
    return(NA_real_)
  }

  reasoning_tokens <- suppressWarnings(as.numeric(reasoning_tokens))
  if (length(reasoning_tokens) == 0L || all(is.na(reasoning_tokens))) {
    return(NA_real_)
  }

  reasoning_tokens[[1]]
}

#' Build a telemetry envelope for one solver row
#'
#' @param payload_kind Payload label, usually `"text"` or `"structured"`.
#' @param payload Raw solver payload for the row.
#' @param chat The row-level `Chat` object used for telemetry extraction.
#' @param status Final row status label.
#' @param attempt_count Number of solver-visible attempts for this row.
#' @param cache_hit Whether the row was served from any runtime cache layer.
#' @param restored_from_cache Whether the row was recovered from persistent
#'   stable cache state during initialization or recovery.
#' @param used_direct_fallback Whether the row required direct fallback.
#' @param failure_reason Normalized failure-history vector.
#' @param failure_class Failure class label or `NA_character_`.
#' @param technical_scope Technical-error scope label or `NA_character_`.
#'
#' @return A list with `payload_kind`, `payload`, and `execution`.
#' @keywords internal
.llm_solver_telemetry_envelope <- function(
  payload_kind,
  payload,
  chat,
  status,
  attempt_count,
  cache_hit,
  restored_from_cache,
  used_direct_fallback,
  failure_reason,
  failure_class = NA_character_,
  technical_scope = NA_character_
) {
  metrics <- .llm_solver_turn_metrics(chat)

  list(
    payload_kind = payload_kind,
    payload = payload,
    execution = list(
      status = status,
      attempt_count = as.integer(attempt_count),
      cache_hit = isTRUE(cache_hit),
      restored_from_cache = isTRUE(restored_from_cache),
      used_direct_fallback = isTRUE(used_direct_fallback),
      elapsed_seconds = metrics$elapsed_seconds,
      input_tokens = metrics$input_tokens,
      output_tokens = metrics$output_tokens,
      cached_input_tokens = metrics$cached_input_tokens,
      reasoning_tokens = metrics$reasoning_tokens,
      cost = metrics$cost,
      failure_reason = failure_reason,
      failure_class = if (
        rlang::is_string(failure_class) && nzchar(failure_class)
      ) {
        failure_class
      } else {
        NA_character_
      },
      technical_scope = if (
        rlang::is_string(technical_scope) && nzchar(technical_scope)
      ) {
        technical_scope
      } else {
        NA_character_
      }
    )
  )
}

#' Mark an envelope as restored from cache
#'
#' @param envelope A telemetry envelope.
#'
#' @return The same envelope with cache-hit flags updated.
#' @keywords internal
.llm_solver_mark_cached_envelope <- function(envelope) {
  if (
    is.null(envelope) ||
      !is.list(envelope) ||
      is.null(envelope$execution) ||
      !is.list(envelope$execution)
  ) {
    return(envelope)
  }

  envelope$execution$cache_hit <- TRUE
  envelope$execution$restored_from_cache <- TRUE
  envelope
}

#' Normalize a failure reason from solver-visible artifacts
#'
#' @param row_metadata Optional row metadata or condition object.
#' @param chat Optional row-level chat object.
#' @param default_reason Fallback failure reason.
#'
#' @return A single failure reason string.
#' @keywords internal
.llm_solver_failure_reason <- function(
  row_metadata = NULL,
  chat = NULL,
  default_reason = "The solver returned no usable result"
) {
  # Prefer direct R condition messages when the provider client failed before
  # returning any row-level payload.
  if (inherits(row_metadata, "condition")) {
    return(conditionMessage(row_metadata))
  }

  if (inherits(row_metadata, "error")) {
    return(conditionMessage(row_metadata))
  }

  if (inherits(row_metadata, "try-error")) {
    return(as.character(row_metadata)[[1]])
  }

  # Next, prefer explicit provider `.error` payloads when the batch returned a
  # row-level transport failure marker instead of throwing.
  payload_error <- .llm_solver_payload_error_message(row_metadata)
  if (rlang::is_string(payload_error) && nzchar(payload_error)) {
    return(payload_error)
  }

  # Finally, recover the embedded message from synthetic failure chats used by
  # direct fallback paths.
  if (!is.null(chat)) {
    failure_message <- .llm_solver_failure_chat_message(chat)
    if (rlang::is_string(failure_message) && nzchar(failure_message)) {
      return(failure_message)
    }
  }

  default_reason
}

#' Extract a provider error string from a row payload
#'
#' @param row_metadata Raw row payload or row-level condition metadata.
#'
#' @return A character scalar or `NA_character_`.
#'
#' @keywords internal
.llm_solver_payload_error_message <- function(row_metadata) {
  # Start from a missing error field and populate it only for supported
  # row-metadata shapes that expose a `.error` entry.
  error_field <- NULL

  # Read provider error text from structured data-frame rows returned by
  # ellmer when one request in the batch fails in transport.
  if (
    inherits(row_metadata, "data.frame") && ".error" %in% names(row_metadata)
  ) {
    error_field <- row_metadata[[".error"]]
  } else if (
    is.list(row_metadata) &&
      !is.null(names(row_metadata)) &&
      ".error" %in% names(row_metadata)
  ) {
    # Read provider error text from list-like row payloads produced by some
    # fallback and cache-restored structured paths.
    error_field <- row_metadata[[".error"]]
  }

  # Return missing when the row payload carries no provider error field.
  if (is.null(error_field) || length(error_field) == 0L) {
    return(NA_character_)
  }

  # Collapse the error field to a single scalar so downstream classification
  # can work with one normalized message per row.
  if (is.list(error_field)) {
    error_field <- error_field[[1]]
  } else {
    error_field <- error_field[[1]]
  }

  # Preserve condition objects returned inside row-level `.error` payloads so
  # provider capability errors do not collapse to a generic no-response reason.
  if (inherits(error_field, "condition")) {
    return(.llm_solver_condition_error_message(error_field))
  }

  # Reject empty or malformed values so callers can fall back to other error
  # sources such as condition messages or synthetic failure chats.
  if (!rlang::is_string(error_field) || !nzchar(error_field)) {
    return(NA_character_)
  }

  # Return the provider-visible error text exactly as supplied for later
  # failure-history recording and technical-error classification.
  error_field
}

#' Extract detailed text from a condition object
#'
#' @param error_condition A condition object from a provider or transport layer.
#'
#' @return A character scalar or `NA_character_`.
#' @keywords internal
.llm_solver_condition_error_message <- function(error_condition) {
  # Start with the regular condition message because every condition should
  # expose it, even when no HTTP response body is retained.
  message <- conditionMessage(error_condition)

  # Append the raw response body when httr2 preserved provider metadata on the
  # condition object. This catches model-capability details hidden below a
  # generic HTTP status message.
  response_body <- .llm_solver_condition_error_body(error_condition)
  if (rlang::is_string(response_body) && nzchar(response_body)) {
    message <- paste(message, response_body, sep = "\n")
  }

  # Return missing only when neither source produced usable diagnostic text.
  if (!rlang::is_string(message) || !nzchar(message)) {
    return(NA_character_)
  }

  message
}

#' Extract the raw provider body from a condition object
#'
#' @param error_condition A condition object from a provider or transport layer.
#'
#' @return A character scalar or `NA_character_`.
#' @keywords internal
.llm_solver_condition_error_body <- function(error_condition) {
  # Pull the raw provider body out of httr2 conditions when it exists.
  response_body <- tryCatch(
    rawToChar(error_condition$resp$body),
    error = function(e) NA_character_
  )

  if (!rlang::is_string(response_body) || !nzchar(response_body)) {
    return(NA_character_)
  }

  response_body
}

#' Collapse provider text onto one log-friendly line
#'
#' @param x Character scalar from an error or provider body.
#'
#' @return A single-line character scalar or `NA_character_`.
#' @keywords internal
.llm_solver_compact_text <- function(x) {
  # Leave non-character inputs alone so callers can decide how to coerce them.
  if (!is.character(x)) {
    return(x)
  }

  # Flatten all whitespace to spaces because solver log records are one
  # physical line each.
  x <- trimws(x)
  missing_idx <- is.na(x) | !nzchar(x)
  x[missing_idx] <- NA_character_
  gsub("[[:space:]]+", " ", x)
}

#' Extract the failure message embedded in a synthetic failure chat
#'
#' @param chat A row-level chat object.
#'
#' @return A character scalar or `NA_character_`.
#'
#' @keywords internal
.llm_solver_failure_chat_message <- function(chat) {
  # Return missing when the chat does not expose a usable last turn.
  last_turn <- tryCatch(chat$last_turn(), error = function(e) NULL)
  if (is.null(last_turn)) {
    return(NA_character_)
  }

  # Accept only the synthetic failure-chat shape so normal assistant messages
  # are never mistaken for transport errors.
  last_text <- tryCatch(
    last_turn@text[[1]],
    error = function(e) NA_character_
  )
  if (
    !is.character(last_text) ||
      length(last_text) != 1L ||
      !nzchar(last_text) ||
      !grepl("^FAILURE:", last_text)
  ) {
    return(NA_character_)
  }

  sub("^FAILURE:\\s*", "", last_text)
}

#' Validate the retry-condition contract
#'
#' @param retry_on Character vector of retryable failure conditions.
#'
#' @return A unique character vector of validated retry conditions.
#' @keywords internal
.llm_solver_validate_retry_on <- function(retry_on) {
  # Keep the supported retry conditions explicit so solver behaviour stays
  # predictable across task pipelines.
  allowed <- c("no_response", "not_parsable", "empty_response")

  # Treat NULL as an explicit "do not retry" request.
  if (is.null(retry_on)) {
    return(character())
  }

  # Reject malformed retry vectors before any network work starts.
  if (!rlang::is_character(retry_on) || anyNA(retry_on)) {
    cli::cli_abort(
      "{.arg retry_on} must be a character vector of retry conditions."
    )
  }

  # Keep the interface narrow so future condition names are added
  # deliberately.
  invalid <- setdiff(unique(retry_on), allowed)
  if (length(invalid) > 0L) {
    cli::cli_abort(
      c(
        "{.arg retry_on} contains unsupported values.",
        "x" = paste(invalid, collapse = ", "),
        "i" = paste("Supported values:", paste(allowed, collapse = ", "))
      )
    )
  }

  unique(retry_on)
}

#' Classify one row-level solver failure
#'
#' @param result Serialized row result from the selected helper.
#' @param row_metadata Raw row payload or row-level condition metadata.
#' @param chat Row-level chat returned by the helper.
#' @param type Structured type object or `NULL`.
#'
#' @return A list with `condition` and `reason` fields.
#' @keywords internal
.llm_solver_classify_failure <- function(
  result,
  row_metadata = NULL,
  chat = NULL,
  type = NULL
) {
  # Unstructured rows need only distinguish empty text from transport failure.
  if (is.null(type)) {
    return(.llm_solver_classify_failure_unstructured(
      result,
      row_metadata = row_metadata,
      chat = chat
    ))
  }

  # Structured rows need a richer path because row metadata may carry parsed
  # payloads, `.error` transport markers, or direct-fallback conditions.
  # Structured rows also need host-side parsing and emptiness checks.
  .llm_solver_classify_failure_structured(row_metadata, chat, type)
}

#' Classify one unstructured solver row
#'
#' @param result Serialized row result from the selected helper.
#' @param chat Row-level chat returned by the helper.
#'
#' @return A list with `condition`, `reason`, `failure_class`, and
#'   `technical_scope` fields.
#' @keywords internal
.llm_solver_classify_failure_unstructured <- function(
  result,
  row_metadata = NULL,
  chat = NULL
) {
  # Successful unstructured rows always carry a non-empty text result.
  if (!.llm_solver_is_empty_text(result) && !is.null(chat)) {
    return(list(
      condition = NULL,
      reason = NA_character_,
      failure_class = NA_character_,
      technical_scope = NA_character_
    ))
  }

  # Distinguish genuine empty text from synthetic failure chats.
  if (!is.null(chat) && !.llm_solver_chat_has_failure_marker(chat)) {
    return(list(
      condition = "empty_response",
      reason = "The solver returned an empty response.",
      failure_class = "llm_failure",
      technical_scope = NA_character_
    ))
  }

  # Everything else is a transport-side no-response failure.
  failure_reason <- .llm_solver_failure_reason(
    row_metadata = row_metadata,
    chat = chat,
    default_reason = "The solver returned no usable result"
  )
  # Upgrade only a narrow set of high-confidence provider/request errors to
  # the technical bucket; everything else stays in the LLM-failure path.
  technical <- .llm_solver_classify_technical_failure(failure_reason)

  list(
    condition = "no_response",
    reason = failure_reason,
    failure_class = technical$failure_class,
    technical_scope = technical$technical_scope
  )
}

#' Classify one structured solver row
#'
#' @param row_metadata Raw row payload or row-level condition metadata.
#' @param chat Row-level chat returned by the helper.
#' @param type Ellmer structured type object.
#'
#' @return A list with `condition` and `reason` fields.
#' @keywords internal
.llm_solver_classify_failure_structured <- function(
  row_metadata,
  chat,
  type
) {
  # Preserve explicit parse errors from direct structured calls.
  if (
    inherits(row_metadata, "condition") ||
      inherits(row_metadata, "error") ||
      inherits(row_metadata, "try-error")
  ) {
    # Normalize the direct error first, then decide whether it behaves like a
    # parsing/schema problem or a broader provider/transport failure.
    failure_reason <- .llm_solver_failure_reason(
      row_metadata = row_metadata,
      chat = chat,
      default_reason = if (
        .llm_solver_is_parse_error_message(
          .llm_solver_failure_reason(row_metadata = row_metadata)
        )
      ) {
        "The structured response could not be parsed into the requested schema."
      } else {
        "The solver returned no usable result"
      }
    )
    condition <- if (
      .llm_solver_is_parse_error_message(
        failure_reason
      )
    ) {
      "not_parsable"
    } else {
      "no_response"
    }
    technical <- .llm_solver_classify_technical_failure(failure_reason)

    return(list(
      condition = condition,
      reason = failure_reason,
      failure_class = technical$failure_class,
      technical_scope = technical$technical_scope
    ))
  }

  # Treat ellmer transport markers and synthetic failure chats as no-response.
  if (
    .llm_solver_structured_row_has_transport_error(row_metadata) ||
      is.null(chat) ||
      .llm_solver_chat_has_failure_marker(chat)
  ) {
    # This branch preserves provider `.error` text and applies the same
    # conservative technical classifier used by unstructured failures.
    failure_reason <- .llm_solver_failure_reason(
      row_metadata = row_metadata,
      chat = chat,
      default_reason = "The solver returned no usable result"
    )
    technical <- .llm_solver_classify_technical_failure(failure_reason)

    return(list(
      condition = "no_response",
      reason = failure_reason,
      failure_class = technical$failure_class,
      technical_scope = technical$technical_scope
    ))
  }

  # Walk the required schema fields after conversion so empty strings, NULLs,
  # and NA placeholders can trigger retries without task-specific code.
  required_fields <- .llm_solver_required_field_paths(type)
  empty_fields <- .llm_solver_empty_required_fields(row_metadata, type)

  if (length(empty_fields) == 0L) {
    return(list(
      condition = NULL,
      reason = NA_character_,
      failure_class = NA_character_,
      technical_scope = NA_character_
    ))
  }

  # When every required field is empty, treat the row as structurally
  # unusable instead of merely incomplete.
  if (
    length(required_fields) > 0L &&
      setequal(empty_fields, required_fields)
  ) {
    return(list(
      condition = "not_parsable",
      reason = "The structured response could not be parsed into the requested schema.",
      failure_class = "llm_failure",
      technical_scope = NA_character_
    ))
  }

  # Partial emptiness means the row parsed but the payload is incomplete.
  list(
    condition = "empty_response",
    reason = paste0(
      "The structured response had empty required field(s): ",
      paste(empty_fields, collapse = ", ")
    ),
    failure_class = "llm_failure",
    technical_scope = NA_character_
  )
}

#' Classify a failure message as LLM-like or technical
#'
#' @param message Failure message extracted from the provider or fallback layer.
#'
#' @return A list with `failure_class` and `technical_scope`.
#'
#' @keywords internal
.llm_solver_classify_technical_failure <- function(message) {
  # Default unknown messages to the ordinary LLM-failure bucket so the solver
  # does not over-abort on weak or ambiguous evidence.
  if (!rlang::is_string(message) || !nzchar(message)) {
    return(list(
      failure_class = "llm_failure",
      technical_scope = NA_character_
    ))
  }

  # Normalize once and extract any visible HTTP-like status code so later
  # checks can combine numeric signals with textual clues.
  message_lower <- tolower(message)
  status_code <- .llm_solver_http_status_code(message)

  # Treat request-construction and provider-configuration faults as global
  # technical errors because retrying more rows is wasted work.
  if (!is.na(status_code) && status_code %in% c(401L, 403L, 404L)) {
    return(list(
      failure_class = "technical_error",
      technical_scope = "global"
    ))
  }

  if (!is.na(status_code) && status_code %in% c(400L, 422L)) {
    # Restrict 400/422 escalation to request-shape/configuration signals so
    # output-quality failures are not mistaken for deterministic bugs.
    if (
      grepl(
        paste(
          c(
            "invalid api key",
            "unauthoriz",
            "forbidden",
            "unsupported",
            "unknown parameter",
            "invalid parameter",
            "invalid argument",
            "unexpected argument",
            "required parameter",
            "malformed request",
            "validation error",
            "deployment",
            "model",
            "endpoint",
            "base url"
          ),
          collapse = "|"
        ),
        message_lower
      )
    ) {
      return(list(
        failure_class = "technical_error",
        technical_scope = "global"
      ))
    }
  }

  if (!is.na(status_code) && status_code == 405L) {
    # Method-not-allowed responses with structured-output capability text are
    # deterministic model/backend incompatibilities, not retryable row issues.
    if (
      grepl(
        paste(
          c(
            "json_schema response format is not supported",
            "response format is not supported",
            "structured output",
            "structured outputs",
            "method not allowed"
          ),
          collapse = "|"
        ),
        message_lower
      )
    ) {
      return(list(
        failure_class = "technical_error",
        technical_scope = "global"
      ))
    }
  }

  if (!is.na(status_code) && status_code == 413L) {
    return(list(
      failure_class = "technical_error",
      technical_scope = "row"
    ))
  }

  if (
    grepl(
      paste(
        c(
          "invalid api key",
          "api key",
          "credential",
          "authentication",
          "unauthoriz",
          "forbidden",
          "resource not found",
          "model not found",
          "deployment not found",
          "endpoint not found",
          "unsupported parameter",
          "unknown parameter",
          "invalid parameter",
          "invalid argument",
          "unexpected argument",
          "required parameter",
          "malformed request",
          "validation error",
          "unused argument",
          "bad endpoint",
          "base url"
        ),
        collapse = "|"
      ),
      message_lower
    )
  ) {
    return(list(
      failure_class = "technical_error",
      technical_scope = "global"
    ))
  }

  # Treat prompt-specific deterministic failures as row-scoped technical
  # issues so the solver can salvage other rows before aborting.
  if (
    grepl(
      paste(
        c(
          "context length",
          "maximum context",
          "max context",
          "too many tokens",
          "prompt is too long",
          "prompt too long",
          "token limit",
          "content filter",
          "content policy",
          "safety system",
          "blocked by policy"
        ),
        collapse = "|"
      ),
      message_lower
    )
  ) {
    return(list(
      failure_class = "technical_error",
      technical_scope = "row"
    ))
  }

  list(
    failure_class = "llm_failure",
    technical_scope = NA_character_
  )
}

#' Extract an HTTP-like status code from a provider error message
#'
#' @param message Failure message extracted from the provider or fallback layer.
#'
#' @return An integer status code or `NA_integer_`.
#'
#' @keywords internal
.llm_solver_http_status_code <- function(message) {
  # Return missing when the provider message carries no usable text.
  if (!rlang::is_string(message) || !nzchar(message)) {
    return(NA_integer_)
  }

  # Search a small set of permissive patterns because provider clients vary in
  # how they embed HTTP status fragments inside error messages.
  patterns <- c(
    "status(?: code)?\\s*[:=]?\\s*([245][0-9]{2})",
    "http[^0-9]*([245][0-9]{2})",
    "\\b([245][0-9]{2})\\b"
  )

  # Use the first match so downstream classification can work from one stable
  # integer status code or a missing value.
  for (pattern in patterns) {
    matches <- regexec(pattern, message, ignore.case = TRUE)
    matched <- regmatches(message, matches)[[1]]
    if (length(matched) >= 2L) {
      return(as.integer(matched[[2]]))
    }
  }

  NA_integer_
}

#' Abort a solver call after technical failures were detected
#'
#' @param technical_failures List of technical-failure records.
#'
#' @return This function is called for its side effect and always aborts.
#'
#' @keywords internal
.llm_solver_abort_technical_failures <- function(technical_failures) {
  # Summarize the scopes and a few distinct reasons so the pipeline error
  # explains whether the fault was global or row-specific.
  scopes <- purrr::map_chr(technical_failures, "scope")
  reasons <- unique(purrr::map_chr(technical_failures, "reason"))
  example_reasons <- .llm_solver_escape_cli_braces(
    .llm_solver_compact_text(utils::head(reasons, 3L))
  )

  cli::cli_abort(
    c(
      "llm_solver hit provider/request technical errors after salvage.",
      "x" = paste0(
        length(technical_failures),
        " technical row(s) detected: ",
        sum(scopes == "global"),
        " global, ",
        sum(scopes == "row"),
        " row-specific."
      ),
      "i" = paste0(
        "Example error(s): ",
        paste(example_reasons, collapse = " | ")
      )
    ),
    class = "haiLLM_llm_solver_technical_error"
  )
}

#' Escape literal braces for cli templates
#'
#' @param x Character vector to escape.
#'
#' @return Character vector safe for `cli` template rendering.
#' @keywords internal
.llm_solver_escape_cli_braces <- function(x) {
  # Return early for empty vectors so downstream callers can forward them as-is.
  if (length(x) == 0L) {
    return(x)
  }

  # Double braces so `cli` treats provider JSON as literal text rather than a
  # template expression.
  x <- gsub("{", "{{", x, fixed = TRUE)
  gsub("}", "}}", x, fixed = TRUE)
}

#' Check whether a structured row carries a transport failure marker
#'
#' @param row_metadata Raw row payload or row-level condition metadata.
#'
#' @return `TRUE` when ellmer marked the row as failed in transport.
#' @keywords internal
.llm_solver_structured_row_has_transport_error <- function(row_metadata) {
  # Extract the optional `.error` column that ellmer adds for failed turns.
  error_field <- NULL
  if (
    inherits(row_metadata, "data.frame") && ".error" %in% names(row_metadata)
  ) {
    error_field <- row_metadata[[".error"]]
  } else if (
    is.list(row_metadata) &&
      !is.null(names(row_metadata)) &&
      ".error" %in% names(row_metadata)
  ) {
    error_field <- row_metadata[[".error"]]
  }

  # Treat any populated `.error` value as a transport-side failure marker.
  if (is.null(error_field) || length(error_field) == 0L) {
    return(FALSE)
  }

  if (is.list(error_field)) {
    return(!is.null(error_field[[1]]))
  }

  !all(is.na(error_field))
}

#' Detect parse-like structured error messages
#'
#' @param message A normalized failure message string.
#'
#' @return `TRUE` when the message describes parsing or schema extraction.
#' @keywords internal
.llm_solver_is_parse_error_message <- function(message) {
  # Match a narrow set of parsing and schema keywords so transport errors stay
  # in the no-response bucket.
  if (!rlang::is_string(message) || !nzchar(message)) {
    return(FALSE)
  }

  grepl(
    paste(
      c(
        "parse",
        "parsing",
        "extract data",
        "json",
        "schema"
      ),
      collapse = "|"
    ),
    tolower(message)
  )
}

#' Collect the required field paths for a structured type
#'
#' @param type Ellmer structured type object.
#' @param path Current nested field path.
#'
#' @return A character vector of required field paths.
#' @keywords internal
.llm_solver_required_field_paths <- function(type, path = character()) {
  # Ignore optional branches because they do not participate in the retry
  # contract.
  if (!isTRUE(type@required)) {
    return(character())
  }

  # Recurse into required objects so leaf fields can be reported precisely.
  if (inherits(type, "ellmer::TypeObject")) {
    return(unlist(
      purrr::imap(
        type@properties,
        function(property_type, name) {
          .llm_solver_required_field_paths(property_type, c(path, name))
        }
      ),
      use.names = FALSE
    ))
  }

  # Arrays participate in the emptiness check at their own field path.
  current_path <- paste(path, collapse = ".")
  if (inherits(type, "ellmer::TypeArray")) {
    return(current_path)
  }

  # Basic and enum fields are reported at their leaf path.
  if (nzchar(current_path)) {
    return(current_path)
  }

  character()
}

#' Find empty required fields in a structured payload
#'
#' @param value Raw structured payload for one row.
#' @param type Ellmer structured type object.
#' @param path Current nested field path.
#'
#' @return A character vector of empty required field paths.
#' @keywords internal
.llm_solver_empty_required_fields <- function(
  value,
  type,
  path = character()
) {
  # Ignore optional schema branches because they do not drive retries.
  if (!isTRUE(type@required)) {
    return(character())
  }

  # Recurse through required objects so partial emptiness stays visible.
  if (inherits(type, "ellmer::TypeObject")) {
    return(unlist(
      purrr::imap(
        type@properties,
        function(property_type, name) {
          .llm_solver_empty_required_fields(
            .llm_solver_structured_field(value, name),
            property_type,
            c(path, name)
          )
        }
      ),
      use.names = FALSE
    ))
  }

  # Treat empty arrays and empty leaf values as retryable empty responses.
  current_path <- paste(path, collapse = ".")
  if (.llm_solver_is_empty_structured_value(value) && nzchar(current_path)) {
    return(current_path)
  }

  character()
}

#' Extract one named field from a structured row payload
#'
#' @param value Raw structured payload for one row.
#' @param name Field name to extract.
#'
#' @return The extracted field value or `NULL`.
#' @keywords internal
.llm_solver_structured_field <- function(value, name) {
  # Pull fields from one-row tibbles and data frames returned by ellmer.
  if (inherits(value, "data.frame") && name %in% names(value)) {
    field <- value[[name]]
    if (is.list(field) && length(field) == 1L) {
      return(field[[1]])
    }
    return(field)
  }

  # Pull fields from named list payloads returned by direct fallback.
  if (
    is.list(value) &&
      !is.null(names(value)) &&
      name %in% names(value)
  ) {
    return(value[[name]])
  }

  NULL
}

#' Check whether a structured field value is empty
#'
#' @param value Structured field value to inspect.
#'
#' @return `TRUE` when the value is empty, missing, or whitespace-only.
#' @keywords internal
.llm_solver_is_empty_structured_value <- function(value) {
  # Treat missing fields and zero-length containers as empty.
  if (is.null(value) || length(value) == 0L) {
    return(TRUE)
  }

  # Evaluate scalar and vector text values after trimming whitespace.
  if (is.character(value) || is.factor(value)) {
    value <- as.character(value)
    return(all(is.na(value) | !nzchar(trimws(value))))
  }

  # Numeric and logical fields are empty only when every element is missing.
  if (is.logical(value) || is.integer(value) || is.numeric(value)) {
    return(all(is.na(value)))
  }

  # For list payloads, require at least one non-empty element.
  if (is.list(value)) {
    return(all(purrr::map_lgl(value, .llm_solver_is_empty_structured_value)))
  }

  FALSE
}

#' Check whether a chat carries the synthetic solver failure marker
#'
#' @param chat Row-level chat returned by the helper.
#'
#' @return `TRUE` when the last assistant turn is the synthetic failure marker.
#' @keywords internal
.llm_solver_chat_has_failure_marker <- function(chat) {
  # Read the final assistant turn text and match the synthetic failure prefix.
  last_turn <- tryCatch(chat$last_turn(), error = function(e) NULL)
  last_text <- tryCatch(
    last_turn@text[[1]],
    error = function(e) NA_character_
  )

  is.character(last_text) &&
    length(last_text) == 1L &&
    !is.na(last_text) &&
    grepl("^FAILURE:", last_text)
}

#' Resolve a solver chat object for `llm_solver()`
#'
#' Accepts either an `ellmer` `Chat` object or a zero-argument factory that
#' returns one. The object path is cloned so `vitals` can mutate the resolved
#' chat without affecting the caller's original instance.
#'
#' @param solver_chat Either an `ellmer` `Chat` object or a zero-argument
#'   factory that returns one.
#'
#' @return A cloned `ellmer` `Chat` object.
#' @keywords internal
.llm_solver_resolve_chat <- function(solver_chat) {
  # Direct Chat objects are cloned so that multiple solver runs do not share
  # the same mutable state or message history.
  if (inherits(solver_chat, "Chat")) {
    return(solver_chat$clone())
  }

  # If a factory function is provided, execute it and ensure it produces
  # a valid Chat object with zero arguments.
  if (rlang::is_function(solver_chat)) {
    chat <- tryCatch(
      solver_chat(),
      error = function(e) {
        cli::cli_abort(
          "{.arg solver_chat} factory must be callable with zero arguments.",
          parent = e
        )
      }
    )

    # Validate that the factory output conforms to the expected Chat class.
    if (!inherits(chat, "Chat")) {
      cli::cli_abort(
        "{.arg solver_chat} factory must return an ellmer {.cls Chat} object."
      )
    }

    return(chat)
  }

  # Fail early if the input is neither a Chat nor a factory to keep the
  # evaluation error surface clean.
  cli::cli_abort(
    c(
      "{.arg solver_chat} must be an ellmer {.cls Chat} object",
      "or a zero-argument factory returning one."
    )
  )
}

#' Extract the last assistant turn text from a chat
#'
#' This helper normalizes the `parallel_chat()` return value to a single text
#' string or `NA_character_` when the assistant text cannot be read.
#'
#' @param chat A chat returned by `ellmer::parallel_chat()`.
#'
#' @return A character scalar containing the last assistant turn text or
#'   `NA_character_` when no usable text is available.
#' @keywords internal
.llm_solver_last_text <- function(chat) {
  # Return a missing marker for NULL chats so the result vector remains aligned.
  if (is.null(chat)) {
    return(NA_character_)
  }

  # Identify the final turn in the chat history, which corresponds to the
  # model's response.
  last_turn <- tryCatch(chat$last_turn(), error = function(e) NULL)
  if (is.null(last_turn)) {
    return(NA_character_)
  }

  # Safely extract the text component from the turn object, handle potential
  # missing slots.
  txt <- tryCatch(last_turn@text, error = function(e) NA_character_)
  if (!is.character(txt) || length(txt) == 0L) {
    return(NA_character_)
  }

  # Ensure the extracted text is a valid, non-empty character string before
  # returning it as the final result.
  txt <- txt[[1]]
  if (.llm_solver_is_empty_text(txt)) {
    return(NA_character_)
  }

  txt
}

#' Check whether a text response is empty
#'
#' @param value Candidate text response.
#'
#' @return `TRUE` when the value is missing, zero-length, or whitespace-only.
#' @keywords internal
.llm_solver_is_empty_text <- function(value) {
  # Keep text emptiness checks identical across extraction and retry logic.
  !is.character(value) ||
    length(value) != 1L ||
    is.na(value) ||
    !nzchar(trimws(value))
}

#' Build a failure chat for a row that could not be resolved
#'
#' Returns a synthetic chat that stays vitals-compatible, containing the
#' original prompt as the user turn and a failure marker as the assistant turn.
#'
#' @param base_chat A resolved `ellmer` chat object to clone.
#' @param prompt The original prompt string for the failed row.
#'
#' @return A synthetic `Chat` object with a failure marker in the assistant
#'   turn.
#'
#' @keywords internal
.llm_solver_failure_chat <- function(
  base_chat,
  prompt,
  message = "The solver returned no usable result"
) {
  # Clone the resolved solver chat to create a row-specific logging object.
  chat <- base_chat$clone()

  # Normalize the embedded failure text so downstream code can recover the
  # original provider or request error when one is available.
  if (!rlang::is_string(message) || !nzchar(trimws(message))) {
    message <- "The solver returned no usable result"
  }
  message <- sub("^FAILURE:\\s*", "", trimws(message))

  # Construct a standard UserTurn to preserve the original prompt in the log.
  user_turn <- ellmer::UserTurn(
    contents = list(ellmer::ContentText(as.character(prompt)))
  )

  # Attach a synthetic failure message as the assistant turn so that downstream
  # vitals tools can clearly identify why the row is unresolved.
  assistant_turn <- ellmer::AssistantTurn(
    contents = list(ellmer::ContentText(
      paste0("FAILURE: ", message)
    )),
    tokens = c(0, 0, 0)
  )

  # Commit the synthetic turns to the row-level chat object.
  chat$set_turns(list(user_turn, assistant_turn))
  chat
}

#' Direct single-prompt fallback for unstructured chat
#'
#' When `parallel_chat()` fails catastrophically, this helper calls the
#' underlying `Chat` methods directly on cloned chat objects, one row at a
#' time. It avoids re-entering the parallel wrapper.
#'
#' @param base_chat A resolved `ellmer` chat object to clone.
#' @param prompts A list of rendered prompts aligned to the dataset rows.
#' @param failure_chat Optional failure chat helper.
#' @param last_text Optional helper for extracting the final assistant turn.
#' @param log_context Optional log context from
#'   `.llm_solver_create_log_context()`.
#' @param emit_message Optional message helper. Defaults to
#'   `.llm_solver_emit_message()`.
#'
#' @return A list with `result`, `solver_chat`, and `solver_metadata = NULL`.
#'
#' @keywords internal
.llm_solver_direct_fallback_unstructured <- function(
  base_chat,
  prompts,
  failure_chat = NULL,
  last_text = NULL,
  log_context = NULL,
  emit_message = NULL
) {
  # Default helper dependencies here as well so fallback can run standalone or
  # be called from a worker with the same body.
  if (is.null(failure_chat)) {
    failure_chat <- .llm_solver_failure_chat
  }
  if (is.null(last_text)) {
    last_text <- .llm_solver_last_text
  }
  if (is.null(emit_message)) {
    emit_message <- .llm_solver_emit_message
  }
  n <- length(prompts)
  result <- rep(NA_character_, n)
  solver_chat_list <- vector("list", n)

  for (i in seq_len(n)) {
    # Announce row-level fallback progress before each direct request so the
    # console and log file keep moving even after a batch failure.
    emit_message(
      "info",
      paste0("solver: row fallback ", i, "/", n),
      log_context
    )

    # Clone the chat so each row gets an independent message history.
    chat <- base_chat$clone()

    # Call the chat method directly on the cloned object while mirroring any
    # stdout or cli progress events into the active solver log.
    out <- .llm_solver_capture_console_output(
      log_context = log_context,
      expr = tryCatch(
        chat$chat(as.character(prompts[[i]])),
        error = function(e) e
      )
    )

    if (inherits(out, "error")) {
      solver_chat_list[[i]] <- failure_chat(
        base_chat,
        prompts[[i]],
        conditionMessage(out)
      )
    } else {
      solver_chat_list[[i]] <- chat
      result[[i]] <- last_text(chat)
    }
  }

  list(
    result = result,
    solver_chat = solver_chat_list,
    solver_metadata = NULL,
    used_direct_fallback = TRUE
  )
}

#' Direct single-prompt fallback for structured chat
#'
#' When `parallel_chat_structured()` fails catastrophically, this helper calls
#' `chat_structured()` directly on cloned chat objects, one row at a time.
#' It avoids re-entering the parallel wrapper.
#'
#' @param base_chat A resolved `ellmer` chat object to clone.
#' @param prompts A list of rendered prompts aligned to the dataset rows.
#' @param type An ellmer structured type object.
#' @param dots Named solver arguments collected from `...`.
#' @param failure_chat Optional failure chat helper.
#' @param json_string Optional helper for JSON serialization.
#' @param log_context Optional log context from
#'   `.llm_solver_create_log_context()`.
#' @param emit_message Optional message helper. Defaults to
#'   `.llm_solver_emit_message()`.
#'
#' @return A list with `result`, `solver_chat`, and `solver_metadata`.
#'
#' @keywords internal
.llm_solver_direct_fallback_structured <- function(
  base_chat,
  prompts,
  type,
  dots,
  failure_chat = NULL,
  json_string = NULL,
  log_context = NULL,
  emit_message = NULL
) {
  # Default helper dependencies here as well so structured fallback keeps one
  # code path regardless of where it is invoked.
  if (is.null(failure_chat)) {
    failure_chat <- .llm_solver_failure_chat
  }
  if (is.null(json_string)) {
    json_string <- .llm_solver_json_string
  }
  if (is.null(emit_message)) {
    emit_message <- .llm_solver_emit_message
  }
  dots <- .llm_solver_direct_fallback_dots(dots)

  n <- length(prompts)
  result <- rep(NA_character_, n)
  solver_chat_list <- vector("list", n)
  solver_metadata_list <- vector("list", n)

  for (i in seq_len(n)) {
    # Announce row-level fallback progress before each direct request so the
    # console and log file keep moving even after a batch failure.
    emit_message(
      "info",
      paste0("solver: row fallback ", i, "/", n),
      log_context
    )

    # Clone the chat so each row gets an independent message history.
    chat <- base_chat$clone()

    # Call chat_structured directly on the cloned object while mirroring any
    # stdout or cli progress events into the active solver log.
    out <- .llm_solver_capture_console_output(
      log_context = log_context,
      expr = tryCatch(
        rlang::exec(
          chat$chat_structured,
          as.character(prompts[[i]]),
          type = type,
          !!!dots
        ),
        error = function(e) e
      )
    )

    if (inherits(out, "condition")) {
      solver_chat_list[[i]] <- failure_chat(
        base_chat,
        prompts[[i]],
        conditionMessage(out)
      )
      solver_metadata_list[[i]] <- out
    } else {
      # Serialize the structured result for downstream compatibility.
      result[[i]] <- json_string(out)
      solver_metadata_list[[i]] <- out
      solver_chat_list[[i]] <- chat
    }
  }

  list(
    result = result,
    solver_chat = solver_chat_list,
    solver_metadata = solver_metadata_list,
    used_direct_fallback = TRUE
  )
}

#' Filter parallel-only arguments from direct fallback calls
#'
#' @param dots Named solver arguments collected from `...`.
#'
#' @return A named list suitable for direct `Chat` method calls.
#'
#' @keywords internal
.llm_solver_direct_fallback_dots <- function(dots) {
  # Direct single-row chat methods do not understand arguments that only belong
  # to ellmer's parallel helpers.
  dots[setdiff(names(dots), c("on_error", "max_active"))]
}
