#' Parallel LLM helpers (promises-based)
#'
#' Provides a reimplementation of the parallel chat stack using a
#' promises/curl-multi engine with per-request retry, caching, and
#' progress logging. Top-level functions mirror the behaviour of
#' `ellmer::parallel_chat()` and `ellmer:::parallel_turns()`.

#' Compute backoff wait time (rate limit headers first)
#'
#' @noRd
.compute_backoff_wait <- function(err, delay, cap) {
  # First, try to extract retry-after header from HTTP response
  if (
    !is.null(err) &&
      is.list(err) &&
      ("resp" %in% names(err) || "response" %in% names(err))
  ) {
    resp <- if ("resp" %in% names(err)) err$resp else err$response
    # Check for explicit retry-after header (in seconds)
    ra_secs <- suppressWarnings(as.numeric(httr2::resp_headers(resp)[[
      "retry-after"
    ]]))
    if (!rlang::is_scalar_double(ra_secs) || ra_secs <= 0) {
      # Fallback to x-ratelimit-reset header (usually in milliseconds since epoch)
      reset <- suppressWarnings(as.numeric(httr2::resp_headers(resp)[[
        "x-ratelimit-reset"
      ]]))
      if (rlang::is_scalar_double(reset) && reset > 0) {
        # Convert reset timestamp to seconds from now
        now_ms <- as.numeric(Sys.time()) * 1000
        ra_secs <- max(1, (reset - now_ms) / 1000)
      }
    }
    # Use server-provided delay if available, capped at maximum
    if (rlang::is_scalar_double(ra_secs) && ra_secs > 0) {
      return(min(cap, ra_secs))
    }
  }
  # Fallback to exponential backoff with jitter
  base <- min(cap, delay * 2)
  # Add randomness using exponential distribution to avoid thundering herd
  max(1, min(cap, stats::rexp(1, rate = 1 / base)))
}


#' Build a stable cache key for an httr2 request
#'
#' Uses a hash of the HTTP method, full URL (including query), and canonical
#' JSON serialization of the request body data when present. Volatile headers
#' or cookies are intentionally excluded.
#' @noRd
.hash_request_key <- function(req) {
  # Extract method
  method <- tryCatch(req$method, error = \(e) "GET")
  # Extract full URL (string)
  url_chr <- tryCatch(as.character(req$url), error = \(e) "")
  # Extract JSON body data when present (as used by req_body_json)
  body_data <- tryCatch(req$body$data, error = \(e) NULL)
  body_json <- if (!is.null(body_data)) {
    jsonlite::toJSON(body_data, auto_unbox = TRUE, null = "null", digits = NA)
  } else {
    # Fallback: use raw body if present
    raw_body <- tryCatch(req$body, error = \(e) NULL)
    if (is.raw(raw_body)) paste(raw_body, collapse = ",") else ""
  }
  rlang::hash(paste0(method, "|", url_chr, "|", body_json))
}


#' Best-effort repair for slightly malformed JSON payloads
#'
#' Some providers occasionally append non-JSON bytes after a valid JSON object
#' or return leading noise. This helper attempts a minimal, safe recovery by:
#' - Dropping any prefix before the first opening brace `{` or bracket `[`.
#' - Trimming any suffix after the last closing brace `}` or bracket `]`.
#' Returns a character string containing the trimmed JSON text, or NULL if it
#' cannot detect plausible JSON bounds.
#' @noRd
.repair_json_text <- function(txt) {
  if (!rlang::is_string(txt) || !nzchar(txt)) {
    return(NULL)
  }
  # Find first plausible JSON start
  m_start <- regexpr("[\\{\\[]", txt, perl = TRUE)
  if (is.na(m_start) || m_start < 1L) {
    return(NULL)
  }
  core <- substring(txt, m_start[1])
  # Find last closing brace/bracket
  close_braces <- gregexpr("}", core, fixed = TRUE)[[1]]
  close_bracks <- gregexpr("]", core, fixed = TRUE)[[1]]
  last_brace <- if (length(close_braces) && close_braces[1] != -1L) {
    max(close_braces)
  } else {
    -1L
  }
  last_brack <- if (length(close_bracks) && close_bracks[1] != -1L) {
    max(close_bracks)
  } else {
    -1L
  }
  last_close <- max(last_brace, last_brack)
  if (last_close <= 0L) {
    return(NULL)
  }
  substring(core, 1L, last_close)
}


#' Colorize helper for TTY output
#'
#' @noRd
.ansi_colorize <- function(text, color) {
  # ANSI color codes for terminal output
  colors <- list(
    reset = "\033[0m",
    bright_cyan = "\033[96m",
    bright_blue = "\033[94m",
    yellow = "\033[33m",
    bright_magenta = "\033[95m",
    bright_green = "\033[92m",
    green = "\033[32m",
    red = "\033[31m",
    bright_yellow = "\033[93m"
  )
  # Skip colorization if NO_COLOR env var is set or not in interactive session
  if (Sys.getenv("NO_COLOR") != "" || !interactive()) {
    return(text)
  }
  paste0(colors[[color]], text, colors$reset)
}


#' Extract HTTP status from an error if available
#'
#' Tries to read `err$resp` from httr2 errors, otherwise returns NA.
#' @noRd
.extract_http_status <- function(err) {
  if (
    !is.null(err) &&
      is.list(err) &&
      ("resp" %in% names(err) || "response" %in% names(err))
  ) {
    resp <- if ("resp" %in% names(err)) err$resp else err$response
    status <- tryCatch(httr2::resp_status(resp), error = \(e) NA_real_)
    if (is.finite(status)) return(as.integer(status))
  }
  NA_integer_
}


#' Determine if an HTTP status is retryable
#'
#' Retries on 408, 429, and any 5xx.
#' @noRd
.is_retryable_status <- function(status) {
  if (is.null(status) || length(status) != 1L || is.na(status)) {
    return(FALSE)
  }
  status <- as.integer(status)
  status == 408L || status == 429L || (status >= 500L && status <= 599L)
}


#' Should we retry given an error condition?
#'
#' Uses structured status when possible; falls back to message pattern.
#' @noRd
.should_retry_http <- function(err) {
  status <- .extract_http_status(err)
  if (.is_retryable_status(status)) {
    return(TRUE)
  }
  msg <- tryCatch(conditionMessage(err), error = \(e) "")
  grepl("HTTP (429|408|50[0-9])", msg)
}


#' Progress logger for parallel requests
#'
#' Renders a single-line, colorized status using counters stored in `state$qs`.
#' The state is an explicit environment to avoid deep assignment side effects.
#' @noRd
.log_q <- function(state, tag, total_reqs) {
  # Calculate elapsed seconds since start
  elapsed <- round(
    as.numeric(difftime(Sys.time(), state$qs$start_time, units = "secs")),
    1
  )
  # Estimate next client-side rate-limiter delay in seconds using the
  # timestamp reserved for the following slot (if any).
  rl_delay <- 0
  if (isTRUE(state$rate$enabled) && !is.null(state$rate$next_time)) {
    rl_delay <- max(0, round(state$rate$next_time - as.numeric(Sys.time())))
  }
  # Build status parts, conditionally including non-zero sections
  parts <- c(
    .ansi_colorize(sprintf("[%s]", tag), "bright_cyan"),
    .ansi_colorize(
      sprintf("P:%d/%d", state$qs$pending, total_reqs),
      "bright_blue"
    ),
    .ansi_colorize(
      sprintf("R:%d (%d s)", state$qs$retries, round(state$qs$max_delay)),
      "yellow"
    ),
    if (rl_delay > 0) {
      .ansi_colorize(sprintf("L:(%d s)", rl_delay), "yellow")
    } else {
      NULL
    },
    if (state$qs$active > 0) {
      .ansi_colorize(sprintf("A:%d", state$qs$active), "bright_magenta")
    } else {
      NULL
    },
    if (state$qs$cache_hits > 0) {
      .ansi_colorize(sprintf("C:%d", state$qs$cache_hits), "bright_green")
    } else {
      NULL
    },
    .ansi_colorize(sprintf("S:%d/%d", state$qs$success, total_reqs), "green"),
    if (state$qs$failed > 0) {
      .ansi_colorize(sprintf("F:%d", state$qs$failed), "red")
    } else {
      NULL
    },
    .ansi_colorize(sprintf("[%.1fs]", elapsed), "bright_yellow")
  )
  # Print padded line to overwrite previous output
  msg <- paste(parts, collapse = " ")
  pad <- max(0, state$qs$last_nchar - nchar(msg))
  cat("\r", msg, strrep(" ", pad))
  state$qs$last_nchar <- nchar(msg)
}


#' Wait for a master promise, handling interrupts
#'
#' @noRd
.wait_blocking <- function(master, tick = 0.1, abort_fun) {
  # Ensure promises is available and bind operators locally
  rlang::check_installed("promises")
  `%...>%` <- promises::`%...>%`
  `%...!%` <- promises::`%...!%`

  # State tracking for promise resolution using explicit environment
  state <- new.env(parent = emptyenv())
  state$resolved <- FALSE
  state$value <- NULL
  # Attach handlers for both success and failure
  master %...>%
    {
      state$value <- .
      state$resolved <- TRUE
    } %...!%
    {
      state$value <- .
      state$resolved <- TRUE
    }
  tryCatch(
    {
      # Poll the event loop until promise resolves
      while (!isTRUE(state$resolved)) {
        # Process pending events with timeout
        later::run_now(timeoutSecs = tick)
        # Brief sleep to prevent busy waiting
        Sys.sleep(tick / 2)
      }
      state$value
    },
    interrupt = function(e) {
      # Handle Ctrl+C gracefully by calling abort function if provided
      if (is.function(abort_fun)) {
        abort_fun()
      }
      cli::cli_alert_warning(
        "Aborted by user
      "
      )
      NULL
    }
  )
}


#' Consolidate temp JSON cache files into a global cache
#'
#' Merges any JSON files found in `temp_dir` into a single RDS stored at
#' `global_cache_path`. The global cache is a named list mapping conversation
#' hash to the parsed JSON object. After consolidation, the temp directory is
#' removed. Returns the updated global cache list.
#'
#' @noRd
.consolidate_temp_json_cache <- function(temp_dir, global_cache_path) {
  global_cache <- if (fs::file_exists(global_cache_path)) {
    tryCatch(readr::read_rds(global_cache_path), error = \(e) list())
  } else {
    list()
  }

  initial_cache <- global_cache

  if (!fs::dir_exists(temp_dir)) {
    return(global_cache)
  }

  temp_files <- fs::dir_ls(temp_dir, glob = "*.json")
  if (rlang::is_empty(temp_files)) {
    return(global_cache)
  }

  for (file_path in temp_files) {
    stem <- fs::path_ext_remove(fs::path_file(file_path))
    # Allow for unique temp suffixes like "<id>_<i>_<attempt>"; consolidate by
    # base id
    hash_key <- sub("[-_].*$", "", stem)
    json <- tryCatch(
      jsonlite::fromJSON(file_path, simplifyVector = FALSE),
      error = \(e) NULL
    )
    if (!is.null(json)) {
      global_cache[[hash_key]] <- json
    }
  }

  if (!identical(global_cache, initial_cache)) {
    fs::dir_create(fs::path_dir(global_cache_path))
    readr::write_rds(global_cache, global_cache_path)
  }

  fs::dir_delete(temp_dir)
  global_cache
}


#' Initialize per-call error directory for diagnostics
#'
#' @noRd
.init_error_dir <- function() {
  base <- fs::path_temp()
  unique <- paste0(
    "req_parallel_errors_",
    as.integer(Sys.time()),
    "_",
    sample.int(1e9, 1)
  )
  dir <- fs::path(base, unique)
  fs::dir_create(dir)
  dir
}


#' Write error payload (HTTP or generic) to JSON file in diagnostics dir
#'
#' @noRd
.write_error_payload <- function(id, err_obj, dest_dir) {
  path <- fs::path(dest_dir, paste0(id, ".json"))
  payload <- list()
  # Try to extract response fields if present
  if (
    is.list(err_obj) && "resp" %in% names(err_obj) && !is.null(err_obj$resp)
  ) {
    resp <- err_obj$resp
    status <- tryCatch(httr2::resp_status(resp), error = \(e) NA_integer_)
    headers <- tryCatch(as.list(httr2::resp_headers(resp)), error = \(e) list())
    if (inherits(headers, "httr2_headers")) {
      headers <- unclass(headers)
    }
    body_json <- tryCatch(
      httr2::resp_body_json(resp, simplifyVector = FALSE),
      error = \(e) NULL
    )
    body_text <- NULL
    if (is.null(body_json)) {
      body_text <- tryCatch(httr2::resp_body_string(resp), error = \(e) NULL)
    }
    payload <- list(
      status = status,
      headers = headers,
      json = body_json,
      text = body_text
    )
  } else {
    # Fallback to basic condition info
    payload <- list(
      message = tryCatch(conditionMessage(err_obj), error = \(e) NULL)
    )
  }
  fs::dir_create(fs::path_dir(path))
  jsonlite::write_json(
    payload,
    path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  invisible(path)
}


#' Perform JSON HTTP requests concurrently with promises
#'
#' Executes a list of `httr2` requests concurrently via a shared curl multi
#' pool, with per-request retry using capped exponential backoff or
#' server-provided delay headers when available. Optionally throttles to a
#' target requests-per-minute capacity and supports a two-tier cache controlled
#' by `global_cache_path`.
#'
#' When `global_cache_path` is provided, a two-tier cache is enabled:
#' - Global cache (RDS) storing parsed JSON results using stable keys derived
#' from request method + URL + query + body
#' - Temp cache directory storing per-request JSON files for the current run
#'
#' If `global_cache_path` is `NULL` or `FALSE`, caching is disabled. If caching
#' is enabled and `temp_cache_dir` is `NULL`, a temp directory next to the
#' global cache file is used (named `<basename>_temp`).
#'
#' On user interrupt, the function returns early and sets the `"aborted"`
#' attribute on the result list to `TRUE`.
#'
#' Retry semantics: For retryable HTTP statuses (408, 429, and 5xx), requests
#' are retried using capped exponential backoff, preferring server-provided
#' delays when available (e.g., `Retry-After`, `X-RateLimit-Reset`). The
#' per-retry wait is capped by `backoff_cap`, and retries proceed only while the
#' current backoff delay is strictly less than `backoff_cap`.
#'
#' @param reqs A list of `httr2` request objects.
#' @param global_cache_path Path to an RDS file used as the global cache. Set to
#'   `NULL` or `FALSE` to disable caching.
#' @param temp_cache_dir Directory where temporary per-request JSON files are
#'   stored during this run. If `NULL` and caching is enabled, a sibling
#'   directory next to `global_cache_path` is used.
#' @param rpm Optional numeric requests-per-minute throttle. If `NULL`, no
#'   client-side throttling is applied.
#' @param backoff_base Initial backoff delay in seconds when retrying
#'   408/429/5xx.
#' @param backoff_cap Maximum backoff delay in seconds. Also acts as the retry
#'   cut-off: retries continue only while the current backoff delay is strictly
#'   less than this cap. Any server-provided wait (e.g., `Retry-After`) is
#'   capped at this value as well.
#' @param max_concurrency Optional positive integer cap on concurrently active
#'   HTTP requests. Set to `NULL` (default) for no client-side concurrency
#'   limit.
#' @param halve_rpm_on_retry Logical flag to reduce client-side throughput when
#'   the server signals trouble. When `TRUE` and a retry is scheduled due to a
#'   retryable failure, the effective RPM used by the client-side rate limiter
#'   is halved (down to a minimum of 1). This helps alleviate pressure during
#'   congestion or rate limiting events.
#'
#' @return A list with one element per request. Each element is a list with
#'   fields `kind` ("ok" or "err"), and either `json` (parsed JSON result) on
#'   success, or `err` (the error condition) on failure. When returned from the
#'   global cache, elements also carry `cached = TRUE`.
#'
#' @examples
#' \donttest{
#' if (rlang::is_installed(c("httr2", "promises", "later", "curl", "jsonlite"))) {
#'   # Basic usage without caching
#'   r1 <- httr2::request("https://httpbin.org/json")
#'   r2 <- httr2::request("https://httpbin.org/anything")
#'   res <- BaySREn::req_perform_parallel_promises(
#'     reqs = list(r1, r2),
#'     rpm = 120,
#'     backoff_base = 1,
#'     backoff_cap = 4,
#'     global_cache_path = FALSE
#'   )
#'   vapply(res, function(x) x$kind, character(1))
#'
#'   # With global cache path (second call should hit cache)
#'   cache_path <- fs::file_temp(ext = "rds")
#'   res1 <- BaySREn::req_perform_parallel_promises(
#'     reqs = list(r1, r2),
#'     rpm = 120,
#'     global_cache_path = cache_path
#'   )
#'   res2 <- BaySREn::req_perform_parallel_promises(
#'     reqs = list(r1, r2),
#'     rpm = 120,
#'     global_cache_path = cache_path
#'   )
#'   vapply(res2, function(x) isTRUE(x$cached), logical(1))
#' }
#' }
#'
#' @seealso [parallel_turns_promises()], [parallel_chat_promises()]
#' @export
req_perform_parallel_promises <- function(
  reqs,
  global_cache_path = NULL,
  temp_cache_dir = NULL,
  rpm = NULL,
  backoff_base = 5,
  backoff_cap = 120,
  max_concurrency = NULL,
  halve_rpm_on_retry = FALSE
) {
  # Check if promises is installed
  rlang::check_installed("promises")
  `%...>%` <- promises::`%...>%`
  `%...!%` <- promises::`%...!%`

  # Create curl pool
  pool <- curl::new_pool()
  # Explicit mutable state environment to avoid deep assignment
  state <- new.env(parent = emptyenv())
  # Per-call error capture directory (diagnostics for total failures)
  state$error_dir <- .init_error_dir()

  # Helper: write a structured JSON error payload to per-call error dir
  write_error_payload <- function(i, err_obj) {
    id <- ids[[i]]
    path <- fs::path(state$error_dir, paste0(id, ".json"))
    payload <- list()
    # Try to extract response fields if present
    if (
      is.list(err_obj) && "resp" %in% names(err_obj) && !is.null(err_obj$resp)
    ) {
      resp <- err_obj$resp
      status <- tryCatch(
        httr2::resp_status(resp),
        error = function(e) NA_integer_
      )
      headers <- tryCatch(
        as.list(httr2::resp_headers(resp)),
        error = function(e) list()
      )
      if (inherits(headers, "httr2_headers")) {
        headers <- unclass(headers)
      }
      body_json <- tryCatch(
        httr2::resp_body_json(resp, simplifyVector = FALSE),
        error = function(e) NULL
      )
      body_text <- NULL
      if (is.null(body_json)) {
        body_text <- tryCatch(
          httr2::resp_body_string(resp),
          error = function(e) NULL
        )
      }
      payload <- list(
        status = status,
        headers = headers,
        json = body_json,
        text = body_text
      )
    } else {
      # Fallback to basic condition info
      payload <- list(
        message = tryCatch(conditionMessage(err_obj), error = function(e) NULL)
      )
    }
    # Ensure directory exists and write JSON
    fs::dir_create(fs::path_dir(path))
    jsonlite::write_json(
      payload,
      path,
      auto_unbox = TRUE,
      pretty = TRUE,
      null = "null"
    )
    invisible(path)
  }
  # Derive ids by hashing method + URL + query + body (stable, internal)
  ids <- vapply(reqs, .hash_request_key, FUN.VALUE = character(1))

  # Global cache two-tier setup
  caching_enabled <- !is.null(global_cache_path) &&
    !identical(global_cache_path, FALSE)
  if (isTRUE(caching_enabled)) {
    if (is.null(temp_cache_dir)) {
      base <- fs::path_ext_remove(fs::path_file(global_cache_path))
      temp_cache_dir <- fs::path(
        fs::path_dir(global_cache_path),
        paste0(base, "_temp")
      )
    }
    state$global_cache <- .consolidate_temp_json_cache(
      temp_cache_dir,
      global_cache_path
    )
  } else {
    state$global_cache <- list()
  }
  # Optional client-side rate limiter (leaky bucket)
  if (!is.null(rpm) && is.finite(rpm) && rpm > 0) {
    state$rate <- list(
      enabled = TRUE,
      rpm = as.integer(rpm),
      initial_rpm = as.integer(rpm),
      floor_rpm = 1L,
      next_time = NULL, # next slot reserved for throttled call
      halve_on_retry = isTRUE(halve_rpm_on_retry)
    )
  } else {
    state$rate <- list(enabled = FALSE)
  }
  # Optional concurrency limiter
  if (
    !is.null(max_concurrency) &&
      is.finite(max_concurrency) &&
      max_concurrency > 0
  ) {
    state$concurrency <- list(
      enabled = TRUE,
      limit = max(1L, as.integer(max_concurrency)),
      inflight = 0L,
      queue = list()
    )
  } else {
    state$concurrency <- list(enabled = FALSE)
  }
  # Global abort flag to stop all operations on interrupt
  state$aborted <- FALSE
  # Track scheduled retry timers for cleanup
  state$retry_tokens <- vector("list", length(reqs))
  # Count attempts per request for debugging
  state$attempts <- rep(0L, length(reqs))
  # Progress state counters
  state$qs <- list(
    pending = length(reqs),
    retries = 0L,
    max_delay = 0,
    active = 0L,
    cache_hits = 0L,
    success = 0L,
    failed = 0L,
    start_time = Sys.time(),
    last_nchar = 0
  )

  # Schedule a function call respecting optional rate limit and minimum delay
  schedule_call <- function(expr_fun, min_delay = 0, use_rate_limit = TRUE) {
    promises::promise(function(resolve, reject) {
      now <- as.numeric(Sys.time())
      # Earliest due to explicit backoff
      start_at <- now + max(0, min_delay)
      if (use_rate_limit && isTRUE(state$rate$enabled)) {
        # Ensure we never schedule earlier than the next reserved slot.
        next_time <- state$rate$next_time
        if (
          !is.numeric(next_time) || length(next_time) != 1L || is.na(next_time)
        ) {
          next_time <- now
        }
        if (start_at < next_time) {
          start_at <- next_time
        }
        # Reserve the subsequent slot according to the current RPM.
        spacing <- 60 / max(1, as.numeric(state$rate$rpm))
        state$rate$next_time <- start_at + spacing
      }
      delay_s <- max(0, start_at - now)
      if (delay_s > 0) {
        later::later(
          function() {
            expr_fun() %...>%
              resolve %...!%
              (function(e) {
                # Propagate error to the outer promise to avoid unhandled
                # rejections.
                reject(e)
              })
          },
          delay = delay_s
        )
      } else {
        expr_fun() %...>%
          resolve %...!%
          (function(e) {
            # Propagate error to the outer promise to avoid unhandled
            # rejections.
            reject(e)
          })
      }
    })
  }

  acquire_concurrency <- function() {
    if (!isTRUE(state$concurrency$enabled)) {
      return(promises::promise_resolve(FALSE))
    }
    promises::promise(function(resolve, reject) {
      grant <- function() {
        state$concurrency$inflight <- state$concurrency$inflight + 1L
        resolve(TRUE)
      }
      if (state$concurrency$inflight < state$concurrency$limit) {
        grant()
      } else {
        state$concurrency$queue <- c(state$concurrency$queue, list(grant))
      }
    })
  }

  release_concurrency <- function() {
    if (!isTRUE(state$concurrency$enabled)) {
      return(invisible(NULL))
    }
    state$concurrency$inflight <- max(0L, state$concurrency$inflight - 1L)
    if (length(state$concurrency$queue)) {
      next_grant <- state$concurrency$queue[[1]]
      state$concurrency$queue <- state$concurrency$queue[-1]
      later::later(next_grant, delay = 0)
    }
    invisible(NULL)
  }

  # Cancel active transfers and scheduled retries
  abort_all <- function() {
    state$aborted <- TRUE
    # Cancel all active curl handles
    handles <- curl::multi_list(pool)$handle
    if (length(handles)) {
      lapply(handles, curl::multi_cancel)
    }
    # Cancel all scheduled retry timers
    lapply(
      state$retry_tokens,
      function(tok) if (inherits(tok, "later_timer_handle")) later::cancel(tok)
    )
  }

  # Core per-request retry logic
  retry_one <- function(i, delay = backoff_base, skip_rpm = FALSE) {
    # Early exit if globally aborted
    if (isTRUE(state$aborted)) {
      return(promises::promise_reject("aborted"))
    }

    # Global cache hit short-circuit
    id <- ids[[i]]
    if (isTRUE(caching_enabled) && !is.null(state$global_cache[[id]])) {
      state$qs$pending <- state$qs$pending - 1L
      state$qs$cache_hits <- state$qs$cache_hits + 1L
      .log_q(state, "working", length(reqs))
      return(promises::promise(function(resolve, reject) {
        resolve(list(
          kind = "ok",
          json = state$global_cache[[id]],
          cached = TRUE
        ))
      }))
    }

    # Increment attempt counter for this request
    state$attempts[[i]] <- state$attempts[[i]] + 1L
    first_attempt <- state$attempts[[i]] == 1L
    req <- reqs[[i]]
    path_i <- if (isTRUE(caching_enabled)) {
      # Use a unique temp filename per request attempt to avoid concurrent
      # writers clobbering the same file when multiple identical requests run
      # in parallel (e.g., replicates with identical bodies).
      attempt_idx <- state$attempts[[i]]
      fs::path(temp_cache_dir, paste0(id, "_", i, "_", attempt_idx, ".json"))
    } else {
      NULL
    }

    release_active <- function() {
      if (state$qs$active > 0L) {
        state$qs$active <- state$qs$active - 1L
      } else {
        state$qs$active <- 0L
      }
      if (isTRUE(state$concurrency$enabled)) {
        release_concurrency()
      }
    }

    begin_request <- function() {
      if (isTRUE(first_attempt)) {
        state$qs$pending <- state$qs$pending - 1L
      }
      state$qs$active <- state$qs$active + 1L
      .log_q(state, "working", length(reqs))
      if (!is.null(path_i)) {
        if (!fs::dir_exists(temp_cache_dir)) {
          fs::dir_create(temp_cache_dir)
        }
        httr2::req_perform_promise(req, pool = pool, path = path_i)
      } else {
        httr2::req_perform_promise(req, pool = pool)
      }
    }

    start_request <- function() {
      safe_launch <- function() {
        if (isTRUE(state$aborted)) {
          return(promises::promise_reject("aborted"))
        }
        tryCatch(
          begin_request(),
          error = function(e) {
            attr(e, "slot_released") <- TRUE
            release_active()
            stop(e)
          }
        )
      }
      if (isTRUE(state$concurrency$enabled)) {
        acquire_concurrency() %...>%
          (function(...) {
            if (isTRUE(state$aborted)) {
              release_concurrency()
              return(promises::promise_reject("aborted"))
            }
            safe_launch()
          })
      } else {
        safe_launch()
      }
    }

    # Perform request, optionally gated by client-side rate limiter
    prom <- schedule_call(
      function() start_request(),
      min_delay = 0,
      use_rate_limit = isTRUE(state$rate$enabled) && !isTRUE(skip_rpm)
    )

    release_once_factory <- function() {
      released <- FALSE
      function() {
        if (!released) {
          release_active()
          released <<- TRUE
        }
      }
    }

    prom %...>%
      (function(resp) {
        release_once <- release_once_factory()
        on.exit(release_once(), add = TRUE)
        if (is.null(resp)) {
          release_once()
          return(list(kind = "err", err = simpleError("NULL response")))
        }
        status <- tryCatch(
          httr2::resp_status(resp),
          error = function(e) NA_integer_
        )
        if (is.na(status) || status < 200L || status >= 300L) {
          if (!is.null(path_i) && fs::file_exists(path_i)) {
            fs::file_delete(path_i)
          }
          err <- tryCatch(
            httr2::resp_check_status(resp),
            error = function(e) e
          )
          release_once()
          attr(err, "slot_released") <- TRUE
          stop(err)
        }
        json <- NULL
        if (!is.null(path_i)) {
          json <- tryCatch(
            jsonlite::fromJSON(path_i, simplifyVector = FALSE),
            error = function(e) NULL
          )
          if (is.null(json)) {
            txt <- tryCatch(readr::read_file(path_i), error = function(e) NULL)
            if (rlang::is_string(txt) && nzchar(txt)) {
              repaired <- .repair_json_text(txt)
              if (!is.null(repaired)) {
                json <- tryCatch(
                  jsonlite::fromJSON(repaired, simplifyVector = FALSE),
                  error = function(e) NULL
                )
              }
            }
          }
        } else {
          json <- tryCatch(
            httr2::resp_body_json(resp, simplifyVector = FALSE),
            error = function(e) NULL
          )
          if (is.null(json)) {
            txt <- tryCatch(httr2::resp_body_string(resp), error = function(e) {
              NULL
            })
            if (rlang::is_string(txt) && nzchar(txt)) {
              repaired <- .repair_json_text(txt)
              if (!is.null(repaired)) {
                json <- tryCatch(
                  jsonlite::fromJSON(repaired, simplifyVector = FALSE),
                  error = function(e) NULL
                )
              }
            }
          }
        }
        if (is.null(json)) {
          err <- simpleError("Failed to parse JSON result")
          attr(err, "slot_released") <- TRUE
          release_once()
          stop(err)
        }
        release_once()
        list(kind = "ok", json = json, cached = FALSE)
      }) %...!%
      (function(err) {
        if (
          inherits(err, "promises_error") &&
            is.list(err) &&
            inherits(err$cnd, "condition")
        ) {
          err <- err$cnd
        }
        if (!isTRUE(attr(err, "slot_released"))) {
          release_active()
        }
        if (
          .should_retry_http(err) &&
            delay < backoff_cap &&
            !isTRUE(state$aborted)
        ) {
          wait <- .compute_backoff_wait(err, delay, backoff_cap)
          state$qs$retries <- state$qs$retries + 1L
          state$qs$max_delay <- max(state$qs$max_delay, wait)
          .log_q(state, "working", length(reqs))
          if (isTRUE(state$rate$enabled) && isTRUE(state$rate$halve_on_retry)) {
            state$rate$rpm <- max(
              state$rate$floor_rpm,
              as.integer(ceiling(state$rate$rpm / 2))
            )
          }
          return(schedule_call(
            function() {
              retry_one(
                i,
                delay = max(delay * 2, backoff_base),
                skip_rpm = TRUE
              )
            },
            min_delay = wait,
            use_rate_limit = FALSE
          ))
        }
        .write_error_payload(ids[[i]], err, state$error_dir)
        list(kind = "err", err = err)
      })
  }

  # Kick off all requests
  state$results <- vector("list", length(reqs))
  state$qs$pending <- length(reqs)
  .log_q(state, "init", length(reqs))
  # Create promise for each request
  prs <- lapply(seq_along(reqs), function(i) {
    # Start request and handle completion
    retry_one(i) %...>%
      (function(res) {
        # Completion counters updated in handlers; just store result here
        if (res$kind == "ok") {
          state$qs$success <- state$qs$success + 1L
        } else {
          state$qs$failed <- state$qs$failed + 1L
        }
        state$results[[i]] <- res
        .log_q(state, "working", length(reqs))
        res
      }) %...!%
      (function(err) {
        # Handle promise rejection
        state$qs$failed <- state$qs$failed + 1L
        state$results[[i]] <- list(kind = "err", err = err)
        .log_q(state, "working", length(reqs))
        err
      })
  })

  # Wait for all promises to complete
  waited <- .wait_blocking(
    promises::promise_all(.list = prs),
    abort_fun = abort_all
  )
  cat("\n")
  if (is.null(waited)) {
    attr(state$results, "aborted") <- TRUE
  }

  # Merge temp cache files into the global cache if enabled
  if (isTRUE(caching_enabled)) {
    state$global_cache <- .consolidate_temp_json_cache(
      temp_cache_dir,
      global_cache_path
    )
  }

  # Warn user if any error payloads were captured; include brief HTTP status
  # table
  err_files <- tryCatch(
    fs::dir_ls(state$error_dir, glob = "*.json"),
    error = \(e) character()
  )

  # Warn user if any error payloads were captured; include brief HTTP status
  # table
  if (!rlang::is_empty(err_files)) {
    # Build simple status table
    .read_err_status <- \(path) {
      tryCatch(
        purrr::pluck(
          jsonlite::read_json(path, simplifyVector = TRUE),
          "status",
          .default = NA_integer_
        ),
        error = \(e) NA_integer_
      )
    }

    statuses <- purrr::map_int(err_files, .read_err_status)
    tbl <- sort(table(statuses, useNA = "always"), decreasing = TRUE)

    # Format status table for warning message
    fmt <- sprintf("%s: %d", names(tbl), as.integer(tbl))
    cli::cli_alert_warning(
      "Some requests failed; diagnostics in: {state$error_dir}\nStatuses: {fmt}"
    )
  }
  # Attach error_dir for programmatic access
  attr(state$results, "error_dir") <- state$error_dir
  state$results
}
