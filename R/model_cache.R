#' Normalize cache spec components
#'
#' Recursively sorts named lists so the serialized cache spec is stable before
#' hashing or persistence.
#'
#' @param x A list-like object, atomic vector, or scalar.
#'
#' @return A canonicalized copy of `x`.
#'
#' @keywords internal
.caching_normalize <- function(x) {
  # Treat leaf values as already canonical.
  if (
    rlang::is_null(x) ||
      is.atomic(x) ||
      inherits(x, "Date") ||
      inherits(x, "POSIXt")
  ) {
    return(x)
  }

  # Convert data frames to lists before recursive normalization.
  if (inherits(x, "data.frame")) {
    return(.caching_normalize(as.list(x)))
  }

  # Sort named list elements so hashing is stable across field order changes.
  if (is.list(x)) {
    nms <- names(x)
    if (!rlang::is_null(nms) && !rlang::is_empty(nms)) {
      ord <- order(nms)
      x <- x[ord]
      names(x) <- nms[ord]
    }
    return(lapply(x, .caching_normalize))
  }

  x
}

#' Drop execution-only controls from cache settings
#'
#' @param dots Named list of solver arguments.
#'
#' @return A filtered copy of `dots`.
#'
#' @keywords internal
.caching_drop_runtime_controls <- function(dots) {
  # Nothing to filter when the forwarded argument list is empty.
  if (rlang::is_empty(dots)) {
    return(list())
  }

  drop_keys <- c(
    "batch_size",
    "parallelize_batches",
    "cache_dir",
    "max_retries",
    "max_retry",
    "rpm",
    "max_active",
    "convert",
    "include_tokens",
    "include_cost",
    "on_error",
    "view",
    "solver_chat",
    "solver",
    "inputs",
    "input",
    "prompts"
  )

  dots[setdiff(names(dots), drop_keys)]
}

#' Extract the cache-relevant provider configuration
#'
#' @param provider An `ellmer` provider object or provider-like list.
#'
#' @return A normalized provider descriptor.
#'
#' @keywords internal
.caching_provider_spec <- function(provider) {
  # Preserve only the provider fields that change the solver behaviour.
  if (rlang::is_null(provider)) {
    return(list())
  }

  provider_name <- tryCatch(provider$name, error = function(e) NULL)
  if (rlang::is_null(provider_name)) {
    provider_name <- tryCatch(provider@name, error = function(e) NULL)
  }

  base_url <- tryCatch(provider$base_url, error = function(e) NULL)
  if (rlang::is_null(base_url)) {
    base_url <- tryCatch(provider@base_url, error = function(e) NULL)
  }

  params <- tryCatch(provider$params, error = function(e) NULL)
  if (rlang::is_null(params)) {
    params <- tryCatch(provider@params, error = function(e) NULL)
  }

  extra_args <- tryCatch(provider$extra_args, error = function(e) NULL)
  if (rlang::is_null(extra_args)) {
    extra_args <- tryCatch(provider@extra_args, error = function(e) NULL)
  }

  list(
    name = provider_name,
    base_url = base_url,
    params = params,
    extra_args = extra_args
  ) |>
    .caching_normalize()
}

#' Resolve a stable cache spec for llm_solver()
#'
#' @param chat A resolved `ellmer` chat object.
#' @param type Structured type object or `NULL`.
#' @param dots Named list of forwarded solver parameters.
#' @param cache_failure Logical flag for whether terminal failures are cached.
#'
#' @return A normalized list describing the cache-relevant solver context.
#'
#' @keywords internal
.caching_spec <- function(chat, type, dots, cache_failure = TRUE) {
  # Pull provider metadata defensively because provider objects vary by backend.
  provider <- tryCatch(chat$get_provider(), error = function(e) NULL)
  provider_spec <- .caching_provider_spec(provider)
  if (rlang::is_empty(provider_spec$name)) {
    provider_spec$name <- paste(class(provider), collapse = "::")
  }

  # Keep model and system-prompt identity available in the payload.
  model_name <- tryCatch(chat$get_model(), error = function(e) "unknown")
  system_prompt <- tryCatch(chat$get_system_prompt(), error = function(e) NULL)
  type_repr <- if (rlang::is_null(type)) {
    NULL
  } else {
    tryCatch(
      jsonlite::toJSON(type, auto_unbox = TRUE, null = "null", digits = NA),
      error = function(e) paste(class(type), collapse = "::")
    )
  }

  list(
    model = model_name,
    provider = provider_spec,
    system_prompt = system_prompt,
    type = type_repr,
    dots = .caching_normalize(.caching_drop_runtime_controls(dots)),
    cache_failure = isTRUE(cache_failure)
  ) |>
    .caching_normalize()
}

#' Hash a cache spec into a namespace key
#'
#' @param cache_spec Normalized cache spec from `.caching_spec()`.
#' @param spec_version Integer cache schema version.
#'
#' @return A character scalar hash.
#'
#' @keywords internal
.caching_namespace_hash <- function(cache_spec, spec_version = 3L) {
  payload <- .caching_normalize(list(
    spec_version = as.integer(spec_version),
    cache_spec = cache_spec
  ))
  rlang::hash(
    jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", digits = NA)
  )
}

#' Build a row-level cache record
#'
#' @param input Prompt string.
#' @param row_index Original row position in the solver input vector.
#' @param row_id Optional external row identifier.
#' @param result Row result string or `NA_character_`.
#' @param solver_chat Row chat object.
#' @param solver_metadata Optional structured metadata.
#' @param failure_count Row failure count.
#' @param failure_reason Character vector of failure reasons for the row.
#' @param status Row status label.
#'
#' @return A cache row record list.
#'
#' @keywords internal
.caching_row_record <- function(
  input,
  row_index,
  row_id = NULL,
  result,
  solver_chat,
  solver_metadata = NULL,
  failure_count = 0L,
  failure_reason = character(),
  status = c("success", "failed_retryable", "failed_final")
) {
  status <- match.arg(status)

  # Keep failure history strictly character and strip empty values.
  failure_reason <- .normalize_failure_reason_history(failure_reason)

  list(
    input = input,
    row_index = as.integer(row_index),
    row_id = row_id,
    result = result,
    solver_chat = solver_chat,
    solver_metadata = solver_metadata,
    failure_count = as.integer(failure_count),
    failure_reason = failure_reason,
    status = status
  )
}

#' Build a cache payload
#'
#' @param cache_spec Normalized cache spec.
#' @param namespace_hash Stable namespace hash.
#' @param rows Named list of row records.
#' @param spec_version Cache schema version.
#'
#' @return A cache payload list.
#'
#' @keywords internal
.caching_payload <- function(
  cache_spec,
  namespace_hash,
  rows = list(),
  spec_version = 3L
) {
  now <- Sys.time()
  rows <- purrr::discard(rows, is.null)

  # Store the normalized spec alongside the payload so the hash is inspectable.
  list(
    cache_spec = cache_spec,
    namespace_hash = namespace_hash,
    spec_version = as.integer(spec_version),
    created_at = now,
    updated_at = now,
    rows = rows
  )
}

#' Resolve the stable cache path
#'
#' @param cache_dir Cache root directory.
#' @param cache_spec Normalized cache spec from `.caching_spec()`.
#' @param namespace_hash Stable namespace hash.
#'
#' @return A file path.
#'
#' @keywords internal
.caching_namespace_labels <- function(cache_spec) {
  # Fall back to generic labels when provider/model metadata is absent.
  provider_label <- if (rlang::is_empty(cache_spec$provider)) {
    "provider"
  } else {
    cache_spec$provider$name %||% "provider"
  }
  model_label <- if (rlang::is_empty(cache_spec$model)) {
    "model"
  } else {
    cache_spec$model[[1]]
  }

  # Preserve provider/model naming while replacing path separators so the
  # cache and related runtime artifacts stay flat under one directory.
  provider_label <- gsub("/", ".", as.character(provider_label), fixed = TRUE)
  model_label <- gsub("/", ".", as.character(model_label), fixed = TRUE)

  list(
    provider = as.character(provider_label),
    model = as.character(model_label)
  )
}

#' Resolve the stable cache path
#'
#' @param cache_dir Cache root directory.
#' @param cache_spec Normalized cache spec from `.caching_spec()`.
#' @param namespace_hash Stable namespace hash.
#'
#' @return A file path.
#'
#' @keywords internal
.caching_stable_path <- function(cache_dir, cache_spec, namespace_hash) {
  labels <- .caching_namespace_labels(cache_spec)

  fs::path(
    cache_dir,
    paste0(labels$provider, "_", labels$model, "_", namespace_hash, ".rds")
  )
}

#' Read a cache payload
#'
#' @param cache_dir Cache root directory.
#' @param cache_spec Normalized cache spec from `.caching_spec()`.
#' @param namespace_hash Stable namespace hash.
#'
#' @return A cache payload list or `NULL`.
#'
#' @keywords internal
.caching_read <- function(
  cache_dir,
  cache_spec,
  namespace_hash
) {
  path <- .caching_stable_path(cache_dir, cache_spec, namespace_hash)

  # Return a miss instead of throwing when the cache file is absent.
  if (!fs::file_exists(path)) {
    return(NULL)
  }

  tryCatch(readRDS(path), error = function(e) NULL)
}

#' Validate a cache payload against the current cache spec
#'
#' @param cache_payload Payload returned by `.caching_read()`.
#' @param cache_spec Current normalized cache spec.
#' @param namespace_hash Current namespace hash.
#' @param spec_version Cache schema version.
#'
#' @return `TRUE` when the payload is usable, otherwise `FALSE`.
#'
#' @keywords internal
.caching_validate <- function(
  cache_payload,
  cache_spec,
  namespace_hash,
  spec_version = 3L
) {
  # Invalid or malformed payloads are treated as cache misses.
  if (rlang::is_null(cache_payload) || !is.list(cache_payload)) {
    return(FALSE)
  }

  # Namespace, schema version, and normalized spec must all match.
  if (!identical(cache_payload$namespace_hash, namespace_hash)) {
    return(FALSE)
  }

  if (!identical(cache_payload$spec_version, as.integer(spec_version))) {
    return(FALSE)
  }

  if (!identical(cache_payload$cache_spec, cache_spec)) {
    return(FALSE)
  }

  TRUE
}

#' Normalize a failure-history vector
#'
#' @param failure_reason Failure history from solver telemetry or cache rows.
#'
#' @return A character vector with empty values removed.
#'
#' @keywords internal
.normalize_failure_reason_history <- function(failure_reason) {
  if (is.null(failure_reason)) {
    return(character())
  }

  if (is.list(failure_reason)) {
    failure_reason <- unlist(failure_reason, use.names = FALSE)
  }

  failure_reason <- as.character(failure_reason)
  failure_reason[!is.na(failure_reason) & nzchar(failure_reason)]
}

#' Write a cache payload
#'
#' @param cache_dir Cache root directory.
#' @param cache_spec Normalized cache spec from `.caching_spec()`.
#' @param namespace_hash Stable namespace hash.
#' @param cache_payload Payload to persist.
#'
#' @return Invisible `NULL`.
#'
#' @keywords internal
.caching_write <- function(
  cache_dir,
  cache_spec,
  namespace_hash,
  cache_payload
) {
  path <- .caching_stable_path(cache_dir, cache_spec, namespace_hash)
  path_tmp <- fs::file_temp(
    pattern = fs::path_ext_remove(fs::path_file(path)),
    ext = "rds",
    tmp_dir = cache_dir
  )

  # Create the parent directory lazily, then persist the payload atomically
  # through a same-directory rename.
  fs::dir_create(cache_dir)
  saveRDS(cache_payload, path_tmp)
  if (!file.rename(path_tmp, path)) {
    cli::cli_abort("Failed to atomically write cache payload to {.file {path}}.")
  }
  invisible(NULL)
}
