#' Normalize method cache components
#'
#' Recursively canonicalizes list-like structures to ensure stable cache hashing
#' regardless of key/field ordering. The normalization consists of:
#' \itemize{
#'   \item Returning \code{NULL}, atomic vectors, \code{Date}, or \code{POSIXt}
#'         objects directly as stable leaf values.
#'   \item Converting data frames to standard lists recursively.
#'   \item Sorting list elements lexicographically by their field names so that
#'         key-value structure serialization remains independent of the original
#'         insertion or input order, and recursively applying this normalization
#'         to each list element.
#' }
#'
#' @param x A list-like object, atomic vector, or scalar.
#'
#' @return A canonicalized copy of `x`.
#'
#' @keywords internal
.method_cache_normalize <- function(x) {
  # Leaf values are already stable enough to hash directly.
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
    return(.method_cache_normalize(as.list(x)))
  }

  # Sort named list fields so serialization does not depend on input order.
  if (is.list(x)) {
    nms <- names(x)
    if (!rlang::is_null(nms) && !rlang::is_empty(nms)) {
      ord <- order(nms)
      x <- x[ord]
      names(x) <- nms[ord]
    }
    return(lapply(x, .method_cache_normalize))
  }

  x
}

#' Drop runtime-only cache controls
#'
#' Removes solver arguments that should not change the cache identity because
#' they only affect how work is executed.
#'
#' @param dots Named list of solver arguments.
#'
#' @return A filtered copy of `dots`.
#'
#' @keywords internal
.method_cache_drop_runtime_controls <- function(dots) {
  # Nothing to filter when the forwarded argument list is empty.
  if (rlang::is_empty(dots)) {
    return(list())
  }

  drop_keys <- c(
    "batch_size",
    "parallelize_batches",
    "cache_mode",
    "cache_root",
    "cache_overlay_root",
    "cache_bucket",
    "cache_family",
    "cache_schema_version",
    "diagnostics_path",
    "log_dir",
    "inputs",
    "input",
    "prompts",
    "solver_chat",
    "chat"
  )

  dots[setdiff(names(dots), drop_keys)]
}

#' Extract a normalized provider spec for cache hashing
#'
#' @param provider An `ellmer` provider object or provider-like list.
#'
#' @return A normalized provider descriptor.
#'
#' @keywords internal
.method_cache_provider_spec <- function(provider) {
  # Preserve only the provider fields that influence the model behavior.
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
    .method_cache_normalize()
}

#' Represent a structured type for cache hashing
#'
#' @param type Structured type object or `NULL`.
#'
#' @return A JSON string or a fallback class marker.
#'
#' @keywords internal
.method_cache_type_repr <- function(type) {
  if (rlang::is_null(type)) {
    return(NULL)
  }

  tryCatch(
    jsonlite::toJSON(
      .method_cache_normalize(type),
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    ),
    error = function(e) paste(class(type), collapse = "::")
  )
}

#' Build a method cache specification
#'
#' The cache spec captures the solver identity before row-specific input
#' details are added.
#'
#' @param chat A resolved `ellmer` chat object.
#' @param type Structured type object or `NULL`.
#' @param dots Named list of forwarded solver parameters.
#' @param cache_family Cache family label.
#' @param cache_schema_version Cache schema version.
#' @param cache_failure Logical flag for whether terminal failures are cached.
#'
#' @return A normalized list describing the cache-relevant solver context.
#'
#' @keywords internal
method_cache_spec <- function(
  chat,
  type = NULL,
  dots = list(),
  cache_family = "method_llm_solver",
  cache_schema_version = 1L,
  cache_failure = TRUE
) {
  # Pull provider metadata defensively because provider objects vary by backend.
  provider <- tryCatch(chat$get_provider(), error = function(e) NULL)
  provider_spec <- .method_cache_provider_spec(provider)
  if (rlang::is_empty(provider_spec$name)) {
    provider_spec$name <- paste(class(provider), collapse = "::")
  }

  # Keep model and system-prompt identity available in the payload.
  model_name <- tryCatch(chat$get_model(), error = function(e) "unknown")
  system_prompt <- tryCatch(chat$get_system_prompt(), error = function(e) NULL)

  list(
    cache_family = as.character(cache_family)[1],
    provider = provider_spec,
    model = model_name,
    system_prompt = system_prompt,
    type = .method_cache_type_repr(type),
    solver_params = .method_cache_normalize(
      .method_cache_drop_runtime_controls(dots)
    ),
    cache_failure = isTRUE(cache_failure),
    cache_schema_version = as.integer(cache_schema_version)
  ) |>
    .method_cache_normalize()
}

#' Hash a method cache specification
#'
#' @param cache_spec Normalized cache spec from `method_cache_spec()`.
#'
#' @return A character scalar hash.
#'
#' @keywords internal
method_cache_namespace_hash <- function(cache_spec) {
  payload <- .method_cache_normalize(cache_spec)
  rlang::hash(
    jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", digits = NA)
  )
}

#' Build a row-level cache key
#'
#' Combines the namespace hash with row-specific input identity so each prompt
#' row can be restored independently.
#'
#' @param namespace_hash Stable namespace hash.
#' @param input_hash Stable hash of the rendered input string.
#' @param row_key Row key for the current prompt.
#' @param row_index Original row position in the solver input vector.
#'
#' @return A character scalar hash.
#'
#' @keywords internal
method_cache_row_key <- function(namespace_hash, input_hash, row_key, row_index) {
  rlang::hash(
    list(
      namespace_hash = namespace_hash,
      input_hash = input_hash,
      row_key = as.character(row_key),
      row_index = as.integer(row_index)
    )
  )
}

#' Build a cache row record
#'
#' @param input Prompt string.
#' @param row_index Original row position in the solver input vector.
#' @param row_key Row key for the current prompt.
#' @param input_hash Stable hash of the rendered input string.
#' @param cache_key Row-level cache key.
#' @param result Row result payload.
#' @param solver_chat Row chat object or structured payload.
#' @param solver_metadata Optional structured metadata.
#' @param attempt_count Number of live attempts used for this row.
#' @param failure_count Number of failed attempts recorded for this row.
#' @param failure_reason Character vector of failure reasons for the row.
#' @param status Row status label.
#' @param cache_hit Logical flag marking a restored row.
#' @param cache_source Cache source label.
#'
#' @return A cache row record list.
#'
#' @keywords internal
method_cache_row_record <- function(
  input,
  row_index,
  row_key,
  input_hash,
  cache_key,
  result,
  solver_chat,
  solver_metadata = NULL,
  attempt_count = 0L,
  failure_count = 0L,
  failure_reason = character(),
  status = c("pending", "success", "failed_retryable", "failed_final"),
  cache_hit = FALSE,
  cache_source = NA_character_
) {
  status <- match.arg(status)

  list(
    input = input,
    row_index = as.integer(row_index),
    row_key = as.character(row_key),
    input_hash = as.character(input_hash),
    cache_key = as.character(cache_key),
    result = result,
    solver_chat = solver_chat,
    solver_metadata = solver_metadata,
    attempt_count = as.integer(attempt_count),
    failure_count = as.integer(failure_count),
    failure_reason = as.character(failure_reason),
    status = status,
    cache_hit = isTRUE(cache_hit),
    cache_source = cache_source,
    updated_at = Sys.time()
  )
}

#' Build a cache payload
#'
#' @param cache_spec Normalized cache spec.
#' @param namespace_hash Stable namespace hash.
#' @param rows Named list of row records.
#' @param cache_schema_version Cache schema version.
#'
#' @return A cache payload list.
#'
#' @keywords internal
method_cache_payload <- function(
  cache_spec,
  namespace_hash,
  rows = list(),
  cache_schema_version = 1L
) {
  now <- Sys.time()
  rows <- purrr::discard(rows, is.null)

  # Store the normalized spec alongside the payload so the hash is inspectable.
  list(
    cache_spec = cache_spec,
    namespace_hash = namespace_hash,
    cache_schema_version = as.integer(cache_schema_version),
    created_at = now,
    updated_at = now,
    rows = rows
  )
}

#' Build a cache file path
#'
#' @param cache_root Cache root directory.
#' @param cache_bucket Cache bucket label such as `llm`.
#' @param cache_family Cache family label.
#' @param namespace_hash Stable namespace hash.
#'
#' @return A file path.
#'
#' @keywords internal
.method_cache_path <- function(
  cache_root,
  cache_bucket,
  cache_family,
  namespace_hash
) {
  cache_bucket <- gsub("[/\\\\]", ".", as.character(cache_bucket)[1])
  cache_family <- gsub("[/\\\\]", ".", as.character(cache_family)[1])

  fs::path(
    cache_root,
    cache_bucket,
    cache_family,
    paste0(namespace_hash, ".rds")
  )
}

#' Validate a cache payload
#'
#' Malformed payloads are treated as misses rather than errors.
#'
#' @param cache_payload Payload returned by `method_cache_read()`.
#' @param cache_spec Current normalized cache spec.
#' @param namespace_hash Current namespace hash.
#' @param cache_schema_version Cache schema version.
#'
#' @return `TRUE` when the payload is usable, otherwise `FALSE`.
#'
#' @keywords internal
.method_cache_validate <- function(
  cache_payload,
  cache_spec,
  namespace_hash,
  cache_schema_version = 1L
) {
  # Invalid or malformed payloads are treated as cache misses.
  if (rlang::is_null(cache_payload) || !is.list(cache_payload)) {
    return(FALSE)
  }

  # Namespace, schema version, and normalized spec must all match.
  if (!identical(cache_payload$namespace_hash, namespace_hash)) {
    return(FALSE)
  }

  if (
    !identical(
      cache_payload$cache_schema_version,
      as.integer(cache_schema_version)
    )
  ) {
    return(FALSE)
  }

  if (!identical(cache_payload$cache_spec, cache_spec)) {
    return(FALSE)
  }

  if (rlang::is_null(cache_payload$rows) || !is.list(cache_payload$rows)) {
    return(FALSE)
  }

  TRUE
}

#' Read a cache payload
#'
#' Official mode reads and writes `cache/`. Interim mode reads `cache/` first
#' and falls back to `experiments/cache/`.
#'
#' @param cache_spec Normalized cache spec from `method_cache_spec()`.
#' @param cache_mode Cache mode: `official`, `interim`, or `none`.
#' @param cache_root Official cache root.
#' @param cache_overlay_root Interim cache root.
#' @param cache_bucket Cache bucket label such as `llm`.
#' @param cache_schema_version Cache schema version.
#'
#' @return A list with `payload`, `path`, `root`, and `source`, or `NULL`.
#'
#' @keywords internal
method_cache_read <- function(
  cache_spec,
  cache_mode = c("official", "interim", "none"),
  cache_root = "cache",
  cache_overlay_root = file.path("experiments", "cache"),
  cache_bucket = "llm",
  cache_schema_version = 1L
) {
  cache_mode <- match.arg(cache_mode)
  if (identical(cache_mode, "none")) {
    return(NULL)
  }

  namespace_hash <- method_cache_namespace_hash(cache_spec)
  roots <- if (identical(cache_mode, "official")) {
    list(
      list(root = cache_root, source = "official")
    )
  } else {
    list(
      list(root = cache_root, source = "official"),
      list(root = cache_overlay_root, source = "overlay")
    )
  }

  for (root_spec in roots) {
    path <- .method_cache_path(
      cache_root = root_spec$root,
      cache_bucket = cache_bucket,
      cache_family = cache_spec$cache_family,
      namespace_hash = namespace_hash
    )

    # Return a miss instead of throwing when the cache file is absent.
    if (!fs::file_exists(path)) {
      next
    }

    payload <- tryCatch(readRDS(path), error = function(e) NULL)
    if (.method_cache_validate(
      payload,
      cache_spec = cache_spec,
      namespace_hash = namespace_hash,
      cache_schema_version = cache_schema_version
    )) {
      return(list(
        payload = payload,
        path = path,
        root = root_spec$root,
        source = root_spec$source,
        namespace_hash = namespace_hash
      ))
    }
  }

  NULL
}

#' Write a cache payload atomically
#'
#' @param cache_payload Payload to persist.
#' @param cache_spec Normalized cache spec from `method_cache_spec()`.
#' @param cache_mode Cache mode: `official`, `interim`, or `none`.
#' @param cache_root Official cache root.
#' @param cache_overlay_root Interim cache root.
#' @param cache_bucket Cache bucket label such as `llm`.
#'
#' @return Invisible `NULL`.
#'
#' @keywords internal
method_cache_write <- function(
  cache_payload,
  cache_spec,
  cache_mode = c("official", "interim", "none"),
  cache_root = "cache",
  cache_overlay_root = file.path("experiments", "cache"),
  cache_bucket = "llm"
) {
  cache_mode <- match.arg(cache_mode)
  if (identical(cache_mode, "none")) {
    return(invisible(NULL))
  }

  if (!is.list(cache_payload)) {
    cli::cli_abort("{.arg cache_payload} must be a list.")
  }

  namespace_hash <- cache_payload$namespace_hash
  if (rlang::is_null(namespace_hash)) {
    namespace_hash <- method_cache_namespace_hash(cache_spec)
    cache_payload$namespace_hash <- namespace_hash
  }

  cache_payload$cache_spec <- cache_spec
  cache_payload$cache_schema_version <- if (!is.null(cache_payload$cache_schema_version)) {
    as.integer(cache_payload$cache_schema_version)
  } else if (!is.null(cache_spec$cache_schema_version)) {
    as.integer(cache_spec$cache_schema_version)
  } else {
    1L
  }
  cache_payload$updated_at <- Sys.time()

  write_root <- if (identical(cache_mode, "official")) {
    cache_root
  } else {
    cache_overlay_root
  }

  path <- .method_cache_path(
    cache_root = write_root,
    cache_bucket = cache_bucket,
    cache_family = cache_spec$cache_family,
    namespace_hash = namespace_hash
  )

  # Create the parent directory lazily, then persist the payload atomically
  # through a same-directory rename.
  fs::dir_create(fs::path_dir(path))
  tmp_path <- tempfile(
    pattern = paste0(fs::path_ext_remove(fs::path_file(path)), "-"),
    tmpdir = fs::path_dir(path),
    fileext = ".rds"
  )
  on.exit(
    if (file.exists(tmp_path)) unlink(tmp_path),
    add = TRUE
  )

  saveRDS(cache_payload, tmp_path)
  if (!file.rename(tmp_path, path)) {
    cli::cli_abort("Failed to atomically write cache payload to {.file {path}}.")
  }

  invisible(NULL)
}
