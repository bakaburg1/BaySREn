#' Normalize embedding text before hashing
#'
#' Normalize text input before embedding and hashing to ensure cache hits remain
#' robust to minor formatting changes. The normalization consists of:
#' \itemize{
#'   \item Checking that the input is a single character string.
#'   \item Normalizing line endings by replacing Windows carriage returns (\code{\\r\\n} or \code{\\r})
#'         with standard Unix newlines (\code{\\n}).
#'   \item Squishing whitespace using \code{stringr::str_squish}, which trims leading and
#'         trailing whitespace and collapses consecutive internal whitespace characters into single spaces.
#' }
#'
#' @param text Character scalar.
#'
#' @return A trimmed single string.
#'
#' @keywords internal
.normalize_embedding_text <- function(text) {
  if (!rlang::is_string(text)) {
    cli::cli_abort("{.arg text} must be a single character string.")
  }

  text |>
    stringr::str_replace_all("\r\n?", "\n") |>
    stringr::str_squish()
}

#' Supported embedding model registry
#'
#' Each row defines the exact public label, provider routing, API model name,
#' and stable cache slug used in embedding cache file paths.
#'
#' @return A tibble of supported embedding models.
#'
#' @keywords internal
.supported_embedding_models <- function() {
  tibble::tibble(
    label = c(
      "cohere/embed-v4.0",
      "google/gemini-embedding-001",
      "google/gemini-embedding-2-preview",
      "perplexity/pplx-embed-v1-0.6b",
      "perplexity/pplx-embed-v1-4b"
    ),
    provider = c(
      "cohere",
      "gemini",
      "gemini",
      "openrouter",
      "openrouter"
    ),
    api_model = c(
      "embed-v4.0",
      "gemini-embedding-001",
      "gemini-embedding-2-preview",
      "perplexity/pplx-embed-v1-0.6b",
      "perplexity/pplx-embed-v1-4b"
    ),
    cache_model_slug = c(
      "cohere_embed_v4_0",
      "google_gemini_embedding_001",
      "google_gemini_embedding_2_preview",
      "perplexity_pplx_embed_v1_0.6b",
      "perplexity_pplx_embed_v1_4b"
    )
  )
}

#' Look up one supported embedding model by label
#'
#' @param label Public embedding model label.
#'
#' @return One registry row as a list.
#'
#' @keywords internal
.lookup_embedding_model <- function(label) {
  if (!rlang::is_string(label) || !nzchar(label)) {
    cli::cli_abort("{.arg label} must be a single non-empty string.")
  }

  registry <- .supported_embedding_models()
  row <- registry[registry$label == label, , drop = FALSE]
  if (!nrow(row)) {
    supported <- registry$label
    cli::cli_abort(c(
      "Unsupported embedding model {.val {label}}.",
      "i" = "Supported labels: {.val {supported}}."
    ))
  }
  as.list(row[1, , drop = FALSE])
}

#' Supported embedding text roles
#'
#' @return Character vector of supported text roles.
#'
#' @keywords internal
.embedding_text_roles <- function() {
  c("abstracts", "seed_abstracts", "criteria_items", "criteria_blocks")
}

#' Normalize an embedding text role
#'
#' @param text_role Requested text role.
#'
#' @return A validated text role.
#'
#' @keywords internal
.normalize_embedding_text_role <- function(text_role) {
  match.arg(text_role, choices = .embedding_text_roles())
}

#' Resolve an embedding API key
#'
#' @param provider Embedding provider label.
#'
#' @return A scalar API key.
#'
#' @keywords internal
get_embedding_api_key <- function(provider) {
  if (!rlang::is_string(provider) || !nzchar(provider)) {
    cli::cli_abort("{.arg provider} must be a single non-empty string.")
  }

  env_vars <- switch(
    provider,
    cohere = "COHERE_API_KEY",
    gemini = c("GEMINI_API_KEY", "GOOGLE_API_KEY"),
    openrouter = c("OPENROUTER_API_KEY", "OPENAI_API_KEY"),
    cli::cli_abort("Unsupported embedding provider {.val {provider}}.")
  )

  for (env_var in env_vars) {
    key <- Sys.getenv(env_var, unset = "")
    if (nzchar(key)) {
      return(key)
    }
  }

  readRenviron("~/.Renviron")
  for (env_var in env_vars) {
    key <- Sys.getenv(env_var, unset = "")
    if (nzchar(key)) {
      return(key)
    }
  }

  cli::cli_abort(
    "No API key found for embedding provider {.val {provider}} in {.var {env_vars}}."
  )
}

#' Normalize an embedding mode
#'
#' Embedding cache paths use the common modality names `query` and `document`.
#' Provider-specific request values are derived later so cache layout stays
#' shared across providers.
#'
#' @param embedding_mode Requested embedding mode.
#'
#' @return `"query"` or `"document"`.
#'
#' @keywords internal
.normalize_embedding_mode <- function(embedding_mode = c("document", "query")) {
  # Accept only the shared public mode names. Provider-specific values are
  # generated inside request builders, not accepted at cache boundaries.
  match.arg(embedding_mode)
}

#' Map common embedding modes to provider request fields
#'
#' @param provider Embedding provider name.
#' @param embedding_mode Normalized embedding mode.
#'
#' @return A list with `input_type` and `task_type` entries.
#'
#' @keywords internal
.embedding_provider_mode_fields <- function(provider, embedding_mode) {
  switch(
    provider,
    cohere = list(
      input_type = if (identical(embedding_mode, "query")) {
        "search_query"
      } else {
        "search_document"
      },
      task_type = NULL
    ),
    gemini = list(
      input_type = NULL,
      task_type = if (identical(embedding_mode, "query")) {
        "RETRIEVAL_QUERY"
      } else {
        "RETRIEVAL_DOCUMENT"
      }
    ),
    list(input_type = NULL, task_type = NULL)
  )
}

#' Build an embedding model specification
#'
#' @param embedding_model Embedding model label.
#' @param embedding_mode Common embedding mode: `query` or `document`.
#'
#' @return A normalized embedding model specification.
#'
#' @keywords internal
get_embedding_model_spec <- function(
  embedding_model,
  embedding_mode = c("document", "query")
) {
  if (!rlang::is_string(embedding_model) || !nzchar(embedding_model)) {
    cli::cli_abort("{.arg embedding_model} must be a single non-empty string.")
  }
  embedding_mode <- .normalize_embedding_mode(embedding_mode)
  registry_row <- .lookup_embedding_model(embedding_model)
  provider_mode <- .embedding_provider_mode_fields(
    registry_row$provider,
    embedding_mode
  )

  list(
    provider = registry_row$provider,
    model = registry_row$api_model,
    label = registry_row$label,
    cache_model_slug = registry_row$cache_model_slug,
    embedding_mode = embedding_mode,
    input_type = provider_mode$input_type,
    task_type = provider_mode$task_type
  )
}

#' Parse a Cohere embedding response
#'
#' @param response An `httr2` response.
#'
#' @return A numeric embedding matrix.
#'
#' @keywords internal
.parse_cohere_embedding_response <- function(response) {
  body <- jsonlite::fromJSON(
    httr2::resp_body_string(response),
    simplifyVector = FALSE
  )
  embeddings <- body$embeddings$float
  if (is.null(embeddings)) {
    embeddings <- body$embeddings
  }
  if (is.null(embeddings)) {
    message <- body$message
    if (is.null(message)) {
      message <- body$error$message
    }
    cli::cli_abort(
      message %||% "No embeddings field in Cohere response."
    )
  }

  embeddings <- lapply(embeddings, as.numeric)
  dims <- unique(lengths(embeddings))
  if (length(dims) != 1L) {
    cli::cli_abort(
      "Cohere embedding response returned inconsistent dimensions."
    )
  }

  matrix(
    unlist(embeddings, use.names = FALSE),
    nrow = length(embeddings),
    byrow = TRUE
  )
}

#' Parse an OpenAI-compatible embedding response
#'
#' @param response An `httr2` response.
#'
#' @return A numeric embedding matrix.
#'
#' @keywords internal
.parse_openai_embedding_response <- function(response) {
  body <- jsonlite::fromJSON(
    httr2::resp_body_string(response),
    simplifyVector = FALSE
  )
  if (is.null(body$data)) {
    message <- body$error$message
    cli::cli_abort(
      message %||% "No data field in embedding response."
    )
  }

  embeddings <- lapply(body$data, function(item) as.numeric(item$embedding))
  dims <- unique(lengths(embeddings))
  if (length(dims) != 1L) {
    cli::cli_abort("Embedding response returned inconsistent dimensions.")
  }

  matrix(
    unlist(embeddings, use.names = FALSE),
    nrow = length(embeddings),
    byrow = TRUE
  )
}

#' Parse a Gemini embedding response
#'
#' @param response An `httr2` response.
#'
#' @return A numeric embedding matrix.
#'
#' @keywords internal
.parse_gemini_embedding_response <- function(response) {
  body <- jsonlite::fromJSON(
    httr2::resp_body_string(response),
    simplifyVector = FALSE
  )
  embeddings <- body$embeddings
  if (is.null(embeddings)) {
    message <- body$error$message
    cli::cli_abort(
      message %||% "No embeddings field in Gemini response."
    )
  }

  embeddings <- lapply(embeddings, function(item) as.numeric(item$values))
  dims <- unique(lengths(embeddings))
  if (length(dims) != 1L) {
    cli::cli_abort("Gemini embedding response returned inconsistent dimensions.")
  }

  matrix(
    unlist(embeddings, use.names = FALSE),
    nrow = length(embeddings),
    byrow = TRUE
  )
}

#' Request Cohere embeddings
#'
#' @param texts Character vector of input texts.
#' @param spec Embedding model specification.
#' @param api_key Cohere API key.
#'
#' @return A numeric embedding matrix.
#'
#' @keywords internal
.request_cohere_embeddings <- function(texts, spec, api_key) {
  rlang::check_installed("httr2")

  body <- .make_cohere_embedding_body(texts = texts, spec = spec)
  response <- httr2::request("https://api.cohere.com/v2/embed") |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_timeout(180) |>
    httr2::req_perform()

  .parse_cohere_embedding_response(response)
}

#' Build a Cohere embedding request body
#'
#' Cohere expects `texts` to be a JSON array even when a batch has a single
#' cache miss. Wrapping each string in a list element prevents
#' `jsonlite::toJSON(..., auto_unbox = TRUE)` from serializing a single text as
#' a scalar string.
#'
#' @param texts Character vector of input texts.
#' @param spec Embedding model specification.
#'
#' @return A list suitable for `httr2::req_body_json()`.
#'
#' @keywords internal
.make_cohere_embedding_body <- function(texts, spec) {
  if (!is.character(texts) || !length(texts)) {
    cli::cli_abort("{.arg texts} must be a non-empty character vector.")
  }

  list(
    model = spec$model,
    texts = as.list(texts),
    input_type = spec$input_type %||% "search_document",
    embedding_types = list("float")
  )
}

#' Build a Gemini embedding request body
#'
#' Gemini uses `taskType` rather than Cohere's `input_type` to distinguish
#' query-like anchors from indexed documents.
#'
#' @param texts Character vector of input texts.
#' @param spec Embedding model specification.
#'
#' @return A list suitable for `httr2::req_body_json()`.
#'
#' @keywords internal
.make_gemini_embedding_body <- function(texts, spec) {
  if (!is.character(texts) || !length(texts)) {
    cli::cli_abort("{.arg texts} must be a non-empty character vector.")
  }

  model_path <- paste0("models/", spec$model)
  requests <- lapply(texts, function(text) {
    list(
      model = model_path,
      content = list(parts = list(list(text = text))),
      taskType = spec$task_type %||% "RETRIEVAL_DOCUMENT"
    )
  })

  list(requests = requests)
}

#' Request Gemini embeddings
#'
#' @param texts Character vector of input texts.
#' @param spec Embedding model specification.
#' @param api_key Gemini API key.
#'
#' @return A numeric embedding matrix.
#'
#' @keywords internal
.request_gemini_embeddings <- function(texts, spec, api_key) {
  rlang::check_installed("httr2")

  body <- .make_gemini_embedding_body(texts = texts, spec = spec)
  response <- httr2::request("https://generativelanguage.googleapis.com/v1beta") |>
    httr2::req_url_path_append(
      paste0("models/", spec$model, ":batchEmbedContents")
    ) |>
    httr2::req_url_query(key = api_key) |>
    httr2::req_headers(`Content-Type` = "application/json") |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_timeout(180) |>
    httr2::req_perform()

  .parse_gemini_embedding_response(response)
}

#' Request OpenAI-compatible embeddings
#'
#' @param texts Character vector of input texts.
#' @param spec Embedding model specification.
#' @param api_key API key.
#' @param base_url Provider base URL.
#'
#' @return A numeric embedding matrix.
#'
#' @keywords internal
.request_openai_compatible_embeddings <- function(
  texts,
  spec,
  api_key,
  base_url
) {
  rlang::check_installed("httr2")

  body <- list(
    model = spec$model,
    input = texts,
    encoding_format = "float"
  )
  if (!is.null(spec$input_type)) {
    body$input_type <- spec$input_type
  }

  response <- httr2::request(base_url) |>
    httr2::req_url_path_append("embeddings") |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `HTTP-Referer` = "http://localhost",
      `X-OpenRouter-Title` = "BaySREn abstract concentration"
    ) |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_timeout(180) |>
    httr2::req_perform()

  .parse_openai_embedding_response(response)
}

#' Build an embedding transport from a model specification
#'
#' @param embedding_model Embedding model label.
#' @param embedding_mode Common embedding mode: `query` or `document`.
#'
#' @return An embedding transport function with a `model_spec` attribute.
#'
#' @keywords internal
make_embedding_transport <- function(
  embedding_model,
  embedding_mode = c("document", "query")
) {
  spec <- get_embedding_model_spec(
    embedding_model,
    embedding_mode = embedding_mode
  )
  api_key <- get_embedding_api_key(spec$provider)

  transport <- switch(
    spec$provider,
    cohere = function(texts) {
      .request_cohere_embeddings(texts = texts, spec = spec, api_key = api_key)
    },
    gemini = function(texts) {
      .request_gemini_embeddings(texts = texts, spec = spec, api_key = api_key)
    },
    openrouter = function(texts) {
      .request_openai_compatible_embeddings(
        texts = texts,
        spec = spec,
        api_key = api_key,
        base_url = "https://openrouter.ai/api/v1"
      )
    },
    cli::cli_abort("Unsupported embedding provider {.val {spec$provider}}.")
  )

  attr(transport, "model_spec") <- spec
  transport
}

#' Create an embedding cache key
#'
#' @param text A single text string.
#'
#' @return A stable hash for the normalized input text.
#'
#' @keywords internal
create_embedding_cache_key <- function(text) {
  rlang::hash(.normalize_embedding_text(text))
}

#' Resolve the dataset-level embedding cache path
#'
#' @param embedding_model Embedding model label.
#' @param dataset Dataset label.
#' @param text_role Text role within the benchmark dataset.
#' @param embedding_mode Common embedding mode: `query` or `document`.
#' @param cache_root Official embedding cache root.
#'
#' @return A file path for the dataset embedding cache.
#'
#' @keywords internal
get_embedding_cache_path <- function(
  embedding_model,
  dataset,
  text_role = .embedding_text_roles(),
  embedding_mode = c("document", "query"),
  cache_root = here::here("cache", "embeddings")
) {
  if (!rlang::is_string(dataset) || !nzchar(dataset)) {
    cli::cli_abort("{.arg dataset} must be a single non-empty string.")
  }

  embedding_mode <- .normalize_embedding_mode(embedding_mode)
  text_role <- .normalize_embedding_text_role(text_role)
  model_spec <- get_embedding_model_spec(
    embedding_model = embedding_model,
    embedding_mode = embedding_mode
  )
  cache_file <- paste0(
    model_spec$cache_model_slug,
    "__",
    embedding_mode,
    "_embeddings.rds"
  )

  fs::path(cache_root, text_role, dataset, cache_file)
}

#' Create an empty embedding cache table
#'
#' @return A zero-row embedding cache tibble.
#'
#' @keywords internal
.empty_embedding_cache <- function() {
  tibble::tibble(
    dataset = character(),
    embedding_model = character(),
    embedding_mode = character(),
    text_role = character(),
    text_hash = character(),
    text = character(),
    embedding = list()
  )
}

#' Validate an embedding cache table
#'
#' @param cache Candidate cache table.
#' @param dataset Expected dataset label, or `NULL`.
#' @param embedding_model Expected embedding model label, or `NULL`.
#' @param text_role Expected text role, or `NULL`.
#' @param embedding_mode Expected embedding mode.
#'
#' @return A normalized embedding cache tibble.
#'
#' @keywords internal
.validate_embedding_cache <- function(
  cache,
  dataset = NULL,
  embedding_model = NULL,
  text_role = NULL,
  embedding_mode = NULL
) {
  if (!is.null(dataset) && (!rlang::is_string(dataset) || !nzchar(dataset))) {
    cli::cli_abort("{.arg dataset} must be `NULL` or a single non-empty string.")
  }

  if (!is.null(text_role)) {
    if (!rlang::is_string(text_role) || !nzchar(text_role)) {
      cli::cli_abort("{.arg text_role} must be `NULL` or a single non-empty string.")
    }
    text_role <- .normalize_embedding_text_role(text_role)
  }

  if (!is.null(embedding_mode)) {
    embedding_mode <- .normalize_embedding_mode(embedding_mode)
  }

  if (!is.null(embedding_model)) {
    embedding_model <- get_embedding_model_spec(
      embedding_model,
      embedding_mode = embedding_mode %||% "document"
    )$label
  }

  required <- c(
    "dataset",
    "embedding_model",
    "embedding_mode",
    "text_role",
    "text_hash",
    "text",
    "embedding"
  )
  missing <- setdiff(required, names(cache))
  if (length(missing)) {
    cli::cli_abort("Embedding cache is missing columns {.field {missing}}.")
  }

  out <- tibble::as_tibble(cache) |>
    dplyr::select(dplyr::all_of(required)) |>
    dplyr::mutate(
      dataset = as.character(.data$dataset),
      embedding_model = as.character(.data$embedding_model),
      embedding_mode = as.character(.data$embedding_mode),
      text_role = as.character(.data$text_role),
      text_hash = as.character(.data$text_hash),
      text = vapply(.data$text, .normalize_embedding_text, character(1))
    )

  if (!nrow(out)) {
    return(.empty_embedding_cache())
  }

  if (any(!nzchar(out$dataset)) || length(unique(out$dataset)) != 1L) {
    cli::cli_abort("Embedding cache must contain exactly one dataset value.")
  }
  if (
    any(!nzchar(out$embedding_model)) ||
      length(unique(out$embedding_model)) != 1L
  ) {
    cli::cli_abort("Embedding cache must contain exactly one model value.")
  }

  if (any(!out$embedding_mode %in% c("query", "document"))) {
    cli::cli_abort(
      "{.field embedding_mode} must contain only query or document."
    )
  }
  if (any(!out$text_role %in% .embedding_text_roles())) {
    cli::cli_abort("{.field text_role} contains an unsupported text role.")
  }

  if (!is.null(dataset) && !identical(unique(out$dataset), dataset)) {
    cli::cli_abort("Embedding cache dataset does not match {.val {dataset}}.")
  }
  if (
    !is.null(embedding_model) &&
      !identical(unique(out$embedding_model), embedding_model)
  ) {
    cli::cli_abort("Embedding cache model does not match the requested model.")
  }

  if (!is.null(text_role) && !identical(unique(out$text_role), text_role)) {
    cli::cli_abort("Embedding cache role does not match the requested role.")
  }

  if (!is.null(embedding_mode)) {
    if (!identical(unique(out$embedding_mode), embedding_mode)) {
      cli::cli_abort("Embedding cache mode does not match the requested mode.")
    }
  }

  expected_hash <- vapply(out$text, create_embedding_cache_key, character(1))
  if (!identical(unname(out$text_hash), unname(expected_hash))) {
    cli::cli_abort("Embedding cache contains stale or invalid text hashes.")
  }
  if (anyDuplicated(out$text_hash)) {
    cli::cli_abort("Embedding cache contains duplicated text hashes.")
  }

  if (!is.list(out$embedding)) {
    cli::cli_abort("{.field embedding} must be a list-column.")
  }
  out$embedding <- lapply(out$embedding, as.numeric)
  dims <- unique(lengths(out$embedding))
  if (length(dims) != 1L || dims[[1]] < 1L) {
    cli::cli_abort("Embedding cache vectors must share one positive dimension.")
  }

  out
}

#' Read one dataset-level embedding cache file
#'
#' @param path Cache file path.
#' @param dataset Expected dataset label.
#' @param embedding_model Expected embedding model label.
#' @param text_role Expected text role.
#' @param embedding_mode Expected embedding mode.
#'
#' @return A validated embedding cache tibble.
#'
#' @keywords internal
read_embedding_cache <- function(
  path,
  dataset,
  embedding_model,
  text_role,
  embedding_mode = c("document", "query")
) {
  if (!fs::file_exists(path)) {
    return(.empty_embedding_cache())
  }

  cache <- readr::read_rds(path)
  .validate_embedding_cache(
    cache,
    dataset = dataset,
    embedding_model = embedding_model,
    text_role = text_role,
    embedding_mode = embedding_mode
  )
}

#' Write one dataset-level embedding cache file
#'
#' @param cache Embedding cache tibble.
#' @param path Cache file path.
#' @param dataset Expected dataset label, or `NULL`.
#' @param embedding_model Expected embedding model label, or `NULL`.
#' @param text_role Expected text role, or `NULL`.
#' @param embedding_mode Expected embedding mode, or `NULL`.
#'
#' @return Invisible `path`.
#'
#' @keywords internal
write_embedding_cache <- function(
  cache,
  path,
  dataset = NULL,
  embedding_model = NULL,
  text_role = NULL,
  embedding_mode = NULL
) {
  cache <- .validate_embedding_cache(
    cache,
    dataset = dataset,
    embedding_model = embedding_model,
    text_role = text_role,
    embedding_mode = embedding_mode
  )
  cache_dir <- fs::path_dir(path)
  path_tmp <- fs::file_temp(
    pattern = fs::path_ext_remove(fs::path_file(path)),
    ext = "rds",
    tmp_dir = cache_dir
  )

  # Persist through a same-directory rename so readers never see partial files.
  fs::dir_create(cache_dir)
  saveRDS(cache, path_tmp)
  if (!file.rename(path_tmp, path)) {
    if (fs::file_exists(path_tmp)) {
      fs::file_delete(path_tmp)
    }
    cli::cli_abort("Failed to atomically write embedding cache to {.file {path}}.")
  }
  invisible(path)
}

#' Coerce an embedding result to a numeric matrix
#'
#' @param x Result returned by an embedding transport.
#'
#' @return A numeric matrix with one row per input text.
#'
#' @keywords internal
.coerce_embedding_matrix <- function(x) {
  if (is.null(x)) {
    cli::cli_abort("Embedding transport returned `NULL`.")
  }

  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }

  if (is.vector(x) && !is.list(x)) {
    x <- matrix(as.numeric(x), nrow = 1L)
  }

  if (!is.matrix(x) || !is.numeric(x)) {
    cli::cli_abort(
      "Embedding transport must return a numeric matrix or vector."
    )
  }

  x
}

#' Generate embeddings with a dataset-level cache-first policy
#'
#' Each dataset/model/mode cache file stores one row per unique normalized text.
#' Duplicate input texts are returned as duplicate matrix rows but are stored
#' once in the cache.
#'
#' @param texts Character vector of input texts.
#' @param embedding_model Embedding model label.
#' @param dataset Dataset label.
#' @param text_role Text role within the benchmark dataset.
#' @param embedding_mode Common embedding mode: `query` or `document`.
#' @param embedder Function that converts character vectors to an embedding
#'   matrix. When `NULL`, one is created with `make_embedding_transport()`.
#' @param cache_root Official embedding cache root.
#' @param batch_size Positive batch size for cache misses.
#' @param force Recompute embeddings even if cache files already exist.
#'
#' @return A numeric matrix with one row per text.
#'
#' @keywords internal
generate_embeddings <- function(
  texts,
  embedding_model,
  dataset,
  text_role = .embedding_text_roles(),
  embedding_mode = c("document", "query"),
  embedder = NULL,
  cache_root = here::here("cache", "embeddings"),
  batch_size = 64L,
  force = FALSE
) {
  if (!is.character(texts)) {
    cli::cli_abort("{.arg texts} must be a character vector.")
  }
  if (!length(texts)) {
    cli::cli_abort("{.arg texts} must contain at least one text.")
  }
  if (!rlang::is_string(dataset) || !nzchar(dataset)) {
    cli::cli_abort("{.arg dataset} must be a single non-empty string.")
  }
  if (!is.null(embedder) && !is.function(embedder)) {
    cli::cli_abort("{.arg embedder} must be `NULL` or a function.")
  }
  if (
    !is.numeric(batch_size) ||
      length(batch_size) != 1L ||
      is.na(batch_size) ||
      batch_size < 1
  ) {
    cli::cli_abort("{.arg batch_size} must be a positive whole number.")
  }

  # Normalize cache identity inputs once so hashing and transport calls agree.
  batch_size <- as.integer(batch_size)
  embedding_mode <- .normalize_embedding_mode(embedding_mode)
  text_role <- .normalize_embedding_text_role(text_role)
  model_spec <- get_embedding_model_spec(
    embedding_model = embedding_model,
    embedding_mode = embedding_mode
  )
  texts <- unname(vapply(texts, .normalize_embedding_text, character(1)))
  text_hashes <- unname(vapply(texts, create_embedding_cache_key, character(1)))

  # Read the one cache file for this dataset/model/mode branch.
  cache_path <- get_embedding_cache_path(
    embedding_model = embedding_model,
    dataset = dataset,
    text_role = text_role,
    embedding_mode = embedding_mode,
    cache_root = cache_root
  )
  cache <- read_embedding_cache(
    path = cache_path,
    dataset = dataset,
    embedding_model = embedding_model,
    text_role = text_role,
    embedding_mode = embedding_mode
  )

  # Force mode evicts only the requested text hashes, not unrelated cache rows.
  if (isTRUE(force)) {
    cache <- cache |>
      dplyr::filter(!.data$text_hash %in% unique(text_hashes))
  }

  # Compute only cache-missing unique text hashes.
  missing_hashes <- setdiff(unique(text_hashes), cache$text_hash)
  if (length(missing_hashes)) {
    unique_missing <- tibble::tibble(
      text_hash = text_hashes,
      text = texts
    ) |>
      dplyr::filter(.data$text_hash %in% .env$missing_hashes) |>
      dplyr::distinct(.data$text_hash, .keep_all = TRUE)

    # Delay API key discovery until actual API work is needed.
    if (is.null(embedder)) {
      embedder <- make_embedding_transport(
        model_spec$label,
        embedding_mode = embedding_mode
      )
    }

    batch_ids <- split(
      seq_len(nrow(unique_missing)),
      ceiling(seq_len(nrow(unique_missing)) / batch_size)
    )

    for (batch_id in batch_ids) {
      batch <- unique_missing[batch_id, , drop = FALSE]
      batch_embedding <- .coerce_embedding_matrix(embedder(batch$text))
      if (nrow(batch_embedding) != nrow(batch)) {
        cli::cli_abort("Embedding transport returned the wrong number of rows.")
      }

      # Store one vector per row in a list column, following the finalized cache
      # schema and avoiding any timestamp/provenance fields.
      cache <- dplyr::bind_rows(
        cache,
        tibble::tibble(
          dataset = dataset,
          embedding_model = embedding_model,
          embedding_mode = embedding_mode,
          text_role = text_role,
          text_hash = batch$text_hash,
          text = batch$text,
          embedding = unname(split(batch_embedding, row(batch_embedding)))
        )
      )
    }

    write_embedding_cache(
      cache = cache,
      path = cache_path,
      dataset = dataset,
      embedding_model = embedding_model,
      text_role = text_role,
      embedding_mode = embedding_mode
    )
  }

  # Re-read through validation after possible writes and restore input order.
  cache <- read_embedding_cache(
    path = cache_path,
    dataset = dataset,
    embedding_model = embedding_model,
    text_role = text_role,
    embedding_mode = embedding_mode
  )
  row_index <- match(text_hashes, cache$text_hash)
  if (anyNA(row_index)) {
    cli::cli_abort("Embedding cache is missing rows after generation.")
  }
  embeddings <- unname(cache$embedding[row_index])

  matrix_data <- do.call(rbind, embeddings)
  rownames(matrix_data) <- NULL
  matrix_data
}
