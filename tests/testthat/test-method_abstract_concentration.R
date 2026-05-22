test_that("abstract concentration discovers datasets with criteria", {
  data_dir <- withr::local_tempdir()
  save(list = character(), file = file.path(data_dir, "alpha.rda"))
  save(list = character(), file = file.path(data_dir, "alpha_criteria.rda"))
  save(list = character(), file = file.path(data_dir, "beta.rda"))
  save(list = character(), file = file.path(data_dir, "orphan_criteria.rda"))

  out <- get_datasets(data_dir)

  expect_identical(out$dataset, "alpha")
  expect_true(all(c("data_path", "criteria_path") %in% names(out)))
})

test_that("get_dataset loads paired packaged data and criteria objects", {
  data_dir <- withr::local_tempdir()
  alpha <- tibble::tibble(title = "a", abstract = "b", keywords = "c")
  alpha_criteria <- list(include = "include", exclude = "exclude")
  save(alpha, file = file.path(data_dir, "alpha.rda"))
  save(alpha_criteria, file = file.path(data_dir, "alpha_criteria.rda"))

  out <- get_dataset("alpha", data_dir)

  expect_s3_class(out$data, "tbl_df")
  expect_identical(out$criteria, alpha_criteria)
})

test_that("embedding cache keys depend only on normalized text", {
  same_a <- create_embedding_cache_key("  Hello   world  ")
  same_b <- create_embedding_cache_key("Hello world")
  diff_text <- create_embedding_cache_key("Hello world!")

  expect_identical(same_a, same_b)
  expect_false(identical(same_a, diff_text))
})

test_that("Cohere embedding body preserves single-text arrays", {
  spec <- get_embedding_model_spec(
    "cohere/embed-v4.0",
    embedding_mode = "query"
  )
  body <- .make_cohere_embedding_body("short query", spec)
  encoded <- jsonlite::fromJSON(
    jsonlite::toJSON(body, auto_unbox = TRUE),
    simplifyVector = FALSE
  )

  expect_type(encoded$texts, "list")
  expect_identical(encoded$texts[[1]], "short query")
  expect_identical(encoded$input_type, "search_query")
})

test_that("Gemini embedding specs map common modes to native task type", {
  query_spec <- get_embedding_model_spec(
    "google/gemini-embedding-001",
    embedding_mode = "query"
  )
  document_spec <- get_embedding_model_spec(
    "google/gemini-embedding-001",
    embedding_mode = "document"
  )
  preview_spec <- get_embedding_model_spec(
    "google/gemini-embedding-2-preview",
    embedding_mode = "document"
  )
  body <- .make_gemini_embedding_body("short query", query_spec)
  encoded <- jsonlite::fromJSON(
    jsonlite::toJSON(body, auto_unbox = TRUE),
    simplifyVector = FALSE
  )

  expect_identical(query_spec$provider, "gemini")
  expect_identical(query_spec$task_type, "RETRIEVAL_QUERY")
  expect_identical(document_spec$task_type, "RETRIEVAL_DOCUMENT")
  expect_identical(preview_spec$model, "gemini-embedding-2-preview")
  expect_identical(preview_spec$cache_model_slug, "google_gemini_embedding_2_preview")
  expect_identical(encoded$requests[[1]]$taskType, "RETRIEVAL_QUERY")
  expect_identical(
    encoded$requests[[1]]$content$parts[[1]]$text,
    "short query"
  )
})

test_that("unsupported embedding models abort with supported labels", {
  expect_error(
    get_embedding_model_spec("google/not-an-embedding"),
    "Supported labels"
  )
  expect_error(
    get_embedding_cache_path(
      "google/not-an-embedding",
      dataset = "Gastaldi",
      text_role = "abstracts",
      embedding_mode = "document",
      cache_root = withr::local_tempdir()
    ),
    "Supported labels"
  )
})

test_that("embedding model labels must be scalar strings", {
  vector_models <- c("cohere/embed-v4.0", "google/gemini-embedding-001")
  cache_root <- withr::local_tempdir()

  expect_error(
    get_embedding_model_spec(vector_models),
    "single non-empty string"
  )
  expect_error(
    get_embedding_cache_path(
      vector_models,
      dataset = "Gastaldi",
      text_role = "abstracts",
      embedding_mode = "document",
      cache_root = cache_root
    ),
    "single non-empty string"
  )
})

test_that("write_embedding_cache validates destination identity when provided", {
  cache_root <- withr::local_tempdir()
  cache_path <- get_embedding_cache_path(
    "cohere/embed-v4.0",
    dataset = "alpha",
    text_role = "abstracts",
    embedding_mode = "document",
    cache_root = cache_root
  )
  cache <- tibble::tibble(
    dataset = "alpha",
    embedding_model = "cohere/embed-v4.0",
    embedding_mode = "document",
    text_role = "abstracts",
    text_hash = create_embedding_cache_key("alpha"),
    text = "alpha",
    embedding = list(c(1, 2))
  )

  expect_error(
    write_embedding_cache(
      cache,
      cache_path,
      dataset = "alpha",
      embedding_model = "google/gemini-embedding-001",
      text_role = "abstracts",
      embedding_mode = "document"
    ),
    "model does not match"
  )
  expect_error(
    write_embedding_cache(
      cache,
      cache_path,
      dataset = "beta",
      embedding_model = "cohere/embed-v4.0",
      text_role = "abstracts",
      embedding_mode = "document"
    ),
    "dataset does not match"
  )
  expect_error(
    write_embedding_cache(
      cache,
      cache_path,
      dataset = "alpha",
      embedding_model = "cohere/embed-v4.0",
      text_role = "seed_abstracts",
      embedding_mode = "document"
    ),
    "role does not match"
  )
  expect_error(
    write_embedding_cache(
      cache,
      cache_path,
      dataset = "alpha",
      embedding_model = "cohere/embed-v4.0",
      text_role = "abstracts",
      embedding_mode = "query"
    ),
    "mode does not match"
  )
})

test_that("write_embedding_cache rejects non-scalar expected identity values", {
  cache_root <- withr::local_tempdir()
  cache_path <- get_embedding_cache_path(
    "cohere/embed-v4.0",
    dataset = "alpha",
    text_role = "abstracts",
    embedding_mode = "document",
    cache_root = cache_root
  )
  cache <- tibble::tibble(
    dataset = "alpha",
    embedding_model = "cohere/embed-v4.0",
    embedding_mode = "document",
    text_role = "abstracts",
    text_hash = create_embedding_cache_key("alpha"),
    text = "alpha",
    embedding = list(c(1, 2))
  )

  expect_error(
    write_embedding_cache(
      cache,
      cache_path,
      dataset = "alpha",
      embedding_model = c("cohere/embed-v4.0", "google/gemini-embedding-001"),
      text_role = "abstracts",
      embedding_mode = "document"
    ),
    "single non-empty string"
  )
  expect_error(
    write_embedding_cache(
      cache,
      cache_path,
      dataset = c("alpha", "beta"),
      embedding_model = "cohere/embed-v4.0",
      text_role = "abstracts",
      embedding_mode = "document"
    ),
    "single non-empty string"
  )
  expect_error(
    write_embedding_cache(
      cache,
      cache_path,
      dataset = "alpha",
      embedding_model = "cohere/embed-v4.0",
      text_role = c("abstracts", "seed_abstracts"),
      embedding_mode = "document"
    ),
    "single non-empty string"
  )
  expect_error(
    write_embedding_cache(
      cache,
      cache_path,
      dataset = "alpha",
      embedding_model = "google/not-an-embedding",
      text_role = "abstracts",
      embedding_mode = "document"
    ),
    "Supported labels"
  )
  expect_error(
    write_embedding_cache(
      cache,
      cache_path,
      dataset = "alpha",
      embedding_model = "cohere/embed-v4.0",
      text_role = "bad_role",
      embedding_mode = "document"
    ),
    "should be one of"
  )
})

test_that("embedding cache source avoids removed helpers and dead transport branches", {
  cache_source <- readLines(
    testthat::test_path("..", "..", "R", "method_embedding_cache.R"),
    warn = FALSE
  )
  cache_text <- paste(cache_source, collapse = "\n")

  expect_false(grepl(".normalize_embedding_cache_value <- function", cache_text, fixed = TRUE))
  expect_false(grepl(".model_slug <- function", cache_text, fixed = TRUE))
  expect_false(grepl("ragnar::embed_openai", cache_text, fixed = TRUE))
  expect_false(grepl("openai = function", cache_text, fixed = TRUE))
})

test_that("architecture documents current embedding and seed cache paths", {
  architecture <- readLines(
    testthat::test_path("..", "..", "ARCHITECTURE.md"),
    warn = FALSE
  )
  architecture_text <- paste(architecture, collapse = "\n")

  expect_match(
    architecture_text,
    "cache/embeddings/<text_role>/<dataset>/<cache_model_slug>__<document\\|query>_embeddings\\.rds"
  )
  expect_match(architecture_text, "cache/llm/seed_abstracts")
  expect_false(grepl("cache/llm/synthetic_seed_banks", architecture_text, fixed = TRUE))
})

test_that("embedding cache paths include text role, dataset, model slug, and mode", {
  cache_root <- withr::local_tempdir()

  query_path <- get_embedding_cache_path(
    "cohere/embed-v4.0",
    dataset = "Gastaldi",
    text_role = "seed_abstracts",
    embedding_mode = "query",
    cache_root = cache_root
  )
  document_path <- get_embedding_cache_path(
    "cohere/embed-v4.0",
    dataset = "Gastaldi",
    text_role = "abstracts",
    embedding_mode = "document",
    cache_root = cache_root
  )
  perplexity_path <- get_embedding_cache_path(
    "perplexity/pplx-embed-v1-0.6b",
    dataset = "Gastaldi",
    text_role = "seed_abstracts",
    embedding_mode = "document",
    cache_root = cache_root
  )

  expect_match(
    query_path,
    "seed_abstracts/Gastaldi/cohere_embed_v4_0__query_embeddings[.]rds$"
  )
  expect_match(
    document_path,
    "abstracts/Gastaldi/cohere_embed_v4_0__document_embeddings[.]rds$"
  )
  expect_match(
    perplexity_path,
    "seed_abstracts/Gastaldi/perplexity_pplx_embed_v1_0\\.6b__document_embeddings[.]rds$"
  )
})

test_that("write_embedding_cache accepts query-mode caches", {
  cache_root <- withr::local_tempdir()
  cache_path <- get_embedding_cache_path(
    "cohere/embed-v4.0",
    dataset = "alpha",
    text_role = "seed_abstracts",
    embedding_mode = "query",
    cache_root = cache_root
  )
  cache <- tibble::tibble(
    dataset = "alpha",
    embedding_model = "cohere/embed-v4.0",
    embedding_mode = "query",
    text_role = "seed_abstracts",
    text_hash = create_embedding_cache_key("query text"),
    text = "query text",
    embedding = list(c(0.1, 0.2))
  )

  write_embedding_cache(cache, cache_path)
  out <- read_embedding_cache(
    path = cache_path,
    dataset = "alpha",
    embedding_model = "cohere/embed-v4.0",
    text_role = "seed_abstracts",
    embedding_mode = "query"
  )

  expect_identical(out$embedding_mode, "query")
})

test_that("generate_embeddings writes and reads query-mode caches", {
  cache_root <- withr::local_tempdir()
  embedder <- function(texts) {
    matrix(c(1, 2), nrow = length(texts), ncol = 2, byrow = TRUE)
  }

  out <- generate_embeddings(
    texts = "query text",
    embedding_model = "cohere/embed-v4.0",
    dataset = "alpha",
    text_role = "seed_abstracts",
    embedding_mode = "query",
    embedder = embedder,
    cache_root = cache_root
  )
  cache_path <- get_embedding_cache_path(
    "cohere/embed-v4.0",
    dataset = "alpha",
    text_role = "seed_abstracts",
    embedding_mode = "query",
    cache_root = cache_root
  )
  cached <- read_embedding_cache(
    path = cache_path,
    dataset = "alpha",
    embedding_model = "cohere/embed-v4.0",
    text_role = "seed_abstracts",
    embedding_mode = "query"
  )

  expect_identical(dim(out), c(1L, 2L))
  expect_identical(cached$embedding_mode, "query")
})

test_that("generate_embeddings caches by dataset, text role, text, model, and mode", {
  cache_root <- withr::local_tempdir()
  calls <- 0L
  embedder <- function(texts) {
    calls <<- calls + 1L
    matrix(seq_along(texts), ncol = 1L)
  }

  first <- generate_embeddings(
    texts = c(" a ", "b"),
    embedding_model = "cohere/embed-v4.0",
    dataset = "alpha",
    text_role = "abstracts",
    embedding_mode = "document",
    embedder = embedder,
    cache_root = cache_root,
    batch_size = 2L
  )
  second <- generate_embeddings(
    texts = c("a", "b"),
    embedding_model = "cohere/embed-v4.0",
    dataset = "alpha",
    text_role = "abstracts",
    embedding_mode = "document",
    embedder = embedder,
    cache_root = cache_root,
    batch_size = 2L
  )

  expect_identical(first, second)
  expect_identical(calls, 1L)
})

test_that("generate_embeddings reuses dataset-level cache rows", {
  cache_root <- withr::local_tempdir()
  texts <- c("alpha", "beta")
  embeddings <- matrix(c(1, 0, 0, 1), ncol = 2, byrow = TRUE)
  cache_path <- get_embedding_cache_path(
    "cohere/embed-v4.0",
    dataset = "alpha",
    text_role = "abstracts",
    embedding_mode = "document",
    cache_root = cache_root
  )
  embedder <- function(texts) {
    stop("dataset cache should avoid live embedding")
  }

  write_embedding_cache(
    tibble::tibble(
      dataset = "alpha",
      embedding_model = "cohere/embed-v4.0",
      embedding_mode = "document",
      text_role = "abstracts",
      text_hash = vapply(texts, create_embedding_cache_key, character(1)),
      text = texts,
      embedding = unname(split(embeddings, row(embeddings)))
    ),
    path = cache_path
  )
  out <- generate_embeddings(
    texts = texts,
    embedding_model = "cohere/embed-v4.0",
    dataset = "alpha",
    text_role = "abstracts",
    embedding_mode = "document",
    embedder = embedder,
    cache_root = cache_root
  )

  expect_identical(out, embeddings)
})

test_that("generate_embeddings embeds only missing cache rows", {
  cache_root <- withr::local_tempdir()
  texts <- c("alpha", "beta")
  cached <- matrix(c(1, 2), ncol = 2)
  cache_path <- get_embedding_cache_path(
    "cohere/embed-v4.0",
    dataset = "alpha",
    text_role = "abstracts",
    embedding_mode = "document",
    cache_root = cache_root
  )
  embedder <- function(texts) {
    expect_identical(texts, "beta")
    matrix(c(3, 4), ncol = 2)
  }

  write_embedding_cache(
    tibble::tibble(
      dataset = "alpha",
      embedding_model = "cohere/embed-v4.0",
      embedding_mode = "document",
      text_role = "abstracts",
      text_hash = create_embedding_cache_key("alpha"),
      text = "alpha",
      embedding = list(as.numeric(cached[1, ]))
    ),
    path = cache_path
  )
  out <- generate_embeddings(
    texts = texts,
    embedding_model = "cohere/embed-v4.0",
    dataset = "alpha",
    text_role = "abstracts",
    embedding_mode = "document",
    embedder = embedder,
    cache_root = cache_root
  )

  expect_identical(out, matrix(c(1, 2, 3, 4), ncol = 2, byrow = TRUE))
})

test_that("generate_embeddings drops long input names before binding rows", {
  cache_root <- withr::local_tempdir()
  texts <- c("alpha", "beta")
  names(texts) <- c(strrep("long-name-", 2000), strrep("other-name-", 2000))
  embedder <- function(texts) {
    matrix(c(1, 2, 3, 4), ncol = 2, byrow = TRUE)
  }

  out <- generate_embeddings(
    texts = texts,
    embedding_model = "cohere/embed-v4.0",
    dataset = "alpha",
    text_role = "abstracts",
    embedder = embedder,
    cache_root = cache_root
  )

  expect_null(rownames(out))
  expect_identical(dim(out), c(2L, 2L))
})

test_that("generate_embeddings force evicts only requested text hashes", {
  cache_root <- withr::local_tempdir()
  texts <- c("alpha", "beta")
  cache_path <- get_embedding_cache_path(
    "cohere/embed-v4.0",
    dataset = "alpha",
    text_role = "abstracts",
    embedding_mode = "document",
    cache_root = cache_root
  )
  write_embedding_cache(
    tibble::tibble(
      dataset = "alpha",
      embedding_model = "cohere/embed-v4.0",
      embedding_mode = "document",
      text_role = "abstracts",
      text_hash = vapply(texts, create_embedding_cache_key, character(1)),
      text = texts,
      embedding = list(c(1, 0), c(0, 1))
    ),
    path = cache_path
  )
  calls <- 0L
  embedder <- function(texts) {
    calls <<- calls + 1L
    expect_identical(texts, "alpha")
    matrix(c(9, 9), nrow = 1L, ncol = 2)
  }

  out <- generate_embeddings(
    texts = "alpha",
    embedding_model = "cohere/embed-v4.0",
    dataset = "alpha",
    text_role = "abstracts",
    embedding_mode = "document",
    embedder = embedder,
    cache_root = cache_root,
    force = TRUE
  )
  cached <- read_embedding_cache(
    path = cache_path,
    dataset = "alpha",
    embedding_model = "cohere/embed-v4.0",
    text_role = "abstracts",
    embedding_mode = "document"
  )

  expect_identical(calls, 1L)
  expect_identical(out, matrix(c(9, 9), nrow = 1L, ncol = 2))
  expect_identical(nrow(cached), 2L)
  beta_hash <- create_embedding_cache_key("beta")
  beta_row <- cached[cached$text_hash == beta_hash, , drop = FALSE]
  expect_identical(beta_row$embedding[[1]], c(0, 1))
  alpha_hash <- create_embedding_cache_key("alpha")
  alpha_row <- cached[cached$text_hash == alpha_hash, , drop = FALSE]
  expect_identical(alpha_row$embedding[[1]], c(9, 9))
})

test_that("write_embedding_cache persists readable cache files", {
  cache_root <- withr::local_tempdir()
  cache_path <- get_embedding_cache_path(
    "cohere/embed-v4.0",
    dataset = "alpha",
    text_role = "abstracts",
    embedding_mode = "document",
    cache_root = cache_root
  )
  cache <- tibble::tibble(
    dataset = "alpha",
    embedding_model = "cohere/embed-v4.0",
    embedding_mode = "document",
    text_role = "abstracts",
    text_hash = create_embedding_cache_key("alpha"),
    text = "alpha",
    embedding = list(c(1, 2))
  )

  write_embedding_cache(cache, cache_path)

  expect_true(fs::file_exists(cache_path))
  expect_s3_class(readRDS(cache_path), "tbl_df")
})

test_that("abstract concentration target controller is simplified", {
  targets_path <- testthat::test_path(
    "..",
    "..",
    "method_pipelines",
    "abstract_concentration",
    "_targets.R"
  )
  targets_text <- readLines(targets_path, warn = FALSE)

  expect_length(grep("^tar_option_set\\(", targets_text), 1L)
  expect_false(any(grepl("BAYSREN_ABSTRACT_CONCENTRATION_WORKERS", targets_text)))
  expect_true(any(grepl("readRenviron\\(\"\\.Renviron\"\\)", targets_text)))
  expect_true(any(grepl("document_embedding_mode", targets_text)))
  expect_true(any(grepl("gemini_2_preview_query_document", targets_text)))
})

test_that("ranking helpers keep lower scores earlier and preserve alignment", {
  doc_mat <- matrix(
    c(
      1, 0,
      0.8, 0.1,
      -1, 0,
      0, 1
    ),
    ncol = 2,
    byrow = TRUE
  )
  seed_mat <- matrix(
    c(
      1, 0,
      0.9, 0.1,
      -1, 0,
      -0.9, 0.1
    ),
    ncol = 2,
    byrow = TRUE
  )
  seed_bank <- tibble::tibble(
    pair_id = c(1L, 2L, 1L, 2L),
    seed_type = c("positive", "positive", "negative", "negative"),
    text = paste("seed", 1:4)
  ) |>
    validate_seed_bank(n_positive = 2L, n_negative = 2L)

  expect_equal(which.min(rank_embeddings_positive_mean(doc_mat, seed_bank, seed_mat)), 1L)
  expect_equal(which.min(rank_embeddings_positive_closest(doc_mat, seed_bank, seed_mat)), 1L)
  expect_equal(which.min(rank_embeddings_positive_centroid(doc_mat, seed_bank, seed_mat)), 1L)
  expect_equal(
    which.min(rank_embeddings_contrastive_centroid(doc_mat, seed_bank, seed_mat, lambda = 0.25)),
    1L
  )
  expect_false(identical(
    rank_embeddings_contrastive_mean(doc_mat, seed_bank, seed_mat, lambda = 0.25),
    rank_embeddings_positive_mean(doc_mat, seed_bank, seed_mat)
  ))
  expect_false(identical(
    rank_embeddings_contrastive_closest(doc_mat, seed_bank, seed_mat, lambda = 0.25),
    rank_embeddings_positive_closest(doc_mat, seed_bank, seed_mat)
  ))
  expect_false(identical(
    rank_embeddings_contrastive_centroid(doc_mat, seed_bank, seed_mat, lambda = 0.25),
    rank_embeddings_positive_centroid(doc_mat, seed_bank, seed_mat)
  ))
  expect_equal(
    which.min(rank_embeddings_centroid_contrastive_weighted(doc_mat, seed_bank, seed_mat)),
    1L
  )
  expect_identical(length(rank_embeddings_positive_mean(doc_mat, seed_bank, seed_mat)), 4L)
})

test_that("validate_seed_bank normalizes common LLM seed type variants", {
  seed_bank <- tibble::tibble(
    pair_id = c(1L, 1L, 2L, 2L),
    seed_type = c("Positive abstract", "EXCLUDED", "included", "negative case"),
    text = c("a", "b", "c", "d")
  )

  out <- validate_seed_bank(seed_bank, n_positive = 2L, n_negative = 2L)

  expect_identical(
    out$seed_type,
    c("positive", "negative", "positive", "negative")
  )
})

test_that("generate_seed_abstracts delegates caching to llm_solver", {
  criteria <- list(
    include = c("Adults with confirmed infection", "Randomised trial"),
    exclude = c("Animal-only studies")
  )
  seen <- list()

  testthat::local_mocked_bindings(
    llm_solver = function(
      inputs,
      solver_chat,
      cache_dir,
      cache_failure,
      batch_size,
      parallelize_batches,
      ...
    ) {
      seen$inputs <<- inputs
      seen$cache_dir <<- cache_dir
      seen$cache_failure <<- cache_failure
      seen$batch_size <<- batch_size
      seen$parallelize_batches <<- parallelize_batches

      list(
        result = list(jsonlite::toJSON(
          list(
            abstracts = list(
              list(pair_id = 1L, seed_type = "positive", text = "Positive one"),
              list(pair_id = 2L, seed_type = "positive", text = "Positive two"),
              list(pair_id = 1L, seed_type = "negative", text = "Negative one"),
              list(pair_id = 2L, seed_type = "negative", text = "Negative two")
            )
          ),
          auto_unbox = TRUE
        ))
      )
    }
  )

  out <- generate_seed_abstracts(
    criteria = criteria,
    dataset = "alpha",
    n_positive = 2L,
    n_negative = 2L,
    cache_root = "cache"
  )

  expect_identical(seen$cache_dir, here::here("cache", "llm", "seed_abstracts", "alpha"))
  expect_identical(seen$cache_failure, FALSE)
  expect_identical(seen$batch_size, 1L)
  expect_identical(seen$parallelize_batches, FALSE)
  expect_true(grepl('"Randomised trial"', seen$inputs[[1]], fixed = TRUE))
  expect_identical(out$seed_type, c("positive", "positive", "negative", "negative"))
})

test_that("generate_seed_abstracts rejects malformed solver replies", {
  testthat::local_mocked_bindings(
    llm_solver = function(...) {
      list(result = list('{"wrong": []}'))
    }
  )

  expect_error(
    generate_seed_abstracts(
      criteria = list(include = "include", exclude = "exclude"),
      dataset = "alpha",
      n_positive = 1L,
      n_negative = 1L
    ),
    "abstracts"
  )
})

fake_dispatch_method <- function(doc_mat, seed_bank, seed_mat, ...) {
  seq_len(nrow(doc_mat))
}

test_that("run_abstract_concentration_benchmark dispatches methods via get()", {
  doc_mat <- matrix(
    c(
      1, 0,
      0.8, 0.1,
      -1, 0
    ),
    ncol = 2,
    byrow = TRUE
  )
  seed_mat <- matrix(
    c(
      1, 0,
      0.9, 0.1,
      -1, 0,
      -0.9, 0.1
    ),
    ncol = 2,
    byrow = TRUE
  )
  seed_bank <- tibble::tibble(
    pair_id = c(1L, 2L, 1L, 2L),
    seed_type = c("positive", "positive", "negative", "negative"),
    text = paste("seed", 1:4)
  ) |>
    validate_seed_bank(n_positive = 2L, n_negative = 2L)

  prepared <- list(
    embedding_profile = "profile-a",
    document_embedding_model = "doc-model",
    seed_embedding_model = "seed-model",
    data = tibble::tibble(id = 1:3, included = c(TRUE, FALSE, TRUE)),
    seed_bank = seed_bank,
    seed_embeddings = seed_mat,
    criteria_item_bank = seed_bank,
    criteria_item_embeddings = seed_mat,
    criteria_block_bank = seed_bank,
    criteria_block_embeddings = seed_mat,
    document_embeddings = doc_mat
  )

  out <- run_abstract_concentration_benchmark(
    dataset = "alpha",
    embedding_model = "model-a",
    method_function = "fake_dispatch_method",
    method_args = list(ranking_method = "fake_dispatch"),
    prepared = prepared
  )

  expect_identical(out$ranking_method, "fake_dispatch")
  expect_identical(out$dataset, "alpha")
  expect_identical(out$embedding_profile, "profile-a")
  expect_identical(out$document_embedding_model, "doc-model")
  expect_identical(out$seed_embedding_model, "seed-model")
  expect_identical(out$total_n, 3L)
})

test_that("criteria wrappers return one requested score vector", {
  doc_mat <- matrix(
    c(
      1, 0,
      0.8, 0.1,
      -1, 0
    ),
    ncol = 2,
    byrow = TRUE
  )
  seed_mat <- matrix(
    c(
      1, 0,
      0.9, 0.1,
      -1, 0,
      -0.9, 0.1
    ),
    ncol = 2,
    byrow = TRUE
  )
  seed_bank <- tibble::tibble(
    pair_id = c(1L, 2L, 1L, 2L),
    seed_type = c("positive", "positive", "negative", "negative"),
    text = paste("seed", 1:4)
  ) |>
    validate_seed_bank(n_positive = 2L, n_negative = 2L)

  prepared <- list(
    embedding_profile = "model-a",
    document_embedding_model = "model-a",
    seed_embedding_model = "model-a",
    data = tibble::tibble(id = 1:3, included = c(TRUE, FALSE, TRUE)),
    seed_bank = seed_bank,
    seed_embeddings = seed_mat,
    criteria_item_bank = seed_bank,
    criteria_item_embeddings = seed_mat,
    criteria_block_bank = seed_bank,
    criteria_block_embeddings = seed_mat,
    document_embeddings = doc_mat
  )

  out <- run_abstract_concentration_benchmark(
    dataset = "alpha",
    embedding_model = "model-a",
    method_function = "rank_embeddings_criteria_item",
    method_args = list(
      ranking_method = "criteria_item_positive_mean",
      score_name = "positive_mean"
    ),
    prepared = prepared
  )

  expect_identical(out$ranking_method, "criteria_item_positive_mean")
  expect_identical(nrow(out), 1L)

  expect_true(is.numeric(out$n_25))
})

test_that("mixed-model summaries and output writers use the track outputs folder", {
  metrics <- tibble::tibble(
    dataset = rep(c("a", "b"), each = 4),
    embedding_profile = rep(c("profile-1", "profile-2"), times = 4),
    embedding_model = rep(c("profile-1", "profile-2"), times = 4),
    document_embedding_model = rep(c("doc-1", "doc-2"), times = 4),
    seed_embedding_model = rep(c("seed-1", "seed-2"), times = 4),
    ranking_method = rep(
      c("positive_mean", "positive_centroid", "contrastive_centroid_l0p25", "criteria_item_positive_mean"),
      times = 2
    ),
    total_n = 100L,
    total_pos = 20L,
    n_25 = c(10L, 11L, 12L, 13L, 9L, 10L, 11L, 12L),
    n_50 = c(12L, 11L, 13L, 14L, 8L, 9L, 10L, 11L),
    n_100 = c(18L, 18L, 19L, 20L, 17L, 18L, 18L, 19L),
    rank_to_5 = c(5L, 6L, 5L, 4L, 7L, 7L, 6L, 5L),
    rank_to_10 = c(10L, 11L, 10L, 9L, 12L, 12L, 11L, 10L),
    rank_to_20 = c(15L, 16L, 14L, 13L, 17L, 16L, 15L, 14L)
  )

  fit <- fit_abstract_concentration_model(metrics)
  summaries <- get_abstract_concentration_marginal_summaries(fit)

  expect_true(inherits(fit, "merMod"))
  expect_true(all(c("embedding_profile", "ranking_method") %in% names(summaries)))
  expect_true(all(c("estimated_recall", "std_error", "conf_low", "conf_high") %in% names(summaries$embedding_profile)))

  output_dir <- file.path(withr::local_tempdir(), "outputs")
  paths <- write_abstract_concentration_outputs(
    metrics = metrics,
    marginal_summaries = summaries,
    output_dir = output_dir
  )

  expect_true(fs::file_exists(paths$ranking_metrics))
  expect_true(fs::file_exists(paths$marginal_embedding_profile_csv))
  expect_true(fs::file_exists(paths$marginal_embedding_profile_md))
  expect_true(fs::file_exists(paths$marginal_ranking_method_csv))
  expect_true(fs::file_exists(paths$marginal_ranking_method_md))
  expect_true(all(startsWith(unname(unlist(paths)), fs::path_abs(output_dir))))
})
