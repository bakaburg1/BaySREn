make_model_cache_fake_chat <- function(
  model = "fake-model",
  provider_name = "fake-provider",
  system_prompt = "fake system prompt",
  provider_base_url = NULL
) {
  chat <- new.env(parent = emptyenv())

  # Mimic the small subset of the ellmer Chat API used by the cache layer.
  chat$get_model <- function() model
  chat$get_system_prompt <- function() system_prompt
  chat$get_provider <- function() {
    structure(
      list(
        name = provider_name,
        base_url = provider_base_url,
        params = list(),
        extra_args = list()
      ),
      class = "fake_provider"
    )
  }

  class(chat) <- c("Chat", "fake_model_cache_chat")
  chat
}

test_that("model cache namespace hashes ignore runtime-only controls", {
  skip_if_not_installed("jsonlite")

  chat <- make_model_cache_fake_chat()

  # Solver runtime controls should not create new cache namespaces.
  spec_a <- .caching_spec(
    chat = chat,
    type = NULL,
    dots = list(alpha = 1, batch_size = 10, cache_dir = "first")
  )
  spec_b <- .caching_spec(
    chat = chat,
    type = NULL,
    dots = list(cache_dir = "second", batch_size = 20, alpha = 1)
  )

  expect_identical(.caching_namespace_hash(spec_a), .caching_namespace_hash(spec_b))
})

test_that("model cache namespace hashes ignore cache_failure", {
  skip_if_not_installed("jsonlite")

  chat <- make_model_cache_fake_chat()

  # Failure-caching is runtime-only and must not split successful cache hits.
  spec_false <- .caching_spec(
    chat = chat,
    type = NULL,
    dots = list(),
    cache_failure = FALSE
  )
  spec_true <- .caching_spec(
    chat = chat,
    type = NULL,
    dots = list(),
    cache_failure = TRUE
  )

  expect_false("cache_failure" %in% names(spec_true))
  expect_false("cache_failure" %in% names(spec_false))
  expect_identical(
    .caching_namespace_hash(spec_false),
    .caching_namespace_hash(spec_true)
  )
  expect_identical(
    .caching_stable_path(
      cache_dir = tempdir(),
      cache_spec = spec_false,
      namespace_hash = .caching_namespace_hash(spec_false)
    ),
    .caching_stable_path(
      cache_dir = tempdir(),
      cache_spec = spec_true,
      namespace_hash = .caching_namespace_hash(spec_true)
    )
  )
})

test_that("cache_failure forwarded in dots is dropped from cache spec", {
  skip_if_not_installed("jsonlite")

  chat <- make_model_cache_fake_chat()
  clean_spec <- .caching_spec(
    chat = chat,
    type = NULL,
    dots = list(),
    cache_failure = FALSE
  )
  dotted_spec <- .caching_spec(
    chat = chat,
    type = NULL,
    dots = list(cache_failure = TRUE),
    cache_failure = TRUE
  )

  expect_false("cache_failure" %in% names(dotted_spec))
  expect_false("cache_failure" %in% names(dotted_spec$dots))
  expect_identical(
    .caching_namespace_hash(clean_spec),
    .caching_namespace_hash(dotted_spec)
  )
})

test_that("model and system prompt changes still alter namespace hash", {
  skip_if_not_installed("jsonlite")

  base_chat <- make_model_cache_fake_chat()
  alt_model_chat <- make_model_cache_fake_chat(model = "other-model")
  alt_prompt_chat <- make_model_cache_fake_chat(system_prompt = "other prompt")

  base_spec <- .caching_spec(chat = base_chat, type = NULL, dots = list())
  alt_model_spec <- .caching_spec(chat = alt_model_chat, type = NULL, dots = list())
  alt_prompt_spec <- .caching_spec(chat = alt_prompt_chat, type = NULL, dots = list())

  base_hash <- .caching_namespace_hash(base_spec)
  expect_false(identical(base_hash, .caching_namespace_hash(alt_model_spec)))
  expect_false(identical(base_hash, .caching_namespace_hash(alt_prompt_spec)))
})

test_that("model cache rejects stale namespace hashes", {
  skip_if_not_installed("jsonlite")

  chat <- make_model_cache_fake_chat()
  current_spec <- .caching_spec(
    chat = chat,
    type = NULL,
    dots = list(),
    cache_failure = FALSE
  )
  namespace_hash <- .caching_namespace_hash(current_spec)

  payload <- .caching_payload(
    cache_spec = current_spec,
    namespace_hash = "stale-hash",
    rows = list()
  )

  expect_false(.caching_validate(payload, current_spec, namespace_hash))
})

test_that("model cache rejects payloads with cache_failure in cache_spec", {
  skip_if_not_installed("jsonlite")

  chat <- make_model_cache_fake_chat()
  current_spec <- .caching_spec(
    chat = chat,
    type = NULL,
    dots = list(),
    cache_failure = FALSE
  )
  namespace_hash <- .caching_namespace_hash(current_spec)

  stale_spec_payload <- .caching_payload(
    cache_spec = list(
      model = current_spec$model,
      provider = current_spec$provider,
      system_prompt = current_spec$system_prompt,
      type = current_spec$type,
      dots = list(),
      cache_failure = TRUE
    ) |> .caching_normalize(),
    namespace_hash = namespace_hash,
    rows = list()
  )

  expect_false(.caching_validate(stale_spec_payload, current_spec, namespace_hash))
})

test_that("model cache uses flat provider_model_hash paths", {
  skip_if_not_installed("fs")
  skip_if_not_installed("jsonlite")

  cache_dir <- file.path(tempdir(), paste0("model-cache-path-", Sys.getpid()))
  chat <- make_model_cache_fake_chat(
    model = "google/gemini-3.1-pro-preview",
    provider_name = "openrouter"
  )
  spec <- .caching_spec(chat = chat, type = NULL, dots = list())
  namespace_hash <- .caching_namespace_hash(spec)

  # The copied cache system stores one flat RDS file per provider/model/spec hash.
  path <- .caching_stable_path(
    cache_dir = cache_dir,
    cache_spec = spec,
    namespace_hash = namespace_hash
  )

  expect_identical(
    path,
    fs::path(cache_dir, paste0("openrouter_google.gemini-3.1-pro-preview_", namespace_hash, ".rds"))
  )
})

test_that("model cache treats malformed or stale payloads as misses", {
  skip_if_not_installed("jsonlite")

  chat <- make_model_cache_fake_chat()
  spec <- .caching_spec(chat = chat, type = NULL, dots = list(alpha = 1))
  namespace_hash <- .caching_namespace_hash(spec)

  # Cache validation protects callers from stale payloads after prompt/spec changes.
  expect_false(.caching_validate(NULL, spec, namespace_hash))
  expect_false(.caching_validate(list(namespace_hash = "wrong"), spec, namespace_hash))
})
