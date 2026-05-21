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
