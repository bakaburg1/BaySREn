skip_method_cache_prereqs <- function() {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("fs")
}

make_method_cache_fake_chat <- function(
  model = "fake-model",
  provider_name = "fake-provider",
  system_prompt = "fake system prompt",
  provider_base_url = NULL
) {
  chat <- new.env(parent = emptyenv())
  chat$clone <- function(deep = TRUE) make_method_cache_fake_chat(
    model = model,
    provider_name = provider_name,
    system_prompt = system_prompt,
    provider_base_url = provider_base_url
  )
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
  class(chat) <- c("Chat", "fake_method_cache_chat")
  chat
}

test_that("method cache namespace hashes stay stable when solver params are reordered", {
  skip_method_cache_prereqs()

  chat <- make_method_cache_fake_chat()
  spec_a <- method_cache_spec(
    chat = chat,
    dots = list(alpha = 1, beta = 2, batch_size = 99),
    cache_family = "method_llm_solver"
  )
  spec_b <- method_cache_spec(
    chat = chat,
    dots = list(beta = 2, batch_size = 12, alpha = 1),
    cache_family = "method_llm_solver"
  )

  expect_identical(method_cache_namespace_hash(spec_a), method_cache_namespace_hash(spec_b))
})

test_that("method cache reads official roots before interim overlay roots", {
  skip_method_cache_prereqs()

  chat <- make_method_cache_fake_chat()
  spec <- method_cache_spec(
    chat = chat,
    dots = list(alpha = 1),
    cache_family = "method_llm_solver"
  )
  namespace_hash <- method_cache_namespace_hash(spec)

  official_root <- file.path(tempdir(), paste0("official-", Sys.getpid()))
  overlay_root <- file.path(tempdir(), paste0("overlay-", Sys.getpid()))
  unlink(official_root, recursive = TRUE, force = TRUE)
  unlink(overlay_root, recursive = TRUE, force = TRUE)
  on.exit({
    unlink(official_root, recursive = TRUE, force = TRUE)
    unlink(overlay_root, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  official_path <- file.path(official_root, "llm", "method_llm_solver", paste0(namespace_hash, ".rds"))
  dir.create(dirname(official_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(cache_spec = spec, namespace_hash = "wrong", cache_schema_version = 1L, rows = list()), official_path)

  overlay_payload <- method_cache_payload(
    cache_spec = spec,
    namespace_hash = namespace_hash,
    rows = list(
      one = method_cache_row_record(
        input = "alpha",
        row_index = 1L,
        row_key = "1",
        input_hash = rlang::hash("alpha"),
        cache_key = "one",
        result = "ok-alpha",
        solver_chat = "chat-alpha",
        solver_metadata = list(payload_kind = "text"),
        attempt_count = 1L,
        failure_count = 0L,
        failure_reason = character(),
        status = "success",
        cache_hit = FALSE,
        cache_source = "overlay"
      )
    )
  )
  method_cache_write(
    cache_payload = overlay_payload,
    cache_spec = spec,
    cache_mode = "interim",
    cache_root = official_root,
    cache_overlay_root = overlay_root
  )

  restored <- method_cache_read(
    cache_spec = spec,
    cache_mode = "interim",
    cache_root = official_root,
    cache_overlay_root = overlay_root
  )

  expect_true(is.list(restored))
  expect_identical(restored$source, "overlay")
  expect_identical(restored$payload$rows$one$result, "ok-alpha")
})

test_that("method cache writes official and interim payloads to the expected roots", {
  skip_method_cache_prereqs()

  chat <- make_method_cache_fake_chat()
  spec <- method_cache_spec(
    chat = chat,
    dots = list(alpha = 1),
    cache_family = "method_llm_solver"
  )
  payload <- method_cache_payload(
    cache_spec = spec,
    namespace_hash = method_cache_namespace_hash(spec),
    rows = list(
      one = method_cache_row_record(
        input = "alpha",
        row_index = 1L,
        row_key = "1",
        input_hash = rlang::hash("alpha"),
        cache_key = "one",
        result = "ok-alpha",
        solver_chat = "chat-alpha",
        solver_metadata = list(payload_kind = "text"),
        attempt_count = 1L,
        failure_count = 0L,
        failure_reason = character(),
        status = "success",
        cache_hit = FALSE,
        cache_source = "official"
      )
    )
  )

  official_root <- file.path(tempdir(), paste0("official-write-", Sys.getpid()))
  overlay_root <- file.path(tempdir(), paste0("overlay-write-", Sys.getpid()))
  unlink(official_root, recursive = TRUE, force = TRUE)
  unlink(overlay_root, recursive = TRUE, force = TRUE)
  on.exit({
    unlink(official_root, recursive = TRUE, force = TRUE)
    unlink(overlay_root, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  method_cache_write(
    cache_payload = payload,
    cache_spec = spec,
    cache_mode = "official",
    cache_root = official_root,
    cache_overlay_root = overlay_root
  )
  method_cache_write(
    cache_payload = payload,
    cache_spec = spec,
    cache_mode = "interim",
    cache_root = official_root,
    cache_overlay_root = overlay_root
  )

  namespace_hash <- method_cache_namespace_hash(spec)
  official_path <- file.path(official_root, "llm", "method_llm_solver", paste0(namespace_hash, ".rds"))
  overlay_path <- file.path(overlay_root, "llm", "method_llm_solver", paste0(namespace_hash, ".rds"))

  expect_true(file.exists(official_path))
  expect_true(file.exists(overlay_path))

  restored_official <- method_cache_read(
    cache_spec = spec,
    cache_mode = "official",
    cache_root = official_root
  )
  expect_identical(restored_official$source, "official")
  expect_identical(restored_official$payload$rows$one$result, "ok-alpha")
})

test_that("method cache treats stale payloads as misses", {
  skip_method_cache_prereqs()

  chat <- make_method_cache_fake_chat()
  spec <- method_cache_spec(
    chat = chat,
    dots = list(alpha = 1),
    cache_family = "method_llm_solver"
  )
  namespace_hash <- method_cache_namespace_hash(spec)

  cache_root <- file.path(tempdir(), paste0("stale-", Sys.getpid()))
  unlink(cache_root, recursive = TRUE, force = TRUE)
  on.exit(unlink(cache_root, recursive = TRUE, force = TRUE), add = TRUE)

  path <- file.path(cache_root, "llm", "method_llm_solver", paste0(namespace_hash, ".rds"))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(
    list(
      cache_spec = spec,
      namespace_hash = "wrong",
      cache_schema_version = 1L,
      rows = list()
    ),
    path
  )

  expect_null(
    method_cache_read(
      cache_spec = spec,
      cache_mode = "official",
      cache_root = cache_root
    )
  )
})

test_that("method cache roundtrips atomically", {
  skip_method_cache_prereqs()

  chat <- make_method_cache_fake_chat()
  spec <- method_cache_spec(
    chat = chat,
    dots = list(alpha = 1),
    cache_family = "method_llm_solver"
  )
  payload <- method_cache_payload(
    cache_spec = spec,
    namespace_hash = method_cache_namespace_hash(spec),
    rows = list(
      one = method_cache_row_record(
        input = "alpha",
        row_index = 1L,
        row_key = "1",
        input_hash = rlang::hash("alpha"),
        cache_key = "one",
        result = "ok-alpha",
        solver_chat = "chat-alpha",
        solver_metadata = list(payload_kind = "text"),
        attempt_count = 1L,
        failure_count = 0L,
        failure_reason = character(),
        status = "success",
        cache_hit = FALSE,
        cache_source = "official"
      )
    )
  )

  cache_root <- file.path(tempdir(), paste0("roundtrip-", Sys.getpid()))
  unlink(cache_root, recursive = TRUE, force = TRUE)
  on.exit(unlink(cache_root, recursive = TRUE, force = TRUE), add = TRUE)

  method_cache_write(
    cache_payload = payload,
    cache_spec = spec,
    cache_mode = "official",
    cache_root = cache_root
  )
  restored <- method_cache_read(
    cache_spec = spec,
    cache_mode = "official",
    cache_root = cache_root
  )

  expect_true(is.list(restored))
  expect_identical(restored$payload$rows$one$result, "ok-alpha")
  expect_identical(restored$payload$rows$one$cache_key, "one")
})
