skip_llm_solver_method_prereqs <- function() {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("fs")
  skip_if_not_installed("withr")
}

make_llm_solver_fake_chat <- function(
  model = "fake-model",
  provider_name = "fake-provider",
  provider_base_url = NULL,
  system_prompt = "fake system prompt"
) {
  chat <- new.env(parent = emptyenv())
  chat$model_name <- model
  chat$provider_name <- provider_name
  chat$provider_base_url <- provider_base_url
  chat$system_prompt <- system_prompt
  chat$clone <- function(deep = TRUE) make_llm_solver_fake_chat(
    model = chat$model_name,
    provider_name = chat$provider_name,
    provider_base_url = chat$provider_base_url,
    system_prompt = chat$system_prompt
  )
  chat$get_model <- function() chat$model_name
  chat$get_system_prompt <- function() chat$system_prompt
  chat$get_provider <- function() {
    structure(
      list(
        name = chat$provider_name,
        base_url = chat$provider_base_url,
        params = list(),
        extra_args = list()
      ),
      class = "fake_provider"
    )
  }
  class(chat) <- c("Chat", "fake_method_llm_chat")
  chat
}

test_that("llm_solver rejects invalid inputs", {
  skip_llm_solver_method_prereqs()

  expect_error(
    llm_solver(
      inputs = 1:3,
      solver_chat = make_llm_solver_fake_chat()
    ),
    "character vector"
  )

  expect_error(
    llm_solver(
      inputs = character(),
      solver_chat = make_llm_solver_fake_chat()
    ),
    "at least one"
  )
})

test_that("llm_solver dispatches to unstructured ellmer helpers", {
  skip_llm_solver_method_prereqs()

  cache_root <- file.path(tempdir(), paste0("method-llm-unstructured-", Sys.getpid()))
  unlink(cache_root, recursive = TRUE, force = TRUE)
  on.exit(unlink(cache_root, recursive = TRUE, force = TRUE), add = TRUE)

  seen <- list(call_count = 0L)

  testthat::local_mocked_bindings(
    parallel_chat = function(chat, prompts, ...) {
      seen$call_count <<- seen$call_count + 1L
      seen$prompts <<- prompts
      paste0("ok-", unlist(prompts, use.names = FALSE))
    },
    .package = "ellmer"
  )

  out <- llm_solver(
    inputs = c("alpha", "beta"),
    solver_chat = make_llm_solver_fake_chat(),
    cache_mode = "official",
    cache_root = cache_root,
    batch_size = 2L,
    max_retries = 1L
  )

  expect_identical(seen$call_count, 1L)
  expect_identical(seen$prompts, list("alpha", "beta"))
  expect_identical(out$result, list("ok-alpha", "ok-beta"))
  expect_identical(out$status, c("success", "success"))
  expect_true(all(vapply(out$solver_metadata, \(x) x$execution$status, character(1)) == "success"))
})

test_that("llm_solver dispatches to structured ellmer helpers", {
  skip_llm_solver_method_prereqs()

  cache_root <- file.path(tempdir(), paste0("method-llm-structured-", Sys.getpid()))
  unlink(cache_root, recursive = TRUE, force = TRUE)
  on.exit(unlink(cache_root, recursive = TRUE, force = TRUE), add = TRUE)

  structured_type <- ellmer::type_object(
    answer = ellmer::type_string()
  )

  testthat::local_mocked_bindings(
    parallel_chat_structured = function(chat, prompts, type, ...) {
      tibble::tibble(
        answer = paste0("structured-", unlist(prompts, use.names = FALSE))
      )
    },
    .package = "ellmer"
  )

  out <- llm_solver(
    inputs = c("alpha", "beta"),
    solver_chat = make_llm_solver_fake_chat(),
    type = structured_type,
    cache_mode = "official",
    cache_root = cache_root,
    batch_size = 2L,
    max_retries = 1L
  )

  expect_identical(out$result[[1]]$answer, "structured-alpha")
  expect_identical(out$result[[2]]$answer, "structured-beta")
  expect_identical(out$status, c("success", "success"))
  expect_identical(
    vapply(out$solver_metadata, \(x) x$payload_kind, character(1)),
    c("structured", "structured")
  )
})

test_that("llm_solver extracts text from Chat-like responses", {
  skip_llm_solver_method_prereqs()

  turn <- list(text = "chat text")
  chat_response <- new.env(parent = emptyenv())
  chat_response$last_turn <- function() turn
  class(chat_response) <- c("Chat", "fake_method_llm_chat_response")

  expect_identical(.llm_extract_text(chat_response), "chat text")
})

test_that("llm_solver reuses cached rows without calling ellmer again", {
  skip_llm_solver_method_prereqs()

  cache_root <- file.path(tempdir(), paste0("method-llm-cache-", Sys.getpid()))
  unlink(cache_root, recursive = TRUE, force = TRUE)
  on.exit(unlink(cache_root, recursive = TRUE, force = TRUE), add = TRUE)

  live_calls <- 0L

  testthat::local_mocked_bindings(
    parallel_chat = function(chat, prompts, ...) {
      live_calls <<- live_calls + 1L
      paste0("ok-", unlist(prompts, use.names = FALSE))
    },
    .package = "ellmer"
  )

  first <- llm_solver(
    inputs = c("alpha", "beta"),
    solver_chat = make_llm_solver_fake_chat(),
    cache_mode = "official",
    cache_root = cache_root,
    batch_size = 2L,
    max_retries = 1L
  )
  second <- llm_solver(
    inputs = c("alpha", "beta"),
    solver_chat = make_llm_solver_fake_chat(),
    cache_mode = "official",
    cache_root = cache_root,
    batch_size = 2L,
    max_retries = 1L
  )

  expect_identical(live_calls, 1L)
  expect_false(any(first$cache_hit))
  expect_true(all(second$cache_hit))
  expect_identical(second$result, first$result)
})

test_that("llm_solver retries unresolved rows and caches terminal failures", {
  skip_llm_solver_method_prereqs()

  cache_root <- file.path(tempdir(), paste0("method-llm-failure-", Sys.getpid()))
  unlink(cache_root, recursive = TRUE, force = TRUE)
  on.exit(unlink(cache_root, recursive = TRUE, force = TRUE), add = TRUE)

  call_count <- 0L

  testthat::local_mocked_bindings(
    parallel_chat = function(chat, prompts, ...) {
      call_count <<- call_count + 1L
      if (length(prompts) == 2L) {
        list("ok-alpha", NULL)
      } else {
        list(NULL)
      }
    },
    .package = "ellmer"
  )

  first <- llm_solver(
    inputs = c("alpha", "beta"),
    solver_chat = make_llm_solver_fake_chat(),
    cache_mode = "official",
    cache_root = cache_root,
    batch_size = 2L,
    max_retries = 2L,
    cache_failure = TRUE
  )
  second <- llm_solver(
    inputs = c("alpha", "beta"),
    solver_chat = make_llm_solver_fake_chat(),
    cache_mode = "official",
    cache_root = cache_root,
    batch_size = 2L,
    max_retries = 2L,
    cache_failure = TRUE
  )

  expect_identical(call_count, 2L)
  expect_identical(first$status, c("success", "failed_final"))
  expect_identical(second$status, c("success", "failed_final"))
  expect_identical(second$failure_count[[2]], 2L)
  expect_true(second$cache_hit[[1]])
  expect_true(second$cache_hit[[2]])
})
