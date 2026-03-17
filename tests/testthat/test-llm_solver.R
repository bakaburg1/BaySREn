skip_llm_solver_prereqs <- function() {
  skip_if_not_installed("jsonlite")
}

fake_solver_chat <- function() {
  provider <- structure(
    list(),
    name = "FakeProvider",
    model = "fake-model",
    extra_args = list(temperature = 0)
  )

  chat <- new.env(parent = emptyenv())
  chat$get_provider <- function() provider
  chat$get_turns <- function(include_system_prompt = TRUE) list(list())
  chat$clone <- function() chat
  chat
}

test_that("llm_solver expands reps and includes rep in the cache key", {
  skip_llm_solver_prereqs()

  calls <- list()

  with_mocked_bindings(
    {
      out <- llm_solver(
        prompts = c("alpha", "beta"),
        chat = fake_solver_chat(),
        reps = 2L,
        cache_dir = NULL,
        max_attempts = 1L
      )

      expect_equal(nrow(out), 4L)
      expect_equal(sort(unique(out$rep)), c(1L, 2L))
      expect_equal(length(unique(out$cache_key[out$row_id == 1L])), 2L)
      expect_identical(vapply(calls, length, integer(1)), c(4L))
    },
    parallel_chat_text = function(chat, prompts, max_active, rpm, on_error) {
      calls <<- c(calls, list(prompts))
      paste0("ok-", unlist(prompts, use.names = FALSE))
    },
    .package = "ellmer"
  )
})

test_that("llm_solver uses structured mode when schema is provided", {
  skip_llm_solver_prereqs()

  structured_calls <- 0L

  with_mocked_bindings(
    {
      out <- llm_solver(
        prompts = c("alpha", "beta"),
        chat = fake_solver_chat(),
        schema = ellmer::type_object(
          final_decision = ellmer::type_boolean("decision"),
          justification = ellmer::type_string("why")
        ),
        cache_dir = NULL,
        max_attempts = 1L
      )

      expect_equal(structured_calls, 1L)
      expect_equal(out$final_decision, c(TRUE, FALSE))
      expect_false(any(out$has_error))
    },
    parallel_chat_structured = function(chat, prompts, type, max_active, rpm, on_error, convert = TRUE, include_tokens = FALSE, include_cost = FALSE) {
      structured_calls <<- structured_calls + 1L
      tibble::tibble(
        final_decision = c(TRUE, FALSE),
        justification = c("yes", "no")
      )
    },
    .package = "ellmer"
  )
})

test_that("llm_solver reuses cached rows without calling ellmer again", {
  skip_llm_solver_prereqs()

  cache_dir <- file.path(tempdir(), paste0("llm_solver_cache_", Sys.getpid()))
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  live_calls <- 0L

  with_mocked_bindings(
    {
      first <- llm_solver(
        prompts = c("alpha", "beta"),
        chat = fake_solver_chat(),
        cache_dir = cache_dir,
        max_attempts = 1L
      )

      second <- llm_solver(
        prompts = c("alpha", "beta"),
        chat = fake_solver_chat(),
        cache_dir = cache_dir,
        max_attempts = 1L
      )

      expect_equal(live_calls, 1L)
      expect_false(any(first$from_cache))
      expect_true(all(second$from_cache))
      expect_true(file.exists(file.path(cache_dir, "result_journal.jsonl")))
      expect_true(file.exists(file.path(cache_dir, "result_index.rds")))
    },
    parallel_chat_text = function(chat, prompts, max_active, rpm, on_error) {
      live_calls <<- live_calls + 1L
      paste0("ok-", unlist(prompts, use.names = FALSE))
    },
    .package = "ellmer"
  )
})

test_that("llm_solver retries only unresolved rows and keeps NA payloads after exhaustion", {
  skip_llm_solver_prereqs()

  call_sizes <- integer()
  schema <- ellmer::type_object(
    final_decision = ellmer::type_boolean("decision"),
    justification = ellmer::type_string("why")
  )

  with_mocked_bindings(
    {
      out <- llm_solver(
        prompts = c("alpha", "beta"),
        chat = fake_solver_chat(),
        schema = schema,
        cache_dir = NULL,
        max_attempts = 2L
      )

      expect_identical(call_sizes, c(2L, 1L))
      expect_false(out$has_error[[1]])
      expect_true(out$has_error[[2]])
      expect_true(is.na(out$final_decision[[2]]))
      expect_match(out$error_message[[2]], "still bad|Structured response failed")
      expect_equal(out$attempts_used[[2]], 2L)
    },
    parallel_chat_structured = local({
      call_idx <- 0L
      function(chat, prompts, type, max_active, rpm, on_error, convert = TRUE, include_tokens = FALSE, include_cost = FALSE) {
        call_idx <<- call_idx + 1L
        call_sizes <<- c(call_sizes, length(prompts))
        if (call_idx == 1L) {
          return(tibble::tibble(
            final_decision = c(TRUE, NA),
            justification = c("yes", NA_character_),
            .error = list(NULL, simpleError("first bad"))
          ))
        }
        tibble::tibble(
          final_decision = NA,
          justification = NA_character_,
          .error = list(simpleError("still bad"))
        )
      }
    }),
    .package = "ellmer"
  )
})

test_that("llm_solver persists completed rows after each cache chunk", {
  skip_llm_solver_prereqs()

  cache_dir <- file.path(tempdir(), paste0("llm_solver_chunk_", Sys.getpid()))
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  schema <- ellmer::type_object(
    final_decision = ellmer::type_boolean("decision"),
    justification = ellmer::type_string("why")
  )

  with_mocked_bindings(
    {
      out <- llm_solver(
        prompts = c("alpha", "beta"),
        chat = fake_solver_chat(),
        schema = schema,
        cache_dir = cache_dir,
        max_attempts = 1L,
        cache_batch_size = 1L
      )

      journal <- readLines(file.path(cache_dir, "result_journal.jsonl"), warn = FALSE)

      expect_length(journal, 1L)
      expect_false(out$has_error[[1]])
      expect_true(out$has_error[[2]])
    },
    parallel_chat_structured = local({
      call_idx <- 0L
      function(chat, prompts, type, max_active, rpm, on_error, convert = TRUE, include_tokens = FALSE, include_cost = FALSE) {
        call_idx <<- call_idx + 1L
        if (call_idx == 1L) {
          return(tibble::tibble(
            final_decision = TRUE,
            justification = "yes"
          ))
        }
        tibble::tibble(
          final_decision = NA,
          justification = NA_character_,
          .error = list(simpleError("bad"))
        )
      }
    }),
    .package = "ellmer"
  )
})

test_that("llm_solver deduplicates identical live requests by cache key", {
  skip_llm_solver_prereqs()

  prompt_lengths <- integer()

  with_mocked_bindings(
    {
      out <- llm_solver(
        prompts = c("same", "same"),
        chat = fake_solver_chat(),
        cache_dir = NULL,
        max_attempts = 1L
      )

      expect_identical(prompt_lengths, 1L)
      expect_equal(out$text, c("ok-same", "ok-same"))
      expect_equal(length(unique(out$cache_key)), 1L)
    },
    parallel_chat_text = function(chat, prompts, max_active, rpm, on_error) {
      prompt_lengths <<- c(prompt_lengths, length(prompts))
      paste0("ok-", unlist(prompts, use.names = FALSE))
    },
    .package = "ellmer"
  )
})

test_that("llm_solver treats max_active NULL as uncapped within the chunk", {
  skip_llm_solver_prereqs()

  seen_max_active <- integer()
  seen_rpm <- integer()

  with_mocked_bindings(
    {
      out <- llm_solver(
        prompts = list("alpha", "beta", "gamma"),
        chat = fake_solver_chat(),
        cache_dir = NULL,
        max_attempts = 1L,
        max_active = NULL,
        rpm = 77,
        cache_batch_size = 3L
      )

      expect_identical(seen_max_active, 3L)
      expect_equal(seen_rpm, 77)
      expect_false(any(out$has_error))
    },
    parallel_chat_text = function(chat, prompts, max_active, rpm, on_error) {
      seen_max_active <<- c(seen_max_active, max_active)
      seen_rpm <<- c(seen_rpm, rpm)
      paste0("ok-", unlist(prompts, use.names = FALSE))
    },
    .package = "ellmer"
  )
})

test_that("llm_solver rejects missing max_active and rpm together", {
  skip_llm_solver_prereqs()

  expect_snapshot_error(
    llm_solver(
      prompts = list("alpha"),
      chat = fake_solver_chat(),
      max_active = NULL,
      rpm = NULL
    )
  )
})
