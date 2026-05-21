make_llm_solver_fake_chat <- function(
  model = "fake-model",
  provider_name = "fake-provider",
  system_prompt = "fake system prompt"
) {
  chat <- new.env(parent = emptyenv())

  # The solver only needs these accessors before it reaches live ellmer calls.
  chat$get_model <- function() model
  chat$get_system_prompt <- function() system_prompt
  chat$get_provider <- function() {
    structure(
      list(
        name = provider_name,
        params = list(),
        extra_args = list()
      ),
      class = "fake_provider"
    )
  }

  class(chat) <- c("Chat", "fake_llm_solver_chat")
  chat
}

test_that("llm_solver rejects invalid inputs before API work", {
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

test_that("llm_solver default cache directory points at the official LLM cache", {
  defaults <- formals(llm_solver)

  # Keep the local default aligned with the repo cache contract.
  expect_identical(
    deparse(defaults$cache_dir),
    "here::here(\"cache\", \"llm\")"
  )
})
