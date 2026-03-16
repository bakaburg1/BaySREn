# Tests for assisted targets -----------------------------------------------

test_that("assisted experiment target resolves branch metadata locally", {
  # Resolve the assisted target script from the testthat working directory.
  script_path <- normalizePath(
    file.path(
      "..",
      "..",
      "experiments",
      "targets_scripts",
      "_target_llm_assisted_screening.R"
    ),
    mustWork = TRUE
  )

  manifest <- suppressWarnings(
    targets::tar_manifest(
      script = script_path,
      callr_function = NULL
    )
  )

  command <- manifest$command[
    manifest$name == "experiment_result_vella"
  ]

  expect_length(command, 1)
  expect_match(
    command,
    '\\[\\["dataset_name"\\]\\]\\[\\[1\\]\\]',
    perl = TRUE
  )
  expect_match(
    command,
    "current_experiment_name <- param_sets\\$experiment_name\\[\\[1\\]\\]",
    perl = TRUE
  )
  expect_match(
    command,
    "dataset_name = current_dataset_name",
    fixed = TRUE
  )
  expect_match(
    command,
    "experiment_name = current_experiment_name",
    fixed = TRUE
  )
  expect_no_match(
    command,
    "dataset_name = dataset_name",
    fixed = TRUE
  )
  expect_no_match(
    command,
    "{dataset_name}",
    fixed = TRUE
  )
})

test_that("seed generation wraps prompts in a list", {
  script_path <- normalizePath(
    file.path(
      "..",
      "..",
      "experiments",
      "targets_scripts",
      "_target_llm_assisted_screening.R"
    ),
    mustWork = TRUE
  )

  script_text <- paste(readLines(script_path, warn = FALSE), collapse = "\n")

  expect_match(
    script_text,
    "as\\.character\\(\\) \\|>\\s+as\\.list\\(\\)",
    perl = TRUE
  )
})

test_that("embedder builder materializes the configured embed function", {
  script_path <- normalizePath(
    file.path(
      "..",
      "..",
      "experiments",
      "targets_scripts",
      "_target_llm_assisted_screening.R"
    ),
    mustWork = TRUE
  )

  script_text <- paste(readLines(script_path, warn = FALSE), collapse = "\n")

  expect_match(
    script_text,
    'build_embedder <- function\\(config\\)',
    perl = TRUE
  )
  expect_match(
    script_text,
    'ragnar::embed_openai\\(',
    perl = TRUE
  )
  expect_no_match(
    script_text,
    'purrr::partial\\(\\s*ragnar::embed_openai',
    perl = TRUE
  )
})

test_that("state write-back batches retain adjudication tracking columns", {
  script_path <- normalizePath(
    file.path(
      "..",
      "..",
      "experiments",
      "targets_scripts",
      "_target_llm_assisted_screening.R"
    ),
    mustWork = TRUE
  )

  script_text <- paste(readLines(script_path, warn = FALSE), collapse = "\n")

  expect_gte(
    lengths(regmatches(
      script_text,
      gregexpr('"adjudication_triggered"', script_text, fixed = TRUE)
    ))[[1]],
    3L
  )
  expect_gte(
    lengths(regmatches(
      script_text,
      gregexpr('"adjudication_applied"', script_text, fixed = TRUE)
    ))[[1]],
    3L
  )
  expect_gte(
    lengths(regmatches(
      script_text,
      gregexpr('"adjudication_round"', script_text, fixed = TRUE)
    ))[[1]],
    3L
  )
})

test_that("main-loop final label selection uses scalar branching, not if_else", {
  script_path <- normalizePath(
    file.path(
      "..",
      "..",
      "experiments",
      "targets_scripts",
      "_target_llm_assisted_screening.R"
    ),
    mustWork = TRUE
  )

  script_text <- paste(readLines(script_path, warn = FALSE), collapse = "\n")

  expect_match(
    script_text,
    "final_bool = if \\(isTRUE\\(config\\$human_after_warmup\\)\\)",
    perl = TRUE
  )
  expect_no_match(
    script_text,
    "final_bool = dplyr::if_else",
    fixed = TRUE
  )
})

test_that("assisted target script writes durable run logs", {
  script_path <- normalizePath(
    file.path(
      "..",
      "..",
      "experiments",
      "targets_scripts",
      "_target_llm_assisted_screening.R"
    ),
    mustWork = TRUE
  )

  script_text <- paste(readLines(script_path, warn = FALSE), collapse = "\n")

  expect_match(script_text, 'run_logs', fixed = TRUE)
  expect_match(script_text, 'branch_events.tsv', fixed = TRUE)
  expect_match(script_text, 'seed_texts.tsv', fixed = TRUE)
  expect_match(script_text, 'ranking_snapshots.tsv', fixed = TRUE)
  expect_match(script_text, 'warmup_rounds.tsv', fixed = TRUE)
  expect_match(script_text, 'iteration_metrics.tsv', fixed = TRUE)
  expect_match(script_text, 'branch_summaries.tsv', fixed = TRUE)
})

test_that("assisted aggregate targets flatten nested experiment results", {
  script_path <- normalizePath(
    file.path(
      "..",
      "..",
      "experiments",
      "targets_scripts",
      "_target_llm_assisted_screening.R"
    ),
    mustWork = TRUE
  )

  script_text <- paste(readLines(script_path, warn = FALSE), collapse = "\n")

  expect_match(
    script_text,
    'collect_experiment_component <- function\\(all_experiment_results, component\\)',
    perl = TRUE
  )
  expect_match(
    script_text,
    'purrr::flatten\\(\\)',
    perl = TRUE
  )
  expect_no_match(
    script_text,
    'purrr::map_dfr\\(all_experiment_results',
    perl = TRUE
  )
})
