# experiments/targets_scripts/_target_llm_assisted_screening.R

# Load package data/functions in the current target session.
devtools::load_all()

# Load target pipeline libraries.
library(targets)
library(tarchetypes)

# Load core dependencies used directly in this script.
library(dplyr)
library(ggplot2)
library(rlang)

# Validate optional dependencies at script load time.
requireNamespace("cli")
requireNamespace("crew")
requireNamespace("dbarts")
requireNamespace("ellmer")
requireNamespace("fs")
requireNamespace("jsonlite")
requireNamespace("openxlsx")
requireNamespace("purrr")
requireNamespace("ragnar")
requireNamespace("stringr")
requireNamespace("tibble")
requireNamespace("tidyr")

# Source package helper functions so this file remains orchestration-focused.
tar_source(here::here("R"))

# Set target-level options.
tar_option_set(
  packages = c(
    "dplyr",
    "ggplot2",
    "purrr",
    "rlang",
    "stringr",
    "tibble",
    "tidyr"
  ),
  controller = crew::crew_controller_local(workers = 6)
)

# Load the datasets and base criteria used by this assisted workflow version.
data(
  list = c("gastaldi", "vella", "gastaldi_criteria", "vella_criteria"),
  package = "BaySREn",
  envir = environment()
)

# Keep the dataset scope intentionally narrow for this iteration.
datasets <- c("gastaldi", "vella")

# Keep one run identifier stable across the current script session.
assisted_run_id <- paste0(
  format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y%m%dT%H%M%SZ"),
  "-",
  Sys.getpid()
)

# Map datasets to their packaged base criteria objects.
selection_criteria <- list(
  gastaldi = list(
    include = gastaldi_criteria$include,
    exclude = gastaldi_criteria$exclude
  ),
  vella = list(
    include = vella_criteria$include,
    exclude = vella_criteria$exclude
  )
)

# Keep scenario metadata local to the assisted-screening redesign.
scenario_paths <- list(
  gastaldi = list(
    scenario_type = "protocol_amendment",
    revised_criteria_path = here::here(
      "data-raw",
      "SIIAM",
      "Gastaldi",
      "criteria_revised.R"
    )
  ),
  vella = list(
    scenario_type = "adjudication",
    revision_workbook = here::here(
      "data-raw",
      "SIIAM",
      "Vella",
      "label_revision.xlsx"
    )
  )
)

# Reuse the same three cheap screening models as the main screening workflow.
assisted_labeller_registry <- tibble::tibble(
  labeller_model = c(
    "x-ai/grok-4.1-fast",
    "openai/gpt-oss-20b",
    "openai/gpt-oss-120b"
  ),
  label_api_args = list(
    list(temperature = 0, reasoning = list(effort = "high")),
    list(temperature = 0, reasoning = list(effort = "high")),
    list(temperature = 0, reasoning = list(effort = "high"))
  )
)

# Build the default assisted-screening configuration.
default_assisted_config <- function() {
  list(
    seed_count = 3L,
    seed_model = "openai/gpt-5.1",
    seed_api_args = list(
      reasoning = list(effort = "minimal"),
      temperature = 0
    ),
    labeller_model = assisted_labeller_registry$labeller_model[[1]],
    label_api_args = assisted_labeller_registry$label_api_args[[1]],
    label_response_mode = "structured",
    human_after_warmup = TRUE,
    warmup_refiner = TRUE,
    refiner_model = "openai/gpt-5.1",
    refiner_api_args = list(reasoning = list(effort = "high")),
    warmup_refiner_sample_n = 12L,
    warmup_batch_size = 25L,
    warmup_k_stable = 2L,
    warmup_k_zero_fn = 2L,
    warmup_min_reviewed = 50L,
    warmup_min_positives = 5L,
    warmup_max_reviewed = 300L,
    warmup_max_rounds = 20L,
    vella_contradiction_threshold = 1L,
    gastaldi_signal_rounds = 2L,
    main_max_iterations = 200L,
    main_batch_size = 25L,
    prediction_quantiles = c(0.1, 0.9),
    model_ranking_metric = "Pred_Med",
    rerank_on_new_positives = TRUE,
    bart_n_trees = 200L,
    bart_n_threads = max(1L, parallel::detectCores() - 1L),
    bart_n_chains = 3L,
    bart_n_burn = 300L,
    bart_n_samples = 600L,
    embedding_base_url = "https://api.cohere.ai/compatibility/v1",
    embedding_model = "embed-v4.0",
    embedding_batch_size = 96L,
    human_oracle_upper_bound = TRUE,
    run_id = assisted_run_id,
    cache_root = here::here(
      "experiments",
      "targets_scripts",
      "_target_llm_assisted_screening"
    ),
    run_log_dir = here::here(
      "experiments",
      "targets_scripts",
      "_target_llm_assisted_screening",
      "run_logs",
      assisted_run_id
    )
  )
}

# Build the experiment grid from labeller models and review modes.
selected_experiments <- build_assisted_experiment_grid(
  model_registry = assisted_labeller_registry,
  human_after_warmup_values = c(TRUE, FALSE)
)

# Replace NULL or empty values with a scalar default.
scalar_default <- function(x, default) {
  if (is.null(x) || !length(x)) {
    return(default)
  }
  x
}

# Build a deterministic embedder function for ranking calls.
build_embedder <- function(config) {
  # Materialize the embedding function now so it does not retain config lazily.
  ragnar::embed_openai(
    base_url = config[["embedding_base_url"]],
    api_key = Sys.getenv("COHERE_API_KEY"),
    model = config[["embedding_model"]],
    batch_size = config[["embedding_batch_size"]],
    user = NULL
  )
}

# Normalize list columns before appending tabular logs to disk.
normalize_log_data <- function(data) {
  if (rlang::is_empty(data) || !nrow(data)) {
    return(data)
  }

  data |>
    dplyr::mutate(
      dplyr::across(
        where(is.list),
        \(col) {
          vapply(
            col,
            \(entry) {
              jsonlite::toJSON(entry, auto_unbox = TRUE, null = "null")
            },
            character(1)
          )
        }
      )
    )
}

# Append one structured log table inside the current assisted-run directory.
append_run_log <- function(config, file_name, data) {
  if (rlang::is_empty(data) || !nrow(data)) {
    return(invisible(NULL))
  }

  path <- fs::path(config$run_log_dir, file_name)
  fs::dir_create(fs::path_dir(path))
  data <- normalize_log_data(data)
  file_exists <- fs::file_exists(path)

  utils::write.table(
    data,
    file = path,
    sep = "\t",
    row.names = FALSE,
    col.names = !file_exists,
    quote = TRUE,
    na = "",
    append = file_exists
  )

  invisible(path)
}

# Append one branch-level event with stable metadata for background inspection.
log_branch_event <- function(
  config,
  dataset_name,
  event,
  details = tibble::tibble()
) {
  event_tbl <- tibble::tibble(
    timestamp = format(as.POSIXct(Sys.time(), tz = "UTC"), "%FT%TZ"),
    run_id = config$run_id,
    pid = Sys.getpid(),
    dataset_name = dataset_name,
    experiment_name = scalar_default(config$experiment_name, NA_character_),
    labeller_model = scalar_default(config$labeller_model, NA_character_),
    human_after_warmup = scalar_default(
      config$human_after_warmup,
      NA
    ),
    event = event
  )

  append_run_log(
    config = config,
    file_name = "branch_events.tsv",
    data = dplyr::bind_cols(event_tbl, details)
  )
}

# Persist the seed texts used at one ranking stage.
log_seed_texts <- function(
  config,
  dataset_name,
  stage,
  criteria_version,
  seed_texts
) {
  seed_tbl <- tibble::tibble(
    timestamp = format(as.POSIXct(Sys.time(), tz = "UTC"), "%FT%TZ"),
    run_id = config$run_id,
    dataset_name = dataset_name,
    experiment_name = scalar_default(config$experiment_name, NA_character_),
    stage = stage,
    criteria_version = criteria_version,
    seed_index = seq_along(seed_texts),
    seed_text = unlist(seed_texts)
  )

  append_run_log(
    config = config,
    file_name = "seed_texts.tsv",
    data = seed_tbl
  )
}

# Persist the top-ranked records at each ranking checkpoint.
log_ranking_snapshot <- function(
  config,
  dataset_name,
  stage,
  ranked_data,
  top_n = 25L,
  round_index = NA_integer_,
  iteration = NA_integer_,
  criteria_version = NA_character_
) {
  snapshot <- ranked_data |>
    dplyr::arrange(.data$sorting_id) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::mutate(rank_position = dplyr::row_number()) |>
    dplyr::select(
      "rank_position",
      "id",
      "sorting_id",
      "embedding_score",
      dplyr::any_of(c(
        "title",
        "label_original",
        "label_operational",
        "final_label",
        "Predicted_label",
        "review_source"
      ))
    ) |>
    dplyr::mutate(
      timestamp = format(as.POSIXct(Sys.time(), tz = "UTC"), "%FT%TZ"),
      run_id = config$run_id,
      dataset_name = dataset_name,
      experiment_name = scalar_default(
        config$experiment_name,
        NA_character_
      ),
      stage = stage,
      round = round_index,
      iteration = iteration,
      criteria_version = criteria_version,
      .before = 1
    )

  append_run_log(
    config = config,
    file_name = "ranking_snapshots.tsv",
    data = snapshot
  )
}

# Flatten nested branch outputs before collecting one experiment component.
collect_experiment_component <- function(all_experiment_results, component) {
  all_experiment_results |>
    purrr::flatten() |>
    purrr::map(component) |>
    dplyr::bind_rows()
}

# Load the authoritative revised Gastaldi criteria object from disk.
load_gastaldi_revised_criteria <- function(path) {
  if (!fs::file_exists(path)) {
    cli::cli_abort("Revised criteria file not found at {.file {path}}.")
  }

  criteria_env <- rlang::env()
  sys.source(path, envir = criteria_env)

  if (!exists("gastaldi_criteria_revised", envir = criteria_env)) {
    cli::cli_abort(
      "Expected object {.val gastaldi_criteria_revised} in {.file {path}}."
    )
  }

  criteria_env$gastaldi_criteria_revised
}

# Load the Vella revision workbook and normalize it into a text lookup.
load_vella_revision_lookup <- function(path) {
  if (!fs::file_exists(path)) {
    cli::cli_abort("Revision workbook not found at {.file {path}}.")
  }

  openxlsx::read.xlsx(
    path,
    sheet = "Corrections",
    startRow = 3
  ) |>
    build_vella_revision_lookup()
}

# Resolve the scenario-specific data needed for one dataset branch.
build_scenario_context <- function(dataset_name) {
  scenario_info <- scenario_paths[[dataset_name]]
  if (is.null(scenario_info)) {
    cli::cli_abort("No scenario configuration found for {.field {dataset_name}}.")
  }

  if (identical(scenario_info$scenario_type, "adjudication")) {
    return(list(
      scenario_type = "adjudication",
      revision_lookup = load_vella_revision_lookup(
        scenario_info$revision_workbook
      ),
      revised_criteria = NULL
    ))
  }

  list(
    scenario_type = "protocol_amendment",
    revision_lookup = tibble::tibble(),
    revised_criteria = load_gastaldi_revised_criteria(
      scenario_info$revised_criteria_path
    )
  )
}

# Build the initial dataset state, including scenario-specific truth metadata.
prepare_dataset_data <- function(data, dataset_name, scenario_context) {
  initialize_assisted_dataset_state(
    data = data,
    dataset_name = dataset_name,
    scenario_type = scenario_context$scenario_type,
    revision_lookup = scenario_context$revision_lookup
  )
}

# Build prompt text used to generate a single synthetic seed abstract.
seed_prompt <- function(criteria, seed_index, seed_count) {
  paste(
    "Generate one synthetic abstract for systematic-review screening.",
    "The abstract must represent a prototypical included study.",
    "Write exactly one abstract paragraph and nothing else.",
    "Use plain text only.",
    paste0("This is abstract ", seed_index, " of ", seed_count, "."),
    "<inclusion_criteria>",
    scalar_default(criteria$include, ""),
    "</inclusion_criteria>",
    "<exclusion_criteria>",
    scalar_default(criteria$exclude, ""),
    "</exclusion_criteria>",
    sep = "\n\n"
  )
}

# Generate and cache seed abstracts for a criteria snapshot.
generate_seed_abstracts <- function(
  criteria,
  dataset_name,
  cache_root,
  n_seed = 3L,
  model = "openai/gpt-5.1",
  api_args = list(reasoning = list(effort = "minimal"), temperature = 0)
) {
  criteria_hash <- make_criteria_hash(criteria)
  cache_dir <- fs::path(cache_root, "seed_cache", dataset_name, criteria_hash)
  fs::dir_create(cache_dir)

  chat <- ellmer::chat_openrouter(
    model = model,
    api_args = api_args,
    echo = "none"
  )
  chat$set_system_prompt(
    paste(
      "You create synthetic but realistic abstracts used as embedding seeds.",
      "Return only one abstract paragraph per prompt with no preamble.",
      sep = "\n"
    )
  )

  prompts <- vapply(
    seq_len(n_seed),
    FUN = \(idx) seed_prompt(criteria, idx, n_seed),
    FUN.VALUE = character(1)
  ) |>
    as.character() |>
    as.list()

  solved <- llm_solver(
    prompts = prompts,
    chat = chat,
    schema = NULL,
    cache_dir = cache_dir,
    max_attempts = 10L,
    max_active = min(3L, n_seed),
    rpm = 60
  )

  seed_texts <- purrr::map_chr(solved$text, trimws)

  if (any(!nzchar(seed_texts) | solved$has_error)) {
    cli::cli_abort("Failed to generate all seed abstracts for warmup ranking.")
  }

  seed_texts
}

# Rank records using multiple seed abstracts and aggregate by mean score.
rank_with_seed_abstracts <- function(
  data,
  seed_texts,
  store_location,
  embedder,
  return_embeddings = FALSE
) {
  fs::dir_create(store_location)

  ranking_runs <- purrr::imap(seed_texts, \(seed_text, seed_idx) {
    rank_by_embeddings(
      data = data,
      query = seed_text,
      store_location = store_location,
      embedder = embedder,
      return_embeddings = return_embeddings && seed_idx == 1L
    )
  })

  ranking_data <- purrr::map(ranking_runs, "data")
  aggregate <- aggregate_seed_rankings(ranking_data)

  ranked <- data |>
    dplyr::left_join(
      aggregate |>
        dplyr::select("id", "sorting_id", "embedding_score"),
      by = "id"
    ) |>
    dplyr::arrange(.data$sorting_id)

  out <- list(data = ranked)

  if (isTRUE(return_embeddings)) {
    out$embeddings <- ranking_runs[[1]]$embeddings
  }

  out
}

# Evaluate one model on a record subset for a fixed criteria snapshot.
build_label_prompts <- function(data) {
  # Build the record-level prompts expected by both labeling modes.
  paste(
    "Title: {{title}}\nAbstract: {{abstract}}\n",
    "Authors: {{authors}}\nKeywords: {{keywords}}",
    sep = ""
  ) |>
    ellmer::interpolate(
      title = data$title,
      abstract = data$abstract,
      authors = data$authors,
      keywords = data$keywords
    ) |>
    as.character() |>
    as.list()
}

# Build the minimal structured schema used for assisted-screening labels.
build_structured_label_schema <- function() {
  ellmer::type_object(
    final_decision = ellmer::type_boolean(
      "Whether the record should be included for review"
    ),
    justification = ellmer::type_string(
      "One short sentence naming the decisive evidence"
    )
  )
}

# Build a concise structured system prompt that avoids verbose free-text output.
build_structured_label_system_prompt <- function(criteria) {
  paste(
    "You are screening records for a systematic review.",
    "Use only the criteria below and the provided record text.",
    "Return a structured result with two fields only:",
    "- `final_decision`: TRUE when the record should be kept for review.",
    "- `justification`: one short sentence citing the decisive evidence.",
    "Decision rules:",
    "- Prioritize recall when the record explicitly matches the review objective and no exclusion criterion is triggered.",
    "- Do not invent publication metadata or rely on outside knowledge.",
    "- If the abstract lacks decisive evidence for a required criterion, set `final_decision` to FALSE.",
    "<inclusion criteria>",
    scalar_default(criteria$include, ""),
    "</inclusion criteria>",
    "<exclusion criteria>",
    scalar_default(criteria$exclude, ""),
    "</exclusion criteria>",
    sep = "\n\n"
  )
}

# Evaluate one model on a record subset for a fixed criteria snapshot.
label_with_single_model <- function(
  data,
  criteria,
  model,
  api_args,
  cache_dir,
  response_mode = "structured"
) {
  # Build the shared chat object once per labeling batch.
  chat <- ellmer::chat_openrouter(
    model = model,
    api_args = api_args,
    echo = "none"
  )

  # Route labeling through the structured path when requested.
  if (identical(response_mode, "structured")) {
    chat$set_system_prompt(build_structured_label_system_prompt(criteria))

    classified <- llm_solver(
      prompts = build_label_prompts(
        data |>
          dplyr::select("title", "abstract", "authors", "keywords")
      ),
      chat = chat,
      schema = build_structured_label_schema(),
      cache_dir = cache_dir,
      max_attempts = 20L,
      max_active = min(100L, nrow(data)),
      rpm = 1000,
      cache_batch_size = 50L
    )

    # Validate the wrapped output before coercing the final decision.
    if (is.null(classified) || !is.data.frame(classified)) {
      cli::cli_abort("Structured labeling failed before any rows were returned.")
    }

    # Surface unresolved structured rows without reviving the regex parser.
    if (any(classified$has_error)) {
      failed_list <- paste0(
        which(classified$has_error),
        ": ",
        classified$error_message[classified$has_error]
      )
      cli::cli_alert_warning(
        "{sum(classified$has_error)} structured responses were unresolved after retries: {failed_list}"
      )
      invisible(failed_list)
    }

    return(
      tibble::tibble(
        id = data$id,
        match = as.logical(classified$final_decision)
      )
    )
  }

  # Preserve the existing free-text classifier as an explicit fallback path.
  classified <- classify_citations(
    data = data |>
      dplyr::select("title", "abstract", "authors", "keywords"),
    query = criteria,
    chat = chat,
    cache_dir = cache_dir,
    max_batch_size = min(100L, nrow(data))
  )

  tibble::tibble(
    id = data$id,
    match = as.logical(classified$matches)
  )
}

# Label records with one configured labeller model per experiment branch.
llm_label_records <- function(
  data,
  criteria,
  labeller_model,
  api_args,
  cache_root,
  dataset_name,
  response_mode = "structured"
) {
  if (rlang::is_empty(data) || !nrow(data)) {
    return(
      tibble::tibble(
        id = integer(),
        ai_label = logical(),
        labeller_model = character()
      )
    )
  }

  criteria_hash <- make_criteria_hash(criteria)
  model_cache <- fs::path(
    cache_root,
    "label_cache",
    dataset_name,
    criteria_hash,
    gsub("/", "_", labeller_model)
  )
  fs::dir_create(model_cache)

  labeled <- label_with_single_model(
    data = data,
    criteria = criteria,
    model = labeller_model,
    api_args = api_args,
    cache_dir = model_cache,
    response_mode = response_mode
  )

  tibble::tibble(
    id = data$id,
    ai_label = as.logical(labeled$match),
    labeller_model = labeller_model
  )
}

# Build the reviewed-only prompt used for warmup criteria refinement.
build_warmup_refiner_prompt <- function(
  criteria,
  reviewed_data,
  cumulative_confusion,
  batch_confusion,
  n_total,
  round_index,
  scenario_type,
  sample_n = 12L
) {
  sampled <- select_warmup_refiner_samples(
    reviewed_data = reviewed_data,
    max_total = sample_n
  )

  sampled_text <- if (!nrow(sampled)) {
    "(no reviewed samples)"
  } else {
    sampled |>
      dplyr::mutate(
        human_chr = ifelse(.data$human_label, "included", "excluded"),
        ai_chr = ifelse(.data$ai_label, "included", "excluded"),
        row_txt = paste0(
          "[",
          .data$sample_bucket,
          "] ID ",
          .data$id,
          " | human=",
          .data$human_chr,
          " | ai=",
          .data$ai_chr,
          "\nTitle: ",
          .data$title,
          "\nAbstract: ",
          .data$abstract
        )
      ) |>
      dplyr::pull(.data$row_txt) |>
      paste(collapse = "\n\n---\n\n")
  }

  paste(
    "You refine assisted-screening criteria during warmup.",
    "Use only reviewed records and reviewed metrics.",
    "Return JSON only with keys include, exclude,",
    "protocol_mismatch_signal, and reasoning.",
    "If no change is needed for a field, return the exact string unchanged.",
    "Set protocol_mismatch_signal to true only when the pattern suggests",
    "a protocol-scope mismatch rather than isolated record-level errors.",
    paste0("Warmup round: ", round_index),
    paste0("Scenario type: ", scenario_type),
    paste0("Reviewed records: ", nrow(reviewed_data), " of ", n_total),
    paste0("Batch reviewed: ", batch_confusion$TP + batch_confusion$TN +
      batch_confusion$FP + batch_confusion$FN),
    paste0("Batch FN: ", batch_confusion$FN[[1]]),
    paste0("Batch FP: ", batch_confusion$FP[[1]]),
    paste0("Cumulative FN: ", cumulative_confusion$FN[[1]]),
    paste0("Cumulative FP: ", cumulative_confusion$FP[[1]]),
    "<current_include>",
    scalar_default(criteria$include, ""),
    "</current_include>",
    "<current_exclude>",
    scalar_default(criteria$exclude, ""),
    "</current_exclude>",
    "<reviewed_samples>",
    sampled_text,
    "</reviewed_samples>",
    paste0(
      "{\"include\":\"unchanged|updated text\",",
      "\"exclude\":\"unchanged|updated text\",",
      "\"protocol_mismatch_signal\":true|false,",
      "\"reasoning\":\"short rationale\"}"
    ),
    sep = "\n\n"
  )
}

# Run one high-level warmup refinement pass and parse the proposed criteria.
refine_warmup_criteria <- function(
  criteria,
  reviewed_data,
  cumulative_confusion,
  batch_confusion,
  n_total,
  round_index,
  model,
  api_args,
  cache_root,
  dataset_name,
  scenario_type,
  sample_n
) {
  if (rlang::is_empty(reviewed_data) || !nrow(reviewed_data)) {
    return(list(
      criteria = criteria,
      criteria_change_suggested = FALSE,
      protocol_mismatch_signal = FALSE,
      reasoning = "No reviewed records available."
    ))
  }

  criteria_hash <- make_criteria_hash(criteria)
  cache_dir <- fs::path(
    cache_root,
    "warmup_refiner",
    dataset_name,
    criteria_hash,
    paste0("round_", round_index)
  )
  fs::dir_create(cache_dir)

  chat <- ellmer::chat_openrouter(
    model = model,
    api_args = api_args,
    echo = "none"
  )
  chat$set_system_prompt(
    paste(
      "You refine inclusion and exclusion criteria during warmup.",
      "Use only reviewed evidence and return JSON only.",
      sep = "\n"
    )
  )

  prompt <- build_warmup_refiner_prompt(
    criteria = criteria,
    reviewed_data = reviewed_data,
    cumulative_confusion = cumulative_confusion,
    batch_confusion = batch_confusion,
    n_total = n_total,
    round_index = round_index,
    scenario_type = scenario_type,
    sample_n = sample_n
  )

  schema <- ellmer::type_object(
    include = ellmer::type_string("unchanged or updated text"),
    exclude = ellmer::type_string("unchanged or updated text"),
    protocol_mismatch_signal = ellmer::type_boolean("true or false"),
    reasoning = ellmer::type_string("short rationale")
  )

  solved <- llm_solver(
    prompts = list(prompt),
    chat = chat,
    schema = schema,
    cache_dir = cache_dir,
    max_attempts = 10L,
    max_active = 1L
  )

  if (isTRUE(solved$has_error[[1]])) {
    return(list(
      criteria = criteria,
      criteria_change_suggested = FALSE,
      protocol_mismatch_signal = FALSE,
      reasoning = "Warmup refinement parse failed; kept criteria unchanged."
    ))
  }

  refined_include <- scalar_default(solved$include[[1]], "unchanged") |>
    as.character() |>
    trimws()
  refined_exclude <- scalar_default(solved$exclude[[1]], "unchanged") |>
    as.character() |>
    trimws()
  protocol_mismatch_signal <- scalar_default(
    solved$protocol_mismatch_signal[[1]],
    FALSE
  )

  refined_criteria <- list(
    include = if (
      identical(tolower(refined_include), "unchanged")
    ) {
      criteria$include
    } else {
      refined_include
    },
    exclude = if (
      identical(tolower(refined_exclude), "unchanged")
    ) {
      criteria$exclude
    } else {
      refined_exclude
    }
  )

  list(
    criteria = refined_criteria,
    criteria_change_suggested = criteria_changed(criteria, refined_criteria),
    protocol_mismatch_signal = isTRUE(protocol_mismatch_signal),
    reasoning = scalar_default(solved$reasoning[[1]], "No rationale provided.")
  )
}

# Record one reviewed batch back into the dataset-wide state table.
update_state_from_review_batch <- function(state, review_batch) {
  update_idx <- match(review_batch$id, state$id)

  fields <- c(
    "label_operational",
    "operative_truth_version",
    "adjudication_triggered",
    "adjudication_applied",
    "adjudication_round",
    "contradiction_count",
    "criteria_version",
    "criteria_version_reviewed",
    "human_label",
    "ai_label",
    "final_label",
    "review_source",
    "review_round"
  )

  for (field_name in fields) {
    state[[field_name]][update_idx] <- review_batch[[field_name]]
  }

  state
}

# Run the common warmup workflow with scenario-specific validity handling.
run_warmup_phase <- function(
  dataset_state,
  base_criteria,
  scenario_context,
  dataset_name,
  embedding_store_location,
  config
) {
  state <- dataset_state
  embedder <- build_embedder(config)
  current_criteria <- base_criteria
  current_criteria_version <- "original"
  protocol_amendment_applied <- FALSE
  protocol_amendment_round <- NA_integer_
  post_amendment_refinement_pending <- FALSE
  post_amendment_refinement_completed <- FALSE
  residual_issue_n <- 0L

  warmup_log <- tibble::tibble(
    round = integer(),
    criteria_version = character(),
    criteria_version_before = character(),
    criteria_version_after = character(),
    reviewed_n = integer(),
    reviewed_positive_n = integer(),
    batch_reviewed_n = integer(),
    batch_FN = integer(),
    batch_FP = integer(),
    cumulative_FN = integer(),
    cumulative_FP = integer(),
    batch_se_num = integer(),
    batch_se_denom = integer(),
    batch_se = double(),
    batch_sp_num = integer(),
    batch_sp_denom = integer(),
    batch_sp = double(),
    batch_ppv_num = integer(),
    batch_ppv_denom = integer(),
    batch_ppv = double(),
    cumulative_se_num = integer(),
    cumulative_se_denom = integer(),
    cumulative_se = double(),
    cumulative_sp_num = integer(),
    cumulative_sp_denom = integer(),
    cumulative_sp = double(),
    cumulative_ppv_num = integer(),
    cumulative_ppv_denom = integer(),
    cumulative_ppv = double(),
    criteria_change_suggested = logical(),
    protocol_mismatch_signal = logical(),
    criteria_changed = logical(),
    stable_streak = integer(),
    zero_fn_streak = integer(),
    stop_now = logical(),
    cap_hit = logical(),
    protocol_amendment_applied = logical(),
    protocol_amendment_round = integer(),
    post_amendment_refinement_completed = logical(),
    residual_issue_logged = logical(),
    residual_issue_n = integer(),
    refiner_reasoning = character()
  )

  review_log <- tibble::tibble()
  stop_reason <- "warmup_max_rounds"

  for (round_idx in seq_len(config$warmup_max_rounds)) {
    cli::cli_alert_info(
      "Warmup {.field {dataset_name}} round {.val {round_idx}}"
    )

    criteria_version_before <- current_criteria_version

    # Rebuild the seed ranking for the current criteria snapshot.
    seed_texts <- generate_seed_abstracts(
      criteria = current_criteria,
      dataset_name = dataset_name,
      cache_root = config$cache_root,
      n_seed = config$seed_count,
      model = config$seed_model,
      api_args = config$seed_api_args
    )
    log_seed_texts(
      config = config,
      dataset_name = dataset_name,
      stage = "warmup_seed_generation",
      criteria_version = criteria_version_before,
      seed_texts = seed_texts
    )

    ranked <- rank_with_seed_abstracts(
      data = state,
      seed_texts = seed_texts,
      store_location = embedding_store_location,
      embedder = embedder,
      return_embeddings = FALSE
    )
    log_ranking_snapshot(
      config = config,
      dataset_name = dataset_name,
      stage = "warmup_seed_ranking",
      ranked_data = ranked$data,
      round_index = round_idx,
      criteria_version = criteria_version_before
    )

    # Review only unseen records during warmup.
    candidates <- ranked$data |>
      dplyr::filter(is.na(.data$final_label)) |>
      dplyr::arrange(.data$sorting_id) |>
      dplyr::slice_head(n = config$warmup_batch_size)

    if (!nrow(candidates)) {
      stop_reason <- "warmup_no_candidates"
      break
    }

    # Label the warmup batch with one configured labeller model.
    ai_labels <- llm_label_records(
      data = candidates,
      criteria = current_criteria,
      labeller_model = config$labeller_model,
      api_args = config$label_api_args,
      cache_root = config$cache_root,
      dataset_name = dataset_name,
      response_mode = config$label_response_mode
    )

    reviewed_batch <- candidates |>
      dplyr::select(
        "id",
        "record_key",
        "dataset_name",
        "scenario_type",
        "title",
        "abstract",
        "label_original",
        "label_revised_candidate",
        "revision_entry_available",
        "label_operational",
        "operative_truth_version",
        "adjudication_triggered",
        "adjudication_applied",
        "adjudication_round",
        "contradiction_count"
      ) |>
      dplyr::left_join(ai_labels, by = "id")

    # Apply record-level adjudication only in the Vella scenario.
    if (identical(scenario_context$scenario_type, "adjudication")) {
      reviewed_batch <- apply_vella_adjudication(
        review_batch = reviewed_batch,
        round_index = round_idx,
        contradiction_threshold = config$vella_contradiction_threshold
      )
    } else {
      reviewed_batch <- reviewed_batch |>
        dplyr::mutate(human_label = .data$label_operational)
    }

    # Treat warmup review labels as an oracle upper bound in the simulation.
    reviewed_batch <- reviewed_batch |>
      dplyr::mutate(
        criteria_version = criteria_version_before,
        criteria_version_reviewed = criteria_version_before,
        final_label = ifelse(.data$human_label, "y", "n"),
        review_source = "human_warmup",
      review_round = round_idx,
      review_phase = paste0("warmup_", criteria_version_before)
    )

    state <- update_state_from_review_batch(state, reviewed_batch)

    reviewed_state <- state |>
      dplyr::filter(!is.na(.data$final_label))

    batch_confusion <- compute_review_confusion(
      human_label = reviewed_batch$human_label,
      ai_label = reviewed_batch$ai_label
    )
    cumulative_confusion <- compute_review_confusion(
      human_label = reviewed_state$human_label,
      ai_label = reviewed_state$ai_label
    )
    batch_metrics <- summarise_confusion_metrics(
      TP = batch_confusion$TP[[1]],
      FP = batch_confusion$FP[[1]],
      TN = batch_confusion$TN[[1]],
      FN = batch_confusion$FN[[1]]
    )
    cumulative_metrics <- summarise_confusion_metrics(
      TP = cumulative_confusion$TP[[1]],
      FP = cumulative_confusion$FP[[1]],
      TN = cumulative_confusion$TN[[1]],
      FN = cumulative_confusion$FN[[1]]
    )

    refiner_result <- list(
      criteria = current_criteria,
      criteria_change_suggested = FALSE,
      protocol_mismatch_signal = FALSE,
      reasoning = "Warmup refinement disabled for this round."
    )

    # Run warmup refinement in both scenarios while Gastaldi still allows it.
    refinement_allowed <- isTRUE(config$warmup_refiner) && (
      !identical(scenario_context$scenario_type, "protocol_amendment") ||
        !isTRUE(protocol_amendment_applied) ||
        isTRUE(post_amendment_refinement_pending)
    )

    if (refinement_allowed) {
      refiner_result <- refine_warmup_criteria(
        criteria = current_criteria,
        reviewed_data = reviewed_state,
        cumulative_confusion = cumulative_confusion,
        batch_confusion = batch_confusion,
        n_total = nrow(state),
        round_index = round_idx,
        model = config$refiner_model,
        api_args = config$refiner_api_args,
        cache_root = config$cache_root,
        dataset_name = dataset_name,
        scenario_type = scenario_context$scenario_type,
        sample_n = config$warmup_refiner_sample_n
      )
    }

    criteria_change_suggested <- isTRUE(refiner_result$criteria_change_suggested)
    protocol_mismatch_signal <- isTRUE(refiner_result$protocol_mismatch_signal)
    criteria_changed_now <- FALSE
    criteria_version_after <- criteria_version_before
    residual_issue_logged <- FALSE

    # Apply regular warmup refinement before any scenario-specific switch.
    if (
      criteria_change_suggested &&
        !(
          identical(scenario_context$scenario_type, "protocol_amendment") &&
            isTRUE(protocol_amendment_applied) &&
            !isTRUE(post_amendment_refinement_pending)
        )
    ) {
      current_criteria <- refiner_result$criteria
      criteria_changed_now <- TRUE
      criteria_version_after <- if (
        identical(criteria_version_before, "revised")
      ) {
        "revised_refined"
      } else {
        criteria_version_before
      }
      current_criteria_version <- criteria_version_after
    }

    # Apply the Gastaldi amendment only once, from the authoritative source.
    if (
      identical(scenario_context$scenario_type, "protocol_amendment") &&
        !isTRUE(protocol_amendment_applied) &&
        should_trigger_protocol_amendment(
          warmup_log = dplyr::bind_rows(
            warmup_log,
            tibble::tibble(
              criteria_version = criteria_version_before,
              protocol_mismatch_signal = protocol_mismatch_signal,
              batch_FN = batch_confusion$FN[[1]]
            )
          ),
          k_evidence = config$gastaldi_signal_rounds
        )
    ) {
      current_criteria <- scenario_context$revised_criteria
      current_criteria_version <- "revised"
      criteria_version_after <- current_criteria_version
      criteria_changed_now <- TRUE
      protocol_amendment_applied <- TRUE
      protocol_amendment_round <- round_idx
      post_amendment_refinement_pending <- TRUE
    } else if (
      identical(scenario_context$scenario_type, "protocol_amendment") &&
        isTRUE(post_amendment_refinement_pending)
    ) {
      post_amendment_refinement_pending <- FALSE
      post_amendment_refinement_completed <- TRUE

      residual_state <- update_gastaldi_residual_issues(
        residual_issue_n = residual_issue_n,
        batch_fn = batch_confusion$FN[[1]],
        criteria_change_suggested = criteria_change_suggested,
        protocol_mismatch_signal = protocol_mismatch_signal
      )
      residual_issue_logged <- residual_state$residual_issue_logged
      residual_issue_n <- residual_state$residual_issue_n
    } else if (
      identical(scenario_context$scenario_type, "protocol_amendment") &&
        isTRUE(post_amendment_refinement_completed)
    ) {
      residual_state <- update_gastaldi_residual_issues(
        residual_issue_n = residual_issue_n,
        batch_fn = batch_confusion$FN[[1]],
        criteria_change_suggested = FALSE,
        protocol_mismatch_signal = FALSE
      )
      residual_issue_logged <- residual_state$residual_issue_logged
      residual_issue_n <- residual_state$residual_issue_n
    }

    warmup_log <- dplyr::bind_rows(
      warmup_log,
      tibble::tibble(
        round = round_idx,
        criteria_version = criteria_version_before,
        criteria_version_before = criteria_version_before,
        criteria_version_after = criteria_version_after,
        reviewed_n = nrow(reviewed_state),
        reviewed_positive_n = sum(reviewed_state$human_label, na.rm = TRUE),
        batch_reviewed_n = nrow(reviewed_batch),
        batch_FN = batch_confusion$FN,
        batch_FP = batch_confusion$FP,
        cumulative_FN = cumulative_confusion$FN,
        cumulative_FP = cumulative_confusion$FP,
        batch_se_num = batch_metrics$se_num,
        batch_se_denom = batch_metrics$se_denom,
        batch_se = batch_metrics$se,
        batch_sp_num = batch_metrics$sp_num,
        batch_sp_denom = batch_metrics$sp_denom,
        batch_sp = batch_metrics$sp,
        batch_ppv_num = batch_metrics$ppv_num,
        batch_ppv_denom = batch_metrics$ppv_denom,
        batch_ppv = batch_metrics$ppv,
        cumulative_se_num = cumulative_metrics$se_num,
        cumulative_se_denom = cumulative_metrics$se_denom,
        cumulative_se = cumulative_metrics$se,
        cumulative_sp_num = cumulative_metrics$sp_num,
        cumulative_sp_denom = cumulative_metrics$sp_denom,
        cumulative_sp = cumulative_metrics$sp,
        cumulative_ppv_num = cumulative_metrics$ppv_num,
        cumulative_ppv_denom = cumulative_metrics$ppv_denom,
        cumulative_ppv = cumulative_metrics$ppv,
        criteria_change_suggested = criteria_change_suggested,
        protocol_mismatch_signal = protocol_mismatch_signal,
        criteria_changed = criteria_changed_now,
        stable_streak = 0L,
        zero_fn_streak = 0L,
        stop_now = FALSE,
        cap_hit = FALSE,
        protocol_amendment_applied = protocol_amendment_applied,
        protocol_amendment_round = protocol_amendment_round,
        post_amendment_refinement_completed =
          post_amendment_refinement_completed,
        residual_issue_logged = residual_issue_logged,
        residual_issue_n = residual_issue_n,
        refiner_reasoning = scalar_default(refiner_result$reasoning, "")
      )
    )
    append_run_log(
      config = config,
      file_name = "warmup_rounds.tsv",
      data = warmup_log |>
        dplyr::slice_tail(n = 1) |>
        dplyr::mutate(
          dataset_name = dataset_name,
          experiment_name = config$experiment_name,
          labeller_model = config$labeller_model,
          human_after_warmup = config$human_after_warmup,
          .before = 1
        )
    )

    stop_state <- should_stop_warmup(
      warmup_log = warmup_log,
      k_stable = config$warmup_k_stable,
      k_zero_fn = config$warmup_k_zero_fn,
      min_reviewed = config$warmup_min_reviewed,
      min_positives = config$warmup_min_positives,
      max_reviewed = config$warmup_max_reviewed,
      max_rounds = config$warmup_max_rounds
    )

    warmup_log$stable_streak[[nrow(warmup_log)]] <- stop_state$stable_streak
    warmup_log$zero_fn_streak[[nrow(warmup_log)]] <- stop_state$zero_fn_streak
    warmup_log$stop_now[[nrow(warmup_log)]] <- stop_state$stop
    warmup_log$cap_hit[[nrow(warmup_log)]] <- stop_state$cap_hit

    review_log <- dplyr::bind_rows(
      review_log,
      reviewed_batch |>
        dplyr::select(
          "id",
          "record_key",
          "dataset_name",
          "scenario_type",
          "title",
          "label_original",
          "label_revised_candidate",
          "label_operational",
          "operative_truth_version",
          "ai_label",
          "human_label",
          "final_label",
          "review_source",
          "review_round",
          "review_phase",
          "criteria_version",
          "labeller_model",
          "adjudication_triggered",
          "adjudication_applied",
          "adjudication_round"
        )
    )

    if (isTRUE(stop_state$stop)) {
      stop_reason <- "warmup_confirmation"
      break
    }

    if (isTRUE(stop_state$cap_hit)) {
      stop_reason <- "warmup_caps"
      break
    }
  }

  state$criteria_version <- current_criteria_version

  list(
    state = state,
    review_log = review_log,
    warmup_log = warmup_log,
    final_criteria = current_criteria,
    criteria_version = current_criteria_version,
    stop_reason = stop_reason,
    protocol_amendment_applied = protocol_amendment_applied,
    protocol_amendment_round = protocol_amendment_round,
    post_amendment_refinement_completed = post_amendment_refinement_completed,
    residual_issue_n = residual_issue_n
  )
}

# Convert embedding list-columns into fixed numeric feature columns.
embedding_features_from_rank <- function(embedding_tbl) {
  clean_tbl <- embedding_tbl |>
    dplyr::distinct(.data$id, .keep_all = TRUE) |>
    dplyr::arrange(.data$id)

  embedding_data <- clean_tbl$embedding |> as.data.frame()
  names(embedding_data) <- paste0("emb", seq_len(ncol(embedding_data)))

  dplyr::bind_cols(
    tibble::tibble(id = clean_tbl$id),
    tibble::as_tibble(embedding_data)
  )
}

# Run the post-warmup assisted-screening loop with frozen criteria.
run_assisted_screening <- function(
  warmup_result,
  dataset_name,
  embedding_store_location,
  config
) {
  state <- warmup_result$state
  criteria <- warmup_result$final_criteria
  criteria_version <- warmup_result$criteria_version
  embedder <- build_embedder(config)

  # Rebuild ranking features from the final frozen criteria snapshot.
  seed_texts <- generate_seed_abstracts(
    criteria = criteria,
    dataset_name = dataset_name,
    cache_root = config$cache_root,
    n_seed = config$seed_count,
    model = config$seed_model,
    api_args = config$seed_api_args
  )
  log_seed_texts(
    config = config,
    dataset_name = dataset_name,
    stage = "main_seed_generation",
    criteria_version = criteria_version,
    seed_texts = seed_texts
  )

  ranking <- rank_with_seed_abstracts(
    data = state,
    seed_texts = seed_texts,
    store_location = embedding_store_location,
    embedder = embedder,
    return_embeddings = TRUE
  )
  log_ranking_snapshot(
    config = config,
    dataset_name = dataset_name,
    stage = "main_seed_ranking",
    ranked_data = ranking$data,
    criteria_version = criteria_version
  )

  feature_tbl <- embedding_features_from_rank(ranking$embeddings)

  state <- state |>
    dplyr::select(
      -dplyr::any_of(c(
        "sorting_id",
        "embedding_score",
        names(feature_tbl)[names(feature_tbl) != "id"],
        "Pred_Med",
        "Pred_Low",
        "Pred_Up",
        "Predicted_label"
      ))
    ) |>
    dplyr::left_join(
      ranking$data |>
        dplyr::select("id", "sorting_id", "embedding_score"),
      by = "id"
    ) |>
    dplyr::left_join(feature_tbl, by = "id") |>
    dplyr::mutate(
      Pred_Med = NA_real_,
      Pred_Low = NA_real_,
      Pred_Up = NA_real_,
      Predicted_label = NA_character_,
      criteria_version = criteria_version
    )

  iteration_log <- tibble::tibble(
    iteration = integer(),
    reviewed_total = integer(),
    reviewed_human = integer(),
    reviewed_ai = integer(),
    new_positives = integer(),
    new_true_positives = integer(),
    new_false_positives = integer(),
    batch_se_num = integer(),
    batch_se_denom = integer(),
    batch_se = double(),
    batch_sp_num = integer(),
    batch_sp_denom = integer(),
    batch_sp = double(),
    batch_ppv_num = integer(),
    batch_ppv_denom = integer(),
    batch_ppv = double(),
    cumulative_se_num = integer(),
    cumulative_se_denom = integer(),
    cumulative_se = double(),
    cumulative_sp_num = integer(),
    cumulative_sp_denom = integer(),
    cumulative_sp = double(),
    cumulative_ppv_num = integer(),
    cumulative_ppv_denom = integer(),
    cumulative_ppv = double(),
    rerank_triggered = logical(),
    candidates_remaining = integer(),
    mode = character()
  )

  review_log <- tibble::tibble()
  feature_cols <- names(feature_tbl) |> setdiff("id")
  stop_reason <- "main_max_iterations"

  for (iter_idx in seq_len(config$main_max_iterations)) {
    reviewed <- state |>
      dplyr::filter(!is.na(.data$final_label))

    can_model <- nrow(reviewed) >= 10 &&
      any(reviewed$final_label %in% "y") &&
      any(reviewed$final_label %in% "n")

    candidates <- NULL
    mode <- "embedding_fallback"

    # Use BART uncertainty only once both classes have been reviewed.
    if (can_model) {
      mode <- "bart_uncertainty"

      x_train <- reviewed |>
        dplyr::select(dplyr::all_of(feature_cols)) |>
        as.matrix()
      x_test <- state |>
        dplyr::select(dplyr::all_of(feature_cols)) |>
        as.matrix()
      y_train <- as.numeric(reviewed$final_label %in% "y")

      model <- dbarts::bart2(
        Y ~ .,
        data = data.frame(Y = y_train, as.data.frame(x_train)),
        n.trees = config$bart_n_trees,
        n.threads = config$bart_n_threads,
        n.chains = config$bart_n_chains,
        n.burn = config$bart_n_burn,
        n.samples = config$bart_n_samples,
        verbose = FALSE,
        keepTrees = TRUE,
        keepTrainingFits = TRUE
      )

      preds <- predict(model, newdata = as.data.frame(x_test))
      quant_df <- apply(
        t(preds),
        1,
        quantile,
        c(0.5, config$prediction_quantiles)
      ) |>
        t() |>
        as.data.frame() |>
        stats::setNames(c("Pred_Med", "Pred_Low", "Pred_Up"))

      prediction_tbl <- dplyr::bind_cols(
        state |> dplyr::select("id", "final_label"),
        quant_df
      ) |>
        dplyr::mutate(
          Predicted_label = assign_uncertainty_zone_labels(
            pred_low = .data$Pred_Low,
            pred_up = .data$Pred_Up,
            target = .data$final_label
          )
        )

      state <- state |>
        dplyr::select(-dplyr::any_of(c(
          "Pred_Med",
          "Pred_Low",
          "Pred_Up",
          "Predicted_label"
        ))) |>
        dplyr::left_join(
          prediction_tbl |>
            dplyr::select(
              "id",
              "Pred_Med",
              "Pred_Low",
              "Pred_Up",
              "Predicted_label"
            ),
          by = "id"
        )

      rank_metric <- config$model_ranking_metric
      candidates <- state |>
        dplyr::filter(
          is.na(.data$final_label),
          .data$Predicted_label %in% c("y", "unk")
        ) |>
        dplyr::arrange(dplyr::desc(.data[[rank_metric]]))
    } else {
      candidates <- state |>
        dplyr::filter(is.na(.data$final_label)) |>
        dplyr::arrange(.data$sorting_id)
    }

    if (!nrow(candidates)) {
      stop_reason <- "main_no_candidates"
      break
    }

    candidates <- candidates |>
      dplyr::slice_head(n = config$main_batch_size)

    # Score the current main-loop batch with the frozen criteria.
    ai_tbl <- llm_label_records(
      data = candidates,
      criteria = criteria,
      labeller_model = config$labeller_model,
      api_args = config$label_api_args,
      cache_root = config$cache_root,
      dataset_name = dataset_name,
      response_mode = config$label_response_mode
    )

    update_tbl <- candidates |>
      dplyr::select(
        "id",
        "record_key",
        "dataset_name",
        "scenario_type",
        "title",
        "abstract",
        "label_original",
        "label_revised_candidate",
        "revision_entry_available",
        "label_operational",
        "operative_truth_version",
        "adjudication_triggered",
        "adjudication_applied",
        "adjudication_round",
        "contradiction_count"
      ) |>
      dplyr::left_join(ai_tbl, by = "id")

    # Apply Vella adjudication before choosing the simulated reviewer action.
    if (identical(dplyr::first(state$scenario_type), "adjudication")) {
      update_tbl <- apply_vella_adjudication(
        review_batch = update_tbl,
        round_index = iter_idx,
        contradiction_threshold = config$vella_contradiction_threshold
      )
    } else {
      update_tbl <- update_tbl |>
        dplyr::mutate(human_label = .data$label_operational)
    }

    # Treat human labels as an oracle upper bound in this simulation.
    update_tbl <- update_tbl |>
      dplyr::mutate(
        criteria_version = criteria_version,
        criteria_version_reviewed = criteria_version,
        review_phase = paste0("main_", criteria_version),
        final_bool = if (isTRUE(config$human_after_warmup)) {
          .data$human_label
        } else {
          .data$ai_label
        },
        final_label = ifelse(.data$final_bool, "y", "n"),
        review_source = if (isTRUE(config$human_after_warmup)) {
          "human_after_warmup"
        } else {
          "ai_after_warmup"
        },
        review_round = iter_idx
      )

    state <- update_state_from_review_batch(state, update_tbl)
    batch_confusion <- compute_review_confusion(
      human_label = update_tbl$human_label,
      ai_label = update_tbl$ai_label
    )
    cumulative_confusion <- compute_review_confusion(
      human_label = state$human_label[!is.na(state$final_label)],
      ai_label = state$ai_label[!is.na(state$final_label)]
    )
    batch_metrics <- summarise_confusion_metrics(
      TP = batch_confusion$TP[[1]],
      FP = batch_confusion$FP[[1]],
      TN = batch_confusion$TN[[1]],
      FN = batch_confusion$FN[[1]]
    )
    cumulative_metrics <- summarise_confusion_metrics(
      TP = cumulative_confusion$TP[[1]],
      FP = cumulative_confusion$FP[[1]],
      TN = cumulative_confusion$TN[[1]],
      FN = cumulative_confusion$FN[[1]]
    )

    new_positives <- sum(update_tbl$final_label %in% "y", na.rm = TRUE)
    new_true_positives <- sum(
      update_tbl$human_label & update_tbl$final_label %in% "y",
      na.rm = TRUE
    )
    new_false_positives <- sum(
      !update_tbl$human_label & update_tbl$final_label %in% "y",
      na.rm = TRUE
    )
    rerank_triggered <- isTRUE(config$rerank_on_new_positives) &&
      new_positives > 0

    # Keep the current AI-only reranking mechanic, but log TP/FP inflow.
    if (rerank_triggered) {
      positive_query <- state |>
        dplyr::filter(.data$id %in% update_tbl$id, .data$final_label %in% "y") |>
        dplyr::mutate(
          snippet = paste0(
            "Title: ",
            .data$title,
            "\nKeywords: ",
            .data$keywords,
            "\nAbstract: ",
            .data$abstract
          )
        ) |>
        dplyr::pull(.data$snippet) |>
        paste(collapse = "\n\n")

      reranked <- rank_by_embeddings(
        data = state,
        query = positive_query,
        store_location = embedding_store_location,
        embedder = embedder,
        return_embeddings = FALSE
      )

      state <- state |>
        dplyr::select(-"sorting_id", -"embedding_score") |>
        dplyr::left_join(
          reranked$data |>
            dplyr::select("id", "sorting_id", "embedding_score"),
          by = "id"
        )
      log_ranking_snapshot(
        config = config,
        dataset_name = dataset_name,
        stage = "main_rerank",
        ranked_data = state,
        iteration = iter_idx,
        criteria_version = criteria_version
      )
    }

    remaining <- state |>
      dplyr::filter(is.na(.data$final_label))

    remaining_candidates <- if (can_model) {
      sum(remaining$Predicted_label %in% c("y", "unk"), na.rm = TRUE)
    } else {
      nrow(remaining)
    }

    iteration_log <- dplyr::bind_rows(
      iteration_log,
      tibble::tibble(
        iteration = iter_idx,
        reviewed_total = sum(!is.na(state$final_label)),
        reviewed_human = sum(
          grepl("^human", state$review_source),
          na.rm = TRUE
        ),
        reviewed_ai = sum(
          grepl("^ai", state$review_source),
          na.rm = TRUE
        ),
        new_positives = new_positives,
        new_true_positives = new_true_positives,
        new_false_positives = new_false_positives,
        batch_se_num = batch_metrics$se_num,
        batch_se_denom = batch_metrics$se_denom,
        batch_se = batch_metrics$se,
        batch_sp_num = batch_metrics$sp_num,
        batch_sp_denom = batch_metrics$sp_denom,
        batch_sp = batch_metrics$sp,
        batch_ppv_num = batch_metrics$ppv_num,
        batch_ppv_denom = batch_metrics$ppv_denom,
        batch_ppv = batch_metrics$ppv,
        cumulative_se_num = cumulative_metrics$se_num,
        cumulative_se_denom = cumulative_metrics$se_denom,
        cumulative_se = cumulative_metrics$se,
        cumulative_sp_num = cumulative_metrics$sp_num,
        cumulative_sp_denom = cumulative_metrics$sp_denom,
        cumulative_sp = cumulative_metrics$sp,
        cumulative_ppv_num = cumulative_metrics$ppv_num,
        cumulative_ppv_denom = cumulative_metrics$ppv_denom,
        cumulative_ppv = cumulative_metrics$ppv,
        rerank_triggered = rerank_triggered,
        candidates_remaining = remaining_candidates,
        mode = mode
      )
    )
    append_run_log(
      config = config,
      file_name = "iteration_metrics.tsv",
      data = iteration_log |>
        dplyr::slice_tail(n = 1) |>
        dplyr::mutate(
          dataset_name = dataset_name,
          experiment_name = config$experiment_name,
          labeller_model = config$labeller_model,
          human_after_warmup = config$human_after_warmup,
          .before = 1
        )
    )

    review_log <- dplyr::bind_rows(
      review_log,
      update_tbl |>
        dplyr::select(
          "id",
          "record_key",
          "dataset_name",
          "scenario_type",
          "title",
          "label_original",
          "label_revised_candidate",
          "label_operational",
          "operative_truth_version",
          "ai_label",
          "human_label",
          "final_label",
          "review_source",
          "review_round",
          "review_phase",
          "criteria_version",
          "labeller_model",
          "adjudication_triggered",
          "adjudication_applied",
          "adjudication_round"
        )
    )

    if (remaining_candidates == 0) {
      stop_reason <- "main_no_remaining_candidates"
      break
    }
  }

  all_review_log <- dplyr::bind_rows(warmup_result$review_log, review_log)

  final_metrics <- dplyr::bind_rows(
    summarise_assisted_metrics(
      state = state,
      truth_column = "label_original",
      metric_scope = "original_history"
    ),
    summarise_assisted_metrics(
      state = state,
      truth_column = "label_operational",
      metric_scope = "operative_final"
    )
  ) |>
    dplyr::mutate(
      criteria_version_final = criteria_version,
      protocol_amendment_applied = warmup_result$protocol_amendment_applied,
      protocol_amendment_round = warmup_result$protocol_amendment_round,
      post_amendment_refinement_completed =
        warmup_result$post_amendment_refinement_completed,
      residual_issue_n = warmup_result$residual_issue_n,
      adjudications_triggered = sum(
        all_review_log$adjudication_triggered,
        na.rm = TRUE
      ),
      adjudications_applied = sum(
        all_review_log$adjudication_applied,
        na.rm = TRUE
      ),
      operative_truth_version = dplyr::case_when(
        all(state$operative_truth_version %in% "original") ~ "original",
        all(state$operative_truth_version %in% "revised") ~ "revised",
        TRUE ~ "mixed"
      ),
      human_oracle_upper_bound = isTRUE(config$human_oracle_upper_bound)
    )

  phase_metrics <- summarise_review_phase_metrics(all_review_log) |>
    dplyr::mutate(
      protocol_amendment_applied = warmup_result$protocol_amendment_applied,
      protocol_amendment_round = warmup_result$protocol_amendment_round,
      post_amendment_refinement_completed =
        warmup_result$post_amendment_refinement_completed,
      residual_issue_n = warmup_result$residual_issue_n
    )

  adjudication_log <- all_review_log |>
    dplyr::filter(.data$adjudication_triggered) |>
    dplyr::select(
      "dataset_name",
      "review_phase",
      "review_round",
      "id",
      "record_key",
      "title",
      "label_original",
      "label_revised_candidate",
      "label_operational",
      "operative_truth_version",
      "labeller_model",
      "adjudication_triggered",
      "adjudication_applied",
      "adjudication_round"
    )

  protocol_event <- if (isTRUE(warmup_result$protocol_amendment_applied)) {
    tibble::tibble(
      dataset_name = dataset_name,
      protocol_amendment_applied = TRUE,
      protocol_amendment_round = warmup_result$protocol_amendment_round,
      criteria_version_final = criteria_version,
      post_amendment_refinement_completed =
        warmup_result$post_amendment_refinement_completed,
      residual_issue_n = warmup_result$residual_issue_n
    )
  } else {
    tibble::tibble()
  }

  list(
    state = state,
    review_log = all_review_log,
    iteration_log = iteration_log,
    final_metrics = final_metrics,
    phase_metrics = phase_metrics,
    adjudication_log = adjudication_log,
    protocol_event = protocol_event,
    seed_texts = seed_texts,
    stop_reason = stop_reason
  )
}

# Run one full experiment branch for one dataset and one parameter set.
run_assisted_experiment <- function(
  dataset_data,
  base_criteria,
  scenario_context,
  dataset_name,
  experiment_name,
  params,
  embedding_store_location
) {
  config <- utils::modifyList(
    default_assisted_config(),
    list(
      experiment_name = experiment_name,
      labeller_model = params$labeller_model[[1]],
      label_api_args = params$label_api_args[[1]],
      human_after_warmup = params$human_after_warmup[[1]]
    )
  )
  log_branch_event(
    config = config,
    dataset_name = dataset_name,
    event = "branch_start",
    details = tibble::tibble(
      embedding_store_location = embedding_store_location
    )
  )

  tryCatch(
    {
      warmup_result <- run_warmup_phase(
        dataset_state = dataset_data,
        base_criteria = base_criteria,
        scenario_context = scenario_context,
        dataset_name = dataset_name,
        embedding_store_location = embedding_store_location,
        config = config
      )
      log_branch_event(
        config = config,
        dataset_name = dataset_name,
        event = "warmup_complete",
        details = tibble::tibble(
          warmup_stop_reason = warmup_result$stop_reason,
          warmup_rounds = nrow(warmup_result$warmup_log)
        )
      )

      assisted_result <- run_assisted_screening(
        warmup_result = warmup_result,
        dataset_name = dataset_name,
        embedding_store_location = embedding_store_location,
        config = config
      )

      warmup_log <- warmup_result$warmup_log |>
        dplyr::mutate(
          dataset_name = dataset_name,
          experiment_name = experiment_name,
          labeller_model = config$labeller_model,
          human_after_warmup = config$human_after_warmup,
          .before = 1
        )

      iteration_log <- assisted_result$iteration_log |>
        dplyr::mutate(
          dataset_name = dataset_name,
          experiment_name = experiment_name,
          labeller_model = config$labeller_model,
          human_after_warmup = config$human_after_warmup,
          .before = 1
        )

      final_metrics <- assisted_result$final_metrics |>
        dplyr::mutate(
          experiment_name = experiment_name,
          labeller_model = config$labeller_model,
          human_after_warmup = config$human_after_warmup,
          warmup_stop_reason = warmup_result$stop_reason,
          main_stop_reason = assisted_result$stop_reason,
          warmup_rounds = nrow(warmup_result$warmup_log),
          warmup_reviewed = sum(
            warmup_result$review_log$review_phase %in% c(
              "warmup_original",
              "warmup_revised",
              "warmup_revised_refined"
            )
          ),
          .before = 1
        )

      phase_metrics <- assisted_result$phase_metrics |>
        dplyr::mutate(
          experiment_name = experiment_name,
          labeller_model = config$labeller_model,
          human_after_warmup = config$human_after_warmup,
          .before = 1
        )

      adjudication_log <- assisted_result$adjudication_log |>
        dplyr::mutate(
          experiment_name = experiment_name,
          labeller_model = config$labeller_model,
          human_after_warmup = config$human_after_warmup,
          .before = 1
        )

      protocol_event <- assisted_result$protocol_event |>
        dplyr::mutate(
          experiment_name = experiment_name,
          labeller_model = config$labeller_model,
          human_after_warmup = config$human_after_warmup,
          .before = 1
        )

      append_run_log(
        config = config,
        file_name = "branch_summaries.tsv",
        data = final_metrics
      )
      log_branch_event(
        config = config,
        dataset_name = dataset_name,
        event = "branch_complete",
        details = tibble::tibble(
          warmup_stop_reason = warmup_result$stop_reason,
          main_stop_reason = assisted_result$stop_reason,
          reviewed_total = final_metrics$reviewed_total[
            final_metrics$metric_scope %in% "operative_final"
          ][[1]],
          recall = final_metrics$recall[
            final_metrics$metric_scope %in% "operative_final"
          ][[1]],
          specificity = final_metrics$specificity[
            final_metrics$metric_scope %in% "operative_final"
          ][[1]],
          precision = final_metrics$precision[
            final_metrics$metric_scope %in% "operative_final"
          ][[1]]
        )
      )

      list(
        final_metrics = final_metrics,
        phase_metrics = phase_metrics,
        warmup_log = warmup_log,
        iteration_log = iteration_log,
        review_log = assisted_result$review_log,
        adjudication_log = adjudication_log,
        protocol_event = protocol_event,
        final_state = assisted_result$state,
        final_criteria = warmup_result$final_criteria,
        final_criteria_version = warmup_result$criteria_version,
        config = config
      )
    },
    error = function(cnd) {
      log_branch_event(
        config = config,
        dataset_name = dataset_name,
        event = "branch_error",
        details = tibble::tibble(error_message = conditionMessage(cnd))
      )
      stop(cnd)
    }
  )
}

# Define the processing branch of the targets graph.
pipeline_process <- list(
  tar_target(
    param_sets,
    selected_experiments
  ),
  mapped_targets = tar_map(
    values = data.frame(dataset_name = datasets),
    names = "dataset_name",
    tar_target(
      scenario_context,
      build_scenario_context(dataset_name),
      deployment = "main"
    ),
    tar_target(base_criteria, selection_criteria[[dataset_name]]),
    tar_target(
      dataset_data,
      prepare_dataset_data(
        data = get(dataset_name),
        dataset_name = dataset_name,
        scenario_context = scenario_context
      ),
      deployment = "main"
    ),
    tar_target(
      embedding_store_location,
      file.path("experiments", "embeddings_assisted", dataset_name)
    ),
    tar_target(
      experiment_result,
      {
        # Resolve branch metadata from bound upstream target values.
        current_dataset_name <- dataset_data[["dataset_name"]][[1]]
        current_experiment_name <- param_sets$experiment_name[[1]]

        cli::cli_alert_info(
          "Running {.field {current_dataset_name}} / {.field {current_experiment_name}}"
        )

        run_assisted_experiment(
          dataset_data = dataset_data,
          base_criteria = base_criteria,
          scenario_context = scenario_context,
          dataset_name = current_dataset_name,
          experiment_name = current_experiment_name,
          params = param_sets,
          embedding_store_location = embedding_store_location
        )
      },
      pattern = map(param_sets),
      iteration = "list"
    )
  )
)

# Define downstream analysis and aggregate reporting targets.
pipeline_analysis <- list(
  tar_combine(
    all_experiment_results,
    pipeline_process$mapped_targets$experiment_result,
    command = list(!!!.x)
  ),
  tar_target(
    metrics_all,
    collect_experiment_component(
      all_experiment_results = all_experiment_results,
      component = "final_metrics"
    )
  ),
  tar_target(
    phase_metrics_all,
    collect_experiment_component(
      all_experiment_results = all_experiment_results,
      component = "phase_metrics"
    )
  ),
  tar_target(
    warmup_logs_all,
    collect_experiment_component(
      all_experiment_results = all_experiment_results,
      component = "warmup_log"
    )
  ),
  tar_target(
    iteration_logs_all,
    collect_experiment_component(
      all_experiment_results = all_experiment_results,
      component = "iteration_log"
    )
  ),
  tar_target(
    adjudication_logs_all,
    collect_experiment_component(
      all_experiment_results = all_experiment_results,
      component = "adjudication_log"
    )
  ),
  tar_target(
    protocol_events_all,
    collect_experiment_component(
      all_experiment_results = all_experiment_results,
      component = "protocol_event"
    )
  ),
  tar_target(
    pareto_summary,
    metrics_all |>
      dplyr::filter(.data$metric_scope %in% "operative_final") |>
      dplyr::select(
        "dataset_name",
        "scenario_type",
        "experiment_name",
        "labeller_model",
        "human_after_warmup",
        "recall",
        "human_reviewed",
        "ai_reviewed",
        "reviewed_total",
        "protocol_amendment_applied",
        "adjudications_applied"
      )
  ),
  tar_target(
    pareto_plot,
    ggplot2::ggplot(
      pareto_summary,
      ggplot2::aes(
        x = .data$human_reviewed,
        y = .data$recall,
        color = .data$labeller_model,
        shape = .data$human_after_warmup
      )
    ) +
      ggplot2::geom_point(size = 3) +
      ggplot2::facet_wrap(~dataset_name) +
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
      ggplot2::labs(
        title = "Recall vs Human Workload",
        subtitle = "Operative truth after adjudication or amendment",
        x = "Human-reviewed records",
        y = "Recall",
        color = "Labeller model",
        shape = "Human after warmup"
      ) +
      ggplot2::theme_minimal(base_size = 12)
  )
)

# Export the full targets graph.
c(pipeline_process, pipeline_analysis)
