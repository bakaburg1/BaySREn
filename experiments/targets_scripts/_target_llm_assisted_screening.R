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
requireNamespace("crew")
requireNamespace("dbarts")
requireNamespace("ellmer")
requireNamespace("fs")
requireNamespace("jsonlite")
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

# Load only datasets used by this assisted workflow version.
data(
  list = c("gastaldi", "vella", "gastaldi_criteria", "vella_criteria"),
  package = "BaySREn",
  envir = environment()
)

# Keep the dataset scope intentionally narrow for this iteration.
datasets <- c("gastaldi", "vella")

# Map datasets to their criteria objects.
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

# Build default assisted-screening configuration.
default_assisted_config <- function() {
  list(
    seed_count = 3L,
    seed_model = "openai/gpt-5.1",
    seed_api_args = list(
      reasoning = list(effort = "minimal"),
      temperature = 0
    ),
    label_models = c("openai/gpt-5.1"),
    tie_break_model = "openai/gpt-5.1",
    label_api_args = list(reasoning = list(effort = "minimal")),
    label_strategy = "single",
    human_after_warmup = TRUE,
    warmup_refiner = TRUE,
    refiner_model = "openai/gpt-5.1",
    refiner_api_args = list(reasoning = list(effort = "high")),
    warmup_batch_size = 25L,
    warmup_k_stable = 2L,
    warmup_k_errorfree = 2L,
    warmup_min_reviewed = 50L,
    warmup_min_positives = 5L,
    warmup_max_reviewed = 300L,
    warmup_max_rounds = 20L,
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
    cache_root = here::here(
      "experiments",
      "targets_scripts",
      "_target_llm_assisted_screening"
    )
  )
}

# Keep the experiment grid focused on new workflow knobs.
selected_experiments <- list(
  E_single_human = list(
    label_strategy = "single",
    human_after_warmup = TRUE
  ),
  E_single_ai = list(
    label_strategy = "single",
    human_after_warmup = FALSE
  ),
  E_vote_human = list(
    label_strategy = "vote",
    human_after_warmup = TRUE,
    label_models = c("openai/gpt-5.1", "openai/gpt-5.1")
  ),
  E_vote_ai = list(
    label_strategy = "vote",
    human_after_warmup = FALSE,
    label_models = c("openai/gpt-5.1", "openai/gpt-5.1")
  ),
  E_tie_break_human = list(
    label_strategy = "tie_break",
    human_after_warmup = TRUE,
    label_models = c("openai/gpt-5.1", "openai/gpt-5.1"),
    tie_break_model = "openai/gpt-5.1"
  ),
  E_tie_break_ai = list(
    label_strategy = "tie_break",
    human_after_warmup = FALSE,
    label_models = c("openai/gpt-5.1", "openai/gpt-5.1"),
    tie_break_model = "openai/gpt-5.1"
  )
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
  purrr::partial(
    ragnar::embed_openai,
    base_url = config$embedding_base_url,
    api_key = Sys.getenv("COHERE_API_KEY"),
    model = config$embedding_model,
    batch_size = config$embedding_batch_size,
    user = NULL
  )
}

# Ensure all required screening columns exist with stable types.
prepare_dataset_data <- function(data, dataset_name) {
  required <- c("title", "abstract", "authors", "keywords", "included")
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    cli::cli_abort(
      "Dataset {.field {dataset_name}} is missing {.field {missing}}."
    )
  }

  data |>
    tibble::as_tibble() |>
    dplyr::mutate(
      id = dplyr::row_number(),
      dataset_name = dataset_name,
      title = as.character(.data$title),
      abstract = as.character(.data$abstract),
      authors = as.character(.data$authors),
      keywords = as.character(.data$keywords),
      included = as.logical(.data$included)
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
    as.character()

  chats <- parallel_chat_promises(
    chat = chat,
    prompts = prompts,
    max_active = min(3L, n_seed),
    rpm = 60,
    cache_dir = cache_dir,
    backoff_base = 5,
    backoff_cap = 120,
    halve_rpm_on_retry = TRUE
  )

  seed_texts <- purrr::map_chr(chats, \(conversation) {
    txt <- tryCatch(
      conversation$last_turn()@text,
      error = function(...) ""
    )
    trimws(txt)
  })

  if (any(!nzchar(seed_texts))) {
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
label_with_single_model <- function(
  data,
  criteria,
  model,
  api_args,
  cache_dir
) {
  chat <- ellmer::chat_openrouter(
    model = model,
    api_args = api_args,
    echo = "none"
  )

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

# Label records using single, vote, or tie-break strategy.
llm_label_records <- function(
  data,
  criteria,
  strategy,
  models,
  tie_break_model,
  api_args,
  cache_root,
  dataset_name
) {
  if (rlang::is_empty(data) || !nrow(data)) {
    return(
      tibble::tibble(
        id = integer(),
        ai_label = logical(),
        label_strategy = character()
      )
    )
  }

  strategy <- match.arg(strategy, c("single", "vote", "tie_break"))
  criteria_hash <- make_criteria_hash(criteria)
  model_ids <- models

  if (identical(strategy, "single")) {
    model_ids <- model_ids[[1]]
  }

  primary_votes <- purrr::map(model_ids, \(model_name) {
    model_cache <- fs::path(
      cache_root,
      "label_cache",
      dataset_name,
      criteria_hash,
      gsub("/", "_", model_name)
    )
    fs::dir_create(model_cache)

    label_with_single_model(
      data = data,
      criteria = criteria,
      model = model_name,
      api_args = api_args,
      cache_dir = model_cache
    )
  })

  primary_matrix <- do.call(
    what = cbind,
    args = purrr::map(primary_votes, "match")
  )

  tie_break_labels <- NULL
  if (identical(strategy, "tie_break")) {
    tie_cache <- fs::path(
      cache_root,
      "label_cache",
      dataset_name,
      criteria_hash,
      gsub("/", "_", tie_break_model),
      "tie_break"
    )
    fs::dir_create(tie_cache)

    tie_break_tbl <- label_with_single_model(
      data = data,
      criteria = criteria,
      model = tie_break_model,
      api_args = api_args,
      cache_dir = tie_cache
    )
    tie_break_labels <- tie_break_tbl$match
  }

  final_labels <- combine_llm_labels(
    primary_labels = primary_matrix,
    strategy = strategy,
    tie_break_labels = tie_break_labels
  )

  tibble::tibble(
    id = data$id,
    ai_label = as.logical(final_labels),
    label_strategy = strategy
  )
}

# Build the reviewed-only refinement prompt for warmup rounds.
build_warmup_refiner_prompt <- function(
  criteria,
  reviewed_data,
  confusion,
  n_total,
  round_index
) {
  reviewed_n <- nrow(reviewed_data)
  reviewed_pos <- sum(reviewed_data$human_label, na.rm = TRUE)

  sampled <- reviewed_data |>
    dplyr::arrange(dplyr::desc(.data$human_label), .data$id) |>
    dplyr::slice_head(n = 12L) |>
    dplyr::mutate(
      human_chr = ifelse(.data$human_label, "included", "excluded"),
      ai_chr = ifelse(.data$ai_label, "included", "excluded"),
      row_txt = paste0(
        "ID ",
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

  paste(
    "You refine systematic-review criteria during a warmup phase.",
    "Metrics below are computed on reviewed records only.",
    "Do not assume access to unreviewed labels.",
    "Return JSON only with keys include, exclude, reasoning.",
    paste0("Warmup round: ", round_index),
    paste0("Reviewed records: ", reviewed_n, " of ", n_total),
    paste0("Reviewed positives: ", reviewed_pos),
    paste0("Reviewed FN: ", confusion$FN[[1]]),
    paste0("Reviewed FP: ", confusion$FP[[1]]),
    "<current_include>",
    scalar_default(criteria$include, ""),
    "</current_include>",
    "<current_exclude>",
    scalar_default(criteria$exclude, ""),
    "</current_exclude>",
    "<reviewed_samples>",
    sampled,
    "</reviewed_samples>",
    "JSON schema:",
    "{\"include\":\"unchanged or revised include\",",
    "\"exclude\":\"unchanged or revised exclude\",",
    "\"reasoning\":\"short rationale\"}",
    sep = "\n\n"
  )
}

# Refine criteria based only on reviewed warmup records.
refine_warmup_criteria <- function(
  criteria,
  reviewed_data,
  confusion,
  n_total,
  round_index,
  model,
  api_args,
  cache_root,
  dataset_name
) {
  if (rlang::is_empty(reviewed_data) || !nrow(reviewed_data)) {
    return(
      list(
        criteria = criteria,
        changed = FALSE,
        reasoning = "No reviewed records available."
      )
    )
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
      "You revise criteria to maximize recall with minimal false positives.",
      "Use only reviewed records and reviewed metrics.",
      sep = "\n"
    )
  )

  prompt <- build_warmup_refiner_prompt(
    criteria = criteria,
    reviewed_data = reviewed_data,
    confusion = confusion,
    n_total = n_total,
    round_index = round_index
  )

  response <- chat_prompt_with_retry(
    chat = chat,
    prompt = prompt,
    cache_dir = cache_dir
  )

  parsed <- tryCatch(
    extract_json_object(response),
    error = function(...) NULL
  )

  if (is.null(parsed)) {
    return(
      list(
        criteria = criteria,
        changed = FALSE,
        reasoning = "Refiner response parse failed; kept criteria unchanged."
      )
    )
  }

  include_txt <- scalar_default(parsed$include, "unchanged")
  exclude_txt <- scalar_default(parsed$exclude, "unchanged")

  include_next <- if (
    is.character(include_txt) &&
      identical(tolower(trimws(include_txt)), "unchanged")
  ) {
    criteria$include
  } else {
    include_txt
  }

  exclude_next <- if (
    is.character(exclude_txt) &&
      identical(tolower(trimws(exclude_txt)), "unchanged")
  ) {
    criteria$exclude
  } else {
    exclude_txt
  }

  next_criteria <- list(include = include_next, exclude = exclude_next)

  list(
    criteria = next_criteria,
    changed = criteria_changed(criteria, next_criteria),
    reasoning = scalar_default(parsed$reasoning, "Refiner provided no rationale.")
  )
}

# Run warmup with criteria refinement and A+B stop logic.
warmup_refine_criteria_AplusB <- function(
  dataset,
  base_criteria,
  dataset_name,
  embedding_store_location,
  config
) {
  reviewed_state <- tibble::tibble(
    id = integer(),
    human_label = logical(),
    ai_label = logical(),
    final_label = character(),
    review_source = character(),
    review_round = integer()
  )

  warmup_log <- tibble::tibble(
    round = integer(),
    reviewed_n = integer(),
    reviewed_positive_n = integer(),
    FN = integer(),
    FP = integer(),
    criteria_changed = logical(),
    cumulative_changes = integer(),
    stop_now = logical(),
    cap_hit = logical(),
    criteria_include = character(),
    criteria_exclude = character(),
    refiner_reasoning = character()
  )

  embedder <- build_embedder(config)
  current_criteria <- base_criteria
  cumulative_changes <- 0L
  stop_reason <- "warmup_max_rounds"

  for (round_idx in seq_len(config$warmup_max_rounds)) {
    cli::cli_alert_info(
      "Warmup {.field {dataset_name}} round {.val {round_idx}}"
    )

    seed_texts <- generate_seed_abstracts(
      criteria = current_criteria,
      dataset_name = dataset_name,
      cache_root = config$cache_root,
      n_seed = config$seed_count,
      model = config$seed_model,
      api_args = config$seed_api_args
    )

    ranked <- rank_with_seed_abstracts(
      data = dataset,
      seed_texts = seed_texts,
      store_location = embedding_store_location,
      embedder = embedder,
      return_embeddings = FALSE
    )

    candidates <- ranked$data |>
      dplyr::filter(!.data$id %in% reviewed_state$id) |>
      dplyr::arrange(.data$sorting_id) |>
      dplyr::slice_head(n = config$warmup_batch_size)

    if (!nrow(candidates)) {
      stop_reason <- "warmup_no_candidates"
      break
    }

    ai_labels <- llm_label_records(
      data = candidates,
      criteria = current_criteria,
      strategy = config$label_strategy,
      models = config$label_models,
      tie_break_model = config$tie_break_model,
      api_args = config$label_api_args,
      cache_root = config$cache_root,
      dataset_name = dataset_name
    )

    reviewed_batch <- candidates |>
      dplyr::select("id", "title", "abstract", "included") |>
      dplyr::left_join(ai_labels, by = "id") |>
      dplyr::mutate(
        human_label = .data$included,
        ai_label = .data$ai_label,
        final_label = ifelse(.data$human_label, "y", "n"),
        review_source = "human_warmup",
        review_round = round_idx
      ) |>
      dplyr::select(
        "id",
        "title",
        "abstract",
        "human_label",
        "ai_label",
        "final_label",
        "review_source",
        "review_round"
      )

    reviewed_state <- dplyr::bind_rows(
      reviewed_state,
      reviewed_batch |>
        dplyr::select(
          "id",
          "human_label",
          "ai_label",
          "final_label",
          "review_source",
          "review_round"
        )
    )

    confusion <- compute_review_confusion(
      human_label = reviewed_state$human_label,
      ai_label = reviewed_state$ai_label
    )

    refiner_result <- list(
      criteria = current_criteria,
      changed = FALSE,
      reasoning = "Warmup refiner disabled."
    )

    if (isTRUE(config$warmup_refiner)) {
      reviewed_for_refiner <- reviewed_state |>
        dplyr::left_join(
          dataset |>
            dplyr::select("id", "title", "abstract"),
          by = "id"
        )

      refiner_result <- refine_warmup_criteria(
        criteria = current_criteria,
        reviewed_data = reviewed_for_refiner,
        confusion = confusion,
        n_total = nrow(dataset),
        round_index = round_idx,
        model = config$refiner_model,
        api_args = config$refiner_api_args,
        cache_root = config$cache_root,
        dataset_name = dataset_name
      )
    }

    if (isTRUE(refiner_result$changed)) {
      cumulative_changes <- cumulative_changes + 1L
    }

    reviewed_n <- nrow(reviewed_state)
    reviewed_pos_n <- sum(reviewed_state$human_label, na.rm = TRUE)

    warmup_log <- dplyr::bind_rows(
      warmup_log,
      tibble::tibble(
        round = round_idx,
        reviewed_n = reviewed_n,
        reviewed_positive_n = reviewed_pos_n,
        FN = confusion$FN,
        FP = confusion$FP,
        criteria_changed = isTRUE(refiner_result$changed),
        cumulative_changes = cumulative_changes,
        stop_now = FALSE,
        cap_hit = FALSE,
        criteria_include = scalar_default(refiner_result$criteria$include, ""),
        criteria_exclude = scalar_default(refiner_result$criteria$exclude, ""),
        refiner_reasoning = scalar_default(refiner_result$reasoning, "")
      )
    )

    stop_state <- should_stop_warmup(
      warmup_log = warmup_log,
      k_stable = config$warmup_k_stable,
      k_errorfree = config$warmup_k_errorfree,
      min_reviewed = config$warmup_min_reviewed,
      min_positives = config$warmup_min_positives,
      max_reviewed = config$warmup_max_reviewed,
      max_rounds = config$warmup_max_rounds
    )

    warmup_log$stop_now[[nrow(warmup_log)]] <- stop_state$stop
    warmup_log$cap_hit[[nrow(warmup_log)]] <- stop_state$cap_hit

    current_criteria <- refiner_result$criteria

    if (isTRUE(stop_state$stop)) {
      stop_reason <- "warmup_AplusB"
      break
    }

    if (isTRUE(stop_state$cap_hit)) {
      stop_reason <- "warmup_caps"
      break
    }
  }

  final_criteria <- current_criteria

  if (identical(stop_reason, "warmup_caps") && nrow(warmup_log)) {
    best_round <- select_best_warmup_round(warmup_log)

    final_criteria <- list(
      include = best_round$criteria_include[[1]],
      exclude = best_round$criteria_exclude[[1]]
    )
  }

  list(
    final_criteria = final_criteria,
    warmup_log = warmup_log,
    review_state = reviewed_state,
    stop_reason = stop_reason
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

# Apply one full assisted-screening loop after warmup criteria are frozen.
run_assisted_screening <- function(
  dataset,
  criteria,
  warmup_state,
  dataset_name,
  embedding_store_location,
  config
) {
  embedder <- build_embedder(config)

  seed_texts <- generate_seed_abstracts(
    criteria = criteria,
    dataset_name = dataset_name,
    cache_root = config$cache_root,
    n_seed = config$seed_count,
    model = config$seed_model,
    api_args = config$seed_api_args
  )

  ranking <- rank_with_seed_abstracts(
    data = dataset,
    seed_texts = seed_texts,
    store_location = embedding_store_location,
    embedder = embedder,
    return_embeddings = TRUE
  )

  feature_tbl <- embedding_features_from_rank(ranking$embeddings)

  state <- dataset |>
    dplyr::left_join(
      ranking$data |>
        dplyr::select("id", "sorting_id", "embedding_score"),
      by = "id"
    ) |>
    dplyr::left_join(feature_tbl, by = "id") |>
    dplyr::mutate(
      final_label = NA_character_,
      human_label = NA,
      ai_label = NA,
      review_source = NA_character_,
      review_round = NA_integer_,
      Pred_Med = NA_real_,
      Pred_Low = NA_real_,
      Pred_Up = NA_real_,
      Predicted_label = NA_character_
    )

  if (!rlang::is_empty(warmup_state) && nrow(warmup_state)) {
    warmup_idx <- match(warmup_state$id, state$id)

    state$human_label[warmup_idx] <- warmup_state$human_label
    state$ai_label[warmup_idx] <- warmup_state$ai_label
    state$final_label[warmup_idx] <- warmup_state$final_label
    state$review_source[warmup_idx] <- warmup_state$review_source
    state$review_round[warmup_idx] <- warmup_state$review_round
  }

  iteration_log <- tibble::tibble(
    iteration = integer(),
    reviewed_total = integer(),
    reviewed_human = integer(),
    reviewed_ai = integer(),
    new_positives = integer(),
    candidates_remaining = integer(),
    mode = character()
  )

  feature_cols <- names(feature_tbl) |> setdiff("id")

  for (iter_idx in seq_len(config$main_max_iterations)) {
    reviewed <- state |>
      dplyr::filter(!is.na(.data$final_label))

    can_model <- nrow(reviewed) >= 10 &&
      any(reviewed$final_label %in% "y") &&
      any(reviewed$final_label %in% "n")

    candidates <- NULL
    mode <- "embedding_fallback"

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
      break
    }

    candidates <- candidates |>
      dplyr::slice_head(n = config$main_batch_size)

    ai_tbl <- llm_label_records(
      data = candidates,
      criteria = criteria,
      strategy = config$label_strategy,
      models = config$label_models,
      tie_break_model = config$tie_break_model,
      api_args = config$label_api_args,
      cache_root = config$cache_root,
      dataset_name = dataset_name
    )

    update_tbl <- candidates |>
      dplyr::select("id", "included") |>
      dplyr::left_join(ai_tbl, by = "id") |>
      dplyr::mutate(
        human_label = .data$included,
        final_bool = dplyr::if_else(
          isTRUE(config$human_after_warmup),
          .data$human_label,
          .data$ai_label
        ),
        final_label = ifelse(.data$final_bool, "y", "n"),
        review_source = ifelse(
          isTRUE(config$human_after_warmup),
          "human_after_warmup",
          "ai_after_warmup"
        ),
        review_round = iter_idx
      )

    update_idx <- match(update_tbl$id, state$id)

    state$human_label[update_idx] <- update_tbl$human_label
    state$ai_label[update_idx] <- update_tbl$ai_label
    state$final_label[update_idx] <- update_tbl$final_label
    state$review_source[update_idx] <- update_tbl$review_source
    state$review_round[update_idx] <- update_tbl$review_round

    new_positives <- sum(update_tbl$final_label %in% "y", na.rm = TRUE)

    if (isTRUE(config$rerank_on_new_positives) && new_positives > 0) {
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
        data = dataset,
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
        reviewed_human = sum(state$review_source %in% c(
          "human_warmup",
          "human_after_warmup"
        )),
        reviewed_ai = sum(state$review_source %in% "ai_after_warmup"),
        new_positives = new_positives,
        candidates_remaining = remaining_candidates,
        mode = mode
      )
    )

    if (remaining_candidates == 0) {
      break
    }
  }

  included_pred <- state$final_label %in% "y"

  final_metrics <- tibble::tibble(
    dataset_name = dataset_name,
    reviewed_total = sum(!is.na(state$final_label)),
    human_reviewed = sum(state$review_source %in% c(
      "human_warmup",
      "human_after_warmup"
    )),
    ai_reviewed = sum(state$review_source %in% "ai_after_warmup"),
    TP = sum(state$included & included_pred, na.rm = TRUE),
    FP = sum(!state$included & included_pred, na.rm = TRUE),
    TN = sum(!state$included & !included_pred, na.rm = TRUE),
    FN = sum(state$included & !included_pred, na.rm = TRUE),
    recall = TP / (TP + FN),
    precision = TP / (TP + FP),
    missed_includes = FN
  )

  list(
    state = state,
    iteration_log = iteration_log,
    final_metrics = final_metrics,
    seed_texts = seed_texts
  )
}

# Run one full experiment branch for one dataset and one parameter set.
run_assisted_experiment <- function(
  dataset_data,
  base_criteria,
  dataset_name,
  experiment_name,
  params,
  embedding_store_location
) {
  config <- utils::modifyList(default_assisted_config(), params)

  warmup_result <- warmup_refine_criteria_AplusB(
    dataset = dataset_data,
    base_criteria = base_criteria,
    dataset_name = dataset_name,
    embedding_store_location = embedding_store_location,
    config = config
  )

  assisted_result <- run_assisted_screening(
    dataset = dataset_data,
    criteria = warmup_result$final_criteria,
    warmup_state = warmup_result$review_state,
    dataset_name = dataset_name,
    embedding_store_location = embedding_store_location,
    config = config
  )

  warmup_log <- warmup_result$warmup_log |>
    dplyr::mutate(
      dataset_name = dataset_name,
      experiment_name = experiment_name,
      .before = 1
    )

  iteration_log <- assisted_result$iteration_log |>
    dplyr::mutate(
      dataset_name = dataset_name,
      experiment_name = experiment_name,
      .before = 1
    )

  final_metrics <- assisted_result$final_metrics |>
    dplyr::mutate(
      experiment_name = experiment_name,
      label_strategy = config$label_strategy,
      human_after_warmup = config$human_after_warmup,
      warmup_stop_reason = warmup_result$stop_reason,
      warmup_rounds = nrow(warmup_result$warmup_log),
      warmup_reviewed = nrow(warmup_result$review_state),
      .before = 1
    )

  list(
    final_metrics = final_metrics,
    warmup_log = warmup_log,
    iteration_log = iteration_log,
    final_state = assisted_result$state,
    final_criteria = warmup_result$final_criteria,
    config = config
  )
}

# Define the processing branch of the targets graph.
pipeline_process <- list(
  tar_target(
    param_sets,
    tibble::enframe(
      selected_experiments,
      name = "experiment_name",
      value = "params"
    )
  ),
  mapped_targets = tar_map(
    values = data.frame(dataset_name = datasets),
    names = "dataset_name",
    tar_target(
      dataset_data,
      prepare_dataset_data(get(dataset_name), dataset_name),
      deployment = "main"
    ),
    tar_target(base_criteria, selection_criteria[[dataset_name]]),
    tar_target(
      embedding_store_location,
      file.path("experiments", "embeddings_assisted", dataset_name)
    ),
    tar_target(
      experiment_result,
      {
        cli::cli_alert_info(
          "Running {.field {dataset_name}} / {.field {param_sets$experiment_name}}"
        )

        run_assisted_experiment(
          dataset_data = dataset_data,
          base_criteria = base_criteria,
          dataset_name = dataset_name,
          experiment_name = param_sets$experiment_name,
          params = param_sets$params[[1]],
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
    purrr::map_dfr(all_experiment_results, "final_metrics")
  ),
  tar_target(
    warmup_logs_all,
    purrr::map_dfr(all_experiment_results, "warmup_log")
  ),
  tar_target(
    iteration_logs_all,
    purrr::map_dfr(all_experiment_results, "iteration_log")
  ),
  tar_target(
    pareto_summary,
    metrics_all |>
      dplyr::select(
        "dataset_name",
        "experiment_name",
        "label_strategy",
        "human_after_warmup",
        "recall",
        "human_reviewed",
        "ai_reviewed",
        "reviewed_total"
      )
  ),
  tar_target(
    pareto_plot,
    ggplot2::ggplot(
      pareto_summary,
      ggplot2::aes(
        x = .data$human_reviewed,
        y = .data$recall,
        color = .data$label_strategy,
        shape = .data$human_after_warmup
      )
    ) +
      ggplot2::geom_point(size = 3) +
      ggplot2::facet_wrap(~dataset_name) +
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
      ggplot2::labs(
        title = "Recall vs Human Workload",
        x = "Human-reviewed records",
        y = "Recall",
        color = "Label strategy",
        shape = "Human after warmup"
      ) +
      ggplot2::theme_minimal(base_size = 12)
  )
)

c(pipeline_process, pipeline_analysis)
