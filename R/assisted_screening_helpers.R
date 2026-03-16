# Normalize criteria text for stable comparisons and cache keys.
normalize_criteria_text <- function(text) {
  if (is.null(text) || !length(text)) {
    return("")
  }

  text |>
    as.character() |>
    trimws() |>
    gsub("[[:space:]]+", " ", x = _, perl = TRUE) |>
    tolower()
}

# Replace NULL with a fallback scalar.
null_default <- function(x, default) {
  if (is.null(x) || !length(x)) {
    return(default)
  }
  x
}

# Normalize a criteria list with include/exclude fields.
normalize_criteria <- function(criteria) {
  list(
    include = normalize_criteria_text(null_default(criteria$include, "")),
    exclude = normalize_criteria_text(null_default(criteria$exclude, ""))
  )
}

# Build a deterministic criteria hash used by cache paths.
make_criteria_hash <- function(criteria) {
  normalized <- normalize_criteria(criteria)
  rlang::hash(
    paste0(
      "include::",
      normalized$include,
      "::exclude::",
      normalized$exclude
    )
  )
}

# Build the assisted experiment grid from model settings and review modes.
build_assisted_experiment_grid <- function(
  model_registry,
  human_after_warmup_values = c(TRUE, FALSE)
) {
  required <- c("labeller_model", "label_api_args")
  missing <- setdiff(required, names(model_registry))
  if (length(missing)) {
    cli::cli_abort(
      "model_registry is missing columns {.field {missing}}."
    )
  }

  if (!is.list(model_registry$label_api_args)) {
    cli::cli_abort("model_registry$label_api_args must be a list-column.")
  }

  if (anyDuplicated(model_registry$labeller_model)) {
    cli::cli_abort("labeller_model values must be unique.")
  }

  human_modes <- tibble::tibble(
    human_after_warmup = as.logical(human_after_warmup_values)
  )

  model_registry |>
    tibble::as_tibble() |>
    dplyr::mutate(
      model_slug = gsub("[^A-Za-z0-9]+", "_", .data$labeller_model),
      model_slug = gsub("^_|_$", "", .data$model_slug)
    ) |>
    tidyr::crossing(human_modes) |>
    dplyr::mutate(
      experiment_name = ifelse(
        .data$human_after_warmup,
        paste0(.data$model_slug, "_human"),
        paste0(.data$model_slug, "_ai")
      ),
      .before = 1
    ) |>
    dplyr::select(
      "experiment_name",
      "labeller_model",
      "label_api_args",
      "human_after_warmup"
    )
}

# Detect whether criteria changed between two snapshots.
criteria_changed <- function(previous, current) {
  prev_norm <- normalize_criteria(previous)
  curr_norm <- normalize_criteria(current)

  !identical(prev_norm, curr_norm) &&
    !(identical(curr_norm$include, "unchanged") &&
      identical(curr_norm$exclude, "unchanged"))
}

# Build a stable record key from dataset and citation fields.
make_record_key <- function(
  dataset_name,
  title,
  abstract,
  authors,
  keywords
) {
  purrr::pmap_chr(
    list(dataset_name, title, abstract, authors, keywords),
    \(dataset_name, title, abstract, authors, keywords) {
      rlang::hash(
        list(
          dataset_name,
          title,
          abstract,
          authors,
          keywords
        )
      )
    }
  )
}

# Parse review labels from workbook text into logical inclusion values.
label_text_to_logical <- function(label) {
  label_chr <- label |>
    as.character() |>
    stringr::str_trim() |>
    stringr::str_to_lower()

  dplyr::case_when(
    label_chr %in% "relevant" ~ TRUE,
    label_chr %in% "not relevant" ~ FALSE,
    TRUE ~ NA
  )
}

# Build a Vella revision lookup keyed by title and abstract.
build_vella_revision_lookup <- function(corrections_tbl) {
  required <- c("title", "abstract", "human_label", "human_relabel")
  missing <- setdiff(required, names(corrections_tbl))
  if (length(missing)) {
    cli::cli_abort(
      "corrections_tbl is missing columns {.field {missing}}."
    )
  }

  corrections_tbl |>
    tibble::as_tibble() |>
    dplyr::mutate(
      title = as.character(.data$title),
      abstract = as.character(.data$abstract),
      label_original_revision = label_text_to_logical(.data$human_label),
      label_revised_candidate = dplyr::coalesce(
        label_text_to_logical(.data$human_relabel),
        label_text_to_logical(.data$human_label)
      ),
      revision_entry_available = !is.na(.data$label_revised_candidate)
    ) |>
    dplyr::select(
      dplyr::any_of("display_id"),
      "title",
      "abstract",
      "label_original_revision",
      "label_revised_candidate",
      "revision_entry_available"
    ) |>
    dplyr::distinct(.data$title, .data$abstract, .keep_all = TRUE)
}

# Initialize record-level truth state for assisted screening.
initialize_assisted_dataset_state <- function(
  data,
  dataset_name,
  scenario_type,
  revision_lookup = NULL
) {
  required <- c("title", "abstract", "authors", "keywords", "included")
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    cli::cli_abort("data is missing columns {.field {missing}}.")
  }

  if (!scenario_type %in% c("adjudication", "protocol_amendment")) {
    cli::cli_abort(
      "scenario_type must be one of {.val adjudication} or {.val protocol_amendment}."
    )
  }

  if (!"id" %in% names(data)) {
    data <- data |>
      tibble::as_tibble() |>
      dplyr::mutate(id = dplyr::row_number(), .before = 1)
  }

  state <- data |>
    tibble::as_tibble() |>
    dplyr::mutate(
      id = as.integer(.data$id),
      dataset_name = dataset_name,
      title = as.character(.data$title),
      abstract = as.character(.data$abstract),
      authors = as.character(.data$authors),
      keywords = as.character(.data$keywords),
      included = as.logical(.data$included),
      record_key = make_record_key(
        dataset_name = dataset_name,
        title = .data$title,
        abstract = .data$abstract,
        authors = .data$authors,
        keywords = .data$keywords
      ),
      scenario_type = scenario_type,
      label_original = .data$included,
      label_revised_candidate = NA,
      revision_entry_available = FALSE,
      label_operational = .data$included,
      operative_truth_version = "original",
      adjudication_triggered = FALSE,
      adjudication_applied = FALSE,
      adjudication_round = NA_integer_,
      contradiction_count = 0L,
      criteria_version = "original",
      criteria_version_reviewed = NA_character_,
      human_label = NA,
      ai_label = NA,
      final_label = NA_character_,
      review_source = NA_character_,
      review_round = NA_integer_
    )

  if (
    identical(scenario_type, "adjudication") &&
      !rlang::is_null(revision_lookup) &&
      nrow(revision_lookup)
  ) {
    state <- state |>
      dplyr::left_join(
        revision_lookup |>
          dplyr::select(
            "title",
            "abstract",
            "label_revised_candidate",
            "revision_entry_available"
          ),
        by = c("title", "abstract")
      ) |>
      dplyr::mutate(
        label_revised_candidate = dplyr::coalesce(
          .data$label_revised_candidate.y,
          .data$label_revised_candidate.x
        ),
        revision_entry_available = dplyr::coalesce(
          .data$revision_entry_available.y,
          .data$revision_entry_available.x,
          FALSE
        )
      ) |>
      dplyr::select(
        -dplyr::any_of(c(
          "label_revised_candidate.x",
          "label_revised_candidate.y",
          "revision_entry_available.x",
          "revision_entry_available.y"
        ))
      )
  }

  state
}

# Merge multiple seed ranking tables into a deterministic aggregate ranking.
aggregate_seed_rankings <- function(rankings) {
  if (rlang::is_empty(rankings)) {
    cli::cli_abort("rankings must include at least one ranking table.")
  }

  expected_cols <- c("id", "embedding_score", "sorting_id")

  ranking_table <- purrr::imap(rankings, \(tbl, idx) {
    missing_cols <- setdiff(expected_cols, names(tbl))
    if (length(missing_cols)) {
      cli::cli_abort(
        "Ranking {.val {idx}} is missing columns {.field {missing_cols}}."
      )
    }

    tbl |>
      dplyr::select("id", "embedding_score", "sorting_id") |>
      dplyr::mutate(seed_index = idx)
  }) |>
    dplyr::bind_rows()

  ranking_table |>
    dplyr::summarise(
      embedding_score = mean(.data$embedding_score, na.rm = TRUE),
      mean_rank = mean(.data$sorting_id, na.rm = TRUE),
      seed_count = dplyr::n(),
      .by = "id"
    ) |>
    dplyr::arrange(
      .data$embedding_score,
      .data$mean_rank,
      .data$id
    ) |>
    dplyr::mutate(sorting_id = dplyr::row_number()) |>
    dplyr::select(
      "id",
      "embedding_score",
      "sorting_id",
      "mean_rank",
      "seed_count"
    )
}

# Summarize a vote matrix into deterministic per-record vote metadata.
summarise_vote_matrix <- function(primary_labels) {
  labels <- as.matrix(primary_labels)
  if (!is.logical(labels)) {
    cli::cli_abort("primary_labels must be logical.")
  }
  if (!nrow(labels) || !ncol(labels)) {
    cli::cli_abort("primary_labels must be a non-empty logical matrix.")
  }

  vote_signature <- vapply(
    seq_len(nrow(labels)),
    FUN = \(row_idx) {
      row_vals <- labels[row_idx, ]
      encoded <- dplyr::case_when(
        is.na(row_vals) ~ "U",
        row_vals ~ "Y",
        TRUE ~ "N"
      )
      paste(encoded, collapse = "|")
    },
    FUN.VALUE = character(1)
  )

  votes_available <- rowSums(!is.na(labels))
  votes_positive <- rowSums(labels, na.rm = TRUE)
  votes_negative <- votes_available - votes_positive
  tie_rows <- votes_available > 0 & (votes_positive * 2 == votes_available)

  tibble::tibble(
    votes_available = as.integer(votes_available),
    votes_positive = as.integer(votes_positive),
    votes_negative = as.integer(votes_negative),
    vote_signature = vote_signature,
    tie_break_used = tie_rows
  )
}

# Derive fallback tie-break labels from the ordered primary vote matrix.
derive_tie_break_labels <- function(primary_labels) {
  labels <- as.matrix(primary_labels)
  if (!is.logical(labels)) {
    cli::cli_abort("primary_labels must be logical.")
  }
  if (!nrow(labels) || !ncol(labels)) {
    cli::cli_abort("primary_labels must be a non-empty logical matrix.")
  }

  vapply(
    seq_len(nrow(labels)),
    FUN = \(row_idx) {
      row_vals <- labels[row_idx, ]
      available_idx <- which(!is.na(row_vals))
      if (!length(available_idx)) {
        return(FALSE)
      }
      as.logical(row_vals[[available_idx[[1]]]])
    },
    FUN.VALUE = logical(1)
  )
}

# Combine per-model logical labels according to the selected strategy.
combine_llm_labels <- function(
  primary_labels,
  strategy = c("single", "vote", "tie_break"),
  tie_break_labels = NULL
) {
  strategy <- match.arg(strategy)

  labels <- as.matrix(primary_labels)
  if (!is.logical(labels)) {
    cli::cli_abort("primary_labels must be logical.")
  }
  if (!nrow(labels) || !ncol(labels)) {
    cli::cli_abort("primary_labels must be a non-empty logical matrix.")
  }

  if (identical(strategy, "single")) {
    return(labels[, 1])
  }

  votes_n <- rowSums(!is.na(labels))
  votes_pos <- rowSums(labels, na.rm = TRUE)
  majority <- votes_pos > (votes_n / 2)
  majority <- dplyr::if_else(votes_n == 0, FALSE, majority)

  if (identical(strategy, "vote")) {
    return(majority)
  }

  if (is.null(tie_break_labels)) {
    cli::cli_abort("tie_break_labels must be provided for tie_break mode.")
  }
  if (!is.logical(tie_break_labels) || length(tie_break_labels) != nrow(labels)) {
    cli::cli_abort(
      "tie_break_labels must be a logical vector matching row count."
    )
  }

  ties <- votes_n > 0 & (votes_pos * 2 == votes_n)
  dplyr::if_else(ties, tie_break_labels, majority)
}

# Compute confusion counts for a reviewed batch or cumulative reviewed set.
compute_review_confusion <- function(human_label, ai_label) {
  if (length(human_label) != length(ai_label)) {
    cli::cli_abort("human_label and ai_label must have the same length.")
  }

  if (!is.logical(human_label) || !is.logical(ai_label)) {
    cli::cli_abort("human_label and ai_label must both be logical.")
  }

  tibble::tibble(
    FN = sum(human_label & !ai_label, na.rm = TRUE),
    FP = sum(!human_label & ai_label, na.rm = TRUE),
    TP = sum(human_label & ai_label, na.rm = TRUE),
    TN = sum(!human_label & !ai_label, na.rm = TRUE)
  )
}

# Summarize Se, Sp, and PPV from confusion-count inputs.
summarise_confusion_metrics <- function(
  TP,
  FP,
  TN,
  FN
) {
  safe_ratio <- function(num, denom) {
    ifelse(denom > 0, num / denom, NA_real_)
  }

  se_num <- TP
  se_denom <- TP + FN
  sp_num <- TN
  sp_denom <- TN + FP
  ppv_num <- TP
  ppv_denom <- TP + FP

  tibble::tibble(
    se_num = as.integer(se_num),
    se_denom = as.integer(se_denom),
    se = safe_ratio(se_num, se_denom),
    sp_num = as.integer(sp_num),
    sp_denom = as.integer(sp_denom),
    sp = safe_ratio(sp_num, sp_denom),
    ppv_num = as.integer(ppv_num),
    ppv_denom = as.integer(ppv_denom),
    ppv = safe_ratio(ppv_num, ppv_denom)
  )
}

# Return consecutive trailing TRUE count from a logical vector.
trailing_true_streak <- function(x) {
  if (!length(x)) {
    return(0L)
  }

  run <- rle(rev(x))
  if (!length(run$lengths) || !isTRUE(run$values[[1]])) {
    return(0L)
  }

  as.integer(run$lengths[[1]])
}

# Select a compact stratified reviewed sample for the warmup refiner prompt.
select_warmup_refiner_samples <- function(
  reviewed_data,
  max_total = 12L,
  quotas = c(FN = 4L, FP = 4L, TP = 2L, TN = 2L)
) {
  required <- c("id", "title", "abstract", "human_label", "ai_label")
  missing <- setdiff(required, names(reviewed_data))
  if (length(missing)) {
    cli::cli_abort(
      "reviewed_data is missing columns {.field {missing}}."
    )
  }

  if (rlang::is_empty(reviewed_data) || !nrow(reviewed_data)) {
    return(tibble::tibble())
  }

  bucket_order <- c("FN", "FP", "TP", "TN")
  quotas <- quotas[bucket_order]
  quotas[is.na(quotas)] <- 0L

  sampled <- reviewed_data |>
    tibble::as_tibble() |>
    dplyr::mutate(
      sample_bucket = dplyr::case_when(
        .data$human_label & !.data$ai_label ~ "FN",
        !.data$human_label & .data$ai_label ~ "FP",
        .data$human_label & .data$ai_label ~ "TP",
        TRUE ~ "TN"
      )
    )

  initial <- purrr::map(
    bucket_order,
    \(bucket_name) {
      sampled |>
        dplyr::filter(.data$sample_bucket == bucket_name) |>
        dplyr::arrange(.data$id) |>
        dplyr::slice_head(n = quotas[[bucket_name]])
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      sample_bucket = factor(
        .data$sample_bucket,
        levels = bucket_order,
        ordered = TRUE
      )
    ) |>
    dplyr::arrange(.data$sample_bucket, .data$id)

  if (nrow(initial) >= max_total) {
    return(
      initial |>
        dplyr::slice_head(n = max_total) |>
        dplyr::mutate(sample_bucket = as.character(.data$sample_bucket))
    )
  }

  remaining_n <- max(0L, max_total - nrow(initial))
  top_up <- sampled |>
    dplyr::filter(!.data$id %in% initial$id) |>
    dplyr::mutate(
      sample_bucket = factor(
        .data$sample_bucket,
        levels = bucket_order,
        ordered = TRUE
      )
    ) |>
    dplyr::arrange(.data$sample_bucket, .data$id) |>
    dplyr::slice_head(n = remaining_n) |>
    dplyr::mutate(sample_bucket = as.character(.data$sample_bucket))

  dplyr::bind_rows(initial, top_up) |>
    dplyr::mutate(
      sample_bucket = factor(
        .data$sample_bucket,
        levels = bucket_order,
        ordered = TRUE
      )
    ) |>
    dplyr::arrange(.data$sample_bucket, .data$id) |>
    dplyr::mutate(sample_bucket = as.character(.data$sample_bucket))
}

# Evaluate warmup stopping with prospective zero-FN confirmation.
should_stop_warmup <- function(
  warmup_log,
  k_stable = 2L,
  k_zero_fn = 2L,
  min_reviewed = 50L,
  min_positives = 5L,
  max_reviewed = 300L,
  max_rounds = 20L
) {
  if (rlang::is_empty(warmup_log) || !nrow(warmup_log)) {
    return(list(
      stop = FALSE,
      cap_hit = FALSE,
      stable_streak = 0L,
      zero_fn_streak = 0L,
      guards_met = FALSE
    ))
  }

  required <- c(
    "criteria_changed",
    "batch_FN",
    "reviewed_n",
    "reviewed_positive_n"
  )
  missing <- setdiff(required, names(warmup_log))
  if (length(missing)) {
    cli::cli_abort(
      "warmup_log is missing columns {.field {missing}}."
    )
  }

  stable_flags <- !as.logical(warmup_log$criteria_changed)
  zero_fn_flags <- stable_flags & warmup_log$batch_FN == 0

  stable_streak <- trailing_true_streak(stable_flags)
  zero_fn_streak <- trailing_true_streak(zero_fn_flags)

  reviewed_n <- warmup_log$reviewed_n[[nrow(warmup_log)]]
  positive_n <- warmup_log$reviewed_positive_n[[nrow(warmup_log)]]
  round_n <- nrow(warmup_log)

  guards_met <- reviewed_n >= min_reviewed && positive_n >= min_positives
  cap_hit <- reviewed_n >= max_reviewed || round_n >= max_rounds
  stop <- guards_met &&
    stable_streak >= k_stable &&
    zero_fn_streak >= k_zero_fn

  list(
    stop = stop,
    cap_hit = cap_hit,
    stable_streak = stable_streak,
    zero_fn_streak = zero_fn_streak,
    guards_met = guards_met
  )
}

# Pick the best warmup round using FN, FP, then cumulative changes.
select_best_warmup_round <- function(warmup_log) {
  if (rlang::is_empty(warmup_log) || !nrow(warmup_log)) {
    cli::cli_abort("warmup_log must contain at least one round.")
  }

  required <- c("round", "FN", "FP", "cumulative_changes")
  missing <- setdiff(required, names(warmup_log))
  if (length(missing)) {
    cli::cli_abort("warmup_log is missing columns {.field {missing}}.")
  }

  warmup_log |>
    dplyr::arrange(
      .data$FN,
      .data$FP,
      .data$cumulative_changes,
      .data$round
    ) |>
    dplyr::slice_head(n = 1)
}

# Flag records for Vella-style adjudication when evidence is strong enough.
should_trigger_adjudication <- function(
  label_original,
  ai_label,
  has_revision_entry,
  contradiction_count,
  contradiction_threshold = 1L
) {
  required_lengths <- c(
    length(label_original),
    length(ai_label),
    length(has_revision_entry),
    length(contradiction_count)
  )
  if (length(unique(required_lengths)) != 1L) {
    cli::cli_abort("All adjudication inputs must have the same length.")
  }

  contradiction <- !is.na(ai_label) & ai_label != label_original
  flagged_contradiction <- contradiction &
    contradiction_count >= contradiction_threshold

  as.logical(has_revision_entry) &
    flagged_contradiction
}

# Apply Vella adjudication rules to a reviewed batch.
apply_vella_adjudication <- function(
  review_batch,
  round_index,
  contradiction_threshold = 1L
) {
  required <- c(
    "label_original",
    "label_operational",
    "label_revised_candidate",
    "revision_entry_available",
    "ai_label",
    "contradiction_count"
  )
  missing <- setdiff(required, names(review_batch))
  if (length(missing)) {
    cli::cli_abort(
      "review_batch is missing columns {.field {missing}}."
    )
  }

  contradiction_next <- dplyr::if_else(
    !is.na(review_batch$ai_label) &
      review_batch$ai_label != review_batch$label_operational,
    review_batch$contradiction_count + 1L,
    0L
  )

  adjudication_triggered <- should_trigger_adjudication(
    label_original = review_batch$label_original,
    ai_label = review_batch$ai_label,
    has_revision_entry = review_batch$revision_entry_available,
    contradiction_count = contradiction_next,
    contradiction_threshold = contradiction_threshold
  )

  adjudication_applied <- adjudication_triggered &
    !is.na(review_batch$label_revised_candidate)

  label_operational_next <- dplyr::if_else(
    adjudication_applied,
    review_batch$label_revised_candidate,
    review_batch$label_operational
  )

  tibble::as_tibble(review_batch) |>
    dplyr::mutate(
      contradiction_count = contradiction_next,
      adjudication_triggered = adjudication_triggered,
      adjudication_applied = adjudication_applied,
      adjudication_round = dplyr::if_else(
        .data$adjudication_applied,
        round_index,
        NA_integer_
      ),
      label_operational = label_operational_next,
      operative_truth_version = dplyr::if_else(
        .data$label_operational != .data$label_original,
        "revised",
        "original"
      ),
      human_label = .data$label_operational
    )
}

# Detect whether Gastaldi-style amendment evidence has accumulated.
should_trigger_protocol_amendment <- function(
  warmup_log,
  k_evidence = 2L
) {
  if (rlang::is_empty(warmup_log) || !nrow(warmup_log)) {
    return(FALSE)
  }

  required <- c("criteria_version", "protocol_mismatch_signal", "batch_FN")
  missing <- setdiff(required, names(warmup_log))
  if (length(missing)) {
    cli::cli_abort(
      "warmup_log is missing columns {.field {missing}}."
    )
  }

  evidence_flags <- warmup_log$criteria_version %in% "original" &
    warmup_log$protocol_mismatch_signal &
    warmup_log$batch_FN > 0

  trailing_true_streak(evidence_flags) >= k_evidence
}

# Update the Gastaldi residual-issue counters after the revised pass.
update_gastaldi_residual_issues <- function(
  residual_issue_n,
  batch_fn,
  criteria_change_suggested,
  protocol_mismatch_signal
) {
  residual_issue_logged <- batch_fn > 0 ||
    isTRUE(criteria_change_suggested) ||
    isTRUE(protocol_mismatch_signal)

  list(
    residual_issue_logged = residual_issue_logged,
    residual_issue_n = as.integer(residual_issue_n) +
      as.integer(residual_issue_logged)
  )
}

# Summarize assisted-screening performance for one truth definition.
summarise_assisted_metrics <- function(
  state,
  truth_column,
  metric_scope
) {
  required <- c("final_label", "review_source", truth_column)
  missing <- setdiff(required, names(state))
  if (length(missing)) {
    cli::cli_abort("state is missing columns {.field {missing}}.")
  }

  predicted_included <- state$final_label %in% "y"
  truth <- state[[truth_column]]
  tp <- sum(truth & predicted_included, na.rm = TRUE)
  fp <- sum(!truth & predicted_included, na.rm = TRUE)
  tn <- sum(!truth & !predicted_included, na.rm = TRUE)
  fn <- sum(truth & !predicted_included, na.rm = TRUE)
  metric_tbl <- summarise_confusion_metrics(
    TP = tp,
    FP = fp,
    TN = tn,
    FN = fn
  )

  tibble::tibble(
    dataset_name = dplyr::first(state$dataset_name),
    scenario_type = dplyr::first(state$scenario_type),
    metric_scope = metric_scope,
    reviewed_total = sum(!is.na(state$final_label)),
    human_reviewed = sum(grepl("^human", state$review_source), na.rm = TRUE),
    ai_reviewed = sum(grepl("^ai", state$review_source), na.rm = TRUE),
    TP = tp,
    FP = fp,
    TN = tn,
    FN = fn,
    recall = metric_tbl$se,
    precision = metric_tbl$ppv,
    specificity = metric_tbl$sp,
    se_num = metric_tbl$se_num,
    se_denom = metric_tbl$se_denom,
    sp_num = metric_tbl$sp_num,
    sp_denom = metric_tbl$sp_denom,
    ppv_num = metric_tbl$ppv_num,
    ppv_denom = metric_tbl$ppv_denom,
    missed_includes = fn
  )
}

# Summarize reviewed-batch performance by phase and criteria version.
summarise_review_phase_metrics <- function(review_log) {
  if (rlang::is_empty(review_log) || !nrow(review_log)) {
    return(tibble::tibble())
  }

  required <- c(
    "dataset_name",
    "review_phase",
    "criteria_version",
    "human_label",
    "ai_label"
  )
  missing <- setdiff(required, names(review_log))
  if (length(missing)) {
    cli::cli_abort(
      "review_log is missing columns {.field {missing}}."
    )
  }

  review_log |>
    dplyr::summarise(
      reviewed_n = dplyr::n(),
      TP = sum(.data$human_label & .data$ai_label, na.rm = TRUE),
      FP = sum(!.data$human_label & .data$ai_label, na.rm = TRUE),
      TN = sum(!.data$human_label & !.data$ai_label, na.rm = TRUE),
      FN = sum(.data$human_label & !.data$ai_label, na.rm = TRUE),
      .by = c("dataset_name", "review_phase", "criteria_version")
    ) |>
    dplyr::mutate(
      se_num = .data$TP,
      se_denom = .data$TP + .data$FN,
      recall = dplyr::if_else(
        .data$se_denom > 0,
        .data$se_num / .data$se_denom,
        NA_real_
      ),
      sp_num = .data$TN,
      sp_denom = .data$TN + .data$FP,
      specificity = dplyr::if_else(
        .data$sp_denom > 0,
        .data$sp_num / .data$sp_denom,
        NA_real_
      ),
      ppv_num = .data$TP,
      ppv_denom = .data$TP + .data$FP,
      precision = dplyr::if_else(
        .data$ppv_denom > 0,
        .data$ppv_num / .data$ppv_denom,
        NA_real_
      )
    ) |>
    dplyr::arrange(.data$dataset_name, .data$review_phase)
}

# Apply the same uncertainty-zone labeling rules used in the model code.
assign_uncertainty_zone_labels <- function(pred_low, pred_up, target) {
  if (
    length(pred_low) != length(pred_up) ||
      length(pred_low) != length(target)
  ) {
    cli::cli_abort("pred_low, pred_up, and target must have the same length.")
  }

  reviewed_pos <- which(target %in% "y")
  reviewed_neg <- which(target %in% "n")

  if (!length(reviewed_pos) || !length(reviewed_neg)) {
    return(rep("unk", length(target)))
  }

  neg_lim <- max(pred_up[reviewed_neg], na.rm = TRUE)
  pos_lim <- min(pred_low[reviewed_pos], na.rm = TRUE)

  labels <- dplyr::case_when(
    pred_low > neg_lim & pred_low > pos_lim ~ "y",
    pred_up < pos_lim & pred_up < neg_lim ~ "n",
    TRUE ~ "unk"
  )

  dplyr::if_else(
    !is.na(target) & labels != target & labels != "unk",
    "check",
    labels
  )
}
