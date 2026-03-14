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

# Detect whether criteria changed between two snapshots.
criteria_changed <- function(previous, current) {
  prev_norm <- normalize_criteria(previous)
  curr_norm <- normalize_criteria(current)

  !identical(prev_norm, curr_norm) &&
    !(identical(curr_norm$include, "unchanged") &&
      identical(curr_norm$exclude, "unchanged"))
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
    dplyr::arrange(dplyr::desc(.data$embedding_score), .data$mean_rank, .data$id) |>
    dplyr::mutate(sorting_id = dplyr::row_number()) |>
    dplyr::select("id", "embedding_score", "sorting_id", "mean_rank", "seed_count")
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

# Compute reviewed-set confusion counts for strict warmup checks.
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

# Evaluate A+B warmup stopping with minimum guards and hard caps.
should_stop_warmup <- function(
  warmup_log,
  k_stable = 2L,
  k_errorfree = 2L,
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
      errorfree_streak = 0L,
      guards_met = FALSE
    ))
  }

  required <- c(
    "criteria_changed",
    "FN",
    "FP",
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
  errorfree_flags <- warmup_log$FN == 0 & warmup_log$FP == 0

  stable_streak <- trailing_true_streak(stable_flags)
  errorfree_streak <- trailing_true_streak(errorfree_flags)

  reviewed_n <- warmup_log$reviewed_n[[nrow(warmup_log)]]
  positive_n <- warmup_log$reviewed_positive_n[[nrow(warmup_log)]]
  round_n <- nrow(warmup_log)

  guards_met <- reviewed_n >= min_reviewed && positive_n >= min_positives
  cap_hit <- reviewed_n >= max_reviewed || round_n >= max_rounds
  stop <- guards_met &&
    stable_streak >= k_stable &&
    errorfree_streak >= k_errorfree

  list(
    stop = stop,
    cap_hit = cap_hit,
    stable_streak = stable_streak,
    errorfree_streak = errorfree_streak,
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
