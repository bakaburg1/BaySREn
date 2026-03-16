# Helper functions -----------------------------------------------------------

helper_reviewed_records <- function() {
  tibble::tibble(
    id = 1:6,
    title = letters[1:6],
    abstract = LETTERS[1:6],
    human_label = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE),
    ai_label = c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE)
  )
}

helper_dataset_records <- function() {
  tibble::tibble(
    title = c("study a", "study b"),
    abstract = c("abstract a", "abstract b"),
    authors = c("alpha", "beta"),
    keywords = c("foo", "bar"),
    included = c(TRUE, FALSE)
  )
}

# Tests for aggregate_seed_rankings() ---------------------------------------

test_that("aggregate seed rankings is deterministic across input order", {
  # Build two ranking tables with different row orders.
  rank_1 <- tibble::tibble(
    id = c(1L, 2L, 3L),
    embedding_score = c(0.90, 0.40, 0.70),
    sorting_id = c(1L, 3L, 2L)
  )
  rank_2 <- tibble::tibble(
    id = c(3L, 1L, 2L),
    embedding_score = c(0.80, 0.60, 0.20),
    sorting_id = c(1L, 2L, 3L)
  )

  # Aggregate with one order and then with reversed list order.
  out_1 <- aggregate_seed_rankings(list(rank_1, rank_2))
  out_2 <- aggregate_seed_rankings(list(rank_2, rank_1))

  # Verify the final ranking order and scores are stable.
  expect_identical(out_1$id, out_2$id)
  expect_identical(out_1$sorting_id, out_2$sorting_id)
  expect_equal(out_1$embedding_score, out_2$embedding_score)
  expect_identical(out_1$id, c(2L, 1L, 3L))
})

test_that("aggregate seed rankings orders lower embedding distances first", {
  # Build two rankings where the smallest mean distance should rank first.
  rank_1 <- tibble::tibble(
    id = c(1L, 2L),
    embedding_score = c(0.20, 0.60),
    sorting_id = c(1L, 2L)
  )
  rank_2 <- tibble::tibble(
    id = c(1L, 2L),
    embedding_score = c(0.30, 0.10),
    sorting_id = c(2L, 1L)
  )

  out <- aggregate_seed_rankings(list(rank_1, rank_2))

  expect_identical(out$id, c(1L, 2L))
  expect_equal(out$embedding_score, c(0.25, 0.35))
  expect_identical(out$sorting_id, c(1L, 2L))
})

# Tests for build_assisted_experiment_grid() ---------------------------------

test_that("build_assisted_experiment_grid expands models and review modes", {
  # Build a three-model registry with distinct API args.
  model_registry <- tibble::tibble(
    labeller_model = c(
      "x-ai/grok-4.1-fast",
      "openai/gpt-oss-20b",
      "openai/gpt-oss-120b"
    ),
    label_api_args = list(
      list(temperature = 0),
      list(temperature = 0),
      list(temperature = 0)
    )
  )

  # Expand the registry across both human-review modes.
  out <- build_assisted_experiment_grid(
    model_registry = model_registry,
    human_after_warmup_values = c(TRUE, FALSE)
  )

  expect_identical(nrow(out), 6L)
  expect_setequal(
    out$experiment_name,
    c(
      "x_ai_grok_4_1_fast_human",
      "x_ai_grok_4_1_fast_ai",
      "openai_gpt_oss_20b_human",
      "openai_gpt_oss_20b_ai",
      "openai_gpt_oss_120b_human",
      "openai_gpt_oss_120b_ai"
    )
  )
  expect_setequal(out$labeller_model, model_registry$labeller_model)
  expect_identical(sort(unique(out$human_after_warmup)), c(FALSE, TRUE))
})

# Tests for select_warmup_refiner_samples() ---------------------------------

# Tests for summarise_confusion_metrics() ------------------------------------

test_that("summarise_confusion_metrics returns Se Sp and PPV numerators", {
  # Summarize one confusion table with non-zero denominators.
  out <- summarise_confusion_metrics(
    TP = 8L,
    FP = 2L,
    TN = 5L,
    FN = 1L
  )

  expect_identical(out$se_num[[1]], 8L)
  expect_identical(out$se_denom[[1]], 9L)
  expect_equal(out$se[[1]], 8 / 9)
  expect_identical(out$sp_num[[1]], 5L)
  expect_identical(out$sp_denom[[1]], 7L)
  expect_equal(out$sp[[1]], 5 / 7)
  expect_identical(out$ppv_num[[1]], 8L)
  expect_identical(out$ppv_denom[[1]], 10L)
  expect_equal(out$ppv[[1]], 0.8)
})

test_that("summarise_confusion_metrics uses NA for zero denominators", {
  # Return missing ratios when a metric denominator is undefined.
  out <- summarise_confusion_metrics(
    TP = 0L,
    FP = 0L,
    TN = 4L,
    FN = 0L
  )

  expect_true(is.na(out$se[[1]]))
  expect_equal(out$sp[[1]], 1)
  expect_true(is.na(out$ppv[[1]]))
})

test_that("select_warmup_refiner_samples returns a stratified reviewed set", {
  # Build reviewed records spanning all four confusion buckets.
  reviewed <- helper_reviewed_records()

  # Request one sample per bucket.
  out <- select_warmup_refiner_samples(
    reviewed_data = reviewed,
    max_total = 4L,
    quotas = c(FN = 1L, FP = 1L, TP = 1L, TN = 1L)
  )

  expect_length(out$id, 4L)
  expect_identical(out$sample_bucket, c("FN", "FP", "TP", "TN"))
})

test_that("select_warmup_refiner_samples enforces the global sample cap", {
  # Build reviewed records where default quotas would otherwise exceed the cap.
  reviewed <- helper_reviewed_records()

  # Default quotas sum above five, so the helper must truncate deterministically.
  out <- select_warmup_refiner_samples(
    reviewed_data = reviewed,
    max_total = 5L
  )

  expect_length(out$id, 5L)
  expect_identical(out$sample_bucket[[1]], "FN")
  expect_identical(out$sample_bucket[[2]], "FP")
})

# Tests for should_stop_warmup() --------------------------------------------

test_that("should_stop_warmup uses prospective zero-FN confirmation", {
  # Build a warmup log with two stable future batches and non-zero FPs.
  warmup_log <- tibble::tibble(
    round = 1:3,
    reviewed_n = c(25L, 50L, 75L),
    reviewed_positive_n = c(2L, 5L, 6L),
    batch_FN = c(1L, 0L, 0L),
    batch_FP = c(5L, 4L, 3L),
    criteria_changed = c(TRUE, FALSE, FALSE)
  )

  # Stop should depend on stable zero-FN batches, not zero FPs.
  out <- should_stop_warmup(
    warmup_log = warmup_log,
    k_stable = 2L,
    k_zero_fn = 2L,
    min_reviewed = 50L,
    min_positives = 5L,
    max_reviewed = 300L,
    max_rounds = 20L
  )

  expect_true(out$stop)
  expect_identical(out$stable_streak, 2L)
  expect_identical(out$zero_fn_streak, 2L)
})

test_that("should_stop_warmup resets confirmation after a criteria change", {
  # Build a log where a late criteria change resets the confirmation streak.
  warmup_log <- tibble::tibble(
    round = 1:4,
    reviewed_n = c(50L, 75L, 100L, 125L),
    reviewed_positive_n = c(5L, 6L, 7L, 8L),
    batch_FN = c(0L, 0L, 0L, 0L),
    batch_FP = c(1L, 1L, 1L, 1L),
    criteria_changed = c(FALSE, FALSE, TRUE, FALSE)
  )

  # The final round should not freeze after only one post-change batch.
  out <- should_stop_warmup(
    warmup_log = warmup_log,
    k_stable = 2L,
    k_zero_fn = 2L,
    min_reviewed = 50L,
    min_positives = 5L,
    max_reviewed = 300L,
    max_rounds = 20L
  )

  expect_false(out$stop)
  expect_identical(out$stable_streak, 1L)
  expect_identical(out$zero_fn_streak, 1L)
})

test_that("should_stop_warmup reports cap hit when limits are reached", {
  # Build a log that reaches the reviewed cap before confirmation succeeds.
  warmup_log <- tibble::tibble(
    round = 1:2,
    reviewed_n = c(40L, 60L),
    reviewed_positive_n = c(2L, 3L),
    batch_FN = c(2L, 1L),
    batch_FP = c(3L, 2L),
    criteria_changed = c(TRUE, TRUE)
  )

  out <- should_stop_warmup(
    warmup_log = warmup_log,
    k_stable = 2L,
    k_zero_fn = 2L,
    min_reviewed = 50L,
    min_positives = 5L,
    max_reviewed = 60L,
    max_rounds = 20L
  )

  expect_false(out$stop)
  expect_true(out$cap_hit)
})

# Tests for initialize_assisted_dataset_state() -----------------------------

test_that("initialize_assisted_dataset_state attaches Vella revisions", {
  # Build a small dataset and one matching revision entry.
  dataset_records <- helper_dataset_records()
  revision_lookup <- tibble::tibble(
    title = "study b",
    abstract = "abstract b",
    label_revised_candidate = TRUE,
    revision_entry_available = TRUE
  )

  # Initialize the adjudication scenario with title/abstract matching.
  out <- initialize_assisted_dataset_state(
    data = dataset_records,
    dataset_name = "vella",
    scenario_type = "adjudication",
    revision_lookup = revision_lookup
  )

  expect_identical(out$scenario_type, c("adjudication", "adjudication"))
  expect_length(unique(out$record_key), 2L)
  expect_false(out$revision_entry_available[[1]])
  expect_true(out$revision_entry_available[[2]])
  expect_true(out$label_revised_candidate[[2]])
})

# Tests for Vella adjudication helpers --------------------------------------

test_that("apply_vella_adjudication switches only strongly flagged records", {
  # Build one immediate contradiction and one record without a revision entry.
  review_batch <- tibble::tibble(
    label_original = c(TRUE, TRUE),
    label_operational = c(TRUE, TRUE),
    label_revised_candidate = c(FALSE, FALSE),
    revision_entry_available = c(TRUE, FALSE),
    ai_label = c(FALSE, FALSE),
    contradiction_count = c(0L, 0L)
  )

  # Only the record with a mapped correction should switch operative truth.
  out <- apply_vella_adjudication(
    review_batch = review_batch,
    round_index = 2L,
    contradiction_threshold = 1L
  )

  expect_identical(out$adjudication_applied, c(TRUE, FALSE))
  expect_identical(out$human_label, c(FALSE, TRUE))
  expect_identical(out$operative_truth_version, c("revised", "original"))
  expect_identical(out$adjudication_round, c(2L, NA_integer_))
  expect_identical(out$contradiction_count, c(1L, 1L))
})

test_that("should_trigger_protocol_amendment requires repeated evidence", {
  # Build repeated original-criteria evidence for a scope mismatch.
  warmup_log <- tibble::tibble(
    criteria_version = c("original", "original"),
    protocol_mismatch_signal = c(TRUE, TRUE),
    batch_FN = c(2L, 1L)
  )

  expect_true(should_trigger_protocol_amendment(warmup_log, k_evidence = 2L))
})

test_that("should_trigger_protocol_amendment ignores revised-criteria rounds", {
  # Build the same evidence after amendment, which should no longer retrigger.
  warmup_log <- tibble::tibble(
    criteria_version = c("revised", "revised"),
    protocol_mismatch_signal = c(TRUE, TRUE),
    batch_FN = c(2L, 1L)
  )

  expect_false(should_trigger_protocol_amendment(warmup_log, k_evidence = 2L))
})

test_that("update_gastaldi_residual_issues logs post-switch mismatches", {
  # Count one residual issue from a remaining false negative batch.
  first_out <- update_gastaldi_residual_issues(
    residual_issue_n = 0L,
    batch_fn = 1L,
    criteria_change_suggested = FALSE,
    protocol_mismatch_signal = FALSE
  )

  # Count a second issue from continued mismatch pressure without new FN.
  second_out <- update_gastaldi_residual_issues(
    residual_issue_n = first_out$residual_issue_n,
    batch_fn = 0L,
    criteria_change_suggested = TRUE,
    protocol_mismatch_signal = TRUE
  )

  expect_true(first_out$residual_issue_logged)
  expect_identical(first_out$residual_issue_n, 1L)
  expect_true(second_out$residual_issue_logged)
  expect_identical(second_out$residual_issue_n, 2L)
})

# Tests for summary helpers --------------------------------------------------

test_that("summarise_assisted_metrics ignores missing review sources", {
  # Build a partial state table with one human and one AI-reviewed record.
  state <- tibble::tibble(
    dataset_name = "vella",
    scenario_type = "adjudication",
    final_label = c("y", NA_character_, "n"),
    review_source = c("human_warmup", NA_character_, "ai_after_warmup"),
    label_operational = c(TRUE, TRUE, FALSE)
  )

  # Missing review sources should not turn count summaries into NA.
  out <- summarise_assisted_metrics(
    state = state,
    truth_column = "label_operational",
    metric_scope = "operative_final"
  )

  expect_identical(out$human_reviewed[[1]], 1L)
  expect_identical(out$ai_reviewed[[1]], 1L)
})

# Tests for select_best_warmup_round() --------------------------------------

test_that("select_best_warmup_round follows FN FP then changes ordering", {
  # Build candidate warmup rounds for fallback selection.
  warmup_log <- tibble::tibble(
    round = c(1L, 2L, 3L, 4L),
    FN = c(1L, 0L, 0L, 0L),
    FP = c(1L, 2L, 2L, 2L),
    cumulative_changes = c(4L, 3L, 1L, 2L)
  )

  # Expect round 3 because it minimizes FN, FP, then cumulative changes.
  best <- select_best_warmup_round(warmup_log)

  expect_identical(best$round[[1]], 3L)
})
