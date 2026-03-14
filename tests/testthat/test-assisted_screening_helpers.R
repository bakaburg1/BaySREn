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
  expect_identical(out_1$id, c(1L, 3L, 2L))
})

# Tests for combine_llm_labels() --------------------------------------------

test_that("combine_llm_labels returns first model for single strategy", {
  # Create a two-model vote matrix.
  vote_matrix <- matrix(
    c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    ncol = 2
  )

  # Single strategy should return first column labels.
  out <- combine_llm_labels(vote_matrix, strategy = "single")

  expect_identical(out, vote_matrix[, 1])
})

test_that("combine_llm_labels applies strict majority vote", {
  # Build a three-model matrix with clear majorities.
  vote_matrix <- matrix(
    c(
      TRUE, TRUE, FALSE,
      FALSE, FALSE, FALSE,
      TRUE, FALSE, TRUE
    ),
    byrow = TRUE,
    ncol = 3
  )

  # Majority vote should use > 50% positives.
  out <- combine_llm_labels(vote_matrix, strategy = "vote")

  expect_identical(out, c(TRUE, FALSE, TRUE))
})

test_that("combine_llm_labels uses tie breaker on tied rows", {
  # Build an even-model matrix where rows 1 and 3 are ties.
  vote_matrix <- matrix(
    c(
      TRUE, FALSE,
      TRUE, TRUE,
      FALSE, TRUE
    ),
    byrow = TRUE,
    ncol = 2
  )
  tie_break <- c(FALSE, TRUE, TRUE)

  # Tie-break strategy should replace only tied rows.
  out <- combine_llm_labels(
    primary_labels = vote_matrix,
    strategy = "tie_break",
    tie_break_labels = tie_break
  )

  expect_identical(out, c(FALSE, TRUE, TRUE))
})

# Tests for should_stop_warmup() --------------------------------------------

test_that("should_stop_warmup stops only when A+B and guards are satisfied", {
  # Build a warmup log that satisfies all stop conditions.
  warmup_log <- tibble::tibble(
    round = 1:4,
    reviewed_n = c(25L, 50L, 75L, 100L),
    reviewed_positive_n = c(2L, 5L, 6L, 7L),
    FN = c(2L, 1L, 0L, 0L),
    FP = c(3L, 2L, 0L, 0L),
    criteria_changed = c(TRUE, TRUE, FALSE, FALSE),
    cumulative_changes = c(1L, 2L, 2L, 2L)
  )

  # Expect stop because trailing stable and errorfree streaks are both 2.
  out <- should_stop_warmup(
    warmup_log = warmup_log,
    k_stable = 2L,
    k_errorfree = 2L,
    min_reviewed = 50L,
    min_positives = 5L,
    max_reviewed = 300L,
    max_rounds = 20L
  )

  expect_true(out$stop)
  expect_true(out$guards_met)
  expect_identical(out$stable_streak, 2L)
  expect_identical(out$errorfree_streak, 2L)
  expect_false(out$cap_hit)
})

test_that("should_stop_warmup enforces strict zero-error condition", {
  # Build a log where latest round has FN > 0.
  warmup_log <- tibble::tibble(
    round = 1:3,
    reviewed_n = c(50L, 75L, 100L),
    reviewed_positive_n = c(5L, 6L, 7L),
    FN = c(0L, 0L, 1L),
    FP = c(0L, 0L, 0L),
    criteria_changed = c(FALSE, FALSE, FALSE),
    cumulative_changes = c(1L, 1L, 1L)
  )

  # Expect no stop because strict FN == 0 is violated in the latest round.
  out <- should_stop_warmup(
    warmup_log = warmup_log,
    k_stable = 2L,
    k_errorfree = 2L,
    min_reviewed = 50L,
    min_positives = 5L,
    max_reviewed = 300L,
    max_rounds = 20L
  )

  expect_false(out$stop)
  expect_identical(out$errorfree_streak, 0L)
})

test_that("should_stop_warmup reports cap hit when limits are reached", {
  # Build a log that hits reviewed cap before meeting stop condition.
  warmup_log <- tibble::tibble(
    round = 1:2,
    reviewed_n = c(40L, 60L),
    reviewed_positive_n = c(2L, 3L),
    FN = c(2L, 1L),
    FP = c(3L, 2L),
    criteria_changed = c(TRUE, TRUE),
    cumulative_changes = c(1L, 2L)
  )

  # Expect cap hit and no stop.
  out <- should_stop_warmup(
    warmup_log = warmup_log,
    k_stable = 2L,
    k_errorfree = 2L,
    min_reviewed = 50L,
    min_positives = 5L,
    max_reviewed = 60L,
    max_rounds = 20L
  )

  expect_false(out$stop)
  expect_true(out$cap_hit)
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
