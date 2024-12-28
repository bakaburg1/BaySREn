library(dplyr)
library(rlang)

default_timestamp <- "2024-12-21T18.01.24"

# Helper functions for tests --------------------------------------------

#' Add or subtract minutes from a timestamp string
#'
#' @param timestamp String timestamp in format "YYYY-MM-DDTHH.MM.SS".
#' @param minutes Number of minutes to add (positive) or subtract (negative).
#'
#' @return Modified timestamp string in same format.
#'
add_minutes <- function(timestamp, minutes) {
  # Parse timestamp string into POSIXct
  dt <- as.POSIXct(
    strptime(timestamp, format = "%Y-%m-%dT%H.%M.%S"),
    tz = "UTC"
  )

  # Add/subtract minutes
  new_dt <- dt + minutes * 60

  # Format back to string
  format(new_dt, "%Y-%m-%dT%H.%M.%S")
}

#' Create a temporary test directory that gets cleaned up
#'
#' @param env Environment to defer the cleanup to.
#'
#' @return Path to temporary directory.
local_test_dir <- function(env = parent.frame()) {
  temp_dir <- tempfile("baysren_test_")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(temp_dir, recursive = TRUE), env)
  temp_dir
}

#' Generate Mock Annotations with Manual Labels and Optional Predictions
#'
#' This function creates a mock annotations data frame with a specified
#' proportion of manually labeled records and a defined positivity rate. It can
#' generate both initial annotations and subsequent iterations with predictions.
#'
#' The function can operate in two modes: 1. Creating new annotations from
#' scratch with specified parameters 2. Building upon previous annotations to
#' simulate iteration progress.
#'
#' For new annotations, it generates:
#' - Manual labels based on manual_rate;
#' - Positive/negative labels based on positivity_rate;
#'
#' For iterations with predictions, it adds:
#' - Prediction probabilities within specified bounds;
#' - Review flags (*) for uncertain predictions based on unk_rate parameter;
#' - New predictions targeting sensitivity level specified by sens parameter;
#' - Updated manual review status;
#'
#' The `sens` parameter controls prediction sensitivity, while `unk_rate`
#' determines the proportion of predictions flagged for manual review.
#'
#' @param n Integer. Total number of records to generate. Default is 100.
#' @param manual_rate Numeric. Proportion of records that are manually labeled.
#'   Must be between 0 and 1.
#' @param positivity_rate Numeric. Proportion of positive labels ("y") among all
#'   records. Must be between 0 and 1.
#' @param with_predictions Logical. If TRUE, adds prediction-related columns.
#'   Default is TRUE.
#' @param positives_lbound Numeric. Lower bound for positive predictions (0-1).
#'   Default is 0.7.
#' @param negatives_ubound Numeric. Upper bound for negative predictions (0-1).
#'   Default is 0.75.
#' @param sens Numeric. Desired sensitivity level (0-1). Default is 0.95.
#' @param unk_rate Numeric. Rate of "unknown" predictions (0-1). Default is
#'   0.25.
#' @param reviewed Logical. Whether predictions marked for review (*) should be
#'   replaced with ground truth. Default is TRUE.
#' @param previous_annotations Data frame. Previous iteration's annotations to
#'   build upon. Default is NULL.
#' @param keep_ground_truth Logical. Whether to keep the Rev_previous column in
#'   output. Default is TRUE.
#' @param seed Integer. Random seed for reproducibility. Needs to be changed
#'   between replications otherwise the replication system gets stuck.
#'
#' @return A data frame with mock annotation data, including manual labels and
#'   optionally prediction-related columns.
#'
mock_annotations <- function(
    n = 100,
    manual_rate = 0.25,
    positivity_rate = 0.25,
    with_predictions = TRUE,
    positives_lbound = 0.7,
    negatives_ubound = 0.75,
    sens = .95,
    unk_rate = 0.25,
    reviewed = TRUE,
    previous_annotations = NULL,
    keep_ground_truth = TRUE,
    seed = 12345
) {

  # Set seed for reproducibility
  # The seed need to modifiable otherwise the replication system gets stuck
  withr::local_seed(seed)

  if (!is.null(previous_annotations)) {
    df <- previous_annotations

    n <- nrow(df)

    # If "Rev_previous" is not present, create it by sampling from the
    # positivity_rate where there are no manual labels
    if ("Rev_previous" %nin% names(df)) {
      df$Rev_previous <- sample(
        c("y", "n"),
        size = n,
        replace = TRUE,
        prob = c(positivity_rate, 1 - positivity_rate)
      )

      df$Rev_previous <- case_when(
        !is.na(df$Rev_manual) ~ df$Rev_manual,
        .default = df$Rev_previous
      )
    }

    # Override with_predictions if previous annotations already have predictions
    with_predictions <- any(
      grepl("pred", ignore.case = TRUE, names(df))) || with_predictions

    # Consolidate Rev_prediction and Rev_prediction_new into Rev_prediction
    if (with_predictions && "Rev_prediction_new" %in% names(df)) {

      # Assumes "reviewed" is TRUE
      df$Rev_prediction_new <- case_when(
        df$Rev_prediction_new == "*" ~ df$Rev_previous,
        .default = df$Rev_prediction_new
      )

      # print("Prev df$Rev_prediction_new")
      # print(table(df$Rev_prediction_new, useNA = "always"))

      df$Rev_prediction <- coalesce(df$Rev_prediction_new, df$Rev_prediction)

      # print("Prev df$Rev_prediction")
      # print(table(df$Rev_prediction_new, useNA = "always"))
    }
  } else {

    # Validate input parameters
    if (manual_rate < 0 || manual_rate > 1) {
      stop("'manual_rate' must be between 0 and 1.")
    }

    if (positivity_rate < 0 || positivity_rate > 1) {
      stop("'positivity_rate' must be between 0 and 1.")
    }

    if (!is.null(sens) && (sens < 0 || sens > 1)) {
      stop("'sens' must be between 0 and 1 if provided.")
    }

    # Generate basic annotation data
    df <- data.frame(
      Order = 1:n,
      ID = 1:n,
      Rev_manual = NA_character_,
      Title = sprintf("Mock Title %d", 1:n),
      Abstract = sprintf("Mock Abstract %d", 1:n),
      Keywords = sprintf("keyword%d; keyword%d", 1:n, sample(1:n)),
      Authors = sprintf("Author%d et al.", 1:n),
      Year = sample(2000:2024, n, replace = TRUE),
      Journal = sprintf("Journal %d", sample(1:20, n, replace = TRUE)),
      DOI = sprintf("10.1234/mock%d", 1:n)
    )

    # First create Rev_previous to get the ground truth for all records
    df$Rev_previous <- sample(
      c("y", "n"),
      size = n,
      replace = TRUE,
      prob = c(positivity_rate, 1 - positivity_rate)
    )

    # Generate manual labels by copying Rev_previous and then randomly
    # removing some of them according to the manual_rate
    df$Rev_manual <- df$Rev_previous
    df$Rev_manual[
      sample(
        c(TRUE, FALSE), n, c(1 - manual_rate, manual_rate), replace = TRUE)
      ] <- NA_character_

    if (sum(df$Rev_manual %in% "y") == 0) {
      stop("No positive labels in mock annotations. Increase `n` ",
        "or `positivity_rate`")
    }

  }

  # If with_predictions is TRUE, add prediction-related columns
  if (with_predictions) {
    # Initialize prediction columns
    if ("Rev_prediction" %nin% names(df)) {
      df$Rev_prediction <- NA_character_
    }
    df$Rev_prediction_new <- NA_character_ # Reset to NA
    df$Predicted_label <- "n" # Default to not relevant

    manually_reviewed <- coalesce(df$Rev_manual, df$Rev_prediction)

    # print("manually_reviewed")
    # print(table(manually_reviewed, useNA = "always"))

    # Now create the predicted labels based on the previous labels, the ground
    # truth and the simulation parameters
    df$Predicted_label <- case_when(
      # Assume the model will match the training data
      !is.na(manually_reviewed) ~ manually_reviewed,
      # For the rest, sample based on the ground truth and sensitivity/unk rates
      df$Rev_previous == "y" ~ sample(
        c("y", "n", "unk"), nrow(df),
        c(sens, (1 - sens) * (1 - unk_rate), (1 - sens) * unk_rate),
        replace = TRUE),
      df$Rev_previous == "n" ~ sample(
        c("unk", "n"), nrow(df), c(unk_rate, 1 - unk_rate), replace = TRUE),
      .default = "n"
    )

    # print("Predicted_label")
    # print(table(df$Predicted_label, useNA = "always"))

    # Create Rev_prediction_new column where records in need of review are
    # highlighted with *
    df$Rev_prediction_new <- case_when(
      !is.na(df$Rev_prediction) | df$Predicted_label == "n" ~ NA_character_,
      df$Predicted_label == df$Rev_manual ~ NA_character_,
      df$Predicted_label %in% c("check", "unk", "y") ~ "*"
    )

    # If the annotations are reviewed, the * will be replaced by the ground
    # truth
    if (reviewed) {
      df$Rev_prediction_new <- case_match(
        df$Rev_prediction_new,
        "*" ~ df$Rev_previous,
        .default = df$Rev_prediction_new
      )
    }

    # print("Rev_prediction_new")
    # print(table(df$Rev_prediction_new, useNA = "always"))

    # Create the Target column
    df$Target <- coalesce(
      df$Rev_prediction_new |> # Exclude unreviewed labels
      replace(df$Rev_prediction_new %in% "*", NA_character_),
      manually_reviewed
    )

    # print("Target")
    # print(table(df$Target, useNA = "always"))

    # Finally, simulate the posterior predictive intervals
    upper_bound <- max(positives_lbound, negatives_ubound)
    lower_bound <- min(positives_lbound, negatives_ubound)

    posterior_preds <- purrr::map(seq_len(nrow(df)), \(i) {
      values <- if (df$Predicted_label[i] == "y") {

        runif(3, upper_bound, 1) |> sort()
      } else if (df$Predicted_label[i] == "n") {
        runif(3, 0, lower_bound) |> sort()
      } else {
        runif(3, lower_bound, upper_bound) |> sort()
      }

      as.data.frame.list(values[c(2, 1, 3)]) |>
        stats::setNames(c("Pred_Med", "Pred_Low", "Pred_Up"))
    }) |>
    dplyr::bind_rows() |>
    mutate(
      Pred_delta = Pred_Up - Pred_Low
    )

    df <- df |> mutate(df, posterior_preds, .after = "Predicted_label")

  }

  col_order <- c("Order", "Rev_manual", "Rev_prediction", "Rev_prediction_new",
    "Rev_previous", "Predicted_label", "Pred_Med", "Pred_Low", "Pred_Up",
    "Pred_delta", "DOI", "ID", "Title", "Abstract", "Authors", "Year", "URL",
    "Journal", "Journal_short", "Keywords", "Mesh", "Article_type",
    "N_citations", "Source", "Source_type", "FileID", "Parent_file", "Target")

  df <- select(df, any_of(col_order))

  if (!keep_ground_truth) {
    df$Rev_previous <- NULL
  }

  return(df)
}

#' Generate mock document-term matrix
#'
#' Creates a mock document-term matrix with binary features for testing
#' purposes.
#'
#' @param annotations Data frame with annotations. Optional if n is provided.
#' @param n_terms Integer. Number of terms to include in the matrix. Default is
#'   20.
#' @param n Integer. Number of rows to generate. Only used if annotations is
#'   NULL. Default is 100.
#'
#' @return Data frame with document-term matrix where columns are binary
#'   features prefixed by section (TITLE__, ABSTR__, KEYS__) and a Target
#'   column.
mock_dtm <- function(annotations = NULL, n_terms = 20, n = 100) {
  # Get number of rows from annotations or n parameter
  if (is.null(annotations) && is.null(n)) {
    stop("Either annotations or n must be provided")
  }
  n_rows <- if (!is.null(annotations)) nrow(annotations) else n

  sections <- c("TITLE__", "ABSTR__", "KEYS__")

  withr::local_seed(12345)

  # Create base data frame with ID
  dtm <- data.frame(ID = 1:n_rows)

  # Add random binary columns for each term, using a fixed number of terms per
  # section
  for (section in sections) {
    for (i in 1:min(5, n_terms)) {  # Use fixed number of terms (5 or less)
      col_name <- paste0(section, "term", i)
      dtm[[col_name]] <- stats::rbinom(n_rows, 1, 0.3)
    }
  }

  dtm$Target <- if (is.null(annotations)) {
    sample(c("y", "n"), n_rows, replace = TRUE)
  } else {
    annotations$Target
  }

  return(dtm)
}

#' Generate mock predictive distribution samples
#'
#' Creates mock MCMC samples for each record's predictive distribution.
#'
#' @param n_records Integer. Number of records. Default is 100.
#' @param n_samples Integer. Number of MCMC samples. Default is 20.
#' @param annotations Data frame. Annotations with predictions to base samples
#'   on. Default is generated using mock_annotations().
#'
#' @return Data frame with ID and sample probabilities columns.
mock_predictive_distr <- function(n_records = 100, n_samples = 20,
    annotations = mock_annotations(n = n_records, with_predictions = TRUE)) {
  withr::local_seed(12345)

  if ("Pred_Med" %nin% names(annotations)) {
    annotations <- mock_annotations(
      with_predictions = TRUE,
      previous_annotations = annotations
    )
  }

  n_records <- nrow(annotations)

  samples <- purrr::map(
    seq_len(n_records),
    ~ runif(n_samples, annotations$Pred_Low[.x], annotations$Pred_Up[.x])
  ) |> do.call(what = "rbind")

  data.frame(
    ID = 1:n_records,
    samples
  ) |>
    stats::setNames(c("ID", paste0("X", 1:n_samples)))
}

#' Generate mock variable importance data
#'
#' Creates mock variable importance scores for terms in the document-term
#' matrix.
#'
#' @param dtm Data frame. Document-term matrix to extract terms from. If NULL,
#'   generates mock DTM.
#' @param n_terms Integer. Number of terms to include. Default is 20.
#'
#' @return Data frame with columns Term (feature names), Value (importance
#'   scores), and Score (additional metric), sorted by Value descending.
mock_var_imp <- function(dtm = NULL, n_terms = 20) {
  if (is.null(dtm)) {
    dtm <- mock_dtm(n = n_terms)
  }

  terms <- colnames(dtm)[-1]
  n_terms <- length(terms)

  withr::local_seed(12345)

  data.frame(
    Term = sample(terms),
    Value = round(stats::runif(n_terms, 0.1, 0.9), 3),
    Score = round(stats::runif(n_terms, 1, 3), 2)
  ) |>
    arrange(desc(.data$Value))
}

#' Generate mock results data
#'
#' Creates a mock results summary data frame as would be produced by an
#' iteration of the active learning process.
#'
#' @param annotations Data frame. Annotations to base results on. Default is
#'   mock_annotations().
#' @param tot_pos Integer. Number of positive cases. Overrides value from
#'   annotations.
#' @param tot_records Integer. Total number of records. Overrides value from
#'   annotations.
#' @param n_features Integer. Number of features used.
#' @param repl Integer. Replication number. Default is 1.
#' @param iteration Integer. Current iteration number. Default is 1.
#' @param parent_file Character. Path to parent file.
#'
#' @return Data frame with Indicator and Value columns summarizing the iteration
#'   results.
mock_results <- function(
    annotations = mock_annotations(),
    tot_pos = NULL,
    tot_records = NULL,
    n_features = NULL,
    repl = 1,
    iteration = 1,
    parent_file = NULL
) {

  # Extract totals from annotation file if not provided
  if (is.null(tot_pos)) {
    tot_pos <- sum(annotations$Rev_prediction == "y", na.rm = TRUE)
  }

  if (is.null(tot_records)) {
    tot_records <- nrow(annotations)
  }

  if (is.null(n_features)) {
    n_features <- mock_dtm(annotations) |>
      select(-any_of(c("ID", "Target"))) |>
      ncol()
  }

  if (is.null(annotations) &&
    (is.null(tot_pos) || is.null(tot_records) || is.null(n_features))) {
    stop("annotations or tot_pos, tot_records, and n_features",
      "must be provided")
  }

  # Helper function to summarize vectors like in the main code
  mock_summarise_vector <- function(x) {
    if (length(x) == 0) {
      return("incorrect input")
    }

    n_neg <- sum(x %in% "n")
    n_pos <- sum(x %in% "y")
    total <- length(x)

    sprintf("n: %d (%.1f%%), y: %d (%.1f%%)",
            n_neg, n_neg/total * 100,
            n_pos, n_pos/total * 100)
  }

  before_preds <- coalesce_labels(
    annotations, c("Rev_prediction", "Rev_manual"))

  # Create results data frame with actual indicators and values from annotations
  with(annotations, {
    tibble(
      Iter = iteration,
      "Parent file" = parent_file %||% "",
      "Replication n." = repl,
      "N. features" = n_features,
      "Previous labeling" = mock_summarise_vector(before_preds),
      "New labels" = sum(!is.na(Rev_prediction_new)),
      "New labels' distribution" = Rev_prediction_new %>%
        na.omit() %>%
        mock_summarise_vector(),
      "Records to review" = mock_summarise_vector(
        Predicted_label[Rev_prediction_new %in% "*"]
      ),
      "Final labeling" = mock_summarise_vector(
        coalesce(Rev_prediction_new,
                Rev_prediction,
                Rev_manual,
                Predicted_label)
      ),
      "Target: n" = sum(Target %in% "n"),
      "Target: y" = sum(Target %in% "y"),
      # Total labeled and new labels counts
      "Total_labeled" = sprintf("%d (%.1f%%)",
        sum(!is.na(Target)),
        mean(!is.na(Target)) * 100),
      "New_labels" = sprintf("%d (%.1f%%)",
        sum(!is.na(Rev_prediction_new)),
        mean(!is.na(Rev_prediction_new)) * 100),
      # Calculate changes between states
      "Change: n -> n" = sum(before_preds %in% "n" &
        Target %in% "n"),
      "Change: n -> y" = sum(before_preds %in% "n" &
        Target %in% "y"),
      "Change: y -> n" = sum(before_preds %in% "y" &
        Target %in% "n"),
      "Change: unlab. -> n" = sum(is.na(before_preds) &
        Target %in% "n"),
      "Change: unlab. -> unlab." = sum(is.na(before_preds) &
        is.na(Target)),
      "Change: unlab. -> y" = sum(is.na(before_preds) &
        Target %in% "y"),
      "Change: y -> y" = sum(before_preds %in% "y" &
        Target %in% "y")
    )
  }) |>
    # Filter out "Change" columns that have zero values
    select(-(starts_with("Change") & where(~ .x == 0))) |>
    mutate(across(everything(), as.character)) |>
    tidyr::pivot_longer(
      everything(),
      names_to = "Indicator",
      values_to = "Value"
    )
}

#' Generate mock parameter page data
#'
#' Creates a mock parameters data frame as would be stored in the Arguments
#' sheet of a Records file.
#'
#' @param records_file Character. Path to records file.
#' @param pos_mult Numeric. Positive multiplier value. Default is 1.
#' @param n_models Integer. Number of models. Default is 1.
#' @param resample Logical. Whether to resample. Default is FALSE.
#' @param pred_quants Numeric vector. Prediction quantiles. Default is c(0.1,
#'   0.5, 0.9).
#'
#' @return Data frame with name and value columns containing parameter settings.
mock_param_page <- function(
    records_file = NULL,
    pos_mult = 1,
    n_models = 1,
    resample = FALSE,
    pred_quants = c(0.1, 0.5, 0.9)
) {
  # Create list of parameter pairs
  df <- tibble(
    session_name = basename(dirname(records_file %||% "")),
    file = records_file,
    DTM = "NULL",
    pos_mult = pos_mult,
    n_models = n_models,
    resample = resample,
    pred_quants = paste(pred_quants, collapse = ", "),
    sessions_folder = dirname(dirname(records_file %||% "")),
    autorun = TRUE,
    replication = "NULL",
    dup_session_action = "fill",
    limits = "List of 3
    $ stop_after    : num 4
    $ pos_target    : NULL
    $ labeling_limit: NULL",
    compute_performance = FALSE,
    test_data = "NULL",
    use_prev_labels = TRUE,
    prev_classification = "tbl_df, tbl, data.frame",
    rebuild = TRUE
  ) |>
    mutate(across(everything(), as.character)) |>
    tidyr::pivot_longer(everything(), names_to = "name", values_to = "value")

  return(df)
}

#' Generate a Records file with all sheets
#'
#' Creates a mock Records Excel file with all required sheets for testing.
#'
#' @param base_dir Character. Base directory for the file. Default is
#'   local_test_dir().
#' @param sessions_folder Character. Folder containing the sessions. Default is
#'   "Sessions".
#' @param session_name Character. Name of the session. Default is "Session1".
#' @param timestamp Character. Timestamp for the filename. Default is
#'   default_timestamp.
#' @param iteration Integer. Iteration number. Default is 1.
#' @param repl Integer. Replication number. Default is 1.
#' @param annotations Data frame. Optional annotations data frame.
#' @param var_imp Data frame. Optional variable importance data frame.
#' @param results Data frame. Optional results data frame.
#' @param parameters List. Optional parameters list for Arguments sheet.
#' @param n_records Integer. Number of records to generate if annotations is
#'   NULL. Default is 100.
#' @param ... Additional arguments passed to mock_annotations().
#'
#' @return Path to the created Excel file.
mock_records_file <- function(
    base_dir = local_test_dir(),
    sessions_folder = "Sessions",
    session_name = "Session1",
    timestamp = default_timestamp,
    iteration = 1,
    repl = 1,
    annotations = NULL,
    var_imp = NULL,
    results = NULL,
    parameters = list(),
    n_records = 100,
    ...
) {
  # Create directory structure
  full_path <- file.path(base_dir, sessions_folder, session_name)

  with_predictions <- iteration > 0

  # if it has predictions, it should be in the Annotations folder
  if (with_predictions) {
    full_path <- file.path(full_path, "Annotations")
  }
  dir.create(full_path, recursive = TRUE, showWarnings = FALSE)

  # Generate annotations if not provided
  if (!is.null(annotations)) {
    # Determine if the file includes predictions
    with_predictions <- all(
      c("Rev_prediction", "Rev_prediction_new") %in% names(annotations))
  } else {
    annotations <- mock_annotations(
      n = n_records,
      with_predictions = with_predictions,
      ...
    )
  }

  # Prepare sheets
  sheets <- list(Records = annotations)

  if (with_predictions) {

    repl_string <- if (repl > 1) paste0("rep", repl, "_") else ""
    filename <- sprintf(
      "%d.Records_%s%s.xlsx", iteration, repl_string, timestamp)

    # Generate any missing sheets
    if (is.null(var_imp)) {
      dtm <- mock_dtm(annotations)
      var_imp <- mock_var_imp(dtm)
    }

    if (is.null(results)) {
      if (iteration == 1) {
        parent_file <- sprintf("Records_%s.xlsx", add_minutes(timestamp, -20))
        parent_file <- file.path(sessions_folder, session_name, parent_file)
      } else {
        repl_string <- if (repl - 1 > 1) paste0("rep", repl, "_") else ""
        parent_file <- sprintf(
          "%d.Records_%s%s.xlsx", iteration - 1, repl_string, timestamp)
        parent_file <- file.path(
          sessions_folder, session_name, "Annotations", parent_file
        )
      }

      results <- mock_results(
        annotations = annotations,
        n_features = ncol(dtm) - 1,
        records_file = parent_file
      )
    }

    args <- mock_param_page(
      records_file = file.path(
        sessions_folder, session_name, filename
      ),
      pos_mult = parameters$pos_mult,
      n_models = parameters$n_models,
      resample = parameters$resample,
      pred_quants = parameters$pred_quants
    )

    # Add additional sheets
    sheets$Variable_importance <- var_imp
    sheets$Results <- results
    sheets$Arguments <- args

  } else {
    filename <- sprintf("Records_%s.xlsx", timestamp)
  }

  # Create file path and save
  file_path <- file.path(full_path, filename)
  writexl::write_xlsx(sheets, path = file_path)

  return(file_path)
}

#' Create a complete session folder structure with consistent files
#'
#' Generates a complete session folder structure with all necessary files and
#' subdirectories for testing purposes.
#'
#' @param base_dir Character. Base directory to create the sessions folder in.
#'   Default is local_test_dir().
#' @param sessions_folder Character. Folder containing the sessions. Default is
#'   "Sessions".
#' @param session_name Character. Name of the session. Default is "Session1".
#' @param records_timestamp Character. Timestamp for the original Records file.
#'   Default is default_timestamp.
#' @param max_iterations Integer. Maximum number of iterations to generate files
#'   for. Default is Inf.
#' @param max_replications Integer. Maximum number of replications per
#'   iteration. Default is 3.
#' @param n_records Integer. Number of records to generate. Default is 100.
#' @param parameters List. Parameters to use for Arguments sheets.
#' @param ... Additional arguments passed to mock_annotations().
#'
#' @return Path to the created session folder.
mock_session_folder <- function(
    base_dir = local_test_dir(),
    sessions_folder = "Sessions",
    session_name = "Session1",
    records_timestamp = default_timestamp,
    max_iterations = Inf,
    max_replications = 3,
    n_records = 100,
    parameters = list(),
    ...
) {

  # Create session folder structure
  session_dir <- file.path(base_dir, sessions_folder, session_name)
  dir.create(session_dir, recursive = TRUE, showWarnings = FALSE)

  annotations <- mock_annotations(
    n = n_records,
    with_predictions = FALSE,
    ...
  )

  # Create unprocessed Records file
  records_file <- mock_records_file(
    base_dir = base_dir,
    sessions_folder = sessions_folder,
    session_name = session_name,
    timestamp = records_timestamp,
    iteration = 0,
    annotations = annotations,
    parameters = parameters
  )

  annotations$Target <- NA

  iteration <- 1
  repl <- 1

  while (
    iteration <= max_iterations &&
    repl <= max_replications &&
    any(is.na(annotations$Target %||% NA))
  ) {

    # print(iteration)

    # Create processed folders
    samples_dir <- file.path(session_dir, "Samples")
    results_dir <- file.path(session_dir, "Results")
    dir.create(samples_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

    cur_timestamp <- add_minutes(records_timestamp, iteration * 20)

    # Generate processed data
    annotations <- mock_annotations(
      n = n_records,
      with_predictions = TRUE,
      previous_annotations = annotations,
      seed = 12345 + repl - 1,
      ...
    )

    # Generate DTM
    dtm <- mock_dtm(annotations)

    # Generate variable importance
    var_imp <- mock_var_imp(dtm)

    # Generate results
    results <- mock_results(
      annotations = annotations,
      iteration = iteration,
      repl = repl,
      parent_file = fs::path_rel(records_file, base_dir)
    )

    # Generate samples
    samples <- mock_predictive_distr(annotations = annotations)

    # Save files
    repl_string <- if (repl > 1) paste0("rep", repl, "_") else ""
    readr::write_csv(
      results,
      file.path(
        results_dir,
        sprintf("%d.Results_%s%s.csv", iteration, repl_string, cur_timestamp))
    )
    readr::write_rds(dtm, file.path(session_dir, "DTM.rds"))
    readr::write_rds(
      samples,
      file.path(
        samples_dir,
        sprintf("%d.Samples_%s%s.rds", iteration, repl_string, cur_timestamp)
      )
    )

    # Create processed Records file with all data
    records_file <- mock_records_file(
      base_dir = base_dir,
      sessions_folder = sessions_folder,
      session_name = session_name,
      timestamp = cur_timestamp,
      iteration = iteration,
      repl = repl,
      annotations = annotations,
      var_imp = var_imp,
      results = results,
      parameters = parameters
    )

    # If there are no new positives, increase the replication
    if ("y" %in% annotations$Rev_prediction_new) {
      repl <- 1
    } else repl <- repl + 1

    iteration <- iteration + 1
  }

  return(session_dir)
}

#' Test that two data frames have the same column names
#'
#' @param obs Observed data frame.
#' @param exp Expected data frame.
#'
#' @return Invisible NULL, throws error if column names don't match.
expect_same_colnames <- function(obs, exp) {
  testthat::expect_equal(sort(colnames(obs)), sort(colnames(exp)))
}

#' Test that two data frames have the same column types
#'
#' @param obs Observed data frame.
#' @param exp Expected data frame.
#'
#' @return Invisible NULL, throws error if column types don't match.
expect_same_coltypes <- function(obs, exp) {
  testthat::expect_equal(sort(colnames(obs)), sort(colnames(exp)))
}

#' Test that a search result matches expected structure
#'
#' @param results Data frame containing search results to test.
#' @param expected_struct Named vector of expected column types.
#'
#' @return Invisible NULL, throws error if expectations not met.
test_search_result <- function(results, expected_struct) {
  testthat::expect_s3_class(results, class = "data.frame")

  if (is.data.frame(results)) {
    result_struct <- purrr::map_chr(results, ~ class(.x)[1])

    testthat::expect_mapequal(result_struct, expected = expected_struct)
  }

  invisible(NULL)
}
