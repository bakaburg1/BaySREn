#' Select the best reasoning model for a given dataset and sample class
#'
#' This function selects the most appropriate reasoning model based on
#' performance metrics, prioritizing different metrics depending on the sample
#' class (FN, FP, TP).
#' For FN samples, it prioritizes recall; for FP samples, precision; for
# TP samples, F1 score.
#'
#' @param model_metrics A data frame containing model performance metrics with
#'   columns including 'dataset', 'model', 'precision', 'recall', and optionally
#'   'f1'.
#' @param dataset_name Character string specifying the dataset name to filter
#'   metrics.
#' @param sample_class Character string indicating the sample class ("FN",
# "FP",
#'   or "TP"). "TN" is also accepted and treated like "FP" (precision focus).
#'
#' @return Character string with the name of the selected model, or
#'   NA_character_ if no suitable model is found.
#'
#' @examples
#' \dontrun{
#' # Example model metrics data frame
#' metrics <- data.frame(
#'   dataset = c("dataset1", "dataset1"),
#'   model = c("model1", "model2"),
#'   precision = c(0.8, 0.9),
#'   recall = c(0.7, 0.6)
#' )
#'
#' selected <- select_reasoning_model(metrics, "dataset1", "FN")
#' }
select_reasoning_model <- function(model_metrics, dataset_name, sample_class) {
  dataset_metrics <- model_metrics |>
    dplyr::filter(.data$dataset == dataset_name)
  if (!nrow(dataset_metrics)) {
    return(NA_character_)
  }
  dataset_metrics <- dataset_metrics |>
    dplyr::mutate(
      f1 = dplyr::if_else(
        (.data$precision + .data$recall) > 0,
        2 * .data$precision * .data$recall / (.data$precision + .data$recall),
        0
      )
    )
  sample_class_chr <- as.character(sample_class)
  ordered <- switch(
    sample_class_chr,
    "FN" = dataset_metrics |>
      dplyr::arrange(dplyr::desc(.data$recall)),
    "FP" = dataset_metrics |>
      dplyr::arrange(dplyr::desc(.data$precision)),
    "TP" = dataset_metrics |>
      dplyr::arrange(dplyr::desc(.data$f1)),
    "TN" = dataset_metrics |>
      dplyr::arrange(dplyr::desc(.data$precision)),
    dataset_metrics
  )
  ordered$model[[1]] %||% NA_character_
}

#' Pick the most useful reasoning traces for a record
#'
#' Prefers the best-performing models that failed on the record for the given
#' class (FN/FP/TP), returning up to `max_reasonings` traces. If no model failed
#' for that class, it falls back to the class-specific "least wrong" model.
#' This helps the refiner see high-quality rationales for difficult cases.
#'
#' @param results Data frame of model outputs including 'dataset', 'record_key',
#'   'model', 'included', 'matches', and 'raw_matches'.
#' @param model_metrics Data frame of model-level metrics (precision, recall,
#'   etc.) as returned by \code{\link{summarise_model_metrics}}. If NULL,
#'   metrics are computed from `results`.
#' @param record_key Record identifier to select the row(s). If NULL, selection
#'   is performed on all records.
#' @param sample_class One of "FN", "FP", "TP", or "TN" indicating the failure
#'   type being examined. If NULL, traces are gathered for all classes present
#'   in the data and combined.
#' @param max_reasonings Maximum number of reasoning traces to return (default
#'   is 2).
#'
#' @return A list with elements:
#'   \item{models}{Character vector of model names used for reasoning}
#'   \item{text}{Single character string with one or more reasoning blocks
#'     separated by "---", or NA if unavailable}
#'
#' @examples
#' \dontrun{
#' pick_reasoning_trace(results, metrics, "dataset1", "record-123", "FN", 2)
#' }
#'
pick_reasoning_trace <- function(
  results,
  model_metrics = NULL,
  record_key = NULL,
  sample_class = NULL,
  max_reasonings = 2L
) {
  if (rlang::is_empty(results) || !nrow(results)) {
    return(list(models = character(), text = NA_character_))
  }

  # Compute metrics on the fly if not provided
  if (rlang::is_null(model_metrics)) {
    model_metrics <- summarise_model_metrics(results)
  }

  # Optionally narrow to a record_key; otherwise operate on all provided rows
  record_rows <- results |>
    (\(df) {
      if (!is.null(record_key)) {
        dplyr::filter(df, .data$record_key == !!record_key)
      } else {
        df
      }
    })()
  if (!nrow(record_rows)) {
    return(list(models = character(), text = NA_character_))
  }

  # Label each model's outcome to see which ones failed in this class
  record_rows <- assign_classification_classes(record_rows)
  classes_to_process <- if (rlang::is_null(sample_class)) {
    intersect(c("FN", "FP", "TP", "TN"), record_rows$class)
  } else if (all(is.na(sample_class))) {
    intersect(c("FN", "FP", "TP", "TN"), record_rows$class)
  } else {
    as.character(sample_class)
  }
  classes_to_process <- unique(stats::na.omit(classes_to_process))
  if (!length(classes_to_process)) {
    return(list(models = character(), text = NA_character_))
  }

  # Attach metrics so we can rank failing models by class-relevant skill
  metrics_ds <- model_metrics |>
    dplyr::mutate(
      f1 = dplyr::if_else(
        (.data$precision + .data$recall) > 0,
        2 * .data$precision * .data$recall / (.data$precision + .data$recall),
        0
      ),
      specificity = dplyr::if_else(
        (.data$TN + .data$FP) > 0,
        .data$TN / (.data$TN + .data$FP),
        0
      )
    )

  max_reasonings <- max(1L, as.integer(max_reasonings))

  process_class <- function(class_chr) {
    rank_column <- switch(
      class_chr,
      "FN" = "recall",
      "FP" = "precision",
      "TP" = "f1",
      "TN" = "specificity",
      "score"
    )

    failing <- record_rows |>
      dplyr::filter(.data$class == class_chr)

    # Rank failing models by the chosen metric (recall for FN, precision for FP, etc.)
    ranked_failing <- failing |>
      dplyr::left_join(
        metrics_ds,
        by = c("model", "dataset")
      ) |>
      dplyr::mutate(
        order_val = .data[[rank_column]]
      ) |>
      dplyr::arrange(
        dplyr::desc(dplyr::coalesce(.data$order_val, -Inf)),
        .data$model,
        .data$record_key
      )

    chosen <- if (nrow(ranked_failing)) {
      dplyr::slice_head(ranked_failing, n = max_reasonings)
    } else {
      # No failing models: fall back to the class-specific least-wrong model
      fallback_model <- select_reasoning_model(
        model_metrics,
        class_chr
      )
      record_rows |>
        dplyr::filter(.data$model == fallback_model) |>
        dplyr::slice_head(n = 1)
    }

    chosen_models <- chosen$model
    if (!length(chosen_models)) {
      return(list(models = character(), text = NA_character_))
    }

    # Build a human-readable multi-block string for the refiner
    blocks <- purrr::pmap_chr(
      list(
        class_chr = rep(class_chr, length.out = nrow(chosen)),
        model = chosen_models,
        text = chosen$raw_matches
      ),
      \(class_chr, model, text) {
        glue::glue(
          "<trace class=\"{class_chr}\" model=\"{model}\" ranked_by=\"{rank_column}\">\n{text %||% '(missing)'}\n</trace>"
        )
      }
    )

    list(
      models = chosen_models,
      text = paste(blocks, collapse = "\n")
    )
  }

  class_outputs <- purrr::map(classes_to_process, process_class)
  all_models <- unique(unlist(purrr::map(class_outputs, "models")))
  all_models <- all_models[!is.na(all_models)]
  texts <- purrr::map_chr(class_outputs, "text")
  texts <- texts[!is.na(texts) & nzchar(texts)]
  final_text <- if (length(texts)) {
    paste(texts, collapse = "\n\n====\n\n")
  } else {
    NA_character_
  }

  list(
    models = all_models,
    text = final_text
  )
}

#' Assign classification classes based on model predictions and ground truth
#'
#' This function assigns TP/FP/FN/TN classes to records based on their model
#' predictions (matches) and ground truth labels (included).
#'
#' @param data A data frame containing 'matches' and 'included' columns
#'
#' @return A data frame with an additional 'class' column containing "TP", "FP",
#'   "FN", or "TN"
assign_classification_classes <- function(data) {
  data |>
    dplyr::mutate(
      class = dplyr::case_when(
        .data$matches & .data$included ~ "TP",
        .data$matches & !.data$included ~ "FP",
        !.data$matches & .data$included ~ "FN",
        !.data$matches & !.data$included ~ "TN"
      )
    )
}

#' Prepare prompt refinement samples from model results
#'
#' This function prepares a balanced set of FN (false negative), FP (false
# positive),
#' and TP (true positive) samples for prompt refinement. It aggregates
# model predictions
#' by record, determines majority votes, classifies records into TP/FP/FN/TN
# categories,
#' and selects representative samples with reasoning traces from the best-performing
#' models.
#'
#' @param results A data frame containing model classification results
# with columns
#'   including 'dataset', 'record_key', 'title', 'abstract', 'authors',
# 'keywords',
#'   'included', 'matches', 'model', and 'raw_matches'.
#' @param model_metrics A data frame containing model performance metrics as
#'   returned by \code{\link{summarise_model_metrics}}.
#' @param max_fn Integer specifying the maximum number of false negative
# samples
#'   to select.
#' @param max_fp Integer specifying the maximum number of false positive
# samples
#'   to select.
#' @param max_tp Integer specifying the maximum number of true positive samples
#'   to select.
#'
#' @return A data frame with selected samples containing formatted reasoning
# traces
#'   and metadata for prompt refinement. Each row represents one sample with
#'   columns
#'   including 'dataset', 'record_key', 'sample_class', 'reasoning_model',
#'   'reasoning_trace', and text fields.
#'
#' @export
prepare_prompt_refinement_samples <- function(
  results,
  model_metrics,
  max_fn = 10,
  max_fp = 10,
  max_tp = 5
) {
  if (rlang::is_empty(results) || !nrow(results)) {
    cli::cli_abort("No results available.")
  }
  if (rlang::is_empty(model_metrics) || !nrow(model_metrics)) {
    cli::cli_abort("Model metrics are required to attach reasoning traces.")
  }
  base <- results |>
    dplyr::summarise(
      dplyr::across(
        c("title", "abstract", "authors", "keywords", "included"),
        dplyr::first
      ),
      TP_votes = sum(.data$matches & .data$included, na.rm = TRUE),
      FP_votes = sum(.data$matches & !.data$included, na.rm = TRUE),
      TN_votes = sum(!.data$matches & !.data$included, na.rm = TRUE),
      FN_votes = sum(!.data$matches & .data$included, na.rm = TRUE),
      .by = c("dataset", "record_key")
    )

  class_types <- c("FN", "FP", "TP")

  # Calculate available counts
  counts <- base |>
    dplyr::group_by(.data$dataset) |>
    dplyr::summarise(
      FN_count = sum(.data$FN_votes > 0),
      FP_count = sum(.data$FP_votes > 0),
      TP_count = sum(.data$TP_votes > 0)
    )

  # Adjust sample limits dynamically if needed
  # If we don't have enough FNs, use more FPs to fill the "error budget"
  # If we don't have enough FPs, use more FNs
  target_errors <- max_fn + max_fp

  dataset_class_samples <- expand.grid(
    dataset = unique(base$dataset),
    class = class_types,
    stringsAsFactors = FALSE
  )

  samples <- purrr::map(
    seq_len(nrow(dataset_class_samples)),
    function(i) {
      combination <- dataset_class_samples[i, ]
      ds <- combination$dataset
      cl <- combination$class

      # Get counts for this dataset
      ds_counts <- counts |> dplyr::filter(.data$dataset == ds)

      # Determine limit for this class
      limit <- if (cl == "FN") {
        # If we have plenty of FNs, take max_fn.
        # If FPs are scarce, we might want more FNs, but let's stick to the request:
        # "if there are no FN, then we can get 15 FP".
        # So if this is FN, we just take up to max_fn (or more if we want to balance).
        # Actually, the logic requested is: compensate FN/FP to sum to FN+FP.
        # Let's calculate the dynamic limit for FN first.
        # We can't easily do this inside this map without pre-calculation.
        # Let's simplify: We will just take what we can get, and then if we are processing
        # the other error class, we see what remains of the budget.
        max_fn
      } else if (cl == "FP") {
        max_fp
      } else {
        max_tp
      }

      # Pre-calculate limits to allow compensation
      if (cl %in% c("FN", "FP")) {
        fn_avail <- ds_counts$FN_count

        if (cl == "FN") {
          # If FP is low, can we take more FN?
          # The prompt says: "if there are no FN, then we can get 15 FP".
          # This implies sharing the budget (max_fn + max_fp).
          # Let's aim for max_fn, but if FP is short, we could expand.
          # However, usually we care most about FNs.
          # Let's try to fill the FN quota first.
          limit <- max_fn
          # If we have fewer FNs than limit, we will just take all of them.
          # The compensation happens in the FP step.
        } else if (cl == "FP") {
          # How many FNs did we actually get?
          fn_taken <- min(max_fn, fn_avail)
          # How much budget is left?
          budget_left <- target_errors - fn_taken
          # Take up to budget_left FPs
          limit <- budget_left
        }
      }

      class_votes_col <- paste0(cl, "_votes")
      filtered <- base |>
        dplyr::filter(
          .data$dataset == ds,
          .data[[class_votes_col]] > 0
        )

      if (!nrow(filtered)) {
        return(NULL)
      }

      filtered |>
        dplyr::arrange(
          dplyr::desc(.data[[class_votes_col]]),
          .data$record_key
        ) |>
        dplyr::slice_head(n = limit) |>
        dplyr::mutate(
          sample_class = cl,
          .after = "dataset"
        )
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      dplyr::across(
        c("title", "abstract", "authors", "keywords"),
        ~ tidyr::replace_na(.x, "")
      )
    )
  reasoning_lookup <- results |>
    dplyr::select(
      "dataset",
      "record_key",
      "model",
      "included",
      "matches",
      "raw_matches"
    )
  samples |>
    dplyr::rowwise() |>
    dplyr::mutate(
      reasoning_info = list(
        pick_reasoning_trace(
          results = reasoning_lookup,
          model_metrics = model_metrics,
          dataset = dataset,
          record_key = record_key,
          sample_class = sample_class,
          max_reasonings = 2L
        )
      ),
      reasoning_model = {
        models_vec <- reasoning_info$models
        if (length(models_vec)) {
          paste(models_vec, collapse = "; ")
        } else {
          NA_character_
        }
      },
      reasoning_trace = reasoning_info$text %||% NA_character_
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-reasoning_info)
}

#' Format refinement samples for LLM prompt input
#'
#' This function formats the selected refinement samples into a structured text
#' format suitable for inclusion in LLM prompts. Samples are grouped by
# dataset and
#' sample class, with each sample containing title, keywords, correct label,
# model
#' vote summary, reference model reasoning, and abstract.
#'
#' @param samples A data frame returned by \code{\link{prepare_prompt_refinement_samples}}
#'   containing the samples to format.
#'
#' @return Character string containing the formatted samples, grouped by
# dataset and
#'   class, with separators between sections. Returns "No refinement samples
#'   available." if no
#'   samples are provided.
#'
#' @export
format_samples_for_llm <- function(samples) {
  if (rlang::is_empty(samples) || !nrow(samples)) {
    return("No refinement samples available.")
  }
  class_order <- c("FN", "FP", "TP")
  samples |>
    dplyr::mutate(
      sample_class = factor(
        .data$sample_class,
        levels = class_order,
        ordered = TRUE
      ),
      formatted = stringr::str_glue(
        "Title: {.data$title}
Keywords: {.data$keywords}
Correct label: {ifelse(.data$included, 'relevant', 'not relevant')}
Model votes: {.data$TN_votes} TN, {.data$TP_votes} TP, {.data$FN_votes} FN, {.data$FP_votes} FP
Reference model (least wrong for this class): {.data$reasoning_model %||% '(missing)'}
Reference reasoning:\n{stringr::str_trunc(.data$reasoning_trace %||% '(missing)', 600)}
Abstract: {.data$abstract}"
      )
    ) |>
    dplyr::arrange(.data$dataset, .data$sample_class) |>
    dplyr::group_by(.data$dataset, .data$sample_class) |>
    dplyr::summarise(
      block = paste(.data$formatted, collapse = "\n\n---\n\n"),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      header = glue::glue(
        "Dataset: {.data$dataset}\nClass: {.data$sample_class}"
      )
    ) |>
    dplyr::mutate(
      section = paste(
        .data$header,
        .data$block,
        "\n\n=============================\n\n"
      )
    ) |>
    dplyr::pull(.data$section) |>
    paste(collapse = "\n\n")
}

#' Summarize model performance metrics across datasets
#'
#' This function computes comprehensive performance metrics (TP, FP, TN, FN,
#' precision, recall, score) for each model/dataset combination. The score
# represents
#' the Euclidean
#' distance from perfect precision and recall (lower is better).
#'
#' @param results A data frame containing classification results with columns
#'   including 'model', 'dataset', 'included' (ground truth), 'matches'
#'   (prediction), and token usage columns.
#'
#' @return A data frame with one row per model/dataset combination containing:
#'   \item{TP}{True positives count}
#'   \item{FP}{False positives count}
#'   \item{TN}{True negatives count}
#'   \item{FN}{False negatives count}
#'   \item{total_rows}{Total number of records}
#'   \item{prompt_tokens}{Total prompt tokens used}
#'   \item{completion_tokens}{Total completion tokens used}
#'   \item{precision}{Precision score (TP/(TP+FP))}
#'   \item{recall}{Recall score (TP/(TP+FN))}
#'   \item{score}{Composite score measuring distance from perfect performance}
#'
#' @export
summarise_model_metrics <- function(
  results
) {
  if (rlang::is_empty(results) || !nrow(results)) {
    return(tibble::tibble())
  }

  results |>
    dplyr::summarise(
      TP = sum(.data$included & .data$matches, na.rm = TRUE),
      FP = sum(!.data$included & .data$matches, na.rm = TRUE),
      TN = sum(!.data$included & !.data$matches, na.rm = TRUE),
      FN = sum(.data$included & !.data$matches, na.rm = TRUE),
      total_rows = dplyr::n(),
      prompt_tokens = sum(.data$prompt_tokens, na.rm = TRUE),
      completion_tokens = sum(.data$completion_tokens, na.rm = TRUE),
      precision = .data$TP / (.data$TP + .data$FP),
      recall = .data$TP / (.data$TP + .data$FN),
      score = sqrt((1 - .data$recall)^2 + (1 - .data$precision)^2),
      .by = c("model", "dataset")
    ) |>
    dplyr::arrange(.data$score)
}

#' Format model metrics for display in prompts and logs
#'
#' This function converts a metrics data frame into a human-readable text
# format
#' suitable for inclusion in LLM prompts or logging output. Each model's
#' performance is summarized in a single line with key statistics.
#'
#' @param metrics A data frame containing model metrics as returned by
#'   \code{\link{summarise_model_metrics}}.
#'
#' @return Character string containing formatted metrics, one line per
#'   model/dataset combination. Returns "No metrics available." if no metrics
# are
#'   provided.
#'
#' @export
format_metrics_block <- function(metrics) {
  if (rlang::is_empty(metrics) || !nrow(metrics)) {
    return("No metrics available.")
  }
  metrics |>
    dplyr::mutate(
      summary = sprintf(
        "%s / %s => precision %.3f | recall %.3f | TP %d | FP %d | TN %d | FN %d",
        .data$model,
        .data$dataset,
        .data$precision,
        .data$recall,
        .data$TP,
        .data$FP,
        .data$TN,
        .data$FN
      )
    ) |>
    dplyr::pull(.data$summary) |>
    paste(collapse = "\n")
}

#' Send a chat prompt with retry logic and error handling
#'
#' This function sends a prompt to a chat model with built-in retry logic
# for reliability.
#' It uses parallel chat promises with rate limiting and exponential backoff.
#'
#' @param chat An ellmer chat object configured for the target LLM.
#' @param prompt Character string containing the prompt to send.
#'
#' @return Character string containing the model's response, or NULL if the
#'   request fails after retries.
#'
#' @examples
#' \dontrun{
#' chat <- ellmer::chat_openrouter(model = "openai/gpt-4")
#' response <- chat_prompt_with_retry(chat, "Hello, world!")
#' }
chat_prompt_with_retry <- function(chat, prompt) {
  chats <- tryCatch(
    parallel_chat_promises(
      chat = chat,
      prompts = list(prompt), # Must be a list, not a string
      max_active = 1L,
      rpm = 60,
      cache_dir = NULL,
      backoff_base = 5,
      backoff_cap = 60,
      halve_rpm_on_retry = TRUE
    ),
    error = function(...) NULL
  )
  if (rlang::is_empty(chats)) {
    return(NULL)
  }
  convo <- chats[[1]]
  turns <- convo$get_turns(include_system_prompt = TRUE)
  if (!length(turns)) {
    return(NULL)
  }
  chat$set_turns(turns)
  tryCatch(convo$last_turn()@text, error = function(...) NULL)
}

#' Extract JSON object from text response
#'
#' This function attempts to extract and parse a JSON object from a text #
#' string, typically an LLM response. It searches for JSON-like content between
#' curly braces and attempts to parse it.
#'
#' @param txt Character string potentially containing JSON content.
#'
#' @return Parsed JSON object (list/vector) if successful, NULL if extraction #
#'   or parsing fails.
#'
#' @examples
#' \dontrun{
#' text <- 'Here is some text {"key": "value"} and more text'
#' json_obj <- extract_json_object(text)
#' # Returns: list(key = "value")
#' }
extract_json_object <- function(txt) {
  if (is.null(txt)) {
    return(NULL)
  }
  block <- stringr::str_extract(txt, stringr::regex("\\{.*\\}", dotall = TRUE))
  if (rlang::is_na(block) || !nzchar(block)) {
    return(NULL)
  }
  tryCatch(
    jsonlite::fromJSON(block, simplifyVector = TRUE),
    error = function(e) {
      cli::cli_warn(
        c(
          "extract_json_object failed to parse JSON block.",
          "Error" = conditionMessage(e),
          "Block snippet" = stringr::str_trunc(block, 200)
        )
      )
      NULL
    }
  )
}

#' Evaluate inclusion/exclusion criteria on a dataset slice
#'
#' This function evaluates given inclusion/exclusion criteria on a subset of
#' records using a specified LLM model. It returns predictions for the provided
#' rows; metrics can be computed by the caller after merging the results back
#' into the full dataset.
#'
#' @param data A data frame containing the records to evaluate with columns
#'   including 'title', 'abstract', 'authors', 'keywords', 'included'
#'   (ground truth), and 'record_key'.
#' @param include_text Character string containing the inclusion criteria text.
#' @param exclude_text Character string containing the exclusion criteria text,
#'   or NULL/empty for no exclusion criteria.
#' @param store_path Character string specifying the path for caching results.
#' @param eval_model Character string specifying the LLM model to use for
#'   evaluation.
#' @param eval_args List of additional arguments to pass to the LLM API.
#'
#' @return A data frame with classification results for the provided records.
#'
#' @export
evaluate_criteria <- function(
  data,
  include_text,
  exclude_text = "",
  store_path,
  eval_model = "openai/gpt-oss-20b",
  eval_args = list(reasoning = list(effort = "high"))
) {
  if (rlang::is_empty(data) || !nrow(data)) {
    return(dplyr::tibble())
  }

  # Use smaller batch size for reliability
  batch_size <- min(50, nrow(data))

  chat_eval <- ellmer::chat_openrouter(
    model = eval_model,
    api_args = eval_args,
    echo = "none"
  )
  max_attempts <- 10L
  result <- NULL
  for (attempt in seq_len(max_attempts)) {
    if (attempt > 1) {
      cli::cli_alert_info(
        "Retrying evaluation attempt {attempt}/{max_attempts} for model {.field {eval_model}}"
      )
    }
    result <- try(
      classify_citations(
        data = data |>
          dplyr::select("title", "abstract", "authors", "keywords"),
        chat = chat_eval,
        query = list(include = include_text, exclude = exclude_text),
        cache_dir = here::here(store_path, "criteria_refiner_eval"),
        max_batch_size = batch_size, # Smaller batch size for reliability
        system_prompt_template = default_system_prompt_template()
      ),
      silent = TRUE
    )
    if (!inherits(result, "try-error")) {
      break
    }
    cli::cli_warn(
      "Evaluation attempt {attempt}/{max_attempts} failed for model {.field {eval_model}}: {conditionMessage(attr(result, 'condition'))}"
    )
    Sys.sleep(2)
  }

  if (inherits(result, "try-error")) {
    cli::cli_abort(
      "Evaluation failed after {max_attempts} attempts for model {.field {eval_model}}"
    )
  }

  if (nrow(result) != nrow(data)) {
    cli::cli_warn(
      "Evaluation returned {nrow(result)} rows but expected {nrow(data)} for model {.field {eval_model}}"
    )
  }

  result |>
    dplyr::mutate(
      included = data$included,
      dataset = data$dataset,
      record_key = data$record_key,
      model = eval_model,
      .before = 1
    )
}

#' Summarize the history of criteria refinement iterations
#'
#' This function formats the history of refinement iterations into a readable
#' text summary, showing the evolution of criteria and performance metrics
#' across iterations.
#'
#' @param history A list containing refinement history as returned by
#'   \code{\link{run_criteria_refiner}}, with each element representing # one
#'   iteration.
#'
#' @return Character string containing the formatted history summary, with one
#'   section per iteration showing notes and metrics. Returns "No previous #
#'   iterations." if history is empty.
#'
#' @export
summarise_refiner_history <- function(history) {
  if (!length(history)) {
    return("No previous iterations.")
  }
  history |>
    purrr::map_chr(\(entry) {
      metrics_block <- format_metrics_block(entry$metrics)
      glue::glue(
        "Iteration {entry$iteration}: {entry$notes %||% '(no notes)'}\n",
        "Metrics:\n{metrics_block}"
      )
    }) |>
    paste(collapse = "\n\n")
}

#' Select refinement samples based on classification performance
#'
#' This function selects records for manual review and criteria refinement #
#' based on their classification performance. It handles both initial seeding #
#' (when all requests are NULL) and iterative sampling based on LLM requests.
#' Records are selected from different classes (FN, FP, TP, TN) using various
#' ordering strategies to maximize learning value.
#'
#' @param current_dataset A data frame with classification results containing #
#'   columns including 'record_key', 'class' (TP/FP/FN/TN), and vote counts.
#' @param fn Named numeric vector specifying false negative sampling: name is
#'   order ("asc", "desc", or "sample"), value is count, e.g. c(desc = 10). NULL
#'   means no FN sampling.
#' @param fp Named numeric vector for false positive sampling, same format # as
#'   fn.
#' @param tp Named numeric vector for true positive sampling, same format # as
#'   fn.
#' @param tn Named numeric vector for true negative sampling, same format # as
#'   fn.
#'
#' @return A list containing:
#'   \item{selected_ids}{Character vector of selected record keys}
#'   \item{selection_table}{Data frame with detailed selection information}
#'   \item{notes}{Character string describing the selection process and
# any limitations}
#'
#' @details
#' Ordering options:
#' \describe{
#'   \item{"desc"}{High concordance first (models agree strongly)}
#'   \item{"asc"}{Low concordance first (models disagree - most informative)}
#'   \item{"sample"}{Random sampling}
#' }
#'
#' @export
select_refinement_samples <- function(
  current_dataset,
  fn = c("desc" = 10),
  fp = c("desc" = 5),
  tp = c("desc" = 5),
  tn = 0
) {
  classes <- c("fn", "fp", "tp", "tn")

  # Assign defaults from the signature if any of fn, fp, tp, tn are NULL
  defaults <- formals(select_refinement_samples)
  for (cls in classes) {
    val <- get(cls)
    if (is.null(val)) {
      assign(cls, eval(defaults[[cls]]))
    }
  }

  # Check for invalid sampling requests
  faulty_classes <- purrr::keep(classes, \(cls) {
    val <- get(cls)
    !rlang::is_scalar_vector(val) ||
      !rlang::is_integerish(val, finite = TRUE)
  })

  if (length(faulty_classes) > 0) {
    cli::cli_abort(
      "Invalid sampling requests: {.var {faulty_classes}}
      must be a single integer."
    )
  }

  if (rlang::is_empty(current_dataset) || !nrow(current_dataset)) {
    cli::cli_abort("No records available for sampling.")
  }

  # Ensure we only select samples with valid labels
  if (
    !"included" %in% names(current_dataset) ||
      !"matches" %in% names(current_dataset)
  ) {
    cli::cli_abort("Dataset must include 'included' and 'matches' columns.")
  }

  current_dataset <- current_dataset |>
    dplyr::filter(!is.na(.data$included) & !is.na(.data$matches))

  if (nrow(current_dataset) == 0) {
    cli::cli_abort(
      "No records with valid human or LLM classification decisions found."
    )
  }

  # Add class labels for sampling
  current_dataset <- assign_classification_classes(current_dataset)

  # Build request list (using normalized values)
  requests <- list(
    "FN" = fn,
    "FP" = fp,
    "TP" = tp,
    "TN" = tn
  )

  # Filter out requests with zero count
  requests <- purrr::keep(requests, ~ .x > 0)

  if (length(requests) == 0) {
    cli::cli_abort("No sampling requests provided.")
  }

  # Process each request
  selections <- purrr::imap(requests, function(req, class_name) {
    count <- as.numeric(req)
    order <- names(req)

    # Default order to 'desc' if not provided
    if (rlang::is_empty(order)) {
      order <- "desc"
    }

    # Check if order is valid
    if (!order %in% c("asc", "desc", "sample")) {
      cli::cli_abort(
        "Invalid order: {.var {order}} must be
      'asc', 'desc', or 'sample'."
      )
    }

    # Filter by class
    class_filtered <- current_dataset |>
      dplyr::filter(.data$class == class_name)

    if (!nrow(class_filtered)) {
      return(NULL)
    }

    # Compute concordance
    class_filtered <- class_filtered |>
      dplyr::mutate(
        concordance = dplyr::case_when(
          # Negative votes are the sum of TN and FN votes
          class_name %in% c("FN", "TN") ~ .data$TN_votes + .data$FN_votes,
          # Positive votes are the sum of TP and FP votes
          class_name %in% c("FP", "TP") ~ .data$TP_votes + .data$FP_votes
        )
      )

    # Apply ordering
    if (order == "asc") {
      ordered <- class_filtered |>
        dplyr::arrange(.data$concordance, .data$record_key)
    } else if (order == "desc") {
      ordered <- class_filtered |>
        dplyr::arrange(dplyr::desc(.data$concordance), .data$record_key)
    } else if (order == "sample") {
      ordered <- class_filtered |> dplyr::slice_sample(n = nrow(class_filtered))
    }

    # Select requested count
    selected <- ordered |> dplyr::slice_head(n = count)

    # Add class and order info
    selected |>
      dplyr::mutate(
        requested_class = class_name,
        requested_order = order,
        requested_count = count
      )
  })

  # Combine selections and remove duplicates
  all_selected <- dplyr::bind_rows(selections)

  if (rlang::is_empty(all_selected) || !nrow(all_selected)) {
    empty_selection <- tibble::tibble(
      dataset = character(),
      record_key = character(),
      class = character(),
      order = character(),
      count = integer(),
      concordance = integer(),
      TP_votes = integer(),
      FP_votes = integer(),
      TN_votes = integer(),
      FN_votes = integer()
    )
    return(
      list(
        selected_ids = character(),
        selection_table = empty_selection,
        notes = "No records matched the requested sampling classes"
      )
    )
  }

  all_selected <- all_selected |>
    dplyr::distinct(.data$record_key, .keep_all = TRUE)

  # Check for truncation
  total_requested <- sum(purrr::map_dbl(requests, as.numeric))
  total_selected <- nrow(all_selected)
  notes <- if (total_selected < total_requested) {
    sprintf(
      "Requested %d records, selected %d (duplicates removed)",
      total_requested,
      total_selected
    )
  } else {
    sprintf("Selected %d records as requested", total_selected)
  }

  # Create selection table
  selection_table <- all_selected |>
    dplyr::select(
      "dataset",
      "record_key",
      "requested_class",
      "requested_order",
      "requested_count",
      "concordance",
      "TP_votes",
      "FP_votes",
      "TN_votes",
      "FN_votes"
    ) |>
    dplyr::rename(
      class = "requested_class",
      order = "requested_order",
      count = "requested_count"
    )

  list(
    selected_ids = all_selected$record_key,
    selection_table = selection_table,
    notes = notes
  )
}

#' Prepare structured prompt for the criteria refinement LLM
#'
#' This function constructs a comprehensive prompt for the LLM criteria
# refiner,
#' including current criteria, performance metrics with deltas, cumulative
#' evaluation history, and newly sampled records requiring review. The prompt
#' guides the LLM to optimize inclusion/exclusion criteria for perfect
# recall while
#' minimizing human screening burden.
#'
#' @param dataset_name Character string specifying the name of the dataset
# being
#'   refined.
#' @param current_include Character string containing the current inclusion
#'   criteria.
#' @param current_exclude Character string containing the current exclusion
# criteria.
#' @param metrics_current Data frame with current model metrics including
# delta columns
#'   showing changes from the previous iteration.
#' @param refinement_table Data frame containing the cumulative evaluation
# history
#'   with columns including 'display_id', 'title', 'human_label', vote
# counts, etc.
#' @param new_records Data frame containing sampled records for this iteration
#'   with abstracts and baseline vote information.
#' @param iteration Integer specifying the current iteration number.
#' @param max_new Integer specifying the maximum number of sampled records
#'   allowed per iteration (for budget guidance).
#' @param total_iterations Integer specifying the total planned iterations for
#'   the refinement run.
#'
#' @return Character string containing the complete structured prompt for
# the LLM refiner.
#'
#' @details
#' The prompt includes sections for:
#' \itemize{
#'   \item Current criteria and goals (100\% recall, minimal human workload)
#'   \item Performance metrics with iteration deltas
#'   \item Cumulative evaluation table (previously reviewed records)
#'   \item Sampled records requiring evaluation
#'   \item Instructions for criteria modification and sampling requests
#' }
#'
#' @export
prepare_refiner_prompt <- function(
  dataset_name,
  current_include,
  current_exclude,
  metrics_current,
  refinement_table,
  new_records,
  iteration,
  max_new,
  total_iterations
) {
  iterations_left <- max(total_iterations - iteration, 0L)

  include_cumulative <- iteration >= 2

  # Format metrics table with deltas
  metrics_text <- if (
    !rlang::is_empty(metrics_current) && nrow(metrics_current)
  ) {
    metrics_current |>
      dplyr::mutate(
        formatted = sprintf(
          "%s: P=%.3f (Δ%.3f), R=%.3f (Δ%.3f), FP=%d (Δ%d), TP=%d (Δ%d), TN=%d (Δ%d), FN=%d (Δ%d)",
          .data$model,
          .data$precision,
          .data$delta_precision,
          .data$recall,
          .data$delta_recall,
          .data$FP,
          .data$delta_fp,
          .data$TP,
          .data$delta_tp,
          .data$TN,
          .data$delta_tn,
          .data$FN,
          .data$delta_fn
        )
      ) |>
      dplyr::pull(.data$formatted) |>
      paste(collapse = "\n")
  } else {
    "No metrics available."
  }

  # Format refinement table (cumulative, no abstracts)
  refinement_text <- if (include_cumulative) {
    if (!rlang::is_empty(refinement_table) && nrow(refinement_table)) {
      table_rows <- refinement_table |>
        dplyr::mutate(
          status_label = ifelse(
            .data$last_iteration_seen == iteration,
            "new",
            paste0("last seen: iter ", .data$last_iteration_seen)
          ),
          warnings_label = dplyr::case_when(
            .data$times_flagged > 0 & nzchar(.data$warning_reasons) ~ paste0(
              .data$times_flagged,
              " (",
              .data$warning_reasons,
              ")"
            ),
            .data$times_flagged > 0 ~ as.character(.data$times_flagged),
            TRUE ~ "0"
          ),
          reevaluation_label = ifelse(
            .data$reevaluated,
            "reevaluated in this iteration",
            "not reevaluated (criteria unchanged)"
          ),
          row_text = stringr::str_glue(
            "ID {.data$display_id}: '{substr(.data$title, 1, 60)}'
Human: {.data$human_label} | Votes: TP{.data$TP_votes} FP{.data$FP_votes} TN{.data$TN_votes} FN{.data$FN_votes}
Model votes: {.data$previous_model_votes %||% '(n/a)'} -> {.data$latest_model_votes %||% '(n/a)'} [{reevaluation_label}]
Warnings: {warnings_label}
Status: {status_label}"
          )
        ) |>
        dplyr::pull(.data$row_text) |>
        paste(collapse = "\n\n")
      sprintf(
        "Cumulative evaluated records (%d total):\n%s",
        nrow(refinement_table),
        table_rows
      )
    } else {
      "No previously evaluated records."
    }
  } else {
    ""
  }

  # Format sampled records section (with abstracts and reasoning traces)
  sampled_records_text <- if (
    !rlang::is_empty(new_records) && nrow(new_records)
  ) {
    record_blocks <- new_records |>
      dplyr::mutate(
        reasoning_trace = reasoning_trace %||% "(not available)",
        reevaluation_label = ifelse(
          reevaluated,
          "reevaluated in this iteration",
          "not reevaluated (criteria unchanged)"
        ),
        block = glue::glue(
          "ID {display_id}: '{title}'
Human label: {human_label}
Baseline votes: {TP_votes} TP, {FP_votes} FP, {TN_votes} TN, {FN_votes} FN
Model votes: {previous_model_votes %||% '(n/a)'} -> {latest_model_votes %||% '(n/a)'} [{reevaluation_label}]

<model_justification description=\"strongest failing models for this class\">

{reasoning_trace}

</model_justification>

<abstract>

{abstract}

</abstract>"
        )
      ) |>
      dplyr::pull(.data$block) |>
      paste(collapse = "\n\n---\n\n")
    sprintf(
      "Sampled records to evaluate (%d):\n\n%s",
      nrow(new_records),
      record_blocks
    )
  } else {
    "No sampled records this iteration."
  }

  cumulative_section <- if (include_cumulative) {
    glue::glue(
      "<cumulative_evaluation_table>
{refinement_text}
</cumulative_evaluation_table>

"
    )
  } else {
    ""
  }

  # Build complete prompt (XML tags help the refiner locate sections)
  glue::glue(
    "<goals>
- Achieve 100% recall: every relevant study must be found; if you believe a human label is wrong (i.e. strongly contradictory to the criteria), flag it via warn_ids (see below) instead of relaxing criteria.
- When forced to trade off, always sacrifice precision to protect recall; never accept lower recall in exchange for higher precision.
- Minimize human workload: lower FP counts reduce screening burden; keep FPs low while never reducing recall to do so; in practice we would like humans to manually review at most a few hundred records in total (ideally fewer), not thousands.
- Cost awareness: request at most {max_new} sampled records per iteration; fewer is better.
- This is an iterative process: you are at iteration {iteration} of {total_iterations}; you have {iterations_left} iterations remain—balance exploration and exploitation to reach acceptable criteria by the end.
</goals>

<current_criteria>
<include>
{current_include}
</include>

<exclude>
{current_exclude}
</exclude>
</current_criteria>

<performance_metrics description=\"Current perfomance metrics on the full dataset after revaluating FP+FN+TP records plus previously sampled records if iteration > 1 and criteria changed; deltas are changes vs the previous iteration (first iteration deltas are 0). P = precision, R = recall, FP = false positives, TP = true positives, TN = true negatives, FN = false negatives.\">
{metrics_text}
</performance_metrics>

{cumulative_section}

<sampled_records>
{sampled_records_text}
</sampled_records>

<instructions>
1) Treat recall as strictly dominant over precision: when in doubt, broaden criteria or include borderline concepts so that no relevant study is missed, even if this increases FPs.
2) Metrics cover the entire dataset. The records shown above are a small illustrative subset: never tailor criteria just to fit them. Assume many similar unseen studies exist and write rules that would work robustly for them.
3) While recall is < 1.0, focus sampling on error-prone and uncertain regions: prioritize FN-class candidates and low-concordance records (order \"asc\"), and request only small numbers of TP/TN, especially TN (reserve TN sampling for later iterations when recall is already high).
4) When available, compare criteria against the cumulative table to spot patterns the rules miss.
5) Study sampled records (titles + abstracts), the baseline votes, and the provided reasoning traces (taken from the strongest failing models for that class) to understand failure reasons and ambiguity.
6) Update inclusion and exclusion criteria to fix recall gaps and reduce FPs without harming recall; be explicit and concise. If you do not want to change a section, set it to the literal string \"unchanged\"—the system will keep the previous text and skip re-evaluating models.
7) If criteria change, all current FN/FP/TP records plus all previously sampled records are re-evaluated; metrics are recomputed on the full dataset after overwriting baseline votes for those records. Only the newly sampled records are shown above (to keep context small).
8) Use warn_ids only for clear human mislabeling: list display_ids in warn_ids with a 1–2 sentence reason when the human label obviously contradicts the intended criteria. Do not use warn_ids to avoid fixing genuine recall problems.
9) Request up to {max_new} sampled records using sampling orders asc (low concordance/disagreement), desc (high concordance/agreement), or sample (random). If unsure, bias sampling toward fn asc and fp asc.
10) Stop only when recall is perfect and the implied human workload is acceptable: humans should not need to manually review more than a few hundred candidate records; otherwise continue and set stop=false (see <response_format>).
</instructions>

<response_format>
Return only a JSON string exactly in this shape, with no extra text, XML, markdown, or backticks before or after it. Here is the shape:
{{
  \"include\": \"updated inclusion criteria\",
  \"exclude\": \"updated exclusion criteria\",
  \"reasoning\": \"explanation of changes and what you learned\",
  \"sampling\": {{
    \"fn\": {{\"order\": \"desc\", \"count\": 5}},
    \"fp\": {{\"order\": \"asc\", \"count\": 3}},
    \"tp\": {{\"order\": \"desc\", \"count\": 0}},
    \"tn\": {{\"order\": \"sample\", \"count\": 2}}
  }},
  \"warn_ids\": [
    {{\"id\": 1, \"reason\": \"short reason (max 1-2 sentences)\"}},
    {{\"id\": 7, \"reason\": \"short reason (max 1-2 sentences)\"}}
  ],
  \"stop\": false
}}
Notes: Set count to 0 or omit a class to skip it. Total requested must not exceed {max_new}. Use \"asc\" for low concordance, \"desc\" for high concordance, \"sample\" for random. Set include or exclude to the literal string \"unchanged\" if you do not want to modify that section; when both are \"unchanged\" and stop=false, the system will reuse the previous evaluation and sample without re-running evaluator models. Do not add any extra fields or change field types.
</response_format>"
  )
}

#' Run automated criteria refinement using LLM guidance
#'
#' This function implements an iterative process to optimize inclusion/exclusion
#' criteria for systematic review screening. It uses an LLM "refiner" to
# analyze
#' performance, suggest criteria modifications, and request specific samples
# for
#' manual review. The process aims for perfect recall (no missed relevant
# studies)
#' while minimizing human screening burden.
#'
#' @param base_criteria A list with 'include' and 'exclude' elements containing
#'   the initial inclusion and exclusion criteria text.
#' @param dataset_results A data frame containing baseline classification
# results for the dataset, with columns including 'dataset', 'record_key',
# 'included', 'matches', etc.
#' @param dataset_metrics A data frame with baseline model performance metrics
#'   as returned by \code{\link{summarise_model_metrics}}.
#' @param store_path Character string specifying the directory path for caching
#'   evaluation results.
#' @param max_iterations Integer specifying the maximum number of refinement
#'   iterations to run.
#' @param eval_models Character vector of LLM model names to use for criteria
#'   evaluation during refinement.
#' @param refiner_model Character string specifying the LLM model to use as
#'   the "refiner" (decision maker).
#' @param eval_args List of API arguments for evaluation models.
#' @param initial_sample List specifying the initial sampling counts per class
#'   (fn, fp, tp, tn) used in the first iteration when no LLM request exists.
#'
#' @return A list containing refinement results:
#'   \item{dataset}{Character string with the dataset name}
#'   \item{history}{List of iteration results with criteria, metrics, and
#'   decisions}
#'   \item{final}{List with final 'include' and 'exclude' criteria}
#'   \item{cumulative_evaluation}{List with evaluation state and metadata}
#'
#' @details
#' The refinement process works as follows:
#' \enumerate{
#'   \item Start with initial criteria and baseline performance
#'   \item Sample records for manual review (initially random, then LLM-guided)
#'   \item Re-evaluate sampled records with current criteria
#'   \item Present performance data to LLM refiner
#'   \item LLM suggests new criteria and requests specific samples
#'   \item Repeat until stopping criteria met (perfect recall + acceptable
# precision)
#' }
#'
#' The process includes recall protection - if recall drops between iterations,
#' the system forces continuation and warns the LLM.
#'
#' @export
run_criteria_refiner <- function(
  base_criteria,
  dataset_results,
  dataset_metrics,
  store_path,
  max_iterations = 20L,
  eval_models = c("openai/gpt-oss-120b"),
  refiner_model = "x-ai/grok-4.1-fast",
  eval_args = list(reasoning = list(effort = "high")),
  initial_sample = list(fn = 10, fp = 10, tp = 5, tn = 0)
) {
  if (rlang::is_empty(dataset_results) || !nrow(dataset_results)) {
    cli::cli_abort("Criteria refiner requires dataset-level results.")
  }
  dataset_names <- unique(dataset_results$dataset)
  if (length(dataset_names) != 1) {
    cli::cli_abort("Provide results for a single dataset per refinement run.")
  }
  dataset_name <- dataset_names[[1]]

  cli::cli_alert(
    "Starting criteria refinement for dataset {.field {dataset_name}}"
  )
  cli::cli_alert_info(
    "Refiner model: {.field {refiner_model}}"
  )
  cli::cli_alert_info(
    "Evaluation models: {.field {eval_models}}"
  )
  cli::cli_alert_info(
    "Maximum iterations: {.val {max_iterations}}"
  )

  # Initialize state
  current_include <- base_criteria$include
  current_exclude <- base_criteria$exclude %||% ""
  history <- list()

  # State for cumulative evaluation
  cumulative_sample_ids <- character()
  display_id_map <- list() # record_key -> display_id
  flagged_outliers <- list() # record_key -> times_flagged
  flagged_reasons <- list() # record_key -> character vector of reasons
  last_seen_iteration <- list() # record_key -> last iteration seen in prompt
  cumulative_evaluations <- NULL # cache last evaluation results
  current_dataset <- dataset_results # Start with baseline

  cli::cli_alert("Calculating baseline vote statistics...")
  # Calculate baseline vote stats
  vote_stats <- dataset_results |>
    dplyr::group_by(.data$record_key) |>
    dplyr::summarise(
      TP_votes = sum(.data$matches & .data$included, na.rm = TRUE),
      FP_votes = sum(.data$matches & !.data$included, na.rm = TRUE),
      TN_votes = sum(!.data$matches & !.data$included, na.rm = TRUE),
      FN_votes = sum(!.data$matches & .data$included, na.rm = TRUE),
      .groups = "drop"
    )

  current_dataset <- current_dataset |>
    dplyr::left_join(vote_stats, by = "record_key")

  cli::cli_alert_info("Initializing refiner LLM...")
  # Initialize refiner LLM
  refiner_chat <- ellmer::chat_openrouter(
    model = refiner_model,
    api_args = list(reasoning = list(effort = "high")),
    echo = "none"
  )
  refiner_chat$set_system_prompt(
    "You are an AI acting better than a human senior systematic review methodologist optimizing criteria for LLM-based screening. Your goals: 100% Recall (mandatory) while minimizing human workload (keep FPs low). You can request up to 40 sampled records per step from specific classes using asc (low concordance), desc (high concordance), or sample ordering. Flag suspected mislabels instead of overfitting criteria."
  )

  # Summarises the sampling request, producing a concise human-readable summary
  # string.
  # - request: List of sampling requests for each class (e.g. fn, fp, tp, tn).
  #   Each can specify an 'order' (asc/desc) and a 'count'.
  # Returns a string summarising the sampling request for each class.
  summarise_sampling_request <- function(request) {
    # Handle empty or missing sampling request
    if (rlang::is_empty(request)) {
      return("defaults (function limits)")
    }

    # Build summary for each class in the request
    parts <- purrr::imap(request, \(entry, cls) {
      # Omit empty entries
      if (rlang::is_empty(entry)) {
        return(NULL)
      }
      # Extract sampling order and count, set sensible defaults if missing
      order <- entry$order %||% "desc"
      count <- entry$count %||% 0
      # Human-readable string for this class
      sprintf("%s=%s (%s)", cls, count, order)
    }) |>
      purrr::compact() # Remove NULLs from the parts

    # Combine summary parts or provide fallback message if none
    if (!length(parts)) {
      "no classes requested"
    } else {
      paste(parts, collapse = "; ")
    }
  }

  # Build the initial sampling request from user-specified counts (defaults to
  # fn=10, fp=10, tp=5, tn=0) with desc ordering. Supports legacy max_* names.
  defaults_init <- list(fn = 10L, fp = 10L, tp = 5L, tn = 0L)
  counts_init <- purrr::imap(defaults_init, function(.x, nm) {
    val <- initial_sample[[nm]] %||%
      initial_sample[[paste0("max_", nm)]] %||%
      .x
    as.integer(val)
  })
  initial_sampling_request <- purrr::imap(counts_init, function(count, nm) {
    if (is.na(count) || count <= 0) {
      return(NULL)
    }
    list(order = "desc", count = count)
  }) |>
    purrr::compact()

  # Normalize warn_ids into a tidy tibble with id and reason columns.
  parse_warn_ids <- function(raw_warn) {
    if (rlang::is_empty(raw_warn)) {
      return(tibble::tibble(id = integer(), reason = character()))
    }
    # Data frame input
    if (is.data.frame(raw_warn)) {
      out <- tibble::as_tibble(raw_warn)
      if (!"id" %in% names(out) && ncol(out)) {
        out$id <- out[[1]]
      }
      if (!"reason" %in% names(out)) {
        out$reason <- NA_character_
      }
      return(out |> dplyr::select("id", "reason"))
    }
    # List input
    if (is.list(raw_warn)) {
      out <- purrr::map(raw_warn, function(entry) {
        if (is.null(entry)) {
          return(NULL)
        }
        if (is.atomic(entry) && length(entry)) {
          return(tibble::tibble(id = as.integer(entry), reason = NA_character_))
        }
        if (is.list(entry)) {
          id_val <- entry$id %||% entry$display_id %||% entry[[1]]
          if (is.null(id_val)) {
            return(NULL)
          }
          reason_val <- entry$reason %||% NA_character_
          return(tibble::tibble(
            id = as.integer(id_val),
            reason = as.character(reason_val)
          ))
        }
        NULL
      }) |>
        dplyr::bind_rows()
      if (!nrow(out)) {
        out <- tibble::tibble(id = integer(), reason = character())
      }
      return(out)
    }
    # Atomic vector input
    if (is.atomic(raw_warn)) {
      return(
        tibble::tibble(
          id = as.integer(raw_warn),
          reason = NA_character_
        )
      )
    }
    tibble::tibble(id = integer(), reason = character())
  }

  # Main iteration loop
  for (iteration in seq_len(max_iterations)) {
    cat("\n\n")

    cli::cli_alert("Starting iteration {.val {iteration}}")

    if (!"record_key" %in% names(current_dataset)) {
      cli::cli_alert_warning(
        "record_key column missing in current_dataset; restoring from baseline dataset_results for safety."
      )
      current_dataset <- dataset_results
    }

    # Prefilter to exclude already-sampled records
    available_dataset <- current_dataset |>
      dplyr::filter(!.data$record_key %in% cumulative_sample_ids)
    if (!"record_key" %in% names(available_dataset)) {
      cli::cli_abort(
        "record_key missing from available_dataset during sampling step."
      )
    }

    # Add class labels for sampling
    available_dataset <- assign_classification_classes(available_dataset)

    prev_metrics <- if (length(history) > 0) {
      history[[length(history)]]$metrics
    } else {
      NULL
    }
    criteria_changed_prev <- if (iteration == 1) {
      FALSE
    } else {
      isTRUE(history[[iteration - 1]]$criteria_changed %||% FALSE)
    }

    # Get sampling request from previous iteration or use the configured initial
    # sampling on the first iteration
    sampling_request <- if (iteration == 1) {
      initial_sampling_request
    } else {
      history[[iteration - 1]]$sampling_request
    }

    cli::cli_alert_info(
      c(
        "Sampling request summary: ",
        "{.fun {summarise_sampling_request(sampling_request)}}"
      )
    )

    # Build argument list dynamically based on which request is not NULL
    req_list <- list(
      fn = sampling_request$fn,
      fp = sampling_request$fp,
      tp = sampling_request$tp,
      tn = sampling_request$tn
    )

    # Ensure every class is passed (missing classes get count 0) so defaults are
    # not pulled from select_refinement_samples.
    req_args <- purrr::imap(req_list, function(entry, cls) {
      if (is.null(entry)) {
        count <- 0
        order <- "desc"
      } else {
        count <- entry$count %||% 0
        order <- entry$order %||% "desc"
      }
      setNames(count, order)
    })

    if (iteration == 1 && !length(req_args)) {
      cli::cli_abort(
        "No sampling requests provided for the initial iteration."
      )
    }
    # req_args has at most one element; add dataset arg
    sampling_result <- tryCatch(
      do.call(
        select_refinement_samples,
        c(list(current_dataset = available_dataset), req_args)
      ),
      error = function(e) {
        cli::cli_alert_danger(
          "Sampling failed: {conditionMessage(e)}. Entering browser for inspection."
        )
        browser()
      }
    )

    new_sample_ids <- sampling_result$selected_ids
    requested_total <- sum(purrr::map_dbl(req_list, function(entry) {
      entry$count %||% 0
    }))
    cli::cli_alert_info(
      "Selected {.val {length(new_sample_ids)}} sampled records"
    )
    sampling_table <- sampling_result$selection_table %||% dplyr::tibble()
    sampling_counts_msg <- if (nrow(sampling_table)) {
      sampling_table |>
        dplyr::count(.data$class, name = "n") |>
        dplyr::mutate(label = sprintf("%s=%d", .data$class, .data$n)) |>
        dplyr::pull(.data$label)
    } else {
      "none"
    }
    cli::cli_alert_info(
      "Sampled records by class: {.var {sampling_counts_msg}}"
    )
    cli::cli_alert_info(
      "Requested {.val {requested_total}} sampled records; selected {.val {length(new_sample_ids)}} after duplicate removal"
    )

    # Track which records were already seen in prior iterations so we can
    # show only previous history in the cumulative table.
    previous_sample_ids <- cumulative_sample_ids

    # Assign display IDs to new records
    next_display_id <- length(display_id_map) + 1
    for (record_key in new_sample_ids) {
      display_id_map[[record_key]] <- next_display_id
      last_seen_iteration[[record_key]] <- iteration
      next_display_id <- next_display_id + 1
    }

    # Update cumulative sample set
    cumulative_sample_ids <- c(cumulative_sample_ids, new_sample_ids)

    # Snapshot votes before any potential re-evaluation (for later comparison)
    pre_eval_dataset <- current_dataset

    labeled_dataset <- assign_classification_classes(current_dataset)
    fn_fp_tp_ids <- labeled_dataset |>
      dplyr::filter(.data$class %in% c("FN", "FP", "TP")) |>
      dplyr::pull(.data$record_key) |>
      unique()

    evaluation_subset <- unique(c(cumulative_sample_ids, fn_fp_tp_ids))
    # Re-evaluate only after the first iteration and only when criteria changed.
    evaluation_performed <- iteration > 1 && criteria_changed_prev
    reevaluated_records <- if (evaluation_performed) {
      evaluation_subset
    } else {
      character()
    }

    if (evaluation_performed) {
      # Evaluate only the cumulative sampled records; other records keep their
      # baseline votes. Metrics will later be computed on the full dataset with
      # these updated votes.
      eval_data <- current_dataset |>
        dplyr::filter(.data$record_key %in% evaluation_subset) |>
        dplyr::distinct(.data$record_key, .keep_all = TRUE)

      if (!nrow(eval_data)) {
        cli::cli_alert_warning(
          "Criteria changed but no records available for reevaluation; skipping."
        )
      }

      cumulative_evaluations <- list()
      for (eval_model in eval_models) {
        if (iteration > 1) {
          cli::cli_alert("Running evaluation with model: {.field {eval_model}}")
          eval_result <- evaluate_criteria(
            data = eval_data,
            include_text = current_include,
            exclude_text = current_exclude,
            store_path = store_path,
            eval_model = eval_model,
            eval_args = eval_args
          )
        } else {
          # Extract baseline predictions
          subset <- dataset_results |>
            dplyr::filter(
              .data$model == eval_model
            )

          eval_result <- subset
        }
        cumulative_evaluations[[eval_model]] <- eval_result
      }

      # Update current_dataset with new predictions for sampled records
      for (eval_model in eval_models) {
        eval_predictions <- cumulative_evaluations[[eval_model]]
        model_col <- paste0("matches_", sub("/", "_", eval_model))

        # Ensure the per-model column exists
        if (!model_col %in% names(current_dataset)) {
          current_dataset[[model_col]] <- NA
        }

        # Update matches for evaluated records for this model
        # First update main matches column for this model's rows
        current_dataset <- current_dataset |>
          dplyr::rows_update(
            eval_predictions |>
              dplyr::select("record_key", "matches") |>
              dplyr::mutate(model = eval_model),
            by = c("record_key", "model")
          )

        # Then update the widened vote column
        match_idx <- match(
          eval_predictions$record_key,
          current_dataset$record_key
        )
        match_vals <- eval_predictions$matches
        current_dataset[[model_col]][match_idx] <- match_vals
      }

      current_metrics <- summarise_model_metrics(current_dataset) |>
        dplyr::mutate(eval_model = .data$model)
    } else {
      if (iteration == 1) {
        cumulative_evaluations <- list()
        current_metrics <- summarise_model_metrics(current_dataset) |>
          dplyr::mutate(eval_model = .data$model)
      } else {
        cli::cli_alert_info(
          "Criteria unchanged; re-using previous evaluation without re-running models."
        )
        cumulative_evaluations <- history[[
          iteration - 1
        ]]$cumulative_evaluations
        current_metrics <- prev_metrics
        if (
          !rlang::is_null(current_metrics) &&
            nrow(current_metrics) &&
            !"eval_model" %in% names(current_metrics)
        ) {
          current_metrics <- current_metrics |>
            dplyr::mutate(eval_model = .data$model)
        }
      }
    }

    # Compute deltas vs previous iteration (first iteration deltas are zero)
    if (
      !rlang::is_empty(prev_metrics) &&
        nrow(prev_metrics) &&
        !rlang::is_empty(current_metrics) &&
        nrow(current_metrics)
    ) {
      current_metrics <- current_metrics |>
        dplyr::left_join(
          prev_metrics |>
            dplyr::select(
              "model",
              "precision",
              "recall",
              "TP",
              "FP",
              "TN",
              "FN"
            ) |>
            dplyr::rename_with(~ paste0("prev_", .x), -"model"),
          by = c("eval_model" = "model")
        )

      if (
        all(
          c(
            "prev_precision",
            "prev_recall",
            "prev_TP",
            "prev_FP",
            "prev_TN",
            "prev_FN"
          ) %in%
            names(current_metrics)
        )
      ) {
        current_metrics <- current_metrics |>
          dplyr::mutate(
            delta_precision = .data$precision - .data$prev_precision,
            delta_recall = .data$recall - .data$prev_recall,
            delta_tp = .data$TP - .data$prev_TP,
            delta_fp = .data$FP - .data$prev_FP,
            delta_tn = .data$TN - .data$prev_TN,
            delta_fn = .data$FN - .data$prev_FN
          )
      } else {
        current_metrics <- current_metrics |>
          dplyr::mutate(
            delta_precision = 0,
            delta_recall = 0,
            delta_tp = 0,
            delta_fp = 0,
            delta_tn = 0,
            delta_fn = 0
          )
      }
    } else if (!rlang::is_empty(current_metrics) && nrow(current_metrics)) {
      current_metrics <- current_metrics |>
        dplyr::mutate(
          delta_precision = 0,
          delta_recall = 0,
          delta_tp = 0,
          delta_fp = 0,
          delta_tn = 0,
          delta_fn = 0
        )
    } else {
      current_metrics <- tibble::tibble()
    }

    metrics_summary <- if (
      !rlang::is_empty(current_metrics) && nrow(current_metrics)
    ) {
      format_metrics_block(current_metrics)
    } else {
      "No metrics available."
    }
    cli::cli_alert_info(
      c(
        "Metrics after evaluation ",
        "(iteration {.val {iteration}}): "
      )
    )

    cat(metrics_summary, "\n")

    # Build vote strings before and after this iteration's evaluation
    make_vote_string <- function(base_tbl) {
      out <- base_tbl |>
        dplyr::select("record_key") |>
        dplyr::distinct(.data$record_key, .keep_all = TRUE)

      for (eval_model in eval_models) {
        model_col <- paste0("matches_", sub("/", "_", eval_model))
        if (model_col %in% names(base_tbl)) {
          vote_source <- base_tbl |>
            dplyr::select("record_key", !!model_col) |>
            dplyr::distinct(.data$record_key, .keep_all = TRUE) |>
            dplyr::rename(!!eval_model := !!model_col)
          out <- out |>
            dplyr::left_join(vote_source, by = "record_key")
        }
      }

      model_vote_cols <- eval_models[eval_models %in% names(out)]
      vote_string_tbl <- if (length(model_vote_cols)) {
        vote_snapshot <- out |>
          dplyr::select(dplyr::all_of(model_vote_cols))
        out |>
          dplyr::mutate(
            vote_string = purrr::pmap_chr(
              vote_snapshot,
              \(...) {
                vals <- c(...)
                votes <- ifelse(
                  is.na(vals),
                  "NA",
                  ifelse(vals, "TRUE", "FALSE")
                )
                paste(sprintf("%s=%s", model_vote_cols, votes), collapse = "; ")
              }
            )
          ) |>
          dplyr::select("record_key", "vote_string")
      } else {
        out |>
          dplyr::mutate(
            vote_string = "(not available)"
          ) |>
          dplyr::select("record_key", "vote_string")
      }
      vote_string_tbl
    }

    vote_strings_prev <- make_vote_string(pre_eval_dataset)
    vote_strings_curr <- make_vote_string(current_dataset)

    refinement_table <- current_dataset |>
      dplyr::filter(.data$record_key %in% previous_sample_ids) |>
      dplyr::distinct(.data$record_key, .keep_all = TRUE) |>
      dplyr::mutate(
        display_id = purrr::map_int(.data$record_key, ~ display_id_map[[.x]]),
        human_label = ifelse(.data$included, "relevant", "not relevant"),
        times_flagged = purrr::map_int(
          .data$record_key,
          ~ flagged_outliers[[.x]] %||% 0
        ),
        warning_reasons = purrr::map_chr(
          .data$record_key,
          ~ paste(unique(flagged_reasons[[.x]]), collapse = " | ")
        ),
        last_iteration_seen = purrr::map_int(
          .data$record_key,
          ~ last_seen_iteration[[.x]] %||% (iteration - 1)
        )
      ) |>
      dplyr::select(
        "record_key",
        "display_id",
        "title",
        "human_label",
        "TP_votes",
        "FP_votes",
        "TN_votes",
        "FN_votes",
        "times_flagged",
        "warning_reasons",
        "last_iteration_seen"
      )

    refinement_table <- refinement_table |>
      dplyr::left_join(
        vote_strings_prev |>
          dplyr::rename(
            previous_model_votes = "vote_string"
          ),
        by = "record_key"
      ) |>
      dplyr::left_join(
        vote_strings_curr |>
          dplyr::rename(
            latest_model_votes = "vote_string"
          ),
        by = "record_key"
      ) |>
      dplyr::mutate(
        reevaluated = .data$record_key %in% reevaluated_records
      )

    new_records <- current_dataset |>
      dplyr::filter(.data$record_key %in% new_sample_ids) |>
      dplyr::distinct(.data$record_key, .keep_all = TRUE) |>
      dplyr::mutate(
        display_id = purrr::map_int(.data$record_key, ~ display_id_map[[.x]]),
        human_label = ifelse(.data$included, "relevant", "not relevant")
      ) |>
      dplyr::select(
        "record_key",
        "display_id",
        "title",
        "abstract",
        "human_label",
        "TP_votes",
        "FP_votes",
        "TN_votes",
        "FN_votes"
      )

    new_records <- new_records |>
      dplyr::left_join(
        vote_strings_prev |>
          dplyr::rename(previous_model_votes = "vote_string"),
        by = "record_key"
      ) |>
      dplyr::left_join(
        vote_strings_curr |>
          dplyr::rename(latest_model_votes = "vote_string"),
        by = "record_key"
      ) |>
      dplyr::mutate(
        reevaluated = .data$record_key %in% reevaluated_records
      )

    # Attach reasoning traces (from strongest failing models per class) for each sampled record
    reasoning_metrics <- if (
      !rlang::is_null(dataset_metrics) && nrow(dataset_metrics)
    ) {
      dataset_metrics
    } else {
      summarise_model_metrics(dataset_results)
    }

    reasoning_data <- dataset_results |>
      dplyr::filter(.data$record_key %in% new_sample_ids)

    reasoning_by_record <- purrr::map(new_sample_ids, function(rk) {
      class_chr <- sampling_table |>
        dplyr::filter(.data$record_key == rk) |>
        dplyr::pull(.data$class) |>
        dplyr::first()
      info <- pick_reasoning_trace(
        results = reasoning_data,
        model_metrics = reasoning_metrics,
        record_key = rk,
        sample_class = class_chr,
        max_reasonings = 2L
      )
      tibble::tibble(
        record_key = rk,
        reasoning_model = if (length(info$models)) {
          paste(info$models, collapse = "; ")
        } else {
          NA_character_
        },
        reasoning_trace = info$text %||% NA_character_
      )
    }) |>
      dplyr::bind_rows()

    new_records <- new_records |>
      dplyr::left_join(reasoning_by_record, by = "record_key") |>
      dplyr::select(-"record_key")

    prompt <- prepare_refiner_prompt(
      dataset_name = dataset_name,
      current_include = current_include,
      current_exclude = current_exclude,
      metrics_current = current_metrics,
      refinement_table = refinement_table,
      new_records = new_records,
      iteration = iteration,
      max_new = 40,
      total_iterations = max_iterations
    )

    cli::cli_alert(
      "Sending prompt to LLM refiner (model: {.field {refiner_model}})..."
    )

    response <- chat_prompt_with_retry(refiner_chat, prompt)

    cat("\n\n", response, "\n\n")

    cli::cli_alert("Received LLM response, parsing JSON...")

    parsed <- tryCatch(
      {
        # Extract the JSON block from the response (robust to extra text)
        response_json <- extract_json_object(response)

        if (is.null(response_json)) {
          # Try regex fallback
          response_block <- stringr::str_extract(
            response,
            stringr::regex("\\{.*\\}", dotall = TRUE)
          )
          response_json <- tryCatch(
            jsonlite::fromJSON(response_block, simplifyVector = TRUE),
            error = function(...) NULL
          )
        }
        if (is.null(response_json)) {
          cli::cli_abort(
            c(
              "Failed to extract JSON from LLM response, stopping",
              "Response snippet" = stringr::str_trunc(
                response %||% "(empty)",
                500
              )
            )
          )
        }
        response_json
      },
      error = function(e) {
        cli::cli_alert_danger(
          "Failed to parse LLM JSON: {conditionMessage(e)}"
        )
        browser()
        stop(e)
      }
    )

    stop_loop <- FALSE
    tryCatch(
      {
        # Step 7: Process LLM response
        # Update flags
        # Parse warn_ids from the LLM response into a data frame.
        warn_df <- parse_warn_ids(parsed$warn_ids)

        # If there are any warnings, loop through each one.
        if (nrow(warn_df)) {
          for (i in seq_len(nrow(warn_df))) {
            # Extract the display_id associated with the warning.
            display_id <- warn_df$id[[i]]
            # Map this display_id back to the corresponding record_key.
            record_key <- names(display_id_map)[display_id_map == display_id]

            # If a valid record_key exists, proceed to update outlier tracking.
            if (length(record_key)) {
              # Increment the outlier flag count for this record_key.
              flagged_outliers[[record_key]] <- (flagged_outliers[[
                record_key
              ]] %||%
                0) +
                1

              # Extract the reason for the warning (if provided).
              reason <- warn_df$reason[[i]]

              # If the reason is not NULL, not NA, and is non-empty, append it to
              # flagged_reasons.
              if (rlang::is_string(reason) && nzchar(reason)) {
                flagged_reasons[[record_key]] <- c(
                  flagged_reasons[[record_key]],
                  reason
                )
              }
            }
          }
        }

        # Update criteria
        include_unchanged <- is.character(parsed$include) &&
          identical(tolower(trimws(parsed$include)), "unchanged")
        exclude_unchanged <- is.character(parsed$exclude) &&
          identical(tolower(trimws(parsed$exclude)), "unchanged")

        proposed_include <- if (include_unchanged) {
          current_include
        } else {
          parsed$include %||% current_include
        }
        proposed_exclude <- if (exclude_unchanged) {
          current_exclude
        } else {
          parsed$exclude %||% current_exclude
        }

        criteria_changed <- !(include_unchanged && exclude_unchanged)

        reasoning_text <- parsed$reasoning %||% "(not provided)"
        cli::cli_alert_info(
          "LLM reasoning: {reasoning_text}"
        )
        cli::cli_alert_info(
          "LLM sampling request for next iteration: {summarise_sampling_request(parsed$sampling)}"
        )
        cli::cli_alert_info(
          "LLM stop decision: {isTRUE(parsed$stop)}"
        )
        cli::cli_alert_info(
          "LLM criteria change request: include={if (include_unchanged) 'unchanged' else 'updated'}, exclude={if (exclude_unchanged) 'unchanged' else 'updated'}"
        )

        # Step 8: Recall protection
        current_recall <- current_metrics$recall[1] # Use first model's recall
        prev_recall <- if (
          !rlang::is_empty(prev_metrics) && nrow(prev_metrics)
        ) {
          prev_metrics$recall[1]
        } else {
          NA
        }

        if (
          !is.na(prev_recall) &&
            !is.na(current_recall) &&
            current_recall < prev_recall
        ) {
          recall_drop_msg <- glue::glue(
            "CRITICAL WARNING: Recall dropped from {round(prev_recall, 3)} to {round(current_recall, 3)}. ",
            "You lost {round((prev_recall - current_recall) * 100, 1)}% of previously found relevant records. ",
            "MUST fix in next iteration."
          )
          cli::cli_alert_danger("{recall_drop_msg}")

          # Force continue
          parsed$stop <- FALSE

          # Add warning to current iteration notes
          parsed$reasoning <- paste(
            parsed$reasoning %||% "",
            recall_drop_msg,
            sep = "\n\n"
          )
        }

        # Step 9: Record iteration
        history[[iteration]] <- list(
          iteration = iteration,
          include = proposed_include,
          exclude = proposed_exclude,
          reasoning = parsed$reasoning %||% "",
          sampling_request = parsed$sampling %||% list(),
          warn_ids = parsed$warn_ids %||% integer(),
          metrics = current_metrics,
          criteria_changed = criteria_changed,
          cumulative_evaluations = cumulative_evaluations,
          evaluation_performed = evaluation_performed,
          cumulative_sample_count = length(cumulative_sample_ids),
          notes = sampling_result$notes
        )

        # Update current criteria
        current_include <- proposed_include
        current_exclude <- proposed_exclude

        cli::cli_alert_info(
          "Iteration {iteration} completed. Cumulative samples: {length(cumulative_sample_ids)}"
        )

        if (isTRUE(parsed$stop)) {
          cli::cli_alert_success("Refiner requested to stop")
          stop_loop <- TRUE
        }
      },
      error = function(e) {
        cli::cli_alert_danger(
          "Failure during post-refiner processing (iteration {iteration}): {conditionMessage(e)}"
        )
        browser()
        stop(e)
      }
    )

    if (isTRUE(stop_loop)) {
      break
    }
  }

  if (iteration == max_iterations) {
    cli::cli_alert_warning(
      "Reached maximum iterations ({max_iterations}) without convergence"
    )
  } else {
    cli::cli_alert("Refinement process completed")
  }

  cli::cli_alert_info(
    "Final criteria - Include: {substr(current_include, 1, 80)}..."
  )
  cli::cli_alert_info(
    "Final criteria - Exclude: {substr(current_exclude, 1, 80)}..."
  )
  cli::cli_alert_info("Total iterations: {length(history)}")
  cli::cli_alert_info(
    "Total records evaluated: {length(cumulative_sample_ids)}"
  )

  list(
    dataset = dataset_name,
    history = history,
    final = list(include = current_include, exclude = current_exclude),
    cumulative_evaluation = list(
      sample_ids = cumulative_sample_ids,
      display_id_map = display_id_map,
      flagged_outliers = flagged_outliers,
      dataset = current_dataset
    )
  )
}

#' Create visualization of model performance metrics
#'
#' This function generates a bar plot comparing precision and recall across
# different
#' models and datasets. The plot uses dodged bars to show both metrics
# side-by-side
#' for easy comparison.
#'
#' @param metrics A data frame containing model metrics as returned by
#'   \code{\link{summarise_model_metrics}}, with columns including 'model',
#'   'dataset', 'precision', and 'recall'.
#'
#' @return A ggplot2 object showing precision and recall comparison. Returns
# NULL
#'   if no metrics are provided.
#'
#' @examples
#' \dontrun{
#' metrics <- data.frame(
#'   model = c("model1", "model2"),
#'   dataset = c("dataset1", "dataset1"),
#'   precision = c(0.8, 0.9),
#'   recall = c(0.7, 0.6)
#' )
#'
#' plot <- plot_analysis_results(metrics)
#' print(plot)
#' }
#'
#' @export
plot_analysis_results <- function(metrics) {
  if (rlang::is_empty(metrics) || !nrow(metrics)) {
    cli::cli_alert_warning("No metrics supplied, returning NULL.")
    return(NULL)
  }
  metrics_long <- metrics |>
    dplyr::select("model", "dataset", precision, recall) |>
    tidyr::pivot_longer(
      c("precision", "recall"),
      names_to = "metric",
      values_to = "value"
    )
  ggplot2::ggplot(metrics_long) +
    ggplot2::aes(model, value, fill = metric) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge2(padding = 0.2),
      width = 0.5,
      color = "black"
    ) +
    ggplot2::facet_wrap(~ .data$dataset) +
    ggplot2::labs(
      title = "Model precision/recall summary",
      x = "Model",
      y = "Value",
      fill = ""
    ) +
    scale_fill_manual(
      values = c("precision" = "steelblue", "recall" = "red")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
