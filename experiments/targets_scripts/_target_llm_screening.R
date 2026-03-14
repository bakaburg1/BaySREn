# experiments/scripts/_targets_classification.R

# tar_config_set(script = here::here("experiments/targets_scripts/_target_llm_screening.R"), store = here::here("experiments/targets_scripts/_target_llm_screening"), use_crew = T)

# Load the package so that example data sets (ace, hall, …) are on search path
devtools::load_all()

# Explicitly load the datasets and criteria objects needed by the pipeline
data(
  list = c("gastaldi", "vella", "gastaldi_criteria", "vella_criteria"),
  package = "BaySREn",
  envir = globalenv()
)

store_path <- file.path(
  "experiments",
  "targets_scripts",
  "_target_llm_screening"
)

# Libraries & target-level options ----
library(targets)
library(tarchetypes)

# Load required packages with explicit namespace requirements
requireNamespace("ellmer")
requireNamespace("dplyr")
requireNamespace("ggplot2")
requireNamespace("purrr")
requireNamespace("cli")
requireNamespace("stringr")
requireNamespace("rlang")
requireNamespace("glue")
requireNamespace("jsonlite")
requireNamespace("openxlsx")


# Pipeline options ----
tar_option_set(
  packages = c(
    "dplyr",
    "ggplot2"
  ),
  controller = crew::crew_controller_local(workers = 10)
)

tar_source(here::here("R"))


# datasets <- c("ace", "wilson") #, "hall", "virus", "ptsd", "nudging")
# datasets <- "CD012268_calcium_suppl"
datasets <- c("gastaldi", "vella")

# Model list ----

models <- list(
  # "openai/gpt-4.1-nano" = list(
  #   model = "openai/gpt-4.1-nano",
  #   args = list(temperature = 0)
  # ),
  # "openai/gpt-4.1-mini" = list(
  #   model = "openai/gpt-4.1-mini",
  #   args = list(temperature = 0)
  # ),
  # "openai/gpt-4.1" = list(
  #   model = "openai/gpt-4.1",
  #   args = list(temperature = 0)
  # ),
  # "openai/o4-mini" = list(
  #   model = "openai/o4-mini",
  #   args = list()
  # ),
  # # "openai/o3" = list(
  # #   model = "openai/o3",
  # #   args = list(temperature = 0)
  # # ),
  # "google/gemma-3-4b-it" = list(
  #   model = "google/gemma-3-4b-it",
  #   args = list(temperature = 0)
  # ),
  # "google/gemma-3-12b-it" = list(
  #   model = "google/gemma-3-12b-it",
  #   args = list(temperature = 0)
  # ),
  # "google/gemma-3-27b-it" = list(
  #   model = "google/gemma-3-27b-it",
  #   args = list(temperature = 0)
  # ),
  # "google/gemma-3n-e4b-it" = list(
  #   model = "google/gemma-3n-e4b-it",
  #   args = list(temperature = 0)
  # ),
  # # "google/gemini-2.5-pro" = list(
  # #   model = "google/gemini-2.5-pro",
  # #   args = list(temperature = 0)
  # # ),
  # "google/gemini-2.5-flash" = list(
  #   model = "google/gemini-2.5-flash",
  #   args = list(temperature = 0)
  # ),
  # "google/gemini-2.5-flash-lite" = list(
  #   model = "google/gemini-2.5-flash-lite",
  #   args = list(temperature = 0)
  # ),
  # "openai/gpt-5-nano" = list(
  #   model = "openai/gpt-5-nano"
  # ),
  # "openai/gpt-5-mini" = list(
  #   model = "openai/gpt-5-mini"
  # ),
  # "qwen/qwen3-30b-a3b-instruct-2507" = list(
  #   model = "qwen/qwen3-30b-a3b-instruct-2507",
  #   args = list(temperature = 0)
  # ),
  # "qwen/qwen3-235b-a22b-thinking-2507" = list(
  #   model = "qwen/qwen3-235b-a22b-thinking-2507",
  #   args = list(temperature = 0)
  # ),
  # "qwen/qwen3-235b-a22b-2507" = list(
  #   model = "qwen/qwen3-235b-a22b-2507",
  #   args = list(temperature = 0)
  # ),
  # "z-ai/glm-4.5" = list(
  #   model = "z-ai/glm-4.5",
  #   args = list(temperature = 0)
  # ),
  # "z-ai/glm-4.5-air" = list(
  #   model = "z-ai/glm-4.5-air",
  #   args = list(temperature = 0)
  # ),
  # "x-ai/grok-4-fast" = list(
  #   model = "x-ai/grok-4-fast",
  #   args = list(temperature = 0, reasoning = list(effort = "high"))
  # ),
  # "deepseek/deepseek-v3.2-exp" = list(
  #   model = "deepseek/deepseek-v3.2-exp",
  #   args = list(temperature = 0, reasoning = list(effort = "high"))
  # ),
  "x-ai/grok-4.1-fast" = list(
    model = "x-ai/grok-4.1-fast",
    args = list(temperature = 0, reasoning = list(effort = "high"))
  ),
  "openai/gpt-oss-20b" = list(
    model = "openai/gpt-oss-20b",
    args = list(temperature = 0, reasoning = list(effort = "high"))
  ),
  "openai/gpt-oss-120b" = list(
    model = "openai/gpt-oss-120b",
    args = list(temperature = 0, reasoning = list(effort = "high"))
  )
)

# Select active models without editing the full list
# Reads comma-separated identifiers from env var BAYSREN_ACTIVE_MODELS.
# Falls back to all models when unset.
active_models <- local({
  env <- Sys.getenv("BAYSREN_ACTIVE_MODELS", unset = "")
  if (nzchar(env)) {
    toks <- strsplit(env, ",", fixed = TRUE)[[1]]
    toks <- trimws(toks)
    intersect(names(models), toks)
  } else {
    names(models)
  }
})

# Helper functions ----

#' Extract Pareto-optimal subset of data
#'
#' Given a dataset with x and y columns, returns the Pareto-optimal subset where
#' for each x value, only the row with the highest y value is kept. This creates
#' a monotonic frontier of best y values across x values.
#'
#' @param data A data frame containing the data
#' @param x_col Character string specifying the x column name
#' @param y_col Character string specifying the y column name
#'
#' @return A filtered data frame containing only Pareto-optimal rows
#'
#' @examples # Example with performance vs cost trade-off
#' df <- data.frame(
#'   cost = c(1, 2, 2, 3, 4, 5),
#'   performance = c(0.6, 0.7, 0.65, 0.8, 0.85, 0.9)
#' )
#' pareto_subset(df, "cost", "performance")
pareto_subset <- function(data, x_col, y_col) {
  # Drop rows with missing x or y
  data <- data[!is.na(data[[x_col]]) & !is.na(data[[y_col]]), , drop = FALSE]
  if (nrow(data) == 0) {
    return(data)
  }

  # Sort by x column to process in order
  data_sorted <- data[order(data[[x_col]]), , drop = FALSE]

  # Initialize with first row
  pareto_rows <- 1
  best_y <- data_sorted[[y_col]][1]

  # Iterate through remaining rows
  if (nrow(data_sorted) >= 2) {
    for (i in 2:nrow(data_sorted)) {
      current_y <- data_sorted[[y_col]][i]
      if (current_y >= best_y) {
        pareto_rows <- c(pareto_rows, i)
        best_y <- current_y
      }
    }
  }

  data_sorted[pareto_rows, , drop = FALSE]
}


# Setup targets ----

setup_targets <- list(
  tar_target(
    selection_criteria,
    {
      list(
        ace = list(
          include = paste(
            # Language
            "English-language publication",
            # Populations (any one is enough)
            "Population: adults with at least one of",
            "  • Hypertension (with or without compelling indications)",
            "  • High cardiovascular risk (diabetes, smoking, hyperlipidaemia, prior CHD/CVD)",
            "  • Recent myocardial infarction (normal or asymptomatic LV function)",
            "  • Symptomatic heart failure due to LV systolic dysfunction",
            "  • Diabetic nephropathy",
            "  • Nondiabetic nephropathy",
            # Interventions
            "Intervention: monotherapy with a listed ACE inhibitor",
            "  (benazepril, captopril, cilazapril, enalapril, fosinopril,",
            "   lisinopril, moexipril, perindopril, quinapril, ramipril, trandolapril)",
            # Outcomes (any one is enough)
            "Reports at least one eligible outcome:",
            "  • Mortality (all-cause or cardiovascular)",
            "  • Major CV events (stroke, MI, incident HF)",
            "  • ESRD / sustained fall in renal function",
            "  • HF symptom class / functional status / QoL",
            "  • HF hospitalisations",
            "  • ACEI adverse events (hypotension, cough, angio-oedema, hyperkalaemia)",
            # Designs
            "Acceptable study designs:",
            "  - Systematic reviews of clinical efficacy or safety for included conditions",
            "  - RCTs comparing one listed ACEI vs another listed ACEI OR",
            "    placebo-controlled RCTs with >100 participants",
            "  - Large, good-quality observational studies of adverse events",
            # Timing
            "No lower limit on follow-up duration (single-dose / pre-discharge studies excluded)",
            sep = "\n"
          ),
          exclude = paste(
            "Non-English publication",
            "Animal or in-vitro study",
            "Focuses only on blood-pressure reduction with no clinical outcome",
            "Single-dose study or study confined to in-hospital period before discharge",
            "Uses ACEI exclusively in fixed-dose combination where the ACEI effect",
            "  cannot be separated",
            "Intervention is not one of the specified ACE inhibitors",
            sep = "\n"
          )
        ),
        wilson = list(
          include = "
1. **Population**

   * Patients have **Wilson disease (WD) of any age and any disease stage**;

2. **Intervention / study drug**

   * The experimental arm uses **exactly one** of these four established therapies:

     * D-penicillamine (DPen)
     * Trientine
     * Tetrathiomolybdate (TTM)
     * Zinc salts (Zn);

3. **Comparator**

   * Control can be **placebo, no treatment, or any other treatment that does *not* contain the study drug**

     * *Example allowed*: Zn vs trientine
     * *Example *not* allowed*: Zn 50 mg vs Zn 100 mg;
   * **Concomitant therapies must be identical** in the two arms (e.g. trientine + Zn vs TTM + Zn);

4. **Outcomes—study must report *≥ 1* of these patient-relevant endpoints**;

   * **All-cause mortality**
   * **Orthotopic liver transplantation (OLT)**
   * **Neurological symptoms**: dystonia, dysarthria, cognitive decline, drooling, tremor, gait disturbance, chorea, seizure, psychosis
   * **Liver-related symptoms**: icterus, ascites, steatosis, fibrosis, mild hepatitis, acute liver failure, cirrhosis, serum-transaminase levels
   * **Adverse effects** (any listed), e.g. dermatologic reactions, nephro- or pulmonary toxicity, autoimmune disorders, anaemia, agranulocytosis, thrombocytopenia, hypothyroidism, liver dysfunction, colitis, status dystonicus, myasthenia gravis, arthropathy, macromastia, early neurological deterioration, gastrointestinal irritation
   * **Treatment discontinuation**: switching drug, stopping, or changing therapy

5. **Study design**

   * **Prospective or retrospective** comparative studies, including:

     * Randomised controlled trials
     * Non-randomised controlled trials
     * Comparative observational studies;

6. **Language**

   * Article is written in **English, German, Dutch, French, Spanish, or Portuguese**;
",
          exclude = "
* **Animal studies**;
* **Case reports** or **case series**;
* **Cross-sectional studies**;
* **Before-after (pre-/post-) studies**;
* **Non-controlled studies** (single-arm);
* **Reviews, letters, editorials, abstract-only publications, or diagnostic/other testing studies**;
* **Comparisons between a monotherapy and a combination regimen that *includes* that same drug** (e.g. DPen + Zn vs Zn) — those were analysed elsewhere and *not* considered here;
"
        ),
        CD012268_calcium_suppl = list(
          include = "
# Study Designs
* Randomised controlled trials (RCTs).

# Populations
* Participants with overweight or obesity.
* Participants of any age or sex.
* Pregnant women are included.
* Overweight and obesity are classified using Body Mass Index (BMI):
    * **Adults**: Overweight is defined as a BMI of 25 to 29.9; obesity is a BMI of 30 or higher.
    * **Children and adolescents**: Validated classifications are accepted, such as WHO child growth standards, WHO growth references using BMI for age, International Obesity Task Force (IOTF) child BMI cut-offs, and BMI z scores.

# Interventions
* The intervention must have a minimum duration of two months.
* Studies of any calcium dose are included.
* Concomitant interventions are accepted if both the intervention and comparator groups receive the same co-intervention.
* The following comparisons are eligible:
    * Oral calcium supplementation versus placebo.
    * Calcium-fortified food or beverage versus placebo.
    * Calcium-fortified food or beverage versus non-calcium-fortified food or beverage.
* **Definition of Calcium Fortification**: Calcium fortification can include various salts such as calcium carbonate, sulphate, citrate, citrate malate, chloride, hydroxyapatite, phosphate, acetate, lactate, glycerophosphate, gluconate, oxide, or hydroxide.
",
          exclude = "
# Excluded Study Designs
* Cross-over studies are excluded.

# Excluded Populations
* Studies involving participants with chronic illnesses that affect calcium absorption or metabolism. Examples include:
    * Lactose intolerance.
    * Inflammatory bowel disease (Crohn's disease, ulcerative colitis).
    * Bariatric surgery patients.

# Excluded Interventions
* Studies that evaluated the effect of calcium combined with vitamin D compared to placebo.
* Studies that evaluated mixed minerals compared to placebo.
"
        ),
        gastaldi = list(
          include = gastaldi_criteria$include,
          exclude = gastaldi_criteria$exclude
        ),
        vella = list(
          include = vella_criteria$include,
          exclude = vella_criteria$exclude
        )
      )
    }
  ),

  tar_target(
    model_names,
    active_models
  )
)

# Process targets ----

process_targets <- list(
  mapped_targets = tar_map(
    values = data.frame(dataset_name = datasets),
    names = "dataset_name",

    tar_target(
      dataset_data,
      {
        get(dataset_name)
      },
      # "main" allows the target to see in the env created by
      # devtools::load_all()
      deployment = "main"
    ),

    tar_target(selection_query, selection_criteria[[dataset_name]]),

    tar_target(
      classify_records,
      {
        readRenviron(here::here())

        model <- models[[model_names]]
        system_prompt_template <- default_system_prompt_template()

        combination_hash <- rlang::hash(
          list(
            dataset = dataset_data,
            model = model,
            query = selection_query,
            prompt = system_prompt_template
          )
        )

        results_path <- file.path(
          store_path,
          "processed_combinations",
          paste0(combination_hash, ".rds")
        )

        fs::dir_create(
          dirname(results_path)
        )

        if (!fs::file_exists(results_path)) {
          api_args <- if (is.null(model$args)) list() else model$args
          chat_model <- ellmer::chat_openrouter(
            model = model$model,
            api_args = api_args,
            echo = "none"
          )

          # OpenAI o3 fails in the OpenRouter API
          if (model$model == "openai/o3") {
            chat_model <- ellmer::chat_openai(
              model = "o3"
            )
          }

          data_name <- dataset_name
          data_rows <- nrow(dataset_data)

          cli::cli_alert_info(
            "Classifying {data_rows} records from `{data_name}` data with model {model$model}."
          )

          # Optionally limit number of rows via env var BAYSREN_N_ROWS during tests
          n_limit <- suppressWarnings(as.integer(Sys.getenv(
            "BAYSREN_N_ROWS",
            ""
          )))
          data_to_use <- if (
            !is.na(n_limit) && is.finite(n_limit) && n_limit > 0
          ) {
            dataset_data[
              seq_len(min(n_limit, nrow(dataset_data))),
              ,
              drop = FALSE
            ]
          } else {
            dataset_data
          }

          results <- classify_citations(
            data_to_use,
            chat = chat_model,
            max_batch_size = 400,
            query = selection_query,
            cache_dir = here::here(store_path, "llm_cache", dataset_name),
            system_prompt_template = system_prompt_template
          )

          saveRDS(
            results,
            file = results_path
          )
        }
        results_path
      },
      # cue = tar_cue("always"),
      pattern = map(model_names),
      format = "file",
      deployment = "main"
    ),

    # Validate that all rows were processed for each model/dataset
    tar_target(
      validate_classify_records,
      {
        paths <- classify_records
        if (rlang::is_empty(paths)) {
          rlang::abort("No classification outputs to validate.")
        }

        paths <- as.character(paths)
        expected_n <- nrow(dataset_data)

        invalid_paths <- character(0)

        for (i in seq_along(paths)) {
          p <- paths[[i]]
          if (is.na(p) || !nzchar(p) || !fs::file_exists(p)) {
            invalid_paths <- c(invalid_paths, p)
            next
          }

          res <- try(readRDS(p), silent = TRUE)
          if (inherits(res, "try-error")) {
            invalid_paths <- c(invalid_paths, p)
            next
          }

          actual_n <- try(nrow(res), silent = TRUE)
          if (
            inherits(actual_n, "try-error") || !identical(actual_n, expected_n)
          ) {
            invalid_paths <- c(invalid_paths, p)
          }
        }

        if (!rlang::is_empty(invalid_paths)) {
          unique_invalid <- unique(invalid_paths)
          cli::cli_alert_warning(
            "Deleting {length(unique_invalid)} invalid classification file(s) for `{dataset_name}`."
          )
          purrr::walk(unique_invalid, \(p) {
            if (!is.na(p) && nzchar(p) && fs::file_exists(p)) {
              cli::cli_alert_info("Deleting invalid classification file: {p}")
              fs::file_delete(p)
            }
          })
          rlang::abort(
            "Invalid classification outputs removed; upstream branches will rebuild."
          )
        }

        paths
      },
      # cue = tar_cue("always"),
    ),

    tar_target(
      combined_results,
      validate_classify_records |>
        purrr::map(readRDS) |>
        dplyr::bind_rows() |>
        dplyr::mutate(
          prompt_tokens_details = NULL,
          completion_tokens_details = NULL,
          dataset = dataset_name,
          .after = 0
        ) |>
        dplyr::mutate(
          record_key = purrr::pmap_chr(
            list(dataset, title, abstract, authors, keywords),
            ~ rlang::hash(list(...))
          ),
          .after = "dataset"
        )
    )
  )
)

# Analyse targets ----

analyse_targets <- list(
  tar_combine(
    all_datasets_results,
    process_targets$mapped_targets$combined_results,
    command = {
      dplyr::bind_rows(!!!.x)
    }
  ),

  tar_target(
    analysis_results,
    summarise_model_metrics(all_datasets_results)
  ),

  tar_target(
    pareto_frontier,
    {
      analysis_results

      frontier_data <- purrr::map(
        datasets,
        function(dataset_name) {
          # Filter results for the current dataset
          analysis_results |>
            filter(dataset == dataset_name) |>
            filter(!is.na(precision) & !is.na(recall)) |>
            mutate(recall = 1 - recall) |>
            pareto_subset("recall", "precision")
        }
      ) |>
        bind_rows() |>
        mutate(recall = 1 - recall)

      # Create the plot
      p <- ggplot(analysis_results) +
        aes(recall, precision, color = score) +
        annotate(
          "segment",
          x = 0,
          y = 1,
          xend = 1,
          yend = 0,
          linetype = "dashed"
        ) +
        geom_line(
          data = frontier_data,
          color = "black",
          linewidth = 1,
          linetype = "solid",
          group = "dataset"
        ) +
        geom_point() +
        ggrepel::geom_text_repel(aes(label = model)) +
        facet_wrap(~dataset, scales = "free") +
        scale_color_gradient(low = "red", high = "steelblue") +
        labs(
          title = "Precision-Recall Analysis with Frontier",
          # x = "Precision",
          # y = "Recall",
          color = "Distance Score"
        ) +
        theme_minimal()

      # Return both data and plot path
      list(
        frontier_models = frontier_data,
        plot = p
      )
    }
  )
)

# Criteria refinement ----

mapped_refinement <- tar_map(
  values = data.frame(dataset_name = datasets),
  names = "dataset_name",

  tar_target(
    refinement_run,
    {
      # Filter inputs for this specific dataset
      ds_results <- all_datasets_results |>
        dplyr::filter(.data$dataset == dataset_name)
      ds_metrics <- analysis_results |>
        dplyr::filter(.data$dataset == dataset_name)

      run_criteria_refiner(
        base_criteria = selection_criteria[[dataset_name]],
        dataset_results = ds_results,
        dataset_metrics = ds_metrics,
        store_path = store_path,
        max_iterations = 20L,
        eval_models = model_names,
        refiner_model = "openai/gpt-5.1",
        eval_args = list(reasoning = list(effort = "high")),
        initial_sample = list(fn = 15, fp = 10, tp = 5, tn = 0),
        context_limit_tokens = 150000L
      )
    }
  ),

  ## Mislabel evaluation ----
  tar_target(
    mislabel_eval,
    {
      run_mislabel_evaluator(
        refinement_run = refinement_run,
        base_criteria = selection_criteria[[dataset_name]],
        batch_size = 50L,
        evaluator_model = "openai/gpt-5.1",
        cache_dir = here::here(store_path, "mislabel_eval", dataset_name)
      )
    }
  ),

  tar_target(
    mislabel_eval_summary,
    {
      analyze_mislabel_evaluator_results(mislabel_eval)
    }
  ),

  # Corrected performance (post-review relabels) ----
  tar_target(
    corrected_performance,
    {
      # Load the corrected label spreadsheet for this dataset.
      revision_path <- file.path(
        "data-raw",
        "SIIAM",
        stringr::str_to_title(dataset_name),
        "label_revision.xlsx"
      )
      corrections_tbl <- openxlsx::read.xlsx(
        revision_path,
        sheet = "Corrections",
        startRow = 3
      )

      # Normalize label columns for consistent parsing.
      corrections_tbl <- corrections_tbl |>
        dplyr::mutate(
          human_label = stringr::str_to_lower(
            stringr::str_trim(.data$human_label %||% "")
          ),
          human_relabel = stringr::str_to_lower(
            stringr::str_trim(.data$human_relabel %||% "")
          ),
          human_relabel = dplyr::na_if(.data$human_relabel, "")
        )

      # Helper to turn label text into logical inclusion values.
      label_to_logical <- function(label) {
        dplyr::case_when(
          label %in% "relevant" ~ TRUE,
          label %in% "not relevant" ~ FALSE,
          TRUE ~ NA
        )
      }

      # Map corrections to record_key using the most reliable identifier.
      if ("record_key" %in% names(corrections_tbl)) {
        key_tbl <- corrections_tbl |>
          dplyr::mutate(record_key = .data$record_key)
      } else {
        record_lookup <- all_datasets_results |>
          dplyr::filter(.data$dataset == dataset_name) |>
          dplyr::distinct(.data$title, .data$abstract, .data$record_key)
        duplicate_keys <- record_lookup |>
          dplyr::count(.data$title, .data$abstract) |>
          dplyr::filter(.data$n > 1)
        if (nrow(duplicate_keys)) {
          cli::cli_warn(
            "Found {nrow(duplicate_keys)} duplicate title/abstract pairs ",
            "for `{dataset_name}`; record_key joins may be ambiguous."
          )
        }
        key_tbl <- corrections_tbl |>
          dplyr::left_join(record_lookup, by = c("title", "abstract"))
      }

      # Build corrected labels and join them to model outputs.
      corrected_labels <- key_tbl |>
        dplyr::mutate(
          included_original = label_to_logical(.data$human_label),
          included_corrected = dplyr::coalesce(
            label_to_logical(.data$human_relabel),
            .data$included_original
          )
        ) |>
        dplyr::select("record_key", "included_corrected")

      # Apply corrected labels and recompute performance metrics.
      final_snapshot <- dplyr::last(refinement_run$history)$dataset_snapshot
      corrected_results <- final_snapshot |>
        dplyr::left_join(corrected_labels, by = "record_key") |>
        dplyr::mutate(
          included = dplyr::coalesce(
            .data$included_corrected,
            .data$included
          )
        ) |>
        dplyr::select(-"included_corrected")

      summarise_model_metrics(corrected_results)
    }
  ),
  # Mislabel evaluation report ----
  tar_target(
    mislabel_eval_report,
    {
      ## Setup ----

      # Set output path for the generated Excel report.
      output_path <- file.path(
        store_path,
        "reports",
        paste0("mislabel_eval_report_", dataset_name, ".xlsx")
      )

      # Ensure the 'reports' directory exists (create if missing).
      dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

      # Utility: Return object 'x' if not NULL/empty, otherwise return a given
      # default.
      .get_or <- function(x, default) {
        if (is.null(x) || length(x) == 0) {
          return(default)
        }
        x
      }

      # Utility: Add '%' sign to percent/share/pct-labeled columns for human
      # readability.
      .format_percent_cols <- function(tbl) {
        percent_cols <- names(tbl)[
          stringr::str_detect(
            names(tbl),
            "percent|share|pct|sensitivity|specificity|precision"
          )
        ]
        if (!length(percent_cols)) {
          return(tbl)
        }
        tbl |>
          dplyr::mutate(
            dplyr::across(
              dplyr::all_of(percent_cols),
              \(x) ifelse(is.na(x), NA_character_, paste0(x, "%"))
            )
          )
      }

      # Utility: Combine n and percent columns into a single cell.
      .combine_count_percent <- function(tbl) {
        if (!nrow(tbl) || !"n" %in% names(tbl)) {
          return(tbl)
        }
        percent_cols <- names(tbl)[
          stringr::str_detect(names(tbl), "percent")
        ]
        if (!length(percent_cols)) {
          return(tbl)
        }
        percent_col <- percent_cols[[1]]
        count_vals <- tbl[["n"]]
        percent_vals <- tbl[[percent_col]]
        tbl[["count (percent)"]] <- ifelse(
          is.na(count_vals) & is.na(percent_vals),
          NA_character_,
          paste0(count_vals, " (", percent_vals, ")")
        )
        drop_cols <- intersect(names(tbl), c("n", percent_col))
        tbl[, !names(tbl) %in% drop_cols, drop = FALSE]
      }

      # Utility: Sort by first column for cross-tab summaries.
      .sort_by_first_col <- function(tbl) {
        if (!nrow(tbl) || !ncol(tbl)) {
          return(tbl)
        }
        tbl[order(tbl[[1]], na.last = TRUE), , drop = FALSE]
      }

      # Utility: Build a two-row label/definition table with count column.
      .make_label_count_table <- function(
        counts_tbl,
        label_col,
        def_map,
        label_map = NULL,
        all_labels = NULL
      ) {
        if (is.null(counts_tbl) || !nrow(counts_tbl)) {
          counts_tbl <- tibble::tibble()
        }
        if (!label_col %in% names(counts_tbl)) {
          counts_tbl[[label_col]] <- character()
        }
        if (!"n" %in% names(counts_tbl)) {
          counts_tbl[["n"]] <- integer()
        }
        if (!"percent" %in% names(counts_tbl)) {
          counts_tbl[["percent"]] <- numeric()
        }
        counts_tbl$label_raw <- as.character(counts_tbl[[label_col]])
        if (!is.null(label_map)) {
          counts_tbl$label_display <- unname(label_map[counts_tbl$label_raw])
        } else {
          counts_tbl$label_display <- counts_tbl$label_raw
        }
        counts_tbl <- counts_tbl[,
          c("label_display", "n", "percent"),
          drop = FALSE
        ]
        if (!is.null(all_labels)) {
          base_tbl <- tibble::tibble(label_display = all_labels)
          counts_tbl <- dplyr::left_join(
            base_tbl,
            counts_tbl,
            by = "label_display"
          )
        }
        percent_vals <- counts_tbl[["percent"]]
        if (is.character(percent_vals)) {
          percent_vals <- suppressWarnings(
            as.numeric(gsub("%", "", percent_vals))
          )
        }
        counts_tbl[["n"]] <- dplyr::coalesce(counts_tbl[["n"]], 0L)
        counts_tbl[["percent"]] <- dplyr::coalesce(percent_vals, 0)
        label_rows <- integer()
        desc_rows <- integer()
        out <- tibble::tibble(label = character(), count = character())
        row_idx <- 0L
        for (i in seq_len(nrow(counts_tbl))) {
          label_val <- counts_tbl$label_display[[i]]
          desc_val <- def_map[[label_val]] %||% ""
          count_val <- paste0(
            counts_tbl$n[[i]],
            " (",
            counts_tbl$percent[[i]],
            "%)"
          )
          out <- dplyr::bind_rows(
            out,
            tibble::tibble(label = label_val, count = count_val),
            tibble::tibble(label = desc_val, count = "")
          )
          label_rows <- c(label_rows, row_idx + 1L)
          desc_rows <- c(desc_rows, row_idx + 2L)
          row_idx <- row_idx + 2L
        }
        list(
          table = out,
          label_rows = label_rows,
          desc_rows = desc_rows
        )
      }

      # Extract evaluator's item-level output, summary tables, and meta-info.
      items <- .get_or(mislabel_eval$items, tibble::tibble())
      summary <- .get_or(mislabel_eval_summary, list())
      meta <- .get_or(summary$meta, list())

      # Ensure item-level tibble has all columns (for proper report schema).
      items_template <- tibble::tibble(
        display_id = integer(), # Unique anonymized display record ID
        record_key = character(), # Source record key string
        title = character(), # Publication title
        abstract = character(), # Abstract text
        included = logical(), # Human-assigned in/exclusion (raw)
        divergence_type = character(), # "majority", "consensus", or NA
        mismatch_ratio = character(), # Model disagreement ratio (string)
        reason_code = character(), # Reason for possible mislabel
        suggested_action = character(), # "flag...", "request..." or other
        comment = character(), # Evaluator comments
        evidence = character(), # Textual evidence or rationale
        warning_reasons = character(), # Refiner warnings for reviewer
        parse_error = character(), # Parse warnings, if any
        n_mismatch = integer(), # # disagreeing models
        n_models = integer() # # models total
      )
      items <- dplyr::bind_rows(items, items_template)

      ## Summary Sheet Metadata ----

      # Metadata for top of report (dataset, model etc.).
      meta_tbl <- tibble::tibble(
        field = c("dataset", "evaluator_model", "evaluated"),
        value = c(
          .get_or(meta$dataset, NA_character_),
          .get_or(meta$evaluator_model, NA_character_),
          as.character(.get_or(meta$evaluated, NA_integer_))
        )
      )
      # Definitions for codes used in summary tables.
      divergence_defs <- c(
        consensus = "All models disagree with the human label.",
        majority = "Half or more models disagree with the human label."
      )
      reason_defs <- c(
        criteria_contradiction = paste(
          "Abstract clearly matches or violates ORIGINAL criteria,",
          "but the human label says the opposite."
        ),
        criteria_unclear = paste(
          "ORIGINAL criteria are too vague to decide from the abstract",
          "without an extra rule."
        ),
        model_misread = paste(
          "Model reasoning conflicts with the abstract (hallucinated",
          "details, misread findings, or flipped meaning)."
        ),
        label_inconsistent = paste(
          "Label conflicts with similar items implied by criteria or",
          "model reasoning."
        )
      )
      action_defs <- c(
        flag_mislabel_for_author_review = paste(
          "Likely human label error; author should review the record."
        ),
        request_author_rule_clarification = paste(
          "Criteria ambiguity or inconsistency; author needs to clarify",
          "the rule."
        )
      )
      included_defs <- c(
        relevant = "Human label indicates inclusion.",
        `not relevant` = "Human label indicates exclusion."
      )
      mismatch_ratio_def <- "Number of disagreeing models / total models."

      # Helper: combine label and definition in one wrapped cell.
      .label_with_definition <- function(label, def_map) {
        def_val <- def_map[label]
        if (is.na(def_val) || !nzchar(def_val)) {
          return(label)
        }
        paste0(label, "\n", def_val)
      }

      # Build before/after refinement performance metrics per model.
      refinement_history <- .get_or(refinement_run$history, list())
      .prep_performance <- function(metrics_tbl) {
        if (is.null(metrics_tbl) || !nrow(metrics_tbl)) {
          return(tibble::tibble())
        }
        metrics_tbl |>
          dplyr::mutate(
            sensitivity = dplyr::coalesce(
              .data$recall,
              dplyr::if_else(
                (.data$TP + .data$FN) > 0,
                .data$TP / (.data$TP + .data$FN),
                NA_real_
              )
            ),
            specificity = dplyr::if_else(
              (.data$TN + .data$FP) > 0,
              .data$TN / (.data$TN + .data$FP),
              NA_real_
            ),
            precision = dplyr::coalesce(
              .data$precision,
              dplyr::if_else(
                (.data$TP + .data$FP) > 0,
                .data$TP / (.data$TP + .data$FP),
                NA_real_
              )
            )
          ) |>
          dplyr::select(
            "model",
            "sensitivity",
            "specificity",
            "precision",
            "TP",
            "FP",
            "TN",
            "FN"
          )
      }
      before_metrics <- if (length(refinement_history) >= 1) {
        refinement_history[[1]]$metrics
      } else {
        tibble::tibble()
      }
      after_metrics <- if (length(refinement_history) >= 1) {
        dplyr::last(refinement_history)$metrics
      } else {
        tibble::tibble()
      }
      performance_before_tbl <- .prep_performance(before_metrics)
      performance_after_tbl <- .prep_performance(after_metrics)
      if (nrow(performance_before_tbl)) {
        performance_before_tbl <- performance_before_tbl |>
          dplyr::mutate(
            dplyr::across(
              c("sensitivity", "specificity", "precision"),
              \(x) round(100 * x, 1)
            )
          ) |>
          .format_percent_cols()
      }
      if (nrow(performance_after_tbl)) {
        performance_after_tbl <- performance_after_tbl |>
          dplyr::mutate(
            dplyr::across(
              c("sensitivity", "specificity", "precision"),
              \(x) round(100 * x, 1)
            )
          ) |>
          .format_percent_cols()
      }

      ## Prepare Summary Tables ----

      # Divergence counts formatted for label + definition layout.
      divergence_raw <- .get_or(summary$divergence, tibble::tibble())
      divergence_tbl_info <- .make_label_count_table(
        counts_tbl = divergence_raw,
        label_col = "divergence_type",
        def_map = divergence_defs,
        all_labels = c("consensus", "majority")
      )

      # Reason codes formatted for label + definition layout.
      reason_raw <- .get_or(summary$reason, tibble::tibble())
      reason_tbl_info <- .make_label_count_table(
        counts_tbl = reason_raw,
        label_col = "reason_code",
        def_map = reason_defs
      )

      # Suggested actions formatted for label + definition layout.
      action_raw <- .get_or(summary$action, tibble::tibble())
      action_tbl_info <- .make_label_count_table(
        counts_tbl = action_raw,
        label_col = "suggested_action",
        def_map = action_defs
      )

      # Included distribution formatted for label + definition layout.
      included_raw <- .get_or(summary$included, tibble::tibble())
      if ("included" %in% names(included_raw)) {
        included_raw <- included_raw |>
          dplyr::mutate(
            included = dplyr::case_when(
              .data$included %in% TRUE ~ "Relevant",
              .data$included %in% FALSE ~ "Not relevant",
              TRUE ~ NA_character_
            )
          )
      }
      included_tbl_info <- .make_label_count_table(
        counts_tbl = included_raw,
        label_col = "included",
        def_map = c(
          "Relevant" = included_defs[["relevant"]],
          "Not relevant" = included_defs[["not relevant"]]
        ),
        all_labels = c("Relevant", "Not relevant")
      )

      # Mismatch ratio distribution formatted for label + definition layout.
      mismatch_ratio_raw <- .get_or(summary$mismatch_ratio, tibble::tibble())
      mismatch_ratio_labels <- if (
        "mismatch_ratio" %in% names(mismatch_ratio_raw)
      ) {
        mismatch_ratio_raw$mismatch_ratio
      } else {
        character()
      }
      mismatch_ratio_defs <- setNames(
        rep(mismatch_ratio_def, length(mismatch_ratio_labels)),
        mismatch_ratio_labels
      )
      mismatch_ratio_tbl_info <- .make_label_count_table(
        counts_tbl = mismatch_ratio_raw,
        label_col = "mismatch_ratio",
        def_map = mismatch_ratio_defs
      )

      # Cross-tab summaries (n with percents merged into one column).
      reason_by_divergence_tbl <- .sort_by_first_col(.combine_count_percent(
        .format_percent_cols(.get_or(
          summary$reason_by_divergence,
          tibble::tibble()
        ))
      ))
      action_by_divergence_tbl <- .sort_by_first_col(.combine_count_percent(
        .format_percent_cols(.get_or(
          summary$action_by_divergence,
          tibble::tibble()
        ))
      ))
      reason_by_included_tbl <- .sort_by_first_col(.combine_count_percent(
        .format_percent_cols(.get_or(
          summary$reason_by_included,
          tibble::tibble()
        ))
      ))
      action_by_included_tbl <- .sort_by_first_col(.combine_count_percent(
        .format_percent_cols(.get_or(
          summary$action_by_included,
          tibble::tibble()
        ))
      ))
      reason_by_action_tbl <- .sort_by_first_col(.combine_count_percent(
        .format_percent_cols(.get_or(
          summary$reason_by_action,
          tibble::tibble()
        ))
      ))

      # Stats for mean/median disagreement fraction by reason
      mismatch_by_reason_tbl <- .get_or(
        summary$mismatch_by_reason,
        tibble::tibble()
      )
      mismatch_by_reason_template <- tibble::tibble(
        reason_code = character(),
        mismatch_fraction_mean = double(),
        mismatch_fraction_median = double()
      )
      mismatch_by_reason_tbl <- dplyr::bind_rows(
        mismatch_by_reason_tbl,
        mismatch_by_reason_template
      ) |>
        dplyr::mutate(
          mismatch_fraction_mean_pct = round(
            100 * .data$mismatch_fraction_mean,
            1
          ),
          mismatch_fraction_median_pct = round(
            100 * .data$mismatch_fraction_median,
            1
          )
        ) |>
        dplyr::select(
          "reason_code",
          "mismatch_fraction_mean_pct",
          "mismatch_fraction_median_pct"
        ) |>
        .format_percent_cols() |>
        .sort_by_first_col()

      ## Corrections Table (Candidate Mislabels for Review) ----

      # Compose one record per item including all context fields
      corrections_tbl <- items |>
        dplyr::mutate(
          human_label = dplyr::case_when(
            .data$included %in% TRUE ~ "relevant",
            .data$included %in% FALSE ~ "not relevant",
            TRUE ~ NA_character_
          ),
          warning_reasons = dplyr::coalesce(.data$warning_reasons, "")
        ) |>
        dplyr::select(
          "display_id",
          "record_key",
          "title",
          "abstract",
          "human_label", # reader-friendly version of 'included'
          "divergence_type",
          "mismatch_ratio",
          "reason_code",
          "suggested_action",
          "comment",
          "evidence",
          "warning_reasons"
        )

      # Drop display_id when any records are missing it; keep record_key
      # instead.
      has_missing_display_id <- any(is.na(corrections_tbl$display_id))
      if (has_missing_display_id) {
        corrections_tbl <- corrections_tbl |>
          dplyr::select(
            "record_key",
            "title",
            "abstract",
            "human_label",
            "divergence_type",
            "mismatch_ratio",
            "reason_code",
            "suggested_action",
            "comment",
            "evidence",
            "warning_reasons"
          )
      } else {
        corrections_tbl <- corrections_tbl |>
          dplyr::select(
            "display_id",
            "title",
            "abstract",
            "human_label",
            "divergence_type",
            "mismatch_ratio",
            "reason_code",
            "suggested_action",
            "comment",
            "evidence",
            "warning_reasons"
          )
      }

      # Add columns for reviewer input.
      corrections_tbl <- corrections_tbl |>
        dplyr::mutate(
          human_relabel = "",
          human_justification = ""
        )

      ## Write XLSX Output ----

      # Create a new Excel workbook, add two sheets: "Summary" and "Corrections"
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Summary")
      openxlsx::addWorksheet(wb, "Corrections")
      openxlsx::addWorksheet(wb, "Original criteria")
      openxlsx::addWorksheet(wb, "Refined criteria")

      # Define styling for section titles and header rows.
      title_style <- openxlsx::createStyle(
        textDecoration = "bold",
        fontSize = 12
      )
      header_style <- openxlsx::createStyle(textDecoration = "bold")
      subtitle_style <- openxlsx::createStyle(textDecoration = "bold")
      wrap_style <- openxlsx::createStyle(wrapText = TRUE)
      field_style <- openxlsx::createStyle(textDecoration = "bold")
      italic_style <- openxlsx::createStyle(textDecoration = "italic")

      # Utility: Write criteria text into a dedicated sheet.
      .write_criteria_sheet <- function(sheet, title, criteria) {
        include_text <- criteria$include %||% ""
        exclude_text <- criteria$exclude %||% ""
        openxlsx::writeData(
          wb,
          sheet,
          title,
          startRow = 1,
          startCol = 1,
          colNames = FALSE
        )
        openxlsx::addStyle(
          wb,
          sheet,
          title_style,
          rows = 1,
          cols = 1,
          gridExpand = TRUE
        )
        openxlsx::writeData(
          wb,
          sheet,
          "Inclusion criteria",
          startRow = 3,
          startCol = 1,
          colNames = FALSE
        )
        openxlsx::addStyle(
          wb,
          sheet,
          header_style,
          rows = 3,
          cols = 1,
          gridExpand = TRUE
        )
        openxlsx::writeData(
          wb,
          sheet,
          include_text,
          startRow = 4,
          startCol = 1,
          colNames = FALSE
        )
        openxlsx::addStyle(
          wb,
          sheet,
          wrap_style,
          rows = 4,
          cols = 1,
          gridExpand = TRUE,
          stack = TRUE
        )
        openxlsx::writeData(
          wb,
          sheet,
          "Exclusion criteria",
          startRow = 6,
          startCol = 1,
          colNames = FALSE
        )
        openxlsx::addStyle(
          wb,
          sheet,
          header_style,
          rows = 6,
          cols = 1,
          gridExpand = TRUE
        )
        openxlsx::writeData(
          wb,
          sheet,
          exclude_text,
          startRow = 7,
          startCol = 1,
          colNames = FALSE
        )
        openxlsx::addStyle(
          wb,
          sheet,
          wrap_style,
          rows = 7,
          cols = 1,
          gridExpand = TRUE,
          stack = TRUE
        )
      }

      # Utility: Write a titled table (section) at a given row, return row after
      # the section.
      .write_section <- function(
        title,
        explanation,
        tbl,
        start_row,
        title_style,
        header_style,
        wrap_style
      ) {
        if (is.null(tbl) || !nrow(tbl)) {
          return(start_row)
        }
        openxlsx::writeData(
          wb,
          "Summary",
          title,
          startRow = start_row,
          startCol = 1,
          colNames = FALSE
        )
        openxlsx::addStyle(
          wb,
          "Summary",
          title_style,
          rows = start_row,
          cols = 1,
          gridExpand = TRUE
        )
        openxlsx::writeData(
          wb,
          "Summary",
          explanation,
          startRow = start_row + 1,
          startCol = 1,
          colNames = FALSE
        )
        openxlsx::writeData(
          wb,
          "Summary",
          tbl,
          startRow = start_row + 2,
          startCol = 1
        )
        openxlsx::addStyle(
          wb,
          "Summary",
          header_style,
          rows = start_row + 2,
          cols = seq_len(ncol(tbl)),
          gridExpand = TRUE
        )
        openxlsx::addStyle(
          wb,
          "Summary",
          wrap_style,
          rows = (start_row + 2):(start_row + 2 + nrow(tbl)),
          cols = seq_len(ncol(tbl)),
          gridExpand = TRUE,
          stack = TRUE
        )
        start_row + nrow(tbl) + 4
      }

      # Utility: Write label/definition rows with counts and custom styles.
      .write_label_count_section <- function(
        title,
        explanation,
        table_info,
        start_row,
        title_style,
        label_style,
        italic_style,
        wrap_style
      ) {
        tbl <- table_info$table
        if (is.null(tbl) || !nrow(tbl)) {
          return(start_row)
        }
        openxlsx::writeData(
          wb,
          "Summary",
          title,
          startRow = start_row,
          startCol = 1,
          colNames = FALSE
        )
        openxlsx::addStyle(
          wb,
          "Summary",
          title_style,
          rows = start_row,
          cols = 1,
          gridExpand = TRUE
        )
        openxlsx::writeData(
          wb,
          "Summary",
          explanation,
          startRow = start_row + 1,
          startCol = 1,
          colNames = FALSE
        )
        table_start <- start_row + 2
        openxlsx::writeData(
          wb,
          "Summary",
          tbl,
          startRow = table_start,
          startCol = 1,
          colNames = FALSE
        )
        label_rows <- table_start - 1 + table_info$label_rows
        desc_rows <- table_start - 1 + table_info$desc_rows
        openxlsx::addStyle(
          wb,
          "Summary",
          label_style,
          rows = label_rows,
          cols = 1,
          gridExpand = TRUE
        )
        openxlsx::addStyle(
          wb,
          "Summary",
          italic_style,
          rows = desc_rows,
          cols = 1,
          gridExpand = TRUE
        )
        openxlsx::addStyle(
          wb,
          "Summary",
          wrap_style,
          rows = table_start:(table_start + nrow(tbl) - 1),
          cols = 1,
          gridExpand = TRUE,
          stack = TRUE
        )
        start_row + nrow(tbl) + 3
      }

      # Write performance table with pre/post subtables in one section.
      .write_performance_section <- function(
        title,
        explanation,
        pre_tbl,
        post_tbl,
        start_row,
        title_style,
        subtitle_style,
        header_style,
        wrap_style
      ) {
        if (is.null(pre_tbl) || !nrow(pre_tbl)) {
          pre_tbl <- tibble::tibble()
        }
        if (is.null(post_tbl) || !nrow(post_tbl)) {
          post_tbl <- tibble::tibble()
        }
        if (!nrow(pre_tbl) && !nrow(post_tbl)) {
          return(start_row)
        }
        openxlsx::writeData(
          wb,
          "Summary",
          title,
          startRow = start_row,
          startCol = 1,
          colNames = FALSE
        )
        openxlsx::addStyle(
          wb,
          "Summary",
          title_style,
          rows = start_row,
          cols = 1,
          gridExpand = TRUE
        )
        openxlsx::writeData(
          wb,
          "Summary",
          explanation,
          startRow = start_row + 1,
          startCol = 1,
          colNames = FALSE
        )
        current <- start_row + 2
        if (nrow(pre_tbl)) {
          openxlsx::writeData(
            wb,
            "Summary",
            "Pre-refinement",
            startRow = current,
            startCol = 1,
            colNames = FALSE
          )
          openxlsx::addStyle(
            wb,
            "Summary",
            subtitle_style,
            rows = current,
            cols = 1,
            gridExpand = TRUE
          )
          openxlsx::writeData(
            wb,
            "Summary",
            pre_tbl,
            startRow = current + 1,
            startCol = 1
          )
          openxlsx::addStyle(
            wb,
            "Summary",
            header_style,
            rows = current + 1,
            cols = seq_len(ncol(pre_tbl)),
            gridExpand = TRUE
          )
          openxlsx::addStyle(
            wb,
            "Summary",
            wrap_style,
            rows = (current + 1):(current + 1 + nrow(pre_tbl)),
            cols = seq_len(ncol(pre_tbl)),
            gridExpand = TRUE,
            stack = TRUE
          )
          current <- current + nrow(pre_tbl) + 3
        }
        if (nrow(post_tbl)) {
          openxlsx::writeData(
            wb,
            "Summary",
            "Post-refinement",
            startRow = current,
            startCol = 1,
            colNames = FALSE
          )
          openxlsx::addStyle(
            wb,
            "Summary",
            subtitle_style,
            rows = current,
            cols = 1,
            gridExpand = TRUE
          )
          openxlsx::writeData(
            wb,
            "Summary",
            post_tbl,
            startRow = current + 1,
            startCol = 1
          )
          openxlsx::addStyle(
            wb,
            "Summary",
            header_style,
            rows = current + 1,
            cols = seq_len(ncol(post_tbl)),
            gridExpand = TRUE
          )
          openxlsx::addStyle(
            wb,
            "Summary",
            wrap_style,
            rows = (current + 1):(current + 1 + nrow(post_tbl)),
            cols = seq_len(ncol(post_tbl)),
            gridExpand = TRUE,
            stack = TRUE
          )
          current <- current + nrow(post_tbl) + 2
        }
        current + 1
      }

      # Utility: Write a field/value table without column headers.
      .write_field_value_section <- function(
        title,
        explanation,
        tbl,
        start_row,
        title_style,
        field_style
      ) {
        if (is.null(tbl) || !nrow(tbl)) {
          return(start_row)
        }
        openxlsx::writeData(
          wb,
          "Summary",
          title,
          startRow = start_row,
          startCol = 1,
          colNames = FALSE
        )
        openxlsx::addStyle(
          wb,
          "Summary",
          title_style,
          rows = start_row,
          cols = 1,
          gridExpand = TRUE
        )
        openxlsx::writeData(
          wb,
          "Summary",
          explanation,
          startRow = start_row + 1,
          startCol = 1,
          colNames = FALSE
        )
        openxlsx::writeData(
          wb,
          "Summary",
          tbl,
          startRow = start_row + 2,
          startCol = 1,
          colNames = FALSE
        )
        openxlsx::addStyle(
          wb,
          "Summary",
          field_style,
          rows = (start_row + 2):(start_row + 1 + nrow(tbl)),
          cols = 1,
          gridExpand = TRUE
        )
        start_row + nrow(tbl) + 3
      }

      # Write summary sections in sequence.
      current_row <- 1
      current_row <- .write_field_value_section(
        "Report metadata",
        paste(
          "Dataset and evaluator metadata for this run so readers can",
          "reproduce and compare results across datasets."
        ),
        meta_tbl,
        current_row,
        title_style,
        field_style
      )
      current_row <- .write_performance_section(
        "Performance",
        paste(
          "Per-model screening performance before and after criteria",
          "refinement. The refiner updates inclusion/exclusion rules to",
          "reduce mismatches while preserving recall."
        ),
        performance_before_tbl,
        performance_after_tbl,
        current_row,
        title_style,
        subtitle_style,
        header_style,
        wrap_style
      )
      current_row <- .write_label_count_section(
        "Divergence counts",
        paste(
          "Breakdown of majority/consensus disagreements between models",
          "and human labels. Use this to gauge disagreement severity."
        ),
        divergence_tbl_info,
        current_row,
        title_style,
        field_style,
        italic_style,
        wrap_style
      )
      current_row <- .write_label_count_section(
        "Reason codes",
        paste(
          "Evaluator agent labels explaining why disagreements occur,",
          "using only abstracts, model reasoning, and criteria. The",
          "definitions below explain each code in plain language."
        ),
        reason_tbl_info,
        current_row,
        title_style,
        field_style,
        italic_style,
        wrap_style
      )
      current_row <- .write_label_count_section(
        "Suggested actions",
        paste(
          "What the evaluator recommends the authors do for each type",
          "of mismatch. Definitions clarify when to review labels vs",
          "clarify criteria."
        ),
        action_tbl_info,
        current_row,
        title_style,
        field_style,
        italic_style,
        wrap_style
      )
      current_row <- .write_label_count_section(
        "Included distribution",
        paste(
          "Distribution of the human gold-standard labels among the",
          "evaluated mismatches."
        ),
        included_tbl_info,
        current_row,
        title_style,
        field_style,
        italic_style,
        wrap_style
      )
      current_row <- .write_label_count_section(
        "Mismatch ratio distribution",
        paste(
          "How many models disagree with the human label per record.",
          "Ratios are shown as counts with percentages."
        ),
        mismatch_ratio_tbl_info,
        current_row,
        title_style,
        field_style,
        italic_style,
        wrap_style
      )
      summary_sections <- list(
        list(
          title = "Reason by divergence",
          explanation = paste(
            "Reason codes broken down by divergence severity to show",
            "whether contradictions or unclear criteria dominate."
          ),
          table = reason_by_divergence_tbl
        ),
        list(
          title = "Action by divergence",
          explanation = paste(
            "Suggested actions broken down by divergence severity to",
            "prioritize reviewer effort."
          ),
          table = action_by_divergence_tbl
        ),
        list(
          title = "Reason by human label",
          explanation = paste(
            "Reason codes split by human inclusion/exclusion to highlight",
            "where criteria conflicts are more common."
          ),
          table = reason_by_included_tbl
        ),
        list(
          title = "Action by human label",
          explanation = paste(
            "Suggested actions split by human inclusion/exclusion.",
            "Helps target reviewer follow-up."
          ),
          table = action_by_included_tbl
        ),
        list(
          title = "Reason by action",
          explanation = paste(
            "Reason codes summarized within each suggested action.",
            "Use this to see which issues drive each action type."
          ),
          table = reason_by_action_tbl
        ),
        list(
          title = "Mismatch by reason",
          explanation = paste(
            "Average disagreement fraction by reason code. Higher values",
            "indicate stronger model consensus against the human label."
          ),
          table = mismatch_by_reason_tbl
        )
      )
      for (section in summary_sections) {
        current_row <- .write_section(
          section$title,
          section$explanation,
          section$table,
          current_row,
          title_style,
          header_style,
          wrap_style
        )
      }

      # Write candidate mislabels/corrections table to the "Corrections" sheet.
      openxlsx::writeData(
        wb,
        "Corrections",
        "Corrections",
        startRow = 1,
        startCol = 1,
        colNames = FALSE
      )
      openxlsx::addStyle(
        wb,
        "Corrections",
        title_style,
        rows = 1,
        cols = 1,
        gridExpand = TRUE
      )
      openxlsx::writeData(
        wb,
        "Corrections",
        paste(
          "Review each record using the abstract and evaluator notes.",
          "Fill in human_relabel and human_justification for any changes."
        ),
        startRow = 2,
        startCol = 1,
        colNames = FALSE
      )
      openxlsx::writeData(
        wb,
        "Corrections",
        corrections_tbl,
        startRow = 3,
        startCol = 1
      )
      if (nrow(corrections_tbl)) {
        openxlsx::addStyle(
          wb,
          "Corrections",
          header_style,
          rows = 3,
          cols = seq_len(ncol(corrections_tbl)),
          gridExpand = TRUE
        )
        openxlsx::addStyle(
          wb,
          "Corrections",
          wrap_style,
          rows = 3:(3 + nrow(corrections_tbl)),
          cols = seq_len(ncol(corrections_tbl)),
          gridExpand = TRUE,
          stack = TRUE
        )
      }

      # Write criteria sheets.
      .write_criteria_sheet(
        "Original criteria",
        "Original criteria",
        selection_criteria[[dataset_name]]
      )
      .write_criteria_sheet(
        "Refined criteria",
        "Refined criteria",
        refinement_run$final %||% list(include = "", exclude = "")
      )

      # Expand all columns (up to 20 each) for visibility in both sheets.
      openxlsx::setColWidths(wb, "Summary", cols = 1:20, widths = "auto")
      openxlsx::setColWidths(wb, "Corrections", cols = 1:20, widths = "auto")
      openxlsx::setColWidths(
        wb,
        "Original criteria",
        cols = 1:5,
        widths = "auto"
      )
      openxlsx::setColWidths(
        wb,
        "Refined criteria",
        cols = 1:5,
        widths = "auto"
      )

      # Write the workbook to the output Excel file (overwriting any old
      # report).
      openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)

      # Return output path so this can be targeted by 'tar_target' as a file.
      output_path
    },
    format = "file"
  )
)

# Corrected analysis targets ----
analyse_targets <- c(
  analyse_targets,
  list(
    tar_combine(
      corrected_performance_all,
      mapped_refinement$corrected_performance,
      command = {
        dplyr::bind_rows(!!!.x)
      }
    ),
    tar_target(
      pareto_frontier_corrected,
      {
        # Define the baseline and corrected series for plotting.
        series_defs <- list(
          Baseline = analysis_results,
          Corrected = corrected_performance_all
        )
        series_order <- names(series_defs)

        # Combine per-series points for plotting.
        points_data <- purrr::map(
          series_order,
          function(series_name) {
            series_defs[[series_name]] |>
              dplyr::mutate(series = series_name)
          }
        ) |>
          dplyr::bind_rows() |>
          dplyr::filter(!is.na(.data$precision) & !is.na(.data$recall)) |>
          dplyr::mutate(
            series = factor(.data$series, levels = series_order)
          )

        # Build per-model arrows from baseline to corrected.
        arrow_data <- points_data |>
          dplyr::select(
            "dataset",
            "model",
            "series",
            "recall",
            "precision"
          ) |>
          tidyr::pivot_wider(
            names_from = "series",
            values_from = c("recall", "precision")
          ) |>
          dplyr::filter(
            !is.na(.data$recall_Baseline) &
              !is.na(.data$recall_Corrected) &
              !is.na(.data$precision_Baseline) &
              !is.na(.data$precision_Corrected)
          )

        # Build the Pareto frontier per dataset for each series.
        frontier_data <- purrr::map(
          series_order,
          function(series_name) {
            series_tbl <- series_defs[[series_name]] |>
              dplyr::filter(
                !is.na(.data$precision) & !is.na(.data$recall)
              ) |>
              dplyr::mutate(
                series = series_name,
                recall = 1 - .data$recall
              )

            if (!nrow(series_tbl)) {
              return(tibble::tibble())
            }

            split(series_tbl, series_tbl$dataset) |>
              purrr::map(\(dataset_tbl) {
                dataset_tbl |>
                  pareto_subset("recall", "precision")
              }) |>
              dplyr::bind_rows() |>
              dplyr::mutate(
                recall = 1 - .data$recall,
                series = factor(.data$series, levels = series_order)
              )
          }
        ) |>
          dplyr::bind_rows()

        # Create the overlay plot with both frontiers and points.
        p <- ggplot2::ggplot(points_data) +
          ggplot2::aes(
            x = .data$recall,
            y = .data$precision,
            color = .data$series,
            shape = .data$series
          ) +
          ggplot2::annotate(
            "segment",
            x = 0,
            y = 1,
            xend = 1,
            yend = 0,
            linetype = "dashed"
          ) +
          ggplot2::geom_line(
            data = frontier_data,
            ggplot2::aes(
              x = .data$recall,
              y = .data$precision,
              linetype = .data$series,
              group = interaction(.data$dataset, .data$series)
            ),
            linewidth = 1,
            alpha = 0.5
          ) +
          ggplot2::geom_segment(
            data = arrow_data,
            ggplot2::aes(
              x = .data$recall_Baseline,
              y = .data$precision_Baseline,
              xend = .data$recall_Corrected,
              yend = .data$precision_Corrected
            ),
            arrow = grid::arrow(length = grid::unit(0.08, "inches")),
            color = "grey50",
            linewidth = 0.6,
            inherit.aes = FALSE
          ) +
          ggplot2::geom_point(alpha = 0.7) +
          ggplot2::facet_wrap(~dataset, scales = "free") +
          ggplot2::scale_color_manual(values = c(
            Baseline = "grey40",
            Corrected = "steelblue"
          )) +
          ggplot2::scale_linetype_manual(values = c(
            Baseline = "solid",
            Corrected = "solid"
          )) +
          ggplot2::scale_shape_manual(values = c(
            Baseline = 16,
            Corrected = 17
          )) +
          ggplot2::labs(
            title = "Precision-Recall Frontier: Baseline vs Corrected",
            color = "Series",
            shape = "Series",
            linetype = "Series"
          ) +
          ggplot2::theme_minimal()

        # Return both frontier data and plot for downstream use.
        list(
          frontier_models = frontier_data,
          plot = p
        )
      }
    )
  )
)

c(setup_targets, process_targets, analyse_targets, mapped_refinement)
