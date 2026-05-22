# Official target graph for the abstract concentration track.

library(targets)
library(tarchetypes)

# Source package helpers so this script stays orchestration-only.
tar_source("R")

# Load local API keys before any model or cache configuration is read.
readRenviron(".Renviron")

config <- list(
  data_dir = here::here("data"),
  cache_root = here::here("cache"),
  output_dir = here::here(
    "method_pipelines",
    "abstract_concentration",
    "outputs"
  ),
  seed_model = "google/gemini-3.1-pro-preview",
  n_positive = 10L,
  n_negative = 10L,
  embedding_batch_size = 64L
)

profile_grid <- tibble::tibble(
  embedding_profile = c(
    "cohere_query_document",
    "cohere_document_document",
    "gemini_query_document",
    "gemini_document_document",
    "gemini_2_preview_query_document",
    "gemini_2_preview_document_document",
    "perplexity_0.6b",
    "perplexity_4b"
  ),
  document_embedding_model = c(
    "cohere/embed-v4.0",
    "cohere/embed-v4.0",
    "google/gemini-embedding-001",
    "google/gemini-embedding-001",
    "google/gemini-embedding-2-preview",
    "google/gemini-embedding-2-preview",
    "perplexity/pplx-embed-v1-0.6b",
    "perplexity/pplx-embed-v1-4b"
  ),
  document_embedding_mode = c(
    "document",
    "document",
    "document",
    "document",
    "document",
    "document",
    "document",
    "document"
  ),
  seed_embedding_model = c(
    "cohere/embed-v4.0",
    "cohere/embed-v4.0",
    "google/gemini-embedding-001",
    "google/gemini-embedding-001",
    "google/gemini-embedding-2-preview",
    "google/gemini-embedding-2-preview",
    "perplexity/pplx-embed-v1-0.6b",
    "perplexity/pplx-embed-v1-4b"
  ),
  seed_embedding_mode = c(
    "query",
    "document",
    "query",
    "document",
    "query",
    "document",
    "document",
    "document"
  )
)

dataset_grid <- get_datasets(
  data_dir = config$data_dir
)

base_methods <- tibble::tibble(
  ranking_method = c(
    "positive_mean",
    "positive_closest",
    "positive_centroid",
    "contrastive_mean_l0p25",
    "contrastive_mean_l0p5",
    "contrastive_mean_l1",
    "contrastive_closest_l0p25",
    "contrastive_closest_l0p5",
    "contrastive_closest_l1",
    "contrastive_centroid_l0p25",
    "contrastive_centroid_l0p5",
    "contrastive_centroid_l1",
    "centroid_contrastive_weighted"
  ),
  method_function = c(
    "rank_embeddings_positive_mean",
    "rank_embeddings_positive_closest",
    "rank_embeddings_positive_centroid",
    "rank_embeddings_contrastive_mean",
    "rank_embeddings_contrastive_mean",
    "rank_embeddings_contrastive_mean",
    "rank_embeddings_contrastive_closest",
    "rank_embeddings_contrastive_closest",
    "rank_embeddings_contrastive_closest",
    "rank_embeddings_contrastive_centroid",
    "rank_embeddings_contrastive_centroid",
    "rank_embeddings_contrastive_centroid",
    "rank_embeddings_centroid_contrastive_weighted"
  ),
  method_args = list(
    list(),
    list(closest_k = 3L),
    list(),
    list(lambda = 0.25),
    list(lambda = 0.5),
    list(lambda = 1),
    list(lambda = 0.25, closest_k = 3L),
    list(lambda = 0.5, closest_k = 3L),
    list(lambda = 1, closest_k = 3L),
    list(lambda = 0.25),
    list(lambda = 0.5),
    list(lambda = 1),
    list(lambdas = c(0.25, 0.5, 1))
  )
)

criteria_score_args <- list(
  positive_mean = list(score_name = "positive_mean"),
  positive_closest = list(score_name = "positive_closest", closest_k = 3L),
  positive_centroid = list(score_name = "positive_centroid"),
  contrastive_mean_l0p25 = list(
    score_name = "contrastive_mean_l0p25",
    lambda = 0.25
  ),
  contrastive_mean_l0p5 = list(
    score_name = "contrastive_mean_l0p5",
    lambda = 0.5
  ),
  contrastive_mean_l1 = list(score_name = "contrastive_mean_l1", lambda = 1),
  contrastive_closest_l0p25 = list(
    score_name = "contrastive_closest_l0p25",
    lambda = 0.25,
    closest_k = 3L
  ),
  contrastive_closest_l0p5 = list(
    score_name = "contrastive_closest_l0p5",
    lambda = 0.5,
    closest_k = 3L
  ),
  contrastive_closest_l1 = list(
    score_name = "contrastive_closest_l1",
    lambda = 1,
    closest_k = 3L
  ),
  contrastive_centroid_l0p25 = list(
    score_name = "contrastive_centroid_l0p25",
    lambda = 0.25
  ),
  contrastive_centroid_l0p5 = list(
    score_name = "contrastive_centroid_l0p5",
    lambda = 0.5
  ),
  contrastive_centroid_l1 = list(
    score_name = "contrastive_centroid_l1",
    lambda = 1
  ),
  centroid_contrastive_weighted = list(
    score_name = "centroid_contrastive_weighted"
  )
)

method_grid <- dplyr::bind_rows(
  base_methods,
  tibble::tibble(
    ranking_method = paste(
      "criteria_item",
      names(criteria_score_args),
      sep = "_"
    ),
    method_function = "rank_embeddings_criteria_item",
    method_args = unname(criteria_score_args)
  ),
  tibble::tibble(
    ranking_method = paste(
      "criteria_block",
      names(criteria_score_args),
      sep = "_"
    ),
    method_function = "rank_embeddings_criteria_block",
    method_args = unname(criteria_score_args)
  ),
  tibble::tibble(
    ranking_method = c(
      "positive_seed_centroid_negative_criteria_l0p25",
      "positive_seed_centroid_negative_criteria_l0p5",
      "positive_seed_centroid_negative_criteria_l1"
    ),
    method_function = "rank_embeddings_positive_seed_negative_criteria",
    method_args = list(
      list(lambda = 0.25),
      list(lambda = 0.5),
      list(lambda = 1)
    )
  )
)

target_packages <- c("dplyr", "fs", "here", "purrr", "readr", "tibble")
worker_count <- min(
  max(1L, parallel::detectCores(logical = TRUE) - 1L),
  nrow(profile_grid)
)
use_parallel <- worker_count > 1L &&
  isTRUE(as.logical(Sys.getenv("ENABLE_PIPELINE_PARALLELIZATION", "TRUE")))

tar_option_set(
  packages = target_packages,
  controller = if (use_parallel) {
    crew::crew_controller_local(workers = worker_count)
  } else {
    NULL
  }
)

profile_targets <- tar_map(
  values = profile_grid,
  names = embedding_profile,
  tar_target(
    profile_results,
    run_abstract_concentration_profile(
      embedding_profile = embedding_profile,
      document_embedding_model = document_embedding_model,
      document_embedding_mode = document_embedding_mode,
      seed_embedding_model = seed_embedding_model,
      seed_embedding_mode = seed_embedding_mode,
      datasets = dataset_grid,
      method_grid = method_grid,
      data_dir = config$data_dir,
      cache_root = config$cache_root,
      seed_model = config$seed_model,
      n_positive = config$n_positive,
      n_negative = config$n_negative,
      embedding_batch_size = config$embedding_batch_size
    )
  )
)

list(
  profile_targets,
  tar_combine(
    metrics,
    profile_targets,
    command = combine_abstract_concentration_results(list(!!!.x))
  ),
  tar_target(
    model_fit,
    fit_abstract_concentration_model(metrics)
  ),
  tar_target(
    marginal_summaries,
    get_abstract_concentration_marginal_summaries(model_fit)
  ),
  tar_target(
    outputs,
    write_abstract_concentration_outputs(
      metrics = metrics,
      marginal_summaries = marginal_summaries,
      output_dir = config$output_dir
    )
  )
)
