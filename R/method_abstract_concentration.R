#' Discover packaged benchmark datasets
#'
#' @param data_dir Directory that contains the packaged `.rda` files.
#'
#' @return A tibble with dataset names and paired data/criteria paths.
#'
#' @keywords internal
#' @noRd
get_datasets <- function(data_dir = "data") {
  if (!fs::dir_exists(data_dir)) {
    cli::cli_abort("Data directory {.path {data_dir}} does not exist.")
  }

  criteria_files <- fs::dir_ls(
    data_dir,
    glob = "*_criteria.rda",
    recurse = FALSE
  )
  dataset_names <- sub(
    "_criteria$",
    "",
    fs::path_ext_remove(fs::path_file(criteria_files))
  )
  data_files <- fs::path(data_dir, paste0(dataset_names, ".rda"))

  tibble::tibble(
    dataset = dataset_names,
    data_path = data_files,
    criteria_path = criteria_files
  ) |>
    dplyr::filter(fs::file_exists(.data$data_path)) |>
    dplyr::arrange(.data$dataset)
}

#' Load one packaged benchmark dataset and its criteria
#'
#' @param dataset Dataset label.
#' @param data_dir Directory containing packaged `.rda` files.
#'
#' @return A list with `data` and `criteria`.
#'
#' @keywords internal
#' @noRd
get_dataset <- function(dataset, data_dir = "data") {
  if (!rlang::is_string(dataset) || !nzchar(dataset)) {
    cli::cli_abort("{.arg dataset} must be a single non-empty string.")
  }

  data_path <- fs::path(data_dir, paste0(dataset, ".rda"))
  criteria_path <- fs::path(data_dir, paste0(dataset, "_criteria.rda"))

  if (!fs::file_exists(data_path)) {
    cli::cli_abort("Dataset file {.path {data_path}} does not exist.")
  }
  if (!fs::file_exists(criteria_path)) {
    cli::cli_abort("Criteria file {.path {criteria_path}} does not exist.")
  }

  data_env <- new.env(parent = emptyenv())
  load(data_path, envir = data_env)
  data <- data_env[[dataset]]
  if (is.null(data)) {
    cli::cli_abort(
      "Dataset object {.val {dataset}} was not found in {.path {data_path}}."
    )
  }

  criteria_env <- new.env(parent = emptyenv())
  load(criteria_path, envir = criteria_env)
  criteria_name <- paste0(dataset, "_criteria")
  criteria <- criteria_env[[criteria_name]]
  if (is.null(criteria)) {
    cli::cli_abort(
      "Criteria object {.val {criteria_name}} was not found in {.path {criteria_path}}."
    )
  }

  list(
    data = tibble::as_tibble(data),
    criteria = criteria
  )
}

#' Build document text for embedding
#'
#' @param data Data frame with the requested fields.
#' @param fields Character vector of fields to stitch together.
#'
#' @return A character vector of rendered document texts.
#'
#' @keywords internal
#' @noRd
make_document_text <- function(
  data,
  fields = c("title", "abstract", "keywords")
) {
  missing <- setdiff(fields, names(data))
  if (length(missing)) {
    cli::cli_abort("Document data is missing columns {.field {missing}}.")
  }

  purrr::map_chr(
    seq_len(nrow(data)),
    function(i) {
      parts <- lapply(fields, function(field) {
        value <- data[[field]][[i]]
        value <- if (is.null(value) || is.na(value)) "" else as.character(value)
        paste0(stringr::str_to_title(field), ":\n", value)
      })
      paste(parts, collapse = "\n\n")
    }
  )
}

#' Validate a seed bank
#'
#' @param seed_bank Data frame of seed abstracts.
#' @param n_positive Expected number of positive seeds, or `NULL`.
#' @param n_negative Expected number of negative seeds, or `NULL`.
#'
#' @return A normalized tibble with stable columns.
#'
#' @keywords internal
#' @noRd
validate_seed_bank <- function(
  seed_bank,
  n_positive = NULL,
  n_negative = NULL
) {
  required <- c("pair_id", "seed_type", "text")
  missing <- setdiff(required, names(seed_bank))
  if (length(missing)) {
    cli::cli_abort("Seed bank is missing columns {.field {missing}}.")
  }

  out <- tibble::as_tibble(seed_bank) |>
    dplyr::mutate(
      pair_id = as.integer(.data$pair_id),
      seed_type = stringr::str_to_lower(
        stringr::str_squish(as.character(.data$seed_type))
      ),
      seed_type = dplyr::case_when(
        .data$seed_type %in% c("positive", "included", "include", "relevant") ~
          "positive",
        .data$seed_type %in% c("negative", "excluded", "exclude", "irrelevant") ~
          "negative",
        stringr::str_starts(.data$seed_type, "positive") ~ "positive",
        stringr::str_starts(.data$seed_type, "negative") ~ "negative",
        TRUE ~ .data$seed_type
      ),
      text = stringr::str_squish(as.character(.data$text))
    ) |>
    dplyr::mutate(seed_id = dplyr::row_number(), .before = 1L) |>
    dplyr::select("seed_id", "seed_type", "pair_id", "text")

  if (any(!out$seed_type %in% c("positive", "negative"))) {
    cli::cli_abort(
      "{.field seed_type} must contain only positive or negative values."
    )
  }
  if (any(!nzchar(out$text))) {
    cli::cli_abort("Seed bank contains empty seed text.")
  }
  if (!is.null(n_positive) && sum(out$seed_type == "positive") != n_positive) {
    cli::cli_abort("Seed bank does not contain {n_positive} positive seeds.")
  }
  if (!is.null(n_negative) && sum(out$seed_type == "negative") != n_negative) {
    cli::cli_abort("Seed bank does not contain {n_negative} negative seeds.")
  }

  out
}

#' Build a criteria seed bank
#'
#' @param criteria Criteria object with `include` and `exclude` fields.
#' @param level Whether to build item-level or block-level criteria seeds.
#'
#' @return A validated seed bank tibble.
#'
#' @keywords internal
#' @noRd
make_criteria_seed_bank <- function(criteria, level = c("item", "block")) {
  level <- rlang::arg_match(level)

  include_items <- criteria$include
  exclude_items <- criteria$exclude
  if (is.null(include_items)) {
    include_items <- character()
  }
  if (is.null(exclude_items)) {
    exclude_items <- character()
  }

  include_items <- trimws(as.character(include_items))
  exclude_items <- trimws(as.character(exclude_items))
  include_items <- include_items[nzchar(include_items)]
  exclude_items <- exclude_items[nzchar(exclude_items)]

  if (identical(level, "block")) {
    out <- tibble::tibble(
      pair_id = c(NA_integer_, NA_integer_),
      seed_type = c("positive", "negative"),
      text = c(
        paste(c("Inclusion criteria:", include_items), collapse = "\n"),
        paste(c("Exclusion criteria:", exclude_items), collapse = "\n")
      )
    ) |>
      dplyr::filter(nzchar(.data$text))
  } else {
    out <- tibble::tibble(
      seed_type = c(
        rep("positive", length(include_items)),
        rep("negative", length(exclude_items))
      ),
      pair_id = NA_integer_,
      text = c(
        paste0("Inclusion criterion: ", include_items),
        paste0("Exclusion criterion: ", exclude_items)
      )
    )
  }

  validate_seed_bank(out)
}

#' Build the seed-generation system prompt
#'
#' @param n_positive Number of positive seeds to generate.
#' @param n_negative Number of negative seeds to generate.
#'
#' @return A plain-text system prompt.
#'
#' @keywords internal
#' @noRd
make_seed_abstract_system_prompt <- function(
  n_positive,
  n_negative
) {
  # Put the complete task contract in the system message so the user prompt can
  # remain only the criteria payload that drives cache identity.
  paste(
    "You generate synthetic but realistic scientific abstracts for systematic-review retrieval benchmarking.",
    "Return only valid JSON with one top-level field named `abstracts`.",
    "Each `abstracts` element must contain `pair_id`, `seed_type`, and `text`.",
    paste0(
      "Generate exactly ",
      n_positive,
      " positive abstracts and ",
      n_negative,
      " matched negative abstracts."
    ),
    "Positive abstracts must represent included studies that stay faithful to the criteria.",
    "Negative abstracts must remain plausible near-misses that fail one or more criteria.",
    "Do not mention labels, benchmark names, markdown, code fences, or commentary.",
    sep = "\n\n"
  )
}

#' Build the seed-generation user prompt
#'
#' @param criteria Criteria object.
#'
#' @return A plain-text user prompt containing only the raw criteria payload.
#'
#' @keywords internal
#' @noRd
make_seed_abstract_prompt <- function(criteria) {
  # Preserve the criteria as supplied so the solver cache key follows real
  # criteria changes instead of a parallel hand-written digest.
  criteria_json <- jsonlite::toJSON(
    list(
      include = criteria$include %||% character(),
      exclude = criteria$exclude %||% character()
    ),
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )

  # Keep this prompt deliberately narrow: it only carries the review criteria.
  paste(
    "<criteria>",
    criteria_json,
    "</criteria>",
    sep = "\n\n"
  )
}

#' Generate seed abstracts through the shared LLM cache
#'
#' @param criteria Criteria object.
#' @param dataset Dataset label used to group seed-abstract cache files.
#' @param seed_model Chat model label used for seed generation.
#' @param n_positive Number of positive seeds.
#' @param n_negative Number of negative seeds.
#' @param cache_root Official reusable cache root.
#'
#' @return A validated seed bank tibble.
#'
#' @keywords internal
#' @noRd
generate_seed_abstracts <- function(
  criteria,
  dataset,
  seed_model = "google/gemini-3.1-pro-preview",
  n_positive = 10L,
  n_negative = 10L,
  cache_root = "cache"
) {
  # Dataset-scoped directories keep the shared flat LLM cache readable without
  # adding another cache format on top of llm_solver().
  if (!rlang::is_string(dataset) || !nzchar(dataset)) {
    cli::cli_abort("{.arg dataset} must be a single non-empty string.")
  }
  cache_dir <- here::here(
    cache_root,
    "llm",
    "seed_abstracts",
    dataset
  )

  # The prompt text is part of the shared solver cache key, so criteria changes
  # naturally produce a different LLM cache file in the same dataset folder.
  prompt <- make_seed_abstract_prompt(criteria)
  system_prompt <- make_seed_abstract_system_prompt(
    n_positive,
    n_negative
  )

  # Let llm_solver own all cache lookup/write behavior; this function only
  # parses and validates the resulting payload.
  solver <- llm_solver(
    inputs = c(seed_bank = prompt),
    solver_chat = function() {
      ellmer::chat_openrouter(
        model = seed_model,
        system_prompt = system_prompt
      )
    },
    cache_dir = cache_dir,
    cache_failure = FALSE,
    batch_size = 1L,
    parallelize_batches = FALSE
  )

  # The current seed prompt asks for plain JSON; structured generation can be
  # added later without changing the cache location contract.
  reply <- solver$result[[1]]
  if (is.null(reply) || !nzchar(reply)) {
    cli::cli_abort("Seed-bank solver returned an empty reply.")
  }

  parsed <- extract_json_object(reply)
  if (is.null(parsed$abstracts)) {
    cli::cli_abort("Seed-bank reply did not contain an `abstracts` field.")
  }

  seed_bank <- tibble::as_tibble(parsed$abstracts)
  seed_bank <- validate_seed_bank(
    seed_bank,
    n_positive = n_positive,
    n_negative = n_negative
  )
  seed_bank
}

#' Normalize the rows of an embedding matrix
#'
#' Normalize the rows of a numeric matrix to unit length (L2 norm of 1).
#' The normalization consists of:
#' \itemize{
#'   \item Verifying that the input is a numeric matrix.
#'   \item Computing the Euclidean norm (square root of the sum of squared values) for each row vector.
#'   \item Dividing each row vector's elements by its Euclidean norm. If the norm is zero,
#'         \code{NA_real_} is used as the denominator temporarily, and any resulting non-finite
#'         values are subsequently replaced with \code{0} to guarantee a finite unit-length output.
#' }
#'
#' @param mat Numeric matrix.
#'
#' @return A row-normalized numeric matrix.
#'
#' @keywords internal
#' @noRd
.row_normalize_embedding_matrix <- function(mat) {
  if (!is.matrix(mat) || !is.numeric(mat)) {
    cli::cli_abort("{.arg mat} must be a numeric matrix.")
  }

  denom <- sqrt(rowSums(mat * mat))
  denom[denom == 0] <- NA_real_
  out <- mat / denom
  out[!is.finite(out)] <- 0
  out
}

#' Compute cosine distances between documents and seeds
#'
#' @param doc_norm Row-normalized document matrix.
#' @param seed_norm Row-normalized seed matrix.
#'
#' @return A numeric distance matrix.
#'
#' @keywords internal
#' @noRd
.cosine_distance_matrix <- function(doc_norm, seed_norm) {
  1 - tcrossprod(doc_norm, seed_norm)
}

#' Compute the centroid distance from documents to a seed family
#'
#' @param doc_norm Row-normalized document matrix.
#' @param seed_norm Row-normalized seed matrix.
#'
#' @return A numeric distance vector where lower values rank earlier.
#'
#' @keywords internal
#' @noRd
.centroid_distance_vector <- function(doc_norm, seed_norm) {
  centroid <- colMeans(seed_norm)
  centroid <- .row_normalize_embedding_matrix(matrix(centroid, nrow = 1L))
  as.numeric(1 - doc_norm %*% t(centroid))
}

#' Compute the mean of the closest seed distances
#'
#' @param distances Distance matrix.
#' @param closest_k Number of closest distances to average.
#'
#' @return A numeric vector.
#'
#' @keywords internal
#' @noRd
.closest_mean_distance <- function(distances, closest_k) {
  if (
    !is.numeric(closest_k) ||
      length(closest_k) != 1L ||
      is.na(closest_k) ||
      closest_k < 1
  ) {
    cli::cli_abort("{.arg closest_k} must be a positive whole number.")
  }
  closest_k <- as.integer(closest_k)

  apply(
    distances,
    1L,
    function(x) {
      sorted <- sort(x)
      mean(sorted[seq_len(min(closest_k, length(sorted)))], na.rm = TRUE)
    }
  )
}

#' Format a contrastive lambda suffix
#'
#' @param lambda Numeric contrastive penalty weight.
#'
#' @return A method-name suffix.
#'
#' @keywords internal
#' @noRd
.lambda_suffix <- function(lambda) {
  if (!is.numeric(lambda) || length(lambda) != 1L || is.na(lambda)) {
    cli::cli_abort("{.arg lambda} must be a numeric scalar.")
  }

  gsub("\\.", "p", as.character(lambda))
}

#' Validate score-matrix geometry
#'
#' @param doc_mat Document embedding matrix.
#' @param seed_bank Seed bank tibble.
#' @param seed_mat Seed embedding matrix.
#'
#' @return Invisible `NULL`.
#'
#' @keywords internal
#' @noRd
.validate_score_geometry <- function(doc_mat, seed_bank, seed_mat) {
  if (!is.matrix(doc_mat) || !is.numeric(doc_mat)) {
    cli::cli_abort("{.arg doc_mat} must be a numeric matrix.")
  }
  if (!is.matrix(seed_mat) || !is.numeric(seed_mat)) {
    cli::cli_abort("{.arg seed_mat} must be a numeric matrix.")
  }
  if (nrow(seed_bank) != nrow(seed_mat)) {
    cli::cli_abort("Seed bank and seed embedding rows must match.")
  }
  if (ncol(doc_mat) != ncol(seed_mat)) {
    cli::cli_abort("Document and seed embeddings must have the same dimension.")
  }

  invisible(NULL)
}

#' Compute a standard abstract-concentration score family
#'
#' @param doc_mat Document embedding matrix.
#' @param seed_bank Seed bank tibble.
#' @param seed_mat Seed embedding matrix.
#' @param lambdas Contrastive penalty values.
#' @param closest_k Number of closest seeds used by the closest-k score.
#'
#' @return A named list of numeric score vectors.
#'
#' @keywords internal
#' @noRd
.score_standard_family <- function(
  doc_mat,
  seed_bank,
  seed_mat,
  lambdas = c(0.25, 0.5, 1),
  closest_k = 3L
) {
  .validate_score_geometry(doc_mat, seed_bank, seed_mat)

  doc_norm <- .row_normalize_embedding_matrix(doc_mat)
  seed_norm <- .row_normalize_embedding_matrix(seed_mat)
  pos_norm <- seed_norm[seed_bank$seed_type == "positive", , drop = FALSE]
  neg_norm <- seed_norm[seed_bank$seed_type == "negative", , drop = FALSE]
  if (!nrow(pos_norm) || !nrow(neg_norm)) {
    cli::cli_abort("Seed bank must include positive and negative seeds.")
  }

  pos_dist <- .cosine_distance_matrix(doc_norm, pos_norm)
  neg_dist <- .cosine_distance_matrix(doc_norm, neg_norm)
  pos_centroid <- .centroid_distance_vector(doc_norm, pos_norm)
  neg_centroid <- .centroid_distance_vector(doc_norm, neg_norm)

  scores <- list(
    positive_mean = rowMeans(pos_dist),
    positive_closest = .closest_mean_distance(pos_dist, closest_k),
    positive_centroid = pos_centroid
  )

  for (lambda in lambdas) {
    suffix <- .lambda_suffix(lambda)
    scores[[paste0("contrastive_mean_l", suffix)]] <-
      rowMeans(pos_dist) - lambda * rowMeans(neg_dist)
    scores[[paste0("contrastive_closest_l", suffix)]] <-
      .closest_mean_distance(pos_dist, closest_k) -
      lambda * apply(neg_dist, 1L, min)
    scores[[paste0("contrastive_centroid_l", suffix)]] <-
      pos_centroid - lambda * neg_centroid
  }

  if (all(c(0.25, 0.5) %in% lambdas)) {
    reciprocal_rank <- function(score) {
      score_rank <- rank(score, ties.method = "first")
      1 / (60 + score_rank)
    }
    scores$centroid_contrastive_weighted <- -(1.00 *
      reciprocal_rank(pos_centroid) +
      0.35 * reciprocal_rank(pos_centroid - 0.25 * neg_centroid) +
      0.20 * reciprocal_rank(pos_centroid - 0.5 * neg_centroid))
  }

  scores
}

#' Rank embeddings by positive-seed mean distance
#'
#' @param doc_mat Document embedding matrix.
#' @param seed_bank Validated seed bank tibble.
#' @param seed_mat Seed embedding matrix.
#' @param ... Unused extra arguments.
#'
#' @return A numeric score vector where lower values rank earlier.
#'
#' @keywords internal
#' @noRd
rank_embeddings_positive_mean <- function(doc_mat, seed_bank, seed_mat, ...) {
  .score_standard_family(doc_mat, seed_bank, seed_mat)$positive_mean
}

#' Rank embeddings by the closest positive seeds
#'
#' @param doc_mat Document embedding matrix.
#' @param seed_bank Validated seed bank tibble.
#' @param seed_mat Seed embedding matrix.
#' @param closest_k Number of closest positive seeds to average.
#' @param ... Unused extra arguments.
#'
#' @return A numeric score vector where lower values rank earlier.
#'
#' @keywords internal
#' @noRd
rank_embeddings_positive_closest <- function(
  doc_mat,
  seed_bank,
  seed_mat,
  closest_k = 3L,
  ...
) {
  .score_standard_family(
    doc_mat = doc_mat,
    seed_bank = seed_bank,
    seed_mat = seed_mat,
    closest_k = closest_k
  )$positive_closest
}

#' Rank embeddings by the positive centroid
#'
#' @param doc_mat Document embedding matrix.
#' @param seed_bank Validated seed bank tibble.
#' @param seed_mat Seed embedding matrix.
#' @param ... Unused extra arguments.
#'
#' @return A numeric score vector where lower values rank earlier.
#'
#' @keywords internal
#' @noRd
rank_embeddings_positive_centroid <- function(
  doc_mat,
  seed_bank,
  seed_mat,
  ...
) {
  .score_standard_family(doc_mat, seed_bank, seed_mat)$positive_centroid
}

#' Rank embeddings by contrastive mean distance
#'
#' @param doc_mat Document embedding matrix.
#' @param seed_bank Validated seed bank tibble.
#' @param seed_mat Seed embedding matrix.
#' @param lambda Contrastive penalty weight.
#' @param closest_k Number of closest positive seeds used in the same family.
#' @param ... Unused extra arguments.
#'
#' @return A numeric score vector where lower values rank earlier.
#'
#' @keywords internal
#' @noRd
rank_embeddings_contrastive_mean <- function(
  doc_mat,
  seed_bank,
  seed_mat,
  lambda = 0.25,
  closest_k = 3L,
  ...
) {
  suffix <- .lambda_suffix(lambda)
  .score_standard_family(
    doc_mat = doc_mat,
    seed_bank = seed_bank,
    seed_mat = seed_mat,
    lambdas = lambda,
    closest_k = closest_k
  )[[paste0("contrastive_mean_l", suffix)]]
}

#' Rank embeddings by contrastive closest-k distance
#'
#' @param doc_mat Document embedding matrix.
#' @param seed_bank Validated seed bank tibble.
#' @param seed_mat Seed embedding matrix.
#' @param lambda Contrastive penalty weight.
#' @param closest_k Number of closest positive seeds.
#' @param ... Unused extra arguments.
#'
#' @return A numeric score vector where lower values rank earlier.
#'
#' @keywords internal
#' @noRd
rank_embeddings_contrastive_closest <- function(
  doc_mat,
  seed_bank,
  seed_mat,
  lambda = 0.25,
  closest_k = 3L,
  ...
) {
  suffix <- .lambda_suffix(lambda)
  .score_standard_family(
    doc_mat = doc_mat,
    seed_bank = seed_bank,
    seed_mat = seed_mat,
    lambdas = lambda,
    closest_k = closest_k
  )[[paste0("contrastive_closest_l", suffix)]]
}

#' Rank embeddings by contrastive centroid distance
#'
#' @param doc_mat Document embedding matrix.
#' @param seed_bank Validated seed bank tibble.
#' @param seed_mat Seed embedding matrix.
#' @param lambda Contrastive penalty weight.
#' @param ... Unused extra arguments.
#'
#' @return A numeric score vector where lower values rank earlier.
#'
#' @keywords internal
#' @noRd
rank_embeddings_contrastive_centroid <- function(
  doc_mat,
  seed_bank,
  seed_mat,
  lambda = 0.25,
  ...
) {
  suffix <- .lambda_suffix(lambda)
  .score_standard_family(
    doc_mat = doc_mat,
    seed_bank = seed_bank,
    seed_mat = seed_mat,
    lambdas = lambda
  )[[paste0("contrastive_centroid_l", suffix)]]
}

#' Rank embeddings by reciprocal-rank fusion over centroid scores
#'
#' @param doc_mat Document embedding matrix.
#' @param seed_bank Validated seed bank tibble.
#' @param seed_mat Seed embedding matrix.
#' @param lambdas Contrastive penalty values.
#' @param ... Unused extra arguments.
#'
#' @return A numeric score vector where lower values rank earlier.
#'
#' @keywords internal
#' @noRd
rank_embeddings_centroid_contrastive_weighted <- function(
  doc_mat,
  seed_bank,
  seed_mat,
  lambdas = c(0.25, 0.5, 1),
  ...
) {
  .score_standard_family(
    doc_mat = doc_mat,
    seed_bank = seed_bank,
    seed_mat = seed_mat,
    lambdas = lambdas
  )$centroid_contrastive_weighted
}

#' Rank embeddings with criteria-item seeds
#'
#' @param doc_mat Document embedding matrix.
#' @param criteria_seed_bank Criteria-item seed bank.
#' @param criteria_mat Criteria-item embedding matrix.
#' @param score_name Standard score-family member to return.
#' @param lambda Contrastive penalty value when `score_name` is contrastive.
#' @param closest_k Number of closest positive seeds.
#' @param ... Unused extra arguments.
#'
#' @return A numeric score vector where lower values rank earlier.
#'
#' @keywords internal
#' @noRd
rank_embeddings_criteria_item <- function(
  doc_mat,
  criteria_seed_bank,
  criteria_mat,
  score_name = "positive_mean",
  lambda = 0.25,
  closest_k = 3L,
  ...
) {
  lambdas <- if (identical(score_name, "centroid_contrastive_weighted")) {
    c(0.25, 0.5, 1)
  } else if (startsWith(score_name, "contrastive_")) {
    lambda
  } else {
    numeric()
  }
  scores <- .score_standard_family(
    doc_mat = doc_mat,
    seed_bank = criteria_seed_bank,
    seed_mat = criteria_mat,
    lambdas = lambdas,
    closest_k = closest_k
  )
  if (!score_name %in% names(scores)) {
    cli::cli_abort("Unknown criteria item score {.val {score_name}}.")
  }
  scores[[score_name]]
}

#' Rank embeddings with criteria-block seeds
#'
#' @param doc_mat Document embedding matrix.
#' @param criteria_block_bank Criteria-block seed bank.
#' @param criteria_block_mat Criteria-block embedding matrix.
#' @param score_name Standard score-family member to return.
#' @param lambda Contrastive penalty value when `score_name` is contrastive.
#' @param closest_k Number of closest positive seeds.
#' @param ... Unused extra arguments.
#'
#' @return A numeric score vector where lower values rank earlier.
#'
#' @keywords internal
#' @noRd
rank_embeddings_criteria_block <- function(
  doc_mat,
  criteria_block_bank,
  criteria_block_mat,
  score_name = "positive_mean",
  lambda = 0.25,
  closest_k = 3L,
  ...
) {
  lambdas <- if (identical(score_name, "centroid_contrastive_weighted")) {
    c(0.25, 0.5, 1)
  } else if (startsWith(score_name, "contrastive_")) {
    lambda
  } else {
    numeric()
  }
  scores <- .score_standard_family(
    doc_mat = doc_mat,
    seed_bank = criteria_block_bank,
    seed_mat = criteria_block_mat,
    lambdas = lambdas,
    closest_k = closest_k
  )
  if (!score_name %in% names(scores)) {
    cli::cli_abort("Unknown criteria block score {.val {score_name}}.")
  }
  scores[[score_name]]
}

#' Rank embeddings with positive seeds and negative criteria
#'
#' @param doc_mat Document embedding matrix.
#' @param seed_bank Positive-seed bank.
#' @param seed_mat Positive-seed embeddings.
#' @param criteria_seed_bank Criteria seed bank.
#' @param criteria_mat Criteria embeddings.
#' @param lambda Contrastive penalty value.
#' @param ... Unused extra arguments.
#'
#' @return A numeric score vector where lower values rank earlier.
#'
#' @keywords internal
#' @noRd
rank_embeddings_positive_seed_negative_criteria <- function(
  doc_mat,
  seed_bank,
  seed_mat,
  criteria_seed_bank,
  criteria_mat,
  lambda = 0.25,
  ...
) {
  .validate_score_geometry(doc_mat, seed_bank, seed_mat)
  .validate_score_geometry(doc_mat, criteria_seed_bank, criteria_mat)

  doc_norm <- .row_normalize_embedding_matrix(doc_mat)
  seed_norm <- .row_normalize_embedding_matrix(seed_mat)
  criteria_norm <- .row_normalize_embedding_matrix(criteria_mat)
  pos_seed_norm <- seed_norm[seed_bank$seed_type == "positive", , drop = FALSE]
  neg_criteria_norm <- criteria_norm[
    criteria_seed_bank$seed_type == "negative",
    ,
    drop = FALSE
  ]
  if (!nrow(pos_seed_norm) || !nrow(neg_criteria_norm)) {
    cli::cli_abort(
      "Positive seed banks and negative criteria banks must both contain usable rows."
    )
  }

  pos_centroid <- .centroid_distance_vector(doc_norm, pos_seed_norm)
  neg_centroid <- .centroid_distance_vector(doc_norm, neg_criteria_norm)
  pos_centroid - lambda * neg_centroid
}

#' Build a rank-metric row
#'
#' @param ranked Ranked data frame sorted in review order and containing
#'   `included`.
#' @param ranking_method Ranking-method label.
#' @param dataset Dataset label.
#' @param embedding_model Backward-compatible embedding-model label.
#' @param embedding_profile Embedding-profile label.
#' @param document_embedding_model Document embedding-model label.
#' @param seed_embedding_model Seed and criteria embedding-model label.
#'
#' @return A one-row tibble of early retrieval metrics.
#'
#' @keywords internal
#' @noRd
make_rank_metrics_row <- function(
  ranked,
  ranking_method,
  dataset,
  embedding_model,
  embedding_profile = embedding_model,
  document_embedding_model = embedding_model,
  seed_embedding_model = embedding_model
) {
  if (!"included" %in% names(ranked)) {
    cli::cli_abort("Ranked data must contain an {.field included} column.")
  }
  if (!rlang::is_string(ranking_method) || !nzchar(ranking_method)) {
    cli::cli_abort("{.arg ranking_method} must be a single non-empty string.")
  }

  top_n <- function(n) {
    sum(ranked$included[seq_len(min(n, nrow(ranked)))], na.rm = TRUE)
  }
  rank_to <- function(n) {
    hits <- which(cumsum(ranked$included) >= n)
    if (!length(hits)) NA_integer_ else hits[[1]]
  }

  tibble::tibble(
    dataset = dataset,
    embedding_profile = embedding_profile,
    embedding_model = embedding_model,
    document_embedding_model = document_embedding_model,
    seed_embedding_model = seed_embedding_model,
    ranking_method = ranking_method,
    total_n = nrow(ranked),
    total_pos = sum(ranked$included, na.rm = TRUE),
    n_25 = top_n(25L),
    n_50 = top_n(50L),
    n_100 = top_n(100L),
    rank_to_5 = rank_to(5L),
    rank_to_10 = rank_to(10L),
    rank_to_20 = rank_to(20L)
  )
}

#' Normalize a score result
#'
#' Coerce a scoring result into a canonical named list of numeric vectors.
#' The normalization consists of:
#' \itemize{
#'   \item Checking if the input is a bare numeric vector; if so, wrapping it in a list
#'         and naming it with the \code{fallback_name}.
#'   \item Ensuring that if the input is a list, it is non-empty (otherwise aborting).
#'   \item Assigning names to the list if it lacks them: using \code{fallback_name} directly
#'         if it has a single element, or generating sequential names by appending 1-based
#'         indices to \code{fallback_name} (e.g. \code{fallback_name_1}, \code{fallback_name_2})
#'         if it has multiple elements.
#' }
#'
#' @param scores A numeric vector or named list of numeric vectors.
#' @param fallback_name Name to use when `scores` is an unnamed vector.
#'
#' @return A named list of numeric score vectors.
#'
#' @keywords internal
#' @noRd
.normalize_score_result <- function(scores, fallback_name) {
  if (is.numeric(scores)) {
    return(stats::setNames(list(as.numeric(scores)), fallback_name))
  }

  if (!is.list(scores) || !length(scores)) {
    cli::cli_abort(
      "Method function must return a numeric vector or a non-empty list."
    )
  }

  if (is.null(names(scores))) {
    if (length(scores) == 1L) {
      names(scores) <- fallback_name
    } else {
      names(scores) <- paste0(fallback_name, "_", seq_along(scores))
    }
  }

  scores
}

#' Prepare the reusable pieces for one dataset/profile branch
#'
#' @param dataset Dataset label.
#' @param embedding_model Backward-compatible embedding-model label.
#' @param embedding_profile Embedding-profile label.
#' @param document_embedding_model Document embedding-model label.
#' @param seed_embedding_model Seed and criteria embedding-model label.
#' @param document_embedding_mode Document embedding modality.
#' @param seed_embedding_mode Seed and criteria embedding modality.
#' @param data_dir Directory containing packaged datasets.
#' @param cache_root Official cache root.
#' @param seed_model Seed-generation chat model.
#' @param n_positive Number of positive seed abstracts.
#' @param n_negative Number of negative seed abstracts.
#' @param embedding_batch_size Batch size used when embedding cache misses.
#'
#' @return A list with data, criteria, seed banks, and embedding matrices.
#'
#' @keywords internal
#' @noRd
prepare_abstract_concentration_inputs <- function(
  dataset,
  embedding_model = NULL,
  embedding_profile = embedding_model,
  document_embedding_model = embedding_model,
  seed_embedding_model = embedding_model,
  document_embedding_mode = "document",
  seed_embedding_mode = "query",
  data_dir = "data",
  cache_root = "cache",
  seed_model = "google/gemini-3.1-pro-preview",
  n_positive = 10L,
  n_negative = 10L,
  embedding_batch_size = 64L
) {
  if (is.null(document_embedding_model) || is.null(seed_embedding_model)) {
    cli::cli_abort(
      "Both {.arg document_embedding_model} and {.arg seed_embedding_model} are required."
    )
  }
  if (is.null(embedding_model)) {
    embedding_model <- embedding_profile
  }
  loaded <- get_dataset(dataset, data_dir = data_dir)
  data <- tibble::as_tibble(loaded$data) |>
    dplyr::mutate(id = dplyr::row_number(), .before = 1L)
  criteria <- loaded$criteria

  document_text <- make_document_text(data)
  seed_bank <- generate_seed_abstracts(
    criteria = criteria,
    dataset = dataset,
    seed_model = seed_model,
    n_positive = n_positive,
    n_negative = n_negative,
    cache_root = cache_root
  )
  criteria_item_bank <- make_criteria_seed_bank(criteria, level = "item")
  criteria_block_bank <- make_criteria_seed_bank(criteria, level = "block")

  document_embeddings <- generate_embeddings(
    texts = document_text,
    embedding_model = document_embedding_model,
    dataset = dataset,
    text_role = "abstracts",
    embedding_mode = document_embedding_mode,
    cache_root = here::here(cache_root, "embeddings"),
    batch_size = embedding_batch_size
  )
  seed_embeddings <- generate_embeddings(
    texts = seed_bank$text,
    embedding_model = seed_embedding_model,
    dataset = dataset,
    text_role = "seed_abstracts",
    embedding_mode = seed_embedding_mode,
    cache_root = here::here(cache_root, "embeddings"),
    batch_size = embedding_batch_size
  )
  criteria_item_embeddings <- generate_embeddings(
    texts = criteria_item_bank$text,
    embedding_model = seed_embedding_model,
    dataset = dataset,
    text_role = "criteria_items",
    embedding_mode = seed_embedding_mode,
    cache_root = here::here(cache_root, "embeddings"),
    batch_size = embedding_batch_size
  )
  criteria_block_embeddings <- generate_embeddings(
    texts = criteria_block_bank$text,
    embedding_model = seed_embedding_model,
    dataset = dataset,
    text_role = "criteria_blocks",
    embedding_mode = seed_embedding_mode,
    cache_root = here::here(cache_root, "embeddings"),
    batch_size = embedding_batch_size
  )

  list(
    dataset = dataset,
    embedding_profile = embedding_profile,
    embedding_model = embedding_model,
    document_embedding_model = document_embedding_model,
    seed_embedding_model = seed_embedding_model,
    document_embedding_mode = document_embedding_mode,
    seed_embedding_mode = seed_embedding_mode,
    data = data,
    criteria = criteria,
    seed_bank = seed_bank,
    criteria_item_bank = criteria_item_bank,
    criteria_block_bank = criteria_block_bank,
    document_embeddings = document_embeddings,
    seed_embeddings = seed_embeddings,
    criteria_item_embeddings = criteria_item_embeddings,
    criteria_block_embeddings = criteria_block_embeddings
  )
}

#' Run one abstract-concentration method on one dataset/model branch
#'
#' @param dataset Dataset label.
#' @param embedding_model Backward-compatible embedding-model label.
#' @param embedding_profile Embedding-profile label.
#' @param document_embedding_model Document embedding-model label.
#' @param seed_embedding_model Seed and criteria embedding-model label.
#' @param method_function Method function name or function object.
#' @param method_args Named list of method-specific arguments.
#' @param prepared Optional prepared inputs from
#'   `prepare_abstract_concentration_inputs()`.
#' @param ... Additional arguments forwarded to the preparation step.
#'
#' @return A tibble of metric rows for one method function.
#'
#' @keywords internal
#' @noRd
run_abstract_concentration_benchmark <- function(
  dataset,
  embedding_model = NULL,
  embedding_profile = embedding_model,
  document_embedding_model = embedding_model,
  seed_embedding_model = embedding_model,
  method_function,
  method_args = list(),
  prepared = NULL,
  ...
) {
  if (is.null(prepared)) {
    prepared <- prepare_abstract_concentration_inputs(
      dataset = dataset,
      embedding_model = embedding_model,
      embedding_profile = embedding_profile,
      document_embedding_model = document_embedding_model,
      seed_embedding_model = seed_embedding_model,
      ...
    )
  }

  required_prepared <- c(
    "data",
    "seed_bank",
    "seed_embeddings",
    "criteria_item_bank",
    "criteria_item_embeddings",
    "criteria_block_bank",
    "criteria_block_embeddings",
    "document_embeddings"
  )
  missing_prepared <- setdiff(required_prepared, names(prepared))
  if (length(missing_prepared)) {
    cli::cli_abort(
      "Prepared inputs are missing fields {.field {missing_prepared}}."
    )
  }

  if (is.function(method_function)) {
    method_fun <- method_function
    fallback_name <- deparse(substitute(method_function))
  } else if (rlang::is_string(method_function)) {
    method_fun <- get(
      method_function,
      mode = "function",
      envir = parent.frame()
    )
    fallback_name <- method_function
  } else {
    cli::cli_abort(
      "{.arg method_function} must be a function or function name."
    )
  }

  if (!is.list(method_args)) {
    cli::cli_abort("{.arg method_args} must be a named list.")
  }

  ranking_method <- method_args$ranking_method
  method_args$ranking_method <- NULL
  if (is.null(ranking_method)) {
    ranking_method <- fallback_name
  }

  scores <- rlang::exec(
    method_fun,
    doc_mat = prepared$document_embeddings,
    seed_bank = prepared$seed_bank,
    seed_mat = prepared$seed_embeddings,
    criteria_seed_bank = prepared$criteria_item_bank,
    criteria_mat = prepared$criteria_item_embeddings,
    criteria_block_bank = prepared$criteria_block_bank,
    criteria_block_mat = prepared$criteria_block_embeddings,
    !!!method_args
  )
  scores <- .normalize_score_result(scores, ranking_method)

  purrr::imap_dfr(
    scores,
    function(score, method_label) {
      if (length(score) != nrow(prepared$data)) {
        cli::cli_abort(
          "Method {.val {method_label}} returned a score vector with the wrong length."
        )
      }

      ranked <- prepared$data |>
        dplyr::mutate(embedding_score = as.numeric(score)) |>
        dplyr::arrange(.data$embedding_score, .data$id)

      make_rank_metrics_row(
        ranked = ranked,
        ranking_method = method_label,
        dataset = dataset,
        embedding_model = embedding_model %||% embedding_profile,
        embedding_profile = prepared$embedding_profile,
        document_embedding_model = prepared$document_embedding_model,
        seed_embedding_model = prepared$seed_embedding_model
      )
    }
  )
}

#' Run the abstract-concentration branch for one embedding profile
#'
#' This is the unit of target-level parallelism. For one embedding profile it
#' loops sequentially over datasets, prepares that dataset's reusable document,
#' synthetic-seed, and criteria embeddings once, and then loops sequentially
#' over the ranking-method grid using those prepared inputs. Keeping the inner
#' loops sequential avoids multiplying API pressure within a model branch while
#' still allowing independent embedding profiles to run in separate `crew`
#' workers.
#'
#' @param embedding_profile Embedding-profile label.
#' @param document_embedding_model Document embedding-model label.
#' @param seed_embedding_model Seed and criteria embedding-model label.
#' @param document_embedding_mode Document embedding modality.
#' @param seed_embedding_mode Seed and criteria embedding modality.
#' @param datasets Tibble of dataset metadata from `get_datasets()`.
#' @param method_grid Tibble of method functions and arguments.
#' @param ... Additional arguments forwarded to
#'   `prepare_abstract_concentration_inputs()`.
#'
#' @return A tibble of metric rows for all datasets and methods for one model.
#'
#' @keywords internal
#' @noRd
run_abstract_concentration_profile <- function(
  embedding_profile,
  document_embedding_model,
  seed_embedding_model,
  document_embedding_mode = "document",
  seed_embedding_mode = "query",
  datasets,
  method_grid,
  ...
) {
  if (is.list(embedding_profile)) {
    profile <- embedding_profile
    embedding_profile <- profile$embedding_profile
    document_embedding_model <- profile$document_embedding_model
    seed_embedding_model <- profile$seed_embedding_model
    document_embedding_mode <- profile$document_embedding_mode
    seed_embedding_mode <- profile$seed_embedding_mode
    if (is.null(embedding_profile) || !length(embedding_profile)) {
      embedding_profile <- profile[[1]]
    }
  }

  purrr::map_dfr(
    datasets$dataset,
    function(dataset) {
      prepared <- prepare_abstract_concentration_inputs(
        dataset = dataset,
        embedding_model = embedding_profile,
        embedding_profile = embedding_profile,
        document_embedding_model = document_embedding_model,
        seed_embedding_model = seed_embedding_model,
        document_embedding_mode = document_embedding_mode,
        seed_embedding_mode = seed_embedding_mode,
        ...
      )

      purrr::map_dfr(
        seq_len(nrow(method_grid)),
        function(i) {
          method_row <- method_grid[i, , drop = FALSE]
          method_args <- method_row$method_args[[1]]
          method_args$ranking_method <- method_row$ranking_method[[1]]
          run_abstract_concentration_benchmark(
            dataset = dataset,
            embedding_model = embedding_profile,
            embedding_profile = embedding_profile,
            document_embedding_model = document_embedding_model,
            seed_embedding_model = seed_embedding_model,
            method_function = method_row$method_function[[1]],
            method_args = method_args,
            prepared = prepared
          )
        }
      )
    }
  )
}

#' Run the abstract-concentration branch for one embedding model
#'
#' Backward-compatible wrapper around `run_abstract_concentration_profile()`
#' for callers that use the same model for documents and query-like anchors.
#'
#' @param embedding_model Embedding-model label.
#' @param datasets Tibble of dataset metadata from `get_datasets()`.
#' @param method_grid Tibble of method functions and arguments.
#' @param ... Additional arguments forwarded to
#'   `run_abstract_concentration_profile()`.
#'
#' @return A tibble of metric rows for all datasets and methods for one model.
#'
#' @keywords internal
#' @noRd
run_abstract_concentration_model <- function(
  embedding_model,
  datasets,
  method_grid,
  ...
) {
  run_abstract_concentration_profile(
    embedding_profile = embedding_model,
    document_embedding_model = embedding_model,
    seed_embedding_model = embedding_model,
    document_embedding_mode = "document",
    seed_embedding_mode = "query",
    datasets = datasets,
    method_grid = method_grid,
    ...
  )
}

#' Combine abstract-concentration results
#'
#' @param results List of result tibbles or a single tibble.
#'
#' @return One tibble sorted by dataset, model, and method.
#'
#' @keywords internal
#' @noRd
combine_abstract_concentration_results <- function(results) {
  if (is.data.frame(results)) {
    results <- list(results)
  }

  dplyr::bind_rows(results) |>
    dplyr::arrange(
      .data$dataset,
      .data$embedding_profile,
      .data$ranking_method
    )
}

#' Fit the abstract-concentration mixed model
#'
#' @param metrics Combined metrics table.
#'
#' @return A fitted `blme::bglmer()` model.
#'
#' @keywords internal
#' @noRd
fit_abstract_concentration_model <- function(metrics) {
  rlang::check_installed(c("blme"))

  if (
    !all(
      c(
        "dataset",
        "embedding_profile",
        "embedding_model",
        "ranking_method",
        "n_50",
        "total_pos"
      ) %in%
        names(metrics)
    )
  ) {
    cli::cli_abort(
      "Metrics table is missing columns required for model fitting."
    )
  }

  model_data <- tibble::as_tibble(metrics) |>
    dplyr::mutate(
      dataset = as.factor(.data$dataset),
      embedding_profile = as.factor(.data$embedding_profile),
      ranking_method = as.factor(.data$ranking_method),
      failures = .data$total_pos - .data$n_50
    )

  if (any(model_data$failures < 0)) {
    cli::cli_abort(
      "Metrics table contains rows where `n_50` exceeds `total_pos`."
    )
  }

  blme::bglmer(
    cbind(n_50, failures) ~ embedding_profile + ranking_method + (1 | dataset),
    data = model_data,
    family = stats::binomial()
  )
}

#' Build marginal summaries from the fitted model
#'
#' @param model Fitted model from `fit_abstract_concentration_model()`.
#'
#' @return A named list with marginal summaries by model and method.
#'
#' @keywords internal
#' @noRd
get_abstract_concentration_marginal_summaries <- function(model) {
  rlang::check_installed(c("marginaleffects"))

  summarize_one <- function(by) {
    # Marginaleffects warns about the bglmer class even when the summary is
    # valid for the fixed-effect marginal comparison we want here.
    suppressWarnings(marginaleffects::avg_predictions(model, by = by)) |>
      tibble::as_tibble() |>
      dplyr::rename(
        estimated_recall = estimate,
        std_error = std.error,
        conf_low = conf.low,
        conf_high = conf.high
      ) |>
      dplyr::arrange(dplyr::desc(.data$estimated_recall))
  }

  list(
    embedding_profile = summarize_one("embedding_profile"),
    ranking_method = summarize_one("ranking_method")
  )
}

#' Write abstract-concentration outputs
#'
#' @param metrics Combined metrics table.
#' @param marginal_summaries Named list from
#'   `get_abstract_concentration_marginal_summaries()`.
#' @param output_dir Final outputs directory.
#'
#' @return Invisible list of written file paths.
#'
#' @keywords internal
#' @noRd
write_abstract_concentration_outputs <- function(
  metrics,
  marginal_summaries,
  output_dir = fs::path("method_pipelines", "abstract_concentration", "outputs")
) {
  fs::dir_create(output_dir)

  metrics_path <- fs::path(output_dir, "ranking_metrics.csv")
  readr::write_csv(metrics, metrics_path)

  write_markdown_table <- function(data, path, title) {
    table_text <- c(
      paste0("# ", title),
      "",
      as.character(print_table(data))
    )
    readr::write_lines(table_text, path)
  }

  embed_csv <- fs::path(output_dir, "marginal_embedding_profile.csv")
  rank_csv <- fs::path(output_dir, "marginal_ranking_method.csv")
  readr::write_csv(marginal_summaries$embedding_profile, embed_csv)
  readr::write_csv(marginal_summaries$ranking_method, rank_csv)

  embed_md <- fs::path(output_dir, "marginal_embedding_profile.md")
  rank_md <- fs::path(output_dir, "marginal_ranking_method.md")
  write_markdown_table(
    marginal_summaries$embedding_profile,
    embed_md,
    "Marginal Recall by Embedding Profile"
  )
  write_markdown_table(
    marginal_summaries$ranking_method,
    rank_md,
    "Marginal Recall by Ranking Method"
  )

  invisible(list(
    ranking_metrics = metrics_path,
    marginal_embedding_profile_csv = embed_csv,
    marginal_embedding_profile_md = embed_md,
    marginal_ranking_method_csv = rank_csv,
    marginal_ranking_method_md = rank_md
  ))
}
