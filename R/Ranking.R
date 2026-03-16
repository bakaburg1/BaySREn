#' Rank documents using ragnar embedding stores and flexible retrieval
#'
#' @description `rank_by_embeddings()` builds or reuses one or more ragnar
#'   embedding stores, embeds specified fields (single or merged), applies
#'   optional weighted scoring across strategies, and retrieves similarity
#'   scores via vector similarity search (VSS), BM25, or both. It handles
#'   idempotent insertion, flexible retry/backoff, in-memory or on-disk storage,
#'   and can return both ranking scores and raw embeddings.
#'
#' @param data A data frame or tibble of documents; must include `id` and any
#'   fields named in `embed_strategies` (e.g. "title", "abstract").
#' @param query A single string query.
#' @param embed_strategies Which text fields to embed together or in different
#'   stores. Each element may be:
#' - A single field name (e.g. "title").
#' - A comma-separated set of fields to embed together (e.g. "title,keywords").
#' - Or "all_fields" which embeds together title, abstract and keywords.
#' @param scoring_strategy Which embedding set to use and how to join them if
#'   more than one. Either:
#' - One or more strategy names (must be one of `embed_strategies`). They will
#'   all have equal weight.
#' - A named named vector of weights (names matching the `embed_strategies` to
#'   use), internally normalized to sum to 1.
#' @param similarity_metric One of `"cosine_distance"`, `"cosine_similarity"`,
#'   `"dot_product"`, `"euclidean_distance"`; used for VSS.
#' @param embedder A function taking a character vector and returning an
#'   embedding matrix; defaults to `ragnar::embed_openai()`.
#' @param store_location Path to directory for on-disk SQLite stores. If NULL,
#'   each strategy uses an in-memory `":memory:"` store; multiple in-memory
#'   stores are kept in a session option list.
#' @param return_embeddings Logical; if TRUE, returns raw embeddings for each
#'   document and strategy, annotated with `sorting_id`.
#' @param retrieval_method One of `"vss"`, `"bm25"`, or `"both"`.
#' @param bm25_obj Optional pre-built BM25 index (to reuse across calls).
#' @param retry_strategy A `list` of parameters to override the internal ragnar
#'   retry/backoff strategy (which is hard-coded and cannot be modified
#'   directly). Use this when the default rate-limits or timing assumptions in
#'   ragnar do not align with your embedding API's requirements (e.g.,
#'   `list(max_times=5, pause_base=30, pause_cap=300)`).g. `list(max_times=5,
#'   pause_base=60, pause_cap=300)`.
#'
#' @return A list with:
#' - `data`: original `data` augmented with `score` and `sorting_id`
#' (preserving row order).
#' - `embeddings`: (if requested) tibble of raw embeddings per `id`, `strategy`,
#' and `sorting_id`.
#'
#' @export
rank_by_embeddings <- function(
  data,
  query,
  embed_strategies = "all_fields",
  scoring_strategy = embed_strategies,
  similarity_metric = c(
    "cosine_distance",
    "cosine_similarity",
    "dot_product",
    "euclidean_distance"
  ),
  embedder = ragnar::embed_openai(),
  store_location = NULL,
  return_embeddings = FALSE,
  retry_strategy = list(max_times = 10L, pause_base = 1L, pause_cap = 3600L)
) {

  # Validate inputs
  similarity_metric <- match.arg(similarity_metric)
  stopifnot(
    is.data.frame(data),
    is.character(query),
    length(query) == 1,
    is.character(embed_strategies),
    is.character(scoring_strategy),
    is.function(embedder),
    is.null(store_location) || is.character(store_location),
    is.logical(return_embeddings),
    is.list(retry_strategy),
    all(c("max_times", "pause_base", "pause_cap") %in% names(retry_strategy))
  )

  if (!"id" %in% names(data)) {
    data$id <- seq_len(nrow(data))
  }

  # Resolve embed_strategies presets
  resolve_fields <- function(strat) {

    if (strat == "all_fields") {
      c("title", "abstract", "keywords")
    } else {
      fields <- strsplit(strat, ",", fixed = TRUE)[[1]]

      if (any(!fields %in% names(data))) {
        cli::cli_abort(
          "Fields {fields} not found in data"
        )
      }
      fields
    }
  }

  # Normalize scoring_strategy to named weights
  if (is.numeric(scoring_strategy) && rlang::is_named(scoring_strategy)) {
    weights <- scoring_strategy / sum(scoring_strategy)
    strategies <- names(weights)
  } else {
    strategies <- scoring_strategy
    weights <- rep(1 / length(strategies), length(strategies))
    names(weights) <- strategies
  }

  # Build or fetch stores
  stores <- purrr::map(
    strategies,
    ~ connect_embedding_store(
        .x,
        store_location = store_location,
        embedder = embedder
    )
  )

  names(stores) <- strategies

  # Insert pending documents with retry/backoff
  for (strat in strategies) {
    st <- stores[[strat]]

    # Retrieve existing documents to avoid re-embedding
    existing <- tryCatch(
      dplyr::tbl(st@.con, "chunks") |> dplyr::pull(text) |> unique(),
      error = function(e) character()
    )

    # Interpret embed_strategies to resolve fields
    fields <- resolve_fields(strat)

    # Create text representation for each document
    df_text <- tibble::tibble(
      id = data$id,
      text = purrr::map_chr(
        seq_len(nrow(data)),
        \(i) {
          msgs <- purrr::map_chr(
            fields,
            ~ paste0(stringr::str_to_title(.x), ":\n", data[[.x]][i])
          )
          paste(msgs, collapse = "\n\n")
        }
      )
    )

    pending <- df_text[!df_text$text %in% existing,]

    cli::cli_inform(
      "Strategy '{strat}': {nrow(pending)} new documents out of {nrow(data)} to embed"
    )

    if (nrow(pending) > 0) {

      batch_size <- body(embedder)[["batch_size"]] %||% 100L

      batches <- split(pending, (seq_len(nrow(pending)) - 1L) %/% batch_size)

      insert_one <- function(batch_df) {
        ragnar::ragnar_store_insert(st, batch_df)
        TRUE
      }

      wrapped <- purrr::insistently(
        insert_one,
        rate = purrr::rate_backoff(
          retry_strategy$pause_base,
          max_times = retry_strategy$max_times,
          pause_cap = retry_strategy$pause_cap
        ),
        quiet = FALSE
      )
      purrr::walk(batches, wrapped, .progress = TRUE)
    }

  }

  scores_list <- purrr::map(
    stores,
    ~ ragnar::ragnar_retrieve_vss(
      .x,
      query,
      method = similarity_metric,
      top_k = nrow(data)
    ) |>
      dplyr::select(id, score = metric_value)
  )

  # Combine weighted scores
  combined <- purrr::reduce2(
    scores_list,
    weights,
    .init = NULL,
    function(acc, sc, w) {
      sc <- sc |> dplyr::mutate(score = .data$score * w)
      if (is.null(acc)) sc else
        acc |>
          dplyr::inner_join(sc, by = "id", suffix = c("", "_new")) |>
          dplyr::mutate(score = .data$score + .data$score_new) |>
          dplyr::select("id", "score")
    }
  )

  # Compute sorting_id by rank of score
  desc_rank <- if (similarity_metric %in% c("cosine_similarity", "dot_product"))
    TRUE else FALSE

  combined <- combined |>
    dplyr::mutate(
      sorting_id = if (desc_rank) dplyr::dense_rank(-.data$score) else
        dplyr::dense_rank(.data$score)
    )

  # Drop stale ranking columns before attaching fresh retrieval scores.
  out_data <- data |>
    dplyr::select(-dplyr::any_of(c("sorting_id", "embedding_score"))) |>
    dplyr::left_join(combined, by = "id") |>
    dplyr::rename(embedding_score = "score")

  result <- list(data = out_data)

  if (return_embeddings) {
    emb_list <- purrr::imap(stores, \(st, strat) {
      dplyr::tbl(st@.con, "chunks") |>
        dplyr::select("id", "embedding") |>
        dplyr::mutate(strategy = strat) |>
        dplyr::collect() # collect since it's a db connection until now
    })
    embs <- dplyr::bind_rows(emb_list)
    # annotate with sorting_id
    embs <- dplyr::left_join(
      embs,
      combined |> dplyr::select("id", "sorting_id"),
      by = "id"
    )
    result$embeddings <- embs
  }

  result
}

#' Connect or create a DuckDB‑backed *ragnar* store
#'
#' Establishes a connection to a DuckDB‐based embedding store used by
#' **ragnar**.  The function will reuse an existing on‑disk database (or
#' in‑memory store cached in `options()`), or create a fresh one when
#' necessary.  It prints detailed diagnostics with **cli** unless
#' `verbose = FALSE`, and never overwrites an existing file unless
#' `overwrite = TRUE`.
#'
#' @section File‑naming logic:
#' * If `store_location` is **`NULL`** the store lives in RAM
#'   (`":memory:"`) and is cached under
#'   `options("baysren.embedding_stores.<strat>")`.
#' * Otherwise the base file is
#'   `file.path(store_location, paste0(strat, ".sqlite"))` after stripping
#'   any trailing `.sqlite` or `.wal` already present in `strat`.
#' * DuckDB writes a companion `*.sqlite.wal` automatically while the
#'   connection is open; it disappears after a clean checkpoint/close.
#'
#' @param strat Character scalar.  Logical name of the strategy
#'   (e.g. `"all_fields"`).
#' @param store_location Character scalar or `NULL`.  Directory in which to
#'   place the database file, or `NULL` for an in‑memory store.
#' @param embedder A function taking a character vector and returning a
#'   numeric matrix of embeddings; passed unchanged to
#'   `ragnar::ragnar_store_create()`.
#' @param overwrite Logical.  If `FALSE` (default) the function aborts
#'   rather than clobber an existing but unreadable database.
#' @param verbose Logical.  Emit human‑friendly progress messages with
#'   **cli** (default `TRUE`).
#'
#' @return An object of class `ragnar::DuckDBRagnarStore`.  When the store
#'   is in‑memory the returned connection is also cached in
#'   `options("baysren.embedding_stores.<strat>")` for reuse in the current
#'   R session.
#'
#' @examples
#' \dontrun{
#' st <- connect_embedding_store(
#'   strat           = "all_fields",
#'   store_location  = "experiments/embeddings_classification/nudging",
#'   embedder        = ragnar::embed_openai(model = "embed‑v4.0"),
#'   overwrite       = FALSE
#' )
#' }
#'
#' @keywords internal
connect_embedding_store <- function(
  strat,
  store_location,
  embedder,
  overwrite = FALSE,
  verbose   = TRUE
) {
  # session‑level key for caching in‑memory stores
  opt_key <- paste0("baysren.embedding_stores.", strat)

  # return cached in‑memory store if available
  if (is.null(store_location) && !is.null(getOption(opt_key))) {
    if (verbose) cli::cli_alert_info("↪ re‑using cached in‑memory store `{strat}`")
    return(getOption(opt_key))
  }

  # helper: coerce “foo”, “foo.sqlite”, or “foo.sqlite.wal” → “foo.sqlite”
  normalise <- function(x)
    sub("(\\.sqlite(\\.wal)?)?$", ".sqlite", x, perl = TRUE)

  # derive canonical base & WAL paths
  if (is.null(store_location)) {
    db_path  <- ":memory:"
    wal_path <- NA_character_
  } else {
    db_path  <- fs::path(store_location, normalise(strat))
    wal_path <- paste0(db_path, ".wal")
  }

  # optional diagnostics
  if (verbose) {
    cli::cli_h1("Connecting strategy `{strat}`")
    cli::cli_alert_info("base path : {db_path}")
    if (!is.na(wal_path)) cli::cli_alert_info("WAL  path : {wal_path}")
  }

  # try opening an existing on‑disk store
  store <- NULL
  if (db_path != ":memory:" && fs::file_exists(db_path)) {
    if (verbose) cli::cli_alert_info("↪ attempting ragnar_store_connect()")
    store <- tryCatch(
      ragnar::ragnar_store_connect(db_path, read_only = FALSE),
      error = function(e) {
        if (verbose)
          cli::cli_alert_warning("connect failed → {e$message}")
        NULL
      }
    )
  }

  # create new store if needed
  if (is.null(store)) {
    if (fs::file_exists(db_path) && !overwrite) {
      cli::cli_abort(c(
        "!" = "Store exists but could not be opened.",
        "i" = "Set {.code overwrite = TRUE} if you really want to recreate it."
      ))
    }

    if (verbose)
      cli::cli_alert_info("↪ creating NEW store (overwrite = {overwrite})")

    if (db_path != ":memory:")
      fs::dir_create(fs::path_dir(db_path), recurse = TRUE)

    store <- ragnar::ragnar_store_create(
      location  = db_path,
      embed     = embedder,
      overwrite = TRUE
    )
  }

  # cache in‑memory store
  if (db_path == ":memory:") {
    opts <- options()
    opts[[opt_key]] <- store
    options(opts)
    if (verbose) cli::cli_alert_success("✔ cached in‑memory store `{strat}`")
  } else if (verbose) {
    size <- if (fs::file_exists(db_path)) fs::file_size(db_path) else 0
    cli::cli_alert_success(
      "✔ connected; base file size = {format(size, big.mark = ',')} bytes"
    )
  }

  invisible(store)
}
