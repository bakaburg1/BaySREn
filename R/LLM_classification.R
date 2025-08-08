#' Classify citations against inclusion / exclusion rules with an LLM
#'
#' @param data  A data frame that contains at least the columns
#'              `title`, `abstract`, `authors`, `keywords`.
#' @param query A named list, e.g.
#'              list(include = "studies on ACE-inhibition in adults",
#'                   exclude = "animal models OR case reports").
#' @param chat  A *base* ellmer Chat object (default: \code{chat_openai()}).
#'              The object is cloned and its turns wiped so every prompt
#'              is independent.
#' @param max_active,rpm,cache_dir Passed on to \code{safe_parallel_chat()}.
#' @return       The original data frame with a new logical column `match`.
#'               The elapsed processing time of the `ellmer::parallel_chat` call
#'               is stored in the `processing_time` attribute.
#' @examples
#' data_with_match <- classify_citations(citations,
#'   query = list(include = "hypertension AND ACE blockade",
#'                exclude = "in vitro"),
#'   chat = chat_openai(model = "gpt-4o-mini")
#' )
#' table(data_with_match$match)
#' attr(data_with_match, "processing_time")
#' @export
#' @importFrom rlang .data

classify_citations <- function(
  data,
  query,
  chat = ellmer::chat_openai(),
  cache_dir = "parallel_cache_dir"
) {
  stopifnot(all(c("title", "abstract", "authors", "keywords") %in% names(data)))
  if (!is.list(query) || is.null(query$include))
    stop("`query` must be a list with at least element `include`")

  # Instantiate a fresh chat object
  chat_base <- chat$clone(deep = TRUE)$set_turns(list())

  # Build one prompt per citation
  # syst_prompt <- paste(
  #   "You are a field expert AI helping with a systematic review.",
  #   "You will be passed citation data relative to a scientific article and will need to decide whether this article is relevant or not according to the following criteria:\n\n",
  #   "Inclusion criteria:\n{{include}}\n",
  #   if (!is.null(query$exclude)) "Exclusion criteria:\n{{exclude}}\n" else NULL,
  #   "\nReturn ONLY TRUE or FALSE indicating whether the article",
  #   "should be included (TRUE) or excluded (FALSE)."
  # ) |>
  #   ellmer::interpolate(
  #     include = query$include,
  #     exclude = query$exclude %||% ""
  #   )

  syst_prompt <- paste(
    "You are a field expert AI helping with a systematic review.",
    "You will be passed citation data relative to a scientific article and will need to decide whether this article is relevant or not according to the following criteria.",
    "A study is considered included if it meets ALL theinclusion criteria.",
    "If a study meets ANY of the exclusion criteria, it must be excluded.\n\n",
    "Here are the sets of criteria:\n\n",
    "<inclusion criteria>\n{{include}}</inclusion criteria>\n\n",
    if (!is.null(query$exclude))
      "<exclusion criteria>\n{{exclude}}</exclusion criteria>\n\n" else NULL,
    "\nReview each criterion one by one, with a short analysis followed by a YES or NO label",
    "You must strictly apply the provided criteria. Do not take initiatives or decide to ignore any of them",
    "Finally, shortly justify your inclusion choice followed by TRUE or FALSE (all capital letters) indicating whether the article.",
    "Report TRUE or FALSE only once in your answer so that I can capture it via text matching."
  ) |>
    ellmer::interpolate(
      include = query$include,
      exclude = query$exclude %||% ""
    )

  query_prompts <- paste(
    "Title: {{title}}\nAbstract: {{abstract}}\n",
    "Authors: {{authors}}\nKeywords: {{keywords}}",
    sep = ""
  ) |>
    ellmer::interpolate(
      title = data$title,
      abstract = data$abstract,
      authors = data$authors,
      keywords = data$keywords
    )

  chat_base$set_system_prompt(syst_prompt)

  # Ask the model in parallel
  chats <- NULL
  processing_time <- system.time(
    chats <- parallel_chat_promises(
      chat = chat_base,
      prompts = query_prompts,
      rpm = 1000,
      cache_dir = cache_dir,
      backoff_base = 5,
      backoff_cap = 300
    )
  )

  # Extract results
  results <- chats |>
    purrr::map(\(chat) {
      # Guard missing chats or missing last turn
      if (is.null(chat) || length(chat$get_turns()) == 0) {
        return(c(prompt_tokens = NA_integer_, completion_tokens = NA_integer_, raw_matches = ""))
      }
      last <- chat$last_turn()
      usage <- tryCatch(last@json$usage[c("prompt_tokens", "completion_tokens")], error = function(e) c(prompt_tokens = NA_integer_, completion_tokens = NA_integer_))
      txt <- tryCatch(last@text, error = function(e) "")
      c(usage, raw_matches = txt)
    }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      # Extract the boolean value from the response
      matches = stringr::str_extract_all(.data$raw_matches, "TRUE|FALSE") |>
        purrr::map_chr(~ if (length(.x)) dplyr::last(.x) else NA_character_) |>
        as.logical(),
      model = chat_base$get_model()
    )

  if (any(is.na(results$matches))) {
    failed_list <- paste0(
      which(is.na(results$matches)),
      ": ",
      results$raw_matches[is.na(results$raw_matches)]
    )
    cli::cli_alert_warning("{sum(is.na(results$matches))} responses were not TRUE/FALSE: {failed_list}")
    invisible(failed_list) # Use the variable to avoid linter warning
  }

  # Bind result and return
  output <- dplyr::bind_cols(data, results)
  attr(output, "processing_time") <- processing_time[["elapsed"]]
  output
}
