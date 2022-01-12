#' Transform a year range into the format required by a specific search engine
#'
#' Take a period definition and transform it into a specific format passed
#' through the \code{cases} argument.
#'
#' The input period specified in \code{year_query} must follow this format:
#' \itemize{\item{\code{> Year}: }{greater than \code{Year}.}\item{\code{>=
#' Year}: }{greater then or equal to \code{Year}.}\item{\code{< Year}: }{lower
#' than \code{Year}.}\item{\code{<= Year}: }{lower then or equal to
#' \code{Year}.}\item{\code{Year}: }{equal to
#' \code{Year}.}\item{\code{Year1-Year2}: }{range from \code{Year1} to
#' \code{Year2}.}}
#'
#' The input is checked to evaluate if \code{year_query} the format is clearly
#' invalid.
#'
#' @param year_query A string identifying a search period (see Details).
#' @param cases The search engine syntax specification into which convert the
#'   year query It needs to be a list with the following items: gt (greater
#'   than), ge (greater than or equal to), lt (lower than), le (lower than or
#'   equal to), eq (equal to), range (range of years). Inside the specification,
#'   the year in the query can be retrieved with the \code{year_piece}
#'   placeholder, which in the case of a closed range is a vector of two
#'   elements (see examples).
#' @param arg_in_query_test A search engine syntax tag that identifies a year
#'   filter. Useful to define an already existing year filter in the query. If
#'   \code{arg_in_query_test} is set, also the \code{query} argument must be
#'   set.
#' @param query The search query. Needed to test if a year filter is already
#'   present. If \code{query} is set, also the \code{arg_in_query_test} argument
#'   must be set.
#'
#' @return The formatted, search engine specific, period filter.
#'
#' @export
#'
#' @examples
#'
#' query <- "Test query"
#' year_query <- "2010-2020"
#'
#'
#' ## Example for Web Of Science
#' year_query <- clean_date_filter_arg(year_query,
#'   cases = list(
#'     gt = "{year_piece + 1}-{year(today())}", ge = "{year_piece}-{year(today())}",
#'     eq = "{year_piece}-{year_piece}", le = "1985-{year_piece}",
#'     range = "{year_piece[1]}-{year_piece[2]}", lt = "1985-{year_piece - 1}"
#'   ),
#'   arg_in_query_test = "PY ?=", query = query
#' )
#'
#' ## Example for Pubmed
#' year_query <- "2010-2020"
#'
#' year_query <- clean_date_filter_arg(year_query,
#'   cases = list(
#'     gt = "{year_piece + 1}[PDAT]:{year(today())}[PDAT]",
#'     ge = "{year_piece}[PDAT]:{year(today())}[PDAT]",
#'     eq = "{year_piece}[PDAT]:{year_piece}[PDAT]",
#'     le = "1000[PDAT]:{year_piece}[PDAT]",
#'     lt = "1000[PDAT]:{year_piece - 1}[PDAT]",
#'     range = "{year_piece[1]}[PDAT]:{year_piece[2]}[PDAT]"
#'   ),
#'   arg_in_query_test = "[PDAT]", query = query
#' )
#'
clean_date_filter_arg <- function(year_query, cases,
                                  arg_in_query_test = NULL, query = NULL) {
  if (class(year_query) %nin% c("NULL", "character") | length(year_query) > 1) {
    stop("Year filter query should be a single character string or NULL")
  }

  if (is.character(year_query)) {
    year_query <- stringr::str_remove_all(year_query, "\\s+|^\\(|\\)$")

    if (!is.null(arg_in_query_test) & !is.null(query)) {
      if (stringr::str_detect(query, stringr::fixed(arg_in_query_test))) {
        warning("Year filter already in query. The query will be used", call. = FALSE, immediate. = TRUE)
        return(NULL)
      }
    }

    if (stringr::str_detect(year_query, "^\\d{4}-\\d{4}$")) { # range

      year_piece <- stringr::str_split(year_query, "-") %>% unlist()

      if (year_piece[2] < year_piece[1]) warning("Years' order seems wrong, please check it.", call. = FALSE, immediate. = TRUE)

      year_query <- glue(cases$range)
    } else if (stringr::str_detect(year_query, "^(<|<=|>=|>)\\d{4}$")) { # boundary

      pieces <- stringr::str_split(year_query, "\\b") %>% unlist()
      comparator <- pieces[1]
      year_piece <- as.numeric(pieces[2])

      year_query <- switch(comparator,
        ">" = glue(cases$gt),
        ">=" = glue(cases$ge),
        "<=" = glue(cases$le),
        "<" = glue(cases$lt)
      )
    } else if (stringr::str_detect(year_query, "^\\d{4}$")) {
      year_piece <- year_query
      year_query <- glue(cases$eq)
    } else {
      stop("Year filter query is malformed. Possible patterns are:
	gt: > Year;
	ge: >= Year;
	lt: < Year;
	le: <= Year;
	eq: Year;
	range: Year1 - Year2")
    }
  }

  as.character(year_query)
}

#'Automatic search on Web Of Science database
#'
#'This function performs an API search on Web Of Science (WOS), taking care of
#'the authorization steps and query normalization. The user is needed to provide
#'an API key by accessing \url{https://www.webofknowledge.com} with an
#'authorized account (e.g. access through an academic VPN or proxy). The key can
#'be found in the URL once the user is authorized. The key has a time limit, so
#'it will need to be regenerated. The function is a wrapper over
#'[wosr::pull_wos()] and requires the `wosr` package.
#'
#'@param query A boolean query with AND/OR/NOT operators, brackets for term
#'  grouping and quotation marks for n-grams.
#'@param year_query A year based filtering query. See [clean_date_filter_arg()]
#'  for more info.
#'@param additional_fields Additional fields to add to the query. Won't be
#'  normalized so it must already follow WOS specific syntax.
#'@param api_key Necessary to access WOS database. See Details.
#'@param parallel Whether to use parallel execution to speed up result
#'  collection. Works only on Unix-based systems.
#'@param parse_query Whether to normalize the query into WOS specific syntax. If
#'  \code{FALSE}, it is assumed that the query is already in the format required
#'  by WOS API.
#'@param ... Additional arguments for [wosr::pull_wos()], excluding \code{query}
#'  and \code{sid}.
#'
#'@return A data frame of records.
#'
#'@export
#'
#' @examples
#'
#' # Initial query to be built on domain knowledge. It accepts OR, AND, NOT
#' # boolean operators and round brackets to group terms.
#' query <- '((model OR models OR modeling OR network OR networks) AND
#' (dissemination OR transmission OR spread OR diffusion) AND (nosocomial OR
#' hospital OR "long-term-care" OR "long term care" OR "longterm care" OR
#' "long-term care" OR "healthcare associated") AND (infection OR resistance OR
#' resistant))'
#'
#' # Year filter. The framework converts it to the API-specific format
#' # seamlessly. Common logical comparators can be used, i.e. <, <=, >, >=,
#' # while dashes denotes inclusive date intervals. A single year restricts
#' # results to one year period.
#' year_filter <- "2010-2020"
#'
#'\dontrun{
#' records <- search_wos(query, year_filter)
#'}
search_wos <- function(query, year_query = NULL, additional_fields = NULL,
                       api_key = getOption("baysren.wos_api_key"),
                       parallel = TRUE, parse_query = TRUE, ...) {

	# Silence CMD CHECK about non standard eval
	schema <- ut <- title <- abstract <- doi <- journal <- tot_cites <- display_name <- jsc <- doc_type <- keyword <- keywords_plus <- NULL

	output_template <- c(Order = "integer", ID = "character", Title = "character", Abstract = "character",
											 DOI = "character", Journal = "character", N_citations = "integer",
											 Published = "character", Source = "character", Source_type = "character",
											 Creation_date = "character", Authors = "character", Topic = "character",
											 Article_type = "character", Author_keywords = "character", Keywords = "character"
	)

	# Declare some non-exported wosr functions
	one_parse <- utils::getFromNamespace("one_parse", "wosr")
	download_wos <- utils::getFromNamespace("download_wos", "wosr")
	data_frame_wos <- utils::getFromNamespace("data_frame_wos", "wosr")
	process_wos_apply <- utils::getFromNamespace("process_wos_apply", "wosr")
	enforce_schema <- utils::getFromNamespace("enforce_schema", "wosr")
	append_class <- utils::getFromNamespace("append_class", "wosr")

	if (!requireNamespace("wosr", quietly = TRUE)) {
		ans <- ask_user_permission(
			q = "Package 'wosr' is required to download results from the WOS database. Do you want to install it now? y/n",
			y_action = function() {
				utils::install.packages("wosr")
				return(TRUE)
			},
			n_action = function() return(FALSE)
		)

		if (isFALSE(ans)) {
			warning("Research on WOS database will be skipped", call. = FALSE, immediate. = TRUE)

			return(create_empty_df(output_template))
		}
	}

  message("Searching WOS...")

  default_field <- "TS" # may change in the future

  if (parallel) { ## Use mclapply which is faster
    pull_records <- function(query, editions = c(
                               "SCI", "SSCI", "AHCI", "ISTP",
                               "ISSHP", "BSCI", "BHCI", "IC", "CCR", "ESCI"
                             ),
                             sid = wosr::auth(Sys.getenv("WOS_USERNAME"), Sys.getenv("WOS_PASSWORD")), ...) {
      parse_wos <- function(all_resps) {
        pbmcapply::pbmclapply(all_resps, one_parse)
      }

      qr_out <- wosr::query_wos(query,
        editions = editions, sid = sid,
        ...
      )
      if (qr_out$rec_cnt == 0) {
        dfs <- unique(schema$df)
        wos_unenforced <- vector("list", length = length(dfs))
        names(wos_unenforced) <- dfs
      } else {
        message("- fetching records")
        all_resps <- download_wos(qr_out, ...)
        all_resps <- all_resps[vapply(all_resps, length, numeric(1)) >
          1]
        message("- parsing results")
        parse_list <- parse_wos(all_resps)
        df_list <- data_frame_wos(parse_list)
        wos_unenforced <- process_wos_apply(df_list)
      }
      wos_data <- enforce_schema(wos_unenforced)
      append_class(wos_data, "wos_data")
    }
  } else {
    pull_records <- wosr::pull_wos
  }


  if (parse_query) {
    query <- stringr::str_squish(query)

    if (stringr::str_detect(query, "^\\w+ ?= ?")) {
      pieces <- stringr::str_split(query, "( ?AND ?)?\\w{2} ?= ?") %>%
        unlist() %>%
        stringr::str_squish() %>%
        stringr::str_subset("^$", negate = TRUE)

      fields <- stringr::str_extract_all(query, "\\w{2} ?= ?") %>%
        unlist() %>%
        stringr::str_squish() %>%
        stringr::str_remove(" ?= ?")

      query <- setNames(pieces, fields)
    } else {
      query <- setNames(glue("{query}"), default_field)
    }

    if (is.character(year_query)) {
      year_query <- clean_date_filter_arg(year_query,
        cases = list(
          gt = "{year_piece + 1}-{year(today())}", ge = "{year_piece}-{year(today())}",
          eq = "{year_piece}-{year_piece}", le = "1985-{year_piece}",
          range = "{year_piece[1]}-{year_piece[2]}", lt = "1985-{year_piece - 1}"
        ),
        arg_in_query_test = "PY ?=", query = query
      )

      additional_fields <- c(
        setNames(year_query, "PY"),
        additional_fields[names(additional_fields) %nin% "PY"]
      )
    }

    query <- c(query, additional_fields)

    query <- paste(
      glue("{names(query)} = ({query})"),
      collapse = " AND "
    )
  }

  records_list <- tryCatch(
    pull_records(query, sid = api_key, ...),
    error = function(e) stop(e, glue("\n\nquery: {query}"))
  )

  if (is.null(records_list$publication)) {
  	warning("The query returned zero results.", call. = FALSE, immediate. = TRUE)
  	return(create_empty_df(output_template))
  }

  records <- records_list$publication %>%
    transmute(
      Order = 1:n(), ID = ut, Title = title, Abstract = abstract,
      DOI = doi, Journal = journal, N_citations = tot_cites,
      Published = format(lubridate::ymd(date), "%b %Y"), Source = "WOS",
      Source_type = "API", Creation_date = safe_now()
    )

  additional_infos <- list(
    authors = records_list$author %>% group_by(ID = ut) %>%
      summarise(Authors = paste(display_name, collapse = "; ")),
    topics = records_list$jsc %>% group_by(ID = ut) %>%
      summarise(Topic = paste(jsc, collapse = "; ")),
    art_type = records_list$doc_type %>% group_by(ID = ut) %>%
      summarise(Article_type = paste(doc_type, collapse = "; ")),
    auth_keys = records_list$keyword %>% group_by(ID = ut) %>%
      summarise(Author_keywords = paste(keyword, collapse = "; ")),
    keys = records_list$keywords_plus %>% group_by(ID = ut) %>%
      summarise(Keywords = paste(keywords_plus, collapse = "; "))
  )

  for (info in additional_infos) records <- left_join(records, info, by = "ID")

  records <- records %>% clean_record_textfields()

  message("...found ", nrow(records), " records.")

  records
}


#' Automatic search on Pubmed database
#'
#' Perform an API search using Pubmed E-utilities
#' \url{https://www.ncbi.nlm.nih.gov/books/NBK25501/}.
#'
#' An API key is not mandatory but may avoid quota limitation for searches that
#' return a large number of results. Large results sets are obtained by
#' iterative querying.
#'
#' Requires the `rentrez` package.
#'
#' @param query A boolean query with AND/OR/NOT operators, brackets for term
#'   grouping and quotation marks for n-grams.
#' @param year_query A year based filtering query. See [clean_date_filter_arg()]
#'   for more info.
#' @param additional_fields Additional fields to add to the query. Will not be
#'   normalized, so it must already follow Pubmed specific syntax.
#' @param api_key Not mandatory but is helpful when performing searches with a
#'   large number of results to avoid quota limitations.
#' @param record_limit A limit on the number of records collected.
#'
#' @return A data frame of records.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Initial query to be built on domain knowledge. It accepts OR, AND, NOT
#' # boolean operators and round brackets to group terms.
#' query <- '((model OR models OR modeling OR network OR networks) AND
#' (dissemination OR transmission OR spread OR diffusion) AND (nosocomial OR
#' hospital OR "long-term-care" OR "long term care" OR "longterm care" OR
#' "long-term care" OR "healthcare associated") AND (infection OR resistance OR
#' resistant))'
#'
#' # Year filter. The framework converts it to the API-specific format seamlessly.
#' # common logical comparators can be used, i.e. <, <=, >, >=, while dashes
#' # denotes inclusive date intervals. A single year restricts results to one year
#' # period.
#' year_filter <- "2010-2020"
#'
#' records <- search_pubmed(query, year_filter)
#' }
search_pubmed <- function(query, year_query = NULL, additional_fields = NULL,
                          api_key = getOption("baysren.ncbi_api_key"),
                          record_limit = numeric()) {

	output_template <- c(Order = "integer", ID = "character", Title = "character", Abstract = "character",
											 DOI = "character", Authors = "character", URL = "character",
											 Journal = "character", Journal_short = "character", Article_type = "character",
											 Mesh = "character", Author_keywords = "character", Published = "character",
											 Source = "character", Source_type = "character", Creation_date = "POSIXct"
	)

  message("Searching Pubmed...")

	if (!requireNamespace("rentrez", quietly = TRUE)) {
		ans <- ask_user_permission(
			q = "Package 'wosr' is required to download results from the PUBMED database. Do you want to install it now? y/n",
			y_action = function() {
				utils::install.packages("rentrez")
				return(TRUE)
			},
			n_action = function() return(FALSE)
		)

		if (isFALSE(ans)) {
			warning("Research on PUBMED database will be skipped", call. = FALSE, immediate. = TRUE)

			return(create_empty_df(output_template))
		}
	}

  if (is.null(api_key)) warning("NCBI API key is not set.", call. = FALSE, immediate. = TRUE)

  query <- stringr::str_squish(query)

  year_query <- clean_date_filter_arg(year_query,
    cases = list(
      gt = "{year_piece + 1}[PDAT]:{year(today())}[PDAT]",
      ge = "{year_piece}[PDAT]:{year(today())}[PDAT]",
      eq = "{year_piece}[PDAT]:{year_piece}[PDAT]",
      le = "1000[PDAT]:{year_piece}[PDAT]",
      lt = "1000[PDAT]:{year_piece - 1}[PDAT]",
      range = "{year_piece[1]}[PDAT]:{year_piece[2]}[PDAT]"
    ),
    arg_in_query_test = "[PDAT]", query = query
  )

  year_query <- glue("({year_query})") # adding parenthesis around the dates

  if (!is.null(additional_fields)) {
    additional_fields <- paste(glue("({additional_fields})[{names(additional_fields)}]"), collapse = " AND ")
  }

  query <- paste(query, year_query, additional_fields, collapse = " AND ") %>% stringr::str_squish()

  res <- rentrez::entrez_search(
    db = "pubmed", term = query, retmax = 0,
    api_key = api_key, use_history = TRUE
  )

  total_count <- min(res$count, record_limit)

  if (total_count == 0) {
  	warning("The query returned zero results.", call. = FALSE, immediate. = TRUE)
  	return(create_empty_df(output_template))
  }

  message("- fetching records")

  steps <- floor((total_count - 1) / min(total_count, 200))

  # ~ 20x faster than pubmedR::pmApiRequest plus xml parsing
  records <- pbmcapply::pbmclapply(0:steps, function(step) {
    # print(paste(step * 200))
    have.results <- F
    trials <- 0

    while (!have.results & trials < 20) {
      records <- try(rentrez::entrez_fetch(
        db = "pubmed", web_history = res$web_history,
        retstart = step * 200, retmax = min(200, total_count - step * 200),
        rettype = "medline", parsed = FALSE,
        api_key = api_key
      ), silent = TRUE)

      have.results <- class(records) == "character"
      trials <- trials + 1
    }

    records
  })

  if (sapply(records, function(x) class(x) == "try-error") %>% any()) {
    stop("Couldn't get results for some steps after 20 attempts")
  }

  message("- parsing results")

  records <- parse_pubmed(records %>% unlist() %>% paste(collapse = "\\n\\n")) %>%
    mutate(Source_type = "API")

  message("...found ", nrow(records), " records.")

  records
}

#' Automatic search on IEEE database
#'
#' Perform a search on \url{https://ieeexplore.ieee.org/Xplore/home.jsp}.
#'
#' If an API key is available, the IEEE API will be used, otherwise Google
#' Chrome APIs through the `crrri` package will be used to scrape records
#' simulating a manual user search. This second method is not ensured to work
#' and IEEE may blacklist your IP if abused.
#'
#' @param query A boolean query with AND/OR/NOT operators, brackets for term
#'   grouping and quotation marks for n-grams.
#' @param year_query A year based filtering query. See [clean_date_filter_arg()]
#'   for more info.
#' @param additional_fields Additional fields to add to the query. Will not be
#'   normalized, so it must already follow WOS specific syntax.
#' @param api_key Necessary to use IEEE APIs. See details.
#' @param allow_web_scraping If \code{api_key} is \code{NULL}, web scraping can
#'   be attempted to collect the records. This approach is not suggested in
#'   production and may fail with a large number of results. See Details.
#' @param wait_for If web scraping is used, a certain amount of time is needed
#'   to let the IEEE page load completely before collecting the results. This
#'   delay depends on the user network and browser speed, thus the default 20
#'   seconds may not be sufficient.
#' @param record_limit A limit on the number of records collected.
#'
#' @return A data frame of records.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Initial query to be built on domain knowledge. It accepts OR, AND, NOT
#' # boolean operators and round brackets to group terms.
#' query <- '((model OR models OR modeling OR network OR networks) AND
#' (dissemination OR transmission OR spread OR diffusion) AND (nosocomial OR
#' hospital OR "long-term-care" OR "long term care" OR "longterm care" OR
#' "long-term care" OR "healthcare associated") AND (infection OR resistance OR
#' resistant))'
#'
#' # Year filter. The framework converts it to the API-specific format seamlessly.
#' # common logical comparators can be used, i.e. <, <=, >, >=, while dashes
#' # denotes inclusive date intervals. A single year restricts results to one year
#' # period.
#' year_filter <- "2010-2020"
#'
#' records <- search_ieee(query, year_filter)
#' }
search_ieee <- function(query, year_query = NULL, additional_fields = NULL,
                        api_key = getOption("baysren.ieee_api_key"), allow_web_scraping = TRUE,
                        wait_for = 20, record_limit = NULL) {

	# Silence CMD CHECK about non standard eval
	articleNumber <- articleTitle <- doi <- authors <- publicationTitle <- contentType <- citationCount <- publicationDate <- abstract_url <- title <- abstract <- publication_title <- content_type <- citing_paper_count <- publication_date <- kwd <- firstName <- lastName <- Order <- ID <- Title <- Abstract <- DOI <- URL <- Authors <- Journal <- Article_type <- Author_keywords <- Keywords <- Mesh <- N_citations <- Published <- Source <- Source_type <- NULL

	output_template <- c(Order = "integer", ID = "character", Title = "character", Abstract = "character",
											 DOI = "character", URL = "character", Authors = "character",
											 Journal = "character", Article_type = "character", Author_keywords = "character",
											 Keywords = "character", N_citations = "integer", Published = "character",
											 Source = "character", Source_type = "character")

  message("Searching IEEE...")

  if (!is.null(additional_fields) & length(additional_fields) > 0) {
    if (!is.list(additional_fields)) stop("additional_fields must be a list.")

    additional_fields <- lapply(additional_fields, function(el) {
      if (el == TRUE) {
        "true"
      } else if (el == FALSE) {
        "false"
      } else {
        el
      }
    })
  }

  if (is.null(api_key)) {
    warning("IEEE API key is not set, defaulting to webscraping.", call. = FALSE, immediate. = TRUE)

    if (!allow_web_scraping) stop("If API key is not present web scraping must be allowed.")

  	if (!requireNamespace("crrri", quietly = TRUE)) {
  		ans <- ask_user_permission(
  			q = "Package 'crrri' is required for non-API downoload of results from IEEE. Do you want to install it now? y/n",
  			y_action = function() {
  				utils::install.packages("crrri")
  				return(TRUE)
  				},
  			n_action = function() {
  				return(FALSE)
  			}
  		)

  		if (isFALSE(ans)) {
  			warning("Research on IEEE database will be skipped", call. = FALSE, immediate. = TRUE)

  			return(create_empty_df(output_template))
  		}
  	}

    default_fields <- list(
      rowsPerPage = 100
    )

    default_fields[names(additional_fields)] <- NULL

    additional_fields <- c(default_fields, additional_fields)

    additional_fields$rowsPerPage <- min(as.numeric(additional_fields$rowsPerPage), record_limit)

    if ("ranges" %nin% names(additional_fields) & !is.null(year_query)) {
      year_arg <- clean_date_filter_arg(year_query, cases = list(
        gt = "{year_piece + 1}_{year(today())}",
        ge = "{year_piece}_{year(today())}",
        eq = "{year_piece}_{year_piece}",
        range = "{year_piece[1]}_{year_piece[2]}",
        le = "1800_{year_piece}",
        lt = "1800_{year_piece - 1}"
      ))

      additional_fields$ranges <- glue("{year_arg}_Year")
    }

    endpoint <- httr::parse_url("https://ieeexplore.ieee.org/search/searchresult.jsp?")

    endpoint$query <- c(queryText = query, additional_fields) %>% lapply(stringr::str_squish)

    url <- httr::build_url(endpoint)

    message("- fetching records")
    response <- get_website_resources(
      url = url, url_filter = "rest/search",
      type_filter = "XHR", wait_for = wait_for
    )

    if (length(response) == 0) {
      stop("No results were scraped. Try using a longer wait_time to allow for more time to load results.")
    }

    response <- jsonlite::fromJSON(response[[1]]$response$body)

    records <- response$records

    if (response$totalPages > 1) {
      if (response$totalPages > 100) {
      	warning("Only results up to page 100 are available", call. = FALSE, immediate. = TRUE)
      }

      other_pages <- pbapply::pblapply(2:min(response$totalPages, 100), function(page) {
        endpoint$query$pageNumber <- page

        if (page * endpoint$query$pageNumber > record_limit) {
          endpoint$query$rowsPerPage <- record_limit - ((page - 1) * endpoint$query$rowsPerPage)
        }

        url <- httr::build_url(endpoint)

        response <- get_website_resources(
          url = url, url_filter = "rest/search",
          type_filter = "XHR", wait_for = wait_for
        )

        response$records
      })

      records <- bind_rows(records, other_pages)
    }

    if (is.null(response$records)) {
    	warning("The query returned zero results.", call. = FALSE, immediate. = TRUE)
    	return(create_empty_df(output_template))
    }

    records <- response$records %>%
      transmute(
        Order = 1:n(),
        ID = paste0("IEEE:", articleNumber),
        Title = articleTitle,
        Abstract = abstract,
        DOI = doi, URL = paste0("https://ieeexplore.ieee.org/document/", articleNumber),
        Authors = authors %>% sapply(function(df) paste(df$normalizedName, collapse = "; ")),
        Journal = publicationTitle,
        Article_type = stringr::str_remove(`contentType`, "(IEEE|OUP) "), # there may be more...
        N_citations = citationCount,
        Published = publicationDate
      )

    # if (!is.null(record_limit)) records <- head(records, record_limit)
  } else {
    default_fields <- list(
      max_records = 200
    )

    default_fields[names(additional_fields)] <- NULL

    additional_fields <- c(default_fields, additional_fields)

    additional_fields$max_records <- min(as.numeric(additional_fields$max_records), record_limit)

    if (all(c("start_year", "end_year") %nin% names(additional_fields)) & !is.null(year_query)) {
      year_arg <- clean_date_filter_arg(year_query, cases = list(
        gt = "{year_piece + 1}_x",
        ge = "{year_piece}_x",
        eq = "{year_piece}_{year_piece}",
        range = "{year_piece[1]}_{year_piece[2]}",
        le = "x_{year_piece}",
        lt = "x_{year_piece - 1}"
      )) %>%
        stringr::str_split("_") %>%
        unlist() %>%
        setNames(c("start_year", "end_year")) %>%
        Filter(f = function(el) el != "x")

      additional_fields <- c(additional_fields, year_arg)
    }

    endpoint <- "http://ieeexploreapi.ieee.org/api/v1/search/articles"

    query <- c(
      querytext = query, additional_fields,
      apikey = api_key, format = "json"
    ) %>% lapply(stringr::str_squish)

    message("- fetching records")

    response <- httr::GET(url = endpoint, query = query)

    if (response$status_code != 200) {
      stop("Error fetching results, with code", response$status_code, "

					 ", httr::content(response, "text"))
    }

    results <- httr::content(response, "text") %>% jsonlite::fromJSON()

    records <- results$articles

    if (is.null(records)) {
    	warning("The query returned zero results.", call. = FALSE, immediate. = TRUE)
    	return(create_empty_df(output_template))
    }

    max_records <- additional_fields$max_records

    if (results$total_records > max_records) {
      total_count <- min(results$total_records, record_limit)

      steps <- floor((total_count - 1) / min(total_count, max_records))

      other_pages <- pbapply::pblapply(1:steps, function(step) {
        print(paste(step * max_records + 1))

        query$start_record <- step * max_records + 1
        query$max_records <- min(max_records, total_count - step * max_records)

        response <- httr::GET(url = endpoint, query = query)

        if (response$status_code != 200) {
          stop("Error fetching results, with code", response$status_code, "

					 ", httr::content(response, "text"))
        } else {
          results <- httr::content(response, "text") %>% jsonlite::fromJSON()

          results$articles
        }
      }) %>% bind_rows()

      records <- bind_rows(records, other_pages)
    }

    records <- records %>%
      transmute(
        Order = 1:n(),
        ID = paste0("IEEE:", stringr::str_extract(abstract_url, "\\d+/$")),
        Title = title,
        Abstract = abstract,
        DOI = doi,
        URL = abstract_url,
        Journal = publication_title,
        Article_type = content_type,
        N_citations = citing_paper_count,
        Published = publication_date,
      )
  }

  message("- fetching individual article data")

  article_data <- pbmcapply::pbmclapply(records$URL, function(URL) {
    if (!stringr::str_detect(URL, stringr::fixed("https://ieeexplore.ieee.org/document/"))) {
      return(NULL)
    }

    data <- readr::read_file(URL) %>%
      stringr::str_extract("(?<=xplGlobal\\.document\\.metadata=).+") %>%
      stringr::str_remove(";$") %>%
      jsonlite::fromJSON()

    if (!is.null(data$keywords)) {
      Keys <- data$keywords %>%
        group_by(type = case_when(
          stringr::str_detect(type, "MeSH") ~ "Mesh",
          stringr::str_detect(type, "Author") ~ "Author",
          TRUE ~ "IEEE"
        )) %>%
        summarise(kwd = paste(unlist(kwd), collapse = "; "))

      Keys <- setNames(as.list(Keys$kwd), Keys$type)
    } else {
      Keys <- data.frame(IEEE = NA, Mesh = NA, Author = NA)
    }

    ret <- bind_cols(
      URL = URL,
      Keywords = Keys$IEEE,
      Mesh = Keys$Mesh,
      Author_keywords = Keys$Author,
    )

    if ("Authors" %nin% names(records)) {
      ret$Authors <- data$authors %>%
        as.data.frame() %>%
        transmute(
          firstName = if (exists("firstName")) firstName else NA,
          lastName = if (exists("lastName")) lastName else NA,
          across(c(firstName, lastName), ~ replace(.x, is.na(.x), ""))
        ) %>%
        with(paste(
          stringr::str_replace_all(firstName, c(
            "[^\\w]+" = " ",
            "\\b(\\w)\\w*" = "\\1."
          )),
          lastName,
          collapse = "; "
        ))
    }

    ret
  }) %>% bind_rows()

  records <- left_join(records, article_data, by = "URL") %>%
    mutate(Source = "IEEE", Source_type = "API", Creation_date = now()) %>%
    clean_record_textfields() %>%
    select(
      Order, ID, Title, Abstract, DOI, URL, Authors, Journal,
      Article_type, Author_keywords, Keywords, Mesh, N_citations,
      Published, Source, Source_type
    )

  message("...found ", nrow(records), " records.")

  records
}

#' Wrapper function to acquire citation data from multiple sources
#'
#' It is better to use this function instead of the individual \code{search_*}
#' tools, since it also automatically acquires manually downloaded records
#' (e.g., for EMBASE and SCOPUS for which automatic search is not available).
#'
#' The function organizes search results into folder defined by the pattern
#' \code{records_folder}/\code{session_name}/\code{query_name}, which allows to
#' have different search sessions (and classification sessions) and multiple
#' queries per session (useful when it is too complex to convey all information
#' into a single query). These folders can be created manually and manually
#' downloaded citation data must be put into these folders to be acquired by the
#' functions.
#'
#' To acquire the manually downloaded files, they must be given a name
#' containing the source as in \code{sources}. There could be more files for the
#' same sources, since all research databases have download limits and users may
#' need to download results in batches. The function acquires these files and
#' parse them into a standard format, creating a new file for each source.
#'
#' The output is a "journal" file storing all information about the queries, the
#' sources used, the number of results, etc... which allow keeping track of all
#' search sessions. If a journal file is already present, the new results will
#' be added.
#'
#' @param query A boolean query with AND/OR/NOT operators, brackets for term
#'   grouping and quotation marks for n-grams.
#' @param year_query A year based filtering query. See [clean_date_filter_arg()]
#'   for more info.
#' @param actions Whether to acquire records through automatic search, parsing
#'   of manually downloaded data, or both.
#' @param sources The sources for which records should be collected
#' @param session_name How to name the current search session and will be used
#'   to create a folder to collect search results. It should be the same as the
#'   name used for classification session of the same records.
#' @param query_name A label for the current query. It will be used to name a
#'   folder inside the \code{session_name} folder. It is useful to separate
#'   records acquired with different queries in the same search session.
#' @param records_folder The path to a folder where to store search results.
#' @param overwrite Whether to overwrite results for a given
#'   \code{session_name}/\code{query_name}/\code{sources} if the search is
#'   repeated and a result file already exists.
#' @param journal A path to a file (Excel or CSV) to store a summary of the
#'   search results. If the file already exists, the summary of the new
#'   \code{session_name}/\code{query_name}/\code{sources} will be added to the
#'   file.
#'
#' @return A "Journal" data frame containing a summary of the search results
#'   grouped by
#'   \code{session_name}/\code{query_name}/\code{sources}/\code{actions}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Initial query to be built on domain knowledge. It accepts OR, AND, NOT
#' # boolean operators and round brackets to group terms.
#' query <- '((model OR models OR modeling OR network OR networks) AND
#' (dissemination OR transmission OR spread OR diffusion) AND (nosocomial OR
#' hospital OR "long-term-care" OR "long term care" OR "longterm care" OR
#' "long-term care" OR "healthcare associated") AND (infection OR resistance OR
#' resistant))'
#'
#' # Year filter. The framework converts it to the API-specific format seamlessly.
#' # common logical comparators can be used, i.e. <, <=, >, >=, while dashes
#' # denotes inclusive date intervals. A single year restricts results to one year
#' # period.
#' year_filter <- "2010-2020"
#'
#' journal <- perform_search_session(
#'   query = query, year_query = year_filter,
#'   session_name = "Session1", query_name = "Query1",
#'   records_folder = "Records",
#'   journal = "Session_journal.csv"
#' )
#' }
perform_search_session <- function(query, year_query = NULL, actions = c("API", "parsed"),
                                   sources = c("IEEE", "WOS", "Pubmed", "Scopus", "Embase"),
                                   session_name = "Session1", query_name = "Query1",
                                   records_folder = "Records", overwrite = FALSE,
                                   journal = "Session_journal.csv") {

	# Silence CMD CHECK about non standard eval
	write_csv <- Session_ID <- Query_ID <- Source <- Type <- Timestamp <- NULL

  load_if_exists <- function(out_file, overwrite) {
    if (file.exists(out_file)) {
      if (!overwrite) {
        warning(basename(out_file), " already present and argument overwrite is FALSE.", call. = FALSE, immediate. = TRUE)

        readr::read_csv(out_file, col_types = readr::cols())
      } else {
        warning(basename(out_file), " will be overwritten.", call. = FALSE, immediate. = TRUE)
        NULL
      }
    } else {
      NULL
    }
  }

  folder_path <- file.path(records_folder, session_name, query_name)

  if (!dir.exists(folder_path)) dir.create(folder_path, recursive = TRUE)

  search_ts <- now()

  input_files <- NA

  record_data <- lapply(sources, function(source) {
    lapply(actions, function(action) {
      records <- NULL

      if (action == "API") {
        ## API search

        output_file <- file.path(folder_path, glue("{source}_API.csv"))

        records <- load_if_exists(output_file, overwrite) # load records if output already existing

        search_fun <- paste0("search_", stringr::str_to_lower(source))

        if (is.null(records) & exists(search_fun)) { # if output not existing search via API and API search is available

          records <- get(search_fun)(query = query, year_query = year_query)
        }
      } else if (action == "parsed") {
        ## Parsing downloaded records

        # find input files (i.e. files not containing API or parsed in the name)
        input_files <- list.files(folder_path, full.names = FALSE) %>%
          stringr::str_subset("API|parsed", negate = TRUE) %>%
          stringr::str_subset(stringr::regex(source, ignore_case = TRUE))

        if (length(input_files) > 0) { # continue if any input file exists

          output_file <- file.path(folder_path, glue("{source}_parsed.csv"))

          records <- load_if_exists(output_file, overwrite) # load records if output already existing

          if (is.null(records)) { # in output not existing parse the raw data
            records <- file.path(folder_path, input_files) %>%
              read_bib_files() %>%
              bind_rows()
          }

          input_files <- paste(input_files, collapse = ", ")
        } else {
          input_files <- NA
        }
      }

      if (!is.null(records)) {
        records$FileID <- file.path(session_name, query_name, basename(output_file))

        readr::write_csv(records, output_file)

        data.frame(
          Session_ID = session_name,
          Query_ID = query_name,
          Source = source,
          Type = action,
          Input_files = input_files,
          Output_file = basename(output_file),
          Timestamp = search_ts,
          Filter = year_query,
          N_results = nrow(records),
          Query = query
        )
      }
    })
  }) %>% bind_rows()

  if (nrow(record_data) == 0) {
    warning("No records were added", call. = FALSE, immediate. = TRUE)
  }

  if (!is.null(journal)) {
    if (stringr::str_detect(journal, "\\.csv$")) {
      write_fun <- write_csv
    } else if (stringr::str_detect(journal, "\\.xlsx?$")) {
      write_fun <- function(x, file) openxlsx::write.xlsx(x, file, asTable = TRUE)
    } else {
      stop("Session journal file type must be in CSV or Excel format.")
    }

    if (file.exists(journal)) {
      previous_data <- import_data(journal)

      record_data <- previous_data %>%
        bind_rows(record_data) %>%
        group_by(Session_ID, Query_ID, Source, Type) %>%
        arrange(Timestamp, .by_group = TRUE) %>%
        summarise(across(.fns = last))
    }

    write_fun(record_data, journal)
  }

  record_data
}
