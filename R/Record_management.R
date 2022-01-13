
#' Read a data file/object
#'
#' General wrapper that imports CSV/Excel/RDS files if a file path is given in
#' \code{input}. If \code{input} is a data frame, it will just be passed
#' through. An error is raised in the case of unrecognized objects/files.
#'
#' @param input A file path to an Excel/CSV/RDS file or a data frame object.
#' @param ... Additional arguments to pass to [readxl::read_excel()],
#'   [readr::read_csv()], or [readr::read_rds()], depending on the file type.
#'
#' @return A data frame object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # These all work:
#'
#' data <- import_data("data.csv")
#'
#' data <- import_data("data.xlsx", sheet = 2)
#'
#' data <- import_data("data.rds")
#'
#' data <- import_data(data)
#' }
import_data <- function(input, ...) {
  if (is.character(input) | is.factor(input)) {
    if (stringr::str_detect(input, "\\.xlsx?$")) {
      return(readxl::read_excel(input, guess_max = 10^6, ...))
    } else if (stringr::str_detect(input, "\\.csv$")) {
      return(readr::read_csv(input, guess_max = 10^6, col_types = readr::cols(), ...))
    } else if (stringr::str_detect(input, "\\.rds$")) {
      return(readr::read_rds(input))
    }
  } else if (is.data.frame(input)) {
    return(input)
  }

  stop('Input should be an existing csv/excel/rds file path or a data.frame, found "', class(input), '".')
}


#' Clean up problematic text in the citation data.
#'
#' Used internally by the \code{parse_*} functions.
#'
#' @param df The data frame to be cleaned.
#'
#' @return The cleaned data frame.
#'
clean_record_textfields <- function(df) {
  mutate(
    df,
    across(
    	tidyselect::vars_select_helpers$where(is.character),
      ~ stringr::str_replace_all(.x, c(" *; *" = ";", '["\']+' = " ")) %>%
        stringr::str_squish() %>%
        {
          replace(., . %in% c("", "NA"), NA)
        }
    )
  )
}


#' Extract the path to citation records files
#'
#' The information on the records' location is stored in the session journal
#' created by [perform_search_session()]. It is possible to select records from
#' specific session, query, or source combinations. Only parsed or API
#' downloaded record paths will be returned, not the raw data source files.
#'
#' @param journal A data frame produced by [perform_search_session()] or a file
#'   path to it.
#' @param sessions,queries,sources Sessions, queries and sources for the which
#'   one wants to get the record data. By default, all record file paths are
#'   retrieved.
#' @param records_folder The path to the folder where the records are stored. By
#'   default is named \code{Records}.
#'
#' @return A vector of file paths.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' journal <- perform_search_session(
#'   query = query, year_query = year_filter,
#'   session_name = "Session1", query_name = "Query1",
#'   records_folder = "Records",
#'   journal = "Session_journal.csv"
#' )
#'
#' # using the journal as data frame
#' paths <- extract_source_file_paths(journal)
#'
#' # using the journal path
#' paths <- extract_source_file_paths("Session_journal.csv")
#' }
extract_source_file_paths <- function(journal, sessions = journal$Session_ID,
                                      queries = journal$Query_ID,
                                      sources = journal$Source,
                                      records_folder = "Records") {

	# Silence CMD CHECK about non standard eval
	Session_ID <- Query_ID <- Source <- Output_file <- NULL

  import_data(journal) %>%
    filter(Session_ID %in% sessions, Query_ID %in% queries, Source %in% sources) %>%
    with(file.path(records_folder, Session_ID, Query_ID, Output_file)) %>%
    unique()
}

#' Parse Pubmed raw data
#'
#' Parse and normalize Pubmed ".nbib" files downloaded through
#' \url{https://pubmed.ncbi.nlm.nih.gov/}.
#'
#' @param entries A character vector containing the citation data.
#' @param timestamp A timestamp as provided by [lubridate::now()].
#'
#' @return A data frame with the parsed data.
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' dataRaw <- readr::read_file(file.path(
#'   "Records", "Session 1", "Query",
#'   "Pubmed.nbib"
#' ))
#'
#' parse_pubmed(dataRaw)
#' }
parse_pubmed <- function(entries, timestamp = now()) {

	# Silence CMD CHECK about non standard eval
	PMID <- TI <- BTI <- AB <- LID <- AID <- FAU <- JT <- TA <- PT <- MH <- OT <- DP <- NULL

  entries <- entries %>%
    stringr::str_remove_all("\\r") %>%
    stringr::str_replace_all("\\n\\s\\s+", " ") %>%
    stringr::str_trim() %>%
    stringr::str_split("\\n+(?=PMID-)") %>%
    unlist()

  tags <- c("TI", "BTI", "AB", "JT", "TA", "DP")
  info <- lapply(tags, function(tag) {
    stringr::str_extract(entries, sprintf("(?<=\\n)%s *- .+", tag)) %>% stringr::str_remove("[A-Z]+ *- ")
  }) %>%
    setNames(tags) %>%
    bind_cols()

  tags <- c("FAU", "PT", "MH", "OT")
  info <- cbind(info, lapply(tags, function(tag) {
    stringr::str_extract_all(entries, sprintf("(?<=\\n)%s *- .+", tag)) %>% sapply(function(x) stringr::str_remove(x, "[A-Z]+ *- ") %>% paste0(collapse = "; "))
  }) %>% setNames(tags) %>% bind_cols())

  tags <- c("LID", "AID")
  info <- cbind(info, lapply(tags, function(tag) {
    stringr::str_extract(entries, sprintf("(?<=\\n)%s *- .+(?= \\[doi\\])", tag)) %>% stringr::str_remove("[A-Z]+ *- ")
  }) %>% setNames(tags) %>% bind_cols())

  info$PMID <- stringr::str_extract(entries, "(?<=PMID- )\\d+")

  info %>%
    transmute(
      Order = 1:n(),
      ID = paste0("PMID:", PMID), Title = coalesce(TI, BTI),
      Abstract = AB, DOI = coalesce(LID, AID),
      Authors = FAU, URL = paste0("https://pubmed.ncbi.nlm.nih.gov/", PMID),
      Journal = JT, Journal_short = TA, Article_type = PT, Mesh = MH,
      Author_keywords = OT, Published = DP,
      Source = "Pubmed",
      Source_type = "parsed",
      Creation_date = timestamp
    ) %>%
    clean_record_textfields()
}

#' Parse Web of Science raw data
#'
#' Parse and normalize Web of Science data downloaded manually (as CSV or Excel
#' files) from \url{https://www.webofknowledge.com}.
#'
#' @param entries An imported data frame.
#' @param timestamp A timestamp as provided by [lubridate::now()].
#'
#' @return A data frame with the parsed data.
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' dataRaw <- import_data(file.path("Records", "Session 1", "Query", "WOS.csv"))
#'
#' parse_wos(dataRaw)
#' }
parse_wos <- function(entries, timestamp = now()) {

	# Silence CMD CHECK about non standard eval
	`Times Cited, All Databases` <- `Journal ISO Abbreviation` <- `UT (Unique WOS ID)` <- `Article Title` <- Abstract <- DOI <- `Author Full Names` <- `Source Title` <- `Document Type` <- `Author Keywords` <- `Keywords Plus` <- `WoS Categories` <- `Publication Date` <- `Publication Year` <- `Pubmed Id` <- NULL

  entries %>%
    transmute(
      Order = 1:n(),
      ID = `UT (Unique WOS ID)`,
      Title = `Article Title`,
      Abstract, DOI,
      Authors = `Author Full Names`,
      Journal = `Source Title`,
      Journal_short = `Journal ISO Abbreviation`,
      Article_type = `Document Type`,
      Author_keywords = `Author Keywords`,
      Keywords = `Keywords Plus`,
      Topic = `WoS Categories`,
      N_citations = `Times Cited, All Databases`,
      Published = paste(`Publication Date`, `Publication Year`),
      PMID = `Pubmed Id`,
      Source = "WOS",
      Source_type = "parsed",
      Creation_date = timestamp
    ) %>%
    clean_record_textfields() %>%
  	as.data.frame()
}

#' Parse IEEE raw data
#'
#' Parse and normalize IEEE data downloaded manually (as csv or excel files)
#' from \url{https://ieeexplore.ieee.org/Xplore/home.jsp}.
#'
#' @param entries An imported data frame.
#' @param timestamp A timestamp as provided by [lubridate::now().
#'
#' @return A data frame with the parsed data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dataRaw <- import_data(file.path(
#'   "Records", "Session 1", "Query",
#'   "IEEE.csv"
#' ))
#'
#' parse_ieee(dataRaw)
#' }
parse_ieee <- function(entries, timestamp = now()) {

	# Silence CMD CHECK about non standard eval
	`Article Citation Count` <- `Document Identifier` <- `INSPEC Non-Controlled Terms` <- `INSPEC Controlled Terms` <- `PDF Link` <- `Document Title` <- Abstract <- DOI <- Authors <- `Publication Title` <- `Author Keywords` <- `IEEE Terms` <- Mesh_Terms <- `Online Date` <- NULL

  entries %>%
    transmute(
      Order = 1:n(),
      ID = paste0("IEEE:", stringr::str_remove(`PDF Link`, stringr::fixed("https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber="))),
      Title = `Document Title`,
      Abstract, DOI, URL = `PDF Link`,
      Authors, Journal = `Publication Title`,
      Author_keywords = `Author Keywords`,
      Keywords = cbind(`IEEE Terms`, `INSPEC Controlled Terms`, `INSPEC Non-Controlled Terms`) %>%
        apply(1, function(x) if (any(!is.na(x))) paste(na.omit(x), collapse = ";") else NA),
      Mesh = Mesh_Terms,
      Article_type = stringr::str_remove(`Document Identifier`, "IEEE "),
      N_citations = `Article Citation Count`,
      Published = `Online Date`,
      Source = "IEEE",
      Source_type = "parsed",
      Creation_date = now()
    ) %>%
    clean_record_textfields() %>%
  	as.data.frame()
}

#' Parse EMBASE raw data
#'
#' Parse and normalize EMBASE data downloaded manually (as csv or excel files)
#' from \url{https://www.embase.com/#advancedSearch/default}.
#'
#' @param entries An imported data frame.
#' @param timestamp A timestamp as provided by [lubridate::now()].
#'
#' @return A data frame with the parsed data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dataRaw <- import_data(file.path(
#'   "Records", "Session 1", "Query",
#'   "EMBASE.csv"
#' ))
#'
#' parse_embase(dataRaw)
#' }
parse_embase <- function(entries, timestamp = now()) {

	# Silence CMD CHECK about non standard eval
	PUI <- Title <- Abstract <- DOI <- `Author Names` <- `Source title` <- `Author Keywords` <- `Publication Type` <- `Date of Publication` <- NULL

  entries %>%
    transmute(
      Order = 1:n(),
      ID = paste0("EM:", PUI),
      Title,
      Abstract, DOI, URL = paste0("https://www.embase.com/a/#/search/results?id=", PUI),
      Authors = `Author Names` %>%
        stringr::str_replace_all(c("\\s*,\\s*" = ";", " (?=\\w\\.)" = ", ", "\\." = " ")),
      Journal = `Source title`,
      Author_keywords = `Author Keywords` %>% stringr::str_replace_all("\\s*,\\s*", ";"),
      Keywords = select(cur_data(), contains("Emtree")) %>%
        apply(1, function(x) na.omit(x) %>% paste(collapse = ", ")) %>%
        stringr::str_replace_all(c(
          "\\s*,\\s*" = ";",
          "\\(.+\\)" = ""
        )),
      Article_type = `Publication Type`,
      Published = `Date of Publication`,
      Source = "Embase",
      Source_type = "parsed",
      Creation_date = timestamp
    ) %>%
    clean_record_textfields() %>%
  	as.data.frame()
}

#' Parse SCOPUS raw data
#'
#' Parse and normalize SCOPUS data downloaded manually (as csv or excel files)
#' from \url{https://www.scopus.com/search/form.uri?display=basic#basic}.
#'
#' @param entries An imported data frame.
#' @param timestamp A timestamp as provided by [lubridate::now()].
#'
#' @return A data frame with the parsed data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dataRaw <- import_data(file.path("Records", "Session 1", "Query", "EMBASE.csv"))
#'
#' parse_embase(dataRaw)
#' }
parse_scopus <- function(entries, timestamp = now()) {

	# Silence CMD CHECK about non standard eval
	EID <- Title <- Abstract <- DOI <- Link <- Authors <- `Source title` <- `Author Keywords` <- `Index Keywords` <- `Document Type` <- `Cited by` <- Year <- NULL

  entries %>%
    transmute(
      Order = 1:n(),
      ID = paste0("SCP:", EID),
      Title,
      Abstract = stringr::str_remove(Abstract, stringr::fixed("[No abstract available]")),
      DOI, URL = Link,
      Authors = Authors %>%
        stringr::str_replace_all(c("\\s*,\\s*" = ";", " (?=\\w\\.)" = ", ", "\\." = " ")),
      Journal = `Source title`,
      Author_keywords = `Author Keywords`,
      Keywords = `Index Keywords`,
      Article_type = `Document Type`,
      N_citations = `Cited by`,
      Published = Year,
      Source = "Scopus",
      Source_type = "parsed",
      Creation_date = timestamp
    ) %>%
    clean_record_textfields() %>%
  	as.data.frame()
}

#' Import and parse citation data files
#'
#' Parse and normalize citation data files. If the file was already parsed, will
#' be just read.
#'
#' @param files A vector of citation data file paths.
#'
#' @return A list of data frames containing the parsed citation data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data_files <- list.files(file.path("Records", "Session 1", "Query"))
#'
#' read_bib_files(data_files)
#' }
read_bib_files <- function(files) {
  ts <- now()

  pbapply::pblapply(files, function(file) {
    if (stringr::str_detect(file, "(parsed|API)\\.csv")) { # no parsing necessary
      message("Reading ", basename(file), "...")

      return(import_data(file))
    }

    message("Parsing ", basename(file), "...")

    type <- NULL

    if (stringr::str_detect(file, "\\.(nbib|txt)$")) {
      entries <- readr::read_file(file)

      if (stringr::str_detect(entries, "PMID-")) type <- "pubmed"
    } else if (stringr::str_detect(file, "\\.(xlsx?|csv)$")) {
      entries <- import_data(file)

      if ("UT (Unique WOS ID)" %in% colnames(entries)) {
        type <- "wos"
      } else if ("IEEE Terms" %in% colnames(entries)) {
        type <- "ieee"
      } else if ("Scopus" %in% entries$Source) {
        type <- "scopus"
      } else if ("Embase Accession ID" %in% colnames(entries)) type <- "embase"
    }

    if (is.null(type)) {
      warning("Format not recognized for ", file, call. = FALSE, immediate. = TRUE)
      return(NULL)
    }

    # parse the raw files using the correct interpreter
    get(paste0("parse_", type))(entries, ts) %>%
      data.frame()
  }) %>% setNames(basename(files))
}

#' Join citation data frames and resolve record duplication
#'
#' Take a list of data frames containing parsed citation data created using
#' [read_bib_files()] and joins them, resolving duplicated records using
#' [fix_duplicated_records()].
#'
#' @param record_list A list of data frames as created by [read_bib_files()].
#'
#' @return A record data frame containing the citation data from multiple
#'   sources.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data_files <- list.files(file.path("Records", "Session 1", "Query"))
#'
#' record_list <- read_bib_files(data_files)
#'
#' join_records(record_list)
#' }
join_records <- function(record_list) {

	# Silence CMD CHECK about non standard eval
	Order <- DOI <- ID <- Title <- Abstract <- Authors <- Published <- URL <- Journal <- Journal_short <- Keywords <- Author_keywords <- Mesh <- Article_type <- N_citations <- Source <- Source_type <- FileID <- Keywords <- Author_keywords <- Order <- NULL

  lapply(record_list, function(source) {
    source %>%
      transmute(
        Order,
        DOI, ID, Title, Abstract, Authors,
        Year = Published %>% stringr::str_extract("\\d{4}") %>% as.numeric(),
        URL = if (exists("URL")) URL else NA,
        Journal = if (exists("Journal")) Journal else NA,
        Journal_short = if (exists("Journal_short")) Journal_short else NA,
        Keywords = if (exists("Keywords")) Keywords else NA,
        Author_keywords = if (exists("Author_keywords")) Author_keywords else NA,
        Mesh = if (exists("Mesh")) Mesh else NA,
        Article_type,
        N_citations = if (exists("N_citations")) N_citations else NA,
        Source, Source_type,
        FileID = if (exists("FileID")) FileID else NA,
      )
  }) %>%
    bind_rows() %>%
    mutate(
      Keywords = cbind(Keywords, Author_keywords) %>%
        apply(1, function(x) if (any(!is.na(x))) paste(na.omit(x), collapse = ";") else NA) %>% stringr::str_to_lower(),
      Author_keywords = NULL
    ) %>%
    fix_duplicated_records() %>%
    mutate(
      Keywords = stringr::str_split(Keywords, "\\s*;\\s*") %>% sapply(function(x) {
        stringr::str_remove(x, '^[\\*\\-"\\\']+ *') %>%
          stringr::str_remove(' *[\\*\\-"\\\']+ *$') %>%
          unique() %>%
          paste(collapse = "; ")
      })
    ) %>%
    distinct() %>%
    arrange(Order)
}

#' Resolve duplicated records in a record data frame
#'
#' This function uses the DOI or the (cleaned) record title to match duplicated
#' records and join them. Some fields like the record keywords and source are
#' joined between copies.
#'
#' @param records A record data frame.
#'
#' @return A record data frame with no duplicated records.
#'
#' @export
#'
fix_duplicated_records <- function(records) {

	# Silence CMD CHECK about non standard eval
	ID <- Title <- UID <- DOI <- Order <- Keywords <- N_citations <- NULL

  records <- records %>%
    group_by(ID) %>%
    mutate(Title = na.omit(Title)[1]) %>%
    ungroup() %>%
    mutate(
      UID = stringr::str_to_lower(Title) %>% stringr::str_remove_all("[^\\w\\d]+")
    ) %>%
    group_by(UID) %>%
    mutate(DOI = na.omit(DOI)[1]) %>%
    ungroup() %>%
    mutate(
      UID = coalesce(DOI, UID)
    )

  dup_recs <- records$UID[duplicated(records$UID)]

  unique_sources <- records %>% filter(!(UID %in% dup_recs))
  dup_sources <- records %>% filter(UID %in% dup_recs)

  dup_sources <- dup_sources %>%
    group_by(UID) %>%
    summarise(
      Order = min(Order),
      # Keep only one instance of the data for these field
      across(
        any_of(c(
          "Title", "Abstract", "Authors", "Journal", "Journal_short",
          "Year", "Pred_delta", "Pred_Med", "Pred_Low", "Pred_Up"
        )),
        ~ na.omit(.x)[1]
      ),
      # Join the data from all record copies for these fields
      across(
        any_of(c(
          "ID", "DOI", "URL", "Mesh", "Article_type", "Source",
          "Source_type", "FileID", "Rev_manual", "Rev_prediction",
          "Rev_previous", "Predicted_label"
        )),
        ~ na.omit(.x) %>%
          unique() %>%
          paste(collapse = "; ")
      ),
      Keywords = Keywords %>% stringr::str_split("; ") %>% unlist() %>% na.omit() %>% unique() %>%
        purrr::keep(~ stringr::str_length(.x) > 0) %>%
        paste(collapse = "; "),
      ## TODO: clean up is necessary for Source and Source_type
      N_citations = suppressWarnings(na.omit(N_citations) %>% max(na.rm = TRUE) %>%
        purrr::modify_if(~ !is.finite(.x), ~NA))
    )

  bind_rows(unique_sources, dup_sources) %>%
    select(-UID) %>%
    clean_record_textfields() %>%
    filter(!duplicated(ID))
}


#' Create an annotation data set ready for relevance classification.
#'
#' This function imports citation data, joins them and possibly reorder them in
#' order to put on top records with a higher probability of being relevant given
#' the search query. It also adds the fields needed in the data frame for the
#' record manual classification. It provides the option to import already
#' existing annotation data sets or previous classifications (without importing
#' the data).
#'
#' @param records Either a vector of citation file paths and or folder
#'   containing such files, or a list of record data frames.
#' @param reorder_query A boolean query (as characters) of keywords that can be
#'   used to empirically reorder records by putting on top those more probably
#'   identified by such query. Usually, the initial query used to collect
#'   records is used.
#' @param prev_records A previously created data frame of records to join with
#'   the new ones, removing duplicates.
#' @param prev_classification A previous set of labelled records from which
#'   import the classification, without importing the records themselves.
#'
#' @return A data frame of records ready for manual or automatic relevance
#'   classification.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # This function extracts the appropriate file paths from a session journal.
#' # By default it passes all stored files, otherwise it can be filtered by
#' # session, query, and source (i.e., Pubmed, WOS, IEEE)
#' record_files <- extract_source_file_paths(journal)
#'
#' # create_annotation_file() accept a great variety of inputs:
#'
#' # either record file paths
#' input <- record_files
#'
#' # or specific record file folders
#' input <- file.path("Records", "Session1", "Query1")
#'
#' # or parent folders, since it searches for files recursively
#' input <- "Records"
#'
#' # or the already parsed files
#' input <- read_bib_files(record_files)
#'
#' # We can then call create_annotation_file() with one of the above input
#' Annotation_data <- create_annotation_file(input, reorder_query = query)
#' }
create_annotation_file <- function(records, reorder_query = NULL,
                                   prev_records = NULL,
                                   prev_classification = NULL) {

	# Silence CMD CHECK about non standard eval
	DOI <- ID <- NULL

  if (class(records) %nin% c("character", "list", "data.frame")) {
    stop('"records" should be either of vector of file/folder paths, a list of data.frame or a single data.frame')
  }

  if (is.character(records)) {
    records <- c(
      list.files(records, full.names = TRUE, recursive = TRUE) %>%
        stringr::str_subset("~\\$", negate = TRUE) %>%
        stringr::str_subset("(parsed|API)\\.csv"),
      records[!dir.exists(records)]
    ) %>% unique()

    message("- parsing records...")
    records <- read_bib_files(records)
  }

  if (length(records) == 1) records <- records[[1]]

  if (!is.data.frame(records) & is.list(records)) {
    message("- joining records...")
    records <- join_records(records)
    message(": ", nrow(records), " unique records")
  }

  records <- records %>%
    mutate(
      Rev_manual = NA,
      .before = DOI
    )

  if (!is.null(prev_records)) {
    message("- appending to a previous annotation file...")

    imported_records <- import_data(prev_records)

    records <- records %>% filter(!(ID %in% imported_records$ID))

    message("(", nrow(records), " new records)")

    records <- full_join(
      import_data(prev_records), records
    ) %>%
      fix_duplicated_records()
  }

  if (!is.null(prev_classification)) {
    message("- importing previous classifications...")

    records <- import_classification(records, prev_records = prev_classification)
  }

  if (!is.null(reorder_query)) {
    message("- reordering records...")

    records <- order_by_query_match(records, query = reorder_query)
  }

  invisible(records)
}


#' Reorder a data frame of records according to simple query match
#'
#' This function uses a simple algorithm that counts the keywords matches in
#' the query in the record abstract and title, and use the count to arrange the
#' records in descending order. Usually, the same query used to collect the
#' records in the first place are used.
#'
#' @param records The data frame of records to reorder.
#' @param query The query to use to arrange the records.
#'
#' @return The ordered record data frame.
#'
#' @export
#'
order_by_query_match <- function(records, query) {

	# Silence CMD CHECK about non standard eval
	. <- Title <- Abstract <- text <- term.count <- doc.length <- score <- NULL

  terms <- stringr::str_remove_all(query, "NOT ?(\\w+|\\(.*?\\))") %>%
    stringr::str_remove_all("[^\\w\\s\\*]+|(?<= )(AND|OR)(?= )") %>%
    stringr::str_split("\\s+") %>%
    unlist() %>%
    unique() %>%
    stringr::str_replace_all("\\*", "\\\\w*") %>%
    Filter(function(x) stringr::str_length(x) > 2, .)

  records %>%
    mutate(
      text = paste(Title, Abstract),
      doc.length = stringr::str_count(text, "\\b") + 1,
      term.count = stringr::str_count(text, paste(terms, collapse = "|")),
      score = term.count / doc.length
    ) %>%
    arrange(desc(score)) %>%
    mutate(Order = 1:n()) %>%
    select(-text, -doc.length, -term.count, -score)
}

#' Import classifications from a previously labelled annotation data frame
#'
#' The classification is imported in a new column called \code{Rev_previous}.
#'
#' @param records An annotation data frame.
#' @param prev_records An annotation data frame with already existing manual or
#'   automatic classification.
#' @param IDs Import the labels only from specific records.
#'
#' @return An annotation data frame with an extra column \code{Rev_previous}
#'   storing the imported classification.
#'
#' @export
#'
#'
import_classification <- function(records, prev_records, IDs = records$ID) {

	# Silence CMD CHECK about non standard eval
	uID <- . <- Rev_previous.y <- Rev_previous.x <- Order <- Rev_previous <- NULL

  prev_records <- import_data(prev_records)

  records$uID <- with(
    records,
    ifelse(!is.na(DOI), DOI, stringr::str_to_lower(Title) %>%
      stringr::str_remove_all("[^\\w\\d\\s]+"))
  )

  prev_records$uID <- with(
    prev_records,
    ifelse(!is.na(DOI), DOI, stringr::str_to_lower(Title) %>%
      stringr::str_remove_all("[^\\w\\d\\s]+"))
  )

  target_uID <- records$uID[records$ID %in% IDs]
  prev_records <- filter(prev_records, uID %in% target_uID)

  prev_records <- prev_records %>%
    transmute(
      uID,
      Rev_previous = coalesce_labels(cur_data(), c(
        "Rev_previous",
        "Rev_prediction_new",
        "Rev_prediction", "Rev_manual"
      ))
      # 'Rev_abstract', 'Rev_title')) # for legacy, to be removed.
    ) %>%
    distinct()

  left_join(records, prev_records, by = "uID") %>%
    {
      if ("Rev_previous.y" %in% colnames(.)) {
        mutate(.,
          Rev_previous = coalesce(Rev_previous.y, Rev_previous.x),
          .after = any_of(c("Rev_prediction_new", "Rev_prediction", "Rev_manual"))
        ) %>% select(-Rev_previous.y, -Rev_previous.x)
      } else {
        .
      }
    } %>%
    select(Order, contains("Rev_"), Rev_previous, everything()) %>%
    select(-uID)
}

#' Create a Session starting from an annotation data set.
#'
#' A session is identified by the subsequent iteration of automatic labelling
#' and manual review. It is associated with a folder where the original
#' annotation file (with the initial manual classification) is stored, plus its
#' updates after each classification iteration and supplemental files containing
#' the Document Term Matrix (DTM), a summary of each classification iteration
#' and the posterior samples of the Bayesian predictions.
#'
#' @param Records An annotation data frame.
#' @param session_name A character string to label the session. Usually is
#'   Session followed by a number, without white spaces.
#' @param sessions_folder The path to the folder where all sessions are stored.
#' @param DTM An already existing DTM matrix (see [create_training_set()] and
#'   [text_to_DTM()].
#' @param dup_session_action What to do if a session with the same name already
#'   exists. the options are: skip (if the session exists do nothing but raise a
#'   warning), stop (raise an error), silent (like skip but without warnings),
#'   add (create a new session marking that is a replicate of an existing one),
#'   replace (overwrite the existing session).
#' @param use_time_stamp Add a times tamp to the original annotation file name.
#'
#' @return The path to the created session folder.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' journal <- perform_search_session(
#'   query = query, year_query = year_filter,
#'   session_name = "Session1", query_name = "Query1",
#'   records_folder = "Records",
#'   journal = "Session_journal.csv"
#' )
#'
#' record_files <- extract_source_file_paths(journal)
#'
#' Annotation_data <- create_annotation_file(record_files)
#'
#' create_session(Annotation_data)
#' }
create_session <- function(Records, session_name,
                           sessions_folder = getOption("baysren.sessions_folder"),
                           DTM = NULL,
                           dup_session_action = c("skip", "stop", "silent", "add", "replace"),
                           use_time_stamp = TRUE) {
  message("Creating session: ", session_name)

  dup_session_action <- match.arg(dup_session_action)

  initialise_session <- function(Records, session_path, DTM = NULL,
                                 use_time_stamp = TRUE) {
    if (use_time_stamp) ts <- glue("_{safe_now()}") else ""

    # Create the session folder
    if (!dir.exists(session_path)) {
      message('- create session folder "', session_path, '".')
      dir.create(session_path, recursive = TRUE, showWarnings = FALSE)
    }

    # At the moment csv files will be converted to excel, eventually both file
    # type will be supported
    Records <- import_data(Records)

    message("- copy or write the Record data")

    file_path <- file.path(session_path, glue("Records{ts}.xlsx"))
    if (is.character(Records) | is.factor(Records)) {
      if (!file.exists(Records)) stop(Records, " does not exists.")

      file.copy(Records, file_path, overwrite = TRUE, recursive = FALSE)
    } else {
      openxlsx::write.xlsx(Records, file = file_path, asTable = TRUE)
    }

    message("- Copy or write the DTM data")
    file_path <- file.path(session_path, "DTM.rds")
    if (!is.null(DTM)) {
      if (is.character(DTM) | is.factor(DTM)) {
        if (!file.exists(DTM)) stop(DTM, " does not exists.")

        file.copy(DTM, file_path, overwrite = TRUE, recursive = FALSE)
      } else {
        readr::write_rds(DTM, file = file_path, asTable = TRUE)
      }
    }
  }

  session_path <- file.path(sessions_folder, session_name)

  if (dir.exists(session_path)) {
    switch(dup_session_action,
      silent = {
        return(session_path)
      },
      skip = {
        warning('Session "', session_name, '" exists. Skipping...', call. = FALSE, immediate. = TRUE)
        return(session_path)
      },
      add = {
        warning('Session "', session_name, '" exists. Adding a replicate...', call. = FALSE, immediate. = TRUE)
        cur_rep <- max(stringr::str_extract(session_name, "(?<=_r)\\d+") %>% as.numeric(), 1, na.rm = TRUE)

        session_name <- stringr::str_remove(session_name, "_r\\d+$") %>% paste0("_r", cur_rep + 1)

        session_path <- create_session(
          Records = Records, session_name = session_name,
          sessions_folder = sessions_folder, DTM = DTM,
          dup_session_action = dup_session_action
        )
      },
      replace = {
        warning('Session "', session_name, '" exists. Replacing...', call. = FALSE, immediate. = TRUE)
        failure <- unlink(session_path, recursive = TRUE)

        if (failure == 1) stop("Session removal failed!")
      },
      stop = stop('Session "', session_name, '" is already existing. Stopping...')
    )
  }

  initialise_session(Records, session_path, DTM, use_time_stamp)

  return(session_path)
}

#' Retrieve the path of the resources linked to a session.
#'
#' A helper to retrieve the paths to the original annotation data, the updated
#' files after each classification iteration, the Document Term Matrix, the
#' results summaries and the Bayesian posterior samples.
#'
#' @param session_name The name of the session.
#' @param sessions_folder The folder in which all sessions are stored. It can be
#'   initialized with the \code{baysren.sessions_folder} option.
#' @param which Which resource is required. The default is all of them.
#'
#' @return A list of vectors of file paths.
#'
#' @export
#'
get_session_files <- function(session_name,
                              sessions_folder = getOption("baysren.sessions_folder"),
                              which = c(
                                "Records", "Annotations",
                                "DTM", "Samples", "Results"
                              )) {

	# Silence CMD CHECK about non standard eval
	iter <- NULL

  session_path <- file.path(sessions_folder, session_name)

  lapply(which, function(type) {
    files <- list.files(session_path, recursive = TRUE) %>% stringr::str_subset(type)

    files <- files[stringr::str_detect(basename(files), "^\\w")]

    if (type == "Records") {
      files <- files[!stringr::str_detect(files, "Annotations")]
    }

    if (length(files) == 0) {
      return(NULL)
    }

    files <- tibble(
      files,
      iter = basename(files) %>%
        stringr::str_extract("^\\d+") %>%
        as.numeric() %>%
        pmax(0, na.rm = TRUE) # the source record file would have no iteration in the name, so will be considered as zero
    ) %>%
      arrange(iter) %>%
      pull(files)

    file.path(session_path, files)
  }) %>% setNames(which)
}
