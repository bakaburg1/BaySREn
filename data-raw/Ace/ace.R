## code to prepare `ace` dataset

normalize_text <- function(x) {
    x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
    x <- trimws(x)
    x[x == ""] <- NA_character_
    x
}

extract_medline_tag <- function(record, tag) {
    pattern <- paste0("(?m)^", tag, "\\s*- (.*(?:\\n      .*)*)")
    value <- regmatches(record, regexpr(pattern, record, perl = TRUE))
    if (length(value) == 0 || identical(value, character(0))) {
        return(NA_character_)
    }

    value <- sub(paste0("^", tag, "\\s*- "), "", value)
    normalize_text(gsub("\\n      ", " ", value, perl = TRUE))
}

extract_medline_tags <- function(record, tag) {
    pattern <- paste0("(?m)^", tag, "\\s*- (.*(?:\\n      .*)*)")
    values <- regmatches(record, gregexpr(pattern, record, perl = TRUE))[[1]]
    if (length(values) == 0 || identical(values, -1L)) {
        return(NA_character_)
    }

    values <- sub(paste0("^", tag, "\\s*- "), "", values)
    values <- normalize_text(gsub("\\n      ", " ", values, perl = TRUE))
    paste(stats::na.omit(values), collapse = "; ")
}

parse_pubmed_medline <- function(raw_text) {
    records <- strsplit(trimws(raw_text), "\\n\\s*\\n(?=PMID- )", perl = TRUE)[[1]]

    data.frame(
        pmid = vapply(records, extract_medline_tag, character(1), tag = "PMID"),
        title = vapply(records, extract_medline_tag, character(1), tag = "TI"),
        abstract = vapply(records, extract_medline_tag, character(1), tag = "AB"),
        authors = vapply(records, extract_medline_tags, character(1), tag = "FAU"),
        mesh = vapply(records, extract_medline_tags, character(1), tag = "MH"),
        author_keywords = vapply(records, extract_medline_tags, character(1), tag = "OT"),
        stringsAsFactors = FALSE
    )
}

fetch_pubmed_medline <- function(pmids, api_key = NULL, chunk_size = 200) {
    chunks <- split(pmids, ceiling(seq_along(pmids) / chunk_size))
    pieces <- vector("list", length(chunks))

    for (i in seq_along(chunks)) {
        query <- list(
            db = "pubmed",
            id = paste(chunks[[i]], collapse = ","),
            rettype = "medline",
            retmode = "text",
            tool = "BaySREn"
        )
        if (!is.null(api_key) && !identical(api_key, "")) {
            query$api_key <- api_key
        }

        url <- paste0(
            "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?",
            paste(
                paste0(names(query), "=", utils::URLencode(unlist(query), reserved = TRUE)),
                collapse = "&"
            )
        )

        pieces[[i]] <- paste(readLines(url, warn = FALSE), collapse = "\n")
        Sys.sleep(if (is.null(api_key) || identical(api_key, "")) 0.4 else 0.12)
    }

    paste(pieces, collapse = "\n\n")
}

labels_path <- file.path("data-raw", "Ace", "epc-ir.clean.tsv")
pubmed_path <- file.path("data-raw", "Ace", "pubmed_medline.nbib")
local_path <- file.path("data-raw", "Ace", "data.csv")

# Load criteria documenting the abstract-triage provenance.
criteria_path <- file.path("data-raw", "Ace", "criteria.R")
if (!file.exists(criteria_path)) {
    stop("Criteria script not found: ", criteria_path, call. = FALSE)
}
source(criteria_path, local = FALSE)
if (!exists("ace_criteria")) {
    stop("Object `ace_criteria` is not defined in ", criteria_path, call. = FALSE)
}

# Read the OHSU gold-standard triage labels and keep the ACE review topic.
labels <- read.delim(labels_path, header = FALSE, stringsAsFactors = FALSE)
names(labels) <- c(
    "topic",
    "endnote_id",
    "pmid",
    "abstract_status",
    "article_status"
)
labels <- labels[labels$topic == "ACEInhibitors", ]

# Refresh the local R session environment so NCBI_API_KEY is available.
renviron_path <- path.expand("~/.Renviron")
if (file.exists(renviron_path)) {
    readRenviron(renviron_path)
}
api_key <- Sys.getenv("NCBI_API_KEY", unset = "")

# Cache PubMed MEDLINE records so package-data rebuilds are reproducible offline.
if (!file.exists(pubmed_path)) {
    medline <- fetch_pubmed_medline(labels$pmid, api_key = api_key)
    writeLines(medline, pubmed_path, useBytes = TRUE)
} else {
    warning(
        "File already exists, skipping PubMed download",
        call. = FALSE,
        immediate. = TRUE
    )
}

# Parse PubMed records and join them to the OHSU labels by PMID.
pubmed <- parse_pubmed_medline(
    paste(readLines(pubmed_path, warn = FALSE), collapse = "\n")
)
labels$pmid <- as.character(labels$pmid)
ace <- merge(labels, pubmed, by = "pmid", all.x = TRUE, sort = FALSE)

# Build the package dataset from abstract-triage labels.
ace$keywords <- ifelse(
    is.na(ace$author_keywords) | ace$author_keywords == "",
    ace$mesh,
    ace$author_keywords
)
ace$included <- ace$abstract_status == "I"
ace <- ace[c("title", "abstract", "authors", "keywords", "included")]

# Normalize text fields before excluding records without PubMed abstracts.
ace$title <- normalize_text(ace$title)
ace$abstract <- normalize_text(ace$abstract)
ace$authors <- normalize_text(ace$authors)
ace$keywords <- normalize_text(ace$keywords)

# Keep only records with abstract text available from PubMed.
ace <- ace[!is.na(ace$abstract), ]

# Test column names
testthat::expect_named(
    ace,
    c("title", "abstract", "authors", "keywords", "included")
)

# Test inclusion counts
inclusion_table <- table(ace$included)
testthat::expect_equal(
    as.numeric(inclusion_table),
    c(2067, 169)
)

# Test PubMed retrieval coverage
testthat::expect_equal(nrow(ace), 2236L)
testthat::expect_equal(sum(is.na(ace$title)), 0L)
testthat::expect_equal(sum(is.na(ace$abstract)), 0L)
testthat::expect_equal(sum(is.na(ace$abstract) & ace$included), 0L)

# Save the processed raw data used by the package object.
utils::write.csv(ace, local_path, row.names = FALSE)

# Save the data in the package
usethis::use_data(ace, ace_criteria, overwrite = TRUE)
