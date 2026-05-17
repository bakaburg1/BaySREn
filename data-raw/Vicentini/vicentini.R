## code to prepare `vicentini` dataset

# Point to the abstract-screening export in the standard raw-data location.
local_path <- file.path("data-raw", "Vicentini", "data.csv")

# Fail early when the incoming CSV is missing.
if (!file.exists(local_path)) {
    stop("Raw data not found: ", local_path, call. = FALSE)
}

# Read the review export while preserving the original column names.
vicentini_raw <- read.csv(local_path, check.names = FALSE)

# Load the handwritten review criteria alongside the builder.
criteria_path <- file.path("data-raw", "Vicentini", "criteria.R")
if (!file.exists(criteria_path)) {
    stop("Criteria script not found: ", criteria_path, call. = FALSE)
}
source(criteria_path, local = FALSE)
if (!exists("vicentini_criteria")) {
    stop(
        "Object `vicentini_criteria` is not defined in ",
        criteria_path,
        call. = FALSE
    )
}

# Normalize free-text fields into a single-line analysis-ready format.
normalize_text <- function(x) {
    x <- gsub("\n", " ", x, fixed = TRUE)
    x <- gsub("\\s+", " ", x, perl = TRUE)
    x <- trimws(x)
    x[x == ""] <- NA_character_
    x
}

# Standardize author entries into a stable "Family Initials" representation.
clean_author_entry <- function(entry) {
    if (is.na(entry) || trimws(entry) == "") {
        return(NA_character_)
    }

    entry <- gsub("\\s+(?i:and)\\s+", ";", entry, perl = TRUE)
    entry <- gsub("\t+", ";", entry)
    entry <- gsub("\\s+", " ", entry)
    people <- trimws(unlist(strsplit(entry, ";", fixed = TRUE)))
    people <- people[people != ""]

    if (!length(people)) {
        return(NA_character_)
    }

    formatted <- vapply(
        people,
        function(person) {
            parts <- strsplit(person, ",", fixed = TRUE)[[1]]
            parts <- trimws(parts)

            if (length(parts) > 1) {
                family <- parts[1]
                given <- paste(parts[-1], collapse = " ")
            } else {
                tokens <- unlist(strsplit(person, " ", fixed = FALSE))
                tokens <- tokens[tokens != ""]

                if (!length(tokens)) {
                    return(NA_character_)
                }

                family <- tokens[length(tokens)]
                if (length(tokens) > 1) {
                    given <- paste(tokens[-length(tokens)], collapse = " ")
                } else {
                    given <- ""
                }
            }

            given <- gsub("[^[:alpha:]\\s-]", " ", given)
            given_tokens <- unlist(strsplit(given, "[-\\s]+"))
            given_tokens <- given_tokens[given_tokens != ""]
            initials <- paste0(
                toupper(substr(given_tokens, 1, 1)),
                collapse = ""
            )
            initials <- gsub("[^A-Z]", "", initials)

            family <- trimws(family)
            family <- gsub("[^[:alpha:]'’\\s-]", " ", family, perl = TRUE)
            family <- gsub("\\s+", " ", family)

            if (family == "") {
                return(NA_character_)
            }

            if (initials == "") {
                return(family)
            }

            paste(family, initials)
        },
        character(1),
        USE.NAMES = FALSE
    )

    formatted <- trimws(gsub(",", "", formatted, fixed = TRUE))
    formatted <- gsub("\\s+$", "", formatted, perl = TRUE)
    formatted <- formatted[!is.na(formatted) & formatted != ""]

    if (!length(formatted)) {
        return(NA_character_)
    }

    paste(formatted, collapse = "; ")
}

# Clean keywords while preserving readable phrase boundaries for the DTM.
clean_keywords <- function(x) {
    if (is.na(x) || trimws(x) == "") {
        return(NA_character_)
    }

    x <- gsub("\t+", ";", x)
    terms <- trimws(unlist(strsplit(x, ";", fixed = TRUE)))
    terms <- gsub("^\\*+", "", terms)
    terms <- trimws(terms)
    terms <- terms[terms != ""]
    terms <- unique(terms)

    if (!length(terms)) {
        return(NA_character_)
    }

    paste(terms, collapse = "; ")
}

# Apply field-level cleaning and derive the public screening dataset.
vicentini <- within(vicentini_raw, {
    title <- normalize_text(citation_title)
    abstract <- normalize_text(citation_abstract)
    authors <- vapply(citation_authors, clean_author_entry, character(1))
    keywords <- vapply(citation_keywords, clean_keywords, character(1))
    included <- citation_screening_status == "included"
})

# Keep the packaged dataset aligned with the existing minimal review schema.
vicentini <- vicentini[c("title", "abstract", "authors", "keywords", "included")]

# Assert the public dataset columns match the package convention exactly.
testthat::expect_named(
    vicentini,
    c("title", "abstract", "authors", "keywords", "included")
)

# Assert the full export row count is preserved.
testthat::expect_equal(nrow(vicentini), 2803L)

# Assert the inclusion labels reproduce the abstract-screening totals.
inclusion_table <- table(vicentini$included)
testthat::expect_equal(as.numeric(inclusion_table), c(2603, 200))

# Assert author fields are semicolon-delimited and contain no raw tabs.
author_values <- vicentini$authors[!is.na(vicentini$authors)]
testthat::expect_false(any(grepl("\t", author_values, fixed = TRUE)))

# Assert keyword fields are semicolon-delimited and contain no raw tabs.
keyword_values <- vicentini$keywords[!is.na(vicentini$keywords)]
testthat::expect_false(any(grepl("\t", keyword_values, fixed = TRUE)))

# Save the cleaned dataset and its criteria for package use.
usethis::use_data(
    vicentini,
    vicentini_criteria,
    overwrite = TRUE,
    compress = "xz"
)
