## code to prepare `giesen` dataset

# Point to the original review export without modifying the raw material.
local_path <- file.path(
    "data-raw",
    "Giesen",
    "material",
    "pneumonia_review_dataset[45].csv"
)

# Fail early when the incoming CSV is missing.
if (!file.exists(local_path)) {
    stop("Raw data not found: ", local_path, call. = FALSE)
}

# Read the review export while preserving the original column names.
giesen_raw <- read.csv(local_path, check.names = FALSE)

# Load the handwritten review criteria alongside the builder.
criteria_path <- file.path("data-raw", "Giesen", "criteria.R")
if (!file.exists(criteria_path)) {
    stop("Criteria script not found: ", criteria_path, call. = FALSE)
}
source(criteria_path, local = FALSE)
if (!exists("giesen_criteria")) {
    stop(
        "Object `giesen_criteria` is not defined in ",
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

    formatted <- formatted[!is.na(formatted) & formatted != ""]

    if (!length(formatted)) {
        return(NA_character_)
    }

    paste(formatted, collapse = "; ")
}

# Clean keywords while preserving the reviewed content and removing duplicates.
clean_keywords <- function(x) {
    if (is.na(x) || trimws(x) == "") {
        return(NA_character_)
    }

    terms <- trimws(unlist(strsplit(x, ";", fixed = TRUE)))
    terms <- terms[terms != ""]
    terms <- unique(terms)

    if (!length(terms)) {
        return(NA_character_)
    }

    paste(terms, collapse = "; ")
}

# Apply only the externally verified title corrections required by this import.
repair_giesen_titles <- function(data) {
    corrected_influenza_title <- paste(
        "Determining the Provincial and National Burden of",
        "Influenza-Associated Severe Acute Respiratory Illness in South",
        "Africa Using a Rapid Assessment Methodology"
    )
    corrected_endocarditis_title <- paste0(
        "Les endocardites infectieuses chez l\u2019enfant africain",
        " subsaharien, \u00e9tude transversale \u00e0 propos de 19 cas",
        " \u00e0 Ouagadougou au Burkina Faso"
    )

    doi_key <- trimws(tolower(ifelse(is.na(data$doi), "", data$doi)))
    article_key <- trimws(as.character(data$article_id))

    # Correct the DOI-backed influenza record title from the verified source.
    data$title[doi_key %in% "10.1371/journal.pone.0132078"] <-
        corrected_influenza_title

    # Correct the sparse duplicate of the same influenza record explicitly.
    data$title[article_key %in% "171417899"] <- corrected_influenza_title

    # Correct the DOI-backed French title using the verified publication title.
    data$title[doi_key %in% "10.1016/j.ancard.2013.02.004"] <-
        corrected_endocarditis_title

    # Fill the single blank title from its DOI-backed duplicate record.
    data$title[article_key %in% "171417942"] <- corrected_endocarditis_title

    data
}

# Apply all field-level cleaning before selecting the public dataset columns.
giesen <- within(giesen_raw, {
    title <- normalize_text(title)
    abstract <- normalize_text(abstract)
    authors <- vapply(authors, clean_author_entry, character(1))
    keywords <- vapply(keywords, clean_keywords, character(1))
})

# Repair only the known verified title issues in the imported review export.
giesen <- repair_giesen_titles(giesen)

# Derive the logical inclusion label from the finalized full-text decision.
giesen$included <- tolower(trimws(giesen$`included_full text`)) == "yes"

# Keep the packaged dataset aligned with the existing minimal SIIAM schema.
giesen <- giesen[c("title", "abstract", "authors", "keywords", "included")]

# Assert the public dataset columns match the package convention exactly.
testthat::expect_named(
    giesen,
    c("title", "abstract", "authors", "keywords", "included")
)

# Assert the reviewed row count is preserved, including duplicate rows.
testthat::expect_equal(nrow(giesen), 1225L)

# Assert the inclusion labels reproduce the reviewed decision totals.
inclusion_table <- table(giesen$included)
testthat::expect_equal(as.numeric(inclusion_table), c(1211, 14))

# Assert the influenza title repair is applied to the duplicate pair only.
corrected_influenza_title <- paste(
    "Determining the Provincial and National Burden of",
    "Influenza-Associated Severe Acute Respiratory Illness in South",
    "Africa Using a Rapid Assessment Methodology"
)
testthat::expect_equal(
    sum(giesen$title %in% corrected_influenza_title, na.rm = TRUE),
    2L
)

# Assert the French endocarditis title repair fills the missing duplicate row.
corrected_endocarditis_title <- paste0(
    "Les endocardites infectieuses chez l\u2019enfant africain",
    " subsaharien, \u00e9tude transversale \u00e0 propos de 19 cas",
    " \u00e0 Ouagadougou au Burkina Faso"
)
testthat::expect_equal(
    sum(giesen$title %in% corrected_endocarditis_title, na.rm = TRUE),
    2L
)

# Assert no record with a non-empty abstract is left without a title.
blank_title <- is.na(giesen$title) | trimws(giesen$title) == ""
non_empty_abstract <- !is.na(giesen$abstract) & trimws(giesen$abstract) != ""
testthat::expect_false(any(blank_title & non_empty_abstract))

# Save the cleaned dataset and its criteria for package use.
usethis::use_data(
    giesen,
    giesen_criteria,
    overwrite = TRUE,
    compress = "xz"
)
