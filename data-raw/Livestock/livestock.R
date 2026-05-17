## code to prepare `livestock` dataset

# Point to the full ASReview workbook export for the livestock review.
local_path <- file.path(
    "data-raw",
    "Livestock",
    "asreview_dataset_all_a-review-on-the-role-of-ontologies-in-modern-agriculture.xlsx"
)

# Load the companion criteria object for the packaged dataset.
criteria_path <- file.path("data-raw", "Livestock", "criteria.R")

# Fail early when the Livestock source workbook or criteria file is missing.
required_paths <- c(local_path, criteria_path)
missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths) > 0) {
    stop(
        "Missing Livestock source files: ",
        paste(missing_paths, collapse = ", "),
        call. = FALSE
    )
}

# Source the preformatted criteria definition.
source(criteria_path, local = FALSE)
if (!exists("livestock_criteria")) {
    stop(
        "Object `livestock_criteria` is not defined in ",
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

# Convert Python-like author lists into the package's semicolon-delimited style.
clean_author_entry <- function(entry) {
    if (is.na(entry) || trimws(entry) == "") {
        return(NA_character_)
    }

    entry <- gsub("^\\[|\\]$", "", trimws(entry))
    entry <- gsub("^['\"]|['\"]$", "", entry)
    entry <- gsub("'\\s*,\\s*'", "; ", entry)
    entry <- gsub("\"\\s*,\\s*\"", "; ", entry)
    entry <- gsub("',\\s*\"", "; ", entry)
    entry <- gsub("\",\\s*'", "; ", entry)
    entry <- gsub("['\"]", "", entry)
    entry <- gsub("\\s+", " ", entry)

    people <- trimws(unlist(strsplit(entry, ";", fixed = TRUE)))
    people <- people[people != ""]

    if (!length(people)) {
        return(NA_character_)
    }

    formatted <- vapply(
        people,
        function(person) {
            parts <- trimws(unlist(strsplit(person, ",", fixed = TRUE)))

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
                given <- if (length(tokens) > 1) {
                    paste(tokens[-length(tokens)], collapse = " ")
                } else {
                    ""
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

            family <- trimws(gsub("\\s+", " ", family))
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

# Clean keyword strings while preserving readable phrase boundaries.
clean_keywords <- function(x) {
    if (is.na(x) || trimws(x) == "") {
        return(NA_character_)
    }

    x <- gsub("^\\[|\\]$", "", trimws(x))
    x <- gsub("['\"]", "", x)
    x <- gsub(",", ";", x, fixed = TRUE)
    terms <- trimws(unlist(strsplit(x, ";", fixed = TRUE)))
    terms <- terms[terms != ""]
    terms <- unique(terms)

    if (!length(terms)) {
        return(NA_character_)
    }

    paste(terms, collapse = "; ")
}

# Read the full Livestock ASReview export while preserving original names.
livestock_raw <- readxl::read_excel(local_path)

# Rebuild the public dataset from the exported ASReview annotations.
livestock <- livestock_raw |>
    dplyr::transmute(
        title = normalize_text(.data$title),
        abstract = normalize_text(.data$abstract),
        authors = vapply(.data$authors, clean_author_entry, character(1)),
        keywords = vapply(.data$keywords, clean_keywords, character(1)),
        year = suppressWarnings(as.integer(.data$year)),
        included = as.logical(.data$included)
    )

# Apply the publication-year restriction stated in the supplementary material.
livestock <- livestock |>
    dplyr::filter(!is.na(.data$year), .data$year >= 2011, .data$year <= 2025)

# Remove helper columns so the packaged object matches the benchmark schema.
livestock <- livestock |>
    dplyr::select("title", "abstract", "authors", "keywords", "included")

# Assert the packaged Livestock dataset matches the package schema exactly.
testthat::expect_named(
    livestock,
    c("title", "abstract", "authors", "keywords", "included")
)

# Assert the enforced year restriction removes the known out-of-range records.
testthat::expect_equal(nrow(livestock), 282L)

# Assert the abstract-screening label count remains stable after filtering.
testthat::expect_equal(sum(livestock$included), 111L)

# Save the cleaned Livestock dataset and its criteria for package use.
usethis::use_data(
    livestock,
    livestock_criteria,
    overwrite = TRUE,
    compress = "xz"
)
