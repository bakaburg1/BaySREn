## code to prepare `urban` dataset

# Point to the screened RIS export that retains the full step-1 pool.
local_path <- file.path(
    "data-raw",
    "Urban",
    "B_01_recordsscreened-title-keywords.ris"
)

# Point to the workbook that stores the step-1 and step-2 screening decisions.
screening_path <- file.path(
    "data-raw",
    "Urban",
    "B_05_screeningdecisions-overview.xlsx"
)

# Load the companion criteria object for the packaged dataset.
criteria_path <- file.path("data-raw", "Urban", "criteria.R")

# Fail early when any required Urban source file is missing.
required_paths <- c(local_path, screening_path, criteria_path)
missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths) > 0) {
    stop(
        "Missing Urban source files: ",
        paste(missing_paths, collapse = ", "),
        call. = FALSE
    )
}

# Source the preformatted criteria definition.
source(criteria_path, local = FALSE)
if (!exists("urban_criteria")) {
    stop(
        "Object `urban_criteria` is not defined in ",
        criteria_path,
        call. = FALSE
    )
}

# Parse RIS records while preserving continuation lines under the previous tag.
parse_ris <- function(path) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")

    if (length(lines) == 0) {
        return(data.frame())
    }

    records <- list()
    current_record <- list()
    last_tag <- NULL

    for (line in c(lines, "ER  - ")) {
        if (grepl("^[A-Z0-9]{2}  - ", line)) {
            tag <- tolower(substr(line, 1, 2))
            value <- trimws(substr(line, 7, nchar(line)))

            if (tag == "er") {
                if (length(current_record) > 0) {
                    records[[length(records) + 1L]] <- lapply(
                        current_record,
                        paste,
                        collapse = "; "
                    )
                }

                current_record <- list()
                last_tag <- NULL
                next
            }

            current_record[[tag]] <- c(current_record[[tag]], value)
            last_tag <- tag
            next
        }

        if (!is.null(last_tag) && nzchar(trimws(line))) {
            current_values <- current_record[[last_tag]]
            current_values[length(current_values)] <- paste(
                current_values[length(current_values)],
                trimws(line)
            )
            current_record[[last_tag]] <- current_values
        }
    }

    if (length(records) == 0) {
        return(data.frame())
    }

    dplyr::bind_rows(records)
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

# Deduplicate keywords while preserving readable phrase boundaries.
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

# Read the full step-1 RIS pool and attach workbook serial numbers.
urban_raw <- parse_ris(local_path)
urban_raw$serial_number <- seq_len(nrow(urban_raw))

# Read the title/keyword and abstract screening decision sheets.
step1_tbl <- readxl::read_excel(
    screening_path,
    sheet = "Screening Step 1 Title Keywords"
)
step2_tbl <- readxl::read_excel(
    screening_path,
    sheet = "Screening Step 2 Abstract"
)

# Join both screening sheets to preserve the documented screening provenance.
urban_raw <- urban_raw |>
    dplyr::left_join(
        step1_tbl,
        by = c("serial_number" = "Serial Number")
    ) |>
    dplyr::left_join(
        dplyr::rename(
            step2_tbl,
            step2_final = "FINAL (1/0)"
        ),
        by = c("serial_number" = "Serial Number")
    )

# Rebuild the public dataset, joining title/keyword and abstract screening into
# one pre-fulltext inclusion label.
urban <- urban_raw |>
    dplyr::transmute(
        title = normalize_text(.data$ti),
        abstract = normalize_text(.data$ab),
        authors = vapply(.data$au, clean_author_entry, character(1)),
        keywords = vapply(.data$kw, clean_keywords, character(1)),
        year = suppressWarnings(as.integer(.data$py)),
        language = tolower(trimws(dplyr::coalesce(.data$la, NA_character_))),
        included = !is.na(.data$step2_final) & .data$step2_final == 1
    )

# Apply the documented publication-year restriction that is not fully respected
# by the downloaded RIS export while retaining records with missing year tags.
urban <- urban |>
    dplyr::filter(is.na(.data$year) | (.data$year >= 2016 & .data$year <= 2022))

# Remove helper columns so the packaged object matches the benchmark schema.
urban <- urban |>
    dplyr::select("title", "abstract", "authors", "keywords", "included")

# Assert the source workbook still reflects the reported screening flow.
testthat::expect_equal(nrow(step1_tbl), 1557L)
testthat::expect_equal(sum(step1_tbl$`FINAL (1/0)` == 1), 736L)
testthat::expect_equal(sum(step2_tbl$`FINAL (1/0)` == 1), 143L)

# Assert the packaged Urban dataset matches the package schema exactly.
testthat::expect_named(
    urban,
    c("title", "abstract", "authors", "keywords", "included")
)

# Assert the enforced year restriction removes the known source mismatch.
testthat::expect_equal(nrow(urban), 1549L)

# Assert the merged pre-fulltext label count remains stable after filtering.
testthat::expect_equal(sum(urban$included), 142L)

# Save the cleaned Urban dataset and its criteria for package use.
usethis::use_data(
    urban,
    urban_criteria,
    overwrite = TRUE,
    compress = "xz"
)
