## code to prepare `vella` dataset

# Point the data-raw script at the current Vella source directory.
local_path <- file.path("data-raw", "Vella", "data.csv")

if (!file.exists(local_path)) {
    stop("Raw data not found: ", local_path, call. = FALSE)
}

vella_raw <- read.csv2(local_path, check.names = FALSE)

# Load the finalized adjudication worksheet used to revise Vella labels.
revision_path <- file.path("data-raw", "Vella", "label_revision.xlsx")
if (!file.exists(revision_path)) {
    stop("Revision workbook not found: ", revision_path, call. = FALSE)
}

revision_tbl <- readxl::read_excel(
    revision_path,
    sheet = "Corrections",
    skip = 2
)

# Load finalized selection criteria definition.
criteria_path <- file.path("data-raw", "Vella", "criteria.R")
if (!file.exists(criteria_path)) {
    stop("Criteria script not found: ", criteria_path, call. = FALSE)
}
source(criteria_path, local = FALSE)
if (!exists("vella_criteria")) {
    stop(
        "Object `vella_criteria` is not defined in ",
        criteria_path,
        call. = FALSE
    )
}

# Helpers ---------------------------------------------------------------

normalize_text <- function(x) {
    x <- gsub("\n", " ", x, fixed = TRUE)
    x <- gsub("\\s+", " ", x, perl = TRUE)
    x <- trimws(x)
    x[x == ""] <- NA_character_
    x
}

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

    formatted <- vapply(people, function(person) {
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
        initials <- paste0(toupper(substr(given_tokens, 1, 1)), collapse = "")
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
    }, character(1), USE.NAMES = FALSE)

    formatted <- formatted[!is.na(formatted) & formatted != ""]

    if (!length(formatted)) {
        return(NA_character_)
    }

    paste(formatted, collapse = "; ")
}

label_text_to_logical <- function(x) {
    normalized <- trimws(tolower(as.character(x)))
    dplyr::case_when(
        normalized %in% c("relevant", "include", "included", "yes", "y", "si") ~ TRUE,
        normalized %in% c("not relevant", "exclude", "excluded", "no", "n") ~ FALSE,
        TRUE ~ NA
    )
}

# Build a revision lookup keyed by the normalized title and abstract text.
revision_lookup <- revision_tbl |>
    dplyr::mutate(
        title = normalize_text(.data$title),
        abstract = normalize_text(.data$abstract),
        included_revised = dplyr::coalesce(
            label_text_to_logical(.data$human_relabel),
            label_text_to_logical(.data$human_label)
        )
    ) |>
    dplyr::select("title", "abstract", "included_revised") |>
    dplyr::filter(!is.na(.data$title), !is.na(.data$abstract)) |>
    dplyr::distinct(.data$title, .data$abstract, .keep_all = TRUE)

# Transform -------------------------------------------------------------

# Normalize the raw records before applying any label revisions.
vella <- within(vella_raw, {
    title <- normalize_text(title)
    abstract <- normalize_text(abstract)
    authors <- vapply(authors, clean_author_entry, character(1))
    keywords <- rep(NA_character_, length(title))
    included <- tolower(trimws(`Final decision`)) == "si"
})

# Overwrite the original labels with the adjudicated labels when available.
vella <- vella |>
    dplyr::left_join(revision_lookup, by = c("title", "abstract")) |>
    dplyr::mutate(
        included = dplyr::coalesce(.data$included_revised, .data$included)
    ) |>
    dplyr::select(-"included_revised")

vella <- vella[c("title", "abstract", "authors", "keywords", "included")]

# Tests -----------------------------------------------------------------

testthat::expect_named(
    vella,
    c("title", "abstract", "authors", "keywords", "included")
)

inclusion_table <- table(vella$included)
testthat::expect_equal(
    as.numeric(inclusion_table),
    c(1649, 199)  # FALSE, TRUE
)

# Save ------------------------------------------------------------------

usethis::use_data(
    vella,
    vella_criteria,
    overwrite = TRUE,
    compress = "xz"
)
