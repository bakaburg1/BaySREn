## code to prepare `gastaldi` dataset

# Construct the file path to the raw Gastaldi dataset CSV file
# located in the SIIAM/Gastaldi subdirectory of data-raw
local_path <- file.path("data-raw", "SIIAM", "Gastaldi", "data.csv")

# Check if the raw data file exists before attempting to read it
# This prevents cryptic errors and provides clear feedback if
# the file is missing
if (!file.exists(local_path)) {
    stop("Raw data not found: ", local_path, call. = FALSE)
}

# Read the raw CSV data into R, preserving original column names
# check.names = FALSE prevents R from modifying column names that might contain
# special characters or spaces, maintaining consistency with the source data
gastaldi_raw <- read.csv(local_path, check.names = FALSE)

# Load preformatted selection criteria from companion R file
criteria_path <- file.path("data-raw", "SIIAM", "Gastaldi", "criteria.R")
if (!file.exists(criteria_path)) {
    stop("Criteria script not found: ", criteria_path, call. = FALSE)
}
source(criteria_path, local = FALSE)
if (!exists("gastaldi_criteria")) {
    stop(
        "Object `gastaldi_criteria` is not defined in ",
        criteria_path,
        call. = FALSE
    )
}

# Helper Functions Section ------------------------------------------------

#' Function to normalize text by removing extra whitespace and handling empty
#' strings This is crucial for text analysis and database consistency, as
#' inconsistent whitespace can cause issues in matching and processing
#' operations
#'
#' @param x character vector of text to normalize
#'
#' @return character vector with normalized text, empty strings converted to NA
normalize_text <- function(x) {
    # Replace literal newline characters with single spaces to create
    # single-line text
    x <- gsub("\n", " ", x, fixed = TRUE)
    # Collapse multiple consecutive whitespace characters (spaces, tabs,
    # etc.) into single spaces
    x <- gsub("\\s+", " ", x, perl = TRUE)
    # Remove leading and trailing whitespace from each text entry
    x <- trimws(x)
    # Convert empty strings to NA for consistent missing data representation
    x[x == ""] <- NA_character_
    x
}

#' Function to clean and standardize author name entries from various formats
#' This handles common variations in author name formatting across different
#' data sources and converts them to a consistent "FamilyName Initials" format
#' for each author
#'
#' @param entry character string containing one or more author names
#'
#' @return character string with standardized author names separated by "; "
clean_author_entry <- function(entry) {
    # Handle missing or empty entries by returning NA immediately
    if (is.na(entry) || trimws(entry) == "") {
        return(NA_character_)
    }

    # Standardize author separators: replace "and" with semicolons to
    # create consistent delimiters
    # (?i:and) makes the match case-insensitive for "and", "And", "AND", etc.
    entry <- gsub("\\s+(?i:and)\\s+", ";", entry, perl = TRUE)
    # Normalize whitespace around the entry to single spaces
    entry <- gsub("\\s+", " ", entry)
    # Split the entry into individual author names using semicolon as delimiter
    people <- trimws(unlist(strsplit(entry, ";", fixed = TRUE)))
    # Remove any empty strings that might result from splitting
    people <- people[people != ""]

    # If no valid author names remain after processing, return NA
    if (!length(people)) {
        return(NA_character_)
    }

    # Process each individual author name to standardize formatting
    formatted <- vapply(
        people,
        function(person) {
            # First, try to parse as "Family, Given" format (common in
            # academic citations)
            parts <- strsplit(person, ",", fixed = TRUE)[[1]]
            parts <- trimws(parts)

            if (length(parts) > 1) {
                # If comma-separated, first part is family name, rest are
                # given names
                family <- parts[1]
                given <- paste(parts[-1], collapse = " ")
            } else {
                # If no comma, assume space-separated "Given Family" format
                tokens <- unlist(strsplit(person, " ", fixed = FALSE))
                tokens <- tokens[tokens != ""]
                # If no valid tokens, this author entry is invalid
                if (!length(tokens)) {
                    return(NA_character_)
                }
                # In "Given Family" format, last token is typically the
                # family name
                family <- tokens[length(tokens)]
                # Everything before the last token are given names
                if (length(tokens) > 1) {
                    given <- paste(tokens[-length(tokens)], collapse = " ")
                } else {
                    given <- ""
                }
            }

            # Clean given names: remove non-alphabetic characters except
            # spaces and hyphens
            # This handles titles, punctuation, and other artifacts in
            # name strings
            given <- gsub("[^[:alpha:]\\s-]", " ", given)
            # Split given names into tokens by spaces and hyphens to
            # extract initials
            given_tokens <- unlist(strsplit(given, "[-\\s]+"))
            given_tokens <- given_tokens[given_tokens != ""]
            # Create initials by taking first letter of each given name token
            # and capitalizing
            initials <- paste0(
                toupper(substr(given_tokens, 1, 1)),
                collapse = ""
            )
            # Remove any non-letter characters that might have slipped through
            initials <- gsub("[^A-Z]", "", initials)

            # Clean up family name by normalizing whitespace
            family <- trimws(family)
            family <- gsub("\\s+", " ", family)

            # If family name is empty after cleaning, this author is invalid
            if (family == "") {
                return(NA_character_)
            }

            # Return formatted name: just family name if no initials,
            # otherwise "Family Initials"
            if (initials == "") {
                return(family)
            }

            paste(family, initials)
        },
        character(1),
        USE.NAMES = FALSE
    )

    # Remove any NA or empty formatted names that resulted from
    # invalid author entries
    formatted <- formatted[!is.na(formatted) & formatted != ""]

    # If no valid formatted names remain, return NA for the entire entry
    if (!length(formatted)) {
        return(NA_character_)
    }

    # Join all formatted author names with "; " separator
    paste(formatted, collapse = "; ")
}

#' Function to clean and deduplicate keyword entries Keywords often come from
#' various sources with inconsistent formatting, duplicates, and extra
#' whitespace. This function standardizes them.
#'
#' @param x character string containing semicolon-separated keywords
#'
#' @return character string with cleaned, deduplicated keywords separated by ";
#'   "
clean_keywords <- function(x) {
    # Handle missing values by returning NA
    if (is.na(x)) {
        return(NA_character_)
    }
    # Split keywords by semicolon delimiter and trim whitespace
    # from each term
    terms <- trimws(unlist(strsplit(x, ";", fixed = TRUE)))
    # Remove empty strings that might result from splitting or trimming
    terms <- terms[terms != ""]
    # Remove duplicate keywords to avoid redundancy in analysis
    terms <- unique(terms)
    # If no valid terms remain after cleaning, return NA
    if (!length(terms)) {
        return(NA_character_)
    }
    # Rejoin cleaned and deduplicated terms with consistent "; " separator
    paste(terms, collapse = "; ")
}


# Data Transformation Section ---------------------------------------------
# This section applies all the cleaning and standardization functions
# to the raw data
# transforming it into a consistent, analysis-ready format

# Apply cleaning functions to standardize all text fields in the dataset
# within() allows modification of data frame columns in place
# without reassignment
gastaldi <- within(gastaldi_raw, {
    # Normalize title text by removing extra whitespace and newlines
    title <- normalize_text(title)
    # Normalize abstract text using the same standardization process
    abstract <- normalize_text(abstract)
    # Clean and standardize author names to consistent "Family Initials" format
    authors <- vapply(authors, clean_author_entry, character(1))
    # Clean and deduplicate keywords, removing duplicates and extra whitespace
    keywords <- vapply(keywords, clean_keywords, character(1))
    # Convert inclusion decision to logical: standardize to lowercase,
    # trim whitespace,
    # and check if equals "y" (case-insensitive comparison)
    included <- tolower(trimws(`Included Y/N`)) == "y"
})

# Select only the final columns needed for the cleaned dataset
# This ensures the dataset has a consistent structure with only relevant fields
gastaldi <- gastaldi[c("title", "abstract", "authors", "keywords", "included")]

# Data Validation Tests Section ----------------------------------------
# This section contains automated tests to verify the data processing
# worked correctly
# These tests ensure data quality and catch any unexpected changes in the data

# Test that the final dataset has exactly the expected column names
# in the correct order
# This validates that the column selection worked properly and
# maintains data structure
testthat::expect_named(
    gastaldi,
    c("title", "abstract", "authors", "keywords", "included")
)

# Create a frequency table of inclusion decisions to verify the
# expected distribution
# This ensures that the inclusion/exclusion coding worked correctly
inclusion_table <- table(gastaldi$included)
# Test that the number of excluded (FALSE) and included (TRUE) records
# matches expected values
# The expected values represent the known ground truth for this dataset:
# 1635 records marked as not included, 21 records marked as included
testthat::expect_equal(
    as.numeric(inclusion_table),
    c(1635, 21) # FALSE, TRUE
)

# Data Saving Section ---------------------------------------------------
# This section saves the cleaned and validated dataset as an R data file
# that can be loaded by users of the package

# Save both data and criteria artifacts for downstream usage
usethis::use_data(
    gastaldi,
    gastaldi_criteria,
    overwrite = TRUE,
    compress = "xz"
)
