## Code to prepare `ssri` dataset.

# Source the review-specific eligibility criteria.
source(file.path("data-raw", "SSRIsCovid", "criteria.R"), local = FALSE)

# Normalize free-text fields into a single-line analysis-ready format.
normalize_text <- function(x) {
    x <- gsub("\n", " ", x, fixed = TRUE)
    x <- gsub("\\s+", " ", x, perl = TRUE)
    x <- trimws(x)
    x[x == ""] <- NA_character_
    x
}

# Load the review-specific screening export.
raw <- read.csv(
    file.path("data-raw", "SSRIsCovid", "data.csv"),
    check.names = FALSE
)

# Drop unnamed columns introduced by the source CSV export.
raw <- raw[, names(raw) != "", drop = FALSE]

# Standardize the dataset into the package benchmark schema.
ssri <- raw |>
    dplyr::transmute(
        title = normalize_text(.data[["title"]]),
        abstract = normalize_text(.data[["abstract"]]),
        authors = NA_character_,
        keywords = NA_character_,
        included = tolower(trimws(.data[["tag"]])) == "included"
    )

# Verify that the reconstructed data match the source counts.
testthat::expect_named(
    ssri,
    c("title", "abstract", "authors", "keywords", "included")
)
testthat::expect_equal(nrow(ssri), 4018L)
testthat::expect_equal(sum(ssri$included), 29L)

# Save the dataset and criteria object in the package data directory.
usethis::use_data(ssri, ssri_criteria, overwrite = TRUE, compress = "xz")
