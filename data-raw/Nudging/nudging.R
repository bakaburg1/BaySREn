## code to prepare `nudging` dataset

# Define the remote file path
remote_path <- paste0(
    "https://raw.githubusercontent.com/asreview/systematic-review-datasets/",
    "metadata-v1-final/datasets/Nagtegaal_2019/output/",
    "Nagtegaal_2019.csv")
local_path <- file.path("data-raw", "Nudging", "data.csv")

# Load criteria documenting the title/abstract screening provenance.
criteria_path <- file.path("data-raw", "Nudging", "criteria.R")
if (!file.exists(criteria_path)) {
    stop("Criteria script not found: ", criteria_path, call. = FALSE)
}
source(criteria_path, local = FALSE)
if (!exists("nudging_criteria")) {
    stop(
        "Object `nudging_criteria` is not defined in ",
        criteria_path,
        call. = FALSE
    )
}

# Download the file if it doesn't exist
if (!file.exists(local_path)) {
    download.file(remote_path, local_path)
} else {
    warning("File already exists, skipping download",
            call. = FALSE, immediate. = TRUE)
}

# Read the CSV file
nudging <- read.csv(local_path)

# Data preparation
nudging <- within(nudging, {
    # Clean title and abstract: remove newlines and extra spaces
    title <- trimws(gsub("\n", " ", title))
    abstract <- trimws(gsub("\n", " ", abstract))
    # Convert empty strings to NA
    title <- ifelse(title == "", NA_character_, title)
    abstract <- ifelse(abstract == "", NA_character_, abstract)

    # Convert included to logical
    included <- as.logical(included)

    # Add empty authors and keywords columns for consistency
    authors <- rep(NA_character_, length(title))
    keywords <- rep(NA_character_, length(title))
})

# Ensure the data frame has the correct column order
nudging <- nudging[c("title", "abstract", "authors", "keywords", "included")]

# Test column names
testthat::expect_named(
    nudging,
    c("title", "abstract", "authors", "keywords", "included")
)

# Test inclusion counts
inclusion_table <- table(nudging$included)
testthat::expect_equal(
    as.numeric(inclusion_table),
    c(1747, 100)  # 1747 FALSE, 100 TRUE
)

# Save the data in the package
usethis::use_data(nudging, nudging_criteria, overwrite = TRUE, compress = "xz")
