## code to prepare `nature_llm` dataset

# Point to the GPT-5 screening output from the Nature Medicine review repository.
local_path <- file.path(
    "data-raw",
    "NatureMedicineLLM",
    "deduped_and_processed_studies-GPT-5r-high.csv"
)

# Load the companion criteria object for the packaged dataset.
criteria_path <- file.path("data-raw", "NatureMedicineLLM", "criteria.R")

# Fail early when the Nature Medicine source CSV or criteria file is missing.
required_paths <- c(local_path, criteria_path)
missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths) > 0) {
    stop(
        "Missing Nature Medicine LLM source files: ",
        paste(missing_paths, collapse = ", "),
        call. = FALSE
    )
}

# Source the preformatted criteria definition.
source(criteria_path, local = FALSE)
if (!exists("nature_llm_criteria")) {
    stop(
        "Object `nature_llm_criteria` is not defined in ",
        criteria_path,
        call. = FALSE
    )
}

# Normalize free-text fields into a single-line analysis-ready format.
normalize_text <- function(x) {
    x <- gsub("\n", " ", x, fixed = TRUE)
    x <- gsub("\\s+", " ", x, perl = TRUE)
    x <- trimws(x)
    x[x == "" | x == "[No abstract available]"] <- NA_character_
    x
}

# Read the GPT-5 screening output while preserving source column names.
nature_llm_raw <- read.csv(local_path, check.names = FALSE)

# Rebuild the public dataset with the package's minimal benchmark schema.
nature_llm <- nature_llm_raw |>
    dplyr::transmute(
        title = normalize_text(.data$Title),
        abstract = normalize_text(.data$Abstract),
        authors = NA_character_,
        keywords = NA_character_,
        included = tolower(trimws(.data[["Include?"]])) == "yes"
    )

# Assert the packaged Nature Medicine LLM dataset matches the package schema.
testthat::expect_named(
    nature_llm,
    c("title", "abstract", "authors", "keywords", "included")
)

# Assert the collected GitHub CSV counts remain stable.
testthat::expect_equal(nrow(nature_llm), 12896L)
testthat::expect_equal(sum(nature_llm$included), 4609L)

# Save the cleaned Nature Medicine LLM dataset and its criteria for package use.
usethis::use_data(
    nature_llm,
    nature_llm_criteria,
    overwrite = TRUE,
    compress = "xz"
)
