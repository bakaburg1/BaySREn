
query <- "(systematic review) AND (heart failure)"

year_query <- NULL

expected_struct <- c(
  Order = "integer", ID = "character", Title = "character", Abstract = "character",
  DOI = "character", URL = "character", Authors = "character",
  Journal = "character", Article_type = "character", Author_keywords = "character",
  Keywords = "character", N_citations = "integer", Published = "character",
  Source = "character", Source_type = "character"
)

test_that("IEEE can be searched without an API key", {
  results <- try((search_ieee(query, year_query, api_key = NULL, record_limit = 100)), silent = TRUE)

  test_search_result(results, expected_struct)
})

test_that("IEEE can be searched using an API key", {
  skip_if(is.null(getOption("baysren.ieee_api_key")), "IEEE API key required")

  results <- try((search_ieee(query, year_query, api_key = getOption("baysren.ieee_api_key"), record_limit = 100)), silent = TRUE)

  test_search_result(results, expected_struct)
})

rm(query, year_query)
