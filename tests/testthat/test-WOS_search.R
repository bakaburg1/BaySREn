query <- "(systematic review) AND ((heart failure) AND (COPD))"
year_query <- "2021"

expected_struct <- c(
  Order = "integer", ID = "character", Title = "character", Abstract = "character",
  DOI = "character", Journal = "character", N_citations = "integer",
  Published = "character", Source = "character", Source_type = "character",
  Creation_date = "character", Authors = "character", Topic = "character",
  Article_type = "character", Author_keywords = "character", Keywords = "character"
)

test_that("WOS can be searched using an API key", {
  skip_if(is.null(getOption("baysren.wos_api_key")), "WOS API key required")

  results <- try(search_wos(query, year_query, api_key = getOption("baysren.wos_api_key")), silent = TRUE)

  skip_if(grepl("Session not found", results), "WOS API key is outdated and needs renewal")

  test_search_result(results, expected_struct)
})

rm(query, year_query)
