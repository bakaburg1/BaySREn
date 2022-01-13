
query <- "(systematic review) AND ((heart failure) AND (COPD))" # Very strict rule to avoid large result sets
year_query <- "2021"

expected_struct <- c(Order = "integer", ID = "character", Title = "character", Abstract = "character",
										 DOI = "character", Authors = "character", URL = "character",
										 Journal = "character", Journal_short = "character", Article_type = "character",
										 Mesh = "character", Author_keywords = "character", Published = "character",
										 Source = "character", Source_type = "character", Creation_date = "POSIXct")

test_that("PUBMED can be searched without an API key", {
	results <- try(suppressWarnings(search_pubmed(query, year_query, api_key = NULL, record_limit = 100)), silent = TRUE)

	test_search_result(results, expected_struct)
})

test_that("PUBMED can be searched using an API key", {
	skip_if(is.null(getOption("baysren.ncbi_api_key")), 'PUBMED API key required')

	results <- try(search_pubmed(query, year_query, api_key = getOption("baysren.ncbi_api_key"), record_limit = 100), silent = TRUE)

	test_search_result(results, expected_struct)
})

rm(query, year_query)
