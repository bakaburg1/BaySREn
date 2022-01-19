
# Export common functions -------------------------------------------------

`%>%` <- dplyr::`%>%`
select <- dplyr::select
any_of <- dplyr::any_of


# Global variables and setup --------------------------------------------------------

opt_backup <- options()

if (file.exists("secrets.R")) {
  source("secrets.R")
} else {
  warning("A secrets.R file should be created into test/testthat to perform all tests.\nIn this file the \"baysren.ieee_api_key\", \"baysren.wos_api_key\", and \"baysren.ncbi_api_key\" options should be set or the API related tests will be skipped.",
    call. = TRUE, immediate. = TRUE
  )
}

# custom expects --------------------------------------------------------

expect_same_colnames <- function(obs, exp) {
  expect_equal(sort(colnames(obs)), sort(colnames(exp)))
}

expect_same_coltypes <- function(obs, exp) {
  expect_equal(sort(colnames(obs)), sort(colnames(exp)))
}

# custom expects --------------------------------------------------------

test_search_result <- function(results, expected_struct) {
  expect_s3_class(results, class = "data.frame")

  if (is.data.frame(results)) {
    result_struct <- sapply(results, function(x) class(x)[1]) %>% unlist()

    expect_mapequal(result_struct, expected = expected_struct)
  }
}


# clean up --------------------------------------------------------

withr::defer(options(opt_backup), teardown_env())
