

# Export common functions -------------------------------------------------

`%>%` <- dplyr::`%>%`
select <- dplyr::select
any_of <- dplyr::any_of


# Global variables and setup --------------------------------------------------------

if (file.exists("secrets.R")) {
	source("secrets.R")
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

	expect_s3_class(results, class = 'data.frame')

	if (is.data.frame(results)) {
		result_struct <- sapply(results, class) %>% unlist()

		expect_mapequal(result_struct, expected = expected_struct)
	}

}


