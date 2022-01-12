

# Export common functions -------------------------------------------------

`%>%` <- dplyr::`%>%`
select <- dplyr::select
any_of <- dplyr::any_of


# Global variables and setup --------------------------------------------------------

if (file.exists("secrets.R")) {
	source("secrets.R")
}

# General helpers --------------------------------------------------------

expect_same_colnames <- function(obs, exp) {
	expect_equal(sort(colnames(obs)), sort(colnames(exp)))
}

expect_same_coltypes <- function(obs, exp) {
	expect_equal(sort(colnames(obs)), sort(colnames(exp)))
}
