query <- "(systematic review) AND ((heart failure) AND (COPD))"
year_query <- "2021"

test_that('Records get collected properly by perform_search_session()', {
	perform_search_session(
		query = query, year_query = year_query,
		session_name = 'SessionTest', query_name = 'Query1',
		records_folder = 'Records',
		journal = 'Session_journal.csv')
})


rm(query, year_query)
