
query <- "(systematic review) AND ((heart failure) AND (COPD))"
year_query <- "2021"

fake_ts <- structure(1642010354.70704, tzone = "", class = c("POSIXct", "POSIXt"))

expected_parsed_files <- c(
  "Embase_parsed.csv", "Pubmed_parsed.csv",
  "Scopus_parsed.csv", "Pubmed_API.csv"
)

file.remove(file.path("Records", "SessionTest", "Query1", expected_parsed_files))
file.remove("Session_journal.csv")

test_that("record journal is created by perform_search_session()", {
  journal <- perform_search_session(
    query = query, year_query = year_query,
    session_name = "SessionTest", query_name = "Query1",
    records_folder = "Records", skip_on_failure = TRUE,
    overwrite = TRUE,
    journal = "Session_journal.csv"
  ) %>%
    mutate(Timestamp = fake_ts)

  expect_true(file.exists("Session_journal.csv"))

  expect_snapshot_value(journal, style = "deparse")

  penv <- parent.env(environment())
  penv$journal <- journal
})

test_that("record file path extraction from extract_source_file_paths() works", {
  parsed_files <- extract_source_file_paths(journal)

  expect_snapshot_value(parsed_files, style = "deparse")

  penv <- parent.env(environment())
  penv$parsed_files <- parsed_files
})

test_that("source files get parsed without errors", {
  record_list <- read_bib_files(parsed_files)

  expect_length(record_list, 4)
  expect_named(record_list, c(
    "Pubmed_API.csv", "Pubmed_parsed.csv", "Scopus_parsed.csv",
    "Embase_parsed.csv"
  ))

  penv <- parent.env(environment())
  penv$record_list <- record_list
})

test_that("record file get joined and deduplicated", {
  joined_records <- join_records(record_list)

  expect_equal(duplicated(joined_records$ID) %>% sum(), 0)

  expect_snapshot_value(joined_records$ID, style = "deparse")

  penv <- parent.env(environment())
  penv$joined_records <- joined_records
})

test_that("record are reordered correctly", {
  ordered_records <- order_by_query_match(joined_records, query)

  expect_snapshot_value(ordered_records$ID, style = "deparse")

  penv <- parent.env(environment())
  penv$ordered_records <- ordered_records
})

test_that("perform_search_session() generates the expected files", {
  observed_files <- list.files(file.path("Records", "SessionTest", "Query1")) %>%
    intersect(expected_parsed_files)

  expect_setequal(observed_files, expected_parsed_files)

  output <- lapply(expected_parsed_files, function(f) {
    data <- import_data(file.path("Records", "SessionTest", "Query1", f)) %>%
      mutate(Creation_date = fake_ts)
  })

  expect_snapshot_value(output, style = "serialize")
})

test_that("annotation data gets created correctly", {
  Annotation_data <- create_annotation_file(record_list, reorder_query = query)

  expect_snapshot_value(Annotation_data, style = "serialize")

  penv <- parent.env(environment())
  penv$Annotation_data <- Annotation_data
})

test_that("session file get created correctly", {
  unlink(file.path("Sessions", "SessionTemp"), recursive = TRUE)

  folder <- create_session(Annotation_data, session_name = "SessionTest", use_time_stamp = )

  record_file <- list.files(folder, pattern = "Records", full.names = TRUE)

  expect_equal(list.files(folder, pattern = "Records") %>% length(), 1)
  expect_equal(Annotation_data, import_data(record_file))
})
