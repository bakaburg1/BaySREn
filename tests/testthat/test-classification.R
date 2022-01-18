unlink(list.files(pattern = '.pID', all.files = TRUE))
unlink('Model_backup.rds')
unlink('Sessions/Session1/Annotations/', recursive = TRUE)
unlink('Sessions/Session1/Results/', recursive = TRUE)
unlink('Sessions/Session1/Samples/', recursive = TRUE)
unlink('Sessions/Session1/DTM.rds', recursive = TRUE)

print(tryCatch(rJava::.jcheck(silent = TRUE) == 0, error = function(e) FALSE))

oldMem <- getOption('baysren.BartMem')
options(baysren.BartMem = '16')

test_that("annotation file classification doesn't throws errors.", {
  set.seed(1482398429)

  expect_error(enrich_annotation_file("Session1", pos_mult = 20, n_models = 2, autorun = TRUE, limits = list(
  	stop_after = 2, pos_target = NULL,
  	labeling_limit = NULL
  )), NA)

})

session_files <- get_session_files('Session1', sessions_folder = 'Sessions')

sessions_files_exist <- test_that("the expected number of files was created.", {
	exp <- c(Records = 1L, Annotations = 3L, DTM = 1L, Samples = 3L, Results = 3L
	)

	obs <- session_files %>% sapply(length)

	expect_mapequal(obs, exp)
})

if (isTRUE(sessions_files_exist)) {

	test_that("test DTM", {
		expect_snapshot_file(session_files$DTM)
	})

	# There should be three versions of each file type, so no dynamic init here
	for (i in 1:3) {
		test_that(paste("Samples file", i, "matches expectation."), {
			data <- import_data(session_files$Samples[i])[1:20, 1:20] # Limit memory footprint
			expect_snapshot_value(data, style = "serialize")
		})

		test_that(paste("Record file", i, "matches expectation."), {
			data <- import_data(session_files$Results[i])[-2,] # Remove fields with timestamps
			expect_snapshot_value(data, style = "deparse")
		})

		test_that(paste("Annotation file", i, "matches expectation."), {
			data <- import_data(session_files$Annotations[i]) %>%
				select(matches("^(Prev_|Rev_|Order)")) %>%  # Keep varying fields
				head(20)
			expect_snapshot_value(data, style = "serialize")
		})
	}

}

options(baysren.BartMem = oldMem)
