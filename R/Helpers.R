#' Negation of %in% function
"%nin%" <- Negate("%in%")

#' Nicer output than scales::percent()
percent <- function(x) {
	sapply(x, function(x) {
		if (!is.na(x)) {
			if (abs(x * 100) < 1) {
				sprintf("%s%%", signif(x * 100, 2))
			} else {
				sprintf("%s%%", signif(x * 100, 3))
			}
		} else {
			NA
		}
	})
}

#' Override lubridate:today() which always complain about the missing
#' timezone
today <- function() {
	lubridate::as_date(as_date(Sys.time()))
}

#' Override lubridate:now() which always complain about the missing
#' timezone
now <- function() {
	Sys.time()
}

#' A file path friendly lubridate::now()
safe_now <- function() {
	stringr::str_replace_all(BaySREn:::now(), c(" " = "T", ":" = "."))
}

#' Tool to grab XHR messages from dynamic websites
get_website_resources <- function(url, url_filter = ".*", type_filter = ".*",
																	wait_for = 20,
																	n_of_resources = NULL, interactive = FALSE) {
	crrri::perform_with_chrome(function(client) {
		Fetch <- client$Fetch
		Page <- client$Page

		if (interactive) client$inspect()

		out <- new.env()

		out$results <- list()
		out$resolve_function <- NULL

		out$pr <- promises::promise(function(resolve, reject) {
			out$resolve_function <- resolve

			Fetch$enable(patterns = list(list(urlPattern = "*", requestStage = "Response"))) %...>%
				{
					Fetch$requestPaused(callback = function(params) {
						if (stringr::str_detect(params$request$url, url_filter) & stringr::str_detect(params$resourceType, type_filter)) {
							Fetch$getResponseBody(requestId = params$requestId) %...>% {
								resp <- .

								if (resp$body != "") {
									if (resp$base64Encoded) resp$body <- jsonlite::base64_dec(resp$body) %>% rawToChar()

									body <- list(list(
										url = params$request$url,
										response = resp
									)) %>% setNames(params$requestId)

									# str(body)

									out$results <- append(out$results, body)

									if (!is.null(n_of_resources) & length(out$results) >= n_of_resources) out$resolve_function(out$results)
								}
							}
						}

						Fetch$continueRequest(requestId = params$requestId)
					})
				} %...>%
				{
					Page$navigate(url)
				} %>%
				crrri::wait(wait_for) %>%
				crrri::then(~ out$resolve_function(out$results))
		})

		out$pr$then(function(x) x)
	}, timeouts = max(wait_for + 3, 30), cleaning_timeout = max(wait_for + 3, 30))
}
