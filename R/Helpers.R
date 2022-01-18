#' Negation of %in% function
#'
#' @noRd
"%nin%" <- Negate("%in%")

#' Nicer output than scales::percent()
#'
#' @noRd
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
#'
#' @noRd
today <- function() {
  lubridate::as_date(Sys.time())
}

#' Override lubridate:now() which always complain about the missing
#' timezone
#'
#' @noRd
now <- function() {
  Sys.time()
}

#' A file path friendly lubridate::now()
#'
#' @noRd
safe_now <- function() {
  stringr::str_replace_all(now(), c(" " = "T", ":" = "."))
}

#' Create an empty data.frame out of template with column names and types
#'
#' @noRd
create_empty_df <- function(cols_spec) {
  df <- data.frame()

  for (col in cols_spec) {
    if (col %in% c("POSIXct", "POSIXt")) {
      v <- structure(numeric(), class = c("POSIXct", "POSIXt"), tzone = "")
      df <- cbind(df, v)
    } else {
      df <- cbind(df, vector(col, 0))
    }
  }

  setNames(df, names(cols_spec))
}

#' Tool to grab XHR messages from dynamic websites
#'
#' @importFrom promises %...>%
#'
#' @noRd
get_website_resources <- function(url, url_filter = ".*", type_filter = ".*",
                                  wait_for = 20,
                                  n_of_resources = NULL, interactive = FALSE) {

  # Silence CMD CHECK about non standard eval
  . <- NULL

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

#' Ask user for permission before important operations
#'
#' The function requires handlers for refusal or acceptance.
#'
#' @param q The request.
#' @param y_action Handler if the request is accepted.
#' @param n_action Handler if the request is rejected
#'
#' @noRd
ask_user_permission <- function(q, y_action, n_action = NULL) {
  repeat {
    ans <- readline(paste0(q, " "))

    if (ans %nin% c("n", "y")) {
      warning("Only accepted answers are 'y' or 'no'", immediate. = TRUE, call. = FALSE)
    } else {
      if (ans == "y") {
        return(y_action())
      } else {
        if (is.function(n_action)) {
          return(n_action())
        }
      }

      break
    }
  }
}


#' Ask user for permission before installing suggested packages
#'
#' @param pkgs A character vector of packages.
#' @param is_required_msg A message to justify the request.
#' @param stop_on_rejection Whether to throw an error (`TRUE`) or a warning
#'   (`FALSE`) if the user rejects the request.
#' @param on_rejection_msg A message to be shown if the user rejects the
#'   request.

#' @return A boolean vector of the same length of `pkgs`, with `TRUE` if the
#'   installation was successful or if the package was already installed.
#'
#' @noRd
check_suggested_packages <- function(pkgs, is_required_msg = "to use this function",
                                     stop_on_rejection = TRUE,
                                     on_rejection_msg = "The function cannot be run without the required packages.") {
  purrr::map_lgl(pkgs, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      ans <- ask_user_permission(
        q = glue("Package '{pkg}' is required {is_required_msg}. Do you want to install it now? y/n"),
        y_action = function() TRUE,
        n_action = function() FALSE
      )

      if (isTRUE(ans)) {
        inst <- try(utils::install.packages(pkg))

        if (class(inst != "try-error")) {
          return(TRUE)
        } else {
          ans <- FALSE
        }
      }

      if (isFALSE(ans)) {
        if (isTRUE(stop_on_rejection)) {
          stop(on_rejection_msg)
        } else {
          warning(on_rejection_msg, call. = FALSE, immediate. = TRUE)

          return(FALSE)
        }
      }
    } else {
      return(TRUE)
    }
  })
}
