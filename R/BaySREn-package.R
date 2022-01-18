#' A package to automatise citation collection and screening in Systematic
#' Reviews
#'
#' @docType package
#' @name BaySREn-package
#' @aliases BaySREn
#'
#' @description Secondary research is of paramount importance to summarise the
#' latest developments in every research field. Nevertheless, conducting a
#' structured collection and analysis of literature (i.e., a Systematic Review)
#' is becoming exceedingly demanding in terms of time and human resources.We
#' propose an integrated framework that streamlines and automates the citation
#' data collection and title/abstract screening phases of Systematic Reviews,
#' limiting human intervention to a minimum. The framework employs automatic
#' citation collection from online scientific databases and importation tools to
#' import citation data collected manually; it uses a Bayesian active
#' machine-learning algorithm to screen relevant citation, requiring human input
#' only at the beginning of the process and to review uncertain classifications.
#' Finally, the framework provides a tool to generate search queries with online
#' scientific databases based on an already labelled corpus of citation data.
#'
#' @import dplyr ggplot2
#' @importFrom glue glue
#' @importFrom tidyselect all_of any_of one_of contains ends_with starts_with
#'   everything
#' @importFrom utils head tail str capture.output getFromNamespace
#' @importFrom stats setNames quantile na.omit plogis qlogis sd
#'
NULL



.onLoad <- function(libname, pkgname) {
  # options(java.parameters= "-Xmx16g")
  # options(baysren.BartMem = '16')

  if (is.null(getOption("baysren.BartMem"))) {
    if (interactive()) {
      mem <- readline("How many GB of memory should be used by the BART model?\n(better no more than 90% of available RAM)\n ")

      if (is.na(as.numeric(mem))) stop("Input should be a number.")
    } else {
      mem <- 16
    }

    options(baysren.BartMem = mem)
  }

  mem <- paste0("-Xmx", getOption("baysren.BartMem"), "g")

  options(java.parameters = mem)
}
