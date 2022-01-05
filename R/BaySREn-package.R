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
#' @importFrom utils head tail str capture.output
#' @importFrom stats setNames quantile na.omit plogis qlogis sd
#'
NULL
