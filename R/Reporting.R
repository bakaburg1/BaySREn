
# Summarising -------------------------------------------------------------



#' Summarise a discrete vector
#'
#' Shows for each unique element its numerosity and percentage.
#'
#' @param vec A vector.
#'
#' @return A string listing each unique element in the vector, its numerosity
#'   and percentage over the vector length (including missings).
#'
#' @examples
#'
#' summarise_vector(iris$Species)
#' # setosa: 50 (33.3%), versicolor: 50 (33.3%), virginica: 50 (33.3%)
#'
summarise_vector <- function(vec) {
  if (length(vec) == 0 | is.list(vec) | !is.null(dim(vec))) {
    return("incorrect input")
  }
  all_els <- length(vec)
  table(vec) %>%
    {
      paste0(names(.), ": ", ., " (", percent(. / all_els), ")", collapse = ", ")
    }
}

#' Record distribution between sources in an Annotation file
#'
#' Summarises the distribution of the source databases among the citation
#' records in an Annotation data set. It also reports the fraction of records
#' that are unique for each database.
#'
#' @param annotation_file An annotation data frame or a file path to it.
#' @param as_data_frame Whether to return the results as a data frame
#'   (\code{TRUE}) or a list (\code{FALSE}).
#' @param add_session_totals Whether to add the total number of record.
#'
#' @return For each source:\item{Records}{The number of records related to the
#'   source.}\item{\% over total}{the percentage over the total
#'   records.}\item{Source specific records}{the number of record derived only
#'   from that source.}\item{\% over source total}{the value in \code{Source
#'   specific records} as percentage over the source total.} If
#'   \code{as_data_frame} is \code{TRUE}, the results are returned as a data
#'   frame with a row for each source, otherwise as a nested list.
#'   \code{add_session_totals} adds another entry with the total number of
#'   records in the \code{annotation_file}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' records <- get_session_files("Session1")$Records
#'
#' summarise_by_source(records)
#' }
summarise_by_source <- function(annotation_file, as_data_frame = FALSE,
                                add_session_totals = TRUE) {
  data <- import_data(annotation_file)

  sources <- data$Source %>%
    stringr::str_split(., "; *") %>%
    unlist() %>%
    unique()

  total_records <- nrow(data)

  res <- lapply(sources, function(source) {
    Records <- stringr::str_detect(data$Source, glue("{source}")) %>% sum()
    Perc_over_total <- percent(Records / total_records)
    Source_specific <- stringr::str_detect(data$Source, glue("^{source}$")) %>% sum()
    Source_specific_perc <- percent(Source_specific / Records)

    list(
      Records = Records, Perc_over_total = Perc_over_total,
      Source_specific = Source_specific, Source_specific_perc = Source_specific_perc
    )
  }) %>% setNames(sources)

  if (add_session_totals) {
    res$Total <- list(
      Records = nrow(data),
      Perc_over_total = "",
      Source_specific = NA,
      Source_specific_perc = ""
    )
  }

  if (as_data_frame) {
    res <- res %>%
      lapply(as.data.frame.list) %>%
      bind_rows() %>%
      mutate(
        Source = names(res),
        .before = 1
      ) %>%
      arrange(desc(Records)) %>%
      setNames(c("Source", "Records", "% over total", "Source specific records", "% over source total"))
  }

  res
}

#' Record distribution between sources for each session
#'
#' Applies \code{\link{summarise_by_source}()} to all selected sessions.
#'
#' @param sessions A vector of session identifiers corresponding to folders into
#'   the \code{sessions_folder} folder.
#' @param sessions_folder Where to find the \code{sessions} folders.
#' @param add_global_totals Add results for all sessions considered together.
#' @param keep_session_label Add a column which groups rows by session. Useful
#'   for subsequent sub-setting of the results.
#' @param ... Other arguments passed to \code{\link{summarise_by_source}()}.
#'
#' @return A data frame with the number and fraction of total records per source
#'   and number and fraction of source-specific records, grouped by session. An
#'   extra group with the overall results is reported if
#'   \code{add_global_totals} is \code{TRUE}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Describe source distribution by source and in total
#' summarise_sources_by_session(add_global_totals = TRUE)
#' }
summarise_sources_by_session <- function(sessions = list.files(sessions_folder),
                                         sessions_folder = getOption("baysren.sessions_folder"),
                                         add_global_totals = TRUE, keep_session_label = FALSE, ...) {
  if (length(sessions) == 1) {
    res <- get_session_files(session, sessions_folder)$Records %>%
      summarise_by_source(as_data_frame = TRUE, ...)

    return(res)
  }

  records <- pbmcapply::pbmclapply(sessions, function(session) {
    get_session_files(session, sessions_folder)$Records %>%
      import_data()
  }) %>% setNames(sessions)

  res <- parallel::mclapply(1:length(records), function(i) {
    data <- records[[i]]

    if (i > 1) {
      previous_records <- bind_rows(records[1:(i - 1)])$ID
      data <- data %>% filter(ID %nin% previous_records)
    }

    summarise_by_source(data, as_data_frame = TRUE, ...) %>%
      mutate(
        Session_label = sessions[i]
      )
  })

  if (add_global_totals) {
    res <- bind_rows(
      res,
      summarise_by_source(last(records), as_data_frame = TRUE, ...) %>%
        mutate(
          Session_label = "All Sessions"
        )
    )
  }

  res <- res %>%
    group_by(Session_label) %>%
    mutate(
      Session = c(Session_label[1], rep("", n() - 1)),
      .before = 1
    ) %>%
    ungroup()

  if (!keep_session_label) {
    res$Session_label <- NULL
  }

  res
}

#' Format records' source distribution as a list
#'
#' Takes the output of \code{\link{summarise_sources_by_session}()} and formats it
#' as a list.
#'
#' @param source_summary A data frame produced by
#'   \code{\link{summarise_sources_by_session}()}.
#'
#' @return A hierarchical list with record distribution by source and session.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Describe source distribution by source and in total
#' source_summary <- summarise_sources_by_session(add_global_totals = TRUE)
#'
#' # Transform it into a list
#' source_session_summary_to_list(source_summary)
#' }
source_session_summary_to_list <- function(source_summary) { # TODO: include inside summarise_sources_by_session
  source_summary$Session_label %>%
    unique() %>%
    lapply(function(session) {
      df <- source_summary %>% filter(Session_label == session)
      df$Source %>%
        lapply(function(source) {
          df <- source_summary %>% filter(Session_label == session, Source == source)
          list(
            Records = df$Records,
            Perc_over_total = df$`% over total`,
            Source_specific = df$`Source specific records`,
            Source_specific_perc = df$`% over source total`
          )
        }) %>%
        setNames(df$Source)
    }) %>%
    setNames(unique(source_summary$Session_label))
}

#' Distribution of the number of sources in common per record
#'
#' Describe the distribution of the number of sources shared by records.
#'
#' @param annotation_file An annotation data frame or a file path to it.
#' @param as_propr Whether to output the results as absolute numbers or
#'   proportions over the record total.
#' @param format_fun If the results are shown as proportion (\code{as_propr ==
#'   TRUE}), which function to use to format them.
#'
#' @return A named vector with the number of sources in common as names and the
#'   absolute number/proportion as values.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get the annotation file
#' Annotation_file <- get_session_files("Session1")$Annotations[1]
#'
#' # Transform it into a list
#' get_source_distribution(Annotation_file)
#' }
get_source_distribution <- function(annotation_file, as_propr = TRUE, format_fun = percent) {
  res <- import_data(annotation_file)$Source %>%
    pbmcapply::pbmclapply(function(sources) {
      stringr::str_split(sources, "; *") %>%
        unlist() %>%
        n_distinct()
    }) %>%
    unlist() %>%
    table()

  if (as_propr) {
    res <- prop.table(res) %>% format_fun()
  }

  res
}

#' Describe results of a Classification/Review session
#'
#' Take a session identifier as input and describe the changes in the number of
#' positive and negative matches after each Classification/Review iteration. It
#' is necessary to run \code{\link{consolidate_results}()} before this command,
#' otherwise the results would not consider the changes made through the manual
#' review of the automatic classification.
#'
#' @param session_name A session identifier corresponding to folders into the
#'   \code{sessions_folder} folder.
#' @param sessions_folder Where to find the \code{sessions} folders.
#' @param remove_empty_columns If \code{TRUE}, shows change columns even if no
#'   changes of that type ever happened.
#' @param remove_raw_data Remove the \code{tot_reviewed_} and
#'   \code{total_records_} columns from the output, which store data and column
#'   name in machine readable column formats.
#'
#' @return A data frame reporting for a session the number of positive and
#'   negative matches after each Classification/Review iteration and the
#'   specific changes corresponding to each iteration.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' summarise_annotations("Session1")
#' }
summarise_annotations <- function(session_name, sessions_folder = getOption("baysren.sessions_folder"),
                                  remove_empty_columns = TRUE, remove_raw_data = TRUE) {
  result_list <- get_session_files(session_name, sessions_folder)$Results %>%
    lapply(function(file) {
      file %>%
        import_data() %>%
        mutate(
          Value = suppressWarnings(as.numeric(Value))
        ) %>%
        tidyr::pivot_wider(everything(), names_from = Indicator, values_from = Value)
    })

  total_records <- result_list[[1]] %>%
    select(matches("Change:")) %>%
    rowSums()

  lapply(0:length(result_list), function(i) {
    template <- tibble(
      "Change: unlab. -> y" = 0,
      "Change: unlab. -> n" = 0,
      "Change: unlab. -> *" = 0,
      "Change: y -> n" = 0,
      "Change: n -> y" = 0
    )


    if (i == 0) {
      result_data <- result_list[[1]] %>%
        select(!any_of(colnames(template))) %>% # Remove all changes info
        mutate(
          Iter = "Initial labelling",
          "Target: y" = result_list[[1]] %>% select(matches("Change: y")) %>%
            rowSums(),
          "Target: n" = result_list[[1]] %>% select(matches("Change: n")) %>%
            rowSums(),
          "Change: unlab. -> y" = `Target: y`,
          "Change: unlab. -> n" = `Target: n`,
        )
    } else {
      result_data <- result_list[[i]]
    }

    result_data %>%
      bind_cols(
        template %>% select(!any_of(colnames(result_data)))
      ) %>%
      transmute(
        Iteration = as.character(Iter),
        Positives = `Target: y`,
        Negatives = `Target: n`,
        tot_reviewed_ = Positives + Negatives,
        total_records_ = total_records,
        "Total labelled (%)" = glue("{tot_reviewed_} ({percent(tot_reviewed_ / total_records)})"),
        "Unlab. -> y" = `Change: unlab. -> y`,
        "Unlab. -> n" = `Change: unlab. -> n`,
        "Unlab. -> *" = `Change: unlab. -> *`,
        "y -> n" = `Change: y -> n`,
        "n -> y" = `Change: n -> y`,
        "N. features" = `N. features`
      ) %>%
      mutate(
        Changes = select(., matches("->")) %>% rowSums(),
        .before = `N. features`
      )
  }) %>%
    bind_rows() %>%
    {
      if (remove_empty_columns) {
        . <- select(., where(~ any(.x > 0)))
      }

      if (remove_raw_data) {
        . <- select(., !matches("_$"))
      }

      .
    }
}

#' Describe results of all Classification/Review sessions
#'
#' Applies \code{\link{summarise_annotations}()} to all session in the
#' \code{sessions_folder} folder.
#'
#' @param sessions_folder A repository where session folders are stored.
#' @param remove_empty_columns If \code{TRUE}, shows change columns even if no
#'   changes of that type ever happened.
#' @param remove_raw_data Remove the \code{tot_reviewed_} and
#'   \code{total_records_} columns from the output, which store data and column
#'   name in machine readable column formats.
#'
#' @return A data frame reporting for each session the number of positive and
#'   negative matches after each Classification/Review iteration and the
#'   specific changes corresponding to each iteration.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' summarise_annotations_by_session()
#' }
summarise_annotations_by_session <- function(sessions_folder = getOption("baysren.sessions_folder"),
                                             remove_empty_columns = TRUE,
                                             remove_raw_data = TRUE) {
  sessions <- list.files(sessions_folder)

  if (length(sessions) == 0) {
    stop('No session found in "', sessions_folder, '". Are you sure the name is not mispelled?')
  }

  parallel::mclapply(1:length(sessions), function(i) {
    session <- sessions[i]

    res <- summarise_annotations(session, sessions_folder,
      remove_empty_columns = FALSE, remove_raw_data = FALSE
    )

    if (i > 1) {
      res <- tail(res, -1)
    }

    res %>%
      mutate(
        Session = c(glue("{session} (n = {res$total_records_[1]})"), rep("", nrow(res) - 1)),
        Session_ = session,
        .before = 1
      )
  }) %>%
    bind_rows() %>%
    {
      if (remove_empty_columns) {
        . <- select(., where(~ any(.x > 0)))
      }

      if (remove_raw_data) {
        . <- select(., !matches("_$"))
      }

      .
    }
}


# Formatting --------------------------------------------------------------



#' Format a 3-values statistic
#'
#' Useful to format a 3-values statistic in the "point statistic \[interval
#' boundaries\]" format.
#'
#' @param interval A 3 values vector describing a point estimate and two interval
#'  boundaries.
#' @param percent Whether to format the results as percentages.
#'
#' @return A string in the "point statistic [interval boundaries]" format.
#'
#' @examples
#'
#' format_interval(qbeta(c(.05, .5, .95), 10, 14), percent = TRUE)
#'
format_interval <- function(interval, percent = FALSE) { # TODO: change "percent" into a user definable function, like for get_source_distribution()
  interval <- sort(interval)

  if (percent) interval <- percent(interval)

  interval %>%
    {
      glue("{.[2]} [{.[1]}, {.[3]}]")
    }
}

#' Pretty formatting of Session performance analysis
#'
#' Publication friendly formatting of the results of
#' \code{\link{estimate_performance}()}. If more than one results set is passed
#' (i.e., one per session), the will be added as new columns.
#'
#' @param ... One or more performance result data frames produced by
#'  \code{\link{estimate_performance}()}.
#' @param session_names Names of the sessions corresponding to the result data
#'  frames passed to \code{...}. If missing, they will be "Session" followed by
#'  an incremental number for each data frame passed to \code{...}.
#'
#' @return A long format data frame with the statistical indicators on the first
#'  column and a column with values for each data frame passed to \code{...}.
#' @export
#'
#' @examples
#' \dontrun{
#' Performance <- list(
#'   s1 = get_session_files("Session1")$Annotations %>% last() %>%
#'     import_data() %>% estimate_performance(),
#'   s2 = get_session_files("Session2")$Annotations %>% last() %>%
#'     import_data() %>% estimate_performance()
#' )
#'
#' format_performance(Performance$s1, Performance$s2)
#' }
format_performance <- function(..., session_names = NULL) {
  elements <- list(...)

  if (is.null(session_names)) session_names <- paste("Session", 1:length(elements))

  lapply(1:length(elements), function(i) {
    elements[[i]] %>% with({
      tibble(
        # Session = session_names[i],
        "Total records" = total_records,
        "Reviewed records (% over total records)" = glue("{n_reviewed} ({percent(n_reviewed/total_records)})"),
        "Expected efficiency (over random) [trunc. 90% PrI]" = efficiency %>% format_interval(percent = TRUE),
        "Observed positive matches (% over total records)" = glue("{obs_positives} ({percent(obs_positives/total_records)})"),
        "Predicted positive matches [trunc. 90% PrI]" = pred_positives %>% format_interval(),
        "Expected sensitivity [trunc. 90% PrI]" = sensitivity %>% format_interval(percent = TRUE),
        "Simple Model $R^2$ [90% CrI]" = mod_r2 %>% format_interval(percent = TRUE)
      ) %>%
        mutate_all(as.character) %>%
        tidyr::pivot_longer(everything(), names_to = "Indicator", values_to = session_names[i]) %>%
        {
          if (i > 1) .$Indicator <- NULL
          .
        }
    })
  }) %>% bind_cols()
}

#' Format variable importance results
#'
#' Publication ready formatting of the output of
#' [extract_var_imp()]. Separate the term from the part of the
#' record it was found in; uses "&" and "|" to identify non-consecutive n-grams
#' and redundant terms; reduce numeric values to significant digits.
#'
#' @param var_imp A data frame produced by \code{\link{extract_var_imp}()}.
#' @param as_data_frame Whether to format the output as data frame or as text.
#'
#' @return A formatted data frame or a string of text, depending on the
#'   \code{as_data_frame} argument.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' output <- extract_var_imp("Session1")
#'
#' format_var_imp(output)
#' }
format_var_imp <- function(var_imp, as_data_frame = TRUE) {
  var_imp <- var_imp %>%
    transmute(
      Component = stringr::str_extract(Term, "^\\w+(?=__)") %>%
        factor(
          c("ABSTR", "TITLE", "AUTH", "KEYS", "MESH"),
          c("Abstract", "Title", "Author", "Keyword", "Mesh term")
        ),
      Term = stringr::str_replace_all(Term, c("^\\w+__" = "", "\\._\\." = " & ", "\\." = " | ", "_" = " ")) %>% stringr::str_to_title(),
      "Inclusion rate" = signif(Value * 10000, 3),
      IS = signif(Score, 3),
      RR = signif(exp(estimate), 3) %>% stringr::str_remove("\\.?0+$"),
      `Statistic` = signif(statistic, 3) %>% stringr::str_remove("\\.?0+$"),
    )

  if (!as_data_frame) {
    var_imp <- with(var_imp, glue("{Term} ({Component}): {`Inclusion rate`} ({IS}) [{RR}, {`Statistic`}]"))
  }

  var_imp
}

#' Publication friendly tables for .rmd files
#'
#' A publication friendly version of \code{\link[knitr:kable]{knitr::kable()}}.
#' It automatically detect if the output is HTML or PDF and adapt the
#' formatting, allowing for latex formulas, large table, etc in PDF outputs.
#' Allows using \% in PDF tables.
#'
#' @param data A data frame
#' @param caption A caption to be displayed in the table.
#' @param allow_math Whether to allow latex math by disabling special character
#'   escape.
#' @param ... Other arguments passed to
#'   \code{\link[knitr:kable]{knitr::kable()}}
#'
#' @return An \code{\link[rkmarkdown:render]{rkmarkdown::render()}} ready table.
#'
print_table <- function(data, caption = "", allow_math = FALSE, ...) {
  if (knitr::is_latex_output()) {
    if (isTRUE(allow_math)) {
      data <- data %>%
        mutate(across(where(is.character), ~ stringr::str_replace_all(.x, "%", "\\\\%"))) %>%
        rename_with(~ stringr::str_replace_all(.x, "%", "\\\\%"))
    }

    data %>%
      knitr::kable(
        format = "latex", booktabs = TRUE,
        caption = caption %>% stringr::str_squish() %>%
          stringr::str_replace_all(c("%" = "\\\\%", "\\*\\*([^\\n]+)\\*\\*" = "\\\\textbf{\\1}")),
        escape = !allow_math,
        ...
        # format.args = list(floating = FALSE)
      ) %>%
      kableExtra::kable_styling(
        latex_options = c(
          "striped",
          if (ncol(data) > 5) "scale_down" else NULL,
          "hold_position"
        )
      )
  } else {
    knitr::kable(data, caption = caption, ...)
  }
}


# Plotting ----------------------------------------------------------------

#' Plot posterior predictive distributions generated by the classification model
#'
#' For each Classification/Review iteration, the function plots the mixture of
#' the posterior predictive distributions (PPD) of a positive match as predicted
#' by the classification model for the positive, negative, unknown labelled
#' records in the Annotation data sets.
#'
#' @param session_name A session identifiers corresponding to folders into the
#'   \code{sessions_folder} folder.
#' @param sessions_folder Where to find the \code{sessions} folders.
#'
#' @return A \code{ggplot2} object.
#'
#' @export
#'
plot_predictive_densities <- function(session_name,
                                      sessions_folder = getOption("baysren.sessions_folder")) {
  records_files <- get_session_files(session_name, sessions_folder)$Annotations
  samples_files <- get_session_files(session_name, sessions_folder)$Samples

  pbmcapply::pbmclapply(1:(length(records_files) + 1), function(i) {
    index <- min(i, length(records_files))

    # The last file will be imported twice, the second time will show the final labelling
    records <- records_files[[index]] %>%
      import_data()

    if (i <= length(records_files)) {
      records <- records %>%
        mutate(Rev_prediction_new = replace(Rev_prediction_new, !is.na(Rev_prediction_new), "*"))
    }

    records <- records %>%
      transmute(
        Pred_Low, Pred_Up,
        ID,
        Target = coalesce_labels(.)
      )

    neg_lim <- with(records, max(Pred_Up[Target %in% "n"]))
    pos_lim <- with(records, min(Pred_Low[Target %in% "y"]))

    samples <- samples_files[[index]] %>% readr::read_rds()

    unique(records$Target) %>%
      na.omit() %>%
      lapply(function(lab) {
        IDs <- records %>% with(ID[Target %in% lab])
        postsamples <- samples[samples$ID %in% IDs, -1] %>%
          as.matrix() %>%
          as.vector() %>%
          sample(size = 5000)

        data.frame(
          Iteration = factor(
            i,
            1:(length(records_files) + 1),
            c(1:length(records_files), "Final\nlabelling")
          ),
          Label = lab,
          Samples = postsamples,
          Neg_lim = neg_lim,
          Pos_lim = pos_lim
        )
      }) %>%
      bind_rows()
  }) %>%
    bind_rows() %>%
    mutate(
      Label = factor(Label, c("n", "y", "*"), c("Negative", "Positive", "To review"))
    ) %>%
    {
      df <- mutate(., Iteration = factor(Iteration, sort(unique(Iteration), TRUE)))

      unc_range_df <- select(df, -Samples) %>% distinct()

      group_split(df, Iteration, Label) %>%
        lapply(function(g) {
          dens <- density(arm::logit(g$Samples))

          data.frame(
            Iteration = g$Iteration[1],
            Label = g$Label[1],
            Prob = arm::invlogit(dens$x),
            Dens = dens$y
          )
        }) %>%
        bind_rows() %>%
        ggplot(aes(y = Iteration)) +
        ggridges::geom_ridgeline(aes(x = Prob, height = Dens, fill = Label, color = Label), alpha = .5, scale = 1) +
        geom_segment(data = unc_range_df, aes(yend = as.numeric(Iteration) + .1, x = Neg_lim, xend = Neg_lim, color = "Negative")) +
        geom_segment(data = unc_range_df, aes(yend = as.numeric(Iteration) + .1, x = Pos_lim, xend = Pos_lim, color = "Positive")) +
        geom_label(data = unc_range_df, aes(y = as.numeric(Iteration) - .1, x = Pos_lim, label = Pos_lim)) +
        geom_label(data = unc_range_df, aes(y = as.numeric(Iteration) - .1, x = Neg_lim, label = Neg_lim)) +
        scale_color_manual(values = c("Negative" = "darkred", "Positive" = "steelblue", "To review" = "violet")) +
        scale_fill_manual(values = c("Negative" = "darkred", "Positive" = "steelblue", "To review" = "violet")) +
        theme_minimal() +
        labs(x = "Positive match probability", y = "Iteration")
    }
}

#' Plot the cumulative trend of positive and negative labelled records.
#'
#' @param records An annotated data frame of records.
#' @param column The column from which the record labels are taken. By default
#'   the labels are taken by the manual plus the automatic classification,
#'   excluding labels imported using \code{import_classification()}.
#' @param step_size The interval with which the cumulative numbers are plotted.
#' @param limit How many records to display.
#'
#' @return A \code{ggplot2} object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- get_session_files("Session1")$Annotations %>%
#'   last() %>%
#'   import_data()
#'
#' plot_classification_trend(data)
#' }
plot_classification_trend <- function(records, column = NULL,
                                      step_size = 20, limit = NULL) {

  # Join manual classifications in one target column
  if (is.null(column)) {
    records <- records %>%
      mutate(Target = coalesce_labels(., c("Rev_prediction", "Rev_manual")))
  } else {
    records$Target <- records[[column]]
  }

  records <- records %>%
    arrange(Order) %>%
    filter(!is.na(Target))

  # Define plot breaks according to a limit of reviewed records
  if (is.null(limit)) limit <- max(which(!is.na(records$Target)))
  steps <- seq(step_size, limit, by = step_size) %>%
    c(limit) %>%
    unique()

  # Count positive and negative matches in every break
  df <- pbapply::pblapply(steps, function(step) {
    records %>%
      head(step) %>%
      summarise(
        Yes = sum(Target == "y", na.rm = T),
        No = sum(Target == "n", na.rm = T)
      )
  }) %>% bind_rows()

  # Plot trends
  p <- df %>%
    ggplot(aes(x = steps)) +
    geom_line(aes(y = Yes, color = "yes"), size = 1) +
    geom_line(aes(y = No, color = "no"), size = 1) +
    labs(y = "Records", x = "Records", color = "Classification") +
    theme_minimal()

  # Remove consecutive non changing values to avoid label cluttering
  df <- mutate(
    df,
    across(c(Yes, No), function(x) {
      c(x[1], sapply(2:(n() - 1), function(i) {
        if (x[i] == x[i - 1]) NA else x[i]
      }), x[n()])
    })
  )

  # Add labels
  p +
    geom_label(aes(y = Yes, x = steps, label = Yes), data = df, alpha = .8) +
    geom_label(aes(y = No, x = steps, label = No), alpha = .8)
}
