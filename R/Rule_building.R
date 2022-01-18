#' Extract screening rules from an Annotation data set
#'
#' Starting from a Document Term Matrix (DTM) and a posterior predictive
#' distribution (PPD) matrix produced by the Bayesian classification engine, a
#' decision tree algorithm is used to extract rules that partition a subset of
#' draws from the PPD. Beware that the generation of the rules may take a long
#' time.
#'
#' The algorithm allows to use only a subset of the terms in the DTM and of the
#' samples in the PPD matrix to cut on computation time. In the first case, a
#' threshold is used to filter only the most relevant features in the DTM.
#' Before being used, terms in the DTM are aggregated if they appear in multiple
#' fields of the citation records and only their general presence in the record
#' will be stored.
#'
#' @param session_name A session identifier corresponding to folders into the
#'   \code{sessions_folder} folder.
#' @param rebuild_dtm Whether to use the last DTM stored in the
#'   \code{session_name} folder (\code{FALSE}) or rebuild it from the last
#'   Annotation file (\code{TRUE}).
#' @param vimp.threshold A threshold in the standardized variable importance
#'   score to filter out less relevant terms in the DTM.
#' @param n.trees How many draws to use from the PPD matrix to build decision
#'   trees. This parameter strongly impacts computational time but increases
#'   sensitivity of the rules found.
#' @param sessions_folder Where to find the \code{sessions} folders.
#' @param save_path Since generating the rules is a computation intense process
#'   it's advisable to save the output in a .rds file placed inside the
#'   \code{session_name} folder. User need only to provide the name of the file.
#' @param ... Additional arguments passed to [rpart::rpart.control()].
#'
#' @return A list with: \item{SpecificDTM}{The DTM with the less relevant terms
#'   being filtered out and terms in multiple record fields being
#'   aggregated.}\item{DTM}{The full DTM with the predicted
#'   classification.}\item{rules}{A data frame reporting the selected rules with
#'   the average PPD.}
#'
#' @export
#'
extract_rules <- function(session_name, rebuild_dtm = FALSE, vimp.threshold = 1.25,
                          n.trees = 800, sessions_folder = getOption("baysren.sessions_folder", "Sessions"),
                          save_path = file.path(sessions_folder, session_name, "rule_data.rds"), ...) {

	# Silence CMD CHECK about non standard eval
	ID <- Target <- Score <- Term <- NULL

  message("Preparing the data")

  files <- get_session_files(session_name, sessions_folder) %>%
    lapply(last) # get only last files

  Records <- import_data(files$Annotations)

  DTM <- if (rebuild_dtm | is.null(files$DTM)) {
    message("Rebuilding the DTM")

    create_training_set(Records)
  } else {
    readr::read_rds(files$DTM)
  }

  Draws <- readr::read_rds(files$Samples)

  Variable_importance <- import_data(files$Annotations, sheet = "Variable_importance")

  DTM <- Records %>%
    transmute(
      ID,
      Target = coalesce_labels(cur_data(), c(
        "Rev_prediction_new", "Rev_prediction",
        "Rev_manual", "Predicted_label"
      ))
    ) %>%
    right_join(DTM %>% select(-Target), by = "ID")

  message("Generating feature dataset")

  specific.terms <- Variable_importance %>%
    filter(Score > vimp.threshold) %>%
    pull(Term) %>%
    stringr::str_subset("^MESH", negate = TRUE) %>%
    stringr::str_remove(".+__") %>%
    stringr::str_sub(1, 2500) %>% # str_sub() is necessary since many functions cannot use such long names
    unique()


  SpecificDTM <- pbmcapply::pbmclapply(specific.terms, function(term) {
    values <- select(DTM, matches(paste0("__", term, "$"))) %>% rowSums(na.rm = TRUE)

    factor((values > 0) + 0)
  }) %>%
    setNames(paste0("V__", specific.terms)) %>%
    bind_cols() %>%
    mutate_all(~ replace(.x, is.na(.x), 0)) %>%
    select(tidyselect::vars_select_helpers$where(~ n_distinct(.x) > 1))

  print(paste("N. features:", ncol(SpecificDTM)))

  tictoc::toc()

  message("Computing trees")

  if (n.trees > ncol(Draws)) {
  	warning("Number of trees > than number of MCMC draws. All draws will be used", call. = FALSE, immediate. = TRUE)
  }

  # Add the median estimate and ensure it is included among the candidate draws
  Draws <- cbind(Draws[, -1], Records$Pred_Med)
  candidate_draws <- sample(1:ncol(Draws), min(n.trees, ncol(Draws))) %>%
    c(ncol(Draws)) %>%
    unique()

  trees <- pbmcapply::pbmclapply(candidate_draws, function(i) {
    df <- data.frame(
      Pred = Draws[, i],
      SpecificDTM
    )

    rpart::rpart(Pred ~ ., data = df, control = rpart::rpart.control(...)) %>%
      tidytrees::tidy_tree(eval_ready = TRUE, simplify_rules = TRUE)
  }) %>% bind_rows()

  out <- list(
    SpecificDTM = SpecificDTM,
    DTM = DTM,
    rules = trees$rule
  )

  if (!is.null(save_path)) {
    readr::write_rds(save_path)
  }

  out
}

#' Add cumulative numbers of positive and negative matches to a list of rules
#'
#' Given a Rules data frame, arranges it by a score and rule length and computes
#' the cumulative number of positive and negative matches identified by the
#' rules.
#'
#' @param data A Rules data frame.
#' @param order_by A column in \code{data} according to which order rules in
#'   descending order.
#' @param rule_var The name of the column in \code{data} which stores the rules.
#'
#' @return The Rules data frame with two more columns for the cumulative numbers
#'   of positive and negative matches.
#'
add_cumulative <- function(data, order_by = "score", rule_var = "rule") {
  arrange(data, desc(get(order_by)), stringr::str_count(get(rule_var), " & ")) %>%
    mutate(
      cum_pos = purrr::map_int(1:n(), ~ n_distinct(unlist(pos_i[1:.]))),
      cum_neg = purrr::map_int(1:n(), ~ n_distinct(unlist(neg_i[1:.]))),
      .after = any_of(order_by)
    )
}

#' Generate a rule selection set for user review
#'
#' The rules extracted by [extract_rules()] rules are grouped by similar
#' sensitivity and presented to the user who will need to select a subset of
#' them (ideally one per sensitivity group).
#'
#' Only rules with at least one positive component are selected and (optionally)
#' negative terms are added to them from the Document Term Matrix (DTM) to
#' increase specificity. Rules are then arranged by \emph{positive - negative}
#' records matched and grouped by the cumulative number of records identified.
#'
#' The first column of the output shows which rule is the suggested one for each
#' performance group. The user can edit this column, adding/removing rules by
#' setting them \code{TRUE} or \code{FALSE}. Once the rules are reviewed, it's
#' advisable to change the file name, e.g., "Selected_rules.xlsx".
#'
#' @param rules A vector of rules as produced by [extract_rules()].
#' @param target_vec A vector of labels.
#' @param target_data A DTM with a number of rows as the elements in
#'   \code{rules}.
#' @param add_negative_terms Whether to increase specificity by adding negative
#'   terms to the rules. Adding the negative terms in computationally heavy.
#' @param save_path Where to save the Excel file in which users will need to
#'   select the final rules. A best practice is to save the output in the
#'   session folder used to generate the rules and call it
#'   "Selected_rules.xlsx".
#'
#' @return A data frame with groups of rules characterized by equal cumulative
#'   sensitivity. The first column marks the suggested rule for each group. The
#'   data frame gets saved to the file in \code{save_path} if not \code{NULL}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' candidate_queries <- readRDS(file.path("Sessions", "Session1", "rule_data.rds"))
#'
#' Target <- candidate_queries$DTM$Target
#' SpecificDTM <- candidate_queries$SpecificDTM
#'
#' selection_set_file <- file.path("Sessions", "Session1", "Selected_rules_reviewed.xlsx")
#'
#' selection_set <- generate_rule_selection_set(
#'   candidate_queries$rule,
#'   target_vec = Target,
#'   target_data = SpecificDTM,
#'   save_path = selection_set_file
#' )
#' }
generate_rule_selection_set <- function(rules, target_vec, target_data, add_negative_terms = TRUE,
                                        save_path = NULL) {

	# Silence CMD CHECK about non standard eval
	rule <- term <- neg_term <- rule_with_negs <- cum_pos <- score <- NULL

  rules <- rules[stringr::str_detect(rules, '"1"')] %>% # Only rules with a least one positive component
    tidytrees::simplify_rules() %>% # Remove redundant rule components
    unique()

  if (add_negative_terms) {
    rules <- add_negative_terms(rules, target_vec, target_data) %>%
      select(rule, neg_term = term) %>%
      bind_rows(
        data.frame(rule = rules, neg_term = NA)
      ) %>%
      arrange(rule, !is.na(neg_term)) %>%
      group_by(rule) %>%
      mutate(
        rule_with_negs = purrr::map_chr(1:n(), ~ {
          terms <- na.omit(neg_term[1:.])

          if (length(terms) > 0) {
            paste(rule[1], "&", paste(terms, '%in% "0"', collapse = " & "))
          } else {
            rule[1]
          }
        })
      ) %>%
      pull(rule_with_negs)
  }

  message("- computing scores")

  rules <- rules %>%
    pbapply::pblapply(function(rule) {
      filt <- with(target_data, eval(str2expression(rule)))

      # if (!is.na(neg_term)) {
      # 	filt <- filt & target_data[[neg_term]] %in% "0"
      # }

      pos_bool <- (target_vec %in% "y" & filt)
      neg_bool <- (target_vec %in% "n" & filt)

      pos <- sum(pos_bool)
      neg <- sum(neg_bool)

      tibble(
        rule, pos, neg,
        score = pos - neg,
        pos_i = list(which(pos_bool)),
        neg_i = list(which(neg_bool))
      )
    }) %>%
    bind_rows()

  rules <- rules %>%
    add_cumulative() %>%
    group_by(cum_pos) %>%
    mutate(selected_rule = c(TRUE, rep(FALSE, n() - 1)), .before = rule) %>%
    summarise_all(~ c(.x, NA)) %>%
    mutate(cum_pos = replace(cum_pos, is.na(rule), NA)) %>%
    relocate(cum_pos, .after = score) %>%
    ungroup()

  # if (simplify) {
  # 	message('- simplifying rules')
  # 	rules <- simplify_ruleset(rules) %>%
  # 		generate_rule_selection_set(target_vec = target_vec, target_data = target_data,
  # 																add_negative_terms = FALSE, simplify = FALSE)
  # }

  if (!is.null(save_path)) {
    openxlsx::write.xlsx(rules, save_path) # TODO: allow csv output as package options
  }

  rules
}

#' Add negative terms to rules
#'
#' Increases rule specificity by adding negative terms iteratively, selecting
#' only terms that remove non-relevant records without impacting the number of
#' positive matches. Usually not used by itself, but inside
#' [generate_rule_selection_set()].
#'
#' @param rules A vector of rules as produced by [extract_rules()].
#' @param target_vec A vector of labels.
#' @param target_data A DTM with a number of rows as the elements in
#'   \code{rules}.
#'
#' @return A data frame with a column with the negative terms for each
#'   rule, the number of positive and negative matches identified by the
#'   negative terms in the \code{target_data} and the ID of these records..
#'

add_negative_terms <- function(rules, target_vec, target_data) {

	# Silence CMD CHECK about non standard eval
	pos <- word <- term <- selected_term <- NULL

  message("- retrieving negative terms")
  pbmcapply::pbmclapply(rules, function(rule) {
    filt <- with(target_data, which(eval(str2expression(rule))))

    target_data <- target_data[filt, ] %>%
    	select(tidyselect::vars_select_helpers$where(~ n_distinct(.x) > 1))

    target_vec <- target_vec[filt]
    tot_pos <- sum(target_vec == "y")
    tot_neg <- sum(target_vec == "n")

    terms <- colnames(target_data) %>%
      stringr::str_subset("V__\\d+$", negate = TRUE) %>%
      stringr::str_subset("V__\\w{1,3}$", negate = TRUE)

    term_pos_dict <- lexicon::hash_grady_pos %>% with(setNames(pos, word))
    excluded_pos <- term_pos_dict[term_pos_dict %nin% c("Adjective", "Noun", "Plural", "Noun Phrase")]

    terms <- terms[stringr::str_remove(terms, "V__") %nin% names(excluded_pos)]

    if (length(terms) == 0) {
      return(data.frame(
        rule,
        term = NA, term_type = NA, pos = NA, neg = NA, neg_i = NA
      ))
    }

    terms <- lapply(terms, function(v) {
      pos_bool <- (target_vec %in% "y" & target_data[[v]] %in% "1")
      neg_bool <- (target_vec %in% "n" & target_data[[v]] %in% "1")

      pos <- sum(pos_bool)
      neg <- sum(neg_bool)

      tibble(
        term = v,
        term_type = term_pos_dict[stringr::str_remove(v, stringr::fixed("V__"))],
        pos, neg,
        neg_i = list(which(neg_bool))
      )
    }) %>%
      bind_rows() %>%
      # 	{tryCatch(
      # 	filter(., pos == 0 & neg > 0), error = function(e) browser()
      # )} %>%
      filter(pos == 0 & neg > 0) %>%
      mutate(
        rule = rule,
        #' pos/neg' = glue('{tot_pos}/{tot_neg}'),
        .before = 1
      ) %>%
      mutate(selected_term = FALSE, .after = term)

    if (nrow(terms) == 0) {
      return(data.frame(
        rule,
        term = NA, term_type = NA, pos = NA, neg = NA, neg_i = NA
      ))
    }

    terms <- arrange(terms, desc(neg)) %>%
      mutate(id = 1:n())

    terms$selected_term[1] <- TRUE

    rep <- TRUE

    while (rep) {
      terms[!terms$selected_term, ] %>% with({
        neg_i <- sapply(neg_i, setdiff, unlist(terms[terms$selected_term, ]$neg_i))
        neg <- sapply(neg_i, length)

        if (length(neg) > 0) {
          id <- id[which(neg > 0 && neg == max(neg))]
        } else {
          id <- integer()
        }

        if (length(id) > 0) {
          terms$selected_term[terms$id == id] <<- TRUE
        } else {
          rep <<- FALSE
        }
      })
    }

    terms[terms$selected_term, ]
  }) %>%
    bind_rows() %>%
    select(-id, -selected_term)
}

#' Remove redundant rules and rule components
#'
#' Removes whole rules or components of the rules whose removal do not decrease
#' the total number of positive matches found by applying the rules in the
#' original data set.
#'
#' The suggested approach is apply this function on the output of
#' [generate_rule_selection_set()] after manual review of the proposed rules.
#'
#' @param ruleset A rule set as generated by [generate_rule_selection_set()].
#' @param target_vec A vector of labels.
#' @param target_data A DTM with a number of rows as the elements in
#'   \code{rules}.
#'
#' @return A data frame with the preserved rules, the redundant terms removed by
#'   the rules, the number of positive and negative (absolute and cumulative)
#'   records identified by the rules in the \code{target_data} and the position
#'   of these records in the \code{target_data}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' candidate_queries <- readRDS(file.path("Sessions", "Session1", "rule_data.rds"))
#'
#' Target <- candidate_queries$DTM$Target
#' SpecificDTM <- candidate_queries$SpecificDTM
#'
#' simplified_rules <- file.path("Sessions", "Session1", "Selected_rules.xlsx") %>%
#'   import_data() %>%
#'   simplify_ruleset(target_vec = Target, target_data = SpecificDTM) %>%
#'   pull(rule)
#' }
simplify_ruleset <- function(ruleset, target_vec, target_data) {

	# Silence CMD CHECK about non standard eval
	cum_neg <- cum_pos <- selected_rule <- rule <- NULL

  remove_redundant_rules <- function(data) {
    indexes <- data$pos_i
    subsets <- c()
    tot_pos <- indexes %>%
      unlist() %>%
      n_distinct()
    for (i in (1:nrow(data))[order(data$score)]) {
      new_tot <- indexes[-i] %>%
        unlist() %>%
        n_distinct()
      if (new_tot == tot_pos) {
        indexes <- indexes[-i]
        subsets <- c(subsets, i)
      }
    }
    add_cumulative(data[-subsets, ])
  }

  ruleset %>%
    ungroup() %>%
    filter(selected_rule %in% TRUE) %>%
    select(-selected_rule) %>%
    mutate(across(rule, stringr::str_squish)) %>%
    remove_redundant_rules() %>%
    summarise(
      local({
        tot_pos <- last(cum_pos)
        tot_neg <- last(cum_neg)

        pos_i <- neg_i <- vector("list", n())
        pos <- neg <- score <- vector("integer", n())

        removed_terms <- rep(NA, n())
        pb <- pbmcapply::progressBar()

        tick <- 0

        for (i in n():1) {
          pieces <- stringr::str_split(rule[i], " & ") %>% unlist()

          neg_ind <- c()

          for (j in 1:length(pieces)) {
            new_rules <- rule

            new_rules[i] <- paste(pieces[-c(j, neg_ind)], collapse = " & ")

            new_rules <- new_rules[new_rules != ""]

            filt_query <- paste0("(", new_rules, ")", collapse = " | ")

            res <- target_vec[with(target_data, eval(str2expression(filt_query)))]

            if (sum(res == "y") >= tot_pos & sum(res == "n") <= tot_neg) {
              neg_ind <- c(neg_ind, j)

              tot_pos <- sum(res == "y")
              tot_neg <- sum(res == "n")
            }
          }

          if (length(neg_ind) > 0) {
            removed_terms[i] <- paste(pieces[neg_ind], collapse = " & ")
            rule[i] <- paste(pieces[-neg_ind], collapse = " & ")
          }

          filt <- with(target_data, eval(str2expression(rule[i])))

          pos_bool <- (target_vec %in% "y" & filt)
          neg_bool <- (target_vec %in% "n" & filt)

          pos[i] <- sum(pos_bool)
          neg[i] <- sum(neg_bool)
          pos_i[[i]] <- which(pos_bool)
          neg_i[[i]] <- which(neg_bool)

          tick <- tick + 1
          utils::setTxtProgressBar(pb, tick / n())
        }

        tibble(
          rule = rule,
          removed_terms,
          pos, neg, score = pos - neg,
          pos_i, neg_i
        )
      })
    ) %>%
    remove_redundant_rules()
}

#' Transform a rule set in a search query
#'
#' The rules produced by [extract_rules()] and modified by the other tools in
#' the framework are in a format ready for using as boolean filtering tools in
#' R, for example by using \code{eval(str2expression(rule)))}. To be used online
#' with citation database they need to be converted in another format.
#'
#' @param rules A vector of rules.
#'
#' @return A string with the rules joined and transformed into a format ready
#'   for online use.
#'
#' @export
#'
#' @examples
#'
#' rules <- c(
#'   'V__network._.patient %in% "1" & V__patient_transport %in% "1" &
#'   V__transmission %in% "1" & V__personnel %in% "0" & V__therapy %in% "0"',
#'   'V__Donker_T %in% "1" & V__bacterium_isolate %in% "0" & V__able %in% "0"',
#'   'V__network._.patient %in% "1" & V__resistant_staphylococcus_aureus %in% "1"
#'   & V__carlo.monte.carlo._.monte %in% "0"'
#' )
#'
#' new_query <- rules_to_query(rules)
#'
#' \dontrun{
#' # Produce a new query from a rule review file:
#'
#' candidate_queries <- readRDS(file.path("Sessions", "Session1", "rule_data.rds"))
#'
#' Target <- candidate_queries$DTM$Target
#' SpecificDTM <- candidate_queries$SpecificDTM
#'
#' simplified_rules <- file.path(
#'   "Sessions", "Session1",
#'   "Selected_rules_reviewed.xlsx"
#' ) %>%
#'   import_data() %>%
#'   simplify_ruleset(target_vec = Target, target_data = SpecificDTM) %>%
#'   pull(rule)
#'
#' new_query <- rules_to_query(simplified_rules)
#'
#' writeLines(rules_to_query, file.path("Sessions", "Session1", "Resulting_query.txt"))
#' }
rules_to_query <- function(rules) {

	# Silence CMD CHECK about non standard eval
	. <- NULL

  stringr::str_remove_all(rules, "\\bV__") %>%
    sapply(function(r) {
      rules <- stringr::str_split(r, " & ") %>%
        unlist() %>%
        sapply(function(x) {
          if (stringr::str_detect(x, "\\.?_\\.?")) {
            x <- stringr::str_replace_all(x, "\\.?_\\.?", " ")
          }

          x <- stringr::str_replace(x, "\\.", " OR ")

          if (stringr::str_detect(x, ' (?![%"])')) paste0("(", x, ")") else x
        })

      rule_lenght <- length(rules)

      neg.rules <- stringr::str_subset(rules, '%in% "0"')

      rules <- stringr::str_subset(rules, '%in% "1"') %>% paste(collapse = " AND ")

      if (length(neg.rules) > 0) {
        rules <- paste(rules, "NOT", paste(neg.rules, collapse = " NOT "))
      }

      rules <- stringr::str_remove_all(rules, ' %in% "[01]"') %>% stringr::str_squish()

      if (rule_lenght > 1) paste0("(", rules, ")") else rules
    }) %>%
    {
      paste0("(", paste(., collapse = " OR "), ")")
    }
}
