#' Describe changes in the record labels after a Classification/Review Iteration
#'
#' Takes an Annotation file with a \code{Rev_prediction_new} column and computes
#' differences in the labelling introduced by the last automatic classification
#' / review.
#'
#' @param Annotations An Annotation data set, as data frame or as a file path to
#'   it.
#'
#' @return A data frame with total reviewed positive and negative labels and
#'   their sum, the new labels added in the \code{Rev_prediction_new} column,
#'   and and summary of all changes in the last iteration, e.g.: \code{n -> y}
#'   (a previously non relevant record becoming relevant) or \code{unlab. -> n}
#'   (an unlabeled record becoming negative).
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#'
#' # Taking the path to an annotation data file; also the actual data frame can
#' # be passed
#' Annotations <- get_session_files("Session1")$Annotations[2]
#'
#' out <- compute_changes(Annotations)
#'
#' # The output is easier to read if changed in a long format:
#' str(out) # the easiest solution for fast inspection of results
#'
#' # To get a long format data frame
#' tidyr::pivot_longer(out, everything(),
#'   names_to = "Indicator", values_to = "Value")
#' }
compute_changes <- function(Annotations) {

	# Silence CMD CHECK about non standard eval
	Target <- . <- Col <- Total_labeled <- New_labels <- NULL

	Annotations <- import_data(Annotations)

	Annotations %>%
		transmute(
			Target = coalesce_labels(cur_data(), c(
				"Rev_prediction_new",
				"Rev_prediction", "Rev_manual"
			)),
			Change = paste(
				coalesce_labels(cur_data(), c("Rev_prediction", "Rev_manual")),
				Target,
				sep = " -> "
			) %>% stringr::str_replace_all("NA", "unlab.")
		) %>%
		{
			df <- .
			lapply(names(df), function(col) {
				df %>%
					transmute(Col = get(col) %>% factor()) %>%
					filter(!is.na(Col)) %>%
					count(Col) %>%
					tidyr::pivot_wider(
						names_from = Col, values_from = n,
						names_prefix = paste0(col, ": ")
					)
			}) %>%
				bind_cols() %>%
				mutate(
					Total_labeled = sum(!is.na(df$Target)),
					New_labels = if ("Rev_prediction_new" %in% names(Annotations)) {
						sum(!is.na(Annotations$Rev_prediction_new))
					} else {
						NA
					},
					across(c(Total_labeled, New_labels), ~ if (!is.na(.x)) {
						x <- .x # Some changes in glue or dplyr made glue not recognizing .x anymore
						glue("{x} ({percent(x/nrow(Annotations))})") %>% as.character()
					} else {
						NA
					}),
					.after = matches("Target")
				)
		}
}

#' Train a simple Bayesian logistic model on classification results
#'
#' The function uses an Annotation data set to train a Bayesian logistic model
#' that estimates the probability of a relevant record given the lower
#' boundaries of the PPD produced by the classification model for the records
#' whose label was manually reviewed.
#'
#' The produced model can be used to predict the distribution of the cumulative
#' number of total unseen positive records.
#'
#' Usually this function is not to be used directly but through
#' [estimate_performance()].
#'
#' @param train_data An Annotation data set with predictions produced by
#'   [enrich_annotation_file()].
#' @param seed An integer to replicate results
#'
#' @return An object of class \code{brmsfit}. See [brms::brm()] for mode info.
#'
#' @export
#'
estimate_positivity_rate_model <- function(train_data, seed = 14129189) {

	train_data$Target <- factor(coalesce_labels(train_data))

	# TODO: switch to rstanarm to skip compilation
	brms::brm(brms::brmsformula(Target ~ Pred_Low),
						family = brms::bernoulli,
						data = train_data,
						cores = 8, chains = 8, refresh = 0, iter = 8000, control = list(adapt_delta = .95),
						backend = "cmdstan", seed = seed,
						prior = c(
							brms::set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
							brms::set_prior("student_t(3, 0, 1.5)", class = "b")
						)
	)
}

#' Evaluate and report classification performance of given an Annotation file
#'
#' This function estimate Sensitivity and Efficiency (the latter as "Work saved
#' over random classification", WSoR) of the classification process (i.e., both
#' the automatic classification and the human). A robust estimate of the total
#' number of relevant (positive) records in the whole data set is produced to
#' compute these statistics.
#'
#' For this purpose, [estimate_positivity_rate_model()] is employed, which uses
#' a Bayesian logistic model to estimate the probability of a relevant record
#' given the lower boundaries of the PPD produced by the classification model
#' for the records whose label was manually reviewed. This model does not take
#' into account records' other characteristics, providing a simple, maximum
#' uncertainty model.
#'
#' The model is used to predict the distribution of the number of missed
#' relevant matches among the unreviewed records. This number is then used to
#' compute the expected Sensitivity (i.e., the ratio of observed positive
#' matches and the theoretical ones) and Efficiency (i.e. ratio of the number of
#' reviewed records and the number of records needed to review at random to find
#' the same amount of relevant matches, according to the hypergeometric
#' distribution).
#'
#' Finally, several summary statistics are reported, describing the observed
#' results of the classification (i.e., number of reviewed records, number of
#' positives found) and the statistics computed using the surrogate logistic
#' model (i.e., Sensitivity, Efficiency and the R^2 of the surrogate model),
#' including their uncertainty intervals.
#'
#' Optionally, a plot showing the observed cumulative number of positive matches
#' plus its posterior predictive distribution according to the surrogate model.
#'
#'
#' @param records An Annotation data set produced by [enrich_annotation_file()]
#'   or a file path to it.
#' @param model A \code{brm} model built using
#'   [estimate_positivity_rate_model()]. Will be created from \code{records} if
#'   \code{NULL}.
#' @param preds A matrix of posterior predictions as produced by
#'   [brms::posterior_predict()]. If passed they need to be derived by the same
#'   model in \code{model}.
#' @param plot Whether to plot the cumulative number of positive matches plus
#'   the posterior predictive distribution as computed by \code{model},
#'   truncated at the number of observed ones.
#' @param quants Point estimate and boundaries of the posterior distributions to
#'   use in the results and in the plot.
#' @param nsamples Number of samples to use to build the posterior distribution,
#'   lower bounded at the number used to fit the \code{model}.
#' @param seed A seed to reproduce the results.
#' @param save_preds Whether to save the posterior prediction matrix. Can be
#'   passed to \code{preds}.
#' @param save_model Whether to save the model. Can be passed to \code{model}
#'
#' @return A data frame with the following columns: \item{obs_positives}{the
#'   observed number of positive matches;}\item{pred_positives}{the quantiles of
#'   the predicted distribution of the number of positive matches.}
#'   \item{mod_r2}{the surrogate model fit (\eqn{R^2}).}\item{n_reviewed}{the
#'   number of records reviewed.}\item{total_records}{the total records in the
#'   Annotation file;}\item{used_prop}{the posterior distribution of the
#'   proportion of reviewed record over the amount needed with random
#'   classification (1  - WSoR).}\item{efficiency}{the posterior distribution of
#'   one minus the proportion of reviewed record over the amount needed with
#'   random classification (WSoR).}\item{Sensitivity}{the posterior distribution
#'   of the Sensitivity computed over the predicted number of positives
#'   according to the surrogate model.}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' annotation_file <- get_session_files("Session1")$Annotations %>% last()
#'
#' analysis <- estimate_performance(annotation_file)
#' }
#'
estimate_performance <- function(records, model = NULL, preds = NULL, plot = TRUE,
																 quants = getOption("baysren.probs"),
																 nsamples = min(2500, sum(model$fit@sim$n_save)),
																 seed = 23797297,
																 save_preds = FALSE, save_model = FALSE) {

	# Silence CMD CHECK about non standard eval
	Order <- Target <- L <- M <- U <- NULL

	records <- import_data(records)

	if (is.null(model)) {
		message("- build model...")

		model <- estimate_positivity_rate_model(records, seed)
	}

	quants <- sort(quants)

	records <- arrange(records, Order)

	tot_records <- nrow(records)

	if (is.null(preds)) {
		message("- computing predictions...")
		set.seed(seed)
		preds <- brms::posterior_predict(model, newdata = records, nsamples = nsamples)
	}

	message("- integrate with observed data...")
	vec <- as.numeric(records$Target == "y")
	# pb <- progressBar()

	for (i in which(!is.na(records$Target))) {
		preds[, i] <- vec[i]
		# setTxtProgressBar(pb, i / length(vec))
	}
	# close(pb)

	n_sims <- nrow(preds)
	obs_pos <- sum(records$Target %in% "y")
	tot_reviewed <- sum(!is.na(records$Target))
	tot_pos <- pmax(preds %>% rowSums(), obs_pos)
	tot_neg <- tot_records - tot_pos

	n_needed <- extraDistr::rnhyper(n_sims, tot_neg, tot_pos, obs_pos)

	res <- list(
		obs_positives = obs_pos,
		pred_positives = quantile(tot_pos, quants),
		mod_r2 = brms::bayes_R2(model, probs = quants)[3:5],
		n_reviewed = tot_reviewed,
		total_records = tot_records,
		used_prop = quantile(tot_reviewed / n_needed, quants),
		efficiency = quantile(1 - tot_reviewed / n_needed, quants),
		sensitivity = quantile(obs_pos / tot_pos, quants)
	) %>%
		sapply(function(el) { # if quantile, sort them
			if (length(el) == 3) {
				sort(el) %>% setNames(percent(quants))
			} else {
				el
			}
		})

	if (save_preds) {
		res$preds <- preds
	}

	if (save_model) {
		res$model <- model
	}

	if (plot) {
		## TODO: move this into its own function in Reporting.R

		cl <- suppressWarnings(parallel::makeCluster(getOption("mc.cores")))
		on.exit(parallel::stopCluster(cl))
		message("- computing cumulative trends...")

		preds <- preds %>% parallel::parApply(1, cumsum, cl = cl)

		message("- extracting cumulative quantiles...")

		preds <- parallel::parApply(preds, 1, quantile, quants, cl = cl) %>%
			t() %>%
			as.data.frame() %>%
			setNames(c("L", "M", "U"))

		res$plot <- preds %>%
			mutate(
				Target = cumsum(records$Target %in% "y"),
				across(c(Target, L, M, U), ~ tapply(.x, .x, function(x) c(x[1], rep(NA, length(x) - 1))) %>% unlist()),
				max = coalesce(Target, L, M, U),
				Order = 1:n()
			) %>%
			filter(!is.na(max)) %>%
			mutate(
				across(c(L, M, U), ~ sapply(1:length(.x), function(i) {
					if (is.na(.x[i])) max(.x[1:i], na.rm = T) else .x[i]
				}))
			) %>%
			ggplot(aes(Order)) +
			geom_ribbon(aes(ymin = L, ymax = U, color = glue("{diff(range(quants)) * 100}% Predictive Interval")), alpha = .1) +
			geom_line(aes(y = M, color = "Predictive Median")) +
			geom_point(aes(y = Target), color = "darkred") +
			theme_minimal() +
			scale_color_manual(values = c("steelblue", "blue")) +
			guides(alpha = "none") +
			labs(x = "Records", y = "Cum. positive matches", color = "")

		# print(res$plot)
	}

	res
}

#' Extract the importance of features in the Document Term Matrix
#'
#' Inside [enrich_annotation_file()], the feature relevance for the
#' classification model is estimated from the Document Term Matrix (DTM) and
#' stored in the Annotation file. In the case of the default BART model, the
#' feature importance is the rate of posterior trees in which a term was used,
#' plus its Z score if an ensemble of models is used.
#'
#' In addition to the model derived scores, the variable importance according to
#' a Poisson regression is used to estimate the association (as log-linear
#' regressor and Z score) of a term with relevant records. This approach is
#' helpful to distinguish between terms being relevant by themselves (both the
#' model related and the linear Z scores are high) or in association with other
#' terms (only the model Z score is high).
#'
#' @param session_name The name of a session.
#' @param num_vars The number of best features to report, according to model
#'   importance.
#' @param score_filter The model related Z score can be used to filter less
#'   relevant features.
#' @param recompute_DTM Whether to recompute the DTM.
#' @param sessions_folder The folder in which all sessions are stored.
#'
#' @return A data frame with the features (and the part of the record they are
#'   related to), the model importance score and its Z score (if an ensemble of
#'   models is used), the log-linear association according to the Poisson model
#'   and the linear Z score.
#'
#'
#' @export
#'
#' @importFrom stats glm poisson
#'
#' @examples
#' \dontrun{
#' extract_var_imp("Session1")
#' }
extract_var_imp <- function(session_name, num_vars = 15, score_filter = 1.5, recompute_DTM = FALSE,
														sessions_folder = getOption("baysren.sessions_folder")) {

	# Silence CMD CHECK about non standard eval
	. <- ID <- Target <- Term <- Score <- Value <- std.error <- p.value <- NULL

	message("Retrieving data")
	session_files <- get_session_files(session_name, sessions_folder)

	Records <- last(session_files$Annotations) %>%
		import_data() %>%
		mutate(Target = coalesce_labels(.) %>% tidyr::replace_na("n")) %>%
		select(ID, Target)

	Variables <- last(session_files$Annotations) %>%
		import_data(sheet = "Variable_importance") %>%
		filter(!stringr::str_detect(Term, "^\\w+\\.count"), Score > score_filter) %>%
		arrange(desc(Value)) %>%
		head(num_vars)

	if (recompute_DTM) {
		DTM <- create_training_set(Records)
	} else {
		DTM <- import_data(session_files$DTM)
	}

	DTM$Target <- as.numeric(Records[match(DTM$ID, Records$ID), ]$Target == "y")

	message("Computing variable importance")
	pbmcapply::pbmclapply(1:nrow(Variables), function(i) {
		Term_data <- Variables[i, ]
		term <- Term_data$Term

		data.frame(
			Term_data,
			glm(Target ~ get(term), poisson(), DTM) %>% broom::tidy() %>%
				tail(-1) %>% select(-term, -std.error, -p.value)
		)
	}) %>% bind_rows()
}

#' Analyse the results of a parameter grid search
#'
#' Takes as input a folder with multiple session data produced by
#' [perform_grid_evaluation()], each session representing a combination of
#' parameters and computes the best combinations in terms of a perfomance score.
#'
#' For each session a performance score is computed. The default is
#' \eqn{\text{sensitivity} \times \text{efficiency}}{sensitivity x efficiency},
#' with \eqn{efficiency} being the \emph{one minus the ratio of reviewed records
#' over the total}. The statistics are computed on a subset of fully labeled
#' records.
#'
#' The analysis is performed on each session's "Results" files.
#'
#' A partition tree algorithm is used to group parameter combinations by average
#' scores, identifying "performance clusters". The combination with the best
#' sensitivity followed by the best efficiency is then shown for each cluster.
#' Optionally, a plot can be generated with the marginal impact of each
#' parameter.
#'
#' @param session_folder Where to find the result sessions produced by
#'   [perform_grid_evaluation()].
#' @param tot_pos Total number of positive matches among records. If \code{NULL}
#'   it will be inferred by the Annotation files in \code{session_folder} which
#'   then need to be fully labelled.
#' @param tot_records Total number of records. If \code{NULL} it will be
#'   inferred by the Annotation files in \code{session_folder} which then need
#'   to be fully labelled.
#' @param plot Whether to plot the marginal impact of each parameter.
#' @param score Which one of the scores to use to measure classification
#'   performance. Can be \code{Sens_adj_eff}: sensitivity by efficacy,
#'   \code{Pos_rate}: the ratio of positive labels found over the total record
#'   (positive rate), \code{Pos_rate_adj_sens}: sensitivity by positive rate.
#'
#' @return A list with: \item{iterations}{A data frame describing each
#'   classification/review iteration for each session, reporting the parameter
#'   values used and the performance score.}\item{best_parms}{The highest
#'   performance parameter set in the best parameter cluster. First, the
#'   parameter clusters are ordered by average performance score, then parameter
#'   combinations inside the cluster are ordered by sensitivity followed by
#'   efficiency. A data frame with the score, the ratio of positive matches over
#'   the total records, the ratio of reviewed records, the sensitivity and the
#'   efficiency, is reported.}\item{best_by_rule}{A data frame with performance
#'   info (like for \code{best_parms}) for the best parameter set for each
#'   performance cluster.}\item{plot}{A plot with the marginal impact of each
#'   parameter if the \code{plot} argument is \code{TRUE}, otherwise
#'   \code{NULL}.}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' out <- analyse_grid_search()
#'
#' # check the best parameter set (use str() for easy to read output)
#' str(out$best_parms)
#'
#' # check the best set for each performance cluster
#' View(out$best_by_rule)
#'
#' # plot parameter marginal impact is the plot argument is TRUE
#' out$plot
#' }
#'
analyse_grid_search <- function(session_folder = "Grid_Search", tot_pos = NULL,
																tot_records = NULL, plot = TRUE,
																score = c("Sens_adj_eff", "Pos_rate", "Pos_rate_adj_sens")) {

	# Silence CMD CHECK about non standard eval
	. <- Target <- Indicator <- Value <- Iter <- `Replication n.` <- Total_labeled <- `Target: y` <- Session <- Tot_labeled <- Pos_labels <- Pos_rate <- Sensitivity <- Efficiency <- Score <- rule <- estimate <- Score <- group <- Rule <- Rule <- Iter <- Rep <- Cluster <- NULL

	score <- match.arg(score)

	if (is.null(tot_pos) | is.null(tot_records)) {
		Labels <- list.files(session_folder, pattern = "Records_", recursive = T, full.names = T)[1] %>% # TODO: use get_session_files()
			import_data() %>%
			mutate(
				Target = coalesce_labels(., label_cols = c(
					"Rev_prediction_new",
					"Rev_prediction", "Rev_manual",
					"Rev_previous"
				))
			) %>%
			with(table(Target)) %>%
			as.list()


		if (is.null(tot_pos)) tot_pos <- Labels$y
		if (is.null(tot_records)) tot_records <- sum(unlist(Labels))
	}

	out <- list.files(session_folder, pattern = "Results_", recursive = T, full.names = T) %>% # TODO: use get_session_files()
		pbmcapply::pbmclapply(function(file) {
			import_data(file) %>%
				tidyr::pivot_wider(names_from = Indicator, values_from = Value) %>%
				transmute(
					Iter,
					Session = file,
					Rep = `Replication n.`,
					Tot_labeled = Total_labeled,
					Pos_labels = `Target: y`
				)
		}) %>%
		bind_rows() %>%
		mutate(
			Session = stringr::str_remove(Session, "Results.*") %>% basename(),
			Tot_labeled = stringr::str_remove(Tot_labeled, " \\(.*"),
			Session %>% stringr::str_remove(".*GridSession\\.") %>%
				stringr::str_split("\\.", simplify = T) %>%
				as.data.frame() %>%
				lapply(function(piece) {
					label <- stringr::str_remove(piece, "^(\\d+|[yn])")[1]
					value <- stringr::str_remove(piece, stringr::fixed(label))

					setNames(data.frame(value), label)
				}) %>% bind_cols(),
			across(one_of(c(
				"Tot_labeled", "Pos_labels", "Mods", "Quant", "Init",
				"Mult"
			)), as.numeric),
			Pos_rate = Pos_labels / Tot_labeled,
			Sensitivity = Pos_labels / tot_pos,
			Efficiency = 1 - Tot_labeled / tot_records,
			Pos_rate_adj_sens = Pos_rate * Sensitivity,
			Sens_adj_eff = Sensitivity * Efficiency,
			Score = get(score)
		) %>%
		group_by(Session) %>%
		slice_tail(n = 1) %>%
		ungroup()

	params <- c("Mods", "Quant", "Resamp", "Init", "Mult")

	tree <- out %>%
		select(Score, one_of(c("Mods", "Quant", "Resamp", "Init", "Mult"))) %>%
		mutate_at(vars(-Score), as.factor) %>%
		{
			df <- .
			rpart::rpart(Score ~ ., df)
		}

	rules <- tidytrees::tidy_tree(tree)[tree$where - 1, ] %>%
		mutate(
			rule = sprintf(
				"%s. %s (%.2g)",
				as.numeric(factor(rule, unique(rule[order(estimate, decreasing = T)]))),
				rule, estimate
			),
			rule = factor(rule, unique(rule[order(estimate, decreasing = T)]))
		)

	out <- mutate(out, Rule = rules$rule)

	p <- NULL

	if (plot) {
		if (R.version$major >= 4) {
			f <- grDevices::palette()[1:7] %>% grDevices::colorRampPalette() #TODO: remove dependency on grDevices and viridis somehow
			colors <- f(n_distinct(out$Rule)) %>% rev()
		} else {
			colors <- viridis::cividis(n_distinct(out$Rule))
		}

		p <- lapply(params, function(par) {
			params <- setdiff(params, par)

			out %>%
				mutate(group = select(cur_data(), one_of(params)) %>%
												apply(1, paste, collapse = " ")) %>%
				ggplot(aes(factor(get(par)), Score)) +
				geom_boxplot(show.legend = F, outlier.shape = NA) +
				geom_line(aes(group = group, color = Rule), alpha = .5, show.legend = F) +
				geom_boxplot(aes(fill = Rule), show.legend = F, outlier.shape = NA) +
				# geom_line(aes(group = group, color = Rule), alpha = .5, show.legend = F) +
				geom_point(aes(color = Rule), show.legend = F) +
				theme_minimal() +
				scale_y_continuous(labels = function(x) round(x, 2)) +
				scale_color_manual(values = colors) +
				scale_fill_manual(values = colors) +
				ylab(score) +
				xlab(par)
		})

		p_one <- out %>%
			ggplot(aes(factor(get(params[1])), Score)) +
			geom_line(aes(group = Rule, color = Rule), size = 2) +
			theme_minimal() +
			scale_color_manual(values = colors) +
			scale_fill_manual(values = colors) +
			labs(color = glue("Perf. cluster"))

		tmp <- ggplot_gtable(ggplot_build(p_one))
		leg <- which(tmp$layout$name == "guide-box")

		legend <- tmp$grobs[[leg]]

		p <- patchwork::wrap_plots(p) + legend

		# print(p)
	}

	list(
		iterations = out,
		best_parms = out %>% filter(stringr::str_detect(Rule, "^1\\.")) %>%
			arrange(desc(Sensitivity), desc(Efficiency)) %>% head(1) %>%
			select(
				Iter, Rep, Pos_labels, Sensitivity, Tot_labeled, Efficiency,
				Score, any_of(params)
			) %>%
			mutate(
				Score = glue("{signif(Score, 3)} ({score})"),
				Pos_labels = glue("{Pos_labels} / {tot_pos}"),
				Sensitivity = percent(Sensitivity),
				Tot_labeled = glue("{Tot_labeled} / {tot_records}"),
				Efficiency = percent(Efficiency),
				across(.fns = as.character)
			) %>%
			tidyr::pivot_longer(everything(), names_to = "Parameter", "Value"),
		best_by_rule = out %>% group_by(Cluster = Rule) %>%
			arrange(desc(Sensitivity), desc(Efficiency)) %>% slice_head(n = 1) %>%
			select(
				Cluster, Iter, Pos_labels, Tot_labeled, Sensitivity, Efficiency,
				Score, any_of(params)
			) %>%
			mutate(
				Pos_labels = glue("{Pos_labels} / {tot_pos}"),
				Tot_labeled = glue("{Tot_labeled} / {tot_records}"),
				Sensitivity = percent(Sensitivity),
				Efficiency = percent(Efficiency),
				Score = glue("{signif(Score, 3)} ({score})"),
				across(.fns = as.character)
			),
		plot = p
	)
}
