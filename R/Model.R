
#' Join classification labels into one
#'
#' In the same Annotation file, there could be multiple classification labels
#' for a record derived by different phases of the labelling procedure. This
#' function joins them hierarchically, using the label from the lower
#' hierarchies to fill up \code{NA}s in the upper ones. The hierarchy is
#' considered from left to right.
#'
#' This function is a wrapper over [dplyr::coalesce()] which allow to have a
#' standard coalescing scheme for an annotation file.
#'
#' @param data An Annotation data frame.
#' @param label_cols The label columns to coalesce. They are considered
#'   hierarchically from left to right, with a lower level column being used if
#'   a value is missing in the upper level one.
#'
#' @return A vector with the coalesce labels.
#'
coalesce_labels <- function(data, label_cols = c("Rev_prediction_new", "Rev_prediction", "Rev_manual")) {
  # TODO: add a check to avoid non-label columns as inputs
  coalesce(!!!select(data, any_of(label_cols)))
}


#' Create the training set for the automatic classification
#'
#' Given an Annotation file, it creates and join Document Term Matrices (DTM)
#' for the title, abstract, authors, keywords and MESH terms of a record,
#' keeping terms above a given document frequency among the negative labelled
#' records and which are present in at least two records
#'
#' @param Records An Annotation data frame.
#' @param min_freq Minimum document frequency (between 0 and 1) in negative
#'   labelled records above which a term is considered.
#'
#' @return An enriched DTM which is the merge of the title, abstract, authors,
#'   keywords and MESH terms DTMs plus the record ID and label if present.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' Records <- import_data(get_session_files("Session1")$Records)
#'
#' DMT <- create_training_set(Records)
#' }
create_training_set <- function(Records, min_freq = 0.05) {

  # Silence CMD CHECK about non standard eval
  . <- ID <- Title <- Abstract <- Authors <- Keywords <- Mesh <- NULL

  if (min_freq <= 0 | min_freq > 1) stop('"min_freq" should be between 0 and 1.')

  Records <- import_data(Records) %>%
    transmute(
      Target = coalesce_labels(.),
      ID, Title, Abstract, Authors, Keywords, Mesh
    )

  # Compute minimum term frequency among documents
  min_freq <- max(floor(sum(Records$Target %in% c("y", "n")) * min_freq), 1)

  # Ensure that all positive document terms are kept
  Records <- Records[c(
    rep(which(Records$Target %in% "y"), min_freq),
    which(Records$Target %nin% "y")
  ), ]

  if (all(is.na(Records$Target))) {
    stop("There are no labelled records")
  }

  message("(min term freq in negatives: ", min_freq, ")")

  message("Title DTM")
  Title_DTM <- with(
    Records,
    text_to_DTM(Title,
      min.freq = min_freq, label = "TITLE__", ids = ID,
      freq.subset.ids = ID[Target %in% c("y", "n")]
    )
  )

  message("dimensions: ", paste(dim(Title_DTM), collapse = ", "))

  message("\nAbstract DTM")
  Abstract_DTM <- with(
    Records,
    Abstract %>%
      stringr::str_remove_all(stringr::regex("\\b(background|introduction|method\\w*|result\\w*|conclusion\\w*|discussion)", ignore_case = TRUE)) %>%
      text_to_DTM(
        min.freq = min_freq, label = "ABSTR__", ids = ID,
        freq.subset.ids = ID[Target %in% c("y", "n")]
      )
  )

  message("dimensions: ", paste(dim(Abstract_DTM), collapse = ", "))

  message("\nAuthors DTM")
  Authors_DTM <- with(
    Records,
    text_to_DTM(Authors,
      tokenize.fun = tokenize_authors, min.freq = min_freq,
      label = "AUTH__", ids = ID, freq.subset.ids = ID[Target %in% c("y", "n")],
      add.ngrams = FALSE, aggr.synonyms = FALSE
    )
  )

  message("dimensions: ", paste(dim(Authors_DTM), collapse = ", "))

  message("\nKeywords DTM")
  Keywords_DTM <- with(
    Records,
    text_to_DTM(Keywords,
      tokenize.fun = tokenize_keywords, min.freq = min_freq,
      label = "KEYS__", ids = ID,
      freq.subset.ids = ID[Target %in% c("y", "n")]
    )
  )

  message("dimensions: ", paste(dim(Keywords_DTM), collapse = ", "))

  message("\nMesh DTM")
  Mesh_DTM <- with(
    Records,
    text_to_DTM(Mesh,
      tokenize.fun = tokenize_MESH, min.freq = min_freq,
      label = "MESH__", ids = ID, freq.subset.ids = ID[Target %in% c("y", "n")],
      add.ngrams = FALSE
    )
  )

  message("dimensions: ", paste(dim(Mesh_DTM), collapse = ", "))

  DTM <- data.frame(
    Target = Records$Target,
    Title_DTM,
    Abstract_DTM[, -1],
    Authors_DTM[, -1],
    Keywords_DTM[, -1],
    Mesh_DTM[, -1]
  ) %>%
    distinct() %>% # remove the duplicated positive matches
    select(
      tidyselect::vars_select_helpers$where(~ !is.numeric(.x)), # Keep ID and Target
      tidyselect::vars_select_helpers$where(~ suppressWarnings(sum(as.numeric(.x), na.rm = TRUE)) > 1) # Keep features with more than one match in a document
    )
}

#' Use Bayesian Additive Regression Trees to predict records labels
#'
#' The prediction engine of the framework. It produces a model which assigns a
#' probability distribution for each record of the probability of being relevant
#' (i.e., positive label). It is not used alone but inside
#' [enrich_annotation_file()].
#'
#' This implementation is built over [bartMachine::bartMachine()].
#'
#' @param train_data A Document Term Matrix with an outcome column. if
#'   \code{pred} is null, all column apart from \code{Y} will be considered as
#'   features.
#' @param Y The name of the outcome column in the data set.
#' @param preds A vector of column names to use as features.
#' @param save Whether to save the model to disk, with a file name given in
#'   \code{name}, to be placed into \code{folder}.
#' @param folder Where to look for/create the model backup file.
#' @param name The name of the model backup file. If the file exists and
#'   \code{rebuilt} is \code{FALSE}, the model will not be retrained but the
#'   backup will be used instead.
#' @param rebuild If \code{TRUE}, retrain the model even if a model backup file
#'   exists.
#' @param num_trees,k,num_iterations_after_burn_in,run_in_sample,mem_cache_for_speed,use_missing_data,verbose
#' [bartMachine::bartMachine()] specific parameters.
#' @param ... More argument to pass to [bartMachine::bartMachine()].
#'
#' @return An object of class \code{bartMachine}.
#'
#' @export
#'
compute_BART_model <- function(train_data, Y, preds = NULL, save = FALSE,
                               folder = getwd(), name = as.character(Y),
                               rebuild = FALSE, num_trees = 50, k = 2,
                               num_iterations_after_burn_in = 2000,
                               run_in_sample = FALSE, mem_cache_for_speed = TRUE,
                               use_missing_data = TRUE, verbose = TRUE, ...) {
  check_suggested_packages(c("rJava", "bartMachine"),
    is_required_msg = "to run the model",
    stop_on_rejection = TRUE, on_rejection_msg = "The model cannot be run without the {pkg} package."
  )

  get_java_memory <- function() {
    rJava::.jcall(rJava::.jnew("java/lang/Runtime"), "J", "maxMemory") / 1e9
  }

  is_java_on <- function() {
    tryCatch(rJava::.jcheck(silent = TRUE) == 0, error = function(e) FALSE)
  }

  if (is.null(getOption("baysren.BartMem")) & is_java_on()) {
    warning(
      paste("The BART model is running using", get_java_memory(), "GB of memory.\nIf you experience memory problems, consider restarting the R session and assign more memory (in GBs) using the 'baysren.BartMem' R option."),
      call. = FALSE, immediate. = TRUE
    )
  }

  # if (is.null(getOption("baysren.BartMem"))) {
  # 	if (is_java_on()) {
  # 		warning(
  # 			paste("No extra memory was assign to Java using 'getOption(\"baysren.BartMem\")'.\nIf you experience memory problems, consider restarting the R session before running the model and increasing the memory."),
  # 			call. = FALSE, immediate. = TRUE)
  # 	} else {
  # 		if (interactive()) {
  # 			mem <- readline("How much GBs of memory should be used by the BART model?\n(better no more than 90% of available RAM)\n ")
  #
  # 			if (is.na(as.numeric(mem))) stop('Input should be a number.')
  #
  # 			options(baysren.BartMem = mem)
  #
  # 			mem <- paste0("-Xmx", mem, "g")
  #
  # 			options(java.parameters = mem)
  #
  # 		} else {
  # 			warning(
  # 				"Java is using the default amount of memory to run the model. If you experience memory problems consider assign more memory (in GBs) using 'getOption(\"baysren.BartMem\")'.",
  # 				call. = FALSE, immediate. = TRUE)
  # 		}
  #
  # 		rJava::.jpackage('bartMachine')
  # 		rJava::.jpackage('bartMachineJARs')
  # 	}
  # }
  #
  # print(paste("BART machine is running with", get_java_memory(), 'GB of memory'))

  if (is.null(getOption("baysren.BartCores"))) {
    if (interactive()) {
      cores <- readline(glue::glue("How many CPU cores should be used by the BART model?\n(you have {parallel::detectCores()} cores available, use less cores if experience degraded performance using the OS while running the model.)\n "))

      if (is.na(as.numeric(cores))) stop("Input should be a number.")
    } else {
      cores <- parallel::detectCores() - 1
    }

    options(baysren.BartCores = min(cores, parallel::detectCores()))
    bartMachine::set_bart_machine_num_cores(cores)
  }

  # options(java.parameters = getOption('BartMem'))

  if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)

  model_file <- file.path(folder, glue("{name}.rds"))

  if (is.null(preds)) preds <- colnames(train_data)
  preds <- setdiff(preds, Y)

  if (!file.exists(model_file) | rebuild) {
    train_data <- train_data %>%
      filter(across(all_of(Y), ~ !is.na(.x))) %>%
      mutate(across(all_of(Y), ~ if (!is.numeric(.x)) {
        if (is.factor(.x)) {
          stats::relevel(.x, ref = levels(.x)[2])
        } else {
          factor(.x) %>% stats::relevel(ref = as.character(sort(unique(.x))[2]))
        }
      } else {
        .x
      }))

    X <- train_data %>%
      select(all_of(preds)) %>%
      as.data.frame()
    Y <- train_data[[Y]]

    message("Building BART model...")
    model <- bartMachine::bartMachine(
      X = X, y = Y,
      verbose = verbose,
      num_trees = num_trees,
      k = k,
      num_iterations_after_burn_in = num_iterations_after_burn_in,
      run_in_sample = run_in_sample,
      mem_cache_for_speed = mem_cache_for_speed,
      use_missing_data = use_missing_data, serialize = save, ...
    )

    if (save) {
      readr::write_rds(model, model_file, compress = "gz")
    }
  } else {
    message("Loading BART model...")
    model <- readr::read_rds(model_file)
  }

  model
}

#' Enrich an Annotation data set with predictions based on a Bayesian
#' classification model
#'
#' This function is the central component of the framework. It takes as input a
#' session name or the path to an Annotation data frame and trains a Bayesian
#' model to predict the labels of all records.
#'
#'
#' The outcome on which the model is trained on a coalesced column \code{Target}
#' made by the initial manual labels in \code{Rev_manual} and previous predicted
#' labels in \code{Rev_prediction} that were manually reviewed. The labels can
#' be only positive (\code{y}) or negative (\code{n}).
#'
#' The output is another Annotation data frame with an extra column
#' \code{Predicted_label} which contains the labels predicted by the model (see
#' later). Also a \code{Rev_prediction_new} column is created, which contains
#' predictions that require manual review, and \code{Rev_prediction} which
#' stores the previously reviewed predictions.
#'
#' Based on the paradigm of "Active Learning", records in the
#' \code{Rev_prediction_new} column will have a "*" label, which means that they
#' need manual review. The user will need to change the "*" into a positive
#' \code{y} or negative \code{n} label.
#'
#' After the uncertain records in the output Annotation file are reviewed, the
#' function can be called again, establishing a Classification/Review (CR)
#' cycle. If no new positive record labels are found, the function should be
#' still called a number of times (defined in the \code{limits$stop_after}
#' argument) to produce "replication" CR iterations. These are needed to avoid
#' missing uncertain record due to the stochasticity in the Bayesian model.
#'
#' The Bayesian model generates posterior predictive distributions (PPD) of
#' positive label for each record (saved into Samples in the session folder).
#' The PPDs are used to define the "Uncertainty Zone", which is delimited by the
#' lowermost of all PPDs' credibility intervals boundaries at a chosen level for
#' the positive labelled records and the uppermost of the interval boundaries
#' among the manually reviewed negative labelled records. A record is labelled
#' as positive/negative in the \code{Predicted_label} column if both its
#' uncertainty interval boundaries are above/below the Uncertainty Zone.
#' Otherwise, it will be labelled as uncertain and will require manual review.
#' If a prediction in \code{Predicted_label} contradicts one in
#' \code{Rev_manual}, will need to be reviewed by the user.
#'
#' If just a session name is passed, the function will identify automatically
#' the last Annotation file to use and if its a new CR iteration or a
#' replication and will label the output file accordingly.
#'
#' The function take as arguments a number of parameters which may have an
#' impact on the final CR cycle sensitivity and efficiency (fraction of total
#' manually reviewed records): \code{pos_mult}, \code{n_models},
#' \code{resample}, \code{pred_quants}. Default values are proposed for these
#' which should work in most cases; [perform_grid_evaluation()] can be used to
#' evaluate the best parameter combination for a new data set if the user
#' believes that there is margin of improvement, but the evaluation of the grid
#' is extremely computationally intensive.
#'
#' The function saves a number of output files on disk.
#'
#'
#' @param session_name A session name which also identifies a subfolder of
#'   \code{sessions_folder}. The function will automatically retrieve the out of
#'   the last CR iteration to continue the cycle if the conditions in
#'   \code{limits} are not fulfilled.
#' @param file In alternative to \code{session_name}, a direct path to an
#'   Annotation file can be used.
#' @param DTM A path to a Document Term Matrix (DTM) as produced by
#'   [create_training_set()]. If \code{NULL} two conditions can happen: if the
#'   current iteration is a replication and an existing DTM is present into the
#'   \code{session_name} folder, it will be used; if the CR iteration is not a
#'   replication or no backup DTM exists a new one will be generated from the
#'   Annotation data.
#' @param pos_mult A model parameter. Defines the oversampling rate of positive
#'   records before training. A higher number increases sensitivity at the cost
#'   of lower efficiency (more records to manually review) and training times.
#' @param n_models A model parameter. The Bayesian model is run multiple times
#'   and the multiple generated PPDs are averaged, creating an ensemble PPD. A
#'   higher number of models decrease uncertainty and increases efficiency, but
#'   greatly increases computation times.
#' @param resample A model parameter. Whether to bootstrap the training data
#'   before modelling. It makes sense only if \code{n_models >> 1}, otherwise it
#'   equates to loose data.
#' @param pred_quants A model parameter. The levels of the PPD uncertainty
#'   intervals used to build the Uncertainty Zone. A larger uncertainty interval
#'   increases sensitivity but decreases efficiency. The middle level is only
#'   use to provide a descriptive point estimate for the record PPDs, it is not
#'   used to define the record labels.
#' @param sessions_folder The path to the folder where all the sessions are
#'   stored.
#' @param pred_batch_size Since creating the PPD has a big memory burden,
#'   records are separated in batches before computing predictions. Decrease
#'   this number if the function crashes due to Java memory problems.
#' @param autorun If no unreviewed uncertain are present, start a new CR
#'   iteration automatically.
#' @param stop_on_unreviewed Raise an error if there are uncertain records that
#'   have not been yet manually reviewed (i.e. "*" in the
#'   \code{Rev_prediction_new} column). It should be set to \code{FALSE} if the
#'   number of records to manually review is too high. In general we suggest
#'   reviewing manually no more than 250 records per iteration, since this
#'   number is usually enough to provide enough information for the model.
#' @param dup_session_action Similar to the argument in
#'   \code{link{create_session}}. The default \code{fill} tells the function to
#'   create a Session folder with the data in \code{file} if the session does
#'   not exists, otherwise do nothing and keep adding Annotation files to the
#'   session at each iteration. As in \code{link{create_session}}, \code{add}
#'   creation a new session, \code{replace} replace the current session content,
#'   \code{stop} raises an error.
#' @param limits A list of condition that would prevent the function from
#'   running; the function will return \code{NULL} with a message. They are
#'   useful especially if \code{autorun} is \code{TRUE}. \code{stop_after}:
#'   number of CR iterations without new positive predictions;
#'   \code{pos_target}: Number of positive matches found; \code{labeling_limit}
#'   ratio of all records manually reviewed.
#' @param compute_performance Use \code{link{estimate_performance}} to estimate
#'   total posterior sensitivity and efficiency based on a surrogate logistic
#'   model. It is very expensive to compute so it is turned off by default.
#' @param use_prev_labels If the \code{Rev_previous} is present in the data or a
#'   new one is created using \code{prev_classification}, use the labels in it
#'   to solve uncertain record labels.
#' @param prev_classification A data frame with already labeled records to use
#'   to solve automatically uncertain record labels during a CR iteration. Only
#'   used if \code{use_prev_labels} is \code{TRUE}.
#' @param save_samples Whether to save on disk the PPDs of the records. These
#'   are necessary (at least those related to the last CR iteration) to create a
#'   new search query automatically with [extract_rules()].
#' @param rebuild Every time the function is run, a backup of the Bayesian model
#'   outputs (the PPDs samples and the variable importance) is saved in the file
#'   "Model_backup.rds". If \code{rebuild} is \code{FALSE} and such file exist,
#'   the backed up copy is used instead of refitting the model. If set to
#'   \code{FALSE} it allows to skip model retraining if the function failed
#'   before finishing.
#' @param ... Additional parameters passed to [compute_BART_model()].
#'
#' @return An Annotation data frame with a number of extra columns:
#'   \code{Rev_prediction} and a \code{Rev_prediction_new} which contains
#'   respectively manually reviewed predictions from previous CR iterations and
#'   new predictions that require review; \code{Predicted_label} which stores
#'   the labels predicted using the Uncertainty Zone mechanism; \code{Pred_Low},
#'   \code{Pred_Med}, and \code{Pred_Up} which describe a record PPD using the
#'   uncertainty levels defined into the \code{pred_quants} argument.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ## A simple call using a session name will automatically pick up the right
#' # annotation file (the initial one or an already classified one if existing)
#' # and start a CR iteration.
#'
#' enrich_annotation_file("Session1")
#'
#' # Alternatively, an Annotation file can be passed manually (discouraged)
#'
#' records_file <- get_session_files("Session1")$Records
#'
#' enrich_annotation_file("Session1", file = records_file)
#' }
enrich_annotation_file <- function(session_name,
                                   file = NULL, # TODO: change into "records" since it can be a data frame.
                                   DTM = NULL,
                                   ## Model parameters
                                   pos_mult = 10,
                                   n_models = 10,
                                   resample = FALSE,
                                   pred_quants = c(.01, .5, .99),
                                   #
                                   sessions_folder = getOption("baysren.sessions_folder", "Sessions"),
                                   pred_batch_size = 5000,
                                   autorun = TRUE,
                                   stop_on_unreviewed = TRUE,
                                   dup_session_action = c(
                                     "fill", "add",
                                     "replace", "stop"
                                   ),
                                   limits = list(
                                     stop_after = 4, pos_target = NULL,
                                     labeling_limit = NULL
                                   ),
                                   compute_performance = FALSE,
                                   # test_data = NULL,
                                   use_prev_labels = TRUE,
                                   prev_classification = NULL,
                                   save_samples = TRUE,
                                   rebuild = FALSE, ...) {

  # Silence CMD CHECK about non standard eval
  Rev_manual <- . <- Rev_prediction <- ID <- Pred_Up <- Pred_Low <- Predicted_label <- Order <- Rev_previous <- Term <- Val <- Value <- NULL

  # pick the last annotated record file or the source one if any
  if (is.null(file)) {
    file <- with(get_session_files(session_name, sessions_folder), {
      c(last(Annotations), Records)[1]
    })

    if (is.null(file)) {
      stop("No annotation files in this session, or the session folder doesn't exists.")
    }
    message("Processing file: ", file)
  }

  process_id <- paste0(".pID__", stringr::str_replace_all(file, stringr::fixed(.Platform$file.sep), "__"))

  if (file.exists(process_id)) {
    message("File already being processed. Skipping.")
    return(NULL)
  }

  writeLines(paste("Process started on:", safe_now()), process_id)

  on.exit(file.remove(process_id))

  if (length(pred_quants) != 3 & any(pred_quants >= 1 | pred_quants <= 0)) {
    stop('"pred_quants" should be 3 quantiles, between 0 and 1 (included).')
  }

  # log arguments. useful for error checking
  Arguments <- match.call() %>%
    as.list() %>%
    names() %>%
    lapply(function(name) {
      if (name == "" || !exists(name)) {
        return(NULL)
      }
      obj <- get(name)
      data.frame(
        name,
        value = if (is.data.frame(obj)) {
          paste(class(obj), collapse = ", ")
        } else {
          capture.output(str(obj)) %>%
            head() %>%
            paste(collapse = "\n") %>%
            stringr::str_trim()
        }
      )
    }) %>%
    bind_rows()

  pred_quants <- sort(pred_quants)[c(2, 1, 3)]

  dup_session_action <- match.arg(dup_session_action)

  dup_session_action <- if (dup_session_action == "fill") "silent" else dup_session_action

  session_path <- create_session(
    Records = file, session_name = session_name,
    sessions_folder = sessions_folder,
    dup_session_action = dup_session_action
  )

  if (pos_mult < 1) stop('"pos_mult" should be at least 1.')

  if (pos_mult != round(pos_mult)) stop('"pos_mult" should be an integer value.')

  message("Loading Annotation file")

  tictoc::tic()
  # Read the file and use (theoretically) all rows to infer the column type, to
  # avoid misspecification errors.

  Records <- import_data(file)

  if (all(is.na(Records$Rev_manual))) {
    stop("No manually labeled entries found. This may also happen if there is a great number of missings before the first labeled record.")
  }

  repl <- 1

  # If the file is an annotated file the replication is extracted
  if (basename(dirname(file)) == "Annotations") {

    # replication is extracted from the file title or set to one
    prev_run <- stringr::str_extract(file, "(?<=_rep)\\d+") %>% as.numeric()

    repl <- max(1, prev_run, na.rm = TRUE)

    # increase the replication if no new positives
    iter_data <- compute_changes(Records) %>%
      select(-matches("unlab\\. -> unlab\\.|y -> y|n -> n"))

    new_positives <- iter_data %>%
      select(matches("-> y")) %>%
      rowSums()

    if (new_positives == 0) repl <- repl + 1 else repl <- 1
  }

  if (!is.null(limits) & !isFALSE(limits)) {
    message("Checking conditions for a new iteration")

    Target <- coalesce_labels(Records) %>% na.omit()

    # All records have been labeled
    if (length(Target) == nrow(Records)) {
      message("No unlabeled records left!")

      return(NULL)
    }

    # Enough positives found
    if (!is.null(limits$pos_target) &&
      sum(Target %in% "y") >= limits$pos_target) {
      message("Positive records target reached!")
      return(NULL)
    }

    # Too many records have been labeled
    if (!is.null(limits$labeling_limit)) {
      if ((limits$labeling_limit <= 1 & length(Target) / nrow(Records) >= limits$labeling_limit) |
        (limits$labeling_limit > 1 & length(Target) >= limits$labeling_limit)) {
        message("Num/Ratio of labeled records above threshold!")
        return(NULL)
      }
    }

    if (!is.null(limits$stop_after) && (repl > limits$stop_after)) {
      message("Reached limit of consecutive iterations without new positive records!")
      return(NULL)
    }

    rm(Target)
  }

  # Add Rev_prediction for storing prediction reviews if missing
  if ("Rev_prediction" %nin% names(Records)) {
    Records <- Records %>%
      mutate(Rev_prediction = NA, .after = Rev_manual)
  }

  if ("*" %in% Records$Rev_prediction_new & stop_on_unreviewed) {
    stop("There are unreviewed predictions.")
  } else {
    Records$Rev_prediction_new <- replace(
      Records$Rev_prediction_new,
      Records$Rev_prediction_new == "*",
      NA
    )
  }

  # Coalesce Rev_prediction_new into Rev_prediction and clear the former
  Records <- Records %>%
    mutate(
      Rev_prediction = coalesce_labels(., c("Rev_prediction_new", "Rev_prediction")),
      Rev_prediction_new = NA,
      .after = Rev_prediction
    )
  tictoc::toc()

  if (!is.null(prev_classification)) {
    message("Importing previous classification")

    tictoc::tic()

    Records <- import_classification(
      records = Records,
      prev_records = import_data(prev_classification)
    )

    tictoc::toc()
  }

  # if (!is.null(test_data)) {
  # 	message('Importing test data')
  #
  # 	tictoc::tic()
  # 	Test_data <- import_data(test_data)
  #
  # 	tictoc::toc()
  # } else Test_data <- NULL

  tictoc::tic()

  if (is.null(DTM) & repl > 1) {
    DTM <- file.path(sessions_folder, session_name, "DTM.rds")
  }

  # Reload DTM if existing and there were no new positive matches
  if (is.character(DTM) && file.exists(DTM)) {
    message("Loading DTM")

    DTM <- readr::read_rds(DTM)
  } else {
    message("Creating DTM")

    DTM <- create_training_set(Records)

    message("Saving DTM")

    ##
    DTM_file <- file.path(session_path, "DTM.rds")

    readr::write_rds(DTM, file = DTM_file, compress = "gz")
    ##
  }

  if (!(all(Records$ID %in% DTM$ID))) {
    stop("The DTM and the records should be compatible (same IDs).")
  }

  # Import the labeling from the reviewed data to the DTM
  Cur_Target <- Records %>%
    transmute(
      ID,
      Target = coalesce_labels(.)
    )

  if (any(Cur_Target$Target %nin% c(NA, "y", "n"))) {
    stop('Labels can only be "y" or "n"')
  }

  DTM$Target <- NULL

  DTM <- left_join(DTM, Cur_Target, by = "ID")

  # Add features reporting the number of terms present in each block
  for (field in c("ABSTR", "TITLE", "KEYS", "MESH")) {
    DTM[[paste0(field, ".count")]] <- select(DTM, contains(field)) %>%
      rowSums(na.rm = TRUE)
  }
  tictoc::toc()

  message("Training data:")
  message("Positives: ", sum(DTM$Target == "y", na.rm = TRUE))
  message("Negatives: ", sum(DTM$Target == "n", na.rm = TRUE))
  message("Features: ", select(DTM, -ID, -Target) %>% ncol())

  message(glue("\nModel generation (repl: {repl})"))

  if (rebuild == FALSE & file.exists("Model_backup.rds")) {
    message("(loading from disk...)")
    gc()
    bart.mods <- readr::read_rds("Model_backup.rds")

    Samples <- bart.mods$Samples
    Var_imp <- bart.mods$Var_imp
  } else {
    Var_imp <- list()

    pb <- pbapply::startpb(0, n_models)
    on.exit(pbapply::closepb(pb))

    for (i in 1:n_models) {
      print(i)
      train_data <- DTM %>% filter(!is.na(Target))

      if (resample) {
        train_data <- slice_sample(train_data, prop = 1, replace = TRUE)
      }

      train_data <- train_data[c(
        rep(which(train_data$Target %in% "y"), pos_mult),
        which(!(train_data$Target %in% "y"))
      ), ]

      bart.mod <- suppressMessages(compute_BART_model(train_data %>% select(-ID), "Target",
        name = "BartModel", rebuild = TRUE, save = FALSE,
        verbose = FALSE, ...
      ))
      rm(train_data)
      gc()

      message("predicting...")

      preds <- pbapply::pblapply(0:floor(nrow(DTM) / pred_batch_size), function(i) {
        gc()
        start <- i * pred_batch_size + 1
        stop <- min((i + 1) * pred_batch_size, nrow(DTM))
        idx <- start:stop

        bartMachine::bart_machine_get_posterior(
          bart.mod,
          new_data = DTM[idx, ] %>% select(all_of(colnames(bart.mod$X)))
        )$y_hat_posterior_samples
      }) %>% do.call(what = "rbind")

      if (!exists("Samples")) {
        Samples <- preds
      } else {
        Samples <- Samples + preds
      }

      message("variable importance...")

      Var_imp[[i]] <- bartMachine::get_var_props_over_chain(bart.mod, "trees")

      rm(bart.mod, preds)

      gc()

      pbapply::setpb(pb, i)
    }

    # Average posterior samples along the ensemble of models
    Samples <- Samples / n_models


    message("\nWriting model to disk")
    readr::write_rds(list(Samples = Samples, Var_imp = Var_imp),
      "Model_backup.rds",
      compress = "gz"
    )
    gc()
  }

  DTM <- DTM %>% select(-matches("\\.count$"))

  message("Generating labels")

  tictoc::tic()

  Samples <- data.frame(ID = DTM$ID, Samples)

  Predicted_data <- DTM %>%
    select(ID, Target) %>%
    data.frame(
      apply(Samples[, -1], 1, quantile, pred_quants) %>% t() %>%
        as.data.frame() %>%
        stats::setNames(c("Pred_Med", "Pred_Low", "Pred_Up"))
    ) %>%
    mutate(
      Pred_delta = Pred_Up - Pred_Low, # add posterior interval range
      Predicted_label = {
        negLim <- max(Pred_Up[Target %in% "n"])
        posLim <- min(Pred_Low[Target %in% "y"])

        case_when( # assign y if a record range is included into global y range and don't overlap the n range. The opposite is true for n labels
          Pred_Low > negLim & Pred_Low > posLim ~ "y",
          Pred_Up < posLim & Pred_Up < negLim ~ "n",
          TRUE ~ "unk" # Assign unk if the label is not clearly defined
        )
      },
      Predicted_label = replace(
        Predicted_label,
        Predicted_label != Target & Predicted_label != "unk",
        "check"
      ), # mark if predicted label is in contrast with the training data
      across(matches("Pred_"), signif, 3)
    )

  Annotated_data <- left_join(
    select(Records, -any_of(colnames(Predicted_data %>% select(-ID)))),
    Predicted_data,
    by = "ID"
  ) %>%
    select(Order, matches("^Rev"), Predicted_label, matches("^Pred"), everything()) %>%
    arrange(Order) %>%
    mutate(
      Rev_prediction_new = {
        if (!use_prev_labels | "Rev_previous" %nin% names(.)) {
          Previous_lab <- rep("*", n())
        } else {
          Previous_lab <- replace(Rev_previous, is.na(Rev_previous), "*")
        }

        case_when(
          !is.na(Rev_prediction) | Predicted_label == "n" ~ NA_character_,
          Predicted_label == Rev_manual ~ NA_character_,
          Predicted_label %in% c("check", "unk", "y") ~ Previous_lab
        )
      }
    )

  tictoc::toc()

  Performance <- NULL

  if (compute_performance) {
    message("Adding performance summary (may take a while...)")

    Performance <- estimate_performance(Annotated_data) %>%
      format_performance(session_names = session_name)

    print(Performance)
  }

  message("Adding variables' importance")

  Var_imp <- lapply(1:n_models, function(i) {
    data.frame(
      Term = names(Var_imp[[i]]),
      Val = Var_imp[[i]]
    )
  }) %>%
    bind_rows() %>%
    group_by(Term) %>%
    summarise(
      Value = mean(Val),
      Score = if (n_models > 1) Value / sd(Val) else Value,
      n_models = n()
    )

  message("Adding annotation summary")

  Results <- tibble(
    Iter = (list.files(file.path(session_path, "Annotations"), pattern = ".xlsx") %>%
      stringr::str_subset("~\\$", negate = TRUE) %>% length()) + 1,
    "Parent file" = file,
    "Replication n." = repl,
    "N. features" = select(DTM, -ID, -Target) %>% ncol(),
    "New labels" = Annotated_data$Rev_prediction_new %>%
      summarise_vector(),
    "Records to review" = with(Annotated_data, Predicted_label[Rev_prediction_new %in% "*"]) %>%
      summarise_vector(),
    "Final labeling" = coalesce_labels(Annotated_data, c(
      "Rev_prediction_new", "Rev_prediction",
      "Rev_manual", "Predicted_label"
    )) %>%
      summarise_vector()
  ) %>%
    bind_cols(
      compute_changes(Annotated_data)
    ) %>%
    mutate(across(.fns = as.character)) %>%
    tidyr::pivot_longer(everything(), names_to = "Indicator", values_to = "Value")

  print(as.data.frame(Results))

  message("Exporting")

  out <- list(
    Annotated_data = Annotated_data,
    Results = Results,
    Performance = Performance,
    Variable_importance = Var_imp,
    Arguments = Arguments
  ) %>% Filter(f = Negate(is.null))

  common_tag <- glue('{if (repl > 1) paste0("rep", repl, "_") else ""}{safe_now()}')
  iter <- with(Results, Value[Indicator == "Iter"])

  message("- annotated records...")
  tictoc::tic()
  output_file_ann <- file.path(
    session_path, "Annotations",
    glue("{iter}.Records_{common_tag}.xlsx")
  )

  dir.create(dirname(output_file_ann), showWarnings = FALSE, recursive = TRUE)

  openxlsx::write.xlsx(out, file = output_file_ann, asTable = TRUE)

  tictoc::toc()

  message("- annotation summary...")
  tictoc::tic()
  output_file_res <- file.path(
    session_path, "Results",
    glue("{iter}.Results_{common_tag}.csv")
  )
  dir.create(dirname(output_file_res), showWarnings = FALSE, recursive = TRUE)

  readr::write_csv(Results, file = output_file_res)

  tictoc::toc()

  if (save_samples) {
    message("- posterior samples...")
    tictoc::tic()

    output_file_samp <- file.path(
      session_path, "Samples",
      glue("{iter}.Samples_{common_tag}.rds")
    )
    dir.create(dirname(output_file_samp), showWarnings = FALSE, recursive = TRUE)

    readr::write_rds(Samples, file = output_file_samp, compress = "gz")

    tictoc::toc()
  }

  if (file.exists("Model_backup.rds")) file.remove("Model_backup.rds")

  file.remove(process_id)

  if (autorun) {
    if ("*" %nin% Annotated_data$Rev_prediction_new) {
      message("\n\nAutomatic restart")

      rm(Predicted_data, Annotated_data, Var_imp, Samples)
      gc()

      enrich_annotation_file(output_file_ann,
        DTM = get_session_files(session_name, sessions_folder)$DTM,
        ## Model parameters
        pos_mult = pos_mult,
        n_models = n_models,
        pred_quants = pred_quants,
        resample = resample,
        #
        dup_session_action = "fill", # if an automatic reply, this is the logical option
        session_name = session_name,
        compute_performance = compute_performance,
        sessions_folder = sessions_folder, limits = limits,
        autorun = autorun, use_prev_labels = use_prev_labels,
        prev_classification = NULL, # if present it was already imported
        # test_data = test_data,
        rebuild = TRUE, # redundant since the model backup was removed, but just to be on the safe side
        ...
      )
    } else {
      message("Manual labeling needed!")
    }
  }

  invisible(out)
}

#' Include manual review in annotation result summaries
#'
#' [enrich_annotation_file()] creates summary files of each classification
#' iteration into the Results session sub-folder, but these summaries do not
#' includes the manual review of the predicted labels. [consolidate_results()]
#' regenerates these files including the changes resulting from the manual
#' review of the automatic classification.
#'
#' The results summary, not including the manual review, is still available
#' inside the annotation files.
#'
#' @param session_name The session with the result files to consolidate.
#' @param sessions_folder The path to the folder where all the sessions are
#'   stored.
#'
#' @return The list of the regenerated Results' file content as data frames,
#'   invisibly.
#'
#' @export
#'
consolidate_results <- function(session_name, sessions_folder = getOption("baysren.sessions_folder", "Sessions")) {
  # Silence CMD CHECK about non standard eval
  results_files <- Indicator <- results_files <- NULL

  annotations_files <- get_session_files(session_name, sessions_folder)$Annotations

  message("Loading annotations...")
  annotations <- annotations_files %>%
    pbmcapply::pbmclapply(function(file) {
      import_data(file)
    })

  message("Consolidating results...")
  pbmcapply::pbmclapply(1:length(results_files), function(i) {
    final_results <- annotations[[i]] %>%
      compute_changes() %>%
      mutate_all(as.character) %>%
      tidyr::pivot_longer(
        cols = everything(),
        names_to = "Indicator",
        values_to = "Value"
      )

    annotations_files[[i]] %>%
      import_data(sheet = "Results") %>%
      filter(Indicator %nin% c(
        final_results$Indicator, "New labels", "Records to review",
        "Final labeling"
      )) %>%
      filter(!stringr::str_detect(Indicator, "\\*")) %>%
      bind_rows(final_results) %>%
      bind_rows(
        data.frame(
          Indicator = "Reviewed predictions",
          Value = summarise_vector(annotations[[i]]$Rev_prediction_new)
        )
      ) %>%
      readr::write_csv(results_files[[i]])
  }) %>% invisible()
}


#' Perform a grid evaluation of parameters to tune the classification framework
#'
#' The performance of the framework, measured as Sensitivity (rate of relevant
#' record found over all relevant records) and Efficiency (one minus the ratio
#' of manually reviewed records) is strongly impacted by a number of parameters.
#'
#' These parameters are related to the framework only and independent by the
#' specific Bayesian classification method used (which itself has other specific
#' parameters). The parameters are the following:
#'
#' \itemize{ \item \code{n_init}: The number of records in the manually labeled
#' initial training set. \item \code{n_models}: The number of models trained and
#' then averaged to stabilize the posterior predictive distribution (PPD). \item
#' \code{resample}: Whether to bootstrap the data between model retraining if
#' the number of models is more than one. \item \code{pos_mult}: Oversampling
#' rate of the positive labeled records. \item \code{pred_quants}: The quantiles
#' used to summarise the records' PPD and built the Uncertainty zone. }
#'
#' Check [enrich_annotation_file()] for more insight about their influence on
#' the framework and the classification results. Since all records are
#' pre-labelled, the manual review phase is performed automatically.
#'
#' The algorithm starts from a fully labelled Annotation set and performs a
#' Classification/Review cycle for each combination of parameters.
#'
#' A great number of files will be created (40 GB with the default grid
#' parameters for a input \code{records} file with 1200 labelled records), one
#' session folder for each parameter combination. Therefore, be sure to have
#' enough disk space before starting. Also, keep in mind that a full search may
#' requires many days, even on powerful computers.
#'
#' @param records A fully labelled Annotation data set (data frame or a path to
#'   a Excel / CSV file).
#' @param sessions_folder A path to a folder where to store the grid search
#'   results.
#' @param prev_classification An Annotation data set or file with labelled
#'   records. The labels in this data set will be used as ground truth for the
#'   \code{records} file, but the records themselves will not be used.
#' @param n_init A vector of numbers enumerating the size of the initial
#'   training set. The initial training set simulates the initial manual
#'   labelling of records used to train the model. It is generated by the
#'   \code{records} data set selecting records in descending order.
#' @param pos_mult,n_models,resample,pred_quants A vector of values for each
#'   parameter. For \code{pred_quants} a list of vectors. See
#'   [enrich_annotation_file()] for more details.
#' @param limits The conditions on which a CR cycle is stopped. See
#'   [enrich_annotation_file()].
#'
#' @return A message with the number of parameter combinations evaluated.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # First, the user needs to manually label a significant number of records; we
#' # suggest one thousand or more. The new record file can be stored anywhere,
#' # but putting it into the grid search folder is a better practice.
#'
#' records <- file.path("Grid_Search", "Classification_data.xlsx")
#'
#' Grid_search <- perform_grid_evaluation(
#'   records,
#'   sessions_folder = "Grid_Search",
#'   prev_classification = records,
#'   ## Model parameters (can be changed by users)
#'   resample = c(FALSE, TRUE),
#'   n_init = c(50, 100, 250, 500),
#'   n_models = c(1, 5, 10, 20, 40, 60),
#'   pos_mult = c(1, 10, 20),
#'   pred_quants = list(
#'     c(.1, .5, .9),
#'     c(.05, .5, .95),
#'     c(.01, .5, .99)
#'   )
#' )
#' }
perform_grid_evaluation <- function(records, sessions_folder = "Grid_Search",
                                    prev_classification = records,
                                    ## Model parameters
                                    resample = c(FALSE, TRUE),
                                    n_init = c(50, 100, 250, 500),
                                    n_models = c(1, 5, 10, 20, 40, 60),
                                    pos_mult = c(1, 10, 20),
                                    pred_quants = list(
                                      c(.1, .5, .9),
                                      c(.05, .5, .95),
                                      c(.01, .5, .99)
                                    ),
                                    ## Passed arguments
                                    limits = list(
                                      stop_after = 4,
                                      pos_target = NULL, labeling_limit = NULL
                                    )) {

  # Silence CMD CHECK about non standard eval
  Rev_previous <- Order <- NULL

  # file: 'Grid_Search/Classification_data.xlsx'

  # prepare the parameter grid
  Grid <- tidyr::expand_grid(
    resample, n_init, n_models, pos_mult, pred_quants
  ) %>%
    mutate(
      iteration = glue("{1:n()} / {n()}"),
      session = paste(
        "GridSession",
        glue("{n_models}Mods"),
        glue('{ifelse(resample, "y", "n")}Resamp'),
        glue("{n_init}Init"),
        glue("{pos_mult}Mult"),
        glue("{(sapply(pred_quants, max)-sapply(pred_quants, min))*100}Quant"),
        sep = "."
      ) %>% stringr::str_replace_all("\\.+", ".")
    )

  Records <- import_data(records)
  Classification_data <- import_data(prev_classification)

  # import the test classifications and remove unclassified records
  Records <- Records %>%
    import_classification(Classification_data) %>%
    filter(!is.na(Rev_previous))

  pbapply::pblapply(1:nrow(Grid), function(i) {
    str(Grid[i, ], give.attr = FALSE)

    if (file.exists("Model_backup.rds")) file.remove("Model_backup.rds")

    session_path <- file.path(sessions_folder, Grid[i, ]$session)

    # if no record files are present, recreate the session folder
    if (is.null(get_session_files(Grid[i, ]$session, sessions_folder)$Records)) {
      Records <- Records %>%
        # remove labels in excess of "n_init"
        mutate(
          Rev_manual = coalesce_labels(Records, c("Rev_manual", "Rev_prediction", "Rev_prediction_new", "Rev_previous")) %>%
            replace(Order > Grid[i, ]$n_init, NA),
          Rev_prediction = NA,
          Rev_prediction_new = NA
        )

      create_session(Records,
        session_name = Grid[i, ]$session,
        sessions_folder = sessions_folder,
        dup_session_action = "replace"
      )
    }

    # pick the last annotated record file or the source one if any
    last_record_file <- get_session_files(Grid[i, ]$session, sessions_folder)$Annotations %>%
      last()

    if (is.null(last_record_file)) {
      last_record_file <- get_session_files(Grid[i, ]$session, sessions_folder)$Records
    }

    with(
      Grid[i, ],
      enrich_annotation_file(last_record_file,
        session_name = session,
        sessions_folder = sessions_folder, rebuild = TRUE,
        prev_classification = Classification_data, DTM = NULL,
        use_prev_labels = TRUE,
        autorun = TRUE, replication = NULL,
        compute_performance = FALSE,
        # test_data = NULL,
        dup_session_action = "fill",
        limits = limits,
        ## Model parameters
        resample = resample,
        n_models = n_models,
        pos_mult = pos_mult,
        pred_quants = pred_quants[[1]]
      )
    )
  })

  message("Processed ", nrow(Grid), " parameter combinations.")

  return(invisible(NULL))
}
