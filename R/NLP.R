#' Transform words into their base form
#'
#' @param text_vec A vector of text documents.
#' @param dict A dictionary to use to find base forms. See
#'   \code{\link[lexicon:hash_lemmas]{lexicon::hash_lemmas}()} for the required
#'   structure.
#'
#' @return The vector with terms transformed into their base form, when
#'   possible.
#'
lemmatize <- function(text_vec, dict = lexicon::hash_lemmas) {

	dict <- setNames(lexicon::hash_lemmas$lemma, lexicon::hash_lemmas$token)

	separator <- '_tagseparator_'

	terms <- paste(text_vec, separator, collapse = ' ')
	terms <- gsub(sprintf(' *%s$', separator), '', terms, perl = TRUE) %>%
		str_split('\\b') %>% unlist()
	terms <- terms[!(terms %in% c('', ' '))]

	terms.lower <- tolower(terms)

	output <- ifelse(
		terms.lower %in% names(dict),
		dict[terms.lower], terms
	) %>%
		paste(collapse = ' ') %>%
		str_split(sprintf(' *%s *', separator)) %>% unlist

	output <- gsub('\\s+', ' ', output, perl = TRUE)
	output <- gsub('^\\s+|\\s+$', '', output, perl = TRUE)

	replace(output, output == 'NA', NA)
}


#' Clean up text into tokens
#'
#' Removes stop words, punctuation, auxiliary verbs. Lemmatizes text. Changes to
#' lower-case.
#'
#' @param corpus A vector of text documents.
#'
#' @return A cleaned-up vector of text documents.
#'
tokenize_text <- function(corpus) {

	message('- tokenizing text...')

	stopwords <- stopwords("english")

	tictoc::tic()
	corpus <- tolower(corpus)
	corpus <- gsub('-', '_', corpus, fixed = TRUE)
	corpus <- removeWords(corpus, stopwords("english"))
	corpus <- gsub("\'(s|re|t|d)?\\b", '', corpus, perl = TRUE)
	corpus <- gsub('_',' ', corpus, fixed = TRUE)
	corpus <- gsub('[^\\w\\d\\s]+', ' ', corpus, perl = TRUE)
	corpus <- lemmatize(corpus)

	tictoc::toc()

	corpus
}

#' Tokenize authors in records
#'
#' Use different tokenization approaches based on author field format.
#'
#' @param corpus A vector of author fields from an annotation data set.
#'
#' @return The tokenized author list.
#'
tokenize_authors <- function(corpus) {

	message('- tokenizing authors')
	tictoc::tic()

	ids = 1:length(corpus)

	with.comma <- str_detect(corpus, ',')

	corpus <- corpus %>% str_squish()

	output <- mclapply(1:length(corpus), function(i) {
		if (is.na(with.comma[i])) NA # No authors listed
		else if (with.comma[i] == TRUE) { # Pubmed or WOS style author list
			corpus[i] %>%
				str_remove_all('[^\\w ,;]') %>%
				str_replace_all('(?<=,)[ \\-\\w]+?(?:(?=;)|$)', function(x) {
					paste0(str_extract_all(x, '\\b\\w')[[1]], collapse = '')
				}) %>% str_replace_all(',', '_') %>% str_remove_all(' +')
		} else { # IEEE style author list
			corpus[i] %>%
				str_remove_all('[^\\w\\.;]') %>% # remove non letters and other characters
				str_replace_all('[^;]+(?:(?=;)|$)', function(x) { # extract names between ;
					str_replace(x, '([\\w \\.]+)\\.([\\w ]+)', '\\2_\\1') #use the rightmost dot to separate first and last names
				}) %>% str_remove_all('\\.')
		}
	}) %>% unlist %>%
		str_replace_all('; *', ' ')

	tictoc::toc()

	output
}

#' Tokenize keywords in records
#'
#' Clean up the keyword fields in the records.
#'
#' @param corpus A vector of keywords fields from an annotation data set.
#'
#' @return The tokenized keyword list.
#'
tokenize_keywords <- function(keywords) {
	keywords %>%
		str_to_lower() %>%
		str_replace_all(c(
			'\\s*;\\s*' = ';',
			'[^;\\w]+' = '_',
			';' = ' '))
}


#' Tokenize MESH keywords in records
#'
#' Clean up the keyword fields in the records.
#'
#' @param corpus A vector of MESH keywords fields from an annotation data set.
#'
#' @return The tokenized MESH keyword list.
#'
tokenize_MESH <- function(mesh) {

	message('- tokenizing Mesh terms')
	tictoc::tic()

	output <- mesh %>% str_replace_all(c(' *; *' = ';', '[\\(\\)]' = '', '[ ,\\-]+' = '_', '&' = 'and')) %>%
		str_replace_all('(?:(?<=;)|^)[^;]+/[^;]+(?:(?=;)|$)', function(x) {
			x <- str_split(x, '/')[[1]]
			paste(c(x[1], paste(x[1],x[-1], sep = '.sh.')), collapse = ';')
		}) %>% str_replace_all(';', ' ') %>% str_squish()

	tictoc::toc()

	output
}


#' Convert a vector of text documents into a Document Term Matrix
#'
#' A Document Term Matrix (DTM) is a structure describing the association of a
#' term to a document. In this case, we used a binary matrix with ones if a term
#' is present in a document and one otherwise.
#'
#' Before computing the DTM, document terms are cleaned, tokenized and
#' lemmatized, and stop-words are removed.
#'
#' To reduce noise, only terms that appear in a fraction of documents higher
#' than \code{min.freq} are considered. The function also uses cosine similarity
#' to identify relevant subclusters of related terms or redundant ones.
#'
#' @param corpus A vector of text documents.
#' @param min.freq Minimum number of document in which a term need to be present
#'   to be considered.
#' @param ids Identification ID of documents.
#' @param freq.subset.ids IDs to consider when computing term frequency.
#' @param included.pos Part of speech (POS) to consider when building the DTM.
#'   See \code{\link[lexicon:hash_grady_pos]{lexicon::hash_grady_pos}()} for a
#'   list of recognized POS.
#' @param tokenize.fun Function to use to clean up text.
#' @param add.ngrams Whether to search and add non-consecutive n-grams. See
#'   \code{\link{DTM.add_ngrams}}.
#' @param n.gram.thresh The threshold to use to identify the network of
#'   non-consecutive n-grams if \code{add.ngrams} is \code{TRUE}.
#' @param aggr.synonyms Whether to aggregate terms which almost always appear
#'   together. See \code{\link{DTM.aggr_synonyms}()}.
#' @param syn.thresh The threshold to use to identify the network of terms to
#'   aggregate if \code{aggr.synonyms} is \code{TRUE}.
#' @param label A label to prepend to term columns in the DTM.
#' @param na.as.missing Whether to set as \code{NA} the DTM cells for empty
#'   document. If \code{FALSE} those cells will be set to zero.
#'
#' @return A Document Term Matrix with a row for each document and a column for
#'   the terms plus a column with the document IDs.
#'
#' @examples
#'
#' @export
#'
#' \dontrun{
#'
#' Records <- import_data(get_session_files('Session1')$Records)
#'
#' Title_DTM <- with(
#'   Records,
#'   text_to_DTM(Title, min.freq = 20, label = 'TITLE__', ids = ID,
#'     freq.subset.ids = ID[Target %in% c('y', 'n')])
#'   )
#' }
text_to_DTM <- function(corpus, min.freq = 20, ids = 1:length(corpus),
												freq.subset.ids = ids,
												included.pos = c('Noun', 'Verb', 'Adjective'), #TODO: add Plural and Noun Phrase
												tokenize.fun = tokenize_text, add.ngrams = TRUE,
												aggr.synonyms = TRUE, n.gram.thresh = .5,
												syn.thresh = .9, label = 'TERM__', na.as.missing = TRUE) {

	raw.corpus <- corpus
	order.ids <- 1:length(corpus)
	names(ids) <- order.ids

	if (is.na(min.freq)) stop('"min.freq" is NA.')

	if (!is.null(tokenize.fun)) {
		corpus <- tokenize.fun(corpus)
	}

	splitted.corpus <- corpus %>% str_split(' +')

	if (length(splitted.corpus) != length(ids)) stop('Number of documents and ids are different')

	excluded.pos <- lexicon::hash_grady_pos %>%
		mutate(pos = str_remove_all(pos, ' \\(.*')) %>%
		filter(!(pos %in% included.pos) & !(word %in% word[pos %in% included.pos])) # Keep terms that are ONLY associated to non relevant parts of speech

	message('- to long format...')
	tictoc::tic()

	corpus <- data.frame(
		term =  splitted.corpus %>% unlist,
		val = 1,
		ID = rep(order.ids, splitted.corpus %>% sapply(length))
	) %>% na.omit() %>% distinct() %>%
		mutate(
			val = replace(val, str_detect(term, '\\*'), 2),
			term = str_remove(term, '\\*')
		) %>%
		filter(!(term %in% excluded.pos$word))
	tictoc::toc()

	message('- removing rare terms...')
	frequent_terms <- corpus %>%
		filter(ID %in% order.ids[ids %in% freq.subset.ids]) %>%
		count(term, name = 'Freq') %>% # count term frequency, but only in relevant IDs
		filter(Freq >= min.freq) %>% pull(term) # create frequent terms list

	corpus <- corpus %>% filter(term %in% frequent_terms) %>% # filter out unfrequent terms
		arrange(ID, term, desc(val)) %>%
		distinct(ID, term, .keep_all = TRUE) # Remove duplicate terms keeping the first of each occurrence (useful for Mesh data)

	message('- to wide format...')
	tictoc::tic()
	DTM <- tidyr::pivot_wider(corpus, id_cols = ID, names_from = term,
														names_prefix = label, values_from = val,
														values_fill = 0)

	tictoc::toc()

	if (add.ngrams) {
		message('- find non consecutive ngram...')
		tictoc::tic()

		DTM <- DTM.add_ngrams(DTM, min.sim = n.gram.thresh)

		tictoc::toc()
	}

	if (aggr.synonyms) {

		message('- find synonyms...')
		tictoc::tic()

		DTM <- DTM.aggr_synonyms(DTM, min.sim = syn.thresh)

		tictoc::toc()
	}

	# The synonyms creation procedure can create very long names
	DTM <- DTM %>% setNames(str_sub(colnames(DTM), 1, 10000))

	message('- managing missings...')

	if (nrow(DTM) < length(raw.corpus)) { # Add documents with no content, ie. NAs
		missing_docs <- setdiff(order.ids, DTM$ID)

		DTM <- bind_rows(
			DTM,
			DTM[rep(1, length(missing_docs)),] %>% # add the missing documents using the first DTM row as template
				mutate(ID = missing_docs, across(c(-ID), ~ 0)) # set the term score to zero for all added documents
		) %>% arrange(ID)
	}

	if (na.as.missing) { # Put NA to terms for documents with no content, otherwise leave zero
		DTM <- DTM %>% mutate(across(-ID, ~ replace(.x, is.na(raw.corpus), NA)))
	}

	DTM %>% mutate(ID = ids[ID])
}

#' Find non-consecutive n-grams
#'
#' Build a term-term network using a cosine similarity measure built on the term
#' co-presence in documents. A threshold defined in \code{min.sim} is used to
#' identify edges. The maximal cliques of the network represent the discovered
#' n-grams.
#'
#' @param DTM A Document Term Matrix.
#' @param min.sim The minimal cosine similarity that identifies an edge.
#' @param max.terms The maximum size (i.e., the number of terms) in an n-gram.
#'
#' @return The same input Document Term Matrix with extra columns for the
#'   n-grams.
#'
DTM.add_ngrams <- function(DTM, min.sim = .5, max.terms = 10) {

	mat <- as.matrix(DTM[,-1])

	mat.sparse <- as(mat, "dgCMatrix") # Using sparse matrices

	TTM <- (t(mat.sparse) %*% mat.sparse)/sqrt(tcrossprod(colSums(mat^2, na.rm = TRUE))) # Cosine similarity

	ngram.cliques <- graph_from_adjacency_matrix(as.matrix(TTM) >= min.sim, mode = 'undirected', diag = FALSE) %>% # From TTM to undirected network
		max_cliques(min = 2) %>% lapply(names) # Extracting cliques

	if (length(ngram.cliques) > 0) {
		new_cols <- lapply(ngram.cliques, function(clique) { # For each clique
			if (length(clique) <= max.terms) {
			new_col <- data.frame(
				as.numeric(DTM[, clique] %>% rowSums() == length(clique)) # TRUE if all words are present in the doc
			)

			colnames(new_col) <- paste0(str_extract(clique[1], '\\w+__'), str_remove(clique, '\\w+__') %>% paste(collapse = '._.'))

			new_col
			}
		}) %>% bind_cols()

		DTM <- DTM %>% bind_cols(new_cols) # Add joined terms
	}
}

#' Aggregate redundant terms
#'
#' Build a term-term network using a cosine similarity measure built on the term
#' co-presence in documents. A high threshold defined in \code{min.sim} is used
#' to identify edges. The high edge threshold splits the network into multiple
#' components which identify redundant terms.
#'
#' @param DTM A Document Term Matrix.
#' @param min.sim The minimal cosine similarity that identifies an edge.
#'
#' @return The same input Document Term Matrix with redundant terms removed and
#'   joined into new columns.
#'
DTM.aggr_synonyms <- function(DTM, min.sim = .9) {

	mat <- as.matrix(DTM[,-1])

	mat.sparse <- as(mat, "dgCMatrix") # Using sparse matrices

	TTM <- (t(mat.sparse) %*% mat.sparse)/sqrt(tcrossprod(colSums(mat^2, na.rm = TRUE))) # Cosine similarity

	syn.components <- graph_from_adjacency_matrix(as.matrix(TTM) >= min.sim, mode = 'undirected', diag = FALSE) %>% # From TTM to undirected network
		components() # Extracting connected subgraphs

	syn.components <- lapply(which(syn.components$csize > 1), function(i) {
		names(syn.components$membership)[syn.components$membership == i]
	})

	if (length(syn.components) > 0) {
		new_cols <- lapply(syn.components, function(component) { # For each component
			new_col <- data.frame(
				as.numeric(DTM[, component] %>% rowSums() > 0) # TRUE if at least one word is in the doc
			)

			colnames(new_col) <- paste0(str_extract(component[1], '\\w+__'), str_remove(component, '\\w+__') %>% paste(collapse = '.'))

			new_col
		}) %>% bind_cols()

		DTM <- DTM %>% select(-all_of(syn.components %>% unlist)) %>% # Remove single terms
			bind_cols(new_cols) # Add joined terms
	} else DTM
}
