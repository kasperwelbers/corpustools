
#' Calculate the similarity of documents
#'
#' @param tc A \link{tCorpus}
#' @param feature the column name of the feature that is to be used for the comparison.
#' @param date_col a date with time in POSIXct. If given together with hour_window, only documents within the given hour_window will be compared.
#' @param meta_cols a character vector with columns in the meta data / docvars. If given, only documents for which these values are identical are compared
#' @param hour_window A vector of length 1 or 2. If length is 1, the same value is used for the left and right side of the window. If length is 2, the first and second value determine the left and right side. For example, the value 12 will compare each document to all documents between the previous and next 12 hours, and c(-10, 36) will compare each document to all documents between the previous 10 and the next 36 hours.
#' @param measure the similarity measure. Currently supports cosine similarity (symmetric) and overlap_pct (asymmetric)
#' @param min_similarity A threshold for the similarity score
#' @param weight a weighting scheme for the document-term matrix. Default is term-frequency inverse document frequency with normalized rows (document length).
#' @param ngrams an integer. If given, ngrams of this length are used
#' @param from_subset An expression to select a subset. If given, only this subset will be compared to other documents
#' @param to_subset An expression to select a subset. If given, documents are only compared to this subset
#' @param return_igraph If TRUE, return as an igraph network. Otherwise, return as a list with the edgelist and meta data.
#' @param verbose If TRUE, report progress
#'
#' @return An igraph graph in which nodes are documents and edges represent similarity scores
#' @export
#' @examples
#' d = data.frame(text = c('a b c d e',
#'                         'e f g h i j k',
#'                         'a b c'),
#'                date = as.POSIXct(c('2010-01-01','2010-01-01','2012-01-01')))
#' tc = create_tcorpus(d)
#'
#' g = compare_documents(tc)
#' igraph::get.data.frame(g)
#'
#' g = compare_documents(tc, measure = 'overlap_pct')
#' igraph::get.data.frame(g)
#'
#' g = compare_documents(tc, date_col = 'date', hour_window = c(0,36))
#' igraph::get.data.frame(g)
compare_documents <- function(tc, feature='token', date_col=NULL, meta_cols=NULL, hour_window=c(24), measure=c('cosine','overlap_pct'), min_similarity=0, weight=c('norm_tfidf', 'tfidf', 'termfreq','docfreq'), ngrams=NA, from_subset=NULL, to_subset=NULL, return_igraph=T, verbose=T) {
  weight = match.arg(weight)
  measure = match.arg(measure)
  from_subset = tc$eval_meta(substitute(from_subset), parent.frame())
  to_subset = tc$eval_meta(substitute(to_subset), parent.frame())

  dtm = get_dfm(tc, feature=feature, weight = weight, drop_empty_terms = F, context_labels = T, feature_labels=F, ngrams=ngrams)

  if (!is.null(meta_cols)) {
    meta_cols = subset(quanteda::docvars(dtm), select=meta_cols)
    quanteda::docvars(dtm, 'group_col') = do.call(paste, args=c(meta_cols, list(sep='__')))
    group_col = 'group_col'
  } else group_col = NULL

  dtm_y = NULL
  if (!is.null(to_subset)) {
    dtm_y = quanteda::dfm_subset(dtm, subset=to_subset)
  }
  if (!is.null(from_subset)) {
    if (is.null(dtm_y)) dtm_y = dtm
    dtm = quanteda::dfm_subset(dtm, subset=from_subset)
  }

  if (length(hour_window) == 1) hour_window = c(-hour_window, hour_window)

  g = RNewsflow::compare_documents(dtm, dtm_y, only_complete_window = F,
                                   date_var=date_col, hour_window=hour_window, group_var=group_col,
                                   min_similarity=min_similarity, measure=measure, verbose=verbose)
  if (return_igraph) g = RNewsflow::as_document_network(g)
  g
}

#' Deduplicate documents
#'
#' @description
#' Deduplicate documents based on similarity scores. Can be used to filter out identical documents, but also similar documents.
#'
#' Note that deduplication occurs by reference (\link{tCorpus_modify_by_reference}) unless copy is set to TRUE.
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#' \preformatted{deduplicate(feature='token', date_col=NULL, meta_cols=NULL, hour_window=NULL, min_docfreq=2, max_docfreq_pct=0.5, measure=c('cosine','overlap_pct'), similarity=1, keep=c('first','last', 'random'), weight=c('norm_tfidf', 'tfidf', 'termfreq','docfreq'), ngrams=NA, print_duplicates=F, copy=F)}
#'
#' @param feature the column name of the feature that is to be used for the comparison.
#' @param date_col The column name for a column with a date vector (in POSIXct). If given together with hour_window, only documents within the given hour_window will be compared.
#' @param meta_cols a vector with names for columns in the meta data. If given, documents are only considered duplicates if the values of these columns are identical (in addition to having a high similarity score)
#' @param hour_window A vector of length 1 or 2. If length is 1, the same value is used for the left and right side of the window. If length is 2, the first and second value determine the left and right side. For example, the value 12 will compare each document to all documents between the previous and next 12 hours, and c(-10, 36) will compare each document to all documents between the previous 10 and the next 36 hours.
#' @param min_docfreq a minimum document frequency for features. This is mostly to lighten computational load. Default is 2, because terms that occur once cannot overlap across documents
#' @param max_docfreq_pct a maximum document frequency percentage for features. High frequency terms contain little information for identifying duplicates. Default is 0.5 (i.e. terms that occur in more than 50 percent of documents are ignored),
#' @param lowercase If True, make feature lowercase
#' @param measure the similarity measure. Currently supports cosine similarity (symmetric) and overlap_pct (asymmetric)
#' @param similarity the similarity threshold used to determine whether two documents are duplicates. Default is 1, meaning 100 percent identical.
#' @param keep select either 'first', 'last' or 'random'. Determines which document of duplicates to delete. If a date is given, 'first' and 'last' specify whether the earliest or latest document is kept.
#' @param weight a weighting scheme for the document-term matrix. Default is term-frequency inverse document frequency with normalized rows (document length).
#' @param ngrams an integer. If given, ngrams of this length are used
#' @param print_deduplicates if TRUE, print ids of duplicates that are deleted
#' @param verbose if TRUE, report progress
#' @param copy If TRUE, the method returns a new tCorpus object instead of deduplicating the current one by reference.
#'
#' @name tCorpus$deduplicate
#' @aliases deduplicate
#' @examples
#' d = data.frame(text = c('a b c d e',
#'                         'e f g h i j k',
#'                         'a b c'),
#'                date = as.POSIXct(c('2010-01-01','2010-01-01','2012-01-01')))
#' tc = create_tcorpus(d)
#'
#' tc$meta
#' dedup = tc$deduplicate(feature='token', date_col = 'date', similarity = 0.8, copy=TRUE)
#' dedup$meta
#'
#' dedup = tc$deduplicate(feature='token', date_col = 'date', similarity = 0.8, keep = 'last',
#'                        copy=TRUE)
#' dedup$meta
tCorpus$set('public', 'deduplicate', function(feature='token', date_col=NULL, meta_cols=NULL, hour_window=24, min_docfreq=2, max_docfreq_pct=1, lowercase=T, measure=c('cosine','overlap_pct'), similarity=1, keep=c('first','last', 'random'), weight=c('norm_tfidf', 'tfidf', 'termfreq','docfreq'), ngrams=NA, print_duplicates=F, verbose=T, copy=F){
  weight = match.arg(weight)
  measure = match.arg(measure)

  match.arg(feature, self$feature_names)
  if (copy) {
    selfcopy = self$copy()$deduplicate(feature=feature, date_col=date_col, meta_cols=meta_cols, hour_window=hour_window, min_docfreq=min_docfreq, max_docfreq_pct=max_docfreq_pct, measure=measure, similarity=similarity, keep=keep, weight=weight, ngrams=ngrams, print_duplicates=print_duplicates, copy=F)
    return(selfcopy)
  }

  ## adding DEDUPLICATE_FEATURE is not very elegant and memory efficient. Better alternative, perhaps, is to pass docfreq_filter results to compare_documents_fun.
  self$preprocess(feature, new_column = 'DEDUPLICATE_FEATURE', lowercase=lowercase, min_docfreq = min_docfreq, max_docfreq = self$n_meta * max_docfreq_pct)

  .duplicates = get_duplicates(self, feature='DEDUPLICATE_FEATURE', date_col=date_col, meta_cols=meta_cols, hour_window=hour_window, measure=measure, similarity=similarity, keep=keep, weight=weight, print_duplicates=print_duplicates, verbose=verbose)
  self$subset(subset_meta = !doc_id %in% .duplicates, copy=F)
  self$set('DEDUPLICATE_FEATURE', NULL)
  invisible(self)
})

##################################
##################################

to_POSIXct <- function(x){
  tryCatch(as.POSIXct(x),
           warning = function(w) stop(sprintf('Date column cannot be properly interpreted as POSIXct: \n%s', w)),
           error = function(e) stop(sprintf('Date column cannot be interpreted as POSIXct: \n\t-> %s', e)))
}

get_duplicates <- function(tc, feature='token', date_col=NULL, meta_cols=NULL, hour_window=24, measure=c('cosine','overlap_pct'), similarity=1, keep=c('first','last', 'random'), weight=c('termfreq','docfreq','tfidf','norm_tfidf'), ngrams=NA, print_duplicates=F, verbose=T) {
  keep = match.arg(keep)
  for (mvar in meta_cols) if (!mvar %in% tc$meta_names) stop(sprintf('Meta column (%s) not in corpus', mvar))

  #g = compare_documents_fun(tc, feature=feature, date_col=date_col, hour_window=hour_window, measure=measure, min_similarity=similarity, weight=weight, ngrams)
  dtm = get_dfm(tc, feature=feature, weight = weight, drop_empty_terms = F, context_labels = T, feature_labels=F, ngrams=ngrams)

  if (!is.null(meta_cols)) {
    meta_cols = subset(quanteda::docvars(dtm), select=meta_cols)
    quanteda::docvars(dtm, 'group_col') = do.call(paste, args=c(meta_cols, list(sep='__')))
    group_col = 'group_col'
  } else group_col = NULL

  if (length(hour_window) == 1) hour_window = c(-hour_window, hour_window)

  d = RNewsflow::compare_documents(dtm, date_var=date_col, group_var=group_col, hour_window=hour_window, only_complete_window = F,
                                   measure=measure, min_similarity=similarity, verbose=verbose)
  d = d$d

  if (is.null(d) || nrow(d) == 0) {
    message('Deleting 0 duplicates')
    return(c())
  }

  duplicates = c()
  if (!is.null(date_col)) {
    if(keep == 'first') {
      duplicates = c(duplicates, as.character(unique(d$to[d$hourdiff > 0])))
      duplicates = c(duplicates, as.character(unique(d$from[d$hourdiff < 0])))
    }
    if(keep == 'last') {
      duplicates = c(duplicates, as.character(unique(d$from[d$hourdiff > 0])))
      duplicates = c(duplicates, as.character(unique(d$to[d$hourdiff < 0])))
    }
    d = d[!d$from %in% duplicates & !d$to %in% duplicates,]
  }

  ## If date_var is missing, keep == 'random', or if there are identical articles that occured simultaneously, delete randomly
  if (keep == 'random') d = d[sample(1:nrow(d), nrow(d)),]
  d$fromi = match(d$from, unique(d$from, d$to))
  d$toi = match(d$to, unique(d$from, d$to))
  d = d[d$fromi < d$toi,]
  duplicates = unique(c(duplicates, as.character(d$from)))

  message('Deleting ', length(duplicates), ' duplicates')
  if (print_duplicates) sprintf('c(%s)', print(paste(sprintf('"%s"', duplicates), collapse=', ')))
  duplicates
}


