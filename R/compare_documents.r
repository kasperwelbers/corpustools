
#' Calculate the similarity of documents
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#' \preformatted{compare_documents(feature='token', date_col=NULL, hour_window=NULL, measure=c('cosine','overlap_pct'), min_similarity=0, weight=c('norm_tfidf', 'tfidf', 'termfreq','docfreq'), ngrams=NA, from_subset=NULL, to_subset=NULL))}
#'
#' @param feature the column name of the feature that is to be used for the comparison.
#' @param date_col a date with time in POSIXct. If given together with hour_window, only documents within the given hour_window will be compared.
#' @param hour_window an integer. If given together with date_col, only documents within the given hour_window will be compared.
#' @param measure the similarity measure. Currently supports cosine similarity (symmetric) and overlap_pct (asymmetric)
#' @param weight a weighting scheme for the document-term matrix. Default is term-frequency inverse document frequency with normalized rows (document length).
#' @param ngrams an integer. If given, ngrams of this length are used
#' @param from_subset An expression to select a subset. If given, only this subset will be compared to other documents
#' @param to_subset An expression to select a subset. If given, documents are only compared to this subset
#'
#' @name tCorpus$compare_documents
#' @aliases compare_documents
#' @examples
#' d = data.frame(text = c('a b c d e',
#'                         'e f g h i j k',
#'                         'a b c'),
#'                date = c('2010-01-01','2010-01-01','2012-01-01'))
#' tc = create_tcorpus(d)
#'
#' g = tc$compare_documents()
#' igraph::get.data.frame(g)
#'
#' g = tc$compare_documents(measure = 'overlap_pct')
#' igraph::get.data.frame(g)
#'
#' g = tc$compare_documents(date_col = 'date', hour_window = c(0,36))
#' igraph::get.data.frame(g)
tCorpus$set('public', 'compare_documents', function(feature='token', date_col=NULL, hour_window=NULL, measure=c('cosine','overlap_pct'), min_similarity=0, weight=c('norm_tfidf', 'tfidf', 'termfreq','docfreq'), ngrams=NA, from_subset=NULL, to_subset=NULL) {
  weight = match.arg(weight)
  from_subset = self$eval_meta(substitute(from_subset), parent.frame())
  to_subset = self$eval_meta(substitute(to_subset), parent.frame())

  dtm = self$dtm(feature=feature, weight = weight, drop_empty_terms = F, context_labels = T, feature_labels=F, ngrams=ngrams)
  compare_documents_dtm(dtm, meta=self$get_meta(), date_col=date_col, hour_window=hour_window, only_from=from_subset, only_to=to_subset, min_similarity=min_similarity, measure=measure)

})

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
#' @param hour_window an integer. If given together with date_col, only documents within the given hour_window will be compared.
#' @param min_docfreq a minimum document frequency for features. This is mostly to lighten computational load. Default is 2, because terms that occur once cannot overlap across documents
#' @param min_docfreq a maximum document frequency percentage for features. High frequency terms contain little information for identifying duplicates. Default is 0.5 (i.e. terms that occur in more than 50 percent of documents are ignored),
#' @param measure the similarity measure. Currently supports cosine similarity (symmetric) and overlap_pct (asymmetric)
#' @param similarity the similarity threshold used to determine whether two documents are duplicates. Default is 1, meaning 100 percent identical.
#' @param keep select either 'first', 'last' or 'random'. Determines which document of duplicates to delete. If a date is given, 'first' and 'last' specify whether the earliest or latest document is kept.
#' @param weight a weighting scheme for the document-term matrix. Default is term-frequency inverse document frequency with normalized rows (document length).
#' @param ngrams an integer. If given, ngrams of this length are used
#' @param print_deduplicates if TRUE, print ids of duplicates that are deleted
#' @param copy If TRUE, the method returns a new tCorpus object instead of deduplicating the current one by reference.
#'
#' @name tCorpus$deduplicate
#' @aliases deduplicate
#' @examples
#' d = data.frame(text = c('a b c d e',
#'                         'e f g h i j k',
#'                         'a b c'),
#'                date = c('2010-01-01','2010-01-01','2012-01-01'))
#' tc = create_tcorpus(d)
#'
#' tc$get_meta()
#' dedup = tc$deduplicate(feature='token', date_col = 'date', similarity = 0.8, copy=TRUE)
#' dedup$get_meta()
#'
#' dedup = tc$deduplicate(feature='token', date_col = 'date', similarity = 0.8, keep = 'last',
#'                        copy=TRUE)
#' dedup$get_meta()
tCorpus$set('public', 'deduplicate', function(feature='token', date_col=NULL, meta_cols=NULL, hour_window=NULL, min_docfreq=2, max_docfreq_pct=1, measure=c('cosine','overlap_pct'), similarity=1, keep=c('first','last', 'random'), weight=c('norm_tfidf', 'tfidf', 'termfreq','docfreq'), ngrams=NA, print_duplicates=F, copy=F){
  weight = match.arg(weight)
  match.arg(feature, self$feature_names)
  if (copy) {
    selfcopy = self$copy()$deduplicate(feature=feature, date_col=date_col, meta_cols=meta_cols, hour_window=hour_window, min_docfreq=min_docfreq, max_docfreq_pct=max_docfreq_pct, measure=measure, similarity=similarity, keep=keep, weight=weight, ngrams=ngrams, print_duplicates=print_duplicates, copy=F)
    return(selfcopy)
  }

  ## adding DEDUPLICATE_FEATURE is not very elegant and memory efficient. Better alternative, perhaps, is to pass docfreq_filter results to compare_documents_fun.
  self$preprocess(feature, new_column = 'DEDUPLICATE_FEATURE', min_docfreq = min_docfreq, max_docfreq = self$n_meta * max_docfreq_pct)

  .duplicates = get_duplicates(self, feature='DEDUPLICATE_FEATURE', date_col=date_col, meta_cols=meta_cols, hour_window=hour_window, measure=measure, similarity=similarity, keep=keep, weight=weight, print_duplicates=print_duplicates)
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

get_duplicates <- function(tc, feature='token', date_col=NULL, meta_cols=NULL, hour_window=24, measure=c('cosine','overlap_pct'), similarity=1, keep=c('first','last', 'random'), weight=c('termfreq','docfreq','tfidf','norm_tfidf'), ngrams=NA, print_duplicates=F) {
  keep = match.arg(keep)
  for (mvar in meta_cols) if (!mvar %in% tc$meta_names) stop(sprintf('Meta column (%s) not in corpus', mvar))

  #g = compare_documents_fun(tc, feature=feature, date_col=date_col, hour_window=hour_window, measure=measure, min_similarity=similarity, weight=weight, ngrams)
  dtm = tc$dtm(feature=feature, weight = weight, drop_empty_terms = F, context_labels = T, feature_labels=F, ngrams=ngrams)

  g = compare_documents_dtm(dtm, meta=tc$get_meta(), date_col=date_col, meta_cols=meta_cols, hour_window=hour_window, measure=measure, min_similarity=similarity)


  if (is.null(g)) {
    message('Deleting 0 duplicates')
    return(c())
  }

  #e = igraph::get.edges(g, igraph::E(g)) ## edges by indices
  d = igraph::get.data.frame(g, 'edges') ## edges by name and with weigth
  if (!is.null(meta_cols)) {
    mx = tc$get_meta(meta_cols, keep_df = T, doc_id=d$from)
    my = tc$get_meta(meta_cols, keep_df = T, doc_id=d$to)

    allmatch = rowSums(mx == my) == ncol(mx)
    d = d[allmatch,]
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


