
#' Calculate the similarity of documents
#'
#' @section Usage:
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
  if (!requireNamespace('RNewsflow', quietly = T)) stop('RNewsflow package needs to be installed in order to use document comparison methods')
  weight = match.arg(weight)
  from_subset = self$eval_meta(substitute(from_subset), parent.frame())
  to_subset = self$eval_meta(substitute(to_subset), parent.frame())
  compare_documents_fun(self, feature=feature, date_col=date_col, hour_window=hour_window, measure=measure, min_similarity=min_similarity, weight=weight, ngrams=ngrams, from_subset=from_subset, to_subset=to_subset)
})

#' Deduplicate documents
#'
#' Deduplicate documents based on similarity scores. Can be used to filter out identical documents, but also similar documents.
#'
#' Note that deduplication occurs by reference (\link{tCorpus_modify_by_reference}) unless copy is set to TRUE.
#'
#' @section Usage:
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
tCorpus$set('public', 'deduplicate', function(feature='token', date_col=NULL, meta_cols=NULL, hour_window=NULL, min_docfreq=2, max_docfreq_pct=0.5, measure=c('cosine','overlap_pct'), similarity=1, keep=c('first','last', 'random'), weight=c('norm_tfidf', 'tfidf', 'termfreq','docfreq'), ngrams=NA, print_duplicates=F, copy=F){
  if (!requireNamespace('RNewsflow', quietly = T)) stop('RNewsflow package needs to be installed in order to use document comparison methods')

  weight = match.arg(weight)
  match.arg(feature, self$feature_names)
  if (copy) {
    selfcopy = self$copy()$deduplicate(feature=feature, date_col=date_col, meta_cols=meta_cols, hour_window=hour_window, min_docfreq=min_docfreq, max_docfreq_pct=max_docfreq_pct, measure=measure, similarity=similarity, keep=keep, weight=weight, ngrams=ngrams, print_duplicates=print_duplicates, copy=F)
    return(selfcopy)
  }

  ## adding DEDUPLICATE_FEATURE is not very elegant and memory efficient. Better alternative, perhaps, is to pass docfreq_filter results to compare_documents_fun.
  .feature = feature
  self$set('DEDUPLICATE_FEATURE', self$get(.feature))
  self$feature_subset('DEDUPLICATE_FEATURE', 'DEDUPLICATE_FEATURE', subset = docfreq_filter('DEDUPLICATE_FEATURE', min=min_docfreq, max=self$n * max_docfreq_pct), copy=F)

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

compare_documents_fun <- function(tc, feature='token', date_col=NULL, hour_window=c(-24,24), measure=c('cosine','overlap_pct'), min_similarity=0, weight=c('termfreq','docfreq','tfidf','norm_tfidf'), ngrams=NA, from_subset=NULL, to_subset=NULL) {
  measure = match.arg(measure)
  if (measure == 'overlap_pct') measure = 'percentage.from'
  if (!is.null(date_col)) date_col = match.arg(date_col, choices = tc$meta_names)

  meta = as.data.frame(tc$get_meta())

  ## construct dtm's based on subsets, but not if date_col and hour_window are given, because then this needs to be done differently for the newsflow.compare function
  if (is.null(date_col) | is.null(hour_window)){
    dtm = tc$dtm(feature=feature, weight = weight, subset_meta=from_subset, drop_empty_terms = F, context_labels = T, feature_labels=F, ngrams=ngrams, form = 'tm_dtm')
    if (is.null(from_subset) & is.null(to_subset)) {
      dtm.y = NULL
    } else {
      dtm.y = tc$dtm(feature=feature, weight = weight, subset_meta = to_subset, drop_empty_terms = F, context_labels = T, feature_labels=F, ngrams=ngrams, form = 'tm_dtm')
    }
  } else {
    ## if the compare.newsflow function is used, the full DTM is given, and the only_from and only_to arguments are used
    only_from = if (!is.null(from_subset)) tc$get_meta('doc_id')[from_subset] else NULL
    only_to = if (!is.null(to_subset)) tc$get_meta('doc_id')[to_subset] else NULL

    dtm = tc$dtm(feature=feature, weight = weight, drop_empty_terms = T, context_labels = T, feature_labels=F, ngrams=ngrams, form = 'tm_dtm')
  }

  if (is.null(date_col)) {
    d = RNewsflow::documents.compare(dtm, dtm.y, measure=measure, min.similarity=min_similarity)
    if (nrow(d) == 0) return(NULL)

    g = igraph::graph.data.frame(d[,c('x','y')], directed = T)
    igraph::E(g)$weight = d$similarity

    missingmeta = as.character(meta[!meta$doc_id %in% igraph::V(g)$name, 'doc_id'])
    g = igraph::add.vertices(g, nv = length(missingmeta), attr = list(name=missingmeta))
    meta = meta[match(igraph::V(g)$name, meta$doc_id),]
    attribs = colnames(meta)[!colnames(meta) == 'doc_id']
    for(attrib in attribs){
      g = igraph::set.vertex.attribute(g, attrib, value=as.character(meta[,attrib]))
    }
  } else {
    meta[,date_col] = to_POSIXct(meta[,date_col])
    if (is.null(hour_window)) {
      d = RNewsflow::documents.compare(dtm, dtm.y, measure=measure, min.similarity=min_similarity)
      if (nrow(d) == 0) return(NULL)
      g = RNewsflow::document.network(d, meta, 'doc_id', date_col)
    } else {
      g = RNewsflow::newsflow.compare(dtm, meta, id.var='doc_id', date.var=date_col, measure=measure, only.from=only_from, only.to=only_to, min.similarity=min_similarity, hour.window=hour_window)
    }
  }
  g
}

get_duplicates <- function(tc, feature='token', date_col=NULL, meta_cols=NULL, hour_window=NULL, measure=c('cosine','overlap_pct'), similarity=1, keep=c('first','last', 'random'), weight=c('termfreq','docfreq','tfidf','norm_tfidf'), ngrams=NA, print_duplicates=F) {
  keep = match.arg(keep)
  for (mvar in meta_cols) if (!mvar %in% tc$meta_names) stop(sprintf('Meta column (%s) not in corpus', mvar))

  g = compare_documents_fun(tc, feature=feature, date_col=date_col, hour_window=hour_window, measure=measure, min_similarity=similarity, weight=weight, ngrams)
  if (is.null(g)) {
    message('Deleting 0 duplicates')
    return(tc)
  }

  e = igraph::get.edges(g, igraph::E(g)) ## edges by indices
  d = igraph::get.data.frame(g, 'edges') ## edges by name and with weigth
  if (!is.null(meta_cols)) {
    mx = tc$get_meta(meta_cols)[e[,1],]
    my = tc$get_meta(meta_cols)[e[,2],]

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
  d = d[sample(1:nrow(d), nrow(d)),]
  d$fromi = match(d$from, unique(d$from, d$to))
  d$toi = match(d$to, unique(d$from, d$to))
  d = d[d$fromi < d$toi,]
  duplicates = unique(c(duplicates, as.character(d$from)))

  message('Deleting ', length(duplicates), ' duplicates')
  if (print_duplicates) sprintf('c(%s)', print(paste(sprintf('"%s"', duplicates), collapse=', ')))
  duplicates
}
