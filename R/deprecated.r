is_deprecated <- function(f = as.character(sys.call(sys.parent()))[1L]){
  f = gsub('.*\\$', '', f)
  msg <- gettextf("'%s' as a (R6) method is deprecated.\nIt used to be:\t\ttCorpus$%s(...)\nnow use instead:\t%s(tc, ...)\nSee help('%s')", f,f,f,f)
  warning(msg, call. = FALSE, domain = NA)
}

#' Compare tCorpus vocabulary to that of another (reference) tCorpus
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#' \preformatted{compare_corpus(tc_y, feature, smooth=0.1, min_ratio=NULL, min_chi2=NULL, is_subset=F, yates_cor=c('auto','yes','no'), what=c('freq','docfreq','cooccurrence'))}
#'
#' @param tc_y the reference tCorpus
#' @param feature the column name of the feature that is to be compared
#' @param smooth Laplace smoothing is used for the calculation of the ratio of the relative term frequency. Here you can set the added value.
#' @param min_ratio threshold for the ratio value, which is the ratio of the relative frequency of a term in dtm.x and dtm.y
#' @param min_chi2 threshold for the chi^2 value
#' @param yates_cor mode for using yates correctsion in the chi^2 calculation. Can be turned on ("yes") or off ("no"), or set to "auto", in which case cochrans rule is used to determine whether yates' correction is used.
#' @param is_subset Specify whether tc is a subset of tc_y. In this case, the term frequencies of tc will be subtracted from the term frequencies in tc_y
#' @param what choose whether to compare the frequency ("freq") of terms, or the document frequency ("docfreq"). This also affects how chi^2 is calculated, comparing either freq relative to vocabulary size or docfreq relative to corpus size (N)
#'
#' @name tCorpus$compare_corpus
#' @return A vocabularyComparison object
#' @examples
#' tc = create_tcorpus(sotu_texts, doc_column = 'id')
#'
#' tc$preprocess('token', 'feature', remove_stopwords = TRUE, use_stemming = TRUE)
#'
#' obama = tc$subset_meta(president == 'Barack Obama', copy=TRUE)
#' bush = tc$subset_meta(president == 'George W. Bush', copy=TRUE)
#'
#' comp = obama$compare_corpus(bush, 'feature')
#' comp = comp[order(-comp$chi),]
#' head(comp)
#' \dontrun{
#' plot(comp)
#' }
tCorpus$set('public', 'compare_corpus', function(tc_y, feature, smooth=0.1, min_ratio=NULL, min_chi2=NULL, is_subset=F, yates_cor=c('auto','yes','no'), what=c('freq','docfreq','cooccurrence')){
  is_deprecated()
  if (is_subset && self$n > tc_y$n) stop('tCorpus x (the one calling the method) cannot be a subset of tCorpus y, because it has more tokens')
  what = match.arg(what)
  tcorpus_compare(self, tc_y, feature, smooth=smooth, min_ratio=min_ratio, min_chi2=min_chi2, yates_cor=yates_cor, x_is_subset=is_subset, what=what)
})



#' Compare vocabulary of a subset of a tCorpus to the rest of the tCorpus
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#' \preformatted{compare_subset(feature, subset_x=NULL, subset_meta_x=NULL, query_x=NULL, query_feature='token', smooth=0.1, min_ratio=NULL, min_chi2=NULL, yates_cor=c('auto','yes','no'), what=c('freq','docfreq','cooccurrence'))}
#'
#' @param feature the column name of the feature that is to be compared
#' @param subset_x an expression to subset the tCorpus. The vocabulary of the subset will be compared to the rest of the tCorpus
#' @param subset_meta_x like subset_x, but using using the meta data
#' @param query_x like subset_x, but using a query search to select documents (see \link{tCorpus$search_contexts})
#' @param query_feature if query_x is used, the column name of the feature used in the query search.
#' @param smooth Laplace smoothing is used for the calculation of the ratio of the relative term frequency. Here you can set the added value.
#' @param min_ratio threshold for the ratio value, which is the ratio of the relative frequency of a term in dtm.x and dtm.y
#' @param min_chi2 threshold for the chi^2 value
#' @param yates_cor mode for using yates correctsion in the chi^2 calculation. Can be turned on ("yes") or off ("no"), or set to "auto", in which case cochrans rule is used to determine whether yates' correction is used.
#' @param what choose whether to compare the frequency ("freq") of terms, or the document frequency ("docfreq"). This also affects how chi^2 is calculated, comparing either freq relative to vocabulary size or docfreq relative to corpus size (N)
#'
#' @name tCorpus$compare_subset
#' @return A vocabularyComparison object
#' @examples
#' tc = create_tcorpus(sotu_texts, doc_column = 'id')
#'
#' tc$preprocess('token', 'feature', remove_stopwords = TRUE, use_stemming = TRUE)
#'
#' comp = tc$compare_subset('feature', subset_meta_x = president == 'Barack Obama')
#' comp = comp[order(-comp$chi),]
#' head(comp)
#' \dontrun{
#' plot(comp)
#' }
#'
#' comp = tc$compare_subset('feature', query_x = 'terroris*')
#' comp = comp[order(-comp$chi),]
#' head(comp, 10)
tCorpus$set('public', 'compare_subset', function(feature, subset_x=NULL, subset_meta_x=NULL, query_x=NULL, query_feature='token', smooth=0.1, min_ratio=NULL, min_chi2=NULL, yates_cor=c('auto','yes','no'), what=c('freq','docfreq','cooccurrence')){
  is_deprecated()
  subset_x = self$eval(substitute(subset_x), parent.frame())
  subset_meta_x = self$eval_meta(substitute(subset_meta_x), parent.frame())
  what = match.arg(what)

  if(is.null(subset_x) && is.null(subset_meta_x) & is.null(query_x)) stop("at least one of subset_x, subset_meta_x or query_x has to be specified")
  if(!is.null(subset_x) | !is.null(subset_meta_x)) {
    .subset_x = subset_x
    .subset_meta_x = subset_meta_x
    tc_x = self$subset(subset=.subset_x, subset_meta = .subset_meta_x, copy=T)
  }
  if(!is.null(query_x)) tc_x = self$subset_query(query_x, feature=query_feature, copy=T)

  comp = tc_x$compare_corpus(self, feature=feature, smooth=smooth, min_ratio=min_ratio, min_chi2=min_chi2, yates_cor=yates_cor, is_subset=T, what=what)
  comp
})

#' Get common nearby terms given a feature query
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{feature_associations(query=NULL, hits=NULL, feature='token',
#'                                    window=15,  n=25, min_freq=1, sort_by= c('chi2', 'ratio', 'freq'),
#'                                    subset=NULL, subset_meta=NULL}
#'
#' @param query A character string that is a query. See \link{search_features} for documentation of the query language.
#' @param hits Alternatively, instead of giving a query, the results of \link{tCorpus$search_features} can be used.
#' @param feature If keyword is used, the name of the feature column within which to search.
#' @param window The size of the word window (i.e. the number of words next to the feature)
#' @param n the top n of associated features
#' @param min_freq Optionally, ignore features that occur less than min_freq times
#' @param sort_by The value by which to sort the features
#' @param subset A call (or character string of a call) as one would normally pass to subset.tCorpus. If given, the keyword has to occur within the subset. This is for instance usefull to only look in named entity POS tags when searching for people or organization. Note that the condition does not have to occur within the subset.
#' @param subset_meta A call (or character string of a call) as one would normally pass to the subset_meta parameter of subset.tCorpus. If given, the keyword has to occur within the subset documents. This is for instance usefull to make queries date dependent. For example, in a longitudinal analysis of politicians, it is often required to take changing functions and/or party affiliations into account. This can be accomplished by using subset_meta = "date > xxx & date < xxx" (given that the appropriate date column exists in the meta data).
#'
#' @name tCorpus$feature_associations
#' @aliases feature_associations
#' @examples
#' tc = create_tcorpus(sotu_texts, doc_column = 'id')
#'
#' ## directly from query
#' topf = tc$feature_associations('war')
#' head(topf, 20) ## frequent words close to "war"
#'
#' ## adjust window size
#' topf = tc$feature_associations('war', window = 5)
#' head(topf, 20) ## frequent words very close (five tokens) to "war"
#'
#' ## you can also first perform search_features, to get hits for (complex) queries
#' hits = tc$search_features('"war terror"~10')
#' topf = tc$feature_associations(hits = hits)
#' head(topf, 20) ## frequent words close to the combination of "war" and "terror" within 10 words
#'
tCorpus$set('public', 'feature_associations', function(query=NULL, hits=NULL, feature='token', window=15,  n=25, min_freq=1, sort_by= c('chi2', 'ratio', 'freq'), subset=NULL, subset_meta=NULL) {
  is_deprecated()
  if (is.null(query) & is.null(hits)) stop('either keyword or hits has to be specified')
  if (!is.null(query) & !is.null(hits)) stop('only keyword or hits can be specified')
  if (!is.null(query)) hits = self$search_features(query, mode='features')

  feature_associations_fun(self, hits=hits, feature=feature, window=window, n=n, min_freq=min_freq, sort_by=sort_by, subset=subset, subset_meta=subset_meta)
})


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
  is_deprecated()
  weight = match.arg(weight)
  from_subset = self$eval_meta(substitute(from_subset), parent.frame())
  to_subset = self$eval_meta(substitute(to_subset), parent.frame())

  dtm = self$dtm(feature=feature, weight = weight, drop_empty_terms = F, context_labels = T, feature_labels=F, ngrams=ngrams)
  dtm_document_comparison(dtm, meta=self$get_meta(), date_col=date_col, window=hour_window, group_cols=NULL, min_value=min_similarity, measure=measure, unit=c('hours'), only_from=from_subset, only_to = to_subset, verbose=TRUE)
  #compare_documents_dtm(dtm, meta=self$get_meta(), date_col=date_col, hour_window=hour_window, only_from=from_subset, only_to=to_subset, min_similarity=min_similarity, measure=measure)
})

