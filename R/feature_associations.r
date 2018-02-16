#' Get common nearby terms given a feature query
#'
#' @section Usage:
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
  if (is.null(query) & is.null(hits)) stop('either keyword or hits has to be specified')
  if (!is.null(query) & !is.null(hits)) stop('only keyword or hits can be specified')
  if (!is.null(query)) hits = self$search_features(query, mode='features')

  feature_associations(self, hits=hits, feature=feature, window=window, n=n, min_freq=min_freq, sort_by=sort_by, subset=subset, subset_meta=subset_meta)
})



###################################
###################################

feature_associations <- function(tc, hits, feature='token', window=15,  n=25, min_freq=1, sort_by= c('chi2', 'ratio', 'freq'), subset=NULL, subset_meta=NULL) {
  if(methods::is(substitute(subset), 'call')) subset = tc$eval(substitute(subset), parent.frame())
  if(methods::is(substitute(subset_meta), 'call')) subset_meta = tc$eval_meta(substitute(subset_meta), parent.frame())
  sort_by = match.arg(sort_by)

  window = tc$get_token_id(hits$hits$doc_id, hits$hits$token_id, subset, subset_meta, window=window)

  tc_sub = tc$subset(window, copy=T)
  if (tc_sub$n == 0) {
    message('zero hits')
    return(NULL)
  }
  comp = tc_sub$compare_corpus(tc, feature = feature, is_subset = T)
  comp = comp[comp$freq.x > min_freq,]
  comp = comp[, c('feature','freq.x', 'freq.y', 'ratio','chi2')]
  colnames(comp) = c('feature','freq','freq_NOT', 'ratio', 'chi2')
  ord = order(-comp[[sort_by]])
  comp[head(ord, n),]
}
