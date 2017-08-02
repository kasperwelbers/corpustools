#' Get common nearby terms given a feature query
#'
#' @section Usage:
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{feature_associations(keyword=NULL, condition=NA, hits=NULL, feature='token',
#'                                    window=15,  n=25, min_freq=1, sort_by= c('chi2', 'ratio', 'freq'),
#'                                    subset=NULL, subset_meta=NULL}
#'
#' @param keyword The keyword part of the query, see explanation in \link{search_features.tCorpus}.
#' @param condition The condition part of the query, see explanation in \link{search_features.tCorpus}.
#' @param hits Alternatively, instead of giving a query, the results of \link{search_features.tCorpus} can be used.
#' @param feature If keyword is used, the name of the feature column within which to search.
#' @param window The size of the word window (i.e. the number of words next to the feature)
#' @param n the top n of associated features
#' @param min_freq Optionally, ignore features that occur less than min_freq times
#' @param sort_by The value by which to sort the features
#' @param subset A call (or character string of a call) as one would normally pass to subset.tCorpus. If given, the keyword has to occur within the subset. This is for instance usefull to only look in named entity POS tags when searching for people or organization. Note that the condition does not have to occur within the subset.
#' @param subset_meta A call (or character string of a call) as one would normally pass to the subset_meta parameter of subset.tCorpus. If given, the keyword has to occur within the subset documents. This is for instance usefull to make queries date dependent. For example, in a longitudinal analysis of politicians, it is often required to take changing functions and/or party affiliations into account. This can be accomplished by using subset_meta = "date > xxx & date < xxx" (given that the appropriate date column exists in the meta data).
#'
#' @name tCorpus$feature_associations
#' @aliases feature_associations.tCorpus
tCorpus$set('public', 'feature_associations', function(keyword=NULL, condition=NA, hits=NULL, feature='token', window=15,  n=25, min_freq=1, sort_by= c('chi2', 'ratio', 'freq'), subset=NULL, subset_meta=NULL) {
  if (is.null(keyword) & is.null(hits)) stop('either keyword or hits has to be specified')
  if (!is.null(keyword) & !is.null(hits)) stop('only keyword or hits can be specified')
  if (!is.null(keyword)) hits = self$search_features(keyword=keyword, condition=condition)

  feature_associations(self, hits=hits, feature=feature, window=window, n=n, min_freq=min_freq, sort_by=sort_by, subset=subset, subset_meta=subset_meta)
})

###################################
###################################

feature_associations <- function(tc, hits, feature='token', window=15,  n=25, min_freq=1, sort_by= c('chi2', 'ratio', 'freq'), subset=NULL, subset_meta=NULL) {
  if(methods::is(substitute(subset), 'call')) subset = eval(substitute(subset), tc$data, parent.frame())
  if(methods::is(substitute(subset_meta), 'call')) subset_meta = eval(substitute(subset_meta), tc$meta, parent.frame())
  sort_by = match.arg(sort_by)

  i = hits$hits$i
  if (length(i) == 0) return(NULL)
  window = rep(i, window*2 + 1) + rep(-window:window, each=length(i))
  window = setdiff(window, i)

  if (!is.null(subset) | !is.null(subset_meta)){
    subset_i = tc$subset_i(subset, subset_meta)
    window = intersect(window, subset_i)
  }

  tc_sub = tc$subset(window, copy=T)
  comp = tc_sub$compare_corpus(tc, feature = feature, is_subset = T)
  comp = comp[comp$freq.x > min_freq,]
  comp = comp[, c('feature','freq.x', 'freq.y', 'ratio','chi2')]
  colnames(comp) = c('feature','freq','freq_NOT', 'ratio', 'chi2')
  ord = order(-comp[[sort_by]])
  comp[head(ord, n),]
}
