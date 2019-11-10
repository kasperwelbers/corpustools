#' Get common nearby features given a query or query hits
#'
#' @param tc a \link{tCorpus}
#' @param feature The name of the feature column in $tokens
#' @param query A character string that is a query. See \link{search_features} for documentation of the query language.
#' @param hits Alternatively, instead of giving a query, the results of \link{search_features} can be used.
#' @param query_feature If query is used, the column in $tokens on which the query is performed. By default uses 'token'
#' @param window The size of the word window (i.e. the number of words next to the feature)
#' @param n the top n of associated features
#' @param min_freq Optionally, ignore features that occur less than min_freq times
#' @param sort_by The value by which to sort the features
#' @param subset A call (or character string of a call) as one would normally pass to subset.tCorpus. If given, the keyword has to occur within the subset. This is for instance usefull to only look in named entity POS tags when searching for people or organization. Note that the condition does not have to occur within the subset.
#' @param subset_meta A call (or character string of a call) as one would normally pass to the subset_meta parameter of subset.tCorpus. If given, the keyword has to occur within the subset documents. This is for instance usefull to make queries date dependent. For example, in a longitudinal analysis of politicians, it is often required to take changing functions and/or party affiliations into account. This can be accomplished by using subset_meta = "date > xxx & date < xxx" (given that the appropriate date column exists in the meta data).
#' @param include_self If True, include the feature itself in the output
#'
#' @return a data.frame
#' @export
#' @examples
#' tc = create_tcorpus(sotu_texts, doc_column = 'id')
#' tc$preprocess()
#'
#' ## directly from query
#' topf = feature_associations(tc, 'feature', 'war')
#' head(topf, 20) ## frequent words close to "war"
#'
#' ## adjust window size
#' topf = feature_associations(tc, 'feature', 'war', window = 5)
#' head(topf, 20) ## frequent words very close (five tokens) to "war"
#'
#' ## you can also first perform search_features, to get hits for (complex) queries
#' hits = search_features(tc, '"war terror"~10')
#' topf = feature_associations(tc, 'feature', hits = hits)
#' head(topf, 20) ## frequent words close to the combination of "war" and "terror" within 10 words
feature_associations <- function(tc, feature, query=NULL, hits=NULL, query_feature='token', window=15,  n=25, min_freq=1, sort_by= c('chi2', 'ratio', 'freq'), subset=NULL, subset_meta=NULL, include_self=F) {
  if (is.null(query) & is.null(hits)) stop('either keyword or hits has to be specified')
  if (!is.null(query) & !is.null(hits)) stop('only keyword or hits can be specified')
  if (!is.null(query)) hits = search_features(tc, query, feature = query_feature, mode='features')


  fa = feature_associations_fun(tc, hits=hits, feature=feature, window=window, n=n, min_freq=min_freq, sort_by=sort_by, subset=subset, subset_meta=subset_meta)
  if (!include_self) {
    feat = as.character(unique(tc$tokens[hits$hits, on=c('doc_id','token_id')][[feature]]))
    fa = fa[!fa$feature %in% feat,]
  }
  class(fa) = c('featureAssociations', 'data.frame')
  fa
}


feature_associations_fun <- function(tc, hits, feature='token', window=15,  n=25, min_freq=1, sort_by= c('chi2', 'ratio', 'freq'), subset=NULL, subset_meta=NULL) {
  if(methods::is(substitute(subset), 'call')) subset = tc$eval(substitute(subset), parent.frame())
  if(methods::is(substitute(subset_meta), 'call')) subset_meta = tc$eval_meta(substitute(subset_meta), parent.frame())
  sort_by = match.arg(sort_by)

  window = tc$get_token_id(hits$hits$doc_id, hits$hits$token_id, subset, subset_meta, window=window)

  tc_sub = tc$subset(window, copy=T)
  if (tc_sub$n == 0) {
    message('zero hits')
    return(NULL)
  }
  comp = compare_corpus(tc_sub, tc, feature = feature, is_subset = T)
  comp = comp[comp$freq.x > min_freq,]
  comp = comp[, c('feature','freq.x', 'freq.y', 'ratio','chi2')]
  colnames(comp) = c('feature','freq','freq_NOT', 'ratio', 'chi2')
  ord = order(-comp[[sort_by]])
  comp[head(ord, n),]
}
