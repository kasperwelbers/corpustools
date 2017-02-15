#' Subset a tCorpus
#'
#' Returns the subset of a tCorpus. The selection can be made separately (and simultaneously) for the token data (using subset) and the meta data (using subset_meta).
#'
#' In the logical expression (e.g., sent_i ) for subset and subset_meta can contain the
#'
#' Note that you can also use objects from your (global) environment. For example, if you have a vector called filter_doc_ids, your logical expression can be: doc_id %in% filter_doc_ids. This is also a good way to filter words, for instance by using
#'
#' @param tc tCorpus object
#' @param subset logical expression indicating elements or rows to keep in the tokens data.
#' @param subset_meta
#' @param keep_feature_index
#' @param drop_levels
#' @param window If not NULL, an integer specifiying the window to be used to return the subset. For instance, if the subset contains word 10 in a document and window is 5, the subset will contain word 5 to 15. Naturally, this does not apply to subset_meta.
#'
#' @export
subset.tCorpus <- function(tc, subset=NULL, subset_meta=NULL, keep_feature_index=T, drop_levels=F, window=NULL) {
  subset = if(is(substitute(subset), 'call')) deparse(substitute(subset)) else subset
  subset_meta = if(is(substitute(subset_meta), 'call')) deparse(substitute(subset_meta)) else subset_meta

  e = if(is(substitute(subset), 'character')) parse(text=subset) else substitute(subset)
  e_meta = if(is(substitute(subset_meta), 'character')) parse(text=subset_meta) else substitute(subset_meta)

  r_meta = eval(e_meta, tc@meta, parent.frame())
  if(!is.null(r_meta)){
    tc@meta = tc@meta[r_meta,,nomatch=0]
    tc@data = tc@data[as.character(unique(tc@meta$doc_id)),,nomatch=0]
  }

  set_keys(tc)
  r = eval(e, tc@data, parent.frame())
  if(!is.null(r)){
    if(!is.null(window)){
      global_i = get_global_i(tc, max_window_size=window)
      global_r = global_i[r]
      global_window = rep(global_r, window*2 + 1) + rep(-window:window, each=length(global_r)) ## add window
      r = global_i %in% global_window
    }
    tc@data = tc@data[r,,nomatch=0]
    set_keys(tc)
    tc@meta = tc@meta[as.character(unique(tc@data$doc_id)),,nomatch=0]
  }

  if(drop_levels){
    tc@data = droplevels(tc@data)
    tc@meta = droplevels(tc@meta)
  }
  tc@meta$doc_id = as.character(tc@meta$doc_id)

  suppressWarnings(set_keys(tc))

  if(get_provenance(tc)$feature_index){
    if(keep_feature_index){
      cat('Feature index has been reset for the new subset\n')
      tc = reset_feature_index(tc)
    } else {
      tc = delete_feature_index(tc)
    }
  }
  tc
}

#' Title
#'
#' @param tc
#' @param query
#' @param code
#' @param feature
#' @param context_level
#'
#' @export
subset_query <- function(tc, query, feature='word', context_level=c('document','sentence')){
  context_level = match.arg(context_level)
  hits = search_contexts(tc, query, feature=feature, context_level=context_level)
  if(is.null(hits)) return(NULL)
  if(context_level == 'document'){
    tc = subset(tc, doc_id %in% unique(hits$doc_id))
  }
  if(context_level == 'sentence'){
    d = get_data(tc, columns = c('doc_id','sent_i'))
    d$i = 1:nrow(d)
    rows = d[list(hits$doc_id, hits$sent_i)]$i
    tc = subset(tc, rows)
  }
  tc
}

#' subset a tCorpus to all tokens that occur within the given window of a given set of indices
#'
#' @param tc a tCorpus object
#' @param i the indices
#' @param window an integer specifying the window size
#' @param context_level
#'
#' @export
subset_window <- function(tc, i, window, context_level=c('document','sentence')){
  context_level = match.arg(context_level)
  subset(tc, i_window(tc, i, window=window, context_level=context_level))
}



i_window <- function(tc, i, window, context_level=c('document','sentence')){
  context_level = match.arg(context_level)
  gi = get_global_i(tc, context_level, max_window_size = window)
  gi_i = gi[i]
  gi_window = rep(gi_i, window*2 + 1) + rep(-window:window, each=length(gi_i))
  window_i = na.omit(match(gi_window, gi))
  unique(window_i[order(window_i)])
}

#' Subset a tcorpus for a window around the results of search_features()
#'
#' This function works like the search_features() function, but instead of returning a data.frame with the search results, it returns a subset of the tcorpus with the search results, and a specified window arround the results.
#'
#'
#' @param tc
#' @param keyword
#' @param condition
#' @param queries
#' @param feature
#' @param condition_once
#' @param subset_tokens
#' @param subset_meta
#' @param verbose
#'
#' @return a tCorpus object
#' @export
subset_query_window <- function(tc, window, keyword=NA, condition=NA, queries=NULL, feature='word', condition_once=F, subset_tokens=NA, subset_meta=NA, verbose=F){
  hits = search_features(tc, keyword=keyword, condition=condition, code=code, queries=queries, feature=feature, condition_once=condition_once, subset_tokens=subset_tokens, subset_meta=subset_meta, keep_false_condition=F, only_last_mword=F, verbose=verbose)
  tc = subset_window(tc, i=hits$i, window=window, context_level = context_level)
}


subset_i <- function(tc, subset=NA, subset_meta=NA){
  ## subset and subset_meta can be either a call or a character vector of length 1
  subset = if(is(substitute(subset), 'call')) deparse(substitute(subset)) else subset
  subset_meta = if(is(substitute(subset_meta), 'call')) deparse(substitute(subset_meta)) else subset_meta

  n = nrow(get_data(tc))
  if(is.na(subset)){
    r = NULL
  } else {
    e = parse(text=as.character(subset))
    r = eval(e, tc@data, parent.frame())
    r = (1:n)[r]
  }

  n_meta = nrow(get_meta(tc))
  if(is.na(subset_meta)){
    r_meta = NULL
  } else {
    e_meta = parse(text=as.character(subset_meta))
    r_meta = (1:n_meta)[eval(e_meta, tc@meta, parent.frame())]
    if(length(r_meta) > 0) {
      r_meta = 1:n_meta %in% r_meta
      r_meta = r_meta[match(get_column(tc, 'doc_id'), get_meta_column(tc, 'doc_id'))] ## extend to length()
      r_meta = which(r_meta)
    }
  }

  if(!is.null(r) & !is.null(r_meta)) return(intersect(r, r_meta))
  if(is.null(r) & is.null(r_meta)) return(1:n)
  if(!is.null(r) & is.null(r_meta)) return(r)
  if(is.null(r) & !is.null(r_meta)) return(r_meta)
}


## subset functions ##

#' @export
freq <- function(x) {
  d = as.data.frame(table(x))
  d$Freq[match(x, d$x)]
}

#' @export
freq_top <- function(x, n=100) {
  d = as.data.frame(table(x))
  top = head(d[order(-d$Freq),], n)
  d$top = ifelse(d$x %in% top$x, T, F)
  d$top[match(x, d$x)]
}

#' @export
docfreq <- function(x, doc_id=parent.frame()$doc_id) {
  d = unique(data.frame(id=doc_id, term=x))
  d = as.data.frame(table(d$term))
  d$Freq[match(x, d$Var1)]
}

#' @export
docfreq_top <- function(x, n=100, doc_id=parent.frame()$doc_id) {
  d = unique(data.frame(id=doc_id, term=x))
  d = as.data.frame(table(d$term))
  top = head(d[order(-d$Freq),], n)
  d$top = ifelse(d$Var1 %in% top$Var1, T, F)
  d$top[match(x, d$Var1)]
}

#' @export
docfreq_pct <- function(x, doc_id=parent.frame()$doc_id) {
  d = unique(data.frame(id=doc_id, term=x))
  d = as.data.frame(table(d$term))
  d$Freq = (d$Freq / length(unique(doc_id))) * 100
  d$Freq[match(x, d$Var1)]
}
