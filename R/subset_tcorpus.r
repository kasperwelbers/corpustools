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
  data.table::fsort(unique(window_i))
}

#' Subset a tcorpus for a window around the results of search_features()
#'
#' This function works like the search_features() function, but instead of returning a data.frame with the search results, it returns a subset of the tcorpus with the search results, and a specified window arround the results.
#'
#' @return a tCorpus object
subset_query_window <- function(tc, window, keyword=NA, condition=NA, queries=NULL, feature='word', condition_once=F, subset_tokens=NA, subset_meta=NA, verbose=F){
  hits = search_features(tc, keyword=keyword, condition=condition, queries=queries, feature=feature, condition_once=condition_once, subset_tokens=subset_tokens, subset_meta=subset_meta, keep_false_condition=F, only_last_mword=F, verbose=verbose)
  tc = subset_window(tc, i=hits$i, window=window, context_level = 'document')
  tc
}

## subset functions ##

x_filter <- function(ft, min=-Inf, max=Inf, top=NULL, bottom=NULL) {
  select = names(ft[ft >= min & ft <= max])
  if (!is.null(top)) {
    top = names(head(ft[order(-ft)], top))
    select = intersect(select, top)
  }
  if (!is.null(bottom)) {
    bottom = names(head(ft[order(ft)], bottom))
    select = intersect(select, bottom)
  }
  select
}


#' @export
freq_filter <- function(x, min=-Inf, max=Inf, top=NULL, bottom=NULL) {
  if (is(x, 'character')) x = eval(parse(text=x), envir = parent.frame(1))
  freq_table = table(droplevels(x))
  x %in% x_filter(freq_table, min=min, max=max, top=top, bottom=bottom)
}


#' @export
docfreq_filter <- function(x, min=-Inf, max=Inf, top=NULL, bottom=NULL, doc_id=parent.frame()$doc_id) {
  if (is(x, 'character')) x = eval(parse(text=x), envir = parent.frame(1))
  freq_table = unique(data.frame(doc_id=doc_id, x=x))
  freq_table = table(droplevels(freq_table$x))
  x %in% x_filter(freq_table, min=min, max=max, top=top, bottom=bottom)
}

