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
#' @return a tCorpus object
subset_query_window <- function(tc, window, keyword=NA, condition=NA, queries=NULL, feature='word', condition_once=F, subset_tokens=NA, subset_meta=NA, verbose=F){
  hits = search_features(tc, keyword=keyword, condition=condition, queries=queries, feature=feature, condition_once=condition_once, subset_tokens=subset_tokens, subset_meta=subset_meta, keep_false_condition=F, only_last_mword=F, verbose=verbose)
  tc = subset_window(tc, i=hits$i, window=window, context_level = 'document')
  tc
}

subset_i <- function(tc, subset=NA, subset_meta=NA, inverse=F){
  ## subset and subset_meta can be either a call or a character vector of length 1
  subset = if (is(substitute(subset), 'call')) deparse(substitute(subset)) else subset
  subset_meta = if (is(substitute(subset_meta), 'call')) deparse(substitute(subset_meta)) else subset_meta

  n = nrow(tc$data())
  if (is.na(subset)){
    r = NULL
  } else {
    e = parse(text=as.character(subset))
    r = eval(e, tc$data(), parent.frame())
    if (inverse) r=!r
    r = which(r)
  }

  n_meta = nrow(tc$meta())
  if (is.na(subset_meta)){
    r_meta = NULL
  } else {
    e_meta = parse(text=as.character(subset_meta))
    r_meta = (1:n_meta)[eval(e_meta, tc$meta(), parent.frame())]
    if (length(r_meta) > 0) {
      r_meta = 1:n_meta %in% r_meta
      r_meta = r_meta[match(tc$data('doc_id'), tc$meta('doc_id'))] ## extend to length()
      if (inverse) r_meta=!r_meta
      r_meta = which(r_meta)
    }
  }

  if (!is.null(r) & !is.null(r_meta)) r = intersect(r, r_meta)
  if (is.null(r) & is.null(r_meta)) r = 1:n
  if (!is.null(r) & is.null(r_meta)) r = r
  if (is.null(r) & !is.null(r_meta)) r = r_meta
  as.numeric(na.omit(r))
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
