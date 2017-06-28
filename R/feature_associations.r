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
