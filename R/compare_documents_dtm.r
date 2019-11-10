compare_documents_dtm <- function(dtm, meta=NULL, date_col=NULL, meta_cols=NULL, hour_window=24, measure=c('cosine','overlap_pct'), min_similarity=0, n_topsim=NULL, only_from=NULL, only_to=NULL, only_complete_window=FALSE, pvalue = c("none", "normal", "lognormal", "nz_normal", "nz_lognormal", "disparity"), return_as= c("igraph", "edgelist", "matrix"), verbose=T){
  .Deprecated(msg = "The compare_documents_dtm function is deprecated, because corpustools now uses the (very similar) compare_documents function from the RNewsflow package")
  pvalue = match.arg(pvalue)
  if (pvalue == 'none') {
    pvalue = 'disparity'
    max_p=1
  } else {
    max_p = 1-min_similarity
  }

  if (is.null(meta)) {
    if (!methods::is(dtm, 'dfm')) stop('meta can only be NULL if dtm is a quanteda dfm class')
  }
  if (!methods::is(dtm, 'dfm')) {
    dtm = quanteda::as.dfm(dtm)
    quanteda::docvars(dtm) = meta
  }

  if (!is.null(meta_cols)) {
    meta_cols = subset(quanteda::docvars(dtm), select=meta_cols)
    quanteda::docvars(dtm, 'group_col') = do.call(paste, args=c(meta_cols, list(sep='__')))
    group_col = 'group_col'
  } else group_col = NULL

  if (is.null(date_col)) {
    quanteda::docvars(dtm, 'date_var') = as.POSIXct(Sys.time())
    hour_window = c(-1,1)
    date_col = 'date_var'
  }

  if (length(hour_window) == 1) hour_window = c(-hour_window, hour_window)

  RNewsflow::newsflow.compare(dtm=dtm, date.var = date_col, group.var = group_col, hour.window = hour_window, measure=measure, min.similarity=min_similarity, n.topsim = n_topsim, only.from = only_from, only.to = only_to, only.complete.window = only_complete_window, pvalue=pvalue, max_p=max_p, return_as=return_as, verbose=verbose)

}

