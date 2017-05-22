tcorpus_compare <- function(tc_x, tc_y, feature, smooth=0.1, min_over=NULL, min_chi2=NULL, yates_cor=c('auto','yes','no'), x_is_subset=F, what=c('freq','docfreq')) {
  ## it would be more memory efficient to not split up the tcorpus, but return a dtm and split that up (possibly within the dtm_compare function)
  ## so add an alternative route if tc_y == NULL, where an addition selection parameter is used
  comp = dtm_compare(tc_x$dtm(feature, context_labels = F), tc_y$dtm(feature, context_labels = F), smooth=smooth, min_over=min_over, min_chi2=min_chi2, yates_cor=yates_cor, x_is_subset=x_is_subset, what=what)
  class(comp) = c('vocabularyComparison', class(comp))
  comp
}

#' Compare two document term matrices
#'
#' @param dtm.x the main document-term matrix
#' @param dtm.y the 'reference' document-term matrix
#' @param min_over threshold for the over value, which is the ratio of the relative frequency of a term in dtm.x and dtm.y
#' @param min_chi2 threshold for the chi^2 value
#' @param select_rows Alternative to using dtm.y. Has to be a vector with rownames, by which
#' @param yates_cor mode for using yates correctsion in the chi^2 calculation. Can be turned on ("yes") or off ("no"), or set to "auto", in which case cochrans rule is used to determine whether yates' correction is used.
#' @param x_is_subset Specify whether dtm.x is a subset of dtm.y. In this case, the term frequencies of dtm.x will be subtracted from the term frequencies in dtm.y
#' @param smooth the smoothing parameter for Laplace smoothing.
#' @param what choose whether to compare the frequency ("freq") of terms, or the document frequency ("docfreq"). This also affects how chi^2 is calculated, comparing either freq relative to vocabulary size or docfreq relative to corpus size (N)
#'
#' @return A data frame with rows corresponding to the terms in dtm and the statistics in the columns
#' @export
dtm_compare <- function(dtm.x, dtm.y=NULL, smooth=1, min_over=NULL, min_chi2=NULL, select_rows=NULL, yates_cor=c('auto','yes','no'), x_is_subset=F, what=c('freq','docfreq')) {
  what = match.arg(what)
  yates_cor = match.arg(yates_cor)
  if (is.null(dtm.y)) {
    if (is.null(select_rows)) stop("either dtm.y or select_rows has to be specified")
    dtm.y = dtm.x[!(rownames(dtm.x) %in% select_rows), ]
    dtm.x = dtm.x[rownames(dtm.x) %in% select_rows, ]
  }
  if (what == 'docfreq') {
    dtm.x = dtm.x > 0
    dtm.y = dtm.y > 0
  }
  freqs.x = data.frame(feature=colnames(dtm.x), freq=Matrix::colSums(dtm.x))
  freqs.y = data.frame(feature=colnames(dtm.y), freq=Matrix::colSums(dtm.y))

  f = merge(freqs.x, freqs.y, all=T, by="feature")
  f[is.na(f)] = 0
  if (x_is_subset) f$freq.y = f$freq.y - f$freq.x

  f$freq = f$freq.x + f$freq.y
  f = f[f$freq > 0,]

  if (what == 'freq') {
    f$relfreq.x = (f$freq.x+smooth) / (sum(f$freq.x) + (nrow(f)*smooth))
    f$relfreq.y = (f$freq.y+smooth) / (sum(f$freq.y) + (nrow(f)*smooth))

    freq.notx = sum(f$freq.x) - f$freq.x
    freq.noty = sum(f$freq.y) - f$freq.y
  } else {
    nrow_y = if (x_is_subset) nrow(dtm.y) - nrow(dtm.x) else nrow(dtm.y)
    f$relfreq.x = (f$freq.x+smooth) / (nrow(dtm.x) + smooth)
    f$relfreq.y = (f$freq.y+smooth) / (nrow_y + smooth)

    freq.notx = rep(nrow(dtm.x), nrow(f))
    freq.noty = rep(nrow_y, nrow(f))
  }
  f$over = (f$relfreq.x) / (f$relfreq.y)
  f$chi2 = calc_chi2(f$freq.x, f$freq.y, freq.notx, freq.noty,
                     correct = yates_cor == 'yes', cochrans_criteria = yates_cor == 'auto')
  #f$good_chi2 = !test_cochran(f$freq.x, f$freq.y, freq.notx, freq.noty)
  if (!is.null(min_over)) f = f[f$over > min_over,]
  if (!is.null(min_chi2)) f = f[f$chi2 > min_chi2,]
  f
}

