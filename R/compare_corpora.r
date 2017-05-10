tcorpus_compare <- function(tc_x, tc_y, feature, smooth=0.1, min_over=NULL, min_chi2=NULL, yates_cor=c('auto','yes','no'), x_is_subset=F) {
  comp = dtm_compare(tc_x$dtm(feature, context_labels = F), tc_y$dtm(feature, context_labels = F), smooth=smooth, min_over=min_over, min_chi2=min_chi2, yates_cor=yates_cor, x_is_subset=x_is_subset)
  class(comp) = c('vocabularyComparison', class(comp))
  comp
}

#' Compare two document term matrices
#'
#' @param dtm.x the main document-term matrix
#' @param dtm.y the 'reference' document-term matrix
#' @param smooth the smoothing parameter for Laplace smoothing.
#' @return A data frame with rows corresponding to the terms in dtm and the statistics in the columns
#' @export
dtm_compare <- function(dtm.x, dtm.y=NULL, smooth=1, min_over=NULL, min_chi2=NULL, select_rows=NULL, yates_cor=c('auto','yes','no'), x_is_subset=F) {
  yates_cor = match.arg(yates_cor)
  if (is.null(dtm.y)) {
    if (is.null(select_rows)) stop("either dtm.y or select_rows has to be specified")
    dtm.y = dtm.x[!(rownames(dtm.x) %in% select_rows), ]
    dtm.x = dtm.x[rownames(dtm.x) %in% select_rows, ]
  }
  freqs.x = data.frame(feature=colnames(dtm.x), freq=Matrix::colSums(dtm.x))
  freqs.y = data.frame(feature=colnames(dtm.y), freq=Matrix::colSums(dtm.y))

  f = merge(freqs.x, freqs.y, all=T, by="feature")
  f[is.na(f)] = 0
  if (x_is_subset) f$freq.y = f$freq.y - f$freq.x

  f$freq = f$freq.x + f$freq.y
  f = f[f$freq > 0,]

  f$relfreq.x = (f$freq.x+smooth) / (sum(f$freq.x) + (nrow(f)*smooth))
  f$relfreq.y = (f$freq.y+smooth) / (sum(f$freq.y) + (nrow(f)*smooth))
  f$over = (f$relfreq.x) / (f$relfreq.y)

  freq.notx = sum(f$freq.x) - f$freq.x
  freq.noty = sum(f$freq.y) - f$freq.y

  f$chi2 = calc_chi2(f$freq.x, f$freq.y, freq.notx, freq.noty,
                     correct = yates_cor == 'yes', cochrans_criteria = yates_cor == 'auto')
  #f$good_chi2 = !test_cochran(f$freq.x, f$freq.y, freq.notx, freq.noty)
  if (!is.null(min_over)) f = f[f$over > min_over,]
  if (!is.null(min_chi2)) f = f[f$chi2 > min_chi2,]
  f
}
