
#' Compare tCorpus vocabulary to that of another (reference) tCorpus
#'
#' @param tc a \code{\link{tCorpus}}
#' @param tc_y the reference tCorpus
#' @param feature the column name of the feature that is to be compared
#' @param smooth Laplace smoothing is used for the calculation of the probabilities. Here you can set the added (pseuocount) value.
#' @param min_ratio threshold for the ratio value, which is the ratio of the relative frequency of a term in dtm.x and dtm.y
#' @param min_chi2 threshold for the chi^2 value
#' @param yates_cor mode for using yates correctsion in the chi^2 calculation. Can be turned on ("yes") or off ("no"), or set to "auto", in which case cochrans rule is used to determine whether yates' correction is used.
#' @param is_subset Specify whether tc is a subset of tc_y. In this case, the term frequencies of tc will be subtracted from the term frequencies in tc_y
#' @param what choose whether to compare the frequency ("freq") of terms, or the document frequency ("docfreq"). This also affects how chi^2 is calculated, comparing either freq relative to vocabulary size or docfreq relative to corpus size (N)
#'
#' @return A vocabularyComparison object
#' @export
#' @examples
#' tc = create_tcorpus(sotu_texts, doc_column = 'id')
#'
#' tc$preprocess('token', 'feature', remove_stopwords = TRUE, use_stemming = TRUE)
#'
#' obama = tc$subset_meta(president == 'Barack Obama', copy=TRUE)
#' bush = tc$subset_meta(president == 'George W. Bush', copy=TRUE)
#'
#' comp = compare_corpus(tc, bush, 'feature')
#' comp = comp[order(-comp$chi),]
#' head(comp)
#' \donttest{
#' plot(comp)
#' }
compare_corpus <- function(tc, tc_y, feature, smooth=0.1, min_ratio=NULL, min_chi2=NULL, is_subset=F, yates_cor=c('auto','yes','no'), what=c('freq','docfreq','cooccurrence')){
  if (is_subset && tc$n > tc_y$n) stop('tCorpus x (the one calling the method) cannot be a subset of tCorpus y, because it has more tokens')
  what = match.arg(what)
  tcorpus_compare(tc, tc_y, feature, smooth=smooth, min_ratio=min_ratio, min_chi2=min_chi2, yates_cor=yates_cor, x_is_subset=is_subset, what=what)
}

#' Compare vocabulary of a subset of a tCorpus to the rest of the tCorpus
#'
#' @param tc a \code{\link{tCorpus}}
#' @param feature the column name of the feature that is to be compared
#' @param subset_x an expression to subset the tCorpus. The vocabulary of the subset will be compared to the rest of the tCorpus
#' @param subset_meta_x like subset_x, but using using the meta data
#' @param query_x like subset_x, but using a query search to select documents (see \link{search_contexts})
#' @param query_feature if query_x is used, the column name of the feature used in the query search.
#' @param smooth Laplace smoothing is used for the calculation of the probabilities. Here you can set the added (pseuocount) value.
#' @param min_ratio threshold for the ratio value, which is the ratio of the relative frequency of a term in dtm.x and dtm.y
#' @param min_chi2 threshold for the chi^2 value
#' @param yates_cor mode for using yates correctsion in the chi^2 calculation. Can be turned on ("yes") or off ("no"), or set to "auto", in which case cochrans rule is used to determine whether yates' correction is used.
#' @param what choose whether to compare the frequency ("freq") of terms, or the document frequency ("docfreq"). This also affects how chi^2 is calculated, comparing either freq relative to vocabulary size or docfreq relative to corpus size (N)
#'
#' @return A vocabularyComparison object
#' @export
#' @examples
#' tc = create_tcorpus(sotu_texts, doc_column = 'id')
#'
#' tc$preprocess('token', 'feature', remove_stopwords = TRUE, use_stemming = TRUE)
#'
#' comp = compare_subset(tc, 'feature', subset_meta_x = president == 'Barack Obama')
#' comp = comp[order(-comp$chi),]
#' head(comp)
#' \donttest{
#' plot(comp)
#' }
#'
#' comp = compare_subset(tc, 'feature', query_x = 'terroris*')
#' comp = comp[order(-comp$chi),]
#' head(comp, 10)
compare_subset <- function(tc, feature, subset_x=NULL, subset_meta_x=NULL, query_x=NULL, query_feature='token', smooth=0.1, min_ratio=NULL, min_chi2=NULL, yates_cor=c('auto','yes','no'), what=c('freq','docfreq','cooccurrence')){
  subset_x = tc$eval(substitute(subset_x), parent.frame())
  subset_meta_x = tc$eval_meta(substitute(subset_meta_x), parent.frame())
  what = match.arg(what)

  if(is.null(subset_x) && is.null(subset_meta_x) & is.null(query_x)) stop("at least one of subset_x, subset_meta_x or query_x has to be specified")
  if(!is.null(subset_x) | !is.null(subset_meta_x)) {
    .subset_x = subset_x
    .subset_meta_x = subset_meta_x
    tc_x = tc$subset(subset=.subset_x, subset_meta = .subset_meta_x, copy=T)
  }
  if(!is.null(query_x)) tc_x = tc$subset_query(query_x, feature=query_feature, copy=T)

  comp = compare_corpus(tc_x, tc, feature=feature, smooth=smooth, min_ratio=min_ratio, min_chi2=min_chi2, yates_cor=yates_cor, is_subset=T, what=what)
  comp
}

########################
########################

tcorpus_compare <- function(tc_x, tc_y, feature, smooth=0.1, min_ratio=NULL, min_chi2=NULL, yates_cor=c('auto','yes','no'), x_is_subset=F, what=c('freq','docfreq', 'cooccurrence')) {
  ## it would be more memory efficient to not split up the tcorpus, but return a dtm and split that up (possibly within the dtm_compare function)
  ## so add an alternative route if tc_y == NULL, where an addition selection parameter is used
  comp = dtm_compare(get_dtm(tc_x, feature, context_labels = F), get_dtm(tc_y, feature, context_labels = F), smooth=smooth, min_ratio=min_ratio, min_chi2=min_chi2, yates_cor=yates_cor, x_is_subset=x_is_subset, what=what)
  class(comp) = c('vocabularyComparison', class(comp))
  comp
}

#' Compare two document term matrices
#'
#' @param dtm.x the main document-term matrix
#' @param dtm.y the 'reference' document-term matrix
#' @param min_ratio threshold for the ratio value, which is the ratio of the relative frequency of a term in dtm.x and dtm.y
#' @param min_chi2 threshold for the chi^2 value
#' @param select_rows Alternative to using dtm.y. Has to be a vector with rownames, by which
#' @param yates_cor mode for using yates correctsion in the chi^2 calculation. Can be turned on ("yes") or off ("no"), or set to "auto", in which case cochrans rule is used to determine whether yates' correction is used.
#' @param x_is_subset Specify whether dtm.x is a subset of dtm.y. In this case, the term frequencies of dtm.x will be subtracted from the term frequencies in dtm.y
#' @param smooth Laplace smoothing is used for the calculation of the probabilities. Here you can set the added (pseuocount) value.
#' @param what choose whether to compare the frequency ("freq") of terms, or the document frequency ("docfreq"). This also affects how chi^2 is calculated, comparing either freq relative to vocabulary size or docfreq relative to corpus size (N)
#'
#' @return A data frame with rows corresponding to the terms in dtm and the statistics in the columns
dtm_compare <- function(dtm.x, dtm.y=NULL, smooth=0.1, min_ratio=NULL, min_chi2=NULL, select_rows=NULL, yates_cor=c('auto','yes','no'), x_is_subset=F, what=c('freq','docfreq','cooccurrence')) {
  what = match.arg(what)
  yates_cor = match.arg(yates_cor)
  if (is.null(dtm.y)) {
    if (is.null(select_rows)) stop("either dtm.y or select_rows has to be specified")
    dtm.y = dtm.x[!(rownames(dtm.x) %in% select_rows), ]
    dtm.x = dtm.x[rownames(dtm.x) %in% select_rows, ]
  }

  freqs.x = get_dtm_freq(dtm.x, what=what)
  freqs.y = get_dtm_freq(dtm.y, what=what)

  f = merge(freqs.x, freqs.y, all=T, by="feature")
  f[is.na(f)] = 0
  if (x_is_subset) f$freq.y = f$freq.y - f$freq.x

  f$freq = f$freq.x + f$freq.y
  f = f[f$freq > 0,]

  if (what == 'freq') {
    f$p.x = (f$freq.x+smooth) / (sum(f$freq.x) + (nrow(f)*smooth))
    f$p.y = (f$freq.y+smooth) / (sum(f$freq.y) + (nrow(f)*smooth))

    freq.notx = sum(f$freq.x) - f$freq.x
    freq.noty = sum(f$freq.y) - f$freq.y
  } else {
    nrow_y = if (x_is_subset) nrow(dtm.y) - nrow(dtm.x) else nrow(dtm.y)
    f$p.x = (f$freq.x+smooth) / (nrow(dtm.x) + smooth)
    f$p.y = (f$freq.y+smooth) / (nrow_y + smooth)

    freq.notx = rep(nrow(dtm.x), nrow(f))
    freq.noty = rep(nrow_y, nrow(f))
  }

  f$ratio = (f$p.x) / (f$p.y)
  f$chi2 = calc_chi2(f$freq.x, f$freq.y, freq.notx, freq.noty,
                     correct = yates_cor == 'yes', cochrans_criteria = yates_cor == 'auto')
  #f$good_chi2 = !test_cochran(f$freq.x, f$freq.y, freq.notx, freq.noty)
  if (!is.null(min_ratio)) f = f[f$ratio > min_ratio,]
  if (!is.null(min_chi2)) f = f[f$chi2 > min_chi2,]
  f
}

get_dtm_freq <- function(dtm, what) {
  if (what == 'freq') {
    freqs = data.frame(feature=colnames(dtm), freq=Matrix::colSums(dtm))
  }
  if (what == 'docfreq') {
    freqs = data.frame(feature=colnames(dtm), freq=Matrix::colSums(dtm > 0))
  }
  if (what == 'cooccurrence') {
    dtm = dtm[,sort(colnames(dtm))]
    freqs = Matrix::summary(crossprod(dtm > 0))
    freqs = freqs[!freqs[,1] == freqs[,2],]
    freqs = data.frame(feature = stringi::stri_paste(colnames(dtm)[freqs[,1]], colnames(dtm)[freqs[,2]], sep=' & '),
                       freq = freqs[,3])
  }
  freqs
}

