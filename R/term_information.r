#' Compare two corpora
#'
#' Compare the term use in corpus dtm with a refernece corpus dtm.ref, returning relative frequencies
#' and overrepresentation using various measures
#'
#' @param dtm.x the main document-term matrix
#' @param dtm.y the 'reference' document-term matrix
#' @param smooth the smoothing parameter for computing overrepresentation
#' @return A data frame with rows corresponding to the terms in dtm and the statistics in the columns
#' @export
dtm_compare <- function(dtm.x, dtm.y=NULL, smooth=1, min.over=NULL, min.chi=NULL, select.rows=NULL, x_is_subset=F) {
  if (is.null(dtm.y)) {
    if(is.null(select_rows)) stop("either dtm.y or select.rows has to be specified")
    dtm.y = dtm.x[!(rownames(dtm.x) %in% select.rows), ]
    dtm.x = dtm.x[rownames(dtm.x) %in% select.rows, ]
  }
  freqs = data.frame(term=colnames(dtm.x), termfreq=Matrix::colSums(dtm.x))
  freqs.rel = data.frame(term=colnames(dtm.y), termfreq=Matrix::colSums(dtm.y))
  f = merge(freqs, freqs.rel, all=T, by="term")
  if(x_is_subset) f$termfreq.y = f$termfreq.y - f$termfreq.x
  f[is.na(f)] = 0
  f = f[f$termfreq.x + f$termfreq.y > 0,]
  f$termfreq = f$termfreq.x + f$termfreq.y
  f$relfreq.x = (f$termfreq.x+smooth) / (sum(freqs$termfreq) + (nrow(freqs)*smooth))
  f$relfreq.y = (f$termfreq.y+smooth) / (sum(freqs.rel$termfreq) + (nrow(freqs.rel)*smooth))
  f$over = (f$relfreq.x) / (f$relfreq.y)
  f$chi = calc_chi2(f$termfreq.x, f$termfreq.y, sum(f$termfreq.x) - f$termfreq.x, sum(f$termfreq.y) - f$termfreq.y)
  if(!is.null(min.over)) f = f[f$over > min.over,]
  if(!is.null(min.chi)) f = f[f$chi > min.chi,]
  f
}

#' Title
#'
#' @param tc_x
#' @param tc_y
#' @param feature
#' @param context_level
#' @param smooth
#' @param min.over
#' @param min.chi
#' @param x_is_subset
#'
#' @return
#' @export
#'
#' @examples
tcorpus_compare <- function(tc_x, tc_y, feature, context_level=c('document','sentence'), smooth=1, min.over=NULL, min.chi=NULL, x_is_subset=F) {
  dtm_compare(get_dtm(tc_x, feature, context_level), get_dtm(tc_y, feature, context_level), smooth=smooth, min.over=min.over, min.chi=min.chi, x_is_subset=x_is_subset)
}

#' Compute some useful corpus statistics for a dtm
#'
#' Compute a number of useful statistics for filtering words: term frequency, idf, etc.
#'
#' @param dtm a document term matrix (e.g. the output of \code{\link{dtm.create}})
#' @return A data frame with rows corresponding to the terms in dtm and the statistics in the columns
#' @export
term.statistics <- function(tc, feature, context_level=c('document','sentence')) {
  dtm = get_dtm(tc, feature, context_level=context_level)
  dtm = dtm[Matrix::rowSums(dtm) > 0, Matrix::colSums(dtm) > 0]    # get rid of empty rows/columns
  vocabulary = colnames(dtm)
  data.frame(term = as.character(vocabulary),
             characters = nchar(vocabulary),
             number = grepl("[0-9]", vocabulary),
             nonalpha = grepl("\\W", vocabulary),
             termfreq = Matrix::colSums(dtm),
             docfreq = Matrix::colSums(dtm > 0),
             reldocfreq = Matrix::colSums(dtm > 0) / nDocs(dtm),
             tfidf = tapply(dtm$v/Matrix::rowSums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/Matrix::colSums(dtm > 0)),
             stringsAsFactors=F)
}
