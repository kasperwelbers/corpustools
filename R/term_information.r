

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
  dtm_compare(get_dtm(tc_x, feature, context_level, with_labels = F), get_dtm(tc_y, feature, context_level, with_labels = F), smooth=smooth, min.over=min.over, min.chi=min.chi, x_is_subset=x_is_subset)
}

#' Compare two corpora
#'
#' Compare the term use in corpus dtm with a refernece corpus dtm.ref, returning relative frequencies
#' and overrepresentation using various measures.
#'
#' Note that the relative frequency (relfreq) and over-ratio (over) are based on smoothed scores, to prevent the over ratio to be 0 or Inf. Smoothing can be turned of by setting smooth to 0
#'
#' @param dtm.x the main document-term matrix
#' @param dtm.y the 'reference' document-term matrix
#' @param smooth the smoothing parameter for Laplace smoothing. for computing overrepresentation. Currently using Laplace smoothing.
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
             reldocfreq = Matrix::colSums(dtm > 0) / nrow(dtm),
             tfidf = tapply(dtm$v/Matrix::rowSums(dtm)[dtm$i], dtm$j, mean) * log2(nrow(dtm)/Matrix::colSums(dtm > 0)),
             stringsAsFactors=F)
}

rank_unique_rows <- function(d, columns=colnames(d)) {
  ## return a numerical vector of length nrow(d) that indicates how many times the unique combination of column values has occured before (above) in the data.frame. starts at 1
  d = d[,columns,drop=F]
  i = match(apply(d, 1, list), apply(unique(d), 1, list))
  i_ord = order(i)
  rank = local_position(1:nrow(d), i[i_ord])
  rank[match(1:nrow(d), i_ord)]
}

unique_row_id <- function(d, columns=colnames(d)){
  d = d[,columns,drop=F]
  match(apply(d, 1, list), apply(unique(d), 1, list))
}


#' @export
top_features <- function(tc, feature, n=10, group_by=NULL, group_by_meta=NULL, return_long=F){
  if(!is.null(group_by)) group_by = match.arg(group_by, colnames(get_data(tc)), several.ok = T)
  if(!is.null(group_by)) group_by_meta = match.arg(group_by_meta, colnames(get_meta(tc)), several.ok = T)

  group_df = NULL
  if(!is.null(group_by)) group_df = get_data(tc, columns=group_by, as_data_frame = T)
  if(!is.null(group_by_meta)){
    if(is.null(group_df)) {
      group_df = get_meta(tc, columns=group_by_meta, as_data_frame = T, per_token=T)
    } else cbind(group_df, get_meta(tc, columns=group_by_meta, as_data_frame = T, per_token=T))
  }
  if(is.null(group_df)) group_df = data.frame(group=rep('tcorpus', nrow(get_data(tc))))

  ## function for ddply
  get_top_freq <- function(d, n, feature){
    x = d[[feature]]
    scores = data.frame(table(x))
    colnames(scores) = c(feature, 'freq')
    scores = scores[order(-scores$freq),]
    scores = head(scores, n)
    scores = scores[scores$freq > 0,]
    scores$rank = 1:nrow(scores)
    scores
  }

  break_cols = colnames(group_df)
  group_df[[feature]] = get_column(tc, feature)
  scores = ddply(group_df, break_cols, .fun = get_top_freq, n=n, feature=feature)

  if(!return_long) {
    scores = scores[,!colnames(scores) == 'freq', drop=F]
    scores = dcast(scores, ... ~ rank, value.var=feature)
    colnames(scores)[!colnames(scores) %in% break_cols] = paste('rank', colnames(scores)[!colnames(scores) %in% break_cols], sep='_')
  }
  scores
}

