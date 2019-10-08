#' Compare the documents in a dtm with a sliding window over time
#'
#' Given a document-term matrix (DTM) and corresponding document meta data, calculates the document similarities over time using with a sliding window.
#'
#' The meta data.frame should have a column containing document id's that match the rownames of the DTM (i.e. document names) and should have a column indicating the publication time.
#' By default these columns should be labeled "document_id" and "date", but the column labels can also be set using the `doc_col` and `date_col` parameters.
#' Any other columns will automatically be included as document meta information in the output.
#'
#' The calculation of document similarity is performed using a vector space model approach.
#' Inner-product based similarity measures are used, such as cosine similarity.
#' It is recommended to weight the DTM beforehand, for instance using Term frequency-inverse document frequency (tf.idf)
#'
#' @param dtm A document-term matrix in the tm \link[tm]{DocumentTermMatrix} class. It is recommended to weight the DTM beforehand, for instance using \link[tm]{weightTfIdf}.
#' @param meta A data.frame where rows are documents and columns are document meta information.
#' Should at least contain a column with the document date. The date column should be intepretable with \link[base]{as.POSIXct}, and its label is specified in the `date_col` argument.
#' @param date_col The name of the document date column in the `meta` data.frame. If given, only documents within the given date window (specified in hour_window) will be compared
#' @param meta_cols The names of other columsn in the `meta` data.frame. If given, only documents with identical values in these columns will be compared.
#' @param hour_window A vector of length 1 or 2. If length is 1, the same value is used for the left and right side of the window. If length is 2, the first and second value determine the left and right side. For example, the value 12 will compare each document to all documents between the previous and next 12 hours, and c(-10, 36) will compare each document to all documents between the previous 10 and the next 36 hours.
#' @param measure the measure that should be used to calculate similarity/distance/adjacency. Currently supports the symmetrical measure "cosine" (cosine similarity), and the assymetrical measures "overlap_pct" (percentage of term scores in the document that also occur in the other document).
#' @param min_similarity a threshold for similarity. lower values are deleted. Set to 0.1 by default.
#' @param n_topsim An alternative or additional sort of threshold for similarity. Only keep the [n_topsim] highest similarity scores for x. Can return more than [n_topsim] similarity scores in the case of duplicate similarities.
#' @param only_from A vector with names/ids of documents (dtm rownames), or a logical vector that matches the rows of the dtm. Use to compare only these documents to other documents.
#' @param only_to A vector with names/ids of documents (dtm rownames), or a logical vector that matches the rows of the dtm. Use to compare other documents to only these documents.
#' @param only_complete_window if True, only compare articles (x) of which a full window of reference articles (y) is available. Thus, for the first and last [window.size] days, there will be no results for x.
#' @param pvalue If used, transform the similarity score to a p-value. The value is reversed, so that higher means more similar (and thus the min.similarity still makes sense). Currently supports "normal" and "lognormal" distribution, and the uniform distribution used in the "disparity" filter (see Serrano et al.). Also "nz_normal" and "nz_lognormal" can be used to only consider the nonzero values.
#' @param return_as Detemine whether output is returned as an "edgelist", "igraph" network or sparse "matrix'.
#' @param verbose If TRUE, report progress
#'
#' @return A network/graph in the \link[igraph]{igraph} class
#' @export
#'
#' @examples
#' \dontrun{
#' data(dtm)
#' data(meta)
#'
#' dtm = tm::weightTfIdf(dtm)
#' g = compare_documents_dtm(dtm, meta, hour_window = c(0.1, 36))
#'
#' vcount(g) # number of documents, or vertices
#' ecount(g) # number of document pairs, or edges
#'
#' head(igraph::get.data.frame(g, 'vertices'))
#' head(igraph::get.data.frame(g, 'edges'))
#' }
compare_documents_dtm <- function(dtm, meta=NULL, date_col=NULL, meta_cols=NULL, hour_window=24, measure=c('cosine','overlap_pct'), min_similarity=0, n_topsim=NULL, only_from=NULL, only_to=NULL, only_complete_window=FALSE, pvalue = c("none", "normal", "lognormal", "nz_normal", "nz_lognormal", "disparity"), return_as= c("igraph", "edgelist", "matrix"), verbose=T, ...){
  .Deprecated(msg = "The compare_documents_dtm function is deprecated, because corpustools now uses the (very similar) compare_documents function from the RNewsflow package")

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

  RNewsflow::newsflow.compare(dtm=dtm, date.var = date_col, group.var = group_col, hour.window = hour_window, measure=measure, min.similarity=min_similarity, n.topsim = n_topsim, only.from = only_from, only.to = only_to, only.complete.window = only_complete_window, pvalue=pvalue, return_as=return_as, verbose=verbose)

}

