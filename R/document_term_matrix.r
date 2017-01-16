#' Create a document term matrix from a tCorpus
#'
#' @param tc
#' @param feature
#' @param context_level
#'
#' @export
get_dtm <- function(tc, feature, context_level=c('document','sentence')){
  context_level = match.arg(context_level)
  i = get_context(tc, context_level)
  feature = droplevels(get_column(tc, feature))
  notNA = !is.na(feature)
  m = Matrix::spMatrix(length(levels(i)), length(levels(feature)),
                       as.numeric(i)[notNA], as.numeric(feature)[notNA],
                       rep(1, sum(notNA)))
  dimnames(m) = list(levels(i), levels(feature))
  as(as(m,'dgCMatrix'), 'dgTMatrix')
}

