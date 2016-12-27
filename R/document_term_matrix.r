#' Create a document term matrix from a tCorpus
#'
#' @param tc
#' @param feature
#' @param context_level
#'
#' @export
get_dtm <- function(tc, feature, context_level=c('document','sentence')){
  context_level = match.arg(context_level)
  if(context_level == 'document') {
    i = as.numeric(tc@data$doc_id)
    rnames = levels(tc@data$doc_id)
  }
  if(context_level == 'sentence'){
    if(!'sent_i' %in% colnames(tc@data)) stop('Sentence level not possible, since no sentence information is available. To enable sentence level analysis, use split_sentences = T in "create_tcorpus()" or specify sent_i_col in "tokens_to_tcorpus()"')
    i = tc@sent_i
    rnames = levels(tc@data$sent_i)
  }
  j = as.numeric(tc@data[[feature]])
  m = Matrix::spMatrix(max(i), max(j), i, j, rep(1, length(i)))
  dimnames(m) = list(rnames, levels(tc@data[[feature]]))
  m
}
