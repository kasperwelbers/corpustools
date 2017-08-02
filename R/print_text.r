#' Print tokens as text
#'
#' @section Usage:
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{
#' read_text(doc_id, column='token', meta_columns = self$meta_names)}
#'
#' @param doc_id The doc_ids of the documents to be printed.
#' @param column The name of the column from which the text is printed.
#' @param meta_columns The meta data that is printed at the top of each text.
#'
#' @name tCorpus$read_text
#' @aliases read_text.tCorpus
tCorpus$set('public', 'read_text', function(doc_id, column='token', meta_columns = self$meta_names) {
  d = self$get(c('doc_id', column), doc_id = doc_id)
  d = split(d[[column]], f = d$doc_id)
  texts = stringi::stri_paste_list(d, sep = ' ')

  if (length(meta_columns) > 0) {
    meta = self$get_meta(meta_columns, doc_id = doc_id, keep_df=T)
    header = ''
    for (j in 1:ncol(meta)) {
      meta_field = stringi::stri_paste(colnames(meta)[j], meta[[j]], sep=': ')
      header = if (j == 1) meta_field else paste(header, meta_field, sep=', ')
    }
    texts = paste(header, texts, sep='\n\n')
  }

  texts = paste(texts, collapse = '\n------------\n\n')
  texts = pretty_text_paste(texts)
  cat(texts)
  invisible(texts)
})

