


#' Reconstruct original texts
#' 
#' If the tCorpus was created with remember_spaces = T, you can rebuild the original texts.
#'
#' @param tc   A tCorpus, created with \code{\link{create_tcorpus}}, with remember_spaces = TRUE
#'
#' @return  A data.table with the text fields and meta fields as columns.
#' @export
#'
#' @examples
#' tc = create_tcorpus(sotu_texts, doc_column='id')
#' untokenize(tc)
untokenize <- function(tc) {
  tokens = tc$tokens
  if (!'field' %in% colnames(tokens)) tokens$field = 'text'
  if (!'space' %in% colnames(tokens)) tokens$space = ' '
  d = tokens[, list(text = concatenate_text(token,space)),
                by=c('doc_id','field')]
  d = data.table::dcast(d, doc_id ~ field, value.var='text')
  merge(d, tc$meta, by='doc_id')
}

#' Export span annotations
#' 
#' Export columns from a tCorpus as span annotations (annotations over a span of text).
#' The annotations are returned as a data.table where each row is an annotation, with columns:
#' doc_id, variable, value, field, offset, length and text. The key purpose is that these span annotations
#' are linked to exact character positions in the text. This also means that this function can 
#' only be used if position information is available (i.e. if remember_spaces=T was used when creating the tCorpus)  
#' 
#' Note that if there are spans with gaps in them (e.g. based on proximity queries), they are split into different annotations.
#' Thus some information can be lost. 
#'
#' @param tc              A tCorpus, created with \code{\link{create_tcorpus}}, where remember_spaces must have been set to TRUE
#' @param variables       A character vector with variables (columns in tc$tokens) to export
#'
#' @return A data.table where each row is a span annotation, with columns: doc_id, variable, value, field, offset, length, text
#' @export
#'
#' @examples
#' tc = create_tcorpus(sotu_texts, c('president','text'))
#' tc$code_features(c('war# war peace', 'us being# <(i we) (am are)>'))
#' 
#' export_span_annotations(tc, 'code')
export_span_annotations <- function(tc, variables) {
  tokens = tc$tokens
  if (!variable %in% colnames(tokens)) stop('selected variable is not a column im the tokens data.frame')
  if (!'start' %in% colnames(tokens)) stop('can only export annotations if tokens has a column with start positions')
  if (!'end' %in% colnames(tokens)) stop('can only export annotations if tokens has a column with end positions')
  if (!'space' %in% colnames(tokens)) stop('can only export annotations if tokens has a column with space positions')
  
  annotations = list()
  for (variable in variables) {
    ann = tokens[!is.na(tokens[[variable]]),]
    if (!'field' %in% colnames(ann)) ann$field = 'text'
    
    n = nrow(ann)
    ann$new_span = c(T, ann$token_id[2:n] != ann$token_id[1:(n-1)] +1)
    ann$span_id = cumsum(ann$new_span)  ## can span across documents, but thats taken care of in the aggregateion
    ann = ann[,list(offset = first(start), 
                    length = last(end) - first(start) + 1, 
                    text = concatenate_text(token,space)),
              by=c('doc_id','field','span_id',variable)]
    
    data.table::setnames(ann, variable, 'value')
    ann$variable = variable
    annotations[['']] = ann[,c('doc_id','variable','value','field','offset','length','text')]
  }
  data.table::rbindlist(annotations)
}

concatenate_text <- function(token, space) {
  token = as.character(token)
  space = as.character(space)
  space[length(space)] = ''
  stringi::stri_paste(stringi::stri_paste(token,space, sep=''), collapse='')
}

