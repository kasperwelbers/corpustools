
#' Render text as HTML
#'
#' @param tc a tCorpus object
#' @param context_level Either 'document' or 'sentence'
#' @param text_col The column that is used to create the output text
#' @param meta_columns Meta columns are presented in the HTML as a table. If meta_columns is NULL, all meta columns are used. Specific columns can be selected by giving a character vector with meta column names
#' @param highlight A numeric or logical vector of length tc$n. Words that are TRUE, not NA and not zero will be highlighted. If highlight is a numeric vector, values determine the alpha of the highlight color (must be between 0 and 1).
#' @param scale Like highlight, but uses a color scale. Must be a numeric vector with values ranging from -1 to 1.
#' @param index A vector of length tc$n. Unique values will be used to assign words to categories. NA values can be used to leave words unassigned.
#' @param highlight_col a character vector of lenght 1 with a color name. Determines the color used in highlight
#' @param scale_col a character vector of length 2 with color names, giving the left and right color of the scale
#'
#' @export
html_tagged_text <- function(tc, context_level='document', text_col='word', meta_columns=NULL, highlight=NULL, scale=NULL, index=NULL, highlight_col='yellow', scale_col=c('red','blue')) {
  context_level = match.arg(context_level)

  d = tc$get(c('doc_id', 'word_i', text_col,highlight,scale,index))
  d$context = tc$context(context_level)
  colnames(d)[colnames(d) == text_col] = 'text'
  colnames(d)[colnames(d) == highlight] = 'highlight'
  colnames(d)[colnames(d) == scale] = 'scale'
  colnames(d)[colnames(d) == index] = 'index'

  html = unique(d[,c('doc_id','context')])
  html$header = stringi::stri_paste('<h3>', html$context, '</h3>', sep='')

  ## add meta
  html$header = stringi::stri_paste(html$header, '\n<table>', sep='')
  if (is.null(meta_columns)) meta = tc$meta else tc$get_meta(meta_columns, keep_df=T)
  meta = meta[list(html$doc_id),]
  for (name in colnames(meta)) {
    if (name == 'doc_id') next
    html$header = stringi::stri_paste('\n', html$header, '<tr><th>', name, '</th><td>', as.character(meta[[name]]), '</td></tr>')
  }
  html$header = stringi::stri_paste(html$header, '</table>')

  if ('highlight' %in% colnames(d)) {
    d$highlight = as.numeric(d$highlight)
    d$highlight[is.na(d$highlight)] = 0

    hsvcol = col_to_hsv(rep(highlight_col, sum(d$highlight > 0)))
    d$text[d$highlight > 0] = stringi::stri_paste('<color ', col_to_hsv(highlight), '>',  d$text[d$highlight > 0], '</color>', sep='')
  }


  # render words
  cap <- function(x, n) if (is.null(n)) x else head(x,n)
  tok = info$tokens[info$tokens$aid == doc, ]
  tok = tok[cap(order(tok$id), maxwords), ]
  wa = info$wordassignments[info$wordassignments$aid == doc, c("term", "topic")]
  topics = wa$topic[match(tok$term, wa$term)]
  cat(tagTokens(tok$word, topics))
}

