unlist_to_df <- function(l, ids=1:length(l), global_position=F){
  len = sapply(l, length)
  filter = len > 0
  if (global_position){
    position = 1:sum(len)
  } else {
    position = unlist(sapply(len[filter], function(l) 1:l, simplify = F))
  }
  list(id = rep(ids[filter], len[filter]),
       position = position,
       value = unlist(l[filter]))
}

col_to_hsv <- function(col, alpha=1) {
  sapply(1:length(col), function(i) {
    hsv_col = rgb2hsv(col2rgb(col[i]))
    hsv(hsv_col[1], hsv_col[2], hsv_col[3], alpha=alpha[i])
  })
}

col_to_span_rgb <- function(col, alpha=1) {
  rgb_col = col2rgb(col)
  sprintf("rgba(%s, %s, %s, %s)", rgb_col[1,], rgb_col[2,], rgb_col[3,], alpha)
}


#' Render text as HTML
#'
#' @param d a data.frame with tokens
#' @param doc_col The column containing the document id
#' @param text_col The column that is used to create the output text
#' @param meta A data.frame with a doc_id columns (same name as doc_col). Meta columns are presented in the HTML as a table.
#' @param highlight A numeric or logical vector. Words that are TRUE, not NA and not zero will be highlighted. If highlight is a numeric vector, values determine the alpha of the highlight color (must be between 0 and 1).
#' @param scale Like highlight, but uses a color scale. Must be a numeric vector with values ranging from -1 to 1.
#' @param group A vector of length tc$n. Unique values will be used to assign words to categories. NA values can be used to leave words unassigned.
#' @param highlight_col a character vector of lenght 1 with a color name. Determines the color used in highlight
#' @param scale_col a character vector of length 2 with color names, giving the left and right color of the scale
#' @param ... additional arguments to templates
#'
#' @export
html_tagged_text <- function(d, doc_col='doc_id', text_col='word', meta=NULL, highlight=NULL, scale=NULL, group=NULL, highlight_col='yellow', scale_col=c('red','blue'), ...) {
  d = as.data.frame(d)

  ids = as.character(unique(d[[doc_col]]))
  html = data.frame(doc_id = ids,
                    header = stringi::stri_paste('<h3>', as.character(ids), '</h3>', sep=''))

  ## add meta
  html$header = stringi::stri_paste(html$header, '\n<table>', sep='')
  if (!is.null(meta)) {
    meta = as.data.frame(meta)
    meta = meta[match(html$doc_id, meta[[doc_col]]),]
    for (name in colnames(meta)) {
      if (name == 'doc_id') next
      html$header = stringi::stri_paste('\n', html$header, '<tr><th>', name, '</th><td>', as.character(meta[[name]]), '</td></tr>')
    }
  }
  html$header = stringi::stri_paste(html$header, '</table>')

  if (!is.null(highlight)) d[[text_col]] = highlight_words(d[[text_col]], highlight, highlight_col)
  if (!is.null(highlight)) d[[text_col]] = highlight_words(d[[text_col]], highlight, highlight_col)

  # render words
  text = split(d[[text_col]], f = d[[doc_col]])
  html$text = stringi::stri_paste_list(text, sep=' ')
  stringi::stri_paste(html$header, '<div max-width=500px>', html$text, '</div>', sep='')
}

highlight_words <- function(text, highlight, highlight_col) {
  highlight = as.numeric(highlight)
  highlight[is.na(highlight)] = 0

  ucol = data.frame(value = unique(highlight[highlight > 0]))
  ucol$v = ucol$value / max(ucol$value)
  ucol$rgbcol = col_to_span_rgb(rep(highlight_col, nrow(ucol)), alpha = ucol$v)
  highlight = ucol$rgbcol[match(highlight, ucol$value)]

  text = as.character(text)
  text[!is.na(highlight)] = stringi::stri_paste('<span style="background-color: ', highlight[!is.na(highlight)], '">',  text[!is.na(highlight)], '</span>', sep='')
  text
}


html_header <- function(template, header_info=NULL, width=500) {
  TEMPLATE = system.file(sprintf("template/%s.html", template), package="tokenvis", mustWork=T)
  html = readChar(TEMPLATE, file.info(TEMPLATE)$size)
  css = paste(get_css(header_info, width=width), collapse="\n")
  parts = unlist(strsplit(html, "$CONTENT$", fixed = T))
  footer = parts[1]
  gsub("$CSS$", css, footer, fixed = T)
}

html_footer <- function(template) {
  TEMPLATE = system.file(sprintf("template/%s.html", template), package="tokenvis", mustWork=T)
  html = readChar(TEMPLATE, file.info(TEMPLATE)$size)
  parts = unlist(strsplit(html, "$CONTENT$", fixed = T))
  parts[2]
}


get_css <- function(header_info, width=500) {
  CSS_TEMPLATE = system.file("template/style.css", package="tokenvis", mustWork=T)
  css = readLines(CSS_TEMPLATE, warn=F)
  css = gsub('$width$', width, css, fixed = T)
  if (!is.null(header_info)) {
    ## do stuff...
    #colo = substr(rainbow(length(topic_ids)), 1,7)
    #colorcss = paste(".t", topic_ids, " {background-color: ",colo, "}", sep="")
    c(css, colorcss)
  }
  css
}

#' Wrap a call that 'cats' html fragments and save it as a html file with header and footer
#'
#' Since R evaluates arguments lazily, call with the actual wrapped call, i.e. wrap_html(render_html(...))
#'
#' @export
wrap_html <- function(wrapped, header_info=NULL, template='simple', output=NULL, width=500) {
  if (is.null(output)) {
    output = tempfile("tokenvis_", fileext = ".html")
    message("Writing html to ", output)
  }
  sink(output)
  tryCatch({
    cat(html_header(template, header_info, width))
    force(wrapped)
    cat(html_footer(template))
  }, finally=sink())
  output
}
