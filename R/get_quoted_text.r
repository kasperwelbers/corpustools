get_verbatim_quote <- function(tokens) {
  verbatim = !is.na(get_quote_positions(tokens, 'token', space_col='space'))
  verbatim & !is.na(tokens$quote_id) & tokens$quote == 'quote'   
}


smartquotes <- function() intToUtf8(c(8220,8221,8222))

get_quote_positions <- function(tokens, text_col, add_quote_symbols=NULL, par_col=NULL, space_col=NULL, quote_subset=NULL) {
  if (!is.null(add_quote_symbols)) {
    add_quote_symbols = paste(add_quote_symbols, collapse='')
    quote_regex = sprintf('^[%s%s"]*$', smartquotes(), add_quote_symbols)
  } else {
    quote_regex = sprintf('^[%s"]*$', smartquotes())
  }
                        
  par = get_paragraph(tokens, text_col, par_col, space_col)
  
  is_quote = grepl(quote_regex, tokens[[text_col]])
  if (!is.null(quote_subset)) is_quote = is_quote & quote_subset
  is_quote[c(FALSE,is_quote[-length(is_quote)])] = FALSE ## to ignore double quotes after reshaping
  
  par_quotes = split(is_quote, par)
  par_quotes = lapply(par_quotes, get_spans)
  
  add_count = cumsum(sapply(par_quotes, function(x) max(c(x,0), na.rm=TRUE)))
  add_count = data.table::shift(add_count, 1, fill = 0)
  par_quotes = lapply(1:length(par_quotes), function(i) par_quotes[[i]] + add_count[i])
  out = as.integer(unlist(par_quotes))
  out[is_quote] = NA
  out
}

get_spans <- function(quotes) {
  quotes = cumsum(quotes) + 1
  if (quotes[length(quotes)] < 2) {
    quotes = rep(NA, length(quotes))
  } else {
    quotes = match(quotes, seq(2, quotes[length(quotes)], by = 2))
    shifted = data.table::shift(quotes, 1)
    quotes = ifelse(is.na(quotes) & !is.na(shifted), shifted, quotes)
  }
  quotes
}

get_paragraph <- function(tokens, text_col, par_col, space_col) {
  if (is.null(par_col)) {
    is_break = grepl('\n', tokens[[text_col]], fixed=TRUE) 
    if (!is.null(space_col)) is_break = is_break | grepl('\n', tokens[[space_col]], fixed=TRUE)
    is_new = !is_break & c(FALSE, is_break[-length(is_break)])
    is_new[!duplicated(tokens$doc_id)] = TRUE
    par = cumsum(is_new)
  } else par = tokens[[par_col]]
  par
}