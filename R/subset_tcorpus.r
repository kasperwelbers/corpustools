x_filter <- function(ft, min=-Inf, max=Inf, top=NULL, bottom=NULL) {
  select = names(ft[ft >= min & ft <= max])
  if (!is.null(top)) {
    top = names(head(ft[order(-ft)], top))
    select = intersect(select, top)
  }
  if (!is.null(bottom)) {
    bottom = names(head(ft[order(ft)], bottom))
    select = intersect(select, bottom)
  }
  select
}

#' Support function for subset method
#'
#' Support function to enable subsetting by frequency stats of a given feature.
#' Should only be used within the tCorpus subset method, or any tCorpus method that supports a subset argument.
#'
#' @param x the name of the feature column. Can be given as a call or a string.
#' @param min A number, setting the minimum frequency value
#' @param max A number, setting the maximum frequency value
#' @param top A number. If given, only the top x features with the highest frequency are TRUE
#' @param bottom A number. If given, only the bottom x features with the highest frequency are TRUE
#'
#' @export
freq_filter <- function(x, min=-Inf, max=Inf, top=NULL, bottom=NULL) {
  if (methods::is(x, 'character')) x = eval(parse(text=x), envir = parent.frame(1))
  freq_table = table(droplevels(x))
  x %in% x_filter(freq_table, min=min, max=max, top=top, bottom=bottom)
}


#' Support function for subset method
#'
#' Support function to enable subsetting by document frequency stats of a given feature.
#' Should only be used within the tCorpus subset method, or any tCorpus method that supports a subset argument.
#'
#' @param x the name of the feature column. Can be given as a call or a string.
#' @param min A number, setting the minimum document frequency value
#' @param max A number, setting the maximum document frequency value
#' @param top A number. If given, only the top x features with the highest document frequency are TRUE
#' @param bottom A number. If given, only the bottom x features with the highest document frequency are TRUE
#' @param doc_id Added for reference, but should not be used. Automatically takes doc_id from tCorpus if the docfreq_filter function is used within the subset method.
#'
#' @export
docfreq_filter <- function(x, min=-Inf, max=Inf, top=NULL, bottom=NULL, doc_id=parent.frame()$doc_id) {
  if (methods::is(x, 'character')) x = eval(parse(text=x), envir = parent.frame(1))
  freq_table = unique(data.frame(doc_id=doc_id, x=x))
  freq_table = table(droplevels(freq_table$x))
  x %in% x_filter(freq_table, min=min, max=max, top=top, bottom=bottom)
}

