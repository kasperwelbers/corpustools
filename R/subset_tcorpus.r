## this only contains the function versions of subset and subset_query.

#' S3 subset for tCorpus class
#'
#' @param x             a tCorpus object
#' @param subset        logical expression indicating rows to keep in the tokens data.
#' @param subset_meta   logical expression indicating rows to keep in the document meta data.
#' @param window        If not NULL, an integer specifiying the window to be used to return the subset. For instance, if the subset contains token 10 in a document and window is 5, the subset will contain token 5 to 15. Naturally, this does not apply to subset_meta.
#' @param ...           not used
#'
#' @method subset tCorpus
#' @examples
#' tc = create_tcorpus(sotu_texts, doc_col='id')
#'
#' ## subset to keep only tokens where token_id <= 20 (i.e.first 20 tokens)
#' tcs1 = subset(tc, token_id < 20)
#' tcs1
#'
#' ## subset to keep only documents where president is Barack Obama
#' tcs2 = subset(tc, subset_meta = president == 'Barack Obama')
#' tcs2
#' @export
subset.tCorpus <- function(x, subset=NULL, subset_meta=NULL, window=NULL, ...) {
  subset = x$eval(substitute(subset), parent.frame())
  subset_meta = x$eval_meta(substitute(subset_meta), parent.frame())
  x$subset(subset=subset, subset_meta=subset_meta, window=window, copy=T)
}

#' Subset tCorpus token data using a query
#'
#' A convenience function that searches for contexts (documents, sentences), and uses the results to \link[=subset]{subset} the tCorpus token data.
#'
#' See the documentation for \link[=search_contexts]{search_contexts} for an explanation of the query language.
#'
#' @param tc      A \code{\link{tCorpus}}
#' @param query A character string that is a query. See \link{search_contexts} for query syntax.
#' @param feature The name of the feature columns on which the query is used.
#' @param context_level Select whether the query and subset are performed at the document or sentence level.
#' @param window  If used, uses a word distance as the context (overrides context_level)
#'
#' @examples
#' text = c('A B C', 'D E F. G H I', 'A D', 'GGG')
#' tc = create_tcorpus(text, doc_id = c('a','b','c','d'), split_sentences = TRUE)
#'
#' ## subset by reference
#' tc2 = subset_query(tc, 'A')
#' tc2$meta
#'
#' @export
subset_query <- function(tc, query, feature='token', context_level=c('document','sentence'), window=NA){
  tc$subset_query(query, feature, context_level, window, copy = T)
}

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
#' @examples
#' tc = create_tcorpus(c('a a a b b'))
#'
#' tc$tokens
#' tc$subset(subset = freq_filter(token, min=3))
#' tc$tokens
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
#' @examples
#' tc = create_tcorpus(c('a a a b b', 'a a c c'))
#'
#' tc$tokens
#' tc$subset(subset = docfreq_filter(token, min=2))
#' tc$tokens
#' @export
docfreq_filter <- function(x, min=-Inf, max=Inf, top=NULL, bottom=NULL, doc_id=parent.frame()$doc_id) {
  if (methods::is(x, 'character')) x = eval(parse(text=x), envir = parent.frame(1))
  freq_table = unique(data.frame(doc_id=doc_id, x=x))
  freq_table = table(droplevels(freq_table$x))
  x %in% x_filter(freq_table, min=min, max=max, top=top, bottom=bottom)
}

