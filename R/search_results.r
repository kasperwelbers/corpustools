featureHits <- function(hits, queries) {
  ## S3 class
  sentence = NULL ## used in data.table syntax, but need to have bindings for R CMD check
  if(is.null(hits)) hits = data.frame(code=character(), feature=character(), doc_id=character(), sentence = numeric(), hit_id=numeric())
  if (!'sentence' %in% colnames(hits)) {
    if (nrow(hits) > 0) hits[, sentence := NA] else hits$sentence = numeric()
  }
  #hits = as.data.frame(hits[,c('code','feature','doc_id','sentence','token_id', 'hit_id')])
  hits = hits[,c('code','feature','doc_id','sentence','token_id', 'hit_id')]
  fh = list(hits=hits, queries=queries)
  class(fh) = c('featureHits', class(fh))
  if(!is.featureHits(fh)) stop('Not a proper featureHits object')
  fh
}

is.featureHits <- function(fh, ...) {
  if (!methods::is(fh$hits, 'data.frame')) return(FALSE)
  if (!all(c('code','feature','doc_id','hit_id', 'sentence', 'token_id') %in% colnames(fh$hits))) return(FALSE)
  #if (!all(c('keyword','condition','code','condition_once') %in% colnames(fh$queries))) return(FALSE)
  return(TRUE)
}

#' S3 print for featureHits class
#'
#' @param x a featureHits object, as returned by \link{tCorpus$search_features}
#' @param ... not used
#'
#' @method print featureHits
#' @examples
#' text = c('A B C', 'D E F. G H I', 'A D', 'GGG')
#' tc = create_tcorpus(text, doc_id = c('a','b','c','d'), split_sentences = TRUE)
#' hits = tc$search_features(c('query label# A AND B', 'second query# (A AND Q) OR ("D E") OR I'))
#'
#' hits
#' @export
print.featureHits <- function(x, ...){
  if(!is.featureHits(x)) stop('Not a proper featureHits object')
  n_hits = nrow(unique(x$hits[,c('code', 'hit_id')]))
  n_docs = length(unique(x$hits$doc_id))
  n_sent = if(any(is.na(x$hits$sentence))) NULL else nrow(unique(x$hits[,c('doc_id','sentence')]))
  cat(n_hits, 'hits (in', n_docs, 'documents')
  if(!is.null(n_sent)) cat(' /', n_sent, 'sentences)\n') else cat(')\n')
}

#' S3 summary for featureHits class
#'
#' @param object a featureHits object, as returned by \link{tCorpus$search_features}
#' @param ... not used
#'
#' @method summary featureHits
#' @examples
#' text = c('A B C', 'D E F. G H I', 'A D', 'GGG')
#' tc = create_tcorpus(text, doc_id = c('a','b','c','d'), split_sentences = TRUE)
#' hits = tc$search_features(c('query label# A AND B', 'second query# (A AND Q) OR ("D E") OR I'))
#'
#' summary(hits)
#' @export
summary.featureHits <- function(object, ...){
  doc_id = sentence = hit_id = NULL ##  used in data.table syntax, but need to have bindings for R CMD check

  #if(is.null(x$hits)) return(NULL)
  if (!any(is.na(object$hits$sentence))){
    object$hits$sentence = paste(object$hits$doc_id, object$hits$sentence, sep='_')
    agg = data.table(object$hits)[, list(hits = length(unique(hit_id)),
                              sentences = length(unique(sentence)),
                              documents = length(unique(doc_id))),
                              by='code']
  } else {
    agg = data.table(object$hits)[, list(hits = length(unique(hit_id)),
                              documents = length(unique(doc_id))),
                           by='code']
  }
  as.data.frame(agg)
}

contextHits <- function(hits, queries) {
  ## S3 class
  sentence = NULL ## used in data.table syntax, but need to have bindings for R CMD check
  if(is.null(hits)) hits = data.frame(code=character(), doc_id=character(), sentence = numeric())
  if (!'sentence' %in% colnames(hits)) {
    if (nrow(hits) > 0) hits[, sentence := NA] else hits$sentence = numeric()
  }
  hits = as.data.frame(hits[,c('code','doc_id','sentence')])

  ch = list(hits=hits, queries=queries)
  class(ch) = c('contextHits', class(ch))
  if(!is.contextHits(ch)) stop('Not a proper contextHits object')
  ch
}

is.contextHits <- function(ch, ...) {
  if (!methods::is(ch$hits, 'data.frame')) return(FALSE)
  if (!all(c('code','doc_id','sentence') %in% colnames(ch$hits))) return(FALSE)
  if (!all(c('query','code') %in% colnames(ch$queries))) return(FALSE)
  return(TRUE)
}

#' S3 print for contextHits class
#'
#' @param x a contextHits object, as returned by \link{tCorpus$search_contexts}
#' @param ... not used
#'
#' @method print contextHits
#' @examples
#' text = c('A B C', 'D E F. G H I', 'A D', 'GGG')
#' tc = create_tcorpus(text, doc_id = c('a','b','c','d'), split_sentences = TRUE)
#' hits = tc$search_contexts(c('query label# A AND B', 'second query# (A AND Q) OR ("D E") OR I'))
#'
#' hits
#' @export
print.contextHits <- function(x, ...){
  if(!is.contextHits(x)) stop('Not a proper featureHits object')
  n_docs = length(unique(x$hits$doc_id))
  n_sent = if(any(is.na(x$hits$sentence))) NULL else nrow(x$hits[,c('doc_id','sentence')])
  cat(n_docs, 'documents')
  if(!is.null(n_sent)) cat(' /', n_sent, 'sentences') else cat('\n')
}

#' S3 summary for contextHits class
#'
#' @param object a contextHits object, as returned by \link{tCorpus$search_contexts}
#' @param ... not used
#'
#' @method summary contextHits
#' @examples
#' text = c('A B C', 'D E F. G H I', 'A D', 'GGG')
#' tc = create_tcorpus(text, doc_id = c('a','b','c','d'), split_sentences = TRUE)
#' hits = tc$search_contexts(c('query label# A AND B', 'second query# (A AND Q) OR ("D E") OR I'))
#'
#' summary(hits)
#' @export
summary.contextHits <- function(object, ...){
  #if(is.null(object$hits)) return(NULL)
  doc_id = sentence = NULL  ## used in data.table syntax, but need to have bindings for R CMD check

  if (!any(is.na(object$hits$sentence))){
    object$hits$sentence = paste(object$hits$doc_id, object$hits$sentence, sep='_')
    object = data.table(object$hits)[, list(sentences = length(unique(sentence)),
                              documents = length(unique(doc_id))),
                           by='code']

  } else {
    object = data.table(object$hits)[, list(documents = length(unique(doc_id))),
                           by='code']
  }
  as.data.frame(object)
}


