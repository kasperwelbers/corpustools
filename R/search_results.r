featureHits <- function(hits, queries) {
  ## S3 class
  if(is.null(hits)) hits = data.frame(code=character(), feature=character(), doc_id=character(), sent_i = numeric(), hit_id=numeric(), i=numeric())
  hits = as.data.frame(hits)
  if (!'sent_i' %in% colnames(hits)) {
    hits$sent_i = if(nrow(hits) == 0) numeric() else NA
  }
  hits = hits[,c('code','feature','doc_id','sent_i','word_i', 'hit_id','i')]
  fh = list(hits=hits, queries=queries)
  class(fh) = c('featureHits', class(fh))
  if(!is.featureHits(fh)) stop('Not a proper featureHits object')
  fh
}

is.featureHits <- function(fh) {
  if (!is(fh$hits, 'data.frame')) return(FALSE)
  if (!all(c('code','feature','i','doc_id','hit_id', 'sent_i', 'word_i') %in% colnames(fh$hits))) return(FALSE)
  if (!all(c('keyword','condition','code','condition_once','subset_tokens','subset_meta') %in% colnames(fh$queries))) return(FALSE)
  return(TRUE)
}

#' @export
print.featureHits <- function(x){
  if(!is.featureHits(x)) stop('Not a proper featureHits object')
  n_hits = length(unique(x$hits$hit_id))
  n_docs = length(unique(x$hits$doc_id))
  n_sent = if(any(is.na(x$hits$sent_i))) NULL else nrow(x$hits[,c('doc_id','sent_i')])
  cat(n_hits, 'hits (in', n_docs, 'documents')
  if(!is.null(n_sent)) cat(' /', n_sent, 'sentences)\n') else cat(')\n')
}

#' @export
summary.featureHits <- function(x){
  #if(is.null(x$hits)) return(NULL)
  if (!any(is.na(x$hits$sent_i))){
    x$hits$sent_i = paste(x$hits$doc_id, x$hits$sent_i, sep='_')
    agg = data.table(x$hits)[,.(hits = length(unique(hit_id)),
                              sentences = length(unique(sent_i)),
                              documents = length(unique(doc_id))),
                              by='code']
  } else {
    agg = data.table(x$hits)[,.(hits = length(unique(hit_id)),
                              documents = length(unique(doc_id))),
                           by='code']
  }
  as.data.frame(agg)
}

contextHits <- function(hits, queries) {
  ## S3 class
  if(is.null(hits)) hits = data.frame(code=character(), doc_id=character(), sent_i = numeric())
  hits = as.data.frame(hits)
  if (!'sent_i' %in% colnames(hits)) {
    hits$sent_i = if(nrow(hits) == 0) numeric() else NA
  }
  hits = hits[,c('code','doc_id','sent_i')]

  ch = list(hits=hits, queries=queries)
  class(ch) = c('contextHits', class(ch))
  if(!is.contextHits(ch)) stop('Not a proper contextHits object')
  ch
}

is.contextHits <- function(ch) {
  if (!is(ch$hits, 'data.frame')) return(FALSE)
  if (!all(c('code','doc_id','sent_i') %in% colnames(ch$hits))) return(FALSE)
  if (!all(c('query','code') %in% colnames(ch$queries))) return(FALSE)
  return(TRUE)
}

#' @export
print.contextHits <- function(x){
  if(!is.contextHits(x)) stop('Not a proper featureHits object')
  n_docs = length(unique(x$hits$doc_id))
  n_sent = if(any(is.na(x$hits$sent_i))) NULL else nrow(x$hits[,c('doc_id','sent_i')])
  cat(n_docs, 'documents')
  if(!is.null(n_sent)) cat(' /', n_sent, 'sentences') else cat('\n')
}

#' @export
summary.contextHits <- function(x){
  #if(is.null(x$hits)) return(NULL)
  if (!any(is.na(x$hits$sent_i))){
    x$hits$sent_i = paste(x$hits$doc_id, x$hits$sent_i, sep='_')
    x = data.table(x$hits)[,.(sentences = length(unique(sent_i)),
                              documents = length(unique(doc_id))),
                           by='code']

  } else {
    x = data.table(x$hits)[,.(documents = length(unique(doc_id))),
                           by='code']
  }
  as.data.frame(x)
}

