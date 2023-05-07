## work in progress.
## enable easy visualization of (ngram) overlap

tag_ngrams <- function(value, ngrams, doc_id, priority=c('long','short','first')) {
  priority = match.arg(priority)
  if (length(ngrams) == 1)
    x = which(!is.na(value))
  else {
    x = which(!is.na(ngrams) & !is.na(value))
    if (priority == 'long')
      x = x[order(-ngrams[x])]
    if (priority == 'short')
      x = x[order(ngrams[x])]
    if (priority == 'first')
      NULL
  }

  l = lapply(x, function(i) {
    ng = if (length(ngrams) == 1) ngrams else ngrams[i]
    if (ng == 1) return(i)
    i_ngram = (i-ng+1):i
    i_ngram = i_ngram[i_ngram > 0]
    same_doc = doc_id[i] == doc_id[i_ngram]
    i_ngram[same_doc]
  })
  d = data.frame(i = unlist(l),
                 value = rep(value[x], ngrams[x]))
  d = d[!duplicated(d$i),]
  value[d$i] = d$value
  value
}

tc_add_idf <- function(tc) {
  dtm = corpustools::get_dtm(tc, feature='feature', weight = 'norm_tfidf', drop_empty_terms = F, context_labels = T, feature_labels=T, ngrams=1)
  dtm = methods::as(methods::as(dtm, 'generalMatrix'), 'TsparseMatrix')
  dtm = data.table::data.table(doc_id = rownames(dtm)[dtm@i+1], feature=colnames(dtm)[dtm@j+1], tfidf=dtm@x)
  tc$tokens = merge(tc$tokens, dtm, by=c('doc_id','feature'))
  tc
}
