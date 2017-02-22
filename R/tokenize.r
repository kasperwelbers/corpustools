
## todo: test whether tokenize package is better match

tokenize_to_dataframe <- function(x, doc_id=1:length(x), split_sentences=F, max_sentences=NULL, max_words=NULL, verbose=F){
  batch_i = get_batch_i(length(doc_id), batchsize=5000, return_list=T)
  prog = if(verbose) 'text' else 'none'
  plyr::ldply(batch_i, tokenize_to_dataframe_batch, x=x, doc_id=doc_id, split_sentences=split_sentences, max_sentences=max_sentences, max_words=max_words, .progress=prog)
}

tokenize_to_dataframe_batch <- function(batch_i, x, doc_id=1:length(x), split_sentences=F, max_sentences=NULL, max_words=NULL){
  x = x[batch_i]
  doc_id = doc_id[batch_i]
  x = gsub('_', ' ', x, fixed=T)
  if(split_sentences | !is.null(max_sentences)) {
    x = quanteda::tokenize(x, what = 'sentence')
    names(x) = doc_id
    if(!is.null(max_sentences)) x = sapply(x, head, max_sentences)
    x = plyr::ldply(x, function(x) unlist_to_df(quanteda::tokenize(x, what='word'), global_position=T))
    if(!is.null(max_words)) x = x[x$position <= max_words,]
    colnames(x) = c('doc_id','sent_i','word_i','word')
  } else {
    x = quanteda::tokenize(x, what = 'word')
    if(!is.null(max_words)) x = sapply(x, head, max_words)
    x = unlist_to_df(x, doc_id)
    colnames(x) = c('doc_id', 'word_i', 'word')
  }
  x$word = as.factor(x$word)
  x$doc_id = as.factor(x$doc_id)
  x
}

unlist_to_df <- function(l, ids=1:length(l), global_position=F){
  len = sapply(l, length)
  filter = len > 0
  if(global_position){
    position = 1:sum(len)
  } else {
    position = unlist(sapply(len[filter], function(l) 1:l, simplify = F))
  }
  data.frame(id = rep(ids[filter], len[filter]),
             position = position,
             value = unlist(l[filter]))
}
