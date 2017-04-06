
## todo: test whether tokenize package is better match

tokenize_to_dataframe <- function(x, doc_id=1:length(x), split_sentences=F, max_sentences=NULL, max_words=NULL, verbose=F){
  batch_i = get_batch_i(length(doc_id), batchsize=5000, return_list=T)
  prog = if (verbose) 'text' else 'none'
  plyr::ldply(batch_i, tokenize_to_dataframe_batch, x=x, doc_id=doc_id, split_sentences=split_sentences, max_sentences=max_sentences, max_words=max_words, .progress=prog)
}

custom_dot_abbreviations <- function(){
  ## stringi::stri_split_boundaries can handle many cases where a dot is not used at the end of a sentence, but some language specific abbreviations have to be added manually
  ## this is mostly important for honorifics, because then the word after the dot abbreviations is often a name, and thus capitalized
  ## here we can add abbreviations. Note that all abbreviations are matched with case-insensitive regex, and are assumed to be followed by a dot
  ## (there should be a list somewhere for this stuff, right?)
  common_titles = c('mr','miss','mrs','ms','mx',  ## english
                    'dhr', 'mw', 'mevr',          ## dutch
                    'herr', 'frau')               ## german
  prof_titles = c('prof', 'dr', 'drs')

  c(common_titles, prof_titles)
}

escape_custom_dot_abbreviations <- function(x){
  cda = custom_dot_abbreviations()
  r = stringi::stri_paste(cda, collapse = '|')
  stringi::stri_replace_all(x, replacement = '___CuDoAb___', regex=sprintf('\\b(?<=(%s))\\.', r), case_insensitive=T)
}

unescape_custom_dot_abbreviation <- function(x) {
  stringi::stri_replace_all(x, '.', fixed = '___CuDoAb___')
}

split_tokens <- function(x, max_words) {
  x = stringi::stri_split_boundaries(x, type='word')
  x = lapply(x, function(x) x[!x == ' '])
  if (!is.null(max_words)) x = sapply(x, head, max_words)
  x
}

tokenize_to_dataframe_batch <- function(batch_i, x, doc_id=1:length(x), split_sentences=F, max_sentences=NULL, max_words=NULL){
  x = x[batch_i]
  doc_id = doc_id[batch_i]
  x = gsub('_', ' ', x, fixed=T)
  if (split_sentences | !is.null(max_sentences)) {
    x = escape_custom_dot_abbreviations(x)
    x = stringi::stri_split_boundaries(x, type='sentence')

    if (!is.null(max_sentences)) x = sapply(x, head, max_sentences)
    x = lapply(x, function(x) unlist_to_df(split_tokens(x, max_words),
                                           global_position=T))
    doclen = sapply(x, function(x) length(x$id))
    x = rbindlist(x)
    colnames(x) = c('sent_i','word_i','word')

    x$doc_id = rep(doc_id, doclen)
    data.table::setcolorder(x, c('doc_id','sent_i','word_i','word'))

    if (!is.null(max_words)) x = x[x$position <= max_words,]
    x$word = unescape_custom_dot_abbreviation(x$word)
  } else {
    x = split_tokens(x, max_words)
    x = as.data.table(unlist_to_df(x, doc_id))
    colnames(x) = c('doc_id', 'word_i', 'word')
  }

  x$word = as.factor(x$word)
  x$doc_id = as.factor(x$doc_id)
  x
}

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
