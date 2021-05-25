
## todo: test whether tokenize package is better match

tokenize_to_dataframe <- function(x, doc_id=1:length(x), split_sentences=F, max_sentences=NULL, max_tokens=NULL, remember_spaces =F, verbose=F){
  space = NULL ## data.table bindings

  batch_i = get_batch_i(length(doc_id), batchsize=5000, return_list=T)
  prog = if (verbose) 'text' else 'none'

  n = length(batch_i)
  if (verbose && n > 1) pb = utils::txtProgressBar(min = 1, max = n, style = 3)
  tokens = vector('list', n)
  for (i in 1:n){
    if (verbose && n > 1) pb$up(i)
    tokens[[i]] = tokenize_to_dataframe_batch(x[batch_i[[i]]], doc_id=doc_id[batch_i[[i]]],
                                              split_sentences=split_sentences, max_sentences=max_sentences, max_tokens=max_tokens, remember_spaces=remember_spaces)
  }

  tokens = data.table::rbindlist(tokens)
  if (remember_spaces) {
    split_terms = uncollapse_terms_cpp(as.character(tokens$token))
    tokens$token = fast_factor(split_terms$left)
    tokens[, space := fast_factor(split_terms$right)]
  }
  tokens
}

split_tokens <- function(x, max_tokens, remember_spaces=F) {
  x = stringi::stri_split_boundaries(x, type='word')
  
  if (remember_spaces) {
    x = lapply(x, function(x) collapse_terms_cpp(x, collapse=stringi::stri_detect(x, regex='^\\s+$'), sep=" ", sep2=""))
  } else {
    x = lapply(x, function(x) x[!stringi::stri_detect(x, regex='^\\s+$')])
  }
  if (!is.null(max_tokens)) x = sapply(x, head, max_tokens, simplify=F)
  x
}



tokenize_to_dataframe_batch <- function(x, doc_id, split_sentences=F, max_sentences=NULL, max_tokens=NULL, remember_spaces=T){
  token = NULL ## for solving CMD check notes (data.table syntax causes "no visible binding" message)
  x = gsub('_', ' ', x, fixed=T)
  x[is.na(x)] = ''
  
  if (split_sentences | !is.null(max_sentences)) {
    
    x = stringi::stri_split_boundaries(x, type='sentence')
    
    x = lapply(x, function(sents) {
      new_non_space = !stringi::stri_detect(sents, regex='^\\s+$')
      if (any(new_non_space))
        sapply(split(sents, cumsum(new_non_space)), stringi::stri_paste, collapse='')
      else 
        sents
    })
    
    if (!is.null(max_sentences)) x = sapply(x, head, max_sentences)
    x = lapply(x, function(x) unlist_to_df(split_tokens(x, max_tokens, remember_spaces),
                                           global_position=T))
    doclen = sapply(x, function(x) length(x$id))
    x = data.table::rbindlist(x)
    data.table::setnames(x, c('sentence','token_id','token'))

    x$doc_id = rep(doc_id, doclen)
    data.table::setcolorder(x, c('doc_id','sentence','token_id','token'))

    if (!is.null(max_tokens)) x = x[x$position <= max_tokens,]
  } else {
    x = split_tokens(x, max_tokens, remember_spaces)
    x = as.data.table(unlist_to_df(x, doc_id))
    colnames(x) = c('doc_id', 'token_id', 'token')
  }
  x[, token := fast_factor(x$token)]
  x[, doc_id := fast_factor(x$doc_id)]
  x
}

unlist_to_df <- function(l, ids=1:length(l), global_position=F){
  len = sapply(l, length)
  
  if (length(len) == 0) return(NULL)
  if (sum(len) == 0) return(NULL)
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
