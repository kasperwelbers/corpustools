
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

custom_dot_abbreviations <- function(){
  ## stringi::stri_split_boundaries can handle many cases where a dot is not used at the end of a sentence, but some language specific abbreviations have to be added manually
  ## this is mostly important for honorifics, because then the token after the dot abbreviations is often a name, and thus capitalized
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

split_tokens <- function(x, max_tokens, remember_spaces=F) {
  x = stringi::stri_split_boundaries(x, type='word')
  if (remember_spaces) {
    x = lapply(x, function(x) collapse_terms_cpp(x, collapse=x %in% c(' ', '\n','\t','\r\n'), sep=" ", sep2=""))
  } else {
    x = lapply(x, function(x) x[!x %in% c(' ', '\n','\t','\r\n')])
  }
  if (!is.null(max_tokens)) x = sapply(x, head, max_tokens)
  x
}

tokenize_to_dataframe_batch <- function(x, doc_id, split_sentences=F, max_sentences=NULL, max_tokens=NULL, remember_spaces=T){
  token = NULL ## for solving CMD check notes (data.table syntax causes "no visible binding" message)
  x = gsub('_', ' ', x, fixed=T)

  if (split_sentences | !is.null(max_sentences)) {
    x = escape_custom_dot_abbreviations(x)
    x = stringi::stri_split_boundaries(x, type='sentence')

    if (!is.null(max_sentences)) x = sapply(x, head, max_sentences)
    x = lapply(x, function(x) unlist_to_df(split_tokens(x, max_tokens, remember_spaces),
                                           global_position=T))
    doclen = sapply(x, function(x) length(x$id))
    x = data.table::rbindlist(x)
    data.table::setnames(x, c('sentence','token_id','token'))

    x$doc_id = rep(doc_id, doclen)
    data.table::setcolorder(x, c('doc_id','sentence','token_id','token'))

    if (!is.null(max_tokens)) x = x[x$position <= max_tokens,]
    x[, token := unescape_custom_dot_abbreviation(x$token)]
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

function(){
test = create_tcorpus('test\n\n\ndit. ok\t\t. en dat\t ook\n.\n\n', udpipe_model='dutch', remember_spaces=T)$tokens
cat(as.character(test$space))

test$space
cat(paste(test$token, test$space))

test = create_tcorpus('test\n\n\ndit. ok\t\t. en dat\t ook\n.\n\n', remember_spaces=T)$tokens
cat(paste(test$token, test$space))
}
