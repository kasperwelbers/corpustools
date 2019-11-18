### most resources work with huge stringmatch tables
### therefore, the following functions are optimized for mathing many strings at once, as opposed to the more versatile search_features function.


#' Dictionary lookup
#'
#' @description
#' Add a column to the token data that contains a code (the query label) for tokens that match the dictionary
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{code_dictionary(...)}
#'
#' @param dict            A dictionary. Can be either a data.frame or a quanteda dictionary. If a data.frame is given, it has to
#'                        have a column named "string" that contains the dictionary terms. All other columns are added to the
#'                        tCorpus $tokens data. Each row has a single string, that can be
#'                        a single word or a sequence of words seperated by a whitespace (e.g., "not bad"), and can have the common ? and * wildcards.
#'                        If a quanteda dictionary is given, it is automatically converted to this type of data.frame with the
#'                        \code{\link{melt_quanteda_dict}} function. This can be done manually for more control over labels.
#' @param token_col       The feature in tc that contains the token text.
#' @param case_sensitive  logical, should lookup be case sensitive?
#' @param column          The name of the column added to $tokens. [column]_id contains the unique id of the match.
#'                        If a quanteda dictionary is given, the label for the match is in the column named [column].
#'                        If a dictionary has multiple levels, these are added as [column]_l[level].
#' @param batchsize       Very large dictionaries will be matched in batches to prevent memory issues.
#' @param flatten_colloc  If true, collocations in the tokens (rows in tc$tokens) will be considered separate words. For example, "President_Obama" will be split to "president" "obama", so that "president obama" in the dictionary matches correctly.
#' @param ascii           If true, convert text to ascii before matching
#' @param low_memory      if true, use slower but more memory efficient algorithm
#' @param verbose         If true, report progress
#'
#' @return A vector with the id value (taken from dict$id) for each row in tc$tokens
#'
#' @name tCorpus$code_dictionary
#'
#' @examples
#' dict = data.frame(string = c('good','bad','ugl*','nice','not pret*'), sentiment=c(1,-1,-1,1,-1))
#' tc = create_tcorpus(c('The good, the bad and the ugly, is nice but not pretty'))
#' tc$code_dictionary(dict)
#' print(tc$tokens)
#' @aliases code_dictionary
tCorpus$set('public', 'code_dictionary', function(dict, token_col='token', case_sensitive=F, column='code', batchsize=50000, flatten_colloc=T, ascii=F, low_memory=F, verbose=F){
  if (methods::is(dict, 'dictionary2')) dict = melt_quanteda_dict(dict, column = column)
  if (!methods::is(dict, 'data.frame')) stop('dict has to be a data.frame or a quanteda dictionary2 class')
  if (!'string' %in% colnames(dict)) stop('dict must have a column named "string"')

  fl = dictionary_lookup(self, data.frame(string=dict$string, id = 1:nrow(dict)),
                        token_col=token_col, case_sensitive=case_sensitive, batchsize=batchsize, flatten_colloc=flatten_colloc, ascii=ascii, low_memory=low_memory, verbose=verbose)

  is_hit = !is.na(fl$dict_i)
  anno = dict[as.numeric(fl$dict_i[is_hit]),]
  hit_id = fl$hit_id[is_hit]

  for (.col in colnames(anno)) {
    if (.col == 'string') next
    if (!paste0(column, '_id') %in% self$names) self$set(paste0(column, '_id'), hit_id, subset = is_hit, subset_value=F)
    .value = anno[[.col]]
    self$set(.col, anno[[.col]], subset = is_hit, subset_value=F)
  }
})


#' Convert a quanteda dictionary to a long data.table format
#'
#' This is used internally in the tCorpus dictionary search functions, but can be used manually
#' for more control. For example, adding numeric scores for sentiment dictionaries, and specifying which label/code to use in search_dictionary().
#'
#' @param dict      The quanteda dictionary
#' @param column    The name of the column with the label/code. If dictionary contains multiple levels,
#'                  additional columns are added with the suffix _l[i], where [i] is the level.
#' @param .index    Do not use (used for recursive melting)
#'
#' @return A data.table
#' @export
#'
#' @examples
#' \donttest{
#' d = quanteda::data_dictionary_LSD2015
#' melt_quanteda_dict(d)
#' }
melt_quanteda_dict <- function(dict, column='code', .index=NULL) {
  if (is.null(.index)) {
    if (!methods::is(dict, 'dictionary2')) stop('dict is not a quanteda dictionary2 class')
    .index = data.table(string = character(length(dict)))
  }
  cname = if (ncol(.index) > 1) paste0(column, '_l', ncol(.index)) else column
  .index[[cname]] = names(dict)

  n = sapply(dict, length)
  .index = .index[rep(1:nrow(.index), n)]
  dict = unlist(dict, recursive = F, use.names = T)
  names(dict) = gsub('.*\\.', '', names(dict))


  if (!any(sapply(dict, class) == 'list')) {
    if (length(unlist(dict)) > nrow(.index)) {
      n = sapply(dict, length)
      .index = .index[rep(1:nrow(.index), n)]
    }
    .index$string = unlist(dict)
    return(.index)
  }

  melt_quanteda_dict(dict, column, .index)
}


#' Dictionary lookup
#'
#' Similar to search_features, but for fast matching of large dictionaries.
#'
#' @param tc              A tCorpus
#' @param dict            A dictionary. Can be either a data.frame or a quanteda dictionary. If a data.frame is given, it has to
#'                        have a column named "string" that contains the dictionary terms, and a column "code" that contains the
#'                        label/code represented by this string. Each row has a single string, that can be
#'                        a single word or a sequence of words seperated by a whitespace (e.g., "not bad"), and can have the common ? and * wildcards.
#'                        If a quanteda dictionary is given, it is automatically converted to this type of data.frame with the
#'                        \code{\link{melt_quanteda_dict}} function. This can be done manually for more control over labels.
#' @param token_col       The feature in tc that contains the token text.
#' @param case_sensitive  logical, should lookup be case sensitive?
#' @param batchsize       Very large dictionaries will be matched in batches to prevent memory issues.
#' @param flatten_colloc  If true, collocations in the tokens (rows in tc$tokens) will be considered separate words. For example, "President_Obama" will be split to "president" "obama", so that "president obama" in the dictionary matches correctly.
#' @param ascii           If true, convert text to ascii before matching
#' @param low_memory      if true, use slower but more memory efficient computation
#' @param verbose         If true, report progress
#'
#' @return A vector with the id value (taken from dict$id) for each row in tc$tokens
#' @export
#'
#' @examples
#' dict = data.frame(string = c('this is', 'for a', 'not big enough'), code=c('a','c','b'))
#' tc = create_tcorpus(c('this is a test','This town is not big enough for a test'))
#' search_dictionary(tc, dict)$hits
search_dictionary <- function(tc, dict, token_col='token', case_sensitive=F, batchsize=50000, flatten_colloc=T, ascii=F, low_memory=F, verbose=F){
  hit_id = NULL
  if (!is_tcorpus(tc)) stop('tc is not a tCorpus')
  if (methods::is(dict, 'dictionary2')) dict = melt_quanteda_dict(dict)
  if (!methods::is(dict, 'data.frame')) stop('dict has to be a data.frame or a quanteda dictionary2 class')
  if (!'string' %in% colnames(dict)) stop('dict must have a column named "string"')
  if (!'code' %in% colnames(dict)) stop('dict must have a column named "code"')


  fl = dictionary_lookup(tc, data.frame(string=dict$string, id = 1:nrow(dict)),
                        token_col=token_col, case_sensitive=case_sensitive, batchsize=batchsize, flatten_colloc=flatten_colloc, ascii=ascii, low_memory=low_memory, verbose=verbose)



  not_na = !is.na(fl$dict_i)
  hits = tc$get(subset = not_na)
  hits$hit_id = fl$hit_id[not_na]
  hits$code = dict$code[as.numeric(fl$dict_i[not_na])]
  if (!'sentence' %in% colnames(hits)) hits[, 'sentence' := numeric()]
  hits = subset(hits, select = intersect(c('doc_id','token_id','sentence','code','hit_id',token_col), colnames(hits)))
  data.table::setnames(hits, token_col, 'feature')


  queries = data.frame()
  featureHits(hits, queries)
}

dictionary_lookup <- function(tc, dict, regex_sep=' ', token_col='token', case_sensitive=F, batchsize=50000, flatten_colloc=T, ascii=F, low_memory=F, verbose=F){
  if (!token_col %in% tc$names) stop(sprintf('To use search_dictionary, the tCorpus must have a feature column with clean (not preprocessed) text, labeled "%s"', token_col))
  if (!'string' %in% colnames(dict)) stop('Dictionary must have column named "string"')
  if (!'id' %in% colnames(dict)) stop('Dictionary must have column named "id"')

  fi = create_feature_index(tc, token_col, context_level='document', max_window_size=100)

  levels(fi$feature) = normalize_string(levels(fi$feature), lowercase=!case_sensitive, ascii = ascii)
  data.table::setkey(fi, 'feature')

  dict$string = normalize_string(dict$string, lowercase=!case_sensitive, ascii=ascii)
  dict = data.table::as.data.table(dict)
  data.table::setkey(dict, 'string')

  if (flatten_colloc) {
    is_colloc = grep(' |_', levels(fi$feature))
    if (length(is_colloc) > 0){
      fi = flatten_collocations(fi, 'feature', 'global_i')
      message(sprintf('flattened %s collocations', length(is_colloc)))
    } else {
      #message('flatten_colloc (collocations) is set to TRUE, but no collocations were found.')
      flatten_colloc = F ## if there are no collocations, ignore flatten_colloc == T
    }
  }

  #if (use_regex) re = expand_resource_regex(re, features=levels(fi$features)) ## not yet implemented. expand resource ir use_regex=T: if there are regular expressions in the resource and use_regex==T, then first stringmatch the regex in the vocabulary and expand the resource with all returned strings
  ## make batches of the resource
  n.batches = ceiling(nrow(dict) / batchsize)
  dict = dict[order(dict$string),]
  batch_i = rep(1:n.batches, length.out=nrow(dict)) ## distribute evenly over batches (basically UNordered by alphabet) so that frequently occuring names (that lead to huge matches) are more evenly distributed
  if (!low_memory) {
    batch_i = batch_i[order(batch_i)] ## alternatively, ordering by alphabet is probably faster for matching
  }
  batches = split(dict, batch_i)

  candidates = vector('list', length(batches))
  counter = verbose_sum_counter(nrow(dict))
  for(i in seq_along(batches)){
    candidates[[i]] = fast_multitoken_stringmatch(batches[[i]], fi=fi, regex_sep=regex_sep, case_sensitive=case_sensitive)
    if (verbose) counter(nrow(batches[[i]]))
  }
  if (verbose) message('Binding results')
  candidates = data.table::rbindlist(candidates)

  ## select candidates (at some point candidate selection could be done better. Perhaps even taking context into account. BUT NOT TODAY!!)
  candidates = candidates[order(-candidates$nterms),] ## go for candidates with the most terms (= most specific)
  candidates = unique(candidates, by = c('global_i'))

  ## turn into a long format, where each token is on a separate row
  index_i = rep(1:nrow(candidates), times=candidates$nterms)
  index_nr = unlist(sapply(candidates$nterms, function(x) seq(1,x), simplify = F))
  index = candidates[index_i, c('global_i','id', 'i')]
  if (flatten_colloc){
    ## if collocations have been flattened, the real i in the tcorpus (which is still a collocation) has multiple global_i (which have been flattened)
    index$global_i = index$global_i + (index_nr - 1)
    index$i = fi$i[match(index$global_i, fi$global_i)]
  } else {
    index$i = index$i + (index_nr - 1) ## if global_i's have only one match to i's, we can recalculate the i's directly
  }

  index$hit_id = match(index$global_i, sort(unique(index$global_i)))
  dict_i = rep(NA, tc$n)
  dict_i[index$i] = index$id
  hit_id = rep(NA, tc$n)
  hit_id[index$i] = index$hit_id
  data.table::data.table(hit_id=hit_id, dict_i=dict_i)
}

create_feature_index <- function(tc, feature, context_level=c('document','sentence'), max_window_size=100, as_ascii=F){
  context_level = match.arg(context_level)
  feature_index = data.table::data.table(feature = tc$get(feature))
  if (!methods::is(feature_index$feature, 'factor')) feature_index$feature = fast_factor(feature_index$feature)
  if (as_ascii) levels(feature_index$feature) = iconv(levels(feature_index$feature), to='ASCII//TRANSLIT')
  feature_index$i = 1:nrow(feature_index)
  feature_index$global_i = get_global_i(tc, context_level, max_window_size)
  data.table::setkey(feature_index, 'feature')
  feature_index
}


normalize_string <- function(x, lowercase=T, ascii=T, trim=T){
  if (lowercase) x = tolower(x)
  if (ascii) x = iconv(x, to='ASCII//TRANSLIT')
  if (trim) x = stringi::stri_trim(x)
  x
}



fast_multitoken_stringmatch <- function(dict, fi, regex_sep=' ', case_sensitive=T){
  i = 1:length(dict$string)

  if (!case_sensitive) {
    string = tolower(dict$string)
    levels(fi$feature) = tolower(levels(fi$feature))
  }

  sn = stringi::stri_split(dict$string, regex=regex_sep)
  nterms = sapply(sn, length)
  candidates = vector('list', max(nterms))

  if (any(grepl('[?*]', dict$string))) {
    ## if wildcards are found, expand the query by looking for all terms in the vocabulary
    ## that match the query term
    sn = expand_wildcards(sn, levels(fi$feature))
    exp_i = floor(as.numeric(names(sn)))
    dict = dict[exp_i,]
    nterms = nterms[exp_i]
    i = i[exp_i]
  } else {
    names(sn) = 1:length(sn)
  }

  lt = data.table::data.table(feature = sapply(sn, data.table::first), id=dict$id, nterms=nterms, s_i=i, key='feature')
  lt = merge(fi, lt, by=c('feature'), allow.cartesian=T)

  is_end = lt$nterms == 1
  candidates[[1]] = lt[is_end, c('i','global_i','id','nterms'), with=F]
  lt = lt[!is_end,]

  sn_n = sapply(sn, length)
  snl = data.table::data.table(s_i = rep(floor(as.numeric(names(sn))), sn_n),
                               feature = unlist(sn))
  get_nr <- function(x) 1:length(x)
  snl[, nr := get_nr(feature), by='s_i']

  for(i in 1:length(candidates)){
    terms = snl[snl$nr == (i+1),]
    term_hits = fi[terms$feature,,on='feature', allow.cartesian=T]

    hits = merge(terms, fi, by='feature', allow.cartesian = T)
    hits$global_i = hits$global_i - i

    lt = lt[hits, on=c('s_i','global_i'), nomatch=0]
    is_end = lt$nterms == i+1
    candidates[[i+1]] = lt[is_end,c('i','global_i','id','nterms'), with=F]
    lt = lt[!is_end,]
  }
  candidates = data.table::rbindlist(candidates)
  candidates = candidates[order(-candidates$nterms),] ## first go for candidates with the most terms (likely to be most specific)
  unique(candidates, by = c('global_i'))
}

expand_wildcards <- function(query_list, voc) {
  ## get a new list where terms with wildcards are repeated for all matches in vocabulary
  ## the names of the list contain ids of which the floor is the index of the dictionary

  #ql = list('a', c('test','this'), c('test*','that','fish'), c('testint','what?'))
  n = sapply(query_list, length)
  i = rep(1:length(query_list), n)
  ql = data.table(t = unlist(query_list), i = i)
  add_n <- function(x) 1:length(x)
  ql[, n := add_n(t), by='i']

  #voc = c('testing','whatsapp','what','tests','bananas')

  ql$is_wc =  grepl('[?*]', ql$t)
  if (!any(ql$is_wc)) {
    names(query_list) = 1:length(query_list)
    return(query_list)
  }
  wct = unique(ql$t[ql$is_wc])
  wctreg = gsub('*', '.*', wct, fixed = T)
  wctreg = gsub('?', '.{0,1}', wctreg, fixed = T)
  wctreg = paste0('\\b',wctreg,'\\b')

  full_t = sapply(wctreg, grep, x=voc, value=T, simplify = F)
  nreg = sapply(full_t, length)
  full_t = data.table(t = rep(wct, nreg),
                      full_t = unlist(full_t),
                      nr = 1:sum(nreg))

  full_t = merge(full_t, ql[,c('i','t')], by='t')
  out = merge(full_t, ql, by='i', all=T)

  out$nr[is.na(out$nr)] = 0
  data.table::setorderv(out, 'n', 1)
  out$id = out$i + (out$nr / (max(out$nr)+1))
  split(ifelse(out$is_wc, out$full_t, out$t.y), out$id)
}

