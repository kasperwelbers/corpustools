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
#' @param dict            A data.frame with the columns "string" and "id". The text in "string" will be matched to the tokens, and "id" will be the id given to tokens that matched
#' @param regex_sep       A regular expression for splitting multiple terms in dict$string. By default whitespace.
#' @param token_col       The feature in tc that contains the token text.
#' @param column          The name of the new column in tc$tokens. Default is "code"
#' @param case_sensitive  logical, should lookup be case sensitive?
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
#' dict = data.frame(string = c('this is', 'a', 'test'), id=1:3)
#' tc = create_tcorpus(c('this is a test','This town is not big enough for a test'))
#' tc$code_dictionary(dict)
#' print(tc$tokens)
#' @aliases code_dictionary
tCorpus$set('public', 'code_dictionary', function(dict, regex_sep=' ', token_col='token', column='code', case_sensitive=F, batchsize=50000, flatten_colloc=T, ascii=F, low_memory=F, verbose=F){
  f = dictionary_lookup(self, dict, regex_sep=regex_sep, token_col=token_col, case_sensitive=case_sensitive, batchsize=batchsize, flatten_colloc=flatten_colloc, ascii=ascii, low_memory=low_memory, verbose=verbose)
  self$tokens[, (column) := f]
})



#' Dictionary lookup
#'
#' Fast matching of large lookup dictionaries.
#'
#' @param tc              A tCorpus
#' @param dict            A data.frame with the columns "string" and "id". The text in "string" will be matched to the tokens, and "id" will be the id given to tokens that matched
#' @param regex_sep       A regular expression for splitting multiple terms in dict$string. By default whitespace.
#' @param token_col       The feature in tc that contains the token text.
#' @param case_sensitive  logical, should lookup be case sensitive?
#' @param batchsize       Very large dictionaries will be matched in batches to prevent memory issues.
#' @param flatten_colloc  If true, collocations in the tokens (rows in tc$tokens) will be considered separate words. For example, "President_Obama" will be split to "president" "obama", so that "president obama" in the dictionary matches correctly.
#' @param ascii           If true, convert text to ascii before matching
#' @param low_memory      if true, use slower but more memory efficient algorithm
#' @param verbose         If true, report progress
#'
#' @return A vector with the id value (taken from dict$id) for each row in tc$tokens
#' @export
#'
#' @examples
#' dict = data.frame(string = c('this is', 'a', 'test'), id=1:3)
#' tc = create_tcorpus(c('this is a test','This town is not big enough for a test'))
#' search_dictionary(tc, dict)
search_dictionary <- function(tc, dict, regex_sep=' ', token_col='token', case_sensitive=F, batchsize=50000, flatten_colloc=T, ascii=F, low_memory=F, verbose=F){
  code = NULL; hit_id = NULL
  if (!is_tcorpus(tc)) stop('tc is not a tCorpus')
  f = dictionary_lookup(tc, dict, regex_sep=regex_sep, token_col=token_col, case_sensitive=case_sensitive, batchsize=batchsize, flatten_colloc=flatten_colloc, ascii=ascii, low_memory=low_memory, verbose=verbose)

  hits = tc$get(subset = !is.na(f))
  hits[, code := f[!is.na(f)]]
  if (!'sentence' %in% colnames(hits)) hits[, 'sentence' := numeric()]
  hits = subset(hits, select = intersect(c('doc_id','token_id','sentence',token_col), colnames(hits)))
  data.table::setnames(hits, token_col, 'feature')
  hits[, code := f[!is.na(f)]]

  newcode = hits$code != data.table::shift(hits$code, fill = NA)
  newcode[1] = T
  hits[, hit_id := cumsum(newcode)]

  queries = data.frame()
  featureHits(hits, queries)
}

dictionary_lookup <- function(tc, dict, regex_sep=' ', token_col='token', case_sensitive=F, batchsize=50000, flatten_colloc=T, ascii=F, low_memory=F, verbose=F){
  if (!token_col %in% tc$names) stop(sprintf('To use search_dictionary, the tCorpus must have a feature column with clean (not preprocessed) text, labeled "%s"', token_col))
  #fi = tc$feature_index(feature = 'token')
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

  ## find the candidate matches
  gf = globalFeatureVector$new(fi$feature, fi$global_i)

  candidates = vector('list', length(batches))
  counter = verbose_sum_counter(nrow(dict))
  for(i in seq_along(batches)){
    candidates[[i]] = fast_multitoken_stringmatch(batches[[i]], fi=fi, gf=gf, regex_sep=regex_sep, case_sensitive=case_sensitive)
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

  out = rep(NA, tc$n)
  out[index$i] = index$id
  out
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



fast_multitoken_stringmatch <- function(dict, fi, gf, regex_sep, case_sensitive=T){
  i = 1:length(dict$string)

  if (!case_sensitive) {
    string = tolower(dict$string)
    levels(fi$feature) = tolower(levels(fi$feature))
  }
  sn = stringi::stri_split(dict$string, regex=regex_sep)
  nterms = sapply(sn, length)
  candidates = vector('list', max(nterms))

  lt = data.table::data.table(feature = sapply(sn, data.table::first), id=dict$id, nterms=nterms, s_i=i, key='feature')
  lt = merge(fi, lt, by=c('feature'), allow.cartesian=T)

  is_end = lt$nterms == 1
  candidates[[1]] = lt[is_end, c('i','global_i','id','nterms'), with=F]
  lt = lt[!is_end,]

  for(i in 1:length(candidates)){
    if (nrow(lt) == 0) break
    hit = sapply(sn[lt$s_i], function(x) x[i+1]) == gf[lt$global_i + i, allow_na=T]
    hit[is.na(hit)] = F
    lt = lt[hit,]

    is_end = lt$nterms == i+1
    candidates[[i+1]] = lt[is_end,c('i','global_i','id','nterms'), with=F]
    lt = lt[!is_end,]
  }
  candidates = data.table::rbindlist(candidates)
  candidates = candidates[order(-candidates$nterms),] ## first go for candidates with the most terms (likely to be most specific)
  unique(candidates, by = c('global_i'))
}
