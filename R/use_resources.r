use_search_resource <- function(tc, re, batchsize, verbose){
  NULL
  ## for resources that are best implemented using the current search_ functions.
}

## !!!!!! check whether iconv is necessary!!!


### most resources work with huge stringmatch tables
### therefore, the following functions are optimized for mathing many strings at once, as opposed to the more versatile search_features function.

normalize_string <- function(x, lowercase=T, ascii=T, trim=T){
  if(lowercase) x = tolower(x)
  if(ascii) x = iconv(x, to='ASCII//TRANSLIT')
  if(trim) x = stringi::stri_trim(x)
  x
}


use_stringmatch_resource <- function(tc, re, regex_sep, case_sensitive, batchsize=50000, flatten_colloc=T, lowercase=F, ascii=F, verbose=F){
  fi = get_feature_index(tc)

  levels(fi$feature) = normalize_string(levels(fi$feature), lowercase=lowercase, ascii = ascii)
  setkey(fi, 'feature')

  re$string = normalize_string(re$string, lowercase=lowercase, ascii=ascii)


  if(flatten_colloc) {
    is_colloc = grep(' |_', levels(fi$feature))
    if(length(is_colloc) > 0){
      fi = flatten_collocations(fi, 'feature', 'global_i')
      message(sprintf('flattened %s collocations', length(is_colloc)))
    } else {
      #message('flatten_colloc (collocations) is set to TRUE, but no collocations were found.')
      flatten_colloc = F ## if there are no collocations, ignore flatten_colloc == T
    }
  }
  #if(use_regex) re = expand_resource_regex(re, features=levels(fi$features)) ## not yet implemented. expand resource ir use_regex=T: if there are regular expressions in the resource and use_regex==T, then first stringmatch the regex in the vocabulary and expand the resource with all returned strings
  ## make batches of the resource

  n.batches = ceiling(nrow(re) / batchsize)
  re = re[order(re$string),]
  batch_i = rep(1:n.batches, length.out=nrow(re)) ## distribute evenly over batches (basically UNordered by alphabet) so that frequently occuring names (that lead to huge matches) are more evenly distributed
  #batch_i = batch_i[order(batch_i)] ## alternatively, ordering by alphabet is probably faster for matching, but my current guess is that memory is more important than speed
  batches = split(re, batch_i)

  ## find the candidate matches
  gf = global_feature_vector(fi)
  candidates = vector('list', length(batches))
  counter = verbose_sum_counter(nrow(re))
  for(i in seq_along(batches)){
    candidates[[i]] = fast_multiword_stringmatch(batches[[i]], fi=fi, gf=gf, regex_sep=regex_sep, case_sensitive=case_sensitive)
    if(verbose) counter(nrow(batches[[i]]))
  }
  if(verbose) message('Binding results')
  candidates = rbindlist(candidates)

  ## select candidates (at some point candidate selection could be done better. Perhaps even taking context into account. BUT NOT TODAY!!)
  candidates = candidates[order(-candidates$nterms),] ## go for candidates with the most terms (= most specific)
  candidates = unique(candidates, by = c('global_i'))

  ## turn into a long format, where each token is on a separate row
  index_i = rep(1:nrow(candidates), times=candidates$nterms)
  index_nr = unlist(sapply(candidates$nterms, function(x) seq(1,x), simplify = F))
  index = candidates[index_i, c('global_i','id', 'i')]
  if(flatten_colloc){
    ## if collocations have been flattened, the real i in the tcorpus (which is still a collocation) has multiple global_i (which have been flattened)
    index$global_i = index$global_i + (index_nr - 1)
    index$i = fi$i[match(index$global_i, fi$global_i)]
  } else {
    index$i = index$i + (index_nr - 1) ## if global_i's have only one match to i's, we can recalculate the i's directly
  }

  index
}

global_feature_vector <- function(fi){
  global_f = rep(as.factor(''), nrow(fi))
  levels(global_f) = c('', levels(fi$feature))
  global_f[fi$global_i] = fi$feature
  global_f
}

fast_multiword_stringmatch <- function(re, fi, gf, regex_sep, case_sensitive=T){
  i = 1:length(re$string)

  if(!case_sensitive) {
    string = tolower(re$string)
    levels(fi$feature) = tolower(levels(fi$feature))
  }
  sn = stringi::stri_split(re$string, regex=regex_sep)
  nterms = sapply(sn, length)
  candidates = vector('list', max(nterms))

  lt = data.table(feature = sapply(sn, first), id=re$id, nterms=nterms, s_i=i, key='feature')
  lt = merge(fi, lt, by=data.table::.EACHI, allow.cartesian=T)

  is_end = lt$nterms == 1
  candidates[[1]] = lt[is_end, c('i','global_i','id','nterms'), with=F]
  lt = lt[!is_end,]

  for(i in 1:length(candidates)){
    if(nrow(lt) == 0) break
    hit = sapply(sn[lt$s_i], function(x) x[i+1]) == gf[lt$global_i + i]
    hit[is.na(hit)] = F
    lt = lt[hit,]

    is_end = lt$nterms == i+1
    candidates[[i+1]] = lt[is_end,c('i','global_i','id','nterms'), with=F]
    lt = lt[!is_end,]
  }
  candidates = rbindlist(candidates)
  candidates = candidates[order(-candidates$nterms),] ## first go for candidates with the most terms (likely to be most specific)
  unique(candidates, by = c('global_i'))
}
