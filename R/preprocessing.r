
#' Preprocess feature
#'
#' @param tc
#' @param language
#' @param use_stemming
#' @param lowercase
#' @param ngrams
#' @param ngram_context
#' @param column
#' @param new_column
#' @param as_ascii
#' @param remove_punctuation
preprocess_feature <- function(tc, column, new_column, lowercase=T, ngrams=1, ngram_context=c('document', 'sentence'), as_ascii=F, remove_punctuation=T, remove_stopwords=F, use_stemming=F, language='english'){
  is_tcorpus(tc, T)
  if (is(tc, 'shattered_tCorpus')) return(shard_preprocess_feature(stc=tc, column=column, new_column=new_column, use_stemming=use_stemming, lowercase=lowercase, ngrams=ngrams, ngram_context=ngram_context, as_ascii=as_ascii, remove_punctuation=remove_punctuation, remove_stopwords=remove_stopwords))

  feature = tc$data(column)
  if (!is(feature, 'factor')) feature = fast_factor(feature)

  if (ngrams == 1) {
    feature = preprocess_words(feature, context=NA, language=language, use_stemming=use_stemming, lowercase=lowercase, as_ascii=as_ascii, remove_punctuation=remove_punctuation, remove_stopwords=remove_stopwords)
  } else {
    context = tc$context(context_level=ngram_context, with_labels = F)
    feature = preprocess_words(feature, context=context, language=language, use_stemming=use_stemming, lowercase=lowercase, ngrams = ngrams, as_ascii=as_ascii, remove_punctuation=remove_punctuation, remove_stopwords=remove_stopwords)
  }
  tc$set_column(column = new_column, value = feature)
}

#' Filter features
#'
#' Similar to subsetting, but instead of deleting rows, the rows for the specified column as set to NA. This way the vocabulary can be reduced while still beign able to bring results of analyses back to the full text
#'
#' @param tc
#' @param column
#' @param new_column
#' @param filter
#'
#' @return
subset_feature_fun <- function(tc, column, new_column, subset, inverse=F){
  i = subset_i(tc, subset, inverse=!inverse) ## double inverse, because inverse in subset_i means "give me i for the rows for which condition is not true", and here we use i to set the NA.
  if (column == new_column) {
    tc = tc$set_column(new_column, NA, subset = i)
  } else {
    feature = tc$data(column)
    feature[i] = NA
    tc = tc$set_column(new_column, feature)
  }
  tc
}

#' @export
preprocess_words <- function(x, context=NULL, language='english', use_stemming=F, lowercase=T, ngrams=1, replace_whitespace=T, as_ascii=F, remove_punctuation=T, remove_stopwords=F){
  language = match.arg(language, choices=c('danish','dutch','english','finnish','french','german','hungarian','italian','norwegian','porter','portuguese','romanian','russian','spanish','swedish','turkish'))
  if (!is(x, 'factor')) x = fast_factor(x)
  if (replace_whitespace) levels(x) = gsub(' ', '_', levels(x), fixed=T)
  if (lowercase) levels(x) = tolower(levels(x))
  if (as_ascii) levels(x) = iconv(levels(x), to='ASCII//TRANSLIT')
  if (remove_stopwords) levels(x)[levels(x) %in% quanteda::stopwords(language)] = NA
  if (remove_punctuation) levels(x)[!grepl("[[:alnum:]]", levels(x))] = NA
  if (use_stemming) levels(x) = quanteda::char_wordstem(levels(x), language=language)

  if (ngrams > 1) {
    if (is.null(context)) stop('For ngrams, the "context" argument has to be specified. If no context is available, "context" can be NA')
    x = grouped_ngrams(x, context, ngrams)
  }
  x
}

create_ngrams <- function(words, group, n, label=T, hash=F) {
  ngrams = matrix(ncol=n, nrow=length(words))
  for(i in 1:n) ngrams[,n-i+1] = shift(words, n=i-1, fill = '')

  newart = which(!duplicated(group))
  for(i in 1:(n-1)) {
    replace_i = newart+i-1
    replace_i = replace_i[replace_i <= nrow(ngrams)]
    if (length(replace_i) > 0) ngrams[replace_i, 1:(n-i)] = ''
  }
  if (is(words, 'factor')) {
    ngrams = split(as.numeric(t(ngrams)), rep(1:nrow(ngrams), each = ncol(ngrams)))
  } else {
    ngrams = split(t(ngrams), rep(1:nrow(ngrams), each = ncol(ngrams)))
  }
  ungrams = unique(ngrams) ## only perform string binding and hashing on unique ngrams (never keep all ngrams in memory as string)
  ngrams_i = match(ngrams, ungrams)

  if(hash) {
    require(digest)
    ashash <- function(x) readBin(digest::digest(x, 'sha1', raw=T), what='integer')
    hash = as.integer(sapply(ungrams, ashash))
    return(hash[ngrams_i])
  } else {
    if (is(words, 'factor') & label) {
      ungrams = sapply(ungrams, function(x) ifelse(is.na(x), '', levels(words)[x]), simplify = F)
    }
    ungrams = if (label) fast_factor(stringi::stri_paste_list(ungrams, sep='/')) else 1:length(ungrams)
    return(ungrams[ngrams_i])
  }
}

grouped_ngrams <- function(words, group, n, filter=rep(T, length(words)), label=T, hash=F){
  filter = filter & !is.na(words)
  words = words[filter]
  group = if (length(group) == 1) rep(group, length(words)) else group[filter]

  if (label &! hash) {
    ngrams = fast_factor(rep(NA, length(filter)))
    ng = create_ngrams(words, group, n, label=label, hash=hash)
    levels(ngrams) = levels(ng)
    ngrams[which(filter)] = ng
  } else {
    ngrams = vector('numeric', length(filter))
    ngrams[which(filter)] = create_ngrams(words, group, n, label=label, hash=hash)
  }
  ngrams
}
