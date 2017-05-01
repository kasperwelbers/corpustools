preprocess_feature <- function(tc, column, new_column, lowercase=T, ngrams=1, ngram_context=c('document', 'sentence'), as_ascii=F, remove_punctuation=T, remove_stopwords=F, use_stemming=F, language='english'){
  is_tcorpus(tc, T)

  feature = tc$get(column)
  if (!methods::is(feature, 'factor')) feature = factor(feature)

  if (ngrams == 1) {
    evalhere_feature = preprocess_words(feature, context=NA, language=language, use_stemming=use_stemming, lowercase=lowercase, as_ascii=as_ascii, remove_punctuation=remove_punctuation, remove_stopwords=remove_stopwords)
  } else {
    context = tc$context(context_level=ngram_context, with_labels = F)
    evalhere_feature = preprocess_words(feature, context=context, language=language, use_stemming=use_stemming, lowercase=lowercase, ngrams = ngrams, as_ascii=as_ascii, remove_punctuation=remove_punctuation, remove_stopwords=remove_stopwords)
  }
  tc$set(column = new_column, value = evalhere_feature)
}

#' Filter features
#'
#' Similar to subsetting, but instead of deleting rows, the rows for the specified column as set to NA. This way the vocabulary can be reduced while still beign able to bring results of analyses back to the full text
#'
subset_feature_fun <- function(tc, i, column, new_column, inverse=F){
  evalhere_i = i
  if (column == new_column) {
    tc$set(new_column, NA, subset = evalhere_i, copy=F)
  } else {
    evalhere_feature = tc$get(column)
    evalhere_feature[evalhere_i] = NA
    tc$set(new_column, evalhere_feature, copy=F)
  }
  invisible(tc)
}

#' @export
preprocess_words <- function(x, context=NULL, language='english', use_stemming=F, lowercase=T, ngrams=1, replace_whitespace=T, as_ascii=F, remove_punctuation=T, remove_stopwords=F){
  language = match.arg(language, choices=c('danish','dutch','english','finnish','french','german','hungarian','italian','norwegian','porter','portuguese','romanian','russian','spanish','swedish','turkish'))
  if (!methods::is(x, 'factor')) x = fast_factor(x)
  if (replace_whitespace) levels(x) = gsub(' ', '_', levels(x), fixed=T)
  if (lowercase) levels(x) = tolower(levels(x))
  if (as_ascii) levels(x) = iconv(levels(x), to='ASCII//TRANSLIT')
  if (remove_stopwords) levels(x)[levels(x) %in% get_stopwords(language)] = NA
  if (remove_punctuation) levels(x)[!grepl("[[:alnum:]]", levels(x))] = NA
  if (use_stemming) levels(x) = SnowballC::wordStem(levels(x), language=language)

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
  if (methods::is(words, 'factor')) {
    ngrams = split(as.numeric(t(ngrams)), rep(1:nrow(ngrams), each = ncol(ngrams)))
  } else {
    ngrams = split(t(ngrams), rep(1:nrow(ngrams), each = ncol(ngrams)))
  }
  ungrams = unique(ngrams) ## only perform string binding and hashing on unique ngrams (never keep all ngrams in memory as string)
  ngrams_i = match(ngrams, ungrams)

  if(hash) {
    if (!requireNamespace('digest', quietly = T)) stop('"digest" package needs to be installed')
    ashash <- function(x) readBin(digest::digest(x, 'sha1', raw=T), what='integer')
    hash = as.integer(sapply(ungrams, ashash))
    return(hash[ngrams_i])
  } else {
    if (methods::is(words, 'factor') & label) {
      ungrams = sapply(ungrams, function(x) ifelse(is.na(x), '', levels(words)[x]), simplify = F)
    }
    ungrams = if (label) as.factor(stringi::stri_paste_list(ungrams, sep='/')) else 1:length(ungrams)
    return(ungrams[ngrams_i])
  }
}

grouped_ngrams <- function(words, group, n, filter=rep(T, length(words)), label=T, hash=F){
  filter = filter & !is.na(words)
  words = words[filter]
  group = if (length(group) == 1) rep(group, length(words)) else group[filter]

  if (label &! hash) {
    ngrams = as.factor(rep(NA, length(filter)))
    ng = create_ngrams(words, group, n, label=label, hash=hash)
    levels(ngrams) = levels(ng)
    ngrams[which(filter)] = ng
  } else {
    ngrams = vector('numeric', length(filter))
    ngrams[which(filter)] = create_ngrams(words, group, n, label=label, hash=hash)
  }
  ngrams
}

#' Get a character vector of stopwords
#'
#' @param lang The language. Current options are: "danish", "dutch", "english", "finnish", "french", "german", "hungarian", "italian", "norwegian", "portuguese", "romanian", "russian", "spanish" and "swedish"
#'
#' @return A character vector containing stopwords
#' @export
get_stopwords <- function(lang){
  lang = match.arg(lang, names(corpustools::stopwords_list))
  corpustools::stopwords_list[[lang]]
}

