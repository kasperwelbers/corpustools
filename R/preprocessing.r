
#' Preprocess feature
#'
#' @param tc
#' @param input_col
#' @param output_col
#' @param language
#' @param use_stemming
#' @param lowercase
#' @param ngrams
#' @param ngram_context
#'
#' @export
preprocess_feature <- function(tc, column, new_column, language='english', use_stemming=F, lowercase=T, ngrams=1, ngram_context=c('document', 'sentence'), remove_accented=F, remove_punctuation=T){
  is_tcorpus(tc, T)
  if(is(tc, 'shattered_tCorpus')) return(shard_preprocess_feature(stc=tc, column=column, new_column=new_column, use_stemming=use_stemming, lowercase=lowercase, ngrams=ngrams, ngram_context=ngram_context, remove_accented=remove_accented, remove_punctuation=remove_punctuation))

  feature = get_column(tc, column)

  if(ngrams == 1) {
    feature = preprocess_words(feature, context=NA, language=language, use_stemming=use_stemming, lowercase=lowercase)
  } else {
    context = get_context(tc, context_level=ngram_context)
    feature = preprocess_words(feature, context=context, language=language, use_stemming=use_stemming, lowercase=lowercase, ngrams = ngrams, remove_accented=remove_accented)
  }
  set_column(tc, new_column, feature)
}

#' Filter features
#'
#' Similar to subsetting, but keeps data and only sets the . This way the vocabulary can be reduced while still beign able to bring results of analyses back to the full text
#'
#' @param tc
#' @param column
#' @param new_column
#' @param filter
#'
#' @return
#' @export
#'
#' @examples
filter_feature <- function(tc, column, new_column, filter){
  is_tcorpus(tc, T)
  if(is(tc, 'shattered_tCorpus')) return(shard_filter_feature(stc=tc, column=column, new_column=new_column, filter=filter))

  i = subset_i(tc, subset)
  feature = tc@data[[column]]
  feature[i] = NA
  set_column(tc, new_column, feature)
  tc
}

#' @export
preprocess_words <- function(x, context=NULL, language='english', use_stemming=F, lowercase=T, ngrams=1, replace_whitespace=T, remove_accented=F, remove_punctuation=T){
  language = match.arg(language, choices=c('danish','dutch','english','finnish','french','german','hungarian','italian','norwegian','porter','portuguese','romanian','russian','spanish','swedish','turkish'))
  if(!is(x, 'factor')) x = as.factor(x)
  if(replace_whitespace) levels(x) = gsub(' ', '_', levels(x), fixed=T)
  if(lowercase) levels(x) = tolower(levels(x))
  if(remove_accented) levels(x) = iconv(levels(x), to='ASCII//TRANSLIT')
  if(use_stemming) levels(x) = quanteda::char_wordstem(levels(x), language=language)
  if(remove_punctuation) levels(x)[!grepl("[[:alnum:]]", levels(x))] = NA

  if(ngrams > 1) {
    if(is.null(context)) stop('For ngrams, the "context" argument has to be specified. If no context is available, "context" can be NA')
    x = grouped_ngrams(x, context, ngrams)
  }
  x
}

## try to make an ngram function using ddply and rcpp, where ddply loops over contexts and rcpp
## also compare performance to just using rcpp to create ngrams for all words. For this, give a single vector of words, using global_i to add empty strings.
## or combine the two, so that ddply can be used with batches
grouped_ngrams <- function(words, group, n, filter=rep(T, length(words))){
  filter = filter & !is.na(words)
  words = words[filter]
  group = if(length(group) == 1) rep(group, length(words)) else group[filter]

  ngram_mat = matrix(ncol=n, nrow=length(words))
  for(i in 1:n) ngram_mat[,n-i+1] = c(rep('', i-1), as.character(words[1:(length(words)-i+1)]))

  newart = which(!duplicated(group))
  for(i in 1:(n-1)) {
    replace_i = newart+i-1
    replace_i = replace_i[replace_i <= nrow(ngram_mat)]
    if(length(replace_i) > 0) ngram_mat[replace_i, 1:(n-i)] = ''
  }

  ngrams = vector('character', length(filter))
  ngrams[which(filter)] = apply(ngram_mat, 1, stringi::stri_paste, collapse='_')
  as.factor(ngrams)
}


