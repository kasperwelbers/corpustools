preprocess_feature <- function(tc, column, new_column, lowercase=T, ngrams=1, ngram_context=c('document', 'sentence'), as_ascii=F, remove_punctuation=T, remove_stopwords=F, use_stemming=F, language='english'){
  is_tcorpus(tc, T)

  feature = tc$get(column)
  if (!methods::is(feature, 'factor')) feature = factor(feature)

  if (ngrams == 1) {
    evalhere_feature = preprocess_tokens(feature, context=NA, language=language, use_stemming=use_stemming, lowercase=lowercase, as_ascii=as_ascii, remove_punctuation=remove_punctuation, remove_stopwords=remove_stopwords)
  } else {
    context = tc$context(context_level=ngram_context, with_labels = F)
    evalhere_feature = preprocess_tokens(feature, context=context, language=language, use_stemming=use_stemming, lowercase=lowercase, ngrams = ngrams, as_ascii=as_ascii, remove_punctuation=remove_punctuation, remove_stopwords=remove_stopwords)
  }
  tc$set(column = new_column, value = evalhere_feature)
}


#' Preprocess tokens in a character vector
#'
#' @param x A character vector in which each element is a token (i.e. a tokenized text)
#' @param context Optionally, a character vector of the same length as x, specifying the context of token (e.g., document, sentence). Has to be given if ngram > 1
#' @param language The language used for stemming and removing stopwords
#' @param use_stemming Logical, use stemming. (Make sure the specify the right language!)
#' @param lowercase Logical, make token lowercase
#' @param ngrams A number, specifying the number of tokens per ngram. Default is unigrams (1).
#' @param replace_whitespace Logical. If TRUE, all whitespace is replaced by underscores
#' @param as_ascii Logical. If TRUE, tokens will be forced to ascii
#' @param remove_punctuation Logical. if TRUE, punctuation is removed
#' @param remove_stopwords Logical. If TRUE, stopwords are removed (Make sure to specify the right language!)
#'
#' @export
preprocess_tokens <- function(x, context=NULL, language='english', use_stemming=F, lowercase=T, ngrams=1, replace_whitespace=T, as_ascii=F, remove_punctuation=T, remove_stopwords=F){
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

create_ngrams <- function(tokens, group, n, label=T, sep = '/', empty='') {
  if (!length(tokens) == length(group)) stop("tokens has to be of same length as group")
  ng = .Call('corpustools_ngrams', PACKAGE = 'corpustools', tokens, group, n, sep, empty)
  ng = fast_factor(ng)
  if (label) return(ng) else return(as.numeric(ng))
}

grouped_ngrams <- function(tokens, group, n, filter=rep(T, length(tokens)), label=T){
  filter = filter & !is.na(tokens)
  tokens = tokens[filter]
  group = if (length(group) == 1) rep(group, length(tokens)) else group[filter]

  if (label) {
    ngrams = as.factor(rep(NA, length(filter)))
    ng = create_ngrams(tokens, group, n, label=label)
    levels(ngrams) = levels(ng)
    ngrams[which(filter)] = ng
  } else {
    ngrams = vector('numeric', length(filter))
    ngrams[which(filter)] = create_ngrams(tokens, group, n, label=label)
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

