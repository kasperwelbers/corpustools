#' Preprocess feature
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{
#' preprocess(column='token', new_column='feature', lowercase=T, ngrams=1,
#'            ngram_context=c('document', 'sentence'), as_ascii=F, remove_punctuation=T,
#'            remove_stopwords=F, remove_numbers=F, use_stemming=F, language='english',
#'            min_freq=NULL, min_docfreq=NULL, max_freq=NULL, max_docfreq=NULL, min_char=NULL, max_char=NULL)
#'            }
#'
#' @param column the column containing the feature to be used as the input
#' @param new_column the column to save the preprocessed feature. Can be a new column or overwrite an existing one.
#' @param lowercase make feature lowercase
#' @param ngrams create ngrams. The ngrams match the rows in the token data, with the feature in the row being the last token of the ngram. For example, given the features "this is an example", the third feature ("an") will have the trigram "this_is_an". Ngrams at the beginning of a context will have empty spaces. Thus, in the previous example, the second feature ("is") will have the trigram "_is_an".
#' @param ngram_context Ngrams will not be created across contexts, which can be documents or sentences. For example, if the context_level is sentences, then the last token of sentence 1 will not form an ngram with the first token of sentence 2.
#' @param as_ascii convert characters to ascii. This is particularly usefull for dealing with special characters.
#' @param remove_punctuation remove (i.e. make NA) any features that are \emph{only} punctuation (e.g., dots, comma's)
#' @param remove_stopwords remove (i.e. make NA) stopwords. (!) Make sure to set the language argument correctly.
#' @param remove_numbers remove features that are only numbers
#' @param use_stemming reduce features (tokens) to their stem
#' @param language The language used for stopwords and stemming
#' @param min_freq an integer, specifying minimum token frequency.
#' @param min_docfreq an integer, specifying minimum document frequency.
#' @param max_freq an integer, specifying minimum token frequency.
#' @param max_docfreq an integer, specifying minimum document frequency.
#' @param min_char an integer, specifying minimum number of characters in a term
#' @param max_char an integer, specifying maximum number of characters in a term
#'
#' @name tCorpus$preprocess
#' @aliases preprocess
#' @examples
#' tc = create_tcorpus('I am a SHORT example sentence! That I am!')
#'
#' ## default is lowercase without punctuation
#' tc$preprocess('token', 'preprocessed_1')
#'
#' ## delete stopwords and perform stemming
#' tc$preprocess('token', 'preprocessed_2', remove_stopwords = TRUE, use_stemming = TRUE)
#'
#' ## filter on minimum frequency
#' tc$preprocess('token', 'preprocessed_3', min_freq=2)
#'
#' ## make ngrams
#' tc$preprocess('token', 'preprocessed_4', ngrams = 3)
#'
#' tc$tokens
tCorpus$set('public', 'preprocess', function(column='token', new_column='feature', lowercase=T, ngrams=1, ngram_context=c('document', 'sentence'), as_ascii=F, remove_punctuation=T, remove_stopwords=F, remove_numbers=F, use_stemming=F, language='english', min_freq=NULL, min_docfreq=NULL, max_freq=NULL, max_docfreq=NULL, min_char=NULL, max_char=NULL) {
  column = match.arg(column, self$names)
  invisible(preprocess_feature(self, column=column, new_column=new_column, lowercase=lowercase, ngrams=ngrams, ngram_context=ngram_context, as_ascii=as_ascii, remove_punctuation=remove_punctuation, remove_stopwords=remove_stopwords, remove_numbers=remove_numbers, use_stemming=use_stemming, language=language, min_freq=min_freq, min_docfreq=min_docfreq, max_freq=max_freq, max_docfreq=max_docfreq, min_char=min_char, max_char=max_char))
})

#' Filter features
#'
#' @description
#' Similar to using \link{tCorpus$subset}, but instead of deleting rows it only sets rows for a specified feature to NA. This can be very convenient, because it enables only a selection of features to be used in an analysis (e.g. a topic model) but maintaining the context of the full article, so that results can be viewed in this context (e.g. a topic browser).
#'
#' Just as in subset, it is easy to use objects and functions in the filter, including the special functions for using term frequency statistics (see documentation for \link{tCorpus$subset}).
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{feature_subset(column, new_column, subset)}
#'
#' @param column the column containing the feature to be used as the input
#' @param subset logical expression indicating rows to keep in the tokens data. i.e. rows for which the logical expression is FALSE will be set to NA.
#' @param new_column the column to save the filtered feature. Can be a new column or overwrite an existing one.
#' @param min_freq an integer, specifying minimum token frequency.
#' @param min_docfreq an integer, specifying minimum document frequency.
#' @param max_freq an integer, specifying minimum token frequency.
#' @param max_docfreq an integer, specifying minimum document frequency.
#' @param min_char an integer, specifying minimum characters in a token
#' @param max_char an integer, specifying maximum characters in a token
#'
#' @name tCorpus$feature_subset
#' @aliases feature_subset
#' @examples
#' tc = create_tcorpus('a a a a b b b c c')
#'
#' tc$feature_subset('token', 'tokens_subset1', subset = token_id < 5)
#' tc$feature_subset('token', 'tokens_subset2', subset = freq_filter(token, min = 3))
#'
#' tc$tokens
tCorpus$set('public', 'feature_subset', function(column, subset=NULL, new_column=column, inverse=F, min_freq=NULL, min_docfreq=NULL, max_freq=NULL, max_docfreq=NULL, min_char=NULL, max_char=NULL, copy=F){
  column = match.arg(column, self$names)
  if (new_column %in% c('doc_id','sentence','token_id')) stop('The position columns (doc_id, sent_i, token_i) cannot be used')
  if (class(substitute(subset)) %in% c('call', 'name')) subset = self$eval(substitute(subset), parent.frame())

  if (copy) {
    selfcopy = self$copy()$feature_subset(column=column, new_column=new_column, subset=subset, inverse=inverse, copy=F)
    return(selfcopy)
  }

  if (methods::is(subset, 'numeric')) subset = 1:self$n %in% subset ## this can be the case if a vector of indices is passed to subset (which is not a valid call, but is allowed for convenience because it is a common way of subsetting)

  .subset = if (inverse) !subset else subset

  if (new_column %in% self$names) {
    old_column = self$get(column)
    self$set(new_column, NA)
    self$set(new_column, old_column, subset = .subset)
  } else {
    self$set(new_column, self$get(column), subset = .subset)
  }

  if (!is.null(min_freq) || !is.null(max_freq) || !is.null(min_docfreq) || !is.null(max_docfreq) || !is.null(min_char) || !is.null(max_char)) {
    self$preprocess(column=new_column, new_column=new_column, lowercase=F, remove_punctuation=F,
                    min_freq=min_freq, max_freq=max_freq, min_docfreq=min_docfreq, max_docfreq=max_docfreq, min_char=min_char, max_char=max_char)
  }

  self$tokens[]
  invisible(self)
})

preprocess_feature <- function(tc, column, new_column, lowercase=T, ngrams=1, ngram_context=c('document', 'sentence'), as_ascii=F, remove_punctuation=T,remove_stopwords=F, remove_numbers=F, use_stemming=F, language='english', min_freq=NULL, min_docfreq=NULL, max_freq=NULL, max_docfreq=NULL, min_char=NULL, max_char=NULL){
  is_tcorpus(tc)

  feature = tc$get(column)
  if (!methods::is(feature, 'factor')) feature = factor(feature)

  if (ngrams == 1 && is.null(min_docfreq)) {
    .feature = preprocess_tokens(feature, context=NA, language=language, use_stemming=use_stemming, lowercase=lowercase, as_ascii=as_ascii, remove_punctuation=remove_punctuation, remove_stopwords=remove_stopwords, remove_numbers=remove_numbers, min_freq=min_freq, min_docfreq=min_docfreq, max_freq=max_freq, max_docfreq=max_docfreq, min_char=min_char, max_char=max_char)
  } else {
    context = tc$context(context_level=ngram_context, with_labels = F)
    .feature = preprocess_tokens(feature, context=context, language=language, use_stemming=use_stemming, lowercase=lowercase, ngrams = ngrams, as_ascii=as_ascii, remove_punctuation=remove_punctuation, remove_stopwords=remove_stopwords, remove_numbers=remove_numbers, min_freq=min_freq, min_docfreq=min_docfreq, max_freq=max_freq, max_docfreq=max_docfreq, min_char=min_char, max_char=max_char)
  }
  tc$set(column = new_column, value = .feature)
  tc$tokens[]
  tc
}


#' Preprocess tokens in a character vector
#'
#' @param x A character or factor vector in which each element is a token (i.e. a tokenized text)
#' @param context Optionally, a character vector of the same length as x, specifying the context of token (e.g., document, sentence). Has to be given if ngram > 1
#' @param language The language used for stemming and removing stopwords
#' @param use_stemming Logical, use stemming. (Make sure the specify the right language!)
#' @param lowercase Logical, make token lowercase
#' @param ngrams A number, specifying the number of tokens per ngram. Default is unigrams (1).
#' @param replace_whitespace Logical. If TRUE, all whitespace is replaced by underscores
#' @param as_ascii Logical. If TRUE, tokens will be forced to ascii
#' @param remove_punctuation Logical. if TRUE, punctuation is removed
#' @param remove_stopwords Logical. If TRUE, stopwords are removed (Make sure to specify the right language!)
#' @param remove_numbers remove features that are only numbers
#' @param min_freq an integer, specifying minimum token frequency.
#' @param min_docfreq an integer, specifying minimum document frequency.
#' @param max_freq an integer, specifying minimum token frequency.
#' @param max_docfreq an integer, specifying minimum document frequency.
#' @param min_char an integer, specifying minimum number of characters in a term
#' @param max_char an integer, specifying maximum number of characters in a term
#' @param ngram_skip_empty if ngrams are used, determines whether empty (filtered out) terms are skipped (i.e. c("this", NA, "test"), becomes "this_test") or
#'
#' @examples
#' tokens = c('I', 'am', 'a', 'SHORT', 'example', 'sentence', '!')
#'
#' ## default is lowercase without punctuation
#' preprocess_tokens(tokens)
#'
#' ## optionally, delete stopwords, perform stemming, and make ngrams
#' preprocess_tokens(tokens, remove_stopwords = TRUE, use_stemming = TRUE)
#' preprocess_tokens(tokens, context = NA, ngrams = 3)
#' @export
#' @return a factor vector
preprocess_tokens <- function(x, context=NULL, language='english', use_stemming=F, lowercase=T, ngrams=1, replace_whitespace=F, as_ascii=F, remove_punctuation=T, remove_stopwords=F, remove_numbers=F, min_freq=NULL, min_docfreq=NULL, max_freq=NULL, max_docfreq=NULL, min_char=NULL, max_char=NULL, ngram_skip_empty=T){
  language = match.arg(language, choices=c('danish','dutch','english','finnish','french','german','hungarian','italian','norwegian','porter','portuguese','romanian','russian','spanish','swedish','turkish'))
  if (!methods::is(x, 'factor')) x = fast_factor(x)
  if (replace_whitespace) levels(x) = gsub(' ', '_', levels(x), fixed=T)
  if (lowercase) levels(x) = tolower(levels(x))
  if (as_ascii) {
    levels(x) = stringi::stri_trans_general(levels(x),"any-latin")
    levels(x) = stringi::stri_trans_general(levels(x),"latin-ascii")
  }
  if (remove_stopwords) levels(x)[levels(x) %in% get_stopwords(language)] = NA

  if (remove_punctuation && remove_numbers) levels(x)[!grepl("[[:alpha:]]", levels(x))] = NA
  if (remove_punctuation && !remove_numbers) levels(x)[!grepl("[[:alnum:]]", levels(x))] = NA
  if (!remove_punctuation && remove_numbers) levels(x)[!grepl("[[:alpha:][:punct:]]", levels(x))] = NA

  if (use_stemming) levels(x) = SnowballC::wordStem(levels(x), language=language)

  if (ngrams > 1) {
    if (is.null(context)) stop('For ngrams, the "context" argument has to be specified. If no context is available, "context" can be NA')
    x = grouped_ngrams(x, context, ngrams, skip_empty=ngram_skip_empty)
  }

  if (!is.null(min_docfreq) | !is.null(max_docfreq)) {
    if (is.null(min_docfreq)) min_docfreq = -Inf
    if (is.null(max_docfreq)) max_docfreq = Inf
    if (is.null(context)) {
      freq_table = unique(data.frame(doc_id=1, x=x))
    } else {
      freq_table = unique(data.frame(doc_id=context, x=x))
    }
    freq_table = table(droplevels(freq_table$x))
    levels(x)[!levels(x) %in% x_filter(freq_table, min=min_docfreq, max=max_docfreq)] = NA
  }

  if (!is.null(min_freq) | !is.null(max_freq)) {
    if (is.null(min_freq)) min_freq = -Inf
    if (is.null(max_freq)) max_freq = Inf

    freq_table = table(droplevels(x))
    levels(x)[!levels(x) %in% x_filter(freq_table, min=min_freq, max=max_freq)] = NA
  }

  if (!is.null(min_char) | !is.null(max_char)) {
    if (is.null(min_char)) min_char = -Inf
    if (is.null(max_char)) max_char = Inf
    len = stringi::stri_length(levels(x))
    levels(x)[len < min_char | len > max_char] = NA
  }

  x
}

create_ngrams <- function(tokens, group, n, label=T, sep = '/', empty='') {
  if (!length(tokens) == length(group)) stop("tokens has to be of same length as group")

  ng = ngrams_cpp(tokens, group, n, sep, empty)
  ng = fast_factor(ng)
  if (label) return(ng) else return(as.numeric(ng))
}

grouped_ngrams <- function(tokens, group, n, filter=rep(T, length(tokens)), label=T, skip_empty=T){
  filter = filter & !is.na(tokens)

  if (skip_empty) {
    tokens = tokens[filter]
    group = if (length(group) == 1) rep(group, length(tokens)) else group[filter]
  } else {
    if (methods::is(tokens, 'factor')) levels(tokens) = union(levels(tokens), '')
    group = if (length(group) == 1) rep(group, length(tokens)) else group
  }

  if (label) {
    if (skip_empty) {
      ngrams = as.factor(rep(NA, length(filter)))
      ng = create_ngrams(tokens, group, n, label=label)
      levels(ngrams) = levels(ng)
      ngrams[which(filter)] = ng
    } else {
      ngrams = create_ngrams(tokens, group, n, label=label)
    }
  } else {
    if (skip_empty) {
      ngrams = vector('numeric', length(filter))
      ngrams[which(filter)] = create_ngrams(tokens, group, n, label=label)
    } else {
      ngrams = create_ngrams(tokens, group, n, label=label)
      ngrams[!filter] = NA
    }
  }
  ngrams
}

#' Get a character vector of stopwords
#'
#' @param lang The language. Current options are: "danish", "dutch", "english", "finnish", "french", "german", "hungarian", "italian", "norwegian", "portuguese", "romanian", "russian", "spanish" and "swedish"
#'
#' @return A character vector containing stopwords
#' @examples
#' en_stop = get_stopwords('english')
#' nl_stop = get_stopwords('dutch')
#' ge_stop = get_stopwords('german')
#'
#' head(en_stop)
#' head(nl_stop)
#' head(ge_stop)
#' @export
get_stopwords <- function(lang){
  lang = match.arg(lang, names(corpustools::stopwords_list))
  corpustools::stopwords_list[[lang]]
}


