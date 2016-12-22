######  TCORPUS CLASS  ######

## define create_tcorpus class
tCorpus = setClass("tCorpus",
                   slots = c(data = 'data.table',
                             doc_meta = 'data.table',
                             feature_index = 'data.table',
                             p = 'list'),
                   prototype=list(
                     p = list(feature_index=F, context_level=NA, max_window_size=NA),
                     feature_index = data.table()
                   ))

#' @export
as.tcorpus <- function(x) UseMethod('as.tcorpus')

#' @export
as.tcorpus.tCorpus <- function(x) x

#' @export
as.tcorpus.default <- function(x) stop('x has to be a tCorpus object')
## params: preprocess_params=list, filter_params,

###### create_tcorpus method ######

#' Create a tCorpus
#'
#' @rdname create_tcorpus
#'
#' @param x
#' @param doc_meta A data.frame with document meta information (e.g., date, source). The rows of the data.frame need to match the values of x
#' @param split_sentences Logical. If TRUE, the sentence number of tokens is also computed. This is done using the sentence tokenization of quanteda::tokenize, which is noted by the authors to be good (especially for english) but not perfect.
#' @param max_words An integer. Limits the number of words per document to the specified number
#' @param max_sentences An integer. Limits the number of sentences per document to the specified number. If set when split_sentences == FALSE, split_sentences will be set to TRUE.
#'
#' @export
#' @name create_tcorpus
create_tcorpus <- function(x, ...) {
  UseMethod('create_tcorpus')
}

#' @rdname create_tcorpus
#' @export
create_tcorpus.character <- function(x, doc_id=1:length(x), doc_meta=NULL, split_sentences=F, max_sentences=NULL, max_words=NULL, verbose=F) {
  if(!is.null(doc_meta)){
      if(!is(doc_meta, 'data.frame')) stop('"doc_meta" is not a data.frame or data.table')
      if(!nrow(doc_meta) == length(x)) stop('The number of rows in "doc_meta" does not match the number of texts in "x"')
      doc_meta = data.table(doc_meta)
  } else {
    if(!length(doc_id) == length(x)) stop('"doc_id" is not of the same length as "x"')
    doc_meta = data.table(doc_id=doc_id)
  }
  tCorpus(data = tokenize_to_datatable(x, split_sentences=split_sentences, max_sentences=max_sentences, max_words=max_words, verbose=verbose),
          doc_meta = doc_meta)
}

#' @rdname create_tcorpus
#' @export
create_tcorpus.factor <- function(x, doc_meta=NULL, split_sentences=F, max_sentences=NULL, max_words=NULL) {
  create_tcorpus(as.character(x), doc_meta=doc_meta, split_sentences=split_sentences, max_sentences=max_sentences, max_words=max_words)
}


#' @rdname create_tcorpus
#' @export
create_tcorpus.data.frame <- function(d, text_column='text', split_sentences=F, max_sentences=NULL, max_words=NULL) {
  create_tcorpus(d[,text_column],
                 doc_meta = d[,!colnames(d) == text_column],
                 split_sentences = split_sentences, max_sentences = max_sentences, max_words = max_words)
}

#' Create a tcorpus based on tokens (i.e. preprocessed texts)
#'
#' @param tokens
#' @param doc_col
#' @param word_i_col
#' @param sent_i_col
#' @param doc_meta
#' @param feature_cols
#'
#' @export
tokens_to_tcorpus <- function(tokens, doc_col, word_i_col, sent_i_col=NULL, doc_meta=NULL, feature_cols=NULL) {
  for(cname in c(doc_col, word_i_col, sent_i_col)){
    if(!cname %in% colnames(tokens)) stop(sprintf('"%s" is not an existing columnname in "tokens"', cname))
  }

  if(!is(tokens[,word_i_col], 'numeric')) stop('word_i_col has to be numeric')
  if(!is.null(sent_i_col)) if(!is(tokens[,sent_i_col], 'numeric')) stop('sent_i_col has to be numeric')

  if(!is.null(doc_meta)){
    if(!doc_col %in% colnames(doc_meta)) stop(sprintf('"doc_meta" does not contain the document id column (%s)', doc_col))
  }

  ## order and index doc_id, sent_i, word_i
  if(!is.null(sent_i_col)){
    tokens = tokens[order(tokens[,doc_col], tokens[,sent_i_col], tokens[,word_i_col]),]
  } else {
    tokens = tokens[order(tokens[,doc_col], tokens[,word_i_col]),]
  }
  #docs = unique(tokens[,doc_col])
  positions = data.frame(doc_id= as.factor(tokens[,doc_col]),
                         sent_i = tokens[,sent_i_col],
                         word_i = tokens[,word_i_col])

  ## make sure that sent_i and doc_id are locally unique within documents
  if(!is.null(sent_i_col)){
    positions$sent_i = local_position(positions$sent_i, positions$doc_id, presorted = T) ## make sure sentences are locally unique within documents (and not globally)
    positions$word_i = global_position(positions$word_i, positions$sent_i, presorted=T) ## make word positions globally unique, taking sentence id into account (in case words are locally unique within sentences)
  }
  positions$word_i = local_position(positions$word_i, positions$doc_id, presorted=T) ## make words locally unique within documents

  ## match doc_meta
  if(!is.null(doc_meta)) {
    doc_meta = data.table(doc_meta[match(docs, doc_meta[,doc_col]),])
  } else {
    doc_meta = data.table(docs)
    colnames(doc_meta) = doc_col
  }

  if(is.null(feature_cols)) feature_cols = colnames(tokens)[!colnames(tokens) %in% c(doc_col, sent_i_col, word_i_col)]
  for(fcol in feature_cols) {
    if(class(tokens[,fcol]) %in% c('factor','character')) tokens[,fcol] = as.factor(as.character(tokens[,fcol]))
  }

  tCorpus(data = data.table(cbind(positions, tokens[,feature_cols,drop=F]), key = 'doc_id'),
          doc_meta = doc_meta)
}


#### FUNCTIONS FOR CREATING AND CHANGING THE TCORPUS ####

## provenance management ##

set_provenance <- function(tc, ...){
  p = list(...)
  for(key in names(p)) tc@p[[key]] = p[[key]]
  tc
}
get_provenance <- function(tc, keys=NULL) {
  if(is.null(keys)) tc@p else tc@p[keys]
}
is_provenance <- function(tc, ...){
  p = list(...)
  for(key in names(p)) {
    p[[key]] = if(!is.na(tc@p[[key]]) & tc@p[[key]] == p[[key]]) T else F
  }
  unlist(p)
}


## setting positions ##
local_position <- function(position, context, presorted=F){
  if(!presorted){
    ord = order(context, position)
    position = position[ord]
    context = context[ord]
  }
  newcontext = which(!duplicated(context))
  repeat_multiplier = c(newcontext[-1], length(context)+1) - newcontext
  context_start = rep(position[newcontext], repeat_multiplier)
  position = (position - context_start) + 1
  if(!presorted) position = position[match(1:length(position), ord)]
  position
}

global_position <- function(position, context, max_window_size=NA, presorted=F){
  ## makes the word position counter global with dummy positions between contexts to prevent overlapping windows (so it can be used as an index).
  ## this way, overlapping word windows can be calculated for multiple documents within a single matrix.
  ## position and context need to be sorted on order(context,position). If this is already the case, presorted=T can be used for a speed up
  if(!presorted){
    ord = order(context, position)
    position = position[ord]
    context = context[ord]
  }

  ## first, make sure position is local and starts at 1 for each context (otherwise things get very slow)
  position = local_position(position, context, presorted=T)

  if(min(position) == 0) position = position + 1 ## position will be treated as an index, so it cannot be zero in r where an index starts at 1 (and some parsers start indexing at zero)

  if(!length(unique(context)) == 1) {
    newcontext = which(!duplicated(context)) # where does a new context start

    context.max = position[newcontext-1] # the highest value of each context
    if(!is.na(max_window_size)) context.max = context.max + max_window_size # increase the highest value of each context with max_window_size to make sure windows of different contexts do not overlap.
    multiplier_scores = cumsum(c(0,context.max)) # the amount that should be added to the position at the start of each context

    repeat_multiplier = c(newcontext[-1], length(position)+1) - newcontext # the number of times the multiplier scores need to be repeated to match the position vector
    multiplier_vector = rep(multiplier_scores, repeat_multiplier)
    position = position + multiplier_vector
  }
  if(!presorted) position = position[match(1:length(position), ord)]
  position
}

## preprocessing ##

tokenize_to_datatable <- function(x, doc_id=1:length(x), split_sentences=F, max_sentences=NULL, max_words=NULL, verbose=F){
  x = gsub('_', ' ', x, fixed=T)
  if(split_sentences | !is.null(max_sentences)) {
    x = tokenize(x, what = 'sentence', verbose=verbose)
    names(x) = doc_id
    if(!is.null(max_sentences)) x = sapply(x, head, max_sentences)
    x = ldply(x, function(x) unlist_to_df(tokenize(x, what='word'), global_position=T))
    if(!is.null(max_words)) x = x[x$position <= max_words,]
    colnames(x) = c('doc_id','sent_i','word_i','word')
  } else {
    x = tokenize(x, what = 'word', verbose=verbose)
    if(!is.null(max_words)) x = sapply(x, head, max_words)
    x = unlist_to_df(x, doc_id)
    colnames(x) = c('doc_id', 'word_i', 'word')
  }
  x$word = as.factor(x$word)
  x$doc_id = as.factor(x$doc_id)
  data.table(x, key='doc_id')
}

unlist_to_df <- function(l, ids=1:length(l), global_position=F){
  len = sapply(l, length)
  filter = len > 0
  if(global_position){
    position = 1:sum(len)
  } else {
    position = unlist(sapply(len[filter], function(l) 1:l, simplify = F))
  }
  data.frame(id = rep(ids[filter], len[filter]),
             position = position,
             value = unlist(l[filter]))
}
#' @export
featurenames <- function(tc) colnames(tc@data)[!colnames(tc@data) %in% c('doc_id','sent_i','word_i')]

preprocess_feature <- function(tc, input_col, output_col=input_col, language='english', use_stemming=F, lowercase=T, ngrams=1, ngram_context=c('document', 'sentence')){
  feature = as.factor(tc@data[[input_col]])
  if(lowercase) levels(feature) = tolower(levels(feature))
  if(use_stemming) levels(feature) = quanteda::wordstem(levels(feature), language=language)
  if(ngrams > 1) {
    ngram_context = match.arg(ngram_context)
    group_col = c('doc_id','sent_i')[ngram_context == c('document','sentence')]
    words = grouped_ngrams(feature, tc@data[[group_col]], ngrams)
  }
  tc@data[[output_col]] = feature
  tc
}

grouped_ngrams <- function(words, group, n, filter=rep(T, length(words))){
  words = words[filter]
  group = group[filter]

  ngram_mat = matrix(ncol=n, nrow=length(words))
  for(i in 1:n) ngram_mat[,n-i+1] = c(rep('', i-1), as.character(words[1:(length(words)-i+1)]))

  newart = which(!duplicated(group))
  for(i in 1:(n-1)) ngram_mat[newart+i-1, 1:(n-i)] = '#'

  ngrams = vector('character', length(filter))
  ngrams[which(filter)] = apply(ngram_mat, 1, paste, collapse='_')
  as.factor(ngrams)
}

## filtering

#' Subset a tCorpus
#'
#' @param tc tCorpus object
#' @param subset
#'
#' @return
#' @export
#'
#' @examples
subset.tCorpus <- function(tc, subset=NULL, subset_meta=NULL) {
  if(!is.null(subset)){
    e = substitute(subset)
    r = eval(e, tc@data, parent.frame())
    tc@data = x@data[r,]
    tc@doc_meta[unique(tc@data$doc_id)]
  }
  if(!is.null(subset_meta)){
    e = substitute(subset_meta)
    r = eval(e, tc@doc_meta, parent.frame())
    tc@doc_meta = tc@doc_meta[r,]
    tc@data[unique(tc@doc_meta$doc_id)]
  }

  tc@data = droplevels(tc@data)
  tc@doc_meta = droplevels(tc@doc_meta)
  if(!key(tc@data) == 'doc_id') setkey(tc@data, 'doc_id')
  if(!key(tc@doc_meta) == 'doc_id')setkey(tc@doc_meta, 'doc_id')

  if(get_provenance(tc)$feature_index){
    cat('\tResetting feature index\n')
    tc = reset_feature_index(tc)
  }
  tc
}

freq <- function(x) {
  d = as.data.frame(table(x))
  d$Freq[match(x, d$x)]
}

function(){
  tc = filter_tcorpus(tc, feature='word', max_word_i = 100)
  tc = set_feature_index(tc, 'word')
  tc = filter_tcorpus(tc, feature='word', max_word_i = 100, keep_filtered_data = F)

  tc@data
  mean(test)
}


### FEATURE INDEX ###

#' Compute global feature positions
#'
#' Features are given global ids, with an added distance (max_window_size) between contexts (e.g., documents, sentences).
#' This way, the distance of features can be calculated across multiple contexts using a single vector
#'
#' @param tc tCorpus object
#' @param context_level The context within which
#' @param max_window_size
#'
#' @return a tCorpus object
#' @export
get_global_i <- function(tc, context_level=c('document','sentence'), max_window_size=200){
  context_level = match.arg(context_level)
  if(context_level == 'document'){
    global_i = global_position(position = tc@data[['word_i']], context = tc@data[['doc_id']], max_window_size = max_window_size, presorted=T)
  }
  if(context_level == 'sentence'){
    if(!'sent_i' %in% colnames(tc@data)) stop('Sentence level not possible, since no sentence information is available. To enable sentence level analysis, use split_sentences = T in "create_tcorpus()" or specify sent_i_col in "tokens_to_tcorpus()"')
    globsent = global_position(position = tc@data[['sent_i']], context = tc@data[['doc_id']], presorted=T)
    global_i = global_position(position = tc@data[['word_i']], context = globsent, max_window_size = max_window_size, presorted=T)
  }
  global_i
}

#' @export
get_feature_i <- function(tc, feature) as.numeric(tc@data[[feature]])

#' @export
get_feature_label <- function(tc, feature) levels(tc@data[[feature]])

#' Get a data.table for quick lookup of features
#'
#' #' The feature index has 3 columns:
#' \itemize{
#'    \item{feature is a factor of a given feature column, and is set as the key of the data.table}
#'    \item{i is the index of the rows in tc@data.}
#'    \item{global_i is an index for features with gaps between contexts (e.g., documents, sentences) to enable word disstance computations using a single vector.}
#' }
#'
#' Automatically filters out features based on the filter columns in tc@data
#'
#' @param tc
#' @param feature
#' @param context_level
#' @param max_window_size
#'
#' @export
get_feature_index <- function(tc, feature='word', context_level='document', max_window_size=100){
  prov = get_provenance(tc)
  if(!prov[['feature_index']]){
    cat('Creating feature index (can be precomputed with set_feature_index())')
    timer = Sys.time()
    fi = create_feature_index(tc, feature=feature, context_level=context_level, max_window_size=max_window_size)
    cat(sprintf('  (%s seconds)\n', round(as.numeric(difftime(Sys.time(), timer, units = 's')),2)))
  } else {
    ## if a feature index exists, check whether it matches the current parameters
    cfeature = prov[['feature']] == feature
    clevel = prov[['context_level']] == context_level
    cgap = prov[['max_window_size']] >= max_window_size
    if(!cfeature | !clevel | !cgap){
      cat('Existing feature index does not match current paramters. Creating ad-hoc feature index')
      timer = Sys.time()
      fi = create_feature_index(tc, feature=feature, context_level=context_level, max_window_size=max_window_size)
      cat(sprintf('  (%s seconds)\n', round(as.numeric(difftime(Sys.time(), timer, units = 's')),2)))
    } else {
      fi = tc@feature_index
    }
  }
  filter = get_filter(tc)
  if(!is.null(filter)) return(droplevels(fi[filter,])) else return(fi)  # note to self: droplevels drops all unused factors in fi
}


get_filter <- function(tc){
  cn = colnames(tc@data)
  if(!'doc_filter' %in% cn & !'feature_filter' %in% cn) return(NULL)
  if( 'doc_filter' %in% cn &  'feature_filter' %in% cn) return(tc@data$doc_filter & tc@data$feature_filter)
  if( 'doc_filter' %in% cn & !'feature_filter' %in% cn) return(tc@data$doc_filter)
  if(!'doc_filter' %in% cn &  'feature_filter' %in% cn) return(tc@data$feature_filter)
}

create_feature_index <- function(tc, feature, context_level=c('document','sentence'), max_window_size=100){
  context_level = match.arg(context_level)
  feature_index = data.table(feature = tc@data[[feature]])
  if(!is(feature_index$feature, 'factor')) feature_index$feature = as.factor(feature_index$feature)
  feature_index$i = 1:nrow(feature_index)
  feature_index$global_i = get_global_i(tc, context_level, max_window_size)
  setkey(feature_index, 'feature')
  feature_index
}

#' Create a feature index within a tCorpus
#'
#' @param tc
#' @param feature
#' @param context_level
#' @param max_window_size
#'
#' @export
set_feature_index <- function(tc, feature, context_level=c('document','sentence'), max_window_size=100){
  context_level = match.arg(context_level)
  tc@feature_index = create_feature_index(tc, feature, context_level, max_window_size)
  tc = set_provenance(tc, feature_index=T, feature=feature, context_level=context_level, max_window_size=max_window_size)
  tc
}

#' Reset feature index
#'
#' Reset the feature_index, using the . This is for instance necessary after filtering the data without keeping the data
#'
#' @param tc
#'
#' @export
reset_feature_index <- function(tc){
  p = get_provenance(tc)
  set_feature_index(tc, feature=p$feature, context_level=p$context_level, max_window_size=p$max_window_size)
}

#' delete feature index
#'
#' @param tc
#'
#' @export
delete_feature_index <- function(tc){
  tc@feature_index = data.table()
  tc = set_provenance(feature_index=F, feature=NA, context_level=NA, max_window_size=NA)
  tc
}

