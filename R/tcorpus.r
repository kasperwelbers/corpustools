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

setMethod("show", "tCorpus",
    function(object) {
      data = get_data(object)
      meta = get_meta(object)
      fnames = featurenames(object)
      mnames = metanames(object)

      sent_info = if('sent_i' %in% colnames(data)) paste(' and sentences (n = ', nrow(unique(get_data(object, columns = c('doc_id','sent_i')))), ')', sep='') else ''
      cat('tCorpus containing ', nrow(data), ' tokens',
          '\nsplit by documents (n = ', nrow(meta), ')', sent_info,
          '\ncontaining:',
          '\n  - ', length(fnames), ' feature', if(length(fnames) > 1) '(s)', ': ', paste(fnames, collapse=','),
          '\n  - ', length(mnames), ' meta column', if(length(mnames) > 1) '(s)', ': ', paste(mnames, collapse=','), sep='')
    }
)

setMethod("summary", "tCorpus",
          function(object) {
            data = get_data(object)
            meta = get_meta(object)
            fnames = featurenames(object)

            sent_info = if('sent_i' %in% colnames(data)) paste(' and sentences (n = ', nrow(unique(get_data(object, columns = c('doc_id','sent_i')))), ')', sep='') else ''
            cat('tCorpus containing ', nrow(data), ' tokens',
                '\nsplit by documents (n = ', nrow(meta), ')', sent_info, sep='')
            cat('\n\n@data\n')
            print(object@data)

            cat('\n@doc_meta\n')
            print(object@doc_meta)
          }
)

setMethod("print", "tCorpus",
    function(x) {
      print(x@data)
      print(x@doc_meta)
    }
)

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
      if(!'doc_id' %in% colnames(doc_meta)) doc_meta = cbind(doc_id=doc_id, doc_meta)
      doc_meta = data.table(doc_meta, key = 'doc_id')
  } else {
    if(!length(doc_id) == length(x)) stop('"doc_id" is not of the same length as "x"')
    doc_meta = data.table(doc_id=doc_id, key = 'doc_id')
  }
  tc = tCorpus(data = tokenize_to_datatable(x, doc_id=doc_id, split_sentences=split_sentences, max_sentences=max_sentences, max_words=max_words, verbose=verbose),
               doc_meta = droplevels(doc_meta))
  set_keys(tc)
  tc
}

#' @rdname create_tcorpus
#' @export
create_tcorpus.factor <- function(x, doc_id=1:length(x), doc_meta=NULL, split_sentences=F, max_sentences=NULL, max_words=NULL, verbose=F) {
  create_tcorpus(as.character(x), doc_id=doc_id, doc_meta=doc_meta, split_sentences=split_sentences, max_sentences=max_sentences, max_words=max_words, verbose=verbose)
}


#' @rdname create_tcorpus
#' @export
create_tcorpus.data.frame <- function(d, text_columns='text', doc_column=NULL, split_sentences=F, max_sentences=NULL, max_words=NULL) {
  for(cname in text_columns) if(!cname %in% colnames(d)) stop(sprintf('text_column "%s" not in data.frame', cname))

  if(length(text_columns) > 1){
    text = apply(d[,text_columns], 1, paste, collapse = '\n\n')
  } else {
    text = d[[text_columns]]
  }

  doc_id = if(is.null(doc_column)) 1:nrow(d) else d[[doc_column]]

  create_tcorpus(text,
                 doc_id = doc_id,
                 doc_meta = d[,!colnames(d) %in% c(text_columns, doc_column), drop=F],
                 split_sentences = split_sentences, max_sentences = max_sentences, max_words = max_words)
}

#' Create a tcorpus based on tokens (i.e. preprocessed texts)
#'
#' @param tokens A data.frame in which rows represent tokens, and columns indicate (at least) the document in which the token occured (doc_col) and the position of the token in that document or globally (word_i_col)
#' @param doc_col The name of the column that contains the document ids/names
#' @param word_i_col The name of the column that contains the positions of words. If NULL, it is assumed that the data.frame is ordered by the order of words and does not contain gaps (e.g., filtered out words)
#' @param sent_i_col Optionally, the name of the column that indicates the sentences in which tokens occured.
#' @param doc_meta Optionally, a data.frame with document meta data. Needs to contain a column with the document ids (with the same name)
#' @param doc_meta_cols Alternatively, if there are document meta columns in the tokens data.table, doc_meta_cols can be used to recognized them. Note that these values have to be unique within documents.
#' @param feature_cols Optionally, specify which columns to include in the tcorpus. If NULL, all column are included (except the specified columns for documents, sentences and positions)
#'
#' @export
tokens_to_tcorpus <- function(tokens, doc_col='doc_id', word_i_col=NULL, sent_i_col=NULL, doc_meta=NULL, doc_meta_cols=NULL, feature_cols=NULL) {
  tokens = as.data.frame(tokens)
  for(cname in c(doc_col, word_i_col, sent_i_col, doc_meta_cols)){
    if(!cname %in% colnames(tokens)) stop(sprintf('"%s" is not an existing columnname in "tokens"', cname))
  }
  tokens = droplevels(tokens)

  if(is.null(word_i_col)){
    warning('No word_i column specified. It is now assumed that "tokens" is ordered according to the occurence of words in documents. If this is not the case, results of functions where word order is relevant will be wrong!')
    tokens$word_i = 1:nrow(tokens)
    word_i_col = 'word_i'
  }

  if(!is(tokens[,word_i_col], 'numeric')) stop('word_i_col has to be numeric/integer')
  if(!is.null(sent_i_col)) if(!is(tokens[,sent_i_col], 'numeric')) stop('sent_i_col has to be numeric/integer')

  if(!is.null(doc_meta)){
    if(!doc_col %in% colnames(doc_meta)) stop(sprintf('"doc_meta" does not contain the document id column (%s)', doc_col))
  }

  ## order and index doc_id, sent_i, word_i
  if(!is.null(sent_i_col)){
    tokens = tokens[order(tokens[,doc_col], tokens[,sent_i_col], tokens[,word_i_col]),]
  } else {
    tokens = tokens[order(tokens[,doc_col], tokens[,word_i_col]),]
  }

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
  docs = unique(positions$doc_id)
  if(!is.null(doc_meta)) {
    colnames(doc_meta)[colnames(doc_meta) == doc_col] = 'doc_id'
    doc_meta$doc_id = as.factor(doc_meta$doc_id)
    if(!all(docs %in% doc_meta$doc_id)) warning('For some documents in tokens the meta data is missing')
    if(!all(doc_meta$doc_id %in% docs)) warning('For some documents in the meta data there are no tokens. These documents will not be included in the meta data')

    doc_meta = data.table(doc_meta[match(docs, doc_meta$doc_id),], key='doc_id')
  } else {
    doc_meta = data.table(doc_id=as.factor(docs), key='doc_id')
  }

  if(!is.null(doc_meta_cols)){
    add_doc_meta = unique(tokens[,c(doc_col, doc_meta_cols)])
    if(nrow(add_doc_meta) > nrow(doc_meta)) stop('The document meta columns specified in doc_meta_cols are not unique within documents')
    doc_meta = cbind(doc_meta, add_doc_meta[,doc_meta_cols])
  }

  if(is.null(feature_cols)) feature_cols = colnames(tokens)[!colnames(tokens) %in% c(doc_col, sent_i_col, word_i_col, doc_meta_cols)]
  for(fcol in feature_cols) {
    if(class(tokens[,fcol]) %in% c('factor','character')) tokens[,fcol] = as.factor(as.character(tokens[,fcol]))
  }

  tc = tCorpus(data=data.table(cbind(positions, tokens[,feature_cols,drop=F])),
               doc_meta = data.table(doc_meta))
  set_keys(tc)
  tc
}

tokenize_to_datatable <- function(x, doc_id=1:length(x), split_sentences=F, max_sentences=NULL, max_words=NULL, verbose=F){
  x = gsub('_', ' ', x, fixed=T)
  if(split_sentences | !is.null(max_sentences)) {
    x = quanteda::tokenize(x, what = 'sentence', verbose=verbose)
    names(x) = doc_id
    if(!is.null(max_sentences)) x = sapply(x, head, max_sentences)
    x = plyr::ldply(x, function(x) unlist_to_df(quanteda::tokenize(x, what='word'), global_position=T))
    if(!is.null(max_words)) x = x[x$position <= max_words,]
    colnames(x) = c('doc_id','sent_i','word_i','word')
  } else {
    x = quanteda::tokenize(x, what = 'word', verbose=verbose)
    if(!is.null(max_words)) x = sapply(x, head, max_words)
    x = unlist_to_df(x, doc_id)
    colnames(x) = c('doc_id', 'word_i', 'word')
  }
  x$word = as.factor(x$word)
  x$doc_id = as.factor(x$doc_id)
  data.table(x)
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

#### FUNCTIONS FOR CREATING AND CHANGING THE TCORPUS ####

## manage data

set_keys <- function(tc){
  if('sent_i' %in% colnames(tc@data)){
    if(!identical(key(tc@data), c('doc_id', 'sent_i'))) setkey(tc@data, 'doc_id', 'sent_i')
  } else {
    if(!identical(key(tc@data), c('doc_id'))) setkey(tc@data, 'doc_id')
  }
  if(!identical(key(tc@doc_meta), c('doc_id'))) setkey(tc@doc_meta, 'doc_id')
}

#' @export
get_data <- function(tc, columns=NULL, as_data_frame=F) {
  set_keys(tc)
  data = if(!is.null(columns)) tc@data[,columns, with=F] else tc@data
  if(as_data_frame) data = as.data.frame(data)
  data
}

#' @export
get_column <- function(tc, name) get_data(tc)[[name]]

#' @export
set_column <- function(tc, name, value) {
  if(name %in% c('doc_id', 'sent_i', 'word_i')) stop(sprintf('Cannot manually change %s', name))
  tc@data[[name]] = droplevels(value)
  tc
}

#' Recode a (feature) column in a tCorpus
#'
#' @param tc
#' @param column the name of the (feature) column
#' @param new_value the new value
#' @param i an index for which values to replace
#' @param old_value a vector containing one or more old values to replace
#'
#' @return
#' @export
#'
#' @examples
recode_column <- function(tc, column, new_value, i=NULL, old_value=NULL){
  if(!new_value %in% levels(tc@data[[column]])) levels(tc@data[[column]]) = c(levels(tc@data[[column]]), new_value)
  if(!is.null(i)) tc@data[[column]][i] = new_value
  if(!is.null(old_value)) tc@data[[column]][tc@data[[column]] %in% old_value] = new_value
  tc@data[[column]] = droplevels(tc@data[[column]])
  tc
}

#' @export
featurenames <- function(tc) colnames(get_data(tc))[!colnames(get_data(tc)) %in% c('doc_id','sent_i','word_i')]

#' @export
get_context <- function(tc, context_level = c('document','sentence')){
  context_level = match.arg(context_level)
  if(context_level == 'document') context = get_column(tc, 'doc_id')
  if(context_level == 'sentence') {
    if(is.null(get_column(tc, 'sent_i'))) stop('Sentence level not possible, since no sentence information is available. To enable sentence level analysis, use split_sentences = T in "create_tcorpus()" or specify sent_i_col in "tokens_to_tcorpus()"')
    context = factor(global_position(get_column(tc, 'sent_i'), get_column(tc, 'doc_id'), presorted = T))
  }
  context
}

#get_feature_i <- function(tc, feature) {
#  x = get_column(tc, feature)
#  if(!is(feature, 'factor')) set_column(tc, feature, as.factor(x))
#  as.numeric(x)
#}

#get_feature_levels <- function(tc, feature) {
#  x = get_column(tc, feature)
#  if(!is(feature, 'factor')) set_column(tc, feature, as.factor(x))
#  levels(x)
#}

## manage meta

#' @export
get_meta <- function(tc, columns=NULL, as_data_frame=F, per_token=F) {
  set_keys(tc)
  if(!is.null(columns)) meta = tc@doc_meta[,columns, with=F] else meta = tc@doc_meta
  if(as_data_frame) meta = as.data.frame(meta)
  if(per_token) meta = meta[match(get_column(tc, 'doc_id'), get_meta_column(tc, 'doc_id')), ,drop=F]
  meta
}

#' @export
get_meta_column <- function(tc, name, per_token=F) {
  col = get_meta(tc)[[name]]
  if(per_token) col = col[match(get_column(tc, 'doc_id'), get_meta_column(tc, 'doc_id'))]
  col
}

#' @export
set_meta_column <- function(tc, name, value) {
  if(name %in% c('doc_id')) stop(sprintf('Cannot manually change %s', name))
  tc@doc_meta[[name]] = value
  tc
}

#' Recode a document meta column in a tCorpus
#'
#' @param tc
#' @param column the name of the document meta column
#' @param new_value the new value
#' @param i an index for which values to replace
#' @param old_value a vector containing one or more old values to replace
#'
#' @return
#' @export
#'
#' @examples
recode_meta_column <- function(tc, column, new_value, i=NULL, old_value=NULL){
  if(!new_value %in% levels(tc@doc_meta[[column]])) levels(tc@doc_meta[[column]]) = c(levels(tc@doc_meta[[column]]), new_value)
  if(!is.null(i)) tc@doc_meta[[column]][i] = new_value
  if(!is.null(old_value)) tc@doc_meta[[column]][tc@doc_meta[[column]] %in% old_value] = new_value
  tc@data[[column]] = droplevels(tc@doc_meta[[column]])
  tc
}


#' @export
metanames <- function(tc) colnames(get_meta(tc))


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
  repeat_add = c(newcontext[-1], length(context)+1) - newcontext
  context_start = rep(position[newcontext], repeat_add)
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

  ## first, make sure position is local and starts at 1 for each context (otherwise the global id can become absurdly high)
  position = local_position(position, context, presorted=T)

  if(min(position) == 0) position = position + 1 ## position will be treated as an index, so it cannot be zero in r where an index starts at 1 (and some parsers start indexing at zero)

  if(!length(unique(context)) == 1) {
    newcontext = which(!duplicated(context)) # where does a new context start

    context.max = position[newcontext-1] # the highest value of each context
    if(!is.na(max_window_size)) context.max = context.max + max_window_size # increase the highest value of each context with max_window_size to make sure windows of different contexts do not overlap.
    add_scores = cumsum(c(0,context.max)) # the amount that should be added to the position at the start of each context

    repeat_add = c(newcontext[-1], length(position)+1) - newcontext # the number of times the add scores need to be repeated to match the position vector
    add_vector = rep(add_scores, repeat_add)
    position = position + add_vector
  }
  if(!presorted) position = position[match(1:length(position), ord)]
  position
}

