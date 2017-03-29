
#' Create a tCorpus
#'
#' @rdname create_tcorpus
#'
#' @param x
#' @param meta A data.frame with document meta information (e.g., date, source). The rows of the data.frame need to match the values of x
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
create_tcorpus.character <- function(x, doc_id=1:length(x), meta=NULL, split_sentences=F, max_sentences=NULL, max_words=NULL, verbose=F) {
  if (any(duplicated(doc_id))) stop('doc_id should not contain duplicate values')
  if (!is.null(meta)){
    if (!is(meta, 'data.frame')) stop('"meta" is not a data.frame or data.table')
    if (!nrow(meta) == length(x)) stop('The number of rows in "meta" does not match the number of texts in "x"')
    if (!'doc_id' %in% colnames(meta)) meta = cbind(doc_id=doc_id, meta)
    meta = data.table(meta, key = 'doc_id')
  } else {
    if (!length(doc_id) == length(x)) stop('"doc_id" is not of the same length as "x"')
    meta = data.table(doc_id=doc_id, key = 'doc_id')
  }
  meta$doc_id = as.character(meta$doc_id) ## prevent factors, which are unnecessary here and can only lead to conflicting levels with the doc_id in data

  tCorpus$new(data = data.table(tokenize_to_dataframe(x, doc_id=doc_id, split_sentences=split_sentences, max_sentences=max_sentences, max_words=max_words, verbose=verbose)),
              meta = base::droplevels(meta))
}

#' @rdname create_tcorpus
#' @export
create_tcorpus.factor <- function(x, doc_id=1:length(x), meta=NULL, split_sentences=F, max_sentences=NULL, max_words=NULL, verbose=F) {
  create_tcorpus(as.character(x), doc_id=doc_id, meta=meta, split_sentences=split_sentences, max_sentences=max_sentences, max_words=max_words, verbose=verbose)
}


#' @rdname create_tcorpus
#' @export
create_tcorpus.data.frame <- function(d, text_columns='text', doc_column=NULL, split_sentences=F, max_sentences=NULL, max_words=NULL) {
  for(cname in text_columns) if (!cname %in% colnames(d)) stop(sprintf('text_column "%s" not in data.frame', cname))

  if (length(text_columns) > 1){
    text = apply(d[,text_columns], 1, paste, collapse = '\n\n')
  } else {
    text = d[[text_columns]]
  }

  doc_id = if (is.null(doc_column)) 1:nrow(d) else d[[doc_column]]

  create_tcorpus(text,
                 doc_id = doc_id,
                 meta = d[,!colnames(d) %in% c(text_columns, doc_column), drop=F],
                 split_sentences = split_sentences, max_sentences = max_sentences, max_words = max_words)
}

#' Create a tcorpus based on tokens (i.e. preprocessed texts)
#'
#' @param tokens A data.frame in which rows represent tokens, and columns indicate (at least) the document in which the token occured (doc_col) and the position of the token in that document or globally (word_i_col)
#' @param doc_col The name of the column that contains the document ids/names
#' @param word_i_col The name of the column that contains the positions of words. If NULL, it is assumed that the data.frame is ordered by the order of words and does not contain gaps (e.g., filtered out words)
#' @param sent_i_col Optionally, the name of the column that indicates the sentences in which tokens occured.
#' @param meta Optionally, a data.frame with document meta data. Needs to contain a column with the document ids (with the same name)
#' @param meta_cols Alternatively, if there are document meta columns in the tokens data.table, meta_cols can be used to recognized them. Note that these values have to be unique within documents.
#' @param feature_cols Optionally, specify which columns to include in the tcorpus. If NULL, all column are included (except the specified columns for documents, sentences and positions)
#' @param sent_is_local Sentences in the tCorpus must be locally unique within documents. If sent_is_local is FALSE, then sentences are made sure to be locally unique. However,  it is then assumed that the first sentence in a document is sentence 1, which might not be the case if tokens (input) is a subset. If you know for a fact that the sentence column in tokens is already locally unique, you can set sent_is_local to TRUE to keep the original sent_i values.
#' @param word_is_local Same as sent_is_local, but or word_i
#'
#' @export
tokens_to_tcorpus <- function(tokens, doc_col='doc_id', word_i_col=NULL, sent_i_col=NULL, meta=NULL, meta_cols=NULL, feature_cols=NULL, sent_is_local=F, word_is_local=F) {
  tokens = as.data.frame(tokens)
  for(cname in c(doc_col, word_i_col, sent_i_col, meta_cols)){
    if (!cname %in% colnames(tokens)) stop(sprintf('"%s" is not an existing columnname in "tokens"', cname))
  }
  tokens = droplevels(tokens)

  if (is.null(word_i_col)){
    warning('No word_i column specified. Word order used instead (see documentation).')
    tokens$word_i = 1:nrow(tokens)
    word_is_local = F
    word_i_col = 'word_i'
  } else {
    if (is.null(sent_i_col)) {
      if (any(duplicated(tokens[,c(doc_col, word_i_col)]))) stop('tokens should not contain duplicate doubles of documents (doc_col) and word positions (word_i_col)')
    } else {
      if (any(duplicated(tokens[,c(doc_col, sent_i_col, word_i_col)]))) stop('tokens should not contain duplicate triples of documents (doc_col), sentences (sent_i_col) and word positions (word_i_col)')
    }
  }

  if (!is(tokens[,word_i_col], 'numeric')) stop('word_i_col has to be numeric/integer')
  if (!is.null(sent_i_col)) if (!is(tokens[,sent_i_col], 'numeric')) stop('sent_i_col has to be numeric/integer')

  if (!is.null(meta)){
    if (!doc_col %in% colnames(meta)) stop(sprintf('"meta" does not contain the document id column (%s)', doc_col))
  }

  ## order and index doc_id, sent_i, word_i
  if (!is.null(sent_i_col)){
    tokens = tokens[order(tokens[,doc_col], tokens[,sent_i_col], tokens[,word_i_col]),]
  } else {
    tokens = tokens[order(tokens[,doc_col], tokens[,word_i_col]),]
  }

  positions = data.frame(doc_id= fast_factor(tokens[,doc_col]),
                         sent_i = tokens[,sent_i_col],
                         word_i = tokens[,word_i_col])

  ## check whether words and sentences are (likely to be) local

  ## make sure that sent_i and word_i are locally unique within documents
  if (!is.null(sent_i_col)){
    if (sent_is_local) {
      udocsent = unique(positions[,c('doc_id','sent_i')])
      if (!any(duplicated(udocsent$sent_i))) warning("Sentence positions (sent_i) do not appear to be locally unique within document (no duplicates). Unless you are sure they are, set sent_is_local to FALSE (and read documentation)")
      rm(udocsent)
    }
    if (!sent_is_local) positions$sent_i = local_position(positions$sent_i, positions$doc_id, presorted = T) ## make sure sentences are locally unique within documents (and not globally)
    if (!word_is_local) positions$word_i = global_position(positions$word_i, positions$sent_i, presorted=T) ## make word positions globally unique, taking sentence id into account (in case words are locally unique within sentences)
  }
  if (word_is_local) {
    udocword = unique(positions[,c('doc_id','word_i')])
    if (!any(duplicated(udocword$word_i))) warning("Word positions (word_i) do not appear to be locally unique within document (no duplicates). Unless you are sure they are, set word_is_local to FALSE (and read documentation)")
    rm(udocword)
  }
  if (!word_is_local) positions$word_i = local_position(positions$word_i, positions$doc_id, presorted=T) ## make words locally unique within documents

  ## match meta
  docs = unique(positions$doc_id)
  if (!is.null(meta)) {
    colnames(meta)[colnames(meta) == doc_col] = 'doc_id'
    meta$doc_id = fast_factor(meta$doc_id)
    if (!all(docs %in% meta$doc_id)) warning('For some documents in tokens the meta data is missing')
    if (!all(meta$doc_id %in% docs)) warning('For some documents in the meta data there are no tokens. These documents will not be included in the meta data')

    meta = data.table(meta[match(docs, meta$doc_id),], key='doc_id')
  } else {
    meta = data.table(doc_id=docs, key='doc_id')
  }

  if (!is.null(meta_cols)){
    add_meta = unique(tokens[,c(doc_col, meta_cols)])
    if (nrow(add_meta) > nrow(meta)) stop('The document meta columns specified in meta_cols are not unique within documents')
    meta = cbind(meta, add_meta[,meta_cols])
  }
  meta$doc_id = as.character(meta$doc_id) ## prevent factors, which are unnecessary here and can only lead to conflicting levels with the doc_id in data

  if (is.null(feature_cols)) feature_cols = colnames(tokens)[!colnames(tokens) %in% c(doc_col, sent_i_col, word_i_col, meta_cols)]
  for(fcol in feature_cols) {
    if (class(tokens[,fcol]) %in% c('factor','character')) tokens[,fcol] = fast_factor(as.character(tokens[,fcol]))
  }

  tCorpus$new(data=data.table(cbind(positions, tokens[,feature_cols,drop=F])),
              meta = data.table(meta))
}
