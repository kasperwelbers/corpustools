
#' Create a tcorpus based on tokens (i.e. preprocessed texts)
#'
#' @param tokens A data.frame in which rows represent tokens, and columns indicate (at least) the document in which the token occured (doc_col) and the position of the token in that document or globally (token_id_col)
#' @param doc_col The name of the column that contains the document ids/names
#' @param token_id_col The name of the column that contains the positions of tokens. If NULL, it is assumed that the data.frame is ordered by the order of tokens and does not contain gaps (e.g., filtered out tokens)
#' @param token_col Optionally, the name of the column that contains the token text. This column will then be renamed to "token" in the tcorpus, which is the default name
#'                  for many functions (e.g., querying, printing text)
#' @param sentence_col Optionally, the name of the column that indicates the sentences in which tokens occured. This can be necessary if tokens are not local at the document level (see token_is_local argument),
#'                     and sentence information can be used in several tcorpus functions.
#' @param parent_col Optionally, the name of the column that contains the id of the parent (if a dependency parser was used). If token_is_local = FALSE, then the token_ids will be transormed,
#'                   so parent ids need to be changed as well. Default is 'parent', but if this column is not present the parent is ignored.
#' @param meta Optionally, a data.frame with document meta data. Needs to contain a column with the document ids (with the same name)
#' @param meta_cols Alternatively, if there are document meta columns in the tokens data.table, meta_cols can be used to recognized them. Note that these values have to be unique within documents.
#' @param feature_cols Optionally, specify which columns to include in the tcorpus. If NULL, all column are included (except the specified columns for documents, sentences and positions)
#' @param sent_is_local Sentences in the tCorpus are assumed to be locally unique within documents. If sent_is_local is FALSE, then sentences are transformed to be locally unique. However,  it is then assumed that the first sentence in a document is sentence 1, which might not be the case if tokens (input) is a subset.
#' @param token_is_local Same as sent_is_local, but for token_id. !! if the data has a parent column, make sure to specify parent_col, so that the parent ids are also transformed
#' @param ... not used
#'
#' @examples
#' head(corenlp_tokens)
#'
#' tc = tokens_to_tcorpus(corenlp_tokens, doc_col = 'doc_id',
#'                        sentence_col = 'sentence', token_id_col = 'id')
#' tc
#'
#' meta = data.frame(doc_id = 1, medium = 'A', date = '2010-01-01')
#' tc = tokens_to_tcorpus(corenlp_tokens, doc_col = 'doc_id',
#'                        sentence_col = 'sentence', token_id_col = 'id', meta=meta)
#' tc
#' @export
tokens_to_tcorpus <- function(tokens, doc_col='doc_id', token_id_col='token_id', token_col=NULL, sentence_col=NULL, parent_col=NULL, meta=NULL, meta_cols=NULL, feature_cols=NULL, sent_is_local=T, token_is_local=T, ...) {
  tokens = data.table::as.data.table(tokens)
  sentence = token_id = NULL ## used in data.table syntax, but need to have bindings for R CMD check

  ## check whether the columns specified in the arguments exist
  for(cname in c(doc_col, token_id_col, meta_cols)){
    if (!cname %in% colnames(tokens)) stop(sprintf('"%s" is not an existing columnname in "tokens"', cname))
  }

  if (!is.null(meta)){
    if (!doc_col %in% colnames(meta)) stop(sprintf('"meta" does not contain the document id column (%s)', doc_col))
  }

  ## change column names, make doc_id factor and check whether the right types are used
  data.table::setnames(tokens, which(colnames(tokens) == doc_col), 'doc_id')
  tokens[,'doc_id' := fast_factor(tokens$doc_id)]
  if (!is.null(sentence_col)) {
    data.table::setnames(tokens, which(colnames(tokens) == sentence_col), 'sentence')
    if (!methods::is(tokens$sentence, 'numeric')) stop('sentence_col has to be numeric/integer')
    if (!methods::is(tokens$sentence, 'integer')) tokens[,sentence := as.numeric(sentence)]
  }

  if (!is.null(token_id_col)) {
    data.table::setnames(tokens, which(colnames(tokens) == token_id_col), 'token_id')
    if (!methods::is(tokens$token_id, 'numeric')) stop('token_id_col has to be numeric/integer')
    if (!methods::is(tokens$token_id, 'integer')) tokens[,token_id := as.numeric(token_id)]
  } else {
    warning('No token_id column specified. token order used instead (see documentation).')
    tokens$token_id = 1:nrow(tokens)
    token_is_local = F
  }

  ## delete unused columns
  if (is.null(feature_cols)) feature_cols = colnames(tokens)[!colnames(tokens) %in% c(doc_col, sentence_col, token_id_col, meta_cols)]
  used_columns = c('doc_id','sentence','token_id', meta_cols, feature_cols)
  unused_columns = setdiff(colnames(tokens), used_columns)
  if(length(unused_columns) > 0) tokens[, (unused_columns) := NULL]

  for(fcol in feature_cols) {
    if (class(tokens[[fcol]]) %in% c('factor','character')) tokens[,(fcol) := fast_factor(tokens[[fcol]])]
  }

  ## set data.table keys (sorting the data) and verify that there are no duplicates
  if (!is.null(sentence_col)) {
    data.table::setkeyv(tokens, c('doc_id','sentence','token_id'))
    if (!anyDuplicated(tokens, by=c('doc_id','sentence','token_id')) == 0) stop('tokens should not contain duplicate triples of documents (doc_col), sentences (sentence_col) and token positions (token_id_col)')
  } else {
    data.table::setkeyv(tokens, c('doc_id','token_id'))
    if (!anyDuplicated(tokens, by=c('doc_id','token_id')) == 0) stop('tokens should not contain duplicate pairs of documents (doc_col) and token positions (token_id_col)')
  }

  tokens = ensure_local_tokens(tokens, sent_is_local, token_is_local, sentence_col, parent_col)

  ## arrange the meta data
  if (!is.null(meta)) {
    meta = data.table::as.data.table(meta)
    data.table::setnames(meta, which(colnames(meta) == doc_col), 'doc_id')
    meta[,'doc_id' := as.character(meta$doc_id)]
    data.table::setkeyv(meta, 'doc_id')

    if (!all(levels(tokens$doc_id) %in% meta$doc_id)) warning('For some documents in tokens the meta data is missing')
    if (!all(meta$doc_id %in% levels(tokens$doc_id))) warning('For some documents in the meta data there are no tokens. These documents will not be included in the meta data')
    meta = meta[list(levels(tokens$doc_id)),]
  } else {
    meta = data.table::data.table(doc_id=as.character(levels(tokens$doc_id)), key='doc_id')
  }

  if (!is.null(meta_cols)){
    add_meta = unique(tokens[,c('doc_id', meta_cols), with=F])
    if (nrow(add_meta) > nrow(meta)) stop('The document meta columns specified in meta_cols are not unique within documents')
    meta = cbind(meta, add_meta[,meta_cols,with=F])
  }
  meta$doc_id = as.character(meta$doc_id) ## prevent factors, which are unnecessary here and can only lead to conflicting levels with the doc_id in data

  tc = tCorpus$new(tokens=tokens, meta = meta)
  if (!is.null(token_col)) tc$set_name(token_col, 'token')
  if (!is.null(parent_col)) tc$set_name(parent_col, 'parent')
  tc
}




ensure_local_tokens <- function(tokens, sent_is_local, token_is_local, sentence_col, parent_col) {
  if (!token_is_local) {
    if (is.null(parent_col)) {
      message('token_id will be transformed, but note that no parent_col is specified. If there is a parent column make sure to ')
    } else {
      .PARENT_INDEX = tokens[list(tokens$doc_id, tokens$sentence, tokens[[parent_col]]),, on=c('doc_id','sentence','token_id'), which=T]
    }
  }

  ## make sure that sentence and token_id are locally unique within documents
  ndoc = nrow(unique(tokens, by='doc_id'))
  if (!is.null(sentence_col)){
    if (!sent_is_local) tokens[,'sentence' := local_position(tokens$sentence, tokens$doc_id, presorted = T)] ## make sure sentences are locally unique within documents (and not globally)
    if (!token_is_local) {
      tokens[,'token_id' := global_position(tokens$token_id,
                                            global_position(tokens$sentence, tokens$doc_id, presorted = T, position_is_local=T),
                                            presorted = T)]  ## make token positions globally unique, taking sentence id into account (in case tokens are locally unique within sentences)
      if (!is.null(parent_col)) tokens[, (parent_col) := tokens$token_id[.PARENT_INDEX]]
    }

  }

  if (token_is_local) {
    if (!anyDuplicated(tokens, by=c('doc_id','token_id')) == 0) warning("Duplicate token ids (doc_id - token_id pairs) found. If token ids are not local at the document level, you can set token_is_local to False to use a token's position within a document as the token ids")
    #if (ndoc > 10) if (!anyDuplicated(tokens, by=c('doc_id','token_id')) == 0) warning("token positions (token_id) do not appear to be locally unique within documents (no duplicates in at least 10 documents). Unless you are sure they are, set token_is_local to FALSE (and read documentation)")
  } else {
    tokens[,'token_id' := local_position(tokens$token_id, tokens$doc_id, presorted=T)] ## make tokens locally unique within documents
    if (!is.null(parent_col)) tokens[, (parent_col) := tokens$token_id[.PARENT_INDEX]]
  }

  tokens
}
