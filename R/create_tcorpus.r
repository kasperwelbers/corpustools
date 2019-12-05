#' Create a tCorpus
#'
#' Create a \link{tCorpus} from raw text input. Input can be a character (or factor) vector, data.frame or quanteda corpus.
#' If a data.frame is given, all columns other than the document id and text columns are included as meta data. If a quanteda
#' corpus is given, the ids and texts are already specified, and the docvars will be included in the tCorpus as meta data.
#'
#' By default, texts will only be tokenized, and basic preprocessing techniques (lowercasing, stemming) can be applied with the
#' \code{\link{preprocess}} method. Alternatively, the udpipe package can be used to apply more advanced NLP preprocessing, by
#' using the udpipe_model argument.
#'
#' @rdname create_tcorpus
#'
#' @param x main input. can be a character (or factor) vector where each value is a full text, or a data.frame that has a column that contains full texts.
#' @param meta A data.frame with document meta information (e.g., date, source). The rows of the data.frame need to match the values of x
#' @param udpipe_model Optionally, the name of a Universal Dependencies language model (e.g., "english-ewt", "dutch-alpino"), to use the udpipe package
#'                     (\code{\link[udpipe]{udpipe_annotate}}) for natural language processing. You can use \code{\link{show_udpipe_models}} to get
#'                     an overview of the available models. For more information about udpipe and performance benchmarks of the UD models, see the
#'                     GitHub page of the \href{https://github.com/bnosac/udpipe}{udpipe package}.
#' @param split_sentences Logical. If TRUE, the sentence number of tokens is also computed. (only if udpipe_model is not used)
#' @param max_tokens An integer. Limits the number of tokens per document to the specified number
#' @param max_sentences An integer. Limits the number of sentences per document to the specified number. If set when split_sentences == FALSE, split_sentences will be set to TRUE.
#' @param doc_id if x is a character/factor vector, doc_id can be used to specify document ids. This has to be a vector of the same length as x
#' @param doc_column If x is a data.frame, this specifies the column with the document ids.
#' @param text_columns if x is a data.frame, this specifies the column(s) that contains text. The texts are paste together in the order specified here.
#' @param udpipe_model_path If udpipe_model is used, this path wil be used to look for the model, and if the model doesn't yet exist it will be downloaded to this location. Defaults to working directory
#' @param udpipe_cache      The number of persistent caches to keep for inputs of udpipe. The caches store tokens per batch (100 documents).
#'                          This way, if a lot of data has to be parsed, or if R crashes, udpipe can continue from the latest batch instead of start over.
#'                          The caches are stored in the udpipe_models folder (in udpipe_model_path). Only the most recent [udpipe_caches] caches will be stored.
#' @param use_parser If TRUE, use dependency parser (only if udpipe_model is used)
#' @param remember_spaces If TRUE, a column with spaces after each token is included. Enables correct reconstruction of original text and keeps annotations at the level of character positions (e.g., brat) intact.
#' @param verbose If TRUE, report progress
#' @param ...          Arguments passed to create_tcorpus.character
#'
#' @export
#' @name create_tcorpus
#' @examples
#' ## ...
create_tcorpus <- function(x, ...) {
  UseMethod('create_tcorpus')
}

#' @rdname create_tcorpus
#' @examples
#' tc = create_tcorpus(c('Text one first sentence. Text one second sentence', 'Text two'))
#' tc$tokens
#'
#' tc = create_tcorpus(c('Text one first sentence. Text one second sentence', 'Text two'),
#'                     split_sentences = TRUE)
#' tc$tokens
#'
#' ## with meta (easier to S3 method for data.frame)
#' meta = data.frame(doc_id = c(1,2), source = c('a','b'))
#' tc = create_tcorpus(c('Text one first sentence. Text one second sentence', 'Text two'),
#'                     split_sentences = TRUE,
#'                     doc_id = c(1,2),
#'                     meta = meta)
#' tc
#' @export
create_tcorpus.character <- function(x, doc_id=1:length(x), meta=NULL, udpipe_model=NULL, split_sentences=F, max_sentences=NULL, max_tokens=NULL, udpipe_model_path=getwd(), udpipe_cache=3, use_parser=F, remember_spaces=FALSE, verbose=T, ...) {
  space = NULL; misc = NULL ## data.table bindings

  if (any(duplicated(doc_id))) stop('doc_id should not contain duplicate values')
  if (!is.null(meta)){
    if (!methods::is(meta, 'data.frame')) stop('"meta" is not a data.frame or data.table')
    if (!nrow(meta) == length(x)) stop('The number of rows in "meta" does not match the number of texts in "x"')
    if (!'doc_id' %in% colnames(meta)) meta = cbind(doc_id=doc_id, meta)
    meta = data.table::data.table(meta, key = 'doc_id')
  } else {
    if (!length(doc_id) == length(x)) stop('"doc_id" is not of the same length as "x"')
    meta = data.table::data.table(doc_id=doc_id, key = 'doc_id')
  }
  meta$doc_id = as.character(meta$doc_id) ## prevent factors, which are unnecessary here and can only lead to conflicting levels with the doc_id in data

  if (!is.null(udpipe_model)) {
    data = udpipe_parse(x, udpipe_model, udpipe_model_path, cache=udpipe_cache, doc_id=doc_id, use_parser=use_parser, max_sentences=max_sentences, max_tokens=max_tokens, verbose=verbose)
    if (remember_spaces) {
      levels(data$misc) = c(levels(data$misc), " ")
      data$misc[is.na(data$misc)] = " "
      data[, space := fast_factor(gsub('Space[s]?After=', '', misc))]
      levels(data$space) = ifelse(levels(data$space) == 'No', '', levels(data$space))
      levels(data$space) = double_to_single_slash(levels(data$space))
    }
    data$misc = NULL
  } else {
    data = tokenize_to_dataframe(x, doc_id=doc_id, split_sentences=split_sentences, max_sentences=max_sentences, max_tokens=max_tokens, remember_spaces=remember_spaces, verbose=verbose)
  }
  tCorpus$new(tokens = data, meta = base::droplevels(meta))
}

#' @rdname create_tcorpus
#' @examples
#' d = data.frame(text = c('Text one first sentence. Text one second sentence.',
#'                'Text two', 'Text three'),
#'                date = c('2010-01-01','2010-01-01','2012-01-01'),
#'                source = c('A','B','B'))
#'
#' tc = create_tcorpus(d, split_sentences = TRUE)
#' tc
#' tc$tokens
#'
#' ## use multiple text columns
#' d$headline = c('Head one', 'Head two', 'Head three')
#' ## use custom doc_id
#' d$doc_id = c('#1', '#2', '#3')
#'
#' tc = create_tcorpus(d, text_columns = c('headline','text'), doc_column = 'doc_id',
#'                     split_sentences = TRUE)
#' tc
#' tc$tokens
#' @export
create_tcorpus.data.frame <- function(x, text_columns='text', doc_column='doc_id', udpipe_model=NULL, split_sentences=F, max_sentences=NULL, max_tokens=NULL, udpipe_model_path=getwd(), udpipe_cache=3, use_parser=F, remember_spaces=FALSE, verbose=T, ...) {
  for(cname in text_columns) if (!cname %in% colnames(x)) stop(sprintf('text_column "%s" not in data.frame', cname))

  if (length(text_columns) > 1){
    text = do.call(paste, c(as.list(x[,text_columns]), sep='\n\n'))
  } else {
    text = x[[text_columns]]
  }

  if (!doc_column %in% colnames(x)) {
    message('No existing document column (doc_column) specified. Using indices as id.')
    doc_id = 1:nrow(x)
  } else doc_id = x[[doc_column]]

  create_tcorpus(text,
                 doc_id = doc_id,
                 meta = x[,!colnames(x) %in% c(text_columns, doc_column), drop=F],
                 udpipe_model=udpipe_model, split_sentences = split_sentences, max_sentences = max_sentences, max_tokens = max_tokens, udpipe_model_path=udpipe_model_path, udpipe_cache=udpipe_cache, use_parser=use_parser, remember_spaces=remember_spaces, verbose=verbose)
}

#' @rdname create_tcorpus
#' @examples
#' ## It makes little sense to have full texts as factors, but it tends to happen.
#' ## The create_tcorpus S3 method for factors is essentially identical to the
#' ##  method for a character vector.
#' text = factor(c('Text one first sentence', 'Text one second sentence'))
#' tc = create_tcorpus(text)
#' tc$tokens
#' @export
create_tcorpus.factor <- function(x, ...) {
  create_tcorpus(as.character(x), ...)
}

#' @rdname create_tcorpus
#' @examples
#'
#' \donttest{
#' library(quanteda)
#' create_tcorpus(data_corpu_inaugural)
#' }
#' @export
create_tcorpus.corpus <- function(x, ...) {
  x = x$documents
  x$doc_id = rownames(x)
  create_tcorpus(x, text_columns='texts', doc_column='doc_id', ...)
}

#' Create a tcorpus based on tokens (i.e. preprocessed texts)
#'
#' @param tokens A data.frame in which rows represent tokens, and columns indicate (at least) the document in which the token occured (doc_col) and the position of the token in that document or globally (token_id_col)
#' @param doc_col The name of the column that contains the document ids/names
#' @param token_id_col The name of the column that contains the positions of tokens. If NULL, it is assumed that the data.frame is ordered by the order of tokens and does not contain gaps (e.g., filtered out tokens)
#' @param sentence_col Optionally, the name of the column that indicates the sentences in which tokens occured.
#' @param token_col Optionally, the name of the column that contains the token text
#' @param lemma_col Optionally, the name of the column that contains the lemma of the token
#' @param pos_col Optionally, the name of the column that contains the part-of-speech tag of the token
#' @param relation_col Optionally, the name of the column that contains the relation of the token to its parent
#' @param parent_col Optionally, the name of the column that contains the id of the parent
#' @param meta Optionally, a data.frame with document meta data. Needs to contain a column with the document ids (with the same name)
#' @param meta_cols Alternatively, if there are document meta columns in the tokens data.table, meta_cols can be used to recognized them. Note that these values have to be unique within documents.
#' @param feature_cols Optionally, specify which columns to include in the tcorpus. If NULL, all column are included (except the specified columns for documents, sentences and positions)
#' @param sent_is_local Sentences in the tCorpus are assumed to be locally unique within documents. If sent_is_local is FALSE, then sentences are transformed to be locally unique. However,  it is then assumed that the first sentence in a document is sentence 1, which might not be the case if tokens (input) is a subset.
#' @param token_is_local Same as sent_is_local, but for token_id. Note that if a parent column is present, it will not be changed along.
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
tokens_to_tcorpus <- function(tokens, doc_col='doc_id', token_id_col='token_id', sentence_col='sentence', token_col=NULL, lemma_col=NULL, pos_col=NULL, relation_col=NULL, parent_col=NULL, meta=NULL, meta_cols=NULL, feature_cols=NULL, sent_is_local=T, token_is_local=T) {
  tokens = data.table::as.data.table(tokens)
  sentence = token_id = NULL ## used in data.table syntax, but need to have bindings for R CMD check

  ## check whether the columns specified in the arguments exist
  for(cname in c(doc_col, token_id_col, sentence_col, meta_cols)){
    if (!cname %in% colnames(tokens)) stop(sprintf('"%s" is not an existing columnname in "tokens"', cname))
  }
  if (!is.null(meta)){
    if (!doc_col %in% colnames(meta)) stop(sprintf('"meta" does not contain the document id column (%s)', doc_col))
  }

  ## change column names, make doc_id factor (both in reference, for efficiency) and check whether the right types are used
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

  ## make sure that sentence and token_id are locally unique within documents
  ndoc = nrow(unique(tokens, by='doc_id'))
  if (!is.null(sentence_col)){
    #if (sent_is_local) {
    #    #if (ndoc > 10) if (!anyDuplicated(unique(tokens, by=c('doc_id','sentence')), by='sentence') == 0) warning("Sentence positions (sentence) do not appear to be locally unique within documents (no duplicates in at least 10 documents). Unless you are sure they are, set sent_is_local to FALSE (and read documentation)")
    #}
    if (!sent_is_local) tokens[,'sentence' := local_position(tokens$sentence, tokens$doc_id, presorted = T)] ## make sure sentences are locally unique within documents (and not globally)
    if (!token_is_local) tokens[,'token_id' := global_position(tokens$token_id,
                                                            global_position(tokens$sentence, tokens$doc_id, presorted = T, position_is_local=T),
                                                            presorted = T)]  ## make token positions globally unique, taking sentence id into account (in case tokens are locally unique within sentences)
  }
  if (token_is_local) {
    if (!anyDuplicated(tokens, by=c('doc_id','token_id')) == 0) warning("Duplicate token ids (doc_id - token_id pairs) found. If token ids are not local at the document level, you can set token_is_local to False to use a token's position within a document as the token ids")
    #if (ndoc > 10) if (!anyDuplicated(tokens, by=c('doc_id','token_id')) == 0) warning("token positions (token_id) do not appear to be locally unique within documents (no duplicates in at least 10 documents). Unless you are sure they are, set token_is_local to FALSE (and read documentation)")
  } else {
    tokens[,'token_id' := local_position(tokens$token_id, tokens$doc_id, presorted=T)] ## make tokens locally unique within documents
  }

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
  tc$set_special(token=token_col, lemma=lemma_col, POS=pos_col, relation=relation_col, parent=parent_col)
  tc
}


## alternative:: add formal data check and correction methods, and then simply create the tcorpus from the data and then perform the checks and corrections


