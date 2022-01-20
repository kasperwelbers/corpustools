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
#' @param x                 main input. can be a character (or factor) vector where each value is a full text, or a data.frame that has a column that contains full texts.
#' @param meta              A data.frame with document meta information (e.g., date, source). The rows of the data.frame need to match the values of x
#' @param udpipe_model      Optionally, the name of a Universal Dependencies language model (e.g., "english-ewt", "dutch-alpino"), to use the udpipe package
#'                          (\code{\link[udpipe]{udpipe_annotate}}) for natural language processing. You can use \code{\link{show_udpipe_models}} to get
#'                          an overview of the available models. For more information about udpipe and performance benchmarks of the UD models, see the
#'                          GitHub page of the \href{https://github.com/bnosac/udpipe}{udpipe package}.
#' @param split_sentences   Logical. If TRUE, the sentence number of tokens is also computed. (only if udpipe_model is not used)
#' @param max_tokens        An integer. Limits the number of tokens per document to the specified number
#' @param max_sentences     An integer. Limits the number of sentences per document to the specified number. If set when split_sentences == FALSE, split_sentences will be set to TRUE.
#' @param doc_id            if x is a character/factor vector, doc_id can be used to specify document ids. This has to be a vector of the same length as x
#' @param doc_column        If x is a data.frame, this specifies the column with the document ids.
#' @param text_columns      if x is a data.frame, this specifies the column(s) that contains text. Note that using multiple text columns does more than just paste
#'                          them together (which you can do yourself if you're note interested in distinguisthing the text fields). If multiple columns are used,
#'                          A "field" column is added to the tokens that indicates from which text_column it came. If remember_spaces is used, the start and end positions 
#'                          are local within fields.
#' @param udpipe_model_path If udpipe_model is used, this path wil be used to look for the model, and if the model doesn't yet exist it will be downloaded to this location. Defaults to working directory
#' @param udpipe_cache      The number of persistent caches to keep for inputs of udpipe. The caches store tokens in batches.
#'                          This way, if a lot of data has to be parsed, or if R crashes, udpipe can continue from the latest batch instead of start over.
#'                          The caches are stored in the corpustools_data folder (in udpipe_model_path). Only the most recent [udpipe_caches] caches will be stored.
#' @param udpipe_cores      If udpipe_model is used, this sets the number of parallel cores. If not specified, will use the same number of cores as used by data.table (or limited to OMP_THREAD_LIMIT).
#' @param udpipe_batchsize  In order to report progress and cache results, texts are parsed with udpipe in batches of 50.
#'                          The price is that there will be some overhead for each batch, so for very large jobs it can be faster to increase the batchsize.
#'                          If the number of texts divided by the number of parallel cores is lower than the batchsize, the texts are evenly distributed over cores.
#' @param use_parser        If TRUE, use dependency parser (only if udpipe_model is used)
#' @param remember_spaces   If TRUE, a column with spaces after each token and column with the start and end positions of tokens are included. Enables correct reconstruction of original text.
#' @param verbose           If TRUE, report progress. Only if x is large enough to require multiple sequential batches
#' @param ...               Arguments passed to create_tcorpus.character
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
create_tcorpus.character <- function(x, doc_id=1:length(x), meta=NULL, udpipe_model=NULL, split_sentences=F, max_sentences=NULL, max_tokens=NULL, udpipe_model_path=getwd(), udpipe_cache=3, udpipe_cores=NULL, udpipe_batchsize=50, use_parser=F, remember_spaces=TRUE, verbose=T, ...) {
  if (is.null(udpipe_cores)) udpipe_cores = use_n_cores(udpipe_cores)
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

  
  x = transparent_trim(x, remember_spaces)
  if (!is.null(udpipe_model)) {
    data = udpipe_parse(x, udpipe_model, udpipe_model_path, udpipe_cores=udpipe_cores, cache=udpipe_cache, max_batchsize=udpipe_batchsize, doc_ids=doc_id, use_parser=use_parser, max_sentences=max_sentences, max_tokens=max_tokens, remember_spaces=remember_spaces, verbose=verbose)
  } else {
    data = tokenize_to_dataframe(x, doc_id=doc_id, split_sentences=split_sentences, max_sentences=max_sentences, max_tokens=max_tokens, remember_spaces=remember_spaces, verbose=verbose)
  }
  model = if (is.null(udpipe_model)) 'basic tokenization' else udpipe_model
  tCorpus$new(tokens = data, meta = base::droplevels(meta), model=model)
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
create_tcorpus.data.frame <- function(x, text_columns='text', doc_column='doc_id', udpipe_model=NULL, split_sentences=F, max_sentences=NULL, max_tokens=NULL, udpipe_model_path=getwd(), udpipe_cache=3, udpipe_cores=NULL, udpipe_batchsize=50, use_parser=F, remember_spaces=TRUE, verbose=T, ...) {
  
  for(cname in text_columns) {
    if (!cname %in% colnames(x)) stop(sprintf('text_column "%s" not in data.frame', cname))
    x[[cname]] = transparent_trim(x[[cname]], remember_spaces)
  }
 
  if (length(text_columns) > 1){
    text = do.call(paste, c(as.list(x[,text_columns]), sep='\n\n'))
  } else {
    text = x[[text_columns]]
  }

  if (!doc_column %in% colnames(x)) {
    message('No existing document column (doc_column) specified. Using indices as id.')
    doc_id = 1:nrow(x)
  } else doc_id = x[[doc_column]]

  need_spaces = remember_spaces || length(text_columns) > 1
  tc = create_tcorpus(text,
                 doc_id = doc_id,
                 meta = x[,!colnames(x) %in% c(text_columns, doc_column), drop=F],
                 udpipe_model=udpipe_model, split_sentences = split_sentences, max_sentences = max_sentences, max_tokens = max_tokens, 
                 udpipe_model_path=udpipe_model_path, udpipe_cache=udpipe_cache, udpipe_cores=udpipe_cores, udpipe_batchsize=udpipe_batchsize, 
                 use_parser=use_parser, remember_spaces=need_spaces, verbose=verbose)
  
  if (length(text_columns) > 1) {
    tc = add_fields(tc, x, text_columns)
  } 
  
  if (need_spaces && !remember_spaces) {
    tc$tokens$start = NULL
    tc$tokens$end = NULL
    tc$tokens$space = NULL
  }
  tc
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
#' library(quanteda)
#' create_tcorpus(data_corpus_inaugural)
#' @export
create_tcorpus.corpus <- function(x, ...) {
  x = quanteda::convert(x, 'data.frame')
  create_tcorpus(x, text_columns='text', doc_column='doc_id', ...)
}

transparent_trim <- function(x, remember_spaces) {
  beforetrim = nchar(as.character(x))
  x = stringi::stri_trim_left(x)
  aftertrim = nchar(as.character(x))
  trimmed = sum(aftertrim < beforetrim)
  if (trimmed > 0) {
    warnoffset = if (remember_spaces) '(The start and end positions are for the trimmed text)' else ''
    warning(sprintf("Leading whitespace has been trimmed for %s texts. %s", trimmed, warnoffset))
  }
  x
}

add_fields <- function(tc, x, text_columns, local_positions=T) {
  tc$tokens$len = nchar(as.character(tc$tokens$token)) + nchar(as.character(tc$tokens$space))
  tc$tokens[,start := 1 + c(0, head(cumsum(len),-1)), by='doc_id']
  tc$tokens[,end := start + nchar(as.character(token)) - 1]
  
  tf_start = data.table::data.table(doc_id = unique(tc$tokens$doc_id), offset=1)
  tf_start[['1']] = 1
  for (i in 2:length(text_columns)) {
    tf_start$offset = tf_start$offset + nchar(x[[text_columns[i-1]]]) + nchar('\n\n')
    tf_start[[as.character(i)]] = tf_start$offset
  }
  tf_start$offset = NULL
  tf_start = data.table::melt(tf_start, id.vars='doc_id', variable.name='field', value.name = 'start')
  
  tc$tokens = tc$tokens[tf_start, field := as.numeric(tf_start$field), on=c('doc_id','start')]
  tc$tokens[, field := data.table::nafill(field, 'locf'), by='doc_id']
  tc$tokens$field = fast_factor(text_columns[tc$tokens$field])
  if (local_positions) {
    tc$tokens[, end := end - first(start) + 1, by=c('doc_id','field')]
    tc$tokens[, start := start - first(start) + 1, by=c('doc_id','field')]
  }
  tc
}
