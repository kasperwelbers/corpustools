prepare_model <- function(language, local_path=getOption('corpustools_resources', NULL)) {
  udpipe_languages = eval(formals(udpipe::udpipe_download_model)[[1]])
  if (!language %in% udpipe_languages) stop(sprintf('language passed to udpipe_model ("%s") is not available. Choose from:\n%s', language, paste(udpipe_languages, collapse=', ')))

  path = make_dir(local_path, 'udpipe', language)

  fname = list.files(path, full.names = T)[1]
  if (!is.na(fname)) {
    m = udpipe::udpipe_load_model(fname)
    if (grepl('(nil)', deparse(m$model), fixed=T)) fname = NA   ## (nil) pointer indicates model is broken (e.g., partially downloaded), so needs to be downloaded again
  }
  if (is.na(fname)) {
    message(paste0("Model for this language does not yet exist. Will be downloaded to: ", path, '\n'))
    m_file = udpipe::udpipe_download_model(language, model_dir = path)
    m = udpipe::udpipe_load_model(m_file$file_model)
  }
  m
}

udpipe_parse <- function(x, udpipe_model, udpipe_model_path, doc_id=1:length(x), use_parser=use_parser, max_sentences=NULL, max_tokens=NULL, verbose=F){
  udpipe_model = prepare_model(udpipe_model, udpipe_model_path)

  batch_i = get_batch_i(length(doc_id), batchsize=100, return_list=T)
  n = length(batch_i)
  if (verbose && n > 1) {
    pb = utils::txtProgressBar(min = 1, max = n, style = 3)
    pb$up(1)
  }
  tokens = vector('list', n)
  for (i in 1:n){
    tokens[[i]] = udpipe_parse_batch(x[batch_i[[i]]], udpipe_model, doc_id=doc_id[batch_i[[i]]],
                                              use_parser=use_parser, max_sentences=max_sentences, max_tokens=max_tokens)
    if (verbose && n > 1) pb$up(i+1)
  }
  tokens = data.table::rbindlist(tokens)

  ## set factors
  tokens[, doc_id := fast_factor(tokens$doc_id)]
  for(col in colnames(tokens)) {
    if (class(tokens[[col]]) %in% c('character')) tokens[,(col) := fast_factor(tokens[[col]])]
  }
  tokens
}

udpipe_parse_batch <- function(x, udpipe_model, doc_id, use_parser, max_sentences, max_tokens) {
  token_id = NULL; head_token_id = NULL; sentence_id = NULL ## prevent no visible bindings error (due to data table syntax)

  parser = if (use_parser) 'default' else 'none'

  x <- udpipe::udpipe_annotate(udpipe_model, x = x, doc_id=doc_id, parser = parser)
  x = as.data.table(x)

  warning('let op zum')

  x[,token_id := as.numeric(token_id)]
  x[,head_token_id := as.numeric(head_token_id)]

  ## make token_id local within documents instead of within sentences
  #### making sure the parent_id (head_token_id) still matches correctly (only if use_parser is true)
  if (use_parser) parent_match = x[list(x$doc_id, x$sentence_id, x$head_token_id),,on=c('doc_id','sentence_id', 'token_id'), which=T] ## efficient 3 column match that returns indices
  x$token_id = local_position(1:nrow(x), x$doc_id)   ## currently assuming that there are no gaps in token_ids. (otherwise need cumbersome solution from tokens_to_corpus function)
  if (use_parser) x$head_token_id = x$token_id[parent_match]

  if (!is.null(max_tokens) && !is.null(max_sentences)) {
    x = subset(x, sentence_id <= max_sentences & token_id <= max_tokens)
  } else {
    if (!is.null(max_tokens)) x = subset(x, token_id <= max_tokens)
    if (!is.null(max_sentences)) x = subset(x, sentence_id <= max_sentences)
  }

  drop_cols = c('paragraph_id', 'sentence')
  if (!use_parser) drop_cols = c(drop_cols, c('head_token_id','dep_rel','deps'))
  cols = setdiff(colnames(x), drop_cols)
  x = subset(x, select=cols)
  data.table::setnames(x, old = c('sentence_id', 'upos'), new = c('sentence', 'POS'))
  if (use_parser) data.table::setnames(x, old = c('head_token_id', 'dep_rel'), new = c('parent','relation'))

  x
}


