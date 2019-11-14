prepare_model <- function(udpipe_model, local_path=getwd()) {

  udpipe_models = eval(formals(udpipe::udpipe_download_model)[[1]])

  if (!udpipe_model %in% udpipe_models) {
    guess_language = gsub('-.*','',udpipe_model)
    avail_language = stringi::stri_extract(udpipe_models, regex='.*(?=-)')
    suggest = udpipe_models[grep(guess_language, avail_language, ignore.case = T)]
    suggest = stats::na.omit(suggest)
    message = sprintf('"%s" is not a valid udpipe model (in the current repository).\n', udpipe_model)
    if (length(suggest) > 0) message = paste0(message, '\nAvailable models for "', guess_language, '" are: ', paste(paste0('"',suggest,'"'), collapse=', '))
    message = paste(message, '\nUse show_udpipe_models() for an overview of all available models')
    stop(message)
  }

  model_folder = gsub('-', '_', udpipe_model)
  path = make_dir(local_path, 'udpipe_models', model_folder)

  fname = list.files(path, full.names = T, include.dirs = F)
  fname = fname[grep('udpipe', fname)]
  if (length(fname) == 0) fname = NA
  if (!is.na(fname)) {
    m = udpipe::udpipe_load_model(fname)
    if (grepl('(nil)', deparse(m$model), fixed=T)) fname = NA   ## (nil) pointer indicates model is broken (e.g., partially downloaded), so needs to be downloaded again
  }
  if (is.na(fname)) {
    message(paste0("Model for this language does not yet exist. Will be downloaded to: ", path, '\n'))
    m_file = udpipe::udpipe_download_model(udpipe_model, model_dir = path)
    m = udpipe::udpipe_load_model(m_file$file_model)
  }
  m
}



#' Show the names of udpipe models
#'
#' Returns a data.table with the language, treebank and udpipe_model name.
#' Uses the default model repository provided by the udpipe package (\code{\link[udpipe]{udpipe_download_model}}).
#' For more information about udpipe and performance benchmarks of the UD models, see the
#' GitHub page of the \href{https://github.com/bnosac/udpipe}{udpipe package}.
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' show_udpipe_models()
show_udpipe_models <- function() {
  m = eval(formals(udpipe::udpipe_download_model)[[1]])
  language = stringi::stri_extract(m, regex='.*(?=-)')
  treebank = stringi::stri_extract(m, regex='(?<=-).*')
  data.frame(language=language, treebank=treebank, udpipe_model=m)
}


udpipe_parse <- function(x, udpipe_model, udpipe_model_path, cache=1, doc_id=1:length(x), use_parser=use_parser, max_sentences=NULL, max_tokens=NULL, verbose=F){
  m = prepare_model(udpipe_model, udpipe_model_path)

  batchsize = 100  ## keep constant, but if we ever change our minds, note that it is used to hash the cache ids
  batch_i = get_batch_i(length(doc_id), batchsize=batchsize, return_list=T)
  n = length(batch_i)
  if (verbose && n > 1) {
    pb = utils::txtProgressBar(min = 1, max = n, style = 3)
    pb$up(1)
  }

  if (cache > 0) {
    cache_path = make_dir(file.path(udpipe_model_path, 'udpipe_models'), 'caches')
    cache_hash =  digest::digest(list(x, udpipe_model, doc_id, use_parser, max_sentences, max_tokens, batchsize))

    current_caches = list.files(cache_path, full.names = T)
    cache_exists = grep(cache_hash, current_caches, fixed = T, value=T)
    if (length(cache_exists) > 0) {
      if (length(cache_exists) > 1) {
        ## should not happen, so remove if true
        for (cc in cache_exists[-1]) unlink(cc, recursive=T)
      }
      cache_dir = gsub('[0-9\\.]*$', '', cache_exists)  ## remove old time
      cache_dir = paste0(cache_dir, as.numeric(Sys.time())) ## add new time
      file.rename(cache_exists, cache_dir)
    } else {
      cache_dir = make_dir(cache_path, paste(cache_hash, as.numeric(Sys.time()), sep='_'))
    }

    ## remove old caches
    current_caches = list.files(cache_path, full.names = T)
    cache_time = as.numeric(gsub('.*_', '', current_caches))
    remove_caches = current_caches[order(cache_time)]
    remove_caches = head(remove_caches, -cache)
    for (cc in remove_caches) {
        unlink(cc, recursive=T)
    }

    cached_batches = list.files(cache_dir)
  } else cached_batches = c()

  tokens = vector('list', n)
  for (i in 1:n){
    if (i %in% cached_batches) {
      tokens[[i]] = readRDS(file.path(cache_dir, i))
    } else {
      tokens[[i]] = udpipe_parse_batch(x[batch_i[[i]]], m, doc_id=doc_id[batch_i[[i]]],
                                       use_parser=use_parser, max_sentences=max_sentences, max_tokens=max_tokens)
      if (cache > 0) saveRDS(tokens[[i]], file = file.path(cache_dir, i))
    }

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

  #warning('let op zum')

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


make_dir <- function(path=getwd(), ...) {
  if (is.null(path)){
    path = system.file(package='corpustools')
  } else {
    path = if (path == '') getwd() else normalizePath(gsub('\\/$', '', path))
  }
  if (file.access(path,"6") == -1) stop('You do not have write permission for this location')
  #path = paste(path, 'ext_resources', sep='/')

  add = paste(unlist(list(...)), collapse='/')
  if (!add == '') path = file.path(path, add)

  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  path
}


#' Cast the "feats" column in UDpipe tokens to columns
#'
#' If the UDpipe parser is used in \code{\link{create_tcorpus}}, the 'feats' column contains strings with features
#' (e.g, Number=Sing|PronType=Dem). To work with these nested features it is more convenient to cast them to columns.
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{
#' feats_to_columns(keep=NULL, drop=NULL, rm_column=T)
#' }
#'
#' @param keep     Optionally, the names of features to keep
#' @param drop     Optionally, the names of features to drop
#' @param rm_column If TRUE (default), remove the original column
#'
#' @name tCorpus$feats_to_columns
#' @aliases feats_to_columms
#' @examples
#' \donttest{
#' tc = create_tcorpus('This is a test Bobby.', udpipe_model='english-ewt')
#' tc$feats_to_columns()
#' tc$tokens
#'
#' tc = create_tcorpus('This is a test Bobby.', udpipe_model='english-ewt')
#' tc$feats_to_columns(keep = c('Gender','Tense','Person'))
#' tc$tokens
#' }
tCorpus$set('public', 'feats_to_columns', function(keep=NULL, drop=NULL, rm_column=T) {
  if (!'feats' %in% self$names) return(invisible(self))

  get_even <- function(x) x[1:length(x) %% 2 == 0]
  get_uneven <- function(x) x[1:length(x) %% 2 != 0]

  d = stringi::stri_split(self$tokens$feats, regex='[=|]')
  if (rm_column) self$delete_columns('feats')
  d[sapply(d, length) == 1] = list(NULL)  ## skip NA values
  d = data.table::data.table(.I = rep(1:self$n, sapply(d, length)/2),
                             name = get_uneven(unlist(d)),
                             value = get_even(unlist(d)))
  d = dcast(.I ~ name, value.var = 'value', data = d)

  for (col in colnames(d)[-1]) {
    if (!is.null(keep) && !col %in% keep) next
    if (col %in% drop) next
    cname = if (col %in% self$names) paste0('feats.', col) else col
    self$tokens[d$.I, (cname) := d[[col]]]
  }

  tc$tokens[]
  invisible(self)
})
