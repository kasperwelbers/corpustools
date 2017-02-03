
### feature concept ###
# - many functions for working with the tcorpus can be performed on shards (subsets) of a tcorpus, and then merged
# - this can be used to deal with the problem of not being able to fit a large tcorpus within memory
# - the idea is to make a separate class "shattered_tCorpus", that does not directly contain (all) data, but contains references to shards, stored as .rds files
# - functions where a sharded approach is possible should work with this class identically to how they would work for a regular tCorpus
# - the functions for creating a tcorpus should have a "sharded = TRUE" parameter, in which case this class will be created
# - there should be a function to merge shattered_tCorpus objects, so that it's possible to stepwise build a large tcorpus

### alternative ###
# - use sqlite database (report: tried that, much slower compared to using shards in rds)
# - use monetDBlite (report: faster than sqlite, but still slow and somewhat messy with connections)

### application examples
# - search through a large tCorpus.
# - perform various corpus analyses, such as word occurrence and co-occurrence,
# - extract a DTM (often much smaller in memory) to perform an analysis such as topic modeling. Then use the topic model to annotate (and visualize) the whole corpus


### ideas
# for convenience in programming, perhaps the best approach would be to have a check at the start of a function for whether the tcorpus is a shattered tcorpus.
# if it is, then
# note that the function has to return results that can be merged, aggregated or processed for the entire corpus.
# - This is (at least) the case for:
#   - searching, by rbinding hits
#   - subsetting, optionally into a normal tcorpus
#   - recoding, by adding columns per shard (thus also preprocessing)
#   - all things based on matrix multiplication (mainly cooccurrence/semnet) that do not use un-sum-able corpus statistics (see the cooccurrence_matrix and cooccurrence_window_matrix functions)

## store rds files as tmp with tempfile()
## make separate save and load functions for storing a shattered tcorpus (option: store all shards as separate tcorpus object. Effectively, make it so that if you save a shattered tcorpus, the references simply move from the tmp file to the saved files, and a separate rds file is saved for the shattered_tcorpus that ocntains the references)

tCorpus_shard_index = setClass("tCorpus_shard_index",
                   slots = c(index = 'data.table',
                             data_head = 'data.table',
                             meta_head = 'data.table',
                             shard_folders = 'vector',
                             feature_levels = 'vector',
                             p = 'list'),
                  prototype=list(
                     p = list(feature_index=F, context_level=NA, max_window_size=NA),
                     feature_index = data.table()
                   ))


setMethod("show", "tCorpus_shard_index",
          function(object) {
            info = shard_index_summary_data(object)
            cat('Index for a shattered tCorpus containing ', info$n, ' tokens (', info$index_mb, ' in memory)',
                '\nsplit by documents (n = ', info$n_meta, ')', info$sent_info,
                '\ntoken data is stored across ', info$n_shards ,' shards (', info$shards_mb, ' on disk)', info$shard_info,
                '\ncontains:',
                '\n  - ', length(info$data_colnames), ' data column', if(length(info$data_colnames) > 1) '(s)', ':\t', paste(info$data_colnames, collapse=', '),
                '\n  - ', length(info$meta_colnames), ' meta column', if(length(info$meta_colnames) > 1) '(s)', ': \t', paste(info$meta_colnames, collapse=', '),
                '\n', sep='')
          }
)

shard_index_summary_data <- function(tci) {
  ushards = unique(get_index_column(tci, '.SHARD'))
  data_n = regmatches(ushards, gregexpr('(?<=_T=)[0-9]+', ushards, perl = T))
  meta_n = regmatches(ushards, gregexpr('(?<=_M=)[0-9]+', ushards, perl = T))
  data_n = sum(as.numeric(data_n))
  meta_n = sum(as.numeric(meta_n))
  shards_n = length(ushards)

  data_colnames = colnames(tci@data_head)
  meta_colnames = colnames(tci@meta_head)
  sent_info = if('sent_i' %in% data_colnames) ' and sentences' else ''
  shard_info = if(length(tci@shard_folders) > 0) sprintf(' categorized by %s', paste(tci@shard_folders, collapse = ' -> ')) else ''

  fi = file.info(ushards)
  shards_mb = utils:::format.object_size(sum(fi$size), "Mb")
  index_mb = format(object.size(tci), 'Mb')
  list(n=data_n, n_meta=meta_n, n_shards=shards_n, data_colnames=data_colnames, meta_colnames=meta_colnames, shards_mb=shards_mb, index_mb=index_mb, sent_info=sent_info, shard_info=shard_info)
}


#' @export
get_index <- function(tci, columns=NULL, as_data_frame=F) {
  index = if(!is.null(columns)) tci@index[,columns, with=F] else tci@index
  if(as_data_frame) index = as.data.frame(index)
  index
}

n_index <- function(tci) nrow(tci@index)

#' @export
get_index_column <- function(tci, name) tci@index[[name]]


#' Create a shattered tCorpus from a single (large) tCorpus
#'
#' Shatters a tCorpus into one or more shards (i.e. subsets). Each subset is saved on disk and removed from memory.
#' The returned shattered_tCorpus object links to these subsets, and can be used as a normal tCorpus object (though with some restrictions).
#' Where possible, the tCorpus functions will then be performed per shard, and the entire tCorpus does not have to be kept in memory.
#' Put simply: if you run into memory problems because you have too much data, you'll probably want to shatter it.
#'
#' See the documentation for \link{shattered tCorpus} for a more detailed explanation of why you would ever want to shatter your tCorpus.
#'
#' !! please read the documentation for shard_path if you intend to save the shattered_tCorpus
#'
#' @param tc a tCorpus object
#' @param tokens_per_shard the number of tokens per shard. This is an approximation, since the tCorpus can not be broken within documents.
#' @param n_shards Alternatively, you can specify the number of shards. This is also only an approximation, as the algorithm tries to find equal sized shards that do not break documents
#' @param shard_path the path for where to save the shard files. If not specified, the shards will be saved as temporary files. Note that temporary files are not automatically saved if you save the shattered_tCorpus (which only contains the references), so if you intend to save its best to specify a path to store the shards properly. Alternatively, and recommended, you can use save_stk() to save a shattered_tCorpus with its shards
#'
#' @return a shattered_tCorpus object
#' @export
shatter_tcorpus <- function(tc, name, path='', meta_columns=c(), tokens_per_shard=1000000, if_exists=c('stop','overwrite','append'), compress=TRUE) {
  name = paste(name, 'tCorpus', sep='.')
  path = if(path == '') name else sprintf('%s/%s', path, name)
  if_exists = match.arg(if_exists)

  if(dir.exists(path)){
    if(if_exists == 'stop') stop('A shattered tCorpus with this name already exists at this location. Either rename, or set if_exists to "overwrite" or "append"')
    if(if_exists == 'overwrite') {
      unlink(name, recursive = TRUE)
      dir.create(path)
      tc_index = NULL
    }
    if(if_exists == 'append') {
      tc_index = readRDS(sprintf('%s/name/index.rds', path))
      tc = fit_to_index(tc, tc_index, name)
    }
  } else {
    dir.create(name)
    tc_index = NULL
  }
  index=NULL

  shards_path = sprintf('%s/%s', path, 'shards')
  shard_index = shatter_loop(tc, meta_columns=meta_columns, tokens_per_shard=tokens_per_shard, save_path=shards_path, compress=compress)

  index = cbind(.SHARD=shard_index, get_meta(tc))
  index = index[!index$.SHARD == 'DUPLICATE',]

  if(is.null(tc_index)){
    tc_index = tCorpus_shard_index(index=index, data_head=head(tc@data), meta_head=head(tc@meta), shard_folders=meta_columns)
  } else {
    tc_index@index = rbind(tc_index@index, index)
  }
  setkey(tc_index@index, '.SHARD')
  tc_index
}

fit_to_index <- function(tc, tc_index, name){
  cnames = colnames(get_data(tc))
  meta_cnames = colnames(get_meta(tc))
  cnames_index = colnames(tc_index@data_head)
  meta_cnames_index = colnames(tc_index@meta_head)

  if(!identical(cnames, cnames_index)){
    missing_data = cnames_index[!cnames_index %in% cnames]
    if(length(missing_data) > 0) stop(sprintf('Cannot append. the following columns in %s are not in the new batch: %s', name, paste(missing_data, collapse=', ')))
    extra_data = cnames[!cnames %in% cnames_index]
    if(length(extra_data) > 0) warning(sprintf('New batch contains columns that are not in %s [%s]. These columns have not been added', name, paste(extra_data, collapse=', ')))
    tc@data = tc@data[,cnames_index, with=F]
  }
  if(!identical(meta_cnames, meta_cnames_index)){
    missing_meta = meta_cnames_index[!meta_cnames_index %in% meta_cnames]
    if(length(missing_meta) > 0) stop(sprintf('Cannot append. the following META columns in %s are not in the new batch: %s', name, paste(missing_meta, collapse=', ')))
    extra_meta = meta_cnames[!meta_cnames %in% meta_cnames_index]
    if(length(extra_meta) > 0) warning(sprintf('New batch contains META columns that are not in %s [%s]. These columns have not been added', name, paste(extra_meta, collapse=', ')))
    tc@meta= tc@meta[,meta_cnames_index, with=F]
  }
  set_keys(tc)
  tc
}



shatter_loop <- function(tc, meta_columns=c(), tokens_per_shard=1000000, n_shards=NA, save_path='', compress=TRUE, shard_index=rep(NA, n_meta(tc)), verbose_string='') {
  if(!dir.exists(save_path)) dir.create(save_path)

  if(length(meta_columns) == 0){
    shard_index = save_shards(tc, tokens_per_shard=tokens_per_shard, n_shards=n_shards, save_path=save_path, compress=compress, verbose_string=verbose_string)
  } else {
    column = meta_columns[1]
    next_columns = meta_columns[!meta_columns == column]
    cat(verbose_string, column, '\n')
    verbose_string = paste(verbose_string, '---', sep='')

    val = as.character(get_meta_column(tc, column))
    for(uval in unique(val)){
      cat(verbose_string, uval, '\n')
      uval_path = if(save_path=='') uval else paste(save_path, uval, sep='/')
      if(!dir.exists(uval_path)) dir.create(uval_path)
      i = which(val == uval)
      shard_index[i] = shatter_loop(subset(tc, subset_meta= i), next_columns,
                                    tokens_per_shard=tokens_per_shard, n_shards=n_shards, save_path=uval_path, compress=compress,
                                    verbose_string=paste(verbose_string,'---', sep=''))
    }
  }
  shard_index
}


save_shards <- function(tc, tokens_per_shard=1000000, n_shards=NA, save_path='', compress=TRUE, verbose_string){
  existing_shards = list.files(save_path)
  n_existing_shards = sum(grepl('shard_[0-9]+_T=', existing_shards))

  n = n_meta(tc)
  if(n_existing_shards > 0){
    has_doc = vector('list', length(existing_shards))
    for(i in 1:length(existing_shards)) {
      es_file = sprintf('%s/%s', save_path, existing_shards[i])
      has_doc[[i]] = get_meta_column(readRDS(es_file), 'doc_id') ## this could probably be faster if the shattered_tcorpus remembers the meta
    }
    has_doc = unique(unlist(has_doc))
    duplicate = get_meta_column(tc, 'doc_id') %in% has_doc

    tc = subset(tc, subset_meta = which(!duplicate))
    if(sum(duplicate) > 0) cat(verbose_string, '### skipping ', sum(duplicate), ' duplicate doc_id', '\n')
  } else {
    duplicate = rep(F, n)
  }

  if(is.na(n_shards)) n_shards = ceiling(n_data(tc) / tokens_per_shard)
  batch_i = data_batch(tc, 'document', n.batches=n_shards, for_meta = T)

  shard_index = rep('DUPLICATE', n) ## using the original n, before deleting duplicates. Then use !duplicates to only provide the index for the non-duplicate indices
  if(is.null(batch_i)) return(shard_index) ## if there are no batches, stop

  for(i in 1:nrow(batch_i)){
    meta_i = (batch_i$start[i]):(batch_i$end[i])
    shard = subset(tc, subset_meta = meta_i)
    if(n_meta(shard) > 0){
      fname = sprintf('shard_%s_T=%s_M=%s.rds', i+n_existing_shards, n_data(shard), n_meta(shard))
      cat(verbose_string, fname, '\n')
      fpath = sprintf('%s/%s', save_path, fname)
      shard_index[!duplicate][meta_i] = fpath
      saveRDS(shard, fpath, compress=compress)
    }
  }
  shard_index
}

add_shard <- function(x) NULL ## add shard to a shattered_tCorpus, to enable stepwise building of large tCorpus

get_shard <- function(x) NULL ## get a shard as a normal tCorpus

renew_shards <- function(x) NULL ## redistribute tokens equally over shards, and possibly specify a new number of shards

delete_shards <- function(x) NULL


