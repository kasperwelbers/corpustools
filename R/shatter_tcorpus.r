
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

tCorpus_index = setClass("tCorpus_index", slots = c(path = 'vector'))

#tCorpus_shard_index = setClass("tCorpus_shard_index",
#                               slots = c(index = 'data.table',
#                                         data_head = 'data.table',
#                                         meta_head = 'data.table',
#                                         shard_folders = 'vector',
#                                         feature_levels = 'vector',
#                                         p = 'list'),
#                               prototype=list(
#                                 p = list(feature_index=F, context_level=NA, max_window_size=NA),
#                                 feature_index = data.table()
#                               ))

setMethod("show", "tCorpus_index",
          function(object) {
            info = get_info(object)
            data_colnames = colnames(info$data_head)
            meta_colnames = colnames(info$meta_head)
            cat('Index for a shattered tCorpus containing ', info$n, ' tokens',
                '\nsplit by documents (n = ', info$n_meta, ')', info$sent_info,
                '\ntoken data is stored across ', info$n_shards ,' shards ', info$shard_info,
                '\ncontains:',
                '\n  - ', length(data_colnames), ' data column', if(length(data_colnames) > 1) '(s)', ':\t', paste(data_colnames, collapse=', '),
                '\n  - ', length(meta_colnames), ' meta column', if(length(meta_colnames) > 1) '(s)', ': \t', paste(meta_colnames, collapse=', '),
                '\n', sep='')
          }
)

shard_index_summary_data <- function(tc_index, data_head, meta_head, shard_folders) {
  ushards = get_shards(tc_index, full.names = F)
  data_n = regmatches(ushards, gregexpr('(?<=_T=)[0-9]+', ushards, perl = T))
  meta_n = regmatches(ushards, gregexpr('(?<=_M=)[0-9]+', ushards, perl = T))
  data_n = sum(as.numeric(data_n))
  meta_n = sum(as.numeric(meta_n))
  shards_n = length(ushards)

  sent_info = if('sent_i' %in% colnames(data_head)) ' and sentences' else ''
  shard_info = if(length(shard_folders) > 0) sprintf(' categorized by: %s', paste(shard_folders, collapse = ' -> ')) else ''

  #fi = file.info(ushards)
  #shards_mb = utils:::format.object_size(sum(fi$size), "Mb")
  #index_mb = format(object.size(index), 'Mb')
  list(n=data_n, n_meta=meta_n, n_shards=shards_n, data_head=data_head, meta_head=meta_head, sent_info=sent_info, shard_info=shard_info)
}

get_path <- function(tc_index) tc_index@path


#' @export
get_index <- function(tc_index, columns=NULL, as_data_frame=F) {
  #index = if(!is.null(columns)) tci@index[,columns, with=F] else tci@index
  index = readRDS(sprintf('%s/index.rds', get_path(tc_index)))
  if(as_data_frame) index = as.data.frame(index)
  index
}

get_feature_levels <- function(tc_index) readRDS(sprintf('%s/feature_levels.rds', get_path(tc_index)))

get_info <- function(tc_index) readRDS(sprintf('%s/info.rds', get_path(tc_index)))

get_shards <- function(tc_index, full.names=T) list.files(sprintf('%s/shards', get_path(tc_index)), recursive = T, full.names = full.names)

n_index <- function(tc_index) nrow(get_index(tc_index))

#' @export
get_index_column <- function(tc_index, name) get_index(tc_index)[[name]]


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
shatter_tcorpus <- function(tc, tc_index, meta_columns=c(), tokens_per_shard=1000000, if_exists=c('stop', 'append','overwrite'), compress=TRUE) {
  if_exists = match.arg(if_exists)

  if(is(tc_index, 'character')) {
    if(!grepl('.tCorpus$', tc_index)) tc_index = paste(tc_index, 'tCorpus', sep='.')
    tc_index = tCorpus_index(path=tc_index)
  }

  if(dir.exists(get_path(tc_index))){
    if(if_exists == 'stop') stop('A shattered tCorpus with this name already exists at this location. Either rename, or set if_exists to "overwrite" or "append"')
    if(if_exists == 'overwrite') unlink(get_path(tc_index), recursive = TRUE)
    if(if_exists == 'append') {
        tc = fit_columns_to_index(tc, tc_index)
        tc = reindex_features(tc, tc_index)
    }
  }
  if(!dir.exists(get_path(tc_index))){ ## not that this should not be the else statement for the previous if, because it needs to check again in case if_exists == "overwrite"
    dir.create(get_path(tc_index))
  }

  shards_path = sprintf('%s/%s', get_path(tc_index), 'shards')
  shard_index = shatter_loop(tc, meta_columns=meta_columns, tokens_per_shard=tokens_per_shard, save_path=shards_path, compress=compress)

  index = cbind(.PATH=gsub('shard_[0-9]+_T=.*', '', shard_index),
                .SHARD=shard_index,
                .N = ntokens_index(tc),
                get_meta(tc))
  index = index[!index$.SHARD == 'DUPLICATE',]

  if(file.exists(sprintf('%s/index.rds', get_path(tc_index)))) index = rbindlist(list(get_index(tc_index), index))
  setkeyv(index, c('.PATH', '.SHARD'))
  saveRDS(index, sprintf('%s/index.rds', get_path(tc_index)))

  info = shard_index_summary_data(tc_index, head(get_data(tc)), head(get_meta(tc)), meta_columns)
  saveRDS(info, sprintf('%s/info.rds', get_path(tc_index)))

  feature_levels = feature_levels_list(tc) ## note that for appending, the feature levels in tc have already been matched with the previous data, and thus contain all levels
  saveRDS(feature_levels, sprintf('%s/feature_levels.rds', get_path(tc_index)))

  tc_index
}

ntokens_index <- function(tc){
  data = get_data(tc)
  freq = data[,.N,by='doc_id']
  meta_id = get_meta_column(tc, 'doc_id')
  freq$N[match(meta_id, freq$doc_id)]
}

fit_columns_to_index <- function(tc, tc_index){
  info = get_info(tc_index)
  name = get_path(tc_index)

  cnames = colnames(get_data(tc))
  meta_cnames = colnames(get_meta(tc))
  cnames_index = colnames(info$data_head)
  meta_cnames_index = colnames(info$meta_head)

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



## manage factors

feature_levels_list <- function(tc){
  d = get_data(tc)
  factors = colnames(d)[lapply(d, class) == 'factor']
  feature_levels = list()
  for(f in factors) feature_levels[[f]] = levels(d[[f]])
  feature_levels
}

reindex_features <- function(tc, tc_index){
  feature_levels = get_feature_levels(tc_index)
  features = featurenames(tc)
  for(feature in features){
    if(!is(tc@data[[feature]], 'factor')) next
    fl = feature_levels[[feature]]
    tc@data[[feature]] = match_factor_labels(tc@data[[feature]], fl)
  }
  set_keys(tc)
  tc
}

match_factor_labels <- function(f, flabels){
  ## change the integers of f (factor) to match the flabels, and append flabels to include the labels of f that are not in flabels. (by appending, the factor from which the flabels come does not have to change)
  add_labels = levels(f)[!levels(f) %in% flabels]
  flabels = c(flabels, add_labels)
  level_conversion = match(levels(f), flabels)
  factor(level_conversion[as.numeric(f)], levels=1:length(flabels), labels=flabels)
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
    set_keys(shard)
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

equal_groups <- function(index, tokens_per_shard){
  n = sum(index$.N)
  n_shards = ceiling(n / (tokens_per_shard)) ## how many shards are required
  tokens_per_shard = ceiling(n / n_shards) ## what is the most equal distriution over those shards

  mod = cumsum(index$.N) %% tokens_per_shard
  break_i = mod < c(1, mod[-length(mod)]) # wrap around point
  shards = cumsum(break_i)+1

  ## if last shard is small (can happen if due to differences in document length the last few documents are just behind the wrapping point), distribute the documents over the other groups
  shardn = tapply(mod, shards, 'sum')
  if(shardn[length(shardn)] < tokens_per_shard*0.25) {
    is_last = shards == length(shardn)
    shards[is_last] = rep(1:(length(shardn)-1), length.out=sum(is_last))
  }
  match(shards, unique(shards))
}

collect_shards <- function(shard_names, doc_ids=NULL) {
  shard = merge_shards(sapply(shard_names, readRDS))
  if(!is.null(doc_ids)) shard = subset(shard, subset_meta = doc_id %in% as.character(doc_ids))
  shard
}

#' Redistribute shards in a shattered tCorpus
#'
#' For good performance it's generally best to keep shards of moderate size. Not too small to prevent tedious merging and looping, and not too big to prevent memory problems. This function redistributes the shards (within categories) to approximate the specified tokens_per_shard
#'
#' @param tc_index
#' @param tokens_per_shard
#'
#' @export
redistribute_shards <- function(tc_index, tokens_per_shard=10000) {
  shard_index = get_index(tc_index)
  shard_index$.I  = 1:nrow(shard_index) ## keep order to replace .SHARD and return the shard_index (instead of rbinding it)
  new_shard_index = rep(NA, nrow(shard_index))
  for(path in unique(shard_index$.PATH)){
    index = shard_index[path]
    newshards = equal_groups(index, tokens_per_shard)
    for(shard_i in unique(newshards)){
      i = shard_i == newshards
      shard = collect_shards(unique(index$.SHARD[i]), doc_ids = index$doc_id[i])
      shard_name = sprintf('shard_%s_T=%s_M=%s.rds', shard_i, n_data(shard), n_meta(shard))

      saveRDS(shard, sprintf('%s/NEW_%s', path, shard_name)) ## add NEW_ to name -> delete old shards -> delete random string from new name
      new_shard_index[index$.I][i] = shard_name
    }
    old_shards = unique(index$.SHARD)
    removed = file.remove(old_shards)
    new_shards = list.files(path, full.names = T)
    for(ns in new_shards) file.rename(ns, gsub('NEW_(shard_[0-9]+_T=)', '\\1', ns))
  }
  shard_index$.I = NULL
  shard_index$.SHARD = new_shard_index
  setkeyv(shard_index, c('.PATH','.SHARD'))
  saveRDS(shard_index, sprintf('%s/index.rds', get_path(tc_index)))

  info = get_info(tc_index)
  info$n_shards = length(unique(shard_index$.SHARD))
  saveRDS(info, sprintf('%s/info.rds', get_path(tc_index)))

  tc_index
}

reindex_shards <- function(tc_index){
  NULL
  ## easiest way:
  #### rename old shards file to old_shards
  #### rebuild the shards file with the new settings
  #### delete old_shards
}

