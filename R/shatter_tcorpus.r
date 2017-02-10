#' A shattered tCorpus class
#'
#' Shatters a tCorpus into one or more shards (i.e. subsets). Each subset is saved on disk instead of being kept in memory, and will only be loaded into memory when required.
#' The returned shattered_tCorpus object links to these subsets, and can be used as a normal tCorpus object (though with some restrictions).
#' Where possible, the tCorpus functions will then be performed per shard, and the entire tCorpus does not have to be kept in memory.
#' Put simply: if you run into memory problems because you have too much data, you'll probably want to shatter it.
#'
#' If you like analogies, think of your huge corpus of texts as a huge vase.
#' You want to study the patterns on the vase, for obvious reasons, but the darn thing is too big to fit into your special vase pattern scanning maching.
#' Buying a bigger machine is out of your research budget and/or goals in life, so instead you decide to bring the hammer to it.
#' Now, each shard does fit into your machine, and you (and your army of reasearch assistants) can scan each piece separately and put the results back together.
#' However, you now have new problems.
#' One is that organizing this ordeal can be tricky: you don't want to lose track of any shards, and putting the results back together is often not trivial.
#' Another problem is that for some analyses you might also want to take certain characteristics of the whole vase into account, such as its shape.
#' Ideally, you want to study the whole vase, an let the tedious but necessary shattering and glueing to be done for you, behind the scenes.
#' Back to our case: the shattered_tCorpus is a single object that can be treated as a normal tCorpus (with some restrictions), but behind the scenes it shatters the corpus so that it only has to keep one or several shards in memory at a time.
#'
#' So, to summarize.
#' Why do you want to shatter your corpus?
#' - You don't. You simply have to because your data is too big for your machine. You hate this and want to deal with it as little as possible.
#'
#' Why do I want to make a shattered_tCorpus. Why not simply loop over my data?
#' - Firstly, because this can be a hassle. It's convenient to be able to work with your data through a single object. This also makes it easier to develop and share code for all sorts of analysis of big textual corpora.
#' - Secondly, because certain operations become quite more complex to perform (efficiently), especially if they require both local information (shards) and global information (corpus).
#'
#' Can I big data now?
#' - This functionality is not competing with big data software such as Hadoop. The goal is not to facilitate cluster computing of shards.
#'   Still, if you have a decent computer and super-speed is not your goal, then the shattered_tCorpus approach should enable you to scale up pretty well.
#'
#' @slot path vector.
#'
#' @export
shattered_tCorpus = setClass("shattered_tCorpus", slots = c(path = 'vector'))

setMethod("show", "shattered_tCorpus",
          function(object) {
            info = get_info(object)
            data_colnames = colnames(info$data_head)
            meta_colnames = colnames(info$meta_head)
            cat('Index for a shattered tCorpus containing ', info$n, ' tokens',
                '\nsplit by documents (n = ', info$n_meta, ')', info$sent_info,
                '\ntoken data is stored across ', info$n_shards ,' shards ', sprintf(' categorized by: %s', paste(info$shard_folders, collapse = ' -> ')),
                '\ncontains:',
                '\n  - ', length(data_colnames), ' data column', if(length(data_colnames) > 1) '(s)', ':\t', paste(data_colnames, collapse=', '),
                '\n  - ', length(meta_colnames), ' meta column', if(length(meta_colnames) > 1) '(s)', ': \t', paste(meta_colnames, collapse=', '),
                '\n', sep='')
          }
)

shard_index_info <- function(stc, data_head, meta_head, shard_folders) {
  ushards = get_shards(stc, normalize = F)
  data_n = regmatches(ushards, gregexpr('(?<=_T=)[0-9]+', ushards, perl = T))
  meta_n = regmatches(ushards, gregexpr('(?<=_M=)[0-9]+', ushards, perl = T))
  data_n = sum(as.numeric(data_n))
  meta_n = sum(as.numeric(meta_n))
  shards_n = length(ushards)

  sent_info = if('sent_i' %in% colnames(data_head)) ' and sentences' else ''
  list(n=data_n, n_meta=meta_n, n_shards=shards_n, data_head=data_head, meta_head=meta_head, sent_info=sent_info, shard_folders=shard_folders)
}

#' @export
get_path <- function(stc, normalize=F) {
  path = stc@path
  if(!grepl('\\.tCorpus$', path)) stop("path in tCorpus is not a legit tCorpus path (which has the .tCorpus extension)")
  if(normalize) path = normalizePath(path)
  path
}

#' @export
set_path <- function(stc, path) stc@path = path

#' @export
get_index <- function(stc, columns=NULL, as_data_frame=F) {
  #index = if(!is.null(columns)) tci@index[,columns, with=F] else tci@index
  if(!file.exists(sprintf('%s/index.rds', get_path(stc)))) return(NULL)
  index = readRDS(sprintf('%s/index.rds', get_path(stc)))
  if(!is.null(columns)) index = index[,columns, with=F]
  if(as_data_frame) index = as.data.frame(index) else setkey(index, '.PATH','.SHARD')
  index
}

set_index <- function(stc, new_index){
  saveRDS(new_index, sprintf('%s/index.rds', get_path(stc)))
}

#' Open a shattered_tCorpus stored on disk
#'
#' If a shattered_tCorpus has been created, this function can be used to read/open/load it by
#'
#' For the sake of reference: a shattered_tCorpus is nothing more than an object that refers to a folder containing the tCorpus shards and index.
#' To create a shattered_tCorpus, consult the documentation ?shattered_tCorpus.
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
open_shattered_tc <- function(path) {
  shattered_tCorpus(path=path)
}

#' Delete a shattered_tCorpus and all files it refers to
#'
#' Since the shattered_tCorpus object saves all actual data on disk, it cannot be deleted
#'
#' @param stc A shattered_tCorpus object
#'
#' @return
#' @export
#'
#' @examples
delete_shattered_tc <- function(stc) {
  path = get_path(stc)
  if(!grepl('\\.tCorpus$', path)) stop(sprintf('Cannot delete %s. Not a legit shattered_tCorpus folder'))
  unlink(path, recursive = T)
}

#' @export
get_feature_levels <- function(stc) readRDS(sprintf('%s/feature_levels.rds', get_path(stc)))

set_feature_levels <- function(stc, feature_levels) saveRDS(feature_levels, sprintf('%s/feature_levels.rds', get_path(stc)))

#' @export
get_info <- function(stc, name=NULL) {
  info = readRDS(sprintf('%s/info.rds', get_path(stc)))
  if(is.null(name)) info else info[[name]]
}

set_info <- function(stc, value, name=NULL) {
  if(is.null(name)){
    saveRDS(value, sprintf('%s/info.rds', get_path(stc)))
  } else {
    info = readRDS(sprintf('%s/info.rds', get_path(stc)))
    info[[name]] = value
    saveRDS(info, sprintf('%s/info.rds', get_path(stc)))
  }
}

#' @export
get_shards <- function(stc, normalize=T) {
  ##shards = list.files(sprintf('%s/shards', get_path(stc)), recursive = T) ## instead, get shard from the index, in case the shards in the folders are corrupted
  index = unique(get_index(stc, columns = c('.PATH','.SHARD')))
  shards = index$.SHARD
  #shards = paste(index$.PATH, index$.SHARD, sep='') ## path is now included in shard (keeping this row in case this changes back)
  if(normalize) shards = normalizePath(shards)
  shards
}

#' @export
n_index <- function(stc) nrow(get_index(stc))

#' @export
get_index_column <- function(stc, name) get_index(stc)[[name]]


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
#' @param tokens_per_shard The number of tokens per shard. This is an approximation, since a tCorpus should not be broken within documents. Also, given the number of shards that is required, tokens will be evently distributed (for instance, if tokens_per_shard is 1,000,000, and there are 1,500,000 tokens, you'll get 2 tokens of 750,000 shards)
#' @param n_shards Alternatively, you can specify the number of shards. This is also only an approximation, as the algorithm tries to find equal sized shards that do not break documents
#' @param shard_path the path for where to save the shard files. If not specified, the shards will be saved as temporary files. Note that temporary files are not automatically saved if you save the shattered_tCorpus (which only contains the references), so if you intend to save its best to specify a path to store the shards properly. Alternatively, and recommended, you can use save_stk() to save a shattered_tCorpus with its shards
#'
#' @return a shattered_tCorpus object
#' @export
shatter_tcorpus <- function(tc, stc, meta_columns=NULL, tokens_per_shard=1000000, if_exists=c('stop','append','overwrite'), if_duplicates=c('stop','skip','rename'), compress=TRUE, verbose=F) {
  if_exists = match.arg(if_exists)
  if_duplicates = match.arg(if_duplicates)
  meta_columns = if(!is.null(meta_columns)) match.arg(meta_columns, metanames(tc), several.ok = T) else c()

  if(is(stc, 'character')) {
    if(!grepl('.tCorpus$', stc)) stc = paste(stc, 'tCorpus', sep='.')
    stc = shattered_tCorpus(path=stc)
  }

  if(file.exists(get_path(stc))){
    if(if_exists == 'stop') stop('A shattered tCorpus with this name already exists at this location. Either rename, or set if_exists to "overwrite" or "append"')
    if(if_exists == 'overwrite') {
      unlink(get_path(stc), recursive = TRUE)
      message(sprintf('Overwritten %s', get_path(stc)))
    }
    if(if_exists == 'append') {
        index = get_index(stc)
        tc = manage_duplicates(tc, index, if_duplicates)
        if(n_data(tc) == 0) return(stc)
        tc = fit_columns_to_index(tc, stc)
        tc = reindex_features(tc, stc)
    }
  }
  if(!file.exists(get_path(stc))){ ## not that this should not be the else statement for the previous if, because it needs to check again in case if_exists == "overwrite"
    dir.create(get_path(stc))
    index = NULL
  }

  shards_path = sprintf('%s/%s', get_path(stc), 'shards')
  shard_index = shatter_loop(tc, meta_columns=meta_columns, tokens_per_shard=tokens_per_shard, save_path=shards_path, compress=compress, verbose=verbose)

  new_index = cbind(.PATH=gsub('shard_[0-9]+_T=.*', '', shard_index),
                    .SHARD=shard_index,
                    .N = ntokens_index(tc),
                    get_meta(tc))

  index = rbindlist(list(index, new_index))
  setkeyv(index, c('.PATH', '.SHARD'))
  set_index(stc, index)

  info = shard_index_info(stc, head(get_data(tc)), head(get_meta(tc)), meta_columns)
  set_info(stc, info)

  set_feature_levels(stc, feature_levels_list(tc))## note that for appending, the feature levels in tc have already been matched with the previous data, and thus contain all levels

  inspect_shard_index(stc)
  stc
}

manage_duplicates <- function(tc, index, if_duplicates){
  duplicate = tc@meta$doc_id %in% index$doc_id
  if(sum(duplicate) > 0){
    if(if_duplicates == 'stop') stop('DUPLICATES. The new tcorpus contains doc_ids that are already in the shattered_tCorpus. If you know why, you can use the if_duplicates parameter to "skip" the duplicates or automatically "rename" them')
    if(if_duplicates == 'skip') {
      cat('## Skipping ', sum(duplicate), ' duplicate(s)\n')
      tc = subset(tc, subset_meta = !duplicate)
    }
    if(if_duplicates == 'rename') {
      cat('## Renaming ', sum(duplicate), ' duplicate(s)\n')
      docnames = tc@meta$doc_id
      i = 0
      while(sum(duplicate) > 0){
        i = i + 1
        oldname = docnames[duplicate]
        newname = sprintf('%s_D%s', oldname, i)

        levels(tc@data$doc_id)[match(tc@meta$doc_id[duplicate], levels(tc@data$doc_id))] = newname
        tc@meta$doc_id[duplicate] = newname

        duplicate = tc@meta$doc_id %in% index$doc_id | duplicated(tc@meta$doc_id) ## also check whether there are duplicates within tc after changing names (very unlikely, but still)
      }
    }
  }
  tc
}

ntokens_index <- function(tc){
  data = get_data(tc)
  freq = data[,.N,by='doc_id']
  meta_id = get_meta_column(tc, 'doc_id')
  freq$N[match(meta_id, freq$doc_id)]
}

fit_columns_to_index <- function(tc, stc){
  info = get_info(stc)
  name = get_path(stc)

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

reindex_features <- function(tc, stc){
  feature_levels = get_feature_levels(stc)
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

shatter_loop <- function(tc, meta_columns=c(), tokens_per_shard=1000000, n_shards=NA, save_path='', compress=TRUE, verbose=F, shard_index=rep(NA, n_meta(tc)), verbose_string='') {
  if(!dir.exists(save_path)) dir.create(save_path)

  if(length(meta_columns) == 0){
    shard_index = save_shards(tc, tokens_per_shard=tokens_per_shard, n_shards=n_shards, save_path=save_path, compress=compress, verbose=verbose, verbose_string=verbose_string)
  } else {
    column = meta_columns[1]
    next_columns = meta_columns[!meta_columns == column]
    if(verbose) cat(verbose_string, column, '\n')
    verbose_string = paste(verbose_string, '---', sep='')

    val = as.character(get_meta_column(tc, column))
    for(uval in unique(val)){
      if(verbose) cat(verbose_string, uval, '\n')
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


save_shards <- function(tc, tokens_per_shard=1000000, n_shards=NA, save_path='', compress=TRUE, verbose=F, verbose_string){
  existing_shards = list.files(save_path)
  n_existing_shards = sum(grepl('shard_[0-9]+_T=', existing_shards))

  n = n_meta(tc)
  if(is.na(n_shards)) n_shards = ceiling(n_data(tc) / tokens_per_shard)
  batch_i = data_batch(tc, 'document', n.batches=n_shards, for_meta = T)

  shard_index = rep(NA, n) ## using the original n, before deleting duplicates. Then use !duplicates to only provide the index for the non-duplicate indices
  if(is.null(batch_i)) return(shard_index) ## if there are no batches, stop

  for(i in 1:nrow(batch_i)){
    meta_i = (batch_i$start[i]):(batch_i$end[i])
    shard = subset(tc, subset_meta = meta_i)
    set_keys(shard)
    if(n_meta(shard) > 0){
      fname = sprintf('shard_%s_T=%s_M=%s.rds', i+n_existing_shards, n_data(shard), n_meta(shard))
      if(verbose) cat(verbose_string, fname, '\n')
      fpath = sprintf('%s/%s', save_path, fname)
      shard_index[meta_i] = fpath
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
#' @param stc A shattered_tCorpus object
#' @param tokens_per_shard The number of tokens per shard. This is an approximation, since a tCorpus should not be broken within documents. Also, given the number of shards that is required, tokens will be evently distributed (for instance, if tokens_per_shard is 1,000,000, and there are 1,500,000 tokens, you'll get 2 tokens of 750,000 shards)
#'
#' @export
redistribute_shards <- function(stc, tokens_per_shard=100000) {
  shard_index = get_index(stc)
  shard_index$.I  = 1:nrow(shard_index) ## keep order to replace .SHARD and return the shard_index (instead of rbinding it)
  new_shard_index = rep(NA, nrow(shard_index))
  old_shard_files = c()
  new_shard_files = c()

  verbose = verbose_sum_counter(n=get_info(stc, 'n'))
  for(path in unique(shard_index$.PATH)){
    index = shard_index[path]
    newshards = equal_groups(index, tokens_per_shard)
    for(shard_i in unique(newshards)){
      i = shard_i == newshards
      shard = collect_shards(unique(index$.SHARD[i]), doc_ids = index$doc_id[i])
      shard_name = sprintf('shard_%s_T=%s_M=%s.rds', shard_i, n_data(shard), n_meta(shard))
      verbose(n_data(shard))

      new_shard_file = sprintf('%s/NEW_%s', path, shard_name)
      new_shard_files = union(new_shard_files, new_shard_file)
      saveRDS(shard, new_shard_file) ## add NEW_ to name -> delete old shards -> delete random string from new name
      new_shard_index[index$.I][i] = sprintf('%s/%s', path, shard_name)
    }
    old_shard_files = union(old_shard_files, unique(index$.SHARD))
  }

  ## remove old shards, rename new shards and renew the shard_index and info
  ## (do this only as the last step, so that if stuff crashes, the old shards and index stay intact. TODO: some other function (refresh_index?) should check for and delete lingering NEW_ files)
  removed = file.remove(old_shard_files)
  for(ns in new_shard_files) file.rename(ns, gsub('NEW_(shard_[0-9]+_T=)', '\\1', ns))

  shard_index$.I = NULL
  shard_index$.SHARD = new_shard_index
  setkeyv(shard_index, c('.PATH','.SHARD'))
  saveRDS(shard_index, sprintf('%s/index.rds', get_path(stc)))

  info = get_info(stc)
  info$n_shards = length(unique(shard_index$.SHARD))
  saveRDS(info, sprintf('%s/info.rds', get_path(stc)))

  inspect_shard_index(stc)
  stc
}


#' Change the way in which
#'
#' Note that depending on the size of the shattered_tCorpus this can take quite a while.
#'
#' @param stc A shattered_tCorpus object
#' @param new_stc Either a shattered_tCorpus object or a character string giving the name (or path) for a new shattered_tCorpus.
#' @param meta_columns
#' @param tokens_per_shard The number of tokens per shard. This is an approximation, since a tCorpus should not be broken within documents. Also, given the number of shards that is required, tokens will be evently distributed (for instance, if tokens_per_shard is 1,000,000, and there are 1,500,000 tokens, you'll get 2 tokens of 750,000 shards)
#' @param compress
#'
#' @return
#' @export
#'
#' @examples
reindex_shards <- function(stc, new_stc=stc, meta_columns=NULL, tokens_per_shard=1000000, compress=TRUE){
  if(!is.null(meta_columns)){
    mcols = colnames(get_info(stc)$meta_head)
    meta_columns = match.arg(meta_columns, mcols, several.ok = T)
  } else meta_columns = c()

  shards = get_shards(stc)

  if(is(new_stc, 'character')) {
    if(!grepl('.tCorpus$', new_stc)) new_stc = paste(new_stc, 'tCorpus', sep='.')
    new_stc = shattered_tCorpus(path=new_stc)
  }

  path_is_same = get_path(stc, normalize=TRUE) == get_path(new_stc, normalize=TRUE)
  path_exists = file.exists(get_path(new_stc, normalize=TRUE))
  if(!path_is_same & path_exists) stop(sprintf("The path in new_stc (%s) already exists, and is not the same path as the given stc (in which case it will be overwritten). Solve by a) using a different new_stc, b) overwriting the old stc (by also passing the stc to new_stc) or c) deleting the path in new_stc", get_path(new_stc)))

  tmp_path = sprintf('%s_INCOMPLETE%s', get_path(new_stc), sample(1:10000000, 1))
  verbose = verbose_sum_counter(n=get_info(stc, 'n'))
  for(shard in shards){
    tc = readRDS(shard)
    verbose(n_data(tc))
    new_stc = shatter_tcorpus(tc, stc = tmp_path, if_exists = 'append', meta_columns=meta_columns, tokens_per_shard = tokens_per_shard, compress=compress)
  }

  if(path_is_same) {
    ## if the new path is the same as the old path, remove the old path
    unlink(get_path(stc), recursive = TRUE)
  }
  file.rename(tmp_path, get_path(new_stc))

  set_info(stc, '')
  get_info(stc)


  new_stc
}



###### debugging and cleanup #######
## if code is interrupted there can be lingering shards in the .tCorpus folder that are not in the index
## these shards are ignored and do no harm, but it is good manners to delete them
## also, for debugging it's nice to detect when stuff goes wrong here.

inspect_shard_index <- function(stc){
  index_shards = get_shards(stc, normalize = T)
  existing_shards = list.files(sprintf('%s/shards', get_path(stc)), recursive = T, full.names = T)
  existing_shards = normalizePath(existing_shards)

  missing = setdiff(index_shards, existing_shards)
  unindexed = setdiff(existing_shards, index_shards)

  if(length(missing > 0)) stop(sprintf('MISSING SHARDS. %s out of %s of the shard files in the index do not exist. Unless you have been working behind the scenes within the slots and/or shard files (not judging, just saying), this is a bug (and should kindly be reported as such)', length(missing), length(index_shards)))
  if(length(unindexed > 0)) {
    warning(sprintf('DELETED LINGERING SHARDS. There were %s files within the shards folder (%s/shards) that are not in the index (probably due to interupted code in creating or changing the tCorpus). These have been deleted', length(unindexed), get_path(stc)))
    file.remove(unindexed)
  }
}
