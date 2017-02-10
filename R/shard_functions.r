## The logic behind the shard_functions is that within the normal tCorpus function there is a line
## that states: if the given tCorpus is a shattered_tCorpus, then go to the shard version of the function.
## naturally, this only works for functions where a sharded approach is possible (or at least currently implemented)
## so functions that are not possible

parse_mcall <- function(mcall, tc){
  mcall = as.list(mcall)
  fun = as.character(mcall[[1]])
  args = mcall[-1][-1] ## remove the function name, and remove the first argument (the tCorpus)
  list(fun=fun, args=args)
}

shardloop_transform <- function(stc, mcall, verbose){
  ## for any function that only transforms the tCorpus without returning any values
  mcall = parse_mcall(mcall)
  counter = verbose_sum_counter(n = get_info(stc)$n)
  for(shard in get_shards(stc)){
    tc = readRDS(shard)
    if(verbose) counter(n_data(shard))
    tc = do.call(mcall$fun, c(tc=tc, mcall$args))
    saveRDS(tc, shard)
  }
}

shardloop_rbind <- function(stc, mcall, verbose){
  ## for any function of which the results can be row binded
  mcall = parse_mcall(mcall)
  shards = get_shards(stc)
  r = vector('list', length(shards))
  counter = verbose_sum_counter(n = get_info(stc)$n)
  for(i in seq_along(shards)){
    tc = readRDS(shards[i])
    if(verbose) counter(n_data(tc))
    r[[i]] = do.call(mcall$fun, c(tc=tc, mcall$args))
  }
  rbindlist(r)
}


shard_search_recode <- function(stc, feature, new_value, keyword, condition=NA, condition_once=F, subset_tokens=NA, subset_meta=NA){
  NULL
}

shard_feature_cooccurrence <- function(tc, feature, matrix_mode=c('dtm', 'windowXwindow', 'positionXwindow'), count_mode=c('normal','dicho','prob'), mat_stats=c('sum.x','sum.y','magnitude.x','magnitude.y', 'nrow'), context_level=c('document','sentence'), direction='<>', window.size=10, n.batches=1, alpha=2){

  ## for summing matrices using the featurelevel i's. return shard matrix to matrix at tcorpus level
  #spMatrix(nrow(m), ncol(featurenames[[feature]]), m@i+1, as.numeric(colnames(m)[m@j+1]), m@x = m@x)

  NULL
}




shard_recode_column <- function(stc, column, new_value, i=NULL, old_value=NULL){
  NULL
}

#shard_set_feature_index <- function(stc, feature='word', context_level=c('document','sentence'), max_window_size=100) {
#  NULL
#}

shard_reset_feature_index <- function(stc) {
  NULL
}

shard_delete_feature_index <- function(stc) {
  NULL
}

shard_get_dtm <- function(stc, feature, context_level=c('document','sentence'), weight=c('termfreq','docfreq','tfidf','norm_tfidf'), drop_empty_terms=T, form=c('Matrix', 'tm_dtm', 'quanteda_dfm')){
  NULL
}

shard_preprocess_feature <- function(stc, column, new_column, language='english', use_stemming=F, lowercase=T, ngrams=1, ngram_context=c('document', 'sentence'), remove_accented=F){
  NULL
}

shard_filter_feature <- function(stc, column, new_column, filter){
  NULL
}


