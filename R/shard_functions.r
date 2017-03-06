## The logic behind the shard_functions is that within the normal tCorpus function there is a line
## that states: if the given tCorpus is a shattered_tCorpus, then go to the shard version of the function.
## naturally, this only works for functions where a sharded approach is possible (or at least currently implemented)
## so functions that are not possible


## parse_mcall approach does not always work because fuck you match.call() (it won't work when arguments are not explicitly given, e.g. when using mapply)
## perhaps this isn't worth solving, since mapplying over a shattered tcorpus would be a very rare thing to do
parse_mcall <- function(mcall){
  mcall = as.list(mcall)
  fun = as.character(mcall[[1]])
  args = mcall[-1][-1] ## remove the function name, and remove the first argument (the tCorpus)
  list(fun=fun, args=args)
}

shardloop_transform <- function(stc, mcall, verbose){
  ## for any function that only transforms the tCorpus without returning any values
  mcall = parse_mcall(mcall)
  counter = verbose_sum_counter(n = stc$info()$n)
  for(shard in stc$shards()){
    tc = readRDS(shard)
    if (verbose) counter(shard$n)
    tc = do.call(mcall$fun, c(tc=tc, mcall$args))
    saveRDS(tc, shard)
  }
}

shardloop_rbind <- function(stc, mcall, verbose){
  ## for any function of which the results can be row binded
  mcall = parse_mcall(mcall)
  shards = stc$shards()
  r = vector('list', length(shards))
  counter = verbose_sum_counter(n = stc$info()$n)
  for(i in seq_along(shards)){
    tc = readRDS(shards[i])
    if (verbose) counter(tc$n)
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

