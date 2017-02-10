
## this is just for development.

sourceall <- function(){
  for(f in list.files('R', full.names = T)) source(f)
}

demo_tc <- function(){
  data(sotu_texts)
  create_tcorpus(sotu_texts, 'text', doc_column = 'id')
}

demo_stc <- function(){
  tc = demo_tc()
  shatter_tcorpus(tc, 'test', meta_columns=c('party', 'president'), tokens_per_shard=10000, if_exists = 'overwrite')
}

#' @export
testhelp <- function(clear=F){
  if(clear) rm(list = ls())
  sourceall()
  attach(list(tc=demo_tc(), stc=demo_stc()))
}
