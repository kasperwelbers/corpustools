get_batch_i <- function(n, n.batches=NA, batchsize=NA, return_list=F, return_vector=F, for_meta=F){
  if (!is.na(batchsize)) n.batches = ceiling(n / batchsize)

  if (!n.batches == 1){
    splits = split((1:n), cut(seq_along(1:n), n.batches, labels = FALSE))
    splits = sapply(splits, FUN=function(x) cbind(min(x), max(x)))
    batch_i = data.frame(start=splits[1,], end=splits[2,])
  } else batch_i = data.frame(start=1, end=n)

  if (return_list | return_vector) batch_i = mapply(seq, from=batch_i$start, to=batch_i$end, SIMPLIFY=F)
  if (return_vector) batch_i = rep(1:length(batch_i), sapply(batch_i, length))
  batch_i
}


data_batch <- function(tc, context_level, n.batches=10, batchsize=NA, return_list=F, return_vector=F, for_meta=F){
  if (for_meta) {
    if (!context_level == 'document') stop('for_meta only possible if context is document')
    con_id = tc$context(context_level, with_labels = T)
    match_id = as.character(tc$get_meta('doc_id'))
  } else {
    con_id = tc$context(context_level, with_labels = F)
    match_id = con_id
  }

  if (!is.na(batchsize)) n.batches = ceiling(length(con_id) / batchsize)

  if (!n.batches == 1){
    break_each = floor(length(con_id) / n.batches)
    break_con = con_id[rep(break_each, n.batches) * 1:n.batches] # do not break within documents
    if (length(break_con) == 0) return(NULL)

    break_i = match(as.character(break_con), match_id)
    break_i = unique(break_i)
    break_i = break_i[break_i > 1 & break_i < length(match_id)]
    batch_i = data.frame(start=c(1,break_i), end=c(break_i-1, length(match_id)))
  } else batch_i = data.frame(start=1, end=length(match_id))

  if (return_list | return_vector) batch_i = mapply(seq, from=batch_i$start, to=batch_i$end, SIMPLIFY=F)
  if (return_vector) batch_i = rep(1:length(batch_i), sapply(batch_i, length))
  batch_i
}

