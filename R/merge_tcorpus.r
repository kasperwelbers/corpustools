#' Merge tCorpus objects
#'
#' @param ... either a named list or named arguments with tCorpus objects
#' @param keep_data if 'intersect', then only the token data columns that occur in all tCorpurs objects are kept
#' @param keep_meta if 'intersect', then only the document meta columns that occur in all tCorpurs objects are kept
#'
#' @return a tCorpus object
#' @export
merge_tcorpora <- function(..., keep_data=c('intersect', 'all'), keep_meta=c('intersect', 'all')){
  keep_data = match.arg(keep_data)
  keep_meta = match.arg(keep_meta)

  tc_list = list(...)
  if (length(tc_list) == 1 & class(tc_list[[1]]) == 'list') tc_list = tc_list[[1]]
  for(i in 1:length(tc_list)) if (!is(tc_list[[i]], 'tCorpus')) stop(sprintf('%s is not a tCorpus object', names(tc_list)[i]))

  if (keep_data == 'intersect') {
    cnames = lapply(tc_list, function(x) colnames(x$data()))
    cnames = Reduce(intersect, cnames)
    data = plyr::ldply(lapply(tc_list, function(x) x$data(), columns=cnames), .id = NULL)
  } else {
    data = plyr::ldply(lapply(tc_list, function(x) x$data()), .id = NULL)
  }

  if (keep_meta == 'intersect') {
    cnames = lapply(tc_list, function(x) colnames(get_meta(x)))
    cnames = Reduce(intersect, cnames)
    meta = plyr::ldply(lapply(tc_list, get_meta, columns=cnames), .id = 'subcorpus')
  } else {
    meta = plyr::ldply(lapply(tc_list, get_meta), .id = 'subcorpus')
  }

  if ('sent_i' %in% colnames(data)){
    if (any(is.na(data$sent_i))) {
      warning('sent_i contains NAs after merging, and is therefore deleted')
      data$sent_i = NULL
      sent_col = NULL
    } else {
      sent_col = 'sent_i'
    }
  } else {
    sent_col = NULL
  }

  tokens_to_tcorpus(data, doc_col='doc_id', sent_i_col=sent_col, word_i_col='word_i', meta=meta, sent_is_local = T, word_is_local = T)
}

#' Merge tCorpus shards
#'
#' Similar to merge_tcorpora, but assumes that the tCorpora are identical in structure (same columns in the same order and same provenance)
#'
#' @param shards a named list or named arguments with tCorpus objects
#'
#' @return a tCorpus object
#' @export
merge_shards <- function(shards){
  tc = tCorpus$new(data=rbindlist(lapply(shards, function(x) x$data())),
                   meta=rbindlist(lapply(shards, function(x) x$meta())),
                   p = shards[[1]]$provenance())
  tc$set_keys()
  tc
}

