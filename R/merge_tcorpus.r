#' Merge tCorpus objects
#'
#' Create one tcorpus based on multiple tcorpus objects
#'
#' @param ... tCorpus objects, or a list with tcorpus objects
#' @param keep_data if 'intersect', then only the token data columns that occur in all tCorpurs objects are kept
#' @param keep_meta if 'intersect', then only the document meta columns that occur in all tCorpurs objects are kept
#' @param if_duplicate determine behaviour if there are duplicate doc_ids across tcorpora. By default, this yields an error, but you can set it to "rename" to change the names of duplicates (which makes sense of only the doc_ids are duplicate, but not the actual content), or "drop" to ignore duplicates, keeping only the first unique occurence.
#' @param duplicate_tag a character string. if if_duplicates is "rename", this tag is added to the document id. (this is repeated till no duplicates remain)
#'
#' @return a tCorpus object
#' @examples
#' tc1 = create_tcorpus(sotu_texts[1:10,], doc_column = 'id')
#' tc2 = create_tcorpus(sotu_texts[11:20,], doc_column = 'id')
#' tc = merge_tcorpora(tc1, tc2)
#' tc$n_meta
#'
#' #### duplicate handling ####
#' tc1 = create_tcorpus(sotu_texts[1:10,], doc_column = 'id')
#' tc2 = create_tcorpus(sotu_texts[6:15,], doc_column = 'id')
#'
#' ## duplicate error
#' \donttest{tc = merge_tcorpora(tc1,tc2)}
#'
#' ## with "rename", has 20 documents of which 5 duplicates
#' tc = merge_tcorpora(tc1,tc2, if_duplicate = 'rename')
#' tc$n_meta
#' sum(grepl('#D', tc$meta$doc_id))
#'
#' ## with "drop", has 15 documents without duplicates
#' tc = merge_tcorpora(tc1,tc2, if_duplicate = 'drop')
#' tc$n_meta
#' mean(grepl('#D', tc$meta$doc_id))
#' @export
merge_tcorpora <- function(..., keep_data=c('intersect', 'all'), keep_meta=c('intersect', 'all'), if_duplicate = c('stop','rename','drop'), duplicate_tag='#D'){
  keep_data = match.arg(keep_data)
  keep_meta = match.arg(keep_meta)
  if_duplicate = match.arg(if_duplicate)

  tc_list = list(...)
  if (length(tc_list) == 1 && class(tc_list[[1]])[1] == 'list') tc_list = tc_list[[1]]
  for(i in 1:length(tc_list)) if (!methods::is(tc_list[[i]], 'tCorpus')) stop(sprintf('%s is not a tCorpus object', names(tc_list)[i]))

  data = lapply(tc_list, function(x) x$get(copy=F))
  data_names = lapply(tc_list, function(x) x$names)
  meta = lapply(tc_list, function(x) x$get_meta(copy=F))
  meta_names = lapply(tc_list, function(x) x$meta_names)
  rm(tc_list)

  seen_doc_ids = c()
  for (i in 1:length(data)) {
    duplicates = seen_doc_ids[seen_doc_ids %in% meta[[i]]$doc_id]
    while (length(duplicates) > 0) {
      if (if_duplicate == 'stop') stop('Found duplicate doc_ids across tcorpora. Use the if_duplicate argument to specify whether to "rename" duplicate doc_ids, or "drop" them (i.e. only the first occurence is kept)')
      if (if_duplicate == 'rename') {
        data_dup = levels(data[[i]]$doc_id) %in% duplicates
        levels(data[[i]]$doc_id)[data_dup] = stringi::stri_paste(levels(data[[i]]$doc_id)[data_dup], duplicate_tag)
        meta_dup = meta[[i]]$doc_id %in% duplicates
        meta[[i]]$doc_id[meta_dup] = stringi::stri_paste(meta[[i]]$doc_id[meta_dup], duplicate_tag)
        duplicates = seen_doc_ids[seen_doc_ids %in% meta[[i]]$doc_id]
      }
      if (if_duplicate == 'drop') {
        data[[i]] = fsetdiff(data[[i]], data[[i]][list(duplicates),])
        meta[[i]] = fsetdiff(meta[[i]], meta[[i]][list(duplicates),])
        duplicates = c()
      }
    }
    seen_doc_ids = union(seen_doc_ids, meta[[i]]$doc_id)
  }

  data = data.table::rbindlist(data, fill=T)
  if (keep_data == 'intersect') {
    data_names = Reduce(intersect, data_names)
    data = subset(data, select = data_names)
  }

  meta = data.table::rbindlist(meta, fill=T)
  if (keep_meta == 'intersect') {
    meta_names = Reduce(intersect, meta_names)
    meta = subset(meta, select = meta_names)
  }

  if ('sentence' %in% colnames(data)){
    if (any(is.na(data$sentence))) {
      warning('sentence contains NAs after merging, and is therefore deleted')
      data$sentence = NULL
      sent_col = NULL
    } else {
      sent_col = 'sentence'
    }
  } else {
    sent_col = NULL
  }

  tokens_to_tcorpus(data, doc_col='doc_id', sentence_col=sent_col, token_id_col='token_id', meta=meta, sent_is_local = T, token_is_local = T)
}
