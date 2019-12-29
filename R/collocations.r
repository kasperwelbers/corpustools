################## create collocations

################## collocation labels

# Features collapsed into collocation strings
#
# Some features can be grouped in categories that span over multiple rows. For instance, unique ids for named entities. This function groups these collocation categories, and collapses the features into collocation strings.
# This can be used to count how often each collocation string occurs. Also, it is used in the add_collocation_label function to choosse labels for collocation categories based on the most frequent occurring string



collocation_strings <- function(tc, colloc_id, feature='token', pref=NULL){
  f = tc$get(c(feature, colloc_id))
  colnames(f) = c('feature','id')
  f$pref = F
  f$pref[pref] = T

  lag_id = data.table::shift(f$id, fill=as.factor(NA))
  f$new_id = !f$id == lag_id | (!is.na(f$id) & is.na(lag_id))
  f = f[!is.na(f$id),]
  f$new_id = cumsum(f$new_id)
  pref_ids = unique(f$id[f$pref])
  f = f[f$pref | !f$id %in% pref_ids,]

  label = unique(f[,c('id','pref','new_id')], with=F)
  feature = split(as.character(f$feature), f$new_id)
  label$label = sapply(feature, stringi::stri_flatten, collapse=' ')

  label[, list(N = data.table::.N, label=unique(get('label'))), by= 'pref,id,label']
}

#' Choose and add collocation strings based on collocation categories
#'
#' Given a collocation category (e.g., named entity ids), this function finds the most frequently occuring string in this category and adds it as a label for the category
#'
#' @param tc a tcorpus object
#' @param colloc_id the data column containing the unique id for collocation tokens
#' @param feature the name of the feature column
#' @param new_feature the name of the new feature column
#' @param pref_subset Optionally, a subset call, to specify a subset that has priority for finding the most frequently occuring string
add_collocation_label <- function(tc, colloc_id, feature='token', new_feature=sprintf('%s_l', colloc_id), pref_subset=NULL){
  .pref_subset = deparse(substitute(pref_subset))
  if (!pref_subset == 'NULL') pref = tc$get_token_id(subset_meta=.pref_subset) else pref = NULL

  label = collocation_strings(tc, colloc_id, feature=feature, pref=pref)
  ## select most frequent labels, prioritzing pref is true
  setkeyv(label, c('pref','N'))
  label = as.data.frame(label)[!duplicated(label$id, fromLast = T),]

  label$label = as.factor(label$label)
  levels(label$label) = gsub('_', ' ', levels(label$label), fixed=T)

  .colloc_id = colloc_id
  tc$set(new_feature, value = label$label[match(tc$get(.colloc_id), label$id)])
}


################## undo collocations

flatten_collocations <- function(d, feature_col, position_col, sep=' |_', reset_key=T){
  ## position needs to be an integer (or at least shouldn not have decimals)
  if (reset_key) k = key(d)
  fc = flatten_collocations_table(d[[feature_col]], d[[position_col]], sep = sep)

  new_levels = setdiff(unique(fc$feature), levels(d))
  levels(d[[feature_col]]) = c(levels(d[[feature_col]]), new_levels)

  replace_row = fc[fc$nr == 1,]
  d[[feature_col]][replace_row$i] = replace_row$feature

  add_row = fc[fc$nr > 1,]
  new_row = d[add_row$i,]
  new_row[[feature_col]] = add_row$feature
  new_row[[position_col]] = add_row$position

  d = rbindlist(list(d, new_row))

  ## reindex position
  d = d[order(d[[position_col]]),]
  int = floor(d[[position_col]])
  is_inserted = !int == d[[position_col]]
  d[[position_col]] = int + cumsum(is_inserted)

  if (reset_key) setkeyv(d, k)
  d
}

flatten_collocations_table <- function(feature, position, i=1:length(feature), sep=' |_'){
  colloc = grep(sep, feature)
  colloc_list = stringi::stri_split(feature[colloc], regex = sep)
  colloc = data.frame(i=i[colloc], feature=feature[colloc], position=position[colloc], n=sapply(colloc_list, length))
  flat_colloc = colloc[rep(1:nrow(colloc), colloc$n),]
  flat_colloc$nr = unlist(sapply(colloc$n, function(x) seq(1,x), simplify=F))
  flat_colloc$feature = unlist(colloc_list)
  flat_colloc$position = flat_colloc$position + (flat_colloc$nr-1) / flat_colloc$n
  flat_colloc
}

