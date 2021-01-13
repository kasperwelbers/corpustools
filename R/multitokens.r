################## Flatten terms

# Some features can be grouped in categories that span over multiple rows. For instance, unique ids for named entities. This function groups these multitoken categories, and collapses the features into multitoken strings.
# This can be used to count how often each multitoken string occurs. Also, it is used in the add_multitoken_label function to choosse labels for multitoken categories based on the most frequent occurring string


multitoken_strings <- function(tc, colloc_id, feature='token', pref=NULL){
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

#' Choose and add multitoken strings based on multitoken categories
#'
#' Given a multitoken category (e.g., named entity ids), this function finds the most frequently occuring string in this category and adds it as a label for the category
#'
#' @param tc a tcorpus object
#' @param colloc_id the data column containing the unique id for multitoken tokens
#' @param feature the name of the feature column
#' @param new_feature the name of the new feature column
#' @param pref_subset Optionally, a subset call, to specify a subset that has priority for finding the most frequently occuring string
add_multitoken_label <- function(tc, colloc_id, feature='token', new_feature=sprintf('%s_l', colloc_id), pref_subset=NULL){
  .pref_subset = deparse(substitute(pref_subset))
  if (!pref_subset == 'NULL') pref = tc$get_token_id(subset_meta=.pref_subset) else pref = NULL

  label = multitoken_strings(tc, colloc_id, feature=feature, pref=pref)
  ## select most frequent labels, prioritzing pref is true
  setkeyv(label, c('pref','N'))
  label = as.data.frame(label)[!duplicated(label$id, fromLast = T),]

  label$label = as.factor(label$label)
  levels(label$label) = gsub('_', ' ', levels(label$label), fixed=T)

  .colloc_id = colloc_id
  tc$set(new_feature, value = label$label[match(tc$get(.colloc_id), label$id)])
}


################## undo multitokens

standardize_dict_term_spacing <- function(d, wildcards=T) {
  d$string = gsub('_', ' ', d$string)
  d$string = sapply(split_tokens(d$string, Inf, T), paste, collapse=' ')
  if (wildcards) d$string = gsub(' ?([?*]) ?', '\\1', d$string)
  d$string = gsub(' +', ' ', d$string)
  d
}

flatten_terms <- function(d, feature_col, position_col, sep=' |_', reset_key=T){
  ## position needs to be an integer (or at least shouldn not have decimals)
  if (reset_key) k = key(d)
  fc = flatten_terms_table(d[[feature_col]], d[[position_col]], sep = sep)

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
  d$orig_i = int

  if (reset_key) setkeyv(d, k)
  d
}

is_splittable <- function(feature) {
  uf = levels(feature)
  uf = gsub('_', ' ', uf)
  st = split_tokens(uf, NULL, F)
  level_splittable = sapply(st, length) > 1
  out = level_splittable[as.numeric(feature)]
  out[is.na(out)] = F
  out
}

flatten_terms_table <- function(feature, position, i=1:length(feature), sep=' |_'){
  levels(feature) = gsub('_',' ',levels(feature), fixed=T)
  terms = is_splittable(feature)
  terms_list = split_tokens(feature[terms], NULL, F)
  terms = data.frame(i=i[terms], feature=feature[terms], position=position[terms], n=sapply(terms_list, length))
  flat_terms = terms[rep(1:nrow(terms), terms$n),]
  flat_terms$nr = unlist(sapply(terms$n, function(x) seq(1,x), simplify=F))
  flat_terms$feature = unlist(terms_list)
  flat_terms$position = flat_terms$position + (flat_terms$nr-1) / flat_terms$n
  flat_terms
}

