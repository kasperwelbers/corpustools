################## create collocations

################## collocation labels
#' Features collapsed into collocation strings
#'
#' Some features can be grouped in categories that span over multiple rows. For instance, unique ids for named entities. This function groups these collocation categories, and collapses the features into collocation strings.
#' This can be used to count how often each collocation string occurs. Also, it is used in the add_collocation_label function to choosse labels for collocation categories based on the most frequent occurring string
#'
#' @param tc
#' @param colloc_id
#' @param feature
#'
#' @export
collocation_strings <- function(tc, colloc_id, feature='word'){
  f = get_data(tc, columns = c(feature, colloc_id))
  colnames(f) = c('feature','id')

  lag_id = shift(f$id, fill=as.factor(NA))
  f$new_id = !f$id == lag_id | (!is.na(f$id) & is.na(lag_id))
  f = f[!is.na(f$id),]
  f$new_id = cumsum(f$new_id)

  label = f[, list(label=paste(get('feature'), collapse=' '),
                   id = unique(get('id'))), by= 'new_id']
  as.data.frame(label[, list(N = .N), by= 'label,id'])
}

#' Choose and add collocation strigns based on collocation categories
#'
#' Given a collocation category (e.g., named entity ids), this function finds the most frequently occuring string in this category and adds it as a label for the category
#'
#' @param tc a tcorpus object
#' @param colloc_id the data column containing the unique id for collocation words
#' @param feature
#' @param new_feature
#'
#' @export
add_collocation_label <- function(tc, colloc_id, feature='word', new_feature=sprintf('%s_l', colloc_id)){
  label = collocation_strings(tc, colloc_id, feature=feature) ## for shattered_tCorpus, this has to be done for the entire corpus first, or labels will not match across shards

  ## select most frequent labels
  label = label[order(-label$N),]
  label = label[!duplicated(label$id),]

  label$label = as.factor(label$label)
  levels(label$label) = gsub('_', ' ', levels(label$label), fixed=T)
  set_column(tc, new_feature, value = label$label[match(get_column(tc, colloc_id), label$id)])
}


################## undo collocations

flatten_collocations <- function(d, feature_col, position_col, sep='_', reset_key=T){
  ## position needs to be an integer (or at least shouldn not have decimals)
  if(reset_key) k = key(d)
  fc = flatten_collocations_table(d[[feature_col]], d[[position_col]])

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

  if(reset_key) setkeyv(d, k)
  d
}

flatten_collocations_table <- function(feature, position, i=1:length(feature), sep=' |_'){
  colloc = grep('_', feature)
  colloc_list = stringr::str_split(feature[colloc], '_')
  colloc = data.frame(i=i[colloc], feature=feature[colloc], position=position[colloc], n=sapply(colloc_list, length))
  flat_colloc = colloc[rep(1:nrow(colloc), colloc$n),]
  flat_colloc$nr = unlist(sapply(colloc$n, function(x) seq(1,x), simplify=F))
  flat_colloc$feature = unlist(colloc_list)
  flat_colloc$position = flat_colloc$position + (flat_colloc$nr-1) / flat_colloc$n
  flat_colloc
}

