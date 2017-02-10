## Several openly available resources for coding text features work with long lists of exact strings, and often multiword strings
## for example, multilingual named entities: http://optima.jrc.it/data/entities.gzip
## The following functions enable the use of these lists

#' Use a language resource to create a feature
#'
#' Currently available resources:
#' \itemize{
#'    \item{JRC-Names: "JRC-Names is a highly multilingual named entity resource for person and organisation names. [...] JRC-Names is a by-product of the analysis of about 220,000 news reports per day by the Europe Media Monitor (EMM) family of applications." (https://ec.europa.eu/jrc/en/language-technologies/jrc-names)}
#' }
#'
#' ABOUT THE RESOURCE DATA:
#' The resource file is downloaded and saved as a temporary file, which will be used if this function is again called within the same R session.
#' Alternatively, the file can be stored locally by specifying the path. This is a kind thing to do if you mean to use this resource regularly (and do not need it to be up to date).
#'
#' @param tc a tCorpus object
#' @param resource A character string indicating which resource to use.
#' @param new_feature The name of the new feature. Default is the name of the resource
#' @param feature The feature to be used as input. Note that most resources assume that the input consists of unprocessed words
#' @param save_path The path (without the filename) where the resource is stored. Default is a temporary directory, that is cleaned up when the R session ends.
#' @param multiword_labels if True, then for resources that create an id for subsequent words (e.g., named entities), a label column is added based on the most frequently occuring multiword combination in the data.
#' @param force_download If TRUE, the resource will be downloaded even if it is stored locally.
#'
#' @export
feature_from_resource <- function(tc, resource='jrc_names', new_feature=resource, feature='word', save_path=tempdir(), force_download=F, multiword_labels=T, verbose=T){
  resource = match.arg(resource)

  re_data = download_resource(resource, save_path=save_path, force_download=force_download)
  if(resource == 'jrc_names') index = use_lookup_resource(tc, re_data, regex_sep = '\\+', case_sensitive = T, verbose=verbose)

  index$id = as.factor(index$id)
  f = factor(rep(NA, n_data(tc)), 1:length(levels(index$id)), levels(index$id))
  f[index$i] = index$id
  tc = set_column(tc, new_feature, f)

  if(multiword_labels) tc = add_multiword_label(tc, new_feature, feature=feature)
  tc
}

#' Features collapsed into multiword strings
#'
#' Some features can be grouped in categories that span over multiple rows. For instance, unique ids for named entities. This function groups these multiword categories, and collapses the features into multiword strings.
#' This can be used to count how often each multiword string occurs. Also, it is used in the add_multiword_label function to choosse labels for multiword categories based on the most frequent occurring string
#'
#' @param tc
#' @param mword_id
#' @param feature
#'
#' @export
multiword_strings <- function(tc, mword_id, feature='word'){
  f = get_data(tc, columns = c(feature, mword_id))
  colnames(f) = c('feature','id')

  lag_id = shift(f$id, fill=as.factor(NA))
  f$new_id = !f$id == lag_id | (!is.na(f$id) & is.na(lag_id))
  f = f[!is.na(f$id),]
  f$new_id = cumsum(f$new_id)

  label = f[, list(label=paste(get('feature'), collapse=' '),
                   id = unique(get('id'))), by= 'new_id']
  as.data.frame(label[, list(N = .N), by= 'label,id'])
}

#' Choose and add multiword strigns based on multiword categories
#'
#' Given a multiword category (e.g., named entity ids), this function finds the most frequently occuring string in this category and adds it as a label for the category
#'
#' @param tc
#' @param mword_id
#' @param feature
#' @param new_feature
#'
#' @export
add_multiword_label <- function(tc, mword_id, feature='word', new_feature=sprintf('%s_l', mword_id)){
  label = multiword_labels(tc, mword_id, feature=feature) ## for shattered_tCorpus, this has to be done for the entire corpus first, or labels will not match across shards

  ## select most frequent labels
  label = label[order(-label$N),]
  label = label[!duplicated(label$id),]

  label$label = as.factor(label$label)
  set_column(tc, new_feature, value = label$label[match(get_column(tc, mword_id), label$id)])
}


#' Download lookup lists from various resources
#'
#' Currently available resources:
#' \itemize{
#'    \item{JRC-Names: "JRC-Names is a highly multilingual named entity resource for person and organisation names. [...] JRC-Names is a by-product of the analysis of about 220,000 news reports per day by the Europe Media Monitor (EMM) family of applications." (https://ec.europa.eu/jrc/en/language-technologies/jrc-names)}
#' }
#'
#' The resource file is downloaded and saved as a temporary file, which will be used if this function is again called within the same R session.
#' Alternatively, the file can be stored locally by specifying the path. This is a kind thing to do if you mean to use this resource regularly (and do not need it to be up to date).
#'
#' @param resource A character string indicating which resource to download
#' @param save_path The path (without the filename) where the resource is stored. Default is a temporary directory, that is cleaned up when the R session ends.
#' @param force_download If TRUE, the resource will be downloaded even if it is stored locally.
#'
#' @export
download_resource <- function(resource=c('jrc_names'), save_path=tempdir(), force_download=F){
  resource = match.arg(resource)
  if(resource == 'jrc_names') return(download_jrc_names(path=save_path, force_download=force_download))
}

download_jrc_names <- function(path=tempdir(), force_download=F){
  fname = sprintf('%s/jrc_entities.rds', path)
  if(file.exists(fname) & !force_download){
    entities = readRDS(fname)
  } else {
    temp <- tempfile()
    message('This package only links to the JRC-Names resource, and the authors have no affiliation to it. Please consult the JRC website for additional information and the usage conditions: \nhttps://ec.europa.eu/jrc/en/language-technologies/jrc-names\n')
    download.file("http://optima.jrc.it/data/entities.gzip",temp)
    entities = readLines(temp)
    entities = stringr::str_split(entities, '\t', simplify = T)
    entities = data.frame(string=as.character(entities[,4]), id=as.numeric(entities[,1]), language=as.factor(entities[,3]), stringsAsFactors = F)

    saveRDS(entities, sprintf('%s/jrc_entities.rds', path))
    unlink(temp)
  }

  entities
}

use_lookup_resource <- function(tc, re, regex_sep, case_sensitive, batchsize=50000, verbose=F){
  fi = get_feature_index(tc)

  re = re[sample(1:nrow(re)),]
  batches = get_batch_i(nrow(re), batchsize = batchsize)
  candidates = vector('list', nrow(batches))
  counter = verbose_counter(nrow(batches), ticks = 1)
  for(i in 1:nrow(batches)){
    batch = batches[i,]
    candidates[[i]] = fast_multiword_lookup(re[batch$start:batch$end,], fi=fi, regex_sep=regex_sep, case_sensitive=case_sensitive)
    if(verbose) counter()
  }
  candidates = rbindlist(candidates)

  ## at some point this could be done better. Perhaps even taking context into account. BUT NOT TODAY!!
  candidates = candidates[order(-candidates$nterms),] ## first go for candidates with the most terms (likely to be most specific)
  candidates = unique(candidates, by = c('global_i'))

  index_i = rep(1:nrow(candidates), times=candidates$nterms)
  index = candidates[index_i, c('global_i','id', 'i')]

  add_to_i = local_position(1:nrow(index), index$global_i, presorted = T) - 1
  index$i = index$i + add_to_i ## code the [nterms] words from the start

  index
}

global_feature_vector <- function(fi){
  global_f = rep(as.factor(''), nrow(fi))
  levels(global_f) = c('', levels(fi$feature))
  global_f[fi$global_i] = fi$feature
  global_f
}

fast_multiword_lookup <- function(re, fi, regex_sep, case_sensitive=T){
  i = 1:length(re$string)

  if(!case_sensitive) {
    string = tolower(re$string)
    levels(fi$feature) = tolower(levels(fi$feature))
  }

  sn = stringr::str_split(re$string, regex_sep)
  nterms = sapply(sn, length)
  candidates = vector('list', max(nterms))

  lt = data.table(feature = sapply(sn, first), id=re$id, nterms=nterms, s_i=i, key='feature')
  setkey(fi, 'feature') ## somehow it drops the key within ddply?
  lt = merge(fi, lt, by=.EACHI, allow.cartesian=T)

  is_end = lt$nterms == 1
  candidates[[1]] = lt[is_end, c('i','global_i','id','nterms'), with=F]
  lt = lt[!is_end,]

  gf = global_feature_vector(fi)
  for(i in 1:length(candidates)){
    if(nrow(lt) == 0) break
    hit = sapply(sn[lt$s_i], function(x) x[i+1]) == gf[lt$global_i + i]
    hit[is.na(hit)] = F
    lt = lt[hit,]

    is_end = lt$nterms == i+1
    candidates[[i+1]] = lt[is_end,c('i','global_i','id','nterms'), with=F]
    lt = lt[!is_end,]
  }
  candidates = rbindlist(candidates)
  candidates = candidates[order(-candidates$nterms),] ## first go for candidates with the most terms (likely to be most specific)
  unique(candidates, by = c('global_i'))
}
