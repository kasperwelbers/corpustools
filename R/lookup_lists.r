## Several openly available resources for coding text features work with long lists of exact strings, and often multiword strings
## for example, multilingual named entities: http://optima.jrc.it/data/entities.gzip
## The following functions enable the use of these lists

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
  lt = merge(fi, lt)

  is_end = lt$nterms == 1
  candidates[[1]] = lt[is_end,]
  lt = lt[!is_end,]

  gf = global_feature_vector(fi)
  #start loop
  for(i in 1:length(candidates)){
    if(nrow(lt) == 0) break
    hit = sapply(sn[lt$s_i], function(x) x[i+1]) == gf[lt$global_i + i]
    hit[is.na(hit)] = F
    lt = lt[hit,]

    is_end = lt$nterms == i+1
    candidates[[i+1]] = lt[is_end,]
    lt = lt[!is_end,]
  }
  rbindlist(candidates)
}

use_lookup_resource <- function(tc, re, regex_sep, case_sensitive){
  fi = get_feature_index(tc)

  re$batch = get_batch_i(nrow(re), batchsize = 100000, return_vector = T)
  candidates = plyr::ddply(re, 'batch', fast_multiword_lookup, fi=fi, regex_sep=regex_sep, case_sensitive=case_sensitive, .progress = 'text')

  candidates = candidates[order(-candidates$nterms),] ## first go for candidates with the most terms (likely to be most specific)
  candidates = unique(candidates, by = c('global_i'))

  index_i = rep(1:nrow(candidates), times=candidates$nterms)
  index = candidates[index_i, c('global_i','id', 'i')]

  add_to_global = local_position(1:nrow(index), index$global_i, presorted = T) - 1
  index$global_i = index$global_i + add_to_global ## makes global_i count up till the next global_i
  #index$feature = global_f[index$global_i] For manual check.

  index
}

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
#' @param feature The feature to be used as input. Note that most resources assume that the input consists of unprocessed words
#' @param resource A character string indicating which resource to use.
#' @param save_path The path (without the filename) where the resource is stored. Default is a temporary directory, that is cleaned up when the R session ends.
#' @param force_download If TRUE, the resource will be downloaded even if it is stored locally.
#'
#' @export
feature_from_resource <- function(tc, resource='JRC-Names', new_feature=resource, feature='word', save_path=tempdir(), force_download=F){
  resource = match.arg(resource)

  re_data = download_resource(resource, save_path=save_path, force_download=force_download)
  if(resource == 'JRC-Names') index = use_lookup_resource(tc, re_data, regex_sep = '\\+', case_sensitive = T)

  f = rep('', n_data(tc))
  f[index$i] = as.character(index$id)
  tc = set_column(tc, new_feature, as.factor(f))
  tc
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
download_resource <- function(resource=c('JRC-Names'), save_path=tempdir(), force_download=F){
  resource = match.arg(resource)
  if(resource == 'JRC-Names') return(download_jrc_names(path=save_path, force_download=force_download))
}

function(){
  e = download_resource('JRC-Names')
  tc = feature_from_resource(tc, 'JRC-Names')
  test = get_data(tc)
  test[['JRC-Names']]
  head(e)
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
    entities = data.frame(string=as.character(entities[,4]), id=as.numeric(entities[,1]), stringsAsFactors = F)
    saveRDS(entities,sprintf('%s/jrc_entities.rds', path))
    unlink(temp)
  }

  ## get id labels, focussing on the alphabet labels with the fewest number of words
  non_alpha = !grepl('[a-zA-Z]', entities$string)
  nterms = sapply(stringr::str_split(entities$string, pattern = '\\+'), length)
  labels = entities[order(non_alpha, nterms),]
  labels = labels[!duplicated(labels$id),]
  labels$string = sprintf('%s (%s)', labels$string, labels$id)
  entities$id = factor(entities$id, labels$id, labels$string)

  entities
}


