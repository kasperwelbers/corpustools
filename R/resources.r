## Several openly available resources for coding text features work with long lists of exact strings, and often collocation strings
## for example, multilingual named entities: http://optima.jrc.it/data/entities.gzip
## The following functions enable the use of these lists

#' Use a language resource to create a feature
#'
#' Currently available resources:
#' \itemize{
#'    \item{JRC-Names: "JRC-Names is a highly multilingual named entity resource for person and organisation names. [...] JRC-Names is a by-product of the analysis of about 220,000 news reports per day by the Europe Media Monitor (EMM) family of applications." (https://ec.europa.eu/jrc/en/language-technologies/jrc-names)}
#' }
#'
#' The resource needs to be downloaded first. For this you can use the download_resource function, which will (by default) download the resource into the tcorpus package folder.
#'
#' @param tc a tCorpus object
#' @param resource A character string indicating which resource to use.
#' @param new_feature The name of the new feature. Default is the name of the resource
#' @param feature The feature to be used as input. Note that most resources assume that the input consists of unprocessed words
#' @param resource_path The path (without the filename) where the resource is stored. See ?download_resource for more information.
#' @param collocation_labels if True, then for resources that create an id for subsequent words (e.g., named entities), labels are added (in a separate column) based on the most frequent collocation combinations in 'your' data. Note that this means that the labels can be different if you run the same analysis on a different corpus; this is why the id is always kept.
#' @param force_download If TRUE, the resource will be downloaded even if it is stored locally.
#'
#' @export
feature_from_resource <- function(tc, resource='jrc_names', new_feature=resource, feature='word', resource_path=getOption('tcorpus_resources', NULL), collocation_labels=T, verbose=T, batchsize='default'){
  resource = match.arg(resource)

  re = load_resource(resource, resource_path)
  if(resource == 'jrc_names') {
    if(batchsize == 'default') batchsize = 50000
    index = use_stringmatch_resource(tc, re[,c('string','id')], regex_sep = '\\+', case_sensitive = T, batchsize = batchsize, flatten_colloc = T, verbose=verbose)
  }

  index$id = as.factor(index$id)
  f = factor(rep(NA, n_data(tc)), 1:length(levels(index$id)), levels(index$id))
  f[index$i] = index$id
  tc = set_column(tc, new_feature, f)

  if(collocation_labels) tc = add_collocation_label(tc, new_feature, feature=feature)
  tc
}

#' Download language resources
#'
#' Currently available resources:
#' \itemize{
#'    \item{JRC-Names: "JRC-Names is a highly multilingual named entity resource for person and organisation names. [...] JRC-Names is a by-product of the analysis of about 220,000 news reports per day by the Europe Media Monitor (EMM) family of applications." (https://ec.europa.eu/jrc/en/language-technologies/jrc-names)}
#' }
#'
#' The resource file is downloaded and stored on disk.
#' By default, the file will be saved to the location of the tcorpus package (for which you need write access).
#' Alternatively, the file can be saved as the specified local_path.
#' This can also be specified globally by using the set_resources_path function.
#'
#' @param resource A character string indicating which resource to download
#' @param local_path The path (without the filename) where the resource is stored. If NULL (default), the file will be stored in the location of the tcorpus package.
#' @param force_download If TRUE, the resource will be downloaded even if it is already stored locally. Only usefull if resources have been updated.
#'
#' @export
download_resource <- function(resource=c('jrc_names'), local_path=getOption('tcorpus_resources', NULL), force_download=F){
  resource = match.arg(resource)
  fname = make_filename(local_path, resource)

  if(file.exists(fname) & !force_download){
    message('Resource is already installed. Use load_resource() to get the resource object, or set force_download to TRUE to update.')
  } else {
    if(resource == 'jrc_names') return(download_jrc_names(fname))
  }
}

#' Specifiy the path where the downloaded resources are stored
#'
#' By default, tcorpus stores the resources within the tcorpus directory. With set_resources_path you can change this default to a custom path.
#'
#' @param path The path (without the filename) where the resources are stored. If NULL, the file will be stored in the location of the tcorpus package. if tempdir(), the file is stored in a directory that is cleaned up when the R session ends.
#'
#' @export
set_resources_path <- function(path=NULL) options(tcorpus_resources=path)

#' Load one of the language resources that have been downloaded with download_resource().
#'
#' Currently available resources:
#' \itemize{
#'    \item{JRC-Names: "JRC-Names is a highly multilingual named entity resource for person and organisation names. [...] JRC-Names is a by-product of the analysis of about 220,000 news reports per day by the Europe Media Monitor (EMM) family of applications." (https://ec.europa.eu/jrc/en/language-technologies/jrc-names)}
#' }
#'
#' @param resource A character string indicating which resource to download
#' @param local_path The path (without the filename) where the resource is stored. Please consult the documentation for download_resource() for more information regarding downloading and storing resources.
#'
#' @export
load_resource <- function(resource=c('jrc_names'), local_path=getOption('tcorpus_resources', NULL)){
  resource = match.arg(resource)
  fname = make_filename(local_path, resource)
  if(!file.exists(fname)) stop(sprintf('this resource has not yet been downloaded. You can use: download_resource("%s")', resource))

  if(resource == 'jrc_names') message('By using JRC-NAMES you agree to its usage conditions:\nhttps://ec.europa.eu/jrc/en/language-technologies/jrc-names\n')
  readRDS(fname)
}

make_filename <- function(local_path, resource){
  resource = paste(resource, 'rds', sep='.')
  if(is.null(local_path)){
    path = system.file(package='tcorpus')
    if(file.access(path,"6") == -1) stop('You do not have write permission for the location of this package. Either specify a local path to story the resources, or use save_tmp')
    path = paste(path, 'ext_resources', sep='/')
    if(!dir.exists(path)) dir.create(path)
    return(paste(path, resource, sep='/'))
 }
  path = if(local_path == '') getwd() else normalizePath(gsub('\\/$', '', local_path))
  path = paste(path, 'tcorpus_ext_resources', sep='/')
  if(!dir.exists(path)) dir.create(path)
  return(paste(path, resource, sep='/'))
}
