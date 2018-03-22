#' Specifiy the path where the downloaded resources are stored
#'
#' By default, corpustools stores the resources within the corpustools directory. With set_resources_path you can change this default to a custom path.
#'
#' @param path The path (without the filename) where the resources are stored. If NULL, the file will be stored in the location of the tcorpus package. if tempdir(), the file is stored in a directory that is cleaned up when the R session ends.
#'
#' @export
set_resources_path <- function(path=NULL) options(corpustools_resources=path)

#' Get name of the resources location
#'
#' @param local_path Optionally, specify a path instead of using the corpustool directory
#'
#' @export
resources_path <- function(local_path = getOption('corpustools_resources', NULL)) {
  if (is.null(local_path)) {
    path = system.file(package='corpustools')
    path = paste(path, 'ext_resources', sep='/')
  } else {
    path = local_path
  }
  path
}

#' List downloaded resources
#'
#' Returns a vector with the filenames of downloaded resources.
#'
#' @param local_path Optionally, specify a path instead of using the corpustool directory
#'
#' @export
list_resources <- function(local_path = getOption('corpustools_resources', NULL)) {
  if (is.null(local_path)) {
    path = system.file(package='corpustools')
    path = paste(path, 'ext_resources', sep='/')
  } else {
    path = local_path
  }
  if (dir.exists(path)) {
    return(list.files(path, recursive = TRUE))
  } else {
    return(NULL)
  }
}


make_dir <- function(path=getOption('corpustools_resources', NULL), ...) {
  if (is.null(path)){
    path = system.file(package='corpustools')
  } else {
    path = if (path == '') getwd() else normalizePath(gsub('\\/$', '', path))
  }
  if (file.access(path,"6") == -1) stop('You do not have write permission for the location where corpustools is installed, and therefore cannot download to this location. Use set_resources_path to specify a custom path')
  path = paste(path, 'ext_resources', sep='/')

  add = paste(unlist(list(...)), collapse='/')
  if (!add == '') path = paste(path, add, sep='/')

  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  path
}


make_filename <- function(local_path, resource){
  resource = paste(resource, 'rds', sep='.')
  path = make_dir(local_path)
  return(paste(path, resource, sep='/'))
}
