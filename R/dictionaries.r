
get_dictionary_data <- function(dict, local_path=getwd()) {
  path = make_dir(local_path, 'corpustools_data', 'dictionaries')
  
  fname = list.files(path, full.names = T, include.dirs = F)
  fname = fname[grep(dict, fname, fixed=T)]
  if (length(fname) == 0) fname = NA
  
  #if (is.na(fname)) fname = create_dict(dict, path)
  readRDS(fname)
}


download_dictionary_data <- function(dict, path) {
  ## download dictionary files and create dictionary
  if (dict == 'gender_names_US') fname = gender_names_US(path)
    
  fname
}

gender_names_US <- function(path) {
  url = 'https://www.ssa.gov/oact/babynames/names.zip'
  fname = download_dict(url, path, 'gender_names_US')
}


download_dict <- function(url, path, file = gsub('.*/','',url)) {
  ## url can be a vector of urls with alternatives
  fname = file.path(path,file)
  for (u in url) {
    done = tryCatch(utils::download.file(u, destfile = fname) == 0, error = function(e) F, warning = function(w) F)
    if (done) break
    if (length(url) > 1) message('\nDownload failed. Now trying alternative url.\n')  
  }
  if (!done) stop('Failed to download dictionary data. If persists please let us know, since the data might have been removed.')
  fname
}
