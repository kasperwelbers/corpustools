#' Multilingual named entity recognition using the JRC-NAMES resource
#'
#' "JRC-Names is a highly multilingual named entity resource for person and organisation names. [...] JRC-Names is a by-product of the analysis of about 220,000 news reports per day by the Europe Media Monitor (EMM) family of applications." (https://ec.europa.eu/jrc/en/language-technologies/jrc-names)
#'
#' The resource needs to be downloaded first. For this you can use the download_resource function, which will (by default) download the resource into the tcorpus package folder.
#'
#' @param tc a tCorpus object
#' @param new_feature The column name of the new feature.
#' @param feature The feature to be used as input. For JRC names regular (unprocessed) words should be used.
#' @param resource_path The path (without the filename) where the resource is stored. See ?download_resource for more information.
#' @param collocation_labels if True, then for resources that create an id for subsequent words (e.g., named entities), labels are added (in a separate column) based on the most frequent collocation combinations in 'your' data. Note that this means that the labels can be different if you run the same analysis on a different corpus; this is why the id is always kept.
#' @param batchsize The number of named entity string variations per batch. Using bigger batches is faster, but depending on the size of your corpus you might run out of memory (in which case you should use smaller batches). At the time of writing the total number of strings is roughtly 700,000.
#' @param low_memory if TRUE (default) then data will be sorted in a way that tries to get a roughly equal number of string matches per batch, to prevent huge match tables (costing memory). If FALSE, data will be sorted in a way to get fewer unique words per batch, which can speed up matching, but can lead to a very unequal number of matches per batch.
#'
jrc_names <- function(tc, new_feature='jrc_names', feature='word', resource_path=getOption('tcorpus_resources', NULL), collocation_labels=T, batchsize=50000, low_memory=T, verbose=T){
  re = load_resource('jrc_names', resource_path)

  index = use_stringmatch_resource(tc, re[,c('string','id')], regex_sep = '\\+', case_sensitive = T, lowercase = F, ascii=F, batchsize = batchsize, flatten_colloc = T, verbose=verbose)

  index$id = fast_factor(index$id)
  f = factor(rep(NA, tc$n), 1:length(levels(index$id)), levels(index$id))
  f[index$i] = index$id
  tc = tc$set(column=new_feature, value = f)

  if (collocation_labels) {
    if (verbose) message('Adding collocation labels')
    tc = add_collocation_label(tc, new_feature, feature=feature)
  }
  tc
}


download_jrc_names <- function(fname){
  message('This package only links to the JRC-Names resource. By downloading and/or using this package you agree to their usage conditions. Please consult the JRC website for information, conditions and citation instructions: \nhttps://ec.europa.eu/jrc/en/language-technologies/jrc-names\n')

  temp <- tempfile()
  download.file("http://optima.jrc.it/data/entities.gzip",temp)
  re = readLines(temp)
  re = stringi::stri_split(re, regex='\t', simplify = T)
  re = data.frame(string=as.character(re[,4]), id=as.numeric(re[,1]), lang=fast_factor(re[,3]), pos=fast_factor(re[,2]), stringsAsFactors = F)
  re = data.table(re)
  saveRDS(re, fname)
  unlink(temp)
}

## add jrc name lookup
### save data that has been accessed through the endpoint in the resources directory.
### then, if the information is asked for again, it will first look it up locally, and only access the endpoint for unknown info
### let users controll this with the save=T and force_download=F parameters
