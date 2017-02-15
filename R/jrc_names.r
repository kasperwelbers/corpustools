### instead of exporting features_from_resource, make specific functions per resource (do use features_from_resource internally)

## add specific function to get the jrc_names.
## jrc_names <- function()

download_jrc_names <- function(fname){
  message('This package only links to the JRC-Names resource. By downloading and/or using this package you agree to their usage conditions. Please consult the JRC website for information, conditions and citation instructions: \nhttps://ec.europa.eu/jrc/en/language-technologies/jrc-names\n')

  temp <- tempfile()
  download.file("http://optima.jrc.it/data/entities.gzip",temp)
  re = readLines(temp)
  re = stringi::stri_split(re, regex='\t', simplify = T)
  re = data.frame(string=as.character(re[,4]), id=as.numeric(re[,1]), lang=as.factor(re[,3]), pos=as.factor(re[,2]), stringsAsFactors = F)
  re = data.table(re)
  saveRDS(re, fname)
  unlink(temp)
}

## add jrc name lookup
### save data that has been accessed through the endpoint in the resources directory.
### then, if the information is asked for again, it will first look it up locally, and only access the endpoint for unknown info
### let users controll this with the save=T and force_download=F parameters
