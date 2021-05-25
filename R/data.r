#' State of the Union addresses
#'
#' @docType data
#' @usage data(sotu_texts)
#' @format data.frame
'sotu_texts'
##save(sotu_texts, file='data/sotu_texts.rda', compression_level = 9)


#' coreNLP example sentences
#'
#' @docType data
#' @usage data(corenlp_tokens)
#' @format data.frame
'corenlp_tokens'

#' A tCorpus with a small sample of sotu paragraphs parsed with udpipe
#'
#' @docType data
#' @usage data(tc_sotu_udpipe)
#' @format data.frame
'tc_sotu_udpipe'

## run if tc methods have been updated
## tc_sotu_udpipe = refresh_tcorpus(tc_sotu_udpipe)
## save(tc_sotu_udpipe, file='data/tc_sotu_udpipe.rda', compression_level = 9)

#' Basic stopword lists
#'
#' @docType data
#' @usage data(stopwords_list)
#' @format A named list, with names matching the languages used by SnowballC
"stopwords_list"
