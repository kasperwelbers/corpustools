tokenlist <- function(tokens) {
  split(as.numeric(tokens$token_i), as.character(tokens$doc_id))
}

#proximity_match <- function(tokens_x, tokens_y, window=0) {
## probably useless due to changes in search.r (including the c++ code), but keeping it for at least one github push for safety
#  .Call('_corpustools_proximity_match', PACKAGE = 'corpustools', tokenlist(tokens_x), tokenlist(tokens_y), window)
#}
