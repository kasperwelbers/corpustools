#' Subset a tCorpus
#'
#' @param tc tCorpus object
#' @param subset
#'
#' @return
#' @export
#'
#' @examples
subset.tCorpus <- function(tc, subset=NULL, subset_meta=NULL, keep_feature_index=F, drop_levels=T) {
  e = substitute(subset)
  e_meta = substitute(subset_meta)

  r = eval(e, tc@data, parent.frame())
  if(!is.null(r)){
    tc@data = tc@data[r,]
    tc@doc_meta = tc@doc_meta[as.character(unique(tc@data$doc_id)),]
  }

  r_meta = eval(e_meta, tc@doc_meta, parent.frame())
  if(!is.null(r_meta)){
    tc@doc_meta = tc@doc_meta[r_meta,]
    if(is.null(key(tc@data))) setkey(tc@data, 'doc_id')
    tc@data = tc@data[as.character(unique(tc@doc_meta$doc_id))]
  }

  if(drop_levels){
    tc@data = droplevels(tc@data)
    tc@doc_meta = droplevels(tc@doc_meta)
  }
  if(is.null(key(tc@data))) setkey(tc@data, 'doc_id')
  if(is.null(key(tc@doc_meta))) setkey(tc@doc_meta, 'doc_id')

  if(get_provenance(tc)$feature_index){
    if(keep_feature_index){
      cat('\tResetting feature index\n')
      tc = reset_feature_index(tc)
    } else {
      tc = delete_feature_index(tc)
    }
  }
  tc
}

## subset functions ##

#' @export
freq <- function(x) {
  d = as.data.frame(table(x))
  d$Freq[match(x, d$x)]
}

#' @export
freq_top <- function(x, n=100) {
  d = as.data.frame(table(x))
  top = head(d[order(-d$Freq),], n)
  d$top = ifelse(d$x %in% top$x, T, F)
  d$top[match(x, d$x)]
}

#' @export
docfreq <- function(x, doc_id=parent.frame()$doc_id) {
  d = unique(data.frame(id=doc_id, term=x))
  d = as.data.frame(table(d$term))
  d$Freq[match(x, d$Var1)]
}

#' @export
docfreq_top <- function(x, n=100, doc_id=parent.frame()$doc_id) {
  d = unique(data.frame(id=doc_id, term=x))
  d = as.data.frame(table(d$term))
  top = head(d[order(-d$Freq),], n)
  d$top = ifelse(d$Var1 %in% top$Var1, T, F)
  d$top[match(x, d$Var1)]
}

#' @export
docfreq_pct <- function(x, doc_id=parent.frame()$doc_id) {
  d = unique(data.frame(id=doc_id, term=x))
  d = as.data.frame(table(d$term))
  d$Freq = (d$Freq / length(unique(doc_id))) * 100
  d$Freq[match(x, d$Var1)]
}
