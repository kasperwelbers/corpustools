#' Subset a tCorpus
#'
#' @param tc tCorpus object
#' @param subset
#'
#' @export
subset.tCorpus <- function(tc, subset=NULL, subset_meta=NULL, keep_feature_index=T, drop_levels=F) {
  e = if(is(substitute(subset), 'character')) parse(text=subset) else substitute(subset)
  e_meta = if(is(substitute(subset_meta), 'character')) parse(text=subset_meta) else substitute(subset_meta)

  r_meta = eval(e_meta, tc@meta, parent.frame())
  if(!is.null(r_meta)){
    tc@meta = tc@meta[r_meta,,nomatch=0]
    tc@data = tc@data[as.character(unique(tc@meta$doc_id)),,nomatch=0]
  }

  set_keys(tc)
  r = eval(e, tc@data, parent.frame())
  if(!is.null(r)){
    tc@data = tc@data[r,,nomatch=0]
    set_keys(tc)
    tc@meta = tc@meta[as.character(unique(tc@data$doc_id)),,nomatch=0]
  }

  if(drop_levels){
    tc@data = droplevels(tc@data)
    tc@meta = droplevels(tc@meta)
  }
  tc@meta$doc_id = as.character(tc@meta$doc_id)

  suppressWarnings(set_keys(tc))

  if(get_provenance(tc)$feature_index){
    if(keep_feature_index){
      cat('Feature index has been reset for the new subset\n')
      tc = reset_feature_index(tc)
    } else {
      tc = delete_feature_index(tc)
    }
  }
  tc
}

subset_i <- function(tc, subset=NA, subset_meta=NA){
  ## subset and subset need to be given as a character string
  n = nrow(get_data(tc))
  if(is.na(subset)){
    r = NULL
  } else {
    e = parse(text=as.character(subset))
    r = (1:n)[eval(e, tc@data, parent.frame())]
  }

  n_meta = nrow(get_meta(tc))
  if(is.na(subset_meta)){
    r_meta = NULL
  } else {
    e_meta = parse(text=as.character(subset_meta))
    r_meta = (1:n_meta)[eval(e_meta, tc@meta, parent.frame())]
    if(length(r_meta) > 0) {
      r_meta = 1:n_meta %in% r_meta
      r_meta = r_meta[match(get_column(tc, 'doc_id'), get_meta_column(tc, 'doc_id'))] ## extend to length()
      r_meta = which(r_meta)
    }
  }

  if(!is.null(r) & !is.null(r_meta)) return(intersect(r, r_meta))
  if(is.null(r) & is.null(r_meta)) return(1:n)
  if(!is.null(r) & is.null(r_meta)) return(r)
  if(is.null(r) & !is.null(r_meta)) return(r_meta)
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
