#' Create a document term matrix from a tCorpus
#'
#' @param tc
#' @param feature
#' @param context_level
#'
#' @export
get_dtm <- function(tc, feature, context_level=c('document','sentence'), weight=c('termfreq','docfreq','tfidf','norm_tfidf'), drop_empty_terms=T, form=c('Matrix', 'tm_dtm', 'quanteda_dfm'), subset_tokens=NULL, subset_meta=NULL){
  is_tcorpus(tc, T)
  if(is(tc, 'shattered_tCorpus')) return(shard_get_dtm(stc=tc, feature=feature, context_level=context_level, weight=weight, drop_empty_terms=drop_empty_terms, form=form))

  subset_tokens = if(is(substitute(subset_tokens), 'call')) as.character(deparse(substitute(subset_tokens)))
  subset_meta = if(is(substitute(subset_meta), 'call')) as.character(deparse(substitute(subset_meta)))
  if(!is.null(subset_tokens) | !is.null(subset_meta)){
    tc = subset(tc, subset=subset_tokens, subset_meta=subset_meta)
  }

  #context_level = match.arg(context_level)
  weight = match.arg(weight)
  form = match.arg(form)

  if(form == 'tm_dtm') if(!require(tm)) stop('form is set to tm_dtm, but the tm package is not installed.')
  if(form == 'quanteda') if(!require(quanteda)) stop('form is set to quanteda_dtm, but the quanteda package is not installed.')

  i = get_context(tc, context_level)
  feature = get_column(tc, feature)
  if(drop_empty_terms) feature = droplevels(feature)
  notNA = !is.na(feature)
  m = Matrix::spMatrix(length(levels(i)), length(levels(feature)),
                       as.numeric(i)[notNA], as.numeric(feature)[notNA],
                       rep(1, sum(notNA)))

  dimnames(m) = list(levels(i), levels(feature))
  m = weight_dtm(m, weight)

  if(form == 'tm_dtm'){
      m = tm::as.DocumentTermMatrix(m, weight=tm::weightTf)
      if(weight == 'tfidf') attributes(dtm)$weighting = c("term frequency - inverse document frequency", "tf-idf")
      if(weight == 'tfidf_norm') attributes(dtm)$weighting = c("term frequency - inverse document frequency (normalized)", "tf-idf")
      if(!weight %in% c('termfreq','tfidf', 'tfidf_norm')) attributes(dtm)$weighting = c(weight, weight)
  }
  if(form == 'quanteda_dfm') m = new("dfmSparse", as(m, 'dgCMatrix'))
  m
}

weight_dtm <- function(m, weight){
  m = as(m, 'dgTMatrix')
  if(weight %in% c('tfidf', 'norm_tfidf')){
    if(weight == 'norm_tfidf') m@x = m@x / rowSums(m)[m@i+1]
    idf = log2(nrow(m)/colSums(m > 0))
    m@x = m@x * idf[m@j+1]
  }
  if(weight == 'docfreq') {
    m = m > 0
  }
  as(as(m,'dgCMatrix'), 'dgTMatrix')
}
