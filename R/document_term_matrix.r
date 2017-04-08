#' Create a document term matrix from a tCorpus
#'
#' @param tc
#' @param feature
#' @param context_level
#'
get_dtm <- function(tc, feature, context_level=c('document','sentence'), weight=c('termfreq','docfreq','tfidf','norm_tfidf'), drop_empty_terms=T, form=c('Matrix', 'tm_dtm', 'quanteda_dfm'), subset_tokens=NULL, subset_meta=NULL, context=NULL, context_labels=T, feature_labels=T, ngrams=NA, ngram_before_subset=F, env=environment()){
  form = match.arg(form)
  if(form == 'tm_dtm') if(!require(tm)) stop('form is set to tm_dtm, but the tm package is not installed.')
  if(form == 'quanteda_dfm') if(!require(quanteda)) stop('form is set to quanteda_dtm, but the quanteda package is not installed.')

  is_tcorpus(tc, T)
  if(is(tc, 'shattered_tCorpus')) return(shard_get_dtm(stc=tc, feature=feature, context_level=context_level, weight=weight, drop_empty_terms=drop_empty_terms, form=form, context_labels=context_labels))

  weight = match.arg(weight)
  context_levels = match.arg(context_level)

  i = if (!is.null(context)) context else tc$context(context_level, with_labels = context_labels)
  if (!is.null(subset_tokens) | !is.null(subset_meta)) {
    sub_i = tc$subset_i(subset = subset_tokens, subset_meta = subset_meta, env=env)
    i = i[sub_i]
  } else {
    sub_i = 1:tc$n
  }
  if (!is(i, 'factor')) i = fast_factor(i)

  feature = tc$data(feature)
  if(!is(feature, 'factor')) feature = factor(feature)
  if (!is.na(ngrams)) {
    filter = if (ngram_before_subset) NULL else sub_i
    feature = grouped_ngrams(feature, group = i, n = ngrams, filter = filter, label = feature_labels) ## designed to work fast if no labels are needed
  }
  feature = feature[sub_i]
  if(drop_empty_terms & is(feature, 'factor')) feature = droplevels(feature)
  notNA = !is.na(feature)

  m = Matrix::spMatrix(length(levels(i)), max(as.numeric(feature), na.rm = T),
                       as.numeric(i)[notNA], as.numeric(feature)[notNA],
                       rep(1, sum(notNA)))

  dimnames(m) = list(levels(i), levels(feature))
  m = weight_dtm(m, weight)

  if (form == 'tm_dtm'){
    m = tm::as.DocumentTermMatrix(m, weight=tm::weightTf)
    if(weight == 'tfidf') attributes(m)$weighting = c("term frequency - inverse document frequency", "tf-idf")
    if(weight == 'tfidf_norm') attributes(m)$weighting = c("term frequency - inverse document frequency (normalized)", "tf-idf")
    if(!weight %in% c('termfreq','tfidf', 'tfidf_norm')) attributes(m)$weighting = c(weight, weight)
  }
  if (form == 'quanteda_dfm') m = new("dfmSparse", as(m, 'dgCMatrix'))

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

tm_dtm_to_dgTMatrix <- function(dtm){
  sm = spMatrix(nrow(dtm), ncol(dtm), dtm$i, dtm$j, dtm$v)
  rownames(sm) = rownames(dtm)
  colnames(sm) = colnames(dtm)
  sm
}
