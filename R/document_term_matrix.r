#' Create a document term matrix.
#'
#' @description
#' Create a document term matrix. The default output is a sparse matrix (Matrix, dgTMatrix). Alternatively, the dtm style from the tm and quanteda package can be used.
#'
#' The dfm function is shorthand for using quanteda's dfm (document feature matrix) class. The meta data in the tcorpus is then automatically added as docvars in the dfm.
#'
#' @param tc      a \code{\link{tCorpus}}
#' @param feature The name of the feature column
#' @param context_level Select whether the rows of the dtm should represent "documents" or "sentences".
#' @param weight Select the weighting scheme for the DTM. Currently supports term frequency (termfreq), document frequency (docfreq), term frequency inverse document frequency (tfidf) and tfidf with normalized document vectors.
#' @param drop_empty_terms If True, tokens that do not occur (i.e. column where sum is 0) are ignored.
#' @param form The output format. Default is a sparse matrix in the dgTMatrix class from the Matrix package. Alternatives are tm_dtm for a DocumentTermMatrix in the tm package format or quanteda_dfm for the document feature matrix from the quanteda package.
#' @param subset_tokens A subset call to select which rows to use in the DTM
#' @param subset_meta A subset call for the meta data, to select which documents to use in the DTM
#' @param context Instead of using the document or sentence context, an custom context can be specified. Has to be a vector of the same length as the number of tokens, that serves as the index column. Each unique value will be a row in the DTM.
#' @param context_labels If False, the DTM will not be given rownames
#' @param feature_labels If False, the DTM will not be given column names
#' @param ngrams Optionally, use ngrams instead of individual tokens. This is more memory efficient than first creating an ngram feature in the tCorpus.
#' @param ngram_before_subset If a subset is used, ngrams can be made before the subset, in which case an ngram can contain tokens that have been filtered out after the subset. Alternatively, if ngrams are made after the subset, ngrams will span over the gaps of tokens that are filtered out.
#'
#' @return A document term matrix, in the format specified in the form argument
#' @export
#' @examples
#' tc = create_tcorpus(c("First text first sentence. First text first sentence.",
#'                    "Second text first sentence"), doc_column = 'id', split_sentences = TRUE)
#'
#' ## Perform additional preprocessing on the 'token' column, and save as the 'feature' column
#' tc$preprocess('token', 'feature', remove_stopwords = TRUE, use_stemming = TRUE)
#' tc$tokens
#'
#' ## default: regular sparse matrix, using the Matrix package
#' m = get_dtm(tc, 'feature')
#' class(m)
#' m
#'
#' ## alternatively, create quanteda ('quanteda_dfm') or tm ('tm_dtm') class for DTM
#' \donttest{
#' m = get_dtm(tc, 'feature', form = 'quanteda_dfm')
#' class(m)
#' m
#' }
#'
#' ## create DTM with sentences as rows (instead of documents)
#' m = get_dtm(tc, 'feature', context_level = 'sentence')
#' nrow(m)
#'
#' ## use weighting
#' m = get_dtm(tc, 'feature', weight = 'norm_tfidf')
get_dtm <- function(tc, feature, context_level=c('document','sentence'), weight=c('termfreq','docfreq','tfidf','norm_tfidf'), drop_empty_terms=T, form=c('Matrix', 'tm_dtm', 'quanteda_dfm'), subset_tokens=NULL, subset_meta=NULL, context=NULL, context_labels=T, feature_labels=T, ngrams=NA, ngram_before_subset=F) {
  if (class(substitute(subset_tokens)) %in% c('call', 'name')) subset_tokens = tc$eval(substitute(subset_tokens), parent.frame())
  if (class(substitute(subset_meta)) %in% c('call', 'name')) subset_meta = tc$eval_meta(substitute(subset_meta), parent.frame())

  do_get_dtm(tc, feature=feature, context_level=context_level, weight=weight, drop_empty_terms=drop_empty_terms, form=form,
          subset_tokens=subset_tokens, subset_meta=subset_meta, context=context, context_labels=context_labels,
          feature_labels=feature_labels, ngrams=ngrams, ngram_before_subset=ngram_before_subset)
}

#' @rdname get_dtm
#' @export
get_dfm <- function(tc, feature, context_level=c('document','sentence'), weight=c('termfreq','docfreq','tfidf','norm_tfidf'), drop_empty_terms=T, subset_tokens=NULL, subset_meta=NULL, context=NULL, context_labels=T, feature_labels=T, ngrams=NA, ngram_before_subset=F) {
  if (class(substitute(subset_tokens)) %in% c('call', 'name')) subset_tokens = tc$eval(substitute(subset_tokens), parent.frame())
  if (class(substitute(subset_meta)) %in% c('call', 'name')) subset_meta = tc$eval_meta(substitute(subset_meta), parent.frame())
  do_get_dtm(tc, feature=feature, context_level=context_level, weight=weight, drop_empty_terms=drop_empty_terms, form='quanteda_dfm',
          subset_tokens=subset_tokens, subset_meta=subset_meta, context=context, context_labels=context_labels,
          feature_labels=feature_labels, ngrams=ngrams, ngram_before_subset=ngram_before_subset)
}


do_get_dtm <- function(tc, feature, context_level=c('document','sentence'), weight=c('termfreq','docfreq','tfidf','norm_tfidf'), drop_empty_terms=T, form=c('Matrix', 'tm_dtm', 'quanteda_dfm'), subset_tokens=NULL, subset_meta=NULL, context=NULL, context_labels=T, feature_labels=T, ngrams=NA, ngram_before_subset=F){
  form = match.arg(form)
  if(form == 'tm_dtm') require_package('tm', '0.6')
  is_tcorpus(tc)
  weight = match.arg(weight)
  context_levels = match.arg(context_level)

  i = if (!is.null(context)) context else tc$context(context_level, with_labels = context_labels)
  if (!methods::is(i, 'factor')) i = fast_factor(i)

  if (!is.null(subset_tokens) | !is.null(subset_meta)) {
    .subset_tokens = subset_tokens
    .subset_meta = subset_meta
    sub_i = tc$get_token_id(subset = .subset_tokens, subset_meta = .subset_meta)
    all_i = i
    i = droplevels(all_i[sub_i])
  } else {
    sub_i = 1:tc$n
  }

  feature = tc$get(feature)

  if(!methods::is(feature, 'factor')) feature = fast_factor(feature)
  if (!is.na(ngrams)) {
    filter = if (ngram_before_subset) NULL else sub_i
    feature = grouped_ngrams(feature, group = i, n = ngrams, filter = filter, label = feature_labels) ## designed to work fast if no labels are needed
  }

  if (!is.null(subset_tokens) | !is.null(subset_meta)) { ## if a subset is used, calculate the idf here for the entire corpus
    idf = get_idf(all_i, feature)
  } else idf = NULL

  feature = feature[sub_i]
  if(drop_empty_terms && methods::is(feature, 'factor')) feature = droplevels(feature)
  notNA = !is.na(feature)

  m = Matrix::spMatrix(length(levels(i)), length(levels(feature)),
                       as.numeric(i)[notNA], as.numeric(feature)[notNA],
                       rep(1, sum(notNA)))

  dimnames(m) = list(levels(i), levels(feature))

  m = weight_dtm(m, weight, idf=idf)

  if (form == 'tm_dtm'){
    m = tm::as.DocumentTermMatrix(m, weight=tm::weightTf)
    if(weight == 'tfidf') attributes(m)$weighting = c("term frequency - inverse document frequency", "tf-idf")
    if(weight == 'tfidf_norm') attributes(m)$weighting = c("term frequency - inverse document frequency (normalized)", "tf-idf")
    if(!weight %in% c('termfreq','tfidf', 'tfidf_norm')) attributes(m)$weighting = c(weight, weight)
  }
  if (form == 'quanteda_dfm') {
    m = quanteda::as.dfm(methods::as(m, 'dgCMatrix'))
    dvars = tc$get_meta(copy=T, keep_df = T)
    dvars = dvars[match(rownames(m), dvars$doc_id),]  ## in case of subsetting
    for (dvar in colnames(dvars)) {
      if (dvar == 'doc_id') next
      quanteda::docvars(m, field = dvar) = dvars[[dvar]]
    }
  }
  m
}


get_idf <- function(context, feature) {
  idf = unique(cbind(context=context, feature=as.character(feature)))
  idf = table(idf[,'feature'])
  N = length(unique(context))
  log2(N / idf)
}

weight_dtm <- function(m, weight, idf=NULL){
  m = methods::as(m, 'dgTMatrix')
  if(weight %in% c('tfidf', 'norm_tfidf')){
    if(weight == 'norm_tfidf') m@x = m@x / rowSums(m)[m@i+1]
    if(is.null(idf)) {
      idf = log2(nrow(m)/colSums(m > 0))
    } else {
      idf = as.numeric(idf[match(colnames(m), names(idf))])
    }
    m@x = m@x * idf[m@j+1]
  }
  if(weight == 'docfreq') {
    m = m > 0
  }
  methods::as(m,'dgCMatrix')
}

as_dgTMatrix <- function(dtm){
  if (!methods::is(dtm, 'DocumentTermMatrix')) return(methods::as(dtm, 'dgTMatrix'))
  sm = Matrix::spMatrix(nrow(dtm), ncol(dtm), dtm$i, dtm$j, dtm$v)
  rownames(sm) = rownames(dtm)
  colnames(sm) = colnames(dtm)
  methods::as(sm, 'dgTMatrix')
}
