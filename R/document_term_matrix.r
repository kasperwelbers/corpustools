#' Create a document term matrix
#'
#' @section Usage:
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{dtm(feature, context_level=c('document','sentence'), weight=c('termfreq','docfreq','tfidf','norm_tfidf'), drop_empty_terms=T, form=c('Matrix', 'tm_dtm', 'quanteda_dfm'), subset_tokens=NULL, subset_meta=NULL, context=NULL, context_labels=T, feature_labels=T, ngrams=NA, ngram_before_subset=F)}
#'
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
#' @name tCorpus$dtm
#' @examples
#' tc = create_tcorpus(c("First text first sentence. First text first sentence.",
#'                    "Second text first sentence"), doc_column = 'id', split_sentences = TRUE)
#'
#' ## Perform additional preprocessing on the 'token' column, and save as the 'feature' column
#' tc$preprocess('token', 'feature', remove_stopwords = TRUE, use_stemming = TRUE)
#' tc$get()
#'
#' ## default: regular sparse matrix, using the Matrix package
#' m = tc$dtm('feature')
#' class(m)
#' m
#'
#' ## alternatively, create quanteda ('quanteda_dfm') or tm ('tm_dtm') class for DTM
#' \dontrun{
#' m = tc$dtm('feature', form = 'quanteda_dfm')
#' class(m)
#' m
#' }
#'
#' ## create DTM with sentences as rows (instead of documents)
#' m = tc$dtm('feature', context_level = 'sentence')
#' nrow(m)
#'
#' ## use weighting
#' m = tc$dtm('feature', weight = 'norm_tfidf')
#' @aliases dtm.tCorpus
tCorpus$set('public', 'dtm', function(feature, context_level=c('document','sentence'), weight=c('termfreq','docfreq','tfidf','norm_tfidf'), drop_empty_terms=T, form=c('Matrix', 'tm_dtm', 'quanteda_dfm'), subset_tokens=NULL, subset_meta=NULL, context=NULL, context_labels=T, feature_labels=T, ngrams=NA, ngram_before_subset=F) {
  if (class(substitute(subset_tokens)) %in% c('call', 'name')) subset_tokens = self$eval(substitute(subset_tokens), parent.frame())
  if (class(substitute(subset_meta)) %in% c('call', 'name')) subset_meta = self$eval_meta(substitute(subset_meta), parent.frame())

  get_dtm(self, feature=feature, context_level=context_level, weight=weight, drop_empty_terms=drop_empty_terms, form=form,
          subset_tokens=subset_tokens, subset_meta=subset_meta, context=context, context_labels=context_labels,
          feature_labels=feature_labels, ngrams=ngrams, ngram_before_subset=ngram_before_subset)
})

get_dtm <- function(tc, feature, context_level=c('document','sentence'), weight=c('termfreq','docfreq','tfidf','norm_tfidf'), drop_empty_terms=T, form=c('Matrix', 'tm_dtm', 'quanteda_dfm'), subset_tokens=NULL, subset_meta=NULL, context=NULL, context_labels=T, feature_labels=T, ngrams=NA, ngram_before_subset=F){
  form = match.arg(form)
  if(form == 'tm_dtm') if(!requireNamespace('tm', quietly = T)) stop('To use the tm_dtm output form, you need to have the tm package installed.')
  if(form == 'quanteda_dfm') if(!requireNamespace('quanteda', quietly = T)) stop('To use the quanteda_dtm output form, you need to have the quanteda package installed.')
  is_tcorpus(tc, T)

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
  if(drop_empty_terms & methods::is(feature, 'factor')) feature = droplevels(feature)
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
    m = methods::new("dfmSparse", methods::as(m, 'dgCMatrix'))
    meta = tc$get_meta()
    rownames(meta) = meta$doc_id
    m@docvars <- as.data.frame(meta)
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
  methods::as(methods::as(m,'dgCMatrix'), 'dgTMatrix')
}

tm_dtm_to_dgTMatrix <- function(dtm){
  sm = Matrix::spMatrix(nrow(dtm), ncol(dtm), dtm$i, dtm$j, dtm$v)
  rownames(sm) = rownames(dtm)
  colnames(sm) = colnames(dtm)
  sm
}
