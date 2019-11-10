#' Estimate a LDA topic model
#'
#' @description
#' Estimate an LDA topic model using the LDA function from the topicmodels package.
#' The parameters other than dtm are simply passed to the sampler but provide a workable default.
#' See the description of that function for more information
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{
#' lda_fit(feature, create_feature=NULL, K=50, num.iterations=500, alpha=50/K,
#'      eta=.01, burnin=250, context_level=c('document','sentence'), ...)
#' }
#'
#' @param feature the name of the feature columns
#' @param create_feature optionally, add a feature column that indicates the topic to which a feature was assigned (in the last iteration). Has to be a character string, that will be the name of the new feature column
#' @param K the number of clusters
#' @param num.iterations the number of iterations
#' @param method set method. see documentation for LDA function of the topicmodels package
#' @param alpha the alpha parameter
#' @param eta the eta parameter#'
#' @param burnin The number of burnin iterations
#'
#' @return A fitted LDA model, and optionally a new column in the tcorpus (added by reference)
#'
#' @name tCorpus$lda_fit
#' @aliases lda_fit
#' @examples
#' \donttest{
#' tc = create_tcorpus(sotu_texts, doc_column = 'id')
#' tc$preprocess('token', 'feature', remove_stopwords = TRUE, use_stemming = TRUE, min_freq=10)
#' set.seed(1)
#' m = tc$lda_fit('feature', create_feature = 'lda', K = 5, alpha = 0.1)
#'
#' m
#' topicmodels::terms(m, 10)
#' tc$tokens
#' }
tCorpus$set('public', 'lda_fit', function(feature, create_feature=NULL, K=50, num.iterations=500, alpha=50/K, eta=.01, burnin=250, context_level=c('document','sentence'), ...) {
  require_package('topicmodels')

  dtm = get_dtm(self, feature=feature, context_level=context_level, ...)
  m = lda_fit(dtm=dtm, method='Gibbs', K=K, num.iterations=num.iterations, alpha=alpha, eta=eta, burnin=burnin)
  if (!is.null(create_feature)) {
    .d = lda_features(tc=self, m=m, feature=feature, new_feature=new_feature, context_level=context_level)
    self$set(create_feature, .d$v[order(.d$i)])
  }
  m
})

#############################
#############################

lda_features <- function(tc, m, feature, new_feature='LDA_topic', context_level=c('document','sentence')){
  context_level = match.arg(context_level)
  if (!methods::is(m, 'LDA_Gibbs')) stop('An LDA model of class LDA_Gibbs (topicmodels::LDA with method = "Gibbs") is required')

  d = data.table(context = tc$context(context_level),
                 feature = fast_factor(tc$get(feature)),
                 i = 1:tc$n)
  wa = data.table(context=factor(m@documents[m@wordassignments$i]),
                  feature=factor(m@terms[m@wordassignments$j]),
                  v=m@wordassignments$v)

  data.table::setkeyv(d, c('context','feature'))
  data.table::setkeyv(wa, c('context','feature'))
  merge(d, wa, all.x = T)
}

lda_fit <- function(dtm, method='Gibbs', K=50, num.iterations=500, alpha=50/K, eta=.01, burnin=250) {
  require_package('topicmodels')
  if (methods::is(dtm, 'DocumentTermMatrix')) dtm = as_dgTMatrix(dtm)

  empty_rows = sum(Matrix::rowSums(dtm) == 0)
  empty_cols = sum(Matrix::rowSums(dtm) == 0)
  if (empty_rows) message(sprintf('%s rows in the dtm are empty. These have been deleted', empty_rows))
  if (empty_cols) message(sprintf('%s columns in the dtm are empty. These have been deleted', empty_cols))

  dtm = dtm[Matrix::rowSums(dtm) > 0, Matrix::colSums(dtm) > 0]
  m = topicmodels::LDA(dtm, k=K, method=method, control=list(iter=num.iterations, burnin=burnin, alpha=alpha, delta=eta))
  m
}

