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
?tCorpus

#' Estimate a topic model using the lda package
#'
#' Estimate an LDA topic model using the LDA function from the topicmodels package.
#' The parameters other than dtm are simply passed to the sampler but provide a workable default.
#' See the description of that function for more information
#'
#' @param dtm a document term matrix
#' @param K the number of clusters
#' @param num.iterations the number of iterations
#' @param alpha the alpha parameter
#' @param eta the eta parameter
#' @return A fitted LDA model
#' @export
lda_fit <- function(dtm, method='Gibbs', K=50, num.iterations=500, alpha=50/K, eta=.01, burnin=250) {
  if (!requireNamespace('topicmodels', quietly = T)) stop('The topicmodels package is required for this function, but has not yet been installed. You can run install.packages("topicmodels")')
  if (methods::is(dtm, 'DocumentTermMatrix')) dtm = tm_dtm_to_dgTMatrix(dtm)

  empty_rows = sum(Matrix::rowSums(dtm) == 0)
  empty_cols = sum(Matrix::rowSums(dtm) == 0)
  if (empty_rows) message(sprintf('%s rows in the dtm are empty. These have been deleted', empty_rows))
  if (empty_cols) message(sprintf('%s columns in the dtm are empty. These have been deleted', empty_cols))

  dtm = dtm[Matrix::rowSums(dtm) > 0, Matrix::colSums(dtm) > 0]
  m = topicmodels::LDA(dtm, k=K, method=method, control=list(iter=num.iterations, burnin=burnin, alpha=alpha, delta=eta))
  m
}

