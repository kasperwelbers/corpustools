lda_features <- function(tc, m, feature, new_feature='LDA_topic', context_level=c('document','sentence')){
  context_level = match.arg(context_level)
  if (!is(m, 'LDA_Gibbs')) stop('An LDA model of class LDA_Gibbs (topicmodels::LDA with method = "Gibbs") is required')

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

#' Estimate a topic model using the lda package
#'
#' Estimate an LDA topic model using the \code{\link{LDA}} function
#' The parameters other than dtm are simply passed to the sampler but provide a workable default.
#' See the description of that function for more information
#'
#' @param dtm a document term matrix (e.g. the output of \code{\link{amcat.dtm.create}})
#' @param K the number of clusters
#' @param num.iterations the number of iterations
#' @param alpha the alpha parameter
#' @param eta the eta parameter
#' @return A fitted LDA model (see \code{\link{LDA}})
#' @export
lda_fit <- function(dtm, method='Gibbs', K=50, num.iterations=500, alpha=50/K, eta=.01, burnin=250) {
  if (!require(topicmodels)) stop('The topicmodels package is required for this function, but has not yet been installed. You can run install.packages("topicmodels")')
  if (is(dtm, 'DocumentTermMatrix')) dtm = tm_dtm_to_dgTMatrix(dtm)

  empty_rows = sum(Matrix::rowSums(dtm) == 0)
  empty_cols = sum(Matrix::rowSums(dtm) == 0)
  if (empty_rows) message(sprintf('%s rows in the dtm are empty. These have been deleted', empty_rows))
  if (empty_cols) message(sprintf('%s columns in the dtm are empty. These have been deleted', empty_cols))

  dtm = dtm[Matrix::rowSums(dtm) > 0, Matrix::colSums(dtm) > 0]
  m = LDA(dtm, k=K, method=method, control=list(iter=num.iterations, burnin=burnin, alpha=alpha, delta=eta))
  m
}

#' Get word assignments from LDA_GIBBS class (output of lda_fit). This is similar to the documentsums object that comes as the output of lda.collapsed.gibbs.sampler
#'
#' Get word assignments from LDA_GIBBS class (output of lda_fit). This is similar to the documentsums object that comes as the output of lda.collapsed.gibbs.sampler
#' LDA assigns a topic to each unique word in a document. If you also want to take into account how often this word occured, the document term matrix (as used in the input for lda_fit) must be included in the weight.by.dtm argument.
#'
#' @param m The output from one of the topicmodeling functions in the topicmodels package (e.g., LDA_GIBBS)
#' @param weight.by.dtm If you want to weight the topic assignment of a word to the number of times the word occured, give the document term matrix for this argument
#' @return A matrix where rows are topics and columns are documents. Values represent the number of times the topic is assigned to a word in this document (essentially this is the same as the documentsums object in the output of lda.collapsed.gibbs.samler)
#' @export
documentsums <- function(m, weight.by.dtm=NULL){
  assignments = data.frame(i=m@wordassignments$i, j=m@wordassignments$j, v=m@wordassignments$v)
  if(!is.null(weight.by.dtm)){
    dtm = weight.by.dtm[m@documents,m@terms]
    dtm = data.frame(i=dtm$i, j=dtm$j, count=dtm$v)
    assignments = merge(assignments, dtm, by=c('i','j'), all.x=T)
    docsums = reshape2::acast(assignments, v ~ i, value.var='count', fun.aggregate=sum)
  } else docsums = reshape2::acast(assignments, v ~ i, value.var='j', fun.aggregate=length)
  docsums
}

#' Get the topics per document
#'
#' Return a data frame containing article ids and topic occurence per document. Topic occurence can either be based on the posterior distrition (default) or based on wordassignments (as.wordassignments = T).
#'
#' @param m A LDA model from the topicmodels package
#' @param as.wordassignments If True, return the topic occurence per document as the number of words assigned to the topic (instead of the posterior distribution)
#' @return A data frame with rows corresponding to the terms in dtm and the statistics in the columns
#' @export
topics_per_document <- function(m, as.wordassignments=F) {
  ids = as.numeric(m@documents)
  if(as.wordassignments) {
    tpd = t(documentsums(m))
  } else{
    tpd = topicmodels::posterior(m)$topics
  }
  cbind(id=ids, data.frame(tpd))
}
