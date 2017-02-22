fit_lda <- function(dtm, k, num.iterations=500, alpha=50/k, eta=.01, burnin=250, method='Gibbs') {
  if(!require(topicmodels)) stop('The topicmodels package is required for this function, but has not yet been installed. You can run install.packages("topicmodels")')
  if(is(dtm, 'DocumentTermMatrix')) dtm = tm_dtm_to_dgTMatrix(dtm)

  empty_rows = sum(rowSums(dtm) == 0)
  empty_cols = sum(rowSums(dtm) == 0)
  if(empty_rows) message(sprintf('%s rows in the dtm are empty. These have been deleted', empty_rows))
  if(empty_cols) message(sprintf('%s columns in the dtm are empty. These have been deleted', empty_cols))

  dtm = dtm[rowSums(dtm) > 0, colSums(dtm) > 0]
  m = LDA(dtm, k=k, method=method, control=list(iter=num.iterations, burnin=burnin, alpha=alpha, delta=eta))
  m
}

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

lda_features <- function(tc, m, feature, new_feature='LDA_topic', context_level=c('document','sentence'), context_labels=T){
  context_level = match.arg(context_level)
  if(!is(m, 'LDA_Gibbs')) stop('An LDA model of class LDA_Gibbs (topicmodels::LDA with method = "Gibbs") is required')

  d = data.table(context = get_context(tc, context_level, with_labels = context_labels),
                 feature=get_column(tc, feature),
                 i = 1:n_data(tc))
  wa = data.table(context=factor(m@documents[m@wordassignments$i]),
                  feature=factor(m@terms[m@wordassignments$j]),
                  v=m@wordassignments$v)

  setkeyv(d, c('context','feature'))
  setkeyv(wa, c('context','feature'))
  d = merge(d, wa, all.x = T)
  tc = set_column(tc, new_feature, d$v[order(d$i)])
  tc
}
