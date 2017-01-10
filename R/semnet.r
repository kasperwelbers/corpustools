#' Create a semantic network based on the co-occurence of words in documents
#'
#' This function calculates the co-occurence of words and returns a network/graph where nodes are words and edges represent the similarity/adjacency of words. Co-occurence is calcuated based on how often two words occured within the same document (e.g., news article, chapter, paragraph, sentence). Note that the cooc_window() function can be used to calculate co-occurrence of words within a given word distance.
#'
#' @param tc
#'
#' @param feature
#' @param measure
#' @param context_level
#' @param chi2
#' @param alpha
#'
#' @export
cooccurrence <- function(tc, feature, measure=c('conprob', 'con_prob_weighted', 'cosine', 'obs/exp', 'obs/exp_weighted', 'count_directed', 'count_undirected'), context_level=c('document','sentence'), chi2=F, backbone=F, alpha=2){
  tc = as.tcorpus(tc)
  feature = match.arg(feature, featurenames(tc))
  measure = match.arg(measure)
  context_level = match.arg(context_level)

  dtm = get_dtm(tc, feature, context_level)

  g = cooccurrence_graph(dtm, measure=measure, chi2=chi2, backbone = backbone, alpha=2)

  g$N.documents = nrow(dtm)
  class(g) = c('semnet', 'dtm_cooc', measure, class(g))
  g
}

#' A sliding window approach to calculate the co-occurence of words
#'
#'
#' @param tc
#' @param feature
#' @param measure
#' @param context_level
#' @param window.size
#' @param direction
#' @param chi2
#' @param backbone
#'
#' @export
cooccurrence_window <- function(tc, feature, measure=c('con_prob', 'cosine', 'count_directed', 'count_undirected'), context_level=c('document','sentence'), window.size=10, direction='<>', chi2=F, backbone=F){
  tc = as.tcorpus(tc)
  feature = match.arg(feature, featurenames(tc))
  measure = match.arg(measure)
  context_level = match.arg(context_level)

  mat = wordWindowOccurence(tc, feature, context_level, window.size, direction)

  ## window %*% window measures
  if(measure %in% c('cosine', 'count_undirected')) {
    g = cooccurrence_graph(mat$window.mat, measure=measure, chi2=chi2, backbone=backbone)
  }
  ## position %*% window measures
  if(measure %in% c('con_prob', 'count_directed')) {
    g = cooccurrence_graph(mat$position.mat, mat$window.mat, measure=measure, chi2=chi2, backbone=backbone)
  }

  g$N.documents = length(unique(get_column(tc, 'doc_id')))
  g$N.windows = nrow(mat$position.mat)
  class(g) = c('semnet', 'window_cooc', measure, class(g))
  g
}

DocumentTermMatrix_to_dgTMatrix <- function(dtm){
  sm = spMatrix(nrow(dtm), ncol(dtm), dtm$i, dtm$j, dtm$v)
  rownames(sm) = rownames(dtm)
  colnames(sm) = colnames(dtm)
  as(sm, 'dgTMatrix')
}

#####
aggCoOc <- function(x, position.mat, window.mat){
  cooc = position.mat[,x] & window.mat
  cooc = as(cooc, 'lgTMatrix')
  cooc = data.frame(x=x, y=cooc@j+1, context=cooc@i+1, weight=cooc@x)
  cooc = cooc[!cooc$x == cooc$y,]
  plyr::ddply(cooc, .(x,y,context), summarize, weight=sum(weight))
}

calculateAdjacencyPerContext <- function(position.mat, window.mat) {
  adj = plyr::ldply(1:ncol(position.mat), function(x) aggCoOc(x, position.mat, window.mat))
  adj$context = rownames(position.mat)[adj$context]
  adj$x = as.factor(colnames(position.mat)[adj$x])
  adj$y = as.factor(colnames(position.mat)[adj$y])
  adj
}
