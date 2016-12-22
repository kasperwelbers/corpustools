#' Create a semantic network based on the co-occurence of words in documents
#'
#' This function calculates the co-occurence of words and returns a network/graph where nodes are words and edges represent the similarity/adjacency of words. Co-occurence is calcuated based on how often two words occured within the same document (e.g., news article, chapter, paragraph, sentence). Note that the cooc_window() function can be used to calculate co-occurrence of words within a given word distance.
#'
#' @param tc A tCorpus object
#' @param measure the measure to calculate adjacency. Options are "cooccurrence" (number of documents), "cosine" (cosine similarity), "conprob" (conditional probability) or "conprob_weighted". Of these "cooccurrence" and "cosine" are symetric (word1 -> word2 is equal to word2 -> word1) whereas "conprob" is assymetric. "conprob_weighted" is an extension of "conprob" that takes into account how often words occur in documents, by first transforming the number of times the word occurs into a probability.
#' @param feature.col Optional, a character string indicating the column of the tokenlist data that is used to represent the token feature (e.g., words, lemma, ngrams). If NULL, the feature.col slot of the tokenlist object will be used.
#' @return A graph in the Igraph format in which edges represent the adjacency of terms
#' @export
cooccurrence <- function(tc, feature, measure=c('cooccurrence','cosine','conprob','conprob_weighted'), context_level=c('document','sentence')){
  tc = as.tcorpus(tc)
  feature = match.arg(feature, featurenames(tc))
  measure = match.arg(measure)
  x = getDTM(tc, feature, context_level)

  if(measure == 'cosine') {
    mat = as(getCosine(x), 'dgCMatrix')
    g = igraph::graph.adjacency(mat, mode='upper', diag=F, weighted=T)
  }
  if(measure == 'cooccurrence') {
    mat = getcooccurrence(x)
    g = igraph::graph.adjacency(mat, mode='upper', diag=F, weighted=T)
  }
  if(measure == 'conprob') {
    mat = getConditionalProbability(x, weighted=F)
    g = igraph::graph.adjacency(mat, mode='directed', diag=F, weighted=T)
  }
  if(measure == 'conprob_weighted') {
    mat = getConditionalProbability(x, weighted=T)
    g = igraph::graph.adjacency(mat, mode='directed', diag=F, weighted=T)
  }

  igraph::V(g)$freq = Matrix::colSums(x)
  g = igraph::set.edge.attribute(g, measure, value=igraph::E(g)$weight)
  class(g) = c('semnet',class(g))
  g
}

#' A sliding window approach to calculate the co-occurence of words
#'
#' @param tokenlist
#' @param window.size The distance within which words should occur from each other to be counted as a co-occurence.
#' @param measure
#' @param filter
#' @param direction a string indicating whether only the left ('<') or right ('>') side of the window, or both ('<>'), should be used.
#'
#' @return An edgelist (data.frame) with columns x, y and weight, in which weight represents the number of times y occured within a [window.size] word distance from x.
#' @export
cooccurrence_window <- function(tc, feature, context_level=c('document','sentence'), window.size=10, direction='<>'){
  mat = wordWindowOccurence(tc, feature, context_level, window.size, direction)
  calculateAdjacency(mat$position.mat, mat$window.mat)
}

#g = cooccurrence_window(tc, 'word')
#plot.semnet(g)
#plot(g)

dtmToSparseMatrix <- function(dtm){
  sm = spMatrix(nrow(dtm), ncol(dtm), dtm$i, dtm$j, dtm$v)
  rownames(sm) = rownames(dtm)
  colnames(sm) = colnames(dtm)
  sm
}

getcooccurrence <- function(m1, m2=NULL){
  m1@x[m1@x > 0] = 1
  if(!is.null(m2)){
    m2@x[m2@x > 0] = 1
    mat = Matrix::crossprod(m1,m2)
  } else mat = Matrix::crossprod(m1)
  mat@x[is.na(mat@x)] = 0
  mat
}

getConditionalProbability <- function(m1, m2=NULL, weighted=F, alpha=2){
  getProb <- function(x, alpha) 1 - ((1/alpha) ^ x)

  m1@x[m1@x > 0] = if(weighted) getProb(m1@x[m1@x > 0], alpha) else 1
  if(!is.null(m2)){
    m2@x[m2@x > 0] = if(weighted) getProb(m2@x[m2@x > 0], alpha) else 1
    mat = Matrix::crossprod(m1,m2)/Matrix::colSums(m1)
  } else mat = Matrix::crossprod(m1)/Matrix::colSums(m1)
  mat@x[is.na(mat@x)] = 0
  mat
}

getCosine <- function(m1, m2=NULL){
  norm = sqrt(Matrix::colSums(m1^2))
  m1@x = m1@x / norm[m1@j+1]
  if(!is.null(m2)){
    norm = sqrt(Matrix::colSums(m2^2))
    m2@x = m2@x / norm[m2@j+1]
    cp = Matrix::crossprod(m1,m2)
  } else cp = Matrix::crossprod(m1)
  cp
}

calculateAdjacency <- function(position.mat, window.mat){
  adj = Matrix::crossprod(position.mat, window.mat)
  g = graph.adjacency(adj, mode='directed', weighted=T, diag=F)
  V(g)$freq = Matrix::colSums(position.mat)
  E(g)$cooccurrence = E(g)$weight
  E(g)$weight_pct = E(g)$weight / V(g)$freq[get.edgelist(g, names = F)[,1]]
  class(g) = c('semnet',class(g))
  g
}

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
