
getMaxAlphaFilters <- function(g, max.vertices){
  el = get.data.frame(g)
  a = unique(data.frame(node=c(el$from,el$to), alpha=c(el$alpha,el$alpha)))
  a = a[order(a$alpha),]
  a = a[!duplicated(a$node),]
  if(nrow(a) <= max.vertices) {
    max.alpha = 1
    delete.vertices = c()
  } else {
    max.alpha = a$alpha[max.vertices]
    if(max.alpha == a$alpha[max.vertices+1]) max.alpha = max.alpha - 0.000000001
    delete.vertices = na.omit(a$node[(max.vertices+1):nrow(a)])
    delete.edges = which(el$from %in% delete.vertices | el$to %in% delete.vertices)
  }
  list(max.alpha=max.alpha, delete.edges=delete.edges)
}

filterVerticesByAlpha <- function(g, max.vertices, use.original.alpha){
  filters = getMaxAlphaFilters(g, max.vertices)

  if(filters$max.alpha < max(E(g)$alpha)) {
    message(paste('Used cutoff alpha', filters$max.alpha, 'to keep number of vertices under', max.vertices))
    if(use.original.alpha){
      message(paste('(For the edges the threshold assigned in the alpha parameter is still used)'))

      g = delete.edges(g, filters$delete.edges)
    } else {
      g = delete.edges(g, which(E(g)$alpha >= filters$max.alpha))
    }
  }
  g
}



#' Extract the backbone of a network.
#'
#' Based on the following paper: Serrano, M. Á., Boguñá, M., & Vespignani, A. (2009). Extracting the multiscale backbone of complex weighted networks. Proceedings of the National Academy of Sciences, 106(16), 6483-6488.
#'
#' @param g A graph in the `Igraph` format.
#' @param alpha The threshold for the alpha. Can be interpreted similar to a p value (see paper for clarrification).
#' @param direction direction = 'none' can be used for both directed and undirected networks, and is (supposed to be) the disparity filter proposed in Serrano et al. (2009) is used. By setting to 'in' or 'out', the alpha is only calculated for out or in edges. This is an experimental use of the backbone extraction (so beware!) but it seems a logical application.
#' @param delete.isolates If TRUE, vertices with degree 0 (i.e. no edges) are deleted.
#' @param max.vertices Optional. Set a maximum number of vertices for the network to be produced. The alpha is then automatically lowered to the point that only the given number of vertices remains connected (degree > 0). This can be usefull if the purpose is to make an interpretation friendly network. See e.g., http://jcom.sissa.it/archive/14/01/JCOM_1401_2015_A01
#' @param use.original.alpha if max.vertices is not NULL, this determines whether the lower alpha for selecting the top vertices is also used as a threshold for the edges, or whether the original value given in the alpha parameter is used.
#' @return A graph in the Igraph format
#' @export
getBackboneNetwork <- function(g, alpha=0.05, direction='none', delete.isolates=F, max.vertices=NULL, use.original.alpha=T, k.is.Nvertices=F){
  if(direction == 'none') E(g)$alpha = backbone.alpha(g, k.is.Nvertices)
  if(direction == 'in') E(g)$alpha = backbone.indegree.alpha(g, k.is.Nvertices)
  if(direction == 'out') E(g)$alpha = backbone.outdegree.alpha(g, k.is.Nvertices)
  g = delete.edges(g, which(E(g)$alpha >= alpha))
  if(!is.null(max.vertices) & ecount(g) > 0) g = filterVerticesByAlpha(g, max.vertices, use.original.alpha)
  if(delete.isolates) g = delete.vertices(g, which(degree(g) == 0))
  if(ecount(g) == 0) {
    warning("No significant edges (backbone) remain!! Accept it (or lower the backbone_alpha)")
    return(g)
  }
  g
}

calcAlpha <- function(mat, weightsum, k){
  mat@x = mat@x / weightsum[mat@i+1]
  mat@x = (1 - mat@x)^(k[mat@i+1]-1)
  mat@x[is.na(mat@x)] = 1
  mat
}

#' Calculate the alpha values that can be used to extract the backbone of a network.
#'
#' Based on the following paper: Serrano, M. Á., Boguñá, M., & Vespignani, A. (2009). Extracting the multiscale backbone of complex weighted networks. Proceedings of the National Academy of Sciences, 106(16), 6483-6488.
#'
#' @param g A graph in the `Igraph` format.
#' @return A vector of alpha values, which matches the edges. Can thus easily be made an edge attribute: E(g)$alpha = backbone.alpha(g)
#' @export
backbone.alpha <- function(g, k.is.Nvertices=F){
  mat = if(is.directed(g)) get.adjacency(g, attr='weight') else get.adjacency(g, attr='weight', type='upper') # prevents counting edges double in symmetric matrix (undirected graph)
  weightsum = Matrix::rowSums(mat) + Matrix::colSums(mat)
  k = if(k.is.Nvertices) nrow(mat) else Matrix::rowSums(mat>0) + Matrix::colSums(mat>0)
  if(is.directed(g) & k.is.Nvertices) k = k + ncol(mat)

  edgelist_ids = get.edgelist(g, names=F)
  alpha_ij = calcAlpha(mat, weightsum, k)[edgelist_ids] # alpha from the perspective of the 'from' node.
  alpha_ji = Matrix::t(calcAlpha(Matrix::t(mat), weightsum, k))[edgelist_ids] # alpha from the perspective of the 'to' node.
  alpha_ij[alpha_ji < alpha_ij] = alpha_ji[alpha_ji < alpha_ij] # select lowest alpha, because an edge can be 'significant' from the perspective of both the 'from' and 'to' node.
  alpha_ij
}

#' Calculate the alpha values that can be used to extract the backbone of a network, for only the out.degree
#'
#' Based on the following paper: Serrano, M. Á., Boguñá, M., & Vespignani, A. (2009). Extracting the multiscale backbone of complex weighted networks. Proceedings of the National Academy of Sciences, 106(16), 6483-6488.
#'
#' @param g A graph in the `Igraph` format.
#' @return A vector of alpha values, which matches the edges. Can thus easily be made an edge attribute: E(g)$alpha = backbone.alpha(g)
#' @export
backbone.outdegree.alpha <- function(g, k.is.Nvertices=F){
  mat = get.adjacency(g, attr='weight')
  weightsum = Matrix::rowSums(mat)
  k = if(k.is.Nvertices) nrow(mat) else Matrix::rowSums(mat > 0)
  edgelist_ids = get.edgelist(g, names=F)
  calcAlpha(mat, weightsum, k)[edgelist_ids]
}

#' Calculate the alpha values that can be used to extract the backbone of a network, for only the in.degree
#'
#' Based on the following paper: Serrano, M. Á., Boguñá, M., & Vespignani, A. (2009). Extracting the multiscale backbone of complex weighted networks. Proceedings of the National Academy of Sciences, 106(16), 6483-6488.
#'
#' @param g A graph in the `Igraph` format.
#' @return A vector of alpha values, which matches the edges. Can thus easily be made an edge attribute: E(g)$alpha = backbone.alpha(g)
#' @export
backbone.indegree.alpha <- function(g, k.is.Nvertices=F){
  mat = get.adjacency(g, attr='weight')
  weightsum = Matrix::colSums(mat)
  k = if(k.is.Nvertices) nrow(mat) else Matrix::colSums(mat > 0)
  edgelist_ids = get.edgelist(g, names=F)
  Matrix::t(calcAlpha(Matrix::t(mat), weightsum, k))[edgelist_ids]
}
