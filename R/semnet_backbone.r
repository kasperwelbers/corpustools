#' Extract the backbone of a network.
#'
#' Based on the following paper: Serrano, M. A., Boguna, M., & Vespignani, A. (2009). Extracting the multiscale backbone of complex weighted networks. Proceedings of the National Academy of Sciences, 106(16), 6483-6488.
#'
#' @param g A graph in the `Igraph` format.
#' @param alpha The threshold for the alpha. Can be interpreted similar to a p value (see paper for clarrification).
#' @param direction direction = 'none' can be used for both directed and undirected networks, and is (supposed to be) the disparity filter proposed in Serrano et al. (2009) is used. By setting to 'in' or 'out', the alpha is only calculated for out or in edges. This is an experimental use of the backbone extraction (so beware!) but it seems a logical application.
#' @param delete_isolates If TRUE, vertices with degree 0 (i.e. no edges) are deleted.
#' @param max_vertices Optional. Set a maximum number of vertices for the network to be produced. The alpha is then automatically lowered to the point that only the given number of vertices remains connected (degree > 0). This can be usefull if the purpose is to make an interpretation friendly network. See e.g., http://jcom.sissa.it/archive/14/01/JCOM_1401_2015_A01
#' @param use_original_alpha if max_vertices is not NULL, this determines whether the lower alpha for selecting the top vertices is also used as a threshold for the edges, or whether the original value given in the alpha parameter is used.
#' @param k_is_n the disparity filter method for backbone extraction uses the number of existing edges (k) for each node, which can be arbitraty if there are many very weak ties, which is often the case in a co-occurence network. By setting k_is_n to TRUE, it is 'assumed' that all nodes are connected, which makes sense from a language model perspective (i.e. probability for co-occurence is never zero)
#' @return A graph in the Igraph format
#'
#' @examples
#' \donttest{
#' tc = create_tcorpus(sotu_texts, doc_column = 'id')
#' tc$preprocess('token','feature', remove_stopwords = TRUE, use_stemming = TRUE, min_docfreq = 10)
#'
#' g = semnet_window(tc, 'feature', window.size = 10)
#' igraph::vcount(g)
#' igraph::ecount(g)
#' gb = backbone_filter(g, max_vertices = 100)
#' igraph::vcount(gb)
#' igraph::ecount(gb)
#' plot_semnet(gb)
#' }
#' @export
backbone_filter <- function(g, alpha=0.05, direction='none', delete_isolates=T, max_vertices=NULL, use_original_alpha=T, k_is_n=F){
  if (direction == 'none') igraph::E(g)$alpha = backbone_alpha(g, k_is_n)
  if (direction == 'in') igraph::E(g)$alpha = backbone_indegree_alpha(g, k_is_n)
  if (direction == 'out') igraph::E(g)$alpha = backbone_outdegree_alpha(g, k_is_n)
  g = igraph::delete.edges(g, which(igraph::E(g)$alpha >= alpha))
  if (!is.null(max_vertices) && igraph::ecount(g) > 0) g = filter_vertices_by_edgeweight(g, 'alpha', max_vertices, use_original_alpha)
  if (delete_isolates) g = igraph::delete.vertices(g, which(igraph::degree(g) == 0))
  if (igraph::ecount(g) == 0) {
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

backbone_alpha <- function(g, k_is_n=F){
  if (igraph::has.multiple(g)) stop('The corpustools implementation of backbone extraction does not support parallel edges (i.e. having multiple edges between the same nodes with the same direction). If summing multiple edges is OK with you, you can use Igraphs simplify() function')

  mat = igraph::get.adjacency(g, attr='weight')
  edgelist_ids = igraph::get.edgelist(g, names=F)

  if(!igraph::is.directed(g)) {
    mat[lower.tri(mat)] = 0 # prevents counting edges double in symmetric matrix (undirected graph)
    weightsum_ij = weightsum_ji = Matrix::rowSums(mat) + Matrix::colSums(mat)
    k_ij = k_ji = if(k_is_n) nrow(mat) else Matrix::rowSums(mat>0) + Matrix::colSums(mat>0)
  } else {
    weightsum_ij = Matrix::rowSums(mat)
    weightsum_ji = Matrix::colSums(mat)
    k_ij = if(k_is_n) nrow(mat) else Matrix::rowSums(mat>0)
    k_ji = if(k_is_n) ncol(mat) else Matrix::colSums(mat>0)
  }
  alpha_ij = calcAlpha(mat, weightsum_ij, k_ij)[edgelist_ids] # alpha from the perspective of the 'from' node.
  alpha_ji = Matrix::t(calcAlpha(Matrix::t(mat), weightsum_ji, k_ji))[edgelist_ids] # alpha from the perspective of the 'to' node.
  ifelse(alpha_ij < alpha_ji, alpha_ij, alpha_ji) # select lowest alpha, because an edge can be 'significant' from the perspective of both the 'from' and 'to' node.

}

backbone_outdegree_alpha <- function(g, k_is_n=F){
  if (igraph::has.multiple(g)) stop('The corpustools implementation of backbone extraction does not support parallel edges (i.e. having multiple edges between the same nodes with the same direction). If summing multiple edges is OK with you, you can use Igraphs simplify() function')

  mat = igraph::get.adjacency(g, attr='weight')
  weightsum = Matrix::rowSums(mat)
  k = if (k_is_n) nrow(mat) else Matrix::rowSums(mat > 0)
  edgelist_ids = igraph::get.edgelist(g, names=F)
  calcAlpha(mat, weightsum, k)[edgelist_ids]
}

backbone_indegree_alpha <- function(g, k_is_n=F){
  if (igraph::has.multiple(g)) stop('The corpustools implementation of backbone extraction does not support parallel edges (i.e. having multiple edges between the same nodes with the same direction). If summing multiple edges is OK with you, you can use Igraphs simplify() function')

  mat = igraph::get.adjacency(g, attr='weight')
  weightsum = Matrix::colSums(mat)
  k = if (k_is_n) nrow(mat) else Matrix::colSums(mat > 0)
  edgelist_ids = igraph::get.edgelist(g, names=F)
  Matrix::t(calcAlpha(Matrix::t(mat), weightsum, k))[edgelist_ids]
}

get_max_alpha_filters <- function(g, weight_attr, max_vertices){
  el = igraph::get.data.frame(g)
  a = unique(data.frame(node=c(el$from,el$to), weight=c(el[[weight_attr]],el[[weight_attr]])))
  a = a[order(a$weight),]
  a = a[!duplicated(a$node),]
  if (nrow(a) <= max_vertices) {
    max.weight = 1
    delete.vertices = c()
  } else {
    max.weight = a$weight[max_vertices]
    if (max.weight == a$weight[max_vertices+1]) max.weight = max.weight - 0.000000001
    delete.vertices = stats::na.omit(a$node[(max_vertices+1):nrow(a)])
    delete.edges = which(el$from %in% delete.vertices | el$to %in% delete.vertices)
  }
  list(max.weight=max.weight, delete.edges=delete.edges)
}

filter_vertices_by_edgeweight <- function(g, weight_attr, max_vertices, keep.original.weight, delete_isolates=F){
  filters = get_max_alpha_filters(g, weight_attr, max_vertices)

  if (filters$max.weight < max(igraph::E(g)$weight)) {
    message(paste('Used cutoff edge-weight', filters$max.weight, 'to keep number of vertices under', max_vertices))
    if (keep.original.weight){
      message(paste('(For the edges the original weight is still used)'))
      g = igraph::delete.edges(g, filters$delete.edges)
    } else {
      g = igraph::delete.edges(g, which(igraph::get.edge.attribute(g, weight_attr) >= filters$max.weight))
    }
  }
  if (delete_isolates) g = igraph::delete.vertices(g, which(igraph::degree(g) == 0))
  g
}
