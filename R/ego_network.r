#' Create an ego network
#'
#' Create an ego network from an igraph object.
#'
#' The function is similar to the ego function in igraph, but with some notable differences. Firstly, if multiple vertex_names are given, the ego network for both is given in 1 network (whereas igraph creates a list of networks). Secondly, the min_weight and top_edges parameters can be used to focus on the strongest edges.
#'
#' @param g an igraph object
#' @param vertex_names a character string with the names of the ego vertices/nodes
#' @param depth the number of degrees from the ego vertices/nodes that are included. 1 means that only the direct neighbours are included
#' @param only_filter_vertices if True, the algorithm will only filter out vertices/nodes that are not in the ego network. If False (default) then it also filters out the edges.
#' @param weight_attr the name of the edge attribute. if NA, no weight is used, and min_weight and top_edges are ignored
#' @param min_weight a number indicating the minimum weight
#' @param top_edges for each vertex within the given depth, only keep the top n edges with the strongest edge weight. Can also be a vector of the same length as the depth value, in which case a different value is used at each level: first value for level 1, second value for level 2, etc.
#' @param max_edges_level the maximum number of edges to be added at each level of depth.
#' @param directed if the network is directed, specify whether 'out' degrees or 'in' degrees are used
#'
#' @examples
#' tc = create_tcorpus(c('a b c', 'd e f', 'a d'))
#' g = tc$semnet('token')
#'
#' igraph::get.data.frame(g)
#' \donttest{plot_semnet(g)}
#
#' ## only keep nodes directly connected to given node
#' g_ego = ego_semnet(g, 'e')
#' igraph::get.data.frame(g_ego)
#' \donttest{plot_semnet(g_ego)}
#'
#' ## only keep edges directly connected to given node
#' g_ego = ego_semnet(g, 'e', only_filter_vertices = FALSE)
#' igraph::get.data.frame(g_ego)
#' \donttest{plot_semnet(g_ego)}
#'
#' ## only keep nodes connected to given node with a specified degree (i.e. distance)
#' g_ego = ego_semnet(g, 'e', depth = 2)
#' igraph::get.data.frame(g_ego)
#' \donttest{plot_semnet(g_ego)}
#' @export
ego_semnet <- function(g, vertex_names, depth=1, only_filter_vertices=T, weight_attr='weight', min_weight=NULL, top_edges=NULL, max_edges_level=NULL, directed=c('out','in')){
  directed = match.arg(directed)

  missing = vertex_names[!vertex_names %in% igraph::V(g)$name]
  if (length(missing) == length(vertex_names)) stop('None of the given vertex_names exist in g')
  if (length(missing) > 0) warning(sprintf('Some of the given vertex_names do not exist in g: [%s]', paste(missing, collapse=', ')))

  igraph::delete.edges(g, igraph::E(g))
  if (!is.na(weight_attr)) {
    adj = igraph::get.adjacency(g, type='both', attr = weight_attr)
  } else {
    adj = igraph::get.adjacency(g, type='both')
    min_weight = NA; top_edges = NA
  }
  adj = methods::as(adj, 'dgTMatrix')
  if (igraph::is.directed(g)){
    if (directed == 'out') dt = summary(adj)
    if (directed == 'in') dt = summary(t(adj))
  } else {
    dt = summary(adj)
  }
  vnames = fast_factor(colnames(adj))
  dt = data.table(x=dt$i, y=dt$j, weight=dt$x, key='x') ## as data.table

  vertex_ids = which(igraph::V(g)$name %in% vertex_names)
  ego = build_ego_network(dt, vertex_ids, level=1, depth=depth, min_weight=min_weight, top_edges=top_edges, max_edges_level=max_edges_level)
  if (only_filter_vertices){
    i = unique(c(ego$x, ego$y))
    g = igraph::delete.vertices(g, which(!1:igraph::vcount(g) %in% i))
  } else {
    i = igraph::get.edge.ids(g, vp=rbind(ego$x, ego$y))
    g = igraph::delete_edges(g, which(!1:igraph::ecount(g) %in% i))
    g = igraph::delete_vertices(g, which(igraph::degree(g) == 0))
  }
  g
}

build_ego_network <- function(dt, vertex_ids, level, depth, min_weight, top_edges, max_edges_level){
  ego = dt[list(vertex_ids),]
  if (!is.null(min_weight)) ego = ego[ego$weight >= min_weight,]
  if (!is.null(top_edges)){
    thres = if (length(top_edges) == depth) top_edges[depth] else top_edges
    ego = ego[order(ego$x, -ego$weight)]
    top = local_position(1:nrow(ego), ego$x)
    ego = ego[top <= thres,]
  }
  if (!is.null(max_edges_level)) ego = head(ego[order(-ego$weight)], max_edges_level)

  new_vertex_ids = unique(ego$y)
  new_vertex_ids = new_vertex_ids[!new_vertex_ids %in% vertex_ids]
  if (level < depth) ego = rbind(ego, build_ego_network(dt, vertex_ids=new_vertex_ids,
                                                           level=level+1, depth=depth,
                                                           min_weight=min_weight, top_edges=top_edges,
                                                           max_edges_level=max_edges_level)) ## build ego network recursively
  ego
}
