#' Visualize a semnet network
#'
#' plot_semnet is a wrapper for the plot.igraph() function optimized for plotting a semantic network of the "semnet" class.
#'
#' Before plotting the network, the set_network_attributes() function is used to set pretty defaults for plotting. Optionally, reduce_labeloverlap can be used to prevent labeloverlap (as much as possible).
#'
#' @param g A network in the igraph format. Specifically designed for the output of coOccurenceNetwork() and windowedCoOccurenceNetwork()
#' @param weight_attr The name of the weight attribute. Default is 'weight'
#' @param min_weight The minimum weight. All edges with a lower weight are dropped
#' @param delete_isolates If TRUE, isolate vertices (also after applying min_weight) are dropped
#' @param vertexsize_attr a character string indicating a vertex attribute that represents size. Default is 'freq', which is created in the coOccurenceNetwork functions to indicate the number of times a token occured.
#' @param vertexsize_coef a coefficient for changing the vertex size.
#' @param vertexcolor_attr a character string indicating a vertex attribute that represents color. The attribute can also be a numeric value (e.g., a cluster membership) in which case colors are assigned to numbers. If no (valid) color attribute is given, vertex color are based on undirected fastgreedy.community() clustering.
#' @param edgewidth_coef a coefficient for changing the edge width
#' @param max_backbone_alpha If g has an edge attribute named alpha (added if backbone extraction is used), this specifies the maximum alpha value.
#' @param labelsize_coef a coefficient for increasing or decreasing the size of the vertexlabel.
#' @param labelspace_coef a coefficient that roughly determines the minimal distance between vertex labels, based on the size of labels. Only used if reduce_labeloverlap is TRUE.
#' @param reduce_labeloverlap if TRUE, an algorithm is used to reduce overlap as best as possible.
#' @param redo_layout If TRUE, a new layout will be calculated using layout_with_fr(). If g does not have a layout attribute (g$layout), a new layout is automatically calculated.
#' @param return_graph if TRUE, plot_semnet() also returns the graph object with the attributes and layout as shown in the plot.
#' @param ... additional arguments are passed on to plot.igraph()
#' @param vertex.label.dist The distance of the label to the center of the vertex
#' @param layout_fun The igraph layout function that is used.
#'
#' @return Plots a network, and returns the network object if return_graph is TRUE.
#' @examples
#' tc = create_tcorpus(sotu_texts, doc_column = 'id')
#' tc$preprocess('token','feature', remove_stopwords = TRUE, use_stemming = TRUE, min_docfreq=10)
#' \donttest{
#' g = tc$semnet_window('feature', window.size = 10)
#' g = backbone_filter(g, max_vertices = 100)
#' plot_semnet(g)
#' }
#' @export
plot_semnet <- function(g, weight_attr='weight', min_weight=NA, delete_isolates=F, vertexsize_attr='freq', vertexsize_coef=1, vertexcolor_attr=NA, edgewidth_coef=1, max_backbone_alpha=NA, labelsize_coef=1, labelspace_coef=1.1, reduce_labeloverlap=F, redo_layout=F, return_graph=T, vertex.label.dist=0.25, layout_fun=igraph::layout_with_fr, ...){
  ## add: out_r, jpg_file, pdf_file, width, height
  ## add seed!!  default to random number, save as attribute


  if (igraph::ecount(g) > 0) {
    igraph::E(g)$weight = igraph::get.edge.attribute(g, weight_attr)
    if (!is.na(min_weight)) g = igraph::delete.edges(g, which(igraph::E(g)$weight < min_weight))
    if (delete_isolates) g = igraph::delete.vertices(g, which(igraph::degree(g) == 0))
  }
  if (igraph::vcount(g) == 0) {
    igraph::plot.igraph(g, ...)
    if (return_graph) return(g) else return(NULL)
  }

  if (!is.na(max_backbone_alpha)) {
    if (!'alpha' %in% igraph::edge_attr_names(g)) igraph::E(g)$alpha = backbone_alpha(g)
    g = igraph::delete.edges(g, which(igraph::E(g)$alpha > max_backbone_alpha))
  }

  g = set_network_attributes(g, vertexsize_attr, vertexcolor_attr, redo_layout = redo_layout, edgewidth_coef=edgewidth_coef, layout_fun=layout_fun)
  igraph::V(g)$size = vertexsize_coef * igraph::V(g)$size
  igraph::V(g)$label.cex = labelsize_coef * igraph::V(g)$label.cex
  igraph::V(g)$label.dist = vertex.label.dist

  if (reduce_labeloverlap){
    g = reduce_label_overlap(g, labelspace_coef, cex_from_device = T)
  }
  g = plot_args_as_attributes(g, args=list(...))

  igraph::plot.igraph(g, ...)
  if (return_graph) return(invisible(g))
}

plot_args_as_attributes <- function(g, args){
  if (length(args) == 0) return(g)
  for(i in 1:length(args)){
    name = names(args)[i]
    if (!grepl('vertex\\.|edge\\.', name)) g = igraph::set.graph.attribute(g, name, value=args[[i]])
    if (grepl('vertex\\.', name)) g = igraph::set.vertex.attribute(g, gsub('vertex\\.', '', name), value=args[[i]])
    if (grepl('edge\\.', name)) g = igraph::set.edge.attribute(g, gsub('edge\\.', '', name), value=args[[i]])
  }
  g
}

#' Set some default network attributes for pretty plotting
#'
#' The purpose of this function is to create some default network attribute options to plot networks in a nice and insightfull way.
#'
#' @param g A graph in the Igraph format.
#' @param color_attribute the name of the attribute that is used to select the color
#' @param redo_layout if TRUE, force new layout if layout already exists as a graph attribute
#' @param edgewidth_coef A coefficient for changing the edge width
#' @param layout_fun THe igraph layout function used
#' @param size_attribute the name of the vertex attribute to be used to set the size of nodes
#'
#' @return a network in the Igraph format
#' @examples
#' tc = create_tcorpus(c('A B C', 'B C', 'B D'))
#' g = tc$semnet('token')
#'
#' igraph::get.edge.attribute(g)
#' igraph::get.vertex.attribute(g)
#' \donttest{plot(g)}
#' g = set_network_attributes(g, size_attribute = 'freq')
#' igraph::get.edge.attribute(g)
#' igraph::get.vertex.attribute(g)
#' \donttest{plot(g)}
#' @export
set_network_attributes <- function(g, size_attribute='freq', color_attribute=NA, redo_layout=F, edgewidth_coef=1, layout_fun=igraph::layout_with_fr){
  g = set_vertex_attributes(g, size_attribute, color_attribute)
  if (igraph::ecount(g) > 0) g = set_edge_attributes(g, edgewidth_coef)
  if (is.null(g$layout) | redo_layout) g$layout = layout_fun(g)
  g
}

set_vertex_colors <- function(g, color){
  if (!is.null(color)){
    if (class(color) == 'numeric'){
      pal = substr(grDevices::rainbow(length(unique(color)), s=0.5,alpha=0.5), 1,7)
      duplicates = unique(color[duplicated(color)])
      color = match(color, duplicates) # re-index colors, and setting isolates to NA
      igraph::V(g)$color = ifelse(is.na(color), '#AEAEAE', pal[color])
    } else {
      igraph::V(g)$color = color
    }
  } else {
    igraph::V(g)$color = 'cadetblue1'
  }
  igraph::V(g)$frame.color = igraph::V(g)$color
  g
}

set_vertex_attributes <- function(g, size, color){
  vattrs = names(igraph::vertex.attributes(g))
  if (is.na(color) | !color %in% vattrs) {
    color = igraph::fastgreedy.community(igraph::as.undirected(g))$membership
    #message('No (valid) color attribute given. Vertex color now based on undirected fastgreedy.community() clustering')
  } else {
    color = unlist(igraph::get.vertex.attribute(g, color))
  }
  g = set_vertex_colors(g, color)

  if (is.na(size) | !size %in% vattrs) {
    size = rep(1, igraph::vcount(g))
    message('No (valid) size attribute given. Vertex size now set to 1')
  } else {
    size = igraph::get.vertex.attribute(g, size)
  }

  if (!'size' %in% names(igraph::vertex.attributes(g))) igraph::V(g)$size= rescale_var(size^0.4, 2, 10)
  igraph::V(g)$label.color = 'black'

  igraph::V(g)$label.cex = rescale_var(size^0.4, 0.8, 1)
  igraph::V(g)$label = igraph::V(g)$name
  g
}


set_edge_attributes <- function(g, edgewidth_coef){
  igraph::E(g)$width = rescale_var(igraph::E(g)$weight, 1, 10) * edgewidth_coef
  eattrs = names(igraph::edge.attributes(g))
  if (!'arrow.size' %in% eattrs) igraph::E(g)$arrow.size= 0.00001
  if (!'color' %in% eattrs) igraph::E(g)$color='lightgrey'
  g
}


rescale_var <- function(x, new_min=0, new_max=1, x_min=min(x), x_max=max(x)){
  if (x_min == x_max) return(x)
  x = (x - x_min) / (x_max - x_min) # normalize
  x = x * (new_max-new_min)
  return(x + new_min)
}

reduce_label_overlap <- function(g, labelspace_coef=1.1, cex_from_device=F, label.attr='label', labelsize.attr='label.cex', rstep=0.01, tstep=0.2){
  layout_matrix = igraph::layout.norm(g$layout)

  vnames = names(igraph::vertex.attributes(g))
  if (!label.attr %in% vnames) {
    stop('"', label.attr, '" is not a valid vertex attribute)')
  } else {
    label = igraph::get.vertex.attribute(g, label.attr)
  }

  if (labelsize.attr %in% vnames){
    label.cex = igraph::get.vertex.attribute(g, labelsize.attr)
  } else {
    message('"', labelsize.attr, '" is not a valid vertex attribute). Labelsize is set to 1')
    label.cex = 1
  }

  ord = order(-label.cex) ## reorder so that smallest tokens are relocated first
  layout_matrix = layout_matrix[ord,]
  label.cex = label.cex[ord]
  label = label[ord]


  graphics::plot(layout_matrix, axes = F, frame.plot = F, xlab='', ylab='', type='n', xlim = c(-1,1), ylim=c(-1,1))
  newlayout = wordcloud::wordlayout(layout_matrix[,1], layout_matrix[,2], label, cex=label.cex*labelspace_coef, rstep = rstep, tstep=tstep, xlim = c(-1,1), ylim=c(-1,1))

  newlayout = newlayout[match(1:nrow(newlayout), ord),] # return original order
  #g = set.vertex.attribute(g, labelsize.attr, value=newlayout[,'newcex'])
  g$layout = as.matrix(newlayout[,1:2])
  g
}
