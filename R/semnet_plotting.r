#' Visualize a semnet network
#'
#' plot_semnet is a wrapper for the plot.igraph() function optimized for plotting a semantic network of the "semnet" class.
#'
#' Before plotting the network, the setNetworkAttributes() function is used to set pretty defaults for plotting. Optionally, reduce_labeloverlap can be used to prevent labeloverlap (as much as possible).
#'
#' @param g A network in the igraph format. Specifically designed for the output of coOccurenceNetwork() and windowedCoOccurenceNetwork()
#' @param vertexsize_attr a character string indicating a vertex attribute that represents size. Default is 'freq', which is created in the coOccurenceNetwork functions to indicate the number of times a word occured.
#' @param vertexcolor_attr a character string indicating a vertex attribute that represents color. The attribute can also be a numeric value (e.g., a cluster membership) in which case colors are assigned to numbers. If no (valid) color attribute is given, vertex color are based on undirected fastgreedy.community() clustering.
#' @param labelsize_coef a coefficient for increasing or decreasing the size of the vertexlabel.
#' @param labelspace_coef a coefficient that roughly determines the minimal distance between vertex labels, based on the size of labels. Only used if reduce_labeloverlap is TRUE.
#' @param reduce_labeloverlap if TRUE, an algorithm is used to reduce overlap as best as possible.
#' @param redo_layout If TRUE, a new layout will be calculated using layout_with_fr(). If g does not have a layout attribute (g$layout), a new layout is automatically calculated.
#' @param return_graph if TRUE, plot_semnet() also returns the graph object with the attributes and layout as shown in the plot.
#' @param ... additional arguments are passed on to plot.igraph()
#'
#' @return Plots a network, and returns the network object if return_graph is TRUE.
#' @export
plot_semnet <- function(g, weight_attr='weight', min_weight=NA, delete_isolates=F, vertexsize_attr='freq', vertexsize_coef=1, vertexcolor_attr=NA, edgewidth_coef=1, max_backbone_alpha=NA, labelsize_coef=1, labelspace_coef=1.1, reduce_labeloverlap=F, redo_layout=F, return_graph=T, vertex.label.dist=0.25, layout_fun=layout_with_fr, ...){
  ## add: out_r, jpg_file, pdf_file, width, height
  ## add seed!!  default to random number, save as attribute

  E(g)$weight = get.edge.attribute(g, weight_attr)
  if (!is.na(min_weight)) g = delete.edges(g, which(E(g)$weight < min_weight))
  if (delete_isolates) g = delete.vertices(g, which(degree(g) == 0))
  if (vcount(g) == 0) {
    plot.igraph(g, ...)
    if (return_graph) return(g) else return(NULL)
  }

  if (!is.na(max_backbone_alpha)) {
    if (!'alpha' %in% edge_attr_names(g)) E(g)$alpha = backbone.alpha(g)
    g = delete.edges(g, which(E(g)$alpha > max_alpha))
  }

  g = setNetworkAttributes(g, vertexsize_attr, vertexcolor_attr, redo_layout = redo_layout, edgewidth_coef=edgewidth_coef, layout_fun=layout_fun)
  V(g)$size = vertexsize_coef * V(g)$size
  V(g)$label.cex = labelsize_coef * V(g)$label.cex
  V(g)$label.dist = vertex.label.dist

  if (reduce_labeloverlap){
    g = reduceLabelOverlap(g, labelspace_coef, cex_from_device = T)
  }
  g = plotArgsToAttributes(g, args=list(...))

  plot.igraph(g, ...)
  if (return_graph) return(g)
}

plotArgsToAttributes <- function(g, args){
  if (length(args) == 0) return(g)
  for(i in 1:length(args)){
    name = names(args)[i]
    if (!grepl('vertex\\.|edge\\.', name)) g = set.graph.attribute(g, name, value=args[[i]])
    if (grepl('vertex\\.', name)) g = set.vertex.attribute(g, gsub('vertex\\.', '', name), value=args[[i]])
    if (grepl('edge\\.', name)) g = set.edge.attribute(g, gsub('edge\\.', '', name), value=args[[i]])
  }
  g
}

#' Set some default network attributes for pretty plotting
#'
#' The purpose of this function is to create some default network attribute options to plot networks in a nice and insightfull way.
#'
#' @param g A graph in the Igraph format.
#' @param size_attribute the name of the vertex attribute to be used to set the size of nodes
#' @param if TRUE, isolates are placed next to the network
#' @return a network in the Igraph format
#' @export
setNetworkAttributes <- function(g, size_attribute='freq', color_attribute=NA, redo_layout=F, edgewidth_coef=1, layout_fun=layout_with_fr){
  g = setVertexAttributes(g, size_attribute, color_attribute)
  g = setEdgeAttributes(g, edgewidth_coef)
  if (is.null(g$layout) | redo_layout) g$layout = layout_fun(g)
  g
}

setVertexColors <- function(g, color){
  if (!is.null(color)){
    if (class(color) == 'numeric'){
      pal = substr(rainbow(length(unique(color)), s=0.6,alpha=0.5), 1,7)
      duplicates = unique(color[duplicated(color)])
      color = match(color, duplicates) # re-index colors, and setting isolates to NA
      V(g)$color = ifelse(is.na(color), '#AEAEAE', pal[color])
    } else {
      V(g)$color = color
    }
  } else {
    V(g)$color = 'cadetblue1'
  }
  V(g)$frame.color = V(g)$color
  g
}

setVertexAttributes <- function(g, size, color){
  vattrs = names(vertex.attributes(g))
  if (is.na(color) | !color %in% vattrs) {
    color = fastgreedy.community(as.undirected(g))$membership
    #message('No (valid) color attribute given. Vertex color now based on undirected fastgreedy.community() clustering')
  } else {
    color = unlist(get.vertex.attribute(g, color))
  }
  g = setVertexColors(g, color)

  if (is.na(size) | !size %in% vattrs) {
    size = rep(1, vcount(g))
    message('No (valid) size attribute given. Vertex size now set to 1')
  } else {
    size = get.vertex.attribute(g, size)
  }

  V(g)$size= rescale_var(size^0.4, 2, 10)
  V(g)$label.color = 'black'

  V(g)$label.cex = rescale_var(size^0.4, 0.8, 1)
  V(g)$label = V(g)$name
  g
}


setEdgeAttributes <- function(g, edgewidth_coef){
  E(g)$width = rescale_var(E(g)$weight, 1, 10) * edgewidth_coef
  E(g)$arrow.size= 0.00001
  E(g)$color='lightgrey'
  g
}


rescale_var <- function(x, new_min=0, new_max=1, x_min=min(x), x_max=max(x)){
  if (x_min == x_max) return(x)
  x = (x - x_min) / (x_max - x_min) # normalize
  x = x * (new_max-new_min)
  return(x + new_min)
}

reduceLabelOverlap <- function(g, labelspace_coef=1.1, cex_from_device=F, label.attr='label', labelsize.attr='label.cex', rstep=0.01, tstep=0.2){
  layout_matrix = layout.norm(g$layout)

  vnames = names(vertex.attributes(g))
  if (!label.attr %in% vnames) {
    stop('"', label.attr, '" is not a valid vertex attribute)')
  } else {
    label = get.vertex.attribute(g, label.attr)
  }

  if (labelsize.attr %in% vnames){
    label.cex = get.vertex.attribute(g, labelsize.attr)
  } else {
    message('"', labelsize.attr, '" is not a valid vertex attribute). Labelsize is set to 1')
    label.cex = 1
  }

  #ord = order(-centralization.degree(gs)$res) ## reorder so that least central words are relocated first
  ord = order(-label.cex) ## reorder so that smallest words are relocated first
  layout_matrix = layout_matrix[ord,]
  label.cex = label.cex[ord]
  label = label[ord]


  plot(layout_matrix, axes = F, frame.plot = F, xlab='', ylab='', type='n', xlim = c(-1,1), ylim=c(-1,1))
  newlayout = wordcloud::wordlayout(layout_matrix[,1], layout_matrix[,2], label, cex=label.cex*labelspace_coef, rstep = rstep, tstep=tstep, xlim = c(-1,1), ylim=c(-1,1))

  ## calculate new cex based on percentual difference old and new word width
  #oldwidth = mapply(strwidth, s=label, cex=label.cex*labelspace_coef)
  #shrinkcoef = newlayout[,'width'] / oldwidth
  #newlayout = cbind(newlayout, newcex=label.cex*shrinkcoef)

  newlayout = newlayout[match(1:nrow(newlayout), ord),] # return original order
  #g = set.vertex.attribute(g, labelsize.attr, value=newlayout[,'newcex'])
  g$layout = as.matrix(newlayout[,1:2])
  g
}


#' Plot a wordcloud with words ordered and coloured according to a dimension (x)
#'
#' @param x The (approximate) x positions of the words
#' @param y The (approximate) y positions of the words
#' @param words A character vector with the words to plot
#' @param wordfreq The frequency of the words, defaulting to 1
#'
#' @export
plotWords <- function(x, y=NULL, words, wordfreq=rep(1, length(x)), xlab='', ylab='', yaxt='n', scale=2, random.y=F, xlim=NULL, ylim=NULL, col=NULL, ...){
  wordsize = rescale_var(log(wordfreq), 0.75, scale)
  if (is.null(y) & random.y) y = sample(seq(-1, 1, by = 0.001), length(x))
  if (is.null(y) & !random.y) y = wordsize
  xmargin = (max(x) - min(x)) * 0.2
  ymargin = (max(y) - min(y)) * 0.2
  if (is.null(xlim)) xlim = c(min(x) - xmargin, max(x) + xmargin)
  if (is.null(ylim)) ylim = c(min(y) - ymargin, max(y) + ymargin)

  plot(x, y, type = "n", xlim = xlim, ylim = ylim, frame.plot = F, yaxt = yaxt, ylab = ylab, xlab = xlab, ...)
  wl <- as.data.frame(wordcloud::wordlayout(x, y, words, cex = wordsize))

  text(wl$x + 0.5 * wl$width, wl$y + 0.5 * wl$ht, words, cex = wordsize, col = col)
}

#' Get top features from semnet clusters
#'
#'
#' @export
top_cluster_features <- function(g, cluster_attr, measure=c('degree','freq'), top_n=5){
  measure = match.arg(measure)
  d = data.frame(name=V(g)$name,
                 cluster = get.vertex.attribute(g, cluster_attr))
  if (measure == 'degree') d$value = degree(g)
  if (measure == 'freq') d$value = V(g)$freq

  topclusters = table(d$cluster)
  d = d[order(d$cluster, -d$value),]
  d$rank = tcorpus:::local_position(1:nrow(d), d$cluster, presorted=T)
  d = d[d$rank <= top_n,]
  tab = dcast(d, rank ~ cluster, value.var='name')
  tab[is.na(tab)] = ''
  tab[,names(topclusters)[order(-topclusters)]]
}


