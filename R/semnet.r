## double check whether direction of con_prob is correct

#' Create a semantic network based on the co-occurence of tokens in documents
#'
#' @description
#' This function calculates the co-occurence of features and returns a network/graph in the igraph format, where nodes are tokens and edges represent the similarity/adjacency of tokens. Co-occurence is calcuated based on how often two tokens occured within the same document (e.g., news article, chapter, paragraph, sentence). The semnet_window() function can be used to calculate co-occurrence of tokens within a given token distance.
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{
#' semnet(feature, measure = c('con_prob', 'con_prob_weighted', 'cosine', 'count_directed', 'count_undirected', 'chi2'),
#'        context_level = c('document','sentence'), backbone=F, n.batches=NA)
#' }
#'
#' @param feature The name of the feature column
#' @param measure The similarity measure. Currently supports: "con_prob" (conditional probability), "con_prob_weighted", "cosine" similarity, "count_directed" (i.e number of cooccurrences) and "count_undirected" (same as count_directed, but returned as an undirected network, chi2 (chi-square score))
#' @param context_level Determine whether features need to co-occurr within "documents" or "sentences"
#' @param backbone If True, add an edge attribute for the backbone alpha
#' @param n.batches If a number, perform the calculation in batches
#'
#' @name tCorpus$semnet
#' @aliases semnet
#' @examples
#' text = c('A B C', 'D E F. G H I', 'A D', 'GGG')
#' tc = create_tcorpus(text, doc_id = c('a','b','c','d'), split_sentences = TRUE)
#'
#' g = tc$semnet('token')
#' g
#' igraph::get.data.frame(g)
#' \dontrun{plot_semnet(g)}
tCorpus$set('public', 'semnet', function(feature, measure=c('cosine', 'con_prob', 'con_prob_weighted', 'count_directed', 'count_undirected', 'chi2'), context_level=c('document','sentence'), backbone=F, n.batches=NA){
  measure = match.arg(measure)
  require_package('igraph')
  semnet(self, feature=feature, measure=measure, context_level=context_level, backbone=backbone, n.batches=n.batches)
})

#' Create a semantic network based on the co-occurence of tokens in token windows
#'
#' @description
#' This function calculates the co-occurence of features and returns a network/graph in the igraph format, where nodes are tokens and edges represent the similarity/adjacency of tokens. Co-occurence is calcuated based on how often two tokens co-occurr within a given token distance.
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{
#' semnet_window(feature, measure = c('con_prob', 'cosine', 'count_directed', 'count_undirected', 'chi2'),
#'               context_level = c('document','sentence'), window.size = 10, direction = '<>',
#'               backbone = F, n.batches = 5, set_matrix_mode = c(NA, 'windowXwindow', 'positionXwindow'))
#' }
#'
#' @param feature The name of the feature column
#' @param measure The similarity measure. Currently supports: "con_prob" (conditional probability), "cosine" similarity, "count_directed" (i.e number of cooccurrences) and "count_undirected" (same as count_directed, but returned as an undirected network, chi2 (chi-square score))
#' @param context_level Determine whether features need to co-occurr within "documents" or "sentences"
#' @param window.size The token distance within which features are considered to co-occurr
#' @param direction Determine whether co-occurrence is assymmetricsl ("<>") or takes the order of tokens into account. If direction is '<', then the from/x feature needs to occur before the to/y feature. If direction is '>', then after.
#' @param backbone If True, add an edge attribute for the backbone alpha
#' @param n.batches If a number, perform the calculation in batches
#' @param set_matrix_mode Advanced feature. There are two approaches for calculating window co-occurrence. One is to measure how often a feature occurs within a given token window, which can be calculating by calculating the inner product of a matrix that contains the exact position of features and a matrix that contains the occurrence window. We refer to this as the "positionXwindow" mode. Alternatively, we can measure how much the windows of features overlap, for which take the inner product of two window matrices. By default, semnet_window takes the mode that we deem most appropriate for the similarity measure. Substantially, the positionXwindow approach has the advantage of being very easy to interpret (e.g. how likely is feature "Y" to occurr within 10 tokens from feature "X"?). The windowXwindow mode, on the other hand, has the interesting feature that similarity is stronger if tokens co-occurr more closely together (since then their windows overlap more). Currently, we only use the windowXwindow mode for cosine similarity. By using the set_matrix_mode parameter you can override this.
#'
#' @name tCorpus$semnet_window
#' @aliases semnet_window
#' @examples
#' text = c('A B C', 'D E F. G H I', 'A D', 'GGG')
#' tc = create_tcorpus(text, doc_id = c('a','b','c','d'), split_sentences = TRUE)
#'
#' g = tc$semnet_window('token', window.size = 1)
#' g
#' igraph::get.data.frame(g)
#' \dontrun{plot_semnet(g)}
tCorpus$set('public', 'semnet_window', function(feature, measure=c('cosine', 'con_prob', 'count_directed', 'count_undirected', 'chi2'), context_level=c('document','sentence'), window.size=10, direction='<>', backbone=F, n.batches=NA, set_matrix_mode=c(NA, 'windowXwindow','positionXwindow')){
  measure = match.arg(measure)
  require_package('igraph')
  semnet_window(self, feature=feature, measure=measure, context_level=context_level, window.size=window.size, direction=direction, backbone=backbone, n.batches=n.batches, set_matrix_mode=set_matrix_mode)
})


##########################
##########################

semnet <- function(tc, feature, measure=c('con_prob', 'con_prob_weighted', 'cosine', 'count_directed', 'count_undirected', 'chi2'), context_level=c('document','sentence'), backbone=F, n.batches=NA, alpha=2){
  is_tcorpus(tc, T)

  feature = match.arg(feature, tc$feature_names)
  measure = match.arg(measure)
  context_level = match.arg(context_level)

  g = create_semnet(tc, feature, measure=measure, matrix_mode='dtm', context_level=context_level, n.batches=n.batches, alpha=alpha)

  if (backbone) igraph::E(g)$alpha = backbone_alpha(g)
  g$measure = measure

  class(g) = c('semnet', 'dtm_cooc', measure, class(g))
  g
}

# A sliding window approach to calculate the co-occurence of tokens

semnet_window <- function(tc, feature, measure=c('con_prob', 'cosine', 'count_directed', 'count_undirected', 'chi2'), context_level=c('document','sentence'), window.size=10, direction='<>', backbone=F, n.batches=5, set_matrix_mode=c(NA, 'windowXwindow','positionXwindow')){
  is_tcorpus(tc, T)

  feature = match.arg(feature, tc$feature_names)
  measure = match.arg(measure)
  set_matrix_mode = match.arg(set_matrix_mode)
  context_level = match.arg(context_level)

  ## developer note: might be other interesting combinations. Should we allow the user to specifiy the matrix_mode manually, or decide for them which to use? I'm currently thinking of the latter, but with the set_matrix_mode paramter to override it if wanted
  if (measure %in% c('cosine')) matrix_mode = 'windowXwindow'
  if (measure %in% c('con_prob','count_directed','count_undirected','chi2')) matrix_mode='positionXwindow'
  if (!is.na(set_matrix_mode)) matrix_mode = set_matrix_mode

  if (!direction == '<>'){
    if (measure %in% c('cosine','count_undirected')) stop('cannot use assymetrical window with undirected similarity measures')
    if (matrix_mode == 'windowXwindow') stop('cannot use assymetrical window with matrix_mode == windowXwindow')
  }
  g = create_semnet(tc, feature, measure=measure, matrix_mode=matrix_mode, context_level=context_level, window.size=window.size, direction='<>', n.batches=n.batches)

  if (backbone) igraph::E(g)$alpha = backbone_alpha(g)
  g$measure = measure

  class(g) = c('semnet', 'window_cooc', measure, class(g))
  g
}

create_semnet <- function(tc, feature, measure, matrix_mode, context_level, direction, window.size, n.batches, alpha){
  if (measure == 'cosine') {
    ml = feature_cooccurrence(tc, feature, matrix_mode=matrix_mode, count_mode='normal', mat_stats=c('sum.x','count.x','magnitude.x','magnitude.y'), context_level=context_level, direction=direction, window.size=window.size, n.batches=n.batches, alpha=alpha)
    ml$mat@x = ml$mat@x / (ml$magnitude.x[ml$mat@i+1] * ml$magnitude.y[ml$mat@j+1])
    g = igraph::graph.adjacency(squarify_matrix(ml$mat), mode = 'upper', diag = F, weighted = T)
  }
  if (measure == 'con_prob') {
    ml = feature_cooccurrence(tc, feature, matrix_mode=matrix_mode, count_mode='dicho', mat_stats=c('sum.x'), context_level=context_level, direction=direction, window.size=window.size, n.batches=n.batches, alpha=alpha)
    ml$mat = ml$mat / ml$sum.x
    g = igraph::graph.adjacency(squarify_matrix(ml$mat), mode = 'directed', diag = F, weighted = T)
  }
  if (measure == 'con_prob_weighted') {
    ml = feature_cooccurrence(tc, feature, matrix_mode=matrix_mode, count_mode='prob', mat_stats=c('sum.x'), context_level=context_level, direction=direction, window.size=window.size, n.batches=n.batches, alpha=alpha)
    ml$mat = ml$mat / ml$sum.x
    g = igraph::graph.adjacency(squarify_matrix(ml$mat), mode = 'directed', diag = F, weighted = T)
  }
  if (measure == 'count_directed') {
    ml = feature_cooccurrence(tc, feature, matrix_mode=matrix_mode, count_mode='dicho', mat_stats=c(), context_level=context_level, direction=direction, window.size=window.size, n.batches=n.batches, alpha=alpha)
    g = igraph::graph.adjacency(squarify_matrix(ml$mat), mode = 'directed', diag = F, weighted = T)
  }
  if (measure == 'count_undirected') {
    ml = feature_cooccurrence(tc, feature, matrix_mode=matrix_mode, count_mode='dicho', mat_stats=c(), context_level=context_level, direction=direction, window.size=window.size, n.batches=n.batches, alpha=alpha)
    g = igraph::graph.adjacency(squarify_matrix(ml$mat), mode = 'upper', diag = F, weighted = T)
  }
  if (measure == 'chi2'){
    ## add sign and/or ratio
    ml = feature_cooccurrence(tc, feature, matrix_mode=matrix_mode, count_mode='dicho', mat_stats=c('sum.x','sum.y','nrow'), context_level=context_level, direction=direction, window.size=window.size, n.batches=n.batches, alpha=alpha)

    xtab = data.frame(a = ml$mat@x,                           # x=1, y=1
                      b = ml$sum.x[ml$mat@i+1] - ml$mat@x,    # x=0, y=1
                      c = ml$sum.y[ml$mat@j+1] - ml$mat@x)    # x=1, y=0
    xtab$d = ml$nrow - ((xtab$b + xtab$c) - xtab$a)           # x=0, y=0
    ml$mat@x = calc_chi2(xtab$a, xtab$b, xtab$c, xtab$d, correct=T) ## replace sparse matrix values with chi2
    g = igraph::graph.adjacency(squarify_matrix(ml$mat), mode = 'directed', diag = F, weighted = T)
  }

  ## match frequencies (and if available document frequencies)
  match_i = match(igraph::V(g)$name, names(ml$freq))
  igraph::V(g)$freq = ml$freq[match_i]
  if ('doc_freq' %in% ml) igraph::V(g)$doc_freq = ml$doc_freq[match_i]
  g
}

DocumentTermMatrix_to_dgTMatrix <- function(dtm){
  sm = spMatrix(nrow(dtm), ncol(dtm), dtm$i, dtm$j, dtm$v)
  rownames(sm) = rownames(dtm)
  colnames(sm) = colnames(dtm)
  methods::as(sm, 'dgTMatrix')
}




