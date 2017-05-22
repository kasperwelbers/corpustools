## double check whether direction of con_prob is correct

# Create a semantic network based on the co-occurence of words in documents
#
# This function calculates the co-occurence of words and returns a network/graph where nodes are words and edges represent the similarity/adjacency of words. Co-occurence is calcuated based on how often two words occured within the same document (e.g., news article, chapter, paragraph, sentence). Note that the cooc_window() function can be used to calculate co-occurrence of words within a given word distance.

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

# A sliding window approach to calculate the co-occurence of words

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
