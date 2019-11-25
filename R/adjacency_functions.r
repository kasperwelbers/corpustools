#' Gives the window in which a term occured in a matrix.
#'
#' This function returns the occurence of tokens (position.matrix) and the window of occurence (window.matrix). This format enables the co-occurence of tokens within sliding windows (i.e. token distance) to be calculated by multiplying position.matrix with window.matrix.
#'
#' @param tc a tCorpus object
#' @param feature The name of the feature column
#' @param context_level Select whether to use "document" or "sentence" as context boundaries
#' @param window.size The distance within which tokens should occur from each other to be counted as a co-occurence.
#' @param direction a string indicating whether only the left ('<') or right ('>') side of the window, or both ('<>'), should be used.
#' @param distance_as_value If True, the values of the matrix will represent the shorts distance to the occurence of a feature
#' @param batch_rows Used in functions that call this function in batches
#' @param drop_empty_terms If TRUE, emtpy terms (with zero occurence) will be dropped
#'
#' @return A list with two matrices. position.mat gives the specific position of a term, and window.mat gives the window in which each token occured. The rows represent the position of a term, and matches the input of this function (position, term and context). The columns represents terms.
tokenWindowOccurence <- function(tc, feature, context_level=c('document','sentence'), window.size=10, direction='<>', distance_as_value=F, batch_rows=NULL, drop_empty_terms=T){
  is_tcorpus(tc)

  context_level = match.arg(context_level)
  feature = match.arg(feature, tc$feature_names)

  if (direction == '<') shifts = -window.size:0
  if (direction == '<>') shifts = -window.size:window.size
  if (direction == '>') shifts = 0:window.size

  feature = tc$get(feature)
  if (!methods::is(feature,'factor')) feature = fast_factor(feature)
  if (drop_empty_terms) feature = base::droplevels(feature)
  term_index = as.numeric(feature)
  position = get_global_i(tc, context_level, window.size)

  rows = if (!is.null(batch_rows)) batch_rows[!is.na(feature[batch_rows])] else !is.na(feature)
  position.mat = position_matrix(position, term_index, shifts = 0, rows=rows)
  window.mat = position_matrix(position, term_index, shifts, distance_as_value=distance_as_value, rows=rows)

  colnames(position.mat) = colnames(window.mat) = levels(feature)
  rownames(position.mat) = rownames(window.mat) = position

  list(position.mat=position.mat, window.mat=window.mat)
}

#### matrix functions

transform_count <- function(m, count_mode=c('normal','dicho','prob'), alpha=2){
  count_mode = match.arg(count_mode)
  m = methods::as(methods::as(m, 'dgCMatrix'), 'dgTMatrix') ## ensure that values above 1 are not spread out over different indices
  if (count_mode == 'normal') NULL
  if (count_mode == 'dicho') m@x[m@x > 0] = 1
  if (count_mode == 'prob') {
    get_prob <- function(x, alpha) 1 - ((1/alpha) ^ x)
    m@x[m@x > 0] = get_prob(m@x[m@x > 0], alpha)
  }
  m
}

feature_cooccurrence <- function(tc, feature, matrix_mode=c('dtm', 'windowXwindow', 'positionXwindow'), count_mode=c('normal','dicho','prob'), mat_stats=c('sum.x','sum.y','magnitude.x','magnitude.y', 'nrow'), context_level=c('document','sentence'), direction='<>', window.size=10, n.batches=1, alpha=2){
  is_tcorpus(tc)

  matrix_mode = match.arg(matrix_mode)
  count_mode = match.arg(count_mode)
  context_level = match.arg(context_level)
  if (matrix_mode == 'dtm') {
    ml = cooccurrence_matrix(tc, feature, count_mode=count_mode, mat_stats=mat_stats, context_level=context_level, n.batches=n.batches, alpha=alpha)
  }
  if (matrix_mode %in% c('positionXwindow', 'windowXwindow')) {
    ml = cooccurrence_matrix_window(tc, feature, matrix_mode=matrix_mode, count_mode=count_mode, mat_stats=mat_stats, context_level=context_level, direction=direction, window.size=window.size, n.batches=n.batches, alpha=alpha)
  }
  ml$mat = methods::as(methods::as(ml$mat, 'dgCMatrix'), 'dgTMatrix')
  ml
}

cooccurrence_crossprod <- function(m1, m2=NULL, count_mode, mat_stats, alpha){
  m1 = transform_count(methods::as(m1, 'dgTMatrix'), count_mode=count_mode, alpha=alpha)

  if (is.null(m2)){
    mat = Matrix::crossprod(m1)
    mat_stats = get_matrix_stats(m1, mat_stats=mat_stats)
  } else {
    m2 = transform_count(methods::as(m2, 'dgTMatrix'), count_mode=count_mode, alpha=alpha)
    mat = Matrix::crossprod(m1,m2)
    mat_stats = get_matrix_stats(m1, m2=m2, mat_stats=mat_stats)
  }
  c(list(mat=mat), mat_stats)
}

get_matrix_stats <- function(m1, m2=NULL, mat_stats = c('sum.x','sum.y','magnitude.x','magnitude.y', 'nrow')){
  r = list()

  if ('nrow' %in% mat_stats) r[['nrow']] = nrow(m1)
  if ('sum.x' %in% mat_stats) r[['sum.x']] = Matrix::colSums(m1)
  if ('magnitude.x' %in% mat_stats) r[['magnitude.x']] = sqrt(Matrix::colSums(m1^2))

  if(!is.null(m2)){
    if ('sum.y' %in% mat_stats) r[['sum.y']] = Matrix::colSums(m2)
    if ('magnitude.y' %in% mat_stats) r[['magnitude.y']] = sqrt(Matrix::colSums(m2^2))
  } else {
    if ('sum.y' %in% mat_stats) {
      r[['sum.y']] = if ('sum.x' %in% names(r)) r[['sum.x']] else Matrix::colSums(m2)
    }
    if ('magnitude.y' %in% mat_stats) {
      r[['magnitude.y']] = if ('magnitude.x' %in% names(r)) r[['magnitude.x']] else sqrt(Matrix::colSums(m2^2))
    }
  }
  r
}


#### DTM based cooccurrene ####

cooccurrence_matrix <- function(tc, feature, count_mode, mat_stats, context_level, n.batches, alpha, drop_empty_terms=T){
  m = get_dtm(tc, feature=feature, context_level = context_level, drop_empty_terms=drop_empty_terms, context_labels = F)

  if (is.na(n.batches)){
    ml = cooccurrence_crossprod(m, count_mode=count_mode, mat_stats=mat_stats, alpha=alpha)
  } else {
    batch_i = get_batch_i(tc, n.batches=n.batches)
    ml = list()

    for(i in 1:nrow(batch_i)){
      batch_rows = batch_i$start[i]:batch_i$end[i]
      cooc = cooccurrence_crossprod(m[batch_rows,,drop=F], count_mode=count_mode, mat_stats=mat_stats, alpha=alpha)
      for(n in names(cooc)) ml[[n]] = if (n %in% names(ml)) ml[[n]] + cooc[[n]] else cooc[[n]]
    }
  }
  ml$freq = Matrix::colSums(m)
  ml$doc_freq = Matrix::colSums(m)
  ml
}

cooccurrence_matrix_window <- function(tc, feature, matrix_mode='position_to_window', count_mode='dicho', mat_stats=c('sum.x','sum.y','magnitude.x','magnitude.y'), context_level='document', direction='<>', window.size=10, n.batches=window.size, alpha=2, drop_empty_terms=T){
  if (is.na(n.batches)){
    wwo = tokenWindowOccurence(tc, feature, context_level, window.size, direction, drop_empty_terms=drop_empty_terms)
    if (matrix_mode == 'windowXwindow') ml = cooccurrence_crossprod(wwo$window.mat, wwo$window.mat, count_mode=count_mode, alpha=alpha, mat_stats=mat_stats)
    if (matrix_mode == 'positionXwindow') ml = cooccurrence_crossprod(wwo$position.mat, wwo$window.mat, count_mode=count_mode, alpha=alpha, mat_stats=mat_stats)
    ml[['freq']] = Matrix::colSums(wwo$position.mat)
  } else {

    batch_i = data_batch(tc, context_level, n.batches)
    ml = list()
    for(i in 1:nrow(batch_i)){
      batch_rows = batch_i$start[i]:batch_i$end[i]
      wwo = tokenWindowOccurence(tc, feature, context_level, window.size, direction, batch_rows=batch_rows)
      if (matrix_mode == 'windowXwindow') cooc = cooccurrence_crossprod(wwo$window.mat, wwo$window.mat, count_mode=count_mode, alpha=alpha, mat_stats=mat_stats)
      if (matrix_mode == 'positionXwindow') cooc = cooccurrence_crossprod(wwo$position.mat, wwo$window.mat, count_mode=count_mode, alpha=alpha, mat_stats=mat_stats)

      for(n in names(cooc)) ml[[n]] = if (n %in% names(ml)) ml[[n]] + cooc[[n]] else cooc[[n]]
      ml[['freq']] = if ('freq' %in% names(ml)) ml[['freq']] + Matrix::colSums(wwo$position.mat) else Matrix::colSums(wwo$position.mat)
    }
  }

  ml # a list containing an adjacency matrix, with additional statistics
}

cooccurrence_graph_freq <- function(g, m1, m2){
  if (is.null(m2)) {
    igraph::V(g)$sum = Matrix::colSums(m1)
    igraph::V(g)$doc_freq = Matrix::colSums(m1 > 0)
  } else {
    m1freq = data.frame(name=colnames(m1), sum=Matrix::colSums(m1), doc_freq=Matrix::colSums(m1 > 0))
    m2freq = data.frame(name=colnames(m2), sum=Matrix::colSums(m2), doc_freq=Matrix::colSums(m2 > 0))
    freq = unique(rbind(m1freq,m2freq))
    i = match(igraph::V(g)$name, freq$name)
    igraph::V(g)$sum = freq$sum[i]
    igraph::V(g)$doc_freq = freq$doc_freq[i]
  }
  g
}

is_symmetrical <- function(mat) identical(colnames(mat), rownames(mat))

squarify_matrix <- function(mat){
  if (!is_symmetrical(mat)){
    ## necessary since graph.adjacency (for making an igraph object out of a matrix) needs matrix to be symmetrical
    mat = methods::as(mat, 'dgTMatrix')
    rnames = rownames(mat)
    cnames = colnames(mat)
    dnames = unique(c(rnames,cnames))

    mat = summary(mat)

    conv_i = match(rnames, dnames)
    conv_j = match(cnames, dnames)
    mat = spMatrix(length(dnames), length(dnames), conv_i[mat$i], conv_j[mat$j], mat$x)
    dimnames(mat) = list(dnames, dnames)
  }
  mat
}

cooc_chi2 <- function(g, x_sum, y_sum=x_sum, autocorrect=T){
  cooc = igraph::get.edgelist(g, names = F)
  cooc = data.frame(x = cooc[,1], y = cooc[,2], cooc = igraph::E(g)$weight)

  cooc$chi2 = calc_chi2(a = cooc$cooc,                                                   # x=1, y=1
                        b = y_sum[cooc$y] - cooc$cooc,                                   # x=0, y=1
                        c = x_sum[cooc$x] - cooc$cooc,                                   # x=1, y=0
                        d = igraph::ecount(g) - ((x_sum[cooc$x] + y_sum[cooc$y]) - cooc$cooc),    # x=0, y=0
                        cochrans_criteria = autocorrect)
  cooc
}

#binom.coef.log <- function(n, k) {
#  bcl_func <- function(n, k) sum(log(((n-k+1):n) / ((k-k+1):k)))
#  mapply(bcl_func, n, k)
#}

#fishers.exact <- function(a,b,c,d){
#  n = a+b+c+d
#  log_p = binom.coef.log(a+b,a) + binom.coef.log(c+d,c) - binom.coef.log(n,a+c)
#  exp(log_p)
#}

