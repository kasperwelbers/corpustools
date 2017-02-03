#' Compute global feature positions
#'
#' Features are given global ids, with an added distance (max_window_size) between contexts (e.g., documents, sentences).
#' This way, the distance of features can be calculated across multiple contexts using a single vector
#'
#' @param tc tCorpus object
#' @param context_level The context within which
#' @param max_window_size
#'
#' @return a tCorpus object
#' @export
get_global_i <- function(tc, context_level=c('document','sentence'), max_window_size=200){
  context_level = match.arg(context_level)
  if(context_level == 'document'){
    global_i = global_position(position = tc@data[['word_i']], context = tc@data[['doc_id']], max_window_size = max_window_size, presorted=T)
  }
  if(context_level == 'sentence'){
    if(!'sent_i' %in% colnames(tc@data)) stop('Sentence level not possible, since no sentence information is available. To enable sentence level analysis, use split_sentences = T in "create_tcorpus()" or specify sent_i_col in "tokens_to_tcorpus()"')
    globsent = global_position(position = tc@data[['sent_i']], context = tc@data[['doc_id']], presorted=T)
    global_i = global_position(position = tc@data[['word_i']], context = globsent, max_window_size = max_window_size, presorted=T)
  }
  global_i
}

position_matrix <- function(i, j, shifts=0, count_once=T, distance_as_value=F, abs_distance=T, return_i_filter=NULL, rows=NULL){
  shifts = shifts[order(abs(shifts))] # order shifts from 0 to higher (this way the shortest distance is used if distance_as_value = T)

  nrows = max(i, na.rm = T)
  ncols = max(j, na.rm = T)
  return_i = i

  if(!is.null(rows)){
    i = i[rows]
    j = j[rows]
  }

  shift = rep(shifts, times=length(i))
  newi = rep(i, each = length(shifts)) + shift
  newj = rep(j, each = length(shifts))

  if(!is.null(return_i_filter)) {
    select = newi %in% return_i_filter
  } else {
    select = newi > 0 & newi <= max(i)
  }

  if(sum(select) == 0) {
    mat = spMatrix(nrow=nrows, ncol=ncols)
  } else {
    if(distance_as_value){
      select = select & !duplicated(data.frame(newi, newj)) # remove duplicates. since duplicates are ordered by shifts, this leaves the shortest distance to a term when using distance_as_value=T
      if(abs_distance) {
        mat = spMatrix(nrow=nrows, ncol=ncols, i=newi[select], j=newj[select], x=abs(shift)[select]+1)
      } else {
        mat = spMatrix(nrow=nrows, ncol=ncols, i=newi[select], j=newj[select], x=(shift)[select])
      }
    } else{
      mat = spMatrix(nrow=nrows, ncol=ncols, i=newi[select], j=newj[select], x=rep(1, sum(select)))
      mat = as(mat, 'dgCMatrix')
      if(count_once) mat@x[mat@x>0] = 1
    }
  }
  mat = as(mat, 'dgTMatrix')
  mat[return_i,,drop=F]
}

#' Gives the window in which a term occured in a matrix.
#'
#' This function returns the occurence of words (position.matrix) and the window of occurence (window.matrix). This format enables the co-occurence of words within sliding windows (i.e. word distance) to be calculated by multiplying position.matrix with window.matrix.
#'
#' @param window.size The distance within which words should occur from each other to be counted as a co-occurence.
#' @param tokenlist
#' @param distance.as.value
#' @param feature.col
#' @param filter
#' @param direction a string indicating whether only the left ('<') or right ('>') side of the window, or both ('<>'), should be used.
#'
#' @return A list with two matrices. position.mat gives the specific position of a term, and window.mat gives the window in which each word occured. The rows represent the position of a term, and matches the input of this function (position, term and context). The columns represents terms.
#' @export
wordWindowOccurence <- function(tc, feature, context_level=c('document','sentence'), window.size=10, direction='<>', distance_as_value=F, batch_rows=NULL){
  #tc = subset(tc, !is.na(get_column(tc, feature)))

  context_level = match.arg(context_level)
  feature = match.arg(feature, featurenames(tc))
  tc = as.tcorpus(tc)

  if(direction == '<') shifts = -window.size:0
  if(direction == '<>') shifts = -window.size:window.size
  if(direction == '>') shifts = 0:window.size

  feature = droplevels(get_column(tc, feature))
  term_index = as.numeric(feature)
  position = get_global_i(tc, context_level, window.size)

  rows = if(!is.null(batch_rows)) batch_rows[!is.na(feature[batch_rows])] else !is.na(feature)
  position.mat = position_matrix(position, term_index, shifts = 0, rows=rows)
  window.mat = position_matrix(position, term_index, shifts, distance_as_value=distance_as_value, rows=rows)

  colnames(position.mat) = colnames(window.mat) = levels(feature)
  rownames(position.mat) = rownames(window.mat) = position

  list(position.mat=position.mat, window.mat=window.mat)
}

#### matrix functions

transform_count <- function(m, count_mode=c('normal','dicho','prob'), alpha=2){
  count_mode = match.arg(count_mode)
  m = as(as(m, 'dgCMatrix'), 'dgTMatrix') ## ensure that values above 1 are not spread out over different indices
  if(count_mode == 'normal') NULL
  if(count_mode == 'dicho') m@x[m@x > 0] = 1
  if(count_mode == 'prob') {
    get_prob <- function(x, alpha) 1 - ((1/alpha) ^ x)
    m@x[m@x > 0] = get_prob(m@x[m@x > 0], alpha)
  }
  m
}



feature_cooccurrence <- function(tc, feature, matrix_mode=c('dtm', 'windowXwindow', 'positionXwindow'), count_mode=c('normal','dicho','prob'), mat_stats=c('sum.x','sum.y','magnitude.x','magnitude.y', 'nrow'), context_level=c('document','sentence'), direction='<>', window.size=10, n.batches=1, alpha=2){
  matrix_mode = match.arg(matrix_mode)
  count_mode = match.arg(count_mode)
  context_level = match.arg(context_level)
  if(matrix_mode == 'dtm') {
    ml = cooccurrence_matrix(tc, feature, count_mode=count_mode, mat_stats=mat_stats, context_level=context_level, n.batches=n.batches, alpha=alpha)
  }
  if(matrix_mode %in% c('positionXwindow', 'windowXwindow')) {
    ml = cooccurrence_matrix_window(tc, feature, matrix_mode=matrix_mode, count_mode=count_mode, mat_stats=mat_stats, context_level=context_level, direction=direction, window.size=window.size, n.batches=n.batches, alpha=alpha)
  }
  ml$mat = as(as(ml$mat, 'dgCMatrix'), 'dgTMatrix')
  ml
}

cooccurrence_crossprod <- function(m1, m2=NULL, count_mode, mat_stats, alpha){
  m1 = transform_count(as(m1, 'dgTMatrix'), count_mode=count_mode, alpha=alpha)

  if(is.null(m2)){
    mat = Matrix::crossprod(m1)
    mat_stats = get_matrix_stats(m1, mat_stats=mat_stats)
  } else {
    m2 = transform_count(as(m2, 'dgTMatrix'), count_mode=count_mode, alpha=alpha)
    mat = Matrix::crossprod(m1,m2)
    mat_stats = get_matrix_stats(m1, m2=m2, mat_stats=mat_stats)
  }
  c(list(mat=mat), mat_stats)
}

get_matrix_stats <- function(m1, m2=NULL, mat_stats = c('sum.x','sum.y','magnitude.x','magnitude.y', 'nrow')){
  r = list()

  if('nrow' %in% mat_stats) r[['nrow']] = nrow(m1)

  for(mat_stat in mat_stats){
    if(mat_stat == 'sum.x') r[['sum.x']] = Matrix::colSums(m1)
    if(mat_stat == 'magnitude.x') r[['magnitude.x']] = sqrt(Matrix::colSums(m1^2))

    if(!is.null(m2)){
      if(mat_stat == 'sum.y') r[['sum.y']] = Matrix::colSums(m2)
      if(mat_stat == 'magnitude.y') r[['magnitude.y']] = sqrt(Matrix::colSums(m2^2))
    } else {
      if(mat_stat == 'sum.y') r[['sum.y']] = Matrix::colSums(m1)
      if(mat_stat == 'magnitude.y') r[['magnitude.y']] = sqrt(Matrix::colSums(m1^2))
    }
  }
  r
}


#### DTM based cooccurrene ####



cooccurrence_matrix <- function(tc, feature, count_mode, mat_stats, context_level, n.batches, alpha){
  m = get_dtm(tc, feature=feature, context_level = context_level)

  if(is.na(n.batches)){
    ml = cooccurrence_crossprod(m, count_mode=count_mode, mat_stats=mat_stats, alpha=alpha)
  } else {
    batch_i = get_batch_i(tc, n.batches=n.batches)
    ml = list()

    #pb <- txtProgressBar(0, nrow(batch_i), style = 3)
    #setTxtProgressBar(pb, 0)
    for(i in 1:nrow(batch_i)){
      batch_rows = batch_i$start[i]:batch_i$end[i]
      cooc = cooccurrence_crossprod(m[batch_rows,,drop=F], count_mode=count_mode, mat_stats=mat_stats, alpha=alpha)
      for(n in names(cooc)) ml[[n]] = if(n %in% names(ml)) ml[[n]] + cooc[[n]] else cooc[[n]]
      #setTxtProgressBar(pb, i)
    }
    #close(pb)
  }
  ml$freq = Matrix::colSums(m)
  ml$doc_freq = Matrix::colSums(m)
  ml
}


#### window based cooccurrene ####

#' Calculate the cooccurrence of features.
#'
#' Returns a list with the cooccurrence matrix, and the matrix statistics specified in mat_stats.
#'
#' The reason for making this a separate, complex function is that it enables the construction of the window matrix and the matrix multiplication to be executed in batches/shards
#'
#' @param tc a tcorpus object
#' @param feature
#' @param mode Either 'window_to_window' or 'position_to_window'. With 'window_to_window', cooccurrence is calculated as the number of times the window of two features overlaps. With 'position_to_window', cooccurrence is calculated as the number of times a feature occurs (is positioned) within the window of another feature
#' @param count_mode
#' @param mat_stats A character vector indicating which matrix statistics to calculate. These are necessary for calculating similarity/distance metrics.
#' @param context_level
#' @param window.size
#' @param n.batches
#' @param alpha
#'
#' @return a list with the cooccurrence matrix and the specified matrix statistics (mat_stats)
#' @export
cooccurrence_matrix_window <- function(tc, feature, matrix_mode='position_to_window', count_mode='dicho', mat_stats=c('sum.x','sum.y','magnitude.x','magnitude.y'), context_level='document', direction='<>', window.size=10, n.batches=window.size, alpha=2){
  if(is.na(n.batches)){
    wwo = wordWindowOccurence(tc, feature, context_level, window.size, direction)
    if(matrix_mode == 'windowXwindow') ml = cooccurrence_crossprod(wwo$window.mat, wwo$window.mat, count_mode=count_mode, alpha=alpha, mat_stats=mat_stats)
    if(matrix_mode == 'positionXwindow') ml = cooccurrence_crossprod(wwo$position.mat, wwo$window.mat, count_mode=count_mode, alpha=alpha, mat_stats=mat_stats)
    ml[['freq']] = Matrix::colSums(wwo$position.mat)
  } else {

    batch_i = data_batch(tc, context_level, n.batches)
    ml = list()
    #pb <- txtProgressBar(0, nrow(batch_i), style = 3)
    #setTxtProgressBar(pb, 0)
    for(i in 1:nrow(batch_i)){
      batch_rows = batch_i$start[i]:batch_i$end[i]
      wwo = wordWindowOccurence(tc, feature, context_level, window.size, direction, batch_rows=batch_rows)
      if(matrix_mode == 'windowXwindow') cooc = cooccurrence_crossprod(wwo$window.mat, wwo$window.mat, count_mode=count_mode, alpha=alpha, mat_stats=mat_stats)
      if(matrix_mode == 'positionXwindow') cooc = cooccurrence_crossprod(wwo$position.mat, wwo$window.mat, count_mode=count_mode, alpha=alpha, mat_stats=mat_stats)

      for(n in names(cooc)) ml[[n]] = if(n %in% names(ml)) ml[[n]] + cooc[[n]] else cooc[[n]]
      ml[['freq']] = if('freq' %in% names(ml)) ml[['freq']] + Matrix::colSums(wwo$position.mat) else Matrix::colSums(wwo$position.mat)

      #setTxtProgressBar(pb, i)
    }
    #close(pb)
  }

  ml # a list containing an adjacency matrix, with additional statistics
}


cooccurrence_graph_freq <- function(g, m1, m2){
  if(is.null(m2)) {
    V(g)$sum = Matrix::colSums(m1)
    V(g)$doc_freq = Matrix::colSums(m1 > 0)
  } else {
    m1freq = data.frame(name=colnames(m1), sum=colSums(m1), doc_freq=colSums(m1 > 0))
    m2freq = data.frame(name=colnames(m2), sum=colSums(m2), doc_freq=colSums(m2 > 0))
    freq = unique(rbind(m1freq,m2freq))
    i = match(V(g)$name, freq$name)
    V(g)$sum = freq$sum[i]
    V(g)$doc_freq = freq$doc_freq[i]
  }
  g
}

is_symmetrical <- function(mat) identical(colnames(mat), rownames(mat))

squarify_matrix <- function(mat){
  if(!is_symmetrical(mat)){
    ## necessary since graph.adjacency (for making an igraph object out of a matrix) needs matrix to be symmetrical
    mat = as(mat, 'dgTMatrix')
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
  cooc = get.edgelist(g, names = F)
  cooc = data.frame(x=cooc[,1], y=cooc[,2], cooc=E(g)$weight)

  cooc$chi2 = calc_chi2(a = cooc$cooc,                                                   # x=1, y=1
                        b = y_sum[cooc$y] - cooc$cooc,                                   # x=0, y=1
                        c = x_sum[cooc$x] - cooc$cooc,                                   # x=1, y=0
                        d = nrow(m1) - ((x_sum[cooc$x] + y_sum[cooc$y]) - cooc$cooc),    # x=0, y=0
                        autocorrect=autocorrect)
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

