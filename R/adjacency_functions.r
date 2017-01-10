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

position_matrix <- function(i, j, shifts=0, count_once=T, distance_as_value=F, abs_distance=T, return_i_filter=NULL){
  shifts = shifts[order(abs(shifts))] # order shifts from 0 to higher (this way the shortest distance is used if distance_as_value = T)

  shift = rep(shifts, times=length(i))
  newi = rep(i, each = length(shifts)) + shift
  newj = rep(j, each = length(shifts))

  if(!is.null(return_i_filter)) {
    select = newi %in% return_i_filter
  } else {
    select = newi > 0 & newi <= max(i)
  }

  if(sum(select) == 0) {
    mat = spMatrix(nrow=max(i), ncol=max(j))
  } else {
    if(distance_as_value){
      select = select & !duplicated(data.frame(newi, newj)) # remove duplicates. since duplicates are ordered by shifts, this leaves the shortest distance to a term when using distance_as_value=T
      if(abs_distance) {
        mat = spMatrix(nrow=max(i), ncol=max(j), i=newi[select], j=newj[select], x=abs(shift)[select]+1)
      } else {
        mat = spMatrix(nrow=max(i), ncol=max(j), i=newi[select], j=newj[select], x=(shift)[select])
      }
    } else{
      mat = spMatrix(nrow=max(i), ncol=max(j), i=newi[select], j=newj[select], x=rep(1, sum(select)))
      mat = as(mat, 'dgCMatrix')
      if(count_once) mat@x[mat@x>0] = 1
    }
  }

  mat = as(mat, 'dgTMatrix')
  mat[i,,drop=F]
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
wordWindowOccurence <- function(tc, feature, context_level=c('document','sentence'), window.size=10, direction='<>', distance_as_value=F, drop_NA=T){
  tc = subset(tc, !is.na(get_column(tc, feature)))

  context_level = match.arg(context_level)
  feature = match.arg(feature, featurenames(tc))
  tc = as.tcorpus(tc)

  if(direction == '<') shifts = -window.size:0
  if(direction == '<>') shifts = -window.size:window.size
  if(direction == '>') shifts = 0:window.size

  feature = droplevels(get_column(tc, feature))
  notNA = !is.na(feature)
  term_index = as.numeric(feature)
  position = get_global_i(tc, context_level, window.size)

  position.mat = position_matrix(position, term_index, 0)
  window.mat = position_matrix(position, term_index, shifts, distance_as_value=distance_as_value)

  colnames(position.mat) = colnames(window.mat) = levels(feature)
  rownames(position.mat) = rownames(window.mat) = position

  list(position.mat=position.mat, window.mat=window.mat)
}

#### matrix functions

transform_count <- function(m, mode, alpha=2){
  m = as(as(m, 'dgCMatrix'), 'dgTMatrix') ## ensure that values above 1 are not spread out over different indices
  if(mode == 'dicho') {
    m@x[m@x > 0] = 1
  }
  if(mode == 'norm') {
    norm = sqrt(Matrix::colSums(m^2))
    m@x = m@x / norm[m@j+1]
  }
  if(mode == 'prob') {
    get_prob <- function(x, alpha) 1 - ((1/alpha) ^ x)
    m@x[m@x > 0] = get_prob(m@x[m@x > 0], alpha)
  }
  m
}

calc_cooccurrence <- function(m1, m2=NULL, mode='directed', count_mode=c('dicho','prob','norm'), alpha=2, diag=F){
  count_mode = match.arg(count_mode)
  m1 = as(m1, 'dgTMatrix')
  if(!is.null(m2)){
    m2 = as(m2, 'dgTMatrix')
    #same_columns = colnames(m1)[colnames(m1) %in% colnames(m2)]
    #if(!identical(m1[,same_columns], m2[,same_columns])) stop('The two matrices contain columns with the same names but different values.') ## this does make sense in the case of a position matrix and a window matrix
    m1 = transform_count(m1, count_mode, alpha)
    m2 = transform_count(m2, count_mode, alpha)
    mat = Matrix::crossprod(m1,m2)
    mat = squarify_matrix(mat) ## ensure matrix is square and symmetrical (for igraph.adjacency)
  } else {
    m1 = transform_count(m1, count_mode, alpha)
    mat = Matrix::crossprod(m1)
  }

  if(mode == 'directed') g = igraph::graph.adjacency(mat, mode='directed', diag=diag, weighted=T)
  if(mode == 'undirected') g = igraph::graph.adjacency(mat, mode='upper', diag=diag, weighted=T)
  g = cooccurrence_graph_freq(g, m1, m2)
  g
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

cooccurrence_graph <- function(m1, m2=NULL, measure=c('cooccurrence','cosine','con_prob','con_prob_weighted','obs/exp','obs/exp_weighted'), chi2=F, backbone=T, alpha=2){
  N = nrow(m1)
  if(measure == 'cosine') {
    g = calc_cooccurrence(m1, m2, mode='undirected', count_mode='norm')
  }
  if(measure == 'count_undirected') {
    g = calc_cooccurrence(m1, m2, mode='undirected', count_mode='dicho')
  }
  if(measure == 'count_directed') {
    g = calc_cooccurrence(m1, m2, mode='directed', count_mode='dicho')
  }
  if(measure == 'con_prob') {
    g = calc_cooccurrence(m1, m2, mode='directed', count_mode='dicho')
    E(g)$weight = E(g)$weight / V(g)$sum[get.edges(g, E(g))[,1]]
  }
  if(measure == 'con_prob_weighted') {
    g = calc_cooccurrence(m1, m2, mode='directed', count_mode='prob', alpha=alpha)
    E(g)$weight = E(g)$weight / V(g)$sum[get.edges(g, E(g))[,1]]
  }
  if(measure == 'obs/exp') {
    g = calc_cooccurrence(m1, m2, mode='directed', count_mode='dicho')
    p = V(g)$sum / N
    e = get.edges(g, E(g))
    exp = (p[e[,1]] * p[e[,2]]) * N
    E(g)$weight = E(g)$weight / exp
  }

  if(chi2){
    if(measure %in% c('count_undirected', 'count_directed')){
      g_occurrence = g
    } else {
      mode = if(is.directed(g)) 'directed' else 'undirected'
      g_occurrence = calc_cooccurrence(m1, m2, mode=mode, count_mode='dicho')
    }
    E(g)$chi2 = cooc_chi2(g_occurrence, m1, m2, autocorrect=T)$chi2
    E(g)$chi2.p = 1-pchisq(E(g)$chi2, 1)
  }
  if(backbone) E(g)$alpha = backbone.alpha(g)

  g$measure = measure
  V(g)$term_freq = Matrix::colSums(m1)
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
    mat = data.frame(i=mat@i+1, j=mat@j+1, x=mat@x)

    conv_i = match(rnames, dnames)
    conv_j = match(cnames, dnames)
    mat = spMatrix(length(dnames), length(dnames), conv_i[mat$i], conv_j[mat$j], mat$x)
    dimnames(mat) = list(dnames, dnames)
  }
  mat
}

#### chi-square

#' Compute the chi^2 statistic for a 2x2 crosstab containing the values
#' [[a, b], [c, d]]
calc_chi2 <- function(a,b,c,d, autocorrect=T, yates_correction=rep(F, length(a))){
  n = a+b+c+d
  sums = cbind(c1 = a+c, c2 = b+d, r1 = a+b, r2 = c+d)

  if(autocorrect){
    ## apply Cochrans criteria: no expected values below 1 and less than 20% of cells empty (which means none in a 2x2 design)
    ## if these are violated, use the yates_correction
    ## http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2041889/ (similar use)
    e = cbind(sums[,'c1'] / n, sums[,'c2'] / n)
    e = cbind(e * sums[,'r1'], e * sums[,'r2'])
    c1 = rowSums(e < 1) > 0          # at least one expected value below 1
    c2 = rowSums(sums < 5) > 0       # at least one cell with value below 5
    yates_correction = ifelse(c1 | c2, T, F)
  }

  x = a*d - b*c
  x = ifelse(yates_correction, abs(x) - n/2, x)
  chi = n*x^2 / (sums[,'c1'] * sums[,'c2'] * sums[,'r1'] * sums[,'r2'])
  ifelse(is.na(chi), 0, chi)
}

cooc_chi2 <- function(g, m1, m2=NULL, autocorrect=T){
  cooc = get.edgelist(g, names = F)
  cooc = data.frame(x=cooc[,1], y=cooc[,2], cooc=E(g)$weight)
  x_sum = as.numeric(Matrix::colSums(m1 > 0))
  y_sum = if(is.null(m2)) x_sum else as.numeric(Matrix::colSums(m2 > 0))

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
