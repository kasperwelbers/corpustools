get_window_matrix <- function(fdt, return_i_filter=NULL, window_size, distance_as_value=T, abs_distance=T){
  shifts = -window_size:window_size

  position = fdt$global_i
  #position = get_global_i(tc, context_level = context_level, req_window_size = window_size)
  if(!is.null(return_i_filter)) return_i_filter = position[return_i_filter]

  feature_i = as.numeric(fdt$feature)
  feature_label = levels(fdt$feature)
  m = position_matrix(i = position, j = feature_i, shifts, count_once=T, distance_as_value = distance_as_value, return_i_filter = return_i_filter)
  colnames(m) = feature_label[1:max(feature_i)]
  m
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
      if(count.once) mat@x[mat@x>0] = 1
    }
  }

  mat = as(mat, 'dgTMatrix')
  mat[i,,drop=F]
}

###### TOKENSEARCH ######

#' Creates a matrix where rows match the tokens, columns represent the queries in query_regex, and values represent the word distance to each query hit
#'
#' @param tokens
#' @param query_regex
#' @param feature.col
#' @param presorted
#' @param default.window
#'
#' @return a sparse matrix
#' @export
get_query_matrix <- function(fdt, query_regex, default.window=NA, return_i=NULL){
  if(!'window' %in% colnames(query_regex)) query_regex$window = default.window

  ## get query matrix; separately for query terms with a given word distance and query terms at the article level
  document_level = is.na(query_regex$window) | query_regex$window == 'd'
  if(all(document_level == T)) qm = dococc_querymat(fdt, query_regex[document_level,])
  if(all(document_level == F)) qm = worddist_querymat(fdt, query_regex[!document_level,], return_i)
  if(all(c(T,F) %in% document_level)){
    qm = cbind(worddist_querymat(fdt, query_regex[!document_level,], return_i),
    dococc_querymat(fdt, query_regex[document_level,]))
  }
  if(!is.null(return_i)) qm = qm[return_i,,drop=F]
  qm
}

## create a query matrix for query terms at the wordwindow level
worddist_querymat <- function(fdt, query_regex, return_i_filter=NULL){
  if(nrow(query_regex) == 0) return(NULL)
  query_regex$window = as.numeric(query_regex$window) + 1 # Plus 1, because in the window matrix 1 indicates no distance (because zero already indicates no presence [because this keeps the matrix sparse])

  m = get_window_matrix(fdt, return_i_filter = return_i_filter, window_size=max(query_regex$window))
  m = m[return_i_filter,,drop=F]

  ## create the rows and columns for the query matrix by looking in which rows one of the terms that matches the regex is TRUE.
  get_window_query_hits <- function(j, query_regex, m){
    query_m = m[,tokenGrepl(query_regex$regex[j], colnames(m)),drop=F]
    hits = Matrix::rowSums(query_m > 0 & query_m <= query_regex$window[j]) > 0 # at least one column should have a value between 1 and its window size
    if(sum(hits) == 0) return(NULL)
    data.frame(i = which(return_i_filter[hits]), j = j)
  }

  qm = plyr::ldply(1:nrow(query_regex), get_window_query_hits, query_regex=query_regex, m=m)
  qm = spMatrix(nrow(fdt), nrow(query_regex), qm$i, qm$j, rep(T, nrow(qm)))
  colnames(qm) = query_regex$term
  qm
}

## Create a query matrix for query terms at the document level
dococc_querymat <- function(fdt, query_regex){
  if(nrow(query_regex) == 0) return(NULL)

  term_i = as.numeric(fdt$feature)
  term_label = levels(fdt$feature)
  m = spMatrix(max(fdt$doc_i), length(term_label), fdt$doc_i, term_i, rep(1, nrow(fdt)))
  colnames(m) = term_label

  ## create the rows and columns for the query matrix by looking in which rows one of the terms that matches the regex is TRUE.
  get_query_hits <- function(j, query_regex, m){
    query_m = m[,tokenGrepl(query_regex$regex[j], colnames(m)),drop=F]
    hits = Matrix::rowSums(query_m) > 0 # matrix value has to be higher than 0, but not higher than the window size
    if(sum(hits) == 0) return(NULL)
    data.frame(i = which(hits), j = j)
  }

  qm = plyr::ldply(1:nrow(query_regex), get_query_hits, query_regex=query_regex, m=m)

  qm = spMatrix(max(fdt$i), nrow(query_regex), qm$i, qm$j, rep(T, nrow(qm)))
  colnames(qm) = query_regex$term

  qm[fdt$doc_i,,drop=F] ## repeat rows (i.e. documents) to match the token list input
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
wordWindowOccurence <- function(tokenlist, window.size=10, direction='<>', distance.as.value=F, feature.col=tokenlist@feature.col, filter=T){
  if(!is(tokenlist, 'tokenlist')) stop('Tokenlist argument is not of the tokenlist class. Please use asTokenlist() first')
  tokens = getTokens(tokenlist, filter, feature.col=feature.col)

  if(min(tokens$position) == 0) tokens$position = tokens$position + 1 # if indexing starts at 0, set to 1
  tokens = tokens[!is.na(tokens$feature),]

  ## order on context and position
  ord = order(tokens$doc_id, tokens$position)
  tokens = tokens[ord,]

  tokens$position = globalPosition(tokens$position, tokens$doc_id, window.size=window.size, presorted = T)

  if(direction == '<') shifts = -window.size:0
  if(direction == '<>') shifts = -window.size:window.size
  if(direction == '>') shifts = 0:window.size

  terms = unique(tokens$feature)
  term_index = match(tokens$feature, terms)

  position.mat = positionMatrix(tokens$position, term_index, 0)
  window.mat = positionMatrix(tokens$position, term_index, shifts, distance.as.value=distance.as.value)

  colnames(position.mat) = colnames(window.mat) = terms
  rownames(position.mat) = rownames(window.mat) = tokens$doc_id[!duplicated(tokens$position)]

  ## return original order
  if(!identical(ord, 1:nrow(position.mat))){
    inverse_ord = match(1:nrow(position.mat), ord)
    position.mat = position.mat[inverse_ord,]
    window.mat = window.mat[inverse_ord,]
  }

  list(position.mat=position.mat, window.mat=window.mat)
}

#tc = create_tcorpus(c('dit is een test','dit ook'))
#tc = set_global_i(tc, 'document', 100)
#m = get_window_matrix(tc, 'word')
