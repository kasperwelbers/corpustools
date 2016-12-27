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
wordWindowOccurence <- function(tc, feature, context_level=c('document','sentence'), window.size=10, direction='<>', distance_as_value=F){
  context_level = match.arg(context_level)
  feature = match.arg(feature, featurenames(tc))
  tc = as.tcorpus(tc)

  if(direction == '<') shifts = -window.size:0
  if(direction == '<>') shifts = -window.size:window.size
  if(direction == '>') shifts = 0:window.size

  term_index = as.numeric(tc@data[[feature]])
  position = get_global_i(tc, context_level, window.size)

  position.mat = position_matrix(position, term_index, 0)
  window.mat = position_matrix(position, term_index, shifts, distance_as_value=distance_as_value)

  colnames(position.mat) = colnames(window.mat) = levels(tc@data[[feature]])
  rownames(position.mat) = rownames(window.mat) = position

  list(position.mat=position.mat, window.mat=window.mat)
}
