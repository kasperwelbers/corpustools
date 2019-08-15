local_position <- function(position, context, presorted=F){
  if (!presorted){
    ord = order(context, position)
    position = position[ord]
    context = context[ord]
  }
  position = local_id(context, position)
  if (!presorted) position = position[match(1:length(position), ord)]
  position
}

global_position <- function(position, context, max_window_size=NA, presorted=F, position_is_local=F){
  ## makes the (token) position counter global with dummy positions between contexts to prevent overlapping windows (so it can be used as an index).
  ## this way, overlapping token windows can be calculated for multiple documents within a single matrix.
  ## position and context need to be sorted on order(context,position). If this is already the case, presorted=T can be used for a speed up
  if (!presorted){
    ord = order(context, position)
    position = position[ord]
    context = context[ord]
  }

  ## first, make sure position is local and starts at 1 for each context (otherwise the global id can become absurdly high)
  if (!position_is_local) position = local_position(position, context, presorted=T)

  if (min(position) == 0) position = position + 1 ## position will be treated as an index, so it cannot be zero in r where an index starts at 1 (and some parsers start indexing at zero)

  position = global_id(context, position, max_window_size)
  if (!presorted) position = position[match(1:length(position), ord)]
  position
}

#' Compute global feature positions
#'
#' Features are given global ids, with an added distance (max_window_size) between contexts (e.g., documents, sentences).
#' This way, the distance of features can be calculated across multiple contexts using a single vector
#'
#' @param tc tCorpus object
#' @param context_level either 'document' or 'sentence'
#' @param max_window_size Determines the size of the gap between documents. Called max_window_size because this gap determines what the maximum window size is for non-overlapping windows between documents
#'
#' @return a tCorpus object
get_global_i <- function(tc, context_level=c('document','sentence'), max_window_size=200){
  is_tcorpus(tc)
  context_level = match.arg(context_level)
  if (context_level == 'document'){
    global_i = global_position(position = tc$get('token_id'), context = tc$get('doc_id'), max_window_size = max_window_size, presorted=T, position_is_local=T)
  }
  if (context_level == 'sentence'){
    if (!'sentence' %in% tc$names) stop('Sentence level not possible, since no sentence information is available. To enable sentence level analysis, use split_sentences = T in "create_tcorpus()" or specify sentence_col in "tokens_to_tcorpus()"')
    globsent = global_position(position = tc$get('sentence'), context = tc$get('doc_id'), presorted=T, position_is_local=T)
    global_i = global_position(position = tc$get('token_i'), context = globsent, max_window_size = max_window_size, presorted=T, position_is_local=T)
  }
  global_i
}

i_window <- function(tc, i, window, context_level=c('document','sentence')){
  context_level = match.arg(context_level)
  gi = get_global_i(tc, context_level, max_window_size = window)
  gi_i = gi[i]
  gi_window = rep(gi_i, window*2 + 1) + rep(-window:window, each=length(gi_i))
  gi %in% gi_window
}

position_matrix <- function(i, j, shifts=0, count_once=T, distance_as_value=F, abs_distance=T, return_i_filter=NULL, rows=NULL){
  shifts = shifts[order(abs(shifts))] # order shifts from 0 to higher (this way the shortest distance is used if distance_as_value = T)

  nrows = max(i, na.rm = T)
  ncols = max(j, na.rm = T)
  return_i = i

  if (!is.null(rows)){
    i = i[rows]
    j = j[rows]
  }

  shift = rep(shifts, times=length(i))
  newi = rep(i, each = length(shifts)) + shift
  newj = rep(j, each = length(shifts))

  if (!is.null(return_i_filter)) {
    select = newi %in% return_i_filter
  } else {
    select = newi > 0 & newi <= max(i)
  }

  if (sum(select) == 0) {
    mat = Matrix::spMatrix(nrow=nrows, ncol=ncols)
  } else {
    if (distance_as_value){
      select = select & !duplicated(data.frame(newi, newj)) # remove duplicates. since duplicates are ordered by shifts, this leaves the shortest distance to a term when using distance_as_value=T
      if (abs_distance) {
        mat = Matrix::spMatrix(nrow=nrows, ncol=ncols, i=newi[select], j=newj[select], x=abs(shift)[select]+1)
      } else {
        mat = Matrix::spMatrix(nrow=nrows, ncol=ncols, i=newi[select], j=newj[select], x=(shift)[select])
      }
    } else{
      mat = spMatrix(nrow=nrows, ncol=ncols, i=newi[select], j=newj[select], x=rep(1, sum(select)))
      mat = methods::as(mat, 'dgCMatrix')
      if (count_once) mat@x[mat@x>0] = 1
    }
  }
  mat = methods::as(mat, 'dgTMatrix')
  mat[return_i,,drop=F]
}


globalFeatureVector <- R6::R6Class("globalFeatureVector",
                                   ## very restricted sparse vector class for quick indices lookup of features based on global_i (with windows between contexts)
                                   public = list(.v = NULL,
                                                 .levels = NULL,
                                                 initialize = function(feature, global_i, empty_str=''){
                                                   feature = fast_factor(feature)
                                                   self$.levels = c(empty_str, levels(feature))
                                                   self$.v = Matrix::sparseVector(as.numeric(feature), global_i, max(global_i))
                                                 },
                                                 get = function(i, ignore_empty=F, allow_na=F) {
                                                   if (allow_na){
                                                     is_na = i < 0 | i > length(self$.v)
                                                     i = i[!is_na]
                                                   }
                                                   if(ignore_empty) {
                                                     allow_na = F
                                                     v = self$.v[i]
                                                     if (!allow_na) return(self$.levels[v[v > 0] + 1]) else v_notna = self$.levels[v[v > 0] + 1]
                                                   } else {
                                                     if (!allow_na) return(self$.levels[self$.v[i] + 1]) else v_notna = self$.levels[self$.v[i] + 1]
                                                   }
                                                   v = rep(NA, length(is_na))
                                                   v[!is_na] = v_notna
                                                   return(v)
                                                 }
                                   )
)

#' @export
'[.globalFeatureVector' = function(x,i, ignore_empty=F, allow_na=F) {
  if (methods::is(i, 'logical')) x$get(which(i)) else x$get(i, ignore_empty=ignore_empty, allow_na=allow_na)
}

#' @export
length.globalFeatureVector = function(x) length(x$.v)
