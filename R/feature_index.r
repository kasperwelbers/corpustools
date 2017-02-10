#' Get a data.table for quick lookup of features
#'
#' #' The feature index has 3 columns:
#' \itemize{
#'    \item{feature is a factor of a given feature column, and is set as the key of the data.table}
#'    \item{i is the index of the rows in tc@data.}
#'    \item{global_i is an index for features with gaps between contexts (e.g., documents, sentences) to enable word disstance computations using a single vector.}
#' }
#'
#' Automatically filters out features based on the filter columns in tc@data
#'
#' @param tc
#' @param feature
#' @param context_level
#' @param max_window_size
#'
#' @export
get_feature_index <- function(tc, feature='word', context_level='document', max_window_size=100){
  is_tcorpus(tc)

  prov = get_provenance(tc)
  if(!prov[['feature_index']]){
    timer = Sys.time()
    fi = create_feature_index(tc, feature=feature, context_level=context_level, max_window_size=max_window_size)
    time_passed = as.numeric(difftime(Sys.time(), timer, units = 's'))
    if(time_passed > 5) cat(sprintf('Creating feature index took more than 5 seconds (%s). For repeated use, note that the feature index can be precomputed and kept in memory with set_feature_index()', round(time_passed)))
  } else {
    ## if a feature index exists, check whether it matches the current parameters
    cfeature = prov[['feature']] == feature
    clevel = prov[['context_level']] == context_level
    cgap = prov[['max_window_size']] >= max_window_size
    if(!cfeature | !clevel | !cgap){
      cat('Existing feature index does not match current paramters. Creating ad-hoc feature index')
      timer = Sys.time()
      fi = create_feature_index(tc, feature=feature, context_level=context_level, max_window_size=max_window_size)
      cat(sprintf('  (%s seconds)\n', round(as.numeric(difftime(Sys.time(), timer, units = 's')),2)))
    } else {
      fi = tc@feature_index
    }
  }
  setkey(fi, 'feature')
  fi
}

create_feature_index <- function(tc, feature, context_level=c('document','sentence'), max_window_size=100){
  context_level = match.arg(context_level)
  feature_index = data.table(feature = tc@data[[feature]])
  if(!is(feature_index$feature, 'factor')) feature_index$feature = as.factor(feature_index$feature)
  feature_index$i = 1:nrow(feature_index)
  feature_index$global_i = get_global_i(tc, context_level, max_window_size)
  setkey(feature_index, 'feature')
  feature_index
}

#' Create a feature index within a tCorpus
#'
#' @param tc
#' @param feature
#' @param context_level
#' @param max_window_size
#'
#' @export
set_feature_index <- function(tc, feature='word', context_level=c('document','sentence'), max_window_size=100, verbose=F){
  is_tcorpus(tc, T)
  if(is_shattered(tc)) return(shardloop_transform(stc=tc, mcall=match.call(), verbose=verbose))

  context_level = match.arg(context_level)
  tc@feature_index = create_feature_index(tc, feature, context_level, max_window_size)
  tc = set_provenance(tc, feature_index=T, feature=feature, context_level=context_level, max_window_size=max_window_size)
  tc
}



#' Reset feature index
#'
#' Reset the feature_index. This is for instance necessary after filtering the data without keeping the data
#'
#' @param tc
#'
#' @export
reset_feature_index <- function(tc){
  is_tcorpus(tc, T)
  if(is(tc, 'shattered_tCorpus')) return(shard_reset_feature_index(stc=tc))

  p = get_provenance(tc)
  set_feature_index(tc, feature=p$feature, context_level=p$context_level, max_window_size=p$max_window_size)
}

#' delete feature index
#'
#' @param tc
#'
#' @export
delete_feature_index <- function(tc){
  is_tcorpus(tc, T)
  if(is(tc, 'shattered_tCorpus')) return(shard_delete_feature_index(stc=tc))

  tc@feature_index = data.table()
  tc = set_provenance(tc, feature_index=F, feature=NA, context_level=NA, max_window_size=NA)
  tc
}

