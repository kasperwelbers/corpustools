
#' Find tokens using a Lucene-like search query
#'
#' Search tokens in a tokenlist using a query that consists of an keyword, and optionally a condition. For a detailed explanation of the query language please consult the query_tutorial markdown file. For a quick summary see the details below.
#'
#' Note that the query arguments (keyword, condition, code, condition_once) can be vectors to search multiple queries at once. Alternatively, the queries argument can be used to pass these arguments in a data.frame
#'
#' @details
#' Brief summary of the query language
#'
#' The keyword:
#' \itemize{
#'    \item{is the actual feature that has to be found in the token}
#'    \item{can contain multiple words with OR statement (and empty spaces are also considered OR statements)}
#'    \item{CANNOT contain AND or NOT statements (this is what the condition is for)}
#'    \item{accepts the ? wildcard, which means that any single character can be used in this place}
#'    \item{accepts the * wildcard, which means that any number of characters can be used in this place}
#'  }
#'
#' The condition:
#' \itemize{
#'    \item{has to be TRUE for the keyword to be accepted. Thus, if a condition is given, the query can be interpreted as: keyword AND condition}
#'    \item{can contain complex boolean statements, using AND, OR and NOT statements, and using parentheses}
#'    \item{accepts the ? and * wildcards}
#'    \item{can be specified for a maximum word distance of the keyword. The terms in the condition are looked up within this word distance. Individual terms can be given a word distance using the ~ symbol, where "word~50" means that "word" is looked up within 50 words of the keyword.}
#' }
#'
#' Parameters:
#' \itemize{
#'    \item{condition_once -> if TRUE, then if the condition is satisfied at least once in an article, all occurences of the keyword are accepted. }
#' }
#'
#' @param tc a tCorpus object
#' @param keyword The keyword part of the query, see explanation in query_tutorial markdown or in details below
#' @param condition The condition part of the query, see explanation in query_tutorial markdown or in details below
#' @param code The code given to the tokens that match the query (usefull when looking for multiple queries)
#' @param queries Alternatively, a data.frame can be given that contains a "keyword" column, and optionally columns for the "condition", "code" and "condition_once" paramters.
#' @param condition_once logical. If TRUE, then if an keyword satisfies its conditions once in an article, all keywords within that article are coded.
#' @param keep_false_condition if True, the keyword hits for which the condition was not satisfied are also returned, with an additional column that indicates whether the condition was satisfied. This can be used to investigate whether the condition is too strict, causing false negatives
#' @param only_last_mword If TRUE, then if multiword keywords are used (i.e. using double quotes, for instance "the united states"), only return the index of the last word. Note that if this is set to FALSE, it affects the occurence frequency, which is often a bad idea (e.g., counting search hits, word co-occurence analysis)
#' @param feature
#' @param subset_tokens A call (or character string of a call) as one would normally pass to subset.tCorpus. If given, the keyword has to occur within the subset. This is for instance usefull to only look in named entity POS tags when searching for people or organization. Note that the condition does not have to occur within the subset.
#' @param subset_meta A call (or character string of a call) as one would normally pass to the subset_meta parameter of subset.tCorpus. If given, the keyword has to occur within the subset documents. This is for instance usefull to make queries date dependent. For example, in a longitudinal analysis of politicians, it is often required to take changing functions and/or party affiliations into account. This can be accomplished by using subset_meta = "date > xxx & date < xxx" (given that the appropriate date column exists in the meta data).
#' @param verbose
#'
search_features <- function(tc, keyword=NA, condition=NA, code=NA, queries=NULL, feature='word', condition_once=F, subset_tokens=NA, subset_meta=NA, keep_false_condition=F, only_last_mword=F, verbose=F){
  is_tcorpus(tc, T)
  if (is_shattered(tc)) return(shardloop_rbind(stc=tc, mcall=match.call(), verbose=verbose))

  if (is.null(queries)) queries = data.frame(keyword=keyword)
  if (!'condition' %in% colnames(queries)) queries$condition = condition
  if (!'code' %in% colnames(queries)) queries$code = code
  if (!'condition_once' %in% colnames(queries)) queries$condition_once = condition_once
  if (!'subset_tokens' %in% colnames(queries)) queries$subset_tokens = if (is(substitute(subset_tokens), 'call')) deparse(substitute(subset_tokens)) else subset_tokens
  if (!'subset_meta' %in% colnames(queries)) queries$subset_meta = if (is(substitute(subset_meta), 'call')) deparse(substitute(subset_meta)) else subset_meta

  if (!feature %in% tc$names) stop(sprintf('Feature (%s) is not available. Current options are: %s', feature, paste(colnames(tc@data)[!colnames(tc@data) %in% c('doc_id','sent_i','word_i','filter')],collapse=', ')))
  if (any(is.na(queries$keyword))) stop('keyword cannot be NA. Provide either the keyword or queries argument')

  queries$code = as.character(queries$code)
  queries$code = ifelse(is.na(queries$code), sprintf('query_%s', 1:nrow(queries)), queries$code)

  windows = get_feature_regex(queries$condition, default_window = NA)
  windows = na.omit(c(windows$window, windows$condition_window))
  max_window_size = if (length(windows) > 0) max(windows) else 0

  fi = tc$feature_index(feature=feature, context_level='document', max_window_size=max_window_size, as_ascii=T)
  hits = search_features_loop(tc, fi=fi, queries=queries, feature=feature, only_last_mword=only_last_mword, keep_false_condition=keep_false_condition, verbose=verbose)

  featureHits(hits, queries)
}

search_features_loop <- function(tc, fi, queries, feature, only_last_mword, keep_false_condition, verbose){
  res = list()
  n = nrow(queries)
  for (i in 1:n){
    code = queries$code[i]
    if (verbose) print(sprintf('%s / %s: %s', i, n, as.character(code)))
    kw = queries$keyword[i]
    hit = search_string(fi, kw, allow_proximity = T, only_last_mword = only_last_mword)
    if(is.null(hit)) next

    hit$doc_id = tc$data('doc_id')[hit$i]

    ## take subset into account
    subset_tokens = queries$subset_tokens[i]
    subset_meta = queries$subset_meta[i]
    if (!is.na(subset_tokens) | !is.na(subset_meta)){
      if (i == 1) {
        i_filter = subset_i(tc, subset=subset_tokens, subset_meta=subset_meta)
      } else { # if subset_tokens and subset_meta are identical to previous query, i_filter does not need to be calculated again (this is easily the case, since its convenient to give a subset globally by passing a single value)

        if (!identical(subset_tokens, queries$subset_tokens[i-1]) | !identical(subset_meta, queries$subset_meta[i-1])) {
          i_filter = subset_i(tc, subset=subset_tokens, subset_meta=subset_meta)
        }
      }
      hit = hit[hit$i %in% i_filter,,drop=F]
    }

    if (nrow(hit) == 0) next

    if (!is.na(queries$condition[i]) & !queries$condition[i] == ''){
      hit$condition = evaluate_condition(tc, fi, hit, queries$condition[i])
    } else {
      hit$condition = T
    }

    if (queries$condition_once[i]){
      doc_with_condition = unique(hit$doc_id[hit$condition])
      hit$condition[hit$doc_id %in% doc_with_condition] = T
    }

    if (!keep_false_condition) {
      res[[code]] = hit[hit$condition, c('feature','i','doc_id', 'hit_id')]
    } else {
      res[[code]] = hit[,c('feature','i','doc_id','condition', 'hit_id')]
    }
  }

  hits = plyr::ldply(res, function(x) x, .id='code')
  position_cols = if ('sent_i' %in% tc$names) c('sent_i', 'word_i') else c('word_i')
  hits = cbind(hits, tc$data(position_cols, keep_df = T)[hits$i,])

  if (nrow(hits) == 0) hits = data.frame(code=factor(), feature=factor(), i=numeric(), doc_id=factor(), sent_i=numeric(), word_i = numeric(), hit_id=numeric())
  hits = hits[order(hits$i),]
  hits$hit_id = match(hits$hit_id, unique(hits$hit_id))
  hits
}

evaluate_condition <- function(tc, fi, hit, condition){
  con_query = parse_queries(condition)[1,] ## can only be 1 query
  if (length(con_query$terms) == 0){
    return(hit)
  } else {
    qm = Matrix::spMatrix(nrow(hit), length(con_query$terms), x=logical())
    colnames(qm) = con_query$terms

    for (j in 1:length(con_query$terms)){
      con_hit = search_string(fi, con_query$terms[j])

      con_regex = get_feature_regex(con_query$terms[j])
      direction = con_regex$direction
      window = con_regex$condition_window
      if (is.na(window)) {
        con_doc = tc$data('doc_id')[con_hit$i]
        qm[,j] = hit$doc_id %in% con_doc
      } else {
        if(direction == '<') shifts = 0:window
        if(direction == '>') shifts = -window:0
        if(direction == '<>') shifts = -window:window

        if (direction == '<>') {
          shift = rep(shifts, times=nrow(con_hit))
          con_window = rep(con_hit$global_i, each = length(shifts)) + shift
        } else {
          shift = rep(shifts, times=nrow(con_hit))
          con_window = rep(con_hit$global_i, each = length(shifts)) + shift
        }
        qm[,j] = hit$global_i %in% unique(con_window)
      }
    }
  }
  eval_query_matrix(qm, con_query$terms, con_query$form)
}


#' Recode features in a tCorpus based on a search string
#'
#' Search features (see documentation for search_features function for instructions) and replace features with a new value
#'
#' @param tc
#' @param feature
#' @param new_value
#' @param keyword
#' @param condition
#' @param condition_once
#' @param keyword_filter
#'
search_recode <- function(tc, feature, new_value, keyword, condition=NA, condition_once=F, subset_tokens=NA, subset_meta=NA){
  is_tcorpus(tc, T)
  if (is(tc, 'shattered_tCorpus')) return(shard_search_recode(stc=tc, feature=feature, new_value=new_value, keyword=keyword, condition=condition, condition_once=condition_once, subset_tokens=subset_tokens, subset_meta=subset_meta))

  hits = search_features(tc, keyword=keyword, condition=condition, condition_once=condition_once, subset_tokens=subset_tokens, subset_meta=subset_meta)
  x = as.numeric(as.character(hits$hits$i)) ## for one of those inexplicable R reasons, I cannot directly use this numeric vector.... really no clue at all why
  tc$set_column(feature, new_value, subset = x)
}



