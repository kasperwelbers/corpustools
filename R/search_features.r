#' Find tokens using a Lucene-like search query
#'
#' @description
#' Search tokens in a tokenlist using a query that consists of an keyword, and optionally a condition. For a detailed explanation of the query language please consult the query_tutorial markdown file. For a quick summary see the details below.
#'
#' Note that the query arguments (keyword, condition, code, condition_once) can be vectors to search multiple queries at once. Alternatively, the queries argument can be used to pass these arguments in a data.frame
#'
#' @section Usage:
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{
#' search_features(keyword = NA, condition = NA, code = NA,
#'                 queries = NULL, feature = 'token', condition_once=F,
#'                 keep_false_condition = F, verbose = F)
#'              }
#'
#' @param keyword The keyword part of the query, see explanation in query_tutorial markdown or in details below
#' @param condition The condition part of the query, see explanation in query_tutorial markdown or in details below
#' @param code The code given to the tokens that match the query (usefull when looking for multiple queries)
#' @param queries Alternatively, a data.frame can be given that contains a "keyword" column, and optionally columns for the "condition", "code" and "condition_once" paramters.
#' @param feature The name of the feature column within which to search.
#' @param condition_once logical. If TRUE, then if an keyword satisfies its conditions once in an article, all keywords within that article are coded.
#' @param keep_false_condition if True, the keyword hits for which the condition was not satisfied are also returned, with an additional column that indicates whether the condition was satisfied. This can be used to investigate whether the condition is too strict, causing false negatives
#' @param unique_i Queries can overlap. For example "mark rutte" also contains "rutte". For some purposes (e.g., counting how often certain queries occur) its better to ignore these overlapping queries. By setting unique_i to TRUE, features will only be assigned to 1 hit_id.
#' @param verbose If TRUE, progress messages will be printed
#'
#' @details
#' Brief summary of the query language
#'
#' The keyword:
#' \itemize{
#'    \item{is the actual feature that has to be found in the token}
#'    \item{can contain multiple tokens with OR statement (and empty spaces are also considered OR statements)}
#'    \item{can contain multitoken strings, using quotes. e.g. "united states"}
#'    \item{can contain token proximities, using quotes plus tilde and a number specifiying the token distance. e.g. "climate chang*"~10}
#'    \item{accepts the ? wildcard, which means that any single character can be used in this place}
#'    \item{accepts the * wildcard, which means that any number of characters can be used in this place}
#'    \item{is be default not case sensitive, but can be made so by adding ~s. e.g. COP~s}
#'  }
#'
#' The condition:
#' \itemize{
#'    \item{has to be TRUE for the keyword to be accepted. Thus, if a condition is given, the query can be interpreted as: keyword AND condition}
#'    \item{works identical to the keyword, but with several additional options:}
#'    \item{- can also contain complex boolean statements, using AND, OR and NOT statements, and using parentheses}
#'    \item{- can be specified for a maximum token distance of the keyword using the ^ (caret) symbol, where "token^50" means that "token" is looked up within 50 tokens of the keyword. This can also be used after multitoken strings, and in combination with the tilde. e.g. "climate chang*"~5^10 will check if the tokens climate and change/changing/etc. co-occur within 5 tokens, and if so, at least on token should occur within 10 tokens of the keyword}
#'    \item{- the case sensitive and token distance flags can be used together. e.g. COP~s^50 means that all capital COP must be found within 50 tokens of the keyword}
#' }
#'
#' Parameters:
#' \itemize{
#'    \item{condition_once -> if TRUE, then if the condition is satisfied at least once in an article, all occurences of the keyword are accepted. }
#' }
#'
#' @name tCorpus$search_features
#' @aliases search_features.tCorpus
tCorpus$set('public', 'search_features', function(keyword=NA, condition=NA, code=NA, queries=NULL, feature='token', condition_once=F, keep_false_condition=F, unique_i=F, verbose=F){
  search_features(self, keyword=keyword, condition=condition, code=code, queries=queries, feature=feature, condition_once=condition_once, keep_false_condition=keep_false_condition, unique_i=unique_i, verbose=verbose)
})

tCorpus$set('public', 'code_features', function(keyword=NA, condition=NA, code=NA, queries=NULL, feature='token', column='code', condition_once=F, unique_i=F, verbose=F){
  hits = search_features(self, keyword=keyword, condition=condition, code=code, queries=queries, feature=feature, condition_once=condition_once, keep_false_condition=F, unique_i=unique_i, verbose=verbose)

  .i = self$token_i(doc_id = hits$hits$doc_id, token_i = hits$hits$token_i)
  .value = hits$hits$code
  self$set(column=column, subset=.i, value=.value, subset_value=F)

  invisible(self)
})

#' Recode features in a tCorpus based on a search string
#'
#' @description
#' Search features (see \link{tCorpus$search_features}) and replace features with a new value
#'
#' @section Usage:
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{
#' search_recode(feature, new_value, keyword, condition = NA, condition_once = F)
#' }
#'
#' @param feature The feature in which to search
#' @param new_value the character string with which all features that are found are replaced
#' @param ... See \link{tCorpus$search_features} for the query parameters
#'
#' @name tCorpus$search_recode
#' @aliases search_recode.tCorpus
tCorpus$set('public', 'search_recode', function(feature, new_value, keyword, condition=NA, condition_once=F, unique_i=F){
  hits = self$search_features(keyword=keyword, condition=condition, condition_once=condition_once, unique_i=unique_i)
  .i = self$token_i(doc_id = hits$hits$doc_id, token_i = hits$hits$token_i)
  .new_value = new_value
  self$set(feature, .new_value, subset = .i)
  invisible(self)
})


search_features <- function(tc, keyword=NA, condition=NA, code=NA, queries=NULL, feature='token', condition_once=F, keep_false_condition=F, unique_i=T, verbose=F){
  is_tcorpus(tc, T)

  if (is.null(queries)) queries = data.frame(keyword=keyword)
  if (!'condition' %in% colnames(queries)) queries$condition = condition
  if (!'code' %in% colnames(queries)) queries$code = code
  if (!'condition_once' %in% colnames(queries)) queries$condition_once = condition_once

  if (!feature %in% tc$names) stop(sprintf('Feature (%s) is not available. Current options are: %s', feature, paste(tc$feature_names, collapse=', ')))
  if (any(is.na(queries$keyword))) stop('keyword cannot be NA. Provide either the keyword or queries argument')

  queries$code = as.character(queries$code)
  queries$code = ifelse(is.na(queries$code), sprintf('query_%s', 1:nrow(queries)), queries$code)

  windows = get_feature_regex(queries$condition, default_window = NA)
  windows = stats::na.omit(c(windows$window, windows$condition_window))
  max_window_size = if (length(windows) > 0) max(windows) else 0

  hits = search_features_loop(tc, queries=queries, feature=feature, keep_false_condition=keep_false_condition, unique_i=unique_i, verbose=verbose)

  featureHits(hits, queries)
}

search_features_loop <- function(tc, queries, feature, keep_false_condition, unique_i, verbose){
  sent_i = NULL; condition = NULL; hit_id = NULL ## for solving CMD check notes (data.table syntax causes "no visible binding" message)

  n = nrow(queries)
  res = vector('list', n)
  for (i in 1:n){
    code = queries$code[i]
    if (verbose) print(sprintf('%s / %s: %s', i, n, as.character(code)))
    kw = queries$keyword[i]

    hit = search_string(tc, kw, unique_i=unique_i, feature=feature)

    if(is.null(hit)) next
    if (nrow(hit) == 0) next

    hit = subset(hit, select = c('doc_id', 'token_i', 'hit_id', feature))
    data.table::setnames(hit, feature, 'feature')

    if (!is.na(queries$condition[i]) & !queries$condition[i] == ''){
      hit$condition = evaluate_condition(tc, hit, queries$condition[i], feature=feature)

      if (queries$condition_once[i]){
        doc_with_condition = unique(hit$doc_id[hit$condition])
        hit$condition[hit$doc_id %in% doc_with_condition] = T
      }
      if (!keep_false_condition) {
        keep_ids = hit[['hit_id']][hit$condition]
        hit = subset(hit, hit_id %in% keep_ids)
        hit[, condition := NULL]
      }
    }
    res[[i]] = hit
  }

  names(res) = queries$code
  hits = data.table::rbindlist(res)
  if (nrow(hits) > 0) {
    hits$code = rep(names(res), sapply(res, nrow))
    data.table::setorderv(hits, c('doc_id','token_i'))
    hits$hit_id = match(hits$hit_id, unique(hits$hit_id))
    if (!'sent_i' %in% colnames(hits)) hits[,sent_i := NA]
    hits = as.data.frame(hits[, c('code','feature','doc_id','sent_i','token_i','hit_id')])
  } else {
    hits = data.frame(code=factor(), feature=factor(), doc_id=factor(), sent_i=numeric(), token_i = numeric(), hit_id=numeric())
  }
  hits
}

evaluate_condition <- function(tc, hit, condition, feature='token'){
  con_query = parse_queries(condition)[1,] ## can only be 1 query
  setkeyv(hit, c('doc_id','token_i'))

  if (length(con_query$terms) == 0){
    return(hit)
  } else {
    qm = Matrix::spMatrix(nrow(hit), length(con_query$terms), x=logical())
    colnames(qm) = con_query$terms

    for (j in 1:length(con_query$terms)){
      con_hit = search_string(tc, con_query$terms[j], unique_i = F, feature=feature)

      con_regex = get_feature_regex(con_query$terms[j])
      direction = con_regex$direction
      window = con_regex$condition_window
      if (is.na(window)) {
        hit_in_con = hit[list(doc_id=as.character(con_hit$doc_id)),,which=T]
      } else {
        if(direction == '<') shifts = 0:window
        if(direction == '>') shifts = -window:0
        if(direction == '<>') shifts = -window:window

        in_con = con_hit[rep(1:nrow(con_hit), length(shifts)), c('doc_id','token_i')]
        in_con$token_i = in_con$token_i + shifts
        hit_in_con = hit[as.list(unique(in_con)),,which=T]
      }

      hit_in_con = unique(stats::na.omit(hit_in_con))
      if (length(hit_in_con) > 0) qm[hit_in_con, j] = T
    }
  }
  eval_query_matrix(qm, con_query$terms, con_query$form)
}

