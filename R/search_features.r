

search_keyword <- function(tc, fi, kw){
  kw_regex = get_feature_regex(kw)
  hit = fi[J(batch_grepl(kw_regex$regex, levels(fi$feature)))]
  hit$doc_id = tc@data$doc_id[hit$i]
  hit
}

search_condition <- function(tc, fi, hit, condition, feature, default_window=NA){
  con_regex = get_feature_regex(condition, default_window = default_window)
  qm = Matrix::spMatrix(nrow(hit),nrow(con_regex), x=logical())
  colnames(qm) = con_regex$term

  if(nrow(con_regex) == 0){
    return(hit)
  } else {
    hit_doc = unique(hit$doc_id)
    remaining_features = as.character(unique(tc@data[J(hit_doc), feature, with=F][[1]]))

    for(con_regex_term in unique(con_regex$regex)){
      con_hit = fi[J(batch_grepl(con_regex_term, remaining_features)), c('i','global_i'), with=F]

      for(i in which(con_regex$regex == con_regex_term)){
        term = as.character(con_regex$term[i])
        window = con_regex$window[i]

        if(is.na(window)) {
          con_doc = tc@data[con_hit$i,]$doc_id
          qm[,term] = hit$doc_id %in% con_doc
        } else {
          shifts = -window:window
          shift = rep(shifts, times=nrow(con_hit))
          con_window = rep(con_hit$global_i, each = length(shifts)) + shift
          qm[,term] = hit$global_i %in% con_window
        }
      }
    }
  }
  q = parse_queries(condition)
  eval_query_matrix(qm, q[1,]$terms, q[1,]$form)
}


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
#' @param keyword_filter A logical vector that indicates which tokens can match an keyword. Can for instance be used to only select tokens that are proper names (using POS tagging) when looking for people.
#' @param keep_false_condition if True, the keyword hits for which the condition was not satisfied are also returned, with an additional column that indicates whether the condition was satisfied. This can be used to investigate whether the condition is too strict, causing false negatives
#'
#' @export
search_features <- function(tc, keyword=NA, condition=NA, code=NA, queries=NULL, feature='word', condition_once=F, keyword_filter=NULL, keep_false_condition=F, verbose=F){
  if(is.null(queries)) queries = data.frame(keyword=keyword, condition=condition, code=code, condition_once=condition_once)
  if(!'condition' %in% colnames(queries)) queries$condition = NA
  if(!'code' %in% colnames(queries)) queries$code = NA
  if(!'condition_once' %in% queries) queries$condition_once = condition_once

  if(!feature %in% colnames(tc@data)) stop(sprintf('Feature (%s) is not available. Current options are: %s', feature, paste(colnames(tc@data)[!colnames(tc@data) %in% c('doc_id','sent_i','word_i','filter')],collapse=', ')))
  if(any(is.na(queries$keyword))) stop('keyword cannot be NA. Provide either the keyword or queries argument')

  queries$code = ifelse(is.na(queries$code), sprintf('query %s', 1:nrow(queries)), queries$code)
  windows = na.omit(get_feature_regex(queries$condition, default_window = NA)$window)
  max_window_size = if(length(windows) > 0) max(windows) else 0

  fi = get_feature_index(tc, feature=feature, context_level='document', max_window_size = max_window_size)

  res = list()
  for(i in 1:nrow(queries)){
    code = queries$code[i]
    if(verbose) print(sprintf('%s:\t%s', i, code))
    kw = queries$keyword[i]
    hit = search_keyword(tc, fi, queries$keyword[i])
    if(!is.null(keyword_filter)) hit = hit[hit$i]

    if(nrow(hit) == 0) next

    if(!is.na(queries$condition[i]) & !queries$condition[i] == ''){
      hit$condition = search_condition(tc, fi, hit, queries$condition[i], feature=feature, default_window=NA)
    } else {
      hit$condition = T
    }

    if(queries$condition_once[i]){
      doc_with_condition = unique(hit$doc_id[hit$condition])
      hit$condition[hit$doc_id %in% doc_with_condition] = T
    }

    if(!keep_false_condition) {
      res[[code]] = hit[hit$condition, c('feature','i','doc_id'), with=F]
    } else {
      res[[code]] = hit[,c('feature','i','doc_id','condition'), with=F]
    }
  }
  # make proper ldply wrapper to enable verbose
  hits = plyr::ldply(res, function(x) x, .id='code')
  if(nrow(hits) == 0) hits = data.frame(code=factor(), feature=factor(), i=numeric(), doc_id=factor())

  hits[order(hits$i),]
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
#' @export
search_recode <- function(tc, feature, new_value, keyword, condition=NA, condition_once=F, keyword_filter=NULL){
  hits = search_features(tc, keyword=keyword, condition=condition, condition_once=F, keyword_filter=NULL)
  recode_column(tc, feature, new_value, i=hits$i)
}
