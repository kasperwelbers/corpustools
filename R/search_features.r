parse_queries <- function(query){
  query = iconv(query, to='ASCII//TRANSLIT') # if present, try to remove accented characters

  query = gsub(' OR ', ' | ', query)
  query = gsub(' AND ', ' & ', query)
  query = gsub(' NOT ', ' &! ', query)

  ## also allow empty space as OR
  query = gsub('(?<=[+*?.a-zA-Z0-9/~_)-])[ ]+(?=[+*?.a-zA-Z0-9/~_(-])', ' | ', query, perl=T)

  ## make " * ", as a 'find all' solution, an immediate TRUE
  query = tolower(query) # safety first: for the odd possibility that someone uses T or F as a query term, which would be interpreted as TRUE or FALSE
  query = gsub('(?<= )\\*(?= )|(?<=^)\\*(?= )', 'T', query, perl=T)

  query_form = as.list(gsub('([+*?.a-z0-9/~_-]+)', '%s', query)) # note that uppercase is not replaced, to keep the TRUE
  query_terms = regmatches(query, gregexpr('([+*?.a-z0-9/~_-]+)', query))

  query_form[query_form == ''] = NA
  t(mapply(function(x,y) list(form=x, terms=y), query_form, query_terms))
}

fill_query <- function(query_values, query_form){
  do.call(sprintf, as.list(c(query_form, query_values)))
}

eval_query <- function(query_values, query_form){
  eval(parse(text=fill_query(query_values, query_form)))
}

eval_query_matrix <- function(qm, terms, form){
  ## only evaluate unique rows of the query matrix, and then match to return the results for each row
  combination = apply(qm[,terms,drop=F], 1, function(x) paste(as.numeric(x), collapse=''))

  isunique = !duplicated(combination)
  ucombination = combination[isunique]
  uqm = qm[isunique,,drop=F]

  res = apply(uqm[,terms, drop=F], 1, eval_query, query_form=form)
  res[match(combination, ucombination)]
}

get_feature_regex <- function(terms, default_window=NA){
  terms = parse_queries(terms)

  if(length(default_window) == nrow(terms)){
    reptimes = sapply(terms[,2], length)
    default_window = rep(default_window, reptimes)
  }
  terms = unlist(terms[,2])
  terms = data.frame(term = terms,
                     regex = gsub('~.*', '', terms),
                     window = ifelse(grepl('~', terms) == T, gsub('.*~', '', terms), default_window))
  terms$window[terms$window == 'd'] = NA
  terms$window = as.numeric(as.character(terms$window))

  terms$regex = gsub('([.+])', '\\\\\\1', terms$regex) ## escape special regex characters

  terms$regex = gsub('*', '.*', terms$regex, fixed=T) # wildcard: none or any symbols
  terms$regex = gsub('?', '.{1}', terms$regex, fixed=T) # wildcard: one character that can be anything
  terms$regex = sprintf('\\b%s\\b', terms$regex)
  unique(terms)
}

qualify_queries <- function(queries){
  boo = c()
  for(i in 1:nrow(queries)){
    if(queries$keyword[i] == '') boo = c(boo, sprintf('Code "%s": no keyword', queries$code[i]))
    if(queries$keyword[i] == '*') boo = c(boo, sprintf('Code "%s": keyword cannot be *', queries$code[i]))
  }
  if(length(boo) > 0) stop(paste(boo, collapse='\n'))
}

batch_grepl <- function(patterns, x, ignore.case=T, perl=F, batchsize=25, useBytes=T){
  ## make batches of terms and turn each batch into a single regex
  patterns = split(patterns, ceiling(seq_along(patterns)/batchsize))
  patterns = sapply(patterns, paste, collapse='|')

  ## grepl in unique x
  out = rep(F, length(x))
  for(pattern in patterns){
    out = out | grepl(pattern, x, ignore.case=ignore.case, perl=perl, useBytes=useBytes)
  }
  x[out]
}

search_keyword <- function(tc, fi, kw){
  kw_regex = get_feature_regex(kw)
  hit = fi[J(batch_grepl(kw_regex$regex, levels(fi$feature)))]
  hit$doc_i = tc@data$doc_i[hit$i]
  hit
}

search_condition <- function(tc, fi, hit, condition, feature, default_window=NA){
  con_regex = get_feature_regex(condition, default_window = default_window)
  qm = Matrix::spMatrix(nrow(hit),nrow(con_regex), x=logical())
  colnames(qm) = con_regex$term

  if(nrow(con_regex) == 0){
    return(hit)
  } else {
    hit_doc = unique(hit$doc_i)
    remaining_features = as.character(unique(tc@data[J(hit_doc), feature, with=F][[1]]))

    for(con_regex_term in unique(con_regex$regex)){
      con_hit = fi[J(batch_grepl(con_regex_term, remaining_features)), c('i','global_i'), with=F]

      for(i in which(con_regex$regex == con_regex_term)){
        term = as.character(con_regex$term[i])
        window = con_regex$window[i]

        if(is.na(window)) {
          con_doc = tc@data[con_hit$i,]$doc_i
          qm[,term] = hit$doc_i %in% con_doc
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
#'    \item{can be specified for a maximum word distance of the keyword. The terms in the condition are looked up within this word distance. The default word distance can be given with the default_window parameter. More specifically, individual terms can be given a custom word distance using the ~ symbol, where "word~50" means that "word" is looked up within 50 words of the keyword. If a default_window is used, it is also possible to ignore the word distance for specific terms by using word~d (where d stands for document).}
#' }
#'
#' Parameters:
#' \itemize{
#'    \item{default_window -> determines the default word distance of the condition terms to the keyword (thus, if no specific word distance is set with the ~ symbol)}
#'    \item{condition_once -> if TRUE, then if the condition is satisfied at least once in an article, all occurences of the keyword are accepted. }
#' }
#'
#' @param tc a tCorpus object
#' @param keyword The keyword part of the query, see explanation in query_tutorial markdown or in details below
#' @param condition The condition part of the query, see explanation in query_tutorial markdown or in details below
#' @param code The code given to the tokens that match the query (usefull when looking for multiple queries)
#' @param queries Alternatively, a data.frame can be given that contains a "keyword" column, and optionally columns for the "condition", "code" and "condition_once" paramters.
#' @param default_window Determines the default word distance of the condition terms to the keyword (thus, if no specific word distance is set with the ~ symbol)
#' @param condition_once logical. If TRUE, then if an keyword satisfies its conditions once in an article, all keywords within that article are coded.
#' @param keyword_filter A logical vector that indicates which tokens can match an keyword. Can for instance be used to only select tokens that are proper names (using POS tagging) when looking for people.
#' @param keep_false_condition if True, the keyword hits for which the condition was not satisfied are also returned, with an additional column that indicates whether the condition was satisfied. This can be used to investigate whether the condition is to strict, causing false negatives
#'
#' @export
search_features <- function(tc, keyword=NA, condition=NA, code=NA, queries=NULL, feature='word', default_window=NA, condition_once=F, keyword_filter=NULL, keep_false_condition=F, verbose=F){
  if(is.null(queries)) queries = data.frame(keyword=keyword, condition=condition, code=code, condition_once=condition_once)
  if(!'condition' %in% colnames(queries)) queries$condition = NA
  if(!'code' %in% colnames(queries)) queries$code = NA
  if(!'condition_once' %in% queries) queries$condition_once = condition_once

  if(!feature %in% colnames(tc@data)) stop(sprintf('Feature (%s) is not available. Current options are: %s', feature, paste(colnames(tc@data)[!colnames(tc@data) %in% c('doc_i','sent_i','word_i','filter')],collapse=', ')))
  if(any(is.na(queries$keyword))) stop('keyword cannot be NA. Provide either the keyword or queries argument')

  queries$code = ifelse(is.na(queries$code), sprintf('query %s', 1:nrow(queries)), queries$code)
  windows = na.omit(get_feature_regex(queries$condition, default_window = default_window)$window)
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
      hit$condition = search_condition(tc, fi, hit, queries$condition[i], feature=feature, default_window=default_window)
    } else {
      hit$condition = T
    }

    if(queries$condition_once[i]){
      doc_with_condition = unique(hit$doc_i[hit$condition])
      hit$condition[hit$doc_i %in% doc_with_condition] = T
    }

    if(!keep_false_condition) {
      res[[code]] = hit[hit$condition, c('feature','i','doc_i'), with=F]
    } else {
      res[[code]] = hit[,c('feature','i','doc_i','condition'), with=F]
    }
  }
  # make proper ldply wrapper to enable verbose
  hits = plyr::ldply(res, function(x) x, .id='code')
  hits[order(hits$i),]
}


function(){
  #library(tcorpus)
  library(tcorpus)
  tc = readRDS('tc_backup.rds')
  queries = data.frame(code=c('Jolande Sap', 'Mark Rutte', 'goede nederlander'),
                       keyword=c('sap', 'rutte', 'goed sterk krachtig slim'),
                       condition = c('jolande groenlinks~10', 'mark vvd~10 minister~5 president~5', 'nederland* NOT (niet~2 niet~10 allesbehalve~2)'))
  feature = 'word'
  context_level='document'

  test = search_features(tc, queries=queries)
  tc = set_feature_index(tc, feature='word')
  test = search_features(tc, queries=queries)


  library(microbenchmark)
  microbenchmark(
    test1 = get_hits(tc, queries),
    test2 = get_hits2(tc, queries),
    times=1
  )
}
