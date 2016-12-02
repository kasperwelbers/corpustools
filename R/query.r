parseQueries <- function(query){
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

fillQuery <- function(query_values, query_form){
  do.call(sprintf, as.list(c(query_form, query_values)))
}

evalQuery <- function(query_values, query_form){
  eval(parse(text=fillQuery(query_values, query_form)))

}

evalQueryMatrix <- function(qm, terms, form){
  ## only evaluate unique rows of the query matrix, and then match to return the results for each row
  combination = apply(qm[,terms,drop=F], 1, function(x) paste(as.numeric(x), collapse=''))

  isunique = !duplicated(combination)
  ucombination = combination[isunique]
  uqm = qm[isunique,,drop=F]

  res = apply(uqm[,terms, drop=F], 1, evalQuery, query_form=form)
  res[match(combination, ucombination)]
}

get_term_regex <- function(terms, default_window=NA){
  terms = parseQueries(terms)

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

qualifyQueries <- function(queries){
  boo = c()
  for(i in 1:nrow(queries)){
    if(queries$keyword[i] == '') boo = c(boo, sprintf('Code "%s": no keyword', queries$code[i]))
    if(queries$keyword[i] == '*') boo = c(boo, sprintf('Code "%s": keyword cannot be *', queries$code[i]))
  }
  if(length(boo) > 0) stop(paste(boo, collapse='\n'))
}


#' A wrapper for grepl that takes multiple patterns. For efficiency, grepl is performed on unique words, and results for individual values are matched. Uses batches to go easy on memory (though sacrificing a bit of speed)
#'
#' @param patterns
#' @param x
#' @param ignore.case
#' @param perl
#'
#' @return a logical vector
#' @export
tokenGrepl <- function(patterns, x, ignore.case=T, perl=F, batchsize=25, useBytes=T){
  ## make batches of terms and turn each batch into a single regex
  patterns = split(patterns, ceiling(seq_along(patterns)/batchsize))
  patterns = sapply(patterns, paste, collapse='|')

  if(!class(x) == 'factor') x = as.factor(x)

  ## grepl in unique x
  ux = levels(x)
  out = rep(F, length(ux))
  for(pattern in patterns){
    out = out | grepl(pattern, ux, ignore.case=ignore.case, perl=perl, useBytes=useBytes)
  }
  x %in% ux[out]
}

#' Report number of unique terms that matches each query
#'
#' This function is usefull to test whether certain queries match a high number of unique terms, thus possibly causing memory issues.
#'
#' @param terms a character vector of (unique) terms
#' @param queries a dataframe with queries as used in the feature_queries en code_queries functions.
#'
#' @export
queryTermMatches <- function(terms, queries){
  uterms = unique(terms)
  queries$nterms = NA
  for(i in 1:nrow(queries)){
    regterms = unique(c(get_term_regex(queries$keyword[i])$regex, get_term_regex(queries$condition[i])$regex))
    queries$nterms[i] = sum(tokenGrepl(regterms, uterms))
  }
  queries$nterms
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
#' @param tokenlist a tokenlist object, created with the asTokenlist() function.
#' @param keyword The keyword part of the query, see explanation in query_tutorial markdown or in details below
#' @param condition The condition part of the query, see explanation in query_tutorial markdown or in details below
#' @param code The code given to the tokens that match the query (usefull when looking for multiple queries)
#' @param default_window Determines the default word distance of the condition terms to the keyword (thus, if no specific word distance is set with the ~ symbol)
#' @param condition_once logical. If TRUE, then if an keyword satisfies its conditions once in an article, all keywords within that article are coded.
#' @param keyword_filter A logical vector that indicates which tokens can match an keyword. Can for instance be used to only select tokens that are proper names (using POS tagging) when looking for people.
#' @param presorted The data has to be sorted on order(doc_id, position). If this is already the case, presorted can be set to TRUE to save time (which is usefull when testing many individual queries for large tokenlists)
#' @param doc_id A vector with document ids. By default the 'doc_id' column of the tokenlist. This parameter can be used if you do not use a default tokenlist, or want to use an alternative column (e.g., paragraph, sentence)
#' @param position A vector with the positions of words. By default the 'position' column of the tokenlist.
#' @param word A vector with words. By default the 'word' column of the tokenlist. This parameter can be used if you do not use a default tokenlist, or want to use an alternative column (e.g., stem, lemma)
#'
#' @return a data.frame containing the words that match the query, and their locations in the tokenlist
#' @export
feature_query <- function(tc, keyword, condition='', code='', feature='word', default_window=NA, condition_once=FALSE, keyword_filter=NULL){
  queries = data.frame(code=code, keyword=keyword, condition=condition)
  feature_queries(tc, queries, feature=feature, batchsize=1, default_window=default_window, condition_once=condition_once, keyword_filter=keyword_filter, verbose=F)
}

#' Find tokens using Lucene-like search queries
#'
#' For an explanation of the query language please consult ?feature_query
#'
#' @param tokenlist a tokenlist object, created with the asTokenlist() function.
#' @param queries a data frame containing the queries. See ?search_query() for an explanation of the query language and to test individual queries.
#' @param batchsize This function is faster if multiple queries are searched together, but too many queries (with too many tokens) at once can eat up memory or crash R. Try lowering batchsize in case of issues.
#' @param default_window Determines the default word distance of the condition terms to the keyword (thus, if no specific word distance is set with the ~ symbol)
#' @param condition_once logical. If TRUE, then if an keyword satisfies its conditions once in an article, all keywords within that article are coded.
#' @param keyword_filter A logical vector that indicates which tokens can match an keyword. Can for instance be used to only select tokens that are proper names (using POS tagging) when looking for people.
#' @param presorted The data has to be sorted on order(doc_id, position). If this is already the case, presorted can be set to TRUE to save time (which is usefull when testing many individual queries for large tokenlists)
#' @param doc.col The name of the document_id column. Defaults to "doc_id", unless a global default is specified using setTokenlistColnames()
#' @param position.col The name of the column giving the position in a document. Defaults to "position", unless a global default is specified using setTokenlistColnames()
#' @param verbose show progress
#'
#' @return the annotated tokens data frame
#' @export
feature_queries <- function(tc, queries, feature='word', batchsize=5, default_window=NA, condition_once=FALSE, keyword_filter=NULL, verbose=T){
  queries = queries[!queries$keyword == '',]

  qualifyQueries(queries)
  if(!'condition_once' %in% colnames(queries)) queries$condition_once = condition_once
  if(!'default_window' %in% colnames(queries)) queries$default_window = default_window

  windows = na.omit(get_term_regex(queries$condition, default_window)$window)
  max_window_size = if(length(windows) > 0) max(windows) else 0
  fdt = get_feature_dt(tc, 'word', context_level='document')
  ## consider underscores as wordboundaries in order to work with regex (some parsers chunk words together, separated by underscores)
  levels(fdt$feature) = gsub('_', ' ', levels(fdt$feature), fixed=T)

  query_i = 1:nrow(queries)
  batches = split(query_i, ceiling(seq_along(query_i)/batchsize))
  progress = if(verbose) 'text' else 'none'
  plyr::ldply(batches, function(x) feature_queries_batch(fdt, queries[x,,drop=F], keyword_filter), .id = NULL, .progress = progress)
}

emptyHitsDf <- function() data.frame(i=numeric(0), doc_id=numeric(0), code=character(0), feature=character(0))

feature_queries_batch <- function(fdt, queries, keyword_filter){
  fdt$ind_filter = if(!is.null(keyword_filter)) keyword_filter else rep(T, nrow(fdt))

  ind = parseQueries(queries$keyword)
  indr = get_term_regex(queries$keyword, queries$default_window)
  con = parseQueries(queries$condition)
  conr = get_term_regex(queries$condition, queries$default_window)

  #### first make a query matrix for all terms used in the queries
  ## only look at articles if one of the keywords (ind_hit) is found
  fdt$ind_hit = tokenGrepl(indr$regex, fdt$feature)
  fdt$ind_hit = fdt$ind_hit & fdt$ind_filter ## use the keyword filter
  article_filter = unique(fdt$doc_i[fdt$ind_hit])

  fdt = fdt[J(article_filter)]
  ## and look only at tokens that are keywords or condition terms
  fdt$is_cond_term = tokenGrepl(conr$regex, fdt$feature)
  feature_filter = fdt$ind_hit | fdt$is_cond_term
  fdt = fdt[feature_filter,]

  if(nrow(fdt) == 0) return(emptyHitsDf())

  ## create matrix where rows are tokens, columns are the query terms, and cells indicate whether the query terms occur (within the given word distance) at the place of each token.
  ## creating the query matrix can (and should) be skipped if no conditions are given
  if(nrow(conr) > 0){
    if(!NA %in% levels(fdt$feature)) levels(fdt$feature) = c(levels(fdt$feature), NA)
    fdt$cond_term = ifelse(fdt$is_cond_term, fdt$feature, NA) ## when making the query matrix, its more memory efficient to use a single column for all keyword terms (that are not condition terms). Note that this keyword column is necessary because we need to know where the keywords are in the query matrix
    qm = get_query_matrix(fdt, conr, default_window, return_i=fdt$ind_hit)
    fdt = fdt[fdt$ind_hit,]
  }

  #### then evaluate each query individually
  result_i = vector('list', nrow(queries))
  for(i in 1:nrow(queries)){
    fdt$code = queries$code[i]

    indr = get_term_regex(queries$keyword[i])
    fdt$hit = tokenGrepl(indr$regex, fdt$feature) & fdt$ind_filter
    if(sum(fdt$hit) == 0) next

    ## if there is no condition, accept all keyword hits
    if(is.na(con[i,]$form)){
      result_i[[i]] = fdt[fdt$hit, c('i', 'doc_i', 'code', 'feature'), with=F]
      next
    }

    ## evaluate condition query
    fdt$hit_and_condition = F
    fdt$hit_and_condition[fdt$hit] = evalQueryMatrix(qm[fdt$hit,], con[i,]$terms, con[i,]$form)

    ## if condition_once is TRUE, then all keyword hits are also coded if the keyword satisfies its condition at least once within the article
    #### !!!!!!!!!!!!! voorrang aan WEL conditie
    if(queries$condition_once[i]){
      article_with_code = unique(fdt$doc_i[fdt$hit_and_condition]) # articles in which keyword satisfies condition at least once
      hit_and_articlecondition = fdt$doc_i[fdt$hit] %in% article_with_code # for all keyword hits, check whether they occur in one of these articles.
      fdt$hit_and_condition[fdt$hit] = ifelse(hit_and_articlecondition, T, fdt$hit[fdt$hit])
    }
    if(sum(fdt$hit_and_condition) > 0) {
      result_i[[i]] = fdt[fdt$hit_and_condition, c('i', 'doc_i', 'code', 'feature'), with=F]
    } else {
      result_i[[i]] = emptyHitsDf()
    }
  }
  unique(plyr::rbind.fill(result_i), by=NULL)
}



#' Annotate a data frame of tokens with codes using Lucene-like search queries
#'
#' @param tokens a tokenlist object. See ?asTokenlist() for details.
#' @param queries a data frame containing the queries.
#' @param batchsize This function is faster if multiple queries are searched together, but too many queries (with too many tokens) at once can eat up memory or crash R. Try lowering batchsize in case of issues.
#' @param default_window Determines the default word distance of the condition terms to the keyword (thus, if no specific word distance is set with the ~ symbol)
#' @param condition_once logical. If TRUE, then if an keyword satisfies its conditions once in an article, all keywords within that article are coded.
#' @param keyword_filter A logical vector that indicates which tokens can match an keyword. Can for instance be used to only select tokens that are proper names (using POS tagging) when looking for people.
#' @param presorted The data has to be sorted on order(doc_id, position). If this is already the case, presorted can be set to TRUE to save time (which is usefull when testing many individual queries for large tokenlists)
#' @param doc.col The name of the document_id column. Defaults to "doc_id", unless a global default is specified using setTokenlistColnames()
#' @param position.col The name of the column giving the position in a document. Defaults to "position", unless a global default is specified using setTokenlistColnames()
#' @param feature.col The name of the column containing the token feature. Defaults to "word", unless a global default is specified using setTokenlistColnames()
#' @param verbose show progress
#'
#' @return the annotated tokens data frame
#' @export
code_features <- function(tc, queries, batchsize=5, default_window=NA, condition_once=FALSE, keyword_filter=rep(T, nrow(tokens)), presorted=F, doc.col=getOption('doc.col','doc_id'), position.col=getOption('position.col','position'), feature.col=getOption('feature.col','word'), verbose=T){
  hits = feature_queries(tokens, queries, batchsize=batchsize, default_window=default_window, condition_once=condition_once, keyword_filter=keyword_filter, presorted=presorted, doc.col=doc.col, position.col=position.col, feature.col=feature.col, verbose=verbose)

  tokens$code = ''
  if(nrow(hits) == 0) return(as.factor(tokens$code))

  ## if a token has multiple codes, only use the last one (this way, if queries has a top-down hierarchical structure, the most specific coding will be used)
  ## to do so, hits are first ordered according to the reversed order of the queries dataframe, and then duplicate rowindices (i) are deleted
  hits$queryorder = match(hits$code, tokens$code)
  hits = hits[order(-hits$queryorder),]
  hits = hits[!duplicated(hits$i),]

  tokens$code[hits$i] = as.character(hits$code)
  as.factor(tokens$code)
}

function(){
  library(tcorpus)
  library(corpustools)
  data(sotu)
  head(sotu.tokens)
  s = sotu.tokens
  terms = unique(s$lemma)
  docs = unique(s$aid)
  m = spMatrix(length(docs), length(terms), match(s$aid, docs), match(s$lemma, terms), rep(1,nrow(s)))
  colnames(m) = terms
  dim(m)

  m[1:10,1:10]

               tc = tokens_to_tcorpus(sotu.tokens, 'aid', 'id')
  print(nrow(feature_query(tc, 'america')))
  print(nrow(feature_query(tc, 'america', 'a')))
  print(nrow(feature_query(tc, 'america', '(a~10 AND is~20) OR us OR obama')))


  #d = read.csv('~/Dropbox/facebook_data/newssites/data/volkskrant.csv')
  #tc = create_tcorpus(d, text_column = 'text')
  #saveRDS(tc, 'tc_backup.rds')
  tc = readRDS('tc_backup.rds')
  test = feature_query(tc, 'rutte', 'mark minister~10 president~10 vvd~15')


  rm(d); gc()
  fdt = get_feature_dt(tc, feature='word')
  format(object.size(fdt), 'Mb')

  get_feature_label(tc, 'word')

position_feature_matrix <- function(tc, feature, max_window_size=100){
  fdt = get_feature_dt(tc, feature=feature, max_window_size = max_window_size)
  fdt$feature_i = as.numeric(fdt$feature)
  spMatrix(max(fdt$global_i), max(fdt$feature_i), fdt$global_i, fdt$feature_i, rep(1, nrow(fdt)))
}

pfm = position_feature_matrix(tc, 'word')
features = get_feature_label(tc, 'word', )
grep('obama', get_feature_dt())

pfm

  tc1 = tc
  tc2 = set_global_i(tc, context_gap=10)
  microbenchmark(
    feature_query(tc1, 'rutte', 'mark minister~10 president~10 vvd~15'),
    feature_query(tc2, 'rutte', 'mark minister~10 president~10 vvd~15'),
    times = 2
  )
  microbenchmark(fdt[J(aids)], fdt[doc_i %in% aids], times=2)
  fdt = get_feature_dt(tc, 'word')
  aids = sample(unique(fdt$doc_i), 100)
  library(microbenchmark)

  microbenchmark(fdt[J(aids)], fdt[doc_i %in% aids], times=100)
}
