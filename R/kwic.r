#' Get keyword-in-context (KWIC) strings
#'
#' Create a data.frame with keyword-in-context strings for given indices (i), search results (hits) or search strings (keyword).
#'
#' This is mainly for viewing results in the R console. If you want to create a subset corpus based on the context
#' of query results, you can use \link{subset_query} with the window argument. Also, the \link{browse_hits} function
#' is a good alternative for viewing query hits in full text.
#'
#' @param tc a tCorpus
#' @param hits results of feature search. see \link{search_features}.
#' @param i instead of the hits argument, you can give the indices of features directly.
#' @param query instead of using the hits or i arguments, a search string can be given directly. Note that this simply a convenient shorthand for first creating a hits object with \link{search_features}. If a query is given, then the ... argument is used to pass other arguments to \link{tCorpus$search_features}.
#' @param code if 'i' or 'query' is used, the code argument can be used to add a code label. Should be a vector of the same length that gives the code for each i or query, or a vector of length 1 for a single label.
#' @param ntokens an integers specifying the size of the context, i.e. the number of tokens left and right of the keyword.
#' @param n a number, specifying the total number of hits
#' @param nsample like n, but with a random sample of hits. If multiple codes are used, the sample is drawn for each code individually.
#' @param query_feature  If query is used, the feature column that is used to perform the query
#' @param output_feature the feature column that is used to make the KWIC.
#' @param context_level Select the maxium context (document or sentence).
#' @param kw_tag a character vector of length 2, that gives the symbols before (first value) and after (second value) the keyword in the KWIC string. Can for instance be used to prepare KWIC with format tags for highlighting.
#' @param ... See \link{search_features} for the query parameters
#'
#' @export
#' @examples
#' tc = tokens_to_tcorpus(corenlp_tokens, sentence_col = 'sentence', token_id_col = 'id')
#'
#' ## look directly for a term (or complex query)
#' get_kwic(tc, query = 'love*')
#'
#' ## or, first perform a feature search, and then get the KWIC for the results
#' hits = search_features(tc, '(john OR mark) AND mary AND love*', context_level = 'sentence')
#' get_kwic(tc, hits=hits, context_level = 'sentence')
get_kwic <- function(tc, hits=NULL, i=NULL, query=NULL, code='', ntokens=10, n=NA, nsample=NA, output_feature='token', query_feature='token', context_level=c('document','sentence'), kw_tag=c('<','>'), ...){
  if (!is.null(query)) hits = search_features(tc, query=query, code=code, feature = query_feature, ...)
  keyword_in_context(tc, hits=hits, i=i, code=code, ntokens=ntokens, n=n, nsample=nsample, output_feature=output_feature, context_level=context_level, kw_tag=kw_tag)
}

#################################
#################################

keyword_in_context <- function(tc, hits=NULL, i=NULL, code='', ntokens=10, n=NA, nsample=NA, output_feature='token', context_level=c('document', 'sentence'), kw_tag=c('<','>')){
  feature = NULL; hit_id = NULL  ## for data.table
  if (class(i) == 'logical') i = which(i)
  ## remove i and code parameters

  if(!is.featureHits(hits)) stop('hits must be a featureHits object (created with search_features()')
  hits$hits$hit_id = stringi::stri_paste(hits$hits$code, hits$hits$hit_id, sep=' ')
  d = hits$hits

  if(!is.na(nsample)) n = nsample
  if(!is.na(n)) {
    hit_ids = unique(d$hit_id)
    if (n < length(hit_ids)) {
      hit_ids = if(!is.na(nsample)) sample(hit_ids, n) else head(hit_ids, n)
      d = d[d$hit_id %in% hit_ids,]
    }
  }

  shifts = -ntokens:ntokens
  n = nrow(d)
  d = d[rep(1:nrow(d), each=length(shifts)), c('doc_id','hit_id','token_id')]
  d$token_id = d$token_id + shifts
  d$is_kw = rep(shifts == 0, n)
  d$feature = tc$get(output_feature, doc_id = d$doc_id, token_id = d$token_id)
  d = d[!is.na(d$feature),] ## positions that do not exist (token_id out of bounds) returned NA in tc$get

  d$feature = as.character(d$feature)
  d$feature[d$is_kw] = sprintf('%s%s%s', kw_tag[1], d$feature[d$is_kw], kw_tag[2])

  ## kwic's of the same hit_id should be merged.
  d = d[order(d$hit_id, d$token_id, -d$is_kw),]
  d = d[!duplicated(d[,c('hit_id','token_id')]),]

  ## add tag for gap between kwic of merged hit_ids that are not adjacent
  same_hit_id = d$hit_id == shift(d$hit_id, 1, fill = -1)
  not_adjacent = d$token_id - (shift(d$token_id, 1, fill=-1)) > 1
  gap = same_hit_id & not_adjacent
  d$feature[gap] = sprintf('[...] %s', d$feature[gap])

  ## paste features together
  setkey(d, 'hit_id')
  kwic = d[, list(feature=list(feature)), by=hit_id]
  kwic = data.frame(hit_id = kwic$hit_id,
                    kwic = stringi::stri_paste_list(kwic$feature, sep = ' '))
  kwic$kwic = pretty_kwic(kwic$kwic)

  add = hits$hits[hits$hits$hit_id %in% kwic$hit_id, c('doc_id','hit_id','code', 'feature')]
  setkey(add, 'hit_id')
  feature = add[, list(feature=list(feature)), by=hit_id]
  add = unique(add, by='hit_id')
  add$feature = stringi::stri_paste_list(feature$feature, sep = ' -> ')

  kwic = merge(kwic, add, by='hit_id', all.x=T)
  kwic$hit_id = as.numeric(stringi::stri_extract(regex = '[0-9]+$', kwic$hit_id))
  kwic[,c('doc_id','code','hit_id','feature','kwic')]
}

pretty_kwic <- function(x) {
  x = pretty_text_paste(x)
  sprintf('...%s...', x)
}
