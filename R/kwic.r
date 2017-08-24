#' Get keyword-in-context (KWIC) strings
#'
#' @description
#' Create a data.frame with keyword-in-context strings for given indices (i), search results (hits) or search strings (keyword).
#'
#' @section Usage:
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{
#' kwic(hits = NULL, i = NULL, query = NULL, code = '',
#'      ntokens = 10, nsample = NA, output_feature = 'token',
#'      context_levels = c('document','sentence'),
#'      prettypaste = T, kw_tag = c('<','>'), ...)
#' }
#'
#' @param hits results of feature search. see \link{tCorpus$search_features}.
#' @param i instead of the hits argument, you can give the indices of features directly.
#' @param query instead of using the hits or i arguments, a search string can be given directly. Note that this simply a convenient shorthand for first creating a hits object with \link{tCorpus$search_features}. If a query is given, then the ... argument is used to pass other arguments to \link{tCorpus$search_features}.
#' @param code if 'i' or 'query' is used, the code argument can be used to add a code label. Should be a vector of the same length that gives the code for each i or query, or a vector of length 1 for a single label.
#' @param ntokens an integers specifying the size of the context, i.e. the number of tokens left and right of the keyword.
#' @param nsample optionally, get a random sample of the keywords/features. If multiple codes are used, the sample is drawn for each code individually.
#' @param output_feature the feature column that is used to make the KWIC.
#' @param context_level Select the maxium context (document or sentence).
#' @param kw_tag a character vector of length 2, that gives the symbols before (first value) and after (second value) the keyword in the KWIC string. Can for instance be used to prepare KWIC with format tags for highlighting.
#' @param ... See \link{tCorpus$search_features} for the query parameters
#'
#' @name tCorpus$kwic
#' @aliases kwic.tCorpus
tCorpus$set('public', 'kwic', function(hits=NULL, i=NULL, feature=NULL, query=NULL, code='', ntokens=10, nsample=NA, output_feature='token', query_feature='token', context_level=c('document','sentence'), kw_tag=c('<','>'), ...){
  if (!is.null(query)) hits = self$search_features(query=query, code=code, feature = query_feature, ...)
  keyword_in_context(self, hits=hits, i=i, code=code, ntokens=ntokens, nsample=nsample, output_feature=output_feature, context_level=context_level, kw_tag=kw_tag)
})

#################################
#################################

keyword_in_context <- function(tc, hits=NULL, i=NULL, code='', ntokens=10, nsample=NA, output_feature='token', context_level=c('document', 'sentence'), kw_tag=c('<','>')){
  if (class(i) == 'logical') i = which(i)
  ## remove i and code parameters

  if(!is.featureHits(hits)) stop('hits must be a featureHits object (created with the $search_features() method')
  d = hits$hits

  if(!is.na(nsample)) {
    hit_ids = unique(d$hit_id)
    if (nsample < length(hit_ids)) d = d[d$hit_id %in% sample(hit_ids, nsample),]
  }

  shifts = -ntokens:ntokens
  n = nrow(d)
  d = d[rep(1:nrow(d), each=length(shifts)), c('doc_id','hit_id','token_i')]
  d$token_i = d$token_i + shifts
  d$is_kw = rep(shifts == 0, n)
  d$feature = tc$get(output_feature, doc_id = d$doc_id, token_i = d$token_i)
  d = d[!is.na(d$feature),] ## positions that do not exist (token_i out of bounds) returned NA in tc$get

  d$feature = as.character(d$feature)
  d$feature[d$is_kw] = sprintf('%s%s%s', kw_tag[1], d$feature[d$is_kw], kw_tag[2])

  ## kwic's of the same hit_id should be merged.
  d = d[order(d$hit_id, d$token_i, -d$is_kw),]
  d = d[!duplicated(d[,c('hit_id','token_i')]),]

  ## add tag for gap between kwic of merged hit_ids that are not adjacent
  same_hit_id = d$hit_id == shift(d$hit_id, 1, fill = -1)
  not_adjacent = d$token_i - (shift(d$token_i, 1, fill=-1)) > 1
  gap = same_hit_id & not_adjacent
  d$feature[gap] = sprintf('[...] %s', d$feature[gap])

  ## paste features together
  kwic = split(as.character(d$feature), d$hit_id)
  kwic = data.frame(hit_id = as.numeric(names(kwic)),
                    kwic = stringi::stri_paste_list(kwic, sep = ' '))
  kwic$kwic = pretty_kwic(kwic$kwic)

  add = hits$hits[hits$hits$hit_id %in% kwic$hit_id, c('doc_id','hit_id','code', 'feature')]
  feature = split(as.character(add$feature), add$hit_id)
  add = add[!duplicated(add$hit_id),]
  add$feature = stringi::stri_paste_list(feature, sep = ' -> ')

  kwic = merge(kwic, add, by='hit_id', all.x=T)
  kwic[,c('doc_id','code','hit_id','feature','kwic')]
}

pretty_kwic <- function(x) {
  x = pretty_text_paste(x)
  sprintf('...%s...', x)
}
