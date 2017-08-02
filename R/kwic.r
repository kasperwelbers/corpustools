#' Get keyword-in-context (KWIC) strings
#'
#' @description
#' Create a data.frame with keyword-in-context strings for given indices (i), search results (hits) or search strings (keyword).
#'
#' @section Usage:
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{
#' kwic(hits = NULL, i = NULL, keyword = NULL, code = '',
#'      ntokens = 10, nsample = NA, output_feature = 'token',
#'      context_levels = c('document','sentence'),
#'      prettypaste = T, kw_tag = c('<','>'), ...)
#' }
#'
#' @param hits results of feature search. see \link{tCorpus$search_features}.
#' @param i instead of the hits argument, you can give the indices of features directly.
#' @param keyword instead of using the hits or i arguments, a search string can be given directly. Note that this simply a convenient shorthand for first creating a hits object with \link{tCorpus$search_features}. If a keyword is given, then the ... argument is used to pass other arguments to \link{tCorpus$search_features}.
#' @param code if 'i' or 'keyword' is used, the code argument can be used to add a code label. Should be a vector of the same length that gives the code for each i or keyword, or a vector of length 1 for a single label.
#' @param ntokens an integers specifying the size of the context, i.e. the number of tokens left and right of the keyword.
#' @param nsample optionally, get a random sample of the keywords/features. If multiple codes are used, the sample is drawn for each code individually.
#' @param output_feature the feature column that is used to make the KWIC.
#' @param context_level Select the maxium context (document or sentence).
#' @param kw_tag a character vector of length 2, that gives the symbols before (first value) and after (second value) the keyword in the KWIC string. Can for instance be used to prepare KWIC with format tags for highlighting.
#' @param ... See \link{tCorpus$search_features} for the query parameters
#'
#' @name tCorpus$kwic
#' @aliases kwic.tCorpus
tCorpus$set('public', 'kwic', function(hits=NULL, i=NULL, feature=NULL, keyword=NULL, code='', ntokens=10, nsample=NA, output_feature='token', keyword_feature='token', context_level=c('document','sentence'), kw_tag=c('<','>'), ...){
  if (!is.null(keyword)) hits = self$search_features(keyword=keyword, code=code, feature = keyword_feature, ...)
  keyword_in_context(self, hits=hits, i=i, code=code, ntokens=ntokens, nsample=nsample, output_feature=output_feature, context_level=context_level, kw_tag=kw_tag)
})

#################################
#################################


keyword_in_context <- function(tc, hits=NULL, i=NULL, code='', ntokens=10, nsample=NA, output_feature='token', context_level=c('document', 'sentence'), kw_tag=c('<','>')){
  if (class(i) == 'logical') i = which(i)
  ## first filter tokens on document id (to speed up computation)

  if (is.null(tc$provenance('index_feature'))){
    gi = tc$feature_index(feature=output_feature, context_level=context_level, max_window_size = ntokens)$global_i
  } else {
    gi = tc$feature_index(feature=tc$provenance('index_feature'), context_level=context_level, max_window_size = ntokens, as_ascii = tc$provenance('as_ascii'))$global_i
  }
  gi = data.table::fsort(gi)

  gfv = globalFeatureVector$new(tc$get(output_feature), gi)

  if (!is.null(hits)) {
    if(!is.featureHits(hits)) stop('hits must be a featureHits object (created with the $search_features() method')
    d = tc$get(c('doc_id', 'token_i'))
    d$i = 1:nrow(d)
    setkeyv(d, c('doc_id', 'token_i'))
    i = d[hits$hits[,c('doc_id', 'token_i')]]$i
    code = hits$hits$code
    hit_id = hits$hits$hit_id
  } else {
    if(length(code) == 1) {
      code = rep(code, length(i))
      hit_id = 1:length(i)
    } else {
      hit_id = match(code, unique(code))
    }
  }
  global_i = gi[i]

  if (length(code) == 0) return(NULL)

  if(!is.na(nsample)) {
    hit_id_samp = head(sample(unique(hit_id)), nsample)
    samp = hit_id %in% hit_id_samp
    hit_id = hit_id[samp]
    code = code[samp]
    i = i[samp]
    global_i = global_i[samp]
  }

  shifts = -ntokens:ntokens
  d = data.frame(global_i = rep(global_i, each=length(shifts)) + shifts,
                 hit_id = rep(hit_id, each=length(shifts)),
                 is_kw = rep(shifts == 0, length(global_i)))
  d = d[d$global_i > 0 & d$global_i <= max(gi),]

  ## kwic's of the same hit_id should be merged.
  d = d[order(d$hit_id, d$global_i, -d$is_kw),]
  d = d[!duplicated(d[,c('hit_id','global_i')]),]

  d$feature = gfv[d$global_i, ignore_empty = F]
  d$feature[d$is_kw] = sprintf('%s%s%s', kw_tag[1], d$feature[d$is_kw], kw_tag[2])

  ## add tag for gap between kwic of merged hit_ids that are not adjacent
  same_hit_id = d$hit_id == shift(d$hit_id, 1, fill = -1)
  not_adjacent = d$global_i - (shift(d$global_i, 1, fill=-1)) > 1
  gap = same_hit_id & not_adjacent
  d$feature[gap] = sprintf('[...] %s', d$feature[gap])

  d = d[!d$feature == '',]
  kwic = split(as.character(d$feature), d$hit_id)
  kwic = sapply(kwic, stringi::stri_flatten, collapse=' ')

  kwic = data.frame(hit_id = as.numeric(names(kwic)),
                    kwic = pretty_kwic(kwic))

  add = data.frame(hit_id = hit_id, doc_id = tc$get('doc_id')[i], code=code)
  add = add[!duplicated(add$hit_id),]
  feature = split(tc$get(output_feature)[i], hit_id)
  add$feature = sapply(feature, stringi::stri_flatten, collapse=' -> ')

  kwic = merge(kwic, add, by='hit_id', all.x=T)
  kwic[,c('doc_id','code','hit_id','feature','kwic')]
}

pretty_kwic <- function(x) {
  x = pretty_text_paste(x)
  sprintf('...%s...', x)
}
