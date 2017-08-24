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
#' @param query A character string that is a query. See details for available query operators and modifiers. Can be multiple queries (as a vector), in which case it is recommended to also specifiy the code argument, to label results.
#' @param code The code given to the tokens that match the query (usefull when looking for multiple queries)
#' @param feature The name of the feature column within which to search.
#' @param mode There are two modes: "unique_hits" and "features". The "unique_hits" mode prioritizes finding full and unique matches., which is recommended for counting how often a query occurs. However, this also means that some tokens for which the query is satisfied might not assigned a hit_id. The "features" mode, instead, prioritizes finding all tokens, which is recommended for coding coding features (the code_features and search_recode methods always use features mode).
#' @param verbose If TRUE, progress messages will be printed
#'
#' @details
#' Brief summary of the query language
#'
#' The following operators and modifiers are supported:
#' \itemize{
#'    \item{The standaard Boolean operators: AND, OR and NOT. As a shorthand, an empty space can be used as an OR statement, so that "this that those" means "this OR that OR those". NOT statements stricly mean AND NOT, so should only be used between terms. If you want to find \emph{everything except} certain terms, you can use * (wildcard for \emph{anything}) like this: "* NOT (this that those)".}
#'    \item{For complex queries parentheses can (and should) be used. e.g. '(spam AND eggs) NOT (fish and (chips OR albatros))}
#'    \item{Wildcards ? and *. The questionmark can be used to match 1 unknown character or no character at all, e.g. "?at" would find "cat", "hat" and "at". The asterisk can be used to match any number of unknown characters. Both the asterisk and questionmark can be used at the start, end and within a term.}
#'    \item{Multitoken strings, or exact strings, can be specified using quotes. e.g. "united states"}
#'    \item{tokens within a given token distance can be found using quotes plus tilde and a number specifiying the token distance. e.g. "climate chang*"~10}
#'    \item{Alternatively, angle brackets (<>) can be used instead of quotes, which also enables nesting exact strings in proximity/window search}
#'    \item{Queries are not case sensitive, but can be made so by adding the ~s flag. e.g. COP~s only finds "COP" in uppercase. The ~s flag can also be used on parentheses or quotes to make all terms within case sensitive, and this can be combined with the token proximity flag. e.g. "Marco Polo"~s10}
#'    \item{The ~i (invisible) flag can be used to ignore a feature in the results. This is usefull if a certain term is important for getting reliable search results, but not conceptually relevant. This flag can also be used on parentheses or quotes}
#'  }
#'
#' @name tCorpus$search_features
#' @aliases search_features.tCorpus
tCorpus$set('public', 'search_features', function(query, code=NULL, feature='token', mode = c('unique_hits','features'), verbose=F){
  search_features(self, query, code=code, feature=feature, mode=mode, verbose=verbose)
})

tCorpus$set('public', 'code_features', function(query, code=NULL, feature='token', column='code', verbose=F){
  hits = search_features(self, query, code=code, feature=feature, mode='features', verbose=verbose)

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
tCorpus$set('public', 'search_recode', function(feature, new_value, query){
  hits = search_features(self, query, feature=feature, mode='features')
  .i = self$token_i(doc_id = hits$hits$doc_id, token_i = hits$hits$token_i)
  .new_value = new_value
  self$set(feature, .new_value, subset = .i)
  invisible(self)
})

search_features <- function(tc, query, code=NULL, feature='token', mode = c('unique_hits','features'), verbose=F){
  .invisible = NULL ## for solving CMD check notes (data.table syntax causes "no visible binding" message)
  is_tcorpus(tc, T)
  mode = match.arg(mode)

  if (!feature %in% tc$names) stop(sprintf('Feature (%s) is not available. Current options are: %s', feature, paste(tc$feature_names, collapse=', ')))
  codelabel = get_query_code(query, code)

  hits = vector('list', length(query))
  for (i in 1:length(query)) {
    if (verbose) print(code[i])
    q = parse_query(as.character(query[i]))
    h = recursive_search(tc, q, subcontext=NULL, feature=feature, mode = mode)
    if (!is.null(h)) {
      h[, code := codelabel[i]]
      hits[[i]] = h
    }
  }
  hits = data.table::rbindlist(hits)


  if (nrow(hits) > 0) {
    data.table::setnames(hits, feature, 'feature')
    setorderv(hits, c('doc_id','token_i'))
    hits = subset(hits, subset=!.invisible)
  } else {
    hits = data.frame(code=factor(), feature=factor(), doc_id=factor(), sent_i=numeric(), token_i = numeric(), hit_id=numeric())
  }
  queries = data.frame(code=codelabel, query=query)
  featureHits(hits, queries)
}

