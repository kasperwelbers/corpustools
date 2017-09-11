#' Search for documents or sentences using Boolean queries
#'
#' @section Usage:
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{search_contexts(query, code = NULL, feature = 'token', context_level = c('document','sentence'), verbose = F)}
#'
#' @param query A character string that is a query. See details for available query operators and modifiers. Can be multiple queries (as a vector), in which case it is recommended to also specifiy the code argument, to label results.
#' @param code If given, used as a label for the results of the query. Especially usefull if multiple queries are used.
#' @param feature The name of the feature column
#' @param context_level Select whether the queries should occur within while "documents" or specific "sentences". Returns results at the specified level.
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
#'    \item{Queries are not case sensitive, but can be made so by adding the ~s flag. e.g. COP~s only finds "COP" in uppercase. The ~s flag can also be used on quotes to make all terms within quotes case sensitive, and this can be combined with the token proximity flag. e.g. "Marco Polo"~s10}
#'  }
#'
#' @name tCorpus$search_contexts
#' @aliases search_contexts.tCorpus tCorpus$search_contexts
tCorpus$set('public', 'search_contexts', function(query, code=NULL, feature='token', context_level=c('document','sentence'), verbose=F){
  search_contexts(self, query, code=code, feature=feature, context_level=context_level, verbose=verbose)
})

#' Subset tCorpus token data using a query
#'
#' @description
#' A convenience function that searches for contexts (documents, sentences), and uses the results to \link[=tCorpus$search_contexts]{subset} the tCorpus token data.
#'
#' See the documentation for \link[=tCorpus$search_contexts]{subset} for an explanation of the query language.
#'
#' @section Usage:
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{subset_query(query, feature = 'token', context_level = c('document','sentence'))}
#'
#' @param query A character string that is a query. See \link{tCorpus$search_contexts} for query syntax.
#' @param feature The name of the feature columns on which the query is used.
#' @param context_level Select whether the query and subset are performed at the document or sentence level.
#'
#' @name tCorpus$subset_query
#' @aliases subset_query.tCorpus tCorpus$subset_query
tCorpus$set('public', 'subset_query', function(query, feature='token', context_level=c('document','sentence'), copy=F){
  if (copy) {
    selfcopy = self$copy()$subset_query(query=query, feature=feature, context_level=context_level, copy=F)
    return(selfcopy)
  }
  context_level = match.arg(context_level)
  hits = self$search_contexts(query, feature=feature, context_level=context_level)
  hits = hits$hits
  if (is.null(hits)) return(NULL)
  if (context_level == 'document'){
    self$select_meta_rows(self$get_meta('doc_id') %in% hits$doc_id)
  }
  if (context_level == 'sentence'){
    d = self$get(c('doc_id','sent_i'), keep_df=T)
    d$i = 1:nrow(d)
    rows = d[list(hits$doc_id, hits$sent_i)]$i
    self$select_rows(rows)
  }
  invisible(self)
})

#####################
#####################

## Function for the tCorpus$search_contexts method
search_contexts <- function(tc, query, code=NULL, feature='token', context_level=c('document','sentence'), verbose=F){
  is_tcorpus(tc, T)
  context_level = match.arg(context_level)
  if (!feature %in% tc$names) stop(sprintf('Feature (%s) is not available. Current options are: %s', feature, paste(tc$feature_names, collapse=', ')))
  codelabel = get_query_code(query, code)

  cols = if(context_level == 'sentence') c('doc_id','sent_i') else c('doc_id')
  subcontext = if(context_level == 'sentence') 'sent_i' else NULL

  hits = vector('list', length(query))
  for (i in 1:length(query)) {
    if (verbose) print(code[i])
    q = parse_query(as.character(query[i]))
    h = recursive_search(tc, q, subcontext=subcontext, feature=feature, mode = 'contexts')
    if (!is.null(h)) {
      h[, code := codelabel[i]]
      hits[[i]] = h
    }
  }
  hits = data.table::rbindlist(hits)

  if (nrow(hits) > 0) {
    setorderv(hits, cols)
  } else {
    hits = data.frame(code=factor(), doc_id=factor(), sent_i=numeric())
  }
  queries = data.frame(code=codelabel, query=query)
  contextHits(hits, queries)
}
