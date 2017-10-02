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
#' search_features(query, code = NA, feature = 'token',
#'                 mode = c('unique_hits','features'), verbose = F)
#'              }
#'
#' @param query A character string that is a query. See details for available query operators and modifiers. Can be multiple queries (as a vector), in which case it is recommended to also specifiy the code argument, to label results.
#' @param code The code given to the tokens that match the query (usefull when looking for multiple queries). Can also put code label in query with # (see details)
#' @param feature The name of the feature column within which to search.
#' @param mode There are two modes: "unique_hits" and "features". The "unique_hits" mode prioritizes finding full and unique matches., which is recommended for counting how often a query occurs. However, this also means that some tokens for which the query is satisfied might not assigned a hit_id. The "features" mode, instead, prioritizes finding all tokens, which is recommended for coding coding features (the code_features and search_recode methods always use features mode).
#' @param context_level Select whether the queries should occur within while "documents" or specific "sentences".
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
#'    \item{The ~g (ghost) flag can be used to mark a term (or all terms within parentheses/quotes) as a ghost term. This has two effects. Firstly, features that match the query term will not be in the results. This is usefull if a certain term is important for getting reliable search results, but not conceptually relevant. Secondly, ghost terms can be used multiple times, in different query hits (only relevant in unique_hits mode). For example, in the text "A B C", the query 'A~g AND (B C)' will return both B and C as separate hit, whereas 'A AND (B C)' will return A and B as a single hit.}
#'    \item{A code label can be included at the beginning of a query, followed by a # to start the query (label# query). Note that to search for a hashtag symbol, you need to escape it with \ (double \\ in R character vector)}
#'    \item{Aside from the feature column (specified with the feature argument) a query can include any column in the token data. To manually select a column, use 'columnname: ' at the start of a query or nested query (i.e. between parentheses or quotes). See examples for clarification.}
#'    }
#'
#' @name tCorpus$search_features
#' @aliases search_features
#' @examples
#' text = c('A B C', 'D E F. G H I', 'A D', 'GGG')
#' tc = create_tcorpus(text, doc_id = c('a','b','c','d'), split_sentences = TRUE)
#' tc$get() ## (example uses letters instead of words for simple query examples)
#'
#' hits = tc$search_features(c('query label# A AND B', 'second query# (A AND Q) OR ("D E") OR I'))
#' hits          ## print shows number of hits
#' hits$hits     ## hits is a list, with hits$hits being a data.frame with specific features
#' summary(hits) ## summary gives hits per query
#'
#' ## sentence level
#' hits = tc$search_features(c('query label# A AND B', 'second query# (A AND Q) OR ("D E") OR I'),
#'                           context_level = 'sentence')
#' hits$hits     ## hits is a list, with hits$hits being a data.frame with specific features
#'
#'
#' ## query language examples
#'
#' ## single term
#' tc$search_features('A')$hits
#'
#' tc$search_features('G*')$hits    ## wildcard *
#' tc$search_features('*G')$hits    ## wildcard *
#' tc$search_features('G*G')$hits   ## wildcard *
#'
#' tc$search_features('G?G')$hits   ## wildcard ?
#' tc$search_features('G?')$hits    ## wildcard ? (no hits)
#'
#' ## boolean
#' tc$search_features('A AND B')$hits
#' tc$search_features('A AND D')$hits
#' tc$search_features('A AND (B OR D)')$hits
#'
#' tc$search_features('A NOT B')$hits
#' tc$search_features('A NOT (B OR D)')$hits
#'
#'
#' ## sequence search (adjacent words)
#' tc$search_features('"A B"')$hits
#' tc$search_features('"A C"')$hits ## no hit, because not adjacent
#'
#' tc$search_features('"A (B OR D)"')$hits ## can contain nested OR
#' ## cannot contain nested AND or NOT!!
#'
#' tc$search_features('<A B>')$hits ## can also use <> instead of "".
#'
#' ## proximity search (using ~ flag)
#' tc$search_features('"A C"~5')$hits ## A AND C within a 5 word window
#' tc$search_features('"A C"~1')$hits ## no hit, because A and C more than 1 word apart
#'
#' tc$search_features('"A (B OR D)"~5')$hits ## can contain nested OR
#' tc$search_features('"A <B C>"~5')$hits    ## can contain nested sequence (must use <>)
#' tc$search_features('<A <B C>>~5')$hits    ## <> is always OK, but cannot nest "" in ""
#' ## cannot contain nested AND or NOT!!
#'
#' ## case sensitive search (~s flag)
#' tc$search_features('g')$hits     ## normally case insensitive
#' tc$search_features('g~s')$hits   ## use ~s flag to make term case sensitive
#'
#' tc$search_features('(a OR g)~s')$hits   ## use ~s flag on everything between parentheses
#' tc$search_features('(a OR G)~s')$hits
#'
#' tc$search_features('"a b"~s')$hits   ## use ~s flag on everything between quotes
#' tc$search_features('"A B"~s')$hits   ## use ~s flag on everything between quotes
#'
#' ## ghost terms (~g flag)
#' tc$search_features('A AND B~g')$hits    ## ghost term (~g) has to occur, but is not returned
#' tc$search_features('A AND Q~g')$hits    ## no hi
#'
#' # (can also be used on parentheses/quotes/anglebrackets for all nested terms)
#'
#'
#' ## "unique_hits" versus "features" mode
#' tc = create_tcorpus('A A B')
#'
#' tc$search_features('A AND B')$hits ## in "unique_hits" (default), only match full queries
#' # (B is not repeated to find a second match of A AND B)
#'
#' tc$search_features('A AND B', mode = 'features')$hits ## in "features", match any match
#' # (note that hit_id in features mode is irrelevant)
#'
#' # ghost terms (used for conditions) can be repeated
#' tc$search_features('A AND B~g')$hits
#'
#'
#' ## advanced queries
#' tc = tokens_to_tcorpus(corenlp_tokens, doc_col = 'doc_id',
#'                        sent_i_col = 'sentence', token_i_col = 'id')
#' head(tc$get()) ## search in multiple feature columns with "columnname: "
#'
#' ## using the sub/flag query to find only mary as a direct object
#' hits = tc$search_features('mary~{relation: dobj}', context_level = 'sentence')
#' hits$hits
#'
#' ## add a second sub query
#' hits = tc$search_features('mary~{relation: dobj, parent: 12 20}', context_level = 'sentence')
#' hits$hits
#'
#' ## selecting from a different column without changing the feature column
#' ## (can be used to combine columns)
#' hits = tc$search_features('relation: nsubj')
#' hits$hits
#'
#' hits = tc$search_features('(relation: nsubj) AND mary~g{relation: dobj}',
#'                           context_level = 'sentence')
#' hits$hits
#'
#' ## sequence: nsubj say*
#' hits = tc$search_features('"(relation: nsubj) say*"')
#' hits$hits
tCorpus$set('public', 'search_features', function(query, code=NULL, feature='token', mode = c('unique_hits','features'), context_level = c('document','sentence'), verbose=F){
  search_features(self, query, code=code, feature=feature, mode=mode, context_level=context_level, verbose=verbose)
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

search_features <- function(tc, query, code=NULL, feature='token', mode = c('unique_hits','features'), context_level=c('document','sentence'), verbose=F){
  .ghost = NULL ## for solving CMD check notes (data.table syntax causes "no visible binding" message)
  is_tcorpus(tc, T)
  mode = match.arg(mode)
  context_level = match.arg(context_level)

  if (!feature %in% tc$names) stop(sprintf('Feature (%s) is not available. Current options are: %s', feature, paste(tc$feature_names, collapse=', ')))
  codelabel = get_query_code(query, code)
  query = remove_query_label(query)

  subcontext = if(context_level == 'sentence') 'sent_i' else NULL
  hits = vector('list', length(query))
  for (i in 1:length(query)) {
    if (verbose) print(code[i])
    q = parse_query(as.character(query[i]))
    h = recursive_search(tc, q, subcontext=subcontext, feature=feature, mode = mode)
    if (!is.null(h)) {
      h[, code := codelabel[i]]
      hits[[i]] = h
    }
  }
  hits = data.table::rbindlist(hits)


  if (nrow(hits) > 0) {
    data.table::setnames(hits, feature, 'feature')
    setorderv(hits, c('doc_id','token_i'))
    hits = subset(hits, subset=!.ghost)
  } else {
    hits = data.frame(code=factor(), feature=factor(), doc_id=factor(), sent_i=numeric(), token_i = numeric(), hit_id=numeric())
  }
  queries = data.frame(code=codelabel, query=query)
  featureHits(hits, queries)
}

