


#' Code features in a tCorpus based on a search string
#'
#' @description
#' Add a column to the token data that contains a code (the query label) for tokens that match the query (see \link{tCorpus$search_features}).
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{code_features(query, code=NULL, feature='token', column='code', ...)}
#'
#' @param query A character string that is a query. See \link{search_features} for documentation of the query language.
#' @param code The code given to the tokens that match the query (usefull when looking for multiple queries). Can also put code label in query with # (see details)
#' @param feature The name of the feature column within which to search.
#' @param column The name of the column that is added to the data
#' @param add_column list of name-value pairs, used to add additional columns. The name will become the column name, and the value should be a vector of the same length as the query vector.
#' @param context_level Select whether the queries should occur within while "documents" or specific "sentences".
#' @param keep_longest If TRUE, then overlapping in case of overlapping queries strings in unique_hits mode, the query with the most separate terms is kept. For example, in the text "mr. Bob Smith", the query [smith OR "bob smith"] would match "Bob" and "Smith". If keep_longest is FALSE, the match that is used is determined by the order in the query itself. The same query would then match only "Smith".
#' @param as_ascii if TRUE, perform search in ascii.
#' @param verbose If TRUE, progress messages will be printed
#' @param overwrite If TRUE (default) and column already exists, overwrite previous results.
#' @param ... alternative way to specify name-value pairs for adding additional columns
#'
#' @name tCorpus$code_features
#' @examples
#' tc = create_tcorpus('Anna and Bob are secretive')
#'
#' tc$code_features(c("actors# anna bob", "associations# secretive"))
#' tc$tokens
#' @aliases code_features
tCorpus$set('public', 'code_features', function(query, code=NULL, feature='token', column='code', add_column=list(), context_level = c('document','sentence'), keep_longest=T, as_ascii=F, verbose=F, overwrite=T, ...){
  codelabel = get_query_code(query, code)
  code = 1:length(query)
  hits = search_features(self, query, code=code, feature=feature, mode='features', context_level=context_level, keep_longest=keep_longest, as_ascii=as_ascii, verbose=verbose)

  .i = self$get_token_id(doc_id = hits$hits$doc_id, token_id = hits$hits$token_id)
  .value = codelabel[as.numeric(hits$hits$code)]
  if (column %in% self$names && overwrite) self$delete_columns(column)
  self$set(column=column, subset=.i, value=.value, subset_value=F)

  add_column = c(add_column, list(...))
  if (length(add_column) > 0) {
    for (i in 1:length(add_column)) {
      .value = add_column[[i]][as.numeric(hits$hits$code)]
      if (names(add_column)[i] %in% self$names && overwrite) self$delete_columns(names(add_column)[i])
      self$set(column=names(add_column)[i], subset=.i, value=.value, subset_value=F)
    }
  }

  invisible(self)
})



#' Recode features in a tCorpus based on a search string
#'
#' @description
#' Search features (see \link{tCorpus$search_features}) and replace features with a new value
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{
#' search_recode(feature, new_value, keyword, condition = NA, condition_once = F)
#' }
#'
#' @param feature The feature in which to search
#' @param new_value the character string with which all features that are found are replaced
#' @param query See \link{tCorpus$search_features} for the query parameters
#' @param ... Additional search_features parameters. See \link{tCorpus$search_features}
#'
#' @name tCorpus$search_recode
#' @aliases search_recode
tCorpus$set('public', 'search_recode', function(feature, new_value, query, ...){
  hits = search_features(self, query, feature=feature, mode='features', ...)
  .i = self$get_token_id(doc_id = hits$hits$doc_id, token_id = hits$hits$token_id)
  .new_value = new_value
  self$set(feature, .new_value, subset = .i)
  invisible(self)
})


#' Find tokens using a Lucene-like search query
#'
#' @description
#' Search tokens in a tokenlist using Lucene-like queries. For a detailed explanation of the query language, see the details below.
#'
#'
#' @param tc a \code{\link{tCorpus}}
#' @param query A character string that is a query. See details for available query operators and modifiers. Can be multiple queries (as a vector), in which case it is recommended to also specifiy the code argument, to label results.
#' @param code The code given to the tokens that match the query (usefull when looking for multiple queries). Can also put code label in query with # (see details)
#' @param feature The name of the feature column within which to search.
#' @param mode There are two modes: "unique_hits" and "features". The "unique_hits" mode prioritizes finding full and unique matches., which is recommended for counting how often a query occurs. However, this also means that some tokens for which the query is satisfied might not assigned a hit_id. The "features" mode, instead, prioritizes finding all tokens, which is recommended for coding coding features (the code_features and search_recode methods always use features mode).
#' @param context_level Select whether the queries should occur within while "documents" or specific "sentences".
#' @param keep_longest If TRUE, then overlapping in case of overlapping queries strings in unique_hits mode, the query with the most separate terms is kept. For example, in the text "mr. Bob Smith", the query [smith OR "bob smith"] would match "Bob" and "Smith". If keep_longest is FALSE, the match that is used is determined by the order in the query itself. The same query would then match only "Smith".
#' @param as_ascii if TRUE, perform search in ascii.
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
#' @return A featureHits object, which is a list with $hits (data.frame with locations) and $queries (copy of queries for provenance)
#' @export
#' @examples
#' text = c('A B C', 'D E F. G H I', 'A D', 'GGG')
#' tc = create_tcorpus(text, doc_id = c('a','b','c','d'), split_sentences = TRUE)
#' tc$tokens ## (example uses letters instead of words for simple query examples)
#'
#' hits = tc$search_features(c('query label# A AND B', 'second query# (A AND Q) OR ("D E") OR I'))
#' hits          ## print shows number of hits
#' hits$hits     ## hits is a list, with hits$hits being a data.frame with specific features
#' summary(hits) ## summary gives hits per query
#'
#' ## sentence level
#' hits = search_features(tc, c('query label# A AND B', 'second query# (A AND Q) OR ("D E") OR I'),
#'                           context_level = 'sentence')
#' hits$hits     ## hits is a list, with hits$hits being a data.frame with specific features
#'
#'
#' \donttest{
#'
#' ## query language examples
#'
#' ## single term
#' search_features(tc, 'A')$hits
#'
#' search_features(tc, 'G*')$hits    ## wildcard *
#' search_features(tc, '*G')$hits    ## wildcard *
#' search_features(tc, 'G*G')$hits   ## wildcard *
#'
#' search_features(tc, 'G?G')$hits   ## wildcard ?
#' search_features(tc, 'G?')$hits    ## wildcard ? (no hits)
#'
#' ## boolean
#' search_features(tc, 'A AND B')$hits
#' search_features(tc, 'A AND D')$hits
#' search_features(tc, 'A AND (B OR D)')$hits
#'
#' search_features(tc, 'A NOT B')$hits
#' search_features(tc, 'A NOT (B OR D)')$hits
#'
#'
#' ## sequence search (adjacent words)
#' search_features(tc, '"A B"')$hits
#' search_features(tc, '"A C"')$hits ## no hit, because not adjacent
#'
#' search_features(tc, '"A (B OR D)"')$hits ## can contain nested OR
#' ## cannot contain nested AND or NOT!!
#'
#' search_features(tc, '<A B>')$hits ## can also use <> instead of "".
#'
#' ## proximity search (using ~ flag)
#' search_features(tc, '"A C"~5')$hits ## A AND C within a 5 word window
#' search_features(tc, '"A C"~1')$hits ## no hit, because A and C more than 1 word apart
#'
#' search_features(tc, '"A (B OR D)"~5')$hits ## can contain nested OR
#' search_features(tc, '"A <B C>"~5')$hits    ## can contain nested sequence (must use <>)
#' search_features(tc, '<A <B C>>~5')$hits    ## <> is always OK, but cannot nest "" in ""
#' ## cannot contain nested AND or NOT!!
#'
#' ## case sensitive search (~s flag)
#' search_features(tc, 'g')$hits     ## normally case insensitive
#' search_features(tc, 'g~s')$hits   ## use ~s flag to make term case sensitive
#'
#' search_features(tc, '(a OR g)~s')$hits   ## use ~s flag on everything between parentheses
#' search_features(tc, '(a OR G)~s')$hits
#'
#' search_features(tc, '"a b"~s')$hits   ## use ~s flag on everything between quotes
#' search_features(tc, '"A B"~s')$hits   ## use ~s flag on everything between quotes
#'
#' ## ghost terms (~g flag)
#' search_features(tc, 'A AND B~g')$hits    ## ghost term (~g) has to occur, but is not returned
#' search_features(tc, 'A AND Q~g')$hits    ## no hi
#'
#' # (can also be used on parentheses/quotes/anglebrackets for all nested terms)
#'
#'
#' ## "unique_hits" versus "features" mode
#' tc = create_tcorpus('A A B')
#'
#' search_features(tc, 'A AND B')$hits ## in "unique_hits" (default), only match full queries
#' # (B is not repeated to find a second match of A AND B)
#'
#' search_features(tc, 'A AND B', mode = 'features')$hits ## in "features", match any match
#' # (note that hit_id in features mode is irrelevant)
#'
#' # ghost terms (used for conditions) can be repeated
#' search_features(tc, 'A AND B~g')$hits
#'
#' ## advanced queries
#' tc = tokens_to_tcorpus(corenlp_tokens, doc_col = 'doc_id',
#'                        sentence_col = 'sentence', token_id_col = 'id')
#' head(tc$tokens) ## search in multiple feature columns with "columnname: "
#'
#' ## using the sub/flag query to find only mary as a direct object
#' hits = search_features(tc, 'mary~{relation: dobj}', context_level = 'sentence')
#' hits$hits
#'
#' ## add a second sub query
#' hits = search_features(tc, 'mary~{relation: dobj, parent: 12 20}', context_level = 'sentence')
#' hits$hits
#'
#' ## selecting from a different column without changing the feature column
#' ## (can be used to combine columns)
#' hits = search_features(tc, 'relation: nsubj')
#' hits$hits
#'
#' hits = search_features(tc, '(relation: nsubj) AND mary~g{relation: dobj}',
#'                           context_level = 'sentence')
#' hits$hits
#'
#' ## sequence: nsubj say*
#' hits = search_features(tc, '"(relation: nsubj) say*"')
#' hits$hits
#' }
search_features <- function(tc, query, code=NULL, feature='token', mode = c('unique_hits','features'), context_level=c('document','sentence'), keep_longest=TRUE, as_ascii=F, verbose=F){
  .ghost = NULL ## for solving CMD check notes (data.table syntax causes "no visible binding" message)
  is_tcorpus(tc)
  mode = match.arg(mode)
  context_level = match.arg(context_level)

  if (!feature %in% tc$names) stop(sprintf('Feature (%s) is not available. Current options are: %s', feature, paste(tc$feature_names, collapse=', ')))
  codelabel = get_query_code(query, code)
  query = remove_query_label(query)
  subcontext = if(context_level == 'sentence') 'sentence' else NULL
  hits = vector('list', length(query))

  lookup_tables = list()
  for (i in 1:length(query)) {
    if (verbose) cat(as.character(codelabel[i]), '\n')
    q = parse_query_cpp(as.character(query[i]))

    lookup_tables = prepare_lookup_tables(tc, q, lookup_tables, feature = feature, as_ascii = as_ascii)
    h = recursive_search(tc, q, lookup_tables=lookup_tables, subcontext=subcontext, feature=feature, mode = mode, keep_longest=keep_longest, as_ascii=as_ascii)

    if (!is.null(h)) {
      h[, code := codelabel[i]]
      hits[[i]] = h
    }
  }
  t = tc$tokens
  hits = data.table::rbindlist(hits, fill = T)

  if (nrow(hits) > 0) {
    data.table::setnames(hits, feature, 'feature')
    setorderv(hits, c('doc_id','token_id'))
    hits = subset(hits, subset=!.ghost)
  } else {
    hits = data.frame(code=factor(), feature=factor(), doc_id=factor(), sentence=numeric(), token_id = numeric(), hit_id=numeric())
  }

  queries = data.frame(code=codelabel, query=query)
  featureHits(hits, queries)
}

