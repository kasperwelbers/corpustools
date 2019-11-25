

#' Subset tCorpus token data using a query
#'
#' @description
#' A convenience function that searches for contexts (documents, sentences), and uses the results to \link[=subset]{subset} the tCorpus token data.
#'
#' See the documentation for \link[=search_contexts]{search_contexts} for an explanation of the query language.
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{subset_query(query, feature = 'token', context_level = c('document','sentence','window'))}
#'
#' @param query A character string that is a query. See \link{search_contexts} for query syntax.
#' @param feature The name of the feature columns on which the query is used.
#' @param context_level Select whether the query and subset are performed at the document or sentence level.
#' @param window  If used, uses a word distance as the context (overrides context_level)
#' @param copy    If true, return modified copy of data instead of subsetting the input tcorpus by reference.
#'
#' @name tCorpus$subset_query
#' @examples
#' text = c('A B C', 'D E F. G H I', 'A D', 'GGG')
#' tc = create_tcorpus(text, doc_id = c('a','b','c','d'), split_sentences = TRUE)
#'
#' ## subset by reference
#' tc$subset_query('A')
#' tc$meta
#'
#' ## using copy mechanic
#' class(tc$tokens$doc_id)
#' tc2 = tc$subset_query('A AND D', copy=TRUE)
#'
#' tc2$get_meta()
#'
#' tc$meta ## (unchanged)
tCorpus$set('public', 'subset_query', function(query, feature='token', context_level=c('document','sentence'), window=NA, copy=F){
  context_level = match.arg(context_level)

  if (!is.na(window)) {
    hits = self$search_features(query, feature=feature, context_level=context_level, mode='features')
    if (is.null(hits)) return(NULL)
    window = self$get_token_id(hits$hits$doc_id, hits$hits$token_id, window=window)
    out = self$subset(window, copy=copy)
  } else {
    hits = search_contexts(self, query, feature=feature, context_level=context_level)
    if (is.null(hits)) return(NULL)
    if (context_level == 'document'){
      #self$select_meta_rows(self$get_meta('doc_id') %in% hits$hits$doc_id)
      .doc_ids = hits$hits$doc_id
      out = self$subset(subset_meta= doc_id %in% .doc_ids, copy=copy)
    }
    if (context_level == 'sentence'){
      d = self$get(c('doc_id','sentence'), keep_df=T)
      d$i = 1:nrow(d)
      setkeyv(d, c('doc_id','sentence'))
      .rows = d[list(hits$hits$doc_id, hits$hits$sentence),]$i
      #self$select_rows(rows)
      out = self$subset(subset=.rows, copy=copy)
    }
  }
  invisible(out)
})

#####################
#####################

#' Search for documents or sentences using Boolean queries
#'
#' @param tc a \code{\link{tCorpus}}
#' @param query A character string that is a query. See details for available query operators and modifiers. Can be multiple queries (as a vector), in which case it is recommended to also specifiy the code argument, to label results.
#' @param code If given, used as a label for the results of the query. Especially usefull if multiple queries are used.
#' @param feature The name of the feature column
#' @param context_level Select whether the queries should occur within while "documents" or specific "sentences". Returns results at the specified level.
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
#'    \item{Queries are not case sensitive, but can be made so by adding the ~s flag. e.g. COP~s only finds "COP" in uppercase. The ~s flag can also be used on quotes to make all terms within quotes case sensitive, and this can be combined with the token proximity flag. e.g. "Marco Polo"~s10}
#'  }
#'
#' @return A contextHits object, which is a list with $hits (data.frame with locations) and $queries (copy of queries for provenance)
#' @export
#' @examples
#' text = c('A B C', 'D E F. G H I', 'A D', 'GGG')
#' tc = create_tcorpus(text, doc_id = c('a','b','c','d'), split_sentences = TRUE)
#' tc$tokens
#'
#' hits = search_contexts(tc, c('query label# A AND B', 'second query# (A AND Q) OR ("D E") OR I'))
#' hits          ## print shows number of hits
#' hits$hits     ## hits is a list, with hits$hits being a data.frame with specific contexts
#' summary(hits) ## summary gives hits per query
#'
#' ## sentence level
#' hits = search_contexts(tc, c('query label# A AND B', 'second query# (A AND Q) OR ("D E") OR I'),
#'                           context_level = 'sentence')
#' hits$hits     ## hits is a list, with hits$hits being a data.frame with specific contexts
#'
#' \donttest{
#'
#' ## query language examples
#'
#' ## single term
#' search_contexts(tc, 'A')$hits
#'
#' search_contexts(tc, 'G*')$hits    ## wildcard *
#' search_contexts(tc, '*G')$hits    ## wildcard *
#' search_contexts(tc, 'G*G')$hits   ## wildcard *
#'
#' search_contexts(tc, 'G?G')$hits   ## wildcard ?
#' search_contexts(tc, 'G?')$hits    ## wildcard ? (no hits)
#'
#' ## boolean
#' search_contexts(tc, 'A AND B')$hits
#' search_contexts(tc, 'A AND D')$hits
#' search_contexts(tc, 'A AND (B OR D)')$hits
#'
#' search_contexts(tc, 'A NOT B')$hits
#' search_contexts(tc, 'A NOT (B OR D)')$hits
#'
#'
#' ## sequence search (adjacent words)
#' search_contexts(tc, '"A B"')$hits
#' search_contexts(tc, '"A C"')$hits ## no hit, because not adjacent
#'
#' search_contexts(tc, '"A (B OR D)"')$hits ## can contain nested OR
#' ## cannot contain nested AND or NOT!!
#'
#' search_contexts(tc, '<A B>')$hits ## can also use <> instead of "".
#'
#' ## proximity search (using ~ flag)
#' search_contexts(tc, '"A C"~5')$hits ## A AND C within a 5 word window
#' search_contexts(tc, '"A C"~1')$hits ## no hit, because A and C more than 1 word apart
#'
#' search_contexts(tc, '"A (B OR D)"~5')$hits ## can contain nested OR
#' search_contexts(tc, '"A <B C>"~5')$hits    ## can contain nested sequence (must use <>)
#' search_contexts(tc, '<A <B C>>~5')$hits    ## (<> is always OK, but cannot nest quotes in quotes)
#' ## cannot contain nested AND or NOT!!
#'
#'
#' ## case sensitive search
#' search_contexts(tc, 'g')$hits     ## normally case insensitive
#' search_contexts(tc, 'g~s')$hits   ## use ~s flag to make term case sensitive
#'
#' search_contexts(tc, '(a OR g)~s')$hits   ## use ~s flag on everything between parentheses
#' search_contexts(tc, '(a OR G)~s')$hits   ## use ~s flag on everything between parentheses
#'
#' search_contexts(tc, '"a b"~s')$hits   ## use ~s flag on everything between quotes
#' search_contexts(tc, '"A B"~s')$hits   ## use ~s flag on everything between quotes
#'
#' }
search_contexts <- function(tc, query, code=NULL, feature='token', context_level=c('document','sentence'), verbose=F, as_ascii=F){
  is_tcorpus(tc)
  context_level = match.arg(context_level)
  if (!feature %in% tc$names) stop(sprintf('Feature (%s) is not available. Current options are: %s', feature, paste(tc$feature_names, collapse=', ')))
  codelabel = get_query_code(query, code)
  query = remove_query_label(query)

  cols = if(context_level == 'sentence') c('doc_id','sentence') else c('doc_id')
  subcontext = if(context_level == 'sentence') 'sentence' else NULL

  hits = vector('list', length(query))
  lookup_tables = list()
  for (i in 1:length(query)) {
    if (verbose) print(code[i])
    q = parse_query_cpp(as.character(query[i]))

    lookup_tables = prepare_lookup_tables(tc, q, lookup_tables, feature = feature, as_ascii = as_ascii)

    h = recursive_search(tc, q, lookup_tables, subcontext=subcontext, feature=feature, mode = 'contexts', as_ascii=as_ascii)
    if (!is.null(h)) {
      h[, code := codelabel[i]]
      hits[[i]] = h
    }
  }
  hits = data.table::rbindlist(hits)

  if (nrow(hits) > 0) {
    setorderv(hits, cols)
  } else {
    hits = data.frame(code=factor(), doc_id=factor(), sentence=numeric())
  }
  queries = data.frame(code=codelabel, query=query)
  contextHits(hits, queries)
}
