is_deprecated <- function(f = as.character(sys.call(sys.parent()))[1L], new=NULL, warn=F){
  ## wrapper for .Deprecated
  f = gsub('.*\\$', '', f)
  if (is.null(new)) new = f
  msg <- gettextf("'%s' as an R6 method is deprecated.\nIt used to be:\t\ttCorpus$%s(...)\nnow use instead:\t%s(tc, ...)\nSee help('%s')", f,f,new,new)
  #warning(msg, call. = FALSE, domain = NA)
  if (warn) warning(warningCondition(msg, class = "deprecatedWarning"))
}

#' Get keyword-in-context (KWIC) strings
#'
#' @description
#' Create a data.frame with keyword-in-context strings for given indices (i), search results (hits) or search strings (keyword).
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{
#' kwic(hits = NULL, i = NULL, query = NULL, code = '',
#'      ntokens = 10, nsample = NA, output_feature = 'token',
#'      context_levels = c('document','sentence'),
#'      prettypaste = T, kw_tag = c('<','>'), ...)
#' }
#'
#' @param hits results of feature search. see \link{search_features}.
#' @param i instead of the hits argument, you can give the indices of features directly.
#' @param query instead of using the hits or i arguments, a search string can be given directly. Note that this simply a convenient shorthand for first creating a hits object with \link{search_features}. If a query is given, then the ... argument is used to pass other arguments to \link{tCorpus$search_features}.
#' @param code if 'i' or 'query' is used, the code argument can be used to add a code label. Should be a vector of the same length that gives the code for each i or query, or a vector of length 1 for a single label.
#' @param ntokens an integers specifying the size of the context, i.e. the number of tokens left and right of the keyword.
#' @param n a number, specifying the total number of hits
#' @param nsample like n, but with a random sample of hits. If multiple codes are used, the sample is drawn for each code individually.
#' @param output_feature the feature column that is used to make the KWIC.
#' @param context_level Select the maxium context (document or sentence).
#' @param kw_tag a character vector of length 2, that gives the symbols before (first value) and after (second value) the keyword in the KWIC string. Can for instance be used to prepare KWIC with format tags for highlighting.
#' @param ... See \link{search_features} for the query parameters
#'
#' @name tCorpus$kwic
#' @examples
#' tc = tokens_to_tcorpus(corenlp_tokens, sentence_col = 'sentence', token_id_col = 'id')
#'
#' ## look directly for a term (or complex query)
#' tc$kwic(query = 'love*')
#'
#' ## or, first perform a feature search, and then get the KWIC for the results
#' hits = search_features(tc, '(john OR mark) AND mary AND love*', context_level = 'sentence')
#' tc$kwic(hits, context_level = 'sentence')
tCorpus$set('public', 'kwic', function(hits=NULL, i=NULL, feature=NULL, query=NULL, code='', ntokens=10, n=NA, nsample=NA, output_feature='token', query_feature='token', context_level=c('document','sentence'), kw_tag=c('<','>'), ...){
  is_deprecated(new='get_kwic')
  if (!is.null(query)) hits = search_features(self, query=query, code=code, feature = query_feature, ...)
  keyword_in_context(self, hits=hits, i=i, code=code, ntokens=ntokens, n=n, nsample=nsample, output_feature=output_feature, context_level=context_level, kw_tag=kw_tag)
})


#' Feature statistics
#'
#' @description
#' Compute a number of useful statistics for features: term frequency, idf, etc.
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{feature_stats(feature, sent_freq=F)}
#'
#' @param feature The name of the feature column
#' @param sent_freq If True, include sentence frequency (only if sentence information is available).
#'
#' @name tCorpus$feature_stats
#' @examples
#' tc = create_tcorpus(c('Text one first sentence. Text one second sentence', 'Text two'),
#'                     split_sentences = TRUE)
#'
#' fs = tc$feature_stats('token')
#' head(fs)
#'
#' fs = tc$feature_stats('token', context_level = 'sentence')
#' head(fs)
tCorpus$set('public', 'feature_stats', function(feature, context_level=c('document','sentence')){
  is_deprecated()
  feature_stats(self, feature=feature, context_level=context_level)
})


#' Show top features
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{top_features(feature, n = 10, group_by = NULL, group_by_meta = NULL, return_long = F}
#'
#' @param feature The name of the feature
#' @param n Return the top n features
#' @param group_by A column in the token data to group the top features by. For example, if token data contains part-of-speech tags (pos), then grouping by pos will show the top n feature per part-of-speech tag.
#' @param group_by_meta A column in the meta data to group the top features by.
#' @param return_long if True, results will be returned in a long format. Default is a table, but this can be inconvenient if there are many grouping variables.
#'
#' @name tCorpus$top_features
#' @examples
#' tc = tokens_to_tcorpus(corenlp_tokens, token_id_col = 'id')
#'
#' top_features(tc, 'lemma')
#' tc$top_features('lemma')
#' tc$top_features('lemma', group_by = 'relation')
tCorpus$set('public', 'top_features', function(feature, n=10, group_by=NULL, group_by_meta=NULL, return_long=F){
  is_deprecated()
  top_features(self, feature=feature, n=n, group_by=group_by, group_by_meta=group_by_meta, return_long=return_long)
})


#' Create a semantic network based on the co-occurence of tokens in documents
#'
#' @description
#' This function calculates the co-occurence of features and returns a network/graph in the igraph format, where nodes are tokens and edges represent the similarity/adjacency of tokens. Co-occurence is calcuated based on how often two tokens occured within the same document (e.g., news article, chapter, paragraph, sentence). The semnet_window() function can be used to calculate co-occurrence of tokens within a given token distance.
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{
#' semnet(feature, measure = c('con_prob', 'con_prob_weighted', 'cosine', 'count_directed', 'count_undirected', 'chi2'),
#'        context_level = c('document','sentence'), backbone=F, n.batches=NA)
#' }
#'
#' @param feature The name of the feature column
#' @param measure The similarity measure. Currently supports: "con_prob" (conditional probability), "con_prob_weighted", "cosine" similarity, "count_directed" (i.e number of cooccurrences) and "count_undirected" (same as count_directed, but returned as an undirected network, chi2 (chi-square score))
#' @param context_level Determine whether features need to co-occurr within "documents" or "sentences"
#' @param backbone If True, add an edge attribute for the backbone alpha
#' @param n.batches If a number, perform the calculation in batches
#'
#' @name tCorpus$semnet
#' @examples
#' text = c('A B C', 'D E F. G H I', 'A D', 'GGG')
#' tc = create_tcorpus(text, doc_id = c('a','b','c','d'), split_sentences = TRUE)
#'
#' g = tc$semnet('token')
#' g
#' igraph::get.data.frame(g)
#' \donttest{plot_semnet(g)}
tCorpus$set('public', 'semnet', function(feature, measure=c('cosine', 'con_prob', 'con_prob_weighted', 'count_directed', 'count_undirected', 'chi2'), context_level=c('document','sentence'), backbone=F, n.batches=NA){
  is_deprecated()
  measure = match.arg(measure)
  require_package('igraph')
  semnet(self, feature=feature, measure=measure, context_level=context_level, backbone=backbone, n.batches=n.batches)
})

#' Create a semantic network based on the co-occurence of tokens in token windows
#'
#' @description
#' This function calculates the co-occurence of features and returns a network/graph in the igraph format, where nodes are tokens and edges represent the similarity/adjacency of tokens. Co-occurence is calcuated based on how often two tokens co-occurr within a given token distance.
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{
#' semnet_window(feature, measure = c('con_prob', 'cosine', 'count_directed', 'count_undirected', 'chi2'),
#'               context_level = c('document','sentence'), window.size = 10, direction = '<>',
#'               backbone = F, n.batches = 5, set_matrix_mode = c(NA, 'windowXwindow', 'positionXwindow'))
#' }
#'
#' @param feature The name of the feature column
#' @param measure The similarity measure. Currently supports: "con_prob" (conditional probability), "cosine" similarity, "count_directed" (i.e number of cooccurrences) and "count_undirected" (same as count_directed, but returned as an undirected network, chi2 (chi-square score))
#' @param context_level Determine whether features need to co-occurr within "documents" or "sentences"
#' @param window.size The token distance within which features are considered to co-occurr
#' @param direction Determine whether co-occurrence is assymmetricsl ("<>") or takes the order of tokens into account. If direction is '<', then the from/x feature needs to occur before the to/y feature. If direction is '>', then after.
#' @param backbone If True, add an edge attribute for the backbone alpha
#' @param n.batches If a number, perform the calculation in batches
#' @param set_matrix_mode Advanced feature. There are two approaches for calculating window co-occurrence. One is to measure how often a feature occurs within a given token window, which can be calculating by calculating the inner product of a matrix that contains the exact position of features and a matrix that contains the occurrence window. We refer to this as the "positionXwindow" mode. Alternatively, we can measure how much the windows of features overlap, for which take the inner product of two window matrices. By default, semnet_window takes the mode that we deem most appropriate for the similarity measure. Substantially, the positionXwindow approach has the advantage of being very easy to interpret (e.g. how likely is feature "Y" to occurr within 10 tokens from feature "X"?). The windowXwindow mode, on the other hand, has the interesting feature that similarity is stronger if tokens co-occurr more closely together (since then their windows overlap more). Currently, we only use the windowXwindow mode for cosine similarity. By using the set_matrix_mode parameter you can override this.
#'
#' @name tCorpus$semnet_window
#' @examples
#' text = c('A B C', 'D E F. G H I', 'A D', 'GGG')
#' tc = create_tcorpus(text, doc_id = c('a','b','c','d'), split_sentences = TRUE)
#'
#' g = tc$semnet_window('token', window.size = 1)
#' g
#' igraph::get.data.frame(g)
#' \donttest{plot_semnet(g)}
tCorpus$set('public', 'semnet_window', function(feature, measure=c('cosine', 'con_prob', 'count_directed', 'count_undirected', 'chi2'), context_level=c('document','sentence'), window.size=10, direction='<>', backbone=F, n.batches=NA, set_matrix_mode=c(NA, 'windowXwindow','positionXwindow')){
  is_deprecated()
  measure = match.arg(measure)
  set_matrix_mode = match.arg(set_matrix_mode)
  if (is.na(set_matrix_mode)) set_matrix_mode = 'positionXwindow'
  require_package('igraph')
  semnet_window(self, feature=feature, measure=measure, context_level=context_level, window.size=window.size, direction=direction, backbone=backbone, n.batches=n.batches, matrix_mode=set_matrix_mode)
})


#' Search for documents or sentences using Boolean queries
#'
#' \strong{Usage:}
#'
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
#' @examples
#' text = c('A B C', 'D E F. G H I', 'A D', 'GGG')
#' tc = create_tcorpus(text, doc_id = c('a','b','c','d'), split_sentences = TRUE)
#' tc$tokens
#'
#' hits = tc$search_contexts(c('query label# A AND B', 'second query# (A AND Q) OR ("D E") OR I'))
#' hits          ## print shows number of hits
#' hits$hits     ## hits is a list, with hits$hits being a data.frame with specific contexts
#' summary(hits) ## summary gives hits per query
#'
#' ## sentence level
#' hits = tc$search_contexts(c('query label# A AND B', 'second query# (A AND Q) OR ("D E") OR I'),
#'                           context_level = 'sentence')
#' hits$hits     ## hits is a list, with hits$hits being a data.frame with specific contexts
#'
#' \donttest{
#'
#' ## query language examples
#'
#' ## single term
#' tc$search_contexts('A')$hits
#'
#' tc$search_contexts('G*')$hits    ## wildcard *
#' tc$search_contexts('*G')$hits    ## wildcard *
#' tc$search_contexts('G*G')$hits   ## wildcard *
#'
#' tc$search_contexts('G?G')$hits   ## wildcard ?
#' tc$search_contexts('G?')$hits    ## wildcard ? (no hits)
#'
#' ## boolean
#' tc$search_contexts('A AND B')$hits
#' tc$search_contexts('A AND D')$hits
#' tc$search_contexts('A AND (B OR D)')$hits
#'
#' tc$search_contexts('A NOT B')$hits
#' tc$search_contexts('A NOT (B OR D)')$hits
#'
#'
#' ## sequence search (adjacent words)
#' tc$search_contexts('"A B"')$hits
#' tc$search_contexts('"A C"')$hits ## no hit, because not adjacent
#'
#' tc$search_contexts('"A (B OR D)"')$hits ## can contain nested OR
#' ## cannot contain nested AND or NOT!!
#'
#' tc$search_contexts('<A B>')$hits ## can also use <> instead of "".
#'
#' ## proximity search (using ~ flag)
#' tc$search_contexts('"A C"~5')$hits ## A AND C within a 5 word window
#' tc$search_contexts('"A C"~1')$hits ## no hit, because A and C more than 1 word apart
#'
#' tc$search_contexts('"A (B OR D)"~5')$hits ## can contain nested OR
#' tc$search_contexts('"A <B C>"~5')$hits    ## can contain nested sequence (must use <>)
#' tc$search_contexts('<A <B C>>~5')$hits    ## (<> is always OK, but cannot nest quotes in quotes)
#' ## cannot contain nested AND or NOT!!
#'
#'
#' ## case sensitive search
#' tc$search_contexts('g')$hits     ## normally case insensitive
#' tc$search_contexts('g~s')$hits   ## use ~s flag to make term case sensitive
#'
#' tc$search_contexts('(a OR g)~s')$hits   ## use ~s flag on everything between parentheses
#' tc$search_contexts('(a OR G)~s')$hits   ## use ~s flag on everything between parentheses
#'
#' tc$search_contexts('"a b"~s')$hits   ## use ~s flag on everything between quotes
#' tc$search_contexts('"A B"~s')$hits   ## use ~s flag on everything between quotes
#' }
tCorpus$set('public', 'search_contexts', function(query, code=NULL, feature='token', context_level=c('document','sentence'), verbose=F){
  is_deprecated()
  search_contexts(self, query, code=code, feature=feature, context_level=context_level, verbose=verbose)
})

#' Find tokens using a Lucene-like search query
#'
#' @description
#' Search tokens in a tokenlist using Lucene-like queries. For a detailed explanation of the query language, see the details below.
#'
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
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
#' @name tCorpus$search_features
#' @examples
#' text = c('A B C', 'D E F. G H I', 'A D', 'GGG')
#' tc = create_tcorpus(text, doc_id = c('a','b','c','d'), split_sentences = TRUE)
#' tc$tokens
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
#' \donttest{
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
#' ## advanced queries
#' tc = tokens_to_tcorpus(corenlp_tokens, doc_col = 'doc_id',
#'                        sentence_col = 'sentence', token_id_col = 'id')
#' head(tc$tokens) ## search in multiple feature columns with "columnname: "
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
#' }
tCorpus$set('public', 'search_features', function(query, code=NULL, feature='token', mode = c('unique_hits','features'), context_level = c('document','sentence'), keep_longest=T, as_ascii=F, verbose=F){
  is_deprecated()
  search_features(self, query, code=code, feature=feature, mode=mode, context_level=context_level, keep_longest=keep_longest, as_ascii=as_ascii, verbose=verbose)
})

#' Compare tCorpus vocabulary to that of another (reference) tCorpus
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#' \preformatted{compare_corpus(tc_y, feature, smooth=0.1, min_ratio=NULL, min_chi2=NULL, is_subset=F, yates_cor=c('auto','yes','no'), what=c('freq','docfreq','cooccurrence'))}
#'
#' @param tc_y the reference tCorpus
#' @param feature the column name of the feature that is to be compared
#' @param smooth Laplace smoothing is used for the calculation of the ratio of the relative term frequency. Here you can set the added value.
#' @param min_ratio threshold for the ratio value, which is the ratio of the relative frequency of a term in dtm.x and dtm.y
#' @param min_chi2 threshold for the chi^2 value
#' @param yates_cor mode for using yates correctsion in the chi^2 calculation. Can be turned on ("yes") or off ("no"), or set to "auto", in which case cochrans rule is used to determine whether yates' correction is used.
#' @param is_subset Specify whether tc is a subset of tc_y. In this case, the term frequencies of tc will be subtracted from the term frequencies in tc_y
#' @param what choose whether to compare the frequency ("freq") of terms, or the document frequency ("docfreq"). This also affects how chi^2 is calculated, comparing either freq relative to vocabulary size or docfreq relative to corpus size (N)
#'
#' @name tCorpus$compare_corpus
#' @return A vocabularyComparison object
#' @examples
#' tc = create_tcorpus(sotu_texts, doc_column = 'id')
#'
#' tc$preprocess('token', 'feature', remove_stopwords = TRUE, use_stemming = TRUE)
#'
#' obama = tc$subset_meta(president == 'Barack Obama', copy=TRUE)
#' bush = tc$subset_meta(president == 'George W. Bush', copy=TRUE)
#'
#' comp = obama$compare_corpus(bush, 'feature')
#' comp = comp[order(-comp$chi),]
#' head(comp)
#' \donttest{
#' plot(comp)
#' }
tCorpus$set('public', 'compare_corpus', function(tc_y, feature, smooth=0.1, min_ratio=NULL, min_chi2=NULL, is_subset=F, yates_cor=c('auto','yes','no'), what=c('freq','docfreq','cooccurrence')){
  is_deprecated()
  if (is_subset && self$n > tc_y$n) stop('tCorpus x (the one calling the method) cannot be a subset of tCorpus y, because it has more tokens')
  what = match.arg(what)
  tcorpus_compare(self, tc_y, feature, smooth=smooth, min_ratio=min_ratio, min_chi2=min_chi2, yates_cor=yates_cor, x_is_subset=is_subset, what=what)
})



#' Compare vocabulary of a subset of a tCorpus to the rest of the tCorpus
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#' \preformatted{compare_subset(feature, subset_x=NULL, subset_meta_x=NULL, query_x=NULL, query_feature='token', smooth=0.1, min_ratio=NULL, min_chi2=NULL, yates_cor=c('auto','yes','no'), what=c('freq','docfreq','cooccurrence'))}
#'
#' @param feature the column name of the feature that is to be compared
#' @param subset_x an expression to subset the tCorpus. The vocabulary of the subset will be compared to the rest of the tCorpus
#' @param subset_meta_x like subset_x, but using using the meta data
#' @param query_x like subset_x, but using a query search to select documents (see \link{tCorpus$search_contexts})
#' @param query_feature if query_x is used, the column name of the feature used in the query search.
#' @param smooth Laplace smoothing is used for the calculation of the ratio of the relative term frequency. Here you can set the added value.
#' @param min_ratio threshold for the ratio value, which is the ratio of the relative frequency of a term in dtm.x and dtm.y
#' @param min_chi2 threshold for the chi^2 value
#' @param yates_cor mode for using yates correctsion in the chi^2 calculation. Can be turned on ("yes") or off ("no"), or set to "auto", in which case cochrans rule is used to determine whether yates' correction is used.
#' @param what choose whether to compare the frequency ("freq") of terms, or the document frequency ("docfreq"). This also affects how chi^2 is calculated, comparing either freq relative to vocabulary size or docfreq relative to corpus size (N)
#'
#' @name tCorpus$compare_subset
#' @return A vocabularyComparison object
#' @examples
#' tc = create_tcorpus(sotu_texts, doc_column = 'id')
#'
#' tc$preprocess('token', 'feature', remove_stopwords = TRUE, use_stemming = TRUE)
#'
#' comp = tc$compare_subset('feature', subset_meta_x = president == 'Barack Obama')
#' comp = comp[order(-comp$chi),]
#' head(comp)
#' \donttest{
#' plot(comp)
#' }
#'
#' comp = tc$compare_subset('feature', query_x = 'terroris*')
#' comp = comp[order(-comp$chi),]
#' head(comp, 10)
tCorpus$set('public', 'compare_subset', function(feature, subset_x=NULL, subset_meta_x=NULL, query_x=NULL, query_feature='token', smooth=0.1, min_ratio=NULL, min_chi2=NULL, yates_cor=c('auto','yes','no'), what=c('freq','docfreq','cooccurrence')){
  is_deprecated()
  subset_x = self$eval(substitute(subset_x), parent.frame())
  subset_meta_x = self$eval_meta(substitute(subset_meta_x), parent.frame())
  what = match.arg(what)

  if(is.null(subset_x) && is.null(subset_meta_x) & is.null(query_x)) stop("at least one of subset_x, subset_meta_x or query_x has to be specified")
  if(!is.null(subset_x) | !is.null(subset_meta_x)) {
    .subset_x = subset_x
    .subset_meta_x = subset_meta_x
    tc_x = self$subset(subset=.subset_x, subset_meta = .subset_meta_x, copy=T)
  }
  if(!is.null(query_x)) tc_x = self$subset_query(query_x, feature=query_feature, copy=T)

  comp = tc_x$compare_corpus(self, feature=feature, smooth=smooth, min_ratio=min_ratio, min_chi2=min_chi2, yates_cor=yates_cor, is_subset=T, what=what)
  comp
})

#' Get common nearby terms given a feature query
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{feature_associations(query=NULL, hits=NULL, feature='token',
#'                                    window=15,  n=25, min_freq=1, sort_by= c('chi2', 'ratio', 'freq'),
#'                                    subset=NULL, subset_meta=NULL}
#'
#' @param query A character string that is a query. See \link{search_features} for documentation of the query language.
#' @param hits Alternatively, instead of giving a query, the results of \link{tCorpus$search_features} can be used.
#' @param feature If keyword is used, the name of the feature column within which to search.
#' @param window The size of the word window (i.e. the number of words next to the feature)
#' @param n the top n of associated features
#' @param min_freq Optionally, ignore features that occur less than min_freq times
#' @param sort_by The value by which to sort the features
#' @param subset A call (or character string of a call) as one would normally pass to subset.tCorpus. If given, the keyword has to occur within the subset. This is for instance usefull to only look in named entity POS tags when searching for people or organization. Note that the condition does not have to occur within the subset.
#' @param subset_meta A call (or character string of a call) as one would normally pass to the subset_meta parameter of subset.tCorpus. If given, the keyword has to occur within the subset documents. This is for instance usefull to make queries date dependent. For example, in a longitudinal analysis of politicians, it is often required to take changing functions and/or party affiliations into account. This can be accomplished by using subset_meta = "date > xxx & date < xxx" (given that the appropriate date column exists in the meta data).
#'
#' @name tCorpus$feature_associations
#' @examples
#' tc = create_tcorpus(sotu_texts, doc_column = 'id')
#'
#' ## directly from query
#' topf = tc$feature_associations('war')
#' head(topf, 20) ## frequent words close to "war"
#'
#' ## adjust window size
#' topf = tc$feature_associations('war', window = 5)
#' head(topf, 20) ## frequent words very close (five tokens) to "war"
#'
#' ## you can also first perform search_features, to get hits for (complex) queries
#' hits = tc$search_features('"war terror"~10')
#' topf = tc$feature_associations(hits = hits)
#' head(topf, 20) ## frequent words close to the combination of "war" and "terror" within 10 words
#'
tCorpus$set('public', 'feature_associations', function(query=NULL, hits=NULL, feature='token', window=15,  n=25, min_freq=1, sort_by= c('chi2', 'ratio', 'freq'), subset=NULL, subset_meta=NULL) {
  is_deprecated()
  if (is.null(query) & is.null(hits)) stop('either keyword or hits has to be specified')
  if (!is.null(query) & !is.null(hits)) stop('only keyword or hits can be specified')
  if (!is.null(query)) hits = self$search_features(query, mode='features')

  feature_associations_fun(self, hits=hits, feature=feature, window=window, n=n, min_freq=min_freq, sort_by=sort_by, subset=subset, subset_meta=subset_meta)
})


#' Calculate the similarity of documents
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#' \preformatted{compare_documents(feature='token', date_col=NULL, hour_window=NULL, measure=c('cosine','overlap_pct'), min_similarity=0, weight=c('norm_tfidf', 'tfidf', 'termfreq','docfreq'), ngrams=NA, from_subset=NULL, to_subset=NULL))}
#'
#' @param feature the column name of the feature that is to be used for the comparison.
#' @param date_col a date with time in POSIXct. If given together with hour_window, only documents within the given hour_window will be compared.
#' @param meta_cols a character vector with columns in the meta data / docvars. If given, only documents for which these values are identical are compared
#' @param hour_window A vector of length 1 or 2. If length is 1, the same value is used for the left and right side of the window. If length is 2, the first and second value determine the left and right side. For example, the value 12 will compare each document to all documents between the previous and next 12 hours, and c(-10, 36) will compare each document to all documents between the previous 10 and the next 36 hours.
#' @param measure the similarity measure. Currently supports cosine similarity (symmetric) and overlap_pct (asymmetric)
#' @param min_similarity  A threshold for the similarity score
#' @param weight a weighting scheme for the document-term matrix. Default is term-frequency inverse document frequency with normalized rows (document length).
#' @param ngrams an integer. If given, ngrams of this length are used
#' @param from_subset An expression to select a subset. If given, only this subset will be compared to other documents
#' @param to_subset An expression to select a subset. If given, documents are only compared to this subset
#'
#' @name tCorpus$compare_documents
#' @examples
#' \donttest{
#' ## deprecated beyond repair. Please use the new compare_documents function
#' }
tCorpus$set('public', 'compare_documents', function(feature='token', date_col=NULL, meta_cols=NULL, hour_window=NULL, measure=c('cosine','overlap_pct'), min_similarity=0, weight=c('norm_tfidf', 'tfidf', 'termfreq','docfreq'), ngrams=NA, from_subset=NULL, to_subset=NULL) {
  is_deprecated()
  stop('Due to changes in dependencies this method is gone without a grace period (but replaced by the compare_documents() function')
})

#' Create a document term matrix.
#'
#' @description
#' Create a document term matrix. The default output is a sparse matrix (Matrix, dgTMatrix). Alternatively, the dtm style from the tm and quanteda package can be used.
#'
#' The tCorpus$dfm method is shorthand for using quanteda's dfm (document feature matrix) class. The meta data in the tcorpus is then automatically added as docvars in the dfm.
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{dtm(feature, context_level = c('document','sentence'), weight = c('termfreq','docfreq','tfidf','norm_tfidf'),
#'     drop_empty_terms = T, form = c('Matrix', 'tm_dtm', 'quanteda_dfm'), subset_tokens = NULL, subset_meta = NULL,
#'     context = NULL, context_labels = T, feature_labels = T, ngrams = NA, ngram_before_subset = F)}
#'
#' \preformatted{dfm(feature, ...)   ## identical, but without form argument}
#'
#' @param feature The name of the feature column
#' @param context_level Select whether the rows of the dtm should represent "documents" or "sentences".
#' @param weight Select the weighting scheme for the DTM. Currently supports term frequency (termfreq), document frequency (docfreq), term frequency inverse document frequency (tfidf) and tfidf with normalized document vectors.
#' @param drop_empty_terms If True, tokens that do not occur (i.e. column where sum is 0) are ignored.
#' @param form The output format. Default is a sparse matrix in the dgTMatrix class from the Matrix package. Alternatives are tm_dtm for a DocumentTermMatrix in the tm package format or quanteda_dfm for the document feature matrix from the quanteda package.
#' @param subset_tokens A subset call to select which rows to use in the DTM
#' @param subset_meta A subset call for the meta data, to select which documents to use in the DTM
#' @param context Instead of using the document or sentence context, an custom context can be specified. Has to be a vector of the same length as the number of tokens, that serves as the index column. Each unique value will be a row in the DTM.
#' @param context_labels If False, the DTM will not be given rownames
#' @param feature_labels If False, the DTM will not be given column names
#' @param ngrams Optionally, use ngrams instead of individual tokens. This is more memory efficient than first creating an ngram feature in the tCorpus.
#' @param ngram_before_subset If a subset is used, ngrams can be made before the subset, in which case an ngram can contain tokens that have been filtered out after the subset. Alternatively, if ngrams are made after the subset, ngrams will span over the gaps of tokens that are filtered out.
#'
#' @name tCorpus$dtm
#' @aliases tCorpus$dfm
#' @examples
#' tc = create_tcorpus(c("First text first sentence. First text first sentence.",
#'                    "Second text first sentence"), doc_column = 'id', split_sentences = TRUE)
#'
#' ## Perform additional preprocessing on the 'token' column, and save as the 'feature' column
#' tc$preprocess('token', 'feature', remove_stopwords = TRUE, use_stemming = TRUE)
#' tc$tokens
#'
#' ## default: regular sparse matrix, using the Matrix package
#' m = tc$dtm('feature')
#' class(m)
#' m
#'
#' ## alternatively, create quanteda ('quanteda_dfm') or tm ('tm_dtm') class for DTM
#' \donttest{
#' m = tc$dtm('feature', form = 'quanteda_dfm')
#' class(m)
#' m
#' }
#'
#' ## create DTM with sentences as rows (instead of documents)
#' m = tc$dtm('feature', context_level = 'sentence')
#' nrow(m)
#'
#' ## use weighting
#' m = tc$dtm('feature', weight = 'norm_tfidf')
tCorpus$set('public', 'dtm', function(feature, context_level=c('document','sentence'), weight=c('termfreq','docfreq','tfidf','norm_tfidf'), drop_empty_terms=T, form=c('Matrix', 'tm_dtm', 'quanteda_dfm'), subset_tokens=NULL, subset_meta=NULL, context=NULL, context_labels=T, feature_labels=T, ngrams=NA, ngram_before_subset=F) {
  is_deprecated(new='get_dtm')
  if (class(substitute(subset_tokens)) %in% c('call', 'name')) subset_tokens = self$eval(substitute(subset_tokens), parent.frame())
  if (class(substitute(subset_meta)) %in% c('call', 'name')) subset_meta = self$eval_meta(substitute(subset_meta), parent.frame())

  do_get_dtm(self, feature=feature, context_level=context_level, weight=weight, drop_empty_terms=drop_empty_terms, form=form,
          subset_tokens=subset_tokens, subset_meta=subset_meta, context=context, context_labels=context_labels,
          feature_labels=feature_labels, ngrams=ngrams, ngram_before_subset=ngram_before_subset)
})

tCorpus$set('public', 'dfm', function(feature, context_level=c('document','sentence'), weight=c('termfreq','docfreq','tfidf','norm_tfidf'), drop_empty_terms=T, subset_tokens=NULL, subset_meta=NULL, context=NULL, context_labels=T, feature_labels=T, ngrams=NA, ngram_before_subset=F) {
  is_deprecated(new='get_dfm')
  if (class(substitute(subset_tokens)) %in% c('call', 'name')) subset_tokens = self$eval(substitute(subset_tokens), parent.frame())
  if (class(substitute(subset_meta)) %in% c('call', 'name')) subset_meta = self$eval_meta(substitute(subset_meta), parent.frame())
  do_get_dtm(self, feature=feature, context_level=context_level, weight=weight, drop_empty_terms=drop_empty_terms, form='quanteda_dfm',
          subset_tokens=subset_tokens, subset_meta=subset_meta, context=context, context_labels=context_labels,
          feature_labels=feature_labels, ngrams=ngrams, ngram_before_subset=ngram_before_subset)
})


