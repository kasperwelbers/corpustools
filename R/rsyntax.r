
#' Annotate tokens based on rsyntax queries
#'
#' @description 
#' Apply queries to extract syntax patterns, and add the results as three columns to a tokenlist.
#' The first column contains the ids for each hit. The second column contains the annotation label. The third column contains the fill level (which you probably won't use, but is important for some functionalities).
#' Only nodes that are given a name in the tquery (using the 'label' parameter) will be added as annotation.
#' 
#' Note that while queries only find 1 node for each labeld component of a pattern (e.g., quote queries have 1 node for "source" and 1 node for "quote"), 
#' all children of these nodes can be annotated by settting fill to TRUE. If a child has multiple ancestors, only the most direct ancestors are used (see documentation for the fill argument).
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{
#' annotate_rsyntax(column, ..., block = NULL, fill = TRUE, 
#'                  overwrite = FALSE, block_fill = FALSE, copy = TRUE, 
#'                  verbose = FALSE)}
#'
#' @param column      The name of the column in which the annotations are added. The unique ids are added as column_id
#' @param ...         One or multiple tqueries, or a list of queries, as created with \link{tquery}. Queries can be given a named by using a named argument, which will be used in the annotation_id to keep track of which query was used. 
#' @param block       Optionally, specify ids (doc_id - sentence - token_id triples) that are blocked from querying and filling (ignoring the id and recursive searches through the id). 
#' @param fill        Logical. If TRUE (default) also assign the fill nodes (as specified in the tquery). Otherwise these are ignored 
#' @param overwrite   Applies if column already exists. If TRUE, existing column will be overwritten. If FALSE, the existing annotations in the column will be blocked, and new annotations will be added. This is identical to using multiple queries.
#' @param block_fill  If TRUE (and overwrite is FALSE), the existing fill nodes will also be blocked. In other words, the new annotations will only be added if the 
#' @param verbose     If TRUE, report progress (only usefull if multiple queries are given)
#' 
#' @name tCorpus$annotate_rsyntax
#' @aliases annotate_rsyntax
#' @examples
#' library(rsyntax)
#' 
#' ## spacy tokens for: Mary loves John, and Mary was loved by John
#' tokens = tokens_spacy[tokens_spacy$doc_id == 'text3',]
#' tc = tokens_to_tcorpus(tokens)
#' 
#' ## two simple example tqueries
#' passive = tquery(pos = "VERB*", label = "predicate",
#'                  children(relation = c("agent"), label = "subject"))
#' active =  tquery(pos = "VERB*", label = "predicate",
#'                  children(relation = c("nsubj", "nsubjpass"), label = "subject"))
#' 
#' tc$annotate_rsyntax("clause", pas=passive, act=active)
#' tc$tokens
#' 
#' if (interactive()) {
#' plot_tree(tc$tokens, annotation='clause')
#' }
#' if (interactive()) {
#' syntax_reader(tc$tokens, annotation = 'clause', value='subject')
#' }
tCorpus$set('public', 'annotate_rsyntax', function(column, ..., block=NULL, fill=TRUE, overwrite=NA, block_fill=FALSE, verbose=FALSE) {
  if (column %in% self$names && is.na(overwrite)) stop(sprintf('The specified column (%s) already exists. Set overwrite argument to TRUE to overwrite the column or FALSE to consider existing annotations as a chain.', column))
  cnames = paste0(column, c('','_id','_fill'))
  ti = rsyntax::annotate_tqueries(self$tokens, column = column, ..., block = block, fill = fill, overwrite = overwrite, block_fill = block_fill, copy=T, verbose=F) 
  ti = subset(ti, select = c('doc_id','token_id',cnames))
  for (cn in cnames) if (cn %in% self$names) self$set(cn, NULL)
  self$tokens = merge(self$tokens, ti, by=c('doc_id','token_id'))
})


#' Add columns indicating who said what
#'
#' An off-the-shelf application of rsyntax for extracting quotes. Designed for working with a tCorpus created with \code{\link{udpipe_tcorpus}}.
#'
#' @param column      The name of the column in $tokens to store the results. 
#' @param tqueries    A list of tQueries. By default uses the off-the-shelf tqueries in \code{\link{udpipe_quote_tqueries}}
#' @param span_quotes If True, also look for quotes indicated with quotation marks, that can span multiple sentences. The source will be the most recent source in the previous 2 sentences, or if there is none, simple tqueries are used to find the most recent proper names that said/did something.
#' @param say_verbs   If span_quotes is used, say_verbs can be required to find sources. say_verbs should be a character vector of verb lemma that indicate speech (e.g., say, state). A default list is included in verb_lemma('quote'), but certain lemma might be more accurate/appropriate depending on the corpus.
#'
#' @return a tCorpus
#' @export
#'
#' @examples
#' \donttest{
#' txt = 'Bob said that he likes Mary. John did not like that: "how dare he!". "It is I, John, who likes Mary!!"'
#' tc = udpipe_tcorpus(txt, model = 'english-ewt')
#' tc$annotate_quotes()
#' rsyntax::plot_tree(tc$tokens, token, lemma, POS, annotation='quotes')
#' rsyntax::syntax_reader(tc$tokens, annotation='quotes', value='source', value2='quote')
#' }
tCorpus$set('public', 'annotate_quotes', function(column='quotes', tqueries = udpipe_quote_tqueries(verb_lemma('quote')), span_quotes=T, say_verbs=verb_lemma('quote')) {
  cnames = paste0(column, c('','_id','_fill'))
  ti = rsyntax::annotate_tqueries(self$tokens, column = column, overwrite = T, copy=T, verbose=F, tqueries) 
  if (span_quotes) ti = ud_span_quotes(ti, quote_column = column, say_verbs=say_verbs)
  ti = subset(ti, select = c('doc_id','token_id',cnames))
  for (cn in cnames) if (cn %in% self$names) self$set(cn, NULL)
  self$tokens = merge(self$tokens, ti, by=c('doc_id','token_id'))
})

#' Add columns indicating who did what
#'
#' An off-the-shelf application of rsyntax for extracting subject-verb clauses. Designed for working with a tCorpus created with \code{\link{udpipe_tcorpus}}.
#'
#' @param column      The name of the column in $tokens to store the results. 
#' @param tqueries    A list of tQueries. By default uses the off-the-shelf tqueries in \code{\link{udpipe_clause_tqueries}}
#'
#' @return a tCorpus
#' @export
#'
#' @examples
#' \donttest{
#' tc = tc_sotu_udpipe$copy()
#' tc$annotate_clauses()
#' rsyntax::plot_tree(tc$tokens, token, lemma, POS, annotation='clauses')
#' rsyntax::syntax_reader(tc$tokens, annotation='clauses', value='subject')
#' }
tCorpus$set('public', 'annotate_clauses', function(column='clauses', tqueries = udpipe_clause_tqueries()) {
  tc$annotate_rsyntax(column, tqueries)
})

#' Apply rsyntax transformations
#'
#' This is an experimental function for applying rsyntax transformations directly on a tcorpus,
#' to create a new tcorpus with the transformed tokens. The argument f should be self defined function
#' that wraps rsyntax transformations. Or more generally, a function that takes a tokens data.frame (or data.table) as input, and returns a tokens data.frame (or data.table). 
#' For examples, see corpustools:::ud_relcl, or corpustools::udpipe_simplify for a function that wraps multiple transformations.
#'
#' @param tc    a tCorpus
#' @param f     functions that perform rsyntax tree transformations
#' @param ...   arguments passed to f
#'
#' @return a tCorpus after applying the transformations
#' @export
#'
#' @examples
#' tc2 = transform_rsyntax(tc, udpipe_simplify)
#' 
#' browse_texts(tc2)
#' if (interactive()) {
#'    rsyntax::plot_tree(tc_sotu_udpipe$tokens, token, lemma, POS, sentence_i=20)
#'    rsyntax::plot_tree(tc2$tokens, token, lemma, POS, sentence_i=20)
#' }
transform_rsyntax <- function(tc, f, ...) {
  tokens = as_tokenindex(tc$tokens)
  tokens = f(tokens, ...)
  tokens_to_tcorpus(tokens, meta=tc$meta)
}



