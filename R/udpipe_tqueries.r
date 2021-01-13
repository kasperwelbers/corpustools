#' Add columns indicating who said what
#'
#' An off-the-shelf application of rsyntax for extracting quotes. Designed for working with a tCorpus created with \code{\link{udpipe_tcorpus}}.
#' 
#' Default tqueries are provided for detecting source-quote relations within sentences (\code{\link{udpipe_quote_tqueries}}), and for 
#' detecting source candidates for text between quotation marks that can span across multiple sentences (\code{\link{udpipe_spanquote_tqueries}}).
#' These have mainly been developed and tested for the english-ewt udpipe model.
#' 
#' There are two ways to customize this function. One is to specify a custom character vector of verb lemma. This vector should then be passed as an argument
#' to the two functions for generarting the default tqueries. The second (more advanced) way is to provide a custom list of tqueries. 
#' (Note that the udpipe_quote_tqueries and udpipe_spanquote_tqueries functions simply create lists of queries. You can create new lists, or add tqueries to these lists).
#' !! If you create custom tqueries, make sure that the labels for the quote and source tokens are 'source' and 'quote'. For the spanquote_tqueries,
#' the label for the source candidate should be 'source'.
#'
#' @param tqueries       A list of tqueries. By default uses the off-the-shelf tqueries in \code{\link{udpipe_quote_tqueries}}.
#' @param span_tqueries  Additional tqueries for finding candidates for 'span quotes' (i.e. quotes that span multiple sentences, indicated by quotation marks). 
#'                       By default uses the off-the-shelf tqueries in \code{\link{udpipe_spanquote_tqueries}}.
#'
#' @return the columns 'quote', 'quote_id', and 'quote_verbatim' are added to tokens
#' @name tCorpus$udpipe_quotes
#' @aliases udpipe_quotes
#'
#' @examples
#' \dontrun{
#' txt = 'Bob said that he likes Mary. John did not like that: 
#'        "how dare he!". "It is I, John, who likes Mary!!"'
#' tc = udpipe_tcorpus(txt, model = 'english-ewt')
#' tc$udpipe_quotes()
#' 
#' if (interactive()) {
#'   tc_plot_tree(tc, token, lemma, POS, annotation='quote')
#'   browse_texts(tc, rsyntax='quote', value='source')
#' }
#' 
#' ## you can provide your own lists of tqueries, or use the two 
#' ## query generating functions to customize the specific 'verb lemma'
#' ## (i.e. the lemma for verbs that indicate speech)
#' 
#' custom_verb_lemma = c('say','state')   ## this should be longer
#' quote_tqueries =      udpipe_quote_tqueries(custom_verb_lemma)
#' span_quote_tqueries = udpipe_spanquote_tqueries(custom_verb_lemma)
#' 
#' ## note that these use simply lists with tqueries, so you can also
#' ## create your own list or customize these lists
#' 
#' quote_tqueries
#' span_quote_tqueries
#' 
#' if (interactive()) {
#' tc$udpipe_quotes(tqueries = quote_tqueries, span_tqueries = span_quote_tqueries)
#' tc_plot_tree(tc, token, lemma, POS, annotation='quote')
#' browse_texts(tc, rsyntax='quote', value='source')
#' }
#' }
tCorpus$set('public', 'udpipe_quotes', function(tqueries = udpipe_quote_tqueries(verb_lemma('quote')), span_tqueries = udpipe_spanquote_tqueries(verb_lemma('quote'))) {
  cnames = c('quote','quote_id','quote_verbatim')
  ti = rsyntax::annotate_tqueries(self$tokens, column = 'quote', overwrite = T, copy=T, verbose=F, tqueries)
  if ('quote' %in% colnames(ti))
    ti = ud_span_quotes(ti, span_tqueries, quote_column = 'quote')
  if (!'quote_verbatim' %in% colnames(ti)) ti$quote_verbatim = get_verbatim_quote(ti)
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
#' @name tCorpus$udpipe_clauses
#' @aliases udpipe_clauses
#'
#' @examples
#' \donttest{
#' tc = tc_sotu_udpipe$copy()
#' tc$udpipe_clauses()
#' if (interactive()) {
#'   tc_plot_tree(tc, token, lemma, POS, annotation='clause')
#'   browse_texts(tc, rsyntax='clause', value='subject')
#' }
#' }
tCorpus$set('public', 'udpipe_clauses', function(tqueries = udpipe_clause_tqueries()) {
  self$annotate_rsyntax('clause', tqueries, overwrite=T)
})

#' Get a list of tqueries for extracting who did what
#' 
#' An off-the-shelf list of tqueries for extracting subject-verb clauses. Designed for working with a tCorpus created with \code{\link{udpipe_tcorpus}}.
#' 
#' @param verbs   A character vector for specific verbs to use. By default uses all verbs (except for those specified in exclude_verbs) 
#' @param exclude_verbs A character vector for specific verbs NOT to use. By default uses the verbs that indicate speech (that are used for extracting who said what, in \code{\link{udpipe_quote_tqueries}})
#'
#' @import rsyntax
#' @export
#' @examples 
#' udpipe_clause_tqueries()
udpipe_clause_tqueries <- function(verbs=NULL, exclude_verbs=verb_lemma('quote')) {
  
  subject_lookup = AND(relation=c('nsubj', 'agent', 'nmod:agent','pobj','dobj','nsubj:pass'))   ## some of these relations are for object rather than subject, but this can occur after tree transformations
  subject_fill = custom_fill(NOT(relation=c('advmod','relcl','acl','acl:relcl','punct')), connected=T)
  
  verb_lookup = AND(lemma=verbs, NOT(lemma = exclude_verbs))
  verb_fill = custom_fill(relation=c('prt','aux','neg','punct','advmod','acomp','aux:pass'), POS = c('PART','VERB','AUX','ADV','ADJ'), connected=T)
  
  predicate_lookup = NOT(relation=c('nsubj', 'agent', 'nmod:agent','aux','prt','advmod','neg','aux','auxpass', 'punct','mark','cc','aux:pass'))
  predicate_fill = custom_fill(NOT(relation=c('relcl','acl:relcl')), connected=T)
  
  relcl = tquery(label='verb', relation = 'relcl', verb_lookup, verb_fill,
                 parents(label='subject', subject_lookup, subject_fill),
                 children(label='predicate', predicate_lookup, predicate_fill))
  
  relcl_xcomp = tquery(label='verb', relation = 'relcl', verb_fill,
                       children(label='verb', verb_lookup,
                                children(label='predicate', predicate_lookup, predicate_fill)),
                       parents(label='subject', subject_lookup, subject_fill))
  
  verb_tobe = tquery(label='verb', POS = 'VERB',
                     children(label='subject', subject_lookup, subject_fill),
                     children(label='predicate', predicate_fill,
                              children(label='verb', lemma='to'),
                              children(label='verb', lemma = 'be')))
  
  noun_tobe = tquery(label='subject', POS=c('NOUN','PROPN','PRON'), subject_fill,
                     children(label='predicate', predicate_fill,
                              children(lemma='to', block = T),
                              children(label='verb', lemma = 'be')))

  passive = tquery(label='verb', POS = 'VERB', verb_lookup, verb_fill,
                   children(label='subject', relation = c('agent'), subject_fill),
                   children(label='predicate', predicate_lookup, predicate_fill))
  
  direct = tquery(label='verb', POS = 'VERB', verb_lookup, verb_fill,
                  children(label='subject', subject_lookup, subject_fill),
                  children(label='predicate', predicate_lookup, predicate_fill))
  
  cop1 = tquery(label='predicate', POS = 'VERB', relation='aux', predicate_lookup, predicate_fill,
                parents(label='verb', verb_lookup, verb_fill,
                        children(label='subject', subject_lookup, subject_fill)))
  
  cop2 = tquery(label='verb', POS = c('VERB','AUX'), relation='aux', verb_lookup, verb_fill,
                parents(label='predicate', NOT(relation=c('aux','prt','advmod')), predicate_fill,
                        children(label='subject', subject_lookup, subject_fill)))
  
  poss_acl = tquery(label='verb',
                    children(label='verb', relation='acl', verb_lookup, verb_fill,
                             children(label='predicate', predicate_lookup, predicate_fill)),
                    children(label='subject', relation = c('poss'), subject_fill))
  
  acl = tquery(label='predicate', predicate_fill,
               children(label='verb', relation='acl', verb_lookup, verb_fill,
                        children(label='subject', subject_lookup, subject_fill)))
  
  xcomp = tquery(label = 'verb', relation='xcomp', verb_fill,
                 parents(label='subject', subject_lookup, subject_fill),
                 children(label='predicate', predicate_lookup, predicate_fill))
  
  just_verb_pass = tquery(label='verb', POS='VERB', verb_lookup, verb_fill,
                          children(relation='aux:pass'),
                          children(label='object', subject_lookup, subject_fill))
  
  just_verb = tquery(label='verb', POS='VERB', verb_lookup, verb_fill,
                     children(label='subject', subject_lookup, subject_fill))
  
  amod = tquery(label='predicate', predicate_fill,
                children(label='subject', POS=c('NOUN','PROPN','PRON'), subject_fill, relation=c('nmod','nmod:poss'),
                         children(relation='case', block = T)))
  
  list(relcl=relcl, relcl_xcomp=relcl_xcomp, vtb=verb_tobe, ntb=noun_tobe, pas=passive, dir=direct, cop1=cop1, cop2=cop2, pacl=poss_acl, 
       acl=acl, xcomp=xcomp, jvp=just_verb_pass, jv=just_verb, am=amod)
}

#' Get a list of tqueries for extracting quotes 
#' 
#' An off-the-shelf list of tqueries for extracting quotes. Designed for working with a tCorpus created with \code{\link{udpipe_tcorpus}}.
#' 
#' @param say_verbs   A character vector of verb lemma that indicate speech (e.g., say, state). A default list is included in verb_lemma('quote'), but certain lemma might be more accurate/appropriate depending on the corpus.
#'
#' @import rsyntax
#' @export
#' @examples 
#' udpipe_quote_tqueries()
udpipe_quote_tqueries <- function(say_verbs = verb_lemma('quote')) {
  source_fill = custom_fill(NOT(relation=c('advmod','relcl','acl','acl:relcl')), connected=T)
  #source_fill = custom_fill(relation = c('flat','compound','nmod','nmod:poss','det','case','amod','obl:npmod'), connected=T)
  quote_fill = custom_fill()
  verb_fill = custom_fill(relation=c('prt','aux'), POS = c('PART','VERB'), connected=T)
  
  parataxis = tquery(label='quote', quote_fill,
                     children(label='verb', relation='parataxis', lemma=say_verbs, verb_fill,
                              children(label='source', relation= c('su', 'nsubj', 'agent', 'nmod:agent'), source_fill)))
  
  relcl = tquery(label='verb', relation = 'relcl', lemma=say_verbs, verb_fill,
                 parents(label='source', POS=c('PROPN','PRON'), source_fill),
                 children(label='quote', NOT(relation=c('su', 'nsubj', 'agent', 'nmod:agent','advmod','obl','iobj')), quote_fill))
  
  direct = tquery(label='verb', lemma = say_verbs, verb_fill,
                  children(req=F, relation = c('npadvmod'), block=T),
                  children(label = 'source', relation=c('su', 'nsubj', 'agent', 'nmod:agent'), source_fill),
                  children(label = 'quote', NOT(relation=c('mark', 'nsubj', 'agent', 'nmod:agent', 'advmod','conj', 'cc', 'prep','advcl','obl','iobj')), quote_fill,
                           children(relation='mark', block = T, req = F)))
  
  nosrc = tquery(POS='VERB',
                 children(label='source', relation= c('su', 'nsubj', 'agent', 'nmod:agent'), source_fill),
                 children(label='verb', lemma = say_verbs, relation='xcomp', verb_fill,
                          children(label='quote', relation=c("ccomp", "dep", "parataxis", "dobj", "nsubjpass", "advcl"), quote_fill)))
  
  according1 = tquery(label = 'verb', lemma='accord',
                      parents(label='quote'),
                      children(lemma = 'to',
                               children(label='source')))
  
  according2 = tquery(label = 'verb', lemma='accord',
                      children(lemma = 'to'),
                      parents(label='source', source_fill,
                              parents(label='quote', quote_fill)))
  
  advcl = tquery(label='verb', relation = 'advcl', lemma=say_verbs, verb_fill,
                 parents(POS='VERB',
                         children(label='source', relation=c('su', 'nsubj', 'agent', 'nmod:agent'), source_fill)),
                 children(label='quote', NOT(relation=c('su', 'nsubj', 'agent', 'nmod:agent', 'advmod', 'conj', 'cc', 'prep','obl','iobj')), quote_fill))
  
  list(par=parataxis, relcl=relcl, dir=direct, nos=nosrc, acc1=according1, acc2=according2, advcl=advcl)
  
}

#' Get a list of tqueries for finding candidates for span quotes. 
#' 
#' Quote extraction with tqueries is limited to quotes within sentences. When (verbatim) quotes span multiple sentences (which we call span quotes here), they are often indicated
#' with quotation marks. While it is relatively easy to identify these quotes, it is less straightforward to identify the sources of these quotes.
#' A good approach is to first apply tqueries for finding quotes within sentences, because a source mentioned just before (we use 2 sentences) a span quote is often also the
#' source of this span quote. For cases where there is no previous source, we can apply simple queries for finding source candidates. Thats what the
#' tqueries created with the current function are for.
#' 
#' This procedure is supported in rsyntax with the \code{\link[rsyntax]{add_span_quotes}} function. In corpustools this function is implemented within
#' the \code{\link{udpipe_quotes}} method. The current function provides the default tqueries for the span quotes.
#' 
#' @param say_verbs   A character vector of verb lemma that indicate speech (e.g., say, state). A default list is included in verb_lemma('quote'), but certain lemma might be more accurate/appropriate depending on the corpus.
#'
#' @import rsyntax
#' @export
#' @examples 
#' udpipe_spanquote_tqueries()
udpipe_spanquote_tqueries <- function(say_verbs = verb_lemma('quote')) {
  span1 = tquery(POS = 'VERB', lemma = say_verbs,
                 children(relation=c('su', 'nsubj', 'agent', 'nmod:agent'), POS=c('PRON','PROPN'), label='source'))
  span2 = tquery(POS = 'VERB',
                 children(relation=c('su', 'nsubj', 'agent', 'nmod:agent'), POS=c('PRON','PROPN'), label='source'))
  list(span1=span1, span2=span2)
}

verb_lemma <- function(what = c('be', 'quote','should')) {
  what = match.arg(what)
  if (what == 'be') lemma = c('be')
  if (what == 'quote') lemma = c("tell", "tweet", "acknowledge", "admit", "affirm", "allege", "argue", "announce", "assert", "attest", "avow", "call", "claim", "comment", "concede", "confirm", "declare", "deny", "describe","exclaim", "express", "insist", "mention", "note", "post","predict", "proclaim", "promise", "reply", "refer", "remark", "report", "say", "speak", "state", "suggest", "talk", "tell", "think","warn","write")
  if (what == 'should') lemma = c('should','must')
  lemma
}


#' @import rsyntax
ud_span_quotes <- function(tokens, tqueries, quote_column = 'quote', source_value='source', quote_value='quote') {
  POS = NULL
  
  addquotes = intToUtf8(c(39,8216,8217)) ## to avoid CRAN non-ASCII warnings
  space_col = if ('space' %in% colnames(tokens)) 'space' else NULL
  tokens = add_span_quotes(tokens, 'token', quote_col = quote_column, source_val = source_value, space_col = space_col,
                           quote_val = quote_value, tqueries=tqueries,
                           add_quote_symbols=addquotes, quote_subset = POS != "PART", copy=F)
  
  tokens
  
}

#' @import rsyntax
ud_mods <- function(tokens, should_verbs = verb_lemma('should'), verbose=T) {
  neg = tquery(label='not', fill=F,
               children(label='not', relation='dobj', req=F),
               children(relation='neg'))
  should = tquery(label='should', fill=F,
                  children(label='should', relation='dobj', req=F),
                  children(relation = 'aux', lemma=should_verbs))
  should_not = tquery(label='should_not', fill=F,
                      children(label='should_not', relation='dobj', req=F),
                      children(relation = 'neg'),
                      children(relation = 'aux', lemma=should_verbs))
  tokens = rsyntax::annotate(tokens, 'mod', sn=should_not, neg=neg, should=should, overwrite=T, verbose=verbose)
  tokens$mod_id = NULL
  tokens$mod_fill = NULL
  tokens
}
