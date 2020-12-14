#' Add columns indicating who said what
#'
#' An off-the-shelf application of rsyntax for extracting quotes. Designed for working with a tCorpus created with \code{\link{udpipe_tcorpus}}.
#'
#' @param tqueries    A list of tQueries. By default uses the off-the-shelf tqueries in \code{\link{udpipe_quote_tqueries}}
#' @param span_quotes If True, also look for quotes indicated with quotation marks, that can span multiple sentences. The source will be the most recent source in the previous 2 sentences, or if there is none, simple tqueries are used to find the most recent proper names that said/did something.
#' @param say_verbs   If span_quotes is used, say_verbs can be required to find sources. say_verbs should be a character vector of verb lemma that indicate speech (e.g., say, state). A default list is included in verb_lemma('quote'), but certain lemma might be more accurate/appropriate depending on the corpus.
#'
#' @return the columns 'quote', 'quote_id', and 'quote_verbatim' are added to tokens
#' @name tCorpus$udpipe_quotes
#' @aliases udpipe_quotes
#'
#' @examples
#' \donttest{
#' txt = 'Bob said that he likes Mary. John did not like that: 
#'        "how dare he!". "It is I, John, who likes Mary!!"'
#' tc = udpipe_tcorpus(txt, model = 'english-ewt')
#' tc$udpipe_quotes()
#' tc_plot_tree(tc, token, lemma, POS, annotation='quote')
#' browse_texts(tc, rsyntax='quote', value='source')
#' }
tCorpus$set('public', 'udpipe_quotes', function(tqueries = udpipe_quote_tqueries(verb_lemma('quote')), span_quotes=T, say_verbs=verb_lemma('quote')) {
  cnames = c('quote','quote_id','quote_verbatim')
  ti = rsyntax::annotate_tqueries(self$tokens, column = 'quote', overwrite = T, copy=T, verbose=F, tqueries) 
  if (span_quotes) ti = ud_span_quotes(ti, quote_column = 'quote', say_verbs=say_verbs)
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
#' tc_plot_tree(tc, token, lemma, POS, annotation='clause')
#' tc_syntax_reader(tc, annotation='clause', value='subject')
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
#' @examples 
#' udpipe_quote_queries()
udpipe_clause_tqueries <- function(verbs=NULL, exclude_verbs=verb_lemma('quote')) {
  
  ## subject contains some relations that are actually object (:pass), but this can occur after tree transformations
  subject_lookup = AND(relation=c('nsubj', 'agent', 'nmod:agent','pobj','dobj','nsubj:pass'))
  #subject_fill = custom_fill(relation = c('flat','compound','nmod','nmod:poss','det','case','amod','obl:npmod'), connected=T)
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
#' @examples 
#' udpipe_quote_queries()
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

verb_lemma <- function(what = c('be', 'quote','should')) {
  what = match.arg(what)
  if (what == 'be') lemma = c('be')
  if (what == 'quote') lemma = c("tell", "acknowledge", "admit", "affirm", "allege", "argue", "announce", "assert", "attest", "avow", "call", "claim", "comment", "concede", "confirm", "declare", "deny", "describe","exclaim", "express", "insist", "mention", "note", "post","predict", "proclaim", "promise", "reply", "refer", "remark", "report", "say", "speak", "state", "suggest", "talk", "tell", "think","warn","write")
  if (what == 'should') lemma = c('should','must')
  lemma
}

#' @import rsyntax
ud_span_quotes <- function(tokens, quote_column = 'quote', source_value='source', quote_value='quote', say_verbs = verb_lemma('quote')) {
  POS = NULL
  span1 = tquery(POS = 'VERB', lemma = say_verbs,
                 children(relation=c('su', 'nsubj', 'agent', 'nmod:agent'), POS=c('PRON','PROPN'), label='source'))
  span2 = tquery(POS = 'VERB',
                 children(relation=c('su', 'nsubj', 'agent', 'nmod:agent'), POS=c('PRON','PROPN'), label='source'))
  
  addquotes = intToUtf8(c(39,8216,8217)) ## to avoid CRAN non-ASCII warnings
  
  space_col = if ('space' %in% colnames(tokens)) 'space' else NULL
  tokens = add_span_quotes(tokens, 'token', quote_col = quote_column, source_val = source_value, space_col = space_col,
                           quote_val = quote_value, tqueries=list(span1=span1,span2=span2),
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
