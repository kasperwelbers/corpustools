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
  subject_lookup = AND(relation=c('nsubj', 'agent', 'nmod:agent','pobj','dobj','nsubj:pass'))
  subject_fill = custom_fill(NOT(relation=c('case','advmod','relcl','acl','acl:relcl','punct')), connected=T)
  
  verb_lookup = AND(lemma=verbs, NOT(lemma = exclude_verbs))
  verb_fill = custom_fill(relation=c('prt','aux','neg','punct','advmod','acomp'), POS = c('PART','VERB','AUX','ADV','ADJ'), connected=T)
  
  predicate_lookup = NOT(relation=c('nsubj', 'agent', 'nmod:agent','aux','prt','advmod','neg','aux','auxpass', 'punct','mark','cc'))
  predicate_fill = custom_fill(NOT(relation=c('relcl','acl:relcl')), connected=T)
  
  relcl = tquery(label='verb', relation = 'relcl', verb_lookup, verb_fill,
                 parents(label='subject', subject_lookup, subject_fill),
                 children(label='predicate', predicate_lookup, predicate_fill))
  
  relcl_xcomp = tquery(label='verb', relation = 'relcl', verb_fill,
                       children(label='verb', verb_lookup,
                                children(label='predicate', predicate_lookup, predicate_fill)),
                       parents(label='subject', subject_lookup, subject_fill))
  
  passive = tquery(label='verb', POS = 'VERB', verb_lookup, verb_fill,
                   children(label='subject', relation = c('agent'), subject_fill),
                   children(label='predicate', predicate_lookup, predicate_fill))
  
  direct = tquery(label='verb', POS = 'VERB', verb_lookup, verb_fill,
                  children(label='subject', subject_lookup, subject_fill),
                  children(label='predicate', predicate_lookup, predicate_fill))
  
  cop1 = tquery(label='predicate', POS = 'VERB', verb_lookup, predicate_lookup, predicate_fill,
                parents(label='verb', verb_lookup, verb_fill,
                        children(label='subject', subject_lookup, subject_fill)))
  
  cop2 = tquery(label='verb', POS = c('VERB','AUX'), verb_lookup, verb_fill,
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
  
  list(relcl=relcl, relcl_xcomp=relcl_xcomp, pas=passive, dir=direct, cop1=cop1, cop2=cop2, pacl=poss_acl, acl=acl, xcomp=xcomp)
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
  source_fill = custom_fill(NOT(relation=c('case','advmod','relcl','acl','acl:relcl')), connected=T)
  quote_fill = custom_fill()
  verb_fill = custom_fill(relation=c('prt','aux'), POS = c('PART','VERB'), connected=T)
  
  parataxis = tquery(label='quote', quote_fill,
                     children(label='verb', relation='parataxis', lemma=say_verbs, verb_fill,
                              children(label='source', relation= c('su', 'nsubj', 'agent', 'nmod:agent'), source_fill)))
  
  relcl = tquery(label='verb', relation = 'relcl', lemma=say_verbs, verb_fill,
                 parents(label='source', POS=c('PROPN','PRON'), source_fill),
                 children(label='quote', NOT(relation=c('su', 'nsubj', 'agent', 'nmod:agent','advmod')), quote_fill))
  
  direct = tquery(label='verb', lemma = say_verbs, verb_fill,
                  children(req=F, relation = c('npadvmod'), block=T),
                  children(label = 'source', relation=c('su', 'nsubj', 'agent', 'nmod:agent'), source_fill),
                  children(label = 'quote', NOT(relation=c('mark', 'advmod','conj', 'cc', 'prep','advcl')), quote_fill,
                           children(relation='mark', block = T, req = F)))
  
  nosrc = tquery(POS='VERB',
                 children(label='source', relation= c('su', 'nsubj', 'agent', 'nmod:agent'), source_fill),
                 children(label='verb', lemma = say_verbs, relation='xcomp', verb_fill,
                          children(label='quote', relation=c("ccomp", "dep", "parataxis", "dobj", "nsubjpass", "advcl"), quote_fill)))
  
  according = tquery(label='quote',
                     children(label = 'verb', lemma='accord',
                              children(lemma = 'to',
                                       children(label='source'))))
  
  advcl = tquery(label='verb', relation = 'advcl', lemma=say_verbs, verb_fill,
                 parents(POS='VERB',
                         children(label='source', relation=c('su', 'nsubj', 'agent', 'nmod:agent'), source_fill)),
                 children(label='quote', NOT(relation=c('su', 'nsubj', 'agent', 'nmod:agent', 'advmod', 'conj', 'cc', 'prep')), quote_fill))
  
  list(par=parataxis, relcl=relcl, dir=direct, nos=nosrc, acc=according, advcl=advcl)
  
}

verb_lemma <- function(what = c('be', 'quote','should')) {
  what = match.arg(what)
  if (what == 'be') lemma = c('be')
  if (what == 'quote') lemma = c("tell", "acknowledge", "admit", "affirm", "allege", "argue", "announce", "assert", "attest", "avow", "call", "claim", "comment", "concede", "confirm", "declare", "deny", "describe","exclaim", "express", "insist", "mention", "note", "post","predict", "proclaim", "promise", "reply", "refer", "remark", "report", "say", "state", "suggest", "talk", "tell", "think","warn","write")
  if (what == 'should') lemma = c('should','must')
  lemma
}

#' @import rsyntax
ud_span_quotes <- function(tokens, quote_column = 'quote', source_value='source', quote_value='quote', say_verbs = verb_lemma('quote')) {
  span1 = tquery(POS = 'VERB', lemma = say_verbs,
                 children(relation=c('su', 'nsubj', 'agent', 'nmod:agent'), POS=c('PRON','PROPN'), label='source'))
  span2 = tquery(POS = 'VERB',
                 children(relation=c('su', 'nsubj', 'agent', 'nmod:agent'), POS=c('PRON','PROPN'), label='source'))
  
  tokens = add_span_quotes(tokens, 'token', quote_col = quote_column, source_val = source_value,
                           quote_val = quote_value, tqueries=list(span1,span2),
                           add_quote_symbols="'‘’", quote_subset = POS != "PART")
  
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

function() {
  tc = tc_sotu_udpipe$copy()
  tc$annotate_quotes()
  tc$annotate_clauses()
  
  
  tc_syntax_reader(tc, annotation='quote', value='source')
  tc_syntax_reader(tc, annotation='clause', value='subject')
  
  tc_plot_tree(tc, annotation='clause', sentence_i=3)
  tc_plot_tree(tc, doc_id = '2', annotation='clause')
  
  
}