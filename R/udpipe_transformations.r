#' @import rsyntax
ud_split_conjunctions <- function(tokens) {
  ## Use different fill settings for long and short distance conjunctions (as a rough heuristic for argument drop)
  no_fill_long_dist = c('acl:relcl','acl','appos','relcl', 'cop', 'nmod:poss',
                        'advmod','advcl','xcomp','ccomp','aux','det')
  no_fill_short_dist = c('acl:relcl','relcl', 'conj', 'cop')
  
  tokens %>%
    split_UD_conj(POS = 'VERB', right_fill_dist=F, no_fill=no_fill_long_dist) %>%
    split_UD_conj(min_dist = 3, no_fill=no_fill_long_dist) %>%
    split_UD_conj(no_fill= no_fill_short_dist) %>%
    rsyntax::chop(relation = c('cc','cc:preconj'))
}

## a custom fill for nouns and proper names that are copied in isolated sentences
## we only copy the really defining parts of the name (whereas things such as modifiers are isolated)
object_fill = custom_fill(relation = c('flat','compound','nmod','nmod:poss'), connected=T)

ud_short_coref <- function(tokens) {
  coref = NULL
  # short coreference resolution for the most obvious 
  # For any proper name, look for pronouns
  
  tq1 = tquery(POS = c('NOUN','PROPN'), relation=c('nsubj','nsubj:pass'), label='name', object_fill,
              children(relation = c('acl','relcl','acl:relcl','relcl'), 
                       children(POS = 'PRON', relation=c('nsubj','nsubj:pass','nmod:poss'), label='coref', BREAK(NOT(relation = c('nsubj','nsubj:pass','nmod:poss'))), depth=Inf)))
  
  tq2 = tquery(POS = c('NOUN','PROPN'), relation=c('obj'), label='name', object_fill,
               children(relation = c('acl','relcl','acl:relcl','relcl'), 
                        children(POS = 'PRON', lemma = c('who','which','that'), relation=c('nsubj','nsubj:pass','nmod:poss'), label='coref', BREAK(NOT(relation = c('nsubj','nsubj:pass','nmod:poss'))), depth=Inf)))
  
  for (tq in list(tq1,tq2)) {
    tokens = select_nodes(tokens, tq) %>%
      copy_nodes('name', 'name_copy', copy_fill = T) %>%
      mutate_nodes('name_copy', parent=coref$parent, relation=coref$relation) %>%
      remove_nodes('coref')
  }
  tokens
}

#' @import rsyntax
ud_rm_redundant_marks <- function(tokens) {
  # simplify redundant mark nodes
  
  ## "in order to" to "to"
  tq = tquery(POS = 'VERB', 
              children(relation = 'mark', lemma='to', label='mark'),
              children(relation = 'mark', lemma='in', label='redundant_mark'))
  
  select_nodes(tokens, tq) %>%
    remove_nodes('redundant_mark')
}

mark_relation_dict <- function(x) {
  ## mark nodes in branches that need to be isolated (advcl, acl) tell something about the relation of the branch to its parent
  ## for example, subject does something BY doing something else is different from subject does something TO do something else
  ## here we make a dictionary to group different mark lemma to more general labels about this relation.
  dict = list(means=c('by','through'),
              goal =c('to'),
              comparison = c('than'),
              assoc = c('while','besides','when'),
              neg_assoc = c('despite'))
  
  mark = unlist(dict, use.names = F)
  label = rep(names(dict), sapply(dict, length))
  relation = label[match(x, as.character(mark))]
  ifelse(is.na(relation), paste0(x, '*'), relation)
}


#' @import rsyntax
ud_subject_advcl <- function(tokens, rm_mark=F) {
  advcl = mark = verb = NULL
  # [subject] does something [mark_lemma] [doing] [something else]     
  #   - [subject] does something
  #   - [subject] [doing] [something else]
  
  # also with ccomp, which seems to work similarly
  
  ## four versions
  ## - once without a nsubj (often pronoun) child under advcl
  tq = tquery(label='verb', POS='VERB', 
              children(relation=c('nsubj','nsubj:pass'), label='subject'),
              children(relation=c('advcl','ccomp'), label='advcl', depth=Inf, BREAK(NOT(relation = c('advcl','xcomp','conj','ccomp'))),
                       not_children(relation=c('nsubj','nsubj:pass')),
                       children(relation='mark', NOT(lemma='for'), label='mark')))
  
  tokens = select_nodes(tokens, tq) %>%
    copy_nodes('subject', new = 'subject_copy', copy_fill=T) %>%
    mutate_nodes('subject_copy', parent = advcl$token_id) %>%
    mutate_nodes('advcl', parent = NA, relation='ROOT', tree_parent=advcl$parent, tree_relation=mark_relation_dict(mark$lemma))
  if (rm_mark) tokens = remove_nodes(tokens, 'mark')
  
  ## - once with a nsubj and mark
  tq = tquery(label='verb', POS='VERB', 
              children(relation=c('nsubj','nsubj:pass'), label='subject'),
              children(relation=c('advcl','ccomp'), label='advcl', depth=Inf, BREAK(NOT(relation = c('advcl','xcomp','conj','ccomp'))),
                       children(relation='mark', NOT(lemma='for'), label='mark')))
  
  tokens = select_nodes(tokens, tq) %>%
    mutate_nodes('advcl', parent = NA, relation='ROOT', tree_parent=advcl$parent, tree_relation=mark_relation_dict(mark$lemma))
  if (rm_mark) tokens = remove_nodes(tokens, 'mark')
    
  ## - once without a mark but with subject
  tq = tquery(label='verb', POS='VERB', 
              children(relation=c('nsubj','nsubj:pass'), label='subject'),
              children(relation=c('advcl','ccomp'), label='advcl', depth=Inf, BREAK(NOT(relation = c('advcl','xcomp','conj','ccomp'))),
                       not_children(relation = c('nsubj','mark'))))
  
  tokens = select_nodes(tokens, tq) %>%
    copy_nodes('subject', new = 'subject_copy', copy_fill=T) %>%
    mutate_nodes('subject_copy', parent = advcl$token_id) %>%
    mutate_nodes('advcl', parent = NA, relation='ROOT', tree_parent=advcl$parent, tree_relation=rep('implicit', nrow(verb)))

  ## - once without a mark or nsubj
  tq = tquery(label='verb', POS='VERB', 
              children(relation=c('nsubj','nsubj:pass'), label='subject'),
              children(relation=c('advcl','ccomp'), label='advcl', depth=Inf, BREAK(NOT(relation = c('advcl','xcomp','conj','ccomp'))),
                       not_children(relation = 'mark')))
  
  tokens = select_nodes(tokens, tq) %>%
    mutate_nodes('advcl', parent = NA, relation='ROOT', tree_parent=advcl$parent, tree_relation=rep('implicit', nrow(verb)))
  
  
  tokens
}


#' @import rsyntax
ud_obj_advcl <- function(tokens, rm_mark=F) {
  advcl = mark = NULL
  # like subject_advcl, but with advcl as a child of an obj.
  # this seems to (only?) occur when advcl has a subject (so we don't need to not_children(relation='nsubj') here)
  
  tq = tquery(label='obj', relation='obj',
              children(relation='advcl', label='advcl',
                       children(relation='mark', NOT(lemma='for'), label='mark')))
  
  tokens = select_nodes(tokens, tq) %>%
    mutate_nodes('advcl', parent = NA, relation='ROOT', tree_parent=advcl$parent, tree_relation=mark_relation_dict(mark$lemma))
  if (rm_mark) tokens = remove_nodes(tokens, 'mark')
  tokens
}

#' @import rsyntax
ud_object_advcl <- function(tokens, rm_mark=F) {
  advcl = verb = NULL
  ## like subject_advcl, but a somewhat special case where there is an object and a "for" mark, 
  ## in which case the object tends to be the implied subject of the advcl 
  ## for instance: [subject] liked [object] for being awesome 
  tq = tquery(label='verb', POS='VERB',
              children(relation=c('obj','nsubj:pass'), label='object'),
              children(relation='advcl', label='advcl',
                       not_children(relation='nsubj'),
                       children(relation='mark', lemma='for', label='mark')))
  
  tokens = select_nodes(tokens, tq) %>%
    copy_nodes('object', new = 'object_copy', copy_fill=T) %>%
    mutate_nodes('object_copy', parent = advcl$token_id, relation = 'nsubj') %>%
    mutate_nodes('advcl', parent = NA, relation='ROOT', tree_parent=verb$token_id, tree_relation='cause')
  if (rm_mark) tokens = remove_nodes(tokens, 'mark')
  
  tokens
}

#' @import rsyntax
ud_acl <- function(tokens, rm_mark=F) {
  acl = subject = mark = NULL
  # [something/someone], by/to [doing] something, does [something else]     
  #   - [something/someone] does something
  #   - [something/someone] does [something else]
  tq = tquery(POS = c('NOUN','PROPN'), relation=c('nsubj','obj','nsubj:pass','agent'), label='subject', object_fill,
              children(relation='acl', label='acl',
                       children(relation='mark', label='mark')))
  
  tokens = select_nodes(tokens, tq) %>%
    copy_nodes('subject', new = 'subject_copy', copy_fill=T) %>%
    mutate_nodes('subject_copy', parent = acl$token_id) %>%
    mutate_nodes('acl', parent = NA, relation='ROOT', tree_parent=subject$token_id, tree_relation=mark_relation_dict(mark$lemma))
  if (rm_mark) tokens = remove_nodes(tokens, 'mark')
  
  tokens
}

#' @import rsyntax
ud_acl_relcl <- function(tokens) {
  subject = NULL
  # [subject], who [did] [something], [did] [something else]
  
  tq = tquery(POS = c('NOUN','PROPN'), label='subject',
              children(relation='acl:relcl', label='acl_relcl'))
  
  select_nodes(tokens, tq) %>%
    mutate_nodes('acl_relcl', parent = NA, relation='ROOT', tree_parent=subject$token_id, tree_relation='implicit')
}



#' @import rsyntax
ud_appos <- function(tokens) {
  name_copy = NULL
  # [something/someone], [what this something/someone is], did blabla
  tq = tquery(POS = c('NOUN','PROPN'), label='name', object_fill,
              children(relation = 'appos', label='appos'))
  
  select_nodes(tokens, tq) %>%
    copy_nodes('name', 'name_copy', copy_fill = T) %>%
    mutate_nodes('name_copy', parent=NA, relation='ROOT') %>%
    mutate_nodes('appos', parent=name_copy$token_id)
}

ud_amod <- function(tokens) {
  name_copy = NULL
  # Isolate modifiers of nouns and proper names
  tq = tquery(POS = c('NOUN','PROPN'), label='name', object_fill,
              children(relation='amod', label='amod'))
  
  select_nodes(tokens, tq) %>%
    copy_nodes('name', 'name_copy', copy_fill = T) %>%
    mutate_nodes('name_copy', parent=NA, relation='ROOT') %>%
    mutate_nodes('amod', parent=name_copy$token_id)
}

ud_parataxis <- function(tokens) {
  parent = NULL
  tq = tquery(relation = 'parataxis', label='para',
              parents(label='parent'))
  
  select_nodes(tokens, tq) %>%
    mutate_nodes('para', parent=NA, relation='ROOT', tree_parent=parent$token_id, tree_relation='assoc')
}

ud_xcomp <- function(tokens) {
  xcomp = subject_copy = verb = NULL
  tq = tquery(POS = 'VERB', label='verb',
              children(relation = 'obj', POS='PRON', object_fill, label='subject'),
              children(relation = 'xcomp', label='xcomp'))
  
  select_nodes(tokens, tq) %>%
    copy_nodes('subject', 'subject_copy', copy_fill = T) %>%
    mutate_nodes('subject_copy', parent=xcomp$token_id, relation='nsubj') %>%
    mutate_nodes('xcomp', parent=NA, relation='ROOT') %>%
    mutate_nodes('verb', tree_parent = subject_copy$token_id, tree_relation= rep('means', nrow(verb)))
}



ud_reindex_sentences <- function(tokens) {
  new_sentence=NULL; token_id=NULL
  ids = tokens[,c('doc_id','sentence','token_id','parent')]
  if ('orig_sentence' %in% colnames(tokens)) ids$sentence = tokens$orig_sentence
  ids$i = 1:nrow(ids)
  ids$new_sentence = numeric()
  
  i = which(is.na(ids$parent))
  p = ids[i,]
  p[, new_sentence := 1:length(token_id), by=c('doc_id')]
  ids$new_sentence[i] = p$new_sentence
  
  while (nrow(p) > 0) {
    ids$new_sentence[p$i] = p$new_sentence
    p = merge(ids[,c('doc_id','sentence','token_id','parent','i')], p[,c('doc_id','sentence','token_id','new_sentence')], 
              by.x=c('doc_id','sentence','parent'), by.y=c('doc_id','sentence','token_id'))
  }
  
  if ('tree_parent' %in% colnames(tokens)) {
    has_tp = which(!is.na(tokens$tree_parent))
    tp_i = tokens[list(tokens$doc_id[has_tp], tokens$sentence[has_tp], tokens$tree_parent[has_tp]), ,on=c('doc_id','sentence','token_id'), which=T]
    tokens$.I = 1:nrow(tokens)
  }
  
  tokens$tree_sentence = ids$sentence
  tokens$sentence = ids$new_sentence
  data.table::setorderv(tokens, c('doc_id','sentence','token_id'))
  tokens[,token_id := 1:length(token_id), by=c('doc_id')]

  if ('tree_parent' %in% colnames(tokens)) {
    match_tp = data.table(orig_i = has_tp,
                          orig_parent_i = tp_i)
    match_tp$new_i = match(match_tp$orig_i, tokens$.I) 
    match_tp$new_parent_i = match(match_tp$orig_parent_i, tokens$.I) 
    tokens$tree_parent[match_tp$new_i] = tokens$token_id[match_tp$new_parent_i]
    tokens$.I = NULL
  }
  tokens
}


#' Simplify tokenIndex created with the udpipe parser
#'
#' This is an off-the-shelf implementation of several rsyntax transformation for 
#' simplifying text. 
#'
#' @param tokens        A tokenIndex, based on output from the ud parser.
#' @param split_conj    If TRUE, split conjunctions into separate sentences
#' @param rm_punct      If TRUE, remove punctuation afterwards
#' @param new_sentences If TRUE, assign new sentence and token_id after splitting
#' @param rm_mark       If TRUE, remove children with a mark relation if this is used in the simplification.
#'
#' @return a tokenIndex
#' @export
#' @import rsyntax
#' @examples 
#' if (interactive()) {
#' tc = tc_sotu_udpipe$copy()
#' tc2 = transform_rsyntax(tc, udpipe_simplify)
#' 
#' browse_texts(tc2)
#'    rsyntax::plot_tree(tc_sotu_udpipe$tokens, token, lemma, POS, sentence_i=20)
#'    rsyntax::plot_tree(tc2$tokens, token, lemma, POS, sentence_i=20)
#' }
udpipe_simplify <- function(tokens, split_conj=T, rm_punct=F, new_sentences=F, rm_mark=F) {
  tokens = tokens %>%
    ud_short_coref() %>%
    ud_appos() %>%
    ud_object_advcl(rm_mark = rm_mark) %>%
    ud_subject_advcl(rm_mark = rm_mark) %>%
    ud_obj_advcl(rm_mark = rm_mark) %>%
    ud_acl(rm_mark = rm_mark) %>%
    ud_acl_relcl() %>%
    ud_parataxis() %>%
    ud_xcomp()
  
  if (split_conj) tokens = ud_split_conjunctions(tokens)
  if (rm_punct) tokens = chop(tokens, relation = 'punct')
  if (new_sentences) tokens = ud_reindex_sentences(tokens)
  tokens
}
