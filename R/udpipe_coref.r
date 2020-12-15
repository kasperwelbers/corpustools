
#' Perform rule based coreference resolution
#'
#' @description 
#' This is an experimental implementation of coreference resolution, aimed at coreferences of nouns and pronouns.  
#'
#' @param keep_feats   If TRUE, keep the feature columns created for the coref resolution
#'
#' @name tCorpus$udpipe_coref
#' @aliases udpipe_coref
#' @return a tCorpus
#'
#' @examples
#' tc = tc_sotu_udpipe$copy()
#' tc$udpipe_quotes()
#' \dontrun{
#'  tc$udpipe_coref()
#'  tc$tokens[!is.na(tc$tokens$coref_id),]
#' }
tCorpus$set('public', 'udpipe_coref', function(keep_feats=F) {
  coref_id = coref_txt = NULL
  feats = c('Gender','Number','Person','PronType')
  for (f in feats) if (f %in% self$names) data.table::setnames(self$tokens, f, paste0('.TEMP',f))
  on.exit({
    for (cn in grep('\\.TEMP', self$names, value=T)) data.table::setnames(self$tokens, cn, gsub('\\.TEMP','',cn))
  })
  
  self = get_coref_feats(self)
  if ('coref_id' %in% self$names) self$tokens[, coref_id := NULL]
  if ('coref_txt' %in% self$names) self$tokens[, coref_txt := NULL]
  self$tokens = udpipe_coref(self$tokens)
  
  invisible(self)
})


get_coref_feats <- function(tc) {
  Gender = Number = Person = PronType = NULL
  for (f in c('Gender','Number','Person','PronType')) if (f %in% tc$names) tc$tokens[,(f) := NULL]
  if (tc$model == 'english-ewt') {
    tc$feats_to_columns(keep = c('Gender','Number','Person','PronType'), rm_column = F)
    return(tc)
  }
  if (tc$model %in% c('dutch-alpino', 'dutch-lassysmall')) {
    tc$feats_to_columns(keep = c('Number','Person','PronType'), rm_column = F)
    .Gender = rep(NA, tc$n)
    .Gender[stringi::stri_detect(tc$tokens$xpos, regex = '\\bmasc\\b')] = 'Masc'
    .Gender[stringi::stri_detect(tc$tokens$xpos, regex = '\\bfem\\b')] = 'Fem'
    .Gender[stringi::stri_detect(tc$tokens$xpos, regex = '\\bonz\\b')] = 'Neut'
    tc$tokens[, Gender := .Gender]
    
    ## Plural pronouns are often not correctly presented in feats, but mention 'mv' in xpos
    tc$tokens[, Number := as.character(Number)]
    tc$tokens[stringi::stri_detect(tc$tokens$xpos, regex = '\\bmv\\b'), Number := 'Plur']
    return(tc)
  }
  
  warning(sprintf('This method has not yet been tested with the %s model. While we suspect a lot of the stuff is applicable on other languages/models, there are some differences in how models provide key information such as Gender and Number. As such, use at your own risk (and contact us if you want to see whether we can make this work for another model).', tc$model))
  tc
}






#' @import rsyntax
udpipe_coref <- function(tokens, min_sim=1, max_sim_dist=200, lag=50, lead=10) {
  coref_id = NULL
  for (FEAT in c('Gender','Number','PronType','Person')) if (!FEAT %in% colnames(tokens)) tokens[, (FEAT) := character()]
  
  has_quote = 'quote' %in% colnames(tokens) & 'quote_id' %in% colnames(tokens) & 'quote_verbatim' %in% colnames(tokens)
  if (!has_quote) warning('For better results on identifying coferences within quotes, first apply quote extraction (see tc$annotate_quotes() method).')
  
  
  tokens = add_gender_information(tokens)
  e = get_coref_nodes(tokens, has_quote)
  ids = get_coref_ids(tokens, e, has_quote, min_sim=1, max_sim_dist=200, lag=50, lead=10)
  ids = remove_unique_coref_ids(ids)
  ids = add_coref_txt(ids, e)
  
  ## make coref ids unique within documents
  ids[!is.na(ids$coref_id), coref_id := match(coref_id, unique(coref_id)), by='doc_id']
  
  ## merge back to tokens
  e = merge(e, ids[,c('.ID','coref_txt','coref_id')], by='.ID')
  merge(tokens, e[,c('doc_id','token_id','coref_id','coref_txt')], all.x=T)
}

add_gender_information <- function(tokens) {
  Gender = gender = NULL 
  if (!'Gender' %in% colnames(tokens)) tokens[, Gender := factor(),]
  ## udpipe only gives gender for pronouns. For names we need to add this ourselves
  if (requireNamespace('gender', quietly = T)) {
    gd = gender::gender(unique(as.character(tokens$lemma)[tokens$POS == 'PROPN' & is.na(tokens$Gender)]), years = c(1960,2012))
  } else stop('To use udpipe_coref you need to install the "gender" package.')
  gd = data.table::as.data.table(gd)
  data.table::setnames(gd, 'name','lemma')
  tokens[data.table::as.data.table(gd), Gender := gender, on='lemma']
  levels(tokens$Gender) = gsub('male','Masc', gsub('female','Fem', levels(tokens$Gender)))
  tokens
}

get_coref_nodes <- function(tokens, has_quote) {
  pronoun = PronType = NULL
  tq1 = rsyntax::tquery(label='entity', POS = c('PRON','PROPN','X'), NOT(relation = c('flat','compound')),
                        custom_fill(relation = c('flat','compound'), POS=c('PROPN','X')))
  tq2 = rsyntax::tquery(label='entity', POS = c('NOUN'), relation=c('nsubj','nsubj:pass','obj','obl','ROOT'),
                        custom_fill(relation = c('flat','compound'), POS=c('NOUN')))
  e = rsyntax::apply_queries(tokens, tq1, tq2)
  
  if (has_quote)
    e = rsyntax::get_nodes(tokens, e, token_cols = c('lemma','relation','Gender','Number','PronType','Person','POS','parent','quote','quote_id','quote_verbatim'))
  else
    e = rsyntax::get_nodes(tokens, e, token_cols = c('lemma','relation','Gender','Number','PronType','Person','POS','parent'))
  
  
  ## if propn in possessive, get relation of parent to see if its subject or object
  poss = which(e$relation == 'nmod:poss')
  if (length(poss) > 0) e$relation[poss] = tokens[list(e$doc_id[poss], e$sentence[poss], e$parent[poss]), 'relation', on=c('doc_id','sentence','token_id')]$relation
  
  e[, pronoun := !is.na(PronType) & PronType %in% c('Rel','Prs')]
  e
}



get_coref_ids <- function(tokens, e, has_quote, min_sim, max_sim_dist, lag, lead) {
  Gender = Number = relation = Person = PronType = token_id = pronoun = quote_id = quote_verbatim = NULL
  if (has_quote) 
    ids = e[,list(gender = agg_feat(Gender), number = agg_feat(Number), so = agg_so(relation), person = agg_feat(Person), 
                  prontype = agg_feat(PronType), pos = mean(token_id), pronoun = any(pronoun, na.rm = T),
                  source_id = agg_source_quote_id(quote, quote_id), verbatim_id = agg_verbatim_quote_id(quote_verbatim, quote_id)), by=c('doc_id','.ID')]
  else 
    ids = e[,list(gender = agg_feat(Gender), number = agg_feat(Number), so = agg_so(relation), person = agg_feat(Person), 
                  prontype = agg_feat(PronType), pos = mean(token_id), pronoun = any(pronoun, na.rm = T)), by=c('doc_id','.ID')]
  
  ## first, resolve coreferences based on text simimlarities of nouns and proper names
  ids$id = text_sim_coref_id(e, ids, min_sim, max_sim_dist)
  ids$needs_coref = ids$pronoun
  
  ## second, coreference based on syntax (within sentences)
  ids = syntax_based_coref(tokens, ids, e)
  
  ## third, coref adjustment if we know quotes
  if (has_quote) ids = within_quote_coref(ids)
  
  ## fourth, find coreferences based on gender, number, subject/object (so), person (1st,2nd,3rd), pronoun type and position
  ids$coref_id = coref_candidate_select(ids$needs_coref, as.character(ids$doc_id), as.character(ids$gender), as.character(ids$number), as.character(ids$so), as.character(ids$person), as.character(ids$prontype), ids$pos, ids$id, ids$pronoun, lag=lag, lead=lead)
  ids
}

remove_unique_coref_ids <- function(ids) {
  ncorefs = table(ids$coref_id)
  singles = as.numeric(names(ncorefs)[ncorefs == 1])
  ids$coref_id[ids$coref_id %in% singles] = NA
  ids
}

add_coref_txt <- function(ids, e) {
  lemma = NULL
  needs_txt = stats::na.omit(ids$coref_id[ids$pronoun])
  id_txt = merge(e[e$POS %in% c('PROPN','NOUN'), c('.ID','lemma','POS')], 
                 ids[!is.na(ids$coref_id) & ids$coref_id %in% needs_txt & !ids$pronoun,c('.ID','coref_id')], by='.ID')
  data.table::setorderv(id_txt, 'POS', -1)   ## prefer PROPN over NOUN if both match
  uids = unique(id_txt, by = 'coref_id')$.ID
  id_txt = id_txt[list(.ID = uids),,on='.ID']
  id_txt = id_txt[,list(coref_txt = paste(lemma, collapse=' ')), by='coref_id']
  id_txt$pronoun = T
  ids = merge(ids, id_txt, by=c('coref_id','pronoun'), all.x=T)
}

agg_feat <- function(x) {
  x = stats::na.omit(x)
  if (length(x) != 1) return('?')
  return(as.character(x))
}

agg_so <- function(x) {
  if ('nsubj' %in% x) return('Subject')
  if ('nsubj:pass' %in% x) return('Subject')
  if ('obj' %in% x) return('Object')
  if ('obl' %in% x) return('Object')
  return('Other')
}

agg_source_quote_id <- function(quote, quote_id) {
  is_source = which(quote == 'source')
  if (length(is_source) > 0)
    return(as.character(quote_id[is_source[1]]))
  else
    return(character())
}

agg_verbatim_quote_id <- function(quote_verbatim, quote_id) {
  is_verbatim = which(quote_verbatim)
  if (length(is_verbatim) > 0)
    return(as.character(quote_id[is_verbatim[1]]))
  else
    return(character())
}


text_sim_coref_id <- function(e, ids, min_sim, max_sim_dist) {
  ## get terms and ids for non pronouns
  use_term = e$POS %in% c("NOUN","PROPN","X")
  term = as.character(e$lemma[use_term])
  #term[e$POS[use_term] == 'NOUN'] = tolower(term[e$POS[use_term] == 'NOUN'])
  id = e$.ID[use_term]
  voc = unique(term)

  m = Matrix::spMatrix(nrow(ids), length(voc), match(id,ids$.ID), match(term,voc), x=rep(1, length(term)))
  m = methods::as(m,'dgCMatrix')
  if (ncol(m) <= 1) return(1:nrow(ids))
  
  ## only compare withing documents
  sim = RNewsflow::tcrossprod_sparse(m, rowsum_div=T, group=ids$doc_id, only_upper=T, diag=F, min_value = min_sim)
  sim = methods::as(sim, 'dgTMatrix')
  sim = data.table::data.table(i=sim@i, j=sim@j)
  data.table::setorderv(sim, c('i','j'))
  sim_pos_dist = abs(ids$pos[sim$i+1] - ids$pos[sim$j+1])
  sim = sim[sim_pos_dist <= max_sim_dist,]
  
  group_coref_ids(sim$i, sim$j, nrow(ids))
}



syntax_based_coref <- function(tokens, ids, e) {
  new_coref_id = NULL
  syntax_coref = syntax_coref_nodes(tokens)
  if (nrow(syntax_coref) > 0) {
    syntax_coref = merge(syntax_coref, e[,c("doc_id","sentence","token_id",".ID")], by.x=c('doc_id','sentence','coref'), by.y=c('doc_id','sentence','token_id'))
    if (nrow(syntax_coref) > 0) syntax_coref = merge(syntax_coref, e[,c("doc_id","sentence","token_id",".ID")], by.x=c('doc_id','sentence','name'), by.y=c('doc_id','sentence','token_id'))
    if (nrow(syntax_coref) > 0) {
      syntax_coref = merge(syntax_coref, ids[,c('.ID','id')], by.x='.ID.y', by.y='.ID')
      data.table::setnames(syntax_coref, c('.ID.x','id'), c('.ID','new_coref_id'))
      ids[syntax_coref, c('id','needs_coref') := list(new_coref_id, F), on=c('.ID')]
    }
  }
  ids
}

within_quote_coref <- function(ids) {
  not_missing = (!is.na(ids$verbatim_id) & !is.na(ids$person))
  first_person_coref = not_missing & ids$verbatim_id != '' & ids$person == '1'
  if (any(first_person_coref)) {
    ## if first person pronoun, assume it refers to source
    match_i = match(ids$verbatim_id[first_person_coref], ids$source_id)
    ids[first_person_coref, c('id','needs_coref') := list(ids$id[match_i], F)] 
  }
  ##second_person_coref = not_missing & ids$verbatim_id != '' & ids$person == '2'
  ##if (any(second_person_coref)) {
    ## if second person pronoun, assume it refers to an object close to the source (if any)?
    ## First need to test how often this is correct
  #}
  
  ids
}

syntax_coref_nodes <- function(tokens) {
  .ID = Gender = Number = NULL
  ### certain corefs
  tq1 = tquery(POS = c('NOUN','PROPN'), relation=c('nsubj','nsubj:pass'), label='name', 
               children(relation = c('acl','relcl','acl:relcl','relcl','conj'), depth=Inf, connected=T, 
                        children(POS = 'PRON', relation=c('nsubj','nsubj:pass','nmod:poss'), label='coref', fill=F, BREAK(NOT(relation = c('nsubj','nsubj:pass','nmod:poss'))), depth=Inf)))
  
  tq2 = tquery(POS = c('NOUN','PROPN'), relation=c('obj'), label='name', 
               children(relation = c('acl','relcl','acl:relcl','relcl','xcomp','ccomp','conj'), depth=Inf, connected=T, 
                        children(POS = 'PRON', PronType = 'Rel', relation=c('nsubj','nsubj:pass','nmod:poss'), label='coref', fill=F, BREAK(NOT(relation = c('nsubj','nsubj:pass','nmod:poss'))), depth=Inf)))
  
  tq3 = tquery(POS = c('NOUN','PROPN'), label='name', 
               children(relation = c('acl','relcl','acl:relcl','relcl','conj'), depth=Inf, connected=T, 
                        children(POS = 'PRON', relation = 'nmod:poss', label='coref', fill=F, BREAK(NOT(relation = c('nsubj','nsubj:pass','nmod:poss'))), depth=Inf)))
  
  nodes1 = rsyntax::apply_queries(tokens, tq1, tq2, tq3, fill=F)
  nodes1 = merge(nodes1[nodes1$.ROLE == 'name', c('doc_id','sentence','.ID','token_id')],
                 nodes1[nodes1$.ROLE == 'coref', c('doc_id','sentence','.ID','token_id')], by = c('doc_id','sentence','.ID'))
  data.table::setnames(nodes1, c('token_id.x','token_id.y'), c('name','coref'))
  nodes1[,.ID := NULL]
  
  ### uncertain corefs (needs gender/number check)
  tq4 = tquery(POS = 'VERB',
               children(POS = c('NOUN','PROPN'), relation=c('nsubj','nsubj:pass'), label='name'), 
               children(POS = 'VERB', depth=Inf, connected=T,
                        children(POS = 'PRON', PronType = 'Prs', label='coref')))
  
  nodes2 = rsyntax::apply_queries(tokens, tq4, fill=F)
  if (nrow(nodes2) == 0) return(nodes1)
  nodes2 = get_nodes(tokens, nodes2, token_cols = c("Gender","Number"))
  nodes2[is.na(Gender), Gender := '']
  nodes2[is.na(Number), Number := '']
  
  nodes2 = merge(nodes2[nodes2$.ROLE == 'name', c('doc_id','sentence','.ID','token_id','Gender','Number')],
                 nodes2[nodes2$.ROLE == 'coref', c('doc_id','sentence','.ID','token_id','Gender','Number')], by = c('doc_id','sentence','.ID'))
  data.table::setnames(nodes2, c('token_id.x','token_id.y','Gender.x','Gender.y','Number.x','Number.y'), 
                               c('token_id_name','token_id_coref','Gender_name','Gender_coref','Number_name','Number_coref'))
  nodes2[,.ID := NULL]
  
  ## Gender does not need to match if (because noise), but it can't be Masc and Fem combined
  gender_match = !(nodes2$Gender_name == 'Masc' & nodes2$Gender_coref == 'Fem') &! (nodes2$Gender_name == 'Fem' & nodes2$Gender_coref == 'Masc')
  ## If name is singular, coref can be plural (because individuals speaking for a group and single organizations can refer to themselves as plural)
  number_match = !(nodes2$Number_name == 'Plur' & nodes2$Gender_coref == 'Sing')
  nodes2 = nodes2[gender_match & number_match,]
  
  if (nrow(nodes2) > 0) nodes2 = data.table(doc_id = nodes2$doc_id, sentence=nodes2$sentence, name=nodes2$token_id_name, coref=nodes2$token_id_coref)
  if (nrow(nodes1) > 0) {
    if (nrow(nodes2) > 0) 
      return(rbind(nodes1,nodes2))
    else
      return(nodes1)
  } else nodes2
}


