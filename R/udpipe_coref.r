udpipe_coref <- function(tokens, min_sim=1, max_sim_dist=40, lag=30, lead=10) {
  if (!require('gender')) stop('To use corefish you need to install the "gender" package.')
  Gender = NULL
  
  gd = gender::gender(unique(as.character(tokens$lemma)[tokens$POS == 'PROPN']), years = c(1960,2012))
  tokens[list(lemma = gd$name), Gender := gd$gender, on='lemma']
  levels(tokens$Gender) = gsub('male','Masc', gsub('female','Fem', levels(tokens$Gender)))
  
  tq = tquery(label='entity', POS = c('PRON','PROPN'), NOT(relation = c('flat','compound')),
              custom_fill(relation = c('flat','compound'), POS=c('PROPN')))
  e = rsyntax::apply_queries(tokens, tq)
  e = rsyntax::get_nodes(tokens, e, token_cols = c('lemma','relation','Gender','Number','POS','parent'))
  
  ## if propn in possessive, get relation of parent to see if its subject or object
  poss = which(e$relation == 'nmod:poss')
  if (length(poss) > 0) {
    e$relation[poss] = tokens[list(e$doc_id[poss], e$sentence[poss], e$parent[poss]), 'relation', on=c('doc_id','sentence','token_id')]$relation
  }
  
  e[e$Number == 'Plur', Gender := NA]
  e$gendernumber = ifelse(e$Number == 'Plur', 'Plur', as.character(e$Gender))
  
  ids = e[,list(gn = gender_reveal(gendernumber), so = so_reveal(relation), pos = mean(token_id), pronoun = any(POS == 'PRON')), by=c('doc_id','.ID')]
  
  ids$coref_id = text_sim_coref_id(e[e$POS == 'PROPN',], ids, min_sim, max_sim_dist)  
  ids$coref_id[ids$pronoun] = 0
  
  ids$coref_id = coref_candidate_select(as.character(ids$doc_id), as.character(ids$gn), as.character(ids$so), ids$pos, ids$coref_id, lag=lag, lead=lead)
  
  e = merge(e, ids[,c('.ID','coref_id')], by='.ID')
  merge(tokens, e[,c('doc_id','token_id','coref_id')], all.x=T)
}

gender_reveal <- function(x) {
  x = na.omit(x)
  if (length(x) != 1) return('Unknown')
  return(as.character(x))
}

so_reveal <- function(x) {
  if ('nsubj' %in% x) return('Subject')
  if ('nsubj:pass' %in% x) return('Subject')
  if ('obj' %in% x) return('Object')
  if ('obl' %in% x) return('Object')
  return(NA_character_)
}

text_sim_coref_id <- function(e, ids, min_sim, max_sim_dist) {
  term = tolower(e$lemma)
  id = e$.ID
  voc = unique(term)
  
  m = Matrix::spMatrix(length(voc), nrow(ids), match(term,voc), match(id,ids$.ID), x=rep(1, length(term)))
  sim = Matrix::crossprod(m)
  sim = methods::as(sim, 'dgTMatrix')
  cs = colSums(sim)
  sim@x = sim@x / cs[sim@j+1]
  sim = Matrix::triu(sim, 1)
  sim = sim >= min_sim
  
  sim = data.table::data.table(i=sim@i, j=sim@j)
  sim_pos_dist = abs(ids$pos[sim$i+1] - ids$pos[sim$j+1])
  sim = sim[sim_pos_dist <= max_sim_dist,]
  
  coref_ids(sim$i, sim$j, nrow(ids))
}