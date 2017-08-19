parse_query <- function(x){
  .Call('_corpustools_parse_query', PACKAGE = 'corpustools', x)
}

recursive_print <- function(qlist, level=1) {
  for (i in 1:length(qlist$terms)) {
    q = qlist$terms[[i]]
    if (!is.null(q$terms)) {
      recursive_print(q, level = level+1)
    } else {
      print(paste(level, paste(rep('  ', level), collapse=''), q$term, '|', qlist$relation, qlist$window, qlist$direction))
    }
  }
}

recursive_search <- function(tc, qlist, subcontext=NULL, feature='token', mode = c('unique_hits','features'), parent_relation='') {
  .invisible = NULL; .term_i = NULL; .seq_i = NULL ## for solving CMD check notes (data.table syntax causes "no visible binding" message)

  mode = match.arg(mode) ## 'unique_hit' created complete and unique sets of hits (needed for counting) but doesn't assign all features
                         ## 'features' mode does not make full sets of hits, but returns all features for which the query is true (needed for coding/dictionaries)
  i = NULL; hit_id = NULL ## for solving CMD check notes (data.table syntax causes "no visible binding" message)
  hit_list = vector('list', length(qlist$terms))

  nterms = length(qlist$terms)
  for (j in 1:nterms) {
    q = qlist$terms[[j]]
    is_nested = 'terms' %in% names(q)
    if (is_nested) {
      jhits = recursive_search(tc, q, subcontext=NULL, feature='token', mode=mode, parent_relation=qlist$relation)
      if (nterms == 1) return(jhits)
      if (qlist$relation == 'proximity' & q$relation %in% c('proximity','AND')) stop("Cannot nest proximity or AND search within a proximity search")
      if (qlist$relation == 'proximity' & q$relation == 'sequence' & !is.null(jhits)) jhits[, .seq_i := .term_i]
    } else {
      ## add alternative for OR statements, where all terms are combined into single term for more efficient regex
      jhits = tc$lookup(q$term, feature=feature, ignore_case=!q$case_sensitive)
      if (!is.null(jhits)) jhits[, .invisible := q$invisible]
    }
    if (is.null(jhits)) {
      if (qlist$relation %in% c('AND','proximity','sequence')) return(NULL)
      if (nterms == 1) return(NULL)
      next
    }
    jhits[, .term_i := j]
    if (nrow(jhits) > 0) hit_list[[j]] = jhits
  }

  hits = data.table::rbindlist(hit_list, fill=TRUE)
  if (nrow(hits) == 0) return(NULL)

  assign_once = mode == 'unique_hits'
  if (parent_relation %in% c('AND','proximity','sequence')) assign_once = F  ## with these parents, hit_id will be recalculated, and all valid features should be returned

  hits = hits[order(as.numeric(as.character(hits$doc_id)), hits$token_i, hits$.term_i),]


  if (qlist$relation == 'AND') get_proximity_hit(hits, value = '.term_i', n_unique = nterms, window=NA, subcontext=subcontext, assign_once=assign_once) ## assign hit_ids to groups of tokens within the same context
  if (qlist$relation == 'proximity') get_proximity_hit(hits, value = '.term_i', n_unique = nterms, window=qlist$window, subcontext=subcontext, seq_i = '.seq_i', assign_once=assign_once) ## assign hit_ids to groups of tokens within the given window
  if (qlist$relation %in% c('OR', '')) get_OR_hit(hits)
  if (qlist$relation == 'sequence') get_sequence_hit(hits, value = '.term_i', seq_length = nterms, subcontext=subcontext) ## assign hit ids to valid sequences

  print(hits)
  hits = subset(hits, hit_id > 0)
  if ('.seq_i' %in% colnames(hits)) hits[, .seq_i := NULL]
  if (nrow(hits) > 0) hits else NULL
}

function(){
  tc = create_tcorpus(sotu_texts)
  q = parse_query('<and <i have>>~5')
  x = recursive_search(tc, q)
  x
  x[x$hit_id == 1,]
  tc$get('token')[1:40]

  q = parse_query('<and i* <have been>~4>~20')
  q = parse_query('<and i*> <have been>~20')

  q = parse_query('<and i* <have been>>~d20')
  x = recursive_search(tc, q, mode='features')
  x[x$.term_i == 2,]
  x = recursive_search(tc, q)
  x

}

