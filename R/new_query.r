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
  .invisible = NULL; .term_i = NULL; .seq_id = NULL ## for solving CMD check notes (data.table syntax causes "no visible binding" message)

  mode = match.arg(mode) ## 'unique_hit' mode makes nice sets of unique occurences (needed for counting) but doesn't assign all features
                         ## 'features' mode does not assign hit_id, but returns all features for which the query is true (needed for coding/dictionaries)
  i = NULL; hit_id = NULL ## for solving CMD check notes (data.table syntax causes "no visible binding" message)
  hit_list = vector('list', length(qlist$terms))

  nterms = length(qlist$terms)
  nterms_total = nterms
  for (j in 1:nterms) {
    q = qlist$terms[[j]]
    is_nested = 'terms' %in% names(q)
    if (is_nested) {
      jhits = recursive_search(tc, q, subcontext=NULL, feature='token', mode=mode, parent_relation=qlist$relation)
      if (qlist$relation == 'proximity' & q$relation == 'sequence') jhits[, .seq_id = hit_id]  # Given to group_id argument in get_proximity_hit. If any seq term in prox, all seq are used.
      if (qlist$relation == 'proximity' & q$relation %in% c('proximity','AND')) stop("Cannot nest proximity or AND search within a proximity search")
    } else {
      ## add alternative for OR statements, where all terms are combined into single term for more efficient regex
      jhits = tc$lookup(q$regex, feature=feature, ignore_case=!q$case_sensitive, with_i=TRUE)
      jhits[, .invisible := q$invisible]
    }
    jhits[, .term_i := j]

    if (!is.null(jhits)) if (nrow(jhits) == 0) jhits = NULL
    if (is.null(jhits)) {
      if (qlist$relation %in% c('AND','proximity','sequence')) return(NULL)
      if (nterms == 1) return(NULL)
      next
    }

    if (nrow(jhits) > 0) hit_list[[j]] = jhits
  }

  hits = data.table::rbindlist(hit_list, fill=TRUE)
  if (nrow(hits) == 0) return(NULL)

  assign_once = mode == 'unique_hits'
  if (parent_relation %in% c('AND','proximity','sequence')) assign_once = F  ## with these parents, hit_id will be recalculated, and all valid features should be returned

  if (qlist$relation == 'AND') hit_code = get_proximity_hit(hits, value = '.term_i', n_unique = nterms_total, window=NA, subcontext=subcontext, assign_once=assign_once) ## assign hit_ids to groups of tokens within the same context
  if (qlist$relation == 'proximity') hit_code = get_proximity_hit(hits, value = '.term_i', n_unique = nterms_total, window=qlist$window, subcontext=subcontext, group_id = '.seq_id', assign_once=assign_once) ## assign hit_ids to groups of tokens within the given window
  if (qlist$relation %in% c('OR','')) hit_code = get_OR_hit(hits)
  if (qlist$relation == 'sequence') hit_code = get_sequence_hit(hits, value = '.term_i', seq_length = nterms_total, subcontext=subcontext) ## assign hit ids to valid sequences

  if (mode == 'unique_hits') {
    hits[, hit_id := hit_code]
    hits = subset(hits, hit_id > 0)
  } else {
    hits = subset(hits, hit_code > 0)
  }
  if ('.seq_id' %in% colnames(hits)) hits[, .seq_id := NULL]
  if (nrow(hits) > 0) hits else NULL
}

function(){
  tc = create_tcorpus(sotu_texts)
  q = parse_query('"and i"~20')
  x = recursive_search(tc, q, mode='features')
  x
  x = recursive_search(tc, q)
  x
  q = parse_query('"test this <and this>~5"~10')

}
#recursive_queries(x)

