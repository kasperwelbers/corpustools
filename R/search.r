recursive_search <- function(tc, qlist, subcontext=NULL, feature='token', mode = c('unique_hits','features'), parent_relation='', all_case_sensitive=FALSE, all_invisible=FALSE) {
  .invisible = NULL; .term_i = NULL; .seq_i = NULL; .group_i = NULL ## for solving CMD check notes (data.table syntax causes "no visible binding" message)

  mode = match.arg(mode) ## 'unique_hit' created complete and unique sets of hits (needed for counting) but doesn't assign all features
  ## 'features' mode does not make full sets of hits, but returns all features for which the query is true (needed for coding/dictionaries)
  i = NULL; hit_id = NULL ## for solving CMD check notes (data.table syntax causes "no visible binding" message)
  hit_list = vector('list', length(qlist$terms))

  ## all_ conditions are passed down to nested querie
  if (qlist$all_case_sensitive) all_case_sensitive = TRUE
  if (qlist$all_invisible) all_invisible = TRUE
  nterms = length(qlist$terms)
  for (j in 1:nterms) {
    q = qlist$terms[[j]]
    is_nested = 'terms' %in% names(q)
    if (is_nested) {
      jhits = recursive_search(tc, q, subcontext=subcontext, feature='token', mode=mode, parent_relation=qlist$relation, all_case_sensitive, all_invisible)
      if (nterms == 1) return(jhits)
      if (qlist$relation == 'proximity' & q$relation %in% c('proximity','AND')) stop("Cannot nest proximity or AND search within a proximity search")
      if (!is.null(jhits)) {
        if (qlist$relation == 'proximity' & q$relation == 'sequence') jhits[, .seq_i := .term_i]
        if (qlist$relation %in% c('proximity','sequence','AND')) jhits[!is.na(.group_i), .group_i := paste(j, .group_i, sep='_')] ## for keeping track of nested multi word queries
      }
    } else {
      ## add alternative for OR statements, where all terms are combined into single term for more efficient regex
      .case_sensitive = q$case_sensitive | all_case_sensitive
      .invisible = q$invisible | all_invisible
      jhits = tc$lookup(q$term, feature=feature, ignore_case=!.case_sensitive)
      if (!is.null(jhits)) {
        jhits[, .invisible := .invisible]
        if (qlist$relation %in% c('proximity','sequence','AND')) jhits[, .group_i := as.character(j)] else jhits[, .group_i := character()] ## for keeping track of nested multi word queries
      }
    }
    if (is.null(jhits)) {
      if (qlist$relation %in% c('AND','proximity','sequence')) return(NULL)
      if (nterms == 1) return(NULL)
      next
    }
    jhits[, .term_i := j]
    ## if AND,
    if (nrow(jhits) > 0) hit_list[[j]] = jhits
  }

  hits = data.table::rbindlist(hit_list, fill=TRUE)
  if (nrow(hits) == 0) return(NULL)

  unique_hits = mode == 'unique_hits'
  if (parent_relation %in% c('AND','proximity','sequence')) unique_hits = F  ## with these parents, hit_id will be recalculated, and all valid features should be returned

  if (qlist$relation %in% c('AND', 'NOT')) get_AND_hit(hits, n_unique = nterms, subcontext=subcontext, group_i = '.group_i', assign_once=unique_hits) ## assign hit_ids to groups of tokens within the same context
  if (qlist$relation == 'proximity') get_proximity_hit(hits, n_unique = nterms, window=qlist$window, subcontext=subcontext, seq_i = '.seq_i', assign_once=unique_hits, directed=qlist$directed) ## assign hit_ids to groups of tokens within the given window
  if (qlist$relation %in% c('OR', '')) get_OR_hit(hits)
  if (qlist$relation == 'sequence') get_sequence_hit(hits, seq_length = nterms, subcontext=subcontext) ## assign hit ids to valid sequences

  if (qlist$relation == 'NOT') {
    hits = subset(hits, .term_i == 1 & hit_id == 0) ## if NOT (which is an inverse AND statement), we only want the first term_i (before NOT) where no match was found (hit_id == 0)
  } else {
    hits = subset(hits, hit_id > 0)
  }
  if ('.seq_i' %in% colnames(hits)) hits[, .seq_i := NULL]
  if (nrow(hits) > 0) hits else NULL
}

get_query_code <- function(query, code=NULL) {
  hashcount = stringi::stri_count(query, regex='[^\\\\]#')
  if (any(hashcount > 1)) stop("Can only use 1 hash (#) for labeling. Note that you can escape with double backslash (\\#) to search for #. ");
  hashcode = ifelse(hashcount == 1, stringi::stri_replace(query, '$1', regex = '([^\\\\])#.*'), NA)

  if (!is.null(code)) {
    if (!length(code) == length(query)) stop('code and query vectors need to have the same length')
    code = ifelse(is.na(code), hashcode, code)
  } else code = hashcode
  code[is.na(code)] = paste('query', 1:sum(is.na(code)), sep='_')
  if (anyDuplicated(code)) stop('Cannot have duplicate codes')
  code
}



get_sequence_hit <- function(d, seq_length, subcontext=NULL){
  hit_id = NULL ## used in data.table syntax, but need to have bindings for R CMD check
  setorderv(d, c('doc_id', 'token_i', '.term_i'))
  if (!is.null(subcontext)) subcontext = d[[subcontext]]
  .hit_id = .Call('_corpustools_sequence_hit_ids', PACKAGE = 'corpustools', as.integer(d[['doc_id']]), as.integer(subcontext), as.integer(d[['token_i']]), as.integer(d[['.term_i']]), seq_length)
  d[,hit_id := .hit_id]
}

get_proximity_hit <- function(d, n_unique, window=NA, subcontext=NULL, seq_i=NULL, assign_once=T, directed=F){
  hit_id = NULL ## used in data.table syntax, but need to have bindings for R CMD check
  setorderv(d, c('doc_id', 'token_i', '.term_i'))
  if (!is.null(subcontext)) subcontext = d[[subcontext]]
  if (!is.null(seq_i)) seq_i = d[[seq_i]]
  .hit_id = .Call('_corpustools_proximity_hit_ids', PACKAGE = 'corpustools', as.integer(d[['doc_id']]), as.integer(subcontext), as.integer(d[['token_i']]), as.integer(d[['.term_i']]), n_unique, window, as.numeric(seq_i), assign_once, directed)
  d[,hit_id := .hit_id]
}

get_AND_hit <- function(d, n_unique, subcontext=NULL, group_i=NULL, assign_once=T){
  hit_id = NULL ## used in data.table syntax, but need to have bindings for R CMD check
  setorderv(d, c('doc_id', 'token_i', '.term_i'))
  if (!is.null(subcontext)) subcontext = d[[subcontext]]
  if (!is.null(group_i)) group_i = d[[group_i]]
  .hit_id = .Call('_corpustools_AND_hit_ids', PACKAGE = 'corpustools', as.integer(d[['doc_id']]), as.integer(subcontext), as.integer(d[['token_i']]), as.integer(d[['.term_i']]), n_unique, as.character(group_i), assign_once)
  d[,hit_id := .hit_id]
}

get_OR_hit <- function(d) {
  hit_id = NULL ## used in data.table syntax, but need to have bindings for R CMD check
  if (!'hit_id' %in% colnames(d)) {
    i = 1:nrow(d)
  } else i = d$hit_id
  isna = is.na(i)
  if (any(isna)) {
    if (all(isna)) na_ids = 1:length(i) else na_ids = 1:sum(isna) + max(i, na.rm = T)
    i[isna] = na_ids
  }
  .hit_id = i
  d[,hit_id := .hit_id]
}

grep_global_i <- function(fi, regex, ...) {
  exact_feature = levels(fi$feature)[grepl(regex, levels(fi$feature), ...)]
  fi[list(exact_feature),,nomatch=0]$global_i
}

grep_fi <- function(fi, regex, ...) {
  exact_feature = levels(fi$feature)[grepl(regex, levels(fi$feature), ...)]
  i = fi[list(exact_feature),,nomatch=0]

}

expand_window <- function(i, window) {
  unique(rep(i, window*2 + 1) + rep(-window:window, each=length(i)))
}

overlapping_windows <- function(hit_list, window=window) {
  Reduce(intersect, sapply(hit_list, expand_window, window=window, simplify = F))
}

