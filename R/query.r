parse_queries <- function(q, feature='', optimize_OR=T, optimize_seq=T) {
  queries = lapply(q, parse_query, feature=feature, optimize_OR=optimize_OR, optimize_seq=optimize_seq)
  list(queries = lapply(queries, function(x) x$query),
       query_terms = data.table::rbindlist(lapply(queries, function(x) x$query_terms), fill = T))
}

parse_query <- function(q, feature='', optimize_OR=T, optimize_seq=T) {
  q = parse_query_cpp(q)
  q = simplify_query(q, feature=feature)
  if (optimize_OR) q = optimize_query(q, collapse_or_queries)
  if (optimize_seq) q = optimize_query(q, collapse_sequence_queries)
  list(query = q,
       query_terms = query_terms(q))
}

optimize_query <- function(q, fun) {
  q = fun(q)
  for (i in seq_along(q$terms)) {
    if ('terms' %in% names(q$terms[[i]])) q$terms[[i]] = optimize_query(q$terms[[i]], fun)
  }
  q
}


collapse_or_queries <- function(qlist) {
  if (qlist$relation == 'OR') {
    nested = sapply(qlist$terms, function(x) 'terms' %in% names(x))
    has_flag_query = sapply(qlist$terms, function(x) length(x$flag_query) > 0)
    select = !nested & !has_flag_query # these terms are collapse-able
    
    if (sum(select) > 1) {
      terms = sapply(qlist$terms[select], function(x) x[c('feature','case_sensitive','ghost','term')], simplify = F)
      terms = data.table::rbindlist(terms)
      col_terms = stats::aggregate(term ~ feature + case_sensitive + ghost, data=terms, FUN = c, simplify=F)
      col_terms = apply(col_terms, 1, as.list)
      col_terms = sapply(col_terms, function(x) c(x, list(flag_query=list())), simplify = F)
      qlist$terms = c(col_terms, qlist$terms[!select])
    }
  }
  qlist
}

collapse_sequence_queries <- function(qlist) {
  if (qlist$relation == 'sequence') {
    has_nested = any(sapply(qlist$terms, function(x) 'terms' %in% names(x)))
    if (has_nested) return(qlist)
    diff_conditions = length(unique(lapply(qlist$terms, function(x) list(x$ghost, x$case_sensitive, x$feature, x$flag_query)))) > 1
    if (diff_conditions) return(qlist)
    
  
    terms = sapply(qlist$terms, function(x) x$term)
    qlist$flag_query = qlist$terms[[1]]$flag_query
    qlist$ghost = qlist$terms[[1]]$ghost
    qlist$case_sensitive = qlist$terms[[1]]$case_sensitive
    qlist$feature = qlist$terms[[1]]$feature
    qlist$term = paste(terms, collapse=' ')
    qlist$terms = NULL
    qlist$relation = NULL
      #terms = sapply(qlist$terms[select], function(x) x[c('feature','case_sensitive','ghost','term')], simplify = F)
      #terms = data.table::rbindlist(terms)
      ##col_terms = stats::aggregate(term ~ feature + case_sensitive + ghost, data=terms, FUN = c, simplify=F)
      #col_terms = apply(col_terms, 1, as.list)
      #col_terms = sapply(col_terms, function(x) c(x, list(flag_query=list())), simplify = F)
      #qlist$terms = c(col_terms, qlist$terms[!select])
  }
  qlist
}


simplify_query <- function(q, feature='', all_case_sensitive=F, all_ghost=F, all_flag_query=list()) {
  if (!q$feature == "") feature = q[['feature']]  
  if (q$all_case_sensitive) all_case_sensitive = TRUE
  if (q$all_ghost) all_ghost = TRUE
  for (n in names(q$all_flag_query)) all_flag_query[[n]] = unique(c(all_flag_query[[n]], q$all_flag_query[[n]]))
  q$feature = q$all_case_sensitive = q$all_ghost = q$all_flag_query = NULL
  
  for (i in seq_along(q$terms)) {
    if ('terms' %in% names(q$terms[[i]])) q$terms[[i]] = simplify_query(q$terms[[i]], feature, all_case_sensitive, all_ghost, all_flag_query)
    
    if ('term' %in% names(q$terms[[i]])) {
      if (all_case_sensitive) q$terms[[i]]$case_sensitive = T
      if (all_ghost) q$terms[[i]]$ghost = T
      q$terms[[i]]$feature = feature
      for (n in names(all_flag_query)) q$terms[[i]]$flag_query[[n]] = unique(c(q$terms[[i]]$flag_query[[n]], all_flag_query[[n]]))
      
      if (!q$terms[[i]]$case_sensitive) q$terms[[i]]$term = stringi::stri_trans_tolower(q$terms[[i]]$term)
    }
  }
  q
}

query_terms <- function(q) {
  qd = data.table::rbindlist(get_query_terms(q), fill=T)
  unique(qd)
}

get_query_terms <- function(q) {
  terms = list()
  for (i in seq_along(q$terms)) {
    if ('terms' %in% names(q$terms[[i]])) terms = c(terms, get_query_terms(q$terms[[i]]))
    if ('term' %in% names(q$terms[[i]])) {
      term = data.table::data.table(term = q$terms[[i]]$term, feature = q$terms[[i]]$feature, 
                                    case_sensitive = q$terms[[i]]$case_sensitive)
      fq = q$terms[[i]]$flag_query
      if ('tokenexpr' %in% names(fq)) term$token_expr = merge_str_expressions(fq[['tokenexpr']])
      if ('metaexpr' %in% names(fq)) term$meta_expr = merge_str_expressions(fq[['metaexpr']])
      terms[['']] = term
    }
  }
  terms
}  

merge_str_expressions <- function(x) {
  x = x[!x == '']
  paste(paste0('(',x,')'), collapse=' & ')
}



get_query_code <- function(query, code=NULL) {
  hashcount = stringi::stri_count(query, regex='[^\\\\]#')
  if (any(hashcount > 1)) stop("Can only use 1 hash (#) for labeling. Note that you can escape with double backslash (\\#) to search for #. ");
  hashcode = ifelse(hashcount == 1, stringi::stri_replace(query, '$1', regex = '([^\\\\])#.*'), NA)
  
  if (!is.null(code)) {
    code = as.character(code)
    if (!length(code) == length(query)) stop('code and query vectors need to have the same length')
    code = ifelse(is.na(code), hashcode, code)
  } else code = hashcode
  
  code[is.na(code)] = paste('query', 1:sum(is.na(code)), sep='_')
  if (anyDuplicated(code)) stop('Cannot have duplicate codes')
  code
}

remove_query_label <- function(query) {
  ht_count = stringi::stri_count(query, fixed='#')
  ht_nolabel_count = stringi::stri_count(query, regex='\\\\#')
  has_label = (ht_count - ht_nolabel_count) > 0
  
  if (any(has_label)) {
    query[has_label] = unlist(sapply(stringi::stri_split_fixed(query[has_label], pattern = '#', n=2), function(x) x[[2]]))
  }
  query
}