


recursive_search <- function(tc, qlist, lookup_tables=NULL, subcontext=NULL, feature='token', mode = c('unique_hits','features','contexts'), parent_relation='', all_case_sensitive=FALSE, all_ghost=FALSE, all_flag_query=list(), keep_longest=TRUE, as_ascii=F, level=1) {
  .ghost = NULL; .term_i = NULL; .seq_i = NULL; .group_i = NULL ## for solving CMD check notes (data.table syntax causes "no visible binding" message)



  mode = match.arg(mode) ## 'unique_hit' created complete and unique sets of hits (needed for counting) but doesn't assign all features
  ## 'features' mode does not make full sets of hits, but returns all features for which the query is true (needed for coding/dictionaries)
  i = NULL; hit_id = NULL ## for solving CMD check notes (data.table syntax causes "no visible binding" message)
  hit_list = vector('list', length(qlist$terms))

  ## all_ conditions are passed down to nested querie
  if (qlist$all_case_sensitive) all_case_sensitive = TRUE
  if (qlist$all_ghost) all_ghost = TRUE
  if (!qlist$feature == "") feature = qlist[['feature']]  ## if the query specifies a feature column, override the feature parameter
  for (n in names(qlist$all_flag_query)) all_flag_query[[n]] = unique(c(all_flag_query[[n]], qlist$all_flag_query[[n]]))

  qlist = collapse_or_queries(qlist)

  nterms = length(qlist$terms)

  for (j in 1:nterms) {
    q = qlist$terms[[j]]
    is_nested = 'terms' %in% names(q)
    if (is_nested) {


      jhits = recursive_search(tc, q, lookup_tables=lookup_tables, subcontext=subcontext, feature=feature, mode=mode, parent_relation=qlist$relation, all_case_sensitive, all_ghost, all_flag_query, keep_longest, as_ascii, level=level+1)

      if (nterms == 1) {
        if (level == 1 && mode == 'contexts' & !is.null(jhits)) jhits = unique(subset(jhits, select=c('doc_id',subcontext)))
        return(jhits)
      }
      if (qlist$relation == 'proximity' && q$relation %in% c('proximity','AND')) stop("Cannot nest proximity or AND search within a proximity search")
      if (!is.null(jhits)) {
        if (q$relation == 'sequence') jhits[, .seq_i := .term_i]
        if (qlist$relation %in% c('proximity','sequence','AND')) jhits[, .group_i := paste(j, .group_i, sep='_')] ## for keeping track of nested multi word queries
      }
    } else {
      #st = Sys.time()
      ## add alternative for OR statements, where all terms are combined into single term for more efficient regex
      .case_sensitive = q$case_sensitive | all_case_sensitive
      .ghost = q$ghost | all_ghost

      flag_query = q$flag_query
      for (n in names(all_flag_query)) flag_query[[n]] = unique(c(flag_query[[n]], all_flag_query[[n]]))

      only_context = mode == 'contexts' && qlist$relation %in% c('AND','NOT')  ## only for AND and NOT, because proximity and sequence require feature positions (and OR can be nested in them)


      if (!is.null(lookup_tables)) {
        #lookup_table_key = sprintf('%s; ignore_case=%s; as_ascii=%s', feature, !.case_sensitive, as_ascii)
        lookup_table_key = create_lookup_table_key(feature, !.case_sensitive, as_ascii)
        lookup_table = lookup_tables[[lookup_table_key]]
      } else lookup_table = NULL  ## if NULL, lookup_table will be created within tc$lookup, but providing one can be faster

      jhits = tc$lookup(q$term, lookup_table=lookup_table,
                        feature=feature, ignore_case=!.case_sensitive, sub_query=flag_query, only_context=only_context, subcontext=subcontext, as_ascii=as_ascii)

      if (!is.null(jhits)) {
        jhits[, .ghost := .ghost]
        if (qlist$relation %in% c('proximity','sequence','AND')) jhits[,.group_i := paste0(j, '_')] else jhits[,.group_i := ''] ## for keeping track of nested multi word queries
      }
      #print(as.numeric(difftime(Sys.time(), st, units = 'secs')))
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

  feature_mode = mode == 'features'
  if (parent_relation %in% c('AND','proximity','sequence')) feature_mode = TRUE ## with these parents, hit_id will be recalculated, and all valid features should be returned


  if (qlist$relation %in% c('AND')) get_AND_hit(hits, n_unique = nterms, subcontext=subcontext, group_i = '.group_i', replace = '.ghost', feature_mode=feature_mode) ## assign hit_ids to groups of tokens within the same context
  if (qlist$relation %in% c('NOT')) get_AND_hit(hits, n_unique = nterms, subcontext=subcontext, group_i = '.group_i', replace = '.ghost', feature_mode=T)
  if (qlist$relation == 'proximity') get_proximity_hit(hits, n_unique = nterms, window=qlist$window, subcontext=subcontext, seq_i = '.seq_i', replace='.ghost', feature_mode=feature_mode, directed=qlist$directed) ## assign hit_ids to groups of tokens within the given window
  if (qlist$relation %in% c('OR', '')) get_OR_hit(hits)
  if (qlist$relation == 'sequence') get_sequence_hit(hits, seq_length = nterms, subcontext=subcontext) ## assign hit ids to valid sequences

  if (qlist$relation == 'NOT') {
    hits = subset(hits, .term_i == 1 & hit_id == 0) ## if NOT (which is an inverse AND statement), we only want the first term_i (before NOT) where no match was found (hit_id == 0)
  } else {
    hits = subset(hits, hit_id > 0)
  }
  if (nrow(hits) == 0) return(NULL)

  if (level == 1 && mode == 'unique_hits') hits = remove_duplicate_hit_id(hits, keep_longest)
  if (level == 1 && mode == 'contexts') hits = unique(subset(hits, select=c('doc_id',subcontext)))

  return(hits)
}

collapse_or_queries <- function(qlist) {
  if (qlist$relation == 'OR') {
    nested = sapply(qlist$terms, function(x) 'terms' %in% names(x))
    has_flag_query = sapply(qlist$terms, function(x) length(x$flag_query) > 0)
    select = !nested & !has_flag_query # these terms are collapse-able

    if (sum(select) > 1) {
      terms = sapply(qlist$terms[select], function(x) x[c('case_sensitive','ghost','term')], simplify = F)
      terms = data.table::rbindlist(terms)

      col_terms = stats::aggregate(term ~ case_sensitive + ghost, data=terms, FUN = c, simplify=F)
      col_terms = apply(col_terms, 1, as.list)
      col_terms = sapply(col_terms, function(x) c(x, list(flag_query=list())), simplify = F)
      qlist$terms = c(col_terms, qlist$terms[!select])
    }
  }
  qlist
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

get_sequence_hit <- function(d, seq_length, subcontext=NULL){
  hit_id = NULL ## used in data.table syntax, but need to have bindings for R CMD check
  setorderv(d, c('doc_id', 'token_id', '.term_i'))
  if (!is.null(subcontext)) subcontext = d[[subcontext]]

  .hit_id = sequence_hit_ids_cpp(as.numeric(d[['doc_id']]), as.numeric(subcontext), as.numeric(d[['token_id']]), as.numeric(d[['.term_i']]), seq_length)
  d[,hit_id := .hit_id]
}

get_proximity_hit <- function(d, n_unique, window=NA, subcontext=NULL, seq_i=NULL, replace=NULL, feature_mode=F, directed=F){
  hit_id = NULL ## used in data.table syntax, but need to have bindings for R CMD check
  setorderv(d, c('doc_id', 'token_id', '.term_i'))
  if (!is.null(subcontext)) subcontext = d[[subcontext]]
  if (!is.null(seq_i)) seq_i = d[[seq_i]]
  if (!is.null(replace)) replace = d[[replace]]

  .hit_id = proximity_hit_ids_cpp(as.numeric(d[['doc_id']]), as.numeric(subcontext), as.numeric(d[['token_id']]), as.numeric(d[['.term_i']]), n_unique, window, as.numeric(seq_i), replace, feature_mode, directed)
  d[,hit_id := .hit_id]
}

get_AND_hit <- function(d, n_unique, subcontext=NULL, group_i=NULL, replace=NULL, feature_mode=F){
  hit_id = NULL ## used in data.table syntax, but need to have bindings for R CMD check
  setorderv(d, c('doc_id', 'token_id', '.term_i'))
  if (!is.null(subcontext)) subcontext = d[[subcontext]]
  if (!is.null(group_i)) group_i = d[[group_i]]
  if (!is.null(replace)) replace = d[[replace]]
  .hit_id = AND_hit_ids_cpp(as.numeric(d[['doc_id']]), as.numeric(subcontext), as.numeric(d[['token_id']]), as.numeric(d[['.term_i']]), n_unique, as.character(group_i), replace, feature_mode)
  d[,hit_id := .hit_id]
}

get_OR_hit <- function(d) {
  hit_id = NULL ## used in data.table syntax, but need to have bindings for R CMD check

  if (!'hit_id' %in% colnames(d)) {
    .hit_id = 1:nrow(d)
  } else .hit_id = d$hit_id

  isna = is.na(.hit_id)
  if (any(isna)) {
    if (all(isna)) na_ids = 1:length(.hit_id) else na_ids = 1:sum(isna) + max(.hit_id, na.rm = T)
    .hit_id[isna] = na_ids
  }

  if ('.term_i' %in% colnames(d)) .hit_id = global_id(d$.term_i, .hit_id)
  d[,hit_id := .hit_id]
}

remove_duplicate_hit_id <- function(d, keep_longest=TRUE) {
  .hit_id_length = NULL; .ghost = NULL; hit_id = NULL ## for solving CMD check notes (data.table syntax causes "no visible binding" message)
  if (!'token_id' %in% colnames(d)) return(d)

  dup = duplicated(d[,c('doc_id','token_id')])
  if (any(dup)) {
    if (!'.ghost' %in% colnames(d)) d$.ghost = F

    if (keep_longest) {
      d[, .hit_id_length := sum(!.ghost), by=hit_id]   ## count non ghost terms per hit_id
      pd = d[order(-d$.hit_id_length),]                ## sort by this count to keep duplicates with highest score
      dup_id = pd$hit_id[duplicated(pd[,c('doc_id','token_id')]) & !d$.ghost]
      d[, .hit_id_length := NULL]
    } else {
      dup_id = d$hit_id[dup & !d$.ghost]
    }
    d = subset(d, !hit_id %in% unique(dup_id))
  }
  d
}



