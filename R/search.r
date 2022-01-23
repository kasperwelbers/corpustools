## extends dictionary search
## after getting the terms with dictionary search
## apply lucene_like to allow more advanced conditions, like word distances and Boolean

get_dict_results <- function(tc, queries, context_level, as_ascii, feature, verbose) {
  hit_id = feat_i = NULL
  context_cols = if (context_level == 'sentence') c('doc_id','sentence') else 'doc_id'
  
  features = unique(queries$query_terms$feature)
  dict_results = vector('list', length(features))
  names(dict_results) = features
  
  for (i in 1:length(features)) {
    dict = queries$query_terms[list(features[i]),,on='feature']
    fi = dictionary_lookup(tc, data.frame(id=dict$term, string=dict$term), token_col=features[i], mode = 'features', case_sensitive = dict$case_sensitive,  ascii=as_ascii, context_level=context_level, verbose=F)
    
    if (!is.null(fi)) if ('token_expr' %in% colnames(queries$query_terms)) fi = token_expression_filter(tc, fi, queries)
    if (!is.null(fi)) if ('meta_expr' %in% colnames(queries$query_terms)) fi = meta_expression_filter(tc, fi, queries)

    if (!is.null(fi)) {
      #fi = unique(fi, by='hit_id')
      fi = fi[,list(feat_i=min(feat_i), ngram=(1 + max(feat_i) - min(feat_i))), by=c('hit_id','dict_i')]
      fi[, hit_id := NULL]
      
      fi = cbind(dict[fi$dict_i,c('term','case_sensitive')], tc$tokens[fi$feat_i,c(context_cols,'token_id'),with=F], ngram=fi$ngram)
      data.table::setkeyv(fi, c('term','case_sensitive'))
    }
    dict_results[[i]] = fi
  }
  dict_results
}

token_expression_filter <- function(tc, fi, queries) {
  rm_fi = rep(F, nrow(fi))
  for (expr in unique(queries$query_terms$token_expr)) {
    if (is.na(expr)) next
    q_has_expr = !is.na(queries$query_terms$token_expr) & queries$query_terms$token_expr == expr
    fi_has_expr = q_has_expr[fi$dict]                                                                                       ## find which features have the expression as a condition
    which_expr_false = tryCatch(tc$tokens[fi$feat_i[fi_has_expr]][!eval(parse(text=expr)),,which=T], error = function(e) e) ## evaluate expression for these features in tc$tokens
    
    ## special error handling, because this can be confusing
    if (inherits(which_expr_false, 'simpleError')) 
      stop(call. = F, paste('A token subset expression used in a query gave an error:', paste0('The expression\n', expr), paste0('The error\n',which_expr_false$message), sep='\n\n'))
    
    which_expr_false = tc$tokens[fi$feat_i[fi_has_expr]][!eval(parse(text=expr)),,which=T]                                  ## evaluate expression for these features in tc$tokens
    
    
    rm_fi[fi_has_expr][which_expr_false] = T                                                     ## take features with expression, and of those set rm_fi features where expression evaluated to false to T (i.e. should be removed)
  }
  if (all(rm_fi)) return(NULL)
  fi[!rm_fi,]
}

meta_expression_filter <- function(tc, fi, queries) {
  rm_fi = rep(F, nrow(fi))
  for (expr in unique(queries$query_terms$meta_expr)) {
    if (is.na(expr)) next
    q_has_expr = !is.na(queries$query_terms$meta_expr) & queries$query_terms$meta_expr == expr
    fi_has_expr = q_has_expr[fi$dict] 
    #browser()
    doc_ids = tc$tokens$doc_id[fi$feat_i[fi_has_expr]]
    udoc_ids = as.character(unique(doc_ids))
    
    existing_udoc_ids = intersect(udoc_ids, as.character(tc$meta$doc_id))
    if (length(existing_udoc_ids) < length(udoc_ids)) {
      warning('Some document ids in $tokens do not occur in $meta')
      if (length(existing_udoc_ids) == 0) next
      udoc_ids = existing_udoc_ids
    }
    #stop('Does not work yet. The filtering on udoc ids messes up if doc does not exist')
    which_expr_false = tryCatch(tc$meta[list(udoc_ids),,on='doc_id'][!eval(parse(text=expr)),,which=T], error = function(e) e)
    ## special error handling, because this can be confusing
    if (inherits(which_expr_false, 'simpleError')) 
      stop(call. = F, paste('A meta subset expression used in a query gave an error:', paste0('The expression\n', expr), paste0('The error\n',which_expr_false$message), sep='\n\n'))
    
    rm_doc_id = udoc_ids[which_expr_false]
    which_rm_fi = doc_ids %in% rm_doc_id
  
    rm_fi[fi_has_expr][which_rm_fi] = T                                                     ## take features with expression, and of those set rm_fi features where expression evaluated to false to T (i.e. should be removed)
  }
  if (all(rm_fi)) return(NULL)
  fi[!rm_fi,]
}


lucene_like <- function(dict_results, qlist, mode = c('unique_hits','features','contexts'), subcontext=NULL, parent_relation='', keep_longest=TRUE, level=1) {
  .ghost = NULL; .term_i = NULL; .seq_i = NULL; .group_i = NULL ## for solving CMD check notes (data.table syntax causes "no visible binding" message)
  mode = match.arg(mode) ## 'unique_hit' created complete and unique sets of hits (needed for counting) but doesn't assign all features
                         ## 'features' mode does not make full sets of hits, but returns all features for which the query is true (needed for coding/dictionaries)
  i = NULL; hit_id = NULL ## for solving CMD check notes (data.table syntax causes "no visible binding" message)
  hit_list = vector('list', length(qlist$terms))
  
  nterms = length(qlist$terms)
  for (j in 1:nterms) {
    q = qlist$terms[[j]]
    is_nested = 'terms' %in% names(q)
    if (is_nested) {
      jhits = lucene_like(dict_results, q, mode=mode, subcontext=subcontext, parent_relation=qlist$relation, keep_longest=keep_longest, level=level+1)
      
      if (nterms == 1) {
        if (level == 1 && mode == 'unique_hits' & !is.null(jhits)) jhits = remove_duplicate_hit_id(jhits, keep_longest)
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
      only_context = mode == 'contexts' && qlist$relation %in% c('AND','NOT')  ## only_context is more efficient, but can only be used for AND and NOT, because proximity and sequence require feature positions (and OR can be nested in them)
      
      jhits = lookup(dict_results, q$term, feature=q$feature, case_sensitive=q$case_sensitive, only_context=only_context)
      
      if (!is.null(jhits)) {
        jhits[, .ghost := q$ghost]
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
  if (qlist$relation %in% c('NOT')) get_NOT_hit(hits, n_unique = nterms, subcontext=subcontext, group_i = '.group_i', replace = '.ghost', feature_mode=T)
  if (qlist$relation == 'proximity') get_proximity_hit(hits, n_unique = nterms, window=qlist$window, subcontext=subcontext, seq_i = '.seq_i', replace='.ghost', feature_mode=feature_mode, directed=qlist$directed) ## assign hit_ids to groups of tokens within the given window
  if (qlist$relation %in% c('OR', '')) get_OR_hit(hits)
  if (qlist$relation == 'sequence') get_sequence_hit(hits, seq_length = nterms, subcontext=subcontext) ## assign hit ids to valid sequences
  
  
  hits = subset(hits, hit_id > 0)
  
  if (nrow(hits) == 0) return(NULL)
  if (level == 1 && mode == 'unique_hits') hits = remove_duplicate_hit_id(hits, keep_longest)
  if (level == 1 && mode == 'contexts') hits = unique(subset(hits, select=c('doc_id',subcontext)))
  return(hits)
}

lookup <- function(dict_results, terms, feature='token', case_sensitive=TRUE, subcontext=NULL, only_context=F){
  if (!feature %in% names(dict_results))
    return(NULL)
  
  dupl = duplicated(terms)
  if (any(dupl)) {
    terms = terms[!dupl]
    if (length(case_sensitive) > 1) case_sensitive = case_sensitive[!dupl]
  }
  
  dr = dict_results[[feature]]
  f = data.table::data.table(term=terms, case_sensitive=case_sensitive)
  out = dr[f, allow.cartesian=T, nomatch=0]
  if (nrow(out) == 0) return(NULL)
  
  out[, case_sensitive := NULL]
  if (only_context) out = unique(out, by=c('doc_id',subcontext))
  out
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
  .hit_id = AND_hit_ids_cpp(as.numeric(d[['doc_id']]), as.numeric(subcontext), as.numeric(d[['.term_i']]), n_unique, as.character(group_i), replace, feature_mode)
  d[,hit_id := .hit_id]
}


get_NOT_hit <- function(d, n_unique, subcontext=NULL, group_i=NULL, replace=NULL, feature_mode=F){
  hit_id = NULL ## used in data.table syntax, but need to have bindings for R CMD check
  setorderv(d, c('doc_id', 'token_id', '.term_i'))
  if (!is.null(subcontext)) subcontext = d[[subcontext]]
  if (!is.null(group_i)) group_i = d[[group_i]]
  if (!is.null(replace)) replace = d[[replace]]
  .hit_id = AND_hit_ids_cpp(as.numeric(d[['doc_id']]), as.numeric(subcontext), as.numeric(d[['.term_i']]), n_unique, as.character(group_i), replace, feature_mode)
  if (!'hit_id' %in% colnames(d)) d[, hit_id := integer()]
  if (any(is.na(d$hit_id))) d[, hit_id := ifelse(is.na(hit_id), 1:nrow(d), hit_id)]
  d[!(d$.term_i == 1 & .hit_id == 0), hit_id := 0]
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
  .hit_id_length = .ghost = hit_id = ngram = NULL ## for solving CMD check notes (data.table syntax causes "no visible binding" message)
  if (!'token_id' %in% colnames(d)) return(d)
  
  ## first ignore duplicates within same hit_id
  ## this can happen if different feature columns are used
  if (!'.ghost' %in% colnames(d)) d$.ghost = F
  d = subset(d, !duplicated(d[,c('doc_id','token_id','hit_id','.ghost')]))
  
  dup = duplicated(d[,c('doc_id','token_id')]) & !d$.ghost
  
  if (any(dup)) {

    if (keep_longest) {
      d[, .hit_id_length := sum(ngram[!.ghost]), by=hit_id]   ## count non ghost terms per hit_id
      pd = data.table::setorderv(d, '.hit_id_length', -1)
      dup_id = pd$hit_id[duplicated(pd[,c('doc_id','token_id')]) & !d$.ghost]
      d[, .hit_id_length := NULL]
    } else {
      dup_id = d$hit_id[dup & !d$.ghost]
    }
    d = subset(d, !hit_id %in% unique(dup_id))
  }
  
  d
}



