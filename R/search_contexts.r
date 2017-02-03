#' Title
#'
#' @param tc
#' @param query
#' @param code
#' @param feature
#' @param context_level
#'
#' @export
subset_query <- function(tc, query, code=NULL, feature='word', context_level=c('document','sentence')){
  context_level = match.arg(context_level)
  hits = search_contexts(tc, query, code=code, feature=feature, context_level=context_level)
  if(context_level == 'document'){
    tc = subset(tc, doc_id %in% unique(hits$doc_id))
  }
  if(context_level == 'sentence'){
    d = get_data(tc, columns = c('doc_id','sent_i'))
    d$i = 1:nrow(d)
    rows = d[list(hits$doc_id, hits$sent_i)]$i
    tc = subset(tc, rows)
  }
  tc
}


#' Title
#'
#' @param tc
#' @param query
#' @param code
#' @param feature
#' @param context_level
#'
#' @export
search_contexts <- function(tc, query, code=NULL, feature='word', context_level=c('document','sentence')){
  context_level = match.arg(context_level)
  #query = c('(rutte AND vvd) OR "mark rutte"', '"geert AND (wilders OR pvv)')
  ## vergeet niet om OR statements binnen exact string matching toe te staan
  #get_feature_regex('"mark rutte"')

  windows = na.omit(get_feature_regex(query, default_window = NA)$window)
  max_window_size = if(length(windows) > 0) max(windows) else 0
  fi = get_feature_index(tc, feature=feature, context_level=context_level, max_window_size = max_window_size)

  if(!is.null(code)){
    code = if(length(code) == length(query)) code else rep(code, length(query))
  } else code = sprintf('query %s', 1:length(query))

  context = get_context(tc, context_level)
  context_i = as.numeric(context)
  context_label = levels(context)
  queries = parse_queries(query) ## add argument merge_or_groups = T

  res = list()
  for(i in 1:nrow(queries)){
    q = queries[i,]
    qm = Matrix::spMatrix(max(context_i), length(q$terms), x=logical())
    colnames(qm) = q$terms
    #rownames(qm) = context_label # for reference

    for(term in q$terms){
      hits = search_string(tc, fi, term)
      context_hits = unique(context_i[hits$i])
      qm[,term][context_hits] = T
    }

    queryhit = eval_query_matrix(qm, q$terms, q$form)
    first_context_row = match(context_label[queryhit], context)
    if(context_level == 'document') context_columns = c('doc_id')
    if(context_level == 'sentence') context_columns = c('doc_id', 'sent_i')
    code_label = code[[i]]
    res[[code_label]] = unique(get_data(tc)[first_context_row, context_columns, with=F])
  }
  hits = plyr::ldply(res, function(x) x, .id='code')
  if(nrow(hits) == 0) hits = NULL
  hits
}


