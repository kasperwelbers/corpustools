search_doc_query <- function(tc, fi, hit, condition, feature, default_window=NA){
  con_regex = get_feature_regex(condition, default_window = default_window)
  qm = Matrix::spMatrix(nrow(hit),nrow(con_regex), x=logical())
  colnames(qm) = con_regex$term

  if(nrow(con_regex) == 0){
    return(hit)
  } else {
    hit_doc = unique(hit$doc_id)
    remaining_features = as.character(unique(tc@data[J(hit_doc), feature, with=F][[1]]))

    for(con_regex_term in unique(con_regex$regex)){
      con_hit = fi[J(batch_grepl(con_regex_term, remaining_features)), c('i','global_i'), with=F]

      for(i in which(con_regex$regex == con_regex_term)){
        term = as.character(con_regex$term[i])
        window = con_regex$window[i]

        if(is.na(window)) {
          con_doc = tc@data[con_hit$i,]$doc_id
          qm[,term] = hit$doc_id %in% con_doc
        } else {
          shifts = -window:window
          shift = rep(shifts, times=nrow(con_hit))
          con_window = rep(con_hit$global_i, each = length(shifts)) + shift
          qm[,term] = hit$global_i %in% con_window
        }
      }
    }
  }
  q = parse_queries(condition)
  eval_query_matrix(qm, q[1,]$terms, q[1,]$form)
}
