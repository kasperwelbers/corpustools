search_string <- function(tc, string, unique_i=F, skip_i=c(), with_i=F, subcontext=NULL, feature='token'){
  ## supports single token strings, multitoken strings demarcated with quotes (e.g., "this string") and token proximities (e.g., "marco polo"~10)
  ## This function does not manage complex boolean queries (AND, NOT, parentheses).
  ## If multiple strings are given, results are added together as if they were connected with OR statements
  hit_id = NULL ## for solving CMD check notes (data.table syntax causes "no visible binding" message)

  if(any(grepl('\\b(AND|NOT)\\b', string))) stop('string cannot contain Boolean operators AND and NOT')

  ## if unique_i == TRUE, the same features cannot be counted in different strings, so queries are sorted from most to least specific (sequence > proximity > single, and more words is more specific)
  ## features that have already been assigned in a more specific query will not be used again

  ## subcontext can be a column in the token data. If given, the queries will be performed within unique combinations of doc_id and the subcontext column
  if (!is.null(subcontext)) if (!subcontext %in% tc$names) stop('subcontext is not a valid column in token data')

  regex = get_feature_regex(string)
  is_multitoken = grepl(' ', regex$term)  ## split into single and multi term queries (proximity and sequence),
  single = regex[!is_multitoken,,drop=F]  ## because single terms can be pooled, whereas multi term has to be done per term to take positions into account
  multi = regex[is_multitoken,,drop=F]

  offset_id = 0
  if (unique_i | length(skip_i) > 0) with_i = TRUE
  all_hits = list()
  ## multitoken queries
  if (nrow(multi) > 0) {
    multi$proximity = !is.na(multi$window)
    multi$length = sapply(strsplit(multi$regex, split=' '), length)
    multi = multi[order(-multi$length, multi$proximity),,drop=F]
    if (any(multi$window[multi$proximity] < 1)) stop("Proximity search window cannot be smaller than 1")

    for (i in 1:nrow(multi)) {
      regexterms = stringi::stri_split(multi$regex[i], fixed = ' ')[[1]]
      if (multi$proximity[i]) {
        hits = multi_lookup(tc, regexterms, window=multi$window[i], ignore_case=multi$ignore_case[i], skip_i = skip_i, with_i=with_i, subcontext=subcontext, feature=feature)
      } else {
        hits = multi_lookup(tc, regexterms, window=NULL, ignore_case=multi$ignore_case[i], skip_i = skip_i, with_i=with_i, subcontext=subcontext, feature=feature)
      }
      if (!is.null(hits)) {
        hits[, hit_id := hit_id + offset_id] ## offset the hit id (by reference)
        offset_id = max(hits$hit_id)
        if (unique_i) skip_i = union(skip_i, hits$i)
        all_hits[[i]] = hits
      }
    }
  }

  ## single token queries (no loop required. single_lookup combines terms in batches for efficiency)
  if (nrow(single) > 0) {
    hits = single_lookup(tc, x=single$regex, ignore_case=single$ignore_case, skip_i=skip_i, with_i=with_i, feature=feature)
    if (!is.null(hits)) {
      hits[, hit_id := hit_id + offset_id]
      all_hits[[length(all_hits) + 1]] = hits
    }
  }

  droplevels(unique(data.table::rbindlist(all_hits)))
}

single_lookup <- function(tc, x, ignore_case, perl=F, skip_i=c(), with_i=F, feature='token') {
  i = NULL; hit_id = NULL ## for solving CMD check notes (data.table syntax causes "no visible binding" message)

  hit_list = list()
  if(!all(ignore_case)) hit_list[['']] = tc$lookup(x[!ignore_case], feature=feature, ignore_case=F, perl=perl, with_i=with_i)
  if(any(ignore_case))  hit_list[['']] = tc$lookup(x[ignore_case], feature=feature, ignore_case=T, perl=perl, with_i=with_i)

  hits = data.table::rbindlist(hit_list)
  if (length(skip_i) > 0) hits = subset(hits, !i %in% skip_i)
  if (nrow(hits) == 0) return(NULL)

  hits[, hit_id := 1:nrow(hits)]
  hits
}

multi_lookup <- function(tc, x, window=NULL, ignore_case, perl=F, skip_i = c(), with_i=F, subcontext=NULL, feature='token'){
  ## keywords with underscores are considered word sequence strings. These can occur both in one row of the tcorpus features, or multiple
  ## this function doesn't care, and captures both, by walking over the tokens and checking whether they occur in the same or subsequent (i.e. next global_i) position
  ## if window is NULL, x is considered to be a sequence (i.e. each next value of x has to occur on the next (or same) location)
  i = NULL; hit_id = NULL ## for solving CMD check notes (data.table syntax causes "no visible binding" message)

  if (length(skip_i) > 0) with_i = TRUE

  hit_list = vector('list', length(x))
  for(j in 1:length(x)){
    hits = tc$lookup(x[j], feature=feature, ignore_case=ignore_case, perl=perl, with_i=with_i)
    if (length(hits) > 0) hit_list[[j]] = hits
  }
  hits = data.table::rbindlist(hit_list)

  if (nrow(hits) == 0) return(NULL)
  evalhere_j = rep(1:length(hit_list), sapply(hit_list, nrow))
  if (nrow(hits) > 0) hits[,j := evalhere_j]
  if (length(skip_i) > 0) hits = subset(hits, !i %in% skip_i)
  if (nrow(hits) == 0) return(NULL)

  if (is.null(window)) {
    hits[, hit_id := get_sequence_hit(hits, seq_length = length(x), subcontext=subcontext)] ## assign hit ids to valid sequences
  } else {
    hits[, hit_id := get_proximity_hit(hits, n_unique = length(x), window=window, subcontext=subcontext)] ## assign hit_ids to groups of tokens within the given window
  }

  hits[, j := NULL]
  hits = subset(hits, hit_id > 0)
  if (nrow(hits) > 0) hits else NULL
}

#AND_hit_ids <- function(d, n_unique, context='doc_id', value='j', subcontext=NULL){
#  setorderv(d, c(context, value))
#  context = if(is.null(subcontext)) d[[context]] else  global_position(d[[subcontext]], d[[context]], presorted = T, position_is_local=T)
#  .Call('_corpustools_AND_hit_ids', PACKAGE = 'corpustools', as.integer(context), as.integer(d[[value]]), n_unique)
#}

get_sequence_hit <- function(d, seq_length, context='doc_id', position='token_i', value='j', subcontext=NULL){
  setorderv(d, c(context, position, value))
  if (!is.null(subcontext)) subcontext = d[[subcontext]]
  .Call('_corpustools_sequence_hit_ids', PACKAGE = 'corpustools', as.integer(d[[context]]), as.integer(subcontext), as.integer(d[[position]]), as.integer(d[[value]]), seq_length)
}

get_proximity_hit <- function(d, n_unique, window=NA, context='doc_id', position='token_i', value='j', subcontext=NULL, group_id=NULL, assign_once=T){
  setorderv(d, c(context, position, value))
  if (!is.null(subcontext)) subcontext = d[[subcontext]]
  if (!is.null(group_id)) group_id = d[[group_id]]
  .Call('_corpustools_proximity_hit_ids', PACKAGE = 'corpustools', as.integer(d[[context]]), as.integer(subcontext), as.integer(d[[position]]), as.integer(d[[value]]), n_unique, window, as.numeric(group_id), assign_once)
}

get_OR_hit <- function(d) {
  if (!'hit_id' %in% colnames(d)) {
    i = 1:nrow(d)
  } else i = d$hit_id
  isna = is.na(i)
  if (any(isna)) {
    if (all(isna)) na_ids = 1:length(i) else na_ids = 1:sum(isna) + max(i, na.rm = T)
    i[isna] = na_ids
  }
  i
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
