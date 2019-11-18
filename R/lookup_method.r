## implementation of tCorpus$lookup

lookup_terms <- function(patterns, lookup_table, ignore_case=T, raw_regex=T, perl=F, batchsize=25, useBytes=T, as_ascii=FALSE){

  if (as_ascii) {
    patterns = stringi::stri_trans_general(patterns, "any-latin")
    patterns = stringi::stri_trans_general(patterns, "latin-ascii")
  }
  needs_expansion = grepl('?', patterns, fixed=T) | grepl('*', patterns, fixed=T)
  fixed_patterns = patterns[!needs_expansion]
  regex_patterns = patterns[needs_expansion]

  if (length(fixed_patterns) > 0) {
    fixed_patterns = search_term_fixed(fixed_patterns, ignore_case, as_ascii=F)
    fixed_patterns = lookup_table[fixed_patterns, on = 'lookup_term', nomatch=0]
    fixed_patterns = if (nrow(fixed_patterns) > 0) fixed_patterns$term else c()
  }
  if (length(regex_patterns) == 0) return(fixed_patterns)

  ## if there are also regex_patterns
  if (!raw_regex) regex_patterns = search_term_regex(regex_patterns)
  if (length(regex_patterns) > 1) { ## if there are multiple terms, make batches of terms and turn each batch into a single regex
    regex_patterns = split(regex_patterns, ceiling(seq_along(regex_patterns)/batchsize))
    regex_patterns = sapply(regex_patterns, stringi::stri_paste, collapse='|')
    out = rep(F, nrow(lookup_table))
    for(exp_pattern_batch in regex_patterns){
      out = out | grepl(exp_pattern_batch, lookup_table$lookup_term, ignore.case=ignore_case, perl=perl, useBytes=useBytes)
    }
  } else {
    out = grepl(regex_patterns, lookup_table$term, ignore.case=ignore_case, perl=perl, useBytes=useBytes)
  }
  regex_patterns = lookup_table$term[out]

  return(union(regex_patterns, fixed_patterns))
}

search_term_fixed <- function(patterns, ignore_case, as_ascii) {
  if (ignore_case) patterns = stringi::stri_trans_tolower(patterns)
  if (as_ascii) {
    patterns = stringi::stri_trans_general(patterns, "any-latin")
    patterns = stringi::stri_trans_general(patterns, "latin-ascii")
  }
  patterns
}

search_term_regex <- function(patterns) {
  patterns = gsub("([^0-9a-zA-Z])", '\\\\\\1', x=patterns)  # escape special characters
  patterns = gsub('\\\\(\\*)|\\\\(\\?)', '.\\1', patterns)  # process wildcards
  paste0('\\b',patterns,'\\b')                              # set word boundaries
}

create_lookup_table <- function(x, ignore_case, as_ascii) {
  lookup_term = search_term_fixed(x, ignore_case, as_ascii)
  lookup_table = data.table(term = x, lookup_term = lookup_term)

  .SPLIT = stringi::stri_split(lookup_table$lookup_term, regex = '\\_| ')
  len = sapply(.SPLIT, length)
  if (max(len) > 1) {
    lookup_table = lookup_table[rep(1:nrow(lookup_table), len),]
    lookup_table[,lookup_term := unlist(.SPLIT)]
  }
  setkey(lookup_table, 'lookup_term')
  lookup_table
}

## prepares a list of lookup tables
## this is used for more efficient use of the recursive_search function
## if no lookup_tables are provided to the recursive_search function, they are created for each term separately
prepare_lookup_tables <- function(tc, qlist, lookup_tables, feature='', as_ascii=F, all_case_sensitive=FALSE) {
  if ('terms' %in% names(qlist)) {
    if (qlist$all_case_sensitive) all_case_sensitive = TRUE
    if (!qlist$feature == "") feature = qlist[['feature']]

    for (i in seq_along(qlist$terms)) {
      lookup_tables = prepare_lookup_tables(tc, qlist$terms[[i]], lookup_tables, feature=feature, as_ascii=as_ascii, all_case_sensitive=all_case_sensitive)
    }
  } else {
    case_sensitive = qlist$case_sensitive | all_case_sensitive
    lookup_table_key = sprintf('feature=%s; ignore_case=%s; as_ascii=%s', feature, !case_sensitive, as_ascii)
    if (!lookup_table_key %in% names(lookup_tables)) {
      if (!feature %in% tc$names) stop(sprintf('Feature (%s) is not available. Current options are: %s', feature, paste(tc$feature_names, collapse=', ')))
      uval = if (is.factor(tc$tokens[[feature]])) levels(tc$tokens[[feature]]) else unique(tc$tokens[[feature]])
      #lookup_tables[[lookup_table_key]] = mem_create_lookup_table(uval, ignore_case=!case_sensitive, as_ascii=as_ascii)
      lookup_tables[[lookup_table_key]] = create_lookup_table(uval, ignore_case=!case_sensitive, as_ascii=as_ascii)
    }
  }
  lookup_tables
}

create_lookup_table_key <- function(feature, ignore_case, as_ascii) {
  sprintf('feature=%s; ignore_case=%s; as_ascii=%s', feature, ignore_case, as_ascii)
}


## memoise features deleted due to memory issues

#mem_lookup_terms <- memoise::memoise(lookup_terms)
#mem_create_lookup_table <- memoise::memoise(create_lookup_table)

#forget_if_new <- memoise::memoise(function(x){  ## the forget calls will only be performed if x changes
#  forget_all_mem()                              ## otherwise the invisible NULL is simply returned from cache
#  invisible(NULL)
#})

#forget_all_mem <- function(){
#  memoise::forget(mem_lookup_terms)
#  memoise::forget(mem_create_lookup_table)
#  memoise::forget(forget_if_new)
#  invisible(NULL)
#}

