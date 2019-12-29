### most resources work with huge stringmatch tables
### therefore, the following functions are optimized for mathing many strings at once, as opposed to the more versatile search_features function.


#' Dictionary lookup
#'
#' @description
#' Add a column to the token data that contains a code (the query label) for tokens that match the dictionary
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{code_dictionary(...)}
#'
#' @param dict            A dictionary. Can be either a data.frame or a quanteda dictionary. If a data.frame is given, it has to
#'                        have a column named "string" (or use string_col argument) that contains the dictionary terms. All other columns are added to the
#'                        tCorpus $tokens data. Each row has a single string, that can be
#'                        a single word or a sequence of words seperated by a whitespace (e.g., "not bad"), and can have the common ? and * wildcards.
#'                        If a quanteda dictionary is given, it is automatically converted to this type of data.frame with the
#'                        \code{\link{melt_quanteda_dict}} function. This can be done manually for more control over labels.
#' @param token_col       The feature in tc that contains the token text.
#' @param string_col      If dict is a data.frame, the name of the column in dict that contains the dictionary lookup string
#' @param sep             A regular expression for separating multi-word lookup strings (default is " ", which is what quanteda dictionaries use).
#'                        For example, if the dictionary contains "Barack Obama", sep should be " " so that it matches the consequtive tokens "Barack" and "Obama".
#'                        In some dictionaries, however, it might say "Barack+Obama", so in that case sep = '\\+' should be used.
#' @param case_sensitive  logical, should lookup be case sensitive?
#' @param column          The name of the column added to $tokens. [column]_id contains the unique id of the match.
#'                        If a quanteda dictionary is given, the label for the match is in the column named [column].
#'                        If a dictionary has multiple levels, these are added as [column]_l[level].
#' @param use_wildcards   Use the wildcards * (any number including none of any character) and ? (one or none of any character). If FALSE, exact string matching is used.
#' @param flatten_colloc  If true, collocations in the tokens (rows in tc$tokens) will be considered separate words. For example, "President_Obama" will be split to "president" "obama", so that "president obama" in the dictionary matches correctly.
#' @param ascii           If true, convert text to ascii before matching
#' @param verbose         If true, report progress
#'
#' @return A vector with the id value (taken from dict$id) for each row in tc$tokens
#'
#' @name tCorpus$code_dictionary
#'
#' @examples
#' dict = data.frame(string = c('good','bad','ugl*','nice','not pret*'), sentiment=c(1,-1,-1,1,-1))
#' tc = create_tcorpus(c('The good, the bad and the ugly, is nice but not pretty'))
#' tc$code_dictionary(dict)
#' print(tc$tokens)
#' @aliases code_dictionary
tCorpus$set('public', 'code_dictionary', function(dict, token_col='token', string_col='string', sep=' ', case_sensitive=F, column='code', use_wildcards=T, flatten_colloc=T, ascii=F, verbose=F){
  if (methods::is(dict, 'dictionary2')) dict = melt_quanteda_dict(dict, column = column)
  if (!methods::is(dict, 'data.frame')) stop('dict has to be a data.frame or a quanteda dictionary2 class')
  if (!string_col %in% colnames(dict)) stop(sprintf('dict does not have a column named "%s"', string_col))

  column_id = paste0(column, '_id')
  if (column_id %in% self$names) self$delete_columns(column_id)

  fl = dictionary_lookup(self, data.table::data.table(string=dict[[string_col]], id = 1:nrow(dict), stringsAsFactors = F), regex_sep = sep,
                        token_col=token_col, case_sensitive=case_sensitive,
                        flatten_colloc=flatten_colloc, ascii=ascii, use_wildcards=use_wildcards, verbose=verbose)

  if (is.null(fl)) {
    self$set(column_id, numeric())
    return(invisible(self))
  }

  is_hit = !is.na(fl$dict_i)
  anno = dict[as.numeric(fl$dict_i[is_hit]),]
  hit_id = fl$hit_id[is_hit]

  self$set(column_id, hit_id, subset = is_hit, subset_value=F)

  for (.col in colnames(anno)) {
    if (.col == string_col) next
    if (.col %in% c('doc_id','sentence','token_id')) next  ## cant overwrite these
    if (.col %in% self$names) self$delete_columns(.col)
    .value = anno[[.col]]
    self$set(.col, anno[[.col]], subset = is_hit, subset_value=F)
  }
  invisible(self)
})


#' Replace tokens with dictionary match
#'
#' @description
#' Uses \code{\link{search_dictionary}}, and replaces tokens that match the dictionary lookup term with the dictionary code.
#' Multi-token matches (e.g., "Barack Obama") will become single tokens. Multiple lookup terms per code can be used to deal with
#' alternatives such as "Barack Obama", "president Obama" and "Obama".
#'
#' This method can also be use to concatenate ASCII symbols into emoticons, given a dictionary of emoticons.
#' A dictionary with common emoticons is included in the corpustools data as "emoticon_dict" (see examples).
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{replace_dictionary(...)}
#'
#' @param dict            A dictionary. Can be either a data.frame or a quanteda dictionary. If a data.frame is given, it has to
#'                        have a column named "string"  (or use string_col argument) that contains the dictionary terms, and a column "code" (or use code_col argument) that contains the
#'                        label/code represented by this string. Each row has a single string, that can be
#'                        a single word or a sequence of words seperated by a whitespace (e.g., "not bad"), and can have the common ? and * wildcards.
#'                        If a quanteda dictionary is given, it is automatically converted to this type of data.frame with the
#'                        \code{\link{melt_quanteda_dict}} function. This can be done manually for more control over labels.
#' @param token_col       The feature in tc that contains the token text.
#' @param string_col      If dict is a data.frame, the name of the column in dict with the dictionary lookup string. Default is "string"
#' @param code_col        The name of the column in dict with the dictionary code/label. Default is "code".
#'                        If dict is a quanteda dictionary with multiple levels, "code_l2", "code_l3", etc. can be used to select levels.
#' @param replace_cols    The names of the columns in tc$tokens that will be replaced by the dictionary code. Default is the column on which the dictionary is applied,
#'                        but in some cases it might make sense to replace multiple columns (like token and lemma)
#' @param sep             A regular expression for separating multi-word lookup strings (default is " ", which is what quanteda dictionaries use).
#'                        For example, if the dictionary contains "Barack Obama", sep should be " " so that it matches the consequtive tokens "Barack" and "Obama".
#'                        In some dictionaries, however, it might say "Barack+Obama", so in that case sep = '\\+' should be used.
#' @param code_from_features If TRUE, instead of replacing features with the matched code columnm, use the most frequent occuring string in the features.
#' @param code_sep        If code_from_features is TRUE, the separator for pasting features together. Default is an underscore, which is recommended because it has special
#'                        features in corpustools. Most importantly, if a query or dictionary search is performed, multi-word tokens concatenated with an underscore are treated
#'                        as separate consecutive words. So, "Bob_Smith" would still match a lookup for the two consequtive words "bob smith"
#' @param decrement_ids   If TRUE (default), decrement token ids after concatenating multi-token matches. So, if the tokens c(":", ")", "yay") have token_id c(1,2,3),
#'                        then after concatenating ASCII emoticons, the tokens will be c(":)", "yay") with token_id c(1,2)
#' @param case_sensitive  logical, should lookup be case sensitive?
#' @param use_wildcards   Use the wildcards * (any number including none of any character) and ? (one or none of any character). If FALSE, exact string matching is used
#' @param flatten_colloc  If true, collocations in the tokens (tokens with spaces or underscores) will be considered separate words. For example, "President_Obama" will be split to "president" "obama", so that "president obama" in the dictionary matches correctly.
#' @param ascii           If true, convert text to ascii before matching
#' @param verbose         If true, report progress
#'
#' @return A vector with the id value (taken from dict$id) for each row in tc$tokens
#'
#' @name tCorpus$replace_dictionary
#'
#' @examples
#' tc = create_tcorpus('yay :) :* happy')
#' tc$replace_dictionary(emoticon_dict)
#' tc$tokens
#'
#' @aliases replace_dictionary
tCorpus$set('public', 'replace_dictionary', function(dict, token_col='token', string_col='string', code_col='code', replace_cols=token_col, sep=' ', code_from_features=F, code_sep='_', decrement_ids=T, case_sensitive=F, use_wildcards=T, flatten_colloc=T, ascii=F, verbose=F){
  m = search_dictionary(self, dict, token_col=token_col, string_col=string_col, code_col=code_col, sep=sep,
                        case_sensitive=case_sensitive, use_wildcards=use_wildcards,
                        flatten_colloc=flatten_colloc, ascii=ascii, verbose=verbose)
  m = m$hits

  if (nrow(m) == 0) return(invisible(self))

  if (code_from_features) m = code_from_features(m, collapse_sep=code_sep)

  rename = unique(m, by=c('doc_id','hit_id'))
  remove = m[!rename]

  for (col in replace_cols) {
    self$tokens[rename, (col) := rename$code, on=c('doc_id','token_id')]
  }

  if (nrow(remove) > 0) {
    if (decrement_ids) {
      .REMOVE = F
      self$tokens[, .REMOVE:=.REMOVE]
      self$tokens[remove, .REMOVE := T]
      self$tokens[, token_id := token_id - cumsum(.REMOVE), by=c('doc_id')]
      self$tokens = subset(self$tokens, !.REMOVE)
      self$tokens[, .REMOVE := NULL]
    } else {
      self$tokens = self$tokens[!remove, on=c('doc_id','token_id')]
    }
  }
  self$tokens[]
  return(invisible(self))
})

#' Convert a quanteda dictionary to a long data.table format
#'
#' This is used internally in the tCorpus dictionary search functions, but can be used manually
#' for more control. For example, adding numeric scores for sentiment dictionaries, and specifying which label/code to use in search_dictionary().
#'
#' @param dict      The quanteda dictionary
#' @param column    The name of the column with the label/code. If dictionary contains multiple levels,
#'                  additional columns are added with the suffix _l[i], where [i] is the level.
#' @param .index    Do not use (used for recursive melting)
#'
#' @return A data.table
#' @export
#'
#' @examples
#' \donttest{
#' d = quanteda::data_dictionary_LSD2015
#' melt_quanteda_dict(d)
#' }
melt_quanteda_dict <- function(dict, column='code', .index=NULL) {
  if (is.null(.index)) {
    if (!methods::is(dict, 'dictionary2')) stop('dict is not a quanteda dictionary2 class')
    .index = data.table(string = character(length(dict)))
  }
  cname = if (ncol(.index) > 1) paste0(column, '_l', ncol(.index)) else column
  .index[[cname]] = names(dict)

  n = sapply(dict, length)
  .index = .index[rep(1:nrow(.index), n)]
  dict = unlist(dict, recursive = F, use.names = T)
  names(dict) = gsub('.*\\.', '', names(dict))


  if (!any(sapply(dict, class) == 'list')) {
    if (length(unlist(dict)) > nrow(.index)) {
      n = sapply(dict, length)
      .index = .index[rep(1:nrow(.index), n)]
    }
    .index$string = unlist(dict)
    return(.index)
  }

  melt_quanteda_dict(dict, column, .index)
}

#' Dictionary lookup
#'
#' Similar to search_features, but for fast matching of large dictionaries.
#'
#' @param tc              A tCorpus
#' @param dict            A dictionary. Can be either a data.frame or a quanteda dictionary. If a data.frame is given, it has to
#'                        have a column named "string"  (or use string_col argument) that contains the dictionary terms, and a column "code" (or use code_col argument) that contains the
#'                        label/code represented by this string. Each row has a single string, that can be
#'                        a single word or a sequence of words seperated by a whitespace (e.g., "not bad"), and can have the common ? and * wildcards.
#'                        If a quanteda dictionary is given, it is automatically converted to this type of data.frame with the
#'                        \code{\link{melt_quanteda_dict}} function. This can be done manually for more control over labels.
#' @param token_col       The feature in tc that contains the token text.
#' @param string_col      If dict is a data.frame, the name of the column in dict with the dictionary lookup string. Default is "string"
#' @param code_col        The name of the column in dict with the dictionary code/label. Default is "code".
#'                        If dict is a quanteda dictionary with multiple levels, "code_l2", "code_l3", etc. can be used to select levels..
#' @param sep             A regular expression for separating multi-word lookup strings (default is " ", which is what quanteda dictionaries use).
#'                        For example, if the dictionary contains "Barack Obama", sep should be " " so that it matches the consequtive tokens "Barack" and "Obama".
#'                        In some dictionaries, however, it might say "Barack+Obama", so in that case sep = '\\+' should be used.
#' @param case_sensitive  logical, should lookup be case sensitive?
#' @param use_wildcards   Use the wildcards * (any number including none of any character) and ? (one or none of any character). If FALSE, exact string matching is used
#' @param flatten_colloc  If true, collocations in the tokens (rows in tc$tokens) will be considered separate words. For example, "President_Obama" will be split to "president" "obama", so that "president obama" in the dictionary matches correctly.
#' @param ascii           If true, convert text to ascii before matching
#' @param verbose         If true, report progress
#'
#' @return A vector with the id value (taken from dict$id) for each row in tc$tokens
#' @export
#'
#' @examples
#' dict = data.frame(string = c('this is', 'for a', 'not big enough'), code=c('a','c','b'))
#' tc = create_tcorpus(c('this is a test','This town is not big enough for a test'))
#' search_dictionary(tc, dict)$hits
search_dictionary <- function(tc, dict, token_col='token', string_col='string', code_col='code', sep=' ', case_sensitive=F, use_wildcards=T, flatten_colloc=T, ascii=F, verbose=F){
  hit_id = NULL

  if (!is_tcorpus(tc)) stop('tc is not a tCorpus')
  if (methods::is(dict, 'dictionary2')) dict = melt_quanteda_dict(dict)
  if (!methods::is(dict, 'data.frame')) stop('dict has to be a data.frame or a quanteda dictionary2 class')
  if (!string_col %in% colnames(dict)) stop(sprintf('dict does not have a column named "%s"', string_col))
  if (!code_col %in% colnames(dict)) stop(sprintf('dict does not have a column named "%s"', code_col))

  fl = dictionary_lookup(tc, data.table::data.table(string=dict[[string_col]], id = 1:nrow(dict)), regex_sep=sep,
                        token_col=token_col, case_sensitive=case_sensitive, flatten_colloc=flatten_colloc, ascii=ascii, use_wildcards=use_wildcards, verbose=verbose)
  if (is.null(fl)) return(featureHits(NULL, data.frame()))


  not_na = !is.na(fl$dict_i)
  hits = tc$get(subset = not_na)
  hits$hit_id = fl$hit_id[not_na]
  hits$code = dict[[code_col]][as.numeric(fl$dict_i[not_na])]
  if (!'sentence' %in% colnames(hits)) hits[, 'sentence' := numeric()]
  hits = subset(hits, select = intersect(c('doc_id','token_id','sentence','code','hit_id',token_col), colnames(hits)))
  data.table::setnames(hits, token_col, 'feature')


  queries = data.frame()
  featureHits(hits, queries)
}

dictionary_lookup <- function(tc, dict, regex_sep=' ', token_col='token', case_sensitive=F, flatten_colloc=T, ascii=F, use_wildcards=T, context_level=c('document','sentence'), verbose=F){
  if (!token_col %in% tc$names) stop(sprintf('To use search_dictionary, the tCorpus must have a feature column with clean (not preprocessed) text, labeled "%s"', token_col))
  if (!'string' %in% colnames(dict)) stop('Dictionary must have column named "string"')
  if (!'id' %in% colnames(dict)) stop('Dictionary must have column named "id"')
  context_level = match.arg(context_level)

  if (verbose) message("Preparing features")
  fi = data.table::data.table(feature=tc$get(token_col),
                              i=1:tc$n,
                              context = as.numeric(tc$context(context_level)))


  levels(fi$feature) = normalize_string(levels(fi$feature), lowercase=!case_sensitive, ascii = ascii)
  #data.table::setkey(fi, 'feature')
  if (flatten_colloc) {
    is_colloc = grep(' |_', levels(fi$feature))
    if (length(is_colloc) > 0){
      fi = flatten_collocations(fi, 'feature', 'i', reset_key = F)
      message(sprintf('flattened %s collocations', length(is_colloc)))
    } else {
      #message('flatten_colloc (collocations) is set to TRUE, but no collocations were found.')
      flatten_colloc = F ## if there are no collocations, ignore flatten_colloc == T
    }
  }

  if (verbose) message("Preparing dictionary")
  d = collapse_dict(dict$string, regex_sep, use_wildcards, case_sensitive, ascii, levels(fi$feature))
  if (!'terms' %in% names(d)) return(NULL)

  data.table::setindexv(fi, 'feature')
  first_terms = levels(fi$feature)[d$terms_i]
  initial_i = fi[list(feature=first_terms), on='feature', which=T, nomatch=0]
  initial_i = sort(unique(initial_i))

  if (verbose) message("Coding features")
  out = do_code_dictionary(as.numeric(fi$feature), context = fi$context, which = initial_i, dict = d, verbose=verbose)

  out$hit_id[out$hit_id == 0] = NA
  out$dict_i[out$dict_i == 0] = NA
  data.table::data.table(hit_id=out$hit_id, dict_i=out$dict_i)
}

normalize_string <- function(x, lowercase=T, ascii=T, trim=T){
  if (lowercase) x = tolower(x)
  if (ascii) x = iconv(x, to='ASCII//TRANSLIT')
  if (trim) x = stringi::stri_trim(x)
  x
}

collapse_dict <- function(string, regex_sep, use_wildcards, case_sensitive, ascii, feature_levels) {
  dict = data.table::data.table(string = normalize_string(string, lowercase=!case_sensitive, ascii=ascii))

  ## remove separator if at start or end of word
  first_or_last = paste0('^',regex_sep, '|', regex_sep, '$')
  dict$string = gsub(first_or_last, '', dict$string)

  sn = stringi::stri_split(dict$string, regex=regex_sep)
  if (use_wildcards && any(grepl('[?*]', dict$string))) {
    sn = expand_wildcards(sn, feature_levels)
    names(sn) = floor(as.numeric(names(sn)))
  } else {
    names(sn) = 1:length(sn)
  }

  if (length(sn) == 0) return(NULL)

  ## for binary search in c++, there are issues with different ordering of terms in R and c++ (and more genrally encoding issues)
  ## here we replace all terms in the dictionary with factor levels
  sn = replace_string_with_factor(sn, feature_levels)

  if (length(sn) == 0) return(NULL)
  rec_collapse_dict(sn, regex_sep=regex_sep)
}

rec_collapse_dict <- function(l, i=1, regex_sep=' ') {
  out = list()

  has_terms = !is.na(sapply(l, '[', j=i))
  if (any(!has_terms)) {
    out$code = as.numeric(names(l)[which(!has_terms)[1]])
    if (all(!has_terms)) return(out)
    l = l[has_terms]
  }

  term = sapply(l, '[', j=i, simplify = T)
  terms = split(l, term)

  out$terms = sapply(terms, rec_collapse_dict, i=i+1, regex_sep=regex_sep, USE.NAMES = F, simplify=F)
  if (length(out$terms) == 0) {
    out$terms = NULL
  } else {
    out$terms_i = as.numeric(names(out$terms))
    names(out$terms) = NULL
  }
  out
}

replace_string_with_factor <- function(query_list, l) {
  ln = names(query_list)
  ## query_list is the list with split dictionary terms
  ## l is the levels of the features
  n = sapply(query_list, length)

  i = rep(1:length(query_list), n)

  ql = data.table::data.table(t = unlist(query_list), i = i)
  ql$t = as.numeric(factor(ql$t, levels=l))
  new = split(ql$t, ql$i)
  names(new) = ln
  new[!sapply(new, anyNA, simplify = T)]
}

expand_wildcards <- function(query_list, voc) {
  ## get a new list where terms with wildcards are repeated for all matches in vocabulary
  ## the names of the list contain ids of which the floor is the index of the dictionary
  n = sapply(query_list, length)
  i = rep(1:length(query_list), n)
  ql = data.table::data.table(t = unlist(query_list), i = i)
  add_n <- function(x) (1:length(x)) + 0  ## (suspected altrep issues)
  ql[, n := add_n(t), by='i']

  ql$is_wc =  grepl('[?*]', ql$t)
  if (!any(ql$is_wc)) {
    names(query_list) = 1:length(query_list)
    return(query_list)
  }
  wct = unique(ql$t[ql$is_wc])


  wctreg = gsub('([^a-zA-Z0-9\\*\\?])', '\\\\\\1', wct)


  ## find more elegant solution for not matching escaped * and ?
  wctreg = gsub('\\\\\\*', '##ASTER##', wctreg)
  wctreg = gsub('\\\\\\?', '##QUEST##', wctreg)

  wctreg = gsub('\\?+', '?', wctreg)
  wctreg = gsub('\\*+', '*', wctreg)
  justast = wctreg == '*'
  if (any(justast)) {
    warning('Some terms are only an asterisk wildcard, and so could be anything. These are ignored')
    wctreg[justast] = '###IGNORE###'
  }

  wctreg = gsub('\\*', '.*', wctreg)
  wctreg = gsub('\\?', '.{0,1}', wctreg)
  wctreg = gsub('##ASTER##', '\\*', wctreg, fixed=T)
  wctreg = gsub('##QUEST##', '\\?', wctreg, fixed=T)
  wctreg = paste0('\\b',wctreg,'\\b')


  full_t = sapply(wctreg, grep, x=voc, value=T, simplify = F)
  nreg = sapply(full_t, length)

  if (sum(nreg) > 0) nr = (1:sum(nreg)) + 0 else nr = numeric()
  full_t = data.table(t = rep(wct, nreg),
                      full_t = unlist(full_t),
                      nr = nr)

  full_t = merge(full_t, ql[,c('i','t')], by='t', allow.cartesian=T)
  out = merge(full_t, ql, by='i', all=T, allow.cartesian = T)

  out$nr[is.na(out$nr)] = 0
  data.table::setorderv(out, 'n', 1)
  out$id = out$i + (out$nr / (max(out$nr)+1))
  out = split(ifelse(out$is_wc, out$full_t, out$t.y), out$id)
  has_na = sapply(out, anyNA)
  out[!has_na]
}

code_from_features <- function(hits, collapse_sep='_') {
  feature = NULL; hit_id = NULL; group = NULL; code = NULL
  code = hits[, list(.new_code = paste(feature, collapse=collapse_sep)), by=c('hit_id','code')]
  code = code[, list(N = length(hit_id)), by=c('code','.new_code')]
  data.table::setorderv(code, 'N', order = -1)
  code = unique(code, by='code')
  hits = merge(hits, code[,c('code','.new_code')], by='code')
  hits$code = hits$.new_code
  hits
}
