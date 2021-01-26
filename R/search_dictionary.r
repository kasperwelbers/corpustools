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
#'                        (":-)" versus ":" "-" ")"). This is only behind the scenes for the dictionary lookup, and will not affect tokenization in the corpus.
#' @param ascii           If true, convert text to ascii before matching
#' @param verbose         If true, report progress
#'
#' @return the tCorpus
#'
#' @name tCorpus$code_dictionary
#'
#' @examples
#' dict = data.frame(string = c('good','bad','ugl*','nice','not pret*', ':)', ':('), 
#'                   sentiment=c(1,-1,-1,1,-1,1,-1))
#' tc = create_tcorpus(c('The good, the bad and the ugly, is nice :) but not pretty :('))
#' tc$code_dictionary(dict)
#' tc$tokens
#' @aliases code_dictionary
tCorpus$set('public', 'code_dictionary', function(dict, token_col='token', string_col='string', sep=' ', case_sensitive=F, column='code', use_wildcards=T, ascii=F, verbose=F){
  if (methods::is(dict, 'dictionary2')) dict = melt_quanteda_dict(dict, column = column)
  if (!methods::is(dict, 'data.frame')) stop('dict has to be a data.frame or a quanteda dictionary2 class')
  if (!string_col %in% colnames(dict)) stop(sprintf('dict does not have a column named "%s"', string_col))
  
  column_id = paste0(column, '_id')
  if (column_id %in% self$names) self$delete_columns(column_id)

  fi = dictionary_lookup(self, data.table::data.table(string=dict[[string_col]], id = 1:nrow(dict), stringsAsFactors = F), regex_sep = sep,
                        token_col=token_col, case_sensitive=case_sensitive,
                        standardize=T, ascii=ascii, use_wildcards=use_wildcards, verbose=verbose)
  
  if (is.null(fi)) {
    self$set(column_id, numeric())
    return(invisible(self))
  }
  
  #is_hit = !is.na(fi$dict_i)
  self$set(column_id, fi$hit_id, subset = fi$feat_i, subset_value=F)

  anno = dict[as.numeric(fi$dict_i),]
  for (.col in colnames(anno)) {
    if (.col == string_col) next
    if (.col %in% c('doc_id','sentence','token_id')) next  ## cant overwrite these
    if (.col %in% self$names) self$delete_columns(.col)
    .value = anno[[.col]]
    self$set(.col, anno[[.col]], subset = fi$feat_i, subset_value=F)
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
tCorpus$set('public', 'replace_dictionary', function(dict, token_col='token', string_col='string', code_col='code', replace_cols=token_col, sep=' ', code_from_features=F, code_sep='_', decrement_ids=T, case_sensitive=F, use_wildcards=T, ascii=F, verbose=F){
  m = search_dictionary(self, dict, token_col=token_col, string_col=string_col, code_col=code_col, sep=sep,
                        case_sensitive=case_sensitive, use_wildcards=use_wildcards,
                        ascii=ascii, verbose=verbose)
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
#' @param mode            There are two modes: "unique_hits" and "features". The "unique_hits" mode prioritizes finding unique matches, which is recommended for counting how often a dictionary term occurs.
#'                        If a term matches multiple dictionary terms (which should only happen for nested multi-word terms, such as "bad" and "not bad"), the longest term is always used. 
#'                        The features mode does not delete duplicates.
#' @param case_sensitive  logical, should lookup be case sensitive?
#' @param use_wildcards   Use the wildcards * (any number including none of any character) and ? (one or none of any character). If FALSE, exact string matching is used
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
search_dictionary <- function(tc, dict, token_col='token', string_col='string', code_col='code', sep=' ', mode = c('unique_hits','features'), case_sensitive=F, use_wildcards=T, ascii=F, verbose=F){
  hit_id = NULL
  mode = match.arg(mode)
  
  if (!is_tcorpus(tc)) stop('tc is not a tCorpus')
  if (methods::is(dict, 'dictionary2')) dict = melt_quanteda_dict(dict)
  if (!methods::is(dict, 'data.frame')) stop('dict has to be a data.frame or a quanteda dictionary2 class')
  if (!string_col %in% colnames(dict)) stop(sprintf('dict does not have a column named "%s"', string_col))
  if (!code_col %in% colnames(dict)) stop(sprintf('dict does not have a column named "%s"', code_col))

  fi = dictionary_lookup(tc, data.table::data.table(string=dict[[string_col]], id = 1:nrow(dict)), regex_sep=sep, mode=mode,
                        token_col=token_col, case_sensitive=case_sensitive, standardize=T, ascii=ascii, use_wildcards=use_wildcards, verbose=verbose)
  if (is.null(fi)) return(featureHits(NULL, data.frame()))
  
  hits = tc$tokens[fi$feat_i,]
  hits$hit_id = fi$hit_id
  hits$code = dict[[code_col]][as.numeric(fi$dict_i)]
  if (!'sentence' %in% colnames(hits)) hits[, 'sentence' := numeric()]
  hits = subset(hits, select = intersect(c('doc_id','token_id','sentence','code','hit_id',token_col), colnames(hits)))
  data.table::setnames(hits, token_col, 'feature')
  
  queries = data.frame()
  featureHits(hits, queries)
}

dictionary_lookup <- function(tc, dict, regex_sep=' ', token_col='token', mode = c('unique_hits','features'), case_sensitive=F, standardize=T, ascii=F, use_wildcards=T, context_level=c('document','sentence'), verbose=F){
  mode = match.arg(mode)
  if (!token_col %in% tc$names) stop(sprintf('specified token column ("%s") is not a valid column in tokens', token_col))
  if (!methods::is(tc$tokens[[token_col]], 'factor')) tc$set(token_col, fast_factor(tc$tokens[[token_col]]))
  fi = dictionary_lookup_tokens(tokens = tc$get(token_col), context = as.numeric(tc$context(context_level)), token_id=tc$tokens$token_id, dict=dict, mode=mode,
                                regex_sep=regex_sep, case_sensitive=case_sensitive, standardize=standardize, ascii=ascii, use_wildcards=use_wildcards, verbose=verbose)
  
}


dictionary_lookup_tokens <- function(tokens, context, token_id, dict, mode=mode, regex_sep=' ', case_sensitive=F, standardize=T, ascii=F, use_wildcards=T, verbose=F){
  if (!'string' %in% colnames(dict)) stop('Dictionary must have column named "string"')
  if (!'id' %in% colnames(dict)) stop('Dictionary must have column named "id"')
  
  if (verbose) message("Preparing features")

  fi = data.table::data.table(feature=tokens, i=1:length(tokens), context = context, token_id=token_id)


  if (standardize) {
    dict = standardize_dict_term_spacing(dict, use_wildcards)
    is_split = is_splittable(fi$feature)
    if (any(is_split)){
      fi = flatten_terms(fi, 'feature', 'i', reset_key = F)
      flatten = T
    } else {
      flatten = F ## if there are no collocations, ignore flatten_colloc == T
    }
  }

  
  if (any(case_sensitive) && !all(case_sensitive)) {
    if (length(case_sensitive) != nrow(dict)) stop('case_sensitive vector needs to be length 1 or length of dictionary')
    out1 = dictionary_lookup_tokens2(fi, dict[case_sensitive,], dict_i_ids = which(case_sensitive), mode=mode, case_sensitive=T, ascii, regex_sep, use_wildcards, flatten, 1, verbose)
    out2 = dictionary_lookup_tokens2(fi, dict[!case_sensitive,], dict_i_ids = which(!case_sensitive), mode=mode, case_sensitive=F, ascii, regex_sep, use_wildcards, flatten, max(out1$hit_id)+1, verbose)
    out = rbind(out1,out2)
  } else {
    out = dictionary_lookup_tokens2(fi, dict, dict_i_ids = 1:nrow(dict), mode=mode, unique(case_sensitive), ascii, regex_sep, use_wildcards, flatten, 1, verbose)
  }
  
  is_ast = which(dict$string == '*')
  if (any(is_ast)) {
    hit_id_offset = max(out$hit_id)+1
    ast_out = data.table::data.table(hit_id = 1:nrow(fi) + hit_id_offset, dict_i = is_ast, feat_i = fi$i)
    rbind(out, ast_out)
  } else
    out
  
  
}

dictionary_lookup_tokens2 <- function(fi, dict, dict_i_ids, mode, case_sensitive, ascii, regex_sep, use_wildcards, flatten, hit_id_offset=1, verbose=F) {
  ## split into 2 parts for more efficient processing of queries with both case sensitive and insensitive 
  
  levels(fi$feature) = normalize_string(levels(fi$feature), lowercase=!case_sensitive, ascii = ascii)
  #data.table::setkey(fi, 'feature')
  
  if (verbose) message("Preparing dictionary")
  d = collapse_dict(dict$string, regex_sep, use_wildcards, case_sensitive, ascii, levels(fi$feature))
  if (!'terms' %in% names(d)) return(NULL)
  
  data.table::setindexv(fi, 'feature')
  first_terms = levels(fi$feature)[d$terms_i]
  initial_i = fi[list(feature=first_terms), on='feature', which=T, nomatch=0]
  initial_i = sort(unique(initial_i))
  
  if (verbose) message("Coding features")
  
  out = do_code_dictionary(as.numeric(fi$feature), context = fi$context, token_id = fi$token_id, which = initial_i, dict = d, hit_id_offset=hit_id_offset, verbose=verbose)
  if (is.null(out) || nrow(out) == 0) return(NULL)
  out$dict_i = dict_i_ids[out$dict_i]
  
  if (flatten) {
    out$feat_i = fi$orig_i[out$feat_i]
  }
  if (mode == 'unique_hits') {
    data.table::setorderv(out, 'nterms', -1)
    out = out[!duplicated(out$feat_i),]
  }
  data.table::data.table(hit_id=out$hit_id, dict_i=out$dict_i, feat_i=out$feat_i)
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
    out$code = as.numeric(names(l)[which(!has_terms)])
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
    #warning('Some terms are only an asterisk wildcard, and so could be anything. These are ignored')
    wctreg[justast] = '###IGNORE###'
  }
  
  wctreg = gsub('\\*', '.*', wctreg)
  wctreg = gsub('\\?', '.{0,1}', wctreg)
  wctreg = gsub('##ASTER##', '\\*', wctreg, fixed=T)
  wctreg = gsub('##QUEST##', '\\?', wctreg, fixed=T)
  
  ## old approach (just perform regex on all terms)
  #wctreg = paste0('\\b',wctreg,'\\b') 
  #full_t = sapply(wctreg, grep, x=voc, value=T, simplify = F)
  
  ## new (faster) approach (possible due to the standardize step now implemented in dictionary_lookup)
  ## seems to give same results. Only exception is that it really relies on what is split by split_tokens (which might be a good thing)
  ## For instance, "stupid.dot" would before match "dot" because \\b considered the middel dot as a word boundary.
  ## now it doesn't because split_tokens (based on stringi split boundaries) doesn't consider this as two separate tokens
  wctreg = paste0('^',wctreg,'$')
  full_t = fast_wildcard_voc_match(wctreg, voc, n_bin_search = 3)
  
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

fast_wildcard_voc_match <- function(reg, voc, n_bin_search=3) {
  ## create an index for every term in vocabulary where key is the separate columns for the first n_bin_search characters
  ## these enable binary search on first [n_bin_search] terms of the fixed part of a regex
  voc_index = data.table::data.table(voc=voc, n=nchar(voc))
  for (i in 1:n_bin_search) voc_index[,(paste0('voc',i)) := substr(voc, i,i)]
  data.table::setkeyv(voc_index, paste0('voc', 1:n_bin_search))
  
  ## get the 'fixed' part of a regex term (only before a wildcard)
  fixedpart = gsub('\\\\b','',reg)
  fixedpart = gsub('^\\^|\\$$', '', fixedpart)
  fixedpart = gsub('\\.[{*].*', '', fixedpart)
  fixedpart = gsub('\\\\','', fixedpart)
  n = nchar(fixedpart)
  
  ## for every term create a list of the first [n_bin_search] terms from the fixed part
  bin_search_part = substr(fixedpart, 1, n_bin_search)
  qlists = stringi::stri_split_boundaries(bin_search_part, type='character')
  
  ## use multithreading
  cl = data.table::getDTthreads()
  if (.Platform$OS.type %in% c("windows")) {
    cl = parallel::makeCluster(cl)
    on.exit(parallel::stopCluster(cl))
  }
 
  pbapply::pboptions(type='none')
  full_t = pbapply::pbsapply(1:length(reg), cl=cl, FUN=function(i) {
    qlist = as.list(qlists[[i]])             ## get first chars. transform to list for use in data.table search
    subvoc = voc_index
    if (length(qlist) > 0) 
      subvoc = subvoc[qlist, nomatch=0]      ## first filter voc with binary search on first part
    subvoc = subvoc$voc[subvoc$n >= n[i]]    ## also ignore voc terms that are shorter than fixed part of regex
    if (length(subvoc) > 0)
      subvoc[stringi::stri_detect(subvoc, regex = reg[i])]
    else
      character()
  }, simplify = F)
  full_t[sapply(full_t, length) > 0]
  names(full_t) = reg
  full_t
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
