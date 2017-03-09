FORM_SYMBOLS = '!&()| {}'
FORM_REGEX = sprintf('([^%s]+)', FORM_SYMBOLS)


## look for alternative to as_ascii with iconv? Or just allow as_ascii in search for latin languages? (it's nice to not care about accents)
## make a test function that creates a tCorpus with every possible character as a 1 char word, and then search this corpus with queries for each character to see whether it hits
## perhaps better to use switch to a (shorter) list of REGEX symbols that is not allowed (i.e. the symbols used in the R logical expression).

parse_queries <- function(query){
  queries = plyr::llply(query, parse_query)
  do.call(rbind, queries)
}

parse_query <- function(query){
  query = iconv(query, to='ASCII//TRANSLIT') # remove accented characters

  query = gsub('\\{|\\}', '', query) ## drop curly brackets, which are used here for escaping

  escape_special = '\\&\\!' ## Escape the special characters that R uses for logical expressions
  query = gsub(sprintf('([^%s]*[%s][^%s]*)', FORM_SYMBOLS, escape_special, FORM_SYMBOLS),
               '{\\1}', query)

  query = gsub('\\bOR\\b', ' | ', query, perl = T)
  query = gsub('\\bNOT\\b', ' &! ', query, perl= T)
  query = gsub('\\bAND\\b', ' & ', query, perl = T)
  query = gsub(' +', ' ', query)

  ## checks
  if(any(grepl('\\&[ ]*\\&', query))) stop('Query has repeating AND statemenets (... AND AND ...)')
  if(any(grepl('\\![ ]*\\!', query))) stop('Query has repeating NOT statemenets (... NOT NOT ...)')
  if(any(grepl('^[ ]*\\&|\\([ ]*\\&', query))) stop('AND cannot be the first term in a query (or within parentheses)')
  if(any(grepl('^[ ]*\\!|\\([ ]*\\!', query))) stop('NOT cannot be the first term in a query (or within parentheses) because it means AND NOT. If you want to find everything except what comes after NOT, this can be done with "* NOT ..."')

  if(any(grepl('\\^[^0-9]\\b', query, perl=T))) stop('Invalid flag after ^ (caret). This can only be a number (see search_features query details for conditions)')
  if(any(grepl('\\~[^s0-9]\\b', query, perl=T))) stop('Invalid flag after ~ (tilde). This can only be a number (for multiword proximity) or ~s for (case sensitive)')

  ## also allow empty space as OR
  query = gsub('(?<=[+*?".a-zA-Z0-9/~_)-])[ ]+(?=[+*?".a-zA-Z0-9/~_(-])', ' | ', query, perl=T)

  ## parts of the string between quotes are treated as single query terms
  ## if within quotations, spaces stay spaces. Except within parentheses within quotes, spaces are again OR statements
  ## take the flag symbols (~ < > ^) and values ([0-9s])after quotes along as well.
  ## To escape parts within quotes, we use the double curly brackets {}.
  quotes = regmatches(query, gregexpr('(\").*?(\"([<>^~][<>~^0-9s]+)?)', query, perl = T))[[1]]
  for(m in quotes) {
    not_bracketed = gsub('(?<={).*?(?=})', '', m, perl=T)
    if (grepl('&', not_bracketed)) stop('Queries cannot contain AND statements within quotes')
    if (grepl('!', not_bracketed)) stop('Queries cannot contain NOT statements within quotes')

    replacewith = gsub('\\{|\\}', '', m) ## drop curly brackets of already escaped strings, since they will already be escaped by being nested in quotes
    replacewith = sprintf('{%s}', replacewith) ## surround with {} to keep entire string as single term
    replacewith = gsub(' | ', ' ', replacewith, fixed=T)
    query = gsub(m, replacewith, query, fixed=T)

    parmatch = regexpr('\\(.*\\)', m)
    for(m in regmatches(m, parmatch)) {

      query = gsub(m, gsub(' +', '|', m), query, fixed=T)
    }
  }


  query = gsub('(?<= )\\*(?= )|(?<=^)\\*(?= )', '!!', query, perl=T)   ## make " * ", as a 'find all' solution, an immediate TRUE. As a placeholder, we use !! (not not, which is technically true, right?)
  query = gsub(' +', ' ', query)

  split_regex = paste('(?<={).*?(?=})', FORM_REGEX, sep='|') ## escape curly brackets and select by allowed regex
  query_form = as.list(gsub(split_regex, '%s', query, perl=T)) # note that uppercase is not replaced, to keep the TRUE
  query_terms = regmatches(query, gregexpr(split_regex, query, perl=T))

  for (i in seq_along(query_form)) query_form[[i]] = gsub('!!', T, query_form[[i]]) ## replace the "find all" placeholder

  query_form[query_form == ''] = NA
  t(mapply(function(x,y) list(form=x, terms=y), query_form, query_terms))
}

fill_query <- function(query_values, query_form){
  do.call(sprintf, as.list(c(query_form, query_values)))
}

eval_query <- function(query_values, query_form){
  eval(parse(text=fill_query(query_values, query_form)))
}

eval_query_matrix <- function(qm, terms, form){
  ## only evaluate unique rows of the query matrix, and then match to return the results for each row
  combination = apply(qm[,terms,drop=F], 1, function(x) paste(as.numeric(x), collapse=''))

  isunique = !duplicated(combination)
  ucombination = combination[isunique]
  uqm = qm[isunique,,drop=F]

  res = apply(uqm[,terms, drop=F], 1, eval_query, query_form=form)
  res[match(combination, ucombination)]
}

get_feature_regex <- function(terms, default_window=NA){
  terms = parse_queries(terms)

  if (length(default_window) == nrow(terms)){
    reptimes = sapply(terms[,2], length)
    default_window = rep(default_window, reptimes)
  }

  terms = unlist(terms[,2])

  esc = get_escaped_table(terms)
  if(!is.null(esc)) for(i in 1:nrow(esc)) terms = gsub(esc$esc[i], esc$id[i], terms, fixed = T)

  terms = data.frame(term = terms,
                     regex = gsub('[<>^~][0-9s]*', '', terms),
                     window = ifelse(grepl('~[s]*[0-9][s]*', terms) == T, gsub('.*~[s]*([0-9]*).*', '\\1', terms), default_window),
                     direction = get_direction(terms),
                     condition_window = ifelse(grepl('[<>^][0-9]', terms) == T, gsub('.*[<>^]([0-9]*).*', '\\1', terms), default_window),
                     ignore_case = ifelse(grepl('~[0-9]*[s][0-9]*', terms) == T, F, T)) ## if a case sensitive flag occurs (~s) then do not ignore case. Note that the case_insensitive_flags function takes care of case insensitive words in multiword strings

  terms$window = as.numeric(as.character(terms$window))
  terms$condition_window = as.numeric(as.character(terms$condition_window))

  terms$regex = gsub('([.+])', '\\\\\\1', terms$regex) ## escape special regex characters
  terms$regex = gsub('*', '.*', terms$regex, fixed=T) # wildcard: anything, or nothing
  terms$regex = gsub('?', '.?', terms$regex, fixed=T) # wildcard: one character that can be anything, or nothing
  terms$regex = gsub('"', '', terms$regex, fixed=T) # remove quotes
  terms$regex = gsub('\\{|\\}', '', terms$regex)

  #FORM_REGEX

  #print(terms)
  terms$regex = gsub(FORM_REGEX, '\\\\b\\1\\\\b', terms$regex) ## add word boundaries
  terms$regex = ifelse(terms$ignore_case, terms$regex, case_insensitive_flags(terms$term, terms$regex))
  if(!is.null(esc)) for(i in 1:nrow(esc)) terms$regex = gsub(esc$id[i], esc$esc[i], terms$regex, fixed = T)

  unique(terms)
}

get_escaped_table <- function(x){
  if(length(x) == 0) return(NULL)
  esc = stringi::stri_extract_all_regex(x, '\\\\.')
  if (any(is.na(esc)) | any(is.null(esc))) return(NULL)
  esc = unique(unlist(esc))
  data.frame(id=paste0('\\',1:length(esc)), esc=esc)
}

get_direction <- function(term){
  if(length(term) == 0) return(NULL)
  direction = '<>'
  left = grepl('<[0-9]', term)
  right = grepl('>[0-9]', term)
  direction[left &! right] = '<'
  direction[!left & right] = '>'
  direction
}

case_insensitive_flags <- function(term, regex){
  ## we cannot force case sensitive search within a case insensitive regex. However, we can use the (?i) regex flag to force case insensitive in a case sensitive search.
  ## so for terms that contains at least one s flag, we use case sensitive search, and for individual words for which the flag is not set we use the (?i) regex flag
  for (i in 1:length(term)){
    if (grepl('\\"[0-9^]*~[0-9]*s', term[[i]])) {
      next ## if the s flag is set around the multiword string "Like This"~s, then all words are case sensitive
    } else {
      t = regmatches(term[[i]], gregexpr(FORM_REGEX, term[i]))[[1]]
      r = regmatches(regex[[i]], gregexpr(FORM_REGEX, regex[i]))[[1]]
      s_flag = grepl('~[0-9]*s', t)
      for (sreg in r[!s_flag]) {
        regex[i] = gsub(sreg, sprintf('(?i)%s(?-i)', sreg), regex[i], fixed = T)
      }
    }
  }
  #print(regex)
  regex
}

qualify_queries <- function(queries){
  boo = c()
  for(i in 1:nrow(queries)){
    if (queries$keyword[i] == '') boo = c(boo, sprintf('Code "%s": no keyword', queries$code[i]))
    if (queries$keyword[i] == '*') boo = c(boo, sprintf('Code "%s": keyword cannot be *', queries$code[i]))
  }
  if (length(boo) > 0) stop(paste(boo, collapse='\n'))
}
