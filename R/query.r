FORM_SYMBOLS = '()!&| {}"'
FORM_REGEX = sprintf('([^%s]+)', FORM_SYMBOLS)

## look for alternative to as_ascii with iconv? Or just allow as_ascii in search for latin languages? (it's nice to not care about accents)
## make a test function that creates a tCorpus with every possible character as a 1 char word, and then search this corpus with queries for each character to see whether it hits
## perhaps better to use switch to a (shorter) list of REGEX symbols that is not allowed (i.e. the symbols used in the R logical expression).

parse_queries <- function(query){
  query = iconv(query, to='ASCII//TRANSLIT') # remove accented characters

  query = gsub('\\{|\\}', '', query) ## drop curly brackets, which are used here for escaping

  escape_special = '(\\&\\!)' ## Escape the special characters that R uses for logical expressions
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
  if(any(grepl('"~s', query, fixed=T))) stop('Case sensitive flag (~i) can only be used for words, not for words beween quotes')

  ## also allow empty space as OR
  query = gsub('(?<=[+*?".a-zA-Z0-9/~_)-])[ ]+(?=[+*?".a-zA-Z0-9/~_(-])', ' | ', query, perl=T)

  ## parts of the string between quotes are treated as single query terms
  ## if within quotations, spaces stay spaces. Except within parentheses within quotes, spaces are again OR statements
  ## if ~[0-9] after quotes (used to indicate word proximities) take these along as well
  ## To escape parts within quotes, we use the double curly brackets {}.
  quotes = regmatches(query, gregexpr('(\").*?(\"(~[0-9]+)?)', query, perl = T))[[1]]
  for(m in quotes) {
    not_bracketed = gsub('(?<={).*?(?=})', '', m, perl=T)
    if (grepl('&', not_bracketed)) stop('Queries cannot contain AND statements within quotes')
    if (grepl('!', not_bracketed)) stop('Queries cannot contain NOT statements within quotes')

    replacewith = gsub('\\{|\\}', '', m) ## drop curly brackets of already escaped strings, since they will already be escaped by being nested in quotes
    replacewith = sprintf('{%s}', replacewith) ## surround with {} to keep entire string as single term
    replacewith = gsub(' | ', ' ', replacewith, fixed=T)
    query = gsub(m, replacewith, query, fixed=T)

    parmatch = regexpr('\\(.*\\)', query)
    for(m in regmatches(query, parmatch)) {
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
  terms = data.frame(term = terms,
                     regex = gsub('~.*', '', terms),
                     window = ifelse(grepl('~[s]*[0-9][s]*', terms) == T, gsub('.*~[s]*([0-9]*).*', '\\1', terms), default_window),
                     ignore_case = ifelse(grepl('~[0-9]*[s][0-9]*', terms) == T, F, T)) ## if a case sensitive flag occurs (~s) then do not ignore case. Note that the case_sensitive_flags function takes care of case insensitive words in multiword strings
  terms$window = as.numeric(as.character(terms$window))

  terms$regex = gsub('([.+])', '\\\\\\1', terms$regex) ## escape special regex characters
  terms$regex = gsub('*', '.*', terms$regex, fixed=T) # wildcard: anything, or nothing
  terms$regex = gsub('?', '.?', terms$regex, fixed=T) # wildcard: one character that can be anything, or nothing
  terms$regex = gsub('"', '', terms$regex, fixed=T) # remove quotes
  terms$regex = gsub('\\{|\\}', '', terms$regex)

  terms$regex = gsub(FORM_REGEX, '\\\\b\\1\\\\b', terms$regex) ## add word boundaries
  terms$regex = ifelse(terms$ignore_case, terms$regex, case_sensitive_flags(terms$term, terms$regex))
  unique(terms)
}

case_sensitive_flags <- function(term, regex){
  r = stringi::stri_split(regex, regex=' ')
  sapply(r, function(r){
    case_flag = grepl('~[0-9]*s', term)
    r[!case_flag] = sprintf('(?i)%s(?-i)', r[!case_flag])
    paste(r, collapse=' ')
  })
}

qualify_queries <- function(queries){
  boo = c()
  for(i in 1:nrow(queries)){
    if (queries$keyword[i] == '') boo = c(boo, sprintf('Code "%s": no keyword', queries$code[i]))
    if (queries$keyword[i] == '*') boo = c(boo, sprintf('Code "%s": keyword cannot be *', queries$code[i]))
  }
  if (length(boo) > 0) stop(paste(boo, collapse='\n'))
}

