REGEX_ALLOW_SYMBOLS = '([\\[\\+*?\\.a-z0-9%@$€:;#/~_-]+)'

## add case sensitive search
## look for alternative to as_ascii with iconv? Or just allow as_ascii in search for latin languages? (it's nice to not care about accents)
## make a test function that creates a tCorpus with every possible character as a 1 char word, and then search this corpus with queries for each character to see whether it hits


parse_queries <- function(query){
  #query = iconv(query, to='ASCII//TRANSLIT') # remove accented characters
  query = gsub(' +', ' ', query)

  query = gsub(' OR ', ' | ', query)
  query = gsub(' AND ', ' & ', query)
  query = gsub(' NOT ', ' &! ', query)

  ## also allow empty space as OR
  query = gsub('(?<=[+*?".a-zA-Z0-9/~_)-])[ ]+(?=[+*?".a-zA-Z0-9/~_(-])', ' | ', query, perl=T)

  ## parts of the string between quotes are treated as single query terms
  ## if within quotations, spaces stay spaces. Except within parentheses within quotes, spaces are again OR statements
  ## if ~[0-9] after quotes (used to indicate word proximities) take these along as well
  quotes = regmatches(query, gregexpr('(\").*?(\"(~[0-9]+)?)', query, perl = T))[[1]]
  for(m in quotes) {
    #if (grepl('& ', m)) stop('Queries cannot contain &/AND statements within quotes')
    #m = gsub('&', '\\&', m)

    replacewith = sprintf('{%s}', m) ## surround with {} to keep entire string as single term
    replacewith = gsub(' | ', ' ', replacewith, fixed=T)
    replacewith = gsub('&', ' & ', replacewith)
    query = gsub(m, replacewith, query, fixed=T)

    parmatch = regexpr('\\(.*\\)', query)
    for(m in regmatches(query, parmatch)) {
      query = gsub(m, gsub(' ', '|', m, fixed=T), query, fixed=T)
    }
  }

  ## make " * ", as a 'find all' solution, an immediate TRUE
  query = tolower(query) # safety first: for the odd possibility that someone uses T or F as a query term, which would be interpreted as TRUE or FALSE
  query = gsub('(?<= )\\*(?= )|(?<=^)\\*(?= )', 'T', query, perl=T)
  query = gsub(' +', ' ', query)

  split_regex = paste('(?<={).*?(?=})', REGEX_ALLOW_SYMBOLS, sep='|') ##

  query_form = as.list(gsub(split_regex, '%s', query, perl=T)) # note that uppercase is not replaced, to keep the TRUE
  query_terms = regmatches(query, gregexpr(split_regex, query, perl=T))

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
                     window = ifelse(grepl('~', terms) == T, gsub('.*~', '', terms), default_window))
  terms$window[terms$window == 'd'] = NA
  terms$window = as.numeric(as.character(terms$window))

  terms$regex = gsub('([.+])', '\\\\\\1', terms$regex) ## escape special regex characters

  terms$regex = gsub('*', '.*', terms$regex, fixed=T) # wildcard: none or any symbols
  terms$regex = gsub('?', '.?', terms$regex, fixed=T) # wildcard: one character that can be anything
  terms$regex = gsub('"', '', terms$regex, fixed=T) # remove quotes
  terms$regex = gsub(REGEX_ALLOW_SYMBOLS, '\\\\b\\1\\\\b', terms$regex) ## add word boundaries
  unique(terms)
}



qualify_queries <- function(queries){
  boo = c()
  for(i in 1:nrow(queries)){
    if (queries$keyword[i] == '') boo = c(boo, sprintf('Code "%s": no keyword', queries$code[i]))
    if (queries$keyword[i] == '*') boo = c(boo, sprintf('Code "%s": keyword cannot be *', queries$code[i]))
  }
  if (length(boo) > 0) stop(paste(boo, collapse='\n'))
}


