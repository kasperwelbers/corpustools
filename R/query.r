parse_queries <- function(query){
  query = iconv(query, to='ASCII//TRANSLIT') # if present, try to remove accented characters

  query = gsub(' OR ', ' | ', query)
  query = gsub(' AND ', ' & ', query)
  query = gsub(' NOT ', ' &! ', query)

  ## also allow empty space as OR
  query = gsub('(?<=[+*?.a-zA-Z0-9/~_)-])[ ]+(?=[+*?.a-zA-Z0-9/~_(-])', ' | ', query, perl=T)

  ## make " * ", as a 'find all' solution, an immediate TRUE
  query = tolower(query) # safety first: for the odd possibility that someone uses T or F as a query term, which would be interpreted as TRUE or FALSE
  query = gsub('(?<= )\\*(?= )|(?<=^)\\*(?= )', 'T', query, perl=T)

  query_form = as.list(gsub('([+*?.a-z0-9/~_-]+)', '%s', query)) # note that uppercase is not replaced, to keep the TRUE
  query_terms = regmatches(query, gregexpr('([+*?.a-z0-9/~_-]+)', query))

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

  if(length(default_window) == nrow(terms)){
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
  terms$regex = gsub('?', '.{1}', terms$regex, fixed=T) # wildcard: one character that can be anything
  terms$regex = sprintf('\\b%s\\b', terms$regex)
  unique(terms)
}

qualify_queries <- function(queries){
  boo = c()
  for(i in 1:nrow(queries)){
    if(queries$keyword[i] == '') boo = c(boo, sprintf('Code "%s": no keyword', queries$code[i]))
    if(queries$keyword[i] == '*') boo = c(boo, sprintf('Code "%s": keyword cannot be *', queries$code[i]))
  }
  if(length(boo) > 0) stop(paste(boo, collapse='\n'))
}

batch_grepl <- function(patterns, x, ignore.case=T, perl=F, batchsize=25, useBytes=T){
  ## make batches of terms and turn each batch into a single regex
  patterns = split(patterns, ceiling(seq_along(patterns)/batchsize))
  patterns = sapply(patterns, paste, collapse='|')

  ## grepl in unique x
  out = rep(F, length(x))
  for(pattern in patterns){
    out = out | grepl(pattern, x, ignore.case=ignore.case, perl=perl, useBytes=useBytes)
  }
  x[out]
}
