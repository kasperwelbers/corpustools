parse_queries <- function(query){
  query = iconv(query, to='ASCII//TRANSLIT') # if present, try to remove accented characters

  query = gsub(' OR ', ' | ', query)
  query = gsub(' AND ', ' & ', query)
  query = gsub(' NOT ', ' &! ', query)

  ## if within quotations, replace spaces with underscores (to prepare for multi-word string matching)
  rematch = regexpr('".*"', query)
  for(m in regmatches(query, rematch)) {
    replacewith = gsub('"', '', gsub(' ', '_', m, fixed=T), fixed=T) # replace space with underscore and remove quotes
    query = gsub(m, replacewith, query, fixed=T)
  }

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


### search strings

search_string <- function(tc, fi, string, allow_multiword=T, allow_proximity=T, only_last_mword=T){
  ## look up a string
  ## multiple strings can be used at once (in which case they are seen as related by OR statements)
  ## supports single word strings, multiword strings demarcated with quotes (e.g., "this string") and word proximities (e.g., "marco polo"~10)
  #string = 'test "dit eens"~2"'
  regex = get_feature_regex(string)
  is_multiword = if(allow_multiword) grepl('_', regex$term) else rep(F, nrow(regex))
  is_proximity = if(allow_proximity) !is.na(regex$window) else rep(F, nrow(regex))

  single = regex[!is_multiword,,drop=F]
  multi = regex[is_multiword & !is_proximity,,drop=F]
  proxi = regex[is_multiword & is_proximity,,drop=F]

  if(nrow(single) > 0){
    hit_single = batch_grep(single$regex, levels(fi$feature)) # pun intended
    hit_single = fi[J(hit_single)]
  } else {
    hit_single = NULL
  }

  if(nrow(multi) > 0){
    hit_multi = multiword_grepl(fi, multi$regex, only_last=only_last_mword)
    hit_multi = fi[hit_multi]
  } else {
    hit_multi = NULL
  }

  if(nrow(proxi) > 0){
    hit_proxi = proximity_grepl(fi, proxi$regex, proxi$window, only_last=only_last_mword)
    hit_proxi = fi[hit_proxi]
  } else {
    hit_proxi = NULL
  }

  unique(rbind.fill(hit_single, hit_multi, hit_proxi))
}


batch_grep <- function(patterns, x, ignore.case=T, perl=F, batchsize=25, useBytes=T){
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

multiword_grepl <- function(fi, mwords, only_last=T, ignore.case=T, perl=F, useBytes=T){
  ## keywords with underscores are considered multiword strings. These can occur both in one row of the tcorpus features, or multiple
  ## this function doesn't care, and captures both, by walking over the words and checking whether they occur in the same or subsequent (i.e. next global_i) position
  hits = c()

  mwords = gsub('\\b', '', mwords, fixed=T)
  for(mword in strsplit(mwords, split='_')){
    for(word in mword){
      q = sprintf('\\b%s\\b', word)
      if(word == mword[1]) {   ## if first word, search everything
        hit = fi$global_i[grepl(q, fi$feature, ignore.case=ignore.case, perl=perl, useBytes=useBytes)]
        if(!only_last) firsthit = hit ## keep in case only_last is FALSE
      } else { ## if not first word
        ## search whether the word occurs in the same or next position as the previous word
        same_or_next = c(hit, hit + 1)
        fi_next = fi[fi$global_i %in% same_or_next,]
        hit = fi_next$global_i[grepl(q, fi_next$feature, ignore.case=ignore.case, perl=perl, useBytes=useBytes)]
      }
      if(length(hit) == 0) break
    }

    if(!only_last & length(hit) > 0){
      possible_positions_start = rep(firsthit, length(mword)) + (rep(1:length(mword), each=length(firsthit)) - 1)
      possible_positions_found = rep(hit, length(mword)) - (rep(1:length(mword), each=length(hit)) - 1)
      hit = intersect(possible_positions_start, possible_positions_found)
    }
    hits = c(hits, hit)
  }
  fi$global_i %in% hits
}

proximity_grepl <- function(fi, pwords, window, only_last=T, ignore.case=T, perl=F, useBytes=T){
  hits = rep(F, nrow(fi))
  if(window < 1) stop("window cannot be smaller than 1")
  pwords = gsub('\\b', '', pwords, fixed=T)
  pword = strsplit(pwords, '_')[[1]]
  for(pword in strsplit(pwords, split='_')){
    q = sprintf('\\b%s\\b', pword)
    pword_i = c()                       # is this specific global i one of the pwords
    pword_window = 1:max(fi$global_i)   # for which global i's do all pwords occur within the given window
    for(j in 1:length(q)){
      i = fi$global_i[grepl(q[j], fi$feature, ignore.case=ignore.case, perl=perl, useBytes=useBytes)]
      if(only_last){
        if(j == length(q)) pword_i = pword_i = union(pword_i, i) # only remember position of last word
      } else {
        pword_i = pword_i = union(pword_i, i)
      }
      i_window = rep(i, window*2 + 1) + rep(-window:window, each=length(i)) ## add window
      pword_window = intersect(pword_window, i_window)
    }
    hits = c(hits, intersect(pword_i, pword_window))
  }
  fi$global_i %in% hits
}
