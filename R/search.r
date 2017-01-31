### search strings

search_string <- function(tc, fi, string, allow_multiword=T, allow_proximity=T, allow_subset=F, only_last_mword=T){
  ## look up a string
  ## multiple strings can be used at once (in which case they are seen as related by OR statements)
  ## supports single word strings, multiword strings demarcated with quotes (e.g., "this string") and word proximities (e.g., "marco polo"~10)
  #string = 'test "dit eens"~2"'
  regex = get_feature_regex(string)
  is_multiword = grepl(' ', regex$term)
  is_proximity = !is.na(regex$window)
  #is_subset = grepl('subset=', regex$term)


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
    if(!allow_multiword) stop('Multiword queries ("word1 word2") not allowed here (allow_multiword == F)')
    hit_multi = multiword_grepl(fi, multi$regex, only_last=only_last_mword)
    hit_multi = fi[hit_multi]
  } else {
    hit_multi = NULL
  }

  if(nrow(proxi) > 0){
    if(!allow_proximity) stop('Proximity queries ("word1 word2"~5) not allowed here (allow_proximity == F)')
    hit_proxi = proximity_grepl(fi, proxi$regex, proxi$window, only_last=only_last_mword)
    hit_proxi = fi[hit_proxi]
  } else {
    hit_proxi = NULL
  }

  unique(plyr::rbind.fill(hit_single, hit_multi, hit_proxi))
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

  for(mword in strsplit(mwords, split=' ')){
    for(q in mword){
      if(q == mword[1]) {   ## if first word, search everything
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
    hits = union(hits, hit)
  }
  fi$global_i %in% hits
}

proximity_grepl <- function(fi, pwords, window, only_last=T, ignore.case=T, perl=F, useBytes=T){
  hits = rep(F, nrow(fi))
  if(window < 1) stop("window cannot be smaller than 1")

  for(q in strsplit(pwords, split=' ')){
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
    hits = union(hits, intersect(pword_i, pword_window))
  }
  fi$global_i %in% hits
}
