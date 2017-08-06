### search strings

search_string <- function(fi, string, allow_multitoken=T, allow_proximity=T){
  ## look up a string
  ## multiple strings can be used at once (in which case they are seen as related by OR statements)
  ## supports single token strings, multitoken strings demarcated with quotes (e.g., "this string") and token proximities (e.g., "marco polo"~10)
  ## This function does not manage complex boolean queries (AND, NOT, parentheses).
  ## If multiple strings are given, results are added together as if they were connected with OR statements

  regex = get_feature_regex(string)
  is_multitoken = grepl(' ', regex$term)
  is_proximity = !is.na(regex$window)

  single = regex[!is_multitoken & !is_proximity,,drop=F]
  multi = regex[is_multitoken & !is_proximity,,drop=F]
  proxi = regex[is_multitoken & is_proximity,,drop=F]

  if (nrow(single) > 0){
    hit_single = c() # pun intended
    if(sum(single$ignore_case) > 0) hit_single = union(hit_single, batch_grep(single$regex[single$ignore_case], levels(fi$feature)))
    if(sum(!single$ignore_case) > 0) hit_single = union(hit_single, batch_grep(single$regex[!single$ignore_case], levels(fi$feature), ignore.case = F))
    hit_single = fi[list(hit_single),,nomatch=0]
    hit_single$hit_id = stringi::stri_paste('s', 1:nrow(hit_single), sep='#')
  } else {
    hit_single = NULL
  }

  if (nrow(multi) > 0){
    if (!allow_multitoken) stop('Multitoken queries ("token1 token2") not allowed here (allow_multitoken == F)')
    hit_multi = multitoken_grepl(fi, multi)
    hit_multi$hit_id = stringi::stri_paste('m', hit_multi$hit_id, sep='#')
  } else {
    hit_multi = NULL
  }

  if (nrow(proxi) > 0){
    if (!allow_proximity) stop('Proximity queries ("token1 token2"~5) not allowed here (allow_proximity == F)')
    hit_proxi = proximity_grepl(fi, proxi)
    hit_proxi$hit_id = stringi::stri_paste('p', hit_proxi$hit_id, sep='#')
  } else {
    hit_proxi = NULL
  }

  # feature  i global_i hit_id
  unique(data.table::rbindlist(list(hit_single, hit_multi, hit_proxi)))
}


batch_grep <- function(patterns, x, ignore.case=T, perl=F, batchsize=25, useBytes=T){
  ## make batches of terms and turn each batch into a single regex
  patterns = split(patterns, ceiling(seq_along(patterns)/batchsize))
  patterns = sapply(patterns, stringi::stri_paste, collapse='|')

  out = rep(F, length(x))
  for(pattern in patterns){
    out = out | grepl(pattern, x, ignore.case=ignore.case, perl=perl, useBytes=useBytes)
  }
  x[out]
}

grep_global_i <- function(fi, regex, ...) {
  exact_feature = levels(fi$feature)[grepl(regex, levels(fi$feature), ...)]
  fi[list(exact_feature),,nomatch=0]$global_i
}

grep_fi <- function(fi, regex, ...) {
  exact_feature = levels(fi$feature)[grepl(regex, levels(fi$feature), ...)]
  i = fi[list(exact_feature),,nomatch=0]

}

multitoken_grepl <- function(fi, multi, ignore.case=T, perl=F, useBytes=T){
  ## keywords with underscores are considered multitoken strings. These can occur both in one row of the tcorpus features, or multiple
  ## this function doesn't care, and captures both, by walking over the tokens and checking whether they occur in the same or subsequent (i.e. next global_i) position
  hits = list()
  hit_id = 1

  mtoken_regex = strsplit(multi$regex, split=' ')
  for(i in 1:length(mtoken_regex)){
    q = mtoken_regex[[i]]
    ign_case = multi$ignore_case[i]

    hit_list = list()
    for(j in 1:length(q)){
      hit = grep_fi(fi, q[j], ignore.case=ign_case, perl=perl, useBytes=useBytes)
      if (length(hit) > 0) hit_list[[j]] = hit
    }

    hit = data.table::rbindlist(hit_list)
    hit$j = rep(1:length(hit_list), sapply(hit_list, nrow))
    hit = hit[order(hit$global_i),]
    hit$hit_id = sequence_hit_ids(hit$global_i, hit$j) ## assign hit ids to valid sequences
    hit = hit[hit$hit_id > 0,]

    hits[['']] = hit
  }
  rbindlist(hits)
}

sequence_hit_ids <- function(global_i, j){
  .Call('corpustools_sequence_hit_ids', PACKAGE = 'corpustools', global_i, j, length(unique(j)))
}

expand_window <- function(i, window) {
  unique(rep(i, window*2 + 1) + rep(-window:window, each=length(i)))
}

overlapping_windows <- function(hit_list, window=window) {
  Reduce(intersect, sapply(hit_list, expand_window, window=window, simplify = F))
}

proximity_grepl <- function(fi, proxi, ignore.case=T, perl=F, useBytes=T){
  hits = list()
  if (any(proxi$window < 1)) stop("window cannot be smaller than 1")

  ptoken_regex = strsplit(proxi$regex, split=' ')
  for(i in 1:length(ptoken_regex)){
    q = ptoken_regex[[i]]
    ign_case = proxi$ignore_case[i]

    window = proxi$window[i]
    hit_list = list()
    for(j in 1:length(q)){
      hit = grep_fi(fi, q[j], ignore.case=ign_case, perl=perl, useBytes=useBytes)
      if (length(hit) > 0) hit_list[[j]] = hit
    }
    if (length(hit_list) == 0) next

    hit = data.table::rbindlist(hit_list)
    hit$j = rep(1:length(hit_list), sapply(hit_list, nrow))
    hit = hit[order(hit$global_i),]
    hit$hit_id = proximity_hit_ids(hit$global_i, hit$j, window=window) ## assign hit_ids to groups of tokens within the given window
    hit = hit[hit$hit_id > 0,]

    hits[['']] = hit
  }
  rbindlist(hits)
}

proximity_hit_ids <- function(global_i, j, window){
  .Call('corpustools_proximity_hit_ids', PACKAGE = 'corpustools', global_i, j, length(unique(j)), window)
}

