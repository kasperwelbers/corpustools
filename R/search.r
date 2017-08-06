search_string <- function(fi, string, unique_i=F){
  ## look up a string
  ## multiple strings can be used at once (in which case they are seen as related by OR statements)
  ## supports single token strings, multitoken strings demarcated with quotes (e.g., "this string") and token proximities (e.g., "marco polo"~10)
  ## This function does not manage complex boolean queries (AND, NOT, parentheses).
  ## If multiple strings are given, results are added together as if they were connected with OR statements

  ## if unique_i == TRUE, the same features cannot be counted in different strings, so queries are sorted from most to least specific (sequence > proximity > single, and more words is more specific)
  ## features that have already been assigned in a more specific query will not be used again

  #string = '"mark rutte" rutte "mark rutte aight"~2'

  regex = get_feature_regex(string)

  is_multitoken = grepl(' ', regex$term)
  single = regex[!is_multitoken,,drop=F]
  multi = regex[is_multitoken,,drop=F]

  all_hits = list()
  offset_id = 0
  used_i = c()

  ## multitoken queries
  if (nrow(multi) > 0) {
    multi$proximity = !is.na(multi$window)
    multi$length = sapply(strsplit(multi$regex, split=' '), length)
    multi = multi[order(-multi$length, multi$proximity),,drop=F]
    if (any(multi$window[multi$proximity] < 1)) stop("Proximity search window cannot be smaller than 1")

    for (i in 1:nrow(multi)) {
      if (multi$proximity[i]) {
        all_hits[[i]] = proximity_grepl(fi, multi[i,], offset_id = offset_id, used_i=used_i)
      } else {
        all_hits[[i]] = sequence_grepl(fi, multi[i,], offset_id = offset_id, used_i=used_i)
      }
      if (nrow(all_hits[[i]]) > 0) {
        offset_id = max(all_hits[[i]]$hit_id)
        if (unique_i) used_i = union(used_i, all_hits[[i]]$i)
      }
    }
  }

  ## single token queries (no loop required. single terms are combined for efficiency)
  if (nrow(single) > 0) {
    all_hits[[length(all_hits) + 1]] = single_grepl(fi, single, offset_id = offset_id, used_i=used_i)
  }

  ## bind all results
  unique(data.table::rbindlist(all_hits))
}

single_grepl <- function(fi, single, offset_id = 0, used_i = c()) {
  hit_single = c() # pun unintended
  if(sum(single$ignore_case) > 0) hit_single = union(hit_single, batch_grep(single$regex[single$ignore_case], levels(fi$feature)))
  if(sum(!single$ignore_case) > 0) hit_single = union(hit_single, batch_grep(single$regex[!single$ignore_case], levels(fi$feature), ignore.case = F))
  hits = fi[list(hit_single),,nomatch=0]
  if (length(used_i) > 0) hits = hits[!hits$i %in% used_i,]
  hits$hit_id = offset_id + 1:nrow(hits)
  hits
}

sequence_grepl <- function(fi, seq, perl=F, useBytes=T, offset_id = 0, used_i = c()){
  ## keywords with underscores are considered word sequence strings. These can occur both in one row of the tcorpus features, or multiple
  ## this function doesn't care, and captures both, by walking over the tokens and checking whether they occur in the same or subsequent (i.e. next global_i) position
  q = strsplit(seq$regex, split=' ')[[1]]
  ign_case = seq$ignore_case

  hit_list = list()
  for(j in 1:length(q)){
    hits = grep_fi(fi, q[j], ignore.case=ign_case, perl=perl, useBytes=useBytes)
    if (length(hits) > 0) hit_list[[j]] = hits
  }
  hits = data.table::rbindlist(hit_list)
  if (nrow(hits) > 0) hits$j = rep(1:length(hit_list), sapply(hit_list, nrow))
  if (length(used_i) > 0) hits = hits[!hits$i %in% used_i,]
  if (nrow(hits) == 0) return(data.table(feature=character(), i=integer(), global_i=integer(), hit_id=integer()))

  hits = hits[order(hits$global_i),]

  hits$hit_id = sequence_hit_ids(hits$global_i, hits$j, seq_length = length(q)) ## assign hit ids to valid sequences
  hits = hits[hits$hit_id > 0,]
  hits$hit_id = hits$hit_id + offset_id

  hits[,c('feature','i','global_i','hit_id')]
}

proximity_grepl <- function(fi, proxi, perl=F, useBytes=T, offset_id = 0, used_i = c()){
  q = strsplit(proxi$regex, split=' ')[[1]]
  ign_case = proxi$ignore_case
  window = proxi$window

  hit_list = list()
  for(j in 1:length(q)){
    hits = grep_fi(fi, q[j], ignore.case=ign_case, perl=perl, useBytes=useBytes)
    if (length(hits) > 0) hit_list[[j]] = hits
  }

  hits = data.table::rbindlist(hit_list)
  if (nrow(hits) > 0) hits$j = rep(1:length(hit_list), sapply(hit_list, nrow))
  if (length(used_i) > 0) hits = hits[!hits$i %in% used_i,]
  if (nrow(hits) == 0) return(data.table(feature=character(), i=integer(), global_i=integer(), hit_id=integer()))
  hits = hits[order(hits$global_i),]

  hits$hit_id = proximity_hit_ids(hits$global_i, hits$j, n_unique = length(q), window=window) ## assign hit_ids to groups of tokens within the given window
  hits = hits[hits$hit_id > 0,] ## proximity_hit_ids returns 0 for unassigned
  hits$hit_id = hits$hit_id + offset_id

  hits[,c('feature','i','global_i','hit_id')]
}

sequence_hit_ids <- function(global_i, j, seq_length){
  .Call('corpustools_sequence_hit_ids', PACKAGE = 'corpustools', global_i, j, seq_length)
}

proximity_hit_ids <- function(global_i, j, n_unique, window){
  .Call('corpustools_proximity_hit_ids', PACKAGE = 'corpustools', global_i, j, n_unique, window)
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

expand_window <- function(i, window) {
  unique(rep(i, window*2 + 1) + rep(-window:window, each=length(i)))
}

overlapping_windows <- function(hit_list, window=window) {
  Reduce(intersect, sapply(hit_list, expand_window, window=window, simplify = F))
}



