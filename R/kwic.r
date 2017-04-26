
keyword_in_context <- function(tc, hits=NULL, i=NULL, code='', nwords=10, nsample=NA, output_feature='word', context_level=c('document', 'sentence'), prettypaste=T, kw_tag=c('<','>')){
  if (class(i) == 'logical') i = which(i)
  ## first filter tokens on document id (to speed up computation)

  if (is.null(tc$provenance('index_feature'))){
    gi = tc$feature_index(feature=output_feature, context_level=context_level, max_window_size = nwords)$global_i
  } else {
    gi = tc$feature_index(feature=tc$provenance('index_feature'), context_level=context_level, max_window_size = nwords, as_ascii = tc$provenance('as_ascii'))$global_i
  }
  gi = data.table::fsort(gi)

  gfv = globalFeatureVector$new(tc$get(output_feature), gi)

  if (!is.null(hits)) {
    if(!is.featureHits(hits)) stop('hits must be a featureHits object (created with the $search_features() method')
    d = tc$get(c('doc_id', 'word_i'))
    d$i = 1:nrow(d)
    setkeyv(d, c('doc_id', 'word_i'))
    i = d[hits$hits[,c('doc_id', 'word_i')]]$i
    code = hits$hits$code
    hit_id = hits$hits$hit_id
  } else {
    if(length(code) == 1) {
      code = rep(code, length(i))
      hit_id = 1:length(i)
    } else {
      hit_id = match(code, unique(code))
    }
  }
  global_i = gi[i]

  if (length(code) == 0) return(NULL)

  if(!is.na(nsample)) {
    hit_id_samp = head(sample(unique(hit_id)), nsample)
    samp = hit_id %in% hit_id_samp
    hit_id = hit_id[samp]
    code = code[samp]
    i = i[samp]
    global_i = global_i[samp]
  }

  shifts = -nwords:nwords
  d = data.frame(global_i = rep(global_i, each=length(shifts)) + shifts,
                 hit_id = rep(hit_id, each=length(shifts)),
                 is_kw = rep(shifts == 0, length(global_i)))
  d = d[d$global_i > 0 & d$global_i <= max(gi),]

  ## kwic's of the same hit_id should be merged.
  d = d[order(d$hit_id, d$global_i, -d$is_kw),]
  d = d[!duplicated(d[,c('hit_id','global_i')]),]

  d$feature = gfv[d$global_i, ignore_empty = F]
  d$feature[d$is_kw] = sprintf('%s%s%s', kw_tag[1], d$feature[d$is_kw], kw_tag[2])

  ## add tag for gap between kwic of merged hit_ids that are not adjacent
  same_hit_id = d$hit_id == shift(d$hit_id, 1, fill = -1)
  not_adjacent = d$global_i - (shift(d$global_i, 1, fill=-1)) > 1
  gap = same_hit_id & not_adjacent
  d$feature[gap] = sprintf('[...] %s', d$feature[gap])

  d = d[!d$feature == '',]
  kwic = split(as.character(d$feature), d$hit_id)
  kwic = sapply(kwic, stringi::stri_flatten, collapse=' ')

  kwic = data.frame(hit_id = as.numeric(names(kwic)),
                    kwic = pretty_kwic(kwic))

  add = data.frame(hit_id = hit_id, doc_id = tc$get('doc_id')[i], code=code)
  add = add[!duplicated(add$hit_id),]
  feature = split(tc$get(output_feature)[i], hit_id)
  add$feature = sapply(feature, stringi::stri_flatten, collapse=' -> ')

  kwic = merge(kwic, add, by='hit_id', all.x=T)
  kwic[,c('doc_id','code','hit_id','feature','kwic')]
}

pretty_kwic <- function(x){
  x = gsub('_| ', ' ', x)
  x = gsub(" ([.,?!:;>)])", '\\1', x)
  x = gsub('([(<]) ', '\\1', x)
  sprintf('...%s...', x)
}
