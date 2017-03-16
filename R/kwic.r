
keyword_in_context <- function(tc, hits=NULL, i=NULL, code='', nwords=10, nsample=NA, output_feature='word', context_level=c('document', 'sentence'), prettypaste=T, kw_tag=c('<','>')){
  if (class(i) == 'logical') i = which(i)
  ## first filter tokens on document id (to speed up computation)

  gi = get_global_i(tc, context_level=context_level, max_window_size = nwords)
  gfv = globalFeatureVector$new(tc$data(output_feature), gi)

  if (!is.null(hits)) {
    if(!is.featureHits(hits)) stop('hits must be a featureHits object (created with the $search_features() method')
    d = tc$data(c('doc_id', 'word_i'))
    d$i = 1:nrow(d)
    setkeyv(d, c('doc_id', 'word_i'))
    i = d[hits$hits[,c('doc_id', 'word_i')]]$i
    code = hits$hits$code
  }
  global_i = gi[i]
  if(length(code) == 1) code = rep(code, length(i))

  if(!is.na(nsample)) {
    samp = unlist(tapply(1:length(code), code, function(x) head(sample(x), nsample)))
    code = code[samp]
    i = i[samp]
    global_i = global_i[samp]
  }

  shifts = -nwords:nwords
  d = data.frame(global_i = rep(global_i, each=length(shifts)) + shifts,
                 kwic_i = rep(1:length(global_i), each=length(shifts)),
                 is_kw = rep(shifts == 0, length(global_i)))
  d = d[d$global_i > 0 & d$global_i <= max(gi),]
  d$feature = gfv[d$global_i, ignore_empty = F]
  d$feature[d$is_kw] = sprintf('%s%s%s', kw_tag[1], d$feature[d$is_kw], kw_tag[2])

  kwic = split(as.character(d$feature), d$kwic_i)
  kwic = sapply(kwic, stringi:::stri_flatten, collapse=' ')

  data.frame(doc_id = tc$data('doc_id')[i],
             i=i,
             code = code,
             feature = tc$data(output_feature)[i],
             kwic = pretty_kwic(kwic))
}

pretty_kwic <- function(x){
  x = gsub('_| ', ' ', x)
  x = gsub(" ([.,?!:;>)])", '\\1', x)
  x = gsub('([(<]) ', '\\1', x)
  x = sprintf('...%s...', x)
}
