to_POSIXct <- function(x){
  tryCatch(as.POSIXct(x),
           warning = function(w) stop(sprintf('Date column cannot be properly interpreted as POSIXct: \n%s', w)),
           error = function(e) stop(sprintf('Date column cannot be interpreted as POSIXct: \n\t-> %s', e)))
}

compare_documents_fun <- function(tc, feature='word', date_col=NULL, hour_window=c(-24,24), measure=c('cosine','overlap_pct'), min_similarity=0, weight=c('termfreq','docfreq','tfidf','norm_tfidf'), ngrams=NA, from_subset=NULL, to_subset=NULL) {
  from_subset = if (is(substitute(from_subset), 'call')) deparse(substitute(from_subset)) else from_subset
  to_subset = if (is(substitute(to_subset), 'call')) deparse(substitute(to_subset)) else to_subset

  measure = match.arg(measure)
  if (!is.null(date_col)) date_col = match.arg(date_col, choices = tc$meta_names)

  meta = tc$meta(keep_df = T, as.df = T)

  ## construct dtm's based on subsets, but not if date_col and hour_window are given, because then this needs to be done differently for the newsflow.compare function
  if (is.null(date_col) | is.null(hour_window)){
    dtm = tc$dtm(feature=feature, weight = weight, subset_meta=from_subset, drop_empty_terms = F, context_labels = T, feature_labels=F, ngrams=ngrams, form = 'tm_dtm')
    if (is.null(from_subset) & is.null(to_subset)) {
      dtm.y = NULL
    } else {
      dtm.y = tc$dtm(feature=feature, weight = weight, subset_meta = to_subset, drop_empty_terms = F, context_labels = T, feature_labels=F, ngrams=ngrams, form = 'tm_dtm')
    }
  } else {
    ## if the compare.newsflow function is used, the full DTM is given, and the only_from and only_to arguments are used
    only_from = if (!is.null(from_subset)) tc$subset(subset_meta = from_subset)$meta('doc_id') else NULL
    only_to = if (!is.null(to_subset)) tc$subset(subset_meta = to_subset)$meta('doc_id') else NULL
    dtm = tc$dtm(feature=feature, weight = weight, drop_empty_terms = T, context_labels = T, feature_labels=F, ngrams=ngrams, form = 'tm_dtm')
  }

  if (is.null(date_col)) {
    d = RNewsflow::documents.compare(dtm, dtm.y, measure=measure, min.similarity=min_similarity)
    if (nrow(d) == 0) return(NULL)

    g = igraph::graph.data.frame(d[,c('x','y')], directed = T)
    igraph::E(g)$weight = d$similarity

    missingmeta = as.character(meta[!meta$doc_id %in% igraph::V(g)$name, 'doc_id'])
    g = igraph::add.vertices(g, nv = length(missingmeta), attr = list(name=missingmeta))
    meta = meta[match(igraph::V(g)$name, meta$doc_id),]
    attribs = colnames(meta)[!colnames(meta) == 'doc_id']
    for(attrib in attribs){
      g = igraph::set.vertex.attribute(g, attrib, value=as.character(meta[,attrib]))
    }
  } else {
    meta[,date_col] = to_POSIXct(meta[,date_col])
    if (is.null(hour_window)) {
      d = RNewsflow::documents.compare(dtm, dtm.y, measure=measure, min.similarity=min_similarity)
      if (nrow(d) == 0) return(NULL)
      g = RNewsflow::document.network(d, meta, 'doc_id', date_col)
    } else {
      g = RNewsflow::newsflow.compare(dtm, meta, id.var='doc_id', date.var=date_col, measure=measure, only.from=only_from, only.to=only_to, min.similarity=min_similarity, hour.window=hour_window)
    }
  }
  g
}

delete_duplicates <- function(tc, feature='word', date_col=NULL, meta_cols=NULL, hour_window=NULL, measure=c('cosine','overlap_pct'), similarity=1, keep=c('first','last', 'random'), weight=c('termfreq','docfreq','tfidf','norm_tfidf'), ngrams=NA, print_duplicates=F, env=environment()) {
  keep = match.arg(keep)
  for (mvar in meta_cols) if (!mvar %in% tc$meta_names) stop(sprintf('Meta column (%s) not in corpus', mvar))

  g = compare_documents_fun(tc, feature=feature, date_col=date_col, hour_window=hour_window, measure=measure, min_similarity=similarity, weight=weight, ngrams)
  if (is.null(g)) {
    message('Deleting 0 duplicates')
    return(tc)
  }

  e = igraph::get.edges(g, igraph::E(g)) ## edges by indices
  d = igraph::get.data.frame(g, 'edges') ## edges by name and with weigth
  if (!is.null(meta_cols)) {
    mx = tc$meta(meta_cols, keep_df = T)[e[,1],]
    my = tc$meta(meta_cols, keep_df = T)[e[,2],]
    allmatch = rowSums(mx == my) == ncol(mx)
    d = d[allmatch,]
  }

  duplicates = c()
  if (!is.null(date_col)) {
    if(keep == 'first') {
      duplicates = c(duplicates, as.character(unique(d$to[d$hourdiff > 0])))
      duplicates = c(duplicates, as.character(unique(d$from[d$hourdiff < 0])))
    }
    if(keep == 'last') {
      duplicates = c(duplicates, as.character(unique(d$from[d$hourdiff > 0])))
      duplicates = c(duplicates, as.character(unique(d$to[d$hourdiff < 0])))
    }
    d = d[!d$from %in% duplicates & !d$to %in% duplicates,]
  }

  ## If date_var is missing, keep == 'random', or if there are identical articles that occured simultaneously, delete randomly
  d = d[sample(1:nrow(d), nrow(d)),]
  d$fromi = match(d$from, unique(d$from, d$to))
  d$toi = match(d$to, unique(d$from, d$to))
  d = d[d$fromi < d$toi,]
  duplicates = unique(c(duplicates, as.character(d$from)))

  message('Deleting ', length(duplicates), ' duplicates')
  if (print_duplicates) sprintf('c(%s)', print(paste(sprintf('"%s"', duplicates), collapse=', ')))

  if (length(duplicates) > 0) tc$subset(subset_meta = !doc_id %in% duplicates, env=environment(), clone = F) else tc
}
