calc_sim <- function(dtm, meta, date_col, window=2, group_cols, min_value=0.5, measure=c('cosine','overlap_pct'), unit=c('days','hours', 'mins'), verbose=TRUE) {
  ## add only_from and only_to cheaply, using logical vector that are used to filter out results (so the crossprod still includes irrelevant matches)

  measure=match.arg(measure)
  unit = match.arg(unit)
  meta_ids = group_date_ids(meta, date_col, group_cols, unit=unit)
  out = compare_documents_cpp(dtm[meta_ids$i,], meta_ids$.GROUP_ID, meta_ids$.DATE_ID, lwindow=window, rwindow=window, measure = measure, min_value = min_value, verbose=verbose)
  if (measure %in% c('cosine')) out = Matrix::forceSymmetric(out, uplo = 'L')
  original_order = match(1:nrow(out), meta_ids$i)
  out = out[original_order, original_order]
  colnames(out) = rownames(out) = rownames(dtm)
  out
}

group_date_ids <- function(meta, date_col, group_cols, unit) {
  .ID = NULL; .DATE_ID = NULL; .GROUP_ID = NULL ## data.table bindings

  meta$i = 1:nrow(meta)

  if (!is.null(group_cols)) {
    ugroup = unique(subset(meta, select=group_cols))
    ugroup[,.GROUP_ID := 1:nrow(ugroup)]
    meta = merge(meta, ugroup, by=group_cols)
  } else meta[, .GROUP_ID := 1]

  if (!is.null(date_col)) {
    meta$date = as.POSIXct(meta$date)
    .DATE_ID = as.numeric(cut(meta$date, breaks=unit))
    meta[, .DATE_ID := .DATE_ID]
  } else meta[, .DATE_ID := 1]

  data.table::setorder(meta, .GROUP_ID, .DATE_ID)
  subset(meta, select= c('.GROUP_ID','.DATE_ID','i'))
}



#' Compare the documents in a dtm with a sliding window over time
#'
#' Given a document-term matrix (DTM) and corresponding document meta data, calculates the document similarities over time using with a sliding window.
#'
#' The meta data.frame should have a column containing document id's that match the rownames of the DTM (i.e. document names) and should have a column indicating the publication time.
#' By default these columns should be labeled "document_id" and "date", but the column labels can also be set using the `doc_col` and `date_col` parameters.
#' Any other columns will automatically be included as document meta information in the output.
#'
#' The calculation of document similarity is performed using a vector space model approach.
#' Inner-product based similarity measures are used, such as cosine similarity.
#' It is recommended to weight the DTM beforehand, for instance using Term frequency-inverse document frequency (tf.idf)
#'
#' @param dtm A document-term matrix in the tm \link[tm]{DocumentTermMatrix} class. It is recommended to weight the DTM beforehand, for instance using \link[tm]{weightTfIdf}.
#' @param meta A data.frame where rows are documents and columns are document meta information.
#' Should at least contain 2 columns: the document name/id and date.
#' The name/id column should match the document names/ids of the edgelist, and its label is specified in the `doc_col` argument.
#' The date column should be intepretable with \link[base]{as.POSIXct}, and its label is specified in the `date_col` argument.
#' @param doc_col The label for the document name/id column in the `meta` data.frame. Default is "document_id"
#' @param date_col The name of the document date column in the `meta` data.frame. If given, only documents within the given date window (specified in hour_window) will be compared
#' @param meta_cols The names of other columsn in the `meta` data.frame. If given, only documents with identical values in these columns will be compared.
#' @param hour_window A vector of eighter length 1 or 2. If length is 1, the same value is used for the left and right side of the window. If length is 2, the first and second value determine the left and right side. For example, the value 12 will compare each document to all documents between the previous and next 12 hours, and c(-10, 36) will compare each document to all documents between the previous 10 and the next 36 hours.
#' @param measure the measure that should be used to calculate similarity/distance/adjacency. Currently supports the symmetrical measure "cosine" (cosine similarity), and the assymetrical measures "overlap_pct" (percentage of term scores in the document that also occur in the other document).
#' @param min_similarity a threshold for similarity. lower values are deleted. Set to 0.1 by default.
#' @param n_topsim An alternative or additional sort of threshold for similarity. Only keep the [n_topsim] highest similarity scores for x. Can return more than [n_topsim] similarity scores in the case of duplicate similarities.
#' @param only_from A vector with names/ids of documents (dtm rownames), or a logical vector that matches the rows of the dtm. Use to compare only these documents to other documents.
#' @param only_to A vector with names/ids of documents (dtm rownames), or a logical vector that matches the rows of the dtm. Use to compare other documents to only these documents.
#' @param return_zeros If true, all comparison results are returned, including those with zero similarity (rarely usefull and problematic with large data)
#' @param only_complete_window if True, only compare articles (x) of which a full window of reference articles (y) is available. Thus, for the first and last [window.size] days, there will be no results for x.
#' @param verbose If TRUE, report progress
#'
#' @return A network/graph in the \link[igraph]{igraph} class
#' @export
#'
#' @examples
#' \dontrun{
#' data(dtm)
#' data(meta)
#'
#' dtm = tm::weightTfIdf(dtm)
#' g = compare_documents_dtm(dtm, meta, hour_window = c(0.1, 36))
#'
#' vcount(g) # number of documents, or vertices
#' ecount(g) # number of document pairs, or edges
#'
#' head(igraph::get.data.frame(g, 'vertices'))
#' head(igraph::get.data.frame(g, 'edges'))
#' }
compare_documents_dtm <- function(dtm, meta=NULL, doc_col='doc_id', date_col=NULL, meta_cols=NULL, hour_window=24, measure=c('cosine','overlap_pct'), min_similarity=0, n_topsim=NULL, only_from=NULL, only_to=NULL, return_zeros=FALSE, only_complete_window=FALSE, verbose=T){
  if (length(hour_window) == 1) hour_window = c(-hour_window, hour_window)
  measure = match.arg(measure)

  if (is.null(meta)) {
    meta = data.frame(doc_id = rownames(dtm))
    colnames(meta)[1] = doc_col
  } else {
    meta = as.data.frame(meta)
  }

  validate_dtm_meta(meta, doc_col, date_col)
  meta = match_dtm_meta(dtm, meta, doc_col)

  if (verbose && (!is.null(date_col) || !is.null(meta_cols))) message('Indexing articles')
  if(is.null(only_from) & is.null(only_to)){
    date_i_x = date_i_y = get_date_indices(meta, date_col)
    meta_i_x = meta_i_y = get_meta_indices(meta, meta_cols)
    only_from = only_to = rep(TRUE, nrow(dtm))
  } else{
    if(is.null(only_from)) only_from = rep(TRUE, nrow(dtm))
    if(is.null(only_to)) only_to = rep(TRUE, nrow(dtm))
    if(!class(only_from) == 'logical') only_from = rownames(dtm) %in% only_from
    if(!class(only_to) == 'logical') only_to = rownames(dtm) %in% only_to
    date_i_x = get_date_indices(meta, date_col, only_from)
    date_i_y = get_date_indices(meta, date_col, only_to)
    meta_i_x = get_meta_indices(meta, meta_cols, only_from)
    meta_i_y = get_meta_indices(meta, meta_cols, only_to)
  }

  ## dateindex and metaindex are the non-empty indices of the indices.
  metaindex = if (!is.null(meta_i_x) && !is.null(meta_i_y)) which(lapply(meta_i_x, length) > 0) else NULL
  if (!is.null(date_i_x) && !is.null(date_i_y) && !is.null(hour_window)) {
    dateindex = which(lapply(date_i_x, length) > 0)
    ## similarity scores for date intervals will actually be calculated at the level of days since the
    ## cost of the unnecessary columns in the matrix multiplication is often less than the cost of performing it more often.
    window = floor(hour_window[1]/24):ceiling(hour_window[2]/24)
    if(only_complete_window && !is.null(dateindex)){
      if(window[1] < 0) dateindex = dateindex[dateindex > window[1]]
      if(rev(window)[1] > 0) dateindex = dateindex[dateindex <= length(date_i_x) - rev(window)[1]]
    }
  } else dateindex = NULL

  if (verbose) message('Comparing documents')
  output = compare_documents_loop(dtm, dateindex, metaindex, date_i_x, date_i_y, meta_i_x, meta_i_y, only_from, only_to, window, measure, min_similarity, n_topsim, return_zeros, verbose)

  if (verbose) message('Matching document meta')
  g = document_network(output, meta, doc_col, date_col)

  if ('hourdiff' %in% igraph::edge.attributes(g)) {
    delete_pairs = which(igraph::E(g)$hourdiff < hour_window[1] | igraph::E(g)$hourdiff > hour_window[2])
    g = igraph::delete.edges(g, delete_pairs)
  }
  g
}

compare_documents_loop <- function(dtm, dateindex, metaindex, date_i_x, date_i_y, meta_i_x, meta_i_y, only_from, only_to, window, measure, min_similarity, n_topsim, return_zeros, verbose) {
  ## for efficiency, use different code depending on whether there is a dateindex and metaindex

  ## no date and meta
  if (is.null(dateindex) && is.null(metaindex)) {
    return(calculate_docsim(dtm[only_from,,drop=F], dtm[only_to,,drop=F], measure, min_similarity, n_topsim, return_zeros))
  }

  ## only meta
  if (is.null(dateindex) && !is.null(metaindex)) {
    results = vector('list', length(metaindex))
    if (verbose) pb = utils::txtProgressBar(min=0, max=length(results), style=3)
    i = 0
    for (meta_i in metaindex) {
      i = i + 1
      if (verbose) pb$up(i)
      x_i = unique(meta_i_x[[meta_i]])
      if (is.null(x_i)) next
      y_i = unique(meta_i_y[[meta_i]])
      if (is.null(y_i)) next
      results[[i]] = calculate_docsim(dtm[x_i,,drop=F], dtm[y_i,,drop=F], measure, min_similarity, n_topsim, return_zeros)
    }
  }

  ## only date
  if (!is.null(dateindex) && is.null(metaindex)) {
    results = vector('list', length(dateindex))
    if (verbose) pb = utils::txtProgressBar(min=0, max=length(results), style=3)
    i = 0
    for (date_i in dateindex) {
        i = i + 1
        if (verbose) pb$up(i)
        x_i = unique(date_i_x[[date_i]])
        if (is.null(x_i)) next
        y_i = unique(unlist_window(date_i_y, date_i, window))
        if (is.null(y_i)) next

        results[[i]] = calculate_docsim(dtm[x_i,,drop=F], dtm[y_i,,drop=F], measure, min_similarity, n_topsim, return_zeros)
    }
  }

  ## both
  if (!is.null(dateindex) && !is.null(metaindex)) {
    results = vector('list', length(dateindex) * length(metaindex))
    if (verbose) pb = utils::txtProgressBar(min=0, max=length(results), style=3)
    i = 0
    for (date_i in dateindex) {
      for (meta_i in metaindex) {
        i = i + 1
        if (verbose) pb$up(i)
        x_i = intersect(date_i_x[[date_i]],
                        meta_i_x[[meta_i]])
        if (is.null(x_i)) next
        y_i = intersect(unlist_window(date_i_y, date_i, window),
                        meta_i_y[[meta_i]])
        if (is.null(y_i)) next
        results[[i]] = calculate_docsim(dtm[x_i,,drop=F], dtm[y_i,,drop=F], measure, min_similarity, n_topsim, return_zeros)
      }
    }
  }
  cat('\n')
  data.table::rbindlist(results)
}


cosine_similarity <- function(m1, m2=NULL){
  m1 = as_dgTMatrix(m1)
  norm = sqrt(Matrix::colSums(m1^2))
  m1@x = m1@x / norm[m1@j+1]
  if(!is.null(m2)){
    m2 = as_dgTMatrix(m2)
    norm = sqrt(Matrix::colSums(m2^2))
    m2@x = m2@x / norm[m2@j+1]
    cp = Matrix::crossprod(m1,m2)
  } else cp = Matrix::crossprod(m1)
  cp
}

term_overlap_pct <- function(m1, m2=m1, reverse=FALSE){
  totalterms = if(!reverse) Matrix::colSums(methods::as(m1, 'dgCMatrix')) else Matrix::colSums(methods::as(m2, 'dgCMatrix'))
  m2@x[Matrix::which(m2@x > 0)] = 1
  Matrix::crossprod(m1,m2) / totalterms
}

nth_max <- function(x, N){
  N = min(N, length(x))
  -sort(-x, partial=N)[N]
}

filter_results <- function(results, min_similarity, n_topsim){
  if(!is.null(min_similarity)) results@x[Matrix::which(results@x < min_similarity)] = 0
  if(!is.null(n_topsim)) {
    simthres = apply(results, 1, nth_max, N=n_topsim)
    results@x[Matrix::which(results < simthres)] = 0
  }
  results
}

calculate_similarity <- function(m.x, m.y, measure){
  if(measure == 'cosine') results = cosine_similarity(m.x, m.y)
  if(measure == 'overlap_pct') results = term_overlap_pct(m.x, m.y)
  if(measure == 'overlap_pct_rev') results = term_overlap_pct(m.x, m.y, reverse = TRUE)
  results
}

reindex_terms <- function(dtm, terms){
  dtm = as_dgTMatrix(dtm)
  documents = rownames(dtm)
  dtm = Matrix::spMatrix(nrow(dtm), length(terms), dtm@i+1, match(colnames(dtm)[dtm@j+1], terms), dtm@x)
  dimnames(dtm) = list(documents, terms)
  tm::as.DocumentTermMatrix(dtm, weighting = tm::weightTf)
}

calculate_docsim <- function(dtm, dtm.y=NULL, measure='cosine', min_similarity=0, n_topsim=NULL, return.zeros=FALSE, check_columns=F) {

  ### too slow, not neceesary?
  if(!is.null(dtm.y)){
    if (check_columns) {
      if(!all(colnames(dtm) == colnames(dtm.y))){
        ## if colnames do not match, reindex them.
        terms = unique(c(colnames(dtm), colnames(dtm.y)))
        dtm = reindex_terms(dtm, terms)
        dtm.y = reindex_terms(dtm.y, terms)
      }
    }
    m.x = Matrix::t(as_dgTMatrix(dtm))
    m.y = Matrix::t(as_dgTMatrix(dtm.y))
  } else {
    m.x = m.y = Matrix::t(as_dgTMatrix(dtm))
  }

  results = calculate_similarity(m.x, m.y, measure)
  results = filter_results(results, min_similarity, n_topsim)

  results = methods::as(results, 'dgTMatrix')
  if(return.zeros) {
    results = Matrix::Matrix(Matrix::which(!is.na(results), arr.ind=TRUE))
    results = data.frame(x=colnames(m.x)[results[,1]], y=colnames(m.y)[results[,2]], similarity=as.vector(results))
  } else{
    if(sum(results) == 0) return(NULL)
    results = data.frame(x=colnames(m.x)[results@i+1], y=colnames(m.y)[results@j+1], similarity=results@x)
    results = results[results$similarity > 0 & !is.na(results$similarity),]
  }
  results[!as.character(results$x) == as.character(results$y),]
}


document_network <- function(d, meta, doc_col='document_id', date_col='date', min_similarity=0){
  validate_dtm_meta(meta, doc_col, date_col)
  if (nrow(d) == 0) d = data.frame(x=numeric(), y=numeric(), similarity=numeric())

  colnames(d) = c('x','y','similarity')
  d = d[d$similarity >= min_similarity, c('x','y','similarity')]
  d = d[order(-d$similarity),]

  g = igraph::graph.data.frame(d[,c('x','y')])
  igraph::E(g)$weight = d$similarity


  if (nrow(d) > 0) {
    if (!all(igraph::V(g)$name %in% meta[[doc_col]])) stop("Not all documents in d match with an 'id' in the meta information")
  }

  ## add documents in meta data.frame that do not appear in the edgelist (in other words, isolates)
  missingmeta = as.character(meta[!meta[,doc_col] %in% igraph::V(g)$name, doc_col])
  g = igraph::add.vertices(g, nv = length(missingmeta), attr = list(name=missingmeta))

  meta = meta[match(igraph::V(g)$name, meta[,doc_col]),]
  attribs = colnames(meta)[!colnames(meta) == doc_col]
  for(attrib in attribs){
    g = igraph::set.vertex.attribute(g, attrib, value=as.character(meta[,attrib]))
  }

  edgelist = igraph::get.edges(g, igraph::E(g))
  if ('date' %in% igraph::vertex.attributes(g)) {
    dates = igraph::get.vertex.attribute(g, name=date_col)
    dates = as.POSIXct(dates)
    igraph::E(g)$hourdiff = round(difftime(dates[edgelist[,2]], dates[edgelist[,1]], units = 'hours'),3)
  }
  g
}


validate_dtm_meta <- function(meta, doc_col, date_col){
  if (is.null(meta)) return(NULL)
  if(!is.null(doc_col) && !doc_col %in% colnames(meta)) stop(sprintf('Meta data.frame should contain a column that matches the doc_col parameter (currently set to "%s")', doc_col))
  if(!is.null(date_col) && !date_col %in% colnames(meta)) stop(sprintf('Meta data.frame should contain a column that matches the doc_col parameter (currently set to "%s")', date_col))
}

match_dtm_meta <- function(dtm, meta, doc_col){
  if (is.null(meta)) return(NULL)
  if(!all(rownames(dtm) %in% meta[,doc_col])) stop('Not all documents in DTM match with a document in the meta data.frame')
  meta[match(rownames(dtm), meta[,doc_col]),,drop=F]
}

unlist_window <- function(list_object, i, window){
  indices = i + window
  indices = indices[indices > 0 & indices <= length(list_object)]
  unlist(list_object[indices], use.names=FALSE)
}

get_date_indices <- function(meta, date_col, row_filter=NULL){
  if (is.null(date_col)) return(NULL)

  date = meta[[date_col]]
  if(is.null(row_filter)) row_filter = rep(TRUE, length(date))

  datetime = as.Date(date)
  datetimeseq = seq.Date(min(datetime), max(datetime), by='days')

  nonempty = which(datetimeseq %in% unique(datetime))
  nonempty_datetime_ids = plyr::llply(datetimeseq[nonempty], function(dtime) which(datetime == dtime & row_filter))
  datetime_ids = vector("list", length(datetimeseq))
  datetime_ids[nonempty] = nonempty_datetime_ids
  datetime_ids
}

get_meta_indices <- function(meta, meta_cols, row_filter=NULL){
  .ID = NULL ## data.table bindings

  if (is.null(meta_cols)) return(NULL)
  meta = data.table::as.data.table(meta)

  meta = subset(meta, select=meta_cols)
  umeta = unique(meta)
  umeta[,.ID :=1:nrow(umeta)]

  meta$.index = 1:nrow(meta)
  if (!is.null(row_filter)) meta = meta[row_filter,]

  meta = merge(meta, umeta, by=meta_cols, all.x=T)
  meta_indices = split(meta$.index, meta$.ID)

  named_list_to_numbered(meta_indices, max(umeta$.ID))
}

named_list_to_numbered <- function(l, len) {
  out = vector('list', len)
  out[as.numeric(names(l))] = l
  out
}

