
#' Compute some useful corpus statistics for a dtm
#'
#' Compute a number of useful statistics for filtering words: term frequency, idf, etc.
#'
#' @param dtm a document term matrix (e.g. the output of \code{\link{dtm.create}})
#' @return A data frame with rows corresponding to the terms in dtm and the statistics in the columns
term.statistics <- function(tc, feature, context_level=c('document','sentence')) {
  dtm = tc$dtm(feature, context_level=context_level)
  dtm = dtm[Matrix::rowSums(dtm) > 0, Matrix::colSums(dtm) > 0]    # get rid of empty rows/columns
  vocabulary = colnames(dtm)
  data.frame(term = as.character(vocabulary),
             characters = nchar(vocabulary),
             number = grepl("[0-9]", vocabulary),
             nonalpha = grepl("\\W", vocabulary),
             termfreq = Matrix::colSums(dtm),
             docfreq = Matrix::colSums(dtm > 0),
             reldocfreq = Matrix::colSums(dtm > 0) / nrow(dtm),
             tfidf = tapply(dtm@x/Matrix::rowSums(dtm)[dtm@i+1], dtm@j+1, mean) * log2(nrow(dtm)/Matrix::colSums(dtm > 0)),
             stringsAsFactors=F)
}


feature_stats <- function(tc, feature, sent_freq=F){
  dtm = tc$dtm(feature, context_level='document')
  dtm = dtm[Matrix::rowSums(dtm) > 0, Matrix::colSums(dtm) > 0]    # get rid of empty rows/columns
  vocabulary = colnames(dtm)
  d = data.frame(term = as.character(vocabulary),
                 characters = nchar(vocabulary),
                 number = grepl("[0-9]", vocabulary),
                 nonalpha = grepl("\\W", vocabulary),
                 termfreq = Matrix::colSums(dtm),
                 docfreq = Matrix::colSums(dtm > 0),
                 reldocfreq = Matrix::colSums(dtm > 0) / nrow(dtm),
                 tfidf = tapply(dtm$v/Matrix::rowSums(dtm)[dtm$i], dtm$j, mean) * log2(nrow(dtm)/Matrix::colSums(dtm > 0)),
                 stringsAsFactors=F)
  if ('sent_i' %in% tc$names){
    dtm = tc$dtm(feature, context_level='sentence')
    dtm = dtm[Matrix::rowSums(dtm) > 0, Matrix::colSums(dtm) > 0]    # get rid of empty rows/columns
    d$sentfreq = Matrix::colSums(dtm > 0)
  }
  d
}

rank_unique_rows <- function(d, columns=colnames(d)) {
  ## return a numerical vector of length nrow(d) that indicates how many times the unique combination of column values has occured before (above) in the data.frame. starts at 1
  d = d[,columns,drop=F]
  i = match(apply(d, 1, list), apply(unique(d), 1, list))
  i_ord = order(i)
  rank = local_position(1:nrow(d), i[i_ord])
  rank[match(1:nrow(d), i_ord)]
}

unique_row_id <- function(d, columns=colnames(d)){
  d = d[,columns,drop=F]
  match(apply(d, 1, list), apply(unique(d), 1, list))
}


top_features <- function(tc, feature, n=10, group_by=NULL, group_by_meta=NULL, return_long=F){
  if (!is.null(group_by)) group_by = match.arg(group_by, tc$names, several.ok = T)
  if (!is.null(group_by)) group_by_meta = match.arg(group_by_meta, tc$meta_names, several.ok = T)

  group_df = NULL
  if (!is.null(group_by)) group_df = as.data.frame(tc$data[,group_by,with=F])
  if (!is.null(group_by_meta)){
    if (is.null(group_df)) {
      group_df = tc$meta[,group_by_meta, with=F]
      group_df = as.data.frame(group_df[match(tc$data$doc_id, tc$meta$doc_id),])
    } else {
      match_i = match(tc$data$doc_id, tc$meta$doc_id)
      cbind(group_df,
            as.data.frame(tc$meta[match_i,group_by_meta,with=F]))
      rm(match_i)
    }
  }
  if (is.null(group_df)) group_df = data.frame(group=rep('tcorpus', nrow(tc$data)))

  ## function for ddply
  get_top_freq <- function(d, n, feature){
    x = d[[feature]]
    scores = data.frame(table(x))
    colnames(scores) = c(feature, 'freq')
    scores = scores[order(-scores$freq),]
    scores = head(scores, n)
    scores = scores[scores$freq > 0,]
    scores$rank = 1:nrow(scores)
    scores
  }

  break_cols = colnames(group_df)
  group_df[[feature]] = tc$data[[feature]]
  scores = ddply(group_df, break_cols, .fun = get_top_freq, n=n, feature=feature)

  if (!return_long) {
    scores = scores[,!colnames(scores) == 'freq', drop=F]
    scores = dcast(scores, ... ~ rank, value.var=feature)
    colnames(scores)[!colnames(scores) %in% break_cols] = paste('rank', colnames(scores)[!colnames(scores) %in% break_cols], sep='_')
  }
  scores
}

