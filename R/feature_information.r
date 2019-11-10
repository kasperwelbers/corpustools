#' Feature statistics
#'
#' Compute a number of useful statistics for features: term frequency, idf, etc.
#'
#' @param tc a tCorpus
#' @param feature The name of the feature column
#' @param context_level  Should results be returned at document or sentence level
#'
#' @return a data.frame
#' @export
#' @examples
#' tc = create_tcorpus(c('Text one first sentence. Text one second sentence', 'Text two'),
#'                     split_sentences = TRUE)
#'
#' fs = feature_stats(tc, 'token')
#' head(fs)
#'
#' fs = feature_stats(tc, 'token', context_level = 'sentence')
#' head(fs)
feature_stats <- function(tc, feature, context_level=c('document','sentence')) {
  context_level = match.arg(context_level)
  dtm = get_dtm(tc, feature, context_level=context_level)
  dtm_term_statistics(dtm, feature)
}

#' Show top features
#'
#' @param tc a tCorpus
#' @param feature The name of the feature
#' @param n Return the top n features
#' @param group_by A column in the token data to group the top features by. For example, if token data contains part-of-speech tags (pos), then grouping by pos will show the top n feature per part-of-speech tag.
#' @param group_by_meta A column in the meta data to group the top features by.
#' @param return_long if True, results will be returned in a long format. Default is a table, but this can be inconvenient if there are many grouping variables.
#'
#' @return a data.frame
#' @export
#' @examples
#' tc = tokens_to_tcorpus(corenlp_tokens, token_id_col = 'id')
#'
#' top_features(tc, 'lemma')
#' top_features(tc, 'lemma', group_by = 'relation')
top_features <- function(tc, feature, n=10, group_by=NULL, group_by_meta=NULL, return_long=F){
  if (!is.null(group_by)) group_by = match.arg(group_by, tc$names, several.ok = T)
  if (!is.null(group_by)) group_by_meta = match.arg(group_by_meta, tc$meta_names, several.ok = T)

  group_df = NULL
  if (!is.null(group_by)) group_df = as.data.frame(tc$get(group_by, keep_df = T))
  if (!is.null(group_by_meta)){
    if (is.null(group_df)) {
      group_df = tc$get_meta(group_by_meta, keep_df=T)
      group_df = as.data.frame(group_df[match(tc$get('doc_id'), tc$get_meta('doc_id')),])
    } else {
      match_i = match(tc$get('doc_id'), tc$get_meta('doc_id'))
      cbind(group_df,
            as.data.frame(tc$get_meta(group_by_meta,keep_df=T)[match_i,]))
      rm(match_i)
    }
  }
  if (is.null(group_df)) group_df = data.frame(group=rep('tcorpus', tc$n))

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
  group_df[[feature]] = tc$get(feature)
  scores = plyr::ddply(group_df, break_cols, .fun = get_top_freq, n=n, feature=feature)

  if (!return_long) {
    scores = scores[,!colnames(scores) == 'freq', drop=F]
    scores$token = as.character(scores[[feature]])
    scores = dcast(data.table::as.data.table(scores), ... ~ rank, value.var=feature)
    scores = as.data.frame(scores)
    colnames(scores)[!colnames(scores) %in% break_cols] = paste('rank', colnames(scores)[!colnames(scores) %in% break_cols], sep='_')
  }
  scores
}

################################
################################

# Compute some useful corpus statistics for a dtm
#
# Compute a number of useful statistics for filtering tokens: term frequency, idf, etc.



dtm_term_statistics <- function(dtm, feature) {
  dtm = dtm[Matrix::rowSums(dtm) > 0, Matrix::colSums(dtm) > 0]    # get rid of empty rows/columns
  dtm = as_dgTMatrix(dtm)
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



