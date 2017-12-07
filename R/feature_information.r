#' Feature statistics
#'
#' @description
#' Compute a number of useful statistics for features: term frequency, idf, etc.
#'
#' @section Usage:
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{feature_stats(feature, sent_freq=F)}
#'
#' @param feature The name of the feature column
#' @param sent_freq If True, include sentence frequency (only if sentence information is available).
#'
#' @name tCorpus$feature_stats
#' @aliases feature_stats
#' @examples
#' tc = create_tcorpus(c('Text one first sentence. Text one second sentence', 'Text two'),
#'                     split_sentences = TRUE)
#'
#' fs = tc$feature_stats('token')
#' head(fs)
#'
#' fs = tc$feature_stats('token', context_level = 'sentence')
#' head(fs)
tCorpus$set('public', 'feature_stats', function(feature, context_level=c('document','sentence')){
  term_statistics(self, feature=feature, context_level=context_level)
})

#' Show top features
#'
#' @section Usage:
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{top_features(feature, n = 10, group_by = NULL, group_by_meta = NULL, return_long = F}
#'
#' @param feature The name of the feature
#' @param n Return the top n features
#' @param group_by A column in the token data to group the top features by. For example, if token data contains part-of-speech tags (pos), then grouping by pos will show the top n feature per part-of-speech tag.
#' @param group_by_meta A column in the meta data to group the top features by.
#' @param return_long if True, results will be returned in a long format. Default is a table, but this can be inconvenient if there are many grouping variables.
#'
#' @name tCorpus$top_features
#' @aliases top_features
#' @examples
#' tc = tokens_to_tcorpus(corenlp_tokens, token_id_col = 'id')
#'
#' tc$top_features('lemma')
#' tc$top_features('lemma', group_by = 'relation')
tCorpus$set('public', 'top_features', function(feature, n=10, group_by=NULL, group_by_meta=NULL, return_long=F){
  top_features(self, feature=feature, n=n, group_by=group_by, group_by_meta=group_by_meta, return_long=return_long)
})


################################
################################

# Compute some useful corpus statistics for a dtm
#
# Compute a number of useful statistics for filtering tokens: term frequency, idf, etc.

term_statistics <- function(tc, feature, context_level=c('document','sentence')) {
  dtm = tc$dtm(feature, context_level=context_level)
  dtm_term_statistics(dtm, feature)
}

dtm_term_statistics <- function(dtm, feature) {
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
  if ('sentence' %in% tc$names){
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
    scores = dcast(scores, ... ~ rank, value.var=feature)
    colnames(scores)[!colnames(scores) %in% break_cols] = paste('rank', colnames(scores)[!colnames(scores) %in% break_cols], sep='_')
  }
  scores
}

