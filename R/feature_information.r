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
#' \donttest{
#' fs = feature_stats(tc, 'token')
#' head(fs)
#' fs = feature_stats(tc, 'token', context_level = 'sentence')
#' head(fs)
#' }
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
#' @param rank_by        The method for ranking the terms. Currently supports frequency (default) and the 'Chi2' value for the
#'                       relative frequency of a term in a topic compared to the overall corpus.
#'                       If return_long is used, the Chi2 score is also returned, but note that there are negative Chi2 scores.
#'                       This is used to indicate that the relative frequency of a feature in a group was lower than
#'                       the relative frequency in the corpus  (i.e. under-represented).
#' @param dropNA      if TRUE, drop NA features
#' @param return_long if TRUE, results will be returned in a long format that contains more information.
#'
#' @return a data.frame
#' @export
#' @examples
#' tc = tokens_to_tcorpus(corenlp_tokens, token_id_col = 'id')
#'
#' top_features(tc, 'lemma')
#' top_features(tc, 'lemma', group_by = 'NER', group_by_meta='doc_id')
top_features <- function(tc, feature, n=10, group_by=NULL, group_by_meta=NULL, rank_by=c('freq','chi2'), dropNA=T, return_long=F){
  rank_by = match.arg(rank_by)
  .N = NULL   ## data.table bindings
  if (!is.null(group_by)) group_by = match.arg(group_by, tc$names, several.ok = T)
  if (!is.null(group_by_meta)) group_by_meta = match.arg(group_by_meta, tc$meta_names, several.ok = T)
  if (!length(feature) == 1 || !methods::is(feature,'character')) stop("feature argument has to be a single character value")

  feat = tc$get(c(feature, group_by), keep_df=T)
  if (!is.null(group_by_meta))
    feat = cbind(feat, tc$get_meta(group_by_meta, per_token = T, keep_df = T))
  if (ncol(feat) == 1) {
    group_by = '.TOTAL'
    feat$.TOTAL = 'total'
  }

  by_cols = unique(c(feature,group_by,group_by_meta))
  feat = feat[,list(freq = .N), by=by_cols]
  feat = data.table::as.data.table(feat)  ## something really weird is going on here...

  if (dropNA) {
    is_na = is.na(feat[[feature]])
    feat = feat[!is_na,]
  }


  if (rank_by == 'chi2') {
    totals_feature = tc$tokens[,list(.total_feature=.N), by=feature]
    data.table::setkeyv(feat, feature)
    feat = merge(feat, totals_feature, on=feature, all.x=T)

    if (group_by[1] == '.TOTAL') {
      feat$.total_group = tc$n
    } else {
      totals_group = tc$tokens[,list(.total_group=.N), by=c(group_by,group_by_meta)]
      data.table::setkeyv(feat, c(group_by,group_by_meta))
      feat = merge(feat, totals_group, on=c(group_by,group_by_meta), all.x=T)
    }

    a=feat$freq
    b=feat$.total_feature - feat$freq
    c=feat$.total_group - feat$freq
    d=tc$n - feat$.total_group
    feat$chi = calc_chi2(a,b,c,d)

    ## make Chi negative if relative frequency in group is lower than in total
    ratio = (a/c) / (b/d)
    ratio[d==0] = 1
    ratio[c==0] = 0
    ratio[is.na(ratio)] = 0
    chi_sign = ifelse(ratio < 1, -1, 1)
    feat$chi = feat$chi * chi_sign

    data.table::setorderv(feat, 'chi', -1)
  } else {
    data.table::setorderv(feat, 'freq', -1)
  }

  rankfun <- function(x) 1:length(x)
  .RANK = freq = NULL
  feat[,.RANK := rankfun(freq), by=c(group_by,group_by_meta)]
  feat = feat[feat$.RANK <= n,]

  if (!return_long) {
    feat[[feature]] = as.character(feat[[feature]])
    .feature = feature
    feat = data.table::dcast(feat[,c(group_by,group_by_meta,.feature,'.RANK'),with=F], ... ~ .RANK, value.var=.feature)
    is_rank = grepl('^[0-9]*$', colnames(feat))
    colnames(feat)[is_rank] = paste0('r',colnames(feat)[is_rank])
  }
  feat
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



