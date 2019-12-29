#' Count results of search hits, or of a given feature in tokens
#'
#' @param tc          A tCorpus
#' @param meta_cols   The columns in the meta data by which the results should be grouped
#' @param hits        featureHits or contextHits (output of \code{\link{search_features}}, \code{\link{search_dictionary}} or \code{\link{search_contexts}})
#' @param feature     Instead of hits, a specific feature column can be selected.
#' @param count       How should the results be counted? Number of documents, tokens, or unique hits. The difference between tokens and hits is that hits can encompass multiple tokens (e.g., "Bob Smith" is 1 hit and 2 tokens).
#' @param wide        Should results be in wide or long format?
#'
#' @return A data table
#' @export
#'
#' @examples
#' \donttest{
#' tc = create_tcorpus(sotu_texts, doc_col='id')
#' hits = search_features(tc, c("US# <united states>", "Economy# econom*"))
#' count_tcorpus(tc, hits=hits)
#' count_tcorpus(tc, hits=hits, meta_cols='president')
#' count_tcorpus(tc, hits=hits, meta_cols='president', wide=FALSE)
#' }
count_tcorpus <- function(tc, meta_cols=NULL, hits=NULL, feature=NULL, count=c('documents','tokens', 'hits'), wide=T){
  group = NULL; code = NULL
  tc$validate()

  count = match.arg(count)

  meta = data.table::copy(tc$meta)
  if (is.null(meta_cols)) {
    meta[,group := 'total']
    meta_cols = 'group'
  }

  len = tc$tokens[, list(len=.N), by='doc_id']
  meta = merge(meta, len, by='doc_id', all.x=T)
  d = meta[, list(N=.N, V=sum(len)), by=meta_cols]

  if (!is.null(hits) | !is.null(feature)){
    if (!is.null(hits) && !is.null(feature)) stop('Cannot specify both hits and feature')
    if (!is.null(hits)) {
      if (!methods::is(hits, c('featureHits', 'contextHits'))) stop('hits must be a featureHits or contextHits object (see the search_features and search_contexts functions)')
      if (methods::is(hits, 'featureHits')) {
        coldata = hits$hits[!duplicated(hits$hits[,c('code', 'hit_id')]),]
      } else {
        coldata = hits$hits
      }
    }

    if (!is.null(feature)) {
      coldata = data.frame(doc_id = tc$tokens$doc_id, code = tc$tokens[[feature]])
      coldata = coldata[!is.na(coldata$code),]
    }

    if (count == 'documents') coldata = coldata[!duplicated(coldata[,c('doc_id','code')]),]
    if (count == 'hits') coldata = coldata[!duplicated(coldata[,c('doc_id','code','hit_id')]),]

    meta = merge(meta, coldata, by='doc_id', all=T)
    count_d = subset(meta, select = c('code', meta_cols))
    agg_cols = c(meta_cols, 'code')

    count_d = count_d[, list(count=.N), by = agg_cols]
    d = merge(d, count_d, meta_cols, all=T)
    d$count[is.na(d$count)] = 0
    if (wide) {
      d = dcast(d, ... ~ code, value.var='count')
      d[['NA']] = NULL
      d[is.na(d)] = 0
    } else {
      d = subset(d, !is.na(code))
    }
  }

  d
}


#' Aggregate the tokens data
#'
#' This is a wrapper for the data.table aggregate function, for easy aggregation of the tokens data grouped by columns in the tokens or meta data.
#' The .id argument is an important addition, because token annotation often contain values that span multiple rows.
#'
#' @param tc          A tCorpus
#' @param ...         The name of the aggregated column and the function over an existing column are given as a name value pair. For example,
#'                    count = length(token) will count the number of tokens in each group, and sentiment = mean(sentiment, na.rm=T)
#'                    will calculate the mean score for a column with sentiment scores.
#' @param by          A character vector with column names from the tokens and/or meta data.
#' @param .id         If an id column is given, only rows for which this id is not NA are used, and only one row for each id is used.
#'                    This prevents double counting of values in annotations that span multiple rows. For example, a sentiment dictionary can match the tokens "not good", in which case
#'                    the sentiment score (-1) will be assigned to both tokens. These annotations should have an _id column that indicates the unique matches.
#' @param wide        Should results be in wide or long format?
#'
#'
#' @return A data table
#' @export
#'
#' @examples
#' \donttest{
#' tc = create_tcorpus(sotu_texts, doc_col='id')
#'
#' library(quanteda)
#' dict = data_dictionary_LSD2015
#' dict = melt_quanteda_dict(dict)
#' dict$sentiment = ifelse(dict$code %in% c('positive','neg_negative'), 1, -1)
#' tc$code_dictionary(dict)
#'
#' agg_tcorpus(tc, N = length(sentiment), sent = mean(sentiment), .id='code_id')
#' agg_tcorpus(tc, sent = mean(sentiment), .id='code_id', by='president')
#' agg_tcorpus(tc, sent = mean(sentiment), .id='code_id', by=c('president', 'token'))
#' }
agg_tcorpus <- function(tc, ..., by=NULL, .id=NULL, wide=T) {
  e = substitute(list(...))
  f = tc$tokens

  if (!is.null(by)) {
    in_tokens = by %in% tc$names
    in_meta = by %in% tc$meta_names
    only_one = !by %in% c('doc_id')
    if (any(in_tokens & in_meta & only_one)) {
      doubles = by[in_tokens & in_meta & only_one]
      warning(sprintf("columns in 'by' occur in both the tokens and meta data (%s). The columns in tokens are now used.", paste(doubles, collapse=', ')))
    }
    if (any(!in_tokens & !in_meta)) {
      missing = by[!in_tokens & !in_meta]
      warning(sprintf("columns in 'by' do not occur in tokens or meta data (%s)", paste(missing, collapse=', ')))
    }

    in_tokens = by[in_tokens]
    in_meta = by[in_meta]
    in_meta = setdiff(in_meta, in_tokens)
    if (length(in_meta) > 0) f = cbind(f, tc$get_meta(in_meta, per_token = T, keep_df = T))
  }
  if (!is.null(.id)) f = subset(f, !is.na(f[[.id]]) & !duplicated(f[[.id]]))
  f = f[,eval(e), by=by]
  if (!wide) {
    f = data.table::melt(f, id.vars = by)
  }
  f
}

