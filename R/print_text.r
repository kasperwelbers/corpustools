
#' Create and view a full text browser
#'
#' Creates a static HTML file to view the texts in the tcorpus in full text mode.
#'
#' @param tc          a tCorpus
#' @param doc_ids     A vector with document ids to view
#' @param token_col   The name of the column in tc$tokens that contain the token text
#' @param n           Only n of the results are printed (to prevent accidentally making huge browsers).
#' @param select      If n is smaller than the number of documents in tc, select determines how the n documents are selected
#' @param header      Optionally, a title presented at the top of the browser
#' @param subheader   Optionally, overwrite the subheader. By default the subheader reports the number of documents
#' @param highlight   The name of a numeric column in tc$tokens with values between 0 and 1, used to highlight tokens.
#'                    Can also be a character vector, in which case al non-NA values are highlighted
#' @param scale       The name of a numeric column in tc$tokens with values between -1 and 1, used to color tokens on a scale (set colors with scale_col)
#' @param category    The name of a character or factor column in tc$tokens. Each unique value will have its own color, and navigation for categories will be added (nav cannot be used with this option)
#' @param meta_cols   A character vector with names of columns in tc$meta, used to only show the selected columns
#' @param seed        If select is "random", seed can be used to set a random seed. After sampling the seed is re-initialized with set.seed(NULL).
#' @param nav         Optionally, a column in tc$meta to add navigation (only supports simple filtering on unique values).
#'                    This is not possible if annotate is used.
#' @param top_nav     A number. If navigation based on token annotations is used, filters will only apply to top x values with highest token occurence in a document
#' @param thres_nav    Like top_nav, but specifying a threshold for the minimum number of tokens.
#' @param view        If TRUE (default), view the browser in the Viewer window (turn off if this is not supported)
#' @param highlight_col If highlight is used, the color for highlighting
#' @param scale_col     If scale is used, a vector with 2 or more colors used to create a color ramp. That is, -1 is first color, +1 is last color, if three colors are given 0 matches the middle color, and colors in between are interpolated.
#' @param filename    Optionally, save the browser at a specified location
#'
#' @return The url for the file location is returned (invisibly)
#' @export
#'
#' @examples
#' \donttest{
#' tc = create_tcorpus(sotu_texts, doc_column='id')
#' url = browse_texts(tc)
#'
#'
#' ## tokens can be highlighted, scaled or coloured for different categories
#' ## to validate analyses such as dictionaries, scaling and topic modeling
#' tc = create_tcorpus(sotu_texts, doc_column = 'id', udpipe_model='english-ewt')
#' tc$preprocess('lemma', 'feature', min_docfreq = 10)
#' tc$feature_subset('feature', POS %in% c('NOUN','PROPN','VERB'))
#' m = tc$lda_fit('feature', create_feature = 'topic', K = 5, alpha = 0.001)
#'
#' browse_texts(tc, category='topic', view=T, top_nav=1)
#' }
browse_texts <- function(tc, doc_ids=NULL, token_col='token', n=500, select=c('first','random'), header='',
                         subheader=NULL, highlight=NULL, scale=NULL, category=NULL, meta_cols=NULL, seed=NA,
                         nav=NULL, top_nav=NULL, thres_nav=1, view=T, highlight_col='yellow', scale_col=c('red','blue','green'), filename=NULL) {
  if (sum(!is.null(highlight), !is.null(scale), !is.null(category)) > 1) stop('Can only use one annotation option (highlight, scale or category)')

  mode = 'normal'
  if (!is.null(highlight)) {
    mode = 'highlight'
    if (!highlight %in% tc$names) stop('highlight is not a valid column name in tc$tokens')
    if (!(is.numeric(tc$tokens[[highlight]]) || is.character(tc$tokens[[highlight]]) || is.factor(tc$tokens[[highlight]]))) stop("highlight has to be a numeric or character value")
    if (is.numeric(tc$tokens[[highlight]])) {
      if (min(tc$tokens[[highlight]], na.rm=T) < 0 || max(tc$tokens[[highlight]], na.rm=T) > 1) stop('highlight has to be a value between 0 and 1')
    }
  }
  if (!is.null(scale)) {
    mode = 'scale'
    if (!scale %in% tc$names) stop('scale is not a valid column name in tc$tokens')
    if (!is.numeric(tc$tokens[[scale]])) stop("scale has to be a numeric value")
    if (min(tc$tokens[[scale]], na.rm=T) < -1 || max(tc$tokens[[scale]], na.rm=T) > 1) stop('scale has to be a value between -1 and 1')
  }
  if (!is.null(category)) {
    mode = 'category'
    if (!category %in% tc$names) stop('category is not a valid column name in tc$tokens')
    if (!is.character(tc$tokens[[category]]) && !is.factor(tc$tokens[[category]]) && !is.numeric(tc$tokens[[category]])) stop("category has to be a character/factor or numeric value")
  }

  select = match.arg(select)
  if (!is.null(nav)) {
    if (!nav %in% tc$meta_names) stop('nav is not a valid column in tc$meta')
  }

  subhead = ''
  if (is.null(doc_ids)) doc_ids = tc$meta$doc_id

  if (length(doc_ids) > n) {
    if (select == 'first') {
      subhead = sprintf('first <ndoc>%s</ndoc>/%s documents (N = %s)', n,  n, length(doc_ids))
      .DOC_IDS = head(doc_ids, n)
    }
    if (select == 'random') {
      subhead = sprintf('random <ndoc>%s</ndoc> / %s documents (N = %s)', n, n, length(doc_ids))
      if (!is.na(seed)) set.seed(seed)
      .DOC_IDS = sample(doc_ids, size=n)
      if (!is.na(seed)) set.seed(NULL)
    }
  } else {
    subhead = sprintf('<ndoc>%s</ndoc> (N = %s)', length(doc_ids), length(doc_ids))
    .DOC_IDS = doc_ids
  }

  #} else {
  #    subhead = sprintf('<ndoc>%s</ndoc> (N = %s)', length(doc_ids), nrow(tc$meta))
  #    .DOC_IDS = doc_ids
  #}
  doc_id = NULL ## for cran check
  sub_tc = tc$subset_meta(doc_id %in% .DOC_IDS, copy = T)
  if (!is.null(subheader)) subhead = subheader

  if (!is.null(meta_cols)) {
    meta_cols = union('doc_id', meta_cols)
    if (!all(meta_cols %in% sub_tc$meta_names)) stop('Not all values in meta_cols are valid column in tc$meta')
    meta = subset(sub_tc$meta, select=meta_cols)
  } else meta = sub_tc$meta

  if (!is.null(nav)) {
    if (!nav %in% tc$meta_names) stop('nav is not a valid column in tc$meta')
    meta = meta[order(meta[[nav]]),]
  }


  if (mode == 'normal') url = tokenbrowser::create_browser(sub_tc$tokens, meta=meta, token_col = token_col, doc_nav = nav, header=header, subheader = subhead, filename=filename, n=F, top_nav=top_nav, thres_nav=thres_nav)
  if (mode == 'highlight') {
    v = sub_tc$tokens[[highlight]]
    if (is.character(v) || is.factor(v)) v = !is.na(v)
    url = tokenbrowser::highlighted_browser(sub_tc$tokens, meta=meta, token_col=token_col, col = highlight_col, value=v, doc_nav = nav, header=header, subheader = subhead, filename=filename, n=F, top_nav=top_nav, thres_nav=thres_nav)
  }
  if (mode == 'scale') url = tokenbrowser::colorscaled_browser(sub_tc$tokens, meta=meta, token_col=token_col, col_range = scale_col, value=sub_tc$tokens[[scale]], alpha=0.3, doc_nav = nav, header=header, subheader = subhead, filename=filename, n=F, top_nav=top_nav, thres_nav=thres_nav)
  if (mode == 'category') {
    v = sub_tc$tokens[[category]]
    if (is.numeric(v)) v = as.character(v)
    url = tokenbrowser::categorical_browser(sub_tc$tokens, meta=meta, token_col=token_col, category = v, alpha=0.3, header=header, subheader = subhead, filename=filename, n=F, top_nav=top_nav, thres_nav=thres_nav)
  }
  if (view) tokenbrowser::view_browser(url)
  invisible(url)
}


#' View hits in a browser
#'
#' Creates a static HTML file to view the query hits in the tcorpus in full text mode.
#'
#' @param tc          a tCorpus
#' @param hits        a featureHits object, as returned by \link{search_features}
#' @param token_col   The name of the column in tc$tokens that contain the token text
#' @param n           If doc_ids is NULL, Only n of the results are printed (to prevent accidentally making huge browsers).
#' @param select      If n is smaller than the number of documents in tc, select determines how the n documents are selected
#' @param header      Optionally, a title presented at the top of the browser
#' @param subheader   Optionally, overwrite the subheader. By default the subheader reports the number of documents
#' @param meta_cols   A character vector with names of columns in tc$meta, used to only show the selected columns
#' @param seed        If select is "random", seed can be used to set a random seed
#' @param view        If TRUE (default), view the browser in the Viewer window (turn off if this is not supported)
#' @param filename    Optionally, save the browser at a specified location
#'
#' @return The url for the file location is returned (invisibly)
#' @export
#'
#' @examples
#' \donttest{
#' tc = create_tcorpus(sotu_texts, doc_column='id')
#' hits = search_features(tc, c("Terrorism# terroris*", "War# war*"))
#' browse_hits(tc, hits)
#' }
browse_hits <- function(tc, hits, token_col='token', n=500, select=c('first','random'), header='', subheader=NULL,
                      meta_cols=NULL, seed=NA, view=T, filename=NULL) {
  if (nrow(hits$hits) == 0) {
    message('Has zero hits')
    return(NULL)
  }

  .i = tc$get_token_id(doc_id = hits$hits$doc_id, token_id = hits$hits$token_id)
  .value = as.character(hits$hits$code)
  tc$set('#HITS#', value = .value, subset = .i, subset_value=F)

  b = browse_texts(tc, doc_ids = unique(hits$hits$doc_id), category='#HITS#', token_col=token_col, n=n, select=select, header=header, subheader=subheader, meta_cols=meta_cols, seed=seed, view=view, filename=filename)
  tc$delete_columns('#HITS#')
  invisible(b)
}

