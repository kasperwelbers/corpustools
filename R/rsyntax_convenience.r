
#' Visualize a dependency tree
#' 
#' A wrapper for the \code{\link[rsyntax]{plot_tree}} function, that can be used directly on a tCorpus.
#'
#' @param tc            a tCorpus
#' @param ...           Arguments passed to \code{\link[rsyntax]{plot_tree}}. Most importantly, this is used to select which specific columns to display on the bottom rows.
#'                      For instance, tc_plot_tree(tc, token, lemma, POS) shows only these three columns. 
#' @param annotation    Optionally, the name of a column with an rsyntax annotation.
#' @param sentence_i    By default, plot_tree uses the first sentence (sentence_i = 1) in the data. sentence_i can be changed to select other sentences by position (the i-th unique sentence in the data). Note that sentence_i does not refer to the values in the sentence column (for this use the sentence argument together with doc_id)
#' @param doc_id        Optionally, the document id can be specified. If so, sentence_i refers to the i-th sentence within the given document.
#' @param pdf_file      Directly save the plot as a pdf file
#'
#' @return
#' @export
#'
#' @examples
#' tc_plot_tree(tc_sotu_udpipe, token, lemma, POS)
tc_plot_tree <- function(tc, ..., annotation=NULL, sentence_i=1, doc_id=NULL, pdf_file=NULL) {
  rsyntax::plot_tree(tc$tokens, ..., annotation=annotation, sentence_i=sentence_i, doc_id=doc_id, pdf_file=pdf_file)  
}


#' Create a full text browser with highlighted rsyntax annotations
#' 
#' A wrapper for the \code{\link[rsyntax]{syntax_reader}} function, that can be used directly on a tCorpus, and adopts some of the features from \code{\link{browse_texts}}
#' Pro tip for fixing/improving tqueries: if you hover over the highlighted tokens, you see the annotation id. If you labeled your tqueries you can thus easily see which query was applied, and you can look up the id in your tokens data (annotation ids are unique at the corpus level) 
#'
#' @param tc            a tCorpus
#' @param annotation    Optionally, a column with an rsyntax annotation, to add boxes around the annotated nodes. 
#' @param value         A character vector with values used in the annotation column. If used, only these values are fully colored, and the other (non NA) values only have border colors. For example, if annotation distinguishes 'source', 'verb' and 'quote', then value = 'source' will highlight the source tokens and only color the borders of the 'verb' and 'quote' tokens'
#' @param value2        An extension of value. Normally, all (non NA) values that are not in value will be given border colors. With value2 you can specify which specific values to give border colors
#' @param doc_ids       A vector with document ids to view
#' @param token_col     The name of the column in tc$tokens that contain the token text
#' @param n             Only n of the results are printed (to prevent accidentally making huge browsers).
#' @param select        If n is smaller than the number of documents in tc, select determines how the n documents are selected
#' @param meta_cols     A character vector with names of columns in tc$meta, used to only show the selected columns
#' @param seed          If select is "random", seed can be used to set a random seed. After sampling the seed is re-initialized with set.seed(NULL).
#' @param header        Optionally, give a custom header
#' @param subheader     Optionally, give a custom subheader (overwriting some of the default information about n and sampling)
#' @param ...           Arguments passed to \code{\link[rsyntax]{syntax_reader}}
#'
#' @return
#' @export
#'
#' @examples
#' tc = tc_sotu_udpipe$copy()
#'
#' tc_syntax_reader(tc)
#' 
#' tc$udpipe_clauses()
#' tc_syntax_reader(tc, annotation='clause', value='subject',n=2)
tc_syntax_reader <- function(tc, annotation=NULL, value=NULL, value2=NULL, doc_ids=NULL, token_col='token', n=500, select=c('first','random'), meta_cols=NULL, seed=NA, header='', subheader=NULL, ...) {
  select = match.arg(select)
  
  if (is.null(annotation)) {
    return(browse_texts(tc, doc_ids=doc_ids, token_col=token_col, n=n, select=select, header=header, subheader=subheader, meta_cols=meta_cols, seed=seed))
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
  
  doc_id = NULL ## for cran check
  sub_tc = tc$subset_meta(doc_id %in% .DOC_IDS, copy = T)
  if (!is.null(subheader)) subhead = subheader
  
  if (!is.null(meta_cols)) {
    meta_cols = union('doc_id', meta_cols)
    if (!all(meta_cols %in% sub_tc$meta_names)) stop('Not all values in meta_cols are valid column in tc$meta')
    meta = subset(sub_tc$meta, select=meta_cols)
  } else meta = sub_tc$meta
  
  space_column = if ('space' %in% colnames(tc$tokens)) 'space' else NULL

  if ('coref_txt' %in% colnames(tc$tokens)) {
    has_coref = !is.na(sub_tc$tokens$coref_txt)
    sub_tc$tokens[[token_col]] = as.character(sub_tc$tokens[[token_col]])
    sub_tc$tokens[[token_col]][has_coref] = stringi::stri_paste(sub_tc$tokens[[token_col]][has_coref], ' [', sub_tc$tokens$coref_txt[has_coref], ']', sep='')
    sub_tc$tokens[[token_col]] = fast_factor(sub_tc$tokens[[token_col]])
  }

  rsyntax::syntax_reader(sub_tc$tokens, annotation=annotation, value=value, value2=value2, meta=tc$meta, token_col=token_col, space_col=space_column, ...) 
}
