
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
#' @return      plots a dependency tree.
#' @export
#'
#' @examples
#' if (interactive()) 
#'   tc_plot_tree(tc_sotu_udpipe, token, lemma, POS)
tc_plot_tree <- function(tc, ..., annotation=NULL, sentence_i=1, doc_id=NULL, pdf_file=NULL) {
  rsyntax::plot_tree(tc$tokens, ..., annotation=annotation, sentence_i=sentence_i, doc_id=doc_id, pdf_file=pdf_file)  
}

