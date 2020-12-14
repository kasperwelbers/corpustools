
#' Annotate tokens based on rsyntax queries
#'
#' @description 
#' Apply queries to extract syntax patterns, and add the results as three columns to a tokenlist.
#' The first column contains the ids for each hit. The second column contains the annotation label. The third column contains the fill level (which you probably won't use, but is important for some functionalities).
#' Only nodes that are given a name in the tquery (using the 'label' parameter) will be added as annotation.
#' 
#' Note that while queries only find 1 node for each labeld component of a pattern (e.g., quote queries have 1 node for "source" and 1 node for "quote"), 
#' all children of these nodes can be annotated by settting fill to TRUE. If a child has multiple ancestors, only the most direct ancestors are used (see documentation for the fill argument).
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{
#' annotate_rsyntax(column, ..., block = NULL, fill = TRUE, 
#'                  overwrite = FALSE, block_fill = FALSE, copy = TRUE, 
#'                  verbose = FALSE)}
#'
#' @param column      The name of the column in which the annotations are added. The unique ids are added as column_id
#' @param ...         One or multiple tqueries, or a list of queries, as created with \link{tquery}. Queries can be given a named by using a named argument, which will be used in the annotation_id to keep track of which query was used. 
#' @param block       Optionally, specify ids (doc_id - sentence - token_id triples) that are blocked from querying and filling (ignoring the id and recursive searches through the id). 
#' @param fill        Logical. If TRUE (default) also assign the fill nodes (as specified in the tquery). Otherwise these are ignored 
#' @param overwrite   Applies if column already exists. If TRUE, existing column will be overwritten. If FALSE, the existing annotations in the column will be blocked, and new annotations will be added. This is identical to using multiple queries.
#' @param block_fill  If TRUE (and overwrite is FALSE), the existing fill nodes will also be blocked. In other words, the new annotations will only be added if the 
#' @param verbose     If TRUE, report progress (only usefull if multiple queries are given)
#' 
#' @name tCorpus$annotate_rsyntax
#' @aliases annotate_rsyntax
#' @examples
#' library(rsyntax)
#' 
#' ## spacy tokens for: Mary loves John, and Mary was loved by John
#' tokens = tokens_spacy[tokens_spacy$doc_id == 'text3',]
#' tc = tokens_to_tcorpus(tokens)
#' 
#' ## two simple example tqueries
#' passive = tquery(pos = "VERB*", label = "predicate",
#'                  children(relation = c("agent"), label = "subject"))
#' active =  tquery(pos = "VERB*", label = "predicate",
#'                  children(relation = c("nsubj", "nsubjpass"), label = "subject"))
#' 
#' tc$annotate_rsyntax("clause", pas=passive, act=active)
#' tc$tokens
#' 
#' if (interactive()) {
#' plot_tree(tc$tokens, annotation='clause')
#' }
#' if (interactive()) {
#' syntax_reader(tc$tokens, annotation = 'clause', value='subject')
#' }
tCorpus$set('public', 'annotate_rsyntax', function(column, ..., block=NULL, fill=TRUE, overwrite=NA, block_fill=FALSE, verbose=FALSE) {
  if (column %in% self$names && is.na(overwrite)) stop(sprintf('The specified column (%s) already exists. Set overwrite argument to TRUE to overwrite the column or FALSE to consider existing annotations as a chain.', column))
  cnames = paste0(column, c('','_id','_fill'))
  ti = rsyntax::annotate_tqueries(self$tokens, column = column, ..., block = block, fill = fill, overwrite = overwrite, block_fill = block_fill, copy=T, verbose=F) 
  ti = subset(ti, select = c('doc_id','token_id',cnames))
  for (cn in cnames) if (cn %in% self$names) self$set(cn, NULL)
  self$tokens = merge(self$tokens, ti, by=c('doc_id','token_id'))
  self$validate_tokens()
  invisible(self$tokens[])
})


#' Fold rsyntax annotations
#'
#' @description 
#' If a tCorpus has rsyntax annotations (see \code{\link{{annotate_rsyntax}}}), it can be convenient to aggregate tokens that have a certain semantic label.
#' For example, if you have a query for labeling "source" and "quote", you can add an aggegated value for the sources (such as a unique ID) as a column, and then remove the quote tokens. 
#' 
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{
#' fold_rsyntax(annotation, by_label, ..., 
#'              to_label=NULL, rm_by=T, copy=F)}
#'
#' @param annotation    The name of an rsyntax annotation column
#' @param by_label      The labels in this column for which you want to aggregate the tokens
#' @param ...           Specify the new aggregated columns in name-value pairs. The name is the name of the new column, and the value should be a function over a column in $tokens. 
#'                      For example:  subject = paste(token, collapse = ' ')  would create the column 'subject', of which the values are the concatenated tokens. See examples for more.
#' @param txt           If TRUE, add _txt column with concatenated tokens for by_label
#' @param rm_by         If TRUE (default), remove the column(s) specified in by_label
#' @param copy          If TRUE, return a copy of the transformed tCorpus, instead of transforming the tCorpus by reference
#' 
#' @name tCorpus$fold_rsyntax
#' @aliases fold_rsyntax
#' @examples
#' tc = tc_sotu_udpipe$copy()
#' tc$udpipe_clauses()
#' 
#' tc$fold_rsyntax('clause', by_label = 'subject', subject = paste(token, collapse=' '))
#' tc$tokens
tCorpus$set('public', 'fold_rsyntax', function(annotation, by_label, ..., txt=F, rm_by=T, copy=F) {
  if (copy) {
    selfcopy = self$copy()$aggregate_rsyntax(annotation=annotation, by=by, ..., to=to, copy=F)
    return(selfcopy)
  }

  .annotation = annotation
  .annotation_id = paste0(annotation, '_id')
  if (!all(c(.annotation,.annotation_id) %in% self$names)) stop('annotation does not refer to a valid rsyntax annotation column (see annotate_rsyntax)')

  .by_label = by_label
  .is_label = self$tokens[list(.by_label),,on=.annotation, which=T]
  agg_cols = self$tokens[.is_label, eval(substitute(list(...))), by = c('doc_id', 'sentence', .annotation_id)]
  
  if (txt) { 
    txt_col = paste0(paste(.by_label, collapse='_'), '_txt')
    if (nrow(agg_cols) == 0) {
      agg_cols = tokens[.is_label, list(.txt = paste(token, collapse=' ')), by = c('doc_id', 'sentence', .annotation_id)]
      data.table::setnames(agg_cols, '.txt', txt_col)
    }
    else agg_cols[[txt_col]] = tokens[.is_label, list(.txt = paste(token, collapse=' ')), by = c('doc_id', 'sentence', .annotation_id)]$.txt
  }
  
  if (rm_by) self$tokens = self$tokens[!1:nrow(self$tokens) %in% .is_label,]
  
  self$tokens = merge(self$tokens, agg_cols, by=c('doc_id', 'sentence', .annotation_id), all=T, sort = F)
  
  self$validate_tokens()
  invisible(self)
})

#' Fold rsyntax annotations
#'
#' @description 
#' If a tCorpus has rsyntax annotations (see \code{\link{{annotate_rsyntax}}}), it can be convenient to aggregate tokens that have a certain semantic label.
#' For example, if you have a query for labeling "source" and "quote", you can add an aggegated value for the sources (such as a unique ID) as a column, and then remove the quote tokens. 
#'
#' @param tc            A tCorpus
#' @param annotation    The name of an rsyntax annotation column
#' @param by_label      The labels in this column for which you want to aggregate the tokens
#' @param ...           Specify the new aggregated columns in name-value pairs. The name is the name of the new column, and the value should be a function over a column in $tokens. 
#'                      For example:  subject = paste(token, collapse = ' ')  would create the column 'subject', of which the values are the concatenated tokens. See examples for more.
#' @param txt           If TRUE, add _txt column with concatenated tokens for by_label. 
#' @param rm_by         If TRUE (default), remove the column(s) specified in by_label
#'
#' @return  a transformed tCorpus
#' @export
#'
#' @examples
#' tc = tc_sotu_udpipe$copy()
#' tc$udpipe_clauses()
#' 
#' fold_rsyntax(tc, 'clause', by_label = 'subject', subject = paste(token, collapse=' '))
fold_rsyntax <- function(tc, annotation, by_label, ..., txt=F, rm_by=T) {
  tc$fold_rsyntax(annotation=annotation,by_label=by_label, ..., txt=txt, rm_by=rm_by, copy=T)
}

#' Helper function for aggregate_rsyntax
#'
#' This function is used within the \code{\link{aggregate_rsyntax}} function to facilitate aggregating by specific labels. 
#'
#' @param label     The rsyntax label. Needs to be an existing value in the annotation column (as specified when calling \code{\link{aggregate_rsyntax}})
#' @param ...       Specify the new aggregated columns in name-value pairs. The name is the name of the new column, and the value should be a function over a column in $tokens. 
#'                  For example:  subject = paste(token, collapse = ' ')  would create the column 'subject', of which the values are the concatenated tokens. See examples for more.
#'
#' @return  Not relevant. Should only be used within \code{\link{aggregate_rsyntax}}
#' @export
#'
#' @examples
#' tc = tc_sotu_udpipe
#' tc$udpipe_clauses()
#' 
#' ## count number of tokens in predicate
#' aggregate_rsyntax(tc, 'clause', txt=F,
#'                   agg_label('predicate', n = length(token_id)))
agg_label <- function(label, ...) {
  list(label=label, agg_list = substitute(list(...)))
}

#' Aggregate rsyntax annotations
#'
#' @description 
#' A method for aggregating rsyntax annotations. The intended purpose is to compute aggregate values for a given label in an annotation column.
#' 
#' For example, you used annotate_rsyntax to add a column with subject-predicate labels, and now you want to concatenate the tokens with these labels.
#' With annotate_rsyntax you would first aggregate the subject tokens, then aggregate the predicate tokens. By default (txt = T) the column with concatenated tokens are added.
#' 
#' You can specify any aggregation function using any column in tc$tokens. So say you want to perform a sentiment analysis on the quotes of politicians. You first used annotate_rsyntax to create an annotation column 'quote',
#' that has the labels 'source', 'verb', and 'quote'. You also used code_dictionary to add a column with unique politician ID's and a column with sentiment scores.
#' Now you can aggregate the source tokens  to get a single unique ID, and aggregate the quote tokens to get a single sentiment score.
#'
#' @param tc               a tCorpus
#' @param annotation       The name of the rsyntax annotation column
#' @param ...              To aggregate columns for specific 
#' @param by_col           A character vector with other column names in tc$tokens to aggregate by.
#' @param txt              If TRUE, add columns with concatenated tokens for each label. Can also be a character vector specifying for which specific labels to create this column 
#' @param labels           Instead of using all labels, a character vector of labels can be given
#' @param rm_na            If TRUE, remove rows with only NA values
#'
#' @return    A data.table
#' @export
#'
#' @examples
#' tc = tc_sotu_udpipe
#' tc$udpipe_clauses()
#' 
#' subject_verb_predicate = aggregate_rsyntax(tc, 'clause', txt=TRUE)
#' head(subject_verb_predicate)
#' 
#' ## We can also add specific aggregation functions
#' 
#' ## count number of tokens in predicate
#' aggregate_rsyntax(tc, 'clause',
#'                   agg_label('predicate', n = length(token_id)))
#'                   
#' ## same, but with txt for only the subject label
#' aggregate_rsyntax(tc, 'clause', txt='subject',
#'                   agg_label('predicate', n = length(token_id)))
#' 
#'                                     
#' ## example application: sentiment scores for specific subjects
#' 
#' # first use queries to code subjects
#' tc$code_features(column = 'who',
#'                  query  = c('I#  I~s <this president>', 
#'                             'we# we americans <american people>'))
#' 
#' # then use dictionary to get sentiment scores
#' dict = melt_quanteda_dict(quanteda::data_dictionary_LSD2015)
#' dict$sentiment = ifelse(dict$code %in% c('negative','neg_positive'), -1, 1)
#' tc$code_dictionary(dict)
#' 
#' sent = aggregate_rsyntax(tc, 'clause', txt='predicate',
#'                   agg_label('subject', subject = na.omit(who)[1]),
#'                   agg_label('predicate', sentiment = mean(sentiment, na.rm=TRUE)))
#' head(sent)
#' sent[,list(sentiment=mean(sentiment, na.rm=T), n=.N), by='subject']
aggregate_rsyntax <- function(tc, annotation, ..., by_col=NULL, txt=F, labels=NULL, rm_na=T) {
  token = NULL
  tokens = if (methods::is(tc, 'tCorpus')) tc$tokens else tc
  
  .annotation = annotation
  .annotation_id = paste0(.annotation, '_id')
  if (!all(c(.annotation,.annotation_id) %in% colnames(tokens))) stop('annotation does not refer to a valid rsyntax annotation column (see annotate_rsyntax)')
  
  l = list(...)
  lname = sapply(l, function(x) x$label)
  
  if (is.null(labels)) labels = unique(tokens[[.annotation]])
  
  if (methods::is(txt, 'logical')) {
    txt = if (txt) labels else c()
  }
  
  out = unique(tokens[,c('doc_id','sentence', .annotation_id,by_col),with=F])
  if (!is.null(by_col))
    .drop = Matrix::rowSums(is.na(out[,c(.annotation_id,by_col),with=F])) == (length(by_col) + 1)   ## if all cols are NA
  else
    .drop = is.na(out[[.annotation_id]])
  out = out[!.drop,]
  
  for (.label in labels) {
    if (is.na(.label)) next
    .is_label = tokens[list(.label),,on=.annotation, which=T]
    if (length(.is_label) == 0) next
    
    if (.label %in% lname) {
      .by = l[[which(lname == .label)]]
      agg_cols = tokens[.is_label, eval(.by$agg_list), by = c('doc_id', 'sentence', .annotation_id, by_col)]
    } else agg_cols = NULL
    
    if (.label %in% txt) { 
      txt_col = paste0(paste(.label, collapse='_'), '_txt')
      if (is.null(agg_cols)) {
        agg_cols = tokens[.is_label, list(.txt = paste(token, collapse=' ')), by = c('doc_id', 'sentence', .annotation_id, by_col)]
        data.table::setnames(agg_cols, '.txt', txt_col)
      }
      else agg_cols[[txt_col]] = tokens[.is_label, list(.txt = paste(token, collapse=' ')), by = c('doc_id', 'sentence', .annotation_id, by_col)]$.txt
    }
    if (!is.null(agg_cols))
      out = merge(out, agg_cols, by=c('doc_id', 'sentence',.annotation_id, by_col), all=T, allow.cartesian = T, nomatch=0)
  }
  out
}


function() {
  tc$fold_rsyntax('quote', 'subject', )
  aggregate_rsyntax(tc, 'clause', agg_label('predicate', n = length(token_id)))
                    
  tc = udpipe_tcorpus('Steve left when Bob said that he likes wine when the sun is shining')
  tc$udpipe_clauses()
  tc$udpipe_quotes()
  tc$tokens
  tc$tokens[tc$tokens$token_id >= 10, c('clause','clause_id') := list(NA,NA)]
  aggregate_rsyntax(tc, 
                    agg_by('quote','source', txt=T),
                    agg_by('quote','verb', txt=T),
                    agg_by('clause', 'subject', txt=T),
                    agg_by('clause', 'verb', txt=T),
                    agg_by('clause','predicate',txt=T))
  
  
  tc$agg_rsyntax('clause', 'subject') 
  aggregate_rsyntax(tc,
                    tc$agg_rsyntax('clause', 'subject'),
                    tc$agg_rsyntax('quote', 'source') 
                    
  )
}






#' Apply rsyntax transformations
#'
#' This is an experimental function for applying rsyntax transformations directly on a tcorpus,
#' to create a new tcorpus with the transformed tokens. The argument f should be self defined function
#' that wraps rsyntax transformations. Or more generally, a function that takes a tokens data.frame (or data.table) as input, and returns a tokens data.frame (or data.table). 
#' For examples, see corpustools:::ud_relcl, or corpustools::udpipe_simplify for a function that wraps multiple transformations.
#'
#' @param tc    a tCorpus
#' @param f     functions that perform rsyntax tree transformations
#' @param ...   arguments passed to f
#'
#' @return a tCorpus after applying the transformations
#' @export
#'
#' @examples
#' tc = tc_sotu_udpipe$copy()
#' tc2 = transform_rsyntax(tc, udpipe_simplify)
#' 
#' browse_texts(tc2)
#' if (interactive()) {
#'    rsyntax::plot_tree(tc$tokens, token, lemma, POS, sentence_i=20)
#'    rsyntax::plot_tree(tc2$tokens, token, lemma, POS, sentence_i=20)
#' }
transform_rsyntax <- function(tc, f, ...) {
  tokens = as_tokenindex(tc$tokens)
  tokens = f(tokens, ...)
  tokens_to_tcorpus(tokens, meta=tc$meta, model=paste(tc$model))
}



