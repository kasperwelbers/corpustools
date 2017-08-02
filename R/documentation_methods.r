## tCorpus R6 method documentation (which is a bit unorthodox)
#### Separate documentation for each method.
#### This only contains the accessor method documentation. Other methods documentation is included in the R files with the code (e.g., dtm documentation in document_term_matrix.r)
#### Names for each class take the form tCorpus$method(...), with as additional alias method.tCorpus (S3 style)

#' Access the data from a tCorpus
#'
#' @description
#' Get the token and meta data.
#'
#' @section Usage:
#' ## R6 active method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{get(columns=NULL, keep_df=F, as.df=F, subset=NULL, doc_id=NULL, token_i=NULL, safe_copy=T)}
#' \preformatted{get_meta(columns=NULL, keep_df=F, as.df=F, subset=NULL, doc_id=NULL, safe_copy=T)}
#'
#' @param a character vector with the names of the columns
#' @param keep_df if True, the output will be a data.table (or data.frame) even if it only contains 1 columns
#' @param as.df if True, the output will be a regular data.frame instead of a data.table
#' @param subset Optionally, only get a subset of rows (see \link{subset.tCorpus} method)
#' @param doc_id A vector with document ids to select rows. Faster than subset, because it uses binary search. Cannot be used in combination with subset. If duplicate doc_ids are given, duplicate rows are returned.
#' @param token_i A vector with token indices. Can only be used in pairs with doc_id. For example, if doc_id = c(1,1,1,2,2) and token_i = c(1,2,3,1,2), then the first three tokens of doc 1 and the first 2 tokens of doc 2 are returned. This is mainly usefull for fast (binary search) retrieval of specific tokens.
#' @param safe_copy for advanced use. The get methods always return a copy of the data, even if the full data is returned (i.e. use get without parameters). This is to prevent accidental changes within tCorpus data (which can break it) if the returned data is modified by reference (see data.table documentation). If safe_copy is set to FALSE and get is called without parameters---tc$get(safe_copy=F))---then no copy is made, which is much faster and more memory efficient. Use this if you need speed and efficiency, but make sure not to change the output data.table by reference.
#'
#' @name tCorpus$get
#' @aliases get.tCorpus tCorpus$get_meta get_meta.tCorpus
NULL

#' Create or extract a feature index
#'
#' @description
#' The feature index is a data.table with three columns: feature, i and global_i. The feature column is the data.table key, to enable fast lookup. The i column contains the indices of the feature in the token data.
#
#' The global_i represents the global positions of features, with gaps of a certain window_size between contexts (documents or sentences). This offers an efficient way to work with token windows. For example, if we want all tokens within a token window of 10, and the window_size is at least 10, then tokens from 2 different contexts can never occur in the same window.
#'
#' Once a feature_index is created, it is stored within the tCorpus. Then, if the tCorpus$feature_index method is called again, it will first be checked whether the existing feature_index can be used or whether a new one has to be created. The existing feature_index can be used if the parameters are the same, and the max_window_size is equal or lower to the max_window_size of the existing tCorpus. (Note: max_window_size will always be set to at least 100, which should be sufficient for most appliations. While technically max_window_size can be much higher, this can lead to very high integers, to the point where it can slow down or yields overflow errors)
#'
#' You can manually delete the feature_index that is stored in the tCorpus with the tCorpus$reset_feature_index() method.
#'
#' @section Usage:
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{feature_index(feature = 'token', context_level = 'document', max_window_size = 100, as_ascii=F)}
#' \preformatted{reset_feature_index()}
#'
#' @param feature The feature to be indexed.
#' @param context_level Select whether the context is document or sentence level. In the feature_index, this determines the global_i gaps.
#' @param max_window_size Determines the size of the global_i gaps between concepts. If lower than 100, a window size of 100 is still used (you may consider this a very strong recommendation).
#' @param as_ascii use the ascii version of the feature. Use with care (i.e. make sure to also use ascii when looking up features)
#'
#' @name tCorpus$feature_index
#' @aliases feature_index.tCorpus tCorpus$reset_feature_index reset_feature_index.tCorpus
NULL

#' Get a context vector
#'
#' Determining on the purpose, the context of an analysis can be the document level or sentence level (note: at some point we'll add paragraph level). the tCorpus$context() method offers a convenient way to get the context id of tokens for different settings.
#'
#' @section Usage:
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{data(context_level = c('document','sentence'), with_labels = T)}
#'
#' @param context_level Select whether the context is document or sentence level
#' @param with_labels Return context as only ids (numeric, starting at 1) or with labels (factor)
#'
#' @name tCorpus$context
#' @aliases context.tCorpus
NULL



############### MODIFY DATA

#' Modify the token and meta data.tables of a tCorpus
#'
#' Modify the token/meta data.table by setting the values of one (existing or new) column. This is less flexible than within data or transform data, but it has the advantage of allowing columns to be selected as a string, which makes it convenient for modifying the tCorpus from within function. The subset argument can be used to modify only subsets of columns, and can be a logical vector (select TRUE rows), numeric vector (indices of TRUE rows) or logical expression (e.g. pos == 'noun'). If A new column is made whie using a subset, then the rows outside of the selection are set to NA.
#'
#' @section Usage:
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{set(column, value, subset)}
#' \preformatted{set_meta(column, value, subset)}
#'
#' @param column Name of a new column (to create) or existing column (to transform)
#' @param value A vector of the same length as the number of rows in the data. Note that if a subset is used, the length of value should be the same as the length of the subset (the TRUE cases of the subset expression) or a single value.
#' @param subset logical expression indicating rows to keep in the tokens data or meta data
#'
#' @name tCorpus$set
#' @aliases set.tCorpus tCorpus$set_meta set_meta.tCorpus
NULL

#' Change levels of factor columns
#'
#' @section Usage:
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{set_levels(column, levels)}
#' \preformatted{set_meta_levels(column, levels)}
#'
#' @param column the name of the column
#' @param levels The new levels
#'
#' @name tCorpus$set_levels
#' @aliases set_levels.tCorpus tCorpus$set_meta_levels set_meta_levels.tCorpus
NULL


#' Change column names of data and meta data
#'
#' @section Usage:
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{set_colname(oldname, newname)}
#' \preformatted{set_meta_colname(oldname, newname)}
#'
#' @param oldname the current/old column name
#' @param newname the new column name
#'
#' @name tCorpus$set_colname
#' @aliases set_colname.tCorpus tCorpus$set_meta_colname set_meta_colname.tCorpus
NULL

#' Delete column from the data and meta data
#'
#' @section Usage:
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{delete_columns(cnames)}
#' \preformatted{delete_meta_columns(cnames)}
#'
#' @param cnames the names of the columns to delete
#'
#' @name tCorpus$delete_columns
#' @aliases delete_columns.tCorpus tCorpus$delete_meta_columns delete_meta_columns.tCorpus
NULL

#' Subset a tCorpus
#'
#' @description
#' Returns the subset of a tCorpus. The selection can be made separately (and simultaneously) for the token data (using subset) and the meta data (using subset_meta). The subset arguments work according to the \link{subset.data.table} function.
#'
#' Subset can also be used to select rows based on token/feature frequences. This is a common step in corpus analysis, where it often makes sense to ignore very rare and/or very frequent tokens.
#' To do so, there are several special functions that can be used within a subset call.
#' The freq_filter() and docfreq_filter() can be used to filter terms based on term frequency and document frequency, respectively.
#' The first argument to these functions is the name of the feature, e.g., freq_filter(token).
#' To filter this feature you can specify: min (minimum frequency), max (maximum frequency), top (n most frequent tokens) and bottom (n least frequent tokens).
#' For example, tc$freq_filter(token, min=10) filters out all rows in which the specified feature occures at least 10 times in the entire corpus.
#' freq_filter(token, top=100) keeps only the rolws in which the feature is in the top 100 most frequent features.
#' docfreq_filter(token, max = 0.9 * tc$n) deletes all rows with features that occured in more than 90% of all documents.
#'
#' The subset_meta() method is an alternative for using subset(subset_meta = ...), that is added for consistency with the other _meta accessor methods.
#'
#' Note that you can also use the \link{tCorpus$feature_subset} method if you want to filter out low/high frequency tokens, but do not want to delete the rows in the tCorpus.
#'
#' @section Usage:
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{
#' subset(subset = NULL, subset_meta = NULL,
#'        window = NULL, copy = F)
#' subset_meta(subset = NULL, copy = F)
#'              }
#'
#' @param subset logical expression indicating rows to keep in the tokens data.
#' @param subset_meta logical expression indicating rows to keep in the document meta data.
#' @param window If not NULL, an integer specifiying the window to be used to return the subset. For instance, if the subset contains token 10 in a document and window is 5, the subset will contain token 5 to 15. Naturally, this does not apply to subset_meta.
#' @param copy If TRUE, the method returns a new tCorpus object instead of subsetting the current one. This is added for convenience when analyzing a subset of the data. e.g., tc_nyt = tc$subset_meta(medium == "New_York_Times", copy=T)
#'
#' @name tCorpus$subset
#' @aliases subset.tCorpus tCorpus$subset subset_meta.tCorpus tCorpus$subset_meta
NULL

#' Change column names in tCorpus data
#'
#' @section Usage:
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{set(oldname, newname)}
#' \preformatted{set_meta(oldname, newname)}
#'
#' @name tCorpus$set
#' @aliases set.tCorpus tCorpus$set_meta set_meta.tCorpus
NULL
