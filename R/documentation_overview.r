## R6 classes are a bit tricky to document. That's why we use a somewhat unconventional approach
## The tCorpus documentation page serves as a general overview for tcorpus related functions and methods
## This r file contains this overview, including the subpages
## each method is documented separately in the documentation_methods r file

#' tCorpus: a corpus class for tokenized texts
#'
#' @section Working with the tCorpus:
#' The primary goal of the tCorpus is to facilitate various corpus analysis techniques. The documentation for currently implemented techniques can be reached through the following links.
#' \tabular{ll}{
#'   \link[=tCorpus_create]{Create a tCorpus} \tab Functions for creating a tCorpus object \cr
#'   \link[=tCorpus_data]{Manage tCorpus data} \tab Methods for viewing, modifying and subsetting tCorpus data \cr
#'   \link[=tCorpus_features]{Features} \tab Preprocessing, subsetting and analyzing features \cr
#'   \link[=tCorpus_querying]{Using search strings} \tab Use Boolean queries to analyze the tCorpus \cr
#'   \link[=tCorpus_semnet]{Co-occurrence networks}  \tab Feature co-occurrence based semantic network analysis \cr
#'   \link[=tCorpus_compare]{Corpus comparison} \tab Compare corpora \cr
#'   \link[=tCorpus_topmod]{Topic modeling}  \tab Create and visualize topic models \cr
#'   \link[=tCorpus_docsim]{Document similarity} \tab Calculate document similarity \cr
#'   \link[=tCorpus_resources]{Language resources} \tab An overview of external language resource plugins
#' }
#'
#' @name tCorpus
#' @aliases tcorpus
#' @export
NULL

#' Creating a tCorpus
#'
#' \link[=tCorpus]{(back to overview)}
#'
#' \strong{Create a tCorpus}
#' \tabular{ll}{
#'   \link[=create_tcorpus]{create_tcorpus()} \tab Create a tCorpus from raw text input \cr
#'   \link[=tokens_to_tcorpus]{tokens_to_tcorpus()} \tab Create a tCorpus from a data.frame of already tokenized texts
#' }
#'
#' @name tCorpus_create
NULL

#' Methods for viewing, modifying and subsetting tCorpus data
#'
#' \link[=tCorpus]{(back to overview)}
#'
#' \strong{Fields}
#' \tabular{ll}{
#'   $n \tab The number of tokens (i.e. rows in the data) \cr
#'   $n_meta \tab The number of documents (i.e. rows in the document meta data) \cr
#'   $names \tab The names of the token data columns \cr
#'   $names_meta \tab The names of the document meta data columns
#' }
#'
#' \strong{Get data}
#' \tabular{ll}{
#'   \link[=tCorpus$data]{$data} \tab Get the token data (in \link{data.table} format). Can also be used to modify data using regular assignment, but with some restrictions (see Modify data section) \cr
#'   \link[=tCorpus$meta]{$meta} \tab Get the document meta data \cr
#'   \link[=tCorpus$get]{$get} \ \tab Get specific columns (faster and more efficient than $data, which first copies the whole data.table)  \cr
#'   \link[=tCorpus$get]{$get_meta} \ \tab Get specific meta data columns  \cr
#'   \link[=tCorpus$dtm]{$dtm()} \tab Create a document term matrix \cr
#'   \link[=tCorpus$feature_index]{$feature_index()} \tab Create or load a feature index. Used for fast lookup of features. \cr
#'   \link[=tCorpus$context]{$context()} \tab Get a context vector. Currently supports documents or globally unique sentences.
#' }
#'
#' \strong{Modify}
#'
#' There are several ways to modify the data and meta data in tCorpus.
#' The fastest and most memory efficient way is to use the set and set_meta methods, which support modify by reference (this is not the default, see documentation for \link[=tCorpus_reference_mode]{Reference mode}).
#' A less efficient but more convenient approach is to directly assign to the $data and $meta, which can be used as a regular data.table (though note that assignment by reference is not possible)
#'
#' Modifying is restricted in certain ways to ensure that the data always meets the assumptions required for tCorpus methods.
#' tCorpus automatically tests whether assumptions are violated, so you don't have to think about this yourself.
#' The most important limitations are that you cannot subset or append the data this way, and you cannot change the document ids.
#' For this, you can use the appropriate methods (subset, merge_tcorpus, doc_id_levels).
#'
#' \tabular{ll}{
#'   \link[=tCorpus$set]{$set()} \tab Modify the token data by setting the values of one (existing or new) column. \cr
#'   \link[=tCorpus$set]{$set_meta()} \tab The set method for the document meta data
#'   \link[=tCorpus$subset]{$subset()} \tab Modify the token and/or meta data using the \link{subset.tCorpus} function. A subset expression can be specified for both the token data (subset) and the document meta data (subset_meta). \cr
#'   \link[=tCorpus$subset_query]{$subset_query()} \tab to be added
#' }
#'
#' \strong{Check and clean}
#' \tabular{ll}{
#'   \link[=tCorpus$feature_index]{$reset_feature_index()} \tab Reset the feature index. (free up memory) \cr
#'   $set_keys() \tab Check and (re)set the data.table keys. Should not be necessary, but pointed out here till certain. \cr
#'   $droplevels() \tab Drop the unused factor levels in the token and meta data.
#' }
#'
#' @name tCorpus_data
NULL

#' Preprocessing, subsetting and analyzing features
#'
#' \link[=tCorpus]{(back to overview)}
#'
#' \strong{Pre-process features}
#' \tabular{ll}{
#'   \link[=tCorpus$preprocess]{$preprocess()} \tab Create or modify a feature by preprocessing an existing feature \cr
#'   \link[=tCorpus$feature_subset]{$filter()} \tab Similar to using subset, but instead of deleting rows it only sets rows for a specified feature to NA.
#' }
#' \strong{Inspect features}
#' \tabular{ll}{
#'   \link[=tCorpus$feature_stats]{$feature_stats()} \tab Create a data.frame with feature statistics \cr
#'   \link[=tCorpus$top_features]{$top_features()} \tab Show top features, optionally grouped by a given factor
#' }
#'
#' @name tCorpus_features
NULL

#' Use Boolean queries to analyze the tCorpus
#'
#' \link[=tCorpus]{(back to overview)}
#'
#' \strong{Feature-level queries}
#' \tabular{ll}{
#'   \link[=tCorpus$search_features]{$search_features)} \tab Search for features based on keywords and conditions \cr
#'   \link[=tCorpus$search_recode]{$search_recode()} \tab Use the search_features query syntax to recode features \cr
#'   \link[=tCorpus$kwic]{$kwic()} \tab Get keyword-in-context (kwic) strings
#' }
#' \strong{Context-level queries}
#' \tabular{ll}{
#'   \link[=tCorpus$search_contexts]{$search_contexts()} \tab Search for documents or sentences using Lucene-like queries \cr
#'   \link[=tCorpus$search_subset]{$search_subset()} \tab use the search_contexts query syntax to subset the tCorpus
#' }
#'
#' @name tCorpus_querying
NULL

#' Feature co-occurrence based semantic network analysis
#'
#' \link[=tCorpus]{(back to overview)}
#'
#' \strong{Create networks}
#' \tabular{ll}{
#'   \link[=tCorpus$semnet]{$semnet)} \tab Feature co-occurrence within contexts (documents, sentences) \cr
#'   \link[=tCorpus$semnet_window]{$semnet_window()} \tab Feature co-occurrence within a specified word distance
#' }
#' \strong{Support functions for analyzing and visualizing the semantic network}
#' \tabular{ll}{
#'   \link[=ego_semnet]{ego_semnet()} \tab Create an ego network from an Igraph network \cr
#'   \link[=plot_semnet]{plot_semnet()} \tab Convenience function for visualizing an Igraph network, specialized for semantic networks
#' }
#'
#' @name tCorpus_semnet
NULL

#' Corpus comparison
#'
#' \link[=tCorpus]{(back to overview)}
#'
#' @name tCorpus_compare
NULL

#' Topic modeling
#'
#' \link[=tCorpus]{(back to overview)}
#'
#' @name tCorpus_topmod
NULL

#' Document similarity
#'
#' \link[=tCorpus]{(back to overview)}
#'
#' @name tCorpus_docsim
NULL

#' Language resources
#'
#' \link[=tCorpus]{(back to overview)}
#'
#' \strong{Named entity recognition}
#' \tabular{ll}{
#'   \link[=tCorpus$jrc_names]{$jrc_names()} \tab Multilingual named entity recognition (in news items) \cr
#' }
#'
#' @name tCorpus_resources
NULL



