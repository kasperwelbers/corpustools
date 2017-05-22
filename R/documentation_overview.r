## R6 classes are a bit tricky to document. Thats why we use a somewhat unconventional approach
## The tCorpus documentation page serves as a general overview for tcorpus related functions and methods
## This r file contains this overview, including the subpages
## each method is documented separately in the documentation_methods r file

#' tCorpus: a corpus class for tokenized texts
#'
#' @section Working with the tCorpus:
#'
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
#' \strong{Get data}
#' \tabular{ll}{
#'   \link[=tCorpus$data]{$data} \tab Access the token data (in \link{data.table} format). Can also be used to modify data using regular assignment, but with some restrictions (see Modify data section) \cr
#'   \link[=tCorpus$meta]{$meta} \tab Access the document meta data \cr
#'   \link[=tCorpus$get]{$get()} \tab Get specific columns (faster and more efficient than $data, which first copies the whole data.table)  \cr
#'   \link[=tCorpus$get]{$get_meta()} \tab Get specific meta data columns  \cr
#'   \link[=tCorpus$dtm]{$dtm()} \tab Create a document term matrix \cr
#'   \link[=tCorpus$feature_index]{$feature_index()} \tab Create or load a feature index. Used for fast lookup of features. \cr
#'   \link[=tCorpus$context]{$context()} \tab Get a context vector. Currently supports documents or globally unique sentences.
#' }
#'
#' \strong{Modify}
#'
#' There are several ways to modify the data and meta data in tCorpus.
#' The less efficient but more classic approach is to directly assign to the $data and $meta, which can be used as regular data.tables (though note that assignment by reference is not possible)
#' The fastest and most memory efficient way is to use the set_* and delete_* methods, which support modify by reference (see documentation for \link[=tCorpus_always_copy]{always_copy}).
#'
#' \tabular{ll}{
#'   \link[=tCorpus$set]{$set()} \tab Modify the token data by setting the values of one (existing or new) column. Modifies by reference if always_copy is set to FALSE \cr
#'   \link[=tCorpus$set]{$set_meta()} \tab The set method for the document meta data \cr
#'   \link[=tCorpus$set_levels]{$set_levels()} \tab Change the levels of factor columns. Modifies by reference if always_copy is set to FALSE \cr
#'   \link[=tCorpus$set_meta_levels]{$set_meta_levels()} \tab Change the levels of factor columns in the meta data \cr
#'   \link[=tCorpus$set_colname]{$set_colname()} \tab Modify column names of token data. Modifies by reference if always_copy is set to FALSE  \cr
#'   \link[=tCorpus$set_meta_colname]{$set_meta_colname()} \tab Delete columns in the meta data \cr
#'   \link[=tCorpus$delete_columns]{$delete_columns()} \tab Delete columns. Modifies by reference if always_copy is set to FALSE  \cr
#'   \link[=tCorpus$delete_meta_columns]{$delete_meta_columns()} \tab Delete columns in the meta data
#' }
#'
#' Modifying is restricted in certain ways to ensure that the data always meets the assumptions required for tCorpus methods.
#' tCorpus automatically tests whether assumptions are violated, so you don't have to think about this yourself.
#' The most important limitations are that you cannot subset or append the data.
#' For this, you can use the subset and append methods, or merge tCorpora together.
#'
#' \strong{Subsetting, merging/adding}
#' \tabular{ll}{
#'   \link[=tCorpus$subset]{$subset()} \tab Modify the token and/or meta data using the \link{subset.tCorpus} function. A subset expression can be specified for both the token data (subset) and the document meta data (subset_meta). \cr
#'   \link[=tCorpus$subset]{$subset_meta()} \tab For consistency with other *_meta methods \cr
#'   \link[=tCorpus$subset_query]{$subset_query()} \tab Subset the tCorpus based on a query, as used in \link[=tCorpus$search_contexts]{$search_contexts}
#' }
#'
#'
#' \strong{Fields}
#'
#' For the sake of convenience, the number of rows and column names of the data and meta data.tables can be accessed directly. This is also faster and more memory efficient than using nrows() and colnames() on the data and meta fields, because those have to copy the data.tables.
#'
#' \tabular{ll}{
#'   $n \tab The number of tokens (i.e. rows in the data) \cr
#'   $n_meta \tab The number of documents (i.e. rows in the document meta data) \cr
#'   $names \tab The names of the token data columns \cr
#'   $names_meta \tab The names of the document meta data columns
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


#' Modify tCorpus by reference
#'
#' \link[=tCorpus]{(back to overview)}
#'
#' By default, if any tCorpus method is used that changes the corpus (e.g., set, subset),
#' it makes this change by reference. This is very convenient when working with a large
#' corpus, because it means that the corpus does not have to be copied when changes are made,
#' which slows things down, and can cause memory issues.
#'
#' To illustrate, for a tCorpus object named `tc`, the subset method can be called like this:
#'
#' \strong{tc$subset(doc_id \%in\% selection)}
#'
#' The `tc` object itself is now modified, and does not have to be assigned to a name, as would be the more
#' common R philosophy. Like this:
#'
#' \strong{tc = tc$subset(doc_id \%in\% selection)}
#'
#' The results of both lines of code are the same. The assignment in the second approach is not necessary,
#' but doesn't harm either because tc$subset returns the modified corpus invisibly (see ?invisible if that sounds spooky).
#'
#' Be aware, however, that the following does not work!!
#'
#' \strong{tc2 = tc$subset(doc_id \%in\% selection)}
#'
#' In this case, tc2 does contain the subsetted corpus, but tc itself will also be subsetted!!
#' If you want to make a subset (or use any of the other methods that modify the corpus) and also keep the original,
#' you can use the copy parameter.
#'
#' \strong{tc2 = tc$subset(doc_id \%in\% selection, copy=T)}
#'
#' Now, tc will not be subsetted itself, but will subset a copy of itself and return it to be assigned to tc2.
#'
#' Note that tc is also modified by reference if the subset method (or any other method that modified the corpus)
#' is called within a function. No matter where and how you call the method, tc itself will be subsetted unless you
#' explicitly set copy to True.
#'
#' Alternatively, you can also set copy = T to be the default, in which case the tCorpus works more
#' according to the classic R philosophy. To do so, you can set tc$always_copy to True
#'
#' \strong{tc$always_copy = T}
#'
#' This, however is not recommended, especially when working with a huge corpus. As explained above,
#' changing by reference is much faster and more memory efficient.
#'
#' @name tCorpus_always_copy
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
#'   \link[=tCorpus$subset_query]{$subset_query()} \tab use the search_contexts query syntax to subset the tCorpus
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



