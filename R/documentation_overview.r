## R6 classes are a bit tricky to document. Thats why we use a somewhat unconventional approach
## The tCorpus documentation page serves as a general overview for tcorpus related functions and methods
## This r file contains this overview, including the subpages
## each method is documented separately in the documentation_methods r file

#' tCorpus: a corpus class for tokenized texts
#'
#' The tCorpus is a class for managing tokenized texts, stored as a data.frame in which each row represents a token, and columns contain the positions and features of these tokens.
#'
#' @section Methods and Functions:
#'
#' The corpustools package uses both functions and methods for working with the tCorpus.
#'
#' Methods are used for all operations that modify the tCorpus itself, such as subsetting or adding columns.
#' This allows the data to be \link[=tCorpus_modify_by_reference]{modified by reference}.
#' Methods are accessed using the dollar sign after the tCorpus object. For example, if the tCorpus is named tc, the subset method can be called as tc$subset(...)
#'
#' Functions are used for all operations that return a certain output, such as search results or a semantic network.
#' These are used in the common R style that you know and love. For example, if the tCorpus is named tc, a semantic network can be created with semnet(tc, ...)
#'
#' @section Overview of methods and functions:
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

#' Methods and functions for viewing, modifying and subsetting tCorpus data
#'
#' \link[=tCorpus]{(back to overview)}
#'
#' \strong{Get data}
#' \tabular{ll}{
#'   \link[=tCorpus$get]{$get()} \tab Get (by default deep copy) token data, with the possibility to select columns and subset.
#'                                    Instead of copying you can also access the token data with tc$tokens \cr
#'   \link[=tCorpus$get]{$get_meta()} \tab Get meta data, with the possibility to select columns and subset. Like tokens, you can also
#'                                   access meta data with tc$meta  \cr
#'   \link[=get_dtm]{get_dtm()} \tab Create a document term matrix \cr
#'   \link[=get_dfm]{get_dfm()} \tab Create a document term matrix, using the Quanteda dfm format \cr
#'   \link[=tCorpus$context]{$context()} \tab Get a context vector. Currently supports documents or globally unique sentences.
#' }
#'
#' \strong{Modify}
#'
#' The token and meta data can be modified with the set* and delete* methods. All modifications are performed by reference.
#'
#'
#' \tabular{ll}{
#'   \link[=tCorpus$set]{$set()} \tab Modify the token data by setting the values of one (existing or new) column. \cr
#'   \link[=tCorpus$set]{$set_meta()} \tab The set method for the document meta data \cr
#'   \link[=tCorpus$set_levels]{$set_levels()} \tab Change the levels of factor columns. \cr
#'   \link[=tCorpus$set_meta_levels]{$set_meta_levels()} \tab Change the levels of factor columns in the meta data \cr
#'   \link[=tCorpus$set_name]{$set_name()} \tab Modify column names of token data. \cr
#'   \link[=tCorpus$set_meta_name]{$set_meta_name()} \tab Delete columns in the meta data \cr
#'   \link[=tCorpus$delete_columns]{$delete_columns()} \tab Delete columns.  \cr
#'   \link[=tCorpus$delete_meta_columns]{$delete_meta_columns()} \tab Delete columns in the meta data
#' }
#'
#' Modifying is restricted in certain ways to ensure that the data always meets the assumptions required for tCorpus methods.
#' tCorpus automatically tests whether assumptions are violated, so you don't have to think about this yourself.
#' The most important limitations are that you cannot subset or append the data.
#' For subsetting, you can use the \link{tCorpus$subset} method, and to add data to a tcorpus you can use the \link{merge_tcorpora} function.
#'
#' \strong{Subsetting, merging/adding}
#' \tabular{ll}{
#'   \link[=subset]{subset()} \tab Modify the token and/or meta data using the \link{subset} function. A subset expression can be specified for both the token data (subset) and the document meta data (subset_meta). \cr
#'   \link[=subset_query]{subset_query()} \tab  Subset the tCorpus based on a query, as used in \link[=search_contexts]{search_contexts} \cr
#'   \link[=tCorpus$subset]{$subset()} \tab Like subset, but as an R6 method that changes the tCorpus by reference \cr
#'   \link[=tCorpus$subset_query]{$subset_query()} \tab Like subset_query, but as an R6 method that changes the tCorpus by reference
#' }
#'
#'
#' \strong{Fields}
#'
#' For the sake of convenience, the number of rows and column names of the data and meta data.tables can be accessed directly.
#'
#' \tabular{ll}{
#'   $n \tab The number of tokens (i.e. rows in the data) \cr
#'   $n_meta \tab The number of documents (i.e. rows in the document meta data) \cr
#'   $names \tab The names of the token data columns \cr
#'   $names_meta \tab The names of the document meta data columns
#' }
#'
#' @name tCorpus_data
NULL


#' Modify tCorpus by reference
#'
#' \link[=tCorpus]{(back to overview)}
#'
#' If any tCorpus method is used that changes the corpus (e.g., set, subset),
#' the change is made by reference. This is convenient when working with a large
#' corpus, because it means that the corpus does not have to be copied when changes are made,
#' which is slower and less memory efficient.
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
#'
#' Using the R6 method for subset forces this approach on you, because it is faster and more memory efficient.
#' If you do want to make a copy, there are several solutions.
#'
#' Firstly, for some methods we provide identical functions. For example, instead of the $subset() R6 method,
#' we can use the subset() function.
#'
#' \strong{tc2 = subset(tc, doc_id \%in\% selection)}
#'
#' We promise that only the R6 methods (called as tc$method()) will change the data by reference.
#'
#' A second option is that R6 methods where copying is often usefull have copy parameter
#' Modifying by reference only happens in the R6 methods
#'
#' \strong{tc2 = tc$subset(doc_id \%in\% selection, copy=TRUE)}
#'
#' Finally, you can always make a deep copy of the entire tCorpus before modifying it, using the $copy() method.
#'
#' \strong{tc2 = tc$copy()}
#'
#' @name tCorpus_modify_by_reference
NULL

#' Preprocessing, subsetting and analyzing features
#'
#' \link[=tCorpus]{(back to overview)}
#'
#' \strong{Pre-process features}
#' \tabular{ll}{
#'   \link[=tCorpus$preprocess]{$preprocess()} \tab Create or modify a feature by preprocessing an existing feature \cr
#'   \link[=tCorpus$feature_subset]{$feature_subset()} \tab Similar to using subset, but instead of deleting rows it only sets rows for a specified feature to NA.
#' }
#' \strong{Inspect features}
#' \tabular{ll}{
#'   \link[=feature_stats]{feature_stats()} \tab Create a data.frame with feature statistics \cr
#'   \link[=top_features]{top_features()} \tab Show top features, optionally grouped by a given factor
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
#'   \link[=search_features]{search_features())} \tab Search for features based on keywords and conditions \cr
#'   \link[=tCorpus$code_features]{$code_features())} \tab  Add a column to the token data based on feature search results \cr
#'   \link[=tCorpus$search_recode]{$search_recode()} \tab Use the search_features query syntax to recode features \cr
#'   \link[=feature_associations]{feature_associations()} \tab Given a query, get words that often co-occur nearby \cr
#'   \link[=get_kwic]{kwic()} \tab Get keyword-in-context (kwic) strings \cr
#'   \link[=browse_hits]{browse_hits()} \tab Create full-text browsers with highlighted search hits
#' }
#' \strong{Context-level queries}
#' \tabular{ll}{
#'   \link[=search_contexts]{search_contexts()} \tab Search for documents or sentences using Lucene-like queries \cr
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
#'   \link[=semnet]{semnet)} \tab Feature co-occurrence within contexts (documents, sentences) \cr
#'   \link[=semnet_window]{semnet_window()} \tab Feature co-occurrence within a specified token distance
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
#' \strong{Compare vocabulary of two corpora}
#' \tabular{ll}{
#'   \link[=compare_corpus]{compare_corpus()} \tab Compare vocabulary of one tCorpus to another \cr
#'   \link[=compare_subset]{compare_subset()} \tab Compare subset of a tCorpus to the rest of the tCorpus
#' }
#'
#' @name tCorpus_compare
NULL

#' Topic modeling
#'
#' \link[=tCorpus]{(back to overview)}
#'
#' \strong{Train a topic model}
#' \tabular{ll}{
#'   \link[=tCorpus$lda_fit]{$lda_fit()} \tab Latent Dirichlet Allocation
#' }
#'
#' @name tCorpus_topmod
NULL

#' Document similarity
#'
#' \link[=tCorpus]{(back to overview)}
#'
#' \strong{Compare documents, and perform similarity based deduplication}
#' \tabular{ll}{
#'   \link[=compare_documents]{compare_documents()} \tab Compare documents \cr
#'   \link[=tCorpus$deduplicate]{$deduplicate()} \tab Remove duplicate documents
#' }
#'
#' @name tCorpus_docsim
NULL


