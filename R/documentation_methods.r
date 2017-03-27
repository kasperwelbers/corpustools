## tCorpus R6 method documentation
#### Separate documentation for each method
#### Names for each class take the form tCorpus$method(...), with as additional alias method.tCorpus (S3 style)

#' Extract the data from a tCorpus
#'
#' @usage
#' ## R6 method for class tCorpus.
#' \code{tCorpus$data(columns=NULL, keep_df=F, as.df=F)}
#' \code{tCorpus$meta(columns=NULL, keep_df=F, as.df=F, per_token)}
#'
#' @param columns A character string indicating which columns to return. NULL means all columns.
#' @param keep_df Keep the data.table/data.frame if only one column is selected.
#' @param as.df Return a regular data.frame instead of a data.table
#' @param per_token Repeat rows in the document meta data so that it matches with the token data
#'
#' @method tCorpus
#' @name tCorpus$data
#' @aliases data.tCorpus tCorpus$meta meta.tCorpus
NULL


#' Create or extract a feature index
#'
#' @description
#' The feature index is a data.table with three columns: feature, i and global_i. The feature column is the data.table key, to enable fast lookup. The i column contains the indices of the feature in the token data.
#
#' The global_i represents the global positions of features, with gaps of a certain window_size between contexts (documents or sentences). This offers an efficient way to work with word windows. For example, if we want all tokens within a word window of 10, and the window_size is at least 10, then words from 2 different contexts can never occur in the same window.
#'
#' Once a feature_index is created, it is stored within the tCorpus. Then, if the tCorpus$feature_index method is called again, it will first be checked whether the existing feature_index can be used or whether a new one has to be created. The existing feature_index can be used if the parameters are the same, and the max_window_size is equal or lower to the max_window_size of the existing tCorpus. (Note: max_window_size will always be set to at least 100, which should be sufficient for most appliations. While technically max_window_size can be much higher, this can lead to very high integers, to the point where it can slow down or yields overflow errors)
#'
#' You can manually delete the feature_index that is stored in the tCorpus with the tCorpus$reset_feature_index() method.
#'
#' @usage
#' ## R6 method for class tCorpus
#' \code{tCorpus$feature_index(feature='word', context_level='document', max_window_size=100, as_ascii=F)}
#' \code{tCorpus$reset_feature_index()}
#'
#' @param feature The feature to be indexed.
#' @param context_level Select whether the context is document or sentence level. In the feature_index, this determines the global_i gaps.
#' @param max_window_size Determines the size of the global_i gaps between concepts. If lower than 100, a window size of 100 is still used (you may consider this a very strong recommendation).
#' @param as_ascii use the ascii version of the feature. Use with care (i.e. make sure to also use ascii when looking up features)
#'
#' @method tCorpus
#' @name tCorpus$feature_index
#' @aliases feature_index.tCorpus tCorpus$reset_feature_index reset_feature_index.tCorpus
NULL

#' Get a context vector
#'
#' Determining on the purpose, the context of an analysis can be the document level or sentence level (note: at some point we'll add paragraph level). the tCorpus$context() method offers a convenient way to get the context id of tokens for different settings.
#'
#' @usage
#' ## R6 method for class tCorpus.
#' \code{tCorpus$data(context_level = c('document','sentence'), with_labels=T)}
#'
#' @param context_level Select whether the context is document or sentence level
#' @param with_labels Return context as only ids (numeric, starting at 1) or with labels (factor)
#'
#' @method tCorpus
#' @name tCorpus$context
#' @aliases context.tCorpus
NULL

############### MODIFY DATA

#' Modify the token and meta data.tables of a tCorpus
#'
#' Modify the token/meta data.table using the \link{transform.data.table} function. Arguments are name-value pairs where the name indicates a new or existing column in the token data, to which the value is set. The value can be specified quite flexibly, and can directly call columns in the tokens data by name. For example, we can make a new column "word_low" by using the "word" column and making it lowercase: $transform_data(word_low = tolower(word)). Note that this can also be used to delete columns by setting their value to NULL: $transform_data(word_low = NULL).
#'
#' @usage
#' ## R6 method for class tCorpus
#' \code{tCorpus$transform(..., clone=self$clone_on_change, safe=T)}
#' \code{tCorpus$transform_meta(..., clone=self$clone_on_change, safe=T)}
#'
#' @param ... name-value pairs where the name indicates a new or existing column in the token data, to which the value is set.
#' @param clone If TRUE, the method returns a new tCorpus object. This is the normal R way of doing things. Alternatively, the tCorpus can be used as a reference class object by setting clone to FALSE, or setting tCorpus$clone_on_change to FALSE to use this globally. Please consult the general documentation for tCorpus (?tCorpus) for a more detailed explanation.
#' @param safe If TRUE, you are unable to modify the position columns (doc_id, sent_i, word_i), which is very likely to break the tCorpus. Only set to FALSE if you know exactly what you're doing.
#'
#' @method tCorpus
#' @name tCorpus$transform
#' @aliases transform.tCOrpus tCorpus$transform_meta transform_meta.tCorpus
NULL

#' Modify the token and meta data.tables of a tCorpus
#'
#' Modify the token/meta data.table using the \link{within.data.table} function. The main argument (expr) is one or multiple lines of R code between accolades, in which the columns of the token data can be modified as regular vector objects. For example: expr = {word_low = tolower(word)}. The main advantage of within_data compared to transform_data is that it enables modifying subsets of columns (for example: expr = {pos[pos == 'noun'] = 'N'}
#'
#' @usage
#' ## R6 method for class tCorpus
#' \code{tCorpus$within(expr, clone=self$clone_on_change, safe=T)}
#' \code{tCorpus$within_meta(expr, clone=self$clone_on_change, safe=T)}
#'
#' @param expr expression to be evaluated within the data.table.
#' @param clone If TRUE, the method returns a new tCorpus object. This is the normal R way of doing things. Alternatively, the tCorpus can be used as a reference class object by setting clone to FALSE, or setting tCorpus$clone_on_change to FALSE to use this globally. Please consult the general documentation for tCorpus (?tCorpus) for a more detailed explanation.
#' @param safe If TRUE, you are unable to modify the position columns (doc_id, sent_i, word_i), which is very likely to break the tCorpus. Only set to FALSE if you know exactly what you're doing.
#'
#' @method tCorpus
#' @name tCorpus$within
#' @aliases within.tCorpus tCorpus$within_meta within_meta.tCorpus
NULL

#' Modify the token and meta data.tables of a tCorpus
#'
#' Modify the token/meta data.table by setting the values of one (existing or new) column. This is less flexible than within data or transform data, but it has the advantage of allowing columns to be selected as a string, which makes it convenient for modifying the tCorpus from within function. The subset argument can be used to modify only subsets of columns, and can be a logical vector (select TRUE rows), numeric vector (indices of TRUE rows) or logical expression (e.g. pos == 'noun'). If A new column is made whie using a subset, then the rows outside of the selection are set to NA.
#'
#' @usage
#' ## R6 method for class tCorpus
#' \code{tCorpus$set_column(column, value, subset, clone=self$clone_on_change, safe=T)}
#' \code{tCorpus$set_meta_column(column, value, subset, clone=self$clone_on_change, safe=T)}
#'
#' @param column Name of a new column (to create) or existing column (to transform)
#' @param value A vector of the same length as the number of rows in the data. Note that if a subset is used, the length of value should be the same as the length of the subset (the TRUE cases of the subset expression) or a single value.
#' @param subset logical expression indicating rows to keep in the tokens data or meta data
#' @param clone If TRUE, the method returns a new tCorpus object. This is the normal R way of doing things. Alternatively, the tCorpus can be used as a reference class object by setting clone to FALSE, or setting tCorpus$clone_on_change to FALSE to use this globally. Please consult the general documentation for tCorpus (?tCorpus) for a more detailed explanation.
#' @param safe If TRUE, you are unable to modify the position columns (doc_id, sent_i, word_i), which is very likely to break the tCorpus. Only set to FALSE if you know exactly what you're doing.
#'
#' @method tCorpus
#' @name tCorpus$set_column
#' @aliases set_column.tCorpus tCorpus$set_meta_column set_meta_column.tCorpus
NULL

#' Subset a tCorpus
#'
#' @description
#' Returns the subset of a tCorpus. The selection can be made separately (and simultaneously) for the token data (using subset) and the meta data (using subset_meta). The subset arguments work according to the \link{subset.data.table} function.
#'
#' Subset can also be used to select rows based on word/feature frequences. This is a common step in corpus analysis, where it often makes sense to ignore very rare and/or very frequent words.
#' To do so, there are functions that can be used in the subset call to get frequency scores for a given feature column.
#' The freq(feature) and docfreq(feature) functions get the total frequency of a feature and the document frequency, respectively. The docfreq_pct(feature) function gets the document frequency as a percentage (between 0 and 100).
#' For example, if there is a feature called word, then the subset can be "freq(word) > 5 & docfreq_pct(word) < 90". This will select all rows for which the word is a word that occured at least 5 times, and did not occur in more dan 90% of all documents.
#'
#' In addition, you can use freq_top(feature, n=100) and docfreq_top(feature, n=100) to get the top n features. This is given as a logical vector.
#' For example, to select only the rows in which a word occurs that is in the top 100 most frequent words, subset can be "freq_top(word, 100)".
#'
#' Note that you can also use the \link{tCorpus$feature_subset} method if you want to filter out low/high frequency words, but do not want to delete the rows in the tCorpus.
#'
#' @usage
#' ## R6 method for class tCorpus
#' \code{tCorpus$subset(subset=NULL, subset_meta=NULL, drop_levels=F, window=NULL)}
#'
#' @param subset logical expression indicating rows to keep in the tokens data.
#' @param subset_meta logical expression indicating rows to keep in the document meta data.
#' @param drop_levels if TRUE, drop all unused factor levels after subsetting
#' @param window If not NULL, an integer specifiying the window to be used to return the subset. For instance, if the subset contains word 10 in a document and window is 5, the subset will contain word 5 to 15. Naturally, this does not apply to subset_meta.
#' @param clone If TRUE, the method returns a new tCorpus object. This is the normal R way of doing things. Alternatively, the tCorpus can be used as a reference class object by setting clone to FALSE, or setting tCorpus$clone_on_change to FALSE to use this globally. Please consult the general documentation for tCorpus (?tCorpus) for a more detailed explanation.
#'
#' @method tCorpus
#' @name tCorpus$subset
#' @aliases subset.tCorpus
NULL

#' Change column names in tCorpus data
#'
#' @usage
#' ## R6 method for class tCorpus.
#' \code{tCorpus$set_column(oldname, newname)}
#' \code{tCorpus$set_meta_column(oldname, newname)}
#'
#' @method tCorpus
#' @name tCorpus$set_column
#' @aliases set_column.tCorpus tCorpus$set_meta_column set_meta_column.tCorpus
NULL

#### preprocessing

#' Preprocess feature
#'
#' @usage
#' ## R6 method for class tCorpus
#' \code{tCorpus$preprocess(column, new_column=column, lowercase=T, ngrams=1, ngram_context=c('document', 'sentence'), as_ascii=F, remove_punctuation=T, remove_stopwords=F, use_stemming=F, language='english', clone=self$clone_on_change)}
#'
#' @param column the column containing the feature to be used as the input
#' @param new_column the column to save the preprocessed feature. Can be a new column or overwrite an existing one.
#' @param lowercase make feature lowercase
#' @param ngrams create ngrams. The ngrams match the rows in the token data, with the feature in the row being the last word of the ngram. For example, given the features "this is an example", the third feature ("an") will have the trigram "this_is_an". Ngrams at the beginning of a context will have empty spaces. Thus, in the previous example, the second feature ("is") will have the trigram "_is_an".
#' @param ngram_context Ngrams will not be created across contexts, which can be documents or sentences. For example, if the context_level is sentences, then the last word of sentence 1 will not form an ngram with the first word of sentence 2.
#' @param as_ascii convert characters to ascii. This is particularly usefull for dealing with special characters.
#' @param remove_punctuation remove (i.e. make NA) any features that are \emph{only} punctuation (e.g., dots, comma's)
#' @param remove_stopwords remove (i.e. make NA) stopwords. (!) Make sure to set the language argument correctly.
#' @param use_stemming reduce features (words) to their stem
#' @param language The language used for stopwords and stemming
#' @param clone If TRUE, the method returns a new tCorpus object. This is the normal R way of doing things. Alternatively, the tCorpus can be used as a reference class object by setting clone to FALSE, or setting tCorpus$clone_on_change to FALSE to use this globally. Please consult the general documentation for tCorpus (?tCorpus) for a more detailed explanation.
#'
#' @method tCorpus
#' @name tCorpus$preprocess
#' @aliases preprocess.tCorpus
NULL

#' Filter feature
#'
#' @description
#' Similar to using \link{tCorpus$subset}, but instead of deleting rows it only sets rows for a specified feature to NA. This can be very convenient, because it enables only a selection of features to be used in an analysis (e.g. a topic model) but maintaining the context of the full article, so that results can be viewed in this context (e.g. a topic browser).
#'
#' Just as in subset, it is easy to use objects and functions in the filter, including the special functions for using term frequency statistics (see documentation for \link{tCorpus$subset}).
#'
#' @usage
#' ## R6 method for class tCorpus
#' \code{tCorpus$feature_subset(column, new_column, subset, clone=self$clone_on_change)}
#'
#' @param column the column containing the feature to be used as the input
#' @param new_column the column to save the filtered feature. Can be a new column or overwrite an existing one.
#' @param subset logical expression indicating rows to keep in the tokens data. i.e. rows for which the logical expression is FALSE will be set to NA.
#' @param clone If TRUE, the method returns a new tCorpus object. This is the normal R way of doing things. Alternatively, the tCorpus can be used as a reference class object by setting clone to FALSE, or setting tCorpus$clone_on_change to FALSE to use this globally. Please consult the general documentation for tCorpus (?tCorpus) for a more detailed explanation.
#'
#' @method tCorpus
#' @name tCorpus$feature_subset
#' @aliases feature_subset.tCorpus
NULL

#' Feature statistics
#'
#' @description
#' Compute a number of useful statistics for features: term frequency, idf, etc.
#'
#' @usage
#' ## R6 method for class tCorpus
#' \code{tCorpus$feature_stats(feature, sent_freq=F)}
#'
#' @param feature The name of the feature
#' @param sent_freq If True, include sentence frequency (only if sentence information is available).
#'
#' @method tCorpus
#' @name tCorpus$feature_stats
#' @aliases feature_stats.tCorpus
NULL

#' Show top features
#'
#' @usage
#' ## R6 method for class tCorpus
#' \code{tCorpus$top_features(feature, n=10, group_by=NULL, group_by_meta=NULL, return_long=F)}
#'
#' @param feature The name of the feature
#' @param n Return the top n features
#' @param group_by A column in the token data to group the top features by. For example, if token data contains part-of-speech tags (pos), then grouping by pos will show the top n feature per part-of-speech tag.
#' @param group_by_meta A column in the meta data to group the top features by.
#' @param return_long if True, results will be returned in a long format. Default is a table, but this can be inconvenient if there are many grouping variables.
#'
#' @method tCorpus
#' @name tCorpus$top_features
#' @aliases top_features.tCorpus
NULL

#' Find tokens using a Lucene-like search query
#'
#' @description
#' Search tokens in a tokenlist using a query that consists of an keyword, and optionally a condition. For a detailed explanation of the query language please consult the query_tutorial markdown file. For a quick summary see the details below.
#'
#' Note that the query arguments (keyword, condition, code, condition_once) can be vectors to search multiple queries at once. Alternatively, the queries argument can be used to pass these arguments in a data.frame
#'
#' @usage
#' ## R6 method for class tCorpus
#' \code{tCorpus$search_features(keyword=NA, condition=NA, code=NA, queries=NULL, feature='word', condition_once=F, subset_tokens=NA, subset_meta=NA, keep_false_condition=F, only_last_mword=F, verbose=F)}
#'
#' @param keyword The keyword part of the query, see explanation in query_tutorial markdown or in details below
#' @param condition The condition part of the query, see explanation in query_tutorial markdown or in details below
#' @param code The code given to the tokens that match the query (usefull when looking for multiple queries)
#' @param queries Alternatively, a data.frame can be given that contains a "keyword" column, and optionally columns for the "condition", "code" and "condition_once" paramters.
#' @param feature The name of the feature column within which to search.
#' @param condition_once logical. If TRUE, then if an keyword satisfies its conditions once in an article, all keywords within that article are coded.
#' @param subset_tokens A call (or character string of a call) as one would normally pass to subset.tCorpus. If given, the keyword has to occur within the subset. This is for instance usefull to only look in named entity POS tags when searching for people or organization. Note that the condition does not have to occur within the subset.
#' @param subset_meta A call (or character string of a call) as one would normally pass to the subset_meta parameter of subset.tCorpus. If given, the keyword has to occur within the subset documents. This is for instance usefull to make queries date dependent. For example, in a longitudinal analysis of politicians, it is often required to take changing functions and/or party affiliations into account. This can be accomplished by using subset_meta = "date > xxx & date < xxx" (given that the appropriate date column exists in the meta data).
#' @param keep_false_condition if True, the keyword hits for which the condition was not satisfied are also returned, with an additional column that indicates whether the condition was satisfied. This can be used to investigate whether the condition is too strict, causing false negatives
#' @param only_last_mword If TRUE, then if multiword keywords are used (i.e. using double quotes, for instance "the united states"), only return the index of the last word. Note that if this is set to FALSE, it affects the occurence frequency, which is often a bad idea (e.g., counting search hits, word co-occurence analysis)
#' @param verbose
#'
#' @details
#' Brief summary of the query language
#'
#' The keyword:
#' \itemize{
#'    \item{is the actual feature that has to be found in the token}
#'    \item{can contain multiple words with OR statement (and empty spaces are also considered OR statements)}
#'    \item{can contain multiword strings, using quotes. e.g. "united states"}
#'    \item{can contain word proximities, using quotes plus tilde and a number specifiying the word distance. e.g. "climate chang*"~10}
#'    \item{accepts the ? wildcard, which means that any single character can be used in this place}
#'    \item{accepts the * wildcard, which means that any number of characters can be used in this place}
#'    \item{is be default not case sensitive, but can be made so by adding ~s. e.g. COP~s}
#'  }
#'
#' The condition:
#' \itemize{
#'    \item{has to be TRUE for the keyword to be accepted. Thus, if a condition is given, the query can be interpreted as: keyword AND condition}
#'    \item{works identical to the keyword, but with several additional options:}
#'    \item{- can also contain complex boolean statements, using AND, OR and NOT statements, and using parentheses}
#'    \item{- can be specified for a maximum word distance of the keyword using the ^ (caret) symbol, where "word^50" means that "word" is looked up within 50 words of the keyword. This can also be used after multiword strings, and in combination with the tilde. e.g. "climate chang*"~5^10 will check if the words climate and change/changing/etc. co-occur within 5 words, and if so, at least on word should occur within 10 words of the keyword}
#'    \item{- the case sensitive and word distance flags can be used together. e.g. COP~s^50 means that all capital COP must be found within 50 words of the keyword}
#' }
#'
#' Parameters:
#' \itemize{
#'    \item{condition_once -> if TRUE, then if the condition is satisfied at least once in an article, all occurences of the keyword are accepted. }
#' }
#'
#' @method tCorpus
#' @name tCorpus$search_features
#' @aliases search_features.tCorpus
NULL

#' Recode features in a tCorpus based on a search string
#'
#' @description
#' Search features (see \link{tCorpus$search_features}) and replace features with a new value
#'
#' @usage
#' ## R6 method for class tCorpus
#' \code{tCorpus$search_recode(feature, new_value, keyword, condition=NA, condition_once=F, subset_tokens=NA, subset_meta=NA, clone=self$clone_on_change)}
#'
#' @param feature The feature in which to search
#' @param new_value the character string with which all features that are found are replaced
#' @param ... See \link{tCorpus$search_features} for the query parameters
#' @param clone If TRUE, the method returns a new tCorpus object. This is the normal R way of doing things. Alternatively, the tCorpus can be used as a reference class object by setting clone to FALSE, or setting tCorpus$clone_on_change to FALSE to use this globally. Please consult the general documentation for tCorpus (?tCorpus) for a more detailed explanation.
#'
#' @method tCorpus
#' @name tCorpus$search_recode
#' @aliases search_recode.tCorpus
NULL

#' Get keyword-in-context (KWIC) strings
#'
#' @description
#' Create a data.frame with keyword-in-context strings for given indices (i), search results (hits) or search strings (keyword).
#'
#' @usage
#' ## R6 method for class tCorpus
#' \code{tCorpus$kwic(hits=NULL, i=NULL, keyword=NULL, code='', nwords=10, nsample=NA, output_feature='word', context_levels=c('document','sentence'), prettypaste=T, kw_tag=c('<','>'), ...)}
#'
#' @param hits results of feature search. see \link{tCorpus$search_features}.
#' @param i instead of the hits argument, you can give the indices of features directly.
#' @param keyword instead of using the hits or i arguments, a search string can be given directly. Note that this simply a convenient shorthand for first creating a hits object with \link{tCorpus$search_features}. If a keyword is given, then the ... argument is used to pass other arguments to \link{tCorpus$search_features}.
#' @param code if 'i' or 'keyword' is used, the code argument can be used to add a code label. Should be a vector of the same length that gives the code for each i or keyword, or a vector of length 1 for a single label.
#' @param nwords an integers specifying the size of the context, i.e. the number of words left and right of the keyword.
#' @param nsample optionally, get a random sample of the keywords/features. If multiple codes are used, the sample is drawn for each code individually.
#' @param output_feature the feature column that is used to make the KWIC.
#' @param context_level Select the maxium context (document or sentence).
#' @param prettypaste If TRUE, tries to reconstruct the text in a natural way (e.g. no space before a dot). Otherwise, all features are separated by a whitespace.
#' @param kw_tag a character vector of length 2, that gives the symbols before (first value) and after (second value) the keyword in the KWIC string. Can for instance be used to prepare KWIC with format tags for highlighting.
#' @param ... See \link{tCorpus$search_features} for the query parameters
#'
#' @method tCorpus
#' @name tCorpus$kwic
#' @aliases kwic.tCorpus
NULL



#' Search for documents or sentences using Boolean queries
#'
#' @usage
#' ## R6 method for class tCorpus
#' \code{tCorpus$search_contexts(query, code=NULL, feature='word', context_level=c('document','sentence'), verbose=F)}
#'
#' @param query A character string that is a query. See details for available query operators and modifiers. Can be multiple queries (as a vector), in which case it is recommended to also specifiy the code argument, to label results.
#' @param code If given, used as a label for the results of the query. Especially usefull if multiple queries are used.
#' @param feature The name of the feature column
#' @param context_level Select whether the queries should occur within while "documents" or specific "sentences". Returns results at the specified level.
#' @param verbose
#'
#' @details
#' Brief summary of the query language
#'
#' The following operators and modifiers are supported:
#' \itemize{
#'    \item{The standaard Boolean operators: AND, OR and NOT. As a shorthand, an empty space can be used as an OR statement, so that "this that those" means "this OR that OR those". NOT statements stricly mean AND NOT, so should only be used between terms. If you want to find \emph{everything except} certain terms, you can use * (wildcard for \emph{anything}) like this: "* NOT (this that those)".}
#'    \item{For complex queries parentheses can (and should) be used. e.g. '(spam AND eggs) NOT (fish and (chips OR albatros))}
#'    \item{Wildcards ? and *. The questionmark can be used to match 1 unknown character or no character at all, e.g. "?at" would find "cat", "hat" and "at". The asterisk can be used to match any number of unknown characters. Both the asterisk and questionmark can be used at the start, end and within a term.}
#'    \item{Multiword strings, or exact strings, can be specified using quotes. e.g. "united states"}
#'    \item{Words within a given word distance can be found using quotes plus tilde and a number specifiying the word distance. e.g. "climate chang*"~10}
#'    \item{Queries are not case sensitive, but can be made so by adding the ~s flag. e.g. COP~s only finds "COP" in uppercase. The ~s flag can also be used on quotes to make all terms within quotes case sensitive, and this can be combined with the word proximity flag. e.g. "Marco Polo"~s10}
#'  }
#'
#' @method tCorpus
#' @name tCorpus$search_contexts
#' @aliases search_contexts.tCorpus
NULL

#' Subset tCorpus token data using a query
#'
#' @description
#' A convenience function that searches for contexts (documents, sentences), and uses the results to \link[=tCorpus$search_contexts]{subset} the tCorpus token data.
#'
#' See the documentation for \link[=tCorpus$search_contexts]{subset} for an explanation of the query language.
#'
#' @usage
#' ## R6 method for class tCorpus
#' \code{tCorpus$subset_query(query, feature='word', context_level=c('document','sentence'), clone=self$clone_on_change)}
#'
#' @param clone If TRUE, the method returns a new tCorpus object. This is the normal R way of doing things. Alternatively, the tCorpus can be used as a reference class object by setting clone to FALSE, or setting tCorpus$clone_on_change to FALSE to use this globally. Please consult the general documentation for tCorpus (?tCorpus) for a more detailed explanation.
#'
#' @method tCorpus
#' @name tCorpus$subset_query
#' @aliases subset_query.tCorpus
NULL

## CO-OCCURRENCE NETWORKS ##

#' Create a semantic network based on the co-occurence of words in documents
#'
#' @description
#' This function calculates the co-occurence of features and returns a network/graph in the \link{igraph} format where nodes are words and edges represent the similarity/adjacency of words. Co-occurence is calcuated based on how often two words occured within the same document (e.g., news article, chapter, paragraph, sentence). The semnet_window() function can be used to calculate co-occurrence of words within a given word distance.
#'
#' @usage
#' ## R6 method for class tCorpus
#' \code{tCorpus$semnet(feature, measure=c('con_prob', 'con_prob_weighted', 'cosine', 'count_directed', 'count_undirected', 'chi2'), context_level=c('document','sentence'), backbone=F, n.batches=NA)}
#'
#' @param feature The name of the feature column
#' @param measure The similarity measure. Currently supports: "con_prob" (conditional probability), "con_prob_weighted", "cosine" similarity, "count_directed" (i.e number of cooccurrences) and "count_undirected" (same as count_directed, but returned as an undirected network, chi2 (chi-square score))
#' @param context_level Determine whether features need to co-occurr within "documents" or "sentences"
#' @param backbone If True, add an edge attribute for the backbone alpha
#' @param n.batches If a number, perform the calculation in batches
#'
#' @method tCorpus
#' @name tCorpus$semnet
#' @aliases semnet.tCorpus
NULL

#' Create a semantic network based on the co-occurence of words in word windows
#'
#' @description
#' This function calculates the co-occurence of features and returns a network/graph in the \link{igraph} format where nodes are words and edges represent the similarity/adjacency of words. Co-occurence is calcuated based on how often two words co-occurr within a given word distance.
#'
#' @usage
#' ## R6 method for class tCorpus
#' \code{tCorpus$semnet_window(feature, measure=c('con_prob', 'cosine', 'count_directed', 'count_undirected', 'chi2'), context_level=c('document','sentence'), window.size=10, direction='<>', backbone=F, n.batches=5, set_matrix_mode=c(NA, 'windowXwindow', 'positionXwindow'))}
#'
#' @param feature The name of the feature column
#' @param measure The similarity measure. Currently supports: "con_prob" (conditional probability), "cosine" similarity, "count_directed" (i.e number of cooccurrences) and "count_undirected" (same as count_directed, but returned as an undirected network, chi2 (chi-square score))
#' @param context_level Determine whether features need to co-occurr within "documents" or "sentences"
#' @param window.size The word distance within which features are considered to co-occurr
#' @param direction Determine whether co-occurrence is assymmetricsl ("<>") or takes the order of words into account. If direction is '<', then the from/x feature needs to occur before the to/y feature. If direction is '>', then after.
#' @param backbone If True, add an edge attribute for the backbone alpha
#' @param n.batches If a number, perform the calculation in batches
#' @param set_matrix_mode Advanced feature. There are two approaches for calculating window co-occurrence. One is to measure how often a feature occurs within a given word window, which can be calculating by calculating the inner product of a matrix that contains the exact position of features and a matrix that contains the occurrence window. We refer to this as the "positionXwindow" mode. Alternatively, we can measure how much the windows of features overlap, for which take the inner product of two window matrices. By default, semnet_window takes the mode that we deem most appropriate for the similarity measure. Substantially, the positionXwindow approach has the advantage of being very easy to interpret (e.g. how likely is feature "Y" to occurr within 10 words from feature "X"?). The windowXwindow mode, on the other hand, has the interesting feature that similarity is stronger if words co-occurr more closely together (since then their windows overlap more). Currently, we only use the windowXwindow mode for cosine similarity. By using the set_matrix_mode parameter you can override this.
#'
#' @method tCorpus
#' @name tCorpus$semnet_window
#' @aliases semnet_window.tCorpus
NULL

## RESOURCES ##

#' Multilingual named entity recognition using the JRC-NAMES resource
#'
#' @description
#' "JRC-Names is a highly multilingual named entity resource for person and organisation names. [...] JRC-Names is a by-product of the analysis of about 220,000 news reports per day by the Europe Media Monitor (EMM) family of applications." (https://ec.europa.eu/jrc/en/language-technologies/jrc-names)}
#'
#' The resource needs to be downloaded first. For this you can use the download_resource() function, which will (by default) download the resource into the tcorpus package folder.
#'
#' @usage
#' ## R6 method for class tCorpus
#' \code{tCorpus$jrc_names(new_feature='jrc_names', feature='word', resource_path=getOption('tcorpus_resources', NULL), collocation_labels=T, batchsize=50000, low_memory=T, verbose=T, clone=self$clone_on_change)}
#'
#' @param new_feature The column name of the new feature.
#' @param feature The feature to be used as input. For JRC names regular (unprocessed) words should be used.
#' @param resource_path The path (without the filename) where the resource is stored. See ?download_resource for more information.
#' @param collocation_labels if True, then for resources that create an id for subsequent words (e.g. named entities), labels are added (in a separate column) based on the most frequent collocation combinations in 'your' data. Note that this means that the labels can be different if you run the same analysis on a different corpus; this is why the id is always kept.
#' @param batchsize The number of named entity string variations per batch. Using bigger batches is faster, but depending on the size of your corpus you might run out of memory (in which case you should use smaller batches). At the time of writing the total number of strings is roughtly 700,000.
#' @param low_memory if TRUE (default) then data will be sorted in a way that tries to get a roughly equal number of string matches per batch, to prevent huge match tables (costing memory). If FALSE, data will be sorted in a way to get fewer unique words per batch, which can speed up matching, but can lead to a very unequal number of matches per batch.
#' @param verbose
#' @param clone If TRUE, the method returns a new tCorpus object. This is the normal R way of doing things. Alternatively, the tCorpus can be used as a reference class object by setting clone to FALSE, or setting tCorpus$clone_on_change to FALSE to use this globally. Please consult the general documentation for tCorpus (?tCorpus) for a more detailed explanation.
#'
#' @method tCorpus
#' @name tCorpus$jrc_names
#' @aliases jrc_names.tCorpus
NULL
