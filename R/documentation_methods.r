## tCorpus R6 method documentation (which is a bit unorthodox)
#### Separate documentation for each method.
#### This only contains the accessor method documentation. Other methods documentation is included in the R files with the code (e.g., dtm documentation in document_term_matrix.r)
#### Names for each class take the form tCorpus$method(...), with as additional alias method.tCorpus (S3 style)

#' Access the data from a tCorpus
#'
#' @description
#' Get (a copy of) the token and meta data. For quick access recommend using tc$tokens and tc$meta to get the tokens and meta data.tables, which does not copy the data.
#' However, you should then make sure to not change the data.tables by reference, or you might break the tCorpus.
#'
#' \strong{Usage:}
#'
#' ## R6 active method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{get(columns=NULL, keep_df=F, as.df=F, subset=NULL, doc_id=NULL, token_id=NULL, safe_copy=T)}
#' \preformatted{get_meta(columns=NULL, keep_df=F, as.df=F, subset=NULL, doc_id=NULL, safe_copy=T)}
#'
#' @param columns character vector with the names of the columns
#' @param keep_df if True, the output will be a data.table (or data.frame) even if it only contains 1 columns
#' @param as.df if True, the output will be a regular data.frame instead of a data.table
#' @param subset Optionally, only get a subset of rows (see \link{tCorpus$subset} method)
#' @param doc_id A vector with document ids to select rows. Faster than subset, because it uses binary search. Cannot be used in combination with subset. If duplicate doc_ids are given, duplicate rows are returned.
#' @param token_id A vector with token indices. Can only be used in pairs with doc_id. For example, if doc_id = c(1,1,1,2,2) and token_id = c(1,2,3,1,2), then the first three tokens of doc 1 and the first 2 tokens of doc 2 are returned. This is mainly usefull for fast (binary search) retrieval of specific tokens.
#' @param safe_copy for advanced use. The get methods always return a copy of the data, even if the full data is returned (i.e. use get without parameters). This is to prevent accidental changes within tCorpus data (which can break it) if the returned data is modified by reference (see data.table documentation). If safe_copy is set to FALSE and get is called without parameters---tc$get(safe_copy=F))---then no copy is made, which is much faster and more memory efficient. Use this if you need speed and efficiency, but make sure not to change the output data.table by reference.
#'
#' @name tCorpus$get
#' @aliases tCorpus$get_meta get get_meta
#'
#' @examples
#' d = data.frame(text = c('Text one first sentence. Text one second sentence', 'Text two'),
#'                medium = c('A','B'),
#'                date = c('2010-01-01','2010-02-01'),
#'                doc_id = c('D1','D2'))
#' tc = create_tcorpus(d, split_sentences = TRUE)
#'
#' ## get token data
#' tc$tokens                     ## full data.table
#' tc$get(c('doc_id','token'))  ## data.table with selected columns
#' head(tc$get('doc_id'))       ## single column as vector
#' head(tc$get(as.df = TRUE))      ## return as regular data.frame
#'
#' ## get subset
#' tc$get(subset = token_id %in% 1:2)
#'
#' ## subset on keys using (fast) binary search
#' tc$get(doc_id = 'D1')              ## for doc_id
#' tc$get(doc_id = 'D1', token_id = 5) ## for doc_id / token pairs
#'
#'
#' ##### use get for meta data with get_meta
#' tc$meta
#'
#' ## option to repeat meta data to match tokens
#' tc$get_meta(per_token = TRUE) ## (note that first doc is repeated, and rows match tc$n)
#'
NULL

#' Get a context vector
#'
#' Depending on the purpose, the context of an analysis can be the document level or sentence level. the tCorpus$context() method offers a convenient way to get the context id of tokens for different settings.
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{data(context_level = c('document','sentence'), with_labels = T)}
#'
#' @param context_level Select whether the context is document or sentence level
#' @param with_labels Return context as only ids (numeric, starting at 1) or with labels (factor)
#'
#' @name tCorpus$context
#' @aliases context
#' @examples
#' tc <- create_tcorpus(c('Text one first sentence. Text one second sentence', 'Text two'),
#'                      split_sentences = TRUE)
#'
#' doc <- tc$context() ## default context is doc_id (document level)
#' doc
#'
#' sent <- tc$context('sentence') ## can specify sentence level
#' sent
NULL

############### MODIFY DATA



#' Modify the token and meta data.tables of a tCorpus
#'
#' Modify the token/meta data.table by setting the values of one (existing or new) column. The subset argument can be used to modify only subsets of columns, and can be a logical vector (select TRUE rows), numeric vector (indices of TRUE rows) or logical expression (e.g. pos == 'noun'). If a new column is made whie using a subset, then the rows outside of the selection are set to NA.
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{set(column, value, subset)}
#' \preformatted{set_meta(column, value, subset)}
#'
#' @param column Name of a new column (to create) or existing column (to transform)
#' @param value An expression to be evaluated within the token/meta data, or a vector of the same length as the number of rows in the data. Note that if a subset is used, the length of value should be the same as the length of the subset (the TRUE cases of the subset expression) or a single value.
#' @param subset logical expression indicating rows to keep in the tokens data or meta data
#' @param subset_value If subset is used, should value also be subsetted? Default is TRUE, which is what you want if
#'                     the value has the same length as the full data.table (which is the case if a column in tokens is used).
#'                     However, if the vector of values is already of the length of the subset, subset_value should be FALSE
#'
#'
#' @name tCorpus$set
#' @aliases tCorpus$set_meta set set_meta
#' @examples
#' tc = create_tcorpus(sotu_texts[1:5,], doc_column = 'id')
#'
#' tc$tokens  ## show original
#'
#' ## create new column
#' i <- 1:tc$n
#' tc$set(column = 'i', i)
#' ## create new column based on existing column(s)
#' tc$set(column = 'token_upper', toupper(token))
#' ## use subset to modify existing column
#' tc$set('token', paste0('***', token, '***'), subset = token_id == 1)
#' ## use subset to create new column with NA's
#' tc$set('second_token', token, subset = token_id == 2)
#'
#' tc$tokens  ## show after set
#'
#'
#' ##### use set for meta data with set_meta
#' tc$set_meta('party_pres', paste(party, president, sep=': '))
#' tc$meta
NULL

#' Merge the token and meta data.tables of a tCorpus with another data.frame
#'
#' Add columns to token/meta by merging with a data.frame df. Only possible for unique matches (i.e. the columns specified in by are unique in df)
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{merge(df, by, by.x, by.y)}
#' \preformatted{merge_meta(df, by, by.x, by.y)}
#'
#' @param df        A data.frame (can be regular, data.table or tibble)
#' @param by        The columns to match on. Must exist in both tokens/meta and df. If the columns in tokens/meta and df have different names, use by.x and by.y
#' @param by.x      The names of the columns used in tokens/meta
#' @param by.y      The names of the columns used in df
#' @param columns   Optionally, specify which specific columns from df to merge to tokens
#'
#' @name tCorpus$merge
#' @aliases tCorpus$merge merge merge_meta
#' @examples
#' d = data.frame(text = c('This is an example. Best example ever.', 'oh my god', 'so good'),
#'                id = c('a','b','c'),
#'                source  =c('aa','bb','cc'))
#' tc = create_tcorpus(d, doc_col='id', split_sentences = TRUE)
#' 
#' df = data.frame(doc_id=c('a','b'), test=c('A','B'))
#' tc$merge(df, by='doc_id')
#' tc$tokens
#' 
#' df = data.frame(doc_id=c('a','b'), sentence=1, test2=c('A','B'))
#' tc$merge(df, by=c('doc_id', 'sentence'))
#' tc$tokens
#' 
#' df = data.frame(doc_id=c('a','b'), sentence=1, token_id=c(3,4), test3=c('A','B'))
#' tc$merge(df, by=c('doc_id', 'sentence', 'token_id'))
#' tc$tokens
#' 
#' meta = data.frame(doc_id=c('a','b'), test=c('A','B'))
#' tc$merge_meta(meta, by='doc_id')
#' tc$meta
#' 
#' meta = data.frame(source=c('aa'), test2=c('A'))
#' tc$merge_meta(meta, by='source')
#' tc$meta
NULL

#' Change levels of factor columns
#'
#' For factor columns, the levels can be changed directly (and by reference). This is particularly usefull for fast preprocessing (e.g., making tokens lowercase, )
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{set_levels(column, levels)}
#' \preformatted{set_meta_levels(column, levels)}
#'
#' @param column the name of the column
#' @param levels The new levels
#'
#' @name tCorpus$set_levels
#' @aliases tCorpus$set_meta_levels set_levels set_meta_levels
#' @examples
#' tc = create_tcorpus(c('Text one first sentence. Text one second sentence', 'Text two'))
#'
#' ## change factor levels of a column in the token data
#' unique_tokens <- tc$get_levels('token')
#' tc$set_levels('token', toupper(unique_tokens))
#' tc$tokens
NULL


#' Change column names of data and meta data
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{set_name(oldname, newname)}
#' \preformatted{set_meta_name(oldname, newname)}
#'
#' @param oldname the current/old column name
#' @param newname the new column name
#'
#' @name tCorpus$set_name
#' @aliases tCorpus$set_meta_name set_name set_meta_name
#' @examples
#' tc = create_tcorpus(sotu_texts[1:5,], doc_column = 'id')
#'
#' ## change column name in token data
#' tc$names ## original column names
#' tc$set_name(oldname = 'token', newname = 'word')
#' tc$tokens
#'
#' ## change column name in meta data
#' tc$meta_names ## original column names
#' tc$set_meta_name(oldname = 'party', newname = 'clan')
#' tc$set_meta_name(oldname = 'president', newname = 'clan leader')
#' tc$meta
NULL

#' Delete column from the data and meta data
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{delete_columns(cnames)}
#' \preformatted{delete_meta_columns(cnames)}
#'
#' @param cnames the names of the columns to delete
#'
#' @name tCorpus$delete_columns
#' @aliases tCorpus$delete_meta_columns delete_columns delete_meta_columns
#' @examples
#' d = data.frame(text = c('Text one','Text two','Text three'),
#'                date = c('2010-01-01','2010-01-01','2012-01-01'))
#' tc = create_tcorpus(d)
#'
#' tc$tokens
#' tc$delete_columns('token')
#' tc$tokens
#'
#' tc$meta
#' tc$delete_meta_columns('date')
#' tc$meta
NULL

#' Subset a tCorpus
#'
#' @description
#' Returns the subset of a tCorpus. The selection can be made separately (and simultaneously) for the token data (using subset) and the meta data (using subset_meta). The subset arguments work according to the \link{subset.data.table} function.
#'
#' There are two flavours. You can either use subset(tc, ...) or tc$subset(...). The difference is that the second approach changes the tCorpus by reference. 
#' In other words, tc$subset() will delete the rows from the tCorpus, instead of creating a new tCorpus.
#' Modifying the tCorpus by reference is more efficient (which becomes important if the tCorpus is large), but the more classic subset(tc, ...) approach is often more obvious.
#' 
#' Subset can also be used to select rows based on token/feature frequences. This is a common step in corpus analysis, where it often makes sense to ignore very rare and/or very frequent tokens.
#' To do so, there are several special functions that can be used within a subset call.
#' The freq_filter() and docfreq_filter() can be used to filter terms based on term frequency and document frequency, respectively. (see examples)
#'
#' The subset_meta() method is an alternative for using subset(subset_meta = ...), that is added for consistency with the other _meta methods.
#'
#' Note that you can also use the \link{tCorpus$feature_subset} method if you want to filter out low/high frequency tokens, but do not want to delete the rows in the tCorpus.
#'
#' \strong{Usage:}
#'
#' ## R6 method for class tCorpus. Use as tc$method (where tc is a tCorpus object).
#'
#' \preformatted{
#' subset(tc, subset = NULL, subset_meta = NULL, 
#'        window = NULL)
#' tc$subset(subset = NULL, subset_meta = NULL,
#'           window = NULL, copy = F)
#' tc$subset_meta(subset = NULL, copy = F)
#' }
#'
#' @param subset logical expression indicating rows to keep in the tokens data.
#' @param subset_meta logical expression indicating rows to keep in the document meta data.
#' @param window If not NULL, an integer specifiying the window to be used to return the subset. For instance, if the subset contains token 10 in a document and window is 5, the subset will contain token 5 to 15. Naturally, this does not apply to subset_meta.
#' @param copy If TRUE, the method returns a new tCorpus object instead of subsetting the current one. This is added for convenience when analyzing a subset of the data. e.g., tc_nyt = tc$subset_meta(medium == "New_York_Times", copy=T)
#'
#' @name tCorpus$subset
#' @aliases tCorpus$subset_meta subset subset_meta
#' @examples
#' tc = create_tcorpus(sotu_texts[1:5,], doc_column = 'id')
#' tc$n ## original number of tokens
#'
#' ## select only first 20 tokens per document
#' tc2 = subset(tc, token_id < 20)
#' tc2$n
#' 
#' ## Note that the original is untouched
#' tc$n
#' 
#' ## Now we subset by reference. This doesn't make a copy, but changes tc itself
#' tc$subset(token_id < 20)
#' tc$n 
#'
#' ## you can filter on term frequency and document frequency with the freq_filter() and
#' ## docfreq_filter() functions
#' tc = create_tcorpus(sotu_texts[c(1:5,800:805),], doc_column = 'id')
#' tc$subset( freq_filter(token, min = 2, max = 4) )
#' tc$tokens
#'
#' ###### subset can be used for meta data by using the subset_meta argument, or the subset_meta method
#' tc$n_meta
#' tc$meta
#' tc$subset(subset_meta = president == 'Barack Obama')
#' tc$n_meta
NULL
