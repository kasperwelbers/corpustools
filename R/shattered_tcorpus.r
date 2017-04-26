shattered_tCorpus <- R6::R6Class("shattered_tCorpus",
     cloneable=FALSE,
     private = list(
       .path = 'vector'
     ),

     public = list(
       copy_on_modify = T, ## if TRUE, tCorpus works like 'typical' R (modify on copy). If FALSE, all modifications made using methods will be made to the referenced data. Not needing to copy data is a great boon of R6 as a reference class, but we should keep this optional to facilitate the common R workflow

       initialize = function(path=path) {
         private$.path = path
       },

       path = function(normalize=F) {
         path = private$.path
         if (!grepl('\\.tCorpus$', path)) stop("path in tCorpus is not a legit tCorpus path (which has the .tCorpus extension)")
         if (normalize) path = normalizePath(path)
         #if (file.exists(path)) warning('Path does not exist (or link is broken). Use $set_path() method to renew')
         path
       },

       set_path = function(path) {
         if (!grepl('\\.tCorpus$', path)) stop("Not a legit tCorpus path (which has the .tCorpus extension)")
         private$.path = path
       },

       index = function(columns=NULL, keep_df=F, as.df=F) {
         if (!file.exists(sprintf('%s/index.rds', self$path()))) return(NULL)
         index = readRDS(sprintf('%s/index.rds', self$path()))
         if (!is.null(columns)) {
           index = if (length(columns) > 1 | keep_df) index[,columns,with=F] else index[[columns]]
         }
         if (as.df) index = as.data.frame(index) else setkey(index, '.PATH','.SHARD')
         index
       },

       set_index = function(new_index){
         saveRDS(new_index, sprintf('%s/index.rds', self$path()))
       },

       info = function(name=NULL) {
         info = readRDS(sprintf('%s/info.rds', self$path()))
         if (is.null(name)) info else info[[name]]
       },

       set_info = function(value, name=NULL) {
         if (is.null(name)){
           saveRDS(value, sprintf('%s/info.rds', self$path()))
         } else {
           info = readRDS(sprintf('%s/info.rds', self$path()))
           info[[name]] = value
           saveRDS(info, sprintf('%s/info.rds', self$path()))
         }
       },

       shards = function(normalize=T) {
         index = unique(self$index(columns = c('.PATH','.SHARD')))
         shards = index$.SHARD
         if (normalize) shards = normalizePath(shards)
         shards
       },

       feature_levels = function() readRDS(sprintf('%s/feature_levels.rds', self$path())),
       set_feature_levels = function(feature_levels) saveRDS(feature_levels, sprintf('%s/feature_levels.rds', self$path())),

       ## SHOW/GET DATA METHODS ##
       data = function(columns=NULL, keep_df=F, as.df=F) {
         stop('not yet implemented')
       },

       meta = function(columns=NULL, keep_df=F, as.df=F, per_token=F) {
         stop('not yet implemented')
       },

       dtm = function(feature, context_level=c('document','sentence'), weight=c('termfreq','docfreq','tfidf','norm_tfidf'), drop_empty_terms=T, form=c('Matrix', 'tm_dtm', 'quanteda_dfm'), subset_tokens=NULL, subset_meta=NULL, context_labels=T) {
         stop('not yet implemented')
       },

       ## DATA MODIFICATION METHODS ##
       transform = function(..., copy=self$copy_on_modify, safe=T) {
         stop('not yet implemented')
       },

       within = function(expr, copy=self$copy_on_modify, safe=T){
         stop('not yet implemented')
       },

       set = function(column, value, subset=NULL, copy=self$copy_on_modify, safe=T){
         stop('not yet implemented')
       },

       set_colname = function(oldname, newname) {
         stop('not yet implemented')
       },

       transform_meta = function(..., copy=self$copy_on_modify, safe=T) {
         stop('not yet implemented')
       },

       within_meta = function(expr, copy=self$copy_on_modify, safe=T){
         stop('not yet implemented')
       },

       set_meta = function(column, value, subset=NULL, copy=self$copy_on_modify, safe=T){
         stop('not yet implemented')
       },

       set_meta_colname = function(oldname, newname) {
         stop('not yet implemented')
       },

       subset = function(subset=NULL, subset_meta=NULL, drop_levels=F, window=NULL, copy=self$copy_on_modify){
         stop('not yet implemented')
       },


       ## FEATURE MANAGEMENT ##
       preprocess = function(column, new_column=column, lowercase=T, ngrams=1, ngram_context=c('document', 'sentence'), as_ascii=F, remove_punctuation=T, remove_stopwords=F, use_stemming=F, language='english') {
         stop('not yet implemented')
       },

       feature_stats = function(feature, context_level=c('document','sentence')){
         stop('not yet implemented')
       },

       top_features = function(feature, n=10, group_by=NULL, group_by_meta=NULL, return_long=F){
         stop('not yet implemented')
       },


       ## SEARCHING / QUERYING ##
       search_features = function(keyword=NA, condition=NA, code=NA, queries=NULL, feature='word', condition_once=F, subset_tokens=NA, subset_meta=NA, keep_false_condition=F, only_last_mword=F, verbose=F){
         stop('not yet implemented')
       },

       search_recode = function(feature, new_value, keyword, condition=NA, condition_once=F, subset_tokens=NA, subset_meta=NA, copy=self$copy_on_modify){
         stop('not yet implemented')
       },

       search_contexts = function(query, code=NULL, feature='word', context_level=c('document','sentence'), verbose=F){
         stop('not yet implemented')
       },

       subset_query = function(query, feature='word', context_level=c('document','sentence'), copy=self$copy_on_modify){
         stop('not yet implemented')
       },

       ## CO-OCCURRENCE NETWORKS ##
       semnet = function(feature, measure=c('con_prob', 'con_prob_weighted', 'cosine', 'count_directed', 'count_undirected', 'chi2'), context_level=c('document','sentence'), backbone=F, n.batches=NA, alpha=2){
         stop('not yet implemented')
       },

       semnet_window = function(feature, measure=c('con_prob', 'cosine', 'count_directed', 'count_undirected', 'chi2'), context_level=c('document','sentence'), window.size=10, direction='<>', backbone=F, n.batches=5, set_matrix_mode=c(NA, 'windowXwindow','positionXwindow')){
         stop('not yet implemented')
       },

       ## RESOURCES ##

       jrc_names = function(new_feature='jrc_names', feature='word', resource_path=getOption('tcorpus_resources', NULL), collocation_labels=T, batchsize=50000, low_memory=T, verbose=T, copy=self$copy_on_modify){
         stop('not yet implemented')
       },

       ## util
       set_keys = function(){
         stop('not yet implemented')
       },

       reset_feature_index = function(){
         stop('not yet implemented')
       },

       droplevels = function(copy=self$copy_on_modify){
         stop('not yet implemented')
       }
     ),

     active = list(
       n_index = function() nrow(self$index()),
       n = function() self$info('n'),
       n_meta = function() self$info('n_meta'),
       feature_names = function(e=NULL) {
         if (!is.null(e)) stop('Cannot change tcorpus$featurenames by assignment. Instead, use the set_colname() function')
         dnames = self$names
         dnames[!dnames %in% c('doc_id','sent_i','word_i')]
       },
       names = function(e=NULL) {
         if (!is.null(e)) stop('Cannot change tcorpus$names by assignment. Instead, use the set_colname() function')
         colnames(self$info('data_head'))
       },
       meta_names = function(e=NULL) {
         if (!is.null(e)) stop('Cannot change tcorpus$meta_names by assignment. Instead, use the set_meta_colname() function')
         colnames(self$info('meta_head'))
       }
     )
)

#' @export
print.shattered_tCorpus <- function(x, ...) {
  info = x$info()
  dnames = colnames(info$data_head)
  mnames = colnames(info$meta_head)
  cat('tCorpus containing ', info$n, ' tokens',
      '\nsplit by documents (n = ', info$n_meta, ')', info$sent_info,
      '\ncontains:',
      '\n  - ', length(dnames), ' data column', if (length(dnames) > 1) '(s)', ':\t', paste(dnames, collapse=', '),
      '\n  - ', length(mnames), ' meta column', if (length(mnames) > 1) '(s)', ': \t', paste(mnames, collapse=', '),
      '\n', sep='')
}

#' @export
summary.shattered_tCorpus <- function(object, ...) objec

#' Open a shattered_tCorpus stored on disk
#'
#' If a shattered_tCorpus has been created, this function can be used to read/open/load it by
#'
#' For the sake of reference: a shattered_tCorpus is nothing more than an object that refers to a folder containing the tCorpus shards and index.
#' To create a shattered_tCorpus, consult the documentation ?shattered_tCorpus.
#'
#' @param path a path to a .tCorpus folder
#'
#' @export
open_shattered_tc <- function(path) {
  shattered_tCorpus$new(path=path)
}

#' Delete a shattered_tCorpus and all files it refers to
#'
#' @param stc A shattered_tCorpus object
#'
#' @export
delete_shattered_tc <- function(stc) {
  path = stc$path()
  if (!grepl('\\.tCorpus$', path)) stop(sprintf('Cannot delete %s. Not a legit shattered_tCorpus folder'))
  unlink(path, recursive = T)
}
