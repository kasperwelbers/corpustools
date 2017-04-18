tCorpus <- R6::R6Class("tCorpus",
   cloneable=FALSE,
   private = list(
     .data = NULL,
     .meta = NULL,
     .feature_index = NULL,
     .p = list(),

     set_provenance = function(...){
       p = list(...)
       for(key in names(p)) private$.p[[key]] = p[[key]]
     },
     is_provenance = function(...){
       p = list(...)
       for(key in names(p)) {
         if (!key %in% names(private$.p)) return(FALSE)
         if (!private$.p[[key]] == p[[key]]) return(FALSE)
       }
       return(TRUE)
     },
     select_rows = function(selection, keep_meta=F) {
       selection = safe_selection(private$.data, selection)
       private$.data = subset(private$.data, selection)
       if (!keep_meta) {
         private$.meta = private$.meta[as.character(unique(private$.data$doc_id)),,nomatch=0]
         private$.meta$doc_id = as.character(private$.meta$doc_id)
       }
       self$reset_feature_index()
       self$set_keys()
     },

     select_meta_rows = function(selection, keep_data=F) {
       selection = safe_selection(private$.meta, selection)
       private$.meta = subset(private$.meta, selection)
       private$.meta$doc_id = as.character(private$.meta$doc_id)
       if (!keep_data) private$.data = private$.data[as.character(unique(private$.meta$doc_id)),,nomatch=0]
       self$reset_feature_index()
       self$set_keys()
     },

     match_data_to_meta = function(){
       doc_ids = intersect(unique(private$.data$doc_id), unique(private$.meta$doc_id))
       private$.data = private$.data[doc_ids,]
       private$.meta = private$.meta[doc_ids,]
       self$set_keys()
     },

     check_unique_rows = function(d){
       if ('sent_i' %in% colnames(d)) {
         if (!identical(key(tc$data), c('doc_id', 'sent_i', 'word_i'))) setkeyv(mod, c('doc_id','sent_i','word_i'))
         if (nrow(unique(d, by=c('doc_id', 'sent_i', 'word_i'))) < nrow(d)) stop('After transformation, word_i is no longer unique')
         if (nrow(unique(d[,c('doc_id','word_i')])) < nrow(d)) stop('After transformation, word_i is not unique within documents')
       } else {
         if (!identical(key(tc$data), c('doc_id', 'word_i'))) setkeyv(mod, c('doc_id','word_i'))
         if (nrow(unique(d, by=c('doc_id', 'word'))) < nrow(d)) stop('After transformation, word_i is not unique within documents')
       }
     },

     safe_data_mod = function(mod) {
       if (is.null(mod)) return(NULL)
       if (nrow(mod) < self$n) stop('Replacement cannot have fewer rows. For subsetting, please use the $subset method')
       if (nrow(mod) > self$n) stop('Replacement cannot have more rows. For adding more data, please use the merge_tcorpora function or the $add_data method')

       mod = data.table(mod)
       if ('sent_i' %in% colnames(mod)) setkeyv(mod, c('doc_id','sent_i','word_i')) else setkeyv(mod, c('doc_id','word_i'))

       positioncols = intersect(c('doc_id','sent_i','word_i'), colnames(mod))
       if (!identical(private$.data[,positioncols,with=F], mod[,positioncols,with=F])) {
         if (!identical(private$.data$doc_id, mod$doc_id)) {
           stop('Cannot change doc_id. If you want to change doc_id labels, you can overwrite $doc_id_levels.')
         }
         private$check_unique_rows(mod)
         self$reset_feature_index()
       }

       indexcol = self$provenance()$index_feature
       if (!is.null(indexcol)){
         if (!identical(private$.data[[indexcol]], mod[[indexcol]])) {
           self$reset_feature_index()
         }
       }
       private$.data = mod
       if (self$auto_gc) gc()
       self$set_keys()
     },

     safe_meta_mod = function(mod) {
       if (is.null(mod)) return(NULL)
       mod = data.table(mod)
       setkey(mod, 'doc_id')
       if (nrow(mod) < self$n_meta) stop('Replacement cannot have fewer rows. For subsetting, please use the $subset method')
       if (nrow(mod) > self$n_meta) stop('Replacement cannot have more rows. For adding more data, please use the merge_tcorpora function or the $add_data method')
       if (!identical(private$.meta$doc_id, mod$doc_id)) {
         stop('Cannot change doc_id. If you want to change doc_id labels, you can overwrite $doc_id_levels.')
       }
       private$.meta = mod
       if (self$auto_gc) gc()
       self$set_keys()
     }

   ),

   public = list(

     copy_on_modify = T, ## if TRUE, tCorpus works like 'typical' R (modify on copy). If FALSE, all modifications made using methods will be made to the referenced data. Not needing to copy data is a great boon of R6 as a reference class, but we should keep this optional to facilitate the common R workflow
     ## (set to F every once in a while to run unit tests in reference mode)

     auto_gc = T, ## if TRUE, automatically perform gc() within functions that create large copies. R seems to handle the cleaning of R6 environments poorly (which can be huge for a tCorpus). It appears (but needs more testing) that R sometimes goes for the swap memory even though RAM has already been freed.
     help = function() ?tCorpus,

     initialize = function(data, meta, feature_index=NULL, p=NULL) {
       private$.data = data.table(data)
       private$.meta = data.table(meta)
       private$.p = if (!is.null(p)) p else list()
       private$.feature_index = if (!is.null(feature_index)) feature_index else NULL
       self$set_keys()
     },


## SHOW/GET DATA METHODS ##
    get = function(columns=NULL, keep_df=F, as.df=F) {
      if (is.null(columns)) {
        d = data.table::copy(private$.data)
      } else {
        d = if (length(columns) > 1 | keep_df) private$.data[,columns,with=F] else private$.data[[columns]]
      }
      if (as.df) d = as.data.frame(d)
      d
    },

    get_levels = function(column){
      if (!is(private$.data[[column]], 'factor')) stop(sprintf('"%s" is not a factor', column))
      levels(private$.data[[column]])
    },

    get_meta = function(columns=NULL, keep_df=F, as.df=F, per_token=F) {
      if (is.null(columns)) {
        d = data.table::copy(private$.meta)
      } else {
        d = if (length(columns) > 1 | keep_df) private$.meta[,columns,with=F] else private$.meta[[columns]]
      }
      if (as.df) d = as.data.frame(d)
      if (per_token) {
        exp_i = match(private$.data$doc_id, private$.meta$doc_id)
        d = if (is(d, 'data.frame')) d[exp_i,,drop=!keep_df & !is.null(columns)] else d[exp_i]
      }
      d
    },

    get_meta_levels = function(column){
      if (!is(private$.meta[[column]], 'factor')) stop(sprintf('"%s" is not a factor', column))
      levels(private$.meta[[column]])
    },

     provenance = function(name=NULL) if (is.null(name)) private$.p else private$.p[[name]],

     feature_index = function(feature='word', context_level=c('document','sentence'), max_window_size=100, as_ascii=F){
       context_level = match.arg(context_level)
       if (max_window_size < 100) max_window_size = 100 ## always use a window of at least 100,
       prov = private$.p
       if (is.null(private$.feature_index)){
         private$.feature_index = create_feature_index(self, feature=feature, context_level=context_level, max_window_size=max_window_size, as_ascii=as_ascii)
         private$set_provenance(index_feature=feature, context_level=context_level, max_window_size=max_window_size, as_ascii=as_ascii)
         message('Created feature index')
       } else {
         if (!private$is_provenance(index_feature=feature, context_level=context_level, max_window_size=max_window_size, as_ascii=as_ascii)) {
           private$.feature_index = create_feature_index(self, feature=feature, context_level=context_level, max_window_size=max_window_size, as_ascii=as_ascii)
           private$set_provenance(index_feature=feature, context_level=context_level, max_window_size=max_window_size, as_ascii=as_ascii)
           message('Created new feature index')
         }
       }
       private$.feature_index
     },

     context = function(context_level = c('document','sentence'), with_labels=T){
       get_context(self, context_level = context_level, with_labels=with_labels)
     },

     dtm = function(feature, context_level=c('document','sentence'), weight=c('termfreq','docfreq','tfidf','norm_tfidf'), drop_empty_terms=T, form=c('Matrix', 'tm_dtm', 'quanteda_dfm'), subset_tokens=NULL, subset_meta=NULL, context=NULL, context_labels=T, feature_labels=T, ngrams=NA, ngram_before_subset=F) {
       subset_tokens = if (is(substitute(subset_tokens), 'call')) deparse(substitute(subset_tokens)) else subset_tokens
       subset_meta = if (is(substitute(subset_meta), 'call')) deparse(substitute(subset_meta)) else subset_meta

       get_dtm(self, feature=feature, context_level=context_level, weight=weight, drop_empty_terms=drop_empty_terms, form=form, subset_tokens=subset_tokens, subset_meta=subset_meta, context=context, context_labels=context_labels, feature_labels=feature_labels, ngrams=ngrams, ngram_before_subset=ngram_before_subset)
     },

## DATA MODIFICATION METHODS ##

     copy = function(){
       if (self$auto_gc) gc()
       tCorpus$new(data = data.table::copy(private$.data),
                   meta = data.table::copy(private$.meta),
                   feature_index = private$.feature_index,
                   p = private$.p)
     },

     set = function(column, value, subset=NULL, copy=self$copy_on_modify){
       if (column == 'doc_id') stop('Cannot change doc_id. If you want to change doc_id labels, you can overwrite $doc_id_levels.')
       if(is(substitute(subset), 'call')) subset = eval(substitute(subset), private$.data, parent.frame())
       if(is(substitute(value), 'call')) value = eval(substitute(value), private$.data, parent.frame())

       if (copy) {
         selfcopy = self$copy()$set(column=column, value=value, subset=subset, copy=F)
         return(selfcopy)
       }

       if (!is.null(subset)){
         if (!column %in% colnames(private$.data)) {
           private$.data[,(column) := NA]
           if (is(value, 'factor')) private$.data[,(column) := fast_factor(private$.data[[column]])]
         } else {
           if (is(private$.data[[column]], 'factor')) {
             value = fast_factor(value)
             levels(private$.data[[column]]) = c(levels(private$.data[[column]]), levels(value))
           }
           if (column %in% c('sent_i','word_i')) {  ## for position columns, first perform checks (inefficient, but this should be a rare case anyway)
              if (!is(value, 'numeric')) stop('position column has to be numeric/integer')
              value = as.integer(value)
              mod = if ('sent_i' %in% self$names) private$.data[,c('doc_id','sent_i','word_i')] else private$.data[,c('doc_id','word_i')]
              mod[subset, (column) := value]
              check_unique_rows(mod)
           }
           private$.data[subset, (column) := value]
         }
       } else {
         if (column %in% c('sent_i','word_i')) {
           if (!is(value, 'numeric')) stop('position column has to be numeric/integer')
           value = as.integer(value)
           mod = if ('sent_i' %in% self$names) private$.data[,c('doc_id','sent_i','word_i')] else private$.data[,c('doc_id','word_i')]
           suppressWarnings(mod[, (column) := value])
           check_unique_rows(mod)
         }
         ## ugly suppress. Should look into why data.table give the (seemingly harmless) internal.selfref warning
         suppressWarnings(private$.data[,(column) := value])
       }
       if (identical(self$provenance()$index_feature, column)) self$reset_feature_index # reset feature index if necessary
       self$set_keys()
       invisible(self)
     },

     set_levels = function(column, levels, copy=self$copy_on_modify) {
       if (!is(private$.data[[column]], 'factor')) stop(sprintf('"%s" is not a factor', column))
       if (!length(levels) == length(levels(private$.data[[column]]))) stop('new levels of different length than current levels')
       if (copy) {
         selfcopy = self$copy()$set_levels(column=column, levels=levels, copy=F)
         return(selfcopy)
       }
       if (column == 'doc_id') {
         private$.meta$doc_id = levels[match(levels(private$.data$doc_id), private$.meta$doc_id)]
       }
       data.table::setattr(private$.data[[column]], 'levels', levels)
     },

     select_columns = function(cnames, copy=self$copy_on_modify){
       protected_cols = intersect(self$names, c('doc_id', 'sent_i', 'word_i'))
       if (!any(protected_cols %in% cnames)) stop('selection (cnames) must contain the (existing) position columns (doc_id, word_i, sent_i)')
       if (copy) {
         selfcopy = self$copy()$select_columns(cnames=cnames, copy=F)
         return(selfcopy)
       }
       private$.data = private$.data[,cnames,with=F]
       invisible(self)
     },

     set_colname = function(oldname, newname) {
       if (oldname %in% c('doc_id','sent_i','word_i')) stop('The position columns (doc_id, sent_i, word_i) cannot be set or changed (with safe = T)')
       data.table::setnames(private$.data, oldname, newname)
       invisible(self)
     },

     set_meta = function(column, value, subset=NULL, copy=self$copy_on_modify){
       if (column == 'doc_id') stop('Cannot change doc_id. If you want to change doc_id labels, you can overwrite $doc_id_levels.')
       if(is(substitute(subset), 'call')) subset = eval(substitute(subset), private$.meta, parent.frame())
       if(is(substitute(value), 'call')) value = eval(substitute(value), private$.meta, parent.frame())

       if (copy) {
         selfcopy = self$copy()$set_meta(column=column, value=value, subset=subset, copy=F)
         return(selfcopy)
       }

       if (!is.null(subset)){
         if (!column %in% colnames(private$.meta)) {
           private$.meta[[column]] = NA
           if (is(value, 'factor')) private$.meta[[column]] = fast_factor(private$.meta[[column]])
         } else {
           if (is(private$.meta[[column]], 'factor')) {
             value = fast_factor(value)
             levels(private$.meta[[column]]) = c(levels(private$.meta[[column]]), levels(value))
           }
           private$.meta[subset, (column) := value]
         }
       } else {
         ## ugly suppress. Should look into why data.table give the (seemingly harmless) internal.selfref warning
         suppressWarnings(private$.meta[,(column) := value])
       }
       invisible(self)
     },

      set_meta_levels = function(column, levels, copy=self$copy_on_modify) {
        if (!is(private$.meta[[column]], 'factor')) stop(sprintf('"%s" is not a factor', column))
        if (copy) {
          selfcopy = self$copy()$set_meta_levels(column=column, levels=levels, copy=F)
          return(selfcopy)
        }
        data.table::setattr(private$.meta[[column]], 'levels', levels)
      },

     select_meta_columns = function(cnames, copy=self$copy_on_modify){
        protected_cols = intersect(self$names, c('doc_id'))
        if (!any(protected_cols %in% cnames)) stop('selection (cnames) must contain the document id (doc_id)')
        if (copy) {
          selfcopy = self$copy()$select_meta_columns(cnames=cnames, copy=F)
          return(selfcopy)
        }
        private$.meta = private$.meta[,cnames,with=F]
        invisible(self)
      },

     set_meta_colname = function(oldname, newname) {
       if (oldname %in% c('doc_id')) stop('The position columns (doc_id, sent_i, word_i) cannot be set or changed (with safe = T)')
       setnames(private$.meta, oldname, newname)
       invisible(self)
     },

     subset = function(subset=NULL, subset_meta=NULL, drop_levels=T, window=NULL, copy=self$copy_on_modify){
       if(is(substitute(subset), 'call')) subset = eval(substitute(subset), private$.data, parent.frame())
       if(is(substitute(subset_meta), 'call')) subset_meta = eval(substitute(subset_meta), private$.meta, parent.frame())

       if (copy) {
         selfcopy = self$copy()$subset(subset=subset, subset_meta=subset_meta, drop_levels=drop_levels, window=window, copy=F)
         return(selfcopy)
       }

       if (!is.null(subset_meta)) subset_meta[is.na(subset_meta)] = F

       if (!is.null(subset)){
         subset[is.na(subset)] = F
         if (!is.null(window)){
           global_i = get_global_i(self, max_window_size=window)
           global_r = global_i[subset]
           global_window = rep(global_r, window*2 + 1) + rep(-window:window, each=length(global_r)) ## add window
           subset = global_i %in% global_window
         }
       }

       ## subset the data, using different solutions if one or both subsets (data and meta) are used for optimalisation
       if (!is.null(subset_meta) & !is.null(subset)) {
         private$select_meta_rows(subset_meta, keep_data = T) ## if both subsets are used, first perform both without subseting the other, then match on doc_ids.
         private$select_rows(subset, keep_meta = T)           ## (we cannot subset one before the other, because subset call's can contains vectors for which the rows should match)
         private$match_data_to_meta()
       }
       if (!is.null(subset_meta) & is.null(subset)) {
         private$select_meta_rows(subset_meta, keep_data = F)
       }
       if (is.null(subset_meta) & !is.null(subset)) {
         private$select_rows(subset, keep_meta = F)
       }

       if (drop_levels) self$droplevels(copy=F)
       invisible(self)
     },

      subset_i = function(subset=NULL, subset_meta=NULL, window=NULL, inverse=F){
        if(is(substitute(subset), 'call')) subset = eval(substitute(subset), private$.data, parent.frame())
        if(is(substitute(subset_meta), 'call')) subset_meta = eval(substitute(subset_meta), private$.meta, parent.frame())

        ## enable subset_i to be called from a character string. (e.g. used in search_features)
        if(is(subset, 'character')) subset_meta = eval(parse(text=subset_meta), private$.meta, parent.frame())
        if(is(subset_meta, 'character')) subset_meta = eval(parse(text=subset_meta), private$.meta, parent.frame())

        d = private$.data[,c('doc_id')]

        if (!is.null(subset)) {
          subset[is.na(subset)] = F
          d[ , subset := F]
          d[subset, subset := T]
        } else d[ , subset := T]

        if (!is.null(subset_meta)){
          subset_meta[is.na(subset_meta)] = F
          d[ , subset_meta := F]
          keyval = as.character(private$.meta$doc_id[subset_meta])
          d[list(keyval), subset_meta := T]
        } else d[, subset_meta := T]


        i = which(d$subset & d$subset_meta)

        if (!inverse) i else !1:self$n %in% i
      },

     reset_feature_index = function(){
       private$.feature_index = NULL
       private$set_provenance(index_feature=NULL, context_level=NULL, max_window_size=NULL, as_ascii=NULL)
     },

     aggregate = function(meta_cols=NULL, hits=NULL, feature=NULL, doc_count=T){
        meta = data.table::copy(private$.meta)
        if (is.null(meta_cols)) {
          meta[,group := 'total']
          meta_cols = 'group'
        }

        d = meta[, list(N=.N), by=meta_cols]

        if (!is.null(hits) | !is.null(feature)){
          if (!is.null(hits) & !is.null(feature)) stop('Cannot specify both hits and feature')
          if (!is.null(hits)) {
            if (!is(hits, c('featureHits', 'contextHits'))) stop('hits must be a featureHits or contextHits object (see the $search_features and $search_contexts methods)')
            if (is(hits, 'featureHits')) {
              coldata = hits$hits[!duplicated(hits$hits$hit_id),]
            } else {
              coldata = hits$hits
            }
          }

          if (!is.null(feature)) {
            coldata = data.frame(doc_id = private$.data$doc_id, code = private$.data[[feature]])
            coldata = coldata[!is.na(coldata$code),]
          }

          if (doc_count) coldata = coldata[!duplicated(coldata[,c('doc_id','code')]),]

          count_d = meta[list(coldata$doc_id), meta_cols, with=F]

          count_d$code = coldata$code
          agg_cols = c(meta_cols, 'code')

          count_d = count_d[, list(count=.N), by = agg_cols]
          d = merge(d, count_d, meta_cols)
          d$count[is.na(d$count)] = 0
          d = dcast(d, ... ~ code, value.var='count')
        }
        as.data.frame(d)
      },


## FEATURE MANAGEMENT ##
     preprocess = function(column, new_column=column, lowercase=T, ngrams=1, ngram_context=c('document', 'sentence'), as_ascii=F, remove_punctuation=T, remove_stopwords=F, use_stemming=F, language='english', copy=self$copy_on_modify) {
       if (copy) {
       selfcopy = self$copy()$preprocess(column=column, new_column=new_column, lowercase=lowercase, ngrams=ngrams, ngram_context=ngram_context, as_ascii=as_ascii, remove_punctuation=remove_punctuation, remove_stopwords=remove_stopwords, use_stemming=use_stemming, language=language, copy=F)
         return(selfcopy)
       }
       invisible(preprocess_feature(self, column=column, new_column=new_column, lowercase=lowercase, ngrams=ngrams, ngram_context=ngram_context, as_ascii=as_ascii, remove_punctuation=remove_punctuation, remove_stopwords=remove_stopwords, use_stemming=use_stemming, language=language))
     },

     feature_subset = function(column, new_column, subset, inverse=F, copy=self$copy_on_modify){
       if(is(substitute(subset), 'call')) subset = eval(substitute(subset), private$.data, parent.frame())

       if (copy) {
         selfcopy = self$copy()$feature_subset(column=column, new_column=new_column, subset=subset, inverse=inverse, copy=F)
         return(selfcopy)
       }
       invisible(subset_feature_fun(self, i=which(!subset), column=column, new_column=new_column, inverse=inverse))
     },

     feature_stats = function(feature, context_level=c('document','sentence')){
       term.statistics(self, feature=feature, context_level=context_level)
     },

     top_features = function(feature, n=10, group_by=NULL, group_by_meta=NULL, return_long=F){
       top_features(self, feature=feature, n=n, group_by=group_by, group_by_meta=group_by_meta, return_long=return_long)
     },


## SEARCHING / QUERYING ##
     search_features = function(keyword=NA, condition=NA, code=NA, queries=NULL, feature='word', condition_once=F, subset_tokens=NA, subset_meta=NA, keep_false_condition=F, only_last_mword=F, verbose=F){
       subset_tokens = if (is(substitute(subset_tokens), 'call')) deparse(substitute(subset_tokens)) else subset_tokens
       subset_meta = if (is(substitute(subset_meta), 'call')) deparse(substitute(subset_meta)) else subset_meta

       search_features(self, keyword=keyword, condition=condition, code=code, queries=queries, feature=feature, condition_once=condition_once, subset_tokens=subset_tokens, subset_meta=subset_meta, keep_false_condition=keep_false_condition, only_last_mword=only_last_mword, verbose=verbose)
     },

     code_features = function(keyword=NA, condition=NA, code=NA, queries=NULL, feature='word', condition_once=F, subset_tokens=NA, subset_meta=NA, only_last_mword=F, verbose=F, copy=self$copy_on_modify){
       subset = if (is(substitute(subset), 'call')) deparse(substitute(subset)) else subset
       subset_meta = if (is(substitute(subset_meta), 'call')) deparse(substitute(subset_meta)) else subset_meta

       if(copy){
         selfcopy = self$copy()$code_features(keyword=keyword, condition=condition, code=code, queries=queries, feature=feature, condition_once=condition_once, subset_tokens=subset_tokens, subset_meta=subset_meta, only_last_mword=only_last_mword, verbose=verbose, copy=F)
         return(selfcopy)
       }
       hits = search_features(self, keyword=keyword, condition=condition, code=code, queries=queries, feature=feature, condition_once=condition_once, subset_tokens=subset_tokens, subset_meta=subset_meta, keep_false_condition=F, only_last_mword=only_last_mword, verbose=verbose)

       invisible(self)
     },

     search_recode = function(feature, new_value, keyword, condition=NA, condition_once=F, subset_tokens=NA, subset_meta=NA, copy=self$copy_on_modify){
       subset = if (is(substitute(subset), 'call')) deparse(substitute(subset)) else subset
       subset_meta = if (is(substitute(subset_meta), 'call')) deparse(substitute(subset_meta)) else subset_meta

       if (copy) {
          selfcopy = self$copy()$search_recode(feature=feature, new_value=new_value, keyword=keyword, condition=condition, condition_once=condition_once, subset_tokens=subset_tokens, subset_meta=subset_meta, copy=F)
          return(selfcopy)
       }

       hits = self$search_features(keyword=keyword, condition=condition, condition_once=condition_once, subset_tokens=subset_tokens, subset_meta=subset_meta)
       x = as.numeric(as.character(hits$hits$i)) ## for one of those inexplicable R reasons, I cannot directly use this numeric vector.... really no clue at all why
       self$set(feature, new_value, subset = x, copy = F)
       invisible(self)
     },

     search_contexts = function(query, code=NULL, feature='word', context_level=c('document','sentence'), verbose=F){
       search_contexts(self, query, code=code, feature=feature, context_level=context_level, verbose=verbose)
     },

     subset_query = function(query, feature='word', context_level=c('document','sentence'), copy=self$copy_on_modify){
       if (copy) {
         selfcopy = self$copy()$subset_query(query=query, feature=feature, context_level=context_level, copy=F)
         return(selfcopy)
       }
       context_level = match.arg(context_level)
       hits = self$search_contexts(query, feature=feature, context_level=context_level)
       hits = hits$hits
       if (is.null(hits)) return(NULL)
       if (context_level == 'document'){
         private$select_meta_rows(self$meta$doc_id %in% hits$doc_id)
       }
       if (context_level == 'sentence'){
         d = self$get(c('doc_id','sent_i'), keep_df=T)
         d$i = 1:nrow(d)
         rows = d[list(hits$doc_id, hits$sent_i)]$i
         private$select_rows(rows)
       }
       invisible(self)
     },

     kwic = function(hits=NULL, i=NULL, keyword=NULL, code='', nwords=10, nsample=NA, output_feature='word', context_level=c('document','sentence'), prettypaste=T, kw_tag=c('<','>'), ...){
       if (!is.null(keyword)) hits = self$search_features(keyword=keyword, code=code, ...)
       keyword_in_context(self, hits=hits, i=i, code=code, nwords=nwords, nsample=nsample, output_feature=output_feature, context_level=context_level, prettypaste=prettypaste, kw_tag=kw_tag)
     },

## CO-OCCURRENCE NETWORKS ##
     semnet = function(feature, measure=c('con_prob', 'con_prob_weighted', 'cosine', 'count_directed', 'count_undirected', 'chi2'), context_level=c('document','sentence'), backbone=F, n.batches=NA){
       if (!require(igraph)) stop('igraph package needs to be installed in order to use semnet methods')
       semnet(self, feature=feature, measure=measure, context_level=context_level, backbone=backbone, n.batches=n.batches)
     },

     semnet_window = function(feature, measure=c('con_prob', 'cosine', 'count_directed', 'count_undirected', 'chi2'), context_level=c('document','sentence'), window.size=10, direction='<>', backbone=F, n.batches=5, set_matrix_mode=c(NA, 'windowXwindow','positionXwindow')){
       if (!require(igraph)) stop('igraph package needs to be installed in order to use semnet methods')
       semnet_window(self, feature=feature, measure=measure, context_level=context_level, window.size=window.size, direction=direction, backbone=backbone, n.batches=n.batches, set_matrix_mode=set_matrix_mode)
     },

## CORPUS COMPARISON ##

     compare_corpus = function(tc_y, feature, smooth=0.1, min_over=NULL, min_chi2=NULL, is_subset=F, yates_cor=c('auto','yes','no')){
       if (is_subset & self$n > tc_y$n) stop('tCorpus x (the one calling the method) cannot be a subset of tCorpus y, because it has more tokens')
       tcorpus_compare(self, tc_y, feature, smooth=smooth, min_over=min_over, min_chi2=min_chi2, yates_cor=yates_cor, x_is_subset=is_subset)
     },

     compare_subset = function(feature, subset_x=NULL, subset_meta_x=NULL, query_x=NULL, query_feature='word', smooth=0.1, min_over=NULL, min_chi2=NULL, yates_cor=c('auto','yes','no')){
       subset_x = eval(substitute(subset_x), private$.meta, parent.frame(2))
       subset_meta_x = eval(substitute(subset_meta_x), private$.meta, parent.frame(2))

       if(is.null(subset_x) & is.null(subset_meta_x) & is.null(query_x)) stop("at least one of subset_x, subset_meta_x or query_x has to be specified")
       if(!is.null(subset_x) | !is.null(subset_meta_x)) tc_x = self$subset(subset=subset_x, subset_meta=subset_meta_x, copy=T)
       if(!is.null(query_x)) tc_x = self$subset_query(query_x, feature=query_feature, copy=T)

       comp = tc_x$compare_corpus(self, feature=feature, smooth=smooth, min_over=min_over, min_chi2=min_chi2, yates_cor=yates_cor, is_subset=T)
       if (self$auto_gc) rm(tc_x); gc()
       comp
     },

## DOCUMENT COMPARISON ##

     compare_documents = function(feature='word', date_col=NULL, hour_window=NULL, measure=c('cosine','overlap_pct'), min_similarity=0, weight=c('norm_tfidf', 'tfidf', 'termfreq','docfreq'), ngrams=NA, from_subset=NULL, to_subset=NULL) {
       if (!require(RNewsflow)) stop('RNewsflow package needs to be installed in order to use document comparison methods')
        weight = match.arg(weight)
        from_subset = eval(substitute(from_subset), private$.meta, parent.frame(2))
        to_subset = eval(substitute(to_subset), private$.meta, parent.frame(2))
        compare_documents_fun(self, feature=feature, date_col=date_col, hour_window=hour_window, measure=measure, min_similarity=min_similarity, weight=weight, ngrams=ngrams, from_subset=from_subset, to_subset=to_subset)
     },

     deduplicate = function(feature='word', date_col=NULL, meta_cols=NULL, hour_window=NULL, min_docfreq=2, max_docfreq_pct=0.5, measure=c('cosine','overlap_pct'), similarity=1, keep=c('first','last', 'random'), weight=c('norm_tfidf', 'tfidf', 'termfreq','docfreq'), ngrams=NA, print_duplicates=F, copy=self$copy_on_modify){
       if (!require(RNewsflow)) stop('RNewsflow package needs to be installed in order to use document comparison methods')

       weight = match.arg(weight)
       match.arg(feature, self$feature_names)
       if (copy) {
         selfcopy = self$copy()$deduplicate(feature=feature, date_col=date_col, meta_cols=meta_cols, hour_window=hour_window, min_docfreq=min_docfreq, max_docfreq_pct=max_docfreq_pct, measure=measure, similarity=similarity, keep=keep, weight=weight, ngrams=ngrams, print_duplicates=print_duplicates, copy=F)
         return(selfcopy)
       }

       self$set('DEDUPLICATE_FEATURE', self$get(feature), copy = F)
       self$feature_subset('DEDUPLICATE_FEATURE', 'DEDUPLICATE_FEATURE', subset = docfreq_filter('DEDUPLICATE_FEATURE', min=min_docfreq, max=self$n * max_docfreq_pct), copy=F)

       duplicates = get_duplicates(self, feature=feature, date_col=date_col, meta_cols=meta_cols, hour_window=hour_window, measure=measure, similarity=similarity, keep=keep, weight=weight, print_duplicates=print_duplicates)
       self$subset(subset_meta = !doc_id %in% duplicates, copy=F)
       self$set('DEDUPLICATE_FEATURE', NULL, copy=F)
       invisible(self)
     },

## TOPIC MODELING ##

      lda_fit = function(feature, create_feature=NULL, K=50, num.iterations=500, alpha=50/K, eta=.01, burnin=250, context_level=c('document','sentence'), ...) {
        if (!require(topicmodels)) stop('topicmodels package needs to be installed in order to use LDA')

        dtm = self$dtm(feature=feature, context_level=context_level, ...)
        m = lda_fit(dtm=dtm, method='Gibbs', K=K, num.iterations=num.iterations, alpha=alpha, eta=eta, burnin=burnin)
        if (!is.null(create_feature)) self$lda_topic_features(m=m, feature=feature, new_feature=create_feature, context_level=context_level, copy=F)
        m
      },

      lda_topic_features = function(m, feature, new_feature='LDA_topic', context_level=c('document','sentence'), copy=self$copy_on_modify){
        if (copy) {
          selfcopy = self$copy()$lda_topic_features(m=m, feature=feature, new_feature=new_feature, context_level=context_level, copy=F)
          return(selfcopy)
        }
        d = lda_features(tc=self, m=m, feature=feature, new_feature=new_feature, context_level=context_level)
        self$set(new_feature, d$v[order(d$i)], copy=F)
        invisible(self)
      },

## RESOURCES ##

     jrc_names = function(new_feature='jrc_names', feature='word', resource_path=getOption('tcorpus_resources', NULL), collocation_labels=T, batchsize=50000, low_memory=T, verbose=T, copy=self$copy_on_modify){
       if (copy) {
         selfcopy = self$copy()$jrc_names(new_feature=new_feature, feature=feature, resource_path=resource_path, collocation_labels=collocation_labels, batchsize=batchsize, low_memory=low_memory, verbose=verbose, copy=F)
         return(selfcopy)
       }
       jrc_names(self, new_feature=new_feature, feature=feature, resource_path=resource_path, collocation_labels=collocation_labels, batchsize=batchsize, low_memory=low_memory, verbose=verbose)
     },

     ## util
     set_keys = function(){
       ## ignore copy T or F, since setting keys is always a good thing
       if ('sent_i' %in% colnames(private$.data)){
         setkey(private$.data, 'doc_id', 'sent_i', 'word_i')
       } else {
         setkey(private$.data, 'doc_id', 'word_i')
       }
       setkey(private$.meta, 'doc_id')
       if (!is.null(private$.feature_index)) setkey(private$.feature_index, 'feature')
     },

     droplevels = function(copy=self$copy_on_modify){
       if (copy) {
         selfcopy = self$copy()$droplevels(copy=F)
         return(selfcopy)
       }
       private$.data = base::droplevels(private$.data)
       private$.meta = base::droplevels(private$.meta)
       invisible(self)
     }

   ),

   active = list(
     n = function() nrow(private$.data),
     n_meta = function() nrow(private$.meta),
     feature_names = function(e=NULL) {
       if (!is.null(e)) stop('Cannot change tcorpus$featurenames by assignment. Instead, use the set_colname() function')
       fnames = colnames(private$.data)[!colnames(private$.data) %in% c('doc_id','sent_i','word_i')]
     },
     names = function(e=NULL) {
       if (!is.null(e)) stop('Cannot change tcorpus$datanames by assignment. Instead, use the set_colname() function')
       colnames(private$.data)
     },
     meta_names = function(e=NULL) {
       if (!is.null(e)) stop('Cannot change tcorpus$metanames by assignment. Instead, use the set_meta_colname() function')
       colnames(private$.meta)
     },

     doc_id_levels = function(mod=NULL) {
       if (!is.null(mod)){
         private$.meta$doc_id = mod[match(levels(private$.data$doc_id), private$.meta$doc_id)]
         levels(private$.data$doc_id) = mod
       }
       levels(self$get('doc_id'))
     },

     data = function(mod=NULL) {
       ## access data directly, but with checks to protect structure.
       private$safe_data_mod(mod)
       data.table::copy(private$.data)
     },

     meta = function(mod=NULL) {
       ## access data directly, but with checks to protect structure.
       private$safe_meta_mod(mod)
       data.table::copy(private$.meta)
     }
   )
)

#tc = refresh_tcorpus(tc)
#hits = tc$search_features(keyword = 'fuel', subset_meta = doc_id == 'a')
#print(hits)

#' @export
print.tCorpus <- function(tc) {
  sent_info = if ('sent_i' %in% tc$names) paste(' and sentences (n = ', nrow(unique(tc$get(c('doc_id','sent_i')))), ')', sep='') else ''
  cat('tCorpus containing ', tc$n, ' tokens',
      '\nsplit by documents (n = ', tc$n_meta, ')', sent_info,
      '\ncontains:',
      '\n  - ', length(tc$names), ' data column', if (length(tc$names) > 1) '(s)', ':\t', paste(tc$names, collapse=', '),
      '\n  - ', length(tc$meta_names), ' meta column', if (length(tc$meta_names) > 1) '(s)', ': \t', paste(tc$meta_names, collapse=', '),
      '\n', sep='')
}

#' Refresh a tCorpus object using the current version of corpustools
#'
#' As an R6 class, tCorpus contains its methods within the class object (i.e. itself). Therefore, if you use a new version of corpustools with an older tCorpus object (e.g., stored as a .rds. file), then the methods are not automatically updated. You can then use refresh_tcorpus() to reinitialize the tCorpus object with the current version of corpustools.
#'
#' @param tc a tCorpus object
#'
#' @return a tCorpus object
#' @export
refresh_tcorpus <- function(tc){
  tCorpus$new(data=tc$.__enclos_env__$private$.data,
              meta=tc$.__enclos_env__$private$.meta,
              p = tc$.__enclos_env__$private$.p,
              feature_index = tc$.__enclos_env__$private$.feature_index)
}

rebuild_tcorpus <- function(tc) {
  tokens_to_tcorpus(tokens = tc$data,
                    doc_col = 'doc_id',
                    sent_i_col = ifelse('sent_i' %in% tc$names, T, F),
                    word_i_col = 'word_i',
                    meta = tc$meta)
}

#' @export
summary.tCorpus <- function(tc) {
  tc
}

#' @export
summary.tCorpus <- function(tc) tc

#' @export
as.tcorpus <- function(x) UseMethod('as.tcorpus')

#' @export
as.tcorpus.tCorpus <- function(x) x

#' @export
as.tcorpus.default <- function(x) stop('x has to be a tCorpus object')
## params: preprocess_params=list, filter_params,

is_tcorpus <- function(x, allow_stc=F){
  if (!class(x)[1] %in% c('tCorpus', 'shattered_tCorpus')) stop('not a tCorpus object')
  if (is_shattered(x) & !allow_stc) stop('function not implemented for shattered_tCorpus')
  TRUE
}

is_shattered <- function(x) is(x, 'shattered_tCorpus')

###  utility

safe_selection <- function(d, selection){
  if (any(is.na(selection))) stop('selection cannot contain NA')
  if (!is(selection, 'numeric') & !is(selection,'logical')) stop('selection has to be either a logical vector or a numerical vector (indices for TRUE values)')
  if (is(selection, 'numeric')) selection = 1:nrow(d) %in% selection
  selection
}

eval_subset <- function(d, subset){
  subset = if (is(substitute(subset), 'call')) deparse(substitute(subset)) else subset
  subset = if (is(substitute(subset), 'character')) parse(text=subset) else substitute(subset)
  eval(subset, d, parent.frame(2))
}

elip_names <- function(...) names(as.list(match.call()))[-1]

expr_names <- function(expr){
  if (!is(expr, 'character')) expr = deparse(substitute(expr))
  expr = expr[!expr %in% c('{','}')]
  expr = stringi::stri_trim(gsub('[<=[].*', '', expr))
  if (grepl('(', expr, fixed = T)) expr = gsub('.*\\((.*)[,\\)].*', '\\1', expr)
  expr
}

get_context <- function(tc, context_level = c('document','sentence'), with_labels=T){
  context_level = match.arg(context_level)

  if (context_level == 'document') {
    context = tc$get('doc_id')
    if (!with_labels) levels(context) = 1:length(levels(context))
  }
  if (context_level == 'sentence') {
    if (!'sent_i' %in% tc$names) stop('Sentence level not possible, since no sentence information is available. To enable sentence level analysis, use split_sentences = T in "create_tcorpus()" or specify sent_i_col in "tokens_to_tcorpus()"')
    d = tc$data
    if (with_labels){
      ucontext = unique(d[,c('doc_id','sent_i')])
      ucontext = stringi::stri_paste(ucontext$doc_id, ucontext$sent_i, sep=' #')
      context = fast_factor(global_position(d$sent_i, d$doc_id, presorted = T, position_is_local=T), levels = ucontext)
    } else {
      context = fast_dummy_factor(global_position(d$sent_i, d$doc_id, presorted = T, position_is_local=T))
    }
  }
  context
}
