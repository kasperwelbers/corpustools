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
         if (!identical(key(tc$data), c('doc_id', 'sent_i', 'token_i'))) setkeyv(mod, c('doc_id','sent_i','token_i'))
         if (nrow(unique(d, by=c('doc_id', 'sent_i', 'token_i'))) < nrow(d)) stop('After transformation, token_i is no longer unique')
         if (nrow(unique(d[,c('doc_id','token_i')])) < nrow(d)) stop('After transformation, token_i is not unique within documents')
       } else {
         if (!identical(key(tc$data), c('doc_id', 'token_i'))) setkeyv(mod, c('doc_id','token_i'))
         if (nrow(unique(d, by=c('doc_id', 'token'))) < nrow(d)) stop('After transformation, token_i is not unique within documents')
       }
     },

     safe_data_mod = function(mod) {
       if (is.null(mod)) return(NULL)
       if (nrow(mod) < self$n) stop('Replacement cannot have fewer rows. For subsetting, please use the $subset method')
       if (nrow(mod) > self$n) stop('Replacement cannot have more rows. For adding more data, please use the merge_tcorpora function or the $add_data method')

       mod = data.table(mod)
       if (any(grepl('^evalhere_', colnames(mod)))) stop('column names in a tCorpus cannot start with "evalhere_"') ## evalhere_ is used as a prefix for object names when set/subset are called within tCorpus methods. This is to prevent conflict when a tCorpus has an identically named column
       if (!all(c('doc_id','token_i') %in% colnames(mod))) stop('Replacement must have a doc_id and token_i column')
       if ('sent_i' %in% colnames(mod)) setkeyv(mod, c('doc_id','sent_i','token_i')) else setkeyv(mod, c('doc_id','token_i'))

       positioncols = intersect(c('doc_id','sent_i','token_i'), colnames(mod))
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
       self$set_keys()
     },

     safe_meta_mod = function(mod) {
       if (is.null(mod)) return(NULL)
       mod = data.table(mod)
       if (any(grepl('^evalhere_', colnames(mod)))) stop('column names in a tCorpus cannot start with "evalhere_"')
       if (!all(c('doc_id') %in% colnames(mod))) stop('Replacement must have a doc_id column')

       setkey(mod, 'doc_id')
       if (nrow(mod) < self$n_meta) stop('Replacement cannot have fewer rows. For subsetting, please use the $subset method')
       if (nrow(mod) > self$n_meta) stop('Replacement cannot have more rows. For adding more data, please use the merge_tcorpora function or the $add_data method')
       if (!identical(private$.meta$doc_id, mod$doc_id)) {
         stop('Cannot change doc_id. If you want to change doc_id labels, you can overwrite $doc_id_levels.')
       }
       private$.meta = mod
       self$set_keys()
     }

   ),

   public = list(
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
      if (!methods::is(private$.data[[column]], 'factor')) stop(sprintf('"%s" is not a factor', column))
      data.table::copy(levels(private$.data[[column]]))
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
        d = if (methods::is(d, 'data.frame')) d[exp_i,,drop=!keep_df & !is.null(columns)] else d[exp_i]
      }
      d
    },

    get_meta_levels = function(column){
      if (!methods::is(private$.meta[[column]], 'factor')) stop(sprintf('"%s" is not a factor', column))
      data.table::copy(levels(private$.meta[[column]]))
    },

     provenance = function(name=NULL) if (is.null(name)) private$.p else private$.p[[name]],

     feature_index = function(feature='token', context_level=c('document','sentence'), max_window_size=100, as_ascii=F){
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
       if (class(substitute(subset_tokens)) %in% c('call', 'name')) subset_tokens = eval(substitute(subset_tokens), private$.data, parent.frame())
       if (class(substitute(subset_meta)) %in% c('call', 'name')) subset_meta = eval(substitute(subset_meta), private$.meta, parent.frame())

       get_dtm(self, feature=feature, context_level=context_level, weight=weight, drop_empty_terms=drop_empty_terms, form=form,
               subset_tokens=subset_tokens, subset_meta=subset_meta, context=context, context_labels=context_labels,
               feature_labels=feature_labels, ngrams=ngrams, ngram_before_subset=ngram_before_subset)
     },

     print = function(doc_id, column='token', meta_columns = self$meta_names) {
       evalhere_doc_id = as.character(doc_id)

       d = private$.data[list(evalhere_doc_id),]
       d = split(d[[column]], f = d$doc_id)
       texts = stringi::stri_paste_list(d, sep = ' ')

       if (length(meta_columns) > 0) {
         meta = private$.meta[list(evalhere_doc_id), meta_columns, with=F]
         header = ''
         for (j in 1:ncol(meta)) {
           meta_field = stringi::stri_paste(colnames(meta)[j], meta[[j]], sep=': ')
           header = if (j == 1) meta_field else paste(header, meta_field, sep=', ')
         }
         texts = paste(header, texts, sep='\n\n')
       }

       texts = paste(texts, collapse = '\n------------\n\n')
       cat(texts)
       invisible(texts)
     },

## DATA MODIFICATION METHODS ##

     copy = function(){
       tCorpus$new(data = data.table::copy(private$.data),
                   meta = data.table::copy(private$.meta),
                   feature_index = private$.feature_index,
                   p = private$.p)
     },

     set = function(column, value, subset=NULL, subset_value=T){
       if (column == 'doc_id') stop('Cannot change doc_id. If you want to change doc_id labels, you can overwrite $doc_id_levels.')
       if (class(substitute(subset)) %in% c('call', 'name')) subset = eval(substitute(subset), private$.data, parent.frame())
       if (class(substitute(value)) %in% c('call', 'name')) value = eval(substitute(value), private$.data, parent.frame())
       if (grepl('^evalhere_', column)) stop('column names in a tCorpus cannot start with evalhere_')

       if (!is.null(subset)){
         if (subset_value & length(value) > 1) value = value[subset]
         if (!column %in% colnames(private$.data)) {
           private$.data[,(column) := NA]
           if (methods::is(value, 'factor')) {
             private$.data[,(column) := fast_factor(private$.data[[column]])]
           } else {
             private$.data[,(column) := methods::as(private$.data[[column]], class(value))]
           }
         }

         if (methods::is(private$.data[[column]], 'factor')) {
           value = fast_factor(value)
           levels(private$.data[[column]]) = c(levels(private$.data[[column]]), levels(value))
         }

         if (column %in% c('sent_i','token_i')) {  ## for position columns, first perform checks (inefficient, but this should be a rare case anyway)
            if (!methods::is(value, 'numeric')) stop('position column has to be numeric/integer')
            value = as.integer(value)
            mod = if ('sent_i' %in% self$names) private$.data[,c('doc_id','sent_i','token_i')] else private$.data[,c('doc_id','token_i')]
            mod[subset, (column) := value]
            check_unique_rows(mod)
         }
         private$.data[subset, (column) := value]

       } else {
         if (column %in% c('sent_i','token_i')) {
           if (!methods::is(value, 'numeric')) stop('position column has to be numeric/integer')
           value = as.integer(value)
           mod = if ('sent_i' %in% self$names) private$.data[,c('doc_id','sent_i','token_i')] else private$.data[,c('doc_id','token_i')]
           suppressWarnings(mod[, (column) := value])
           check_unique_rows(mod)
         }
         ## ugly suppress. Should look into why data.table give the (seemingly harmless) internal.selfref warning
         suppressWarnings(private$.data[,(column) := value])
       }
       if (identical(self$provenance()$index_feature, column)) self$reset_feature_index # reset feature index if necessary

       ## strangely, the assign by reference in data.table sometimes adds NA as a factor level...
       if (anyNA(levels(private$.data[[column]]))) {
         private$.data[,(column) := factor(private$.data[[column]])]
       }

       self$set_keys()
       invisible(self)
     },

     set_levels = function(column, levels) {
       if (!methods::is(private$.data[[column]], 'factor')) stop(sprintf('"%s" is not a factor', column))
       if (!length(levels) == length(levels(private$.data[[column]]))) stop('new levels of different length than current levels')
       if (column == 'doc_id') {
         private$.meta$doc_id = levels[match(levels(private$.data$doc_id), private$.meta$doc_id)]
       }
       data.table::setattr(private$.data[[column]], 'levels', levels)
     },

     delete_columns = function(cnames){
       protected_cols = intersect(self$names, c('doc_id', 'token_i'))
       if (any(protected_cols %in% cnames)) stop("The position columns doc_id and token_i cannot be deleted")
       for (col in cnames) private$.data[,(col) := NULL]
       invisible(self)
     },

     set_colname = function(oldname, newname) {
       if (oldname %in% c('doc_id','sent_i','token_i')) stop('The position columns (doc_id, sent_i, token_i) cannot be set or changed')
       if (grepl('^evalhere_', newname)) stop('column names in a tCorpus cannot start with evalhere_')

       data.table::setnames(private$.data, oldname, newname)
       invisible(self)
     },

     set_meta = function(column, value, subset=NULL, subset_value=T){
       if (column == 'doc_id') stop('Cannot change doc_id. If you want to change doc_id labels, you can overwrite $doc_id_levels.')
       if (class(substitute(subset)) %in% c('call', 'name')) subset = eval(substitute(subset), private$.meta, parent.frame())
       if (class(substitute(value)) %in% c('call', 'name')) value = eval(substitute(value), private$.meta, parent.frame())

       if (grepl('^evalhere_', column)) stop('column names in a tCorpus cannot start with evalhere_')

       if (!is.null(subset)){
         if (subset_value & length(value) > 1) value = value[subset]

         if (!column %in% colnames(private$.meta)) {
           private$.meta[[column]] = NA
           if (methods::is(value, 'factor')) {
             private$.meta[[column]] = fast_factor(private$.meta[[column]])
           } else {
             private$.meta[,(column) := methods::as(private$.meta[[column]], class(value))]
           }
         }
         if (methods::is(private$.meta[[column]], 'factor')) {
           value = fast_factor(value)
           levels(private$.meta[[column]]) = c(levels(private$.meta[[column]]), levels(value))
         }
         private$.meta[subset, (column) := value]

       } else {
         ## ugly suppress. Should look into why data.table give the (seemingly harmless) internal.selfref warning
         suppressWarnings(private$.meta[,(column) := value])
       }

       ## strangely, the assign by reference in data.table sometimes adds NA as a factor level...
       if (anyNA(levels(private$.meta[[column]]))) {
         private$.meta[,(column) := factor(private$.meta[[column]])]
       }
       invisible(self)
     },

     set_meta_levels = function(column, levels) {
        if (!methods::is(private$.meta[[column]], 'factor')) stop(sprintf('"%s" is not a factor', column))
        data.table::setattr(private$.meta[[column]], 'levels', levels)
      },

     delete_meta_columns = function(cnames){
        protected_cols = intersect(self$names, c('doc_id'))
        if (any(protected_cols %in% cnames)) stop('doc_id cannot be deleted')
        for (col in cnames) private$.meta[,(col) := NULL]
        invisible(self)
      },

     set_meta_colname = function(oldname, newname) {
       if (oldname %in% c('doc_id')) stop('The doc_id column cannot be set or changed')
       if (grepl('^evalhere_', newname)) stop('column names in a tCorpus cannot start with evalhere_')
       setnames(private$.meta, oldname, newname)
       invisible(self)
     },

     subset = function(subset=NULL, subset_meta=NULL, drop_levels=T, window=NULL, copy=F){
       if (class(substitute(subset)) %in% c('call', 'name')) subset = eval(substitute(subset), private$.data, parent.frame())
       if (class(substitute(subset_meta)) %in% c('call', 'name')) subset_meta = eval(substitute(subset_meta), private$.meta, parent.frame())

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

       if (drop_levels) self$droplevels()
       invisible(self)
     },

      subset_meta = function(subset=NULL, drop_levels=T, copy=F){
        ## subset also has a subset_meta argument, but we add this for consistency with other _meta methods
        if (class(substitute(subset)) %in% c('call', 'name')) subset = eval(substitute(subset), private$.meta, parent.frame())
        evalhere_subset = subset
        self$subset(subset_meta = evalhere_subset, drop_levels=drop_levels, copy=copy)
      },

      subset_i = function(subset=NULL, subset_meta=NULL, window=NULL, inverse=F){
        if (class(substitute(subset)) %in% c('call', 'name')) subset = eval(substitute(subset), private$.data, parent.frame())
        if (class(substitute(subset_meta)) %in% c('call', 'name')) subset_meta = eval(substitute(subset_meta), private$.meta, parent.frame())

        ## enable subset_i to be called from a character string. (e.g. used in search_features)
        if(methods::is(subset, 'character')) subset_meta = eval(parse(text=subset_meta), private$.meta, parent.frame())
        if(methods::is(subset_meta, 'character')) subset_meta = eval(parse(text=subset_meta), private$.meta, parent.frame())

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

     aggregate = function(meta_cols=NULL, hits=NULL, feature=NULL, count=c('documents','tokens'), wide=T){
        count = match.arg(count)

        meta = data.table::copy(private$.meta)
        if (is.null(meta_cols)) {
          meta[,group := 'total']
          meta_cols = 'group'
        }

        len = private$.data[, list(length=.N), by='doc_id']
        meta = merge(meta, len, by='doc_id')
        d = meta[, list(N=.N, V=sum(length)), by=meta_cols]

        if (!is.null(hits) | !is.null(feature)){
          if (!is.null(hits) & !is.null(feature)) stop('Cannot specify both hits and feature')
          if (!is.null(hits)) {
            if (!methods::is(hits, c('featureHits', 'contextHits'))) stop('hits must be a featureHits or contextHits object (see the $search_features and $search_contexts methods)')
            if (methods::is(hits, 'featureHits')) {
              coldata = hits$hits[!duplicated(hits$hits$hit_id),]
            } else {
              coldata = hits$hits
            }
          }

          if (!is.null(feature)) {
            coldata = data.frame(doc_id = private$.data$doc_id, code = private$.data[[feature]])
            coldata = coldata[!is.na(coldata$code),]
          }

          if (count == 'documents') coldata = coldata[!duplicated(coldata[,c('doc_id','code')]),]

          count_d = meta[list(coldata$doc_id), meta_cols, with=F]

          count_d$code = coldata$code
          agg_cols = c(meta_cols, 'code')

          count_d = count_d[, list(count=.N), by = agg_cols]
          d = merge(d, count_d, meta_cols)
          d$count[is.na(d$count)] = 0
          if (wide) d = dcast(d, ... ~ code, value.var='count')


        }
        as.data.frame(d)
      },


## FEATURE MANAGEMENT ##
     preprocess = function(column, new_column=column, lowercase=T, ngrams=1, ngram_context=c('document', 'sentence'), as_ascii=F, remove_punctuation=T, remove_stopwords=F, use_stemming=F, language='english') {
       invisible(preprocess_feature(self, column=column, new_column=new_column, lowercase=lowercase, ngrams=ngrams, ngram_context=ngram_context, as_ascii=as_ascii, remove_punctuation=remove_punctuation, remove_stopwords=remove_stopwords, use_stemming=use_stemming, language=language))
     },

     feature_subset = function(column, new_column=column, subset, inverse=F, copy=F){
       if (new_column %in% c('doc_id','sent_i','token_i')) stop('The position columns (doc_id, sent_i, token_i) cannot be used')
       if (class(substitute(subset)) %in% c('call', 'name')) subset = eval(substitute(subset), private$.data, parent.frame())

       if (copy) {
         selfcopy = self$copy()$feature_subset(column=column, new_column=new_column, subset=subset, inverse=inverse, copy=F)
         return(selfcopy)
       }

       if (is(subset, 'numeric')) subset = 1:self$n %in% subset ## this can be the case if a vector of indices is passed to subset (which is not a valid call, but is allowed for convenience because it is a common way of subsetting)

       evalhere_subset = if (inverse) !subset else subset

       if (new_column %in% self$names) {
          old_column = data.table::copy(private$.data[[column]])
          private$.data[, (new_column) := NA]
          self$set(new_column, old_column, subset = evalhere_subset)
       } else {
          self$set(new_column, private$.data[[column]], subset = evalhere_subset)
       }

       invisible(self)
     },

     feature_stats = function(feature, context_level=c('document','sentence')){
       term_statistics(self, feature=feature, context_level=context_level)
     },

     top_features = function(feature, n=10, group_by=NULL, group_by_meta=NULL, return_long=F){
       top_features(self, feature=feature, n=n, group_by=group_by, group_by_meta=group_by_meta, return_long=return_long)
     },

     feature_associations = function(keyword=NULL, condition=NA, hits=NULL, feature='token', window=15,  n=25, min_freq=1, sort_by= c('chi2', 'ratio', 'freq'), subset=NULL, subset_meta=NULL) {
       if (is.null(keyword) & is.null(hits)) stop('either keyword or hits has to be specified')
       if (!is.null(keyword) & !is.null(hits)) stop('only keyword or hits can be specified')
       if (!is.null(keyword)) hits = self$search_features(keyword=keyword, condition=condition)

       feature_associations(self, hits=hits, feature=feature, window=window, n=n, min_freq=min_freq, sort_by=sort_by, subset=subset, subset_meta=subset_meta)
     },


## SEARCHING / QUERYING ##
     search_features = function(keyword=NA, condition=NA, code=NA, queries=NULL, feature='token', condition_once=F, subset_tokens=NA, subset_meta=NA, keep_false_condition=F, only_last_mtoken=F, verbose=F){
       subset_tokens = if (class(substitute(subset_tokens)) %in% c('call')) deparse(substitute(subset_tokens)) else subset_tokens
       subset_meta = if (class(substitute(subset_meta)) %in% c('call')) deparse(substitute(subset_meta)) else subset_meta

       search_features(self, keyword=keyword, condition=condition, code=code, queries=queries, feature=feature, condition_once=condition_once, subset_tokens=subset_tokens, subset_meta=subset_meta, keep_false_condition=keep_false_condition, only_last_mtoken=only_last_mtoken, verbose=verbose)
     },

     code_features = function(keyword=NA, condition=NA, code=NA, queries=NULL, feature='token', column='code', condition_once=F, subset_tokens=NA, subset_meta=NA, only_last_mtoken=F, verbose=F){
       subset = if (class(substitute(subset)) %in% c('call')) deparse(substitute(subset)) else subset
       subset_meta = if (class(substitute(subset_meta)) %in% c('call')) deparse(substitute(subset_meta)) else subset_meta

       hits = search_features(self, keyword=keyword, condition=condition, code=code, queries=queries, feature=feature, condition_once=condition_once, subset_tokens=subset_tokens, subset_meta=subset_meta, keep_false_condition=F, only_last_mtoken=only_last_mtoken, verbose=verbose)

       #feature = private$.data
       evalhere_i = hits$hits$i
       evalhere_value = hits$hits$code
       self$set(column=column, subset=evalhere_i, value=evalhere_value, subset_value=F)
       #private$.data[hits$hits$i, (column) := hits$hits$code]

       invisible(self)
     },

     search_recode = function(feature, new_value, keyword, condition=NA, condition_once=F, subset_tokens=NA, subset_meta=NA){
       subset = if (class(substitute(subset_tokens)) %in% c('call')) deparse(substitute(subset_tokens)) else subset_tokens
       subset_meta = if (class(substitute(subset_meta)) %in% c('call')) deparse(substitute(subset_meta)) else subset_meta

       hits = self$search_features(keyword=keyword, condition=condition, condition_once=condition_once, subset_tokens=subset_tokens, subset_meta=subset_meta)
       evalhere_x = as.numeric(as.character(hits$hits$i))
       evalhere_new_value = new_value
       self$set(feature, evalhere_new_value, subset = evalhere_x)
       invisible(self)
     },

     search_contexts = function(query, code=NULL, feature='token', context_level=c('document','sentence'), verbose=F){
       search_contexts(self, query, code=code, feature=feature, context_level=context_level, verbose=verbose)
     },

     subset_query = function(query, feature='token', context_level=c('document','sentence'), copy=F){
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

     kwic = function(hits=NULL, i=NULL, feature=NULL, keyword=NULL, code='', ntokens=10, nsample=NA, output_feature='token', keyword_feature='token', context_level=c('document','sentence'), prettypaste=T, kw_tag=c('<','>'), ...){
       if (!is.null(keyword)) hits = self$search_features(keyword=keyword, code=code, feature = keyword_feature, ...)
       keyword_in_context(self, hits=hits, i=i, code=code, ntokens=ntokens, nsample=nsample, output_feature=output_feature, context_level=context_level, prettypaste=prettypaste, kw_tag=kw_tag)
     },

## CO-OCCURRENCE NETWORKS ##
     semnet = function(feature, measure=c('cosine', 'con_prob', 'con_prob_weighted', 'count_directed', 'count_undirected', 'chi2'), context_level=c('document','sentence'), backbone=F, n.batches=NA){
       measure = match.arg(measure)
       if (!requireNamespace('igraph', quietly = T)) stop('igraph package needs to be installed in order to use semnet methods')
       semnet(self, feature=feature, measure=measure, context_level=context_level, backbone=backbone, n.batches=n.batches)
     },

     semnet_window = function(feature, measure=c('cosine', 'con_prob', 'count_directed', 'count_undirected', 'chi2'), context_level=c('document','sentence'), window.size=10, direction='<>', backbone=F, n.batches=NA, set_matrix_mode=c(NA, 'windowXwindow','positionXwindow')){
       measure = match.arg(measure)
       if (!requireNamespace('igraph', quietly = T)) stop('igraph package needs to be installed in order to use semnet methods')
       semnet_window(self, feature=feature, measure=measure, context_level=context_level, window.size=window.size, direction=direction, backbone=backbone, n.batches=n.batches, set_matrix_mode=set_matrix_mode)
     },
## CORPUS COMPARISON ##

     compare_corpus = function(tc_y, feature, smooth=0.1, min_ratio=NULL, min_chi2=NULL, is_subset=F, yates_cor=c('auto','yes','no'), what=c('freq','docfreq','cooccurrence')){
       if (is_subset & self$n > tc_y$n) stop('tCorpus x (the one calling the method) cannot be a subset of tCorpus y, because it has more tokens')
       what = match.arg(what)
       tcorpus_compare(self, tc_y, feature, smooth=smooth, min_ratio=min_ratio, min_chi2=min_chi2, yates_cor=yates_cor, x_is_subset=is_subset, what=what)
     },

     compare_subset = function(feature, subset_x=NULL, subset_meta_x=NULL, query_x=NULL, query_feature='token', smooth=0.1, min_ratio=NULL, min_chi2=NULL, yates_cor=c('auto','yes','no'), what=c('freq','docfreq','cooccurrence')){
       subset_x = eval(substitute(subset_x), private$.meta, parent.frame(2))
       subset_meta_x = eval(substitute(subset_meta_x), private$.meta, parent.frame(2))
       what = match.arg(what)

       if(is.null(subset_x) & is.null(subset_meta_x) & is.null(query_x)) stop("at least one of subset_x, subset_meta_x or query_x has to be specified")
       if(!is.null(subset_x) | !is.null(subset_meta_x)) {
         evalhere_subset_x = subset_x
         evalhere_subset_meta_x = subset_meta_x
         tc_x = self$subset(subset=evalhere_subset_x, subset_meta = evalhere_subset_meta_x, copy=T)
       }
       if(!is.null(query_x)) tc_x = self$subset_query(query_x, feature=query_feature, copy=T)

       comp = tc_x$compare_corpus(self, feature=feature, smooth=smooth, min_ratio=min_ratio, min_chi2=min_chi2, yates_cor=yates_cor, is_subset=T, what=what)
       comp
     },

## DOCUMENT COMPARISON ##

     compare_documents = function(feature='token', date_col=NULL, hour_window=NULL, measure=c('cosine','overlap_pct'), min_similarity=0, weight=c('norm_tfidf', 'tfidf', 'termfreq','docfreq'), ngrams=NA, from_subset=NULL, to_subset=NULL) {
       if (!requireNamespace('RNewsflow', quietly = T)) stop('RNewsflow package needs to be installed in order to use document comparison methods')
        weight = match.arg(weight)
        from_subset = eval(substitute(from_subset), private$.meta, parent.frame(2))
        to_subset = eval(substitute(to_subset), private$.meta, parent.frame(2))
        compare_documents_fun(self, feature=feature, date_col=date_col, hour_window=hour_window, measure=measure, min_similarity=min_similarity, weight=weight, ngrams=ngrams, from_subset=from_subset, to_subset=to_subset)
     },

     deduplicate = function(feature='token', date_col=NULL, meta_cols=NULL, hour_window=NULL, min_docfreq=2, max_docfreq_pct=0.5, measure=c('cosine','overlap_pct'), similarity=1, keep=c('first','last', 'random'), weight=c('norm_tfidf', 'tfidf', 'termfreq','docfreq'), ngrams=NA, print_duplicates=F, copy=F){
       if (!requireNamespace('RNewsflow', quietly = T)) stop('RNewsflow package needs to be installed in order to use document comparison methods')

       weight = match.arg(weight)
       match.arg(feature, self$feature_names)
       if (copy) {
         selfcopy = self$copy()$deduplicate(feature=feature, date_col=date_col, meta_cols=meta_cols, hour_window=hour_window, min_docfreq=min_docfreq, max_docfreq_pct=max_docfreq_pct, measure=measure, similarity=similarity, keep=keep, weight=weight, ngrams=ngrams, print_duplicates=print_duplicates, copy=F)
         return(selfcopy)
       }

       ## adding DEDUPLICATE_FEATURE is not very elegant and memory efficient. Better alternative, perhaps, is to pass docfreq_filter results to compare_documents_fun.
       evalhere_feature = feature
       self$set('DEDUPLICATE_FEATURE', self$get(evalhere_feature))
       self$feature_subset('DEDUPLICATE_FEATURE', 'DEDUPLICATE_FEATURE', subset = docfreq_filter('DEDUPLICATE_FEATURE', min=min_docfreq, max=self$n * max_docfreq_pct), copy=F)

       evalhere_duplicates = get_duplicates(self, feature='DEDUPLICATE_FEATURE', date_col=date_col, meta_cols=meta_cols, hour_window=hour_window, measure=measure, similarity=similarity, keep=keep, weight=weight, print_duplicates=print_duplicates)
       self$subset(subset_meta = !doc_id %in% evalhere_duplicates, copy=F)
       self$set('DEDUPLICATE_FEATURE', NULL)
       invisible(self)
     },

## TOPIC MODELING ##

      lda_fit = function(feature, create_feature=NULL, K=50, num.iterations=500, alpha=50/K, eta=.01, burnin=250, context_level=c('document','sentence'), ...) {
        if (!requireNamespace('topicmodels', quietly = T)) stop('topicmodels package needs to be installed in order to use LDA')

        dtm = self$dtm(feature=feature, context_level=context_level, ...)
        m = lda_fit(dtm=dtm, method='Gibbs', K=K, num.iterations=num.iterations, alpha=alpha, eta=eta, burnin=burnin)
        if (!is.null(create_feature)) self$lda_topic_features(m=m, feature=feature, new_feature=create_feature, context_level=context_level)
        m
      },

      lda_topic_features = function(m, feature, new_feature='LDA_topic', context_level=c('document','sentence')){
        evalhere_d = lda_features(tc=self, m=m, feature=feature, new_feature=new_feature, context_level=context_level)
        self$set(new_feature, evalhere_d$v[order(evalhere_d$i)])
        invisible(self)
      },

## UTIL
     set_keys = function(){
       ## ignore copy T or F, since setting keys is always a good thing
       if ('sent_i' %in% colnames(private$.data)){
         setkey(private$.data, 'doc_id', 'sent_i', 'token_i')
       } else {
         setkey(private$.data, 'doc_id', 'token_i')
       }
       setkey(private$.meta, 'doc_id')
       if (!is.null(private$.feature_index)) setkey(private$.feature_index, 'feature')
     },

     droplevels = function(){
       private$.data = base::droplevels(private$.data)
       private$.meta = base::droplevels(private$.meta)
       invisible(self)
     }

   ),

   active = list(
     n = function() nrow(private$.data),
     n_meta = function() nrow(private$.meta),
     feature_names = function(e=NULL) {
       if (!is.null(e)) stop('Cannot change tcorpus$feature_names by assignment. Instead, use the set_colname() function')
       fnames = colnames(private$.data)[!colnames(private$.data) %in% c('doc_id','sent_i','token_i')]
     },
     names = function(e=NULL) {
       if (!is.null(e)) stop('Cannot change tcorpus$names by assignment. Instead, use the set_colname() function')
       colnames(private$.data)
     },
     meta_names = function(e=NULL) {
       if (!is.null(e)) stop('Cannot change tcorpus$meta_names by assignment. Instead, use the set_meta_colname() function')
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


#' S3 print for tCorpus class
#'
#' @param x a tCorpus object
#' @param ... not used
#'
#' @method print tCorpus
#' @export
print.tCorpus <- function(x, ...) {
  sent_info = if ('sent_i' %in% x$names) paste(' and sentences (n = ', nrow(unique(x$get(c('doc_id','sent_i')))), ')', sep='') else ''
  cat('tCorpus containing ', x$n, ' tokens',
      '\nsplit by documents (n = ', x$n_meta, ')', sent_info,
      '\ncontains:',
      '\n  - ', length(x$names), ' data column', if (length(x$names) > 1) 's', ':\t', paste(x$names, collapse=', '),
      '\n  - ', length(x$meta_names), ' meta column', if (length(x$meta_names) > 1) 's', ': \t', paste(x$meta_names, collapse=', '),
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
                    token_i_col = 'token_i',
                    meta = tc$meta)
}

#' Summary of a tCorpus object
#'
#' @param object A tCorpus object
#' @param ... not used
#'
#' @method summary tCorpus
#' @export
summary.tCorpus <- function(object, ...) object

#' Force an object to be a tCorpus class
#'
#' @param x the object to be forced
#' @param ... not used
#'
#' @export
as.tcorpus <- function(x, ...) UseMethod('as.tcorpus')

#' Force an object to be a tCorpus class
#'
#' @param x the object to be forced
#' @param ... not used
#'
#' @export
as.tcorpus.tCorpus <- function(x, ...) x

#' Force an object to be a tCorpus class
#'
#' @param x the object to be forced
#' @param ... not used
#'
#' @export
as.tcorpus.default <- function(x, ...) stop('x has to be a tCorpus object')
## params: preprocess_params=list, filter_params,

is_tcorpus <- function(x, allow_stc=F){
  if (!class(x)[1] %in% c('tCorpus', 'shattered_tCorpus')) stop('not a tCorpus object')
  if (is_shattered(x) & !allow_stc) stop('function not implemented for shattered_tCorpus')
  TRUE
}

is_shattered <- function(x) methods::is(x, 'shattered_tCorpus')

###  utility

safe_selection <- function(d, selection){
  if (any(is.na(selection))) stop('selection cannot contain NA')
  if (!methods::is(selection, 'numeric') & !methods::is(selection,'logical')) stop('selection has to be either a logical vector or a numerical vector (indices for TRUE values)')
  if (methods::is(selection, 'numeric')) selection = 1:nrow(d) %in% selection
  selection
}

eval_subset <- function(d, subset){
  subset = if (class(substitute(subset)) %in% c('call')) deparse(substitute(subset)) else subset
  subset = if (methods::is(substitute(subset), 'character')) parse(text=subset) else substitute(subset)
  eval(subset, d, parent.frame(2))
}

elip_names <- function(...) names(as.list(match.call()))[-1]

expr_names <- function(expr){
  if (!methods::is(expr, 'character')) expr = deparse(substitute(expr))
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
