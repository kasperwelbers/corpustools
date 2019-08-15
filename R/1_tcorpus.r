## Here the main tCorpus class is created
## Additional methods are specified in the matching r files (e.g. dtm method in document_term_matrix.r)

## The additional methods should always use only the accessor methods (so never use self$tokens and self$meta directly).
## This way, all validations, assumptions and optimalizations stay intact

## this file has to be named 1_*, because in building the package the r files are processed in order by name,
## and the tCorpus class has to be created before the additional methods

tCorpus <- R6::R6Class("tCorpus",
   cloneable=FALSE,

   private = list(
     .p = list(),

     sync = function(){
       doc_ids = intersect(unique(self$tokens$doc_id), unique(self$meta$doc_id))
       self$tokens = self$tokens[list(doc_ids),]
       self$meta = self$meta[list(doc_ids),]
       private$set_keys()
     },

     check_unique_rows = function(d){
       if (anyDuplicated(d, by = c('doc_id','token_id'))) stop('After transformation, token_id is not unique within documents')
     },

     set_keys = function(){
       if (!identical(key(self$tokens), c('doc_id', 'token_id'))) setkey(self$tokens, 'doc_id', 'token_id')
       if (!identical(key(self$meta), c('doc_id'))) setkey(self$meta, 'doc_id')
     },

     droplevels = function(){
       self$tokens = base::droplevels(self$tokens)
       self$meta = base::droplevels(self$meta)
       invisible(self)
     },

     select_rows = function(selection, keep_meta=F) {
       selection = safe_selection(self$tokens, selection)
       self$tokens = subset(self$tokens, selection)
       if (!keep_meta) {
         self$meta = self$meta[as.character(unique(self$tokens$doc_id)),,nomatch=0]
         self$meta$doc_id = as.character(self$meta$doc_id)
       }
       private$set_keys()
     },

     select_meta_rows = function(selection, keep_data=F) {
       selection = safe_selection(self$meta, selection)
       self$meta = subset(self$meta, selection)
       self$meta$doc_id = as.character(self$meta$doc_id)
       if (!keep_data) self$tokens = self$tokens[as.character(unique(self$meta$doc_id)),,nomatch=0]
       private$set_keys()
     }

   ),

   public = list(
     help = function() ?tCorpus,

     tokens = NULL,
     meta = NULL,

     validate = function() {
        self$validate_tokens()
        self$validate_meta()
     },
     validate_tokens = function() {
        cnames = self$names
        e = c()
        if (!'doc_id' %in% cnames) e = c(e, '\t- tokens data does not have a "doc_id" column. You can try to fix the data in $tokens')
        if (!'token_id' %in% cnames) e = c(e, '\t- tokens data does not have "token_id" column. You can try to fi the data in $tokens')
        if (any(grepl('^\\.', cnames))) e = c(e, '\t- column names in a tCorpus cannot start with a dot. You can change the column names in $tokens or with the $set_name method')
        if (length(e) > 0) {
          e_message = paste0('TOKEN DATA BROKEN:\n', paste(e, collapse='\n'))
          stop(e_message)
        }

        if (!identical(data.table::key(self$tokens), c('doc_id','token_id'))) data.table::setkeyv(self$tokens, c('doc_id','token_id'))
     },

     validate_meta = function() {
       cnames = self$meta_names
       e = c()
       if (!'doc_id' %in% cnames) e = c(e, '\t- meta data does not have a "doc_id" column. You can try to fix the data in $meta')
       if (any(grepl('^\\.', cnames)))  e = c(e, '\t- column names in a tCorpus cannot start with a dot. You can change the column names in $meta or with the $set_meta_name method')
       if (length(e) > 0) {
         e_message = paste0('META DATA BROKEN:\n', paste(e, collapse='\n'))
         stop(e_message)
       }

       if (!identical(data.table::key(self$meta), 'doc_id')) data.table::setkeyv(self$meta, 'doc_id')
     },

     initialize = function(tokens, meta) {
       self$tokens = data.table(tokens)
       self$meta = data.table(meta)
       private$set_keys()
     },

     finalize = function() {
       forget_all_mem()
     },

     copy = function(){
       tCorpus$new(tokens = data.table::copy(self$tokens),
                   meta = data.table::copy(self$meta))
     },

## SHOW/GET DATA METHODS ##
    get = function(columns=NULL, keep_df=F, as.df=F, subset=NULL, doc_id=NULL, token_id=NULL, copy=F) {
      self$validate_tokens()

      if (class(substitute(subset)) %in% c('call', 'name')) subset = self$eval(substitute(subset), parent.frame())
      if (!is.null(doc_id) && !is.null(subset)) stop('Cannot filter by subset and doc_ids at the same time')
      if (is.null(doc_id) && !is.null(token_id)) stop('token_id can only be given in pairs with doc_id')

      if (is.null(doc_id) && is.null(subset)) {
        if (is.null(columns)) {
          d = self$tokens
        } else {
          d = self$tokens[,columns,with=F]
        }
      } else {
        if (is.null(columns)) columns = colnames(self$tokens)
        if (!is.null(subset)) d = self$tokens[subset,columns,with=F]
        if (!is.null(doc_id)) {
          if (is.null(token_id)) {
            positions = list(doc_ids = as.character(doc_id))
            d = self$tokens[positions,columns,with=F]
          } else {
            if (!length(doc_id) == length(token_id)) stop('token_id can only be given in pairs with doc_id')
            positions = list(doc_ids = as.character(doc_id), token_is = as.numeric(token_id))
            d = self$tokens[positions,columns,with=F]
          }
        }
      }

      if (ncol(d) == 1 && !keep_df) d = d[,1][[1]]
      if (as.df) d = as.data.frame(d)
      if(copy) data.table::copy(d) else d
    },

    get_levels = function(column){
      if (!column %in% self$names) stop(sprintf('"%s" column does not exists in tokens', column))
      if (!methods::is(self$tokens[[column]], 'factor')) stop(sprintf('"%s" is not a factor', column))
      data.table::copy(levels(self$tokens[[column]]))
    },

    get_meta = function(columns=NULL, keep_df=F, as.df=F, per_token=F, subset=NULL, doc_id=NULL, copy=T) {
      self$validate()

      if (class(substitute(subset)) %in% c('call', 'name')) subset = self$eval_meta(substitute(subset), parent.frame())
      if (!is.null(doc_id) && !is.null(subset)) stop('Cannot filter by subset and doc_ids at the same time')


      if (is.null(doc_id) && is.null(subset)) {
        if (is.null(columns)) {
          d = self$meta
        } else {
          d = self$meta[,columns,with=F]
        }
      } else {
        if (is.null(columns)) columns = colnames(self$meta)
        if (!is.null(doc_id)) {
          doc_ids = as.character(doc_id)
          d = self$meta[list(doc_ids),columns,with=F]
        }
        if (!is.null(subset)) self$meta[subset,columns,with=F]
      }

      if (ncol(d) == 1 && !keep_df) d = d[,1][[1]]
      if (as.df) d = as.data.frame(d)
      if (per_token) {
        exp_i = match(self$tokens$doc_id, self$meta$doc_id)
        d = if (methods::is(d, 'data.frame')) d[exp_i,,drop=!keep_df & !is.null(columns)] else d[exp_i]
      }
      if(copy) data.table::copy(d) else d
    },

    get_meta_levels = function(column){
      if (!column %in% self$meta_names) stop(sprintf('"%s" column does not exists in meta', column))
      if (!methods::is(self$meta[[column]], 'factor')) stop(sprintf('"%s" is not a factor', column))
      data.table::copy(levels(self$meta[[column]]))
    },

    context = function(context_level = c('document','sentence'), with_labels=T){
       get_context(self, context_level = context_level, with_labels=with_labels)
     },

    eval = function(x, enclos=parent.frame()) eval(x, self$tokens, enclos),
    eval_meta = function(x, enclos=parent.frame()) eval(x, self$meta, enclos),

    get_token_id = function(doc_id=NULL, token_id=NULL, subset=NULL, subset_meta=NULL, window=NULL, inverse=F){
      self$validate()

      if (class(substitute(subset)) %in% c('call', 'name')) subset = self$eval(substitute(subset), parent.frame())
      if (class(substitute(subset_meta)) %in% c('call', 'name')) subset_meta = self$eval_meta(substitute(subset_meta), parent.frame())
      if (is.null(doc_id) && !is.null(token_id)) stop('token_id can only be given in pairs with doc_id')

      ## enable subset to be called from a character string. (e.g. used in search_features)
      if(methods::is(subset, 'character')) subset = eval(parse(text=subset), self$tokens, parent.frame())
      if(methods::is(subset_meta, 'character')) subset_meta = eval(parse(text=subset_meta), self$meta, parent.frame())

      i = NULL
      if (!is.null(doc_id)) {
        pos = if (is.null(token_id)) list(doc_id) else list(doc_id, token_id)
        i = self$tokens[pos, which=T]
      }
      if (!is.null(subset)) {
        subset[is.na(subset)] = F
        i = if (is.null(i)) which(subset) else intersect(i, which(subset))
      }
      if (!is.null(subset_meta)) {
        subset_meta[is.na(subset_meta)] = F
        doc_ids = self$get_meta('doc_id')[subset_meta]
        meta_i = self$tokens[list(doc_ids), which=T]
        i = if (is.null(i)) meta_i else intersect(i, meta_i)
      }

      if (!is.null(window)) i = which(i_window(self, i=i, window=window))
      if (inverse) i = !1:self$n %in% i
      i
    },


## DATA MODIFICATION METHODS ##

     set_special = function(token=NULL, lemma=NULL, POS=NULL, relation=NULL, parent=NULL) {
       if(!is.null(token)) self$set_name(token, 'token')
       if(!is.null(lemma)) self$set_name(lemma, 'lemma')
       if(!is.null(POS)) self$set_name(POS, 'POS')
       if(!is.null(relation)) self$set_name(relation, 'relation')
       if(!is.null(parent)) self$set_name(parent, 'parent')
     },

     set = function(column, value, subset=NULL, subset_value=T){
       self$validate_tokens()

       if (column == 'doc_id') stop('Cannot change doc_id. If you want to change doc_id labels, you can overwrite $doc_id_levels.')
       if (class(substitute(subset)) %in% c('call', 'name')) subset = self$eval(substitute(subset), parent.frame())
       if (class(substitute(value)) %in% c('call', 'name')) value = self$eval(substitute(value), parent.frame())
       if (grepl('^\\.', column)) stop('column names in a tCorpus cannot start with a dot')

       if (!is.null(subset)){
         if (subset_value && length(value) > 1) value = value[subset]
         if (!column %in% colnames(self$tokens)) {
           self$tokens[,(column) := NA]
           if (methods::is(value, 'factor')) {
             self$tokens[,(column) := fast_factor(self$tokens[[column]])]
           } else {
             self$tokens[,(column) := methods::as(self$tokens[[column]], class(value))]
           }
         }

         if (column %in% c('sentence','token_id')) {  ## for position columns, first perform checks (inefficient, but this should be a rare case anyway)
            if (!methods::is(value, 'numeric')) stop('position column has to be numeric/integer')
            value = as.numeric(value)
            mod = if ('sentence' %in% self$names) self$tokens[,c('doc_id','sentence','token_id')] else self$tokens[,c('doc_id','token_id')]
            mod[subset, (column) := value]
            check_unique_rows(mod)
         }

         .value = value
         self$tokens[subset, (column) := .value]

       } else {
         if (column %in% c('sentence','token_id')) {
           if (!methods::is(value, 'numeric')) stop('position column has to be numeric/integer')
           value = as.numeric(value)
           mod = if ('sentence' %in% self$names) self$tokens[,c('doc_id','sentence','token_id')] else self$tokens[,c('doc_id','token_id')]
           suppressWarnings(mod[, (column) := value])
           check_unique_rows(mod)
         }
         if (is.character(value)) value = fast_factor(value)  ## force the use of factors for character vectors.
         ## ugly suppress. Should look into why data.table give the (seemingly harmless) internal.selfref warning
         suppressWarnings(self$tokens[,(column) := value])
       }

       ## the assign by reference in data.table sometimes adds NA as a factor level...
       if (anyNA(levels(self$tokens[[column]]))) {
         self$tokens[,(column) := fast_factor(self$tokens[[column]])]
       }

       private$set_keys()
       self$tokens[]
       self$meta[]
       invisible(self)
     },

     set_levels = function(column, levels) {
       if (!column %in% self$names) stop(sprintf('"%s" column does not exists in tokens', column))
       if (!methods::is(self$tokens[[column]], 'factor')) stop(sprintf('"%s" is not a factor', column))
       if (!length(levels) == length(levels(self$tokens[[column]]))) stop('new levels of different length than current levels')
       if (column == 'doc_id') {
         self$meta$doc_id = levels[match(levels(self$tokens$doc_id), self$meta$doc_id)]
       }
       data.table::setattr(self$tokens[[column]], 'levels', levels)
     },

     delete_columns = function(cnames){
       protected_cols = intersect(self$names, c('doc_id', 'token_id'))
       if (any(protected_cols %in% cnames)) stop("The position columns doc_id and token_id cannot be deleted")
       for (col in cnames) self$tokens[,(col) := NULL]
       invisible(self)
     },

     set_name = function(oldname, newname) {
       if (oldname %in% c('doc_id','token_id')) stop('The position columns (doc_id, token_id) cannot be set or changed')
       if (grepl('^\\.', newname)) stop('column names in a tCorpus cannot start with a dot')

       data.table::setnames(self$tokens, oldname, newname)
       invisible(self)
     },

     set_meta = function(column, value, subset=NULL, subset_value=T){
       self$validate_meta()

       if (column == 'doc_id') stop('Cannot change doc_id. If you want to change doc_id labels, you can overwrite $doc_id_levels.')
       if (class(substitute(subset)) %in% c('call', 'name')) subset = self$eval_meta(substitute(subset), parent.frame())
       if (class(substitute(value)) %in% c('call', 'name')) value = self$eval_meta(substitute(value), parent.frame())

       if (grepl('^\\.', column)) stop('column names in a tCorpus cannot start with a dot')

       if (!is.null(subset)){
         if (subset_value && length(value) > 1) value = value[subset]

         if (!column %in% colnames(self$meta)) {
           self$meta[[column]] = NA
           if (methods::is(value, 'factor')) {
             self$meta[[column]] = fast_factor(self$meta[[column]])
           } else {
             self$meta[,(column) := methods::as(self$meta[[column]], class(value))]
           }
         }

         .value = value
         self$meta[subset, (column) := .value]

       } else {
         ## ugly suppress. Should look into why data.table give the (seemingly harmless) internal.selfref warning
         suppressWarnings(self$meta[,(column) := value])
       }

       ## strangely, the assign by reference in data.table sometimes adds NA as a factor level...
       if (anyNA(levels(self$meta[[column]]))) {
         self$meta[,(column) := fast_factor(self$meta[[column]])]
       }
       self$tokens[]
       self$meta[]
       invisible(self)
     },

     set_meta_levels = function(column, levels) {
        if (!column %in% self$meta_names) stop(sprintf('"%s" column does not exists in meta', column))
        if (!methods::is(self$meta[[column]], 'factor')) stop(sprintf('"%s" is not a factor', column))
        data.table::setattr(self$meta[[column]], 'levels', levels)
      },

     delete_meta_columns = function(cnames){
        protected_cols = intersect(self$names, c('doc_id'))
        if (any(protected_cols %in% cnames)) stop('doc_id cannot be deleted')
        for (col in cnames) self$meta[,(col) := NULL]
        invisible(self)
      },

     set_meta_name = function(oldname, newname) {
       if (oldname %in% c('doc_id')) stop('The doc_id column cannot be set or changed')
       if (grepl('^\\.', newname)) stop('column names in a tCorpus cannot start with a dot')
       setnames(self$meta, oldname, newname)
       invisible(self)
     },

     subset = function(subset=NULL, subset_meta=NULL, hits=NULL, window=NULL, copy=F, ...){
       self$validate()

       if (class(substitute(subset)) %in% c('call', 'name')) subset = self$eval(substitute(subset), parent.frame())
       if (class(substitute(subset_meta)) %in% c('call', 'name')) subset_meta = self$eval_meta(substitute(subset_meta), parent.frame())
       if (copy) {
         selfcopy = self$copy()$subset(subset=subset, subset_meta=subset_meta, hits=hits, window=window, copy=F)
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
       if (!is.null(subset_meta) && !is.null(subset)) {
         private$select_meta_rows(subset_meta, keep_data = T) ## if both subsets are used, first perform both without subseting the other, then match on doc_ids.
         private$select_rows(subset, keep_meta = T)           ## (we cannot subset one before the other, because subset call's can contains vectors for which the rows should match)
         private$sync()
       }
       if (!is.null(subset_meta) && is.null(subset)) {
         private$select_meta_rows(subset_meta, keep_data = F)
       }
       if (is.null(subset_meta) && !is.null(subset)) {
         private$select_rows(subset, keep_meta = F)
       }

       private$droplevels()
       self$tokens[]
       self$meta[]
       invisible(self)
     },

     subset_meta = function(subset=NULL, copy=F){
        ## subset also has a subset_meta argument, but we add this for consistency with other _meta methods
        if (class(substitute(subset)) %in% c('call', 'name')) subset = self$eval_meta(substitute(subset), parent.frame())
        .subset = subset
        self$subset(subset_meta = .subset, copy=copy)
      },

     aggregate = function(meta_cols=NULL, hits=NULL, feature=NULL, count=c('documents','tokens', 'hits'), wide=T){
        self$validate()

        count = match.arg(count)

        meta = data.table::copy(self$meta)
        if (is.null(meta_cols)) {
          meta[,group := 'total']
          meta_cols = 'group'
        }

        len = self$tokens[, list(len=.N), by='doc_id']
        meta = merge(meta, len, by='doc_id', all.x=T)
        d = meta[, list(N=.N, V=sum(len)), by=meta_cols]

        if (!is.null(hits) | !is.null(feature)){
          if (!is.null(hits) && !is.null(feature)) stop('Cannot specify both hits and feature')
          if (!is.null(hits)) {
            if (!methods::is(hits, c('featureHits', 'contextHits'))) stop('hits must be a featureHits or contextHits object (see the search_features and search_contexts functions)')
            if (methods::is(hits, 'featureHits')) {
              coldata = hits$hits[!duplicated(hits$hits[,c('code', 'hit_id')]),]
            } else {
              coldata = hits$hits
            }
          }

          if (!is.null(feature)) {
            coldata = data.frame(doc_id = self$tokens$doc_id, code = self$tokens[[feature]])
            coldata = coldata[!is.na(coldata$code),]
          }

          if (count == 'documents') coldata = coldata[!duplicated(coldata[,c('doc_id','code')]),]
          if (count == 'hits') coldata = coldata[!duplicated(coldata[,c('doc_id','code','hit_id')]),]

          meta = merge(meta, coldata, by='doc_id', all=T)
          count_d = subset(meta, select = c('code', meta_cols))
          agg_cols = c(meta_cols, 'code')

          count_d = count_d[, list(count=.N), by = agg_cols]
          d = merge(d, count_d, meta_cols, all=T)
          d$count[is.na(d$count)] = 0
          if (wide) {
            d = dcast(d, ... ~ code, value.var='count')
            d[['NA']] = NULL
            d[is.na(d)] = 0
          } else {
            d = subset(d, !is.na(code))
          }
        }

        as.data.frame(d)
      },

      lookup = function(x, feature='token', ignore_case=TRUE, batchsize=25, raw_regex=FALSE, fixed=FALSE, with_i=FALSE, as_ascii=FALSE, sub_query=list(), only_context=F, subcontext=NULL){
        forget_if_new(self$n) ## reset cache if n changes (possibly add some more indicators?)
        ## replace with max cache size once implemented in memoise

        has_sub_query = length(sub_query) > 0
        if (has_sub_query) {
          sub_feature = names(sub_query)[1]
          sub_x = sub_query[[sub_feature]]
          sub_query[[sub_feature]] = NULL
          sub_out = self$lookup(sub_x, feature=sub_feature, ignore_case=TRUE, sub_query=sub_query, only_context=F)
        }

        if (any(x == '*')) {
          if (only_context) {
            out = unique(self$tokens, by = c('doc_id', subcontext))
          } else {
            out = data.table::copy(self$tokens)
          }
          if (with_i && !only_context) out[, i:=1:nrow(out)]
          if (has_sub_query) {
            if (is.null(sub_out)) return(NULL)
            out = data.table::fintersect(out, sub_out)
            if (is.null(out)) return(NULL)
          }
          return(out)
        }

        ## prepare lookup table: set a (secondary) data.table index
        if (!feature %in% indices(self$tokens)) {

          data.table::setindexv(self$tokens, feature)
          message(sprintf('created index for "%s" column', feature))
        }

        ## if not fixed (exact value matching), first lookup x as regex in unique values
        ## note that mem_lookup_terms is memoised
        if (!fixed) {
          uval = if (is.factor(self$tokens[[feature]])) levels(self$tokens[[feature]]) else unique(self$tokens[[feature]])
          x = mem_lookup_terms(x, uval, ignore_case=ignore_case, raw_regex=raw_regex, batchsize=batchsize, useBytes=T, as_ascii=as_ascii)
        }

        if (length(x) == 0) return(NULL)
        if (with_i && !only_context) {
          i = na.omit(self$tokens[list(x), on=feature, which=T])
          out = self$tokens[i,]
          out[,i:=i]
        } else {
          out = self$tokens[list(x), on=feature, which=F]
        }
        if (only_context) out = unique(out, by=c('doc_id',subcontext))

        #if (length(sub_query) > 0) out = filter_sub_query(out, sub_query)
        if (nrow(out) == 0) return(NULL)
        if (has_sub_query) {
          if (is.null(sub_out)) return(NULL)
          out = data.table::fintersect(out, sub_out)
          if (is.null(out)) return(NULL)
        }
        return(out)
      },

      forget_memoise = function() forget_all_mem(),
      indices = function() data.table::indices(self$tokens),
      clear_indices = function() data.table::setindex(self$tokens, NULL)
   ),



   active = list(
     n = function() nrow(self$tokens),
     n_meta = function() nrow(self$meta),
     feature_names = function(e=NULL) {
       if (!is.null(e)) stop('Cannot change tcorpus$feature_names by assignment. Instead, use the set_name() function')
       fnames = colnames(self$tokens)[!colnames(self$tokens) %in% c('doc_id','sentence','token_id')]
     },
     names = function(e=NULL) {
       if (!is.null(e)) stop('Cannot change tcorpus$names by assignment. Instead, use the set_name() function')
       colnames(self$tokens)
     },
     meta_names = function(e=NULL) {
       if (!is.null(e)) stop('Cannot change tcorpus$meta_names by assignment. Instead, use the set_meta_name() function')
       colnames(self$meta)
     },

     doc_id_levels = function(mod=NULL) {
       if (!is.null(mod)){
         self$meta$doc_id = mod[match(levels(self$tokens$doc_id), self$meta$doc_id)]
         levels(self$tokens$doc_id) = mod
       }
       levels(self$get('doc_id'))
     }
   )
)

#' S3 print for tCorpus class
#'
#' @param x a tCorpus object
#' @param ... not used
#'
#' @method print tCorpus
#' @examples
#' tc = create_tcorpus(c('First text', 'Second text'))
#' print(tc)
#' @export
print.tCorpus <- function(x, ...) {
  sent_info = if ('sentence' %in% x$names) paste(' and sentences (n = ', nrow(unique(x$get(c('doc_id','sentence')))), ')', sep='') else ''
  cat('tCorpus containing ', x$n, ' tokens',
      '\ngrouped by documents (n = ', x$n_meta, ')', sent_info,
      '\ncontains:',
      '\n  - ', length(x$names), ' column', if (length(x$names) > 1) 's', ' in $tokens', ':\t', paste(x$names, collapse=', '),
      '\n  - ', length(x$meta_names), ' column', if (length(x$meta_names) > 1) 's', ' in $meta', ':  \t', paste(x$meta_names, collapse=', '),
      '\n', sep='')
}


#' Refresh a tCorpus object using the current version of corpustools
#'
#' As an R6 class, tCorpus contains its methods within the class object (i.e. itself). Therefore, if you use a new version of corpustools with an older tCorpus object (e.g., stored as a .rds. file), then the methods are not automatically updated. You can then use refresh_tcorpus() to reinitialize the tCorpus object with the current version of corpustools.
#'
#' @param tc a tCorpus object
#'
#' @return a tCorpus object
#' @examples
#' tc = create_tcorpus(c('First text', 'Second text'))
#' refresh_tcorpus(tc)
#' @export
refresh_tcorpus <- function(tc){
  tCorpus$new(tokens = data.table::copy(tc$get(keep_df = T)),
              meta = data.table::copy(tc$get_meta(keep_df = T)))
}

rebuild_tcorpus <- function(tc) {
  tokens_to_tcorpus(tokens = tc$get(),
                    doc_col = 'doc_id',
                    sentence_col = ifelse('sentence' %in% tc$names, T, F),
                    token_id_col = 'token_id',
                    meta = tc$get_meta())
}

#' Summary of a tCorpus object
#'
#' @param object A tCorpus object
#' @param ... not used
#'
#' @method summary tCorpus
#' @examples
#' tc = create_tcorpus(c('First text', 'Second text'))
#' summary(tc)
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
#' @examples
#' tc = create_tcorpus(c('First text', 'Second text'))
#' as.tcorpus(tc)
#' @export
as.tcorpus.tCorpus <- function(x, ...) x

#' Force an object to be a tCorpus class
#'
#' @param x the object to be forced
#' @param ... not used
#'
#' @examples
#' \dontrun{
#' x = c('First text','Second text')
#' as.tcorpus(x) ## x is not a tCorpus object
#' }
#' @export
as.tcorpus.default <- function(x, ...) stop('x has to be a tCorpus object')
## params: preprocess_params=list, filter_params,

is_tcorpus <- function(x, allow_stc=F){
  if (!class(x)[1] %in% c('tCorpus', 'shattered_tCorpus')) stop('not a tCorpus object')
  if (is_shattered(x) && !allow_stc) stop('function not implemented for shattered_tCorpus')
  TRUE
}

is_shattered <- function(x) methods::is(x, 'shattered_tCorpus')

###  utility

safe_selection <- function(d, selection){
  if (any(is.na(selection))) stop('selection cannot contain NA')
  if (!methods::is(selection, 'numeric') && !methods::is(selection,'logical')) stop('selection has to be either a logical vector or a numerical vector (indices for TRUE values)')
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
    if (!'sentence' %in% tc$names) stop('Sentence level not possible, since no sentence information is available. To enable sentence level analysis, use split_sentences = T in "create_tcorpus()" or specify sentence_col in "tokens_to_tcorpus()"')
    d = tc$get(c('doc_id','sentence'))
    if (with_labels){
      ucontext = unique(d[,c('doc_id','sentence')])
      ucontext = stringi::stri_paste(ucontext$doc_id, ucontext$sentence, sep=' #')
      context = fast_factor(global_position(d$sentence, d$doc_id, presorted = T, position_is_local=T))
      levels(context) = ucontext
    } else {
      fast_factor(global_position(d$sentence, d$doc_id, presorted = T, position_is_local=T))
      context = fast_dummy_factor(global_position(d$sentence, d$doc_id, presorted = T, position_is_local=T))
    }
  }
  context
}

### memoised regex search

lookup_terms <- function(patterns, x, ignore_case=T, raw_regex=T, perl=F, batchsize=25, useBytes=T, as_ascii=FALSE){
  #forget_if_new(x) ## hacky use of memoise. If input is not in cache because the feature column changed, reset cache
  x = mem_lookup_table(x, ignore_case, as_ascii)

  if (as_ascii) {
    patterns = stringi::stri_trans_general(patterns, "any-latin")
    patterns = stringi::stri_trans_general(patterns, "latin-ascii")
  }
  needs_expansion = grepl('?', patterns, fixed=T) | grepl('*', patterns, fixed=T)
  fixed_patterns = patterns[!needs_expansion]
  regex_patterns = patterns[needs_expansion]

  if (length(fixed_patterns) > 0) {
    fixed_patterns = search_term_fixed(fixed_patterns, ignore_case, as_ascii=F)
    fixed_patterns = x[fixed_patterns, on = 'lookup_term', nomatch=0]
    fixed_patterns = if (nrow(fixed_patterns) > 0) fixed_patterns$term else c()
  }
  if (length(regex_patterns) == 0) return(fixed_patterns)

  ## if there are also regex_patterns
  if (!raw_regex) regex_patterns = search_term_regex(regex_patterns)
  if (length(regex_patterns) > 1) { ## if there are multiple terms, make batches of terms and turn each batch into a single regex
    regex_patterns = split(regex_patterns, ceiling(seq_along(regex_patterns)/batchsize))
    regex_patterns = sapply(regex_patterns, stringi::stri_paste, collapse='|')
    out = rep(F, nrow(x))
    for(exp_pattern_batch in regex_patterns){
      out = out | grepl(exp_pattern_batch, x$lookup_term, ignore.case=ignore_case, perl=perl, useBytes=useBytes)
    }
  } else {
    out = grepl(regex_patterns, x$term, ignore.case=ignore_case, perl=perl, useBytes=useBytes)
  }
  regex_patterns = x$term[out]

  return(union(regex_patterns, fixed_patterns))
}

search_term_fixed <- function(patterns, ignore_case, as_ascii) {
  if (ignore_case) patterns = stringi::stri_trans_tolower(patterns)
  if (as_ascii) {
    patterns = stringi::stri_trans_general(patterns, "any-latin")
    patterns = stringi::stri_trans_general(patterns, "latin-ascii")
  }
  patterns
}
search_term_regex <- function(patterns) {
  patterns = gsub("([^0-9a-zA-Z])", '\\\\\\1', x=patterns)  # escape special characters
  patterns = gsub('\\\\(\\*)|\\\\(\\?)', '.\\1', patterns)  # process wildcards
  paste0('\\b',patterns,'\\b')                              # set word boundaries
}


mem_lookup_terms <- memoise::memoise(lookup_terms)
mem_lookup_table <- memoise::memoise(function(x, ignore_case, as_ascii) {
  lookup_term = search_term_fixed(x, ignore_case, as_ascii)
  lookup_table = data.table(term = x, lookup_term = lookup_term)

  .SPLIT = stringi::stri_split(lookup_table$lookup_term, regex = '\\_| ')
  len = sapply(.SPLIT, length)
  if (max(len) > 1) {
    lookup_table = lookup_table[rep(1:nrow(lookup_table), len),]
    lookup_table[,lookup_term := unlist(.SPLIT)]
  }
  setkey(lookup_table, 'lookup_term')
  lookup_table
})

forget_if_new <- memoise::memoise(function(x){  ## the forget calls will only be performed if x changes
  forget_all_mem()                              ## otherwise the invisible NULL is simply returned from cache
  invisible(NULL)
})

forget_all_mem <- function(){
  memoise::forget(mem_lookup_terms)
  memoise::forget(mem_lookup_table)
  memoise::forget(forget_if_new)
  invisible(NULL)
}
