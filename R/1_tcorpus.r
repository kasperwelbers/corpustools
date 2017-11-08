## Here the main tCorpus class is created
## Additional methods are specified in the matching r files (e.g. dtm method in document_term_matrix.r)
## The additional methods should only use the public functions of the tCorpus class (so avoid private$)
## This way, we can more easily change or add backends in the future.

## note that this file has to be named 1_*, because r files are processed in order by name,
## and the tCorpus class has to be created before the additional methods


tCorpus <- R6::R6Class("tCorpus",
   cloneable=FALSE,

   private = list(
     .data = NULL,
     .meta = NULL,
     .p = list(),

     sync = function(){
       doc_ids = intersect(unique(private$.data$doc_id), unique(private$.meta$doc_id))
       private$.data = private$.data[list(doc_ids),]
       private$.meta = private$.meta[list(doc_ids),]
       private$set_keys()
     },

     check_unique_rows = function(d){
       if (anyDuplicated(d, by = c('doc_id','token_i'))) stop('After transformation, token_i is not unique within documents')
     },

     set_keys = function(){
       if (!identical(key(private$.data), c('doc_id', 'token_i'))) setkey(private$.data, 'doc_id', 'token_i')
       if (!identical(key(private$.meta), c('doc_id'))) setkey(private$.meta, 'doc_id')
     },

     droplevels = function(){
       private$.data = base::droplevels(private$.data)
       private$.meta = base::droplevels(private$.meta)
       invisible(self)
     }

   ),

   public = list(
     help = function() ?tCorpus,

     initialize = function(data, meta) {
       private$.data = data.table(data)
       private$.meta = data.table(meta)
       private$set_keys()
     },

     finalize = function() {
       forget_all_mem()
     },

     copy = function(){
       tCorpus$new(data = data.table::copy(private$.data),
                   meta = data.table::copy(private$.meta))
     },

## SHOW/GET DATA METHODS ##
    get = function(columns=NULL, keep_df=F, as.df=F, subset=NULL, doc_id=NULL, token_i=NULL, safe_copy=T) {
      if (class(substitute(subset)) %in% c('call', 'name')) subset = self$eval(substitute(subset), parent.frame())
      if (!is.null(doc_id) & !is.null(subset)) stop('Cannot filter by subset and doc_ids at the same time')
      if (is.null(doc_id) & !is.null(token_i)) stop('token_i can only be given in pairs with doc_id')

      if (is.null(doc_id) & is.null(subset)) {
        if (is.null(columns)) {
          d = private$.data
        } else {
          d = if (length(columns) > 1 | keep_df) private$.data[,columns,with=F] else private$.data[[columns]]
        }
      } else {
        if (is.null(columns)) columns = colnames(private$.data)
        if (!is.null(subset)) d = if (length(columns) > 1 | keep_df) private$.data[subset,columns,with=F] else private$.data[[columns]][subset]
        if (!is.null(doc_id)) {
          if (is.null(token_i)) {
            positions = list(doc_ids = as.character(doc_id))
            d = if (length(columns) > 1 | keep_df) private$.data[positions,columns,with=F] else private$.data[positions, columns, with=F][[columns]]
          } else {
            if (!length(doc_id) == length(token_i)) stop('token_i can only be given in pairs with doc_id')
            positions = list(doc_ids = as.character(doc_id), token_is = as.numeric(token_i))
            d = if (length(columns) > 1 | keep_df) private$.data[positions,columns,with=F] else private$.data[positions, columns, with=F][[columns]]
          }
        }
      }

      if (as.df) d = as.data.frame(d)
      if(safe_copy) data.table::copy(d) else d
    },

    get_levels = function(column){
      if (!methods::is(private$.data[[column]], 'factor')) stop(sprintf('"%s" is not a factor', column))
      data.table::copy(levels(private$.data[[column]]))
    },

    get_meta = function(columns=NULL, keep_df=F, as.df=F, per_token=F, subset=NULL, doc_id=NULL, safe_copy=T) {
      if (class(substitute(subset)) %in% c('call', 'name')) subset = self$eval_meta(substitute(subset), parent.frame())
      if (!is.null(doc_id) & !is.null(subset)) stop('Cannot filter by subset and doc_ids at the same time')


      if (is.null(doc_id) & is.null(subset)) {
        if (is.null(columns)) {
          d = private$.meta
        } else {
          d = if (length(columns) > 1 | keep_df) private$.meta[,columns,with=F] else private$.meta[[columns]]
        }
      } else {
        if (is.null(columns)) columns = colnames(private$.meta)
        if (!is.null(doc_id)) {
          doc_ids = as.character(doc_id)
          d = if (length(columns) > 1 | keep_df) private$.meta[list(doc_ids),columns,with=F] else private$.meta[list(doc_ids), columns, with=F][[columns]]
        }
        if (!is.null(subset)) d = if (length(columns) > 1 | keep_df) private$.meta[subset,columns,with=F] else private$.meta[[columns]][subset]
      }

      if (as.df) d = as.data.frame(d)
      if (per_token) {
        exp_i = match(private$.data$doc_id, private$.meta$doc_id)
        d = if (methods::is(d, 'data.frame')) d[exp_i,,drop=!keep_df & !is.null(columns)] else d[exp_i]
      }
      if(safe_copy) data.table::copy(d) else d
    },

    get_meta_levels = function(column){
      if (!methods::is(private$.meta[[column]], 'factor')) stop(sprintf('"%s" is not a factor', column))
      data.table::copy(levels(private$.meta[[column]]))
    },

     context = function(context_level = c('document','sentence'), with_labels=T){
       get_context(self, context_level = context_level, with_labels=with_labels)
     },

     eval = function(x, enclos=parent.frame()) eval(x, private$.data, enclos),
     eval_meta = function(x, enclos=parent.frame()) eval(x, private$.meta, enclos),

     token_i = function(doc_id=NULL, token_i=NULL, subset=NULL, subset_meta=NULL, window=NULL, inverse=F){
      if (class(substitute(subset)) %in% c('call', 'name')) subset = self$eval(substitute(subset), parent.frame())
      if (class(substitute(subset_meta)) %in% c('call', 'name')) subset_meta = self$eval_meta(substitute(subset_meta), parent.frame())
      if (is.null(doc_id) & !is.null(token_i)) stop('token_i can only be given in pairs with doc_id')

      ## enable subset to be called from a character string. (e.g. used in search_features)
      if(methods::is(subset, 'character')) subset_meta = eval(parse(text=subset_meta), private$.meta, parent.frame())
      if(methods::is(subset_meta, 'character')) subset_meta = eval(parse(text=subset_meta), private$.meta, parent.frame())

      i = NULL
      if (!is.null(doc_id)) {
        pos = if (is.null(token_i)) list(doc_id) else list(doc_id, token_i)
        i = private$.data[pos, which=T]
      }
      if (!is.null(subset)) {
        subset[is.na(subset)] = F
        i = if (is.null(i)) which(subset) else intersect(i, which(subset))
      }
      if (!is.null(subset_meta)) {
        subset_meta[is.na(subset_meta)] = F
        doc_ids = self$get_meta('doc_id')[subset_meta]
        meta_i = private$.data[list(doc_ids), which=T]
        i = if (is.null(i)) meta_i else intersect(i, meta_i)
      }

      if (!is.null(window)) i = which(i_window(self, i=i, window=window))
      if (inverse) i = !1:self$n %in% i
      i
    },


## DATA MODIFICATION METHODS ##
     set = function(column, value, subset=NULL, subset_value=T){
       if (column == 'doc_id') stop('Cannot change doc_id. If you want to change doc_id labels, you can overwrite $doc_id_levels.')
       if (class(substitute(subset)) %in% c('call', 'name')) subset = self$eval(substitute(subset), parent.frame())
       if (class(substitute(value)) %in% c('call', 'name')) value = self$eval(substitute(value), parent.frame())
       if (grepl('^\\.', column)) stop('column names in a tCorpus cannot start with a dot')

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

         if (column %in% c('sent_i','token_i')) {  ## for position columns, first perform checks (inefficient, but this should be a rare case anyway)
            if (!methods::is(value, 'numeric')) stop('position column has to be numeric/integer')
            value = as.integer(value)
            mod = if ('sent_i' %in% self$names) private$.data[,c('doc_id','sent_i','token_i')] else private$.data[,c('doc_id','token_i')]
            mod[subset, (column) := value]
            check_unique_rows(mod)
         }

         .value = value
         private$.data[subset, (column) := .value]

       } else {
         if (column %in% c('sent_i','token_i')) {
           if (!methods::is(value, 'numeric')) stop('position column has to be numeric/integer')
           value = as.integer(value)
           mod = if ('sent_i' %in% self$names) private$.data[,c('doc_id','sent_i','token_i')] else private$.data[,c('doc_id','token_i')]
           suppressWarnings(mod[, (column) := value])
           check_unique_rows(mod)
         }
         if (is.character(value)) value = fast_factor(value)  ## force the use of factors for character vectors.
         ## ugly suppress. Should look into why data.table give the (seemingly harmless) internal.selfref warning
         suppressWarnings(private$.data[,(column) := value])
       }

       ## strangely, the assign by reference in data.table sometimes adds NA as a factor level...
       if (anyNA(levels(private$.data[[column]]))) {
         private$.data[,(column) := fast_factor(private$.data[[column]])]
       }

       private$set_keys()
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

     set_name = function(oldname, newname) {
       if (oldname %in% c('doc_id','sent_i','token_i')) stop('The position columns (doc_id, sent_i, token_i) cannot be set or changed')
       if (grepl('^\\.', newname)) stop('column names in a tCorpus cannot start with a dot')

       data.table::setnames(private$.data, oldname, newname)
       invisible(self)
     },

     set_meta = function(column, value, subset=NULL, subset_value=T){
       if (column == 'doc_id') stop('Cannot change doc_id. If you want to change doc_id labels, you can overwrite $doc_id_levels.')
       if (class(substitute(subset)) %in% c('call', 'name')) subset = self$eval_meta(substitute(subset), parent.frame())
       if (class(substitute(value)) %in% c('call', 'name')) value = self$eval_meta(substitute(value), parent.frame())

       if (grepl('^\\.', column)) stop('column names in a tCorpus cannot start with a dot')

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

         .value = value
         private$.meta[subset, (column) := .value]

       } else {
         ## ugly suppress. Should look into why data.table give the (seemingly harmless) internal.selfref warning
         suppressWarnings(private$.meta[,(column) := value])
       }

       ## strangely, the assign by reference in data.table sometimes adds NA as a factor level...
       if (anyNA(levels(private$.meta[[column]]))) {
         private$.meta[,(column) := fast_factor(private$.meta[[column]])]
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

     set_meta_name = function(oldname, newname) {
       if (oldname %in% c('doc_id')) stop('The doc_id column cannot be set or changed')
       if (grepl('^\\.', newname)) stop('column names in a tCorpus cannot start with a dot')
       setnames(private$.meta, oldname, newname)
       invisible(self)
     },

      select_rows = function(selection, keep_meta=F) {
        selection = safe_selection(private$.data, selection)
        private$.data = subset(private$.data, selection)
        if (!keep_meta) {
          private$.meta = private$.meta[as.character(unique(private$.data$doc_id)),,nomatch=0]
          private$.meta$doc_id = as.character(private$.meta$doc_id)
        }
        private$set_keys()
      },

      select_meta_rows = function(selection, keep_data=F) {
        selection = safe_selection(private$.meta, selection)
        private$.meta = subset(private$.meta, selection)
        private$.meta$doc_id = as.character(private$.meta$doc_id)
        if (!keep_data) private$.data = private$.data[as.character(unique(private$.meta$doc_id)),,nomatch=0]
        private$set_keys()
      },

     subset = function(subset=NULL, subset_meta=NULL, hits=NULL, window=NULL, copy=F, ...){
       if (class(substitute(subset)) %in% c('call', 'name')) subset = self$eval(substitute(subset), parent.frame())
       if (class(substitute(subset_meta)) %in% c('call', 'name')) subset_meta = self$eval_meta(substitute(subset_meta), parent.frame())
       if (copy) {
         selfcopy = self$copy()$subset(subset=subset, subset_meta=subset_meta, window=window, copy=F)
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
         self$select_meta_rows(subset_meta, keep_data = T) ## if both subsets are used, first perform both without subseting the other, then match on doc_ids.
         self$select_rows(subset, keep_meta = T)           ## (we cannot subset one before the other, because subset call's can contains vectors for which the rows should match)
         private$sync()
       }
       if (!is.null(subset_meta) & is.null(subset)) {
         self$select_meta_rows(subset_meta, keep_data = F)
       }
       if (is.null(subset_meta) & !is.null(subset)) {
         self$select_rows(subset, keep_meta = F)
       }

       private$droplevels()
       invisible(self)
     },

      subset_meta = function(subset=NULL, copy=F){
        ## subset also has a subset_meta argument, but we add this for consistency with other _meta methods
        if (class(substitute(subset)) %in% c('call', 'name')) subset = self$eval_meta(substitute(subset), parent.frame())
        .subset = subset
        self$subset(subset_meta = .subset, copy=copy)
      },

     aggregate = function(meta_cols=NULL, hits=NULL, feature=NULL, count=c('documents','tokens', 'hits'), wide=T){
        count = match.arg(count)

        meta = data.table::copy(private$.meta)
        if (is.null(meta_cols)) {
          meta[,group := 'total']
          meta_cols = 'group'
        }

        len = private$.data[, list(len=.N), by='doc_id']
        meta = merge(meta, len, by='doc_id', all.x=T)
        d = meta[, list(N=.N, V=sum(len)), by=meta_cols]

        if (!is.null(hits) | !is.null(feature)){
          if (!is.null(hits) & !is.null(feature)) stop('Cannot specify both hits and feature')
          if (!is.null(hits)) {
            if (!methods::is(hits, c('featureHits', 'contextHits'))) stop('hits must be a featureHits or contextHits object (see the $search_features and $search_contexts methods)')
            if (methods::is(hits, 'featureHits')) {
              coldata = hits$hits[!duplicated(hits$hits[,c('code', 'hit_id')]),]
            } else {
              coldata = hits$hits
            }
          }

          if (!is.null(feature)) {
            coldata = data.frame(doc_id = private$.data$doc_id, code = private$.data[[feature]])
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
            out = unique(private$.data, by = c('doc_id', subcontext))
          } else {
            out = data.table::copy(private$.data)
          }
          if (with_i & !only_context) out[, i:=1:nrow(out)]
          if (has_sub_query) {
            if (is.null(sub_out)) return(NULL)
            out = data.table::fintersect(out, sub_out)
            if (is.null(out)) return(NULL)
          }
          return(out)
        }

        ## prepare lookup table: set a (secondary) data.table index
        if (!feature %in% indices(private$.data)) {
          data.table::setindexv(private$.data, feature)
          message(sprintf('created index for "%s" column', feature))
        }

        ## if not fixed (exact value matching), first lookup x as regex in unique values
        ## note that mem_lookup_terms is memoised
        if (!fixed) {
          uval = if (is.factor(private$.data[[feature]])) levels(private$.data[[feature]]) else unique(private$.data[[feature]])
          x = mem_lookup_terms(x, uval, ignore_case=ignore_case, raw_regex=raw_regex, batchsize=batchsize, useBytes=T, as_ascii=as_ascii)
        }

        if (length(x) == 0) return(NULL)
        if (with_i & !only_context) {
          i = na.omit(private$.data[list(x), on=feature, which=T])
          out = private$.data[i,]
          out[,i:=i]
        } else {
          out = private$.data[list(x), on=feature, which=F]
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
      indices = function() data.table::indices(private$.data),
      clear_indices = function() data.table::setindex(private$.data, NULL)
   ),

   active = list(
     n = function() nrow(private$.data),
     n_meta = function() nrow(private$.meta),
     feature_names = function(e=NULL) {
       if (!is.null(e)) stop('Cannot change tcorpus$feature_names by assignment. Instead, use the set_name() function')
       fnames = colnames(private$.data)[!colnames(private$.data) %in% c('doc_id','sent_i','token_i')]
     },
     names = function(e=NULL) {
       if (!is.null(e)) stop('Cannot change tcorpus$names by assignment. Instead, use the set_name() function')
       colnames(private$.data)
     },
     meta_names = function(e=NULL) {
       if (!is.null(e)) stop('Cannot change tcorpus$meta_names by assignment. Instead, use the set_meta_name() function')
       colnames(private$.meta)
     },


     doc_id_levels = function(mod=NULL) {
       if (!is.null(mod)){
         private$.meta$doc_id = mod[match(levels(private$.data$doc_id), private$.meta$doc_id)]
         levels(private$.data$doc_id) = mod
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
  sent_info = if ('sent_i' %in% x$names) paste(' and sentences (n = ', nrow(unique(x$get(c('doc_id','sent_i')))), ')', sep='') else ''
  cat('tCorpus containing ', x$n, ' tokens',
      '\ngrouped by documents (n = ', x$n_meta, ')', sent_info,
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
#' @examples
#' tc = create_tcorpus(c('First text', 'Second text'))
#' refresh_tcorpus(tc)
#' @export
refresh_tcorpus <- function(tc){
  tCorpus$new(data=tc$get(),
              meta=tc$get_meta())
}

rebuild_tcorpus <- function(tc) {
  tokens_to_tcorpus(tokens = tc$get(),
                    doc_col = 'doc_id',
                    sent_i_col = ifelse('sent_i' %in% tc$names, T, F),
                    token_i_col = 'token_i',
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
    d = tc$get(c('doc_id','sent_i'))
    if (with_labels){
      ucontext = unique(d[,c('doc_id','sent_i')])
      ucontext = stringi::stri_paste(ucontext$doc_id, ucontext$sent_i, sep=' #')
      context = fast_factor(global_position(d$sent_i, d$doc_id, presorted = T, position_is_local=T))
      levels(context) = ucontext
    } else {
      fast_factor(global_position(d$sent_i, d$doc_id, presorted = T, position_is_local=T))
      context = fast_dummy_factor(global_position(d$sent_i, d$doc_id, presorted = T, position_is_local=T))
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
  data.table(term = x, lookup_term = lookup_term, key = 'lookup_term')
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
