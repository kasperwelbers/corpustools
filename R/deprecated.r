is_deprecated <- function(f = as.character(sys.call(sys.parent()))[1L], new=NULL, warn=T){
  ## wrapper for .Deprecated
  f = gsub('.*\\$', '', f)
  if (is.null(new)) new = f
  msg <- gettextf("'%s' as an R6 method is deprecated.\nIt used to be:\t\ttCorpus$%s(...)\nnow use instead:\t%s(tc, ...)\nSee help('%s')", f,f,new,new)
  #warning(msg, call. = FALSE, domain = NA)
  if (warn) warning(warningCondition(msg, class = "deprecatedWarning"))
}





