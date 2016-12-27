## This is a dummy function to let roxygen2 @import the tm, igraph and Matrix package
## This way, roxygen2 will add them to NAMESPACE when building the package via devtools

#' @import tm
#' @import Matrix
#' @import igraph
#' @import data.table
importglobals <- function(x) NULL
