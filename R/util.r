#' Check if package with given version exists
#'
#' @param package The name of the package
#' @param min_version The minimum version
#'
#' @return An error if package does not exist
require_package <- function(package, min_version = NULL) {
  version_string = if (!is.null(min_version)) sprintf(' (>= %s)', min_version) else ''
  e = sprintf('%s package%s needs to be installed to use this function', package, version_string)
  if(!requireNamespace(package, quietly = T)) stop(e)
  if (!is.null(min_version)) {
    version_comp = utils::compareVersion(as.character(utils::packageVersion(package)), min_version)
    if (version_comp < 0) stop(e)
  }
}

local_id <- function(group, i) {
  ## given global indices per group, make them locally unique
  ## has to be sorted on order(group, i)
  newgroup = which(!duplicated(group))
  repeat_add = c(newgroup[-1], length(group)+1) - newgroup
  group_start = rep(i[newgroup], repeat_add)
  (i - group_start) + 1
}

global_id <- function(group, i, window=NA) {
  ## given local indices per group, make them globally unique
  ## has to be sorted on order(group, i)
  if (!length(unique(group)) == 1) {
    newgroup = which(!duplicated(group)) # where does a new group start

    group.max = i[newgroup-1] # the highest value of each group
    if (!is.na(window)) group.max = group.max + window # increase the highest value of each group with max_window_size to make sure windows of different groups do not overlap.
    add_scores = cumsum(c(0,group.max)) # the amount that should be added to the i at the start of each group

    repeat_add = c(newgroup[-1], length(i)+1) - newgroup # the number of times the add scores need to be repeated to match the i vector
    i + rep(add_scores, repeat_add)
  } else {
    i
  }
}

verbose_counter <- function(n, i=0, ticks=10){
  function() {
    i <<- i + 1
    if (i %% ticks == 0) message(cat(i, ' / ', n, '\n'))
  }
}

verbose_sum_counter <- function(n, i=0){
  function(add) {
    i <<- i + add
    message(cat(i, ' / ', n, '\n'))
  }
}

pretty_text_paste <- function(x){
  x = gsub('_| ', ' ', x)
  x = gsub(" ([.,?!:;>)])", '\\1', x)
  x = gsub('([(<]) ', '\\1', x)
  x
}

fast_dummy_factor <- function(x) { ## if , still return a factor for consistency, but no need to match stuff
  x = as.integer(x)
  nlevels = length(stats::na.omit(unique(x)))
  attr(x, 'levels') = if (nlevels > 0) as.character(1:nlevels) else character()
  class(x) <- 'factor'
  x
}

fast_factor <- function(x, levels=NULL) {
  if (!methods::is(x, 'factor')) {
    if (!all(is.na(x))) {
      if (is.null(levels)) levels = vector('character', 0)
      x = fast_factor_cpp(as.character(x), as.character(levels))
    } else {
      x = fast_dummy_factor(x)
    }
  } else {
    if (length(levels) > 0) levels(x) = levels
  }
  x
}

col_to_hsv <- function(col, alpha=1) {
  ## make mapped to enable vectorization
  hsv_col = grDevices::rgb2hsv(grDevices::col2rgb('red'))
  grDevices::hsv(hsv_col[1], hsv_col[2], hsv_col[3], alpha=alpha)
}

double_to_single_slash <- function(x) {
  x = gsub('\\\\n','\n', x)
  x = gsub('\\\\t','\t', x)
  x = gsub('\\\\r','\r', x)
  x
}

