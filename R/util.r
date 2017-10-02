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
      x = .Call('_corpustools_fast_factor', PACKAGE = 'corpustools', as.character(x), as.character(levels))
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

