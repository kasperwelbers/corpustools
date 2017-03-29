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

fast_dummy_factor <- function(x) { ## if , still return a factor for consistency, but no need to match stuff
  x = as.integer(x)
  nlevels = length(na.omit(unique(x)))
  attr(x, 'levels') = if (nlevels > 0) as.character(1:nlevels) else character()
  class(x) <- 'factor'
  x
}

fast_factor <- function(x, levels=NULL) {
  if (!is(x, 'factor')) {
    if (!all(is.na(x))) {
      if (is.null(levels)) levels = vector(class(x), 0)
      x = .Call('corpustools_fast_factor', PACKAGE = 'corpustools', x, levels)
    } else {
      x = fast_dummy_factor(x)
    }
  } else {
    if (length(levels) > 0) levels(x) = levels
  }
  x
}
