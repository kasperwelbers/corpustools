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
