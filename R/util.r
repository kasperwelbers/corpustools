verbose_counter <- function(n, i=0, ticks=10){
  function() {
    i <<- i + 1
    if(i %% ticks == 0) cat(i*batchsize, ' / ', n*batchsize, '\n')
  }
}

verbose_sum_counter <- function(n, i=0){
  function(add) {
    i <<- i + add
    cat(i, ' / ', n, '\n')
  }
}

