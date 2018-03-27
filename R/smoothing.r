#' Simple Good Turing smoothing
#'
#' Implementation of the Simple Good Turing smoothing proposed in: Gale, W. A., \& Sampson, G. (1995). Good turing frequency estimation without tears. Journal of Quantitative Linguistics, 2(3), 217-237.
#'
#' @param freq A numeric vector of term frequencies (integers).
#'
#' @return A numeric vector with the smoothed term proportions
sgt <- function(freq){
  ff = data.frame(table(freq))
  ff$freq = as.numeric(as.character(ff$freq))
  ff = ff[ff$freq >= 1,]
  if (length(unique(ff$freq)) == 1) {
    warning('Smoothing is disabled, because all term frequencies (other than 0) are identical.')
    freq
  } else {
    sgt_v = sgt_values(ff$freq, ff$Freq)
    v = data.frame(r = c(0, sgt_v$r),
                   p = c(sgt_v$P0 / ff$Freq[1], sgt_v$p))
    ## check  "estimating the number of unseen species"
    ## use separate "ratio" and "p.ratio" in comp results
    v$p[match(freq, v$r)]
  }
}



can_smooth <- function(freq){
  ff = table(freq)
  ff = ff[!names(ff) == '0']
  if (length(unique(ff)) > 1) T else F
}

#' Laplace (i.e. add constant) smoothing
#'
#' @param freq A numeric vector of term frequencies (integers).
#' @param add The added value
#'
#' @return A numeric vector with the smoothed term proportions
#' @examples
#' laplace(c(0,0,1,1,1,2,2,2,3,3,4,7,10))
#' @export
laplace <- function(freq, add=0.5){
  (freq + add) / sum(freq + add)
}

sgt_values <- function(r, Nr, conf=1.96) {
  N = sum(r * Nr)

  # averaging transforms
  Nrz = nrzest(r, Nr)

  # linear good-turing estimate
  f = stats::lm(log(Nrz) ~ log(r))
  coef = f$coefficients
  if (is.na(coef[2])) {
    warning('Could not properly calculate linear Good-Turing estimate.')
    coef[2] = 1
  }
  rst = rstest(r, coef)
  rstrel = rst / r

  ## turing estimates
  rtry = r == c(r[-1]-1, 0)
  rstarel = rep(0, length(r))
  rstarel[rtry] = (r[rtry]+1) / r[rtry] * c(Nr[-1], 0)[rtry] / Nr[rtry]

  ## switch from turing to LGT
  ## (use Turing estimates if significantly different from LGT estimates, and once this is not longer the case, use LGT estimates for every next r)
  ursd = rep(1, length(r))
  i = which(rtry)
  ursd[i] = (i+1) / Nr[i] * sqrt(Nr[i+1] * 1 + Nr[i+1] / Nr[i])

  switch = abs(rstrel - rstarel) * 1:length(r) / ursd <= conf
  if (any(switch) && !all(switch)) {
    switch[which(switch)[1]:length(switch)] = T
  }

  rstar = ifelse(switch, rstrel, rstarel) * r
  bigNprime = sum(Nr * rstar)
  pzero = Nr[1] / N
  p = (1 - pzero) * (rstar / bigNprime)

  list(P0 = pzero,
       r = r,
       p = as.numeric(p))
}

nrzest <- function(r, Nr) {
  d <- c(1, diff(r))
  dr <- c(0.5 * (d[-1] + d[ - length(d)]), d[length(d)])
  Nr/dr
}

rstest <- function(r, coef) {
  r * (1 + 1/r)^(1 + coef[2])
}


