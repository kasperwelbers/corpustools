#' Vectorized computation of chi^2 statistic for a 2x2 crosstab containing the values
#' [a, b]
#' [c, d]
#'
#' @param a topleft value of the table
#' @param b topright value
#' @param c bottomleft value
#' @param d bottomright value
#' @param correct if TRUE, use yates correction. Can be a vector of length a (i.e. the number of tables)
#' @param cochrans_criteria if TRUE, check if cochrans_criteria indicate that a correction should be used. This overrides the correct parameter
calc_chi2 <- function(a,b,c,d, correct=T, cochrans_criteria=F){
  n = a+b+c+d
  sums = cbind(c1 = a+c, c2 = b+d, r1 = a+b, r2 = c+d)
  yates_correction = if (correct) rep(T, nrow(sums)) else rep(F, nrow(sums))

  ## apply Cochrans criteria: no expected values below 1 and less than 20% of cells empty (which means none in a 2x2 design)
  ## only use the yates_correction if these criteria are violated
  ## http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2041889/ (similar use)
  if (cochrans_criteria) yates_correction = test_cochran(a,b,c,d)

  x = as.numeric(a)*as.numeric(d) - as.numeric(b)*as.numeric(c) ## as.numeric to prevent integer overflow
  x = ifelse(yates_correction, abs(x) - n/2, x)
  chi = n*x^2 / (as.numeric(sums[,'c1']) * as.numeric(sums[,'c2']) * as.numeric(sums[,'r1']) * as.numeric(sums[,'r2']))
  ifelse(is.na(chi), 0, chi)
}

test_cochran <- function(a,b,c,d){
  n = a+b+c+d
  sums = cbind(c1 = a+c, c2 = b+d, r1 = a+b, r2 = c+d)
  e = cbind(sums[,'c1'] / n, sums[,'c2'] / n)
  e = cbind(e * sums[,'r1'], e * sums[,'r2'])
  c1 = rowSums(e < 1) > 0          # at least one expected value below 1
  c2 = rowSums(sums < 5) > 0       # at least one cell with value below 5
  c1 | c2
}

