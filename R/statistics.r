#' Compute the chi^2 statistic for a 2x2 crosstab containing the values
#' [a, b]
#' [c, d]
calc_chi2 <- function(a,b,c,d, correct=T, cochrans_criteria=F){
  n = a+b+c+d

  sums = cbind(c1 = a+c, c2 = b+d, r1 = a+b, r2 = c+d)
  yates_correction = if(correct) rep(T, nrow(sums)) else rep(F, nrow(sums))

  if(cochrans_criteria){
    ## apply Cochrans criteria: no expected values below 1 and less than 20% of cells empty (which means none in a 2x2 design)
    ## only use the yates_correction if these criteria are violated
    ## http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2041889/ (similar use)
    e = cbind(sums[,'c1'] / n, sums[,'c2'] / n)
    e = cbind(e * sums[,'r1'], e * sums[,'r2'])
    c1 = rowSums(e < 1) > 0          # at least one expected value below 1
    c2 = rowSums(sums < 5) > 0       # at least one cell with value below 5
    yates_correction = ifelse(c1 | c2, T, F)
  }
  x = a*d - b*c
  x = ifelse(yates_correction, abs(x) - n/2, x)
  chi = n*x^2 / (as.numeric(sums[,'c1']) * as.numeric(sums[,'c2']) * as.numeric(sums[,'r1']) * as.numeric(sums[,'r2'])) ## as.numeric to prevent integer overflow
  ifelse(is.na(chi), 0, chi)
}

