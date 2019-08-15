testthat::context('Dictionary lookup')

test_that("Testing: Dictionary lookup", {
  dict = data.frame(string = c('this is', 'a', 'test'), id=1:3, var1=c('a','b','c'))
  tc = create_tcorpus(c('this is a test','This town is not big enough for a test'))

  tc$code_dictionary(dict)
  expect_equal(tc$tokens$code, c(1,1,2,3,NA,NA,NA,NA,NA,NA,NA,2,3))

  hits = search_dictionary(tc, dict)
  expect_equal(hits$hits$token_id, c(1,2,3,4,8,9))
})


