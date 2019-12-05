testthat::context('Dictionary lookup')

test_that("Testing: Dictionary lookup", {
  dict = data.frame(string = c('this is', 'a', 'test'))
  tc = create_tcorpus(c('this is a test','This town is not big enough for a test'))

  tc$code_dictionary(dict)
  expect_equal(tc$tokens$code_id, c(1,1,2,3,NA,NA,NA,NA,NA,NA,NA,4,5))

  dict = data.frame(string = c('this is', 'a', 'test'), code=c('a','b','c'))
  hits = search_dictionary(tc, dict)
  expect_equal(hits$hits$token_id, c(1,2,3,4,8,9))


  tc = create_tcorpus('yay :) :* happy')
  ed = emoticon_dict[c(5,9),]
  tc$replace_dictionary(ed)
  tc$tokens
  expect_equal(as.character(tc$tokens$token), c('yay',':)',':*','happy'))
})


