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

  ## using quanteda dictionary
  tc$code_dictionary(quanteda::data_dictionary_LSD2015)
  expect_equal(as.numeric(table(tc$tokens$code)), c(2,2))
    
  ## multitoken matching with weird spacing issues
  dict = data.frame(string = c('good','bad','ugl*','nice','not pret*', ':)', ': ('), sentiment=c(1,-1,-1,1,-1,1,-1))
  tc = udpipe_tcorpus(c('The good, the_bad and the ugly, is nice : ) but not pretty :('))
  tc$code_dictionary(dict)
  expect_equal(tc$tokens$sentiment, c(NA,1,NA,-1,NA,NA,-1,NA,NA,1,1,1,NA,-1,-1,-1))
  tc$tokens
  
})


