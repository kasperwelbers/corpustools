testthat::context('Dictionary lookup')

test_that("Testing: Dictionary lookup", {
  dict = data.frame(string = c('this is', 'a', 'test'))
  tc = create_tcorpus(c('this is a test','This town is not big enough for a test'))

  tc$code_dictionary(dict)
  expect_equal(tc$tokens$code_id, c(1,1,2,3,NA,NA,NA,NA,NA,NA,NA,4,5))

  dict = data.frame(string = c('this is', 'a', 'test'), code=c('a','b','c'))
  hits = search_dictionary(tc, dict)
  #hits = search_features(tc, c('this is', 'a', 'test'))
  hits$hits
  expect_equal(hits$hits$token_id, c(1,2,3,4,8,9))

  tc = create_tcorpus(c('yay :) :* happy.', 'boo sad :('))
  tc$replace_dictionary(emoticon_dict)
  tc$tokens
  expect_equal(as.character(tc$tokens$token), c('yay',':)',':*','happy','.','boo','sad',':('))

  ## using quanteda dictionary
  tc$code_dictionary(quanteda::data_dictionary_LSD2015)
  expect_equal(as.numeric(table(tc$tokens$code)), c(2,1))
    
  ## multitoken matching with weird spacing issues
  dict = data.frame(string = c('good','bad','ugl*','nice','not pret*', ':)', ': ('), sentiment=c(1,-1,-1,1,-1,1,-1))
  tc = create_tcorpus(c('The good, the bad and the ugly, is nice : ) but not pretty :('), split_sentences = T)
  
  replace_dict = quanteda::dictionary(list(':('      = ': (',
                                           'the_bad' = 'the bad'))
  tc$replace_dictionary(replace_dict)   ## concatenate the_bad and one of the emoticons (just for example)
  tc$code_dictionary(dict)
  expect_equal(tc$tokens$sentiment, c(NA,1,NA,-1,NA,NA,-1,NA,NA,1,1,1,NA,-1,-1,-1))

})


