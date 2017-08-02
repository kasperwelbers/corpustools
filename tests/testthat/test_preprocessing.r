test_that("preprocessing works", {
  cat('\n', '-> Testing: Preprocessing', '\n')
  start_time = Sys.time()

  library(corpustools)
  #library(testthat)
  tokens = data.frame(document = c(rep(1, 8), rep(2, 5), rep(3, 5)),
                      sentence = c(rep(1, 8), rep(2, 5), rep(3, 5)),
                      id = 1:18,
                      token = c('Renewable','fuel','is','better','than','fossil','fuels','!','A','fueled','debate','about','fuel','Mark','Rutte','is','simply','Rutte'))
  meta = data.frame(document = c(1,2,3), medium=c('a','b','c'))

  tc = tokens_to_tcorpus(tokens, doc_col ='document', token_i_col = 'id', meta=meta)

  dtm = tc$dtm('token')
  ## test preprocess_feature
  token = tc$get('token')

  context = tc$context('document')
  feature = preprocess_tokens(token, context=context, language='english', lowercase = T, use_stemming = T, ngrams = 3)

  tc = tc$preprocess('token', new_column='feature', language='english', lowercase = T, use_stemming = T, ngrams = 3, ngram_context = 'document')
  expect_equal(tc$get('feature'), feature)

  cat('\n    (', round(difftime(Sys.time(), start_time, units = 'secs'), 2), ' sec)', '\n', sep='')

})
