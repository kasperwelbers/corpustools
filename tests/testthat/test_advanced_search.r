testthat::context('Advanced query search')


test_that("Advanced query search works", {
  options(Matrix.warnDeprecatedCoerce = 2)
  
  ## include meta subset
  tc = create_tcorpus(sotu_texts, doc_column = 'id')
  hits = search_contexts(tc, 'test~[party == "nothing"]')
  expect_equal(nrow(hits$hits),0)
  
  hits = search_contexts(tc, 'test')
  hits_dem = search_contexts(tc, 'test~[party == "Democrats"]')
  hits_rep = search_contexts(tc, 'test~[party == "Republicans"]')
  expect_equal(nrow(hits$hits), 9)
  expect_equal(nrow(hits_dem$hits), 4)
  expect_equal(nrow(hits_rep$hits), 5)
  
  ## include token subset
  tc = tokens_to_tcorpus(corenlp_tokens, doc_col = 'doc_id', sentence_col = 'sentence', token_id_col = 'id')
  hits = search_features(tc, 'mary~{POS == "NNP"} OR (lousy example for test)')
  expect_equal(nrow(hits$hits), 6)
  
  ## using the sub/flag query to find only mary as a direct object
  hits = search_features(tc, 'mary~{relation == "dobj"}', context_level = 'sentence')
  expect_equal(as.character(hits$hits$feature), c('Mary','Mary','Mary'))

  ## selecting from a different column without changing the feature column (can be used to combine columns)
  hits = search_features(tc, 'relation: nsubj')
  expect_equal(as.character(hits$hits$feature), c('John','Mary','Pete','he','he','John'))
})

