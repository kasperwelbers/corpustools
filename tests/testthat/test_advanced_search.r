testthat::context('Advanced query search')


test_that("Advanced query search works", {
  tc = tokens_to_tcorpus(corenlp_tokens, doc_col = 'doc_id', sentence_col = 'sentence', token_id_col = 'id')

  ## using the sub/flag query to find only mary as a direct object
  hits = tc$search_features('mary~{relation: dobj}', context_level = 'sentence')
  expect_equal(as.character(hits$hits$feature), c('Mary','Mary','Mary'))

  ## add a second sub query
  hits = tc$search_features('mary~{relation: dobj, parent: 12 20}', context_level = 'sentence')
  expect_equal(as.character(hits$hits$feature), c('Mary','Mary'))

  ## selecting from a different column without changing the feature column (can be used to combine columns)
  hits = tc$search_features('relation: nsubj')
  expect_equal(as.character(hits$hits$feature), c('John','Mary','Pete','he','he','John'))

  ## any subject to mary as direct object, with mary as a ghost term
  hits = tc$search_features('(relation: nsubj) AND mary~g{relation: dobj}', context_level = 'sentence')
  expect_equal(as.character(hits$hits$feature), c('Pete','he','he','John'))

  ## sequence: nsubj say*
  hits = tc$search_features('"(relation: nsubj) say*"')
  expect_equal(as.character(hits$hits$feature), c('John','says'))

})

