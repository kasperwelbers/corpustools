testthat::context('Feature stats')


test_that("feature associations", {
  library(corpustools)
  tc = create_tcorpus(sotu_texts)

  topf = feature_stats(tc, 'token')
  expect_equal(topf$termfreq[1], 7)
  topf = top_features(tc, 'token', group_by_meta = 'president')
  expect_equal(topf$r4[1], 'to')

})


