testthat::context('Feature associations')


test_that("feature associations", {
  library(corpustools)
  tc = create_tcorpus(sotu_texts)

  topf = feature_associations(tc, 'token', 'war')
  topf

  expect_equal(round(topf$ratio[topf$feature == 'terror'], 2), 13.93)
})


