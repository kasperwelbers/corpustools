testthat::context('Feature associations')


test_that("feature associations", {
  tcc = create_tcorpus(sotu_texts)

  topf = tcc$feature_associations('war')
  topf

  expect_equal(round(topf$ratio[topf$feature == 'terror'], 2), 13.93)
})


