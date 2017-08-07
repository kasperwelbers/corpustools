test_that("feature associations", {
  cat('\n', '-> Testing: Feature associations', '\n')
  start_time = Sys.time()

  library(corpustools)
  tcc = create_tcorpus(sotu_texts)

  topf = tcc$feature_associations('war')
  topf

  expect_equal(round(topf$ratio[topf$feature == 'terror'], 4), 13.9301)

  cat('\n    (', round(difftime(Sys.time(), start_time, units = 'secs'), 2), ' sec)', '\n', sep='')
})


