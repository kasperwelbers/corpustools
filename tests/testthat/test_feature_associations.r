test_that("feature associations", {
  cat('\n', '-> Testing: Feature associations', '\n')
  start_time = Sys.time()

  library(corpustools)
  tc = create_tcorpus(sotu_texts)

  topf = tc$feature_associations('war')
  topf

  freq = table(tc$data$token)

  expect_equal(as.character(topf$feature[1:3]),
               c('terror','cold','chiefs'))

  cat('\n    (', round(difftime(Sys.time(), start_time, units = 'secs'), 2), ' sec)', '\n', sep='')
})

