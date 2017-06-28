test_that("comparing documents works", {
  cat('\n', '-> Testing: Compare documents', '\n')
  start_time = Sys.time()

  library(corpustools)
  corpustools:::sourceall()
  d = data.frame(text = c('Renewable fuel is better than fossil fuels!',
                          'A fueled debate about fuel',
                          'Mark Rutte is simply Rutte'),
                 date = c('2010-01-01','2010-01-01','2012-01-01'))
  tc = create_tcorpus(d)

  #tc$dtm('token', subset_tokens = token_i < 10, weight = 'norm_tfidf')
  #m = tc$dtm('token', subset_meta = doc_id == 1)

  g = tc$compare_documents()
  expect_equal(round(igraph::E(g)$weight,3), round(c(0.027, 0.022, 0.027, 0.022),3))

  g = tc$compare_documents(to_subset = doc_id == '1')
  expect_true(igraph::ecount(g) == 2)

  g = tc$compare_documents(date_col = 'date')
  expect_true(igraph::is.directed(g))
  expect_true(igraph::ecount(g) == 4)

  capture.output({ ## ignores print verbose
  g = tc$compare_documents(date_col = 'date', hour_window = c(0,36))
  })

  expect_true(igraph::is.directed(g))
  expect_true(igraph::ecount(g) == 2)

  ## due to random selection of which duplicate to delete, chains can lead to varying number of duplicates. Think of something more smart, or order by id/name instead of random
  dedup = tc$deduplicate(feature='token', similarity = 0.025)
  #expect_true(dedup$n_meta == 1)
  cat('\n    (', round(difftime(Sys.time(), start_time, units = 'secs'), 2), ' sec)', '\n', sep='')

})

