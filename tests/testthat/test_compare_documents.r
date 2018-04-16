testthat::context('Compare documents')


test_that("comparing documents works", {
  d = data.frame(text = c('Renewable fuel is better than fossil fuels!',
                          'A fueled debate about fuel',
                          'Mark Rutte is simply Rutte'),
                 date = c('2010-01-01','2010-01-01','2012-01-01'))
  tc = create_tcorpus(d)

  g = tc$compare_documents()
  tc$semnet('token')

  ## compare as DTM
  dtm = tc$dtm('token', weight = 'norm_tfidf')
  igraph::get.data.frame(compare_documents_dtm(dtm, meta=tc$get_meta(), date_col = 'date', verbose=F))

  ## find out why dtm is suddenly empty...
  g = tc$compare_documents()
  expect_equal(round(igraph::E(g)$weight,3), round(c(0.027, 0.027, 0.022, 0.022),3))

  g = tc$compare_documents(to_subset = doc_id == '1')
  expect_true(igraph::ecount(g) == 2)

  g = tc$compare_documents(date_col = 'date')
  expect_true(igraph::is.directed(g))
  expect_true(igraph::ecount(g) == 4)

  g = tc$compare_documents(date_col = 'date', hour_window = c(0,36), measure = 'overlap_pct')

  expect_true(igraph::is.directed(g))
  expect_true(igraph::ecount(g) == 2)

  ## due to random selection of which duplicate to delete, chains can lead to varying number of duplicates. Think of something more smart, or order by id/name instead of random
  dedup = tc$deduplicate(feature='token', similarity = 0.025)
  #expect_true(dedup$n_meta == 1)
})


