testthat::context('Compare documents')

test_that("comparing documents works", {
  options(Matrix.warnDeprecatedCoerce = 2)
  
  
  d = data.frame(text = c('Renewable fuel is better than fossil fuels!',
                          'A fueled debate about fuel',
                          'Mark Rutte is simply Rutte'),
                 date = as.POSIXct(c('2010-01-01','2010-01-01','2012-01-01')),
                 meta = c('a','a','b'))
  tc = create_tcorpus(d)

  ## compare as DTM
  dtm = get_dtm(tc, 'token', weight = 'norm_tfidf')

  ## find out why dtm is suddenly empty...
  g = compare_documents(tc, verbose=F)
  testthat::expect_equal(round(igraph::E(g)$weight,3), round(c(0.033, 0.033, 0.027, 0.027),3))
  #testthat::expect_true(!igraph::is_directed(g))
  #testthat::expect_equal(round(igraph::E(g)$weight,3), round(c(0.027, 0.022),3))

  g = compare_documents(tc, from_subset = doc_id == '1', measure = 'overlap_pct', verbose=F)
  testthat::expect_true(igraph::ecount(g) == 2)
  #igraph::as_data_frame(g)

  g = compare_documents(tc, measure='overlap_pct', verbose=F)
  testthat::expect_true(igraph::is_directed(g))
  testthat::expect_true(igraph::ecount(g) == 4)

  g = compare_documents(tc, date_col = 'date', hour_window = c(0,36), measure = 'overlap_pct', verbose=F)

  testthat::expect_true(igraph::is_directed(g))
  testthat::expect_true(igraph::ecount(g) == 2)

  ## due to random selection of which duplicate to delete, chains can lead to varying number of duplicates. Think of something more smart, or order by id/name instead of random
  tc = create_tcorpus(d)

  dedup = tc$deduplicate(feature='token', similarity = 0.025, verbose=F)
  testthat::expect_true(dedup$n_meta == 1)
})


