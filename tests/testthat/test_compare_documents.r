test_that("comparing documents works", {
  library(tcorpus)
  d = data.frame(text = c('Renewable fuel is better than fossil fuels!',
                          'A fueled debate about fuel',
                          'Mark Rutte is simply Rutte'),
                 date = c('2010-01-01','2010-01-01','2012-01-01'))
  tc = create_tcorpus(d)

  g = tc$compare_documents()
  expect_equal(round(E(g)$weight,3), round(c(0.1581139, 0.1336306, 0.1581139, 0.1336306),3))

  g = tc$compare_documents(to_subset = doc_id == '1')
  expect_true(ecount(g) == 2)

  g = tc$compare_documents(date_col = 'date')
  expect_true(is.directed(g))
  expect_true(ecount(g) == 4)

  g = tc$compare_documents(date_col = 'date', hour_window = c(0,36))
  expect_true(is.directed(g))
  expect_true(ecount(g) == 2)

  ## due to random selection of which duplicate to delete, chains can lead to varying number of duplicates. Think of something more smart, or order by id/name instead of random
  dedup = tc$deduplicate(feature='word', similarity = 0.1)
  #expect_true(dedup$n_meta == 1)
})

