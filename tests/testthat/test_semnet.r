test_that("Semnet works", {
  library(tcorpus)
  text = c('Renewable fuel is better than fossil fuels!',
           'A fueled debate about fuel',
           'Mark Rutte is simply Rutte')
  tc = create_tcorpus(text)

  g = cooccurrence(tc, 'word', measure = 'count_undirected')
  expect_equal(sum(E(g)$weight), 44)
  g = cooccurrence(tc, 'word', measure = 'cosine')
  expect_equal(round(sum(E(g)$weight),2), 37.94)
  g = cooccurrence(tc, 'word', measure = 'con_prob')
  expect_equal(sum(E(g)$weight), 19.875)
  g = cooccurrence(tc, 'word', measure = 'con_prob_weighted')
  expect_equal(sum(E(g)$weight), 19.875)
  g = cooccurrence(tc, 'word', measure = 'obs/exp')
  get.data.frame(g)

  g = cooccurrence(tc, 'word', measure = 'con_prob_weighted', chi2=T)
  expect_equal(sum(E(g)$weight), 19.875)
  expect_equal(E(g)$chi2[1], 0.1875)


  ## windowed cooccurrence
  g = cooccurrence_window(tc, 'word', window.size = 10, measure = 'count_directed')
  expect_equal(ecount(g), 88)
  g = cooccurrence_window(tc, 'word', window.size = 5, measure = 'count_directed')
  expect_equal(ecount(g), 82)
  g = cooccurrence_window(tc, 'word', window.size = 10, measure = 'cosine')
  g = cooccurrence_window(tc, 'word', window.size = 10, measure = 'con_prob')

  ## backbone extraction
  g = cooccurrence(tc, 'word', measure = 'cosine')
  gb = getBackboneNetwork(g, alpha=0.5)
  expect_equal(ecount(gb), 43)

  ## also works with NA's (which are ignored)
  tc_withNA = search_recode(tc, 'word', NA, 'Rutte OR Renewable')
  g = cooccurrence(tc_withNA, 'word', measure = 'count_undirected')
  expect_true(!'Rutte' %in% V(g)$name)
  g = cooccurrence_window(tc_withNA, 'word', window.size = 10)
  expect_true(!'Rutte' %in% V(g)$name)
})
