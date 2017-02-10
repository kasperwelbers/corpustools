test_that("Semnet works", {
  #devtools::install_github('kasperwelbers/tcorpus')
  library(tcorpus)
  text = c('Renewable fuel is better than fossil fuels!',
           'A fueled debate about fuel',
           'Mark Rutte is simply Rutte')
  tc = create_tcorpus(text)

  g = semnet(tc, 'word', measure = 'count_undirected')
  expect_equal(sum(E(g)$weight), 44)
  g = semnet(tc, 'word', measure = 'cosine')
  expect_equal(round(sum(E(g)$weight),2), 37.94)
  g = semnet(tc, 'word', measure = 'con_prob')
  expect_equal(sum(E(g)$weight), 77.5)
  g = semnet(tc, 'word', measure = 'con_prob_weighted')
  expect_equal(round(sum(E(g)$weight),2), 39.38)
  g = semnet(tc, 'word', measure = 'chi2')
  expect_equal(round(sum(E(g)$weight),2), 36.35)

  g_ego = ego_semnet(g, c('fuel','fuels'), only_filter_vertices = F)
  expect_equal(unique(get.data.frame(g_ego)$from), c('fuel','fuels'))

  ## windowed semnet
  g = semnet_window(tc, 'word', window.size = 10, measure = 'count_directed')
  expect_equal(ecount(g), 88)
  g = semnet_window(tc, 'word', window.size = 5, measure = 'count_directed')
  expect_equal(ecount(g), 82)
  g = semnet_window(tc, 'word', window.size = 10, measure = 'chi2')
  expect_equal(ecount(g), 88)

  g = semnet_window(tc, 'word', window.size = 10, measure = 'cosine')
  g = semnet_window(tc, 'word', window.size = 10, measure = 'con_prob')

  ## backbone extraction
  g = semnet(tc, 'word', measure = 'cosine')
  gb = getBackboneNetwork(g, alpha=0.5)
  expect_equal(ecount(gb), 43)

  ## also works with NA's (which are ignored)
  tc_withNA = search_recode(tc, 'word', NA, 'Rutte OR Renewable')
  g = semnet(tc_withNA, 'word', measure = 'count_undirected')
  expect_true(!'Rutte' %in% V(g)$name)
  g = semnet_window(tc_withNA, 'word', window.size = 10, n.batches = NA)
  expect_true(!'Rutte' %in% V(g)$name)
})
