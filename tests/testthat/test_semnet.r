testthat::context('Semnet')


test_that("Semnet works", {
  text = c('Renewable fuel is better than fossil fuels!',
           'A fueled debate about fuel',
           'Mark Rutte is simply Rutte')
  tc = create_tcorpus(text)

  g = semnet(tc, 'token', measure = 'count_undirected')
  expect_equal(sum(igraph::E(g)$weight), 44)
  g = semnet(tc, 'token', measure = 'cosine')
  expect_equal(round(sum(igraph::E(g)$weight),2), 37.94)
  g = semnet(tc, 'token', measure = 'con_prob')
  expect_equal(sum(igraph::E(g)$weight), 77.5)
  g = semnet(tc, 'token', measure = 'con_prob_weighted')
  expect_equal(round(sum(igraph::E(g)$weight),2), 39.38)
  g = semnet(tc, 'token', measure = 'chi2')
  expect_equal(round(sum(igraph::E(g)$weight),2), 36.35)

  g_ego = ego_semnet(g, c('fuel','fuels'), only_filter_vertices = F)
  expect_equal(unique(igraph::get.data.frame(g_ego)$from), c('fuel','fuels'))

  ## windowed semnet
  g = semnet_window(tc, 'token', window.size = 10, measure = 'count_directed')
  expect_equal(igraph::ecount(g), 88)
  g = semnet_window(tc, 'token', window.size = 5, measure = 'count_directed')
  expect_equal(igraph::ecount(g), 82)
  g = semnet_window(tc, 'token', window.size = 10, measure = 'chi2')
  expect_equal(igraph::ecount(g), 88)

  g = semnet_window(tc, 'token', window.size = 10, measure = 'cosine')
  g = semnet_window(tc, 'token', window.size = 10, measure = 'con_prob')

  ## backbone extraction
  g = semnet(tc, 'token', measure = 'cosine')
  gb = backbone_filter(g, alpha=0.5)
  expect_equal(igraph::ecount(gb), 43)

  tc = refresh_tcorpus(tc)

  search_features(tc, 'Rutte OR Renewable')
  ## also works with NA's (which are ignored)
  tc_withNA = tc$copy()$search_recode('token', NA, 'Rutte OR Renewable')
  tc_withNA$get()

  g = semnet(tc_withNA, 'token', measure = 'count_undirected')
  expect_true(!'Rutte' %in% igraph::V(g)$name)
  g = semnet_window(tc_withNA, 'token', measure = 'con_prob', window.size = 10, n.batches = NA)
  expect_true(!'Rutte' %in% igraph::V(g)$name)
})
