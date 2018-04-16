testthat::context('Semnet')


test_that("Semnet works", {
  text = c('Renewable fuel is better than fossil fuels!',
           'A fueled debate about fuel',
           'Mark Rutte is simply Rutte')
  tc = create_tcorpus(text)

  g = tc$semnet('token', measure = 'count_undirected')
  expect_equal(sum(igraph::E(g)$weight), 44)
  g = tc$semnet('token', measure = 'cosine')
  expect_equal(round(sum(igraph::E(g)$weight),2), 37.94)
  g = tc$semnet('token', measure = 'con_prob')
  expect_equal(sum(igraph::E(g)$weight), 77.5)
  g = tc$semnet('token', measure = 'con_prob_weighted')
  expect_equal(round(sum(igraph::E(g)$weight),2), 39.38)
  g = tc$semnet('token', measure = 'chi2')
  expect_equal(round(sum(igraph::E(g)$weight),2), 36.35)

  g_ego = ego_semnet(g, c('fuel','fuels'), only_filter_vertices = F)
  expect_equal(unique(igraph::get.data.frame(g_ego)$from), c('fuel','fuels'))

  ## windowed semnet
  g = tc$semnet_window('token', window.size = 10, measure = 'count_directed')
  expect_equal(igraph::ecount(g), 88)
  g = tc$semnet_window('token', window.size = 5, measure = 'count_directed')
  expect_equal(igraph::ecount(g), 82)
  g = tc$semnet_window('token', window.size = 10, measure = 'chi2')
  expect_equal(igraph::ecount(g), 88)

  g = tc$semnet_window('token', window.size = 10, measure = 'cosine')
  g = tc$semnet_window('token', window.size = 10, measure = 'con_prob')

  ## backbone extraction
  g = tc$semnet('token', measure = 'cosine')
  gb = backbone_filter(g, alpha=0.5)
  expect_equal(igraph::ecount(gb), 43)

  tc = refresh_tcorpus(tc)

  tc$search_features('Rutte OR Renewable')
  ## also works with NA's (which are ignored)
  tc_withNA = tc$copy()$search_recode('token', NA, 'Rutte OR Renewable')
  tc_withNA$get()

  g = tc_withNA$semnet('token', measure = 'count_undirected')
  expect_true(!'Rutte' %in% igraph::V(g)$name)
  g = tc_withNA$semnet_window('token', measure = 'con_prob', window.size = 10, n.batches = NA)
  expect_true(!'Rutte' %in% igraph::V(g)$name)
})
