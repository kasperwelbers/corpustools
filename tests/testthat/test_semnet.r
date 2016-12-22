test_that("Semnet works", {
  library(tcorpus)
  text = c('Renewable fuel is better than fossil fuels!',
           'A fueled debate about fuel',
           'Mark Rutte is simply Rutte')
  tc = create_tcorpus(text)

  ## document cooccurrence
  g = cooccurrence(tc, 'word', measure = 'cooccurrence')
  expect_equal(sum(E(g)$weight), 47)
  g = cooccurrence(tc, 'word', measure = 'cosine')
  expect_equal(round(sum(E(g)$weight),2), 37.94)
  g = cooccurrence(tc, 'word', measure = 'conprob')
  expect_equal(sum(E(g)$weight), 80)
  g = cooccurrence(tc, 'word', measure = 'conprob_weighted')
  expect_equal(sum(E(g)$weight), 40)

  ## windowed cooccurrence
  g = cooccurrence_window(tc, 'word', window.size = 10)
  expect_equal(ecount(g), 88)
  g = cooccurrence_window(tc, 'word', window.size = 5)
  expect_equal(ecount(g), 82)

  ## backbone extraction

  g = cooccurrence(tc, 'word', measure = 'cosine')
  gb = getBackboneNetwork(g, alpha=0.5)
  expect_equal(ecount(gb), 43)

})
