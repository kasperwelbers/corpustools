testthat::context('Compare corpora')


test_that("Corpus comparison works!", {
  text = c('Renewable fuel is better than fossil fuels!',
           'A fueled debate about fuel',
           'Mark Rutte is simply Rutte')
  tc = create_tcorpus(text, split_sentences = T)

  text_y = c('We are not talking about fuel again!',
           'There must be much more interesting things to discuss',
           'In the meantime, Mark Rutte is still simply Rutte')
  tc_y = create_tcorpus(text_y, split_sentences = T)

  comp = compare_corpus(tc, tc_y, 'token')
  expect_equal(round(sum(comp$chi2),3), 2.215)
  #graphics::plot(comp)
  #graphics::plot(comp, mode = 'both')

  comp = compare_subset(tc, 'token', query_x = 'rutte')
  expect_equal(round(sum(comp$chi2),3), 5.646)
  #graphics::plot(comp)

  comp = compare_subset(tc, 'token', query_x = 'rutte', what='docfreq', smooth=F)
  expect_equal(round(sum(comp$chi2),3), 4.809)

  comp = compare_corpus(tc, tc_y, 'token', what = 'cooccurrence', smooth = F)
})
