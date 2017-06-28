test_that("Corpus comparison works!", {
  cat('\n', '-> Testing: Compare corpora', '\n')
  start_time = Sys.time()
  #corpustools:::sourceall()
  #devtools::install_github('kasperwelbers/tcorpus')
  library(corpustools)
  text = c('Renewable fuel is better than fossil fuels!',
           'A fueled debate about fuel',
           'Mark Rutte is simply Rutte')
  tc = create_tcorpus(text, split_sentences = T)

  text_y = c('We are not talking about fuel again!',
           'There must be much more interesting things to discuss',
           'In the meantime, Mark Rutte is still simply Rutte')
  tc_y = create_tcorpus(text_y, split_sentences = T)

  comp = tc$compare_corpus(tc_y, 'token')
  expect_equal(round(sum(comp$chi2),3), 2.215)
  #graphics::plot(comp)
  #graphics::plot(comp, mode = 'both')

  comp = tc$compare_subset('token', query_x = 'rutte')
  expect_equal(round(sum(comp$chi2),3), 5.646)
  #graphics::plot(comp)

  comp = tc$compare_subset('token', query_x = 'rutte', what='docfreq', smooth=F)
  expect_equal(round(sum(comp$chi2),3), 4.809)

  comp = tc$compare_corpus(tc_y, 'token', what = 'cooccurrence', smooth = F)

  cat('\n    (', round(difftime(Sys.time(), start_time, units = 'secs'), 2), ' sec)', '\n', sep='')
})
