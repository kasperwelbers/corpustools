test_that("Query search works", {
  library(tcorpus)
  text = c('Renewable fuel is better than fossil fuels!',
           'A fueled debate about fuel',
           'Mark Rutte is simply Rutte')
  tc = create_tcorpus(text, doc_id = c('a','b','c'), split_sentences = T)

  #search_contexts(tc, 'mark AND rutte')

  ## simple keyword only
  hits = search_features(tc, keyword = 'fuel')
  expect_equal(as.character(hits$feature), c('fuel','fuel'))

  ## multiword keywords
  hits = search_features(tc, '"a fueled debate"', only_last_mword = T)
  expect_equal(as.character(hits$feature), c('debate'))

  ## keep all words in multiword search
  hits = search_features(tc, '"a fueled debate"', only_last_mword = F)
  expect_equal(as.character(hits$feature), c('A','fueled', 'debate'))

  ## pre-compute feature index
  tc = set_feature_index(tc, feature='word')

  ## two keywords
  hits = search_features(tc, keyword = 'fuel fuels')
  expect_equal(as.character(hits$feature), c('fuel','fuels','fuel'))

  ## keyword with wildcard
  hits = search_features(tc, keyword = 'fuel*')
  expect_equal(as.character(hits$feature), c('fuel','fuels','fueled','fuel'))

  ## keyword and condition
  hits = search_features(tc, keyword = 'fuel*', condition = 'renewable green clean')
  expect_equal(as.character(hits$feature), c('fuel','fuels'))


  ## condition once parameter
  hits_f = search_features(tc, keyword = 'rutte', condition = 'mark~2')
  hits_t = search_features(tc, keyword = 'rutte', condition = 'mark~2', condition_once = T)
  expect_equal(as.character(hits_f$feature), c('Rutte'))
  expect_equal(as.character(hits_t$feature), c('Rutte','Rutte'))

  ## multiple queries
  queries = data.frame(code=c('renewable fuel', 'mark rutte', 'debate'),
                       keyword=c('fuel*', 'rutte', 'debate'),
                       condition = c('renewable green clean', 'mark~2', ''))
  hits = search_features(tc, queries=queries, condition_once=c(F,T,F))
  expect_equal(as.character(hits$feature), c('fuel','fuels','debate', 'Rutte', 'Rutte'))

  ## code queries
  ##code = code_features(tc, queries, condition_once=c(F,T,F))
  ##expect_equal(as.numeric(table(code)), c(12,1,2,2))
})
