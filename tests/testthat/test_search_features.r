test_that("Query search works", {
  cat('\n', '-> Testing: Search features', '\n')
  start_time = Sys.time()

  library(corpustools)
  text = c('Renewable fuel is better than fossil fuels!',
           'A fueled debate about fuel',
           'Mark Rutte is simply Rutte')
  tc = create_tcorpus(text, doc_id = c('a','b','c'), split_sentences = T)
  #corpustools:::sourceall()

  ## simple keyword only
  hits = tc$search_features(keyword = 'fuel')
  expect_equal(as.character(hits$hits$feature), c('fuel','fuel'))

  ## casting results
  res = tc$cast(hits=hits)
  expect_equal(colnames(res), c('group','N','query_1'))

  ## multiword keywords
  hits = tc$search_features('"a fueled debate"', only_last_mword = T)
  expect_equal(as.character(hits$hits$feature), c('debate'))
  hits = tc$search_features('"a fueled debate"', only_last_mword = F)
  expect_equal(as.character(hits$hits$feature), c('A','fueled', 'debate'))

  ## proximity keywords
  hits = tc$search_features('"renewable fuels"~10')
  expect_equal(as.character(hits$hits$feature), c('Renewable','fuels'))
  hits = tc$search_features('"renewable fuels"~2')
  expect_true(nrow(hits$hits) == 0)

  ## two keywords
  hits = tc$search_features(keyword = 'fuel fuels')
  expect_equal(as.character(hits$hits$feature), c('fuel','fuels','fuel'))

  ## keyword with wildcard
  hits = tc$search_features(keyword = 'fuel*')
  expect_equal(as.character(hits$hits$feature), c('fuel','fuels','fueled','fuel'))
  hits = tc$search_features(keyword = 'fue?s')
  expect_equal(as.character(hits$hits$feature), c('fuels'))

  ## case sensitive flag
  hits = tc$search_features(keyword = 'Rutte~s')
  expect_true(nrow(hits$hits) == 2)
  hits = tc$search_features(keyword = 'rutte~s')
  expect_true(nrow(hits$hits) == 0)

  ## keyword and condition
  hits = tc$search_features(keyword = 'fuel*', condition = 'renewable green clean')
  expect_equal(as.character(hits$hits$feature), c('fuel','fuels'))

  ## BOOLEAN conditions
  hits = tc$search_features(keyword = 'fuel', condition = '(renewable AND fuel) OR (debate AND fuel)')
  expect_equal(as.character(hits$hits$feature), c('fuel','fuel'))

  ## multiword and proximity conditions
  hits = tc$search_features(keyword = 'fuel', condition = '"renewable fuel" "a debate"~3')
  expect_equal(as.character(hits$hits$feature), c('fuel','fuel'))

  ## condition close to keyword
  hits = tc$search_features(keyword = 'fuel', condition = 'fossil^2') ## not within 2 words distance
  expect_true(nrow(hits$hits) == 0)
  hits = tc$search_features(keyword = 'fuel', condition = 'fossil^10') ## this matches
  expect_true(nrow(hits$hits) == 1)

  ## condition before and after to keyword
  hits = tc$search_features(keyword = 'fuel', condition = 'better>2') ## better occurs (within) 2 words after fuel
  expect_true(nrow(hits$hits) == 1)
  hits = tc$search_features(keyword = 'fuel', condition = 'better<2') ## but not before
  expect_true(nrow(hits$hits) == 0)

  hits = tc$search_features(keyword = 'fuel', condition = '"is better">1') ## also test with quotes, for which we need only 1 because "is" is adjacent to fuel
  expect_true(nrow(hits$hits) == 1)
  hits = tc$search_features(keyword = 'fuel', condition = '"is better"<1') ##
  expect_true(nrow(hits$hits) == 0)

  #expect_true(nrow(hits$hits) == 0)
  #hits = tc$search_features(keyword = 'fuel', condition = 'fossil^10') ## this matches
  #expect_true(nrow(hits$hits) == 1)


  ## combining flags
  hits = tc$search_features(keyword = 'fuel', condition = '"A~s debate"~3^4')
  expect_equal(as.character(hits$hits$feature), c('fuel'))

  ## condition once parameter
  hits_f = tc$search_features(keyword = 'rutte', condition = 'mark^2')
  hits_t = tc$search_features(keyword = 'rutte', condition = 'mark^2', condition_once = T)
  expect_equal(as.character(hits_f$hits$feature), c('Rutte'))
  expect_equal(as.character(hits_t$hits$feature), c('Rutte','Rutte'))

  ## multiple queries
  queries = data.frame(code=c('renewable fuel', 'mark rutte', 'debate'),
                       keyword=c('fuel*', 'rutte', 'debate'),
                       condition = c('renewable green clean', 'mark^2', ''))
  hits = tc$search_features(queries=queries, condition_once=c(F,T,F))
  expect_equal(as.character(hits$hits$feature), c('fuel','fuels','debate', 'Rutte', 'Rutte'))

  ## with subsetting
  hits = tc$search_features(keyword = 'fuel', subset_meta = doc_id == 'a')
  expect_true(nrow(hits$hits) == 1) ## should be only the hit in doc 'a', instead of 'a' and 'b'

  ## kwic
  kw = tc$kwic(i=4, nwords=2)
  expect_true(kw$kwic == '...fuel is <better> than fossil...')
  hits = tc$search_features(keyword = 'better')
  kw = tc$kwic(hits=hits, nwords=2)
  expect_true(kw$kwic == '...fuel is <better> than fossil...')
  kw = tc$kwic(keyword = 'better', nwords=2)
  expect_true(kw$kwic == '...fuel is <better> than fossil...')

  ## kwic with multiword queries
  kw = tc$kwic(keyword = c('"renewable fuels"~10'), nsample = NA) ## without gap
  expect_equal(kw$feature, 'Renewable -> fuels')
  kw = tc$kwic(keyword = c('"renewable fuels"~10'), nwords = 2) ## without gap
  expect_true(grepl('[...]', kw$kwic))

  ## code queries
  #tc$search_code(tc, queries=queries)
  ##code = code_features(tc, queries, condition_once=c(F,T,F))
  ##expect_equal(as.numeric(table(code)), c(12,1,2,2))

  cat('\n    (', round(difftime(Sys.time(), start_time, units = 'secs'), 2), ' sec)', '\n', sep='')

})

