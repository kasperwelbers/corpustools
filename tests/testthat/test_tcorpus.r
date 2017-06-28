test_that("tCorpus class works", {
  cat('\n', '-> Testing: tCorpus', '\n')
  start_time = Sys.time()

  library(corpustools)
  ### create from data.frame
  tokens = data.frame(document = c(rep(1, 8), rep(2, 5), rep(3, 5)),
                 sentence = c(1:8, 1:5, 1:5),
                 id = 1:18,
                 token = c('Renewable','fuel','is','better','than','fossil','fuels','!','A','fueled','debate','about','fuel','Mark','Rutte','is','simply','Rutte'))
  tc = tokens_to_tcorpus(tokens, doc_col ='document', sent_i_col = 'sentence', token_i_col = 'id')

  doc_id = tc$data$doc_id
  expect_equal(doc_id, as.factor(c(rep('1', 8), rep('2', 5), rep('3', 5))))
  token = tc$data$token
  expect_equal(as.character(token), c('Renewable','fuel','is','better','than','fossil','fuels','!','A','fueled','debate','about','fuel','Mark','Rutte','is','simply','Rutte'))

  ### create from text
  text = c('Renewable fuel is better than fossil fuels!',
           'A fueled debate about fuel',
           'Mark Rutte is simply Rutte')
  tc = create_tcorpus(text, doc_id = c('a','b','c'))

  doc_id = tc$data$doc_id
  expect_equal(doc_id, as.factor(c(rep('a', 8), rep('b', 5), rep('c', 5))))
  token = tc$data$token
  expect_equal(token, as.factor(c('Renewable','fuel','is','better','than','fossil','fuels','!','A','fueled','debate','about','fuel','Mark','Rutte','is','simply','Rutte')))

  ### create from text with sentences
  text = c('Renewable fuel is better than fossil fuels! This is really a fueled debate about fuel', 'Mark Rutte is simply Rutte')
  tc = create_tcorpus(text, split_sentences = T)

  doc = get_context(tc, 'document')
  expect_equal(doc, as.factor(c(rep(1, 16), rep(2,5))))

  docsent = get_context(tc, 'sentence')
  expect_equal(levels(docsent), c('1 #1','1 #2', '2 #1'))

  ### create from data.frame
  d = data.frame(text = c('Renewable fuel is better than fossil fuels!',
                          'A fueled debate about fuel',
                          'Mark Rutte is simply Rutte'),
                 document = c('a','b','c'),
                 medium = c('a','a','b'),
                 date = c('2010-01-01','2010-01-02','2010-01-01'))
  tc = create_tcorpus(d, text_columns='text', doc_column = 'document')

  doc_id = tc$data$doc_id
  expect_equal(doc_id, as.factor(c(rep('a', 8), rep('b', 5), rep('c', 5))))
  token = tc$data$token
  expect_equal(token, as.factor(c('Renewable','fuel','is','better','than','fossil','fuels','!','A','fueled','debate','about','fuel','Mark','Rutte','is','simply','Rutte')))

  meta_medium = tc$meta$medium
  expect_equal(meta_medium, as.factor(c('a','a','b')))

  ## change data
  token = tc$data$token
  newtoken =fast_factor(tolower(token))
  tc = tc$set('token', newtoken)
  expect_equal(tc$data$token, newtoken)

  medium = tc$meta$medium
  newmedium = as.factor(paste('source', medium))
  tc = tc$set_meta('medium', newmedium)
  expect_equal(tc$meta$medium, newmedium)

  cat('\n    (', round(difftime(Sys.time(), start_time, units = 'secs'), 2), ' sec)', '\n', sep='')

})
