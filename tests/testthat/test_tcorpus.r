testthat::context('tCorpus')


test_that("tCorpus class works", {
  ### create from data.frame
  tokens = data.frame(document = c(rep(1, 8), rep(2, 5), rep(3, 5)),
                 sentence = c(rep(1,8), rep(1,5), rep(1,5)),
                 id = 1:18,
                 token = c('Renewable','fuel','is','better','than','fossil','fuels','!','A','fueled','debate','about','fuel','Mark','Rutte','is','simply','Rutte'))
  tc = tokens_to_tcorpus(tokens, doc_col ='document', sentence_col = 'sentence', token_id_col = 'id')
  doc_id = tc$get('doc_id')
  expect_equal(doc_id, as.factor(c(rep('1', 8), rep('2', 5), rep('3', 5))))
  token = tc$get('token')
  expect_equal(as.character(token), c('Renewable','fuel','is','better','than','fossil','fuels','!','A','fueled','debate','about','fuel','Mark','Rutte','is','simply','Rutte'))

  ### create from text
  text = c('Renewable fuel is better than fossil fuels!',
           'A fueled debate about fuel',
           'Mark Rutte is simply Rutte')
  tc = create_tcorpus(text, doc_id = c('a','b','c'))

  doc_id = tc$get('doc_id')
  expect_equal(doc_id, fast_factor(c(rep('a', 8), rep('b', 5), rep('c', 5))))
  token = tc$get('token')
  expect_equal(token, fast_factor(c('Renewable','fuel','is','better','than','fossil','fuels','!','A','fueled','debate','about','fuel','Mark','Rutte','is','simply','Rutte')))

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
  tc = create_tcorpus(d, text_columns='text', doc_column = 'document', split_sentences = T)

  doc_id = tc$get('doc_id')
  expect_equal(doc_id, fast_factor(c(rep('a', 8), rep('b', 5), rep('c', 5))))
  token = tc$get('token')
  expect_equal(token, fast_factor(c('Renewable','fuel','is','better','than','fossil','fuels','!','A','fueled','debate','about','fuel','Mark','Rutte','is','simply','Rutte')))

  meta_medium = tc$get_meta('medium')
  expect_equal(as.character(meta_medium), c('a','a','b'))

  ## change data
  token = tc$get('token')
  newtoken =fast_factor(tolower(token))
  tc = tc$set('token', newtoken)
  expect_equal(tc$get('token'), newtoken)

  medium = tc$get_meta('medium')
  newmedium = as.factor(paste('source', medium))
  tc = tc$set_meta('medium', newmedium)
  expect_equal(tc$get_meta('medium'), newmedium)
  
  
  ## merge data
  d = data.frame(text = c('Renewable fuel is better than fossil fuels! Shouted someone for a test case',
                          'A fueled debate about fuel',
                          'Mark Rutte is simply Rutte'),
                 id = c('a','b','c'),
                 source  =c('aa','bb','cc'))
  tc = create_tcorpus(d, doc_col='id', split_sentences = T)
  
  df = data.frame(doc_id=c('a','b'), test=c('A','B'))
  tc$merge(df, by='doc_id')
  expect_equal(as.numeric(table(tc$tokens$test)), c(14,5))
  
  df = data.frame(doc_id=c('a','b'), sentence=1, test2=c('A','B'))
  tc$merge(df, by=c('doc_id', 'sentence'))
  expect_equal(as.numeric(table(tc$tokens$test2)), c(8,5))
  
  df = data.frame(doc_id=c('a','b'), sentence=1, token_id=c(3,4), test3=c('A','B'))
  tc$merge(df, by=c('doc_id', 'sentence', 'token_id'))
  expect_equal(as.numeric(table(tc$tokens$test3)), c(1,1))

  df = data.frame(document_id=c('a','b'), test=c(1,2), test4=c('A','B'))
  tc$merge(df, by.x = 'doc_id', by.y='document_id', columns='test4')
  expect_equal(as.numeric(table(tc$tokens$test)), c(14,5))
  
  meta = data.frame(doc_id=c('a','b'), test=c('A','B'))
  tc$merge_meta(meta, by='doc_id')
  expect_equal(as.numeric(table(tc$meta$test)), c(1,1))

  meta = data.frame(source=c('aa'), test2=c('A'))
  tc$merge_meta(meta, by='source')
  expect_equal(as.numeric(table(tc$meta$test2)), c(1))
  
  
  
})
