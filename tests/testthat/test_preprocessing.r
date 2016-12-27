test_that("preprocessing works", {
  library(tcorpus)

  tokens = data.frame(document = c(rep(1, 8), rep(2, 5), rep(3, 5)),
                      sentence = c(rep(1, 8), rep(2, 5), rep(3, 5)),
                      id = 1:18,
                      word = c('Renewable','fuel','is','better','than','fossil','fuels','!','A','fueled','debate','about','fuel','Mark','Rutte','is','simply','Rutte'))
  tc = tokens_to_tcorpus(tokens, doc_col ='document', sent_i_col = 'sentence', word_i_col = 'id')

  ## test preprocess_feature
  word = get_column(tc, 'word')
  context = get_context(tc, 'sentence')
  feature = preprocess_words(word, context=context, language='english', lowercase = T, use_stemming = T, ngrams = 3)

  tc = preprocess_feature(tc, 'word', new_column='feature', language='english', lowercase = T, use_stemming = T, ngrams = 3, ngram_context = 'sentence')
  expect_equal(get_column(tc, 'feature'), feature)

})
