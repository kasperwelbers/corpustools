test_that("preprocessing works", {
  library(corpustools)

  tokens = data.frame(document = c(rep(1, 8), rep(2, 5), rep(3, 5)),
                      sentence = c(rep(1, 8), rep(2, 5), rep(3, 5)),
                      id = 1:18,
                      word = c('Renewable','fuel','is','better','than','fossil','fuels','!','A','fueled','debate','about','fuel','Mark','Rutte','is','simply','Rutte'))
  meta = data.frame(document = c(1,2,3), medium=c('a','b','c'))

  tc = tokens_to_tcorpus(tokens, doc_col ='document', word_i_col = 'id', meta=meta)
  ## test preprocess_feature
  word = tc$data('word')

  context = get_context(tc, 'document')
  feature = preprocess_words(word, context=context, language='english', lowercase = T, use_stemming = T, ngrams = 3)

  tc = tc$preprocess('word', new_column='feature', language='english', lowercase = T, use_stemming = T, ngrams = 3, ngram_context = 'document')
  expect_equal(tc$data('feature'), feature)
})
