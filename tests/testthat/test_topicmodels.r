test_that("topicmodels works", {
  cat('\n', '-> Testing: Topic Models')
  start_time = Sys.time()

  library(corpustools)
  data(sotu_texts)

  corpustools:::sourceall()

  tc = create_tcorpus(sotu_texts[1:500,], 'text', doc_column = 'id')
  tc = tc$preprocess('word', 'feature', lowercase = T, remove_punctuation = T, remove_stopwords = T, use_stemming = T, language = 'english')
  tc = tc$feature_subset('feature', 'feature', subset = freq_filter(feature, min=10) & docfreq_filter(feature, max=0.5*tc$n))

  m = tc$lda_fit(feature = 'feature', create_feature = 'lda', K=5)

  cat('\n    (', round(difftime(Sys.time(), start_time, units = 'secs'), 2), ' sec)', '\n', sep='')
})

