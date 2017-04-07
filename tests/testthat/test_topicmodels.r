test_that("topicmodels works", {
  library(corpustools)
  data(sotu_texts)

  corpustools:::sourceall()

  tc = create_tcorpus(sotu_texts[1:500,], 'text', doc_column = 'id')
  tc = tc$preprocess('word', 'feature', lowercase = T, remove_punctuation = T, remove_stopwords = T, use_stemming = T, language = 'english')
  tc = tc$feature_subset('feature', subset = freq_filter(feature, min=10) & docfreq_filter(feature, max=0.5*tc$n))

  m = tc$lda_fit(feature = 'feature', create_feature = 'lda', K=5)

  #library(reshape2)


  #output = createTopicBrowser(m, tokens$lemma, tokens$aid, words=tokens$word, meta=meta)
  #$clusterinfo(m)

  #expect_true('lda' %in% tc$feature_names)
})

