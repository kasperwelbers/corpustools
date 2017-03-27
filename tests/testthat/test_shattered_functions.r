#test_that("shattered tcorpus functions", {
  #library(corpustools)
  #data(sotu_texts)

  #tc = create_tcorpus(sotu_texts, 'text', doc_column = 'id')
  #stc = shatter_tcorpus(tc, 'test', meta_columns=c('party', 'president'), tokens_per_shard=10000, if_exists = 'overwrite')


  #doc_hits = search_contexts(stc, 'us')
  #expect_equal(nrow(doc_hits), 192)

  #token_hits = search_features(stc, 'us')
  #expect_equal(nrow(token_hits), 243)

  #set_feature_index(stc)
#})
