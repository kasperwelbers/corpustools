test_that("shatter tcorpus works", {
  library(tcorpus)
  data(sotu_texts)

  first = create_tcorpus(sotu_texts[1:500,], 'text', doc_column = 'id')
  more = create_tcorpus(sotu_texts[501:808,], 'text', doc_column = 'id')
  more_with_duplicates = create_tcorpus(sotu_texts[700:1090,], 'text', doc_column = 'id')

  capture_output({ ## ignores print verbose
    stc = shatter_tcorpus(first, 'test', meta_columns=c('party', 'president'), tokens_per_shard=10000, if_exists = 'overwrite')
    stc = shatter_tcorpus(more, 'test', meta_columns=c('party', 'president'), tokens_per_shard=10000, if_exists = 'append')
    stc = shatter_tcorpus(more_with_duplicates, 'test', meta_columns=c('party', 'president'), tokens_per_shard=10000, if_exists = 'append', if_duplicates = 'skip')
  })

  info = stc$info()
  expect_equal(info$n, 90825)
  expect_equal(info$n_meta, 1090)
  expect_equal(info$n_shards, 12)

  capture_output({
    redistribute_shards(stc, tokens_per_shard=20000)
  })
  info = stc$info()
  ushards = stc$shards(normalize = F)
  data_n = regmatches(ushards, gregexpr('(?<=_T=)[0-9]+', ushards, perl = T))
  meta_n = regmatches(ushards, gregexpr('(?<=_M=)[0-9]+', ushards, perl = T))
  data_n = sum(as.numeric(data_n))
  meta_n = sum(as.numeric(meta_n))
  expect_equal(info$n, 90825)
  expect_equal(info$n_meta, 1090)
  expect_equal(info$n_shards, 6)

  ## test renaming duplicates (in this case the wrong thing to do because they are actual duplicates)
  capture_output({ ## ignores print verbose
    stc = shatter_tcorpus(more, 'test', meta_columns=c('party', 'president'), tokens_per_shard=10000, if_exists = 'overwrite')
    stc = shatter_tcorpus(more_with_duplicates, 'test', meta_columns=c('party', 'president'), tokens_per_shard=10000, if_exists = 'append', if_duplicates = 'rename')
  })

  unlink('test.tCorpus', recursive = T) ## remove the tCorpus directory (happens automatically in test_that it seems?)
})

