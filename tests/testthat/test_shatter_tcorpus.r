test_that("shatter tcorpus works", {
  library(tcorpus)
  data(sotu_texts)

  first = create_tcorpus(sotu_texts[1:500,], 'text', doc_column = 'id')
  more = create_tcorpus(sotu_texts[501:808,], 'text', doc_column = 'id')
  more_with_duplicates = create_tcorpus(sotu_texts[700:1090,], 'text', doc_column = 'id')

  capture_output({
    tc_index = shatter_tcorpus(first, 'test', meta_columns=c('party', 'president'), tokens_per_shard=10000, if_exists = 'overwrite')
    tc_index = shatter_tcorpus(more, 'test', meta_columns=c('party', 'president'), tokens_per_shard=10000, if_exists = 'append')
    tc_index = shatter_tcorpus(more_with_duplicates, 'test', meta_columns=c('party', 'president'), tokens_per_shard=10000, if_exists = 'append')
  })

  info = tcorpus:::get_info(tc_index)
  expect_equal(info$n, 90825)
  expect_equal(info$n_meta, 1090)
  expect_equal(info$n_shards, 12)

  redistribute_shards(tc_index, tokens_per_shard=20000)
  info = tcorpus:::get_info(tc_index)
  ushards = tcorpus:::get_shards(tc_index, full.names = F)
  data_n = regmatches(ushards, gregexpr('(?<=_T=)[0-9]+', ushards, perl = T))
  meta_n = regmatches(ushards, gregexpr('(?<=_M=)[0-9]+', ushards, perl = T))
  data_n = sum(as.numeric(data_n))
  meta_n = sum(as.numeric(meta_n))
  expect_equal(info$n, 90825)
  expect_equal(info$n_meta, 1090)
  expect_equal(info$n_shards, 6)
})

