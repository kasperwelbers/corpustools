test_that("Query document search works", {
  library(tcorpus)
  text = c('Renewable fuel is better than fossil fuels!',
           'A fueled debate about fuel',
           'Mark Rutte is simply Rutte. Bos, on the other hand, is not always Wouter')
  tc = create_tcorpus(text, doc_id = c('a','b','c'), split_sentences = T)

  hits = tc$search_contexts('mark AND rutte')
  expect_equal(as.character(hits$doc_id), 'c')

  hits = tc$search_contexts('"mark rutte"', context_level = 'sentence')
  expect_equal(hits$sent_i, 1)

  ## test context boundaries
  hits = tc$search_contexts('"rutte bos"~5', context_level = 'document') ## should find rutte and bos across sentences
  expect_true(!is.null(hits))

  hits = tc$search_contexts('"rutte bos"~5', context_level = 'sentence') ## should not find rutte and bos across sentences
  expect_true(is.null(hits))

  ## proximity search
  hits = tc$search_contexts('"bos wouter"~5', context_level = 'sentence') # bos and wouter within a word distance of 5
  expect_true(is.null(hits))

  hits = tc$search_contexts('"bos wouter"~10', context_level = 'sentence') #  bos and wouter within a word distance of 10
  expect_true(!is.null(hits))

  hits = tc$search_contexts('"!bos wouter"~10', context_level = 'sentence') # wouter should not occur within 10 words from bos
  expect_true(is.null(hits))

  ## proximity with additional or statements
  hits = tc$search_contexts('"(bos test) wouter"~10', context_level = 'sentence') # finds "bos wouter"~10 OR "test wouter"~10 (but more efficiently than entering these terms manually)
  expect_true(!is.null(hits))

  hits = tc$search_contexts('"(!bos test) wouter"~10', context_level = 'sentence') # finds "(NOT bos) wouter"~10 OR "test wouter"~10
  expect_true(is.null(hits))

  hits = tc$search_contexts('"(bos !test) wouter"~10', context_level = 'sentence') # finds "bos wouter"~10 OR "(NOT test) wouter"~10
  expect_true(!is.null(hits))

  ## query subsetting
  tc_rutte = tc$subset_query('"mark rutte"~2', context_level = 'sentence')
  expect_equal(tc_rutte$meta('doc_id'), 'c')
})
