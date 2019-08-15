testthat::context('Search Contexts')


test_that("Query document search works", {
  text = c('Renewable fuel is better than fossil fuels!',
           'A fueled debate about fuel',
           'Mark Rutte is simply Rutte. Bos, on the other hand, is not always Wouter',
           'Hey, A ~ symbol!! Can I match that?')
  tc = create_tcorpus(text, doc_id = c('a','b','c','d'), split_sentences = T)

  hits = search_features(tc, '!')

  hits = search_contexts(tc, 'mark AND rutte')
  expect_equal(as.character(hits$hits$doc_id), 'c')

  hits = search_contexts(tc, '"mark rutte"', context_level = 'sentence')
  expect_equal(hits$hits$sentence, 1)

  ## test context boundaries
  hits = search_contexts(tc, '"rutte bos"~5', context_level = 'document') ## should find rutte and bos across sentences
  expect_true(nrow(hits$hits) > 0)

  hits = search_contexts(tc, '"rutte bos"~5', context_level = 'sentence') ## should not find rutte and bos across sentences
  expect_true(nrow(hits$hits) == 0)

  ## proximity search
  hits = search_contexts(tc, '"bos wouter"~5', context_level = 'sentence') # bos and wouter within a token distance of 5
  expect_true(nrow(hits$hits) == 0)

  hits = search_contexts(tc, '"bos wouter"~10', context_level = 'sentence') #  bos and wouter within a token distance of 10
  expect_true(nrow(hits$hits) > 0)

  hits = search_contexts(tc, 'wouter NOT "bos wouter"~10', context_level = 'sentence') # wouter should not occur within 10 tokens from bos
  expect_true(nrow(hits$hits) == 0)

  hits = search_contexts(tc, 'wouter NOT "bos wouter"~3', context_level = 'sentence') # wouter should not occur within 10 tokens from bos
  expect_true(nrow(hits$hits) == 1)

  ## BOOLEAN
  hits = search_contexts(tc, 'wouter AND bos')
  expect_true(nrow(hits$hits) == 1)
  hits = search_contexts(tc, 'wouter NOT bos')
  expect_true(nrow(hits$hits) == 0)
  hits = search_contexts(tc, 'wouter NOT (bos OR banaan)') # neither bos nor banaan may occur
  expect_true(nrow(hits$hits) == 0)
  hits = search_contexts(tc, 'wouter NOT (bos AND banaan)') # bos and banaan may not occur together
  expect_true(nrow(hits$hits) == 1)

  ## case sensitive
  hits = search_contexts(tc, 'bos~s')
  expect_true(nrow(hits$hits) == 0)
  hits = search_contexts(tc, 'Bos~s')
  expect_true(nrow(hits$hits) == 1)

  tc = create_tcorpus(text, doc_id = c('a','b','c','d'), split_sentences = T)
  hits = search_contexts(tc, '"wouter bos"~10s') ## if flag on quotes, all within quotes needs to be case sensitive

  expect_true(nrow(hits$hits) == 0)
  hits = search_contexts(tc, '"Wouter Bos"~s10')
  expect_true(nrow(hits$hits) == 1)
  expect_equal(search_contexts(tc, '"Wouter Bos"~10s')$hits, search_contexts(tc, '"Wouter Bos"~s10')$hits) ## order of flags is irrelevant

  ## using special characters (other than ?, * or ~)
  hits = search_features(tc, '!')
  expect_equal(as.character(hits$hits$doc_id), c('a','d','d'))

  ## query subsetting
  tc = create_tcorpus(text, doc_id = c('a','b','c','d'), split_sentences = T)
  tc_rutte = tc$subset_query('"mark rutte"~2', context_level = 'sentence')
  expect_equal(tc_rutte$get_meta('doc_id'), 'c')
})

