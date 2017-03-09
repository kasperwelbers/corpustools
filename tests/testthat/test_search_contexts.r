test_that("Query document search works", {
  library(tcorpus)
  text = c('Renewable fuel is better than fossil fuels!',
           'A fueled debate about fuel',
           'Mark Rutte is simply Rutte. Bos, on the other hand, is not always Wouter',
           'Hey, A ~ symbol!! Can I match that?')
  tc = create_tcorpus(text, doc_id = c('a','b','c','d'), split_sentences = T)

  hits = tc$search_contexts('mark AND rutte')
  expect_equal(as.character(hits$doc_id), 'c')

  hits = tc$search_contexts('"mark rutte"', context_level = 'sentence')
  expect_equal(hits$sent_i, 1)

  ## test context boundaries
  hits = tc$search_contexts('"rutte bos"~5', context_level = 'document') ## should find rutte and bos across sentences
  expect_true(!is.null(hits))

  get_feature_regex('"rutte bos"~5')
  hits = tc$search_contexts('"rutte bos"~5', context_level = 'sentence') ## should not find rutte and bos across sentences
  expect_true(is.null(hits))

  ## proximity search
  hits = tc$search_contexts('"bos wouter"~5', context_level = 'sentence') # bos and wouter within a word distance of 5
  expect_true(is.null(hits))

  hits = tc$search_contexts('"bos wouter"~10', context_level = 'sentence') #  bos and wouter within a word distance of 10
  expect_true(!is.null(hits))

  hits = tc$search_contexts('wouter NOT "bos wouter"~10', context_level = 'sentence') # wouter should not occur within 10 words from bos
  expect_true(is.null(hits))

  hits = tc$search_contexts('wouter NOT "bos wouter"~3', context_level = 'sentence') # wouter should not occur within 10 words from bos
  expect_true(nrow(hits) == 1)

  ## BOOLEAN
  hits = tc$search_contexts('wouter AND bos')
  expect_true(nrow(hits) == 1)
  hits = tc$search_contexts('wouter NOT bos')
  expect_true(is.null(hits))
  hits = tc$search_contexts('wouter NOT (bos OR banaan)') # neither bos nor banaan may occur
  expect_true(is.null(hits))
  hits = tc$search_contexts('wouter NOT (bos AND banaan)') # bos and banaan may not occur together
  expect_true(nrow(hits) == 1)

  ## case sensitive
  hits = tc$search_contexts('bos~s')
  expect_true(is.null(hits))
  hits = tc$search_contexts('Bos~s')
  expect_true(nrow(hits) == 1)
  hits = tc$search_contexts('"wouter bos"~10s') ## if flag on quotes, all within quotes needs to be case sensitive
  expect_true(is.null(hits))
  hits = tc$search_contexts('"Wouter Bos"~s10')
  expect_true(nrow(hits) == 1)
  expect_equal(tc$search_contexts('"Wouter Bos"~10s'), tc$search_contexts('"Wouter Bos"~s10')) ## order of flags is irrelevant

  ## escaping special characters
  hits = tc$search_contexts('\\~')
  expect_equal(as.character(hits$doc_id), 'd')
  hits = tc$search_features('\\?')
  expect_equal(as.character(hits$doc_id), 'd')

  ## query subsetting
  tc_rutte = tc$subset_query('"mark rutte"~2', context_level = 'sentence')
  expect_equal(tc_rutte$meta('doc_id'), 'c')
})
