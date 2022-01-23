testthat::context('Semnet')

test_that("Positions in remember_spaces are correct", {
  sotu_texts$doc_id = as.character(sotu_texts$id)
  tc = create_tcorpus(sotu_texts, c('president','text'), remember_spaces = T)
  
  d = untokenize(tc)
  d = merge(d, sotu_texts, by='doc_id')
  
  expect_equal(d$president.x, d$president.y)
  expect_equal(d$text.x, d$text.y)
})
