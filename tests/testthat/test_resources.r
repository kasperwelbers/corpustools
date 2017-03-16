test_jrcnames <- function(tc){
  capture_output({
    download_resource('jrc_names')
    tc = tc$jrc_names(batchsize = 9999999, low_memory = F)
    d = tc$data()
  })
  expect_equal(as.character(na.omit(d$jrc_names)), c('1510','2042','76099', '76099'))
  expect_equal(as.character(na.omit(d$jrc_names_l)), c('Barack Obama','Donald Trump', 'Mark Rutte', 'Mark Rutte'))
}

test_that("resources works", {
  ### remember that the actual test (test_jrcnames) is often commented out, since it takes a bit long

  library(tcorpus)
  tokens = data.frame(document = c(rep(1, 8), rep(2, 5), rep(3, 5)),
                      sentence = c(rep(1, 8), rep(2, 5), rep(3, 5)),
                      id = 1:18,
                      word = c('Renewable','Barack_Obama','is','better','than','fossil','Donald_Trump','?','A','fueled','debate','about','fuel','Mark','Rutte','is','simply','Rutte'))
  tc = tokens_to_tcorpus(tokens, doc_col ='document', word_i_col = 'id')

  ## if wanted; disable tests for specific resources, because its not ideal to keep downloading them all the time
  set_resources_path('~/Downloads') ## store locally for repeated use (since building the package removes the resources directory)
  #test_jrcnames(tc)
})

