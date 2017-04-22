test_that("tokenlist visualization", {
  ## Given a data frame with tokens
  library(corpustools)
  data("sotu_texts")
  tc = create_tcorpus(sotu_texts, doc_column = 'id')
  tc$subset(subset_meta = doc_id %in% head(doc_id))

  tokens = tc$data
  meta = tc$meta

  tokens
  meta

  highlight = nchar(as.character(tokens$word))
  html = html_tagged_text(tokens, meta = meta, highlight = highlight)

  url = wrap_html(cat(html))
  #browseURL(url)
})
