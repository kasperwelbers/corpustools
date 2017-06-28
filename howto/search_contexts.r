library(corpustools)

tc = create_tcorpus(sotu_texts, text_columns = 'text', doc_column = 'id', split_sentences = T)

tc$search_contexts('economy')                                    ## single term
tc$search_contexts('"the economy"')                              ## phrase
tc$search_contexts('econom*')                                    ## wildcards
tc$search_contexts('econom* AND grow*')                          ## AND
tc$search_contexts('econom* AND (grow* OR rise OR rising)')      ## parentheses
tc$search_contexts('"econom* (grow* OR rise OR rising)"~10')     ## word distance
tc$search_contexts('God~s')                                      ## case sensitive

## output is actually a list (S3 class).
hits = tc$search_contexts('"america first"~10')
hits
hits$hits

## subset by query
us1 = tc$subset_query('"america first"~10', copy = T)
us1

