library(corpustools)
tc = create_tcorpus(sotu_texts, text_columns = 'text', doc_column = 'id', split_sentences = T)


tc$search_features('nuclear')                             ## single term
tc$search_features('"nuclear weapons"')                   ## phrase
tc$search_features('nuclear weapon?')                     ## wildcards
tc$search_features('nuclear', condition = 'weapon?')      ## only find 'nuclear', but only if 'weapon?' is also mentioned
tc$search_features('nuclear', condition = 'weapon?^5')    ## condition within 5 words
tc$search_features('nuclear', condition = 'weapon?>5')    ## condition within 5 words after keyword

## keywords can only be words, phrases, or word co-occurences within a given word distance. OR statements possible as well.
## conditions can be complex boolean queries

tc$search_features('"(nuclear OR atomic) weapon?"~10', condition = '* NOT (energy)')
tc$search_features('"(nuclear OR atomic) weapon?"~10', condition = 'energy')


## output is actually a list (S3 class).
hits = tc$search_features('"(nuclear OR atomic) weapon?"~10', condition = 'energy')
hits$hits

## KWIC
tc$kwic(hits=hits)


