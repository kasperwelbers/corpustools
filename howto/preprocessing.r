library(corpustools)

colnames(sotu_texts)
head(sotu_texts)

tc = create_tcorpus(sotu_texts, text_columns = 'text', doc_column = 'id', split_sentences = T)

tc$data
tc$meta

tc$subset(word_i %in% 1:5)

tc$data[1:100,]

## preprocess a character vector containing words
word = tc$data$word
feature = preprocess_words(word, language='english', lowercase = T, use_stemming = T, remove_punctuation = T)
head(feature)

## preprocess and add as column in token data
tc$preprocess('word', new_column='feature', language='english', lowercase = T, use_stemming = T, remove_punctuation = T)
tc$data

tc$preprocess('word', new_column='trigrams', language='english', lowercase = T, use_stemming = T, ngrams = 3)
tc$data

## note that the punctuation is NA. NA values will be considered 'deleted' (e.g., when exporting to DTM).
## you can easily delete these rows if you need to free up memory, but keeping them has the advantage of being able to
## reconstruct the original texts
