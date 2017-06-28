library(corpustools)
tc = create_tcorpus(sotu_texts, text_columns = 'text', doc_column = 'id', split_sentences = T)
tc$preprocess('word', new_column='feature', language='english', lowercase = T, use_stemming = T, remove_punctuation = T)

comp = tc$compare_subset('feature', subset_meta_x = president == 'Barack Obama')
head(comp)
plot(comp)

