library(corpustools)

tc = create_tcorpus(sotu_texts, text_columns = 'text', doc_column = 'id', split_sentences = T)
tc$preprocess('word', 'feature', lowercase = T, use_stemming = T, language='english', remove_stopwords = T)
tc$data

## document similarity network
g = tc$compare_documents(feature = 'feature', weight = 'norm_tfidf', min_similarity = 0.3, measure='cosine')
g

library(RNewsflow)
g_agg = network.aggregate(g, by='president')
directed.network.plot(g_agg, vertex.size = 40, edge.label.cex = 1)


## deduplicate
tc_dedup = tc$deduplicate('word', print_duplicates = T, copy=T)
tc$print(111552227)

