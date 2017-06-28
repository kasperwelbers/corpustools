library(corpustools)
corpustools:::sourceall()
tc = create_tcorpus(sotu_texts, text_columns = 'text', doc_column = 'id', split_sentences = T)
tc$preprocess('word', new_column='feature', language='english', lowercase = T, use_stemming = T, remove_punctuation = T)

tc$feature_subset('feature', subset = docfreq_filter('feature', min = 10))

g = tc$semnet('feature')
g = backbone_filter(g, alpha=0.05, max_vertices = 100, use_original_alpha = T)
plot_semnet(g)

g = tc$semnet_window('feature', window.size = 20)
g = backbone_filter(g, alpha=0.05, max_vertices = 100, use_original_alpha = T)
plot_semnet(g)



