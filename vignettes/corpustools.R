## ---- include=FALSE------------------------------------------------------
options(digits=3)
library(knitr)

## ------------------------------------------------------------------------
library(corpustools)

## ---- eval=F-------------------------------------------------------------
#  ?tcorpus

## ------------------------------------------------------------------------
colnames(sotu_texts)

## ------------------------------------------------------------------------
tc = create_tcorpus(sotu_texts, doc_column = 'id', text_columns = 'text')

## ------------------------------------------------------------------------
tc

## ------------------------------------------------------------------------
## first 6 rows (head) of tokens and meta data.tables
head(tc$tokens)   
head(tc$meta)

## ------------------------------------------------------------------------
tc = tokens_to_tcorpus(corenlp_tokens, doc_col = 'doc_id',
                       sentence_col = 'sentence', token_id_col = 'id')
tc

## ------------------------------------------------------------------------
tc = create_tcorpus('This is an example')

tc$set('new_column', "any value")
tc$tokens

## ------------------------------------------------------------------------
tc$set('new_column', toupper(token))

## ------------------------------------------------------------------------
tc$set('new_column', tolower(token), token_id <= 2)

## ------------------------------------------------------------------------
tc$set_name('new_column', 'token2')

## ------------------------------------------------------------------------
tc$delete_columns('token2')

## ------------------------------------------------------------------------
tc = create_tcorpus(sotu_texts, doc_col='id', split_sentences = T)
tc2 = subset(tc, sentence==1)

## number of tokens in full corpus
tc$n
## number of tokens in first sentence
tc2$n

## ------------------------------------------------------------------------
tc2 = subset(tc, subset_meta = president == 'Barack Obama')

## number of tokens in president Obama documents
tc2$n

## ------------------------------------------------------------------------
tc$subset(sentence == 1, subset_meta = president == 'Barack Obama')
tc$n

## ---- message=F----------------------------------------------------------
d = data.frame(doc_id = paste('doc', 1:3),
               text = c('I am a text','I am also text', 'Ich bin ein Berliner'),
               date = as.POSIXct(c('2010-01-01','2010-01-02','2010-01-03')))

# document 1 and 2 are duplicates, and the first is deleted
tc = create_tcorpus(d)
tc$deduplicate(feature='token', date_col = 'date', similarity = 0.75)
tc$meta$doc_id     

# document 1 and 2 are duplicates, and the last is deleted
tc = create_tcorpus(d)
tc$deduplicate(feature='token', date_col = 'date', similarity = 0.75, keep = 'last')
tc$meta$doc_id

## ------------------------------------------------------------------------
tc = create_tcorpus(sotu_texts, doc_column = 'id', text_columns = 'text')

tc$preprocess(use_stemming = T, remove_stopwords=T)
tc$tokens

## ---- eval=F-------------------------------------------------------------
#  tc = create_tcorpus("This is a quick example", udpipe_model='english-ewt')
#  tc$tokens   ## output not shown in this vignette

## ---- eval=F-------------------------------------------------------------
#  tc$feats_to_columns(c('Tense','Number','Person'))

## ---- eval=F-------------------------------------------------------------
#  ## for sake of clarity, recreate the corpus and preprocess it
#  tc = create_tcorpus(sotu_texts, doc_column = 'id', text_columns = 'text')
#  tc$preprocess(use_stemming = T, remove_stopwords=T)
#  
#  tc$feature_subset('feature', !token_id == 5)

## ---- eval=F-------------------------------------------------------------
#  tc$feature_subset('feature', min_freq=10)

## ------------------------------------------------------------------------
tc = create_tcorpus(sotu_texts, doc_col='id', split_sentences=T)

tc$preprocess(column = 'token', new_column = 'feature',
              remove_stopwords=T, use_stemming=T, min_docfreq = 5)

dfm = get_dfm(tc, 'feature')
dfm

## ------------------------------------------------------------------------
##  Use sentences as rows instead of documents
dfm_sent = get_dfm(tc, 'feature', context_level = 'sentence')

##  Use a weighting scheme. 
dfm_weighted = get_dfm(tc, 'feature', weight = 'tfidf')

##  Only use a subset of the tCorpus
dfm_obama = get_dfm(tc, 'feature', subset_meta = president == "Barack Obama")

## ------------------------------------------------------------------------
## create tcorpus and preprocess tokens
tc = create_tcorpus(sotu_texts, doc_column = 'id', text_columns = 'text')
tc$preprocess(use_stemming = T, remove_stopwords=T, min_docfreq = 5)

## fit lda model, using the create_feature argument to store the topic assignments
m = tc$lda_fit('feature', create_feature = 'topic', K = 5, alpha = 0.001)

## ------------------------------------------------------------------------
head(tc$tokens, 10)

## ---- eval=F-------------------------------------------------------------
#  url = browse_texts(tc, category='topic', view=T)

## ---- eval=F-------------------------------------------------------------
#  ?search_features()

## ------------------------------------------------------------------------
tc = create_tcorpus(sotu_texts, doc_column = 'id', text_columns = 'text', split_sentences=T)

## ------------------------------------------------------------------------
hits = search_features(tc, query = c('terror*','war*'))

## ------------------------------------------------------------------------
hits
summary(hits)

## ------------------------------------------------------------------------
hits = search_features(tc, query = c('Terror# terror*','War# war*'))
summary(hits)

## ------------------------------------------------------------------------
hits$queries
head(hits$hits)

## ------------------------------------------------------------------------
queries = data.frame(label = c('War','Terrorism','Economy','Education'),
                     query = c('war* OR army OR bomb*', 'terroris*', 'econom* OR bank*', 'educat* OR school*'))
hits = search_features(tc, query=queries$query, code=queries$label)

## ------------------------------------------------------------------------
count_tcorpus(tc, hits=hits)
count_tcorpus(tc, hits=hits, meta_cols = 'president')  ## break by meta variables

## ---- fig.width = 6, fig.height = 3, fig.align="center", eval=F----------
#  library(ggplot2)
#  
#  date_hits = count_tcorpus(tc, hits, meta_cols='date', wide = F)
#  
#  ggplot(date_hits, aes(x=date, y=count, group=code)) +
#    geom_line(aes(linetype=code))
#  
#  pres_hits = count_tcorpus(tc, hits, meta_cols='president', wide = F)
#  
#  ggplot(pres_hits, aes(president, count)) +
#    geom_col(aes(fill=code), width=.5, position = "dodge")

## ---- fig.width = 6, fig.height = 3, fig.align="center"------------------
g = semnet(hits, measure = 'con_prob')

## ------------------------------------------------------------------------
igraph::get.adjacency(g, attr = 'weight')

## ---- fig.width = 7, fig.height = 4, fig.align="center"------------------
plot(hits)

## ---- eval=F-------------------------------------------------------------
#  url = browse_hits(tc, hits, view=T)

## ------------------------------------------------------------------------
get_kwic(tc, query = 'freedom* AND america*', n = 2)

## ------------------------------------------------------------------------
## example query that matches first words in corpus
q = 'Example# unfinished OR restore OR "basic bargain"' 
tc$code_features(q)
head(tc$tokens, 10)

## ------------------------------------------------------------------------
tc = create_tcorpus(sotu_texts, doc_column = 'id', text_columns = 'text', split_sentences=T)

con = search_contexts(tc, 'war')
con

## ------------------------------------------------------------------------
tc = create_tcorpus(sotu_texts, doc_column = 'id', text_columns = 'text', split_sentences=T)
tc_war = subset_query(tc, 'war')

## ------------------------------------------------------------------------
tc$subset_query('war')

## ---- eval=F-------------------------------------------------------------
#  library(quanteda)
#  dict = quanteda::data_dictionary_LSD2015

## ---- eval=F-------------------------------------------------------------
#  tc = create_tcorpus(sotu_texts, doc_column = 'id', text_columns = 'text', split_sentences=T)
#  hits = search_dictionary(tc, dict)

## ---- eval=F-------------------------------------------------------------
#  library(ggplot2)
#  agg_hits = count_tcorpus(tc, 'date', hits, wide = F)
#  ggplot(agg_hits, aes(x=date, y=count, group=code)) +
#    geom_line(aes(linetype=code))

## ---- eval=F-------------------------------------------------------------
#  dict = melt_quanteda_dict(dict)
#  dict$sentiment = ifelse(dict$code %in% c('positive','neg_negative'), 1, -1)
#  tc$code_dictionary(dict)
#  tc$tokens

## ---- eval=F-------------------------------------------------------------
#  browse_texts(tc, scale='sentiment')

## ---- eval=F-------------------------------------------------------------
#  agg_tcorpus(tc, sent = mean(sentiment), N = length(sentiment), .id = 'code_id')

## ---- eval=F-------------------------------------------------------------
#  agg_tcorpus(tc, sent = mean(sentiment), .id = 'code_id', by='president')
#  agg_tcorpus(tc, sent = mean(sentiment), .id = 'code_id', by=c('token','president'))

## ------------------------------------------------------------------------
head(emoticon_dict)

## ------------------------------------------------------------------------
tc = create_tcorpus('yay :) :* happy')
tc$tokens

## ------------------------------------------------------------------------
tc$replace_dictionary(emoticon_dict)
tc$tokens

## ------------------------------------------------------------------------
tc = create_tcorpus(sotu_texts, doc_col='id')
tc$preprocess(min_docfreq = 20, remove_stopwords = T, remove_numbers = T)

g = semnet_window(tc, 'feature')

## ---- fig.width = 8, fig.height = 6, fig.align="center"------------------
gb = backbone_filter(g, alpha = 0.001, max_vertices = 100)
plot_semnet(gb)

## ------------------------------------------------------------------------
comp = compare_subset(tc, feature='feature',
                      subset_meta_x = president == "Barack Obama")

## ---- fig.width = 6, fig.height = 4, fig.align="center"------------------
plot(comp)

## ------------------------------------------------------------------------
tc = create_tcorpus(sotu_texts, doc_col='id')
tc$preprocess(use_stemming = T, remove_stopwords = T, min_docfreq = 10)

fa = feature_associations(tc, 'feature', query = 'terror*')

## ------------------------------------------------------------------------
head(fa)

## ---- fig.width = 6, fig.height = 6, fig.align="center"------------------
plot(fa, col=c('lightblue','navyblue'))

## ---- eval=F-------------------------------------------------------------
#  tc$subset()

## ------------------------------------------------------------------------
tc = create_tcorpus('this is an example')
tc$subset(token_id < 3)
tc$tokens

## ------------------------------------------------------------------------
tc = create_tcorpus('this is an example')
tc2 = tc$subset(token_id < 3)

## tc and tc2 are both subsets with token_id < 3
identical(tc$tokens, tc2$tokens)

## ------------------------------------------------------------------------
tc = create_tcorpus('this is an example')
tc2 = tc
tc2$subset(token_id < 3)

## tc and tc2 are both subsets with token_id < 3
identical(tc$tokens, tc2$tokens)

## ------------------------------------------------------------------------
tc2 = subset(tc, token_id < 2)

## ------------------------------------------------------------------------
tc2 = tc$subset(token_id < 2, copy=T)

## ------------------------------------------------------------------------
tc2 = tc         ## shallow copy
tc3 = tc$copy()  ## deep copy

