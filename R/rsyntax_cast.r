function() {
  txt = 'Donald Trump has said that he will leave the White House when the electoral college votes for Democratic president-elect Joe Biden in the closest the outgoing president has come to conceding defeat.

Biden won the presidential election with 306 electoral college votes – many more than the 270 required – to Trump\'s 232. Biden also leads Trump by more than 6 million in the popular vote tally.
Can Trump actually stage a coup and stay in office for a second term?
Read more

Trump has so far defied tradition by refusing to concede defeat, instead making a series of baseless claims about alleged ballot fraud and launching legal attempts to challenge the outcomes in several states such as Pennsylvania and Michigan.

But desperate efforts by Trump and his supporters to overturn results in key states, either by lawsuits or by pressuring state legislators, have failed.

Speaking to reporters on the Thanksgiving holiday, Trump said if Biden – who is due to be sworn in on 20 January – was certified the election winner by the electoral college, he would depart the White House.'
  
  #t=1:24
  #plot(t, 100/(1 + exp(5-t)), type='l')
  
  test = udpipe_tcorpus(txt) %>%
    #tc_plot_tree(token,lemma,POS) %>%
    transform_rsyntax(udpipe_simplify)
  test$udpipe_clauses()
  tc_plot_tree(test, token,lemma,POS, tree_parent, tree_relation,annotation='clause', sentence_i=4)
  
  tc_plot_tree(test, token,lemma,POS, annotation='clause', sentence_i=7)
  
  
  d = readr::read_csv('~/projects/ln_old/hong_data/1_The Guardian (London)_2011-06-01_2011-07-01.csv')
  tc = udpipe_tcorpus(paste(d$headline, d$text, sep='.\n\n'), cores=6)
  tc$udpipe_quotes()
  tc$udpipe_coref()
  tc_syntax_reader(tc, annotation='quote', 'source')
  tc_plot_tree(tc, token, lemma, POS, sentence_i=3)
  
  tc_plot_tree(tc, doc_id='1', sentence=67, token, lemma,POS)
  tc$tokens[1580:1630,]
  
  tc$tokens[180:230]
  tc$tokens[tc$tokens$quote_id == 'dir#1.9.203',]
  
  
  tc2 = transform_rsyntax(tc, udpipe_simplify)
  tc_plot_tree(tc2, token,lemma,POS,tree_relation, tree_parent)
  tc2$tokens
  
  tc = udpipe_tcorpus("President Trump said to Biden that he lost")
  tc$udpipe_quotes()
  tc_plot_tree(tc, annotation='quote',token,lemma,POS,quote)
  
  tc_syntax_reader(tc)
  tc_plot_tree(tc, token, lemma, POS, sentence_i=8)
  
  tc$udpipe_quotes()
  tc_syntax_reader(tc, 'quote', 'source', 'quote')
  
  tc$udpipe_coref()
  browse_texts(tc, category = 'coref_id')
  tc$tokens
  
  tc2 = transform_rsyntax(tc, udpipe_simplify, new_sentences=F)
  tc2$udpipe_clauses()
  tc_plot_tree(tc2, token, lemma, POS, annotation='clause', sentence_i=6)
  tc_syntax_reader(tc2, 'clause', 'subject')
  
  
  
  tc_plot_tree(tc, token, lemma, POS, coref_txt)
  tc_plot_tree(tc2, token, lemma, POS, coref_txt, tree_relation, sentence_i=2)
  ## obl, parataxis?
  
  ?rsyntax::add_span_quotes()
  
  txt = c('Bob said "hi I\'m Bob" to Mary')
  tc = udpipe_tcorpus(txt, coref=T, cores=4)
  tc$udpipe_quotes()
  tc$udpipe_coref()
  tc_plot_tree(tc, token, lemma, POS, annotation='quote')
  
  txt = c('Bob said "hi I\'m Bob" to Mary. "What a fine day, right? I think it is"')
  tc = udpipe_tcorpus(txt, coref=T, cores=4)
  tc$tokens
  tc$udpipe_quotes()
  tc$udpipe_coref()
  browse_texts(tc, highlight='quote')
  
  tc$tokens
  tc_syntax_reader(tc)
  tc$tokens[1:50,]
  
  browse_texts(tc, category = 'coref_id')
  
  tc$tokens[50:100,]
  
  tc_plot_tree(udpipe_tcorpus("Donald Trump and his friends did not concede", coref=T), token,lemma,POS)
  
  tc = udpipe_tcorpus(txt, coref=T, cores=4)
  tc$tokens[1:10,]
  tc$annotate_quotes()
  tc$annotate_clauses()

  tc$annotate_quotes()  
  tc_syntax_reader(tc, annotation='quote', value='source')
  
  tc2 = transform_rsyntax(tc, udpipe_simplify, rm_punct=T)
  tc_plot_tree(tc, sentence_i=1)
  tc_plot_tree(tc2, sentence_i=1)
  
  ## remember to add tree_parent and tree_relation to conjunction function in rsyntax
  ## also make an add_node function, and give some presets for things like "is", "does"
  
  txt = 'Het bestuur kwam gistermiddag met een verkapte vredesverklaring aan Baudet. Het stelt voor om een algemene ledenvergadering te houden en daar de leden te laten stemmen of Baudet lid mag blijven. Ook zou er dan een volledig nieuw bestuur worden gekozen, waarvoor het oude bestuur zich niet zou kandideren.

Nog voordat Baudet kon zeggen wat hij daarvan dacht, stapten senatoren Nanninga en Pouw-Verweij op, net als kandidaat-Tweede Kamerleden Eerdmans en Vlaardingerbroek. Ook in de provinciale fractie van Flevoland stapten gisteravond vijf van de zes leden op, in Overijssel vier van de vijf.
'
  txt = c('hij zei tegen haar dat ze tegen het beest moest zeggen dat het zich moet gedragen. Dat vinden zij')

  tc = udpipe_tcorpus(txt, coref=T, cores=4, model='dutch-alpino')
  tc$udpipe_coref()
  browse_texts(tc, category = 'coref_id')
  
  tc = udpipe_tcorpus(txt, coref=T, cores=4, model='dutch-lassysmall')
  tc_plot_tree(tc, token,lemma,POS)
  tc$udpipe_coref()
  browse_texts(tc, category = 'coref_id')
  tc_syntax_reader(tc)
  View(tc$tokens)
  tc_plot_tree(tc,sentence_i=2, token,lemma,POS)
}






