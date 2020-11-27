function() {
  txt = 'Donald Trump has said that he will leave the White House when the electoral college votes for Democratic president-elect Joe Biden in the closest the outgoing president has come to conceding defeat.

Biden won the presidential election with 306 electoral college votes – many more than the 270 required – to Trump’s 232. Biden also leads Trump by more than 6 million in the popular vote tally.
Can Trump actually stage a coup and stay in office for a second term?
Read more

Trump has so far defied tradition by refusing to concede defeat, instead making a series of baseless claims about alleged ballot fraud and launching legal attempts to challenge the outcomes in several states such as Pennsylvania and Michigan.

But desperate efforts by Trump and his supporters to overturn results in key states, either by lawsuits or by pressuring state legislators, have failed.

Speaking to reporters on the Thanksgiving holiday, Trump said if Biden – who is due to be sworn in on 20 January – was certified the election winner by the electoral college, he would depart the White House.'
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
}




