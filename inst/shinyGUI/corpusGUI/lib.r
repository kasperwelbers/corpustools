library(shiny)
library(shinydashboard)
library(DT)

customStyle <- function(sidebarheight='200vh', inputcontainer_height='65px'){
  list(tags$head(tags$style(HTML(  ## add scroll to sidebar (because conditinalpanels mess up the height)
    sprintf(".sidebar {height: %s; overflow-y: auto;}", sidebarheight)
  ))),
  tags$head(tags$style(HTML(  ## reduce height of input containers
    sprintf('.shiny-input-container {height:%s;}', inputcontainer_height)
  ))),
  tags$head(tags$style(HTML(
    sprintf(".shiny-plot-output {overflow: scroll;}", sidebarheight)
  )))
  )
}

warningmessage <- function(x) sprintf('<span style=\"color:red\"><h3>%s</h3></span>', x)

## iterates through default_candidates to see if they occur in choices, and selects the candidate if it does.
## breaks after match, so preferred defaults should be put first
selectInputDefault <- function(session, inputID, choices, default_candidates){
  for(default in default_candidates){
    if(default %in% choices) {
      updateSelectInput(session, inputID, selected = default)
      break
    }}}

## similar to updateSelectDefault(), but if multiple candidates match the choices, they are all selected (in the given order)
selectInputDefaultMulti <- function(session, inputID, choices, default_candidates){
  defaults = c()
  for(default in default_candidates){
    if(default %in% choices) defaults = c(defaults, default)
  }
  if(length(defaults) > 0) updateSelectInput(session, inputID, selected = defaults)
}



#############################################################################################################
############################################### text input functions ########################################
## ui: textInputUI
## server: updatetextInputParameters; loadtextInput


updateTextInputParameters <- function(session, input, try_defaults=T){
  cols = colnames(read.csv(input$text$datapath, nrows=1))
  if(cols[1] == 'X') cols = cols[2:length(cols)] # read.csv has the nasty habbit of not recognizing rownames and then labeling them X

  updateSelectInput(session, 'text_docid_col', choices = cols)
  updateSelectInput(session, 'text_text_col', choices = cols)
  updateSelectInput(session, 'text_meta_col', choices = cols, selected = cols)

  if(try_defaults){
    selectInputDefault(session, 'text_docid_col', cols, c('doc_id', 'docid', 'a.id', 'aid', 'id'))
    selectInputDefaultMulti(session, 'text_text_col', cols, c('headline','byline','lead','abstract','text'))
  }
}

## function that adds user interface elements if the condition is TRUE.
textInputUI <- function(condition='true'){
  conditionalPanel(condition = condition,
                   fileInput('text', label = "Upload a csv file with text and meta", accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                   selectInput('text_docid_col', label = 'Document id column', choices=c(), multiple = F),
                   selectInput('text_text_col', label = 'Text column(s)', choices=c(), multiple = T),
                   selectInput('text_meta_col', label = 'meta columns', choices=c(), multiple = T)
  )
}

############################################################################################################
########################################## token input functions ###########################################
## ui: tokensInputUI
## server: updateTokensInputParameters; loadTokensInput

updateTokenInputParameters <- function(session, input, try_defaults=T){
  cols = colnames(read.csv(input$tokens$datapath, nrows=1))
  if(cols[1] == 'X') cols = cols[2:length(cols)] # read.csv has the nasty habbit of not recognizing rownames and then labeling them X


  updateSelectInput(session, 'tokens_docid_col', choices = cols)
  updateSelectInput(session, 'tokens_sentence_col', choices = c(cols, '[not used]')) # optional
  updateSelectInput(session, 'tokens_tokenid_col', choices = c(cols, '[not used]')) # optional
  updateSelectInput(session, 'tokens_token_col', choices = cols)
  updateSelectInput(session, 'tokens_lemma_col', choices = c(cols, '[not used]')) # optional
  updateSelectInput(session, 'tokens_pos_col', choices = c(cols, '[not used]')) # optional
  updateSelectInput(session, 'tokens_parent_col', choices = c(cols, '[not used]')) # optional
  updateSelectInput(session, 'tokens_relation_col', choices = c(cols, '[not used]')) # optional


  if(try_defaults){
    selectInputDefault(session, 'tokens_docid_col', cols, c('doc_id', 'docid', 'a.id', 'aid'))
    selectInputDefault(session, 'tokens_sentence_col', c(cols, '[not used]'), c('sentence','sentence_id', '[not used]'))
    selectInputDefault(session, 'tokens_tokenid_col', c(cols, '[not used]'), c('position','word_id','token_id','id','[not used]'))
    selectInputDefault(session, 'tokens_token_col', cols, c('word','token'))
    selectInputDefault(session, 'tokens_lemma_col', c(cols, '[not used]'), c('lemma', '[not used]'))
    selectInputDefault(session, 'tokens_pos_col', c(cols, '[not used]'), c('pos1','pos', 'pos', '[not used]'))
    selectInputDefault(session, 'tokens_parent_col', c(cols, '[not used]'), c('parent', '[not used]'))
    selectInputDefault(session, 'tokens_relation_col', c(cols, '[not used]'), c('relation', '[not used]'))

  }
}

## function that adds user interface elements if the condition is TRUE.
tokensInputUI <- function(condition='true'){
  conditionalPanel(
    condition = condition,
    fileInput('tokens', label = "Upload a csv file with tokens", accept=c('text/csv', 'text/comma-separated-values,text/plain')),

    selectInput('tokens_docid_col', label = 'document id', choices=c(), multiple = F),
    selectInput('tokens_sentence_col', label = 'sentence', choices=c(), multiple = F),
    selectInput('tokens_tokenid_col', label = 'token id (position)', choices=c(), multiple = F),
    selectInput('tokens_token_col', label = 'word', choices=c(), multiple = F),
    selectInput('tokens_lemma_col', label = 'lemma', choices=c(), multiple = F),
    selectInput('tokens_pos_col', label = 'part-of-speech', choices=c(), multiple = F),
    selectInput('tokens_parent_col', label = 'parent', choices=c(), multiple = F),
    selectInput('tokens_relation_col', label = 'parent relation', choices=c(), multiple = F),
    br(),
    fileInput('meta', label = "Upload a csv file with meta", accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    selectInput('meta_docid_col', label = 'document.id', choices=c(), multiple = F),
    selectInput('meta_meta_col', label = 'meta columns', choices=c(), multiple = T)
  )
}


############################################################################################################
########################################## environment input functions #####################################
## ui: demoInputUI
## server: loadDemoInput

tcInEnv <- function() {
  tcnames = sapply(names(.GlobalEnv), function(x) is(.GlobalEnv[[x]], 'tCorpus'))
  tcnames = names(tcnames)[tcnames]
  if (length(tcnames) == 0) tcnames = 'No tCorpus available'
  as.list(tcnames)
}

## function that adds user interface elements if the condition is TRUE.
envInputUI <- function(condition='true'){
  conditionalPanel(
    condition = condition,
    selectInput('env_tc', label = 'tCorpus', choices=tcInEnv(), multiple = F)
  )
}

############################################################################################################
########################################## meta input functions ############################################
## ui: metaInputUI
## server: updateMetaInputParameters; loadMetaInput

updateMetaInputParameters <- function(session, input, try_defaults=T){
  cols = colnames(read.csv(input$meta$datapath, nrows=1))
  if(cols[1] == 'X') cols = cols[2:length(cols)] # read.csv has the nasty habbit of not recognizing rownames and then labeling them X

  updateSelectInput(session, 'meta_docid_col', choices = cols)
  updateSelectInput(session, 'meta_meta_col', choices = cols, selected = cols)

  if(try_defaults){
    selectInputDefault(session, 'meta_docid_col', cols, c('doc_id', 'docid', 'a.id', 'aid', 'id'))
  }
}


############################################################################################################
########################################## load data functions ############################################
## ui: loadDataButton
## server: loadData

loadDataButton <- function(){
  list(
    HTML('&nbsp;&nbsp;&nbsp;&nbsp;'),
    actionButton('load', 'load data', icon = icon('play-circle'), width = 200, style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  )
}

loadData <- function(session, input, output){
  if(is.null(input$text) && is.null(input$tokens) && is.null(input$env_tc)) return(NULL)

  if(input$datatype == 'text'){
    dm = readr::read_csv(input$text$datapath)
    dm = dm[,colnames(dm) %in% c(input$text_docid_col, input$text_text_col, input$text_meta_col)]
    .DATA$tc = corpustools::create_tcorpus(dm, doc_column = input$text_docid_col, text_columns = input$text_text_col, split_sentences=T)
  }

  if(input$datatype == 'tokens'){
    tokens = readr::read_csv(input$tokens$datapath)

    if (!is.null(input$meta)){
      meta = readr::read_csv(input$meta$datapath) ## meta is optional
      meta = meta[,unique(c(input$meta_docid_col, input$meta_meta_col)), drop=F]
      colnames(meta)[input$meta_docid_col] = input$tokens_docid_col
    } else meta = NULL

    .DATA$tc = corpustools::tokens_to_tcorpus(tokens, doc_col = input$tokens_docid_col, token_id_col = input$tokens_tokenid_col,
                                          sentence_col = input$tokens_sentence_col, meta=meta)

    if(!input$tokens_token_col == '[not used]') .DATA$tc$set_name(input$tokens_token_col, 'token')
    if(!input$tokens_lemma_col == '[not used]') .DATA$tc$set_name(input$tokens_lemma_col, 'lemma')
    if(!input$tokens_pos_col == '[not used]') .DATA$tc$set_name(input$tokens_pos_col, 'POS')
    if(!input$tokens_parent_col == '[not used]') .DATA$tc$set_name(input$tokens_parent_col, 'parent')
    if(!input$tokens_relation_col == '[not used]') .DATA$tc$set_name(input$tokens_relation_col, 'relation')
  }


  if (input$datatype == 'env') {
    if (is.null(input$env_tc)) return(NULL)
    if (input$env_tc == 'No tCorpus available') return(NULL)
    .DATA$tc = .GlobalEnv[[input$env_tc]]
  }

  .GlobalEnv[['.TCORPUS']] = .DATA$tc
  updateSelectInput(session, 'env_tc', choices = tcInEnv())
  updatePosFilter(session)
  updateDocfreqFilter(session)
  preprocessTokens(session, input, output)
}

calculateTokenStats <- function(){
  .DATA$tc$feature_stats('feature')
}

############################################################################################################
########################################## corpus parameters ###############################################
## ui: preprocessParametersUI
## server: updatePosFilter; updateDocfreqFilter

## ui's
preprocessParametersUI <- function(){
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(width=4,
                    selectInput('pp_input', 'Input feature', c('token','lemma'), multiple=F, selected='lemma'),
                    textInput('pp_output', 'Output feature', value = 'feature'),
                    selectInput('language', 'Language', names(corpustools::stopwords_list), multiple=F, selected='english'),
                    br(),
                    preprocessButton()
      ),
      shiny::column(width=4,
                    checkboxGroupInput('transforms', 'Transform',
                                list('lowercasing'='lowercase', 'stemming'='stem', 'remove accents'='as_ascii'),
                                selected=c('lowercase')),
                    br(),
                    sliderInput('ngrams', label='ngrams', value=1, min=1, max=4, step=1, ticks = F)
      ),
      shiny::column(width=4,
                    checkboxGroupInput('filters', 'Filter',
                                       list('stopwords'='stopwords', 'numbers'='numbers','punctuation'='punctuation'),
                                       selected=c('stopwords','numbers','punctuation')),
                    br(),
                    br(),
                    sliderInput('docfreq', label = 'document frequency', step=1, min = 1, max=1, value=c(1,1)),
                    br(),
                    sliderInput('wordlength', label = 'word length', step=1, min=1, max=100, value=c(1, 100)),
                    br(),
                    selectInput('posfilter', 'Part-of-speech filter', choices=c(), multiple = T)
      )
    ),
    br(),
    shiny::verbatimTextOutput('preprocess_syntax')
  )
}

## update filters
updatePosFilter <- function(session){
  updateSelectInput(session, 'posfilter', choices = c())
  print(.DATA$tc$names)
  if('POS' %in% .DATA$tc$names){
    postags = unique(.DATA$tc$get_levels('POS'))
    if(length(postags) > 100) {
      output$poswarning = renderText('<div style="color:#FF0000">Too many unique pos values</div>')
    } else {
      print(postags)
      updateSelectInput(session, 'posfilter', choices = postags, selected = postags)
    }
  }
}

updateDocfreqFilter <- function(session){
  n = .DATA$tc$n_meta
  updateSliderInput(session, 'docfreq', min=1, max=n, value=c(1,n))
}


############################################################################################################
########################################## prepare corpus ###############################################
## ui: preprocessButton
## server: preprocessTokens; createDTM

preprocessButton <- function(){
  list(
    HTML('&nbsp;&nbsp;&nbsp;&nbsp;'),
    actionButton('preprocess', 'preprocess', icon = icon('play-circle'), width = 200, style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  )

}

preprocessTokens <- function(session, input, output, d){
  if (is.null(.DATA$tc)) return(NULL)

  output$preprocess_syntax = renderText(preprocessSyntax(input))
  evalSyntax(preprocessSyntax(input))


  #min_docfreq = if (input$docfreq[1] > 1) input$docfreq[1] else NULL
  #max_docfreq = if (input$docfreq[2] < .DATA$tc$n_meta) input$docfreq[2] else NULL
  #min_char = if(input$wordlength[1] > 1) input$wordlength[1] else NULL
  #max_char = if(input$wordlength[2] < 100) input$wordlength[2] else NULL

  #.DATA$tc$preprocess(input$pp_input, input$pp_output, language=input$language, use_stemming='stem' %in% input$transforms, lowercase='lowercase' %in% input$transforms, as_ascii='as_ascii' %in% input$transforms,
  #                ngrams=input$ngrams, remove_punctuation='punctuation' %in% input$filters, remove_stopwords='stopwords' %in% input$filters,
  #                remove_numbers='numbers' %in% input$filters, min_docfreq=min_docfreq, max_docfreq=max_docfreq, min_char=input$wordlength[1], max_char=input$wordlength[2])
  updateGeneralOutput(output)

}

preprocessSyntax <- function(input) {
  printSyntax('preprocess',
               fline(
                 param('column', !input$pp_input == 'token', input$pp_input),
                 param('new_column', !input$pp_output == 'feature', input$pp_output),
                 param('language', !input$language == 'english', input$language)
               ),
               fline(
                 param('use_stemming', 'stem' %in% input$transforms, T),
                 param('lowercase', 'lowercase' %in% input$transforms, T),
                 param('as_ascii', 'as_ascii' %in% input$transforms, T),
                 param('ngrams', input$ngrams > 1, input$ngrams)
               ),
               fline(
                 param('remove_punctuation', 'punctuation' %in% input$filters, T),
                 param('remove_stopwords', 'stopwords' %in% input$filters, T),
                 param('remove_numbers', 'numbers' %in% input$filters, T)
               ),
               fline(
                 param('min_docfreq', input$docfreq[1] > 1, input$docfreq[1]),
                 param('max_docfreq', input$docfreq[2] < .DATA$tc$n_meta, input$docfreq[2]),
                 param('min_char', input$wordlength[1] > 1, input$wordlength[1]),
                 param('max_char', input$wordlength[2] < 100, input$wordlength[2])
               )
              )
}

createDTM <- function(input, tokens){
  if(is.null(.DATA$text) && is.null(tokens)) return(NULL)
  ## note that the tokens used here is the result of getTokens(), which gives the already filtered version of d$tokens.
  ## d$text can be used directly because there is no preprocessing involved (this all happens below in RTextTools::create_matrix)

  if(input$datatype == 'text'){
    #dtm = RTextTools::create_matrix(
    #  textColumns = d$text,
    #  language = input$language,
    #  minDocFreq = input$docfreq[1],
    #  maxDocFreq = input$docfreq[2],
    #  minWordLength = input$wordlength[1],
    #  maxWordLength = input$wordlength[2],
    #  removeNumbers = 'numbers' %in% input$filters,
    #  removePunctuation = 'punctuation' %in% input$filters,
    #  removeStopwords='stopwords' %in% input$filters,
    #  stemWords= 'stem' %in% input$filters,
    #  toLower= T)
  }
  if(input$datatype == 'tokens'){
    #dtm = corpustools::dtm.create(
    #  documents = tokens$doc_id,
    #  terms = tokens$text,
    #  freqs = rep(1, nrow(tokens)))
  }
  dtm
}

matchMeta <- function(input, dtm){
  if(is.null(dtm)) return(NULL)
  .DATA$meta[match(rownames(dtm), .DATA$meta$doc_id),]
}


############################################################################################################
########################################## ACTIONS ###############################################
## ui: plotWordcloudUI; plotSemnetUI; PlotCompareUI
## server:

wordcloudParameters <- function(){
  list(
    sliderInput('wordcloud_nterms', 'Number of terms', min=1, max=200, value=100),
    sliderInput('wordcloud_range', 'Wordsize range', min=0.1, max=15, value=c(0.5, 8))
  )
}

plotWordcloudUI <- function(condition='true'){
  conditionalPanel(
    condition = condition,
    h4('Create a wordcloud of the most frequent words in the DTM'),
    br(),
    wordcloudParameters()
  )
}

plotSemnetUI <- function(condition = 'true'){
  conditionalPanel(
    condition = condition,
    h4('Make a semantic network'),
    conditionalPanel(condition = 'input.datatype == "tokens" && input.tokens_position_col != "[not used]"',
                     checkboxInput('use_window', label = 'measure co-occurence within a given word distance', value = F)),
    conditionalPanel(condition = 'input.use_window == true && input.tokens_position_col != "[not used]"',
                     br(),
                     sliderInput('windowsize', label = 'Word distance window', min = 2, max=50, value=20)),

    sliderInput('semnet_nterms', 'Max number of terms', min = 1, max=200, value = 100),
    numericInput('semnet_alpha', 'Backbone extraction alpha (use 1 for no backbone extraction)', value = 0.001, min = 0.000000001, max=1),
    checkboxInput('semnet_clustering', 'Color clusters', value = T)
  )
}

## todo: server side of compareUI
plotCompareUI <- function(condition='true'){
  conditionalPanel(
    condition = condition,
    h4('Compare two corpora'),
    br(),
    selectInput('compare_class_col', 'Class column', choices = c()),
    selectInput('compare_date_col', 'Date column', choices = c()),
    radioButtons('compare_type', 'Comparison type', choices = c('corpus X to corpus Y', 'corpus X to all else'), inline = T),

    fluidPage(
      box(
        h5('Corpus X'),
        selectInput('compare_class_x', 'class', choices = c()),
        dateRangeInput('compare_date_x', 'date', start = '2000-01-01', end = '2100-01-01')
      ),
      conditionalPanel(condition = 'input.compare_type == "corpus X to corpus Y"',
                       box(
                         h5('Corpus Y'),
                         selectInput('compare_class_y', 'class', choices = c()),
                         dateRangeInput('compare_date_y', 'date', start = '2000-01-01', end = '2100-01-01')
                       )
      )
    ),
    h5('Wordcloud parameters'),
    wordcloudParameters()
  )
}


############################################################################################################
########################################## OUTPUT ###############################################
## ui:
## server: plotWordcloud; plotSemnet

prepareWordcloud <- function(input){
  if(is.null(.DATA$tc)) return(NULL)
  ts = .DATA$tc$feature_stats('feature')
  ts = head(ts[order(-ts$termfreq),], input$wordcloud_nterms)
  list(termfreq=data.frame(term=ts$term, freq=as.numeric(ts$termfreq)),
       wordcloud_range = input$wordcloud_range)
}

prepareSemnet <- function(input, dtm, tokens){
  if(is.null(dtm) && is.null(tokens)) return(NULL)
  termselect = col_sums(dtm)
  termselect = head(names(termselect[order(-termselect)]), 500) # only use top 500 terms to speed up computation

  if(input$use_window){
    tokens = tokens[tokens$text %in% termselect,]
    g = semnet::windowedCoOccurenceNetwork(tokens$position, tokens$text, tokens$doc_id, window.size = input$windowsize)
  } else {
    dtm = dtm[,termselect]
    g = semnet::coOccurenceNetwork(dtm)
  }

  g = semnet::getBackboneNetwork(g, alpha=input$semnet_alpha, max.vertices=input$semnet_nterms, use.original.alpha = T)
  if(ecount(g) == 0) return(list(g=NULL, message=warningmessage("No edges found (try increasing the alpha for the backbone extraction)")))

  V(g)$cluster = if(input$semnet_clustering) igraph::fastgreedy.community(as.undirected(g))$membership else 1
  g = semnet::setNetworkAttributes(g, V(g)$freq, V(g)$cluster)
  list(g=g)
}

#prepareCompare <- function(input, dtm, meta){
#  if(is.null(dtm) | is.null(meta)) return(NULL)
#  updateSelectInput(session, 'compare_class_col', choices = c(colnames(d$meta), '[No class filter]'))
#  updateSelectInput(session, 'compare_date_col', choices = c(colnames(d$meta), '[No date filter]'))
#}

plotWordcloud <- function(actiondata){
  if(is.null(actiondata$termfreq)) return(NULL)
  wordcloud::wordcloud(actiondata$termfreq$term, actiondata$termfreq$freq, scale = rev(actiondata$wordcloud_range),
            min.freq = 1, max.words = Inf, random.order = FALSE, rot.per = 0.15, colors = brewer.pal(6, "YlGnBu"))
}

plotSemnet <- function(actiondata){
  if(is.null(actiondata$g)) return(NULL)
  plot(actiondata$g)
}


############################################################################################################
########################################## OUTPUT parameters ###############################################
## ui: plotParametersUI
## server:
plotParametersUI <- function(){
  list(
    h4('Image size and resolution'),
    fluidPage(
      column(sliderInput('plotheight', 'height', step = 10, min = 300, max=2400, value = 1200, post='px'), width = 3),
      column(sliderInput('plotwidth', 'width', step = 10, min = 300, max=2400, value = 1200, post='px'), width=3),
      column(sliderInput('plotres', 'resolution', step = 1, min = 30, max=300, value = 150, post='dpi'), width = 3),
      column(sliderInput('plotzoom', 'zoom', step = 0.01, min = 0, max=200, value = 50, post='%'), width = 3)
    ),
    downloadLink('saveplot', label = 'Download plot')
  )
}


updateGeneralOutput <- function(output){
  output$meta <- renderDataTable({
    if (is.null(.DATA$tc)) NULL else .DATA$tc$get_meta(copy=F)
  }, options = list(autoWidth = F, scrollX=T, columnDefs = list(list(
    targets = '_all',
    render = JS("function(data, type, row, meta) {", "return type === 'display' && data.length > 20 ?",
                "'<span style=\"color:blue\" title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;", "}")
  ))), rownames=F, callback = JS('table.page(3).draw(false);')) ## abbreviate long texts and show full text on hover

  output$tokens <- renderDataTable({

    if (is.null(.DATA$tc)) NULL else .DATA$tc$get(copy=F)
  }, options = list(autoWidth = F, scrollX=T), rownames=F)

  output$summary <- renderText({
    if (is.null(.DATA$tc)) NULL else {
      sprintf('<h4>documents: %s</h4>
                 <h4>tokens: %s</h4>
                 <h4>terms : %s</h4>
                 <br>', .DATA$tc$n_meta, .DATA$tc$n,  length(.DATA$tc$get_levels('feature')))
    }
  })
  output$termstats <- renderDataTable({
    d = if (is.null(.DATA$tc)) NULL else .DATA$tc$feature_stats('feature')
    rownames(d) = NULL
    as.data.table(d[,c('term','characters','number','nonalpha','termfreq','docfreq')])
  }, options = list(autoWidth = F, scrollX=T), rownames=F)

}


###################################################################################
################################# PRINT SYNTAX ####################################
printSyntax <- function(f, ..., tc_name='tc') {
  l = unlist(list(...))
  offset = rep(' ', nchar(f) + nchar(tc_name) + 2)
  offset = paste(offset, collapse='')
  if (!is.null(l)) {
    l = paste(l, collapse= paste0(',\n', offset))
  } else l = ''
  paste0('tc$', f, '(', l, ')')
}

fline <- function(...) {
  l = unlist(list(...))
  if (is.null(l)) return(NULL)
  paste(l, collapse=', ')
}

param <- function(parameter, condition, value=T) {
  if (is(value, 'character')) value = paste0('"', value, '"')
  if (condition) paste0(parameter,' = ',value) else NULL
}


evalSyntax <- function(str){
  print(str)
  evalstring = paste0('.DATA$', str)
  print(evalstring)
  eval(parse(text=evalstring))
}
