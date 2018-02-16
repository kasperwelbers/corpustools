source('lib.r')


shinyServer(function(input, output, session) {
  session$onSessionEnded(function() {
    .TCORPUS = NULL
  })

  ## update input parameters after selecting a file, using the column names to provide the options
  observeEvent(input$text,
               updateTextInputParameters(session, input, try_defaults = T))
  observeEvent(input$tokens,
               updateTokenInputParameters(session, input, try_defaults = T))
  observeEvent(input$meta,
               updateMetaInputParameters(session, input, try_defaults = T))


  ## load data
  observeEvent(input$load,
               loadData(session, input, output))

  observeEvent(input$preprocess,
               preprocessTokens(session, input, output))


  ## By using recordplot() (for R device plots) within a reactive, plotting parameters can be modified without having to reload the plot
  observe({
    p = actionPlot()
    input$plotres ## does not trigger observe with renderPlot call
    output$actionplot = renderPlot(p, res= input$plotres * (input$plotzoom/100),
                                      width=input$plotwidth * (input$plotzoom/100),
                                      height=input$plotheight * (input$plotzoom/100))
  })

  getActiondata <- reactive({
    actiondata = list()
    if(input$actions == 'wordcloud') actiondata = c(actiondata, prepareWordcloud(input))
    if(input$actions == 'semnet') actiondata = c(actiondata, prepareSemnet(input))
    if(input$actions == 'compare') actiondata = c(actiondata, prepareCompare(input))

    message = if('message' %in% names(actiondata)) actiondata$message else ''
    output$actionmessage = renderText(message)
    actiondata
  })

  actionPlot <- eventReactive(input$plotaction, {
    actiondata = getActiondata()
    par(mar=c(0,0,0,0))
    if(input$actions == 'wordcloud') plotWordcloud(actiondata)
    if(input$actions == 'semnet') plotSemnet(actiondata)
    if(input$actions == 'compare') plotCompare(actiondata)
    return(recordPlot())
  })

  actionRawData <- eventReactive(input$plotaction, {
    actiondata = getActiondata()
    if(input$actions == 'wordcloud') return(actiondata$termfreq)
    if(input$actions == 'semnet') return(get.data.frame(actiondata$g, 'edges'))
    if(input$actions == 'compare') return(actiondata$compare)
  })
})


