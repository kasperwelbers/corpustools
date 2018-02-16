source('lib.r')

sidebar <- shinydashboard::dashboardSidebar(width = 300,
  customStyle(),

  shinydashboard::sidebarMenu(
    ################################ READ DATA ####################################
    h3('Read data'),
    shiny::radioButtons('datatype', '', list('Text'='text', 'Tokens'='tokens', 'Environment'='env'), 'text', inline = T),

    textInputUI(condition = "input.datatype == 'text'"),
    tokensInputUI(condition = "input.datatype == 'tokens'"),
    envInputUI(condition = "input.datatype == 'env'"),

    br(),
    loadDataButton(),

    br(), hr(),
    ############################# CORPUS PARAMETERS ################################
    h3('Preprocess'),
    textCorpusParametersUI('input.datatype == "text"'),
    tokensCorpusParametersUI('input.datatype == "tokens"'),
    generalCorpusParametersUI(),
    preprocessButton()
  )
)



## add codebook box
body <- shinydashboard::dashboardBody(
  shiny::fluidRow(
    shinydashboard::tabBox(width = 5,
      shiny::tabPanel(title = 'Meta',
               DT::dataTableOutput('meta')
      ),
      shiny::tabPanel(title = 'Tokens',
               DT::dataTableOutput('tokens')
      ),
      shiny::tabPanel(title = 'Features',
               shiny::htmlOutput('summary'),
               DT::dataTableOutput('termstats')
      ),
      shiny::tabPanel(title = 'Action',
               shiny::radioButtons('actions', 'Action:', list('Wordcloud'='wordcloud', 'Semantic network'='semnet', 'Compare corpora'='compare'), selected='wordcloud', inline=T),
               shiny::br(),
               plotWordcloudUI(condition = "input.actions == 'wordcloud'"),
               plotSemnetUI(condition = "input.actions == 'semnet'"),
               plotCompareUI(condition = "input.actions == 'compare'"),
               shiny::br(),
               shiny::actionButton('plotaction', 'Perform action')
      )
    ),
    shinydashboard::tabBox(width=7,
      shiny::tabPanel(title = "Plot", solidHeader = TRUE,
               shiny::htmlOutput('actionmessage'),
               plotParametersUI(),
               plotOutput('actionplot', width = '95%', height='95%')
      )
    )
  )
)

shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = '', titleWidth = 300),
  sidebar,
  body
)



