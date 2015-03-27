require(shiny)
require(randomForest)
require(ggplot2)

shinyUI(bootstrapPage(fluidPage(
  titlePanel("RandomForest"),
  #headerPanel("Hello Shiny!"),
  sidebarLayout(
    sidebarPanel(
      fileInput('train_data', 'Choose CSV File',accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
      #    tags$hr(),
      checkboxInput(inputId = 'header',label = 'header',value = TRUE),
      radioButtons(inputId = 'separator',label = 'sep',choices = c(Comma=',',Semicolon=';',Tab='\t'),','),
      #    radioButtons(inputId = 'quote',label = 'Quote',choices = c(None='','Double Quote'='"','Single Quote'="'"),'"'),
      tags$hr(),

      selectInput(inputId = 'type',label = 'type',choices = c('Classification','Regression')),

      tags$hr(),

#      checkboxInput(inputId='test_set', label='Test set exist?', value = FALSE),

#      tags$div('Test set exist?',style='color:blue'),
      strong('Test set exist?'),

      fileInput('test_data', 'Choose CSV File',accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),

      tags$hr(),

      numericInput(inputId = "ntree", "Number of trees to grow", 100,min = 1, max = 2000),

      tags$hr(),

      selectInput(inputId = 'replace',label = 'Replace',choices = c(TRUE,FALSE),selected = FALSE),

      tags$hr(),

      numericInput(inputId = "sampsize", "Size(s) of sample to draw", 10,min = 1, max = 2000),

      tags$hr(),

      numericInput("nodesize", "Minimum size of terminal nodes", 10,min = 1, max = 2000),

      tags$hr(),

      numericInput("maxnodes", "Maximum number of terminal nodes trees in the forest can have", 10,min = 1, max = 2000),

      tags$hr(),

      selectInput(inputId = 'importance',label = 'Should importance of predictors be assessed?',choices = c(TRUE,FALSE),selected = FALSE),

      tags$hr(),

      numericInput("nPerm", "Number of times the OOB data are permuted per tree for assessing variable importance", 1,min = 1, max = 20),

      tags$hr(),

      selectInput(inputId = 'proximity',label = 'Should proximity measure among the rows be calculated?',choices = c(TRUE,FALSE),selected = FALSE),

      tags$hr(),

      selectInput(inputId = 'oob.prox',label = 'Should proximity be calculated only on "out-of-bag" data?',choices = c(TRUE,FALSE),selected = FALSE),

      tags$hr(),

      selectInput(inputId = 'keep.forest',label = 'Should forest be retained in the output object?',choices = c(TRUE,FALSE),selected = TRUE),

      tags$hr(),

      #submitButton("Update View", icon = icon("refresh"))
      actionButton("quit", "Stop")
    ),
    mainPanel(
      #tableOutput('contents')
      tabsetPanel(id = "tabs",
                  tabPanel("train data", dataTableOutput('contents')),
                  tabPanel("model call", verbatimTextOutput("summary"))
                  #tabPanel("model type", renderPrint(""))
      )
    )


  )
)
)
)
