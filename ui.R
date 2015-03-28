#require(shiny)
#require(randomForest)
#require(ggplot2)

#######################################################################################################################################
#load packages
#######################################################################################################################################
if('randomForest' %in% installed.packages()){
  require(package = 'randomForest',quietly = TRUE)
}else{
  cat('未安装"randomForest package",后台安装程序已启动')
  install.packages(pkgs = 'randomForest',quiet = TRUE)
  require(package = 'randomForest',quietly = TRUE)
  cat('"randomForest package"安装成功')
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if('ggplot2' %in% installed.packages()){
  require(package = 'ggplot2',quietly = TRUE)
} else{
  cat('未安装"ggplot2 package",后台安装程序已启动')
  install.packages(pkgs = 'ggplot2',quiet = TRUE)
  require(package = 'ggplot2',quietly = TRUE)
  cat('"ggplot2 package"安装成功')
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if('shiny' %in% installed.packages()){
  require(package = 'shiny',quietly = TRUE)
}else{
  cat('未安装"shiny package",后台安装程序已启动')
  install.packages(pkgs = 'shiny',quiet = TRUE)
  require(package = 'shiny',quietly = TRUE)
  cat('"shiny package"安装成功')
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

shinyUI(bootstrapPage(fluidPage(
  titlePanel("RandomForest"),
  #headerPanel("Hello Shiny!"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = 'train_data', label = tags$div('train set *',style='color:blue'),accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),

      fileInput(inputId = 'test_data', label = tags$div('test set *',style='color:blue'), accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),

      checkboxInput(inputId = 'header',label = 'header',value = TRUE),

radioButtons(inputId = 'separator',label = 'sep',choices = c(Comma=',',Semicolon=';',Tab='\t'),','),

      selectInput(inputId = 'type',label = tags$div('type',style='color:blue'),choices = c('Classification','Regression')),

      tags$hr(),

      numericInput(inputId = "ntree", label = tags$div('ntree',style='color:blue'), NA),

      helpText(tags$div('Number of trees to grow',style='color:red')),

      tags$hr(),

      numericInput(inputId = "mtry", label =tags$div('mtry',style='color:blue') , NA),

      helpText(tags$div('Number of variables randomly sampled as candidates at each split',style='color:red')),

      tags$hr(),

      selectInput(inputId = 'replace',label = tags$div('replace',style='color:blue'),choices = c('YES','NO'),selected = 'YES'),

      helpText(tags$div('Should sampling of cases be done with or without replacement?',style='color:red')),

      tags$hr(),

      numericInput(inputId = "sampsize", label =tags$div('sampsize',style='color:blue') , NA),

      helpText(tags$div('Size(s) of sample to draw',style='color:red')),

      tags$hr(),

#      numericInput(inputId = "nodesize", label = tags$div('nodesize',style='color:blue'), NA),

#      helpText(tags$div('Minimum size of terminal nodes',style='color:red')),

#      tags$hr(),

#      numericInput(inputId = "maxnodes", label = tags$div('maxnodes',style='color:blue'), NA),

#      helpText(tags$div('Maximum number of terminal nodes trees in the forest can have',style='color:red')),

#      tags$hr(),

      selectInput(inputId = 'importance',label = tags$div('importance',style='color:blue'),choices = c('YES','NO'),selected = 'YES'),

      helpText(tags$div('Should importance of predictors be assessed?',style='color:red')),

      tags$hr(),

#      numericInput(inputId = "nPerm", label = tags$div('nPerm',style='color:blue'), NA),

#      helpText(tags$div('Number of times the OOB data are permuted per tree for assessing variable importance',style='color:red')),

#      tags$hr(),

      selectInput(inputId = 'proximity',label =tags$div('proximity',style='color:blue') ,choices = c('YES','NO'),selected = 'YES'),

      helpText(tags$div('Should proximity measure among the rows be calculated?',style='color:red')),

      tags$hr(),

      selectInput(inputId = 'oob.prox',label = tags$div('oob.prox',style='color:blue'),choices = c('YES','NO'),selected = 'YES'),

      helpText(tags$div('Should proximity be calculated only on "out-of-bag" data?',style='color:red')),

      tags$hr(),

      selectInput(inputId = 'keep.forest',label = tags$div('keep.forest',style='color:blue'),choices = c('YES','NO'),selected = 'YES'),

      helpText(tags$div('Should forest be retained in the output object?',style='color:red')),

      tags$hr(),

      numericInput(inputId = "getTree", label =tags$div('getTree',style='color:blue') , NA),

      helpText(tags$div('Extract a single tree from a forest',style='color:red')),

      tags$hr(),

      actionButton(inputId = "quit", label = tags$div('Stop',style='color:blue')),

      helpText(tags$div('Press Quit to exit the application',style='color:red'))
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("train set", dataTableOutput('train_data')),
                  tabPanel("test set", dataTableOutput("test_data")),
                  tabPanel("model summary", verbatimTextOutput("summary")),
                  tabPanel("getTree", verbatimTextOutput("getTree")),
                  tabPanel("margin plot", plotOutput("marginPlot")),
                  tabPanel("randomForest plot", plotOutput("randomForest"))
      )
    )


  )
)
)
)
