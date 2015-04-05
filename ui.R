shinyUI(bootstrapPage(fluidPage(
  titlePanel("RandomForest"),
  # aceEditor("myEditor", value = '#R Scripts', mode="r",height = '200px',fontSize = 15,theme="ambiance"),
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",tags$div("Loading...",id="loadmessage")),
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        div(align="center",fileInput(inputId = 'train_data', label = tags$div('train set *',style='color:blue'),accept=c('text/csv','text/comma-separated-values,text/plain','.csv'))),
        div(align="center",fileInput(inputId = 'test_data', label = tags$div('test set *',style='color:blue'), accept=c('text/csv','text/comma-separated-values,text/plain','.csv'))),
        div(align="center",checkboxInput("file_param","Show FileReader parameters",FALSE)),
        conditionalPanel(condition = "input.file_param==true",
                         checkboxInput(inputId = 'header',label = 'header',value = TRUE),
                         radioButtons(inputId = 'separator',label = 'sep',choices = c(Comma=',',Semicolon=';',Tab='\t'),',')
        )
      ),

      wellPanel(
        div(align="center",checkboxInput("randomForest_param","Show randomForest parameters",FALSE)),
        conditionalPanel(condition = "input.randomForest_param==true",
                         div(align="center",selectInput(inputId = 'type',label = tags$div('type',style='color:blue'),choices = c('Classification','Regression'))),
                         div(align="center",numericInput(inputId = "ntree", label = tags$div('ntree',style='color:blue'), NA)),
                         div(align="center",numericInput(inputId = "mtry", label =tags$div('mtry',style='color:blue') , NA)),
                         div(align="center",selectInput(inputId = 'replace',label = tags$div('replace',style='color:blue'),choices = c('YES','NO'),selected = 'YES')),
                         div(align="center",numericInput(inputId = "sampsize", label =tags$div('sampsize',style='color:blue') , NA)),
                         div(align="center",selectInput(inputId = 'importance',label = tags$div('importance',style='color:blue'),choices = c('YES','NO'),selected = 'YES')),
                         div(align="center",selectInput(inputId = 'proximity',label =tags$div('proximity',style='color:blue') ,choices = c('YES','NO'),selected = 'YES')),
                         div(align="center",selectInput(inputId = 'oob.prox',label = tags$div('oob.prox',style='color:blue'),choices = c('YES','NO'),selected = 'YES')),
                         div(align="center",selectInput(inputId = 'keep.forest',label = tags$div('keep.forest',style='color:blue'),choices = c('YES','NO'),selected = 'YES'))

        )
      ),

      wellPanel(div(align="center",checkboxInput("graph_save_param","Show Graph Save Options",FALSE)),
                div(align="center",conditionalPanel(condition = "input.graph_save_param==true",
                                                    radioButtons(inputId = "paramdown",label = "",choices=list("JPG"="jpg","PNG"="png","PDF"="pdf"),selected="jpg")
                ))
      ),

      div(align="center",actionButton(inputId = "quit", label = div(align="center",'Quit the app',style='color:blue')))



    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("train set", dataTableOutput('train_data')),
                  tabPanel("test set", dataTableOutput("test_data")),
                  tabPanel("model summary",
                           wellPanel(
                             div(align="center",textInput(inputId = 'title',label = 'title',value = 'title')),
                             div(align="center",textInput(inputId = 'author',label = 'author',value = 'author')),
                             tags$br(),
                             div(align="center",radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),inline = TRUE)),
                             tags$br(),
                             div(align="center",downloadButton(outputId = 'downloadReport',label = 'Download Report'))
                           ),
                           tags$br(),
                           verbatimTextOutput("names"),
                           verbatimTextOutput("summary"),
                           tags$br(),
                           tags$br(),
                           p(downloadButton("downloadTree","Download the summary"),align="center")
                  ),
                  tabPanel("getTree",
                           div(align="center",numericInput(inputId = "getTree", label =tags$div('getTree',style='color:blue') , NA)),
                           tags$br(),
                           tags$br(),
                           verbatimTextOutput("getTree"),
                           tags$br(),
                           tags$br(),
                           p(downloadButton("downloadRandomForest","Download the summary"),align="center")
                  ),
                  tabPanel('plots',plotOutput("marginPlot"),
                           tags$br(),
                           tags$br(),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='jpg'",
                             p(downloadButton("downloadData16","Download as jpg"),align="center"))),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='png'",
                             p(downloadButton("downloadData17","Download as png"),align="center"))),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='pdf'",
                             p(downloadButton("downloadData18","Download as pdf"),align="center"))),
                           tags$br(),
                           tags$br(),
                           plotOutput("randomForest"),
                           tags$br(),
                           tags$br(),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='jpg'",
                             p(downloadButton("downloadData19","Download as jpg"),align="center"))),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='png'",
                             p(downloadButton("downloadData20","Download as png"),align="center"))),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='pdf'",
                             p(downloadButton("downloadData21","Download as pdf"),align="center"))),
                           tags$br(),
                           tags$br(),
                           plotOutput("pr_curve"),
                           tags$br(),
                           tags$br(),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='jpg'",
                             p(downloadButton("downloadData1","Download as jpg"),align="center"))),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='png'",
                             p(downloadButton("downloadData2","Download as png"),align="center"))),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='pdf'",
                             p(downloadButton("downloadData3","Download as pdf"),align="center"))),
                           tags$br(),
                           tags$br(),
                           plotOutput("Cumulative_curve"),
                           tags$br(),
                           tags$br(),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='jpg'",
                             p(downloadButton("downloadData4","Download as jpg"),align="center"))),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='png'",
                             p(downloadButton("downloadData5","Download as png"),align="center"))),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='pdf'",
                             p(downloadButton("downloadData6","Download as pdf"),align="center"))),
                           tags$br(),
                           tags$br(),
                           plotOutput("roc_curve"),
                           tags$br(),
                           tags$br(),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='jpg'",
                             p(downloadButton("downloadData7","Download as jpg"),align="center"))),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='png'",
                             p(downloadButton("downloadData8","Download as png"),align="center"))),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='pdf'",
                             p(downloadButton("downloadData9","Download as pdf"),align="center"))),
                           tags$br(),
                           tags$br(),
                           plotOutput("lift_curve"),
                           tags$br(),
                           tags$br(),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='jpg'",
                             p(downloadButton("downloadData10","Download as jpg"),align="center"))),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='png'",
                             p(downloadButton("downloadData11","Download as png"),align="center"))),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='pdf'",
                             p(downloadButton("downloadData12","Download as pdf"),align="center"))),
                           tags$br(),
                           tags$br(),
                           plotOutput("grid"),
                           tags$br(),
                           tags$br(),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='jpg'",
                             p(downloadButton("downloadData13","Download as jpg"),align="center"))),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='png'",
                             p(downloadButton("downloadData14","Download as png"),align="center"))),
                           div(align="center",conditionalPanel(
                             condition="input.paramdown=='pdf'",
                             p(downloadButton("downloadData15","Download as pdf"),align="center")))

                  ),
                  tabPanel("About",
                           strong('randomForest with Shiny'),
                           p("The goal of this project is to help students and researchers run randomForest analysis as easily as possible."),
                           p('This application is developed with',
                             a("Shiny.", href="http://www.rstudio.com/shiny/", target="_blank"),
                             ''),
                           p('The code for this application is available at this',
                             a('GitHub.', href='https://github.com/guanlongtianzi/shinyOfRandomForest', target="_blank")),
                           br(),

                           strong('List of Packages Used'), br(),

                           aceEditor("myEditor1", value = '#R Scripts \nrequire(shiny)\nrequire(randomForest)\nrequire(ggplot2)\nrequire(shinyAce)\nrequire(rmarkdown)', mode="r",height = '200px',fontSize = 15,theme="ambiance"),
                           br(),
                           strong('Authors'),
                           HTML('<div style="clear: left;"><img src="my.jpg" alt="" style="float: left; margin-right:5px" /></div>'),
                           br(),
                           br(),
                           br(),
                           wellPanel(
                             tags$div('R session info',style='color:blue'),
                             verbatimTextOutput("info1.out")
                           )
                  )
      )
    )
  )
)
)
)
