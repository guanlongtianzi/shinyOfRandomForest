options(shiny.maxRequestSize=30*1024^2)
require(shiny)
require(randomForest)
#require(ggplot2)

if(getRversion() >= "2.15.1") utils::globalVariables(c('train_data', 'test_data', 'rand_model'))

shinyServer(function(input, output) {

  q <- observe({
    # Stop the app when the quit button is clicked
    if (input$quit == 1) stopApp()
  })

#    train_data <- reactive({
#      if(!is.null(input$train_data))
#        {
#          as.data.frame(read.csv(file = train_data$datapath,header = input$header,sep = input$separator,quote = input$quote))
#        }
#      })

#  train_data <-reactive({
#    read.csv(file = train_data$datapath,header = input$header,sep = input$separator,quote = input$quote)
#  })

  type <- reactive({
    input$type
  })

  test_data <- reactive({
    if(!is.null(test_data)){
      as.data.frame(read.csv(file = test_data$datapath,header = input$header,sep = input$separator,quote = input$quote))
    }
    else{
      NULL
    }
  })

  ntree <- reactive({
    input$ntree
  })

  replace <- reactive({
    input$replace
  })

  sampsize <- reactive({
    input$sampsize
  })

  nodesize <- reactive({
    input$nodesize
  })

  maxnodes <- reactive({
    input$maxnodes
  })


  importance <- reactive({
    input$importance
  })


  nPerm <- reactive({
    input$nPerm
  })


  proximity <- reactive({
    input$proximity
  })

  oob.prox <- reactive({
    input$oob.prox
  })

  keep.forest <- reactive({
    input$keep.forest
  })

#  rand_model <- reactive({
    #  randomForest(x=train_data[,1:ncol(train_data)-1],y=train_data[,ncol(train_data)-1:ncol(train_data)],xtest = test_data[,1:ncol(test_data)-1],ytest = test_data[,ncol(test_data)-1:ncol(test_data)],ntree = ntree,mtry = mtry,replace = replace,sampsize = sampsize,nodesize = nodesize,maxnodes = maxnodes,importance = importance,nPerm = nPerm,proximity = proximity,oob.prox = oob.prox,keep.inbag = keep.inbag)
    #   randomForest(x=train_data[,1:ncol(train_data)-1],y=train_data[,ncol(train_data)-1:ncol(train_data)],ntree = 10)
#    randomForest(x=train_data[,1:14],y=train_data[,15],ntree = 10)
#  })



  output$contents <- renderDataTable({
    train_data <- input$train_data
    if(is.null(train_data)){
      return(NULL)
    }
    else{
      read.csv(file = train_data$datapath,header = input$header,sep = input$separator,quote = input$quote)
    }
  }
  )

  output$summary <- renderPrint({
    train_data <- input$train_data
    if(is.null(train_data)){
      return(NULL)
    }
    else{
      train_data <- as.data.frame(read.csv(file = train_data$datapath,header = T))
    }
#    x <- as.data.frame(train_data)[,c('dengji','jiguan','chengshi')]
#    y <- as.data.frame(train_data)[,c('yuqi')]

    rand_model <- randomForest(x=train_data[,1:14],y=train_data[,15],ntree = 10)
#    rand_model$call

    #as.character(rand_model$call)
    rand_model$predicted

  })

}
)

