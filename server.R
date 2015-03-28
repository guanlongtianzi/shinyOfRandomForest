options(shiny.maxRequestSize=30*1024^2)
#require(shiny)
#require(randomForest)
#require(ggplot2)

if(getRversion() >= "2.15.1") utils::globalVariables(c('train_data', 'test_data', 'rand_model'))
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

#######################################################################################################################################
#shiny server
#######################################################################################################################################
shinyServer(function(input, output) {

  q <- observe({
    # Stop the app when the quit button is clicked
    if (input$quit == 1) stopApp()
  })

  #  train_data  <<- reactive({
  #    if(is.null(input$train_data))
  #    {
  #      return(NULL)
  #    }
  #    else{
  #      as.data.frame(read.csv(file = train_data$datapath,header = input$header,sep = input$separator,quote = input$quote))
  #    }
  #  })

  #  test_data <<- reactive({
  #    if(is.null(test_data)){
  #      return(NULL)
  #    }
  #    else{
  #      as.data.frame(read.csv(file = test_data$datapath,header = input$header,sep = input$separator,quote = input$quote))
  #    }
  #  })

  #type <- reactive({
  #  input$type
  #})

  #ntree <- reactive({
  #  input$ntree
  #})

  #mtry <- reactive({
  #  input$mtry
  #})

  #replace <- reactive({
  #  input$replace
  #})

  #sampsize <- reactive({
  #  input$sampsize
  #})

  #nodesize <- reactive({
  #  input$nodesize
  #})

  #maxnodes <- reactive({
  #  input$maxnodes
  #})

  #importance <- reactive({
  #  input$importance
  #})

  #nPerm <- reactive({
  #  input$nPerm
  #})

  #proximity <- reactive({
  #  input$proximity
  #})

  #oob.prox <- reactive({
  #  input$oob.prox
  #})

  #keep.forest <- reactive({
  #  input$keep.forest
  #})

  #  rand_model <- reactive({
  #  randomForest(x=train_data[,1:ncol(train_data)-1],y=train_data[,ncol(train_data)-1:ncol(train_data)],xtest = test_data[,1:ncol(test_data)-1],ytest = test_data[,ncol(test_data)-1:ncol(test_data)],ntree = ntree,mtry = mtry,replace = replace,sampsize = sampsize,nodesize = nodesize,maxnodes = maxnodes,importance = importance,nPerm = nPerm,proximity = proximity,oob.prox = oob.prox,keep.inbag = keep.inbag)
  #   randomForest(x=train_data[,1:ncol(train_data)-1],y=train_data[,ncol(train_data)-1:ncol(train_data)],ntree = 10)
  #    randomForest(x=train_data[,1:14],y=train_data[,15],ntree = 10)
  #  })



  output$train_data <- renderDataTable({
    train_data <- input$train_data
    if(is.null(train_data)){
      return(NULL)
    }
    else{
      read.csv(file = train_data$datapath,header = input$header,sep = input$separator,quote = input$quote)
    }
  }
  )

  output$test_data <- renderDataTable({
    test_data <- input$test_data
    if(is.null(test_data)){
      return(NULL)
    }
    else{
      read.csv(file = test_data$datapath,header = input$header,sep = input$separator,quote = input$quote)
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
    test_data <- input$test_data
    if(is.null(test_data)){
      return(NULL)
    }
    else{
      test_data <- as.data.frame(read.csv(file = test_data$datapath,header = T))
    }
    n1 <-NCOL(train_data)
    x <- train_data[,1:(n1-1)]
    y <- train_data[,n1]
    xtest <- test_data[,1:(n1-1)]
    ytest <- test_data[,n1]

    if(input$type=='Classification') {
      y <- as.factor(y)
    }
    else{
      y <- as.numeric(y)
    }

    if(input$type=='Classification') {
      ytest <- as.factor(ytest)
    }
    else{
      ytest <- as.numeric(ytest)
    }

    rand_model <<- randomForest(x=x,
                                y=y,
                                xtest=xtest,
                                ytest=ytest,
                                ntree = input$ntree,
                                mtry = input$mtry ,
                                replace=if(input$replace=='NO') FALSE else TRUE,
                                sampsize = if (input$replace=='YES') n1 else ceiling(0.632*n1),
                                nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
                                importance = if(input$importance=='NO') FALSE else TRUE,
                                nPerm = 1,
                                proximity = if(input$proximity=='NO') FALSE else TRUE,
                                norm.votes=TRUE,
                                do.trace=FALSE,
                                oob.prox = if(input$oob.prox=='NO') FALSE else TRUE,
                                keep.forest=if(input$keep.forest=='NO') FALSE else TRUE,
                                corr.bias=FALSE,
                                keep.inbag = F
    )

    list(call=rand_model$call,
         type=rand_model$type,
         predicted=if(!is.null(rand_model$predicted)) rand_model$predicted else NULL,
         importance=if(!is.null(rand_model$importance)) rand_model$importance else NULL,
         importanceSD=if(!is.null(rand_model$importanceSD)) rand_model$importanceSD else NULL,
         ntree=if(!is.null(rand_model$ntree)) rand_model$ntree else NULL,
         mtry=if(!is.null(rand_model$mtry)) rand_model$mtry else NULL,
         forest=if(!is.null(rand_model$forest)) rand_model$forest else NULL,
         err.rate=if(!is.null(rand_model$err.rate)) rand_model$err.rate else NULL,
         confusion=if(!is.null(rand_model$confusion)) rand_model$confusion else NULL,
         #         votes=if(is.null(rand_model$votes)) rand_model$votes else NULL,
         #         oob.times=if(is.null(rand_model$oob.times)) rand_model$oob.times else NULL,
         proximity=if(!is.null(rand_model$proximity)) rand_model$proximity else NULL,
         mse=if(!is.null(rand_model$mse)) rand_model$mse else NULL
    )
  })

  output$getTree <- renderPrint({

    getT <- getTree(rfobj = rand_model,k = input$getTree,labelVar=TRUE)

    list(tree=getT)

  })

  output$marginPlot <- renderPlot({

    margin <- as.list(margin(rand_model))
    margin <- sapply(X = margin,FUN = '[')
    margin <- sort(x = margin,decreasing = FALSE)
    names(margin) <- NULL
    margin <- data.frame(x=c(1:length(margin)),y=margin)

    gg  <- ggplot(data=margin,mapping=aes(x=x,y=y))

    gg <- gg + geom_point(colour=I('steelblue'),size=I(3))

    gg <- gg+labs(x='index',y='margin',title='margin plot')

    gg <- gg+theme(plot.title=element_text(size=20,face='bold'),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15))

    print(gg)
  })


  output$randomForest <- renderPlot({
    test <- !(is.null(rand_model$test$mse) || is.null(rand_model$test$err.rate))
    if(rand_model$type == "regression") {
      err <- as.data.frame(rand_model$mse)
      if(test) err <- as.data.frame(cbind(err, rand_model$test$mse))
    } else {
      err <- as.data.frame(rand_model$err.rate)
      if(test) err <- as.data.frame(cbind(err, rand_model$test$err.rate))
    }
    if(test) {
      data <- data.frame(Trees=1:rand_model$ntree,OOB=err[,1],Test=err[,2])
      gg <- ggplot(data=data,mapping = aes(x=Trees,y=OOB))
      gg <- gg + geom_line(colour=I('steelblue'),size=I(1.1))
      gg <- gg + geom_line(data = data,mapping = aes(x=Trees,y=Test))
      gg <- gg+labs(x='Trees',y='Errors',title='randomForest Error')
      gg <- gg+theme(plot.title=element_text(size=20,face='bold'),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15))

      print(gg)
    } else {

      data <- data.frame(Trees=1:rand_model$ntree,OOB=err[,1])

      gg <- ggplot(data=data,mapping = aes(x=Trees,y=OOB))
      gg <- gg + geom_line(colour=I('steelblue'),size=I(1.1))
      gg <- gg+labs(x='Trees',y='Errors',title='RandomForest Model')
      gg <- gg+theme(plot.title=element_text(size=20,face='bold'),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15))
      print(gg)

    }

  })

}
)

