options(shiny.maxRequestSize=30*1024^2)

if(getRversion() >= "2.15.1") utils::globalVariables(c('plot_1', 'plot_2', 'rand_model','margin','model_list'))

shinyServer(function(input, output) {

  output$names <- renderPrint({
    train_data <- input$train_data
    if(is.null(train_data)){
      return(NULL)
    }
    else{
      name <- names(read.csv(file = train_data$datapath,header = input$header,sep = input$separator,quote = input$quote))
      feature_name <- c()
      for(i in 1:length(name)-1){
        feature_name[i] <- paste('feature_',i,":",name[i],"\n",sep="")
      }
      feature_name[length(name)] <- paste('feature_',length(name),':',name[length(name)],"\n",sep="")
      cat('',feature_name)
    }
  })


  q <- observe({
    if (input$quit == 1) stopApp()
  })

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

    margin_1 <<- margin

    plot_1  <<- ggplot(data=margin,mapping=aes(x=x,y=y))

    plot_1 <<- plot_1 + geom_point(colour=I('steelblue'),size=I(3))

    plot_1 <<- plot_1+labs(x='index',y='margin',title='margin plot')

    plot_1 <<- plot_1+theme(plot.title=element_text(size=20,face='bold'),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15))

    print(plot_1)
  })


  #  output$features_select <- renderUI({
  #    names <- colnames(read.csv(file = train_data$datapath,header = input$header,sep = input$separator,quote = input$quote))
  #   })

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
      plot_2 <- ggplot(data=data,mapping = aes(x=Trees,y=OOB))
      plot_2 <- plot_2 + geom_line(colour=I('steelblue'),size=I(1.1))
      plot_2 <- plot_2 + geom_line(data = data,mapping = aes(x=Trees,y=Test))
      plot_2 <- plot_2+labs(x='Trees',y='Errors',title='randomForest Error')
      plot_2 <- plot_2+theme(plot.title=element_text(size=20,face='bold'),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15))

      print(plot_2)
    } else {

      data <- data.frame(Trees=1:rand_model$ntree,OOB=err[,1])

      plot_2 <- ggplot(data=data,mapping = aes(x=Trees,y=OOB))
      plot_2 <- plot_2 + geom_line(colour=I('steelblue'),size=I(1.1))
      plot_2 <- plot_2+labs(x='Trees',y='Errors',title='RandomForest Model')
      plot_2 <- plot_2+theme(plot.title=element_text(size=20,face='bold'),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15))
      print(plot_2)

    }

  })

  plot_margin <- function(){
    margin <- as.list(margin(rand_model))
    margin <- sapply(X = margin,FUN = '[')
    margin <- sort(x = margin,decreasing = FALSE)
    names(margin) <- NULL
    margin <- data.frame(x=c(1:length(margin)),y=margin)

    plot  <- ggplot(data=margin,mapping=aes(x=x,y=y))
    plot <- plot + geom_point(colour=I('steelblue'),size=I(3))
    plot <- plot+labs(x='index',y='margin',title='margin plot')
    plot <- plot+theme(plot.title=element_text(size=20,face='bold'),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15))
    print(plot)
  }

  plot_randomForest <- function(){
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
      plot_2 <- ggplot(data=data,mapping = aes(x=Trees,y=OOB))
      plot_2 <- plot_2 + geom_line(colour=I('steelblue'),size=I(1.1))
      plot_2 <- plot_2 + geom_line(data = data,mapping = aes(x=Trees,y=Test))
      plot_2 <- plot_2+labs(x='Trees',y='Errors',title='randomForest Error')
      plot_2 <- plot_2+theme(plot.title=element_text(size=20,face='bold'),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15))

      print(plot_2)
    } else {

      data <- data.frame(Trees=1:rand_model$ntree,OOB=err[,1])

      plot_2 <- ggplot(data=data,mapping = aes(x=Trees,y=OOB))
      plot_2 <- plot_2 + geom_line(colour=I('steelblue'),size=I(1.1))
      plot_2 <- plot_2+labs(x='Trees',y='Errors',title='RandomForest Model')
      plot_2 <- plot_2+theme(plot.title=element_text(size=20,face='bold'),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15))
      print(plot_2)

    }
  }

  save_randomForest <- function(file){

    model_list <- list(call=rand_model$call,
                       type=rand_model$type,
                       predicted=if(!is.null(rand_model$predicted)) rand_model$predicted else NULL,
                       importance=if(!is.null(rand_model$importance)) rand_model$importance else NULL,
                       importanceSD=if(!is.null(rand_model$importanceSD)) rand_model$importanceSD else NULL,
                       ntree=if(!is.null(rand_model$ntree)) rand_model$ntree else NULL,
                       mtry=if(!is.null(rand_model$mtry)) rand_model$mtry else NULL,
                       forest=if(!is.null(rand_model$forest)) rand_model$forest else NULL,
                       err.rate=if(!is.null(rand_model$err.rate)) rand_model$err.rate else NULL,
                       confusion=if(!is.null(rand_model$confusion)) rand_model$confusion else NULL,
                       proximity=if(!is.null(rand_model$proximity)) rand_model$proximity else NULL,
                       mse=if(!is.null(rand_model$mse)) rand_model$mse else NULL)


    cat(paste(model_list[[1]]),'\n',paste(model_list[[2]]),'\n',paste(model_list[[3]]),'\n',paste(model_list[[4]]),'\n',paste(model_list[[5]]),'\n',paste(model_list[[6]]),'\n',paste(model_list[[7]]),'\n',paste(model_list[[8]]),'\n',paste(model_list[[9]]),paste(model_list[[10]]),paste(model_list[[11]]),file = file,sep = ' ',append = T)

  }

  save_tree <- function(file){

    getT <- getTree(rfobj = rand_model,k = input$getTree,labelVar=TRUE)

    tree_list <- list(tree=getT)

    cat(paste(tree_list[[1]]),file = file,sep = ' ',append = T)

  }


  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('RandomForestReport', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },

    content = function(file) {
      model_list <- list(call=rand_model$call,
                         type=rand_model$type,
                         predicted=if(!is.null(rand_model$predicted)) rand_model$predicted else NULL,
                         importance=if(!is.null(rand_model$importance)) rand_model$importance else NULL,
                         importanceSD=if(!is.null(rand_model$importanceSD)) rand_model$importanceSD else NULL,
                         ntree=if(!is.null(rand_model$ntree)) rand_model$ntree else NULL,
                         mtry=if(!is.null(rand_model$mtry)) rand_model$mtry else NULL,
                         forest=if(!is.null(rand_model$forest)) rand_model$forest else NULL,
                         err.rate=if(!is.null(rand_model$err.rate)) rand_model$err.rate else NULL,
                         confusion=if(!is.null(rand_model$confusion)) rand_model$confusion else NULL,
                         proximity=if(!is.null(rand_model$proximity)) rand_model$proximity else NULL,
                         mse=if(!is.null(rand_model$mse)) rand_model$mse else NULL)
      cat('---
title: "',input$title,'"
author: "',input$author,'"
date: ',date(),'
output:', input$format,'_document
---
This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:
```{r,echo=T,prompt=T}
summary(rand_model)
```
```{r,echo=T,prompt=T}
model_list
plot_randomForest()
plot_margin()
```       ',file='RandomForestReport.Rmd',append=F)

      #  src <- normalizePath('report.Rmd')
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      #  owd <- setwd(tempdir())
      #  on.exit(setwd(owd))
      #  file.copy(src, 'report.Rmd')

      out <- render('RandomForestReport.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )


  output$downloadRandomForest=downloadHandler(filename = function() {
    paste('summaryofTree','.txt', sep='')
  },
  content = function(file) {
    sink(file = file)
    save_tree(file)
    sink()
  },
  contentType='text/csv')



  output$downloadTree=downloadHandler(filename = function() {
    paste('summaryofTree','.txt', sep='')
  },
  content = function(file) {
    sink(file = file)
    save_randomForest(file)
    sink()
  },
  contentType='text/csv')



  output$downloadData1 = downloadHandler(
    filename = function() {
      paste('margin','.jpg', sep='')
    },
    content = function(file) {
      jpeg(file)
      plot_margin()
      dev.off()
    },
    contentType='image/jpg')

  output$downloadData2 = downloadHandler(
    filename = function() {
      paste('margin','.png', sep='')
    },
    content = function(file) {
      png(file)
      plot_margin()
      dev.off()
    },
    contentType='image/png')

  info1  <- reactive({
    info1 <- paste("This analysis was performed on ", format(Sys.time(), "%A, %B %d %Y at %I:%M:%S %p"), ".", sep = "")
    info2 <- paste(strsplit(R.version$version.string, " \\(")[[1]][1], " was used for this session.", sep = "")
    info3 <- paste("Package version infomation for this session:")
    info4 <- paste("shiny", packageVersion("shiny"))
    info5 <- paste("randomForest", packageVersion("randomForest"))
    info6 <- paste("ggplot2", packageVersion("ggplot2"))
    info7 <- paste("shinyAce", packageVersion("shinyAce"))
    info8 <- paste("rmarkdown", packageVersion("rmarkdown"))

    cat(sprintf(info1), "\n")
    cat(sprintf(info2), "\n")
    cat(sprintf(info3), "\n")
    cat(sprintf(info4), "\n")
    cat(sprintf(info5), "\n")
    cat(sprintf(info6), "\n")
    cat(sprintf(info7), "\n")
    cat(sprintf(info8), "\n")
  })

  output$info1.out <- renderPrint({
    info1()
  })

  output$downloadData3 = downloadHandler(
    filename = function() {
      paste('margin','.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      plot_margin()
      dev.off()
    },
    contentType='image/pdf')

  output$downloadData4 = downloadHandler(
    filename = function() {
      paste('randomForest','.jpg', sep='')
    },
    content = function(file) {
      jpeg(file)
      plot_randomForest()
      dev.off()
    },
    contentType='image/jpg')

  output$downloadData5 = downloadHandler(
    filename = function() {
      paste('randomForest','.png', sep='')
    },
    content = function(file) {
      png(file)
      plot_randomForest()
      dev.off()
    },
    contentType='image/png')

  output$downloadData6 = downloadHandler(
    filename = function() {
      paste('randomForest','.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      plot_randomForest()
      dev.off()
    },
    contentType='image/pdf')

}
)

