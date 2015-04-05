options(shiny.maxRequestSize=30*1024^2)

if(getRversion() >= "2.15.1") utils::globalVariables(c('margin_plot', 'randomForest_plot', 'rand_model','margin','model_list','gg_pr_Curve','gg_Cumulative_curve','gg_roc_curve','gg_lift_curve'))

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
                                keep.inbag = FALSE
    )
    summary(rand_model)
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
    margin_plot  <<- ggplot(data=margin,mapping=aes(x=x,y=y))
    margin_plot <<- margin_plot + geom_point(colour=I('steelblue'),size=I(1.1))
    margin_plot <<- margin_plot+labs(x='index',y='margin',title='margin plot')
    margin_plot <<- margin_plot+theme(plot.title=element_text(size=20,face='bold'),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15))
    print(margin_plot)
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
      randomForest_plot <<- ggplot(data=data,mapping = aes(x=Trees,y=OOB))
      randomForest_plot <<- randomForest_plot + geom_line(colour=I('steelblue'),size=I(1.1))
      randomForest_plot <<- randomForest_plot + geom_line(data = data,mapping = aes(x=Trees,y=Test))
      randomForest_plot <<- randomForest_plot+labs(x='Trees',y='Errors',title='randomForest Error')
      randomForest_plot <<- randomForest_plot+theme(plot.title=element_text(size=20,face='bold'),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15))

      print(randomForest_plot)
    } else {

      data <- data.frame(Trees=1:rand_model$ntree,OOB=err[,1])
      randomForest_plot <<- ggplot(data=data,mapping = aes(x=Trees,y=OOB))
      randomForest_plot <<- randomForest_plot + geom_line(colour=I('steelblue'),size=I(1.1))
      randomForest_plot <<- randomForest_plot+labs(x='Trees',y='Errors',title='RandomForest Model')
      randomForest_plot <<- randomForest_plot+theme(plot.title=element_text(size=20,face='bold'),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15))
      print(randomForest_plot)
    }
  })

  plot_margin <- function(){
    print(margin_plot)
  }

  plot_randomForest <<- function(){
    print(randomForest_plot)
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
plot_gg_pr_Curve()
plot_gg_Cumulative_curve()
plot_gg_roc_curve()
plot_gg_lift_curve()
plot_grid()
```       ',file='RandomForestReport.Rmd',append=F)
      out <- render('RandomForestReport.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )


  output$pr_curve <- renderPlot({
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
    n1 <- NCOL(train_data)
    preds <- predict(rand_model,test_data[,1:(n1-1)],type='prob',norm.votes = T,predict.all = T)
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## predictions,labels
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    pre <- as.data.frame(preds$aggregate)
    or <- order(pre[,2],decreasing = TRUE)
    test <- test_data[or,n1]
    pre <- pre[or,2]
    pre <- data.frame(pre,test)
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## predictions,labels
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    predictions <- pre[,1]
    labels <- ifelse(pre[,2]=='fraud',1,0)
    pred <- prediction(predictions,labels)
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## precision/recall curve
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    pr_Curve <- performance(pred,'prec','rec')
    pr_Curve@y.values <- lapply(pr_Curve@y.values,function(x) rev(cummax(rev(x))))
    pr_Curve_data <- data.frame(as.data.frame(pr_Curve@x.values),as.data.frame(pr_Curve@y.values),row.names = NULL)
    names(pr_Curve_data) <- c('x','y')
    gg_pr_Curve  <<- ggplot(data=pr_Curve_data,mapping=aes(x=x,y=y))
    gg_pr_Curve <<- gg_pr_Curve + geom_line(colour=I('steelblue'),size=I(1.1))
    gg_pr_Curve <<- gg_pr_Curve+labs(x='Recall',y='Precision',title='Precision/Recall Curve')
    gg_pr_Curve <<- gg_pr_Curve+theme(plot.title=element_text(size=15,face='bold'),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15))
    print(gg_pr_Curve)
  })

  output$Cumulative_curve <- renderPlot({
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
    n1 <- NCOL(train_data)
    preds <- predict(rand_model,test_data[,1:(n1-1)],type='prob',norm.votes = T,predict.all = T)
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## predictions,labels
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    pre <- as.data.frame(preds$aggregate)
    or <- order(pre[,2],decreasing = TRUE)
    test <- test_data[or,n1]
    pre <- pre[or,2]
    pre <- data.frame(pre,test)
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## predictions,labels
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    predictions <- pre[,1]
    labels <- ifelse(pre[,2]=='fraud',1,0)
    pred <- prediction(predictions,labels)
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Cumulative_curve
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Cumulative_curve <- performance(pred,'rec','rpp')
    Cumulative_curve_data <- data.frame(as.data.frame(Cumulative_curve@x.values),as.data.frame(Cumulative_curve@y.values),row.names = NULL)
    names(Cumulative_curve_data) <- c('x','y')
    gg_Cumulative_curve  <<- ggplot(data=Cumulative_curve_data,mapping=aes(x=x,y=y))
    gg_Cumulative_curve <<- gg_Cumulative_curve + geom_line(colour=I('steelblue'),size=I(1.1))
    gg_Cumulative_curve <<- gg_Cumulative_curve+labs(x='Rate of positive predictions',y='Recall',title='Cumulative Recall Curve')
    gg_Cumulative_curve <<- gg_Cumulative_curve+theme(plot.title=element_text(size=15,face='bold'),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15))
    print(gg_Cumulative_curve)
  })

  output$roc_curve <- renderPlot({
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
    n1 <- NCOL(train_data)
    preds <- predict(rand_model,test_data[,1:(n1-1)],type='prob',norm.votes = T,predict.all = T)
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## predictions,labels
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    pre <- as.data.frame(preds$aggregate)
    or <- order(pre[,2],decreasing = TRUE)
    test <- test_data[or,n1]
    pre <- pre[or,2]
    pre <- data.frame(pre,test)
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## predictions,labels
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    predictions <- pre[,1]
    labels <- ifelse(pre[,2]=='fraud',1,0)
    pred <- prediction(predictions,labels)
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## ROC_curve
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    roc_curve <- performance(pred,'tpr','fpr')
    roc_curve_data <- data.frame(as.data.frame(roc_curve@x.values),as.data.frame(roc_curve@y.values),row.names = NULL)
    names(roc_curve_data) <- c('x','y')
    gg_roc_curve  <<- ggplot(data=roc_curve_data,mapping=aes(x=x,y=y))
    gg_roc_curve <<- gg_roc_curve + geom_line(colour=I('steelblue'),size=I(1.1))
    gg_roc_curve <<- gg_roc_curve+labs(x='False positive rate',y='True positive rate',title='ROC Curve')
    gg_roc_curve <<- gg_roc_curve+theme(plot.title=element_text(size=15,face='bold'),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15))
    print(gg_roc_curve)
  })

  output$lift_curve <- renderPlot({
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
    n1 <- NCOL(train_data)
    preds <- predict(rand_model,test_data[,1:(n1-1)],type='prob',norm.votes = T,predict.all = T)
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## predictions,labels
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    pre <- as.data.frame(preds$aggregate)
    or <- order(pre[,2],decreasing = TRUE)
    test <- test_data[or,n1]
    pre <- pre[or,2]
    pre <- data.frame(pre,test)
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## predictions,labels
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    predictions <- pre[,1]
    labels <- ifelse(pre[,2]=='fraud',1,0)
    pred <- prediction(predictions,labels)
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Lift_curve
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    lift_curve <- performance(pred,'lift','rpp')
    lift_curve_data <- data.frame(as.data.frame(lift_curve@x.values),as.data.frame(lift_curve@y.values),row.names = NULL)
    names(lift_curve_data) <- c('x','y')
    gg_lift_curve  <<- ggplot(data=lift_curve_data,mapping=aes(x=x,y=y))
    gg_lift_curve <<- gg_lift_curve + geom_line(colour=I('steelblue'),size=I(1.1))
    gg_lift_curve <<- gg_lift_curve+labs(x='Rate of positive predictions',y='Lift value',title='Lift Curve')
    gg_lift_curve <<- gg_lift_curve+theme(plot.title=element_text(size=15,face='bold'),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15))
    print(gg_lift_curve)
  })

  output$grid <- renderPlot({
    require(gridExtra)
    grid.arrange(gg_pr_Curve, gg_Cumulative_curve, gg_roc_curve, gg_lift_curve, ncol=2, nrow=2, widths=c(2,2), heights=c(2,2),main='Adaboost')
  })

  plot_gg_pr_Curve <- function(){
    print(gg_pr_Curve)
  }
  plot_gg_Cumulative_curve <- function(){
    print(gg_Cumulative_curve)
  }
  plot_gg_roc_curve <- function(){
    print(gg_roc_curve)
  }
  plot_gg_lift_curve <- function(){
    print(gg_lift_curve)
  }
  plot_grid <- function(){
    grid.arrange(gg_pr_Curve, gg_Cumulative_curve, gg_roc_curve, gg_lift_curve, ncol=2, nrow=2, widths=c(2,2), heights=c(2,2),main='RndomForest')
  }

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
      paste('pr_curve','.jpg', sep='')
    },
    content = function(file) {
      jpeg(file)
      plot_gg_pr_Curve()
      dev.off()
    },
    contentType='image/jpg')

  output$downloadData2 = downloadHandler(
    filename = function() {
      paste('pr_curve','.png', sep='')
    },
    content = function(file) {
      png(file)
      plot_gg_pr_Curve()
      dev.off()
    },
    contentType='image/png')

  output$downloadData3 = downloadHandler(
    filename = function() {
      paste('pr_curve','.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      plot_gg_pr_Curve()
      dev.off()
    },
    contentType='image/pdf')

  output$downloadData4 = downloadHandler(
    filename = function() {
      paste('cumulative_curve','.jpg', sep='')
    },
    content = function(file) {
      jpeg(file)
      plot_gg_Cumulative_curve()
      dev.off()
    },
    contentType='image/jpg')

  output$downloadData5 = downloadHandler(
    filename = function() {
      paste('cumulative_curve','.png', sep='')
    },
    content = function(file) {
      png(file)
      plot_gg_Cumulative_curve()
      dev.off()
    },
    contentType='image/png')

  output$downloadData6 = downloadHandler(
    filename = function() {
      paste('cumulative_curve','.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      plot_gg_Cumulative_curve()
      dev.off()
    },
    contentType='image/pdf')

  output$downloadData7 = downloadHandler(
    filename = function() {
      paste('roc_curve','.jpg', sep='')
    },
    content = function(file) {
      jpeg(file)
      plot_gg_roc_curve()
      dev.off()
    },
    contentType='image/jpg')

  output$downloadData8 = downloadHandler(
    filename = function() {
      paste('roc_curve','.png', sep='')
    },
    content = function(file) {
      png(file)
      plot_gg_roc_curve()
      dev.off()
    },
    contentType='image/png')

  output$downloadData9 = downloadHandler(
    filename = function() {
      paste('roc_curve','.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      plot_gg_roc_curve()
      dev.off()
    },
    contentType='image/pdf')
  output$downloadData10 = downloadHandler(
    filename = function() {
      paste('lift_curve','.jpg', sep='')
    },
    content = function(file) {
      jpeg(file)
      plot_gg_lift_curve()
      dev.off()
    },
    contentType='image/jpg')

  output$downloadData11 = downloadHandler(
    filename = function() {
      paste('lift_curve','.png', sep='')
    },
    content = function(file) {
      png(file)
      plot_gg_lift_curve()
      dev.off()
    },
    contentType='image/png')

  output$downloadData12= downloadHandler(
    filename = function() {
      paste('lift_curve','.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      plot_gg_lift_curve()
      dev.off()
    },
    contentType='image/pdf')
  output$downloadData13 = downloadHandler(
    filename = function() {
      paste('grid','.jpg', sep='')
    },
    content = function(file) {
      jpeg(file)
      plot_grid()
      dev.off()
    },
    contentType='image/jpg')

  output$downloadData14= downloadHandler(
    filename = function() {
      paste('grid','.png', sep='')
    },
    content = function(file) {
      png(file)
      plot_grid()
      dev.off()
    },
    contentType='image/png')

  output$downloadData15= downloadHandler(
    filename = function() {
      paste('grid','.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      plot_grid()
      dev.off()
    },
    contentType='image/pdf')

  output$downloadData16 = downloadHandler(
    filename = function() {
      paste('margin','.jpg', sep='')
    },
    content = function(file) {
      jpeg(file)
      plot_margin()
      dev.off()
    },
    contentType='image/jpg')

  output$downloadData17 = downloadHandler(
    filename = function() {
      paste('margin','.png', sep='')
    },
    content = function(file) {
      png(file)
      plot_margin()
      dev.off()
    },
    contentType='image/png')

  output$downloadData18 = downloadHandler(
    filename = function() {
      paste('margin','.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      plot_margin()
      dev.off()
    },
    contentType='image/pdf')

  output$downloadData19 = downloadHandler(
    filename = function() {
      paste('randomForest','.jpg', sep='')
    },
    content = function(file) {
      jpeg(file)
      plot_randomForest()
      dev.off()
    },
    contentType='image/jpg')

  output$downloadData20 = downloadHandler(
    filename = function() {
      paste('randomForest','.png', sep='')
    },
    content = function(file) {
      png(file)
      plot_randomForest()
      dev.off()
    },
    contentType='image/png')

  output$downloadData21 = downloadHandler(
    filename = function() {
      paste('randomForest','.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      plot_randomForest()
      dev.off()
    },
    contentType='image/pdf')

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

}
)

