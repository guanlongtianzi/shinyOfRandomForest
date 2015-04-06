# shinyOfRandomForest
基于shiny的随机森林，使用到了`shiny`、`randomForest`、`shinyAce`、`rmarkdown`、`ggplot2`等`R`包，使用方法为：
```R
if(!require(shiny)) {
  install.packages(pkgs = 'shiny',quiet = TRUE)
  require(shiny)
}
if(!require(randomForest)) {
  install.packages(pkgs = 'randomForest',quiet = TRUE)
  require(randomForest)
}
if(!require(shinyAce)) {
  install.packages(pkgs = 'shinyAce',quiet = TRUE)
  require(shinyAce)
}
if(!require(rmarkdown)) {
  install.packages(pkgs = 'rmarkdown',quiet = TRUE)
  require(rmarkdown)
}
if(!require(ggplot2)) {
  install.packages(pkgs = 'ggplot2',quiet = TRUE)
  require(ggplot2)
}
runGitHub(username = 'guanlongtianzi',repo = 'shinyOfRandomForest') 
```
各个参数的介绍如下：
- ntree.Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.
- mtry.Number of variables randomly sampled as candidates at each split. Note that the default values are different for classiﬁcation (sqrt(p) where p is number of variables in x) and regression (p/3).
- replace.Should sampling of cases be done with or without replacement?
- sampsize.Size(s) of sample to draw. For classiﬁcation, if sampsize is a vector of the length the number of strata, then sampling is stratiﬁed by strata, and the elements of sampsize indicate the numbers to be drawn from the strata.
- nodesize.Minimum size of terminal nodes. Setting this number larger causes smaller trees to be grown (and thus take less time). Note that the default values are different for classiﬁcation (1) and regression (5).
- maxnodes.Maximum number of terminal nodes trees in the forest can have. If not given, trees are grown to the maximum possible (subject to limits by nodesize). If set larger than maximum possible, a warning is issued.
- importance.Should importance of predictors be assessed?
- proximity.Should proximity measure among the rows be calculated?
- keep.forest.If set to FALSE, the forest will not be retained in the output object. If xtest is given, defaults to FALSE.
- keep.inbag.Should an n by ntree matrix be returned that keeps track of which samples are "in-bag" in which trees (but not how many times, if sampling with replacement)
