# shinyOfRandomForest
基于shiny的随机森林，使用方法为：
```s
if(!require(shiny)) {
  install.packages(pkgs = 'shiny',quiet = TRUE)
  require(shiny)
}
runGitHub(username = 'guanlongtianzi',repo = 'shinyOfRandomForest') 
```
各个参数的介绍如下：
- ntree

Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.
