# x is a Haystack object
logitEstimator<-function(x,formula=formula){
  glm( formula,family=binomial(link="logit"),data=x$data)
}

rfEstimator<-function(x,formula=formula){
  require(randomForest)
  randomForest( formula, data=x$data,ntree=2000)
}
