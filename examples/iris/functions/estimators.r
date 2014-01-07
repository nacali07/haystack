# x is a Haystack object
logitEstimator<-function(x){
  glm( formula,family=binomial(link="logit"),data=x$data)
}

rfEstimator<-function(x){
  require(randomForest)
  randomForest( formula, data=x$data,ntree=2000)
}
