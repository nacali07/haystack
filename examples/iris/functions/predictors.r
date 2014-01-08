## inputs
#### x: Haystack Object
#### model: predictive model of the appropriate class
#### TODO: create generic for predict.haystack

predict.haystack.randomforest<-function(x,model){

    "      get list of predictions of the Haystack object dataset using randomforest predictive model
    "
  dev_mean<-.5 # mean of binary target in training data
  val_mean<-.5 # mean of binary target in validation data (representative sample)
  pred <- predict(model,newdata=x$data,type="prob")[,"1"]
  pred_adj_odds <- pred/(1-pred)*val_mean/(1-val_mean)*(1-dev_mean)/dev_mean
  pred_adj      <- pred_adj_odds/(1+pred_adj_odds)
  pred_adj[is.nan(pred_adj)]<-1
  return(pred_adj)
}
predict.haystack.glm<-function(x,model){
    "      get list of predictions of the Haystack object dataset using Generalized linear model predictive model
    "

  dev_mean<-.5 # mean of binary target in training data
  val_mean<-.5 # mean of binary target in validation data (representative sample)
  pred<-predict(model,newdata=x$data,type="response")
  pred_adj_odds <- pred/(1-pred)*val_mean/(1-val_mean)*(1-dev_mean)/dev_mean
  pred_adj      <- pred_adj_odds/(1+pred_adj_odds)
  pred_adj[is.nan(pred_adj)]<-1
  return(pred_adj)
}
