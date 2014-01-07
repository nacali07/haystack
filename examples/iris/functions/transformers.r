# x: Haystack Object
y_transformer<-function(x){
  x$data$isVirginica<-as.factor(1*(x$data$Species=="virginica"))
}

# x: Haystack Object
x_transformer<-function(x){
  cat("No dependent variable transformations.\n")
  # No tranformations
}
