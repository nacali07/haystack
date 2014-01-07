# x: Haystack Object
y_transformer<-function(x){
  x$data$isSetosa<-as.factor(x$data$Species=="setosa")
}

# x: Haystack Object
x_transformer<-function(x){
  # No tranformations
}
