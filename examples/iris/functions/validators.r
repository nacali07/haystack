# x: Haystack object
validator<-function(x){
  ##### Constants ###########
  necessaryColumns<-c("Petal.Width","Petal.Length","Sepal.Width","Sepal.Length")
  necessaryColumnTests<-rep("is.numeric",4)
  
  ##### Dataset validation ###
  columnsPresent<- necessaryColumns %in% colnames(x$data)
  errors<-0
  for(i in 1:length(necessaryColumns)){
    if(!columnsPresent[i]){
      print(paste(necessaryColumns[i],"not present in scoring dataset."))
      errors<-errors+1
    } else if(!do.call(necessaryColumnTests[i],list(x$data[1,necessaryColumns[i]]))){
      print(paste(necessaryColumns[i],"failed test:",necessaryColumnTests[i]))
      errors<-errors+1
    }
  }
  if(errors>0){
    stop(paste(now(),": Stopping.  errors>0."))
  }
  
}
