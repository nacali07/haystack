## Classes useful in statistical modeling in finance institutions
## This utilizes the recently developed reference classes.
## Im trying to design this to mimic the real life financial and econometric
## modeling process.  
## 1. Read data into the Dataset reference class (RC) object
## 2. 
## Bill West: 2013-12-30 <williamjwest@gmail.com>


errorInvalidDataset<-function(dsnames){
  stop(paste(
    "Please specify valid data name:"
    ,dsnames)                                   
}

HaystackModel<-setRefClass('HaystackModel'
                   ,fields=c(
                       x_transformer = "function"
                       y_transformer = "function"
                     , formula = "ANY"   
                     , scorer = "function"
                     , validator = "function"
                     , estimator = "function"   
                     , model = "ANY"
                     , name = "character"   
                     , datasets = 'list'   
                    ),
                    methods = list(
                      y_transform = function(x){
                           if(! x %in% names(datasets)){
                             errorInvalidDataset(names(datasets))                                 
                           } else {
                             y_transformer(.self$datasets[[x]])
                           }    
                      },
                      x_transform = function(x){
                           if(! x %in% names(datasets)){
                             errorInvalidDataset(names(datasets))                                 
                           } else {
                             x_transformer(.self$datasets[[x]])
                           }    
                      },
                      score = function(x){
                           if(! x %in% names(datasets)){
                             errorInvalidDataset(names(datasets))                                 
                           } else {
                             pname<-paste("pred_",name,sep="")
                             .self$datasets[[x]]$data[,pname] <<- scorer(.self$datasets[[x]],.self$model)
                           }    
                      },                
                      validate = function(x){
                           if(! x %in% names(datasets)){
                             errorInvalidDataset(names(datasets))                                                                   
                           } else {
                             validator(.self$datasets[[x]])
                           }    
                      },                
                      vts = function(x){
                           validate(x,name)
                           transform(x,name)
                           score(x,name)   
                      },
                      getpred = function(x,key=NULL,names=NULL){
                           if(! x %in% names(datasets)){
                             errorInvalidDataset(names(datasets))                                                                   
                           } else {                                  
                             cols<-c(key,names)
                             if(is.null(cols)){
                               .self$datasets[[x]]$data[,grep("pred_",colnames(.self$datasets[[x]]$data),value=T)]
                             } else if(key %in% colnames(.self$datasets[[x]]$data) & is.null(names)){
                               .self$datasets[[x]]$data[,c(key,grep("pred_",colnames(data),value=T))]
                             } else if(all(cols %in% colnames(.self$datasets[[x]]$data))){
                               .self$datasets[[x]]$data[,cols]
                             } else {
                               for(i in cols){
                                 if(! cols[i]  %in% colnames(.self$datasets[[x]]$data)){
                                   print(paste(cols[i],"not in dataset."))
                                 }
                               }
                             }
                        },
                        viewperformance = function(){
                             
                        },
                        estimate        = function(){
                             model <<- estimator(formula,x=devdata,...)
                        }
                       ))

HaystackQuery<-setRefClass('HaystackQuery'
                   ,fields=c(con="function",query="character",execution="function")
                  )
HaystackJsonQuery<-setRefClass('HaystackJsonQuery'
                       ,contains="HaystackQuery"
                       ,methods=list(
                         initialize=function(){
                           execution <<- function(){
                             require(jsonlite)
                             require(httr)
                             fromJSON(con)
                           }
                         }))

HaystackMySQLQuery<-setRefClass('HaystackMySQLQuery'
                        ,contains="HaystackQuery"
                        ,methods=list(
                          initialize = function(){
                            execution <<- function(){
                              require(RMySQL)
                              conn<-con()
                              dbGetQuery(conn,query)
                            }
                          }))

Haystack<-setRefClass('Haystack'
                     ,fields=c(  data          = "data.frame"  
                               , query         = "ANY"    #returns data.frame
                               , jsonparser    = "function"   
                               , snapshot_date = "Date"
                               , notes         = "character")
                     ,methods=list(
                       addNote = function(x){
                          notes <<- c(notes, paste(now(),":",x))
                       },
                       readJson = function(){
                             data <<- jsonParser$execution()
                             addNote("Data obtained from json")
                       },
                       readDb = function(){
                             data <<- query$execution()
                             addNote("Data obtained from db.")
                       },
                       setJsonParser  = function(x){
                         jsonParser <<-x$copy()
                         addNote("JSON Parser set.")
                       },
                       setDbQuery = function(x){
                         query <<-x$copy()
                         addNote("Query Set.")
                       },
                       getpred = function(key=NULL,names=NULL){
                         cols<-c(key,names)
                         if(is.null(cols)){
                           data[,grep("pred_",colnames(data),value=T)]
                         } else if(key %in% colnames(data) & is.null(names)){
                           data[,c(key,grep("pred_",colnames(data),value=T))]
                         } else if(all(cols %in% colnames(data))){
                           data[,cols]
                         } else {
                           for(i in cols){
                             if(! cols[i]  %in% colnames(data)){
                               print(paste(cols[i],"not in dataset."))
                             }
                           }
                         }
                       },
                       summaryStats=function(){
                         summary(data)  
                       }
                       ))

