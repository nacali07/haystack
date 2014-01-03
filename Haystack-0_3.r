## These classes are designed to improve the efficiency of modeling in R within
## finance institutions.  The improvements these offer over my existing process
## are several:
## 1. Datasets and models are objects that can be passed by reference to
##    functions and methods
## 2. Each verion of a model exists and an object which can be easily copied
##    into a new object and modified slightly.  
## This utilizes the recently developed reference classes.
## Im trying to design this to mimic the real life financial and econometric
## modeling process.
## 0. Create Haystack instance.  Set appropriate parameters
## 1. Read data into the Dataset reference class (RC) object
## 2. Create model instance, set parameters, and associate data
## 3. Run all appropriate methods to do your work
## 4. Save model and dataset to a location
## Bill West: 2013-12-30 <williamjwest@gmail.com>


errorInvalidDataset<-function(dsnames){
  stop(paste(
    "Please specify valid data name:"
    ,dsnames))                                   
}

HaystackModel<-setRefClass('HaystackModel'
     ,fields=c(
         x_transformer = "function"
       , y_transformer = "function"
       , formula = "ANY"   
       , scorer = "function"
       , validator = "function"
       , estimator = "function"   
       , model = "ANY"
       , name = "character"   
       , datasets = 'list'
       , homedir = "character"   
       , performance = 'list'
       ),
      methods = list(
        addDataset<-function(x,dsname){
            datasets[[dsname]] <<- x 
        }            
        saveRds = function(prod=F){
            if(is.null(homedir)){
                stop("Please set homedir before saving: e.g. x$setHomeDir('~/path')")
            }
            if (is.null(name)){
                stop("Please set name before saving: e.g. x$setName('data76') or x$name<-'data76'")
            }
            newfile<-paste(homedir,"models/",name,".rds",sep="")
            if(file.exists(newfile)){
                response<-ask(newfile,' exists. Overwrite? (y/n) > ')
                if(tolower(response)!='y'){
                    stop("saveRDS aborted.")
                }
            }   
            saveRDS(.self,file=newfile)
        },        
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
         avts = function(x,dsname){ #add,validate,transform,score
             addDataset(x,name)
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
            }
         },
         performance = function(pred,target,dsname,name){
            require(ROCR)
            # Use ROCR to do the calculations
            pred<-prediction(
                              datasets[[dsname]]$data[,pred]
                            , target[[dsname]]$data[,target])
      
            # User ROCR to calculate the performance metrics
            perf<-performance(pred,"tpr","fpr")
      
            # Use a home grown function to figure out the lift at 20% of the file
            liftAtCutoff<-function(x,perf){max(perf@y.values[[1]][perf@x.values[[1]]<=x])}
            lift.1<-liftAtCutoff(.2,perf)
          
            # Set up the plotting device
            par(mfrow=c(1,1))
          
            # Relatively pretty plot supplied by ROCR
            plot(perf,avg='threshold', spread.estimate='stddev',colorize=T)
            abline(0,1,col="grey")
            text(lift.1,.2,.8,col="purple")
            newPerformance<-list(
              timestamp = now(), pred = pred, target=target,dsname=dsname                    
            )
            newPerformance$plot<-recordPlot()
            performance[[name]]<-newPerformance
         },
         estimate        = function(dsname){
               model <<- estimator(formula,dsname,...)
         },
         exportProductionModel = function(){
              tmp<-.self$copy()
              tmp$y_transformer<-NULL
              tmp$formula<-NULL
              tmp$datasets<-list()
              tmp$performance<-NULL

              tmp$methods(list(
                               estimate = NULL,
                               saveRds = NULL,
                               y_transform = NULL,
                               getpred = NULL,
                               performance = NULL,
                               estimate = NULL
                               exportProductionModel = NULL))
              return(tmp)
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


RestoreHaystack<-function(name,homedir){
   stopifnot(!is.null(name))
   stopifnot(!is.null(homedir))

   homedir <<- paste(gsub("\\/$","",homedir),"/",sep="")
   filename<-paste(homedir,"datasets/",name,".rds",sep="")
   return(readRDS(filename))
}

RestoreHaystackModel<-function(name,homedir){
   stopifnot(!is.null(name))
   stopifnot(!is.null(homedir))

   homedir <<- paste(gsub("\\/$","",homedir),"/",sep="")
   filename<-paste(homedir,"models/",name,".rds",sep="")
   return(readRDS(filename))
}

Haystack<-setRefClass('Haystack'
      ,fields=c(
            data          = "data.frame"  
          , query         = "ANY"    #returns data.frame
          , jsonparser    = "function"   
          , snapshot_date = "Date"
          , notes         = "character"
          , homedir       = "character"
          , name          = "character"
          , key           = "character"
          , description   = "character")
      ,methods=list(
        setName = function(x){
            name <<- x
        },            
        setHomeDir = function(x){
            homedir <<- paste(gsub("\\/$","",x),"/",sep="")
        },
        saveRds = function(){
            if(is.null(homedir)){
                stop("Please set homedir before saving: e.g. x$setHomeDir('~/path')")
            }
            if (is.null(name)){
                stop("Please set name before saving: e.g. x$setName('data76') or x$name<-'data76'")
            }

            newfile<-paste(homedir,"datasets/",name,".rds",sep="")
            if(file.exists(newfile)){
                response<-ask(newfile,' exists. Overwrite? (y/n) > ')
                if(tolower(response)!='y'){
                    stop("saveRDS aborted.")
                }
            }   
            saveRDS(.self,file=newfile)
        },        
        addNote = function(x){
           notes <<- c(notes, paste(now(),":",x))
        },
        readJson = function(){
              data <<- jsonParser$execution()
              addNote("Data obtained from json")
        },
        printJson = function(fields='ALL'){
            require(httr)
            require(jsonlite)
            if(fields=='ALL'){
              return(toJSON(data))
            } else if(fields=='PRED'){
                  predFields <- data[,grep("pred_",colnames(data),value=T)]
                  return(toJSON(data[,c(key,predFields)]))
            } else if(!is.null(fields)){
                  uniquerFields<-unique(c(key,fields))  
                  return(toJSON(data[,uniqueFields]))
            } else {
              stop("Please enter a valid set of fields to return.")
            }
        },
        insertIntoMongo = function(){ # Should we add Mongo capability and use this instead?
        },
        
        exportToDb = function(con,db,version = "001"){ 
             newTableName <- paste(db,".",name,"_",version,"_",format(now(),"%Y%m%d"),sep="")
             if(dbExistsTable(con, newTableName)){
                dbWriteTable(con, newTableName, data, append = T)
             } else {
               dbWriteTable(conn, newTableName, data) 
             }
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

