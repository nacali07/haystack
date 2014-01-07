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
cat("Haystack: 2014-01-05\n")
require(lubridate)

sourceFunctions<-function(homedir){
  allFiles<-grep(".*\\.r",list.files(paste(homedir,"functions",sep="/"),full.names=T),value=T)
  for(i in allFiles){
    cat(paste("Reading ",i,"...",sep=""))
    source(i)
    cat("Done!\n")
  }
}
pw<-function(x,a,b){
  return( pmax(pmin(x,b),a) - a )
}



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
       , homeDir = "character"   
       , performance = 'list'
       ),
      methods = list(
        addDataset = function(x,dsname){
            datasets[[dsname]] <<- x 
         },
                    
         saveRds = function(){
            if(is.null(homeDir)){
                stop("Please set homedir before saving: e.g. x$setHomeDir('~/path')")
            }
            if (is.null(name)){
                stop("Please set name before saving: e.g. x$setName('data76') or x$name<-'data76'")
            }
            newfile<-paste(homeDir,"models/",name,".rds",sep="")
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
         score = function(dsname){
             print(names(datasets))
             if(! dsname %in% names(datasets)){
               errorInvalidDataset(names(datasets))                                 
             } else {
               pname<-paste("pred_",name,sep="")
                datasets[[dsname]]$data[,pname] <<- 
                 scorer(datasets[[dsname]], model)
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
         listAvailablePreds = function(dsname){
           grep("pred_",colnames(datasets[[dsname]]$data),value=T)
         },
         runPerformance = function(dsname,pred,target){
            require(ROCR)
            # Use ROCR to do the calculations
            pred<-prediction(
                              datasets[[dsname]]$data[,pred]
                            , datasets[[dsname]]$data[,target])
      
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
            performance[[pred]] <<- newPerformance
         },
         runModel        = function(dsname){
               model <<- estimator(x=datasets[[dsname]],formula=formula)
         },
         d=function(dsname,rows=NULL,cols=NULL){
           datasets[[x]]$data[rows,cols]
         },
         exportProductionModel = function(){
              tmp <- .self$copy()
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
                               estimate = NULL,
                               exportProductionModel = NULL))

              return(tmp)
          }
       ))

HaystackQuery<-setRefClass('HaystackQuery'
     ,fields=c(con="function",qry="character",execution="function")
     )

HaystackJsonQuery<-setRefClass('HaystackJsonQuery'
                       ,contains="HaystackQuery"
                       ,methods=list(
                         initialize=function(...){
                           initFields(...)
                           callSuper(...)
                           execution <<- function(){
                             require(jsonlite)
                             require(httr)
                             fromJSON(con)
                           }
                         }))

## Provides ability to connect to a MySQL database
## and run queries
## call: x<-HaystackMySqlQuery$new(query="select * from x limit 1;",
##                     user='bill',pwloc='~/pw/pw.txt',dbname='sandbox',host='localhost')
## update the call:   x$dbname <- 'newdb'
## update the query:  x$dbQuery  <- "select * from x limit 2;"
## execute the query: newdata  <- x$execute()

HaystackProject<-setRefClass('HaystackProject'
                             ,fields=c(models='list',datasets='list')
                             ,methods=list(
                               addModel = function(x,modelname){
                                 models[[modelname]] <<- x
                               },
                               addDataset = function(x,dsname){
                                 datasets[[dsname]] <<- x
                               }))

HaystackMySqlQuery<-setRefClass('HaystackMySQLQuery'
                        ,fields=c(
                             user  ='character'
                           , pwloc ='character'
                           , dbname='character'
                           , host  ='character'
                           , qry   ='character'   
                              )        
                        ,methods=list(
                            execute = function(conn){
                              require(RMySQL)
                              stopifnot(!is.null(qry))                              
                              dbGetQuery(conn,qry)
                            },
                            closeCon=function(conn){dbDisconnect(conn)},
                            con =  function(){
                                ## example:  connection('bill','~/pw/pw.txt')
                                ## where pwloc is a text file with db password on line 1
                                ## make sure this file is only readable by the user
                                ## if you have forwarded your mysql port, you can use localhost
                                ## Note: Assumes that the mysql port is the default (3306)
                                if(!is.null(user) && !is.null(pwloc)){
                                  require(RMySQL)
                                  m<-dbDriver("MySQL")
                                  pipecon<-pipe(paste("cat",pwloc))
                                  password<-readLines(pipecon)
                                  close(pipecon)
                                  conn<-dbConnect(m,user=user,password=password,host=host,dbname=dbname)
                                  return(conn)
                                }  else {
                                  stop("Please enter a valid user and pwloc. See docstring for help.")
                                }
                            }                          
                          ))

RestoreHaystack<-function(name,home){
   stopifnot(!is.null(name))
   stopifnot(!is.null(home))

   homeDir <<- paste(gsub("\\/$","",homeDir),"/",sep="")
   filename<-paste(homeDir,"datasets/",name,".rds",sep="")
   return(readRDS(filename))
}

RestoreHaystackModel<-function(name,home){
   stopifnot(!is.null(name))
   stopifnot(!is.null(home))

   homeDir <<- paste(gsub("\\/$","",home),"/",sep="")
   filename<-paste(homeDir,"models/",name,".rds",sep="")
   return(readRDS(filename))
}

Haystack<-setRefClass('Haystack'
      ,fields=c(
            data          = "data.frame"  
          , dbQuery       = "ANY"    #returns data.frame
          , jsonParser    = "function"   
          , snapshotDate  = "Date"
          , notes         = "character"
          , homeDir       = "character"
          , name          = "character"
          , key           = "character"
          , description   = "character")
      ,methods=list(
        setName = function(x){
            name <<- x
        },            
        setHomeDir = function(x){
            homeDir <<- paste(gsub("\\/$","",x),"/",sep="")
        },
        saveRds = function(){
            if(is.null(homeDir)){
                stop("Please set homedir before saving: e.g. x$setHomeDir('~/path')")
            }
            if (is.null(name)){
                stop("Please set name before saving: e.g. x$setName('data76') or x$name<-'data76'")
            }

            newfile <- paste(homeDir,"datasets/",name,".rds",sep="")
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
              conn<-dbQuery$con()
              data <<- dbQuery$execute(conn)
              dbQuery$closeCon(conn)
              addNote("Data obtained from db.")
        },
        setJsonParser  = function(x){
          jsonParser <<-x$copy()
          addNote("JSON Parser set.")
        },
        setDbQuery = function(x){
          dbQuery <<-x$copy()
          addNote("Query Set.")
        },
        getpred = function(key=NULL,names=NULL){
"
Get predctions.
Call: x$getpred(key='fileno',names='pred')
"
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

