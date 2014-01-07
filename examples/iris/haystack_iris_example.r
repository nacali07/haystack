# Source the class definitions from local github repository
source("~/R/haystack/Haystack-0_04.r")

# Source functions specific to the clc project
sourceFunctions("~/R/haystack/examples/iris")


# Build MySQL Query object
baseSqlQuery   <- HaystackMySqlQuery$new(
    user   ='bill'
  , pwloc  ='~/pw/pw.txt'
  , dbname ='sandbox'
  , host   ='localhost')

## For the example:  Load Iris data to  running MySQL database.
data(iris)
con<-baseSqlQuery$con()
dbSendQuery(con,"drop table if exists sandbox.iris")
dbWriteTable(con,'sandbox.iris',iris,overwrite=T)
dbDisconnect(con)

# copy mysql object to one that will be used for the dev(elopment) data query.
# An export method which takes as an argument fields to be replaced in the
# new object would be nice.  Something like:
# devSqlQuery <- baseSqlQuery$clone(qry="select * from test")

# for now we make copies like this
devSqlQuery<-baseSqlQuery$copy()

# Put query from queries.r into the appropriate field of our object.
devSqlQuery$qry<-"select * from sandbox.iris"

# Create a data instance
# I wanted to call this Dataset, but I thought that might be too presumptuous.
dev<-Haystack$new()

# Set key parameters
dev$snapshotDate  <-as.Date(NA) # date object.  Often used in transformations.  Not needed in iris data.
                         # example:  dev$snapshotDate <- as.Date("2013-01-01")
dev$homeDir       <-'~/R/haystack/examples/iris' # location of two folders: ./datasets and ./models
dev$name          <-'dev' # name of dataset object
dev$key           <-'row_names' # key of dataset
dev$description   <-"Iris dataset used in the example scripts of the Haystack package."

dev$setDbQuery(devSqlQuery) # Places this query in the field:  dev$dbQuery
dev$readDb()                # Executes the query and puts data in the field: dev$data

# This shows you what you just received
summary(dev$data)
# Create model number 01.
# right hand side arguments are created by sourcing  ./function/*.r above.
m01<-HaystackModel$new()
m01$name          <-'rf.1'
m01$formula       <- irisRfFormula       # formula for rf model.  Takes one arguments:
                                     # x: Haystack Object
                                     
m01$estimator     <- rfEstimator    # a function.  Takes two arguments:
                                     # x: Haystack obj
                                     # formula: formula of the appropriate class, defaults to formula
                                     # input:Haystack object
                                     # returns: rf model
m01$scorer        <-predict.haystack.randomforest  # formula
                                     # a function.  Takes two arguments:
                                     # x: Haystack obj
                                     # model: model from a predictive model (output of randomforest, glm, lm, etc.)
                                     # returns: vector of predictions
m01$homeDir       <- '~/R/haystack/examples/iris'   # location of two folders: ./datasets and ./models

m01$addDataset(dev,'dev')            # Adds (or replaces) the named element in the datasets field of the model object
                                     # first argument is a Haystack object
                                     # second argument is the name you wish to use to refer to this dataset
                                     # dataset operations below act on the Haystack object itself, e.g.
                                     #   dev$dataset[['dev']]$data = dev$data, even after transformations below

m01$x_transformer <- x_transformer   # a function.  One input: x (Haystack obj)
                                     # no return value, but makes assignments to x$data$<fields>
m01$y_transformer <- y_transformer   # a function.  One input: x (Haystack obj)
                                     # no return value, but makes assignments to x$data$<fields>

m01$x_transform('dev')               # apply transformation defined above to dev dataset
m01$y_transform('dev')               # apply transformation defined above to dev dataset

summary(dev$data)

m01$runModel('dev')                  # run the model defined above on the dev datset

plot(m01$model)

m01$score('dev')                     # generate predictions on dev dataset

valSqlQuery<-devSqlQuery$copy()      # create a copy of the MySQL query so we can create another similar one.
valSqlQuery$qry<-"select * from sandbox.iris"              # a string with a valid MySQL query

val<-dev$copy()                      # create a copy of the dataset. Should build a method which does this more efficiently.
val$name          <-'val'
val$description   <-"Validation dataset for iris example."

val$setDbQuery(valSqlQuery)
val$readDb()

m01$addDataset(val,'val')            # Add the new dataset reference to the model
m01$x_transform('val')               # transform the indpendent variables in the same way it was done on the dev data
m01$y_transform('val')               # transform the dependent variable
m01$score('val')                     # generate predictions

p<-m01$listAvailablePreds('val')     # get a list of the names of the predictions
p

# even if you havent yet created the methods you want, you can still use the objects.
val$data[,p]
hist(val$data[,p])   # This will work!  Remember, the model is acting on the data references!
varImpPlot(m01$model)
partialPlot(m01$model,m01$datasets[['dev']]$data,"Sepal_Width",which.class="1")


#m01$runPerformance('dev',target='payerf',pred=p)  # this needs to be fixed...


# Other functionality in various states of completion
### runPerformance
### saving datasets and models to the disk
### I would like to save the objects to a searchable database in some way, but that will happen in the future.

