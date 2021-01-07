# Please download datasets "spamlearn.txt" and "spamtest.txt" 
# into a local directory. Set that directory as the current working directory of R.
# You can achive this using the "setwd" command or by selecting "File -> Change dir..."

# We are going to use the following packages:  rpart, CORElearn, e1071, randomForest, 
# kernlab, and nnet. Make sure that the packages are installed.


# You install a package in R with the function install.packages():
#
#     install.packages(c("ggplot2",rpart", "rpart.plot", "CORElearn", "e1071", "randomForest", "kernlab","caret"))
#
# To install packages without root access:
#
#     install.packages(install.packages(c("rpart", "rpart.plot", "CORElearn", "e1071", "randomForest", "kernlab")), lib="path to my folder")
#     library(CORElearn, lib.loc="path to my folder")
#


#
#
# Linear regression
#
#
library(ggplot2)

dataCars <- read.table("cars.txt", sep=',',header = T, row.names = NULL)
plot(dataCars$price, dataCars$horsepower)
qplot(horsepower, price, data = dataCars, geom = c("point","smooth"))
linearReg <- lm(horsepower ~ price, data = dataCars)
linearReg
plot(horsepower ~ price, data = dataCars)
abline(linearReg)

#
#
# Nonlinear regression
#
#

nonlinearReg <- nls(horsepower ~ x*price^(1/2), data=dataCars, start=c(x=1))
plot(dataCars$price, dataCars$horsepower)
o = order(dataCars$price)
lines(dataCars$price[o], predict(nonlinearReg)[o])

nonlinearReg2 <- nls(horsepower ~ x*price^y, data=dataCars, start=c(x=1, y=1))
lines(dataCars[,'price'][o], predict(nonlinearReg2)[o])


#
#
# Classification
#
#

learn <- read.table("spamlearn.txt", header = T)
test <- read.table("spamtest.txt", header = T)


# the target variable is the "Class" attribute
observed <- test$Class


#
#
# KNN
#
#

# fit a model using the "CORElearn" library

library(CORElearn)
cm.knn <- CoreModel(Class ~ ., data = learn, model="knn", kInNN = 10)
predicted <- predict(cm.knn, test, type="class")

# The classification accuracy
CA <- function(observed, predicted)
{
  t <- table(observed, predicted)
  
  sum(diag(t)) / sum(t)
}

CA(observed, predicted)

# How does this compare to the majority classifier?
table(observed)
sum(observed=='spam')/length(observed)

## using cross-validation to determine top k on the learn data?
## key idea: iteratively hide parts of the train set to get more robust estimate of performance
## for a given set of hyperparameters.

folds <- 3
foldIdx <- cvGen(nrow(learn), k=folds)
evalCore<-list()
overallScores <- c()

## we will check K in range from 1 to 10.
for (neigh in 1:10){
  
  ## for each k, we perform a cross validation on the train (learn) set.
  for (j in 1:folds) {
    
    ## some log
    print(paste0("Evaluation of k = ",neigh," and fold: ",j))
    
    ## select data from train and test (within the learn data set!!)
    dTrain <- learn[foldIdx!=j,]
    dTest  <- learn[foldIdx==j,]
    
    ## train a knn with a given parameter k
    modelCore <- CoreModel(Class~., dTrain, model="knn", kInNN = neigh) 
    
    ## predict on the test set (within the learn set)
    predCore <- predict(modelCore, dTest)
    
    ## compute the metrics
    evalCore[[j]] <- modelEval(modelCore, correctClass=dTest$Class,
                             predictedClass=predCore$class, predictedProb=predCore$prob )
    
    ## cleanup
    destroyModels(modelCore)
  }
  
  ## aggregate the results
  results <- gatherFromList(evalCore)
  
  ## get mean performance across all folds
  meanPerformances <- sapply(results, mean)
  
  ## add to the overallScores the accuracy. Note that the index corresponds to the k!
  overallScores <- c(overallScores, meanPerformances['accuracy'])
}

## best performing in CV shall be used as the final model
bestK <- which.max(overallScores)

## train the knn on whole learn data set
cm.knn <- CoreModel(Class ~ ., data = learn, model="knn", kInNN = bestK)

## predict on the test set
predicted <- predict(cm.knn, test, type="class")

## get the performance
CA(observed, predicted)

qplot(1:10, overallScores, xlab = "k", ylab = "Performance (acc)", geom = c("point","line"))

## Many times, libraries have in-built functionality!
performances <- c()
for (kParam in 1:10){
  cm.knn <- cvCoreModel(Class ~ ., data = learn, model="knn", kInNN = kParam, folds = 3)
  performances <- c(performances, cm.knn$avgs['accuracy'])
}
optK <- which.max(performances)
cm.knn <- CoreModel(Class ~ ., data = learn, model="knn", kInNN = optK)
predicted <- predict(cm.knn, test, type="class")
CA(observed, predicted)

#
#
# KNN for regression
#
#

cm.knnReg <- CoreModel(make ~ ., data=learn, model="regTree", modelTypeReg=7, minNodeWeightTree=Inf)
predicted <- predict(cm.knnReg, test, type="class")

MSE <- function(observed, predicted)
{
  sum((observed-predicted)^2)
}

MSE(test$make, predicted)
MSE(test$make, mean(test$make))


#
#
# DECISION TREES
#
#

# fit a model using the "rpart" library

library(rpart)
dt <- rpart(Class ~ ., data = learn)
plot(dt);text(dt)
predicted <- predict(dt, test, type="class")
CA(observed, predicted)

# obtaining decision rules from trees:
library(rpart.plot)
rpart.plot(dt)
rpart.rules(dt)
rpart.rules(dt, extra=4)

#
#
# RANDOM FOREST
#
#

# fit a model using the "CORElearn" library

library(CORElearn)
cm.rf <- CoreModel(Class ~ ., data = learn, model="rf")
predicted <- predict(cm.rf, test, type="class")
CA(observed, predicted)


# fit a model using the "randomForest" library

library(randomForest)
rf <- randomForest(Class ~ ., data = learn)
predicted <- predict(rf, test, type="class")
CA(observed, predicted)


# Some additional models

#
#
# NAIVE BAYES CLASSIFIER
#
#


# fit a model using the "CORElearn" library

library(CORElearn)
cm.nb <- CoreModel(Class ~ ., data = learn, model="bayes")
predicted <- predict(cm.nb, test, type="class")
CA(observed, predicted)


#
#
# SVM
#
#

# fit a model using the "e1071" library

library(e1071)

sm <- svm(Class ~ ., data = learn, cost = 100)
predicted <- predict(sm, test, type="class")
CA(observed, predicted)

library(caret)

# Scheme.
control <- trainControl(method="repeatedcv", number=3, repeats=3)

# Parameter grid.
grid <- expand.grid(C=1:10)

# Training
modelSVM <- train(Class~., data=learn, method="svmLinear", trControl=control, tuneGrid=grid)
predicted <- predict(modelSVM, test, type="raw")
CA(observed, predicted)

# Let's revisit the KNNs.
grid <- expand.grid(k=1:10)

# Training
control <- trainControl(method="repeatedcv", number=3, repeats = 3)
modelKNN <- train(Class~., data=learn, method="knn", trControl=control, tuneGrid=grid)
predicted <- predict(modelKNN, test, type="raw")
CA(observed, predicted)

# Random Search
control <- trainControl(method="repeatedcv", number=3, repeats=1, search="random", allowParallel=TRUE)
set.seed(1523) # repeatability
rf_random <- train(Class~., data=learn, method="rf", metric="Accuracy", tuneLength=5, trControl=control,verbose = TRUE)
qplot(rf_random$results$mtry,rf_random$results$Accuracy, geom = c("line","point"))


# parallelism options

set.seed(112233)
library(parallel) 
# Calculate the number of cores
no_cores <- detectCores() - 1

library(doParallel)
# create the cluster for caret to use
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)

# Random Search
control <- trainControl(method="repeatedcv", number=3, repeats=1, search="random", allowParallel=TRUE)
set.seed(1523) # repeatability
rf_random <- train(Class~., data=learn, method="rf", metric="Accuracy", tuneLength=5, trControl=control,verbose = TRUE)
qplot(rf_random$results$mtry,rf_random$results$Accuracy, geom = c("line","point"))

stopCluster(cl)
registerDoSEQ()
