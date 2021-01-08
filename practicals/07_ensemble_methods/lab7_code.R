################################################################
#
# Combining machine learning algorithms
#
################################################################

#install.packages("CORElearn")
library(CORElearn)

vehicle <- read.table("vehicle.txt", sep=",", header = T)
summary(vehicle)

set.seed(8678686)
sel <- sample(1:nrow(vehicle), size=as.integer(nrow(vehicle)*0.7), replace=F)
learn <- vehicle[sel,]
test <- vehicle[-sel,]

table(learn$Class)
table(test$Class)

CA <- function(observed, predicted)
{
  t <- table(observed, predicted)
  t
  sum(diag(t)) / sum(t)
}

#
# A simple tree baseline.
#


#
# Voting
#

modelDT <- CoreModel(Class ~ ., learn, model="tree")
modelNB <- CoreModel(Class ~ ., learn, model="bayes")
modelKNN <- CoreModel(Class ~ ., learn, model="knn", kInNN = 2)

predDT <- predict(modelDT, test, type = "class")
table(test$Class, predDT)
result.caDT <- CA(test$Class, predDT)
result.caDT

predNB <- predict(modelNB, test, type="class")
result.caNB <- CA(test$Class, predNB)
result.caNB

predKNN <- predict(modelKNN, test, type="class")
result.caKNN <- CA(test$Class, predKNN)
result.caKNN

# combine predictions into a data frame
pred <- data.frame(predDT, predNB, predKNN)
pred

# the class with the most votes wins
voting <- function(predictions)
{
  res <- vector()
  
  for (i in 1 : nrow(predictions))  	
  {
    vec <- unlist(predictions[i,])
    res[i] <- names(which.max(table(vec)))
  }
  
  factor(res, levels=levels(predictions[,1]))
}
pred[1,]
predicted <- voting(pred)
result.voting <- CA(test$Class, predicted)
result.voting

#
# Weighted voting
#

predDT.prob <- predict(modelDT, test, type="probability")
predNB.prob <- predict(modelNB, test, type="probability")
predKNN.prob <- predict(modelKNN, test, type="probability")

# combine predictions into a data frame
pred.prob <- result.caDT * predDT.prob + result.caNB * predNB.prob + result.caKNN * predKNN.prob
pred.prob

# We can visualize the joint output space!
heatmap(pred.prob)

# pick the class with the highest score
highest <- apply(pred.prob, 1, which.max)
classes <- levels(learn$Class)
predicted <- classes[highest]

result.wvoting <- CA(test$Class, predicted)
result.wvoting



#
# Bagging
#

#install.packages("ipred")
library(ipred)

bag <- bagging(Class ~ ., learn, nbagg=14)
bag.pred <- predict(bag, test, type="class")
result.bagging <- CA(test$Class, bag.pred)
result.bagging


#
# Random forest as a variation of bagging
#

# install.packages("randomForest")
library(randomForest)

rf <- randomForest(Class ~ ., learn)
rf.predicted <- predict(rf, test, type = "class")
result.rf <- CA(test$Class, rf.predicted)
result.rf


#
# Boosting
#

# install.packages("adabag")
library(adabag)

bm <- boosting(Class ~ ., learn)
predictions.bm <- predict(bm, test)
names(predictions.bm)

predicted.bm <- predictions.bm$class
result.boosting <- CA(test$Class, predicted.bm)
result.boosting

#
# Caret package
#

# install.packages("caret")
library(caret) # DONT run this part, it won't stop  !!
caretModel <- train(Class ~ ., learn, method="xgbLinear", eta=1, verbose=1)
pred <- predict(caretModel, test)
result.xgb <- CA(test$Class, pred)
result.xgb

# Let's visualize currently considered ensemble methods.
performances <- c(result.rf,
                  result.bagging,
                  result.wvoting,
                  result.boosting,
                  result.caDT,
                  result.caKNN,
                  result.caNB,
                  result.xgb)

algo.names <- c("RF","Bagging","Weighted vote","Boosting","DT","KNN","NB","XGB")
ensemble.model <- as.factor(c(1,1,1,1,0,0,0,1))
result.vec <- data.frame(performances, algo.names,ensemble.model)
reordering <- order(result.vec$performances)
result.vec <- result.vec[reordering,]
rownames(result.vec) <- NULL
result.vec

library(ggplot2)
positions <- as.vector(result.vec$algo.names)
ggplot(data=result.vec, aes(x=algo.names, y=performances, color = ensemble.model)) +
   geom_point(size = 10, shape = 4) +
   scale_x_discrete(limits = positions) +
   ylim(0.5,0.8) +
   xlab("Ensemble type") +
   ylab("Accuracy") +
   geom_hline(yintercept=max(performances), color = "darkgreen") +
   geom_hline(yintercept=min(performances), color = "black") +
   title("Performance comparison") + 
   geom_text(label = positions,nudge_x = 0.2, nudge_y = -0.01) +
   theme_bw() +
   theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


#
# Cross-validation
#

# the library ipred is needed to perform cross-validation
library(ipred)

# Tell the cross-validation which model to use
mymodel.coremodel <- function(formula, data, target.model){
  CoreModel(formula, data, model=target.model)
}
# Tell the cross-validation how to obtain predictions
mypredict.generic <- function(object, newdata){
  predict(object, newdata, type = "class")
}
# force the predict function to return class labels only and also destroy the internal representation of a given model
mypredict.coremodel <- function(object, newdata) {
  pred <- predict(object, newdata)$class; destroyModels(object); pred
}
cvError <- errorest(Class~., data=learn, model = mymodel.coremodel, predict = mypredict.coremodel, target.model = "tree")
1 - cvError$error

