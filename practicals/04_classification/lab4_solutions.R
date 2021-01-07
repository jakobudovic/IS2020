#
# SOLUTIONS
#

#######################################################################################
#
# Exam problem solution:	
#
#
# a) the classification accuracy:
#
#     (300+120)/(300+0+80+120)
#     = 0.84
#
#
# b) default accuracy: 
#
#    the majority class is 0     
#
#    confusion matrix (default classifier):
#
#        0   1
#    --+---+---+								 
#    0 |300| 0 |
#    --+---+---+
#    1 |200| 0 |
#    --+---+---+
#
#    300/500 = 0.6
#
# c) sensitivity (0 is treated as the positive class)
#
#    Sensitivity = TP/POS = 300 / 300 = 1
#
# d) specificity (0 is treated as the positive class)
#
#    Specificity = TN/NEG = 120 / 200 = 0.6
#
#######################################################################################

#######################################################################################
#
# Exam problem solution:	
#	
# a) the average Brier score:	
#	
#	 (
#     (0.00-0.50)^2 + (0.00-0.25)^2 + (0.00-0.00)^2 + (1.00-0.25)^2 + 
#     (0.00-0.50)^2 + (1.00-0.25)^2 + (0.00-0.25)^2 + (0.00-0.00)^2 + 
#     (1.00-0.75)^2 + (0.00-0.00)^2 + (0.00-0.25)^2 + (0.00-0.00)^2 + 
#     (0.00-0.25)^2 + (1.00-0.50)^2 + (0.00-0.00)^2 + (0.00-0.25)^2
#    )/4
#    = 0.5625
#
#	
# b) the average information score:	
#
#    prior probabilities: P(C1) = 0.25, P(C2) = 0.5, P(C3) = 0.0, P(C4) = 0.25
#	
#	 posterior probabilities for the correct class 
#                                        example 1: P'(C4)=0.25
#                                        example 2: P'(C2)=0.25
#                                        example 3: P'(C1)=0.75
#                                        example 4: P'(C2)=0.5
#	
#	
#   (
#    -log2(0.25)+log2(0.25) + 
#    (-log2(0.5)+log2(0.25)) + 
#    (-log2(0.25)+log2(0.75)) + 
#    (-log2(0.5)+log2(0.5))
#   )/4
#   = 0.25
#
#
# c) the information score of a default classifier for the second testing instance:
#
#    if the implementation of a default classifier is such that it assigns an example to the majority class with probability 1.0
#
#    	-log2(0.50)+log2(1.0) = 1
# 
#    if the implementation of a default classifier is such that it returns for every example the prior probability distribution
#
#    	-log2(0.50)+log2(0.5) = 0
#	
#######################################################################################


library(rpart)

CA <- function(observed, predicted)
{
  t <- table(observed, predicted)
  
  sum(diag(t)) / sum(t)
}

Sensitivity <- function(observed, predicted, pos.class)
{
  t <- table(observed, predicted)
  
  t[pos.class, pos.class] / sum(t[pos.class,])
}

Specificity <- function(observed, predicted, pos.class)
{
  t <- table(observed, predicted)
  
  # identify the negative class name
  neg.class <- which(row.names(t) != pos.class)
  
  t[neg.class, neg.class] / sum(t[neg.class,])
}

brier.score <- function(observedMatrix, predictedMatrix)
{
  sum((observedMatrix - predictedMatrix) ^ 2) / nrow(predictedMatrix)
}



mdata <- read.table("movies.txt", header = T, sep = ",")
for (i in 18:24)
  mdata[,i] <- as.factor(mdata[,i])

mdata$title <- NULL
mdata$budget <- NULL

learn <- mdata[mdata$year < 2004,]
test <- mdata[mdata$year >= 2004,]

learn$year <- NULL
test$year <- NULL

dt <- rpart(Comedy~., learn)
plot(dt)
text(dt, pretty = 0)

observed <- test$Comedy
predicted <- predict(dt, test, type="class")

CA(observed, predicted)
Sensitivity(observed, predicted, "1")
Specificity(observed, predicted, "1")


predMat <- predict(dt, test, type = "prob")
obsMat <- model.matrix(~Comedy-1, test)

colnames(predMat)
colnames(obsMat)

brier.score(obsMat, predMat)




library(pROC)

learn <- read.table("tic-tac-toe-learn.txt", header=T, sep=",")
test <- read.table("tic-tac-toe-test.txt", header=T, sep=",")

# the "Class" is our target variable 
dt <- rpart(Class ~ ., data = learn)
plot(dt);text(dt)

observed <- test$Class
predicted <- predict(dt, test, type = "class")
CA(observed, predicted)

Sensitivity(observed, predicted, "positive")
Specificity(observed, predicted, "positive")


predMat <- predict(dt, test, type = "prob")
obsMat <- model.matrix(~Class-1, test)

colnames(predMat)
colnames(obsMat)

# the second column contains the probabilities for our positive class

rocobj <- roc(observed, predMat[,"positive"])
plot(rocobj)



tp = rocobj$sensitivities
fp = 1 - rocobj$specificities
dist <- (1-tp)^2 + fp^2
best.cutoff <- rocobj$thresholds[which.min(dist)]
best.cutoff

predicted.label <- factor(ifelse(predMat[,2] >= best.cutoff, "positive", "negative"))
CA(observed, predicted.label)
Sensitivity(observed, predicted.label, "positive")
Specificity(observed, predicted.label, "positive")

########################################################################################
