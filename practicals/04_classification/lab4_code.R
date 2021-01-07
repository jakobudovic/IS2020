#
#
# INTRODUCTION TO CLASSIFICATION
#
#

#
# Work plan:
#
# - Read in the data
# - Split the data into training and test sets
# - Build a model using the training set
# - Evaluate the model using the test set
#



# Please download the data file "players.txt" into a local directory
# then set that directory as the current working directory of R.
# You can achive this using the "setwd" command or by selecting "File -> Change dir..."


# We are going to use the packages: rpart, and pROC. 
# Make sure that the packages are installed.
#
# You install a package in R with the function install.packages():
#
#     install.packages("pROC")
#     library(pROC)
#
# To install packages without root access:
#
#     install.packages("pROC", lib="/mylibs/Rpackages/")
#     library(pROC, lib.loc="/mylibs/Rpackages/")
#

# Read in the dataset 
players <- read.table("players.txt", header = T, sep = ",")

# Get the summary statistics of the data
summary(players)



############################################################################
#
# ATTENTION!!!!!!!!!
#
# The "id" attribute uniquely identifies a player within our data set. 
# This attribute cannot be used to classify new players as they will 
# have different id numbers.
# 
# Before we continue, we have to remove the "id" attribute from our data set!
#
############################################################################


players$id <- NULL
names(players)


############################################################################
#
#
# Please make sure that the "id" attribute has been removed!!!!!!!!!!
#
#
############################################################################






############################################################################
#
# Example 1
#
# Prediction of playing position
#
############################################################################


#
# We want to build a model to predict a player's playing position 
# with respect to the given player's statistics.
#
# The target variable "position" is discrete - we term this a classification task.   
#
# We aim to verify whether or not it is possible to use historical data to predict
# playing positions for new players. 
#

# We are going to split the data into a training and testing data set.
# The training data set consists of players that ended their careers before 1999.
# The test data set consists of players that began their careers after 1999.

learn <- players[players$lastseason <= 1999,]
test <- players[players$firstseason > 1999,]

# We used the "firstseason" and "lastseason" attributes to split the data.
# Therefore the attributes are not going to contribute to the modelling task, so
# we will remove them.

learn$firstseason <- NULL
learn$lastseason <- NULL

test$firstseason <- NULL
test$lastseason <- NULL

# We inspect the target values distribution in the defined data sets.
# (number of examples and frequency of individual classes)

nrow(learn)
table(learn$position)

nrow(test)
table(test$position)




#
#
# MAJORITY CLASSIFIER
#
#


# The majority class is the class with the highest number of training examples
which.max(table(learn$position))

majority.class <- names(which.max(table(learn$position)))
majority.class

# The majority classifier classifies all test instances into the majority class.
# The accuracy of the majority class
sum(test$position == majority.class) / length(test$position)




#
#
# DECISION TREES
#
#


# load the "rpart" package that implements decision trees in R
library(rpart)

# Fit a decision tree
dt <- rpart(position ~ ., data = learn)

#
# The first argument of the rpart function is a model formula that indicates 
# the functional form of the model. The "~" symbol stands for "modeled as".
#
# The formula "position ~ . " states that we want a model that predicts the 
# target attribute "position" using all other attributes present in the data 
# (which is the meaning of the "." character).
# 
# If we wanted, for example, a model for the "position" attribute as a function 
# of the attributes "height", "width", and "pts", we should have 
# indicated the formula as "position ~ height + weight + pts".  
# 
# The second argument of the rpart function provides the training examples.
#
# Hyperparameters - what are they?

# The content of the dt object 
dt

# A graphical representation of our tree
plot(dt)
text(dt, pretty = 1)

# To navigate the tree, start from the root node. If the attribute of the 
# observation satisfies the logical test, then navigate down the left branch.
# Otherwise navigate down the right branch. Continue until a leaf node is reached.
# A leaf node represents a class label (or class label distribution). 


# Observed values in the test samples (the ground truth)
observed <- test$position
observed


# The "predict" function returns a vector of predicted responses from a fitted tree.
# The "type" argument denotes the form of predicted value returned. The setting
# type = "class" will cause the response in a form of a vector of predicted 
# class labels.

predicted <- predict(dt, test, type = "class")
predicted

# confusion matrix
t <- table(observed, predicted)
t

# The diagonal elements in the confusion matrix represent the number of correctly 
# classified test instances

# The classification accuracy is the fraction of correctly classified test instances
sum(diag(t)) / sum(t)


# Let's write a function that calculates the classification accuracy
CA <- function(observed, predicted)
{
  t <- table(observed, predicted)
  
  sum(diag(t)) / sum(t)
}

# Function call
CA(observed, predicted)




# The setting type = "prob" will cause the predict function to 
# provide its answer in the form of a matrix with class probabilities.

predMat <- predict(dt, test, type = "prob")
predMat

# A matrix of the observed class probabilities (the ground truth) 
# (the real class has the probability of 1, others have 0)
obsMat <- model.matrix( ~ position-1, test)
obsMat


# IMPORTANT: check whether the columns refer to the same attributes
# (if not, change the order of columns)
colnames(predMat)
colnames(obsMat)


# The Brier score
brier.score <- function(observedMatrix, predictedMatrix)
{
  sum((observedMatrix - predictedMatrix) ^ 2) / nrow(predictedMatrix)
}

brier.score(obsMat, predMat)




############################################################################
#
# Example 2
#
# Does a player make at least 80% of free-throws attempted?
#
############################################################################

#
# It is a binary problem (the target variable is discrete with values YES and NO). 
#

#
# We are going to create a data set using the players data.
#

# Get the players who have attempted at least one free throw during the career.
bin.players <- players[players$fta > 0,]

# Free-throw success rate
rate <- bin.players$ftm / bin.players$fta

# Create a discrete attribute "ftexpert" that will be our target variable.
ftexpert <- vector()
ftexpert[rate >= 0.8] <- "YES"
ftexpert[rate < 0.8] <- "NO"
bin.players$ftexpert <- as.factor(ftexpert)

# Remove the "fta" and "ftm" attributes.
bin.players$fta <- NULL
bin.players$ftm <- NULL

summary(bin.players)



# Split the data into two disjoint sets, one for training and one for testing.
bin.learn <- bin.players[1:1500,]
bin.test <- bin.players[-(1:1500),]

# Inspect the distribution of the class values in the data sets.
table(bin.learn$ftexpert)
table(bin.test$ftexpert)


# Train a decision tree
dt2 <- rpart(ftexpert ~ ., data = bin.learn)
plot(dt2)
text(dt2, pretty = 1)


bin.observed <- bin.test$ftexpert
bin.predicted <- predict(dt2, bin.test, type="class")


table(bin.observed, bin.predicted)


# Classification accuracy of the decision tree
CA(bin.observed, bin.predicted)



# The sensitivity of a model (recall)
Sensitivity <- function(observed, predicted, pos.class)
{
  t <- table(observed, predicted)
  
  t[pos.class, pos.class] / sum(t[pos.class,])
}

# The specificity of a model
Specificity <- function(observed, predicted, pos.class)
{
  t <- table(observed, predicted)
  
  # identify the negative class name
  neg.class <- which(row.names(t) != pos.class)
  
  t[neg.class, neg.class] / sum(t[neg.class,])
}

# let's say that the value "YES" is our positive class...

Sensitivity(bin.observed, bin.predicted, "YES")
Specificity(bin.observed, bin.predicted, "YES")

#
# ROC curve
#

library(pROC)

bin.predMat <- predict(dt2, bin.test, type = "prob")

# Remember, we treat the value "YES" as the positive class
rocobj <- roc(bin.observed, bin.predMat[,"YES"])
plot(rocobj)

names(rocobj)

cutoffs <- rocobj$thresholds
cutoffs

# identify the best cut-off value (for example, by minimazing the distance from the point (0,1))
tp = rocobj$sensitivities
fp = 1 - rocobj$specificities.

dist <- (1-tp)^2 + fp^2
dist
which.min(dist)

best.cutoff <- cutoffs[which.min(dist)]
best.cutoff

# the selected cut-off value has impact on the model's performance
predicted.label <- factor(ifelse(bin.predMat[,"YES"] >= best.cutoff, "YES", "NO"))

table(bin.observed, predicted.label)
CA(bin.observed, predicted.label)
Sensitivity(bin.observed, predicted.label, "YES")
Specificity(bin.observed, predicted.label, "YES")
