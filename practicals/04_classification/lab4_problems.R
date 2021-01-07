######################################################################################################
#
# Exam problem
#
# The classifier was tested on a two class problem and achieved the following confusion matrix 
# on a testing set:
#
# +----------------------+-----+-----+
# | True \ classified as |     |     |
# | class \              |  0  |  1  |
# +----------------------+-----+-----+
# | 0                    | 300 |   0 | 
# +----------------------+-----+-----+
# | 1                    |  80 | 120 |
# +----------------------+-----+-----+         
# 
# Calculate the following:
# 
# a) the classification accuracy
#
# b) default accuracy (assume that the most frequent class in the testing set is also the most 
#    frequent class in the training set)
#
# c) sensitivity
# 
# d) specificity
#
######################################################################################################

######################################################################################################
#
# Exam problem
#
# The classifier classified four testing instances in a 4-class problem.
# The table below shows the prediction probability distribution for each of testing instances:
#
#	             
# actual class  | predicted probs:    C1    C2    C3    C4
# --------------+-----------------------------------------
# C4            |                   0.50  0.25  0.00  0.25
# C2            |                   0.50  0.25  0.25  0.00
# C1            |                   0.75  0.00  0.25  0.00
# C2            |                   0.25  0.50  0.00  0.25
#
#
# Assume that the class probability distribution in the testing data set is equal to the distribution 
# in the training set. Calculate the following:
# 
# a) the average Brier score
#
# b) the average information score
# (defined as: 1/n * sum (-log2(p(prior)) + log2(p(predicted)))). p(prior) is the prior probability and
# p(predicted) is the probability of the prediction for a given class.
#
# c) the information score of a default classifier for the second testing instance
#
######################################################################################################


######################################################################################################
#
# PRACTICAL PROBLEMS
#
######################################################################################################
#
# - load the movies dataset
# 
# - transform the attributes "Action", "Animation", "Comedy", "Drama", "Documentary", "Romance",
#   and "Short" into factors
#
# - IMPORTANT: remove the "title" and "budget" attributes from the dataset
#
# - split the data into two sets:
#     training set, which contains information about the movies made before the year 2004
#     test set, which contains information about the movies made in the year 2004 or later
#     then remove the "year" attribute 
#
# - build a decision tree to predict whether or not a movie is a comedy.
#
# - evaluate that model on the test data
# 
#
######################################################################################################
#
# - load the tic-tac-toe training and test datasets:
# 
#	learn <- read.table("tic-tac-toe-learn.txt", header=T, sep=",")
#	test <- read.table("tic-tac-toe-test.txt", header=T, sep=",")
#
# - train a decision tree (the "Class" attribute is our target variable) and evaluate that model
#   on the test data
# 
# - plot the ROC curve for your model (the value "positive" is our positive class) 
#
# - try to improve your model with ROC analysis
#
######################################################################################################

