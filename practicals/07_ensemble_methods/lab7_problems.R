###
# Problem: using the mushroom.txt data set, compare how ensemble classifiers
# perform when compared to simple decision tree models. How big is the difference
# in performance? What do you observe in terms of learning times? Predict gill.size attribute!
# Visualize the tuples (model, time spent) as a barplot 
# (HINT: you can use Sys.time() to crete checkpoints and measure time.)
###

###
# Problem: create a k-fold cross-validation function that can be used without importing external libraries
# The function should work in the following steps:
#     - Split the dataset (which should be provided as a function parameter) into k wholly distinct subsets
#     - Train and test a model k times. Each time, use 1 subset as the testing set and the combination of all other subsets as a training set.
#	      - Each subset should be used as a testing set once		
#	  - Save the classification accuracy of each model trained in this way and return the average as the result of the function 
#
#
#	 You can also use a similar approach to manually implement bagging. The approach is similar:
#		 Training:	
#        - Instead of simply splitting the dataset, create M datasets by sampling (with replacement) from the original dataset (use the sample function with replace=T)
#		 	- The size and amount of the resampled datasets should be determined by a function parameter	
#		 - Train and save a model on each of these samples and return them as the function result
#		 Prediction:
#		 - Use each of the saved models to obtain a prediction on the test dataset (using the predict function)
#        - Use voting to combine the predictions into a final prediction
###


### Problem
#   Create various ensembles of logistic regression classifiers and evaluate their performance
#   on the vehicles data set. Determine the best ensemble strategy by using F1 measure.
###

