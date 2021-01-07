#####################################################################################
#
# PROBLEMS
#
# - load the "student" dataset:
#
#       student <- read.table("student.txt", sep=",", header=T)
#
# - discretize the attributes G1, G2, and G3 into five bins:
#   fail[0,9], sufficient[10,11], satisfactory[12,13], good[14,15], excellent[16,20]
#
# - train different models to predict secondary school student performance
#   (the target variable is the "G3" attribute)
#
# - evaluete trained models using different strategies
#
# - compare the results
#
#####################################################################################
#
# - load the "vehicle" dataset:
#	
#       vehicle <- read.table("vehicle.txt", sep=",", header = T)
#
# - train different models to classify a given silhouette as one of four types 
#   of vehicle (the target variable is the "Class" attribute)
#
# - evaluete trained models using different strategies
#
# - compare the results
#
#####################################################################################
#
# - compare knn's performance across different libraries on the vehicle problem using
#   10 fold cross validation. What do you observe? Explain.
#
#   Visualize the k with respect to knn's performance for k=1..20.
#   Perform a similar visualization for SVMs and the C parameter.
#####################################################################################