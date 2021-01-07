#######################################################################################################
#
# PROBLEMS
#
#######################################################################################################
#
# - The "tic-tac-toe" dataset encodes the complete set of possible board configurations at the end of 
#   tic-tac-toe games, where "x" is assumed to have played first. The target concept is "win for x" and
#   it is encoded in the "Class" attribute with two values (positive and negative). 
#   Attributes "tl", "tm", "tr", "ml", "mm", "mr", "bl", "bm", and "br" represent the left, middle, and
#   and right square in the top, middle, and bottom row of the game board. 
#
#   Load the tic-tac-toe dataset using the following command:	
#	
#		ttt <- read.table("tic-tac-toe.learn.txt", header=T, sep=",")
#
# - use several attribute estimation measures (in classification) to determine the most important 
#   square in the game board.
#
#######################################################################################################
#
# - load the players dataset
# 
#	players <- read.table("players.txt", header = T, sep = ",")
#
# - divide the original dataset into training and test sets
#
# - train several models (using the entire feature set, using a selected  subset of attributes) to 
#   predict the target variable "position" and evaluate them on the test set
#  
#
#######################################################################################################

# How similar are top k features when you compare a ReliefF-type algorithm
# with e.g., RandomForest-based importances?
# Plot k on x and the jaccard index on y axis. (jaccard := |intersection|/|union|)
# What do you observe?
#######################################################################################################




