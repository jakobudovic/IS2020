###
#
# CLUSTERING
#
###

# Load the dataset
cities <- read.table("worldcities.csv", sep=",", header = T)
summary(cities)
x <- cities$lng
y <- cities$lat
x <- x / max(x)
y <- y / max(y)
plot(x,y)
df = data.frame(x, y)

# KMeans clustering
results <- kmeans(df, centers=6)
results
plot(x, y, col=results$cluster)

# Are clusters aligned with classes?
library(ggplot2)
library(gridExtra)
# install.packages("gridExtra")
data(iris)
subset <- iris[,1:2]

# classes
p1<-qplot(subset$Sepal.Length,subset$Sepal.Width,color = iris$Species, main = "Ground truth") + theme_bw()

resultsIris <-kmeans(subset, centers = 3)
assignments <- as.factor(resultsIris$cluster)
p2<-qplot(subset$Sepal.Length,subset$Sepal.Width,color = assignments, main = "Clustering") + theme_bw()
gridExtra::grid.arrange(p1, p2, nrow = 2)

# Exercise -> try to cluster the iris data according to Length and Width!

# Try to fit models and see what can be learnt best!
install.packages("mclust")
library(mclust)
fit <- Mclust(df)
# summarize the fits
plot(fit)
summary(fit)

#Hierarchical clustering
distMat = dist(df)
resultsH <- hclust(distMat)

plot(resultsH)
clusters <- cutree(resultsH, 6)
plot(x, y, col=clusters)


###
#
# ATTRIBUTE EVALUATION
#
###


# Attribute evaluation functions are implemented in the package "CORElearn"

# Load the library
#install.packages("CORElearn")
library(CORElearn)


#
# Attribute estimation in classification
#



#
# Example 1
#

mr <- read.table("mushroom.txt", sep=",", header=T)
summary(mr)

qplot(mr$edibility, ylab="Number of species", main="Edibility of mushrooms", geom = c("bar"))


# the attrEval function evaluates the quality of the attributes/dependent variables 
# specified by the formula. Parameter formula is used as a mechanism to select
# attributes and prediction variable(class). The simplest way is to specify just 
# response variable: "Class ~ .". In this case all other attributes in the data set 
# are evaluated with the selected heuristic method.

# attribute evaluation using information gain
sort(attrEval(edibility ~ ., mr, "InfGain"), decreasing = TRUE)

# build a decision tree using information gain as a splitting criterion
dt <- CoreModel(edibility ~ ., mr, model="tree", selectionEstimator="InfGain")
plot(dt, mr)


#
# Example 2
#

quadrant <- read.table("quadrant.txt", sep=",", header=T)
summary(quadrant)

quadrant$Class <- as.factor(quadrant$Class)

plot(quadrant, col=quadrant$Class)
plot(quadrant$a1, quadrant$a2, col=quadrant$Class)

# attribute evaluation using information gain
sort(attrEval(Class ~ ., quadrant, "InfGain"), decreasing = TRUE)

# information gain is a myopic measure 
# (it assumes that attributes are conditionally independent given the class)

# using information gain to construct a decision tree will produce a poor result
dt2 <- CoreModel(Class ~ ., quadrant, model="tree", selectionEstimator="InfGain")
plot(dt2, quadrant)

# non-myopic measures (Relief and ReleifF)
sort(attrEval(Class ~ ., quadrant, "Relief"), decreasing = TRUE)
sort(attrEval(Class ~ ., quadrant, "ReliefFequalK"), decreasing = TRUE)
sort(attrEval(Class ~ ., quadrant, "ReliefFexpRank"), decreasing = TRUE)

# use ?attrEval to find out different variations of ReliefF measure...

# build a decision tree using ReliefFequalK as a splitting criterion
dt3 <- CoreModel(Class ~ ., quadrant, model="tree", selectionEstimator = "ReliefFequalK")
plot(dt3, quadrant)


#
# Example 3
#

players <- read.table("players.txt", sep=",", header=TRUE)
summary(players)

sort(attrEval(position ~ ., players, "InfGain"), decreasing = TRUE)
sort(attrEval(position ~ ., players, "Gini"), decreasing = TRUE)

# the "id" attribute is overestimated, 
# although it does not carry any useful information

# GainRatio moderates the overestimation of the "id" attribute
sort(attrEval(position ~ ., players, "GainRatio"), decreasing = TRUE)

# Both ReliefF and MDL measures identify the "id" attribute as irrelevant
sort(attrEval(position ~ ., players, "ReliefFequalK"), decreasing = TRUE)
sort(attrEval(position ~ ., players, "MDL"), decreasing = TRUE)

#
#
# Attribute selection
#
#

student <- read.table("student.txt", sep=",", header=T)
student$G1 <- cut(student$G1, c(-Inf, 9, 20), labels=c("fail", "pass"))
student$G2 <- cut(student$G2, c(-Inf, 9, 20), labels=c("fail", "pass"))
student$G3 <- cut(student$G3, c(-Inf, 9, 20), labels=c("fail", "pass"))
student$studytime <- cut(student$studytime, c(-Inf, 1, 2, 3, 4), labels=c("none", "few", "hefty", "endless"))

train = student[1:(nrow(student) * 0.7),]
test = student[(nrow(student) * 0.7):nrow(student),]

#
# Feature selection using a filter method
#

# evaluate the quality of attributes
sort(attrEval(studytime ~ ., student, "MDL"), decreasing = TRUE)

# train a model using everything
model <- CoreModel(studytime ~ ., data=train, model='tree')
predictions <- predict(model, test, type='class')
sum(predictions == test$studytime) / length(predictions)

# train a model using the best evaluated attributes
model <- CoreModel(studytime ~ sex + Walc + Dalc + freetime + higher + paid, data=train, model='tree')
predictions <- predict(model, test, type='class')
sum(predictions == test$studytime) / length(predictions)


# using the appropriate combination of attributes...
model <- CoreModel(studytime ~ Dalc + paid + G2, data=train, model='tree')
predictions <- predict(model, test, type='class')
sum(predictions == test$studytime) / length(predictions)

  # What about some model-based feature importances?
library(caret)
set.seed(123780)
#Number randomly variable selected is mtry
control <- trainControl(method='repeatedcv',number=10, repeats=1)

rf.model <- train(studytime ~ ., 
                    data=train, 
                    method='rf', 
                    metric='Accuracy', 
                    trControl=control)

## varImp extracts the feature relevances
rf.importances<- varImp(rf.model, scale = FALSE)
plot(rf.importances, top = 20)
importance.df.values <- rf.importances$importance
importance.df.names <- rownames(rf.importances$importance)
importance.rf.whole <- data.frame(score = importance.df.values,cnames = importance.df.names)
importance.rf.whole <- importance.rf.whole[order(importance.rf.whole$Overall, decreasing = T),]
feature.names.rf <- importance.rf.whole$cnames[1:20]

mdl.importances <- sort(attrEval(studytime ~ ., student, "MDL"), decreasing = TRUE)[1:20]
feature.names.mdl <- names(mdl.importances)
paste0(length(intersect(feature.names.rf, feature.names.mdl))*100/20,"% overlap between top ranked features!")

## can we visualize the ROC space?
roc_imp <- filterVarImp(x = train[, -ncol(train)], y = train$studytime)
heatmap(as.matrix(roc_imp))
