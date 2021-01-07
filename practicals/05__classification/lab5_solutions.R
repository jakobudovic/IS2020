student <- read.table("student.txt", sep=",", header=T)

for (i in 30:32) 
  student[,i] <- cut(student[,i], c(-Inf, 9, 11, 13, 15, 20), labels=c("fail", "sufficient", "satisfactory", "good", "excellent"))

summary(student)

barplot(table(student$G1), xlab="Grade", ylab="Number of students", main="First period grades barplot for mathematics")
barplot(table(student$G3), xlab="Grade", ylab="Number of students", main="Final grades barplot for mathematics")

sel <- sample(1:nrow(student), size=as.integer(nrow(student)*0.7), replace=F)
learn <- student[sel,]
test <- student[-sel,]


library(CORElearn)

maj.class <- which.max(table(learn$G3))
ca.vals <- table(test$G3)[maj.class]/nrow(test)

observed <- test$G3
for (m in c("tree", "rf", "knn", "bayes"))
{
  obj <- CoreModel(G3 ~ ., learn, model=m)
  
  predicted <- predict(obj, test, type="class")
  tab <- table(observed, predicted)
  ca.vals <- c(ca.vals, sum(diag(tab))/sum(tab))
}

names(ca.vals) <- c("majority", "tree", "rf", "knn", "bayes")
barplot(ca.vals, xlab="models", ylab="Classification accuracy", main="Results")

