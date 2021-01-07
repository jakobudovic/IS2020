###############################################################################
#
# INTRODUCTION TO R
#
###############################################################################

# calculator
(50 + 1.45)/12.5 # (CTRL + ENTER)

# Clean screen with CTRL + L

# assignment operators
x = 945
y <- sin(0.47)^2 * sqrt(5)
y^2 -> z

# to inspect the value of a variable simply type its name
x
y
z

# listing and deleting objects
ls()
rm(y)
rm(x,z)

# remove (almost) everything in the working environment
rm(list=ls())

#
# Vectors (the most basic data objects in R)
#

# creating vectors
v <- c(14,7,23.5,76.2)
v

# generating a regular sequence of numbers
v <- 1:10
v

v <- seq(from=5, to=10, by=2)
v

w <- rep(v, times = 2)
w

# scalars are vectors with a single element
w <- 45.0

# vectors can be created using other vectors
z <- c(v, 2.5, w)
z

#
# Useful functions
#

v <- c(8, 4, 2, 3, 6, 9, 1)

length(v)
max(v)
min(v)
which.min(v)
which.max(v)
sum(v)
mean(v)
sd(v)
rev(v)
sort(v)
sort(v, decreasing=T)
order(v) # indexi

# types of vectors
mode(v)

# logical vector - has logical constants as elements 
b <- c(TRUE, FALSE, F, T)
b
mode(b)

x <- 5 > 3
x
mode(x)

# string vector - has strings as elements
s <- c("character", "logical", "numeric", "complex")
mode(s)

# type coercion (all elements must be of the same type)
x <- c(F, T, 34.56, 'aaa')
x
mode(x)

#
# Vectorization
#

# vector arithmetic (operations are performed element-wise)
v1 <- c(10,20,30,40)
v2 <- 1:4
v1 + v2
v1 * v2

# functions operate directly on each element of a vector
v1^2
sqrt(v1)
exp(v1)
log2(v1)

# the recycling rule (if lengths are different the elements of the shorter vector are repeated)
v1 * 10
v1 + 1
v1 + c(100, 200)

#
# Indexing
#

x <- c(-10,20,-30,40,-50,60,-70,80)
x

# individual elements can be addressed using an integer index vector
# (indexing starts with 1)
x[3]
x[c(1,4,5)]
x[1:3]
x[]

# negative integer indices address all elements but those stated
x[-1]
x[-c(4,6)]
x[-(1:3)]

# vector elements can be addressed using logical vectors
# (elements corresponding to constants TRUE are selected)

# logical vector
x > 0

# logical vector indexing
x[x>0]
x[x <= -20 | x > 50]
x[x > 40 & x < 100]

# equality operator is ==
# inequality operator is !=

# the which() function returns indices corresponding to constants TRUE
which(x > 0)

# character string index vector
point <- c(4.7, 3.6, 2.5)
names(point) <- c('x', 'y', 'z')
point

point['x']
point[c('x','z')]

# empty indices
point[] <- 0
point

# not the same as
point <- 0
point

#
# Vector editing
#

x <- c("a", "b", "c", "d")

# replacing an element
x[2] <- "BBBBB"
x

x[c(1,3)] <- c("AAAAA", "CCCCC")
x

# adding new element
x[length(x)+1] = "EEEEE"
x

# what happens if we do not define all elements in the vector?
x[10] <- "FFFFF"
x

# which elements are not defined
is.na(x)


# removing elements
x <- x[-c(1,3)]
x

x <- c(x[2],x[3])
x

#
# Flow control
#

# for loops
for (x in 1:10) {
  print(x)
}

# while loops
x <- 0
while (x < 10) {
  print(x)
  x <- x+1
}

# if statements
x <- 1
if (x == 0) {
  print('Condition 1')
} else if (x == 1){
  print('Condition 2')
} else {
  print('Condition 3')
}

#
# Factors
#

color <- c("blue","red","red","red","blue","red","blue")
color

# factors are useful when modelling nominal variables
color <- factor(color)
color

# argument "levels" defines all possible elements' values
dir <- factor(c('left','left','up'), levels = c('left','right','up','down'))
dir

# all possible elements' values
levels(dir)

# if no match is found
dir[1] <- "diagonal"
dir

# valid assignment
dir[1] <- "down"
dir

# frequency tables for factors 
table(color)
table(dir)

#
# Lists (an ordered collection of objects - components)
#

# creating a list
student <- list(id=12345,name="Marko",marks=c(10,9,10,9,8,10))
student

# extracting elements of a list (using named components)
student$id
student$name
student$marks

# extracting elements of a list (using indexing)
student[[1]]
student[[2]]
student[[3]]

# extending lists
student$parents <- c("Ana", "Tomaz")
student

#
# Data frames
#

# creating a data frame
height <- c(179, 185, 183, 172, 174, 185, 193, 169, 173, 168)
weight <- c(95, 89, 70, 80, 92, 86, 100, 63, 72, 70)
gender <- factor(c("f","m","m","m","f","m","f","f","m","f"))
student <- c(T, T, F, F, T, T, F, F, F, T)

df <- data.frame(gender, height, weight, student)
df

# some important functions
summary(df)
names(df)
nrow(df)
ncol(df)
head(df)

# accessing elements of data frames
df[5,]
df[1:5,]
df[,1]
df[,c(1,3,4)]
df[1,3]
df[1,-3]

df$height

df[df$height < 180,]
df[df$gender == "m",]

# adding columns to a data frame
df <- cbind(df, age = c(20, 21, 30, 25, 27, 19, 24, 27, 28, 24))
df

df$name = c("Joan","Tom","John","Mike","Anna","Bill","Tina","Beth","Steve","Kim")
df

summary(df)

#
# User defined functions
#

addFunction <- function(a, b) {
  return (a+b)
}

# Load in-built datasets with data()
data(iris)

# Get mean of each column with mean()
lapply(iris[,1:3], mean) ## lapply returns a list!
sapply(iris[,1:3] ,mean) ## sapply returns a vector!
apply(iris[,1:3], 1, sum) ## apply operates across a given dimension (1 = row-wise)
 
# Or using native methods
colSums(iris[,1:3])/nrow(iris[,1:3])


## This is additional material, just to show you how things can also be done.
library(dplyr)
data(iris)

# Lets' do some groupings
summarizedCustom <- iris %>% group_by(Species) %>% summarise(msw = max(Sepal.Width), mpw = mean(Petal.Width))
summarizedCustom

# How to create new features?
newFeature <- iris %>% mutate(newFeature = Petal.Width + Sepal.Length)
head(newFeature)   

# How about some filtering?
filteredDF <- iris %>% filter(Species == c("setosa"), Petal.Width >= 0.4)
head(filteredDF)

