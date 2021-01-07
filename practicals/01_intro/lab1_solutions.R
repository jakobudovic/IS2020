#
# Problem 1
#

v <- 1:20
v


#
# Problem 2
#

v <- c(1:20,19:1)
v


#
# Problem 3
#

v <- rep(c(1,3,5), times = 10)
v


#
# Problem 4
#

x <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
# or
x <- (0:10)/10
# or
x <- seq(from = 0, to=1, by=0.1)

sin(x)


#
# Problem 5
#

height <- c(179, 185, 183, 172, 174, 185, 193, 169, 173, 168)
weight <- c(95, 89, 70, 80, 92, 86, 100, 63, 72, 70)

bmi = weight / (height / 100)^2
bmi


#
# Problem 6
#

x <- c(1, -2, 3, -4, 5, -6, 7, -8)

x[x < 0] <- 0

x[x > 0] <- x[x > 0] * 10
x

# or using sapply (returns a vector!)

x <- c(1, -2, 3, -4, 5, -6, 7, -8)
a <- sapply(sapply(x, function(x) ifelse(x < 0, 0, x)), function(x) ifelse(x > 0, x * 10, x))
a

# or not as efficient:
result <- numeric(0)
for (el in x){
  if (el < 0){
    result <- c(result, 0)
  }else{
    result <- c(result, el*10)
  }
}
result

#
# Problem 7
#

3.25


#
# Problem 8
#

x <- 1:200
which(x %% 11 == 0)
length(which(x %% 11 == 0))
# or more simply
sum(x %% 11 == 0)


#
# Problem 9
#

height <- c(179, 185, 183, 172, 174, 185, 193, 169, 173, 168)
weight <- c(95, 89, 70, 80, 92, 86, 100, 63, 72, 70)
gender <- factor(c("f","m","m","m","f","m","f","f","m","f"))
student <- c(T, T, F, F, T, T, F, F, F, T)
age = c(20, 21, 30, 25, 27, 19, 24, 27, 28, 24)
name = c("Joan","Tom","John","Mike","Anna","Bill","Tina","Beth","Steve","Kim")
df <- data.frame(name, gender, age, height, weight, student)

mean(df$age)

mean(df$age[df$student])

table(df$gender)

df[df$student,]

selection <- df$height >= 180 & df$height <= 190
df[selection,]
# or directly
df[df$height >= 180 & df$height <= 190,]

df[df$student & df$height > mean(df$height),]

df[order(df$age),]
