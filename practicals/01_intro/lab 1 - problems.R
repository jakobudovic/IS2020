################################################################################
#
# PROBLEMS:
#
# 1.  Construct a vector that contains elements: 1,2,3,...,19,20.
x1 <- 1:20
x1

# 2.  Construct a vector that contains elements: 1,2,3,...,19,20,19,...,3,2,1.
x2 <- c(x1,rev(x1)[-1])
x2

# 3.  Construct a vector that contains elements: 1,3,5,1,3,5,...,1,3,5 
#     where there are 10 occurrences of element 5.
x3 <- rep(c(1,3,5), times=10)
x3

# 4.  Calculate the values of sin(x) at 0, 0.1, 0.2, 0.3, ..., 1.0
x4 <- sin((1:10) / 10)
x4

# 5.  Suppose we have measured the heights and weights of ten individuals:
#
      # the vector of heights in 'cm'
      height <- c(179, 185, 183, 172, 174, 185, 193, 169, 173, 168)

      # the vector of weights in 'kg'
      weight <- c(95, 89, 70, 80, 92, 86, 100, 63, 72, 70)

#     Calculate the body mass index (bmi) for each individual using the formula:
#     bmi = weight_in_kg / (height_in_m)^2
      bmi <- weight / (height/100)^2
      bmi
#
#     HINT: first convert heights from 'cm' to 'm', then use the formula above.  
#
#
# 6.  Consider a vector:
# 
      x <- c(1, -2, 3, -4, 5, -6, 7, -8)
   
#     Edit the vector x as follows. Replace all elements with a negative value 
#     with 0. Multiply the elements with a positive value by 10.
      
replace <- function(a) {
   if (a < 0){
      return (0)
   }
   else{
      return (a*10)
   }
}
      y  <- sapply(x, replace)
      y

# 7.  Without using R, determine the result of the following computation:
#
      x <- c(1,2,3)
      x[1]/x[2]^2-1+2*x[3]-x[1+1]
      1/2^2-1+2*3-2
      1/4-1+6-2
      1/4+3
      3.25
#
#
# 8.  Consider a vector:
#
      x <- 1:200

#     Determine how many elements in the vector are exactly divisible by 11.
#
#     HINT: the integer division operator is %/%
#           the modulus operator is %%              
      x
      divide <- function(a){
         return(a%%11)
      }
      
      y <- sapply(x, divide)
      y
      z <- table(y)
      z
      z[names(z) == 0] # ostanek z 0 vrednost vidimo v 1. vrstici aka "names", stevilo elementov je v drugi vrstici

#
# 9.  Consider a data frame:
#
      height <- c(179, 185, 183, 172, 174, 185, 193, 169, 173, 168)
      weight <- c(95, 89, 70, 80, 92, 86, 100, 63, 72, 70)
      gender <- factor(c("f","m","m","m","f","m","f","f","m","f"))
      student <- c(T, T, F, F, T, T, F, F, F, T)
      age = c(20, 21, 30, 25, 27, 19, 24, 27, 28, 24)
      name = c("Joan","Tom","John","Mike","Anna","Bill","Tina","Beth","Steve","Kim")
 
      df <- data.frame(name, gender, age, height, weight, student)
      df
      names(df)
#     
#     - calculate the average age of persons in our dataset. 
#       (HINT: use the mean() function)
      mean(df[,3])
      # or
      df %>% select(age)
      
      # BUT you have to run this first:
      # install.packages("magrittr") # package installations are only needed the first time you use it
      # install.packages("dplyr")    # alternative installation of the %>%
      # library(magrittr) # needs to be run every time you start R and want to use %>%
      # library(dplyr)   
#
#
#     - calculate the average age of students in our dataset.
      mean(df[df$student,3])
#
#     - how many males and females are in our dataset? 
#       (HINT: use the table() function)
      
      table(df$gender)
      
#
#     - print persons that are students. 
      library(dplyr)
      install.packages("tidyverse")

      select(df[df$student,], name)
      # or
      df1 <- filter(df, student)
      df1

#     - print persons who are between 1.8m and 1.9m tall (inclusive). 
      
      
#
#     - print students who are above average height 
#       (considering all persons in the dataset).
#
#
#     - arrange persons by their age. 
#       (HINT: use the order function)             
#
###############################################################################


# blaz.skrlj@ijs.si