##########################################################################################################################
#
# PROBLEMS
#
# gen funkcija ima zakodirane probleme, ki jih potem funkcija evalvira.
library(GA)
##########################################################################################################################
#
# - Use GA search (using the ga() function in the GA package) to find the minimum of the real-valued function 
#   f(x) = abs(x) + cos(x). Restrict the search interval to [-20, 20]. Carefully define the fitness function, 
#   since the ga() can only maximize it! 
#
# f(x) = abs(x) + cos(x)

f <- function(x) 
{
  y <- abs(x) + cos(x) # we want minimum of that
  
  -y ### return(y)
}

curve(f, from = -20, to = 20, n=1000)
GA1 <- ga(type = "real-valued", fitness = f, lower = -20, upper = 20)

# The object returned can be plotted
plot(GA1)
summary(GA1)

curve(f, from = -20, to = 20)
points(GA1@solution, f(GA1@solution), col="red")

##########################################################################################################################
#
# - Use GA search to find the minimum of the real-valued two-dimensional function 
#   f(x1, x2) = 20 + x1^2 + x2^2 - 10*(cos(2*pi*x1) + cos(2*pi*x2)), where x1 and x2 are from the interval [-5.12, 5.12].
#	

# f(x1, x2) = 20 + x1^2 + x2^2 - 10*(cos(2*pi*x1) + cos(2*pi*x2))
f <- function(params) 
{
  y <- 20 + params[1]^2 + params[2]^2 - 10 * (cos(2*pi*params[1]) + cos(2*pi*params[2]))
  
  -y ### return(y)
}

# curve(f, from = -5.12, to = 5.12, n=1000)
GA2 <- ga(type = "real-valued", fitness = f, lower = c(-5.12,-5.12), upper = c(5.12,5.12))

plot(GA2)
summary(GA2)
# curve(f, from = -5.12, to = 5.12) # 3d space so you can't plot it
# points(GA2@solution, f(GA2@solution), col="red")

##########################################################################################################################
#
# - We are given the following data:
#
#   Substrate <- c(1.73, 2.06, 2.20, 4.28, 4.44, 5.53, 6.32, 6.68, 7.28, 7.90, 8.80, 9.14, 9.18, 9.40, 9.88)
#   Velocity <- c(12.48, 13.97, 14.59, 21.25, 21.66, 21.97, 25.36, 22.93, 24.81, 25.63, 24.68, 29.04, 28.08, 27.32, 27.77)
#
#   Use GA search to fit the data to the model:
#   Velocity = (M * Substrate) / (K + Substrate), where M and K are the model parameters. Restrict the search interval 
#   for M to [40.0, 50.0] and for K to [3.0, 5.0].
#

Substrate <- c(1.73, 2.06, 2.20, 4.28, 4.44, 5.53, 6.32, 6.68, 7.28, 7.90, 8.80, 9.14, 9.18, 9.40, 9.88)
Velocity <- c(12.48, 13.97, 14.59, 21.25, 21.66, 21.97, 25.36, 22.93, 24.81, 25.63, 24.68, 29.04, 28.08, 27.32, 27.77)

myFitness = (M * Substrate) / (K + Substrate)

model <- function(params) 
{
  Velocity = (params[0] * Substrate) / (params[1] + Substrate)
  Velocity ### return(y)
}

fitness2 <- function(params) 
{
  -sum((Velocity - model(params))^2)
}


GA3 <- ga(type = "real-valued", fitness = fitness2, lower = c(40.0, 3.0), upper = c(50.0, 5.0), maxiter = 500)

plot(Substrate, Velocity)
lines(Substrate, model(GA3@solution))

points(GA3@solution, f(GA3@solution), col="red")


##########################################################################################################################
#
# - Use a binary GA to select (sub)optimal attribute subset for a linear model:
#
#   train.data <- read.table("AlgaeLearn.txt", header = T)
#   test.data <- read.table("AlgaeTest.txt", header = T)
#   lm.model <- lm(a1 ~., train.data)
#
FILE_learn <- file.choose()
# FILE_learn = "/home/jakob/Documents/fax/fax3_1/IS/practicals/02/AlgaeLearn.txt"
FILE_test <- file.choose()
# FILE_test = "/home/jakob/Documents/fax/fax3_1/IS/practicals/02/AlgaeTest.txt"

train.data <- read.table(FILE_learn, header = T)
test.data <- read.table(FILE_test, header = T)

lm.model <- lm(a1 ~., train.data)
lm

##########################################################################################################################
