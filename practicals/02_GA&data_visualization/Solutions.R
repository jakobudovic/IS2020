library(GA)

f1 <- function(x)
{
  -abs(x)-cos(x)
}

GA1 <- ga(type = "real-valued", fitness = f1, lower = -20, upper = 20)
summary(GA1)

#############################################################################

f2 <- function(x)
{
  -(20 + x[1]^2 + x[2]^2 - 10*(cos(2*pi*x[1]) + cos(2*pi*x[2])))
}

GA2 <- ga(type = "real-valued", fitness = f2, lower = c(-5.12, -5.12), upper = c(5.12, 5.12), crossover = gareal_blxCrossover)
summary(GA2)

############################################################################

Substrate <- c(1.73, 2.06, 2.20, 4.28, 4.44, 5.53, 6.32, 6.68, 7.28, 7.90, 8.80, 9.14, 9.18, 9.40, 9.88)
Velocity <- c(12.48, 13.97, 14.59, 21.25, 21.66, 21.97, 25.36, 22.93, 24.81, 25.63, 24.68, 29.04, 28.08, 27.32, 27.77)
f3 <- function(theta, Substrate, Velocity) 
{
  -sum((Velocity - (theta[1] * Substrate) / (theta[2] + Substrate))^2)
}

GA3 <- ga(type = "real-valued", fitness = f3, Substrate, Velocity, lower = c(40.0, 3.0), upper = c(50.0, 5.0),
          popSize = 500, crossover = gareal_blxCrossover, maxiter = 5000, run = 200, names = c("M", "K"))

summary(GA3)
plot(Substrate, Velocity)
M = GA3@solution[1, "M"]
K = GA3@solution[1, "K"]
lines(Substrate, M * Substrate / (K + Substrate))

###########################################################################

train.data <- read.table("AlgaeLearn.txt", header = T)
test.data <- read.table("AlgaeTest.txt", header = T)
lm.model <- lm(a1 ~., train.data)
mean((test.data$a1 - predict(lm.model, test.data))^2)

Y <- train.data[, "a1", drop = FALSE]
X <- train.data[, which(names(train.data) != "a1")]

## define the fitness function
fitness4 <- function(string) 
{
  if (sum(string) == 0){
    return(0)
  }
  
  ## select only relevant features.
  inc <- which(string == 1)
  
  ## fit a linear model only on the relevant features.
  model <- lm(a1 ~., data = cbind(Y, X[,inc, drop = FALSE]))
  
  ## Final output of the fitness is the negative mean squared error.
  # Minimiziramo napako, želimo jo čim manjšo
  -mean((test.data$a1 - predict(model, test.data))^2)
}

## Evaluate the algorithm.
GA4 <- ga("binary", fitness = fitness4, nBits = ncol(X), names = colnames(X), monitor = plot)
summary(GA4)

## Print the error of the final solution.
sel <- GA4@solution[which.min(apply(GA4@solution, 1, sum)),]
model1 <- lm(a1 ~., data = cbind(Y, X[, which(sel == 1)]))
mean((test.data$a1 - predict(model1, test.data))^2)


