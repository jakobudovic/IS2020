# We are going to use the GA package
# Make sure that the package is installed.
# You install a package in R with the function install.packages():
#
#    install.packages("GA")
library(GA)
#
# To install packages without root access:
#
#     install.packages("GA", lib="/mylibs/Rpackages/") ## or some other path, e.g., C:\yourFolder
#     library(GA, lib.loc="/mylibs/Rpackages/")

#
#
# EXAMPLE 1: One-dimensional function optimization
#
#

# The asymmetric double claw is difficult to maximize because there are many local solutions.
# Standard derivative-based optimizers would simply climb up the hill closest to the starting value.

f <- function(x) 
{
  y <- (0.46 * (dnorm(x, -1, 2/3) + dnorm(x, 1, 2/3)) +
          (1/300) * (dnorm(x, -0.5, 0.01) + dnorm(x, -1, 0.01) +
                       dnorm(x, -1.5, 0.01)) +
          (7/300) * (dnorm(x, 0.5, 0.07) + dnorm(x, 1, 0.07) +
                       dnorm(x, 1.5, 0.07)))
  
  y ### return(y)
}

# Plot the double claw
curve(f, from = -3, to = 3, n = 1000)

# For the maximization of this function we may use f directly as the fitness function
GA <- ga(type = "real-valued", fitness = f, lower = -3, upper = 3)
# we have real values on x axis
# lower and upper are ranges of exploration

# The object returned can be plotted
plot(GA)
summary(GA)

# plot the solution
curve(f, from = -3, to = 3, n = 1000)
points(GA@solution, f(GA@solution), col="red")
# red circle na mestu x, f(x)

# The evolution of the population units and the corresponding functions values at each 
# generation can be obtained by defining a new monitor function and then passing this 
# function as an optional argument to ga

myMonitor <- function(obj) 
{
  curve(f, obj@lower, obj@upper, n = 1000, main = paste("iteration =", obj@iter))
  points(obj@population, obj@fitness, pch = 20, col = 2)
  rug(obj@population, col = 2)
  Sys.sleep(0.2)
}

GA <- ga(type = "real-valued", fitness = f, lower = -3, upper = 3, monitor = myMonitor)

## Inspect fitness across generations
plot(GA)

#
#
# EXAMPLE 2: Model fitting
#
#

# We consider a data on the growth of trees

# The age at which the tree was measured
Age <- c(2.44, 12.44, 22.44, 32.44, 42.44, 52.44, 62.44, 72.44, 82.44, 92.44, 102.44, 112.44)

# The bole volume of the tree
Vol <- c(2.2, 20.0, 93.0, 262.0, 476.0, 705.0, 967.0, 1203.0, 1409.0, 1659.0, 1898.0, 2106.0)

plot(Age, Vol)

# An ecological model for the plant size (measured by volume) as a function of age is the Richards curve:
# f(x) = a*(1-exp(-b*x))^c, where a, b, in c are the model parameters

# Let's fit the Richards curve using genetic algorithms

# We first define our model function (argument params represents a vector of the parameters a, b, and c) 
model <- function(params)
{
  params[1] * (1 - exp(-params[2] * Age))^params[3]
}

# We define the fitness function as the sum of squares of the differences between estimated and observed data
myFitness2 <- function(params) 
{
  -sum((Vol - model(params))^2)
}

# The fitness function needs to be maximized with respect to the model's parameters, given the observed data in x and y.
# A blend crossover is used for improving the search over the parameter space: for two parents x1 and x2 (assume x1 < x2) 
# it randomly picks a solution in the range [x1 - k*(x2-x1), x2 + k*(x2-x1)], where k represents a constant between 0 and 1.


# We restrict the search interval for a,b, and c to [1000.0, 5000.0], [0.0, 5.0], and [0.0, 5.0], respectively.


GA2 <- ga(type = "real-valued", fitness = myFitness2, lower = c(1000, 0, 0), upper = c(5000, 5, 5),
          popSize = 500, crossover = gareal_blxCrossover, maxiter = 5000, run = 200, names = c("a", "b", "c"))
# check what gareal_blxCrossover is by writing ?gareal_blxCrossover 

summary(GA2)

# Let's plot our solution

plot(Age, Vol)
lines(Age, model(GA2@solution))


# we can use a monitor function to plot the current solution

myMonitor2 <- function(obj) 
{
  i <- which.max(obj@fitness)
  plot(Age, Vol)
  lines(Age, model(obj@population[i,]), col="red")
  title(paste("iteration =", obj@iter), font.main = 1)
  Sys.sleep(0.1)
}

GA2 <- ga(type = "real-valued", fitness = myFitness2, lower = c(1000, 0, 0), upper = c(5000, 5, 5),
          popSize = 500, crossover = gareal_blxCrossover, maxiter = 500, run = 200, names = c("a", "b", "c"), monitor=myMonitor2)


#
#
# EXAMPLE 3: The Knapsack problem
#
#

# The Knapsack problem is defined as follows: given a set of items, each with a mass and a value, determine the subset 
# of items to be included in a collection so that the total weight is less than or equal to a given limit and the total value 
# is as large as possible.

# a vector of the items' values
values <- c(5, 8, 3, 4, 6, 5, 4, 3, 2)

# a vector of the item's weights
weights <- c(1, 3, 2, 4, 2, 1, 3, 4, 5)

# the knapsack capacity
Capacity <- 10
# This will be our constraint

# A binary GA can be used to solve the knapsack problem. The solution to this problem is a binary string equal to the number 
# of items where the ith bit is 1 if the ith item is in the subset and 0 otherwise. The fitness function should penalize 
# unfeasible solutions.

knapsack <- function(x) 
{
  f <- sum(x * values) # indicator function, only when the binary value is one, we take the value
  w <- sum(x * weights)
  
  if (w > Capacity)
    f <- Capacity - w # penalising the result so GA will reject it
  
  f	
}

GA3 <- ga(type = "binary", fitness = knapsack, nBits = length(weights), maxiter = 1000, run = 200, popSize = 100)
# binary - vektorji so binarni
# maximiziramo optimizacijo f-ja

summary(GA3)
GA3@solution

#
# Example 4: ESTABLISHING A TIMETABLE
#

# A small football club has a youth team and a senior team. The player 
# training program has seven components: stamina training, strength training, 
# technique, tactics, psychological preparation, teamwork, and regeneration. 
# Due to lack of funds, for each component, a single staff member is responsible 
# for both the youth and the senior team, with the exceptions of tactics and 
# stamina training, where two staff members are assigned, one to each team.
# 
# The weekly training regime is summarized in the following table:
# 
#+----------+---------------------+-----------------+-----------------+
#| Coach    | Component           | Senior team     | Youth team      |
#+----------+---------------------+-----------------+-----------------+
#| Anze     | Strength training   | 1 time a week   | 1 time a week   |
#| Bojan    | Technique           | 3 times a week  | 3 times a week  |
#| Ciril    | Regeneration        | 2 times a week  | 2 times a week  |
#| Dusan    | Stamina training    | doesn't conduct | 4 times a week  |
#| Erik     | Stamina training    | 4 times a week  | doesn't conduct |
#| Filip    | Teamwork            | 3 times a week  | 3 times a week  |
#| Gasper   | Psychological prep. | 1 time a week   | 1 time a week   |
#| Hugo     | Tactics             | 1 time a week   | doesn't conduct |
#| Iztok    | Tactics             | doesn't conduct | 1 time a week   |
#+----------+---------------------+-----------------+-----------------+
#
# Training is performed from Monday to Friday in four different time slots: 
#    8:00 - 10:00, 10:15 - 12:15, 14:00 - 16:00, and 16:15 - 18:15.
# 
# Constraints:
#
# - each time slot can hold only one component for the youth team and one component 
#   for the senior team (the youth and senior teams train separately, so a single
#   staff member can only train one of the two teams in a single time slot). 
# 
# - a team is not allowed to train the same component 2 or more times within one day.
# 
# - the main purpose of the Tactics training component is to prepare the team for 
#   the upcoming match. Matches are usually played during the weekend, so Tactics 
#   training should be scheduled for Thursday in the 16:15 - 18:15 time slot.
# 
# - after a match, the players need to rest. Therefore, there is no training in 
#   the Monday 8:00 - 10:00 time slot.
#
# - the stamina training coach Dusan is not available on Monday mornings 
#   (8:00 - 10:00 in 10:15 - 12:15 time slots)
#
# - there can be no Technique training on Wednesdays, because coach Bojan is 
#   not available.
#
#
# Produce a training schedule that takes into account these two and all of 
# the above restrictions!
#
#

# VARIABLES
#
# senior     - number of sessions per component for the senior team
# youth      - number of sessions per component for the youth team
# staff      - coaching staff -> the staff's actual occupacy is solved for, how much a certain coach can handle is (hard) coded in senior and youth variables!
# slots      - possible slots

senior     = c(1, 3, 2, 0, 4, 3, 1, 1, 0)
youth      = c(1, 3, 2, 4, 0, 3, 1, 0, 1)
slots      = 4*5

valueBin <- function(timetable)
{
  # organize data into a multi-dimensional array
  # days, time slots, staff, teams
  
  t <- array(as.integer(timetable), c(5,4,9,2))
  
  violations <- 0 # minimize the violation to a specific timetable
  
  # check all the conditions
  
  # check the number of sessions per component
  for (i in 1:9)
  {
    violations <- violations + abs(sum(t[,,i,1]) - senior[i])
    violations <- violations + abs(sum(t[,,i,2]) - youth[i])
  }	
  
  # it is not allowed to train the same component 2 or more times within one day
  for (i in 1:9)
  {
    violations <- violations + sum(apply(t[,,i,1], 1, sum) > 1)
    violations <- violations + sum(apply(t[,,i,2], 1, sum) > 1)
  }
  
  # a single staff member can only train one of the two teams in a single time slot
  violations <- violations + sum(t[,,,1] == t[,,,2] & t[,,,1] != 0)
  
  # each time slot can hold only one component for the youth team and one component
  # for the senior team
  for (i in 1:5)
    for (j in 1:4)
    {
      violations <- violations + max(0, sum(t[i,j,,1]) - 1)
      violations <- violations + max(0, sum(t[i,j,,2]) - 1)
    }
  
  # Tactics training should be scheduled for Thursday in the 16:15 - 18:15 time slot
  violations <- violations + (t[4,3,8,1] != 1)
  violations <- violations + (t[4,3,9,2] != 1)
  
  # there is no training in the Monday 8:00 - 10:00 time slot
  violations <- violations + sum(t[1,1,,])
  
  # the stamina training coach Dusan is not available on Monday mornings
  violations <- violations + sum(t[1,1:2,4,] == 1)
  
  # there can be no Technique training on Wednesdays
  violations <- violations + sum(t[3,,2,] == 1)
  
  -violations	# algo will try to minimize this value and get as close to 0 as possible
} # algo maksimizira to negativno vrednost (želi imeti čim manj violacij)

myInitPopulation <- function(object)
{
  p <- gabin_Population(object)
  
  for (i in 1:nrow(p))
  {
    t <- array(p[i,], c(5,4,9,2))
    
    # Tactics training on Thursdays in the 16:15 - 18:15 time slot
    t[4,3,8,1]=1
    t[4,3,9,2]=1
    
    # there is no training in the Monday 8:00 - 10:00 time slot
    t[1,1,,] = 0
    
    # there is no Stamina training on Monday mornings
    t[1,1:2,4,] = 0
    
    # there is no Technique training on Wednesdays
    t[3,,2,] = 0
    
    p[i,] <- as.vector(t)
  }
  p
}

GA4 <- ga(type = "binary", fitness = valueBin, nBits = 4*5*9*2,
          popSize = 500, maxiter = 10, run = 200, population = myInitPopulation)


timetable2 <- function(solution,coach,team){
  t <- array(solution, c(5,4,9,2))
  t[,,coach,team]
} # p-1 ??

## timetable of a coach 2 for team 1.
t <- timetable2(GA4@solution[1,],2,1)
t



#
#
# EXAMPLE 5: Traveling salesman problem
#
#
# permutacijski genetski tip problema

# Given a list of cities and the distances between each pair of cities, what is the shortest possible route that visits 
# each city exactly once and returns to the origin city?

data("eurodist", package = "datasets")
D <- as.matrix(eurodist)
D

# An individual round tour is represented as a permutation of a default numbering of the cities defining the current order 
# in which the cities are to be visited

# Calculation of the tour length
tourLength <- function(tour) 
{
  N <- length(tour)
  
  dist <- 0
  for (i in 2:N) 
    dist <- dist + D[tour[i-1],tour[i]]
  
  dist <- dist + D[tour[N],tour[1]]
  dist
}

# The fitness function to be maximized is defined as the reciprocal of the tour length.
tspFitness <- function(tour) 
{
  1/tourLength(tour)
} # minimiziramo problem z obratno vrednostjo, ker dist maximiziramo in rezultat minimiziramo

GA5 <- ga(type = "permutation", fitness = tspFitness, lower = 1, upper = ncol(D), popSize = 50, maxiter = 5000, run = 500, pmutation = 0.2)
# ti algoritmi so stohastični, kar pomeni, da lahko dobiš različne rezultate

summary(GA5)

# Reconstruct the solution found 
tour <- GA5@solution[1, ]
tour <- c(tour, tour[1])

tourLength(tour)
colnames(D)[tour]

