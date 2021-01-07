##############################################################################
#
# DATA VISUALIZATION
#
##############################################################################

# Please download data files "movies.txt" and "players.txt" into a local directory
# then set that directory as the current working directory of R.
# You can achive this using the "setwd" command or by selecting "File -> Change dir..."

# for example:
# setwd("c:\\labs\\data\\")
library(ggplot2)
library(dplyr)

# To read data from a text file, use the "read.table" command.
# The parameter header=TRUE indicates that the file to be read includes a first line with the column names
md <- read.table(file="movies.txt", sep=",", header=TRUE)

# To get more information on any specific named function, type "?" followed by the function name
?read.table

# Useful functions
head(md)
summary(md)
str(md)
names(md)

# We will transform binary attributes into nominal variables with a fixed number of possible values (factors)
md$Action <- as.factor(md$Action)
md$Animation <- as.factor(md$Animation)

# The remaining columns will be transformed using the for loop
for (i in 20:24)
  md[,i] <- as.factor(md[,i])

#
# Type conversion functions:
#
# as.numeric
# as.integer
# as.character
# as.logical
# as.factor
# as.ordered
#
# values that cannot be converted to the specified type will be converted to a NA value
#

# Binary attributes are now represented as factors
summary(md)

# Accessing data frame elements...
md[30,]
md[30,3]
md[30,"length"]
md[,3]
md$length

# Useful data visualization functions
plot(md$length)
hist(md$length)
plot(density(md$length))
boxplot(md$length)
barplot(table(md$Drama))
pie(table(md$mpaa))

## nicer plots with ggplot2 + dplyr
md %>% ggplot(aes(length)) + geom_histogram(bins = 40) + ggtitle("A genetic histogram") + xlab("Length")

## plotting w.r.t. multiple mpaa categories
md %>% ggplot(aes(length,fill = mpaa)) + geom_density(alpha = 0.2)
# %>% je pipe

## What about a nicer boxplot w.r.t mpaa?
## theme_bw() is more neutral theme
md %>% ggplot(aes(Drama, rating, color = mpaa)) + geom_boxplot() + theme_bw()

## show table view

###############################################################################
#
# EXAMPLE 1: What is the proportion of comedies to other genres in our data set?
#
###############################################################################

# the table() command gives the frequency of values in the vector
table(md$Comedy)

# the proportion of comedies can be plotted
barplot(table(md$Comedy))
pie(table(md$Comedy))


#  it is important to always label graphs ...

tab <- table(md$Comedy)
names(tab) <- c("Other genres", "Comedies")
tab

sum(tab)

barplot(tab, ylab="Number of titles", main="Proportion of comedies to other genres")
barplot(tab / sum(tab) * 100, ylab="Percentage of titles", main="The proportion of comedies to other genres")

pie(tab, main = "Proportion of comedies to other genres")

###############################################################################
#
# EXAMPLE 2: How are ratings distributed for comedies?
#
###############################################################################

# Plot the rating distribution for comedies, 
# vrstice, kjer je žanr Comedy, stolpce vzamemo samo Rating
hist(md[md$Comedy == "1", "rating"], xlab="Rating", ylab="Frequency", main="Histogram of ratings for comedies")

# Box plots provide a visual display of the range and potential skewness of the data
boxplot(md[md$Comedy == "1", "rating"], ylab="Rating", main="Boxplot of ratings for comedies")

quantile(md$rating[md$Comedy == 1])


###############################################################################
# 
# EXAMPLE 3: Are comedies on average better rated than non-comedies?
#
###############################################################################

# Select comedies
comedy <- md$Comedy == "1"

# Calculate the mean rating value for comedies and non-comedies
mean(md[comedy,"rating"])
mean(md[!comedy,"rating"])

# Comedies have, on average, higher ratings than non-comedies

# Side-by-side boxplots of ratings grouped by values of the attribute "Comedy" 
boxplot(rating ~ Comedy, data=md)
boxplot(rating ~ Comedy, data=md, names=c("Other genres", "Comedies"), ylab="Rating", main="Comparison of ratings between comedies and non-comedies")

## or with dplyr directly
md %>% group_by(Comedy) %>% select(rating) %>% summarise(mean(rating))

###############################################################################
#
# EXAMPLE 4: What is the proportion of comedies (per year) from 1990 onwards?
#
###############################################################################

sel <- md$year >= 1990

# the table() command can be used to get a two-way contigency table 
table(md$Comedy[sel], md$year[sel])

table(md$year[sel])

tabcomedy <- table(md$Comedy[sel], md$year[sel])
tabyear <- table(md$year[sel])
tabcomedy[2,]/tabyear

ratio <- tabcomedy[2,]/tabyear
barplot(ratio, xlab="Year", ylab="Relative frequency", main="Proportion of comedies")

plot(x=names(ratio), y=as.vector(ratio), type="l", xlab="Year", ylab="Relative frequency", main="Proportion of comedies, 1990-2005")

## or with dplyr directly
md %>% filter(year >= 1990) %>% 
  group_by(year, Comedy) %>%  
  summarise(n = n()) %>% mutate(freq = n / sum(n)) %>% 
  filter(Comedy == 1) %>% select(year, freq) %>% 
  ggplot(aes(year, freq)) + geom_point() + ggtitle("Frequency of comedies") + ylab("Frequency") + xlab("Year") + geom_line() + theme_bw()

###############################################################################
# 
# EXAMPLE 5: Are there more movies above or below the average rating?
#
###############################################################################

# the average rating
mean(md$rating)

# how many movies are above the average rating?
tab <- table(md$rating > mean(md$rating))
tab

names(tab) <- c("below", "above")
barplot(tab, ylab="Number of titles", main="Proportion of movies above and below the average rating")
pie(tab, main="Proportion of movies above and below the average rating")


# Box plots provide a summarization of the variable distribution
boxplot(md$rating, ylab="Rating", main="Boxplot of movie ratings")

# The horizontal line inside the box represents the median rating value

# Let's plot the mean value...
abline(h=mean(md$rating))

## or with dplyr + ggplot
md %>% mutate(mRate = mean(rating)) %>% 
  mutate(indicator = ifelse(rating - mRate > 0, "above", "below")) %>% 
  group_by(indicator) %>% 
  summarise(counts = n()) %>% 
  ggplot(aes(indicator, counts, fill = indicator))+ geom_bar(stat = "identity")

# The mean differs from the median so the distribution is skewed.
# We can conclude that there are more cases above the mean value. 


###############################################################################
#
# EXAMPLE 6: Do movies with bigger budgets get higher ratings?
#
###############################################################################

# there are missing values in the budget attribute
summary(md$budget)

is.na(md$budget)
table(is.na(md$budget))
which(is.na(md$budget))

# select complete observations only
sel <- is.na(md$budget)
mdsub <- md[!sel,]

nrow(mdsub)
summary(mdsub$budget)


plot(mdsub$budget, mdsub$rating, xlab="Budget in $", ylab="Rating", main="Movie rating vs budget")

# Plotted points are mostly located in the upper left part of the diagram, 
# which means that a higher budget usually leads to a higher rating

# Utilization of the budget in terms of rating  
ratio <- mdsub$budget/mdsub$rating
hist(ratio)

# Which movie has the worst budget utilization?
mdsub[which.max(ratio),]


# Let's discretize these budgets to:
# low (less than 1M), mid (between 1M and 50M) and big (more than 50M)

disbudget <- cut(mdsub$budget, c(0, 1000000, 50000000, 500000000), labels=c("low", "mid", "big"))
barplot(table(disbudget)/length(disbudget), xlab="Budget", ylab="Relative frequency", main="Proportion of movies vs budget")

# Side-by-side boxplots of ratings grouped by budget values
boxplot(mdsub$rating ~ disbudget, xlab="Budget", ylab="Rating", main="Boxplot of movie rating vs budget")

## Is this dependent on the mpaa?
## or with dplyr + ggplot + adding votes
md %>% select(budget, rating, votes, mpaa) %>% 
  na.omit() %>% 
  ggplot(aes(budget, rating, color = votes, fill = mpaa)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x) + theme_bw()

###############################################################################
#
# EXAMPLE 7: 
# What is the cumulative movie budget for each year from 1990 to 2000?
# What is the average movie budget for each year from 1990 to 2000?
# (consider only those movies for witch information on the budget is available!)
#
###############################################################################

# Select the movies that contain information on their budgets
sel <- !is.na(md$budget) & md$year >= 1990 & md$year <= 2000

# We can calculate cumulative budget for each year using the "aggregate" function

# Data overflow problem!
aggregate(budget ~ year, data = md[sel,], sum)

# The budget values are represented as integers
typeof(md$budget)

# In order to avoid the overflow problem we have to convert 
# the budget values into a double-precision representation (using the as.double() command)
aggregate(as.double(budget) ~ year, data = md[sel,], sum)

sum.budget <- aggregate(as.double(budget) ~ year, data = md[sel,], sum) 
plot(sum.budget, type="l", xlab="Year", ylab="Cumulative budget in $", main="Cumulative movie budget per year")

avg.budget <- aggregate(as.double(budget) ~ year, data = md[sel,], mean)
plot(avg.budget, type="l", xlab="Year", ylab="Average budget in $", main="Average movie budget per year")

## or with dplyr
md %>% select(budget, year) %>% na.omit() %>% 
  group_by(year) %>% summarise(budget2 = sum(as.numeric(budget))) %>% 
  arrange(year) %>% mutate(csum = cumsum(budget2)) %>%
  ggplot(aes(year, csum)) + geom_bar(stat = "identity") + theme_bw()

##############################################################################
#
# EXAMPLE 8: (players dataset)
# What is the average height for each season in the period from 1970 to 2000?
#
##############################################################################

# Load the Players dataset
players <- read.table("players.txt", sep=",", header = T)
summary(players)

# Create an empty vector
h <- vector()

# Use a for loop to go through each year in the period from 1970 to 2000
for (y in 1970:2000)
{
  # Select active players in that year
  sel <- players$firstseason <= y & players$lastseason >= y
  
  # Append the resulting vector with the mean height for the current year
  h <- c(h, mean(players$height[sel]))
}

# plot the resulting vector (use type="l" for lines)
plot(1970:2000, h, type="l", xlab="Year", ylab="Height in cm", main="Average height in NBA")

## or with dplyr
dfx <- data.frame(year = 1970:2000, mh = h)
dfx %>% ggplot(aes(year, mh)) + geom_point() + geom_smooth(method = "loess") + theme_bw() + xlab("Year") + ylab("Mean height")
# loess - linearna statisnična enota 
