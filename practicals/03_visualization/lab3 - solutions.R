#
# SOLUTIONS
#

md <- read.table("movies.txt", sep=",", header=TRUE)
for (i in 18:24)
	md[,i] <- as.factor(md[,i])
	
#
# Are there more movies shorter than 100 min or longer than (or equal to) 100 minutes?
#

tab <- table(md$length < 100)
names(tab) <- c("longer than or equal to 100 minutes", "shorter than 100 min")
tab

barplot(tab, ylab="Number of titles", main="Proportion on movies by length")
pie(tab, main="Proportion of movies by length")


#
# Are there more action comedies or romantic comedies?
#
 
action.comedy <- md$Action == "1" & md$Comedy == "1"
romantic.comedy <- md$Romance == "1" & md$Comedy == "1"
tmp <- c(sum(action.comedy), sum(romantic.comedy))
names(tmp) <- c("Action comedies", "Romantic comedies")
tmp
barplot(tmp, ylab="Number of movies", main="Action comedies vs Romantic Comedies")

# or, we can use the table() command.
table(action.comedy, romantic.comedy)


# 
# Plot a histogram of the ratings for drama movies.
#

hist(md$rating[md$Drama == "1"], xlab="Rating", ylab="Number of movies", main="Histogram of ratings for dramas")


#  
# Is the average rating of dramas higher than the average rating of non-dramas? 
#

mean(md$rating[md$Drama == "1"])
mean(md$rating[md$Drama != "1"])

boxplot(rating ~ Drama, data=md, xlab="Drama", ylab="Rating", main="Boxplot of ratings", names=c("Others", "Dramas"))
abline(h = mean(md$rating[md$Drama == "1"]), col = "red")
abline(h = mean(md$rating[md$Drama != "1"]), col = "blue")


#
# Plot the number of animated movies being produced every year for the period 1995-2005. 
#

sel <- md$year >= 1995

t <- table(md$Animation[sel], md$year[sel])
t
x <- colnames(t)
y <- t[2,]
plot(x,y, type="l", xlab="Year", ylab="Number of movies", main="Number of animated movies per year")

# or using the aggregate() command
agg.com <- aggregate((Animation == "1") ~ year, md[md$year >= 1995,], sum)
plot(agg.com[,1], agg.com[,2], type="l", xlab="Year", ylab="Number of movies", main="Number of animated movies per year")


#
# Is there a clear boundary between short and feature movies (according to their length)?
#

boxplot(length ~ Short, md, xlab="Short movie", ylab="Length in min")

# or,
max(md$length[md$Short == "1"]) < min(md$length[md$Short == "0"])


############################################################################################


players <- read.table("players.txt", sep=",", header = T)

#
# Plot the proportion of players according to playing positions.
#

pie(table(players$position), main="Proportion of players by playing positions")


#
# Compare career rebounds (the "reb" attribute) with respect to playing position.
#

boxplot(reb ~ position, players, xlab="Playing position", ylab="Rebounds")
abline(h=mean(players$reb[players$position=="C"]), col="red")
abline(h=mean(players$reb[players$position=="F"]), col="blue")
abline(h=mean(players$reb[players$position=="G"]), col="green")


#
# Show the distribution of free throw percentages.
#

hist(players$ftm/players$fta, xlab="Free throw percentage", ylab="Number of players", main="Histogram of free throw percentages")
boxplot(players$ftm/players$fta, ylab="Free throw percentage", main="Boxplot of free throw percentages")


#
# Compare career 3-pointers made for the players active between 1990 and 2007, with respect to playing position.
#

threepts <- players$pts - (players$fgm * 2 + players$ftm)
sel <- players$firstseason >= 1990 & players$lastseason <= 2007
boxplot(threepts[sel] ~ players$position[sel], xlab="Playing position", ylab="Number of 3-pointers", main="Career 3-pointers made")


#
# How does the average career length of retired players vary from year to year?
#

avg.len <- aggregate((lastseason-firstseason) ~ lastseason, players, mean)
avg.len
plot(avg.len[,1], avg.len[,2], type="l", xlab="Year", ylab="Career length in years", main="Average career length in NBA")
