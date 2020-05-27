# Scenario 1

# store the videodata.txt as data.
data <- read.table("videodata.txt", header=TRUE)
head(data)

# filter dataset so that 'time', or number of hours played in week prior to survey is greater than 0.
time.ind <- which(data['time'] > 0)
data.time <- data[time.ind,]
# get number of participants that played a videogame prior to the survey.
prior.count <- nrow(data.time)
# get number of all participants in the survey.
all.count <- nrow(data)
# divide num participants played by all participants in survey to find fraction.
played.fraction <- prior.count / all.count
# Point estimate fraction of participants that played videogames week prior to study is about 37.4%.
played.fraction


# Scenario 2

# Check to see how the amount of time spent playing vide games in the week prior to the survey
# compares to the reported frequency of play (daily, weekly, etc).
# extract dataset so that only time and frequency columns exist.
extract <- data[,c("time", "freq")]
# for freq column, (1 = daily, 2 = weekly, 3 = monthly, 4 = semesterly)
# exclude rows with freq column 99, because 99 means no input was given.
extract.unknown <- subset(extract, freq != 99)
# find the most frequent (daily users) of videogames.
daily.users <- subset(extract.unknown, freq == 1)
# because there are 168 hours in a week, we believed that daily users would put at least 1 hour into gaming.
# we wanted to see the number of players who played 7 hours or more in that␣
,→given week.
daily.find <- c(nrow(subset(daily.users, time >= 7)), nrow(daily.users))
# only 2 out of the 9 daily players played videogames for at least 7 hours, suggesting that the exam did have an effect.
# find the second most frequent (weekly users) of videogames.
weekly.users <- subset(extract.unknown, freq == 2)
# we wanted to say that a weekly user would spend atleast 1.5 hours of gaming
#per week.
weekly.find <- c(nrow(subset(weekly.users, time >= 1.5)), nrow(weekly.users))
# out of the 28 weekly videogame players, 11 did not play for atleast 1.5 hours, suggesting that the exam did have an effect.
print("Daily User")
daily.find
print("Weekly User")
weekly.find


# Scenario 3

# we will create the sample amount (314) of time from our sample data set.
set.seed(189289)
shuffle.ind=sample(1:nrow(data))
# get the bootstap population of time column.
boot.population <- rep(data$time[shuffle.ind], length.out = 314)
# after creating the bootstap population of 314 participants, sample n=91
# samples from the bootstrap population. We will continue this procedure until we have 400 Bootstrap samples.
B <- 400
boot.sample <- array(dim = c(B, 91))
for (i in 1:B) {
boot.sample[i, ] <- sample(boot.population, size = 91, replace = FALSE)
}
# calculate the sample mean for each bootstrap sample.
boot.mean <- apply(X = boot.sample, MARGIN = 1, FUN = mean)
mean.bootstrap <- mean(boot.mean)
# the histogram shows a "bell curve," meaning that the distribution of␣
#bootstrap sample means are a normal distribution.
hist(boot.mean, breaks = 20, probability = TRUE, density = 20, col = 3, border␣
,→= 3)
lines(density(boot.mean, adjust = 2), col = 2)

# calculate the interval estimtates of average play time for the bootstrap samples.
boot.sd <- sd(boot.mean)
int.boot <- c(mean.bootstrap - 1.96*boot.sd, mean.bootstrap + 1.96*boot.sd)
# calculate the average playtime for the orginal survey's sample population.
original.avg <- mean(data$time)
# The interval estimates were (0.58, 1.85).
int.boot
# The original average play time is inbetween the interval estimates of the bootstrap sample.
original.avg


# Scenario 4

#filter out students who never played video games or don't like video games at all
filter_1 <- subset(data, like == 1)
filter_1_df <- setdiff(data, filter_1)
filter_2 <- subset(filter_1_df, like == 5)
filter_2_df <- setdiff(filter_1_df, filter_2)
filter_3 <- subset(filter_2_df, like == 99)
filter_3_df <- setdiff(filter_2_df, filter_2)
head(filter_2_df)

#import dplyr
library(dplyr)

#students with expected grade of C or lower
poor_grade_df <- filter(data, grade < 3)
poor_grade_df

#2 = like very much
mean(poor_grade_df$like)


#students with expected grade of B or higher
good_grade_df <- filter(data, grade > 2)
head(good_grade_df)

#4 = not really
mean(good_grade_df$like)

busy_df <- filter(data, busy == 0)
mean(busy_df$like)

not_busy_df <- filter(data, busy == 1)
mean(not_busy_df$like)


# Scenario 5

# source for cross tabulation
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

# filter out 1 and 99 to compare only like and dislike
data.ind <- which(data$like != 1 & data$like != 99)
filtered <- data[data.ind,]
crosstab(filtered, row.vars = "sex", col.vars = "like",type = "f")
print("probability of male who like game")
(5+21)/38
print("probability of female who like game")
(18+25)/51

#female index
female.ind <- which(filtered['sex'] == 0)
#male index
male.ind <- which(filtered['sex'] == 1)
#split two groups
female <- filtered[female.ind,'like']
male <- filtered[male.ind,'like']

#histogram
hist(male, breaks = 5, col = rgb(1,0,0,0.5), xlab = "likes",probability =TRUE, ylim = c(0,5))
hist(female, breaks = 5, col = rgb(0,0,1,0.5),probability = TRUE, add = TRUE)
legend("topright", c("male", "female"),fill=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))


#cross tab for ownership
crosstab(filtered, row.vars = "own", col.vars = "like",type = "f")
print("probability of PC owner who like game")
(18+30)/65
print("probability of PC non owner who like game")
(5+16)/24

#owner index
own.ind <- which(filtered['own'] == 1)
#non-owner index
notown.ind <- which(filtered['own'] == 0)
own <- filtered[own.ind,'like']
notown <- filtered[notown.ind,'like']

hist(own, breaks = 15, col = rgb(1,0,0,0.5), xlab = "likes",probability =TRUE, ylim = c(0,5))
hist(notown, breaks = 15, col = rgb(0,0,1,0.5),probability =TRUE, add = TRUE)
legend("topright", c("own", "notown"),fill=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))


copies <- data.frame(data)
# non worker
copies$work <- replace(copies$work, copies$work == 0, 0)
# less worker
copies$work <- replace(copies$work, copies$work > 0 & data$work < 11, 1)
# hard worker
copies$work <- replace(copies$work, copies$work > 10, 2)

# Cross tabulation for worker
crosstab(copies, row.vars = "work", col.vars = "like",type = "f")

# non worker index
low.ind <- which(copies['work'] == 0 & data['like'] != 99)
# less worker index
mid.ind <- which(copies['work'] == 1 & data['like'] != 99)
# hard worker index
high.ind <- which(copies['work'] == 2 & data['like'] != 99)
low.work <- copies[low.ind,'like']
mid.work <- copies[mid.ind,'like']
high.work <- copies[high.ind,'like']


hist(low.work, breaks = 15, col = rgb(0.5,0,0,0.5), xlab = "likes",freq = F,ylim = c(0,5))
hist(mid.work, breaks = 15, col = rgb(1,0,1,0.5),freq = F, add = TRUE)
hist(high.work, breaks = 15, col = rgb(1,0,0,0.5),freq = F, add = TRUE)
legend("topright", c("low.work", "mid.work","high.work"),fill=c(rgb(0.5,0,0,0.5),rgb(1,0,1,0.5),rgb(1,0,0,0.5)))
