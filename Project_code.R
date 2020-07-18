# Project 1

setwd("~/Desktop/Data Science course/5. Reproducible Research/Project 1")


# Loading and preprocessing the data

data <- read.csv('RepData_PeerAssessment1-master/activity.csv')

head(data)
dim(data)   # 17568 obs, 3 vars

sapply(data[,1:3], class)

# Let's change the values in date column to date type

data$date <- as.Date(data$date)


# Each element in interval column represents a time in the day:
# e.g. 1545 represents 15:45, 55 represents 0:55, and so on.

# To do conversion, I could do:

# int2 <- mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 4 - nchar(data$interval))
# data$interval <- paste0(int2, data$interval)
# data$interval <- format(strptime(data$interval, format="%H%M"), format = "%H:%M")


# What is mean total number of steps taken per day?

library('dplyr')

totalsteps <- data %>% group_by(date) %>% summarize(nr_steps = sum(steps, na.rm = TRUE))
    # We group the observation by date and for each date we calculate the total number of steps.

hist(totalsteps$nr_steps, breaks=10, xlab='total number of steps', ylab='Frequency (days)',
     main='Total number of steps per day')

# mean(totalsteps$nr_steps)
# median(totalsteps$nr_steps)

summary(totalsteps$nr_steps)



# What is the average daily activity pattern?

#length(unique(data$date))
    # there are 61 days in this data set

#tb <- table(data$date)
#unique(tb)
    # there are 288 obs per day (this is exactly the nr of 5-min intervals
    # in a day) on a total of 61 days

# Note that 288*61 = 17568 observations.

#dailyactiv <- rep(0, 288)

#for (i in 1:288){
#    dailyactiv[i] <- mean(data$steps[seq(i, 17568, by=288)], na.rm = TRUE)
#}

#plot(dailyactiv, type='l')

#which(dailyactiv==max(dailyactiv))
# result: 104
#(5*104) %/% 60
#(5*104) %% 60
# So it's between 8:35-8:40


# Quicker way:

newdata <- data %>% group_by(interval) %>% summarise(average = mean(steps, na.rm = TRUE))
    # We group the data by interval and for each interval take the mean of the number of steps.

plot(newdata$average ~ newdata$interval, type='l', xlab='time of day (hhmm)', ylab='average (steps)', main='Average number of steps throughout the day')

newdata$interval[ which( newdata$average == max(newdata$average) ) ]



# Imputing missing values

sum(is.na(data$steps))
sum(is.na(data$date))
sum(is.na(data$interval))
    # 2304 nr of NAs in the data set

# I chose the strategy of filling each empty entry of steps with the average
# number of steps in that 5-min interval (this info is saved in dailyactiv).

#data2 <- data
#indexes <- which(is.na(data2$steps))
    # indexes of rows of missing elements

#for (i in indexes){
#    data2[i,1] <- dailyactiv[data2[i,3] %% 288 + 1]
#}

#dates2 <- group_by(data2, date)
#totalsteps2 <- summarize(dates2, nr_steps = sum(steps, na.rm = TRUE))

#hist(totalsteps$nr_steps, breaks=10)
#hist(totalsteps2$nr_steps, breaks=10)

data2 <- data
indexes <- which(is.na(data2$steps))        # indexes of missing values

for (i in indexes){
    data2[i,1] <- newdata$average[newdata$interval == data2[i,3]]
}

totalsteps2 <- data2 %>% group_by(date) %>% summarize(nr_steps = sum(steps))

par(mfrow=c(1,2))

hist(totalsteps$nr_steps, breaks=10, xlab='total number of steps', ylab='Frequency (days)',
     main='With missing data')

hist(totalsteps2$nr_steps, breaks=10, xlab='total number of steps', ylab='Frequency (days)',
     main='Without missing data', ylim=c(0,25))

par(mfrow=c(1,1))

summary(totalsteps$nr_steps)
summary(totalsteps2$nr_steps)

# Overall the total number of steps taken each day increased, especially for
# days that had few total number of steps. This can be seen by the increase of
# the mean of these values from 9354 to 10706. However, median, 3rd quartile
# and maximum remain the same, which means this change had little to no effect
# in days with high values of total nr steps.



# Are there differences in activity patterns between weekdays and weekends?

library(chron)      # needed for the function is.weekend

dic <- c("weekday","weekend")   # will work as a dictionary

data2$type_day <- dic[is.weekend(data2$date) + 1]
    # if the row corresponds to a week day, the result is
    # dic[is.weekend(...) + 1] = dic[FALSE + 1] = dic[1] = "weekday"
    # similarly, on weekdays we get
    # dic[is.weekend(...) + 1] = dic[TRUE + 1] = dic[2] = "weekend"

data2$type_day <- as.factor(data2$type_day)
head(data2)


# meanweekdays <- rep(0, 288)
# meanweekends <- rep(0, 288)

# weekdata <- filter(data2, type_day == "weekday")
# weekenddata <- filter(data2, type_day == 'weekend')

# n <- nrow(weekdata)
# m <- nrow(weekenddata)

# r <- nrow(weekdata)/288     # nr of week days
# s <- nrow(weekenddata)/288      # nr of weekend days

# for (i in 1:288){
#    meanweekdays[i] <- mean(weekdata$steps[seq(i, n, by = 288)])
#    meanweekends[i] <- mean(weekenddata$steps[seq(i, m, by = 288)])
#}

#par(mfrow=c(2,1))
#plot(meanweekdays, type='l')
#plot(meanweekends, type='l')


meanweekdays <- data2 %>% filter(type_day == "weekday") %>%
    group_by(interval) %>% summarize(average = mean(steps))

meanweekends <- data2 %>% filter(type_day == "weekend") %>%
    group_by(interval) %>% summarize(average = mean(steps))

summary(meanweekdays$average)
summary(meanweekends$average)

library(reshape2)
all <- melt(data.frame(time = seq(1,288), week_days = meanweekdays$average,
                       weekends = meanweekends$average), id.var="time")

library(lattice)
xyplot(value~time|variable, all, type="l",
       layout=c(1,2), main='Average number of steps throughout the day')


