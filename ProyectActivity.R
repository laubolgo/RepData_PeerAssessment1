##Code for reading in the dataset and/or processing the data
library(readr)
activity <- read_csv("D:/activity.csv")
View(activity)
activity$date <- as.Date(activity$date, "%Y-%m-%d")
activity <- as.data.frame(activity)
##Histogram of the total number of steps taken each day
###Sum steps by day, create Histogram, and calculate mean and median.
colMeans(is.na(activity))
steps_day <- aggregate(steps ~ date, rm.na = TRUE, data = activity, FUN = sum)
plot(steps_day, type = "h", lwd = 10, lend = "square")
steps <- with(activity, tapply(steps, date, sum, na.rm = TRUE))
mean(steps)
median(steps)
##What is the average daily activity pattern?
daymeans <- with(na.omit(activity), tapply(steps, interval, mean))
head(daymeans)
plot(aggregate(steps ~ interval, data = activity, FUN = mean), type = "l")
max(activity$steps, na.rm = TRUE)
##Imputing missing values
library(scales)
sum(is.na(activity))
percent(sum(is.na(activity))/nrow(activity))
head(activity)
tail(activity)
###*The int and len variable are set up to manage the for loop sequences. The NAin and NA steps variables are sections of the data that will be used to replace the NA data after the loop.*
int <- unique(activity$interval)  
len <- nrow(activity[is.na(activity),])
NAint <-  activity[is.na(activity),3]
NAsteps <- activity[is.na(activity),1]
for (j in 1:2304) {
  for (i in 1:288){
    if (NAint[j] == int[i])
      NAsteps[j] <- daymeans[i]
    
  }
}

NAindex <- is.na(activity$steps)
activity$steps<- replace(activity$steps,NAindex, NAsteps)
###As can be seen here, the NAs have been replaced by the relevant mean for the 5 minute interval.
head(activity)
tail(activity)
###histogram with the updated data. 
plot(activity$step, type = "h", lwd = 10, lend = "square")
###Calculate and report the mean and median total number of steps taken per day.
steps <- with(activity, tapply(steps, date, sum, na.rm = TRUE))
mean(steps)
median(steps)
###Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
###Yes, they are changed, because the NAs were replaced based on the interval means, the mean and the median for the day now match up.
##Are there differences in activity patterns between weekdays and weekends?
library(dplyr)
activity <- mutate(activity, day = weekdays(activity$date))

weekdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

activity$day <- factor((weekdays(activity$date) %in% weekdays), 
                       levels=c(FALSE, TRUE), labels=c('Weekend', 'Weekday'))
weekdays <- subset(activity, day == "Weekday")
weekends <- subset(activity, day == "Weekend")

weekendmeans <- with(weekends, tapply(steps, interval, mean))
weekdaymeans <- with(weekdays, tapply(steps, interval, mean))


