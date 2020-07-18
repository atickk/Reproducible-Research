if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
  filegiven <- tempfile()
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",filegiven)
  unzip(filegiven)
  unlink(filegiven)
}

activity<-read.csv("activity.csv")
head(activity)

#1. Calculate the total number of steps taken per day
stepsperday <-aggregate(steps~date, activity, sum)

#2. Make a histogram of the total number of steps taken each day
hist(stepsperday$steps, xlab = "Class", ylab = "Number of days", main = "Total Number of Steps taken each day" )

#3. Calculate and report the mean and median of the total number of steps taken per day
mean(stepsperday$steps)
median(stepsperday$steps)

#4. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
meanstepsbyinterval <- aggregate(steps~interval, activity, mean)
with(meanstepsbyinterval, plot(interval, steps, type = "l"))

#5. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
meanstepsbyinterval[which.max(meanstepsbyinterval[,2]),1]

#IMPUTING MISSING VALUES
#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with ????????s
missingIndex<-is.na(activity[,1])

#2. Devise a strategy for filling in all of the missing values in the dataset.
m <- mean(meanstepsbyinterval$steps)

#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
activity1 <- activity
activity1[missingIndex,1]<-m
head(activity1)

#4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
stepsperday1 <-aggregate(steps~date, activity1, sum)
hist(stepsperday1$steps, xlab="Class", ylab="Number of Days", main="Number of Steps taken each day after missing values are imputed")

#5.To calculate the mean and median total number of steps per day, we first find total number of steps per day

stepsperday1 <-aggregate(steps~date, activity1, sum)

mean(stepsperday1$steps)
median(stepsperday1$steps)


#Are there differences in activity patterns between weekdays and weekends?

#1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
activity1$date<-as.Date(activity1$date)
library(dplyr)

activity2 <- activity1 %>%
  mutate(dayType= ifelse(weekdays(activity1$date)=="Saturday" | weekdays(activity1$date)=="Sunday", "Weekend", "Weekday"))

head(activity2)

#2. Make a panel plot containing a time series plot (i.e. ???????????????? = "????") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data

meanstespbydaytypeandinterval <- activity2 %>%
  group_by(dayType, interval) %>%
  summarize(stepsperday=sum(steps))

head(meanstespbydaytypeandinterval)

library(lattice)
with(meanstespbydaytypeandinterval, 
     xyplot(stepsperday ~ interval | dayType, 
            type = "l",      
            main = "Total Number of Steps within Intervals by dayType",
            xlab = "Daily Intervals",
            ylab = "Average Number of Steps"))

