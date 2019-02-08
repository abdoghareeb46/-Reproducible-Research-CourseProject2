library(ggplot2)
library(lattice)

data<-read.csv("activity.csv")
data$date<-as.Date(data$date)


steps_by_day <- aggregate(steps ~ date, data, sum)
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
stepmean <- mean(steps_by_day$steps)
stepmedian <- median(steps_by_day$steps)

steps_by_interval <- aggregate(steps ~ interval, data, mean)
plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", 
     ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]

incomplete <- sum(!complete.cases(data))
imputed_data <- transform(data, steps = ifelse(is.na(data$steps),
                 steps_by_interval$steps[match(data$interval, 
                  steps_by_interval$interval)], data$steps))
imputed_data[as.character(imputed_data$date) == "2012-10-01", 1] <- 0


steps_by_day_i <- aggregate(steps ~ date, imputed_data, sum)
hist(steps_by_day_i$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")

#Create Histogram to show difference. 
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "red"), lwd=10)



stepmean.i <- mean(steps_by_day_i$steps)
stepmedian.i <- median(steps_by_day_i$steps)

mean_diff <- stepmean.i - stepmean
med_diff <- stepmedian.i - stepmedian

total_diff <- sum(steps_by_day_i$steps) - sum(steps_by_day$steps)


weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))

steps_by_interval_i <- aggregate(steps ~ interval + dow, imputed_data, mean)


xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow
       , main="Average Steps per Day by Interval"
       ,xlab="Interval", ylab="Steps",layout=c(1,2), type="l")




