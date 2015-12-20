activity <- read.csv("activity.csv")
#activity$interval <- formatC(activity$interval, width = 4, format = "d", flag = "0")
#activity$DateInterval <- paste(activity$date, activity$interval)
dates <- unique(activity$date)

#activity$DateInterval <- strptime(activity$DateInterval, format ="%Y-%m-%d %H%M")
head(activity)
tail(activity)
summary(activity$steps)

steps_per_day <- tapply(activity$steps, activity$date, sum)
hist(steps_per_day, col = "blue", main = "Histogram of Total Steps Taken Per Day",
     xlab = "Total Steps Taken Per Day")

int <- unique(activity$interval)
avg_steps_across_days <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
#plot(int, avg_steps_across_days, type = "l")

max_steps <- max(avg_steps_across_days)
index_max <- which(avg_steps_across_days == max_steps)
max_steps_int <- activity$interval[index_max]

activity$day <- weekdays(as.Date(activity$date))
activity["dayType"] <- "weekday"
activity$dayType[activity$day %in% c("Saturday", "Sunday")] <- "weekend"

avg_steps_df <- tapply(activity$steps, list(activity$interval, activity$dayType), mean,
                       na.rm = TRUE)
avg_steps_df <- data.frame(avg_steps_df)

