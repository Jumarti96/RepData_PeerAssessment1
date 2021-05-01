data <- read.csv('https://raw.githubusercontent.com/Jumarti96/RepData_PeerAssessment1/master/activity.csv')
data$date <- as.Date(data$date, '%Y-%m-%d')
#length(data[is.na(data$steps),1])


library(dplyr)
dailysteps <- with(data, tapply(steps, date, sum, na.rm=TRUE))
barplot(dailysteps ~ unique(data$date),
        ylab = "Number of steps", xlab = "Date", main = "Total steps by day")
abline(h = mean(dailysteps), col = 'red', lwd = 2)
abline(h = median(dailysteps), col = 'blue', lwd = 2)
legend("topleft", legend = c("Median", "Mean"),
       col = c("blue", "red"),
       lty = c(1, 1), lwd = c(2, 2), bty = 'n')

print(paste("Mean: ", mean(dailysteps), ", Median: ", median(dailysteps), sep = "")

mean(dailysteps[-which(dailysteps < 100)])
median(dailysteps[-which(dailysteps < 100)])

hist(dailysteps, breaks = 10, main = "Histogram of total steps by day",
     xlab = "Total steps in a given day", ylab = 'Frequency')

avgsteps <- data.frame(meansteps = tapply(data$steps, data$interval, mean, na.rm = TRUE),
                       interval = unique(data$interval), row.names = NULL)

with(avgsteps, plot(interval, meansteps, type = 'l', main = 'Average steps taken everyday by 5 minute interval',
                    xlab = "5 minute interval (each X00 represent a specific hour)", ylab = 'Average steps'))

avgsteps[which.max(avgsteps$meansteps), 'interval']


nas <- which(is.na(data$steps) == TRUE)
for (i in nas) {
        data[i, 1] <- avgsteps[avgsteps$interval == data[i, 'interval'], 'meansteps']
}


filleddata$wday <- weekdays(filleddata$date)
weekdaylist <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekday <- filleddata[filleddata$wday %in% weekdaylist, ]
weekend <- filleddata[!filleddata$wday %in% weekdaylist, ]

Weekday <- data.frame(meansteps = tapply(weekday$steps, weekday$interval, mean),
                       interval = unique(weekday$interval), row.names = NULL)
Weekend <- data.frame(meansteps = tapply(weekend$steps, weekend$interval, mean),
                         interval = unique(weekend$interval), row.names = NULL)


par(mfrow = c(2, 1), oma = c(4, 4, 2, 2))
with(Weekday, plot(interval, meansteps, type = 'l', axes = FALSE))
axis(2L)
box()
with(Weekend, plot(interval, meansteps, type = 'l', axes = FALSE))

axis(1L)
axis(2L)
box()
mtext("Average steps taken on weekdays by 5 minute interval", outer = TRUE, side = 1, line = -20.5, cex = 1.3)
mtext("5 minute interval (each X00 represent a specific hour", side = 1, outer = TRUE, line = 2.5)
mtext("Weekdays", side = 2, outer = TRUE, line = 2.5, at = 0.75)
mtext("Weekends", side = 2, outer = TRUE, line = 2.5, at = 0.25)

