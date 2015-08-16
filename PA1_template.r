

setwd("D:/R/RepData_PeerAssessment1")
unzip(zipfile="activity.zip")

d1 <- read.csv("activity.csv", colClasses = c("numeric", "Date", "numeric"))

d1$day <- weekdays(d1$date)
library(ggplot2)


## ------------------------------------------------------------------------

steps <- tapply(d1$steps, d1$date, FUN=sum, na.rm=TRUE)
qplot(steps, binwidth=1300, xlab="total number of steps taken per day")
mean(steps, na.rm=TRUE)
median(steps, na.rm=TRUE)

## ------------------------------------------------------------------------



interval1 <- aggregate(steps ~ interval, data=d1, FUN=mean)
plot(interval1, type="l")


interval1$interval[which.max(interval1$steps)]

## ----missing----------------------------------------------------
missing <- is.na(d1$steps)

table(missing)


## ------------------------------replacing NAs------------------------------------------
rep1 <- aggregate(steps ~ interval + day, d1, mean, na.rm = TRUE)
d2 <- merge(d1, rep1, by=c("interval", "day"))
d2 <- transform(d2, steps.x = ifelse(is.na(steps.x),steps.y,steps.x))
d2 <- data.frame(d2[,1:4])
names(d2) <- c("interval", "day","steps", "date")
d2$steps <- round(d2$steps, digits = 0)
d3 <- d2[order(d2$date, d2$interval),]
## ------------------------------------------------------------------------

steps2 <- tapply(d3$steps, d3$date, FUN=sum, na.rm=TRUE)
qplot(steps2, binwidth=1300, xlab="total number of steps taken per day")
mean(steps2, na.rm=TRUE)
median(steps2, na.rm=TRUE)






## ------------------------------------------------------------------------
d3$daytype <- ifelse(d3$day %in% c("Saturday", "Sunday"),"Weekend", "Weekday")
d4 <- aggregate(steps ~ interval + daytype, d3, mean)


## ------------------------------------------------------------------------
ggplot(d4, aes(x=interval,y=steps)) + 
geom_line(color="purple",size=1) + 
facet_wrap(~daytype, nrow=2, ncol=1) + 
labs(x="Interval",y="Number of Steps") 