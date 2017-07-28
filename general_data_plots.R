library(zoo)
library(xts)
library(dplyr)
library(ggplot2)
library(lubridate)

apply.hourly <- function(x, FUN,...) {
  ep <- endpoints(x, 'hours')
  period.apply(x, ep, FUN, ...)
}

train <- trainFull[, c(1,3)]
colnames(train) <- c("DateTime", "Global_active_power")
train$DateTime <- paste(trainFull$Date, trainFull$Time)
train$DateTime <- as.POSIXct(train$DateTime, format='%d/%m/%Y %H:%M:%S')
train <- na.omit(train)

trainTS <- xts(train$Global_active_power, order.by = train$DateTime)
minutes <- fortify(trainTS)
hourly <- fortify(apply.hourly(trainTS,mean))
daily <- fortify(apply.daily(trainTS,mean))
monthly <- fortify(apply.monthly(trainTS,mean)) 

colnames(minutes) <- c("Date", "Global_active_power")
colnames(hourly) <- c("Date", "Global_active_power")
colnames(daily) <- c("Date", "Global_active_power")
colnames(monthly) <- c("Date", "Global_active_power")

z <- read.zoo(monthly, header = TRUE)
plot(z, xaxt = "n", ylab="Average Global Active Power (kW)", xlab="Month", xy.lines = TRUE, col="red")
axis(1, monthly$Date, format(monthly$Date, "%b-%y"), cex.axis = .7)

JanDay <- minutes %>% filter(Date >= '2007-01-09 00:59:00' & Date <= '2007-01-09 23:59:00')
JanDay$Date <- strftime(JanDay$Date, format="%H:%M:%S")
JanDay$Date <- as.POSIXct(JanDay$Date, format="%H:%M:%S")
JulyDay <- minutes %>% filter(Date >= '2007-07-09 00:59:00' & Date <= '2007-07-09 23:59:00')
JulyDay$Date <- strftime(JulyDay$Date, format="%H:%M:%S")
JulyDay$Date <- as.POSIXct(JulyDay$Date, format="%H:%M:%S")
total <- merge(x = JanDay, y = JulyDay, by = "Date", all.x = TRUE)
colnames(total) <- c("Date", "Jan 9, 2007", "July 9, 2007")

z <- read.zoo(total, header = TRUE)
plot(z, xaxt = "n", ylab="Global Active Power (kW)", xlab="Time of Day", col=c("red","green"), plot.type = "single")
axis(1, total$Date, format(total$Date, "%H:%M:%S"), cex.axis = .7)
legend('topleft', names(total)[-1] ,lty=1, col=c("red","green"), bty='n', cex=.75)

JanDay <- hourly %>% filter(Date >= '2007-01-09 00:59:00' & Date <= '2007-01-09 23:59:00')
JanDay$Date <- strftime(JanDay$Date, format="%H:%M:%S")
JanDay$Date <- as.POSIXct(JanDay$Date, format="%H:%M:%S")
JulyDay <- hourly %>% filter(Date >= '2007-07-09 00:59:00' & Date <= '2007-07-09 23:59:00')
JulyDay$Date <- strftime(JulyDay$Date, format="%H:%M:%S")
JulyDay$Date <- as.POSIXct(JulyDay$Date, format="%H:%M:%S")
total <- merge(x = JanDay, y = JulyDay, by = "Date", all.x = TRUE)
colnames(total) <- c("Date", "Jan 9, 2007", "July 9, 2007")

z <- read.zoo(total, header = TRUE)
plot(z, xaxt = "n", ylab="Average Global Active Power (kW)", xlab="Time of Day", col=c("red","green"), plot.type = "single")
axis(1, total$Date, format(total$Date, "%H:%M:%S"), cex.axis = .7)
legend('topleft', names(total)[-1] ,lty=1, col=c("red","green"), bty='n', cex=.75)

yearDaily <- daily %>% filter(Date >= '2007-01-01 00:59:00' & Date <= '2007-12-31 23:59:00')
z <- read.zoo(yearDaily, header=TRUE)
plot(z, xaxt = "n", ylab="Average Global Active Power (kW)", xlab="Date", col="red", plot.type = "single")
axis(1, yearDaily$Date, format(yearDaily$Date, "%b-%y"), cex.axis = .7)

dayHourlyAverage <- with( hourly , hourly[ hour( Date ) >= 8 & hour( Date ) < 20, ] )
dayHourlyAverage <- dayHourlyAverage %>% filter(Date >= '2007-01-01 00:59:00' & Date <= '2007-12-31 23:59:00')
dayHourlyAverage <- aggregate.data.frame(x=dayHourlyAverage$Global_active_power, 
                                         by = list(Date = substr(dayHourlyAverage$Date, 1, 10)), 
                                         FUN=mean)

nightHourlyAverage <- with( hourly , hourly[ hour( Date ) < 8 | hour( Date ) >= 20, ] )
nightHourlyAverage <- nightHourlyAverage %>% filter(Date >= '2007-01-01 00:59:00' & Date <= '2007-12-31 23:59:00')
nightHourlyAverage <- aggregate.data.frame(x=nightHourlyAverage$Global_active_power, 
                                           by = list(Date = substr(nightHourlyAverage$Date, 1, 10)), 
                                           FUN=mean)

total <- merge(x = dayHourlyAverage, y = nightHourlyAverage, by = "Date", all.x = TRUE)
colnames(total) <- c("Date", "Day", "Night")
total$Date <- as.POSIXct(paste(as.character(total$Date), "21:01:00"), format="%Y-%m-%d %H:%M:%S")

z <- read.zoo(total, header = TRUE)
plot(z, xaxt="n", ylab="Average Global Active Power (kW)", xlab="Date", col=c("red", "green"), plot.type = "single")
axis(1, total$Date, format(as.Date(total$Date), "%b %y"),  cex.axis = .7)
legend('topright', names(total)[-1] ,lty=1, col=c("red","green"), bty='n', cex=.75)

january <- hourly %>% filter(Date >= "2007-01-01 00:59:00" & Date <= "2007-01-31 23:59:00")
july <- hourly %>% filter(Date >= "2007-07-01 00:59:00" & Date <= "2007-07-31 23:59:00")

par(mfrow=c(2,1))
z <- read.zoo(january, header = TRUE)
plot(z, xaxt="n", ylab="Average Global Active Power (kW)", xlab="January", col="red", plot.type = "single")
axis(1, january$Date, format(as.Date(january$Date), "%b %d"),  cex.axis = .7)
z <- read.zoo(july, header = TRUE)
plot(z, xaxt="n", ylab="Average Global Active Power (kW)", xlab="July", col="green", plot.type = "single")
axis(1, july$Date, format(as.Date(july$Date), "%b %d"),  cex.axis = .7)

averageSummerDay <- cbind(c(1:28),c(1:28)) 
colnames(averageSummerDay) <- c("Day", "Global_active_power")
averageWinterDay <- cbind(c(1:28),c(1:28)) 
colnames(averageWinterDay) <- c("Day", "Global_active_power")
for (i in 1:28) {
  tmp <- with(minutes, minutes[day(Date) == i & month(Date) >= 6 & month(Date) <= 9, ])
  averageSummerDay[i,2] <- mean(tmp$Global_active_power, na.rm = TRUE)
  tmp <- with(minutes, minutes[day(Date) == i & month(Date) <= 2 | month(Date) >= 11, ])
  averageWinterDay[i,2] <- mean(tmp$Global_active_power, na.rm = TRUE)
}
rm(tmp)

par(mfrow=c(2,1))
plot(averageSummerDay, xlab="Day of the Month - Summer", ylab="Average Global Active Power (kW)", col="red", type="l")
plot(averageWinterDay, xlab="Day of the Month - Winter", ylab="Average Global Active Power (kW)", col="green", type="l")

overallTrend <- c(1:3)
tmp <- with(minutes, minutes[year(Date) == 2007, ])
overallTrend[1] <- mean(tmp$Global_active_power, na.rm = TRUE)
tmp <- with(minutes, minutes[year(Date) == 2008, ])
overallTrend[2] <- mean(tmp$Global_active_power, na.rm = TRUE)
tmp <- with(minutes, minutes[year(Date) == 2009, ])
overallTrend[3] <- mean(tmp$Global_active_power, na.rm = TRUE)

janTrend <- c(1:3)
tmp <- with(minutes, minutes[year(Date) == 2007 & month(Date) == 1, ])
janTrend[1] <- mean(tmp$Global_active_power, na.rm = TRUE)
tmp <- with(minutes, minutes[year(Date) == 2008 & month(Date) == 1, ])
janTrend[2] <- mean(tmp$Global_active_power, na.rm = TRUE)
tmp <- with(minutes, minutes[year(Date) == 2009 & month(Date) == 1, ])
janTrend[3] <- mean(tmp$Global_active_power, na.rm = TRUE)

julyTrend <- c(1:3)
tmp <- with(minutes, minutes[year(Date) == 2007 & month(Date) == 7, ])
julyTrend[1] <- mean(tmp$Global_active_power, na.rm = TRUE)
tmp <- with(minutes, minutes[year(Date) == 2008 & month(Date) == 7, ])
julyTrend[2] <- mean(tmp$Global_active_power, na.rm = TRUE)
tmp <- with(minutes, minutes[year(Date) == 2009 & month(Date) == 7, ])
julyTrend[3] <- mean(tmp$Global_active_power, na.rm = TRUE)

x <- c(2007, 2008, 2009)
plot(seq(2007,2009,0.5), seq(0.7,1.9,0.3), type="n", xlab="Year", ylab="Global Active Power (kW)", xaxt="n")
axis(side=1, at=c(2007, 2008, 2009))
legend('topright', c("Overall Trend", "Trend of January", "Trend of July") , lty=1, col=c("red","green", "blue"), bty='n', cex=.75)
points(x, overallTrend, col="red")
points(x, janTrend, col="green", pch=22)
points(x, julyTrend, col="blue", pch=24)
lines(x, overallTrend, col="red")
lines(x, janTrend, col="green")
lines(x, julyTrend, col="blue")

winterTrend <- c(1:3)
tmp <- with(minutes, minutes[year(Date) == 2007 & (month(Date) <= 2 | month(Date) >= 11), ])
winterTrend[1] <- mean(tmp$Global_active_power, na.rm = TRUE)
tmp <- with(minutes, minutes[year(Date) == 2008 & (month(Date) <= 2 | month(Date) >= 11), ])
winterTrend[2] <- mean(tmp$Global_active_power, na.rm = TRUE)
tmp <- with(minutes, minutes[year(Date) == 2009 & (month(Date) <= 2 | month(Date) >= 11), ])
winterTrend[3] <- mean(tmp$Global_active_power, na.rm = TRUE)

summerTrend <- c(1:3)
tmp <- with(minutes, minutes[year(Date) == 2007 & month(Date) >=5 & month(Date) <= 8, ])
summerTrend[1] <- mean(tmp$Global_active_power, na.rm = TRUE)
tmp <- with(minutes, minutes[year(Date) == 2008 & month(Date) >=5 & month(Date) <= 8, ])
summerTrend[2] <- mean(tmp$Global_active_power, na.rm = TRUE)
tmp <- with(minutes, minutes[year(Date) == 2009 & month(Date) >=5 & month(Date) <= 8, ])
summerTrend[3] <- mean(tmp$Global_active_power, na.rm = TRUE)

sfTrend <- c(1:3)
tmp <- with(minutes, minutes[year(Date) == 2007 & ((month(Date) >= 3 & month(Date) <= 4) | (month(Date) >= 9 & month(Date) <= 10)), ])
sfTrend[1] <- mean(tmp$Global_active_power, na.rm = TRUE)
tmp <- with(minutes, minutes[year(Date) == 2008 & ((month(Date) >= 3 & month(Date) <= 4) | (month(Date) >= 9 & month(Date) <= 10)), ])
sfTrend[2] <- mean(tmp$Global_active_power, na.rm = TRUE)
tmp <- with(minutes, minutes[year(Date) == 2009 & ((month(Date) >= 3 & month(Date) <= 4) | (month(Date) >= 9 & month(Date) <= 10)), ])
sfTrend[3] <- mean(tmp$Global_active_power, na.rm = TRUE)

plot(seq(2007,2009,0.5), seq(0.7,1.9,0.3), type="n", xlab="Year", ylab="Global Active Power (kW)", xaxt="n")
axis(side=1, at=c(2007, 2008, 2009))
legend('topright', c("Overall Trend", "Trend of Winter", "Trend of Summer", "Trend of Spring/Fall"), 
       lty=1, col=c("black", "green","blue", "red"), bty='n', cex=.75)
points(x, overallTrend)
points(x, winterTrend, col="green", pch=22)
points(x, summerTrend, col="blue", pch=24)
points(x, sfTrend, col="red", pch=3)
lines(x, overallTrend)
lines(x, winterTrend, col="green")
lines(x, summerTrend, col="blue")
lines(x, sfTrend, col="red")
