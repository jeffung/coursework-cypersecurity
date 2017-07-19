library(zoo)
library(xts)
library(dplyr)

apply.hourly <- function(x, FUN,...) {
  ep <- endpoints(x, 'hours')
  period.apply(x, ep, FUN, ...)
}

trainTS <- xts(train$Global_active_power, order.by = train$DateTime)
hourly <- fortify(apply.hourly(trainTS,mean))
daily <- fortify(apply.daily(trainTS,mean))
monthly <- fortify(apply.monthly(trainTS,mean)) 

colnames(hourly) <- c("Date", "Global_active_power")
colnames(daily) <- c("Date", "Global_active_power")
colnames(monthly) <- c("Date", "Global_active_power")

z <- read.zoo(monthly, header = TRUE)
plot(z, xaxt = "n", ylab="Average Monthly Global Active Power", xlab="Month-Year", xy.lines = TRUE)
axis(1, monthly$Date, format(monthly$Date, "%b-%y"), cex.axis = .7)

JanDay <- hourly %>% filter(Date >= '2007-01-09 00:59:00' & Date <= '2007-01-09 23:59:00')
JanDay$Date <- strftime(JanDay$Date, format="%H:%M:%S")
JanDay$Date <- as.POSIXct(JanDay$Date, format="%H:%M:%S")
JulyDay <- hourly %>% filter(Date >= '2007-07-09 00:59:00' & Date <= '2007-07-09 23:59:00')
JulyDay$Date <- strftime(JulyDay$Date, format="%H:%M:%S")
JulyDay$Date <- as.POSIXct(JulyDay$Date, format="%H:%M:%S")
total <- merge(x = JanDay, y = JulyDay, by = "Date", all.x = TRUE)
colnames(total) <- c("Date", "Jan 9, 2007", "July 9, 2007")

z <- read.zoo(total, header = TRUE)
plot(z, xaxt = "n", ylab="Average Hourly Global Active Power", xlab="Time of Day", col=c("red","green"), plot.type = "single")
axis(1, total$Date, format(total$Date, "%H:%M:%S"), cex.axis = .7)
legend('topleft', names(total)[-1] ,lty=1, col=c("red","green"), bty='n', cex=.75)
