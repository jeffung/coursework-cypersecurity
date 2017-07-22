library(zoo)
library(xts)
library(dplyr)
library(ggplot2)
library(lubridate)

apply.hourly <- function(x, FUN,...) {
  ep <- endpoints(x, 'hours')
  period.apply(x, ep, FUN, ...)
}

train <- trainFull[, c(2,4)]
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
plot(z, xaxt = "n", ylab="Average Global Active Power", xlab="Month", xy.lines = TRUE, col="red")
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
plot(z, xaxt = "n", ylab="Average Global Active Power", xlab="Time of Day", col=c("red","green"), plot.type = "single")
axis(1, total$Date, format(total$Date, "%H:%M:%S"), cex.axis = .7)
legend('topleft', names(total)[-1] ,lty=1, col=c("red","green"), bty='n', cex=.75)

JanDay <- hourly %>% filter(Date >= '2007-01-11 00:59:00' & Date <= '2007-01-11 23:59:00')
JanDay$Date <- strftime(JanDay$Date, format="%H:%M:%S")
JanDay$Date <- as.POSIXct(JanDay$Date, format="%H:%M:%S")
JulyDay <- hourly %>% filter(Date >= '2007-07-11 00:59:00' & Date <= '2007-07-11 23:59:00')
JulyDay$Date <- strftime(JulyDay$Date, format="%H:%M:%S")
JulyDay$Date <- as.POSIXct(JulyDay$Date, format="%H:%M:%S")
total <- merge(x = JanDay, y = JulyDay, by = "Date", all.x = TRUE)
colnames(total) <- c("Date", "Jan 9, 2007", "July 9, 2007")

z <- read.zoo(total, header = TRUE)
plot(z, xaxt = "n", ylab="Average Global Active Power", xlab="Time of Day", col=c("red","green"), plot.type = "single")
axis(1, total$Date, format(total$Date, "%H:%M:%S"), cex.axis = .7)
legend('topleft', names(total)[-1] ,lty=1, col=c("red","green"), bty='n', cex=.75)

yearDaily <- daily %>% filter(Date >= '2007-01-01 00:59:00' & Date <= '2007-12-31 23:59:00')
z <- read.zoo(yearDaily, header=TRUE)
plot(z, xaxt = "n", ylab="Average Global Active Power", xlab="Date", col="red", plot.type = "single")
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
plot(z, xaxt="n", ylab="Average Global Active Power", xlab="Date", col=c("red", "green"), plot.type = "single")
axis(1, total$Date, format(as.Date(total$Date), "%b %y"),  cex.axis = .7)
legend('topleft', names(total)[-1] ,lty=1, col=c("red","green"), bty='n', cex=.75)

january <- hourly %>% filter(Date >= "2007-01-01 00:59:00" & Date <= "2007-01-31 23:59:00")
july <- hourly %>% filter(Date >= "2007-07-01 00:59:00" & Date <= "2007-07-31 23:59:00")

par(mfrow=c(2,1))
z <- read.zoo(january, header = TRUE)
plot(z, xaxt="n", ylab="Average Global Active Power", xlab="January", col="red", plot.type = "single")
axis(1, january$Date, format(as.Date(january$Date), "%b %d"),  cex.axis = .7)
z <- read.zoo(july, header = TRUE)
plot(z, xaxt="n", ylab="Average Global Active Power", xlab="July", col="green", plot.type = "single")
axis(1, july$Date, format(as.Date(july$Date), "%b %d"),  cex.axis = .7)

daySub <- minutes %>% filter(Date >= '2007-01-09 16:30:00' & Date <= '2007-01-09 22:00:00')
daySub$Date <- strftime(daySub$Date, format="%H:%M:%S")
daySub$Date <- as.POSIXct(daySub$Date, format="%H:%M:%S")

z <- read.zoo(daySub, header = TRUE)
plot(z, xaxt = "n", ylab="Average Global Active Power", xlab="Time of Day", col="red", plot.type = "single")
axis(1, daySub$Date, format(daySub$Date, "%H:%M:%S"), cex.axis = .7)
summary(daySub)