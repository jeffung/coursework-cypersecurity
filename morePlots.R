library(zoo)

train <- trainFull[, c(1,3)]
train$Date <- paste(trainFull$Date, trainFull$Time)
train$Date <- as.POSIXct(train$Date, format='%d/%m/%Y %H:%M:%S')
train <- na.omit(train)

# Compare models
# HMM
Jan <- train %>% filter(Date >= '2007-01-01 00:00:00' & Date <= '2007-01-31 23:59:00')
plot(Jan, xaxt="n", ylab="Global Active Power(kw)", type="l")
check <- predict(hmm, Jan$Global_active_power)
points(Jan$Date,hmm$model$parms.emission$mu[check$s], col="red")
axis(1, Jan$Date, format(Jan$Date, "%b %d"), cex.axis = .7)
legend('topleft',legend=c('Original','Predicted by HMM'), lty=c(1,0), pch=c('','o'), col=c("black","red"), bty='n', cex=.75)

# Hourly trend over 2 years
h <- train
h$Date <- strftime(h$Date, format="%H:%M:%S")
h <-aggregate(h$Global_active_power, by=list(Date= h$Date), FUN=mean)
h$Date <- as.POSIXct(h$Date, format="%H:%M:%S")
plot(h, xaxt = "n", ylab="Average Global Active Power", xlab="Time of Day")
axis(1, h$Date, format(h$Date, "%H:%M:%S"), cex.axis = .7)

# Plot the whole data points
z <- read.zoo(train, header = TRUE)
plot(z, xaxt = "n", ylab="Global Active Power (kW)", xlab="Year")
axis(1, train$Date, format(train$Date, "%Y-%B"), cex.axis = .7)

# Weekly trend
Julyweek1 <- train %>% filter(Date >= '2007-07-02 00:00:00' & Date <= '2007-07-08 23:59:00')
plot(Julyweek1, xaxt = "n", ylab="Global Active Power", xlab="Time of Day", type="l")
Julyweek2 <- train %>% filter(Date >= '2007-07-09 00:00:00' & Date <= '2007-07-15 23:59:00')
lines(Julyweek1$Date, Julyweek2$Global_active_power, col="red")
Julyweek3 <- train %>% filter(Date >= '2007-07-16 00:00:00' & Date <= '2007-07-22 23:59:00')
lines(Julyweek1$Date, Julyweek2$Global_active_power, col="green")
Julyweek4 <- train %>% filter(Date >= '2007-07-23 00:00:00' & Date <= '2007-07-29 23:59:00')
lines(Julyweek1$Date, Julyweek2$Global_active_power, col="blue")
axis(1, Julyweek1$Date, format(Julyweek1$Date, "%A"), cex.axis = .7)
legend('topleft',legend=c('1st week','2nd week','3rd week', '4th week'), lty=c(1,1,1,1), col=c("black","red", "green","blue"), bty='n', cex=.75)

# Other variable

other<- trainFull[, c(1,7,8,9)]
other$Date <- paste(trainFull$Date, trainFull$Time)
other$Date <- as.POSIXct(other$Date, format='%d/%m/%Y %H:%M:%S')
other <- na.omit(other)

Julyweek1 <- other %>% filter(Date >= '2007-07-02 00:00:00' & Date <= '2007-07-08 23:59:00')
plot(Julyweek1$Date, Julyweek1$Sub_metering_1, xaxt = "n", ylab="Sub_metering", xlab="Time of Day", type="l")
lines(Julyweek1$Date, Julyweek1$Sub_metering_2, col="red")
lines(Julyweek1$Date, Julyweek1$Sub_metering_3, col="green")
axis(1, Julyweek1$Date, format(Julyweek1$Date, "%A"), cex.axis = .7)
legend('topleft',legend=c('Sub_metering_1','Sub_metering_2','Sub_metering_3'), lty=c(1,1), col=c("black","red", "green"), bty='n', cex=.75)
