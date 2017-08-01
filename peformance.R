library(bitops)
library(reshape)

windowSize <- 5
# Performance check
# Validation set
Length <- length(train$Date)
start <- which(train$Date == as.POSIXct("2009-07-01 00:00:00"))
#hmm <- hmmfit(train$Global_active_power[1:(start-1)], startmodel, mstep=mstep.norm, maxit=200)

validationSet <- train[(start:Length),]
validationSet$timestamp <- as.numeric(validationSet$Date)
validationSet$timestamp <- validationSet$timestamp - as.numeric(as.POSIXct("2009-01-01 00:00:00"))
validationSet$timestamp <- validationSet$timestamp %% 86400
# Divide the sets by moring, evening, and rest
dawn <- validationSet %>% filter(timestamp >= 3600 & timestamp < 21600)
morning <- validationSet %>% filter(timestamp >= 21600 & timestamp < 57600)
# afternoon <- validationSet %>% filter(timestamp >= 28800 & timestamp < 57600)
# evening <- validationSet %>% filter(timestamp >= 57600 & timestamp < 75600)
night <- validationSet %>% filter(timestamp >= 57600 | timestamp < 3600)

# Divide the set by season
summer1 <- dawn %>% filter(Date >= "2009-07-01 00:00:00" & Date <= "2009-08-31 00:00:00")
summer1 <- matrix(summer1$Global_active_power, nrow=floor(nrow(summer1)/300), ncol=300, byrow=TRUE)

summer2 <- morning %>% filter(Date >= "2009-07-01 00:00:00" & Date <= "2009-08-31 00:00:00")
summer2 <- matrix(summer2$Global_active_power, nrow=floor(nrow(summer2)/600), ncol=600, byrow=TRUE)

# summer3 <- afternoon %>% filter(Date >= "2009-07-01 00:00:00" & Date <= "2009-08-31 00:00:00")
# summer3 <- matrix(summer3$Global_active_power, nrow=floor(nrow(summer3)/480), ncol=480, byrow=TRUE)

# summer4 <- evening %>% filter(Date >= "2009-08-01 00:00:00" & Date <= "2009-08-31 00:00:00")
# summer4 <- matrix(summer4$Global_active_power, nrow=floor(nrow(summer4)/300), ncol=300, byrow=TRUE)
# 
# summer5 <- night %>% filter(Date >= "2009-08-01 00:00:00" & Date <= "2009-08-31 00:00:00")
# summer5 <- matrix(summer5$Global_active_power[1:nrow(summer5)-1], nrow=floor(nrow(summer5)/240), ncol=240, byrow=TRUE)

summer3 <- night %>% filter(Date >= "2009-07-01 00:00:00" & Date <= "2009-08-31 00:00:00")
summer3 <- matrix(summer3$Global_active_power[1:nrow(summer3)-1], nrow=floor(nrow(summer3)/540), ncol=540, byrow=TRUE)

fall1 <- dawn %>% filter(Date >= "2009-09-01 00:00:00" & Date <= "2009-10-31 00:00:00")
fall1 <- matrix(fall1$Global_active_power, nrow=floor(nrow(fall1)/300), ncol=300, byrow=TRUE)

fall2 <- morning %>% filter(Date >= "2009-09-01 00:00:00" & Date <= "2009-10-31 00:00:00")
fall2 <- matrix(fall2$Global_active_power, nrow=floor(nrow(fall2)/600), ncol=600, byrow=TRUE)

# fall3 <- afternoon %>% filter(Date >= "2009-09-01 00:00:00" & Date <= "2009-10-31 00:00:00")
# fall3 <- matrix(fall3$Global_active_power, nrow=floor(nrow(fall3)/480), ncol=480, byrow=TRUE)

# fall4 <- evening %>% filter(Date >= "2009-09-01 00:00:00" & Date <= "2009-10-31 00:00:00")
# fall4 <- matrix(fall4$Global_active_power, nrow=floor(nrow(fall4)/300), ncol=300, byrow=TRUE)
# 
# fall5 <- night %>% filter(Date >= "2009-09-01 00:00:00" & Date <= "2009-10-31 00:00:00")
# fall5 <- matrix(fall5$Global_active_power[1:nrow(fall5)-1], nrow=floor(nrow(fall5)/240), ncol=240, byrow=TRUE)

fall3 <- night %>% filter(Date >= "2009-09-01 00:00:00" & Date <= "2009-10-31 00:00:00")
fall3 <- matrix(fall3$Global_active_power[1:nrow(fall3)-1], nrow=floor(nrow(fall3)/540), ncol=540, byrow=TRUE)

winter1 <- dawn %>% filter(Date >= "2009-11-01 00:00:00" & Date <= "2009-12-31 00:00:00")
winter1 <- matrix(winter1$Global_active_power[292:nrow(winter1)], nrow=floor(nrow(winter1)/300), ncol=300, byrow=TRUE)

winter2 <- morning %>% filter(Date >= "2009-11-01 00:00:00" & Date <= "2009-12-31 00:00:00")
winter2 <- matrix(winter2$Global_active_power[1:18000], nrow=floor(nrow(winter2)/600), ncol=600, byrow=TRUE)

# winter3 <- afternoon %>% filter(Date >= "2009-11-01 00:00:00" & Date <= "2009-12-31 00:00:00")
# winter3 <- matrix(winter3$Global_active_power[1:14400], nrow=floor(nrow(winter3)/480), ncol=480, byrow=TRUE)

# winter4 <- evening %>% filter(Date >= "2009-11-01 00:00:00" & Date <= "2009-12-31 00:00:00")
# winter4 <- matrix(winter4$Global_active_power, nrow=floor(nrow(winter4)/300), ncol=300, byrow=TRUE)
# 
# winter5 <- night %>% filter(Date >= "2009-11-01 00:00:00" & Date <= "2009-12-31 00:00:00")
# winter5 <- matrix(winter5$Global_active_power[70:7029], nrow=floor(nrow(winter5)/240), ncol=240, byrow=TRUE)

winter3 <- night %>% filter(Date >= "2009-11-01 00:00:00" & Date <= "2009-12-31 00:00:00")
winter3 <- matrix(winter3$Global_active_power[70:15729], nrow=floor(nrow(winter3)/540), ncol=540, byrow=TRUE)

logliks <- function(r, n){
  loglikVals <- vector(mode="double", length=(n-windowSize-1))
  j = 1
  for (i in seq(1,(n-windowSize-1))){
    start = i
    end = i+windowSize-1
    pred <- predict(hmm, r[start:end], method="viterbi")
    thresholds <- abs(hmm$model$parms.emission$mu[pred$s]-pred$x)
    if(length(which(thresholds > 1.0)) <= (0.4*windowSize)){
      loglikVals[j] = pred$loglik
      j = j + 1;
    }
  }
  return(as.data.frame(cbind(min(loglikVals[which(loglikVals<0)]), max(loglikVals[which(loglikVals<0)]))))
}

# Calculate loglik intervals 
a1 <- logliks(summer1[1,],ncol(summer1))
a2 <- logliks(summer2[1,],ncol(summer2))
a3 <- logliks(summer3[1,],ncol(summer3))
# a4 <- logliks(summer4[1,],ncol(summer4))
# a5 <- logliks(summer5[1,],ncol(summer5))

b1 <- logliks(fall1[1,],ncol(fall1))
b2 <- logliks(fall2[1,],ncol(fall2))
b3 <- logliks(fall3[1,],ncol(fall3))
# b4 <- logliks(fall4[1,],ncol(fall4))
# b5 <- logliks(fall5[1,],ncol(fall5))

c1 <- logliks(winter1[1,],ncol(winter1))
c2 <- logliks(winter2[1,],ncol(winter2))
c3 <- logliks(winter3[1,],ncol(winter3))
# c4 <- logliks(winter4[1,],ncol(winter4))
# c5 <- logliks(winter5[1,],ncol(winter5))

loglikValues <- rbind(a1,a2,a3,b1,b2,b3,c1,c2,c3)

getlogliks <- function(x){
  timestamp <- (as.numeric(x) - as.numeric(as.POSIXct("2007-01-01 00:00:00"))) %% 86400
  if (timestamp >= 3600 & timestamp < 21600){
    return(loglikValues[1,])
  }
  else if (timestamp >= 21600 & timestamp < 57600 ){
    return(loglikValues[2,])
  }
  # else if (timestamp >= 28800 & timestamp < 57600){
  #   return(loglikValues[3,])
  # }
  # else if (timestamp >= 57600 & timestamp < 75600){
  #   return(loglikValues[4,])
  # }
  else{
    return(loglikValues[3,])
  }
}

# just small noise samples of each type
# continuous
set <- train[(294557:294656),]
corrupt <- rep(0,100)
s <- rnorm(5,4.5,5)
set$Global_active_power[3:7] <- s+set$Global_active_power[3:7]
corrupt[3:7] <- rep(1,5)

g <- rbinom(20,1,0.3)
noise <- rnorm(20,5,1) # generate the noise to add
noise <- g*noise
corrupt[20:39] <- g
set$Global_active_power[20:39] <- noise+set$Global_active_power[20:39]

g <- rep(c(0,0,1), 10)
corrupt[40:69] <- g
noise <- rnorm(30,6,1)
noise <- g*noise
set$Global_active_power[40:69] <- noise+set$Global_active_power[40:69]

s <- rnorm(5,4.5,5)
set$Global_active_power[82:86] <- s+set$Global_active_power[82:86]
corrupt[82:86] <- rep(1,5)

g <- rbinom(10,1,0.1)
noise <- rnorm(10,6,1) # generate the noise to add
noise <- g*noise
corrupt[90:99] <- g
set$Global_active_power[90:99] <- noise+set$Global_active_power[90:99]

set <- rbind(train[294548:294556,], set)
anomaly <- vector(mode="integer", length=100)

for (i in seq(1,100)){
  start <- i
  end <- start+windowSize-1
  X1 <- predict(hmm, set$Global_active_power[start:end], method="viterbi")
  minmax <- getlogliks(set[start,1])
  if (X1$loglik >= minmax[1] & X1$loglik <= minmax[2]){
    anomaly[i] <- 0
  } else{
    anomaly[i] <- 1
    set$Global_active_power[end] <- hmm$model$parms.emission$mu[X1$s[windowSize]]
  }
}


precision = length(which(bitAnd(anomaly,corrupt)==1))/length(which(anomaly==1))
recall = length(which(bitAnd(anomaly,corrupt)==1))/length(which(corrupt==1))
fmeasure = (2*precision*recall)/(precision+recall)
sprintf('Recall: %f'  , recall)
sprintf('Precision: %f'  , precision)
sprintf('F-measure: %f', fmeasure)



