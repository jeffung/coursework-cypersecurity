library(reshape)
source("load_data.R")

train <- trainFull[, c(1,3)]
train$Date <- paste(trainFull$Date, trainFull$Time)
train$Date <- as.POSIXct(train$Date, format='%d/%m/%Y %H:%M:%S')
train <- na.omit(train)
 

test1 <- test1Full[, c(1,3)]
test1$Date <- paste(test1Full$Date, test1Full$Time)
test1$Date <- as.POSIXct(test1$Date, format='%d/%m/%Y %H:%M:%S')
test1 <- na.omit(test1)

test2 <- test2Full[, c(1,3)]
test2$Date <- paste(test2Full$Date, test2Full$Time)
test2$Date <- as.POSIXct(test2$Date, format='%d/%m/%Y %H:%M:%S')
test2 <- na.omit(test2)

threshold <- c(0.5, 1, 1.25, 1.5, 1.75, 2) 

windowSize <- 5
# HMM Model
# number of states HMM    
k=7

#init probabilities
init <- rep(1/k, k)

#transition matrix
P <- matrix(rep(1/k, k*k), nrow = k)

#emission matrix:  here I used a Gaussian distribution, replace muEst and sigmaEst by your initial estimates of mean and variance
muVec <- c(0.6484832, 3.1713822, 1.2757978,  2.4676883, 1.8237814, 1.5377176, 4.7048570)
sigmaVec <- c(0.008257707, 0.3858464933, 0.1301456018, 0.0252060821, 0.4619841991, 0.0251711090, 1.0975356331)

b <- list(mu = muVec, sigma = sigmaVec) 

#starting model for EM
startmodel <- hmmspec(init = init, trans = P, parms.emis = b, dens.emis = dnorm.hsmm)

# Validation set
Length <- length(train$Date)
start <- which(train$Date == as.POSIXct("2009-07-01 00:00:00"))
# hmm <- hmmfit(train$Global_active_power[1:(start-1)], startmodel, mstep=mstep.norm, maxit=200)

validationSet <- train[(start:Length),]
validationSet$timestamp <- as.numeric(validationSet$Date)
validationSet$timestamp <- validationSet$timestamp - as.numeric(as.POSIXct("2009-01-01 00:00:00"))
validationSet$timestamp <- validationSet$timestamp %% 86400

# Divide the sets by moring, evening, and rest
dawn <- validationSet %>% filter(timestamp >= 3600 & timestamp < 21600)
morning <- validationSet %>% filter(timestamp >= 21600 & timestamp < 57600)
night <- validationSet %>% filter(timestamp >= 57600 | timestamp < 3600)

# Divide the set by season
summer1 <- dawn %>% filter(Date >= "2009-07-01 00:00:00" & Date <= "2009-08-31 00:00:00")
summer1 <- matrix(summer1$Global_active_power, nrow=floor(nrow(summer1)/300), ncol=300, byrow=TRUE)

summer2 <- morning %>% filter(Date >= "2009-07-01 00:00:00" & Date <= "2009-08-31 00:00:00")
summer2 <- matrix(summer2$Global_active_power, nrow=floor(nrow(summer2)/600), ncol=600, byrow=TRUE)

summer3 <- night %>% filter(Date >= "2009-07-01 00:00:00" & Date <= "2009-08-31 00:00:00")
summer3 <- matrix(summer3$Global_active_power[1:nrow(summer3)-1], nrow=floor(nrow(summer3)/540), ncol=540, byrow=TRUE)

fall1 <- dawn %>% filter(Date >= "2009-09-01 00:00:00" & Date <= "2009-10-31 00:00:00")
fall1 <- matrix(fall1$Global_active_power, nrow=floor(nrow(fall1)/300), ncol=300, byrow=TRUE)

fall2 <- morning %>% filter(Date >= "2009-09-01 00:00:00" & Date <= "2009-10-31 00:00:00")
fall2 <- matrix(fall2$Global_active_power, nrow=floor(nrow(fall2)/600), ncol=600, byrow=TRUE)

fall3 <- night %>% filter(Date >= "2009-09-01 00:00:00" & Date <= "2009-10-31 00:00:00")
fall3 <- matrix(fall3$Global_active_power[1:nrow(fall3)-1], nrow=floor(nrow(fall3)/540), ncol=540, byrow=TRUE)

winter1 <- dawn %>% filter(Date >= "2009-11-01 00:00:00" & Date <= "2009-12-31 00:00:00")
winter1 <- matrix(winter1$Global_active_power[292:nrow(winter1)], nrow=floor(nrow(winter1)/300), ncol=300, byrow=TRUE)

winter2 <- morning %>% filter(Date >= "2009-11-01 00:00:00" & Date <= "2009-12-31 00:00:00")
winter2 <- matrix(winter2$Global_active_power[1:18000], nrow=floor(nrow(winter2)/600), ncol=600, byrow=TRUE)

winter3 <- night %>% filter(Date >= "2009-11-01 00:00:00" & Date <= "2009-12-31 00:00:00")
winter3 <- matrix(winter3$Global_active_power[70:15729], nrow=floor(nrow(winter3)/540), ncol=540, byrow=TRUE)

logliks <- function(r, n, thresh){
  loglikVals <- vector(mode="double", length=(n-windowSize-1))
  j = 1
  for (i in seq(1,(n-windowSize-1))){
    start = i
    end = i+windowSize-1
    pred <- predict(hmm, r[start:end], method="viterbi")
    thresholds <- abs(hmm$model$parms.emission$mu[pred$s]-pred$x)
    if(length(which(thresholds > thresh)) <= (0.4*windowSize)){
      loglikVals[j] = pred$loglik
      j = j + 1;
    }
  }
  return(as.data.frame(cbind(min(loglikVals[which(loglikVals<0)]), max(loglikVals[which(loglikVals<0)]))))
}

getlogliks <- function(x, loglikValues){
  timestamp <- (as.numeric(x) - as.numeric(as.POSIXct("2007-01-01 00:00:00"))) %% 86400
  Month = month(x)
  if (timestamp >= 3600 & timestamp < 21600){
    if(Month >=6 & Month <= 8){
      return(loglikValues[1,])
    }
    else if (Month >=11 | Month <= 2){
      return(loglikValues[7,])
    }
    else{
      return(loglikValues[4,])
    }
  }
  else if (timestamp >= 21600 & timestamp < 57600 ){
    if(Month >=6 & Month <= 8){
      return(loglikValues[2,])
    }
    else if (Month >=11 | Month <= 2){
      return(loglikValues[8,])
    }
    else{
      return(loglikValues[5,])
    }
  }
  else{
    if(Month >=6 & Month <= 8){
      return(loglikValues[3,])
    }
    else if (Month >=11 | Month <= 2){
      return(loglikValues[9,])
    }
    else{
      return(loglikValues[6,])
    }
  }
}

for (k in seq(1,6)){
  # Calculate loglik intervals 
  a1 <- logliks(summer1[1,],ncol(summer1), threshold[k])
  a2 <- logliks(summer2[1,],ncol(summer2), threshold[k])
  a3 <- logliks(summer3[1,],ncol(summer3), threshold[k])
  b1 <- logliks(fall1[1,],ncol(fall1), threshold[k])
  b2 <- logliks(fall2[1,],ncol(fall2), threshold[k])
  b3 <- logliks(fall3[1,],ncol(fall3), threshold[k])
  c1 <- logliks(winter1[1,],ncol(winter1), threshold[k])
  c2 <- logliks(winter2[1,],ncol(winter2), threshold[k])
  c3 <- logliks(winter3[1,],ncol(winter3), threshold[k])
  loglikValuess <- rbind(a1,a2,a3,b1,b2,b3,c1,c2,c3)

  Length = nrow(test1)
  anomaly1 <- vector(mode="integer", length=Length)
  anomaly2 <- vector(mode="integer", length=Length)
  # Anomaly Detection for each test data set
  testset1 <- rbind(train[(length(train$Date)-windowSize+1):(length(train$Date)-1),], test1)
  testset2 <- rbind(train[(length(train$Date)-windowSize+1):(length(train$Date)-1),], test2)
  
  for (i in seq(1, Length)){
    start <- i
    end <- i+windowSize-1
    X1 <- predict(hmm, testset1$Global_active_power[start:end], method="viterbi")
    minmax <- getlogliks(testset1[start,1], loglikValuess)
    if (X1$loglik >= minmax[1] & X1$loglik <= minmax[2]){
      anomaly1[i] <- 0
    } else{
      anomaly1[i] <- 1
      # substitute the current data point with the normal expectation 
      testset1$Global_active_power[end] <- hmm$model$parms.emission$mu[X1$s[windowSize]]
    }
    X2 <- predict(hmm, testset2$Global_active_power[start:end], method="viterbi")
    minmax <- getlogliks(testset2[start,1], loglikValuess)
    if (X2$loglik >= minmax[1] & X2$loglik <= minmax[2]){
      anomaly2[i] <- 0
    } else{
      anomaly2[i] <- 1
      # substitute the current data point with the normal expectation 
      testset2$Global_active_power[end] <- hmm$model$parms.emission$mu[X2$s[windowSize]]
    }
  }
  print(length(which(anomaly1==1))/Length)
  print(length(which(anomaly2==1))/Length)
  outfile <- paste("point_",threshold[k],".txt",sep="")
  write.table(c(anomaly1, anomaly2), file=outfile, sep=",",row.names=FALSE, col.names=FALSE)  
}