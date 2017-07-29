require(lubridate)
library(reshape)

train <- trainFull[, c(1,3)]
train$Date <- paste(trainFull$Date, trainFull$Time)
train$Date <- as.POSIXct(train$Date, format='%d/%m/%Y %H:%M:%S')
train <- na.omit(train)
train$Date <- strftime(train$Date, format="%H:%M:%S") 

test1 <- test1Full[, c(1,3)]
test1$Date <- paste(test1Full$Date, test1Full$Time)
test1$Date <- as.POSIXct(test1$Date, format='%d/%m/%Y %H:%M:%S')
test1 <- na.omit(test1)
test1$Date <- strftime(test1$Date, format="%H:%M:%S") 

test2 <- test2Full[, c(1,3)]
test2$Date <- paste(test2Full$Date, test2Full$Time)
test2$Date <- as.POSIXct(test2$Date, format='%d/%m/%Y %H:%M:%S')
test2 <- na.omit(test2)
test2$Date <- strftime(test2$Date, format="%H:%M:%S") 

window <- c(5,10,15,20)
threshold <- 1 

prediction <- function(x){
  pred <- predict(hmm_12, x, method="viterbi")
  if (length(which(abs(hmm_12$model$parms.emission$mu[pred$s] - pred$x) > 1)) > floor(0.5*windowSize)){
    return(NA)
  }
  else{
    return(pred$loglik)
  }
}

for (k in seq(1,4)){
  windowSize <- window[k]
  numInterval = 1440/windowSize
  
  minValues <- vector(mode="double", length=numInterval)
  maxValues <- vector(mode="double", length=numInterval)
  
  for (i in seq(1,numInterval)){
    start <- (i-1)*windowSize + 1
    end = start + windowSize -1
    T <- test1$Date[start:end]
    if (any(T == "00:00:00")){
      RWs <- train %>% filter(Date <= T[windowSize] | Date >= T[1])
    } else{
      RWs <- train %>% filter(Date <= T[windowSize] & Date >= T[1])
    }
    if (T[1] != RWs$Date[1]){
      RWs <- RWs[which(RWs$Date == T[1])[1]:length(RWs$Date),]
    }
    nrows = floor(length(RWs$Date)/windowSize)
    d <- matrix(unlist(t(RWs$Global_active_power[1:nrows*windowSize])), byrow=TRUE, nrows, windowSize)
    D <- c(apply(d,1,FUN=prediction))
    D <- na.omit(D)
    minValues[i] = min(D)
    maxValues[i] = max(D)
  }
  
  Length <- floor(nrow(test1)/windowSize)
  
  anomaly1 <- vector(mode="integer", length=Length)
  anomaly2 <- vector(mode="integer", length=Length)
  for (i in seq(1, Length)){
    start <- (i-1)*windowSize + 1
    end = start + windowSize -1
    X1 <- test1$Global_active_power[start:end]
    X1 <- predict(hmm_12, X1, method="viterbi")
    j = (i-1)%%numInterval +1
    if (X1$loglik >= minValues[j] & X1$loglik <= maxValues[j]){
      anomaly1[i] = 0
    }
    else{
      anomaly1[i] = 1
    }
    X2 <- test2$Global_active_power[start:end]
    X2 <- predict(hmm_12, X2, method="viterbi")
    if (X2$loglik >= minValues[j] & X2$loglik <= maxValues[j]){
      anomaly2[i] = 0
    }
    else{
      anomaly2[i] = 1
    }
  }
  print(length(which(anomaly1==1))/Length)
  print(length(which(anomaly2==1))/Length)
  
  outfile <- paste("coolective_",windowSize,".txt",sep="")
  write.table(c(anomaly1, anomaly2), file=outfile, sep=",",row.names=FALSE, col.names=FALSE)  
}