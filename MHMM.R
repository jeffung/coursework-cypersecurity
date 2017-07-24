library(mhsmm)
library(dplyr)
library(lubridate)

train <- trainFull[, c(2,4)]
train$DateTime <- paste(trainFull$Date, trainFull$Time)
train$DateTime <- as.POSIXct(train$DateTime, format='%d/%m/%Y %H:%M:%S')
train <- na.omit(train)
test1Sub <- test1Full[, c(2,4)]
test1Sub$DateTime <- paste(test1Full$Date, test1Full$Time)
test1Sub$DateTime <- as.POSIXct(test1Sub$DateTime, format='%d/%m/%Y %H:%M:%S')
test1Sub <- na.omit(test1Sub)
test2Sub <- test2Full[, c(2,4)]
test2Sub$DateTime <- paste(test2Full$Date, test2Full$Time)
test2Sub$DateTime <- as.POSIXct(test2Sub$DateTime, format='%d/%m/%Y %H:%M:%S')
test2Sub <- na.omit(test2Sub)

traindayform <- formatMhsmm(data.frame(train$Global_active_power))
test1form <- formatMhsmm(data.frame(test1Sub$Global_active_power))
test2form <- formatMhsmm(data.frame(test1Sub$Global_active_power))

# number of states HMM    
k=12

#init probabilities
init <- rep(1/k, k)

#transition matrix
P <- matrix(rep(1/k, k*k), nrow = k)

#emission matrix:  here I used a Gaussian distribution, replace muEst and sigmaEst by your initial estimates of mean and variance
muVec <- c(1:k)
sigmaVec <- c(1:k)

muVec <- c(0.5447714, 0.2779779, 1.8263834, 1.5501700, 1.9615561, 0.8685399, 2.7655574, 0.5594927, 1.2849762, 0.1710025, 4.1631672, 0.4510502)
sigmaVec <- c(0.000668871, 0.002888363, 0.486437022, 0.026234604, 0.061892010, 0.129187716, 0.235773146, 0.026516903, 0.020386461, 0.003444033,
              1.131033014, 0.002118213)

b <- list(mu = muVec, sigma = sigmaVec) 

#starting model for EM
startmodel <- hmmspec(init = init, trans = P, parms.emis = b, dens.emis = dnorm.hsmm)
startmodel

#EM algorithm fits an HMM to the data
hmm <- hmmfit(traindayform$x, startmodel , mstep = mstep.norm,maxit = 200, tol=1e-02)

#print resulting HMM parameters
summary(hmm)
plot(hmm$loglik, type="b", ylab="log-likelihood", xlab="Iteration")

yhat1 <- predict (hmm,traindayform$x)
yhat2 <- predict (hmm,test1form$x)

#plot(yhat1)
#plot(yhat2)
hmm$loglik