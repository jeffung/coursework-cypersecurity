library(mhsmm)
library(dplyr)
library(lubridate)

train <- trainFull[, c(2,4)]
train$DateTime <- paste(trainFull$Date, trainFull$Time)
train$DateTime <- as.POSIXct(train$DateTime, format='%d/%m/%Y %H:%M:%S')
train <- na.omit(train)

traindayform <- formatMhsmm(data.frame(train$Global_active_power))
testdayform <- formatMhsmm(data.frame(test1Sub$Global_active_power))

# number of states HMM    
k=11

#init probabilities
init <- rep(1/k, k)

#transition matrix
P <- matrix(rep(1/k, k*k), nrow = k)

#emission matrix:  here I used a Gaussian distribution, replace muEst and sigmaEst by your initial estimates of mean and variance
muVec <- c(1:k)
sigmaVec <- c(1:k)

s <- with( train , train[ Global_active_power <= 0.25, ] )
muVec[1] <- mean(s$Global_active_power, na.rm = TRUE)
sigmaVec[1] <- var(s$Global_active_power, na.rm = TRUE)

s <- with( train , train[ Global_active_power > 0.25 & Global_active_power <= 0.3451, ] )
muVec[2] <- mean(s$Global_active_power, na.rm = TRUE)
sigmaVec[2] <- var(s$Global_active_power, na.rm = TRUE)

s <- with( train , train[ Global_active_power > 0.3451 & Global_active_power <= 0.5329, ] )
muVec[3] <- mean(s$Global_active_power, na.rm = TRUE)
sigmaVec[3] <- var(s$Global_active_power, na.rm = TRUE)

s <- with( train , train[ Global_active_power > 0.5329 & Global_active_power <= 0.7738, ] )
muVec[4] <- mean(s$Global_active_power, na.rm = TRUE)
sigmaVec[4] <- var(s$Global_active_power, na.rm = TRUE)

s <- with( train , train[ Global_active_power > 0.7738 & Global_active_power <= 1.2851, ] )
muVec[6] <- mean(s$Global_active_power, na.rm = TRUE)
sigmaVec[6] <- var(s$Global_active_power, na.rm = TRUE)

s <- with( train , train[ Global_active_power > 1.2851 & Global_active_power <= 1.684, ] )
muVec[7] <- mean(s$Global_active_power, na.rm = TRUE)
sigmaVec[7] <- var(s$Global_active_power, na.rm = TRUE)

s <- with( train , train[ Global_active_power > 1.684 & Global_active_power <= 2.314, ] )
muVec[8] <- mean(s$Global_active_power, na.rm = TRUE)
sigmaVec[8] <- var(s$Global_active_power, na.rm = TRUE)

s <- with( train , train[ Global_active_power > 2.314 & Global_active_power <= 3.338, ] )
muVec[9] <- mean(s$Global_active_power, na.rm = TRUE)
sigmaVec[9] <- var(s$Global_active_power, na.rm = TRUE)

s <- with( train , train[ Global_active_power > 3.338, ] )
muVec[10] <- mean(s$Global_active_power, na.rm = TRUE)
sigmaVec[10] <- var(s$Global_active_power, na.rm = TRUE)

b <- list(mu = muVec, sigma = sigmaVec) 

#starting model for EM
startmodel <- hmmspec(init = init, trans = P, parms.emis = b, dens.emis = dnorm.hsmm)
startmodel

#EM algorithm fits an HMM to the data
hmm <- hmmfit(traindayform$x, startmodel , mstep = mstep.norm,maxit = 200)

#print resulting HMM parameters
summary(hmm)
plot(hmm$loglik, type="b", ylab="log-likelihood", xlab="Iteration")

yhat1 <- predict (hmm,traindayform$x)
yhat2 <- predict (hmm,testdayform$x)

#plot(yhat1)
#plot(yhat2)
hmm$loglik