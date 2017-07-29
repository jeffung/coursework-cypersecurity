library(mhsmm)
library(dplyr)
library(lubridate)

train <- trainFull[, c(1,3)]
colnames(train) <- c("DateTime", "Global_active_power")
train$DateTime <- paste(trainFull$Date, trainFull$Time)
train$DateTime <- as.POSIXct(train$DateTime, format='%d/%m/%Y %H:%M:%S')
train <- na.omit(train)

test <- test1Full[, c(1,3)]
colnames(test) <- c("DateTime", "Global_active_power")
test$DateTime <- paste(test1Full$Date, test1Full$Time)
test$DateTime <- as.POSIXct(test$DateTime, format='%d/%m/%Y %H:%M:%S')
test <- na.omit(test)

traindayform <- formatMhsmm(data.frame(train$Global_active_power))
noramlform <- formatMhsmm(data.frame(train$Global_active_power[1:1000]))
testform <- formatMhsmm(data.frame(test$Global_active_power))

# number of states HMM    
k=12

#init probabilities
init <- rep(1/k, k)

#transition matrix
P <- matrix(rep(1/k, k*k), nrow = k)

#emission matrix:  here I used a Gaussian distribution, replace muEst and sigmaEst by your initial estimates of mean and variance
muVec <- c(1:k)
sigmaVec <- c(1:k)

muVec <- c(0.5484832, 0.3271606, 0.2719592, 3.1713822, 1.2757978, 1.8831006, 0.8581603, 2.4676883, 0.5519891, 1.8237814, 1.5377176, 4.7048570)
sigmaVec <- c(0.0008257707, 0.0202114701, 0.0029434861, 0.3858464933, 0.0210970072, 0.0460559523, 0.1301456018, 0.0741345796, 0.0252060821,
              0.4619841991, 0.0251711090, 1.0975356331)

b <- list(mu = muVec, sigma = sigmaVec) 

#starting model for EM
startmodel <- hmmspec(init = init, trans = P, parms.emis = b, dens.emis = dnorm.hsmm)
startmodel

#EM algorithm fits an HMM to the data
hmm_12 <- hmmfit(traindayform$x, startmodel, mstep = mstep.norm,maxit = 200, tol=1e-02)

#print resulting HMM parameters
summary(hmm_12)
plot(hmm_12$loglik, type="b", ylab="log-likelihood", xlab="Iteration")

yhat1 <- predict (hmm_12,testform$x)
yhat1$loglik
yhat2 <- predict (hmm_12,noramlform$x)
yhat2$loglik

#plot(yhat1)
#plot(yhat2)
hmm_12$loglik[length(hmm_12$loglik)]