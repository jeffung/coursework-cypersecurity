library(mhsmm)
library(dplyr)
library(lubridate)

train <- trainFull[, c(1,3)]
colnames(train) <- c("DateTime", "Global_active_power")
train$DateTime <- paste(trainFull$Date, trainFull$Time)
train$DateTime <- as.POSIXct(train$DateTime, format='%d/%m/%Y %H:%M:%S')
train <- na.omit(train)

traindayform <- formatMhsmm(data.frame(train$Global_active_power))

# number of states HMM    
k=8

#init probabilities
init <- rep(1/k, k)

#transition matrix
P <- matrix(rep(1/k, k*k), nrow = k)

#emission matrix:  here I used a Gaussian distribution, replace muEst and sigmaEst by your initial estimates of mean and variance
muVec <- c(1:k)
sigmaVec <- c(1:k)

muVec <- c(0.3301653, 0.2730533, 4.5924386, 0.8581603, 2.1744369, 3.1150939, 0.5493970, 1.7928395)
sigmaVec <- c(0.0201126908, 0.0029628074, 1.1349055417, 0.1301456018, 0.2770119212, 0.3368303610, 0.0008511606, 0.0424565463)

b <- list(mu = muVec, sigma = sigmaVec) 

#starting model for EM
startmodel <- hmmspec(init = init, trans = P, parms.emis = b, dens.emis = dnorm.hsmm)
startmodel

#EM algorithm fits an HMM to the data
hmm_8 <- hmmfit(traindayform$x, startmodel, mstep = mstep.norm,maxit = 200, tol=1e-02)

#print resulting HMM parameters
summary(hmm_8)
plot(hmm_8$loglik, type="b", ylab="log-likelihood", xlab="Iteration")

#yhat1 <- predict (hmm_8,traindayform$x)
#yhat2 <- predict (hmm_8,test1form$x)

#plot(yhat1)
#plot(yhat2)
hmm_8$loglik[length(hmm_8$loglik)]

step <- 100
randIndex <- sample(1400616:length(train$Global_active_power), 1000)
lLikelihood <- c(1:1000)
interval <- c(1:2)
for (i in 1:1000) {
  if (randIndex[i] < (length(train$Global_active_power) - step + 1)) {
    start <- randIndex[i]
    end <- start + step
  }
  else {
    end <- randIndex[i]
    start <- end - step
  }
  tmp <- formatMhsmm(data.frame(train$Global_active_power[start:end]))
  yhat <- predict(hmm_8, tmp)
  lLikelihood[i] <- yhat$loglik
}
interval[1] <- min(lLikelihood)
interval[2] <- max(lLikelihood)
LLmean <- mean(lLikelihood)
interval
LLmean