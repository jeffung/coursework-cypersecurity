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
k=10

#init probabilities
init <- rep(1/k, k)

#transition matrix
P <- matrix(rep(1/k, k*k), nrow = k)

#emission matrix:  here I used a Gaussian distribution, replace muEst and sigmaEst by your initial estimates of mean and variance
muVec <- c(1:k)
sigmaVec <- c(1:k)

muVec <- c(0.3301653, 0.2730533, 4.5924386, 0.8581603, 2.1744369, 0.5679845, 3.1150939, 0.5493970, 1.4742556, 1.7928395)
sigmaVec <- c(0.0201126908, 0.0029628074, 1.1349055417, 0.1301456018, 0.2770119212, 0.0291640563, 0.3368303610, 0.0008511606, 0.0262214028,
              0.0424565463)

b <- list(mu = muVec, sigma = sigmaVec) 

#starting model for EM
startmodel <- hmmspec(init = init, trans = P, parms.emis = b, dens.emis = dnorm.hsmm)
startmodel

#EM algorithm fits an HMM to the data
hmm_10 <- hmmfit(traindayform$x, startmodel, mstep = mstep.norm,maxit = 200, tol=1e-02)

#print resulting HMM parameters
summary(hmm_10)
plot(hmm_10$loglik, type="b", ylab="log-likelihood", xlab="Iteration")

#yhat1 <- predict (hmm_10,traindayform$x)
#yhat2 <- predict (hmm_10,test1form$x)

#plot(yhat1)
#plot(yhat2)
hmm_10$loglik[length(hmm_10$loglik)]