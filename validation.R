library(mhsmm)

#separate the last 10% of train set as validationSet
validationset <- trainFull$Global_active_power[(nrow(trainFull)*0.9):nrow(trainFull)]

#add noises to data
corrupt <- rbinom(length(validationset),1,0.006)
corrupt <- as.logical(corrupt)

maxflownoise <- rnorm(sum(corrupt),12,1)
minflownoise <- rnorm(sum(corrupt),0,0.1)

validationset[corrupt] <- validationset[corrupt] + minflownoise + maxflownoise

print(validationset)
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

trainfullform <- formatMhsmm(data.frame(trainFull$Global_active_power[1:(0.9*nrow(trainFull))]))

hmm <- hmmfit(trainfullform$x, startmodel , mstep = mstep.norm,maxit = 200, tol=1e-02)

summary(hmm)

result <- predict (hmm_8,validationset)

difference <- 0
threshold <- 2
totalAnomalies <- length(which(corrupt == TRUE))
detectedAnomalies <- 0
actualAnomalies <- 0

for (i in 1:length(validationset)) {
  state <- result$s[i]
  expected <- hmm_8$model$parms.emission$mu[state]
  alpha <- hmm_8$model$parms.emission$sigma[state]
  difference <- abs(expected - validationset[i]) 
  if (difference >= threshold) {
    detectedAnomalies <- detectedAnomalies + 1
    if (corrupt[i]) {
      actualAnomalies <- actualAnomalies + 1
    }
  }
}
precision <- actualAnomalies / detectedAnomalies
recall <- actualAnomalies / totalAnomalies
fScore <- (2 * precision * recall) / (precision + recall)
precision
recall
fScore