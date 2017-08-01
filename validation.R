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

hmm <- hmm_8
#hmm <- hmm_10
#hmm <- hmm_12
#hmm <- hmm_14

result <- predict (hmm,validationset)

difference <- 0
threshold <- 2
totalAnomalies <- length(which(corrupt == TRUE))
detectedAnomalies <- 0
actualAnomalies <- 0

for (i in 1:length(validationset)) {
  state <- result$s[i]
  expected <- hmm$model$parms.emission$mu[state]
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