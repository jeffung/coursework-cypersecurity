sampleSize <- 1000
subsequenceSize <- 100
randIndex <- sample(1400616:length(train$Global_active_power), sampleSize)
lLikelihood <- as.data.frame(cbind(c(1:1000), c(1:1000), c(1:1000), c(1:1000))) 
interval <- as.data.frame(cbind(c(1:2), c(1:2), c(1:2), c(1:2)))
LLmean <- c(1:4)
for (i in 1:1000) {
  if (randIndex[i] < (length(train$Global_active_power) - sampleSize + 1)) {
    start <- randIndex[i]
    end <- start + sampleSize
  }
  else {
    end <- randIndex[i]
    start <- end - sampleSize
  }
  subSeq1 <- data.frame(train$Global_active_power[start:(start + 450)])
  colnames(subSeq1) <- c("global_active_power")
  subSeq2 <- data.frame(train$Global_active_power[(start + 550):end])
  colnames(subSeq2) <- c("global_active_power")
  tmp <- formatMhsmm(data.frame(rbind(subSeq1, subSeq2)))
  yhat1 <- predict(hmm_8, tmp)
  lLikelihood[i, 1] <- yhat1$loglik
  yhat2 <- predict(hmm_10, tmp)
  lLikelihood[i, 2] <- yhat2$loglik
  yhat3 <- predict(hmm_12, tmp)
  lLikelihood[i, 3] <- yhat3$loglik
  yhat4 <- predict(hmm_14, tmp)
  lLikelihood[i, 4] <- yhat4$loglik
}

for (i in 1:4) {
  interval[1,i] <- max(lLikelihood[, i])
  interval[2,i] <- min(lLikelihood[, i])
  LLmean[i] <- mean(lLikelihood[, i])
}
interval
LLmean