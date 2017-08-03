#data <- na.omit(test1Full$Global_active_power)
data <- na.omit(test2Full$Global_active_power)

hmm <- hmm_12

result <- predict (hmm, data)

# difference <- 0
# threshold <- c(0.5, 1, 1.25 ,1.5, 1.75, 2)
# output <- matrix(0, ncol = 2, nrow = length(data))
# count <- 0
# 
# for (j in 1:length(threshold)) {
#   count <- 0
#   for (i in 1:length(data)) {
#     state <- result$s[i]
#     expected <- hmm$model$parms.emission$mu[state]
#     difference <- abs(expected - data[i])
#     if (difference >= threshold[j]) {
#       output[i, 1] <- 1
#       output[i, 2] <- 1
#       count <- count + 1
#     }
#   }
#   
#   outfile <- paste("point_",threshold[j],".txt",sep="")
#   write.table(output, file=outfile, sep=",",row.names=FALSE, col.names=FALSE)  
# }

difference <- 0
threshold <- 0.75
output <- matrix(0, ncol = 2, nrow = length(data))
count <- 0

for (i in 1:length(data)) {
  state <- result$s[i]
  expected <- hmm$model$parms.emission$mu[state]
  difference <- abs(expected - data[i])
  if (difference >= threshold) {
    output[i, 1] <- 1
    output[i, 2] <- 1
    count <- count + 1
  }
}

outfile <- paste("point_",threshold,".txt",sep="")
write.table(output, file=outfile, sep=",",row.names=FALSE, col.names=FALSE)  

