###
#train data
traintest1 <- list (x= data.frame(na.omit(trainFull$Global_active_power * 1000)), N=length(trainFull$Date))
traintest1$x <- data.matrix(traintest1$x, rownames.force = NA)
traintest1$x <- scale(traintest1$x)
class(traintest1) <- "hsmm.data"
# end
uni_test<- data.frame(as.numeric(na.omit(test1Full$Global_active_power * 1000)))
uni_test <- setNames(uni_test,c("power"))
uni_test$power[is.na(uni_test$power)] <- mean(na.omit(uni_test$power))
xtest1 <- list (x= data.frame(uni_test$power), N=length(uni_test$power))
xtest1$x <- data.matrix(xtest1$x, rownames.force = NA)
xtest1$x <- scale(xtest1$x)
class(xtest1) <- "hsmm.data"
# create M-step

library(mhsmm)
J <- 12
init0 <- rep(1/J, J)

muVec <- c(1:J)
sigmaVec <- c(1:J)
muVec <- c(0.5447714, 0.2779779, 1.8263834, 1.5501700, 1.9615561, 0.8685399, 2.7655574, 0.5594927, 1.2849762, 0.1710025, 4.1631672, 0.4510502)
sigmaVec <- c(0.000668871, 0.002888363, 0.486437022, 0.026234604, 0.061892010, 0.129187716, 0.235773146, 0.026516903, 0.020386461, 0.003444033,
              1.131033014, 0.002118213)
B0 <- list(mu = muVec, sigma = sigmaVec)

P <- matrix(sample(0:1, J * J, replace = TRUE), J, J)
diag(P) <- 0

M <- max(na.omit(trainFull$Global_active_power) * 1000)
d1 <- dunif(1:M,(muVec[1] - sqrt(sigmaVec[1])) * 1000, (muVec[1] + sqrt(sigmaVec[1])) * 1000)
d2 <- dunif(1:M,(muVec[2] - sqrt(sigmaVec[2])) * 1000, (muVec[2] + sqrt(sigmaVec[2])) * 1000)
d3 <- dunif(1:M,(muVec[3] - sqrt(sigmaVec[3])) * 1000, (muVec[3] + sqrt(sigmaVec[3])) * 1000)
d4 <- dunif(1:M,(muVec[4] - sqrt(sigmaVec[4])) * 1000, (muVec[4] + sqrt(sigmaVec[4])) * 1000)
d5 <- dunif(1:M,(muVec[5] - sqrt(sigmaVec[5])) * 1000, (muVec[5] + sqrt(sigmaVec[5])) * 1000)
d6 <- dunif(1:M,(muVec[6] - sqrt(sigmaVec[6])) * 1000, (muVec[6] + sqrt(sigmaVec[6])) * 1000)
d7 <- dunif(1:M,(muVec[7] - sqrt(sigmaVec[7])) * 1000, (muVec[7] + sqrt(sigmaVec[7])) * 1000)
d8 <- dunif(1:M,(muVec[8] - sqrt(sigmaVec[8])) * 1000, (muVec[8] + sqrt(sigmaVec[8])) * 1000)
d9 <- dunif(1:M,(muVec[9] - sqrt(sigmaVec[9])) * 1000, (muVec[9] + sqrt(sigmaVec[9])) * 1000)
d10 <- dunif(1:M,(muVec[10] - sqrt(sigmaVec[10])) * 1000, (muVec[10] + sqrt(sigmaVec[10])) * 1000)
d11 <- dunif(1:M,(muVec[11] - sqrt(sigmaVec[11])) * 1000, (muVec[11] + sqrt(sigmaVec[11])) * 1000)
d12 <- dunif(1:M,(muVec[12] - sqrt(sigmaVec[12])) * 1000, (muVec[12] + sqrt(sigmaVec[12])) * 1000)
d0 <- cbind(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12)

startval <- hsmmspec(init0, P, parms.emis = B0, list(d = d0, type = "nonparametric"), dens.emis = dnorm.hsmm)


nnn <- traintest1$x[,1]
UniHSMM<- hsmmfit(nnn, startval, mstep = mstep.norm, M=M)
summary(UniHSMM)
UniHSMMtrain <- predict.hsmm(UniHSMM,traintest1$x)
UniHSMMtest <- predict.hsmm(UniHSMM,xtest1$x)
plot(UniHSMMtrain)
#plot(UniHSMMtest)
UniHSMMtrain$s
UniHSMMtest$s