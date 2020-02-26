hardwoodDat <- read.csv("Teaching/STAT341Regression/Data/linear_regression_5e_data_sets/Chapter 7/Examples/data-ex-7-1 (Hardwood).csv")
#just cleaing up headers
colnames(hardwoodDat) <- c("hardwoodPct", "tensileStr")
hardwoodLM1 <- lm(tensileStr ~ hardwoodPct, data = hardwoodDat)
summary(hardwoodLM1)
library(MASS)
rStudRes <- studres(hardwoodLM1)
plot(hardwoodLM1$fitted.values, rStudRes)
#obvious!
plot(hardwoodDat[, c("hardwoodPct", "tensileStr")])
abline(hardwoodLM1$coefficients, col = 2)

#building the design matrix
n <- dim(hardwoodDat)[1]
#first order
X <- cbind(rep(1, n), hardwoodDat[,1])
X
y <- hardwoodDat[,2]
betaHat <- solve(t(X) %*% X) %*% t(X) %*% y
betaHat
#second order polynomial
X <- cbind( rep(1, n), hardwoodDat[,1], hardwoodDat[,1]^2)
X
betaHat <- solve(t(X) %*% X) %*% t(X) %*% y
betaHat
#plot
plot(hardwoodDat[, c("hardwoodPct", "tensileStr")])
x <- seq(1, 15, .1)
fitCurve <- betaHat[1] + betaHat[2]*x + betaHat[3]*x^2
lines(x, fitCurve, col = 3)

#fitting in R
hardwoodLM2 <- lm(tensileStr ~ hardwoodPct + I(hardwoodPct^2), data = hardwoodDat)
summary(hardwoodLM2)
rStudRes2 <- studres(hardwoodLM2)
plot(hardwoodLM2$fitted.values, rStudRes2)
qqnorm(rStudRes2)  
abline(0,1)
summary(hardwoodLM1)  #notice change in adj R-squared.
#our fitted model differs from book--they center the predictor variables x so X is better conditioned.




############
#chemical process, section 7.4
x1 <- c(-1, 1, -1, 1, -1.414, 1.414, 0,0,0,0,0,0)  #temperature
x2 <- c(-1, -1, 1, 1, 0,0,-1.414, 1.414, 0, 0, 0, 0)  #concentration
y <- c(43, 78, 69, 73, 48, 76, 65, 74, 76, 79, 83, 81)  #yield
origDat <- data.frame(x1, x2, y)
lmTemp <- lm(y ~ x1 + x2, data = origDat)
rStudRes <- studres(lmTemp)
plot(lmTemp$fitted.values, rStudRes)
#now the story and the explanation of coded data
lmOut <- lm(y ~ x1 + x2 + I(x1^2) + I(x2^2) + I(x1*x2), data = origDat)
summary(lmOut)
rStudRes <- studres(lmOut)
plot(lmOut$fitted.values, rStudRes)
qqnorm(rStudRes)


############
#kernel smoothing
hardwoodDat <- read.csv("Teaching/STAT341Regression/Data/linear_regression_5e_data_sets/Chapter 7/Examples/data-ex-7-1 (Hardwood).csv")
#just cleaing up headers
colnames(hardwoodDat) <- c("hardwoodPct", "tensileStr")

norm2 <- ksmooth(x = hardwoodDat[, "hardwoodPct"], y = hardwoodDat[, "tensileStr"], bandwidth = 2, kernel = "normal")
plot(hardwoodDat)
lines(norm2, col = 2)

norm1 <- ksmooth(x = hardwoodDat[, "hardwoodPct"], y = hardwoodDat[, "tensileStr"], bandwidth = 1, kernel = "normal")
lines(norm1, col = 3)

norm.5 <- ksmooth(x = hardwoodDat[, "hardwoodPct"], y = hardwoodDat[, "tensileStr"], bandwidth = .5, kernel = "normal")
lines(norm.5, col = 4)

box2 <- ksmooth(x = hardwoodDat[, "hardwoodPct"], y = hardwoodDat[, "tensileStr"], bandwidth = 2, kernel = "box")
plot(hardwoodDat)
lines(box2, col = 2)


