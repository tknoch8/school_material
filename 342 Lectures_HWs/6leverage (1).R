load("Teaching/STAT341Regression/Data/linear_regression_5e_data_sets/Chapter 3/Examples/deliveryDataAndFit.RData")
ls()
n <- dim(deliveryDat)[1]

#plot in x-space
plot(deliveryDat[, c(2,3)])
text(deliveryDat[, 2], deliveryDat[,3] + 20, cex = .7, seq(1,n))

X <- cbind(rep(1,n), deliveryDat[, c(2,3)])
lmOut <- lm(time ~ numCases + distance, dat = deliveryDat)
summary(lmOut)
X <- model.matrix(lmOut)  #another way to get X
covBetaHat <- 3.258^2 * solve(t(X) %*% X)
cov(X[,c(2,3)])
corX <- cor(X[,c(2,3)])
corX
solve(corX)


#leverage and influence
influenceOut <- influence(deliveryOut)
h <- influenceOut$hat
rStudResids <- studres(deliveryOut)
cbind(deliveryDat, h, rStudResids)
#point 9 has large h and large resid -> influence
#point 22 has large h, but not large resid -> leverage
#see table in example 6.1

#cook's D
d <- cooks.distance(deliveryOut)
d
cbind(deliveryDat, h, rStudResids, round(d,4))
qf(.95, 3, 22)

#  Q:  So what do you do?