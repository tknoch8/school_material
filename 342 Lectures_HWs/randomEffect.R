# looms selected at random!
# underlying question:  how much of the variability in strength is due to the looms?
looms <- rep(1:4, 1, each = 4)
strength <- c(98,97,99,96,91,90,93,92,96,95,97,95,95,96,99,98)

textileDat <- as.data.frame(cbind(looms, strength))
textileDat[,1] <- as.factor(textileDat[,1])
plot(textileDat)

aovOut <- aov(strength ~ looms, data = textileDat)
summary(aovOut)  #what do we conclude?
#lsmeans?

#method of moments estimation
meanSqVec <- anova(aovOut)[["Mean Sq"]]
sigSqEst <- meanSqVec[2]
sigTauSqEst <- (meanSqVec[1] - meanSqVec[2])/4
sigSqEst
# [1] 1.895833
sigTauSqEst
# [1] 6.958333
totalVariability <- sigSqEst + sigTauSqEst
sigTauSqEst/totalVariability
#.786
#CI for intraclass correlation coefficient (percent of total variability attributed to treatment)
L = 1/4 * (meanSqVec[1]/meanSqVec[2]*1/qf(.975, 3, 12) - 1)
L
U = 1/4 * (meanSqVec[1]/meanSqVec[2]*1/qf(.025, 3, 12) - 1)
U
CI = c(L/(1+L), U/(1+U))
CI
muHat <- mean(strength)
muHat
#[1] 95.4375
CImean = muHat + c(-1,1)*qt(.975, 12)*sqrt(meanSqVec[1]/16)
CImean
