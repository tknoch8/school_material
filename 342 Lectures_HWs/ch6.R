#example 6.2
#A is reactant concentraion, B is catalyst
A <- as.factor(rep(c(-1, 1, -1, 1), 3))
B <- as.factor(rep(c(-1, -1, 1, 1), 3))
response <- c(28,36,18,31,25,32,19,30,27,32,23,29)
chemDat <- data.frame(A, B, response)

chemAOV <- aov(response ~ A*B, data = chemDat)
summary(chemAOV)
library(lsmeans)
grandMean <- mean(response)
grandMean
factorA <- lsmeans(chemAOV, ~A)
factorB <- lsmeans(chemAOV, ~B)
interactionAB <- lsmeans(chemAOV, ~A|B)
summary(factorA)
summary(factorB)
summary(interactionAB)

A <- as.numeric(as.character(A))
B <- as.numeric(as.character(B))
AB <- A*B
X <- cbind( rep(1, 12), A, B, AB)
betaHat <- solve(t(X) %*% X)%*% t(X) %*% response
betaHat

chemLM <- lm(chemDat$response~X-1)  #-1 gets rid of intercept b/c already in X
chemLM
summary(chemLM)

Xalt <- X
Xalt[Xalt == -1] <- 0
betaHat <- solve(t(Xalt) %*% Xalt)%*% t(Xalt) %*% response
betaHat



######################################
#Example 6.3
A <- c(-1, 1, -1, 1, -1, 1, -1, 1)
B <- c(-1, -1, 1, 1, -1, -1, 1, 1)
C <- c(-1, -1, -1, -1, 1, 1, 1, 1)
AB <- A*B
AC <- A*C
BC <- B*C
ABC <- A*B*C
response <- c(550,604,669,650,633,601,642,635,1037,1052,749,868,1075,1063,729,860)
#data are listed in order (1), (1), a, a, b, ....

etchDat <- data.frame(
  as.factor(rep(A, 1, each = 2)), 
  as.factor(rep(B, 1, each = 2)), 
  as.factor(rep(C, 1, each = 2)), 
  response)
etchDat
colnames(etchDat)[1:3] <- c("A", "B", "C")
etchDat

#using formulas to produce effect estimates
aggOut <- aggregate(response ~ A + B + C, data = etchDat, sum)
aggOut
name <- c("one", "a", "b", "ab", "c", "ac", "bc", "abc")
aggOut <- data.frame(name, aggOut)
aggOut

#effect estimates from formulas
Aest <- 1/8*sum(A * aggOut[, "response"])
Aest  #interpret?
Best <- 1/8*sum(B * aggOut[, "response"])
ABest <- 1/8*sum(AB * aggOut[, "response"])
Cest <- 1/8*sum(C * aggOut[, "response"])
ACest <- 1/8*sum(AC * aggOut[, "response"])
BCest <- 1/8*sum(BC * aggOut[, "response"])
ABCest <- 1/8*sum(ABC * aggOut[, "response"])
print(c(Aest, Best, ABest, Cest, ACest, BCest, ABCest))
#agrees with table 6.5 page 247

#anova
etchOut <- lm(response ~ A*B*C, data = etchDat)
anova(etchOut)
#agrees with table 6.6 page 247

#using design matrix X (account for 2 replicates)
I <- rep(1,16)
X <- cbind(I, rep(A,1,each = 2), rep(B,1,each = 2), rep(AB,1,each = 2), 
           rep(C,1,each = 2), rep(AC,1,each = 2), rep(BC,1,each = 2), 
           rep(ABC,1,each = 2))
colnames(X) <- c("I", "A", "B", "AB", "C", "AC", "BC", "ABC")
#via normal equations
betaHat <- solve(t(X) %*% X) %*% t(X) %*% etchDat[, "response"]
betaHat

#using lm with X
etchLM <- lm(etchDat[,"response"] ~ X-1)
summary(etchLM)  #do these agree with above?


#model adequacy
plot(etchOut, 1)
plot(etchOut, 2)



#reduced/refined model
etchOut2 <- lm(response ~ A * C, data = etchDat)
anova(etchOut2)
anova(etchOut)
#why did p-values change?
summary(etchOut2)
summary(etchOut)  
#look at adjusted R-squared


plot(etchOut2, 1)
plot(etchOut2, 2)



##########################################################################
#single replicate example

A <- rep( c(-1, 1), 8)
B <- rep( c(-1, -1, 1, 1), 4)
C <- rep( c(-1, -1, -1, -1, 1, 1, 1, 1), 2)
D <- c(rep(-1, 8), rep(1, 8))
filtration <- c(45,71,48,65,68,60,80,65,43,100,45,104,75,86,70,96)

AB <- A*B
AC <- A*C
AD <- A*D
BC <- B*C
BD <- B*D
CD <- C*D
ABC <- AB*C
ABD <- AB*D
ACD <- AC*D
BCD <- BC*D
ABCD <- AB*CD

lmTemp <- lm(filtration ~ as.factor(A)*as.factor(B)*as.factor(C)*as.factor(D))
anova(lmTemp)
#crashes

Aeffect <- 1/8 * t(A) %*% filtration
Beffect <- 1/8 * t(B) %*% filtration
Ceffect <- 1/8 * t(C) %*% filtration
Deffect <- 1/8 * t(D) %*% filtration
ABeffect <- 1/8 * t(AB) %*% filtration
ACeffect <- 1/8 * t(AC) %*% filtration
ADeffect <- 1/8 * t(AD) %*% filtration
BCeffect <- 1/8 * t(BC) %*% filtration
BDeffect <- 1/8 * t(BD) %*% filtration
CDeffect <- 1/8 * t(CD) %*% filtration
ABCeffect <- 1/8 * t(ABC) %*% filtration
ABDeffect <- 1/8 * t(ABD) %*% filtration
ACDeffect <- 1/8 * t(ACD) %*% filtration
BCDeffect <- 1/8 * t(BCD) %*% filtration
ABCDeffect <- 1/8 * t(ABCD) %*% filtration

effects <- c(Aeffect, Beffect, Ceffect, Deffect, ABeffect, ACeffect, ADeffect, BCeffect, BDeffect, CDeffect,
             ABCeffect, ABDeffect, ACDeffect, BCDeffect, ABCDeffect)
qqnorm(effects)
qqOut <- qqnorm(effects)
plot(qqOut, type = 'n')
text(x = qqOut$x, y = qqOut$y, labels = c("A", "B", "C", "D", "AB", "AC", "AD", "BC", "BD", "CD", "ABC", "ABD", "ACD", "BCD", "ABCD"))

filtrationDat <- data.frame(filtration, as.factor(A), as.factor(B), as.factor(C), as.factor(D))
colnames(filtrationDat) <- c("filtration", "A", "B", "C", "D")

#author's approach:  does a 2^3 design with B left out
aov2 <- lm(filtration ~ A*C*D, data = filtrationDat)
anova(aov2)

#alternative:  including ALL main effects and only two-way interactions for important factors A,C,D
aov3 <- lm(filtration ~ A + B + C + D + AC + AD + CD, data = filtrationDat)
anova(aov3)
plot(aov3, 1)
plot(aov3, 2)

#post-hoc:  What do you report?
library(lsmeans)
#approach 1:  since AC and AD both significant, drill down to ACD level
lsmeans(aov3, ~ A|C|D )

#approach 2:  since ACD and CD not significant, report AC interaction across D and AD interaction across C
lsmeans(aov3, ~A|C)
lsmeans(aov3, ~A|D)