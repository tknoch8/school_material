### (1) ###

## (b) 

tools <- read.csv("~/Downloads/problem6.1.csv")

A <- as.factor(rep(c(-1,1,-1,1,-1,1,-1,1),3))
B <- as.factor(rep(c(-1,-1,1,1,-1,-1,1,1),3))
C <- as.factor(rep(c(-1,-1,-1,-1,1,1,1,1),3))

AB <- A*B
AC <- A*C
BC <- B*C
ABC <- A*B*C

response <- c(22,32,35,55,44,40,60,39,31,43,34,47,45,37,50,41,25,29,
              50,46,38,36,54,47)

lmOut <- lm(response ~ A*B*C)
aovOut <- aov(response ~ A*B*C)
anova(lmOut)


dat <- data.frame(A,B,C,response)
colnames(dat) <- c("A","B","C","response")
dat
library(lsmeans)

anova(lmOut)
(Aest <- lsmeans(aovOut, ~ A))
(Best <- lsmeans(aovOut, ~ B))
(Cest <- lsmeans(aovOut, ~ C))
(ABest <- lsmeans(aovOut, ~ A*B))
(ACest <- lsmeans(aovOut, ~ A*C))
(BCest <- lsmeans(aovOut, ~ B*C))
(ABCest <- lsmeans(aovOut, ~ A*B*C))

one <- sum(tools$Life.Hours[1:3])
a <- sum(tools$Life.Hours[4:6])
b <- sum(tools$Life.Hours[7:9])
ab <- sum(tools$Life.Hours[10:12])
c <- sum(tools$Life.Hours[13:15])
ac <- sum(tools$Life.Hours[16:18])
bc <- sum(tools$Life.Hours[19:21])
abc <- sum(tools$Life.Hours[22:24])

(totals <- data.frame(one, a, b, ab, c, ac, bc, abc))

A <- c(-1,1,-1,1,-1,1,-1,1)
B <- c(-1,-1,1,1,-1,-1,1,1)
C <- c(-1,-1,-1,-1,1,1,1,1)

(Aeff <- (1/12)*sum(A*totals))
(Beff <- (1/12)*sum(B*totals))
(ABeff <- (1/12)*sum(A*B*totals))
(Ceff <- (1/12)*sum(C*totals))
(ACeff <- (1/12)*sum(A*C*totals))
(BCeff <- (1/12)*sum(B*C*totals))
(ABCeff <- (1/12)*sum(A*B*C*totals))
anova(lmOut)

A <- as.factor(rep(c(-1,1,-1,1,-1,1,-1,1),3))
B <- as.factor(rep(c(-1,-1,1,1,-1,-1,1,1),3))
C <- as.factor(rep(c(-1,-1,-1,-1,1,1,1,1),3))

response <- c(22,32,35,55,44,40,60,39,31,43,34,47,45,37,50,41,25,29,
              50,46,38,36,54,47)

lmRed <- lm(response ~ B + C + A*C)
anova(lmRed)
par(mfrow=c(2,2))
plot(lmRed)

aovRed <- aov(response ~ B + C + A*C)
(estRed <- lsmeans(aovRed, ~ B + C + A*C))
