### 342 hw 8 ###

## (2)

pf(29.84,4,20,lower.tail=FALSE)

pf(7.64956,5,20,lower.tail=FALSE)


library(readxl)
chemdat <- read_excel("Downloads/4.3.xlsx")


chemdat <- as.data.frame(chemdat)
chemdat[,1] <- as.integer(chemdat[,1])
chemdat <- chemdat[,-1]
chemdat

chemdat[,1] <- as.factor(chemdat[,1])
chemdat[,2] <- as.factor(chemdat[,2])

lmOut <- lm(Strength ~ Chemist+Bolt, data=chemdat)
summary(lmOut)
anova(lmOut)

par(mfrow=c(2,2))
plot(lmOut)

library(lsmeans)
lsmeans(lmOut, specs = "Chemist")


## (4)

library(readxl)
assemdat <- read_excel("Downloads/4.23.xlsx")
assemdat

assemdat <- as.data.frame(assemdat[,-1])
class(assemdat)
assemdat

ordr     <- as.factor(assemdat[,1])
operator <- as.factor(assemdat[,2])
method   <- as.factor(assemdat[,3])
tim      <- as.numeric(assemdat[,4])

assemOut <- lm(tim ~ method + ordr + operator)
anova(assemOut)

## (b)

library(agricolae)

aovOut <- aov(assemdat[,4] ~ assemdat[,3] + assemdat[,1] + assemdat[,2])

hsdOut <- HSD.test(assemOut,"method")

hsdOut$groups








