# 1 (b)

dat <- read.csv("~/Downloads/13.2data.csv")
head(dat)

impDat <- dat[,-1]
head(impDat)

class(impDat$Part)

impDat$Part <- as.factor(impDat$Part)
impDat$Test <- as.factor(impDat$Test)
impDat$Inspector <- as.factor(impDat$Inspector)

aovWrong <- aov(Impedance ~ Inspector * Part, data = impDat)

summary(aovWrong)

MS_A <- 19.6
MS_B <- 437.3
MS_AB <- 2.7
MS_E <- 0.5

(F_A <- MS_A/MS_AB)
1-pf(F_A, 2, 18)
(F_B <- MS_B/MS_AB)
1-pf(F_B, 9, 18)
(F_AB <- MS_AB/MS_E)
1-pf(F_AB, 18, 58)

Source <- c("Inspector", "Part", "Test", "Inspector:Part",
            "Residuals", "Total")
Df <- c("2", "9", "2", "18", "58","89")
Sum_Sq <- c("39", "3936", "0", "49", "30", "4054")
Mean_Sq <- c("19.6", "437.3", "0.2", "2.7", "0.5","-")
F_Value <- c("7.259259", "161.963", ".", "5.4","-","-")
P_Value <- c("0.0049", "2.33e-15", ".", "4.34e-07","-","-")
anovaTable <- data.frame(Source,Df,Sum_Sq,Mean_Sq,F_Value,P_Value)
anovaTable

a <- 3
b <- 10
n <- 3

## is this correct for 1 (c) ?
(sigSqdHat <- MS_E)
(sigSqd_AB <- (MS_AB-MS_E)/n)
(sigSqd_A <- (MS_A-MS_AB)/(b*n))
(sigSqd_B <- (MS_B-MS_AB)/(a*n))


# (e)

library(lme4)

## should be correct, given inspector and part are random
lmerOut <- lmer(Impedance ~ 1 + (1 | Part) + (1 | Inspector) +
                  (1 | Part:Inspector), data = impDat)
summary(lmerOut)

library(lmerTest)

ranova(lmerOut) ## is this correct? recieved error code for "rand" function

confint(lmerOut) ## is this correct?






















