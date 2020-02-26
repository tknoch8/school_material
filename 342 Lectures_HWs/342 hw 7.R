## 1 ##

feedat <- read.csv("~/Downloads/problem5.4.csv")
feedat <- feedat[,-1]

library(daewr)
library(lsmeans)

feedat[,1] <- as.factor(feedat[,1])
feedat[,2] <- as.factor(feedat[,2])
feedat[,3] <- as.numeric(feedat[,3])

depth <- feedat[,1]
rate <- feedat[,2]
finish <- feedat[,3]

## (a) ##

lmOut1 <- lm(finish ~ depth*rate)
lmOut2 <- lm(finish ~ depth + rate + depth*rate)
aovOut1 <- aov(finish ~ depth*rate)

summary(lmOut1)
summary(aovOut1)

par(mfrow=c(2,2))
plot(lmOut1)

## (b) ##

anova(lmOut1)

lsmeans(lmOut1, ~ depth|rate)
lsmeans(lmOut1, ~ depth)
lsmeans(lmOut1, ~ rate)

with(feedat, (interaction.plot(rate, depth, finish, type = "b")))

pairs(lsOut)
lsOut <- lsmeans(aovOut1,~depth|rate)

library(multcompView)
cld(lsOut)
model.tables(lsOut,type="means", se = T)

### (3) ###

wooDat <- read.csv("~/Downloads/problem5.18.csv")

wooDat[,1] <- as.factor(wooDat[,1])
wooDat[,2] <- as.factor(wooDat[,2])
wooDat[,3] <- as.factor(wooDat[,3])
wooDat[,4] <- as.numeric(wooDat[,4])

hardwood1 <- as.factor(wooDat[,1])
time1 <- as.factor(wooDat[,2])
pressure1 <- as.factor(wooDat[,3])
strength1 <- as.numeric(wooDat[,4])


## (b)

lmWood <- lm(strength1 ~ hardwood1*time1*pressure1)
aovWood <- aov(strength1 ~ hardwood1*time1*pressure1)

par(mfrow=c(1,3))
plot(lmWood)

anova(lmWood)

with(wooDat, (interaction.plot(hardwood1, time1, strength1, type = "b")))
with(wooDat, (interaction.plot(hardwood1, pressure1, strength1, type = "b")))
with(wooDat, (interaction.plot(time1, pressure1, strength1, type = "b")))


## (c)

lsmeans(aovWood, ~ hardwood1|time1|pressure1)

model.tables(aovWood,type="means", se = T)



















