### 342 hw 4 ###

# (2)

library(readxl)
dat <- read_excel("~/Downloads/342hw4_3.7.xlsx")
dat <- data.frame(dat)
dat[,1] <- as.factor(dat[,1])
dat[,2]

# (a)
plot(dat[, c(1,2)])
plot(as.numeric(dat[,1]), dat[,2])
plot(jitter(as.numeric(dat[,1]), factor = .4),dat[,2],axes=F,xlab="Technique",ylab="Strength")
box()
axis(2)
axis(1, at = c(1,2,3,4), labels = c("1","2","3","4"))

mean1 <- (3129+2685+3000+2890)/4
mean2 <- (3200+3300+2975+3150)/4
mean3 <- (2800+2900+2985+3050)/4
mean4 <- (2600+2700+2600+2765)/4

points(1,mean1,pch=19)
points(2,mean2, pch=19)
points(3, mean3, pch=19)
points(4, mean4, pch=19)
text(mean1,mean2, mean3, mean4, labels = mean1) ## figure this out

# (b) test all mu equal

colnames(dat) <- c("technique", "strength")

aovOut <- aov(dat[,2] ~ dat[,1], data=dat)
summary(aovOut)

lmOut <- lm(dat[,2] ~ as.factor(dat[,1]))
anova(lmOut)

# (c) check model assumptions

plot(as.numeric(dat[,1]), lmOut$residuals,
     xlab="Technique",ylab="Residuals",axes=F)
box()
axis(2)
axis(1, at = c(1,2,3,4), labels = c("1","2","3","4"))

plot(lmOut$fitted.values, lmOut$residuals,
     xlab="Fitted Values",ylab="Residuals")

qqnorm(scale(lmOut$residuals))
abline(0,1)

## bartlett test ##

?bartlett.test

bartlett.test(dat[,2],dat[,1])
bartlett.test(dat[,2] ~ dat[,1])

## levine test ##
library(car)
leveneTest(dat[,2] ~ dat[,1])

