library(readxl)
kboard <- read_excel("~/Downloads/5.30.xlsx")
kboard

kboard <- as.data.frame(kboard)
kboard[,1] <- as.factor(kboard[,1])
kboard[,2] <- as.factor(kboard[,2])
kboard[,3] <- as.numeric(kboard[,3])

y <- kboard[,3]
size <- kboard[,1]
feel <- kboard[,2]

lmKey <- lm(y ~ size*feel)
summary(lmKey)

par(mfrow = c(2,2))
plot(lmKey)

anova(lmKey)


library(agricolae)

aovOut <- aov(y ~ size*feel)

(hsdOutsize <- HSD.test(lmKey, "size"))

(hsdOutfeel <- HSD.test(lmKey, "feel"))

library(lsmeans)

lsmeans(lmKey, specs = c("size", "feel"))


## (2) ##

A <- as.factor(rep(c(-1, 1, 1, -1), 4))
B <- as.factor(rep(c(-1, -1, 1, 1), 4))
response <- c(18.2, 27.2, 41, 15.9, 18.9, 24, 43.9, 
              14.5, 12.9, 22.4, 36.3, 15.1, 
              14.4, 22.5, 39.9, 14.2)

lmOut <- lm(response ~ A*B)
anova(lmOut)
par(mfrow=c(2,2))
plot(lmOut)





