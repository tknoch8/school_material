#rice data anova example
#response is seedling shoot dry weight
riceDat <- read.table("Teaching/STAT342DesignAnova/Ch3SingleFactorANOVA/riceData.txt")
riceDat
class(riceDat)  #data.frame
class(riceDat[,1])  #integer (wrong!)
riceDat[,1] <- as.factor(riceDat[,1])
class(riceDat[,1])  #factor
class(riceDat)
class(riceDat[,3])  #numeric
colnames(riceDat) <- c("treatment", "type", "yield")
save(riceDat, file = "Teaching/STAT342DesignAnova/Ch3SingleFactorANOVA/riceData.RData")

#make a plot
plot(riceDat[, c(1,3)])  #boxplot
plot(as.numeric(riceDat[,1]), riceDat[,3])  #where's the fifth point in group 1?
plot(as.numeric(riceDat[,1]) + rnorm(20, 0, sd = .02), riceDat[,3])
#make it pretty
plot(as.numeric(riceDat[,1]) + rnorm(20, 0, sd = .02), riceDat[,3], xlab = "treatment", ylab = "weight")
plot(as.numeric(riceDat[,1]) + rnorm(20, 0, sd = .02), riceDat[,3], xlab = "treatment", ylab = "weight",
     axes = F)
box()
axis(2)
axis(1, at = c(1,2,3,4), labels = c("control", "acetic", "propion", "butryic"))

#perform anova using aov tool
aovOut <- aov(yield ~ treatment, data = riceDat)  
aovOut  #not very useful
summary(aovOut)
attributes(aovOut)
aovOut$coefficients  #does this make sense?
aovOut$fitted.values  

#using lm
lmOut <- lm(yield ~ treatment, data = riceDat)  
lmOut
summary(lmOut)  #not the anova table--this tells about parameter estimates.
anova(lmOut)  #anova table -- where does p-value appear in summary output?


#what would happen if column 1 wasn't class factor?
lmWrong <- lm(riceDat[,3] ~ as.integer(riceDat[,1]))
summary(lmWrong)
anova(lmWrong)  #notice df


#linear model by hand:  formulation 2 in notes
X <- cbind( rep(1, 20), 
            c(rep(0,5), rep(1,5), rep(0,10)),
            c(rep(0,10), rep(1,5), rep(0,5)),
            c(rep(0,15), rep(1,5))
)
X
y <- riceDat[,3]
betaHat <- solve(t(X) %*% X) %*% t(X) %*% y
betaHat  #what's mu_1, mu_2...
model.matrix(aovOut)
model.matrix(lmOut)


#linear model by hand:  formulation 1 in notes
X <- cbind( c(rep(1,5), rep(0, 15)), 
            c(rep(0,5), rep(1,5), rep(0,10)),
            c(rep(0,10), rep(1,5), rep(0,5)),
            c(rep(0,15), rep(1,5))
)
X
betaHat <- solve(t(X) %*% X) %*% t(X) %*% y
betaHat  #what's mu_1, mu_2...

#a useful function for parameter estimation
library(lsmeans)
lsmeans(lmOut, specs = "treatment")


##########
#Model adequacy
##########

### normality

lmOut$fitted.values #predicteds
lmOut$residuals
hist(lmOut$residuals)  #is it okay to lump residuals together across groups?
hist(lmOut$residuals, breaks = seq(-.15, .15, .025), freq = F)
#mse = .004
x <- seq(-.15, .15, .01)
y <- dnorm(x, 0, sd = sqrt(.004))
lines(x,y, col = 2)

qqnorm(lmOut$residuals)



##### homogeneity of variance

plot(riceDat[,1], lmOut$residuals)
plot(as.numeric(riceDat[,1]), lmOut$residuals)
plot(lmOut$fitted.values, lmOut$residuals)

#bartlett test for homogeneity of variances
bartlett.test(riceDat[,3], riceDat[,1])
bartlett.test(riceDat[,3] ~ riceDat[,1])

#levene test for homogeneity of variance
library(car)
leveneTest(riceDat[,3] ~ riceDat[,1])

#summary plots (for both normality and homog. of variance)
par(mfrow = c(2,2))
plot(lmOut)


############
# confidence intervals
############

#mean estimates  (one way to get them, there are others)
y1bar <- lmOut$fitted.values[1]
y2bar <- lmOut$fitted.values[6]
y3bar <- lmOut$fitted.values[11]
y4bar <- lmOut$fitted.values[16]

#one-at-a-time
mse <- anova(lmOut)[2,3]  #.0039
mse
t <- qt(.975, df = 20-4)
t  #2.12
ci1 <- y1bar + c(-1,1)*t*sqrt(mse/5)
ci2 <- y2bar + c(-1,1)*t*sqrt(mse/5)
ci3 <- y3bar + c(-1,1)*t*sqrt(mse/5)
ci4 <- y4bar + c(-1,1)*t*sqrt(mse/5)

ci1
ci2
ci3
ci4

library(lsmeans)
lsmeans(lmOut, specs = "treatment")

#quick plot
plot( rep(seq(1,4), 1, each = 2), c(ci1, ci2, ci3, ci4), ylim = range(riceDat[, "yield"])) #could make lines...
points(as.numeric(riceDat[, "treatment"]), riceDat[, "yield"] , col = 2)

#what about confint function used in STAT 341?
confint(lmOut)      #what are these CI's of?  different than lsmeans!
lmOut$coefficients  #corresponds to coefficients--same as in STAT 341
summary(lmOut)      #reports SE's with "beta's", not for "mu's"

#Bonferroni correction
t <- qt(1 - .05/(2*4), df = 16)
t  #2.813, quite a bit bigger than 2
ci1 <- y1bar + c(-1,1)*t*sqrt(mse/5)
ci2 <- y2bar + c(-1,1)*t*sqrt(mse/5)
ci3 <- y3bar + c(-1,1)*t*sqrt(mse/5)
ci4 <- y4bar + c(-1,1)*t*sqrt(mse/5)
ci1
ci2
lsMeansOut <- lsmeans(lmOut, specs = "treatment")
confint(lsMeansOut, adjust = "bon")


######################
# Pairwise hypothesis tests
######################
#reloading data, running lm, lsmeans
load("Teaching/STAT342DesignAnova/Ch3SingleFactorANOVA/riceData.RData")
lmOut <- lm(yield ~ treatment, data = riceDat) 
library(lsmeans)
lsMeansOut <- lsmeans(lmOut, ~ treatment)  #notice syntax

#doing individual pairwise tests
#1 vs 2
critVal <- qt(.975, 20-4)   #2.12
lsMeansOut
testStat <- (4.282 - 3.868)/sqrt(.00393*2/5)
testStat
#2 vs 3
testStat <- (3.868 - 3.728)/sqrt(.00393*2/5)
testStat
2*(1 - pt(3.53, 16))
#the LSD:
critVal*sqrt(.00393*2/5)  #.084


#Fisher's LSD
library(agricolae)
lsdOut <- LSD.test(lmOut, "treatment")
lsdOut
lsdOut$groups
lsdOut$statistics
lsdOut <- LSD.test(lmOut, "treatment", p.adj = "bonferroni")
lsdOut$groups
lsdOut$statistics
lsdOut <- LSD.test(lmOut, "treatment", alpha = .05/6)
lsdOut$statistics


#Tukey HSD
y1bar
y2bar
y3bar
y4bar
#two ways of running HSD test
hsdOut <- HSD.test(lmOut, "treatment")  #works for either lmOut or aovOut
hsdOut
tukeyOut <- TukeyHSD(aovOut)  #doesn't work for lmOut
tukeyOut
TukeyHSD(aovOut, ordered = T)
TukeyHSD(aovOut, ordered = T, conf.level = .99)






#contrasts
library(gmodels)
L <- matrix( c(1, -1/3, -1/3, -1/3), nrow = 1, ncol = 4)
rownames(L) <- ": control vs. mean acid"
L
fit.contrast(aovOut, "treatment", L)

L2 <- rbind(L, c(0, 1, -1, 0), c(0,1, 0, -1))
rownames(L2) <- c(":  control vs mean acid", ": 2 vs. 3", "2 vs 4")
L2
fit.contrast(aovOut, "treatment", L2)

#seeing the non-orthogonality
X <- model.matrix(aovOut)
out <- X %*% t(L2)
cor(out)
