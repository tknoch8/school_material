## (1)

# (a)
q <- 3.96
MSE <- 10.14
n <- 6
HSD <- q*sqrt(MSE/n)
HSD

y1bar <- 94.15
y2bar <-90.72
y3bar <- 95.57
y4bar <- 102.70

y1bar-y2bar; HSD
y1bar-y3bar; HSD
y1bar-y4bar; HSD
y2bar-y3bar; HSD
y2bar-y4bar; HSD
y3bar-y4bar; HSD

# (c) Dunnet's Procedure

a <- 4
f <- 20
n <- 6
d <- 2.54

dun <- d*sqrt((2*MSE)/n)
dun

y2bar-y1bar; dun
y3bar-y1bar; dun
y4bar-y1bar; dun

# (2)

load("~/Downloads/contrastData.RData")

dataMtx[,1] <- as.factor(dataMtx[,1])
dataMtx[,2] <- as.factor(dataMtx[,2])
dataMtx[,3] <- as.factor(dataMtx[,3])

plot(dataMtx)
y <- dataMtx[,4]
anovaOut <- lm(y ~ dataMtx[,3])
anova(anovaOut)

plot(anovaOut)
par(mfrow=c(2,2))

