### 1 (a) ###

setting <- c("one", "a", "b", "ab", "c", "ac", "bc", "abc", "d", "ad", "bd", "abd", "cd", "acd", "bcd", "abcd")

A.time <- c(-1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1)
B.conc <- c(-1, -1, 1, 1, -1, -1, 1, 1, -1, -1, 1, 1, -1, -1, 1, 1)
C.press <- c(-1, -1, -1, -1, 1, 1, 1, 1, -1, -1, -1, -1, 1, 1, 1, 1)
D.temp <- c(-1, -1, -1, -1, -1, -1, -1, -1, 1, 1, 1, 1, 1, 1, 1, 1)

AB <- A.time*B.conc
AC <- A.time*C.press
AD <- A.time*D.temp
BC <- B.conc*C.press
BD <- B.conc*D.temp
CD <- C.press*D.temp
ABC <- A.time*B.conc*C.press
ABD <- A.time*B.conc*D.temp
ACD <- A.time*C.press*D.temp
BCD <- B.conc*C.press*D.temp
ABCD <- A.time*B.conc*C.press*D.temp

yield <- c(12,18,13,16,17,15,20,15,10,25,13,24,19,21,17,23)

one  <- yield[1]
a    <- yield[2]
b    <- yield[3]
ab   <- yield[4]
c    <- yield[5]
ac   <- yield[6]
bc   <- yield[7]
abc  <- yield[8]
d    <- yield[9]
ad   <- yield[10]
bd   <- yield[11] 
abd  <- yield[12]
cd   <- yield[13]
acd  <- yield[14]
bcd  <- yield[15]
abcd <- yield[16]

totals <- data.frame(one,a,b,ab,c,ac,bc,abc,d,ad,bd,abd,cd,acd,bcd,abcd)

(setUpMtx <- data.frame(setting, A.time, B.conc, C.press, D.temp, AB, AC, AD, BC, BD,
                       CD, ABC, ABD, ACD, BCD, ABCD, yield))

Aest <-   (1/8)*t(A.time) %*% yield
Best <-   (1/8)*t(B.conc) %*% yield
Cest <-   (1/8)*t(C.press) %*% yield
Dest <-   (1/8)*t(D.temp) %*% yield
ABest <-  (1/8)*t(AB) %*% yield
ACest <-  (1/8)*t(AC) %*% yield
ADest <-  (1/8)*t(AD) %*% yield
BCest <-  (1/8)*t(BC) %*% yield
BDest <-  (1/8)*t(BD) %*% yield
CDest <-  (1/8)*t(CD) %*% yield
ABCest <- (1/8)*t(ABC) %*% yield
ABDest <- (1/8)*t(ABD) %*% yield
ACDest <- (1/8)*t(ACD) %*% yield
BCDest <- (1/8)*t(BCD) %*% yield
ABCDest <-(1/8)*t(ABCD) %*% yield

(Effects <- c(Aest,Best,Cest,Dest,ABest,ACest,ADest,BCest,BDest,CDest,
             ABCest,ABDest,ACDest,BCDest,ABCDest))

(qqOut <- qqnorm(Effects))
plot(qqOut, type = 'n', main = "Effects QQ")
text(x = qqOut$x, y = qqOut$y, labels = c("A","B","C","D", "AB","AC","AD","BC",
                                          "BD","CD","ABC","ABD","ACD","BCD","ABCD"))

aov.Dat <- data.frame(as.factor(A.time),as.factor(D.temp),as.factor(AC),as.factor(AD),as.factor(C.press),yield)

aov2 <- lm(yield ~ A.time*C.press*D.temp, data = aov.Dat)
anova(aov2)

# (c)

I <- rep(1,5)
X <- cbind(I,A.time,C.press,AC,D.temp,AD)
colnames(X) <- c("I","A","C","AC","D","AD")

betaHat <- solve(t(X) %*% X) %*% t(X) %*% yield
betaHat

lmOut <- lm(yield ~ X-1)
summary(lmOut)

par(mfrow=c(2,2))
plot(lmOut)

# (2)
# (a)

setMtx <- c("(1)","a","b","ab","c","ac","bc","abc")

order <- as.factor(c(6,7,8,5,3,1,2,4))

temp <- c(-1,1,-1,1,-1,1,-1,1)

cat <- c(-1,-1,1,1,-1,-1,1,1)

press <- c(-1,-1,-1,-1,1,1,1,1)

weight <- c(24.5,29.6,23.4,29.9,22.6,39.6,24.8,42.3)

polMtx <- data.frame(setMtx,order,temp,cat,press,weight)

(catEff <- (1/8) * t(cat) %*% weight)

# (c)






