### 342 hw 2

# (4) (a)

?dgamma

x <- seq(0, 20, .01)
y <- dgamma(x, shape = 2, scale = 3)

plot(x,y)

# (b)

# E[X] = k*theta, k = 2, theta = 3

k <- 2
theta <- 3
k*theta

# (e)

m <- rgamma(12, shape = 2, scale = 3)
mbar <- mean(m)

sd <- sqrt(var(m))

z <- (m-6)/(sd)
z

randSamp <- rgamma(12, shape = 2, scale = 3)
sampMean <- mean(randSamp)
sampSD <- sqrt(var(randSamp))
z <- (sampMean-6)/(sampSD)


iter <- 500
sampMean <- numeric(iter)
SD <- numeric(iter)
z <- numeric(iter)
for(i in seq(1, iter))
{
  randSamp <- rgamma(12, shape = 2, scale = 3)
  sampMean[i] <- mean(randSamp)
  SD[i] <- sqrt(3/2)
  }
z <- (sampMean-6)/(SD)
hist(z, freq = F)
x <- seq(-3,3, .01)
density <- dnorm(x)
lines(x, density, col = 2)


# (f)

iter <- 500
sampMean <- numeric(iter)
s <- numeric(iter)
t <- numeric(iter)
for(i in seq(1, iter))
{
  randSamp <- rgamma(12, shape = 2, scale = 3)
  sampMean[i] <- mean(randSamp)
  s[i] <- sd(randSamp)
}

t <- (sampMean-6)/(s)

hist(t, freq = F)
x <- seq(-3,3, .01)
y <- dt(x, df = 10)
lines(x, y, col = 2)











