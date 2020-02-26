#----------------------------spacial_autocorrelation---------------------------

# http://rspatial.org/analysis/rst/3-spauto.html

# Spatial autocorrelation is an important concept in spatial statistics. 
  # It is a both a nuisance, as it complicates statistical tests, and a 
  # feature, as it allows for spatial interpolation.


#----------------------------temporal autocorrelation--------------------------

# If you measure something about the same object over time, for example a 
  # persons weight or wealth, it is likely that two observations that are 
  # close to each other in time are also similar in measurement. Say that 
  # over a couple of years your weight went from 50 to 80 kg. It is unlikely 
  # that it was 60 kg one day, 50 kg the next and 80 the day after that. 
  # Rather it probably went up gradually, with the occasional tapering off, 
  # or even reverse in direction. The same may be true with your bank account, 
  # but that may also have a marked monthly trend. To measure the degree of 
  # association over time, we can compute the correlation of each observation 
  # with the next observation.

# Let d be a vector of daily observations.

set.seed(20181001)

d <- sample(100, 10)
d

# compute autocorrelation
a <- d[-length(d)]
b <- d[-1]
plot(a, b, xlab = 't', ylab = 't-1')
cor(a, b)

# sort d, then compute cor
d <- sort(d)
d
a <- d[-length(d)]
b <- d[-1]
plot(a, b, xlab='t', ylab='t-1')
cor(a, b)

# The acf() function shows autocorrelation computed in a slightly different way 
  # for several lags (it is 1 to each point it self, very high when comparing 
  # with the nearest neighbour, and than tapering off).
acf(d)  # not really sure how to interpret this plot. it seems like the blue line is
  # somewhat of a cutoff for "acceptable" autocorrelation values at different lags


#----------------------------spatial autocorrelation---------------------------

# The concept of spatial autocorrelation is an extension of temporal autocorrelation. 
  # It is a bit more complicated though. Time is one-dimensional, and only goes in 
  # one direction, ever forward. Spatial objects have (at least) two dimensions and 
  # complex shapes, and it may not be obvious how to determine what is “near”.

# Measures of spatial autocorrelation describe the degree two which observations 
  # (values) at spatial locations (whether they are points, areas, or raster cells),
  # are similar to each other. So we need two things: observations and locations.

# Spatial autocorrelation in a variable can be exogenous (it is caused by another 
  # spatially autocorrelated variable, e.g. rainfall) or endogenous (it is caused 
  # by the process at play, e.g. the spread of a disease).

## example data:

library(raster)

p <- shapefile(system.file("external/lux.shp", package = "raster"))
p <- p[p$NAME_1 == "Diekirch", ]
p$value <- c(10, 6, 4, 11, 6)
data.frame(p)

# Let’s say we are interested in spatial autocorrelation in variable “AREA”. 
  # If there were spatial autocorrelation, regions of a similar size would be 
  # spatially clustered.

# Here is a plot of the polygons. I use the coordinates function to get the 
  # centroids of the polygons to place the labels.
par(mai = c(0,0,0,0))
plot(p, col = 2:7)
xy <- coordinates(p)
points(xy, cex =6 , pch = 20, col = 'white')
text(p, 'ID_2', cex = 1.5)

## Adjacent Polygons##
# Now we need to determine which polygons are “near”, and how to quantify that. 
  # Here we’ll use adjacency as criterion. To find adjacent polygons, we can use 
  # package ‘spdep’.
library(spdep)

w <- poly2nb(p, row.names = p$Id)
class(w)
summary(w)  # describes "neighborhood"
str(w)

# plot links between polygons
plot(p, col = 'gray', border = 'blue', lwd = 2)
plot(w, xy, col = 'red', lwd = 2, add = TRUE)

# We can transform w into a spatial weights matrix. A spatial weights matrix 
  # reflects the intensity of the geographic relationship between observations.
wm <- nb2mat(w, style = 'B')
wm

## Now let’s compute Moran’s index of spatial autocorrelation, I, BY HAND
n <- length(p)
y <- p$value
ybar <- mean(y)

# Now we need (yi−y¯)(yj−y¯), that is, (yi-ybar)(yj-ybar) for all pairs.
dy <- y - ybar
g <- expand.grid(dy, dy)
yiyj <- g[, 1] * g[, 2]

# create matrix of multiplied pairs
pm <- matrix(yiyj, ncol = n)

# And multiply this matrix with the weights to set to zero the value for 
  # the pairs that are not adjacent.
pmw <- pm * wm
pmw

# sum values 
spmw <- sum(pmw)
spmw

# divide by sum of weights
smw <- sum(wm)
sw  <- spmw / smw

# compute inverse variance of y
vr <- n / sum(dy^2)

# The final step to compute Moran’s I
MI <- vr * sw
MI

# This is a simple (but crude) way to estimate the expected value of Moran’s I. 
  # That is, the value you would get in the absence of spatial autocorelation 
  # (if the data were spatially random).
EI <- -1/(n-1)
EI
# appraoches zero as n increases

# same thing (moran's I) using `spdep` package
library(spdep)
ww <-  nb2listw(w, style = 'B')
ww

moran(p$value, ww, n = length(ww$neighbours), S0 = Szero(ww))

## testing moran's I ##
# you should use Monte Carlo simulation. That is the preferred method 
  # (in fact, the only good method). The oay it works that the values 
  # are randomly assigned to the polygons, and the Moran’s I is computed. 
  # This is repeated several times to establish a distribution of expected 
  # values. The observed value of Moran’s I is then compared with the simulated 
  # distribution to see how likely it is that the observed values could be 
  # considered a random draw.
moran.mc(p$value, ww, nsim = 99)
# null hypothesis: there is no ststistically significant spatial autocorrealtion
# p-value = 0.03 -> reject null, there IS significant spatial autocorrelation for
  # values defined (p$value)

# We can make a “Moran scatter plot” to visualize spatial autocorrelation. 
  # We first get the neighbouring values for each value.
n <- length(p)
ms <- cbind(id = rep(1:n, each = n), y = rep(y, each = n), value = as.vector(wm * y))

# Remove the zeros
ms <- ms[ms[,3] > 0, ]

# And compute the average neighbour value
ams <- aggregate(ms[,2:3], list(ms[,1]), FUN = mean)
ams <- ams[,-1]
colnames(ams) <- c('y', 'spatially lagged y')
head(ams)

# plot
plot(ams)
reg <- lm(ams[,2] ~ ams[,1])
abline(reg, lwd = 2)
abline(h = mean(ams[,2]), lt = 2)
abline(v = ybar, lt = 2)

# Note that the slope of the regression line:
coefficients(reg)[2]
# is almost the same as moran's I

# Here is a more direct approach to accomplish the same thing (but hopefully
  # the above makes it clearer how this is actually computed). Note the row 
  # standardisation of the weights matrix:
rwm <- mat2listw(wm, style = 'W')
# Checking if rows add up to 1
mat <- listw2mat(rwm)
apply(mat, 1, sum)[1:15]

# plot
moran.plot(y, rwm)
