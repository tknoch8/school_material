library(vegan)
library(dplyr)

### Skulls Case Study ###

skulls <- read.csv("https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/HSAUR/skulls.csv", header = TRUE)
head(skulls)

skulls <- skulls[-1] 
names(skulls) <- c("Period", "y1", "y2", "y3", "y4")

## y1: maximum breadth
## y2: basibregmatic height
## y3: basialveolar length
## y4: nasal height

## Mahalanobis Distance: Distance between points in MV space (from mean of distr.)
# In this case: skulls measurements across time periods
# 4000 BC, 3300 BC, 1850 BC, 200 BC, 150 AD

(skulls_new <- group_by(skulls, Period)) ## dplyr
str(skulls_new)
levels(skulls_new$Period)

(vecybars <- summarise_all(skulls_new, mean)) ## dplyr
print(data.frame(vecybars), digits = 4)

cov.S <- function(x, group) ## creating function to obtain S
{
  x <- as.matrix(x) ## x is dataset
  p <- ncol(x)      ## number of columns
  n <- nrow(x)      ## total sample size
  k <- max(group)   ## number of groups
  nu <- rep(0,k)    ## sample size of each group stored
  nu <- as.vector(table(group))
  mat <- array(dim = c(p,p,k))
  
  ## compute cov matrix for each group
  for (i in 1:5) {mat[,,i] <- (nu[i] - 1) * cov(x[group == i,])}
  
  ## pooled cov matrix
  S <- apply(mat, 1:2, sum)
  S <- S/(n-k)
  return(round(S, 3))
}

## needed to build Cov.s function in order to estimate MD

## (next) estimating MD for all pairs of periods ##


(S <- cov.S(skulls[,-1], skulls[,1]))  ##### stuck here #####

round(data.frame(vecybars[,2:5]), 2)

library(StatMatch)

m.dist <- mahalanobis.dist(vecybars[,2:5], vc = S)^2
print(round(as.dist(m.dist), 3)) ## MD b/w periods

### (next) Mantel's Test w/skulls

(m.dist <- round(as.dist(m.dist), 3)) ## store as distance object

(y1 <- c(-4000, -3300, -1850, -200, 150)/1000)

(time.mat <- dist(y1, method = "euclidean"))

## calculate rme, observed correlation matrix

(cbind(as.vector(m.dist), as.vector(time.mat)))

(cor(as.vector(m.dist),as.vector(time.mat))) ## r(me)

## seeing if skull measurement changes b/w groups 
## are correlated w/ time change b/w groups
## Now we have the basis for Mantel's test

mantel(time.mat, m.dist)

## H0: P(me) = 0, Ha: P(me) > 0
# evidence suggesting skulls becoming more and more different as
# a function of time, due to correlation between m.dist and time.dist



### Distances based on proportions (Bray-Curtis) ###

## Dissimilarity of time allocations between bears at each of
# six different locations (compositional data).

(bears <- read.csv("~/Downloads/bears.csv", header = TRUE))

sum(bears[1,-1]) ## check that rows (bears) sum to 1
sum(bears[,-1])

## compute BC distances between bears ##

?vegdist
round(vegdist(bears[,-1]), 3) ## dissimilarity index
bears

## bears 5 and 9 most dissimilar

subset(bears, Bear == "5"); subset(bears, Bear == "9")

round(1-vegdist(bears[,-1], method = "jaccard"), 3) ## similiarity index
bears

## 7 and 8 most similar

subset(bears, Bear == "7"); subset(bears, Bear == "8")

