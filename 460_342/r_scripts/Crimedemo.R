### 460 hw 10 ###

library(tidyverse)
library(ggplot2) 
library(GGally)
library(ggrepel)
library(here)

crime <- read.csv(here("data_sets", "crime.csv"))
crime

## convert to table
(crime <- crime %>% 
    column_to_rownames(var = "City"))

## standardize
(std.crime <- round(scale(crime), 4))

(round(cor(crime), 4)) # cor(crime) = cor(std.crime)
(round(cor(std.crime), 4))
## we can see that many of the correlations between crimes
## (across cities) make sense.

(dist.crime <- round(dist(crime), 4))
(dist.std.crime <- round(dist(std.crime), 4))

## 1 (c), nearest neighbor dendrogram

(clust.nn.std.crime <- hclust(dist.std.crime, method = "single"))

## can use standardized or non-standardized data
(clust.nn.crime <- hclust(dist.crime, method = "single"))

plot(clust.nn.std.crime, hang = -1, cex = 0.6,
     ylab = "Standardized Distance between Cities",
     main = "Dendogram of crime rates in Cities")

plot(clust.nn.crime, hang = -1, cex = 0.6,
     ylab = "Distance between Cities",
     main = "Dendrogram of crime rates in Cities")
## standardization affects clustering. Compare to distance
## objects

dist.std.crime
dist.crime

## repeat clustering with just first 6 observations

crime6 <- crime[1:6,]
std.crime6 <- scale(crime6)
dist.crime6 <- dist(crime6)
dist.std.crime6 <- dist(std.crime6)

(clust.nn.crime6 <- hclust(dist.crime6, method = "single"))

plot(clust.nn.crime6, hang = -1, cex = 0.7,
     ylab = "Distance between first 6 Cities",
     main = "Dendogram of crime rates in first 6 Cities")
## compare to distance object
dist.crime6

## 1 (f)

clus.crimall <- hclust(dist.crime, method = "average")

plot(clus.crimall, hang = -1, cex = 0.6,
     ylab = "Distance between Cities",
     main = "Dendrogram of all 16 Cities")


## 1 (g)

library(NbClust)

temp <- NbClust(diss = dist.crime, min.nc = 2, max.nc = 10,
                distance = NULL, method = "average", index = "mcclain")

temp$Best.nc

temp$Best.partition ## "best" partition


## 2 (a)

prot <- read.csv("http://www.stat.colostate.edu/~pturk/data/protein.csv")

prot <- prot %>% column_to_rownames(var="Country")

set.seed(20180416)

prop.wss <- rep(0,10)
index.size <- 1:10

for (i in 1:10)
{
  temp <- kmeans(prot, index.size[i], nstart = 10)
  prop.wss[i] <- temp$tot.withinss/temp$totss
}

plot(index.size, prop.wss, type = "b", xlab = "Number of clusters",
     ylab = "Proportion of within sums of squares")

abline(0.3, 0)


## 2 (b)

# based on the scree plot, we should use 4 clusters, as the within-ss for
# 3 clusters is greater than 0.3


## 2 (c)

prot.kn <- kmeans(prot, 5, nstart = 10)

sort(prot.kn$cluster) ## cluster solution

prot.kn$tot.withinss/prot.kn$totss


## 2 (d)

