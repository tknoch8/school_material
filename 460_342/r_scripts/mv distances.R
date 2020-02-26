### multiv distances between single observations ###

dogs <- read.csv("http://www.stat.colostate.edu/~pturk/data/prehistoric.csv", header = TRUE)
dogs             

matstdcan <- scale(dogs[-1]) ## standardized observation matrix
stdcanprt <- round(matstdcan, digits=2) ## rounded to 2 digits

library(vegan)

dist1can <- dist(matstdcan, method = "euclidean") ## matrix of euclidian distances
print(dist1can, digits = 2)
dist1can <- as.matrix(dist1can)

### mv distance between populations and samples ###

skulls <- read.csv("http://www.stat.colostate.edu/~pturk/data/skulls.csv", header = TRUE)

library(tidyverse)

skulls_new <- group_by(skulls, Period) ## group skulls by period
vec.ybars <- summarise_all(skulls_new, mean) ## creates vectors of ybars by Period
print(data.frame(vec.ybars), digits = 4)

mat.Si <- skulls_new %>% do(data.frame(Si = cov(.[2:5]))) ## matrix of Si's for each sample
mat.Si

S <- (29*mat.Si[1:4,] + 29*mat.Si[5:8,]
     + 29*mat.Si[9:12,] + 29*mat.Si[13:16,]
     + 29*mat.Si[17:20,])/(150 - 5)
round(S[,-1], 3)                             

