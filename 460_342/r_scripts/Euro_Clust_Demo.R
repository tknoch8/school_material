options(warn=-1); library(tidyverse); library(ggplot2);
library(GGally); library(ggrepel); library(here)

euroemp <- readr::read_csv(
  "/Users/travis.knoche/OneDrive - Polaris Alpha/r/from_school/460_342/data_raw/Euroemp.csv"
)

# AGR (y1) = agriculture,forestry, and fishing
# MIN (y2) = mining and quarrying
# MAN (y3) = manufacturing
# PS  (y4) = power and water supplies
# CON (y5) = construction
# SER (y6) = services
# FIN (y7) = finance
# SPS (y8) = social and personal services
# TC  (y9) = transport and communications

euroemp

euroemp <- euroemp %>% 
  unite(Country_Group, 
        Country, 
        Group, 
        sep = "_", 
        remove = TRUE) %>% 
  column_to_rownames(var = "Country_Group")

euroemp

dist.euro <- dist(scale(euroemp))


## "nearest neighbor method"

clustemp.nn <- hclust(dist.euro, 
                      method = "single")

plot(clustemp.nn, 
     hang = -1, 
     cex = 1, 
     ylab = "Distance between Countries",
     main = "Dendrogram of people employed in nine industries from
     European Countries; nearest-neighbor method")

## say we want 4 clusters
rect.hclust(clustemp.nn, 4)
clustcut01.nn <- cutree(clustemp.nn, 4)
sort(clustcut01.nn)

## as we can see, this does not seem to be a very meaningful seperation
## of countries

## Let's try again with K-means

## K-means allows objects to be moved between clusters

set.seed(07272018)

## nstart option runs multiple initial configurations
## and reports best one

clustemp.kn <- kmeans(scale(euroemp), 
                      4, 
                      nstart = 10)

sort(clustemp.kn$cluster)
## seperation seems better

clustemp.kn$tot.withinss/clustemp.kn$totss
## we would like WSS/totSS to be around 0.2, but 0.3 is ok.
## The value of 0.435 is too high, meaning there is not sufficient
## "seperation" between the four clusters. There is too much overlap.

## Was 4 clusters an appropriate choice?
## Let's make a scree plot

prop.wss <- rep(0,10)
index.size <- 1:10
for (i in 1:10) {
  k.mean <- kmeans(scale(euroemp), 
                   index.size[i], 
                   nstart = 10)
  prop.wss[i] <- k.mean$tot.withinss/k.mean$totss
}

plot(index.size, prop.wss, type = "b",
     xlab = "Number of Clusters",
     ylab = "Propertion of Within-Cluster Sum of Squares")

abline(0.3, 0, col = 'blue')
abline(0.2, 0, col = 'green')

## Looks like we should have used 6 or 7 clusters
## achieve a WSS of around 0.2.

clustemp.kn6 <- kmeans(scale(euroemp), 6, nstart = 10) ## 6 clusters
sort(clustemp.kn6$cluster)
clustemp.kn6$tot.withinss/clustemp.kn6$totss

clustemp.kn7 <- kmeans(scale(euroemp), 7, nstart = 10) ## 7 clusters
sort(clustemp.kn7$cluster)
clustemp.kn7$tot.withinss/clustemp.kn7$totss

## Whether these results are meaningful depends on knowledge of these
## countries, as well as the overall research goals.


## Let's try again with a Discrimant Function Analysis (DFA)