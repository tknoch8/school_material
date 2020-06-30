require(tidyverse)
require(vegan)

(bears <- read.csv(here::here("data_sets", "bears.csv"), header = TRUE))

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
