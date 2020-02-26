library(here)
library(tidyverse)

#----------------------------Mantel's Test-------------------------------------

# https://stats.idre.ucla.edu/r/faq/how-can-i-perform-a-mantel-test-in-r/

# ozone measurements from 32 loacations in Los Angeles aggregated over one month
ozone <- read.table("https://stats.idre.ucla.edu/stat/r/faq/ozone.csv", 
                    sep = ",", 
                    header = T)

# write.csv(ozone, file = here::here("data_raw", "ozone_dat.csv"))
# 
# ozone <- read_csv(here::here("data_raw", "ozone_dat.csv"))

head(ozone, n = 10)

# create two distance objects for mantels test. one for spatial distances and one
# for measured outcomes
library(ade4)

station.dists <- dist(cbind(ozone$Lon, ozone$Lat),
                      method = "euclidean")
ozone.dists <- dist(ozone$Av8top,
                    method = "euclidean")

# view a bit of each distance object
as.dist(as.matrix(station.dists)[1:5, 1:5])
as.dist(as.matrix(ozone.dists)[1:5, 1:5])
# these are the two matrices we will use to test for a correlation, using Mantel's Test

#----------------------------performing the test-------------------------------

# The test consists of calculating the correlation of the entries in the matrices, 
# then permuting the matrices and calculating the same test statistic under each 
# permutation and comparing the original test statistic to the distribution of test 
# statistics from the permutations to generate a p-value. The number of permutations 
# defines the precision with which the p-value can be calculated. The function to 
# perform the Mantel test is mantel.rtest and the required arguments are the two 
# distance matrices The number of permutations can also be specified by the user, 
# but is by default 99.

# null hypothesis: the two matrices are not related (no significant correlation)
set.seed(20181001)
mantel.rtest(station.dists, ozone.dists, nrepet = 9999)

#----------------------------test results:-------------------------------------

# Observation: 0.1636308 (observed correlation between m1 and m2) (small positive assoc.)
  # smaller differences in ozone are generally seen among pairs of stations that are 
  # close to each other than far from each other.
# Based on 9999 replicates
# Simulated p-value: 0.0276 (reject null hypothesis at alpha = 0.05)
  # positive correlaton of 0.1636308 is statistically significant*
# Alternative hypothesis: greater (there exists a significant, positive correlation
  # between m1 and m2) (one-sided test)

#----------------------------thoughts------------------------------------------

# we could use mantel's test to explore the relationship between geographic or time 
  # distances and outcomes of certain variables for certain villages/districts 
  # (in conjunction with nMDS and/or PCA and/or FA).

