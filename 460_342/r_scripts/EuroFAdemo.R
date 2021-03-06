### Factor Analysis on European Employment Composition ###

library(here)
library(tidyverse)
library(GGally)
library(ggrepel)

(euroemp <- read.csv(here("data_sets", "Euroemp.csv")))

# AGR (y1) = agriculture,forestry, and fishing
# MIN (y2) = mining and quarrying
# MAN (y3) = manufacturing
# PS  (y4) = power and water supplies
# CON (y5) = construction
# SER (y6) = services
# FIN (y7) = finance
# SPS (y8) = social and personal services
# TC  (y9) = transport and communications

as.dist(round(cor(euroemp[,3:11]), 3)) ## as.dist to hide duplicate values

(std.data <- scale(euroemp[,3:11]))

(PCA <- prcomp(std.data)) ## perform PCA

## PCA$rotation is variable coefficients for PCs

summary(PCA)

## PCA is a dimension reduction technique, used when variables are
# correlated

## Principal components account for most of the variance of observed variables

(fac.loads <- round(cor(std.data, PCA$x), 2)) ## "importance" of each variable in
# determining PCs

plot(PCA) ## keep two (first two account for ~55% of variability)

## (next) obtain first two eigenvectors to get linear combinations of Y's
# that go into first two PCAs

summary(PCA)
(round(PCA$rotation[,1:2], 2))

## PC1 = 0.51AGR + 0.37MIN + -0.25MAN + -0.32PS + -0.22CON + -0.38SER
#   + -0.13FIN + -0.43SPS + -0.21TC

## (next) visualization of countries in terms of PC1 and PC2

p1 <- ggplot(data.frame(PCA$x), aes(x = PC1, y = PC2))
p1 <- p1 + geom_point(color = "red")
p1 <- p1 + geom_text_repel(aes(label = euroemp[,1], color = euroemp[,2]))
p1 + theme(legend.title = element_blank(), legend.position = "top")
p1

## Gibraltar: low PC1, high PC2

### Now transition to Factor Analysis ###

(a_rm <- round(PCA$rotation %*% diag(PCA$sdev)[,1:4], 2)) ## unrotated factor loadings

round(diag(a_rm %*% t(a_rm)), 2) ## proportions of variability attributable
# to common factors, across observations (communalities)

## lots of overlap in factors across variables
# 6/9 variables are related to two or more common factors

## must rotate factors using Varimax rotation, maximizing
# the sum of these variances (factor loadings)^2 for all of the factors to
# break overlap.

# "verimax rotation"

(rot <- varimax(a_rm, eps = 1e-12)$rotmat)

round(t(rot)%*%rot) ## Demonstrate orthogonality for orthogonal rotation
round(rot%*%t(rot))

(g_rm <- round(a_rm %*% rot, 2)) ## New (rotated) factor loadings

a_rm
g_rm ## eliminated most of the overlap by orthogonal rotation

## (reminder): number of factors kept is number of 
# sds (sqrt(eigenvalues)) > 1 in PCs (4 inthis case)
PCA

round(diag(g_rm %*% t(g_rm)), 2)
round(diag(a_rm %*% t(a_rm)), 2) ## communalities are unchanged* by rotation

(Fstar <- round(std.data %*% g_rm %*% solve(t(g_rm) %*% g_rm), 2))

summary(PCA)

colnames(Fstar) <- c("factor 1","factor 2","factor 3","factor 4")

countryloads <- cbind(euroemp[,1:2], Fstar)
head(countryloads)

# AGR (y1) = agriculture,forestry, and fishing
# MIN (y2) = mining and quarrying
# MAN (y3) = manufacturing
# PS  (y4) = power and water supplies
# CON (y5) = construction
# SER (y6) = services
# FIN (y7) = finance
# SPS (y8) = social and personal services
# TC  (y9) = transport and communications

g_rm
subset(countryloads, Country == "Albania"); subset(euroemp, Country == "Albania")

## Factor 1 is largely determined by presence of AGR and absence of SPS. 
# Albania has a high percentage of labor employed in AGR and zero labor
# employed in SPS. Makes sense that Albania has a high Factor 1 score.

countryloads

g_rm
subset(countryloads, Country == "USSRF"); subset(euroemp, Country == "USSRF")
## low Factor 2 score

g_rm
subset(countryloads, Country == "Czech/Slovakia"); subset(euroemp, Country == "Czech/Slovakia")
## low Factor 3 score

g_rm
subset(countryloads, Country == "Romania"); subset(euroemp, Country == "Romania")
## low Factor 2 score (same as USSRF in terms of FIN)

g_rm
subset(countryloads, Country == "Gibraltar"); subset(euroemp, Country == "Gibraltar")
## low Factor 4 score

g_rm
subset(countryloads, Country == "Netherlands"); subset(euroemp, Country == "Netherlands")
## high factor 4 score (opposite of Gibraltar)

subset(countryloads, Country == "Sweden"); subset(euroemp, Country == "Sweden")
## sps doesn't strongly determine any particular factor