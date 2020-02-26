### L22b CCA butterflies ###

library(tidyverse)
library(here)

(bfly <- read.csv(here("data_raw", "bfly.csv")))

(bfly <- mutate(bfly, PFPGi0.4and6 = PFPGi0.40 + PFPGi0.60))

(bfly <- select(bfly, - PFPGi1.30, - PFPGi0.40, - PFPGi0.60))

(bfly <- bfly %>% column_to_rownames(var = "Colony"))

(bfly <- bfly[,c(1:4,8,5:7)])  ## Sensible reordering

(bfly <- scale(bfly))  ## All 4 x_ks and 4 y_js scaled to mean 0, sd 1

(cor.mat <- round(cor(bfly), 3))

(A <- cor.mat[1:4,1:4]) ## Cor matrix of x_ks (env. variables)

(B <- cor.mat[5:8,5:8]) ## Cor matrix of y_js (alleles)

(C <- cor.mat[1:4,5:8]) ## Cross-cor matrix of x_ks and y_js

(magic.matrix <- solve(B) %*% t(C) %*% solve(A) %*% C)

eigen(magic.matrix)
## The eigenvectors bi from above, for i = 1, 2, 3, 4, 
## give the coefficients of the y_js for the canonical variables V_i

(Coef.U <- solve(A) %*% C %*% eigen(magic.matrix)$vectors)
## a_is give coefficients of x_ks for canonical variables U_i

(V <- round(bfly[,5:8] %*% eigen(magic.matrix)$vectors, 3))
## V scores for each colony for all canonical variable V. i = 1,2,3,4

(U <- round(bfly[,1:4] %*% Coef.U, 3))
## U scores for each colony for all canonical variable U. i = 1,2,3,4

round(diag(cor(U, V)), 3) 
round(sqrt(eigen(magic.matrix)$values), 3) ## same thing

# canonical loadings for U_1 (how important each x_j is in determining U_1)
round(cor(U[,1], bfly[,1:4]), 2)

# canonical loadings for V_1 (how important each y_j is in determining V_1)
round(cor(V[,1], bfly[,5:8]), 2)

# canonical crossloadings ("how each individual y_j is related to all x_js")
round(cor(U[,1], bfly[,5:8]), 2)

# canonical crossloadings ("how each individual x_j is related to all y_js")
round(cor(V[,1], bfly[,1:4]), 2)

# correlation between both canonical variables
round(cor(V[,1], U[,1]), 2)

## canonical crossloadings "provide a more pure measure of the dependent and
  # independent variable relationship"

bfly[c("GL", "UO"),]

library(ggrepel)

p1 <- ggplot(data.frame(U1 = U[,1], V1 = V[,1]), aes(x = U1, y = V1))
p1 <- p1 + geom_point(color = "red")
p1 <- p1 + geom_text_repel(aes(label = row.names(bfly)))
p1 <- p1 + geom_hline(yintercept = 0, linetype = "dotted",
                      color = "blue", size = 0.75)
p1 <- p1 + geom_vline(xintercept = 0, linetype = "dotted",
                      color = "blue", size = 0.75)
p1


## additional notes:

# suggested minimum sample size: 10 * number of variables