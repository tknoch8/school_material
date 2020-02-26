## https://www.r-bloggers.com/partial-least-squares-regression-in-r/

## Partial Least Squares Regression ##

library(plsdepot)

data(vehicles)
head(vehicles)
variable.names(vehicles)

# put the variable price (column 13) at the end (needed by plsdepot)
cars <- vehicles[ ,c(1:12, 14:16, 13)]

# Here is the code to build the model and in this case we will examine it with 
  # 3 components (latent variables) by using comps = 3.  We use plsreg1 in this 
  # case because we are only interested in price.  If we had multiple Ys then 
  # plsreg2 would be used.
pls1 <- plsreg1(cars[, 1:15], cars[, 16, drop = FALSE], comps = 3)

pls1  # pls1 options

# R^2 for each of our components
pls1$R2

# correlations plot; notice what is highly correlated with price
plot(pls1)

# plot each observation predicted versus actual
plot(cars$price, pls1$y.pred, type = "n", xlab = "Original", ylab = "Predicted")
title("Comparison of responses", cex.main = 0.9)
abline(a = 0, b = 1, col = "gray85", lwd = 2)
text(cars$price, pls1$y.pred, col = "#5592e3")