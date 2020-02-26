### 341 hw 13

## (1)

library(readr)
library(here)

data_table_B14 <- read_csv(here("data-table-B14.csv"))
invDat <- data_table_B14
invDat

# (a) is there collinearity?

fullModel <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = invDat)
X <- model.matrix(fullModel)
as.dist(cor(X[,-1])) # pairwise relationships of X (as.dist() to hide duplicates)
diag(solve(cor(X[,-1]))) # VIFs on diagonal (for each x)

# (b) use AIC to choose "best" covariates
y <- invDat$y
?AIC
AIC(fullModel)
?step
step(lm(y ~ 1,data = invDat), list(upper = fullModel),
     direction = "forward") 
# forward direction starts with only intercept term
stepModelAIC <- lm(y ~ x1 + x2 + x3 + x4, data = invDat)
summary(stepModelAIC)

# (c) step with BIC..... k = log(n) for BIC

n <- length(y)

step(lm(y ~ 1, data = invDat), list(upper = fullModel),
     direction = "forward", k = log(n))
stepModelBIC <- lm(y ~ x1 + x2 + x3 + x4, data=invDat)
summary(stepModelBIC)

# (d) compare two best AIC models with AICc
# two best AIC models had AIC statistics of 47.49 and 42.55

library(AICcmodavg)
AICc(lm(y ~ x2 + x3 + x4, data=invDat))
AICc(lm(y ~ x1 + x2 + x3 + x4, data=invDat))

summary(lm(y ~ x1 + x2 + x3 + x4, data=invDat))
summary(lm(y ~ x2 + x3 + x4, data=invDat))


# (e) compare both models with PRESS statistic, lower is better

model1 <- lm(y ~ x1 + x2 + x3 + x4, data=invDat)
modelred <- lm(y ~ x2 + x3 + x4, data=invDat)
influenceOut1 <- influence(model1)
influenceOutred <- influence(modelred)

SSres <- sum(model1$residuals^2)

PRESSresids1 <- model1$residuals/(1-influenceOut1$hat)
PRESSstat1 <- sum(PRESSresids1^2)

PRESSresidsred <- modelred$residuals/(1-influenceOutred$hat)
PRESSstatred <- sum(PRESSresidsred^2)

PRESSstat1
PRESSstatred


