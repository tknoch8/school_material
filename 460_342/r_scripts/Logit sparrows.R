sparrows <- read.csv("http://www.stat.colostate.edu/~pturk/data/sparrows.csv", header = TRUE)

attach(sparrows)

(n1 <- table(Survive)["S"])

(n2 <- table(Survive)["NS"])

?ifelse
Group.surv <- ifelse(Survive == "S", 1, 0)

table(Group.surv)

### Ombnibus Test ###

## intercept-only model
logitn.sparr <- glm(Group.surv ~ 1, family = binomial(link = "logit"), data = sparrows)

## full model with predictors
logit.sparr <- glm(Group.surv ~ y1 + y2 + y3 + y4 + y5,
                   family = binomial(link = "logit"), data = sparrows)


anova(logitn.sparr, logit.sparr, test = "Chisq") ## compare models and test if all 5 betas = 0
                                                 ## intercept-only model is better to proceed w/
summ.logit <- summary(logit.sparr) 
summ.logit$coefficients            ## coeffs, SEs, z-values

estim.probs <- predict(logitn.sparr, type = "response") ## pi-hats
mat.probs <- cbind(data.frame(sparrows), estim.probs)
head(mat.probs)







