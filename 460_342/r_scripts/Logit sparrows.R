require(tidyverse)

sparrows <- read_csv("https://raw.githubusercontent.com/chenm7/sparrow-survival/master/survival_sparrow.csv")

# attach(sparrows)

# (n1 <- table(Survive)["S"])

# (n2 <- table(Survive)["NS"])

# ?ifelse
# Group.surv <- ifelse(STATUS == "Survived", 1, 0)

# table(Group.surv)

### Ombnibus Test ###
Group.surv <- sparrows %>% 
  mutate(status = if_else(STATUS == "Survived", 1, 0)) %>% 
  select(status, everything()) %>% 
  select(-STATUS)
## intercept-only model
logitn.sparr <- glm(Group.surv$status ~ 1, family = binomial(link = "logit"), data = Group.surv)

## full model with predictors
logit.sparr <- glm(status ~ .,
                   family = binomial(link = "logit"), data = Group.surv)


anova(logitn.sparr, logit.sparr, test = "Chisq") ## compare models and test if all 5 betas = 0
                                                 ## intercept-only model is better to proceed w/
summ.logit <- summary(logit.sparr) 
summ.logit$coefficients            ## coeffs, SEs, z-values

estim.probs <- predict(logitn.sparr, type = "response") ## pi-hats
mat.probs <- cbind(data.frame(sparrows), estim.probs)
head(mat.probs)







