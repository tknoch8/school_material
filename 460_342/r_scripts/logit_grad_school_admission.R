## https://stats.idre.ucla.edu/r/dae/logit-regression/

library(ggplot2)
library(aod)

mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
head(mydata)
str(mydata)
summary(mydata)
sapply(mydata, sd)  # apply function over a list or vector

# interested in how predictor variables affect grad school admission
# will use logistic regression

# two-way contingency table of categorical outcome and predictors we want
  # to make sure there are not 0 cells
xtabs(~admit + rank, data = mydata)
xtabs(~admit + ., data = mydata)  # don't do with CVs with many values

# convert rank to factor and create logit model object
mydata$rank <- factor(mydata$rank)
mylogit <- glm(admit ~ gre + gpa + rank, 
               data = mydata, 
               family = "binomial")
summary(mylogit)
# For every one unit change in gre, the log odds of admission (versus non-admission) 
  # increases by 0.002.
# For a one unit increase in gpa, the log odds of being admitted to graduate school 
  # increases by 0.804.
# The indicator variables for rank have a slightly different interpretation. 
  # For example, having attended an undergraduate institution with rank of 2, 
  # versus an institution with a rank of 1, changes the log odds of admission 
  # by -0.675.

## CIs using profiled log-likelihood
confint(mylogit)

## CIs using standard errors
confint.default(mylogit)

# We can test for an overall effect of rank using the wald.test function of the 
  # aod library. The order in which the coefficients are given in the table of 
  # coefficients is the same as the order of the terms in the model. This is 
  # important because the wald.test function refers to the coefficients by their 
  # order in the model. We use the wald.test function. b supplies the coefficients, 
  # while Sigma supplies the variance covariance matrix of the error terms, finally 
  # Terms tells R which terms in the model are to be tested, in this case, terms 
  # 4, 5, and 6, are the three terms for the levels of rank.
wald.test(b = coef(mylogit), 
          Sigma = vcov(mylogit), 
          Terms = 4:6)
# The chi-squared test statistic of 20.9, with three degrees of freedom is 
  # associated with a p-value of 0.00011 indicating that the overall effect of 
  # rank is statistically significant.

# We can also test additional hypotheses about the differences in the coefficients 
  # for the different levels of rank. Below we test that the coefficient for rank=2 
  # is equal to the coefficient for rank=3. The first line of code below creates a 
  # vector l that defines the test we want to perform. In this case, we want to test 
  # the difference (subtraction) of the terms for rank=2 and rank=3 (i.e., the 4th 
  # and 5th terms in the model). To contrast these two terms, we multiply one of them 
  # by 1, and the other by -1. The other terms in the model are not involved in the 
  # test, so they are multiplied by 0. The second line of code below uses L=l to tell 
  # R that we wish to base the test on the vector l (rather than using the Terms option 
  # as we did above).
l <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), L = l)
# The chi-squared test statistic of 5.5 with 1 degree of freedom is associated with 
  # a p-value of 0.019, indicating that the difference between the coefficient for
  # rank=2 and the coefficient for rank=3 is statistically significant.

# You can also exponentiate the coefficients and interpret them as odds-ratios. 
  # R will do this computation for you. To get the exponentiated coefficients, you 
  # tell R that you want to exponentiate (exp), and that the object you want to 
  # exponentiate is called coefficients and it is part of mylogit (coef(mylogit)). 
  # We can use the same logic to get odds ratios and their confidence intervals, 
  # by exponentiating the confidence intervals from before. To put it all in one 
  # table, we use cbind to bind the coefficients and confidence intervals column-wise.
# odds ratios only
exp(coef(mylogit))
# odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# Now we can say that for a one unit increase in gpa, the odds of being admitted to 
  # graduate school (versus not being admitted) increase by a factor of 2.23. For 
  # more information on interpreting odds ratios see our FAQ page How do I interpret 
  # odds ratios in logistic regression? . Note that while R produces it, the odds 
  # ratio for the intercept is not generally interpreted.

######### continue ##########

# You can also use predicted probabilities to help you understand the model. 
  # Predicted probabilities can be computed for both categorical and continuous 
  # predictor variables. In order to create predicted probabilities we first 
  # need to create a new data frame with the values we want the independent 
  # variables to take on to create our predictions.
# We will start by calculating the predicted probability of admission at each value 
  # of rank, holding gre and gpa at their means. First we create and view the data 
  # frame.
newdata1 <- with(mydata, 
                 data.frame(gre = mean(gre), 
                            gpa = mean(gpa), 
                            rank = factor(1:4)))
newdata1
# These objects must have the same names as the variables in your logistic 
  # regression above (e.g. in this example the mean for gre must be named gre). 
  # Now that we have the data frame we want to use to calculate the predicted 
  # probabilities, we can tell R to create the predicted probabilities. The first 
  # line of code below is quite compact, we will break it apart to discuss what 
  # various components do. The newdata1$rankP tells R that we want to create a new 
  # variable in the dataset (data frame) newdata1 called rankP, the rest of the 
  # command tells R that the values of rankP should be predictions made using the 
  # predict( ) function. The options within the parentheses tell R that the 
  # predictions should be based on the analysis mylogit with values of the predictor 
  # variables coming from newdata1 and that the type of prediction is a predicted 
  # probability (type="response"). The second line of the code lists the values in 
  # the data frame newdata1. Although not particularly pretty, this is a table of 
  # predicted probabilities.
newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
newdata1

# In the above output we see that the predicted probability of being accepted into 
  # a graduate program is 0.52 for students from the highest prestige undergraduate 
  # institutions (rank=1), and 0.18 for students from the lowest ranked institutions 
  # (rank=4), holding gre and gpa at their means. We can do something very similar 
  # to create a table of predicted probabilities varying the value of gre and rank. 
  # We are going to plot these, so we will create 100 values of gre between 200 and 
  # 800, at each value of rank (i.e., 1, 2, 3, and 4).
newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, 
                                                  length.out = 100),
                                              4), gpa = mean(gpa), 
                                    rank = factor(rep(1:4, each = 100))))
# The code to generate the predicted probabilities (the first line below) is the 
  # same as before, except we are also going to ask for standard errors so we can 
  # plot a confidence interval. We get the estimates on the link scale and back 
  # transform both the predicted values and confidence limits into probabilities.
newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
                                    se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
head(newdata3)

# It can also be helpful to use graphs of predicted probabilities to understand 
  # and/or present the model. We will use the ggplot2 package for graphing. Below 
  # we make a plot with the predicted probabilities, and 95% confidence intervals.
ggplot(newdata3, 
       aes(x = gre, 
           y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL,
                  ymax = UL, 
                  fill = rank), 
              alpha = 0.2) + 
  geom_line(aes(colour = rank), 
            size = 1)

# We may also wish to see measures of how well our model fits. This can be 
  # particularly useful when comparing competing models. The output produced by 
  # summary(mylogit) included indices of fit (shown below the coefficients), 
  # including the null and deviance residuals and the AIC. One measure of model 
  # fit is the significance of the overall model. This test asks whether the model 
  # with predictors fits significantly better than a model with just an intercept 
  # (i.e., a null model). The test statistic is the difference between the residual 
  # deviance for the model with predictors and the null model. The test statistic is 
  # distributed chi-squared with degrees of freedom equal to the differences in degrees 
  # of freedom between the current and the null model (i.e., the number of predictor 
  # variables in the model). To find the difference in deviance for the two models 
  # (i.e., the test statistic) we can use the command:
with(mylogit, null.deviance - deviance)

# The degrees of freedom for the difference between the two models is equal to the 
  # number of predictor variables in the mode, and can be obtained using:
with(mylogit, df.null - df.residual)

# Finally, the p-value can be obtained using:
with(mylogit, 
     pchisq(null.deviance - deviance, 
            df.null - df.residual, 
            lower.tail = FALSE))

# The chi-square of 41.46 with 5 degrees of freedom and an associated p-value of 
  # less than 0.001 tells us that our model as a whole fits significantly better 
  # than an empty model. This is sometimes called a likelihood ratio test (the 
  # deviance residual is -2*log likelihood). To see the model’s log likelihood, 
  # we type:
logLik(mylogit)

# log likelihood explanation:
  # https://stats.stackexchange.com/questions/112451/maximum-likelihood-estimation-
  # mle-in-layman-terms





