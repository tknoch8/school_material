#Ch5 Factorial designs

# Carbon monoxide example in Lawson
library(daewr)
library(lsmeans)
COdata

mod1 <- lm(CO ~ Eth*Ratio, data = COdata)  # includes interactions
anova(mod1)

mod1AOV <- aov(CO ~ Eth*Ratio, data = COdata)
summary(mod1AOV)

mod2 <- lm(CO ~ Eth + Ratio, data = COdata)  # doesn't include interactions
anova(mod1)

# predicted mean values
mod1$coefficients  # not so nice
?model.tables
model.tables(mod1AOV, type = "means", se = T)

lsmeans(mod1, ~Eth|Ratio)
lsmeans(mod1, ~Eth)

# interaction plot
?interaction.plot
# code below uses "with" to associate the components of interaction.plot (x.factor, 
  #	trace.factor, response) to the R object COdata
with(COdata, (interaction.plot(Eth, Ratio, CO, type = "b")))

# reverse roles of Eth and Ratio
with(COdata, (interaction.plot(Ratio, Eth, CO, type = "b")))

# testing
lsmOut <- lsmeans(mod1, ~Eth|Ratio)
pairs(lsmOut)
lsmOut <- lsmeans(mod1, ~Ratio|Eth)
pairs(lsmOut)
library(multcompView)
cld(lsmOut)

# model adequacy
par(mfrow = c(2,2))
plot(mod1)
mod2 <- lm(CO ~Ratio*Eth, data = COdata)
plot(mod2)

mod3 <- lm(CO ~ Eth + Ratio, data = COdata)
anova(mod3)
anova(mod1)

library(agricolae)
hsdOut <- HSD.test(mod1, trt = c("Ratio", "Eth"))
hsdOut

#----------------------------thoughts------------------------------------------

# We could come up with a response variable, such as percieved opinion of 
  # current government (continuous), and come up with a few factor variables, such
  # as level of access to water, percieved level of security, or anything like that, 
  # and run a factorial experiment to explore which factor variables (as well as
  # interactions) have significant effects on the response variable.