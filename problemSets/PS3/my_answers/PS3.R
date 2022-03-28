################
#Problem set 3 #
################
#options(scipen = 999)
setwd('C:/Users/lunag/Desktop/Bureaublad/Problemset 3')

library(tidyverse)
library(ggplot2)
library(nnet)
library(dplyr)
library(MASS)
library(stargazer)

##############
# Question 1 #
##############

data <- read.csv('gdpChange.csv')
data

# The outcome of interest : GDPWdiff (possible categories include: 'positive', 'negative' and 'no change')
# Main predictor of interest : Reg, OIL

# PART 1: Construct and interpret an unordered multinomial logit with GDPWdiff as the output and 'no change' as the reference category, including the estimated cutoff points and coefficients.

# data wrangling
data$GDPWdiff <- replace(data$GDPWdiff, data$GDPWdiff < 0, '-A')
data$GDPWdiff <- replace(data$GDPWdiff, data$GDPWdiff > 0, 'B')

data$GDPWdiff <- factor(data$GDPWdiff, 
                               levels = c( "-A" , 0, "B"),
                               labels = c("negative",
                                          "no change",
                                          "positive"))
# The unordered multinomial logit:
data$GDPWdiff <- relevel(data$GDPWdiff, ref= "no change")
mult.log <- multinom(GDPWdiff ~ REG + OIL, data = data)
summary(mult.log)
stargazer(mult.log)

# The p-values 
z <- summary(mult.log)$coefficients/summary(mult.log)$standard.errors
(p <- (1 - pnorm(abs(z), 0, 1)) * 2) 
stargazer(p)

# The coefficients 
exp(coef(mult.log))

# Confidence intervals
con <- (ci <- confint(mult.log))
stargazer(p)
 
# PART 2: Construct and interpret an ordered multinomial logit with GDPWdiff as the outcome variable, including the estimated cutoff points and coefficients. 
data <- read.csv('gdpChange.csv')
data

# data wrangling
data$GDPWdiff <- replace(data$GDPWdiff, data$GDPWdiff < 0, '-A')
data$GDPWdiff <- replace(data$GDPWdiff, data$GDPWdiff > 0, 'B')

data$GDPWdiff <- factor(data$GDPWdiff, 
                        levels = c( "-A" , 0, "B"),
                        labels = c("negative",
                                   "no change",
                                   "positive"))

ord.log <- polr(GDPWdiff ~ REG + OIL, data = data, Hess = TRUE)
summary(ord.log)
stargazer(ord.log)


# Calculate a p value
ctable <- coef(summary(ord.log))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

# Confidence intervals
(ci <- confint(ord.log))

# convert to odds ratio
exp(cbind(OR = coef(ord.log), ci))

##############
# Question 2 #
##############

dataset <- read.csv ('MexicoMuniData.csv')
summary(dataset)

# The outcome of interest : PAN.visits.06
# Main predictor of interest : competitive.district, marginality.06, PAN.governor.06

# Run a Poisson regression
# Is there evidence that PAN presidential candidates visit swing districts more?
# Provide test statistic and p-value

Pos_reg <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data = dataset, family = poisson)
summary(Pos_reg)
stargazer(Pos_reg)

# Provide the estimated mean number of visits from the winning PAN presidential candidate for a hypothetical district that was competitive (competitive.district = 1), had an average poverty level (marginality.06 = 0), and a PAN governor (PAN.governor.06 = 1).
coeff <- coef(Pos_reg)
coeff
visits <- exp(coeff[1] + coeff[2]*1 + coeff[3]*0 + coeff[4]*1)
visits




