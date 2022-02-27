#####################
# load libraries
library(ggplot2)
# set wd
# clear global .envir
#####################
options(scipen=999)

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2022/blob/main/datasets/climateSupport.RData?raw=true"))

#############
#QUESTION 1
#############
head(climateSupport)
summary(climateSupport)

# Predicting the likelihood of an individual supporting a policy based on the number of countries participating and the possible sanctions for non-compliance. 
# Fit an additive model. 
# Provide the summary output, the global null hypothesis, and p-value. 
# Please describe the results and provide a conclusion. 

climateSupport$choice <- as.numeric(as.factor(climateSupport$choice))-1 # 1 if the individual agreed with the policy;
# 0 if the individual did not support the policy. 
climateSupport$countries <- as.numeric(as.factor(climateSupport$countries))-1 #  0 is 20 for 192, 1 is 80 of 192 and 2 is 160 of 192.
climateSupport$sanctions <- as.numeric(as.factor(climateSupport$sanctions))-1 # 1 is 5%, 2 is 15% and 3 is 20%.

reg <- glm(choice ~ ., 
           data = climateSupport, 
           family = "binomial")
summary(reg) # The . means the same as adding all the explanatory variables. 

# The summary output, the global null hypothesis and p-value: 
# Please describe the results and provide a conclusion: 

# Coefficients:
#   Estimate Std. Error z value             Pr(>|z|)    
# (Intercept) -0.14458    0.04518  -3.200              0.00137 ** 
#  countries    0.32436    0.02689  12.062 < 0.0000000000000002 ***
#   sanctions   -0.12353    0.01964  -6.291       0.000000000315 ***

#############
#QUESTION 2A
#############
# If any of the explanatory variables are significant in this model, then:
# For the policy in which nearly all countries participate [160 of 192], 
# how does increasing sanctions from 5% to 15% change the odds that an individual will support the policy?

# Estimated regression line: y = x + bx1 + bx2 + error 
a_5 <- exp((-0.144558) + (0.32436)*2 + (-0.12353)*1)

a_15 <- exp((-0.144558) + (0.32436)*2 + (-0.12353)*2)

a_diff <- a_5 - a_15
a_diff # 0,17 increase in odds when going from 5 to 15 (without exponential it is log odds, with exponenetial it is only odds).
# (more likely to support climate policy).

#############
#QUESTION 2B
#############
# If any of the explanatory variables are significant in this model, then:
# For the policy in which nearly all countries participate [20 of 192], 
# how does increasing sanctions from 5% to 15% change the odds that an individual will support the policy?

# y = x + bx1 + bx2 + error 
b_5 <- exp((-0.144558) + (0.32436)*0 + (-0.12353)*1)
b_5
b_15 <- exp((-0.144558) + (0.32436)*0 + (-0.12353)*2)
b_15
b_diff <- b_5 - b_15
b_diff # 0.0888 increase in odds when going from 5 to 15 (more likely to support climate policy).

#############
#QUESTION 2C
#############
# If any of the explanatory variables are significant in this model, then:
# What is the estimated probability that an individual will support a policy if there are 80 of 192 countries participating with no sanctions?
c_0 <- exp((-0.144558) + (0.32436)*1 + (-0.12353)*0)/(1+(exp(-0.144558) + (0.32436)*1 + (-0.12353)*0))
c_0

#############
#QUESTION 2D
#############
# If any of the explanatory variables are significant in this model, then:
# Would the answers to 2a and 2b potentially change if we included the interaction term in this model? Why?
# Perform a test to see if including an interaction is appropriate. 

interaction <- glm(choice ~ . + countries:sanctions,
           data = climateSupport, 
           family = "binomial")
summary(interaction)



