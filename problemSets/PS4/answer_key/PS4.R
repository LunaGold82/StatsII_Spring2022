# Set directory:
setwd('C:/Users/lunag/Desktop/Bureaublad/Problemset 4')

# remove objects
rm(list=ls())

# Libraries
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
library(eha)
library(stargazer)


# Modeling the historical causes of infant mortality
# Data: 5641 first-born in seven Swedish parishes 
# Fit a Cox Proportional Hazard Model 
# Covariates: mother's age and infant's gender 
# Present and interpret the output

data(infants)
View(infants)

library("survival")
library("magrittr")
library(ggfortify) 


dataset <- with(infants, Surv(enter, exit, event))

# Plotting the overall survival 
km <- survfit(dataset ~ 1, data = infants)

summary(km, times = seq(0, 15, 1))
plot(km, main = "Kaplan-Meier Plot", xlab = "days", ylim = c(0.4, 1))
autoplot(km, main = "Overall survival")


# Plotting the survival and comparing both sexes 
km_sex <- survfit(dataset ~ sex, data = infants)
autoplot(km_sex, main = "Infant gender survival")

# Run a Cox Proportional Hazard Regression
cox <- coxph(dataset ~ sex + age, data = infants)
summary(cox)
drop1(cox, test = "Chisq")
stargazer(cox)

# exponentiate parameter estimates to obtain hazard ratios
exp(-0.083546)
