# Libraries
library(reshape2)
library(estimatr)
library(plyr)
library(foreign)
library(ltm)
library(MCMCpack)
library(wfe)
library(devtools)
library(panelView)
library(lme4)
library(arm)
library(Matching)
library(rgenoud)
library(cem)
library(quickmatch)
library(zoo)
library(dplyr)
library(did)
library(gsynth)
library(DataCombine)
library(broom)
library(stargazer)
library(parallel)
library(margins)
library(ggplot2)


# Set working directory 
setwd('C:/Users/lunag/Desktop/Bureaublad/Replication')

# The state FIPS codes 
fips_codes <- read.csv("fips_codes_website.csv") # (state codes are numeric and two-letter alphabetic codes)


data <- readRDS("policy_data_updated.RDS") # (All the data)

# Load New Early Voting Data
ev <- read.csv("Early Voting Coding - Sheet1.csv")

# Join the data-set early voting data to data 
data <- join(data, ev[,c("state","year_eip","year_amv","year_abs")])

# For every state year 2014 = 1 
for(i in levels(factor(data$state))){
  
  data$early_voting_person[data$state==i & 
                             (data$year>=data$year_eip) & 
                             data$year<=2014] <- 1
  
  data$early_voting_narrow[data$state==i & 
                             (data$year>=data$year_eip|data$year>=data$year_amv) & 
                             data$year<=2014] <- 1
  data$early_voting_broad[data$state==i & 
                            (data$year>=data$year_eip|data$year>=data$year_amv|data$year>=data$year_abs) & 
                            data$year<=2014] <- 1
  
}

# All the NA value are equal to zero
data$early_voting_narrow[is.na(data$early_voting_narrow) & data$year<=2014] <- 0
data$early_voting_broad[is.na(data$early_voting_broad) & data$year<=2014] <- 0
data$early_voting_person[is.na(data$early_voting_person) & data$year<=2014] <- 0

# Combine the presidential years 
presidential_years <- c(1976,1980,1984,1988,1992,1996,2000,2004,2008,2012,2016)

# Add the Fowler data
fowler <- read.dta("fowler_replication_data.dta", convert.factors = F)

# Dummy variables 
fowler$age_group[fowler$age<25] <- "18-24"
fowler$age_group[fowler$age>=25 & fowler$age<35] <- "25-34"
fowler$age_group[fowler$age>=35 & fowler$age<45] <- "35-44"
fowler$age_group[fowler$age>=45 & fowler$age<55] <- "45-54"
fowler$age_group[fowler$age>=55 & fowler$age<65] <- "55-64"
fowler$age_group[fowler$age>=65] <- "65+"

# Change columns
fowler$year <- fowler$election
fowler$st <- fowler$state

# Pre-process data  
fowler_agg <- aggregate(cbind(voted, pop) ~ age_group + st + year, fowler, mean, na.rm=T)

fowler_sdr <- join(fowler_agg, data, by=c("st","year"))

fowler_sdr$turnout <- (fowler_sdr$voted/fowler_sdr$pop)*100

fowler_sdr <- cbind(fowler_sdr, data.frame(model.matrix(~ age_group - 1, data=fowler_sdr)))

fowler_sdr$age_group_ref <- relevel(factor(fowler_sdr$age_group), ref = 6)

# Linear regression 
lm1 <- lm(turnout ~ sdr*age_group_ref + factor(st) + factor(year), fowler_sdr)
summary(lm1)

# outcome variable:
# Turnout = the percentage of eligible voters who participated in an election.

# independent variable:
# sdr = same day registration 
# age_group_ref = The six age groups per state 
# st = The state 
# year = The election year

# look up interpretation lm interaction 
# (being part of this age group increases turn out percentage by)
# different to the original 
# five different datasets too complex
# critique and limitations


m <- margins(lm1, at = list(age_group_ref=levels(fowler_sdr$age_group_ref)))

# Load and clean cps data 
cps <- read.csv("cps_00021.csv"); gc()

names(cps) <- tolower((names(cps)))

#Removing "refused to answer"
cps$voted_alt <- cps$voted - 1
cps$voted_alt[cps$voted_alt>1] <- NA

#Main dependent variable
cps$voted[cps$voted==99] <- NA
cps$voted <- cps$voted - 1
cps$voted[cps$voted>1] <- 0

cps$faminc[cps$faminc>843] <- NA
cps$faminc <- as.numeric(factor(cps$faminc))

cps$sex[cps$sex>2] <- NA

cps$educ[cps$educ==999|cps$educ==1] <- NA

cps$educ <- as.numeric(factor(cps$educ))

#merge state FIPS codes
cps <- join(cps, fips_codes[,c("st","statefip")], match="first")

#Merge state policy data
cps <- join(cps, data, by=c("st","year")); gc()

cps$age_group[cps$age<=24] <- "18-24"
cps$age_group[cps$age>24 & cps$age<35] <- "25-34"
cps$age_group[cps$age>34 & cps$age<45] <- "35-44"
cps$age_group[cps$age>44 & cps$age<55] <- "45-54"
cps$age_group[cps$age>54 & cps$age<65] <- "55-64"
cps$age_group[cps$age>64] <- "65+"

cps$presidential_year[cps$year %in% presidential_years] <- 1
cps$presidential_year[(cps$year %in% presidential_years)==F] <- 0

cps$sex[cps$sex==0] <- NA
cps$sex <- cps$sex - 1

cps$race_5[cps$race==100] <- "White"
cps$race_5[cps$race==200] <- "Black"
cps$race_5[cps$race==300 | cps$race>=700] <- "Other"
cps$race_5[(cps$race>=600 & cps$race<700) | cps$race==809] <- "Asian"
cps <- within(cps, race_5 <- relevel(factor(race_5), ref = "White"))
cps <- cps[cps$year>=1978,]

cps$asian[cps$race_5=="Asian"] <- 1
cps$asian[cps$race_5!="Asian"] <- 0

#age_group dummy variables
cps <- cbind(cps, data.frame(model.matrix(~ age_group - 1, data=cps)))

#race dummy variables
cps <- cbind(cps, data.frame(model.matrix(~ as.character(race) - 1, data=cps)))
names(cps) <- gsub("as.character.", "", names(cps))

#registration
cps$reg_pollingplace[cps$voreghow<99 & cps$voreghow!=7] <- 0
cps$reg_pollingplace[cps$voreghow<99 & cps$voreghow==7] <- 1

cps$vote_reg[cps$voreg<=98 & cps$voreg!=2] <- 0
cps$vote_reg[cps$voreg<=98 & cps$voreg==2] <- 1

cps$movedless1yr[cps$voteres<14] <- 1
cps$movedless1yr[cps$voteres>=14 & cps$voteres<900] <- 0

#Remove DC
cps <- cps[cps$st!="DC",]; gc()

# Figures:
agg_cps_age_sdr <- aggregate(voted ~ sdr + age_group, cps, mean, na.rm=T)
write.csv(agg_cps_age_sdr, "agg_cps_age_sdr.csv", na="", row.names = F)

pd <- position_dodge(.75)

pdf("bivariate_plot_age.pdf", h=4, w=5)
ggplot(agg_cps_age_sdr, aes(y=voted, x=age_group, fill=factor(sdr)))+
  geom_bar(width = .75, stat = "identity", position=pd, color="black") + 
  scale_fill_manual(values=c("grey","black"), labels=c("non-SDR", "SDR"), name="") +
  ylab("Pr(Voted)") +
  xlab("Age") +
  theme_classic()
dev.off()

# Presidential Bivariat 
agg_cps_age_presidential_sdr <- aggregate(voted ~ sdr + age_group + presidential_year, cps, mean, na.rm=T)

agg_cps_age_presidential_sdr$prez[agg_cps_age_presidential_sdr$presidential_year==1] <- "Presidential"
agg_cps_age_presidential_sdr$prez[agg_cps_age_presidential_sdr$presidential_year==0] <- "Non-Presidential"

pdf("bivariate_plot_age_POTUS.pdf", h=4, w=8)
ggplot(agg_cps_age_presidential_sdr, aes(y=voted, x=age_group, fill=factor(sdr)))+
  geom_bar(width = .75, stat = "identity", position=pd, color="black") + 
  scale_fill_manual(values=c("grey","black"), labels=c("non-SDR", "SDR"), name="") +
  ylab("Pr(Voted)") +
  xlab("Age") +
  facet_wrap(~prez) +
  theme_classic()
dev.off()

# Average Turnout by SDR and Age
 m
setwd("C:/Users/lunag/Desktop/Bureaublad")

pdf("fowler_did_bv.pdf", h=4, w=4)
ggplot(plotdata, aes(x=age_group, y=AME)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0) +
  geom_hline(yintercept=0, linetype=2) +
  xlab("Age Group") +
  ylab("Estimate") +
  scale_y_continuous(breaks=c(-5, 0, 5), limits=c(-5,7.5)) +
  theme_bw()
dev.off()

# Main analysis 
# Pre-processing
agg_cps <- aggregate(voted ~ state + year + sdr + early_voting_narrow + early_voting_broad + presidential_year, cps, mean, na.rm=T)
agg_cps_age <- aggregate(voted ~ state + year + sdr + presidential_year + age_group + 
                           povrate + pct_black_i + pct_white_i, cps, mean, na.rm=T)

cps$white[cps$race.100==1] <- 1
cps$white[cps$race.100!=1] <- 0
cps$black[cps$race.200==1] <- 1
cps$black[cps$race.200!=1] <- 0

cps$asian[cps$race>200 & cps$race<800] <- 1
cps$asian[cps$race<=200 | cps$race>=800] <- 0

race_means_agg_cps_age <- aggregate(cbind(white, black, asian, faminc, educ) ~ state + year + age_group, cps, mean, na.rm=T)

agg_cps_age <- join(agg_cps_age, race_means_agg_cps_age)


agg_cps_age <- agg_cps_age[order(agg_cps_age$state, agg_cps_age$age_group, agg_cps_age$year),]

#SDR
# independent variable:
# voted = whether they voted or not 

# dependent variable:
# sdr = same day registration 
# age_group = for all the 6 age groups 
# race = Race 
# sex = sex
# faminc = Family income of householder
# educ  = Educational attainment record
# year = year
# state = state code 

# Individual SDR 
indiv_sdr_bv <- lm(voted ~ sdr*age_group18.24 + 
                            sdr*age_group25.34 + 
                            sdr*age_group35.44 + 
                            sdr*age_group45.54 + 
                            sdr*age_group55.64 + 
                            factor(year) + factor(state), cps)
#Early Voting (Narrow)
indiv_ev <- lm_robust(voted ~ early_voting_narrow*age_group18.24 + 
                        early_voting_narrow*age_group25.34 + 
                        early_voting_narrow*age_group35.44 + 
                        early_voting_narrow*age_group45.54 + 
                        early_voting_narrow*age_group55.64 + 
                        factor(race) + sex + faminc + educ + factor(year) + factor(state), 
                      clusters=state, se_type="stata",
                      cps)

#OLS
ols_1 <- lm(voted ~ sdr*age_group18.24 + 
              sdr*age_group25.34 + 
              sdr*age_group35.44 + 
              sdr*age_group45.54 + 
              sdr*age_group55.64 + 
              presidential_year + factor(year) + factor(state), 
            cps[cps$presidential_year==1,])


ols_2 <- lm(voted ~ sdr*age_group18.24 + 
              sdr*age_group25.34 + 
              sdr*age_group35.44 + 
              sdr*age_group45.54 + 
              sdr*age_group55.64 + 
              factor(race) + sex + faminc + educ + presidential_year + factor(year) + factor(state), 
            cps[cps$presidential_year==1,])

ols_3 <- lm(voted ~ sdr*age_group18.24 + 
              sdr*age_group25.34 + 
              sdr*age_group35.44 + 
              sdr*age_group45.54 + 
              sdr*age_group55.64 + 
              presidential_year + factor(year) + factor(state), 
            cps[cps$presidential_year==0,])


ols_4 <- lm(voted ~ sdr*age_group18.24 + 
              sdr*age_group25.34 + 
              sdr*age_group35.44 + 
              sdr*age_group45.54 + 
              sdr*age_group55.64 + 
              factor(race) + sex + faminc + educ + presidential_year + factor(year) + factor(state), 
            cps[cps$presidential_year==0,])

