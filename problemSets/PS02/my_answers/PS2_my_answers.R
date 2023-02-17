#####################
# load libraries
# set wd
# clear global .envir
#####################

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
load(url("https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true"))

##############
# Question 1 #
##############

## First, I reassigned the countries and sanctions variables as unordered factors,
## because having them as ordered factors was messing up my lm output:
climateSupport$countries <- factor(climateSupport$countries, ordered=FALSE)
climateSupport$sanctions <- factor(climateSupport$sanctions, ordered=FALSE)

## Fit an additive model:
mod1 <- glm(choice ~ ., family =binomial(link="logit"), data = climateSupport)

## Provide Summary Output:
summary(mod1)

## Global Null Hypothesis: (H0: all slopes (/estimated relationships in our model) = 0)
nullmod1 <- glm(choice ~ 1, family = binomial(link="logit"), data = climateSupport)

summary(nullmod1)

### Going to run an ANOVA test to assess this global null hypothesis:

anova1 <- anova(nullmod1, mod1, test = "LRT")

list(anova1)

## P Value: P value is less than 0.01 (is <2.23-16, so very close to 0); 
## This means that we can conclude that at least one predictor is reliable in our 
## mod1 model (our additive model), i.e. that our additive model is at least
## better fit than our null model with no predictors included.

## Describe Results & Conclusion:
## The results of the anova (analysis of deviance between the two models) test shows
## a very small p-value, which is grounds to reject our 
## null hypothesis (that all slopes are 0, or that there are no relationships 
## between the predictor and outcome variables in mod1). 

##############
# Question 2 # If any of the explanatory (predictor) variables in mod1 are significant, then:
##############

## (a) ## For the [160 of 192] policy, how does increasing sanctions from 5-15% 
## change the odds that an individual will support the policy? (interpret a coefficient)
# To do this, need to use the Odds Ratio (OR) -- for two diff groups (5% 
# sanctions and 15% sanctions) holding countries variable constant at 160-192.

# to interpret, look at mod1 summary:
summary(mod1)

# Now going to plug in the summary estimates to find predicted probability 
# that Y=support when countries = 160-192 and sanctions = 5%

PProb_sanctions5 <- (1/(1+exp(-(-0.27266+.64835+.19186))))
# Probability when sanctions = 5 ==> 0.638

# Next, find probability that Y=support when countries = 160-192 and 
# sanctions = 15%
PProb_sanctions15 <- (1/1+exp(-(-0.27266+.64835-.13325)))
# Probability when sanctions = 15 ==> 1.785

# Then calc the odds ratio of moving from 5 -> 15 percent sanctions w 
# country support held constant at 160-192

OR_5to15 <- PProb_sanctions15/PProb_sanctions5
# = 2.796

# Interpretation: the odds of a bill being supported when sanctions move from
# 5 to 15%, with a policy supported by 160-192 countries, increases
# by 2.796%.

## (b) ## What is the est. probability that an individual will support a policy 
## if there are 80 of 192 countries participating with no sanctions?
# Plug in the estimates:

PProb_b <- (1/1+exp(-(-.27266+.33636+0)))
# PProb_b = 1.938
# Means, the estimated probability that an indiviudal will support a policy
# if 80 of 192 countries participate in it with no sanctions.

## (c) Would the answers to 2a and 2b  potentially change if we included the
## interaction term in this model? Why?

# Yes, potentially the answers to 2a and 2b could change if we included 
# the interaction term in our model. Theoretically speaking this is because 
# it might make sense that the effect of one of our covariates (sanctions, for example),
# increasing from 5 to 15% may have a different effect on the outcome (choice), 
# depending on the other predictor (country support) - if more or less  countries 
# are supportive of the policy.

## Perform a test to see if including an interaction term is appropriate.

# I will perform a significance test for different slopes to see if 
# we get a better model fit by using an interactive term rather than 
# an additive term in our model. The p-value of our ANOVA test will 
# indicate whether our interactive model is a better fit or not.

# First, I will make a mod2 (which has the interactive term)
mod2 <- glm(choice ~ countries*sanctions, family =binomial(link="logit"), data = climateSupport)

# Next, run the ANOVA:

anova2 <- anova(mod1, mod2, test ="LRT")

list(anova2)
# Pvalue is over threshold (of 0.01), is .3912, meaning the model is not a 
# better fit when the interaction term is included. In other words, 
# there does not seem to be a different effect of percentage of sanctions 
# depending on the number of countries that participate in the policy on the likelihood 
# of supporting the policy (choice = supported), thus sticking to the 
# additive model would be preferable. (Using an interactive term is not appropriate).
