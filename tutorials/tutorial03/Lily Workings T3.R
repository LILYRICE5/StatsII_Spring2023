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

## Binary logits:

# Employing a sample of 1643 men between the ages of 20 and 24 from the U.S. National Longitudinal Survey of Youth.
# Powers and Xie (2000) investigate the relationship between high-school graduation and parents' education, race, family income, 
# number of siblings, family structure, and a test of academic ability. 

#The dataset contains the following variables:
# hsgrad Whether: the respondent was graduated from high school by 1985 (Yes or No)
# nonwhite: Whether the respondent is black or Hispanic (Yes or No)
# mhs: Whether the respondent’s mother is a high-school graduate (Yes or No)
# fhs: Whether the respondent’s father is a high-school graduate (Yes or No)
# income: Family income in 1979 (in $1000s) adjusted for family size
# asvab: Standardized score on the Armed Services Vocational Aptitude Battery test 
# nsibs: Number of siblings
# intact: Whether the respondent lived with both biological parents at age 14 (Yes or No)

graduation <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Powers.txt")

# (a) Perform a logistic regression of hsgrad on the other variables in the data set.
# Compute a likelihood-ratio test of the omnibus null hypothesis that none of the explanatory variables influences high-school graduation. 
# Then construct 95-percent confidence intervals for the coefficients of the seven explanatory variables. 
# What conclusions can you draw from these results? Finally, offer two brief, but concrete, interpretations of each of the estimated coefficients of income and intact.

## want to transform the data into hsgrad being a binary variable, do so as follows
## making it a logical vector:

graduation$hsgrad <- as.logical(ifelse(graduation$hsgrad == "Yes", 1, 0))

class(graduation$hsgrad)

## V USEFUL - ., WILL RUN IT ON ALL OF THE COVARIATES
mod1 <- glm(hsgrad ~ ., data = graduation, family ="binomial")

summary(mod1)

## NEXT CREATE A NULL MODEL - so we can then run a likelihood ratio model

nullmod <- glm(hsgrad ~ 1, data = graduation, family = "binomial")

## run anova:
anova(nullmod, mod1, test = "Chisq")
anova(nullmod, mod1, test = "LRT") #equivalent

## want to extract confidence intervals and then amke a dataframe

## data frame studd makes 3 wrror bars (lower ci, mid, and upper; first column is 2.5, (the lower
## third column is upper, and middle is the term itself))

confmod <- data.frame(cbind(lower = exp(confint(mod1)[,1]),
                            coefs = exp(coef(mod1)),
                            upper = exp(confint(mod1)[,2])))

summary(confmod)

## and can graph this

# (b) The logistic regression in the previous problem assumes that the partial relationship between the log-odds 
# (its ok, both will not completely change outcome) 
# of high-school graduation and number of siblings is linear. 
# Test for nonlinearity by fitting a model that treats nsibs as a factor, performing an appropriate likelihood-ratio test. 
# In the course of working this problem, you should discover an issue in the data. 
# Deal with the issue in a reasonable manner. 
# Does the result of the test change?

## problem = there is one case of a negative number of siblings; often why this happens = bc of the codiing
## -1 often means a non-answer, options --> could change to a 1 or drop it

graduation$nsibs <- as.integer(graduation$nsibs)
range(graduation$nsibs)

graduation$nsibs <- as.numeric(graduation$nsibs)
range(graduation$nsibs)

graduation$nsibs <- as.factor(graduation$nsibs)
range(graduation$nsibs)
hist(graduation$nsibs)
