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

library("tidyverse")

load.packages(tidyverse)

## Poisson

# Data: Research Productivity of Doctoral Students in Bio-chemistry (Long 1990) 
# Productivity of doctoral students in biochemistry during the last three yearsof their PhD programmes. 
# The response variables the number of articles published during this period (art)
# Explanatory variables include:
# - gender of the student (fem=1 for women, 0=men)
# - student’s marital status (mar= 1 if married, 0 otherwise)
# - student’s number of children five years old or younger (kid5); 
# - a rating of the prestige of the student’sPhD department (phd); 
# - number of articles published by the student’s mentor during the three-yearperiod (ment)

long_data <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Long.txt", header=T)

## wrangling:
long_data <- within(long_data, {
  fem <- as.logical(fem)
  mar <- as.logical(mar)
})

## "EDA"--> 
str(long_data)
summary(long_data)

# (a) Examine the distribution of the response variable. 
# Does least-squares linear regression appear a promising strategy for these data?

##should look at measures of central tendancy --> like mean and variance

mean_art <- mean(long_data$art)
var_art <- var(long_data$art)

## mean is 1.7, variance is 3.7 (so they are not the same, not going to consider this
## a poisson distribution / use a poisson model)

## thinking, might want to do box plot or histogram
hist(long_data$art)
ggplot(long_data, aes(ment, art, color = fem)) + 
  geom_jitter(alpha=.5)

## OLS? 
mod.lm <- lm(art~., data = long_data)
summary(mod.lm)

mod2.lm <- lm(art ~ fem*., data=long_data)
summary(mod2.lm)

### are ols assumptions met???
#### plot absolute residuals (wont see anything below zero on y axis)
plot(predict(mod2.lm), abs(resid(mod2.lm)), xlab = "Predicted", ylab = "Absolute")
### assumes theyre normally distributed, this graph should have a line going straight up

### standardised resids - do hist (should be norm dist)

### is right skewed, but they are clustered and peak round 0

### par(mfrow = c(2,2)) do before plotting mod output

### first graph, resids do not match the fitted --
### qq plot just after positive side,m really starts to trend off normal line
#####NOT MEETING OUR ASSUMPTIONS
# (b) Perform a Poisson regression of number of articles published on the explanatory variables. 
# What conclusions would you draw from this analysis?

mod.ps <- glm(art ~., data = long_data, family = poisson)
summary(mod.ps)

## first thing we get is the deviance of the residuals
## shows that data are skewed, because median lies before our mean -- so there is a right skew to data
### regardless, look at output
  ## coefficients are in log odds scale: on average, the intercept at .3; phd researchers are more likely to publish articles than not
## female, slightly less likely to publish, married, slightly more, kids, less likely

### and gives the deviance
# estract coefficients:
cfs <- coef(mod.ps)
summary(cfs)
## to get predicted number of articles for married male phd with 1 child at 2 years into phd, exponentiate:

exp(cfs[1] + cfs[2]*0 + cfs[3]*5 + cfs[4]*2 + cfs[5]*1 + cfs[6]*1)

cfs

pred <- data.frame(fem = FALSE,
                   ment = 5,
                   phd = 2,
                   mar = TRUE,
                   kid5 = 1 )

#coeff for kids, exponentiate:
exp(-0.18*5) # gives you .4 of an article now
# gets .83 first (without5), means that

### plot prediction versus count:
ggplot(data = NULL, aes(x=mod.ps$fitted.values, y = long_data$art)) +
  geom_jitter(alpha = .5) +
  geom_abline(color = "blue")

### calc psuedo R squared
1-(mod.ps$deviance/mod.ps$null.deviance)

### calc RMSE
sqrt(mean((mod.ps$art - mod.ps$fitted.values....)))

# (c) Consider the possibility of over-dispersion, either by fitting an over-dispersed Poisson model. 
# Is there evidence for over-dispersion? How, if at all, do the results change when over-dispersion is taken into account

## UCLA shows how to do this

## use AER and pscl packages

install.packages("AER")
library(AER)

dispersiontest(mod.ps)

#indicates perhaps yes, overdispersed, because v low p value

install.packages("pscl")
library(pscl)
 #this has function xip that is for running a zip regression

mod.zip <- zeroinfl(art ~ ., data = long_data, dist = "poisson")
summary(mod.zip)
#the models of the zeros uses logistic reg and the other uses a poisson -->
## gives us two models
## what does it mean --> after you do all the transformations, will give you a probability of if this observation comes from a different zero generating process
## zip model estimates -> only one stat sig = number of publications of mentor:
## as # of publications of your mentor goes up, continues to decrease the odds that you will be 0 published

### potential omission bias -> bc have no variable for year in program that you are in ()

summary(mod.ps)
summary(mod.zip)
 #compare:
## intercpt has incredased 
## role of mentor changes a little bit (larger now)
## prestige of phd program changes sign, but still not stat significant
### could run interaction term, married with kids may have more impact when female than male