##################################
# Tutorial 7: Survival Analysis #
##################################

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

lapply(c("survival", "eha", "tidyverse", "ggfortify", "stargazer"),  pkgTest)

install.packages("survival", "eha", "tidyverse", "ggfortify", "stargazer")
install.packages("eha")
library(eha)

#### Survival Analysis

# The `child` dataset from the `eha` package is a dataset of 26,855 children born in 
# Skellefte?, Sweden, 1850-1884. Children are followed for fifteen years or until death or 
# outmigration.
# The response variable is `exit`
# Explanatory variables include:
# - id: An identification number.
# - m.id: Mother's id.
# - sex: Sex.
# - socBranch: Working branch of family (father).
# - birthdate: Birthdate.
# - enter: Start age of follow-up, always zero.
# - exit: Age of departure, either by death or emigration.
# - event: Type of departure, death = 1, right censoring = 0.
# - illeg: Born out of marriage ("illegitimate")?
# - m.age: Mother's age.

data(child)
head(child)


## a) Using the Surv() function, build a survival object out of the `child` data.frame. 
##    Using survfit() and R's plotting functions, produce a Kaplan-Meier plot of the data,
##    firstly for overall survival, and secondly comparing categories of socBranch. How do
##    you interpret the second plot?

child_surv <- with(child, Surv(enter, exit, event))
km <- survfit(child_surv ~ 1, data = child) ## getting the non-conditioned mean, just the average 
## survival rate not taking into account anything else
summary(km, times = seq(0, 15, 1)) ## gives you each survival by the time periods you have 
## every year, what was the survival percentage; has CIs as well
plot(km, main = "Kaplan-Meier Plot", xlab = "Years", ylim = c(0.7, 1))
autoplot(km) ## gives plot, is a smoothed plot (smooth function over time plus dotted cis --> can
## see the bounds of your estimate; important for moving to next plot, if cis overlap, probs
## dont have stat significant estimates); the autoplot is sometimes easier visually to see; this 
## is just two different versions of same plot

km_socBranch <- survfit(child_surv ~ socBranch, data = child)
autoplot(km_socBranch) ## where error bars are very wide, fewer observations,, this is showing
## just the survival rate --> starting at 100%, seems children of business people are dying more 
## surviving less on avg than 

## b) Run a Cox Proportional Hazard regression on the data, using an additive model with 
##    `socBranch` and `sex` as explanatory variables. Run a test to assess the quality of the
##    model. How can we interpret the coefficients? Plot the model.

## this is where the hazard regression comes in
cox <- coxph(child_surv ~ sex + socBranch, data = child) ## coxph is how to do prop hazard, basic additive
summary(cox)
## interpret coefficients: scale = log of the hazard, being female decreases your log hazard of dying, becuase this 
## is survivial model, relative to your reference category (which is male); having farmer father reduces hazard
## being child of business man increases hazard. Log scale goes from -inf to inf, 0 is the middle. Anything below zero
## reduces odds, anything above increases
## Then when we exponentiate the log, to get the odds scale is 0 to inf, middle is 1; under 1 decreases hazard
## above 1 increases the hazard. ## important, just remember where the middle is

## z score given, so implies residuals are normally distributed, parameter is sigma

# with hazard, if you take the negative exp, is how much more likely they are to survive, than die (second column
# on second set of exponents)
drop1(cox, test = "Chisq") ## test the base model, gender plus branch of soc, drop1 function from add1 stats 
## package (base r) basically drops a variable and sees if model is still significant, basically if the variables in oiur
## model add anything,,, finds: yes, stat significant addition of sex and socbranch
## there is a .08 decrease in the expected log of the hazard for female babies comp to male...check back complete
## file
stargazer(cox, type = "text")


cox_fit <- survfit(cox) ##gets km (exactly same as previous), so need to come up with a dummy dataset and use 
## that as new data for prediction, bc basically plot here has every observation. so create new df with two obs
## male and female for official
autoplot(cox_fit)

newdat <- with(child, 
               data.frame(
                 sex = c("male", "female"), socBranch="official"
               )
)

plot(survfit(cox, newdata = newdat), xscale = 12,
     conf.int = T,
     ylim = c(0.6, 1),
     col = c("red", "blue"),
     xlab = "Time",
     ylab = "Survival proportion",
     main = "")
## above -- dash lines get a bit confusing, shading might be better;; this is the CI around a prediction
## happens end of last term, plotting error around prediction not around model itself, so error greater

# is variance around a prediction, not around a conditional mean

legend("bottomleft",
       legend=c("Male", "Female"),
       lty = 1, 
       col = c("red", "blue"),
       text.col = c("red", "blue"))

# Adding an interaction
cox.int <- coxph(child_surv ~ sex * socBranch, data = child)
summary(cox.int) 

## run summary, interpret: significance is gone now. shows only effect sig at 0.1 level is being a worker
## interacting any of those does not do anything
## conclude we would not run an interactive model, the data is well adjusted by an additive model....
## "the data is well adjusted by an additive model"
## adding an interactive term removes the significance, can run chisq to demonstrate, probability is .62, so 
## not doing work for us. also have stargazer output of interactive model which we said we didnt like

drop1(cox.int, test = "Chisq")
stargazer(cox.int, type = "text")



