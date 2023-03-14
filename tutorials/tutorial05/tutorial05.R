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

library("MASS")
install.packages("MASS")

## Ordered multinomial logits:
  
  # This data set is analyzed by Long (1997).  The response variable has four ordered categories:
  # Strongly Disagree, Disagree, Agree, and Strongly Agree in relation to the statement
  # “A working mother can establishjust as warm and secure a relationship with her children as a mother who does not work."
  
  # The explanatory variables are:
  # the year of the survey (1977 or 1989),
  # the gender of the respondent, 
  # the race of the respondent (white or non-white), 
  # the respondent’s age, and 
  # the prestige of the respondent’s occupation (a quantitative variable)

workingMoms <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/WorkingMoms.txt", header=T)

workMv <- factor(workingMoms$attitude, order = TRUE,
                 levels = c("SD", "D", "A", "SA"))

workingMoms$attitude <- workMv

workingMoms$gender <- as.factor(workingMoms$gender)
workingMoms$race <- factor(workingMoms$race,
                           levels = c(0,1),
                           labels = c("Non-white", "white"))
workingMoms$year <- factor(workingMoms$year,
                           levels = c("Year1977", "Year1989"),
                           labels = c("1977", "1989"))

# (a) Perform an ordered (proportional odds) logistic regression of attitude toward working mothers on the other variables.
# What conclusions do you draw?

ord_logit <- polr(attitude ~ ., data=workingMoms, Hess =T) 

summary(ord_logit)

### interpreting intercepts: sd vs d etc, what this means actually is sd compared 
### to all of the others, etc.
### next, intercepts mean --> being male leads you to be less likely to agree
### greater prestige, more likely to agree
### AS YOU GET OLDER, less likely to agree
### could be easier with this many category models to actually just run predictions

### CALC P VALUE:
ctable <- coef(summary(ord_logit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) *2
(ctable <- cbind(ctable, "p value" = p))

### then CALC CONFIDENCE INTERVALS
(ci <- confint(ord_logit))

### convert just to odds ratio, easier to interpret
exp(cbind(OR = coef(ord_logit), ci)) # note anything 0-1 reduces odds, +1 means increased odds
 
# (b) Assess whether the proportional-odds assumption appears to hold for this regression. 
# Fit a multinomial logit model to the data, and compare and contrast the results with those from the proportional odds model.

### My answer no : use CIs (code in answer file): I think the proportional odds assumption does not appear to hold
### for this regression. When looking at the coefficients from the 
### ordinal logit regression output, there are some going in the positive
### direction and others going in the negative direction, indicating
### that the parallel regression assumption does not hold. Perhaps 
### having this variable as an ordered category is therefore problematic.

### Now: multinomial logit model:

install.packages("nnet")
library("nnet")

multinom_logit <- multinom(attitude ~ ., data = workingMoms, ref = "SD")
summary(multinom_logit)
### here, each term = a separate regression --> end up w a table, for disagree for ex,
### have effect of each iv on that outcome, relevant to reference level which is strongly disagree

# (c) Consider that possibility that gender interacts with the other explanatory variables in influencing the response variable. 
# What do you find?

multinom_logit_interact <- multinom(attitude ~ gender*., data = workingMoms)
summary(multinom_logit_interact)
