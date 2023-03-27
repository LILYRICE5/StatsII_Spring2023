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

#################################
# PS03 # My Answers - Lily Rice #
#################################

# Imported Data (called gdpChange)
library(readr)
gdpChange <- read_csv("~/Documents/GitHub/StatsII_Spring2023/datasets/gdpChange.csv")
View(gdpChange)

# rename dataset

dat <- gdpChange

# Data Wrangling: need to change GDPWdiff into a factor i think

dat <- within(dat, {
  GDPWdiff.cat[GDPWdiff <= -1] <- "negative"
  GDPWdiff.cat[GDPWdiff >= -1 & GDPWdiff <= 1] <- "no change"
  GDPWdiff.cat[GDPWdiff >= 1] <- "positive"
  })

##############
# Question 1 #
##############

# install necessary packages:
install.packages("nnet")
library(nnet)

# Construct & Interpret unordered multinomial logit:
## Note: this is an unordered logit because we have to specify our reference
## category first:
## the way I did this as an unordered factor was by changing it to a factor,
## then assigning the reference level to no change, and then running my mod1

dat$GDPWdiff.cat <- as.factor(dat$GDPWdiff.cat)
dat$GDPWdiff.cat2 <- relevel(dat$GDPWdiff.cat, ref = "no change")

mod1 <- multinom(GDPWdiff.cat2 ~ REG + OIL, data = dat)
summary(mod1)





