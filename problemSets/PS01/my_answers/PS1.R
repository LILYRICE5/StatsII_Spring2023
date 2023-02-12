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

## Ran all code as instructed on Problem Set:
# First, set.seed() as instructed
set.seed(123)
# Next, create my data using the Cauchy random variables, as instructed
data <- rcauchy(1000, location = 0, scale = 1)
# create empirical distribution of observed data (unchanged by me, from PS01 doc)
ECDF <- ecdf(data)
empiricalCDF <- ECDF(data)
# generate test statistic (as shown on PS01 doc)
D <- max(abs(empiricalCDF - pnorm(data)))
# this returns a D = 0.134728...

## Attempted to estimate p-value manually:
# Tried multiple different ways to estimate p value "by hand",
# ultimately was unsuccessful (some workings on this below)
# NOTE: this was attempted because Jeff's function for D (given to us)
# used the absolute value of the difference between the two distributions;
# this implies that we are trying to do a two-sided ks.test on the distributions
# and with two-sided tests, the decision to reject the null hypothesis is based on
# the p-value (comparing this value to sigma)

## Estimated p-value using ks.test() function:
# Ran the ks.test() function however to determine what P value
# I should be looking for (to check my answers). I did this ks.test()
# with the below code:

kstest_on_data <- ks.test(data, "pnorm")
list(kstest_on_data)
# Note: produces D = 0.136 (approximately the same value produced by Jeff's function)
# ; and p = <2.22e-16.

# We reject our null hypothesis (that there is no difference between the normal
# distribution and the distribution of our data) because p < 0.05. 

###########################################
# Start of Question 1 Unsuccessful Workings:
###########################################
# below is (most) of my attempts to answer Q1 "by hand" - was unsuccessful, 
# just wanted to include this to show what was attempted and for reference 
# when comparing with correct answer - not necessary to run to show
# my work for answers included on my PS01 my answers .pdf

# Probability that dist = normal with D value as is or greater
mean <- mean(pnorm(data))
sd <- sd(pnorm(data))
pnorm(q=D, mean = mean, sd = sd, lower.tail = T)
#returns .15

#P(D>=.136)
pnorm(q=D, mean=0, sd = 1, lower.tail = F)
#returns .44... not correct, still working

#trying to write a function for p val
linear.MLE <- optim(fn=ECDF, par=(0:1), hessian=TRUE, x=data,
                    method="BFGS")

#trying again:
linear.MLE <- optim(fn=pnorm(data), par=(0:1), hessian=TRUE, x=data,
                    method="BFGS")

P_val_function <- function(theta, ) {
  k <- 1
  beta <- theta[1:k]
  sigma2 <- theta[k+1]^2
  pvalattempt <- exp(-(2*k-1)^2)*(pi^2)/(8*D^2)
}

p_val_solution <- optim(fn = P_val_function), par = 0:1, hessian = TRUE)

#trying another way
f <- function(pval) {
  p <- exp((-2*1000-1)^2)*(pi^2)/(8*.13472806160635^2)
}
ans <- optim(fn = f, par = 1,1,1, hessian = TRUE) 

######################################### 
# End of Question 1 Unsuccessful Workings
#########################################

#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

## Run OLS regression that uses BFGS (Quasi-Newton Method):
# First, plotted the data:
plot(data$x, data$y, ylab = 'Y', xlab = 'X')
# now going to code this as a function in R with theta as a function of beta and sigma
linear.lik <- function(theta, y, X) {
  n <- nrow(X)
  k <- ncol(X)
  beta <- theta[1:k]
  sigma2 <- theta[k+1]^2
  e <- y - X%*%beta
  logl <- -.5*n*log(2*pi)-.5*n*log(sigma2) - ((t(e)%*%e)/(2*sigma2))
  return(-logl)
}

# NOW (that we have the function coded into R), I will do the regression:
linear.MLE <- optim(fn = linear.lik, par=c(1,1,1), hessian=TRUE, y=data$y,
                    X=cbind(1, data$x), method ="BFGS")

linear.MLE$par
# returns an intercept = 0.1398; beta = 2.7266; and sigma estimate = -1.4391

# CHECK ANSWERS: run lm regression (to check against my Newton-Raphson OLS estimates above):
lm_regression <- lm(data$y ~ data$x)
summary(lm_regression)

# Success! Both estimate an intercept of 0.139 and beta (slope) of 2.727
# One difference = we don't get an estimate of sigma squared with lm