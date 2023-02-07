# TUTORIAL 2 - WORKINGS #

# if you want to use glm for logistic regression, specify
# outcome and predictors same as lm, only diff is you
# set the family as "binomial"

# why? there's probit and logit models , logit more 
# common
# if you use family = binomial, you get logit  model

# second output = getting the log odds

library(stargazer)
install.packages(stargazer) #error - do this
## ages later - finding best fit line

set.seed(2022)
options(scipen = 500)

x <- rnorm(10000)

#set the intercept and slope
# seeing if R can estimate them correctly

intercept <- 3
slope <- 0.2

y_binom <- rbinom(10000, 1, exp(intercept + slope*x)/(1+exp(intercept + slope*x)) )

#run glm function:
binom_glm <- glm(y_binom ~ x, family = "binomial")
stargazer::stargazer(binom_glm, type = "html")

binom_glm <- glm(y_binom ~ x, family = "binomial")

summary(binom_glm)
