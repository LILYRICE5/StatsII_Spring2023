##############################
# Problem Set 4 - My Answers #
##############################

#############
# Question 1:
#############

# Load dataset:"infants" dataset in the eha library

library("eha")
data(infants)

# Fit a Cox Proportional Hazard Model using mother's age and infant's
# gender as covariates:

## Load packages

install.packages(c("survival", "survminer"))
library("survival")
library("survminer")

## FIRST, GOING TO LOOK AT HOW THE TWO VARIABLES IMPACT SURVIVAL INDIVIDUALLY, 
## DOING SEPARATE UNIVARIATE COX REGRESSIONS AT ONE TIME: 
## Apply univariate coxph function to multiple covariates at once (age and sex)
covariates <- c("age", "sex")
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(exit, event) ~', x)))

univ_models <- lapply(univ_formulas, function(x) {coxph(x, data = infants)})

## Extract data
univ_results <- lapply(univ_models,
                       function(x) {
                         x <- summary(x)
                         p.value <- signif(x$wald["pvalue"], digits = 2)
                         wald.test <- signif(x$wald["test"], digits = 2)
                         beta <- signif(x$coef[1], digits = 2); # coefficient beta
                         HR <- signif(x$coef[2], digits = 2); # exp beta
                         HR.confint.lower <- signif(x$conf.int[, "lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })

res <- t(as.data.frame(univ_results, check.names = FALSE))
as.data.frame(res)

# Present and interpret the output:

## Output is observable in console when running code "as.data.frame(res)"
## Interpretation of Results:
### Output shows regression beta coefficients ("beta"), the effect sizes (HRatios),
### statistical significance of each variable in relation to overall survival (pvals)

### Neither variable (age of mother or sex) have statisticalluy significant coefficients
### as the p.values are .49 and .25 respectively. Both of these pvalues are well above 
### common significance levels (of say .1, .05 etc).
### Both age and sex have negative beta coefficients. This implies that having an older mother
### and being a boy is associated with better survival,
### however it is important to note that still, these coefficients are not
### statistically significant so we cannot say the difference between
### groups is differentiable from zero really.

## NOW, GOING TO INTERPRET HOW THE TWO COVARIATES JOINTLY IMPACT SURVIVAL:
## I WILL DO THIS WITH A MULTIVARIATE COX REGRESSION ANALYSIS

# COX regression of exit (age at death) on time constant covariates:

res.cox <- coxph(Surv(exit, event) ~ age + sex, data = infants)
summary(res.cox)

# Interpretation:
## The p values again for the 3 tests run are not significant. This indicates 
## that the model is not significant, and we cannot reject the null hypothesis that
## all of the betas are 0. 
## In the multivariate analysis, the covariates are ot significant (not surprising), 
## they weren't significant in previous model either. 
## If the p vlaues were significant, the fact that the Hazard Ratio (exp(coef))
## for age is .97, and for sex is .61 could be interpreted as follows: 
## Holding other covariates constant, an infant that is a boy reduces the hazard 
## by a factor of .61. Being a boy holding all else constant would reduce the risk of
## death.
## Holding the other covariates constant, an infant with an older mother is 
## also less likely to die. 
## HOWEVER, the CONFIDENCE INTERVALS  for both hazard ratios include 1. This indicates
## This alongside the non-significant p values for both indicates that age of mother
## and sex of infant make relatively small (or null) contributions to likelihood of survival.