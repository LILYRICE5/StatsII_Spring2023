library(haven)
library(tidyverse)
library(ggplot2)
library(psych)
library(survey)
library(labelled)
library(knitr)
library(stargazer)
library(sjPlot)
library(gridExtra)

# Standard ggPlot theme for plots

ggmjntheme <- theme(axis.title.x=element_text(colour="black",size=14),
                    axis.title.y=element_text(colour="black",size=14),
                    axis.text.x=element_text(colour="black",size=12),
                    axis.text.y=element_text(colour="black",size=12),
                    plot.title = element_text(hjust = 0.5))

# Dataset preparation

cces <- read_dta("JudgingProsecutors.dta")

## Recode and create variables for treatments, outcomes, and demographics

cces <- cces %>% mutate(lenient_pre = as.factor(ifelse(PSU357_treat %in% c(1,3), 1, ifelse(is.na(PSU357_treat), NA, 0))),
                        lenient_post = as.factor(ifelse(PSU431_split %in% c(1,3), 1, ifelse(is.na(PSU431_split), NA, 0))),
                        dem = ifelse(pid7 %in% c(1,2,3), 1, 0),
                        rep = ifelse(pid7 %in% c(5,6,7), 1, 0),
                        copartisan = as.factor(ifelse((PSU431_split %in% c(1,2) & dem == 1) | (PSU431_split %in% c(3,4) & rep == 1), 1, ifelse(dem == 0 & rep == 0, NA, 0))),
                        def_white = as.factor(ifelse(PSU357_treat %in% c(1,2), 1, ifelse(is.na(PSU357_treat), NA, 0))),
                        approve_pre = ifelse(is.na(PSU358_1), 3, PSU358_1),
                        elec_sup_pre = -1*PSU360 + 5,
                        fair_pre = -1*PSU359 + 5,
                        elec_sup_post = -1*PSU434 + 5,
                        fair_post = -1*PSU433 + 5,
                        approve_post = ifelse(is.na(PSU432s) & tookpost == 2, 3, PSU432s),
                        ideo = ifelse(ideo5 == 6, 3, ideo5),
                        pknow_1 = ifelse(PSU316 == 1, 1, 0),
                        pknow_2 = ifelse(PSU317 == 2, 1, 0),
                        pknow_3 = ifelse(PSU318 == 3, 1, 0),
                        pknow = pknow_1 + pknow_2 + pknow_3,
                        white = ifelse(race == 1, 1, 0), 
                        black = ifelse(race == 2, 1, 0))

## Create PCA incumbent support measure for both waves

pca_pre <- pca(cces %>% select(c(approve_pre, fair_pre, elec_sup_pre)), scores = T)

cces <- cces %>% cbind(pca_pre$scores) %>% rename(incum_pre = PC1)

pca_post <- pca(cces %>% select(c(approve_post, fair_post, elec_sup_post)), scores = T)

cces <- cces %>% cbind(pca_post$scores) %>% rename(incum_post = PC1)

### Variability explained by PCA

pca_pre$Vaccounted
pca_post$Vaccounted

## Construct survey object

cces_design <- svydesign(id = ~1, weights = ~teamweight, data = cces)

# T-tests, treatment effects, and plots

## Study 1
svyttest(elec_sup_pre~lenient_pre, cces_design)$p.value

### Effect size
svyttest(elec_sup_pre~lenient_pre, cces_design)$estimate 

### Effect size (in terms of standard deviations)
svyttest(elec_sup_pre~lenient_pre, cces_design)$estimate / sqrt(svyvar(~elec_sup_pre, cces_design, na.rm = T))

### Alternative outcomes (appendix)
svyttest(approve_pre~lenient_pre, cces_design)$p.value
svyttest(fair_pre~lenient_pre, cces_design)$p.value
svyttest(incum_pre~lenient_pre, cces_design)$p.value

### Figure 1
ggplot(svyby(~elec_sup_pre,~lenient_pre, cces_design, svymean, na.rm = T)) + geom_pointrange(aes(x = rownames(svyby(~elec_sup_pre,~lenient_pre, cces_design, svymean, na.rm = T)), y=elec_sup_pre, ymin=elec_sup_pre-1.96*se, ymax = elec_sup_pre+1.96*se))+ xlab("Sentence Length") + ylab("Re-Election Support") + scale_x_discrete(labels = c("6 Months", "6 Days")) + scale_y_continuous(limits = c(1,4)) + theme_bw() + labs(title = "Re-Election Support") + ggmjntheme

### Figure C1 (all three plots)
ggplot(svyby(~incum_pre,~lenient_pre, cces_design, svymean, na.rm = T)) + geom_pointrange(aes(x = rownames(svyby(~incum_pre,~lenient_pre, cces_design, svymean, na.rm = T)), y=incum_pre, ymin=incum_pre-1.96*se, ymax = incum_pre+1.96*se))+ xlab("Sentence Length") + ylab("Incumbent Supportl") + scale_x_discrete(labels = c("6 Months", "6 Days")) + scale_y_continuous(limits = c(-3,3)) + theme_bw() + labs(title = "Incumbent Support") + ggmjntheme + geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60")

ggplot(svyby(~approve_pre,~lenient_pre, cces_design, svymean, na.rm = T)) + geom_pointrange(aes(x = rownames(svyby(~approve_pre,~lenient_pre, cces_design, svymean, na.rm = T)), y=approve_pre, ymin=approve_pre-1.96*se, ymax = approve_pre+1.96*se))+ xlab("Sentence Length") + ylab("Prosecutorial Approval") + scale_x_discrete(labels = c("6 Months", "6 Days")) + scale_y_continuous(limits = c(1,5)) + theme_bw() + labs(title = "Prosecutorial Approval") + ggmjntheme

ggplot(svyby(~fair_pre,~lenient_pre, cces_design, svymean, na.rm = T)) + geom_pointrange(aes(x = rownames(svyby(~fair_pre,~lenient_pre, cces_design, svymean, na.rm = T)), y=fair_pre, ymin=fair_pre-1.96*se, ymax = fair_pre+1.96*se))+ xlab("Sentence Length") + ylab("Perceived Fairness") + scale_x_discrete(labels = c("6 Months", "6 Days")) + scale_y_continuous(limits = c(1,4)) + theme_bw() + labs(title = "Perceived Fairness") + ggmjntheme

## Study 2
svyttest(elec_sup_post~lenient_post, cces_design)$p.value
svyttest(elec_sup_post~copartisan, cces_design)$p.value

### Effect sizes
svyttest(elec_sup_post~lenient_post, cces_design)$estimate 
svyttest(elec_sup_post~copartisan, cces_design)$estimate 

### Effect sizes (in terms of standard deviations)
svyttest(elec_sup_post~lenient_post, cces_design)$estimate / sqrt(svyvar(~elec_sup_post, cces_design, na.rm = T))
svyttest(elec_sup_post~copartisan, cces_design)$estimate / sqrt(svyvar(~elec_sup_post, cces_design, na.rm = T))

### Figure 2
ggplot(svyby(~elec_sup_post,~copartisan*lenient_post, cces_design, svymean, na.rm = T)) + geom_pointrange(aes(x = rownames(svyby(~elec_sup_post,~copartisan*lenient_post, cces_design, svymean, na.rm = T)), y=elec_sup_post, ymin=elec_sup_post-1.96*se, ymax = elec_sup_post+1.96*se)) + xlab("Treatment Combination") + ylab("Re-Election Support") + scale_x_discrete(labels = c("1 Year \n + Outpartisan", "1 Day \n + Outpartisan", "1 Year \n + Copartisan", "1 Day \n + Copartisan")) + scale_y_continuous(limits = c(1,5)) + theme_bw() + labs(title = "Re-Election Support") + ggmjntheme

### Figure C2 (all three plots)
ggplot(svyby(~approve_post,~copartisan*lenient_post, cces_design, svymean, na.rm = T)) + geom_pointrange(aes(x = rownames(svyby(~approve_post,~copartisan*lenient_post, cces_design, svymean, na.rm = T)), y=approve_post, ymin=approve_post-1.96*se, ymax = approve_post+1.96*se)) + xlab("Treatment Combination") + ylab("Prosecutorial Approval") + scale_x_discrete(labels = c("1 Year \n + Outpartisan", "1 Day \n + Outpartisan", "1 Year \n + Copartisan", "1 Day \n + Copartisan")) + scale_y_continuous(limits = c(1,5)) + theme_bw() + labs(title = "Prosecutor Approval") + ggmjntheme

ggplot(svyby(~fair_post,~copartisan*lenient_post, cces_design, svymean, na.rm = T)) + geom_pointrange(aes(x = rownames(svyby(~fair_post,~copartisan*lenient_post, cces_design, svymean, na.rm = T)), y=fair_post, ymin=fair_post-1.96*se, ymax = fair_post+1.96*se)) + xlab("Treatment Combination") + ylab("Perceived Fairness") + scale_x_discrete(labels = c("1 Year \n + Outpartisan", "1 Day \n + Outpartisan", "1 Year \n + Copartisan", "1 Day \n + Copartisan")) + scale_y_continuous(limits = c(1,5)) + theme_bw() + labs(title = "Perceived Fairness") + ggmjntheme

ggplot(svyby(~incum_post,~copartisan*lenient_post, cces_design, svymean, na.rm = T)) + geom_pointrange(aes(x = rownames(svyby(~incum_post,~copartisan*lenient_post, cces_design, svymean, na.rm = T)), y=incum_post, ymin=incum_post-1.96*se, ymax = incum_post+1.96*se)) + xlab("Treatment Combination") + ylab("Incumbent Support") + scale_x_discrete(labels = c("1 Year \n + Outpartisan", "1 Day \n + Outpartisan", "1 Year \n + Copartisan", "1 Day \n + Copartisan")) + scale_y_continuous(limits = c(-3,3)) + geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60")  + theme_bw() + labs(title = "Incumbent Support") + ggmjntheme

# Regression models

## Study 1
pre_m1c <- glm(elec_sup_pre ~ lenient_pre + white + black + birthyr + pknow + as.factor(gender) + ideo + educ, family = "gaussian", cces)
pre_m2c <- glm(elec_sup_pre ~ def_white + white + black + birthyr + pknow + as.factor(gender) + ideo + educ, family = "gaussian", cces)
pre_m3c <- glm(elec_sup_pre ~ lenient_pre*def_white + white + black + birthyr + pknow + as.factor(gender) + ideo + educ, family = "gaussian", cces)

### Table C1
stargazer(pre_m1c, pre_m2c, pre_m3c, dep.var.labels = c("Re-election Support"), title = "Regression Models including Defendant Race, Study 1", header = FALSE, digits=2, style = "apsr", star.cutoffs = c(.05, .01, .001), covariate.labels = c("Lenient Sentence (1 Day)", "White", "Black", "Age", "Pros. Knowledge", "Woman", "Conservatism", "Education"))

pre_elecc <- glm(elec_sup_pre ~ lenient_pre + white + black + birthyr + pknow + as.factor(gender) + ideo + educ, family = "gaussian", cces)
pre_supc <- glm(incum_pre ~ lenient_pre + white + black + birthyr + pknow + as.factor(gender) + ideo + educ, family = "gaussian", cces)
pre_appc <- glm(approve_pre ~ lenient_pre + white + black + birthyr + pknow + as.factor(gender) + ideo + educ, family = "gaussian", cces)
pre_fairc <- glm(fair_pre ~ lenient_pre + white + black + birthyr + pknow + as.factor(gender) + ideo + educ, family = "gaussian", cces)

### Table C2
stargazer(pre_elecc, pre_supc, pre_appc, pre_fairc, dep.var.labels = c("Re-election Support", "Incumbent Support", "Approval", "Fairness"), title = "Alternative Outcome Models, Study 1", header = FALSE, digits=2, style = "apsr", star.cutoffs = c(.05, .01, .001), covariate.labels = c("Lenient Sentence (1 Day)", "White", "Black", "Age", "Pros. Knowledge", "Woman", "Conservatism", "Education"))

## Study 2 
post_m1s <- glm(elec_sup_post ~ lenient_post, family = "gaussian", cces %>% filter(copartisan %in% c(0,1)))
post_m2s <- glm(elec_sup_post ~ copartisan, family = "gaussian", cces %>% filter(copartisan %in% c(0,1)))
post_m3s <- glm(elec_sup_post ~ lenient_post*copartisan, family = "gaussian", cces %>% filter(copartisan %in% c(0,1)))

### Table 1
stargazer(post_m1s, post_m2s, post_m3s, dep.var.labels = c("Re-election Support"), title = "Regression Models, Study 2", header = FALSE, digits=2, style = "apsr", star.cutoffs = c(.05, .01, .001), covariate.labels = c("Lenient Sentence (1 Day)", "Copartisan Pros.", "Copartisan $\\times$ Lenient Sentence"))

post_m1c <- glm(elec_sup_post ~ lenient_post + white + black + birthyr + pknow + as.factor(gender) + ideo + educ, family = "gaussian", cces %>% filter(copartisan %in% c(0,1)))
post_m2c <- glm(elec_sup_post ~ copartisan + white + black + birthyr + pknow + as.factor(gender) + ideo + educ, family = "gaussian", cces %>% filter(copartisan %in% c(0,1)))
post_m3c <- glm(elec_sup_post ~ lenient_post*copartisan + white + black + birthyr + pknow + as.factor(gender) + ideo + educ, family = "gaussian", cces %>% filter(copartisan %in% c(0,1)))

### Table C3
stargazer(post_m1c, post_m2c, post_m3c, dep.var.labels = c("Re-election Support"), title = "Regression Models with Controls, Study 2", header = FALSE, digits=2, style = "apsr", star.cutoffs = c(.05, .01, .001), covariate.labels = c("Lenient Sentence (1 Day)", "Copartisan Pros.", "White", "Black", "Age", "Pros. Knowledge", "Woman", "Conservatism", "Education", "Copartisan $\\times$ Lenient Sentence"))

# Partisanship Effects (Supplementary Material)

## Study 1
svyttest(elec_sup_pre~lenient_pre, subset(cces_design, dem == 1))
svyttest(elec_sup_pre~lenient_pre, subset(cces_design, rep == 1))
svyttest(elec_sup_pre~lenient_pre, subset(cces_design, dem == 0 & rep == 0))

## Study 2
svyttest(elec_sup_post~lenient_post, subset(cces_design, dem == 1))
svyttest(elec_sup_post~lenient_post, subset(cces_design, rep == 1))

svyttest(elec_sup_post~copartisan, subset(cces_design, dem == 1))
svyttest(elec_sup_post~copartisan, subset(cces_design, rep == 1))

svyttest(elec_sup_post~copartisan, subset(cces_design, dem == 1 & lenient_post == 1))
svyttest(elec_sup_post~copartisan, subset(cces_design, rep == 1 & lenient_post == 1))

svyttest(elec_sup_post~lenient_post, subset(cces_design, dem == 1 & copartisan == 1))
svyttest(elec_sup_post~lenient_post, subset(cces_design, dem == 1 & copartisan == 0))
svyttest(elec_sup_post~lenient_post, subset(cces_design, rep == 1 & copartisan == 1))
svyttest(elec_sup_post~lenient_post, subset(cces_design, rep == 1 & copartisan == 0))

## Figure C3

### Study 1 (left-side)
rbind(cbind(svyby(~elec_sup_pre, ~lenient_pre, subset(cces_design, rep == 1), svymean, na.rm = T), 
                          party = "rep"), 
                    cbind(svyby(~elec_sup_pre, ~lenient_pre, subset(cces_design, dem == 1), svymean, na.rm = T), 
                          party = "dem")) %>% mutate(treatment = lenient_pre) %>%
  ggplot(aes(x = treatment, group = treatment, color = as.factor(party))) + 
  geom_pointrange(aes(y = elec_sup_pre, ymin = elec_sup_pre - 1.96*se, ymax = elec_sup_pre + 1.96*se),
                  position = position_dodge2(.5)) +
  scale_color_manual(name = "Party", labels = c("Democrat", "Republican"), values = c("#00BFC4", "#F8766D")) + 
  theme_bw() + ggmjntheme + xlab("Treatment Combination") + ylim(1,5) + ggtitle("Re-Election Support, Study 1") +
  ylab("Re-Election Support") + scale_x_discrete(labels = c("6 Months\n", "6 Days\n")) + theme(legend.position = "none")


### Study 2 (right-side)
rbind(cbind(svyby(~elec_sup_post, ~copartisan*lenient_post, subset(cces_design, rep == 1), svymean, na.rm = T), 
            party = "rep"), 
      cbind(svyby(~elec_sup_post, ~copartisan*lenient_post, subset(cces_design, dem == 1), svymean, na.rm = T), 
            party = "dem")) %>% mutate(treatment = paste(copartisan, lenient_post, sep = ".")) %>%
  ggplot(aes(x = treatment, group = treatment, color = as.factor(party))) + 
  geom_pointrange(aes(y = elec_sup_post, ymin = elec_sup_post - 1.96*se, ymax = elec_sup_post + 1.96*se),
                  position = position_dodge2(.5)) +
  scale_color_manual(name = "Party", labels = c("Democrat", "Republican"), values = c("#00BFC4", "#F8766D")) + 
  scale_x_discrete(labels = c("1 Year \n + Outpartisan", "1 Day \n + Outpartisan", "1 Year \n + Copartisan", "1 Day \n + Copartisan")) +
  theme_bw() + ggmjntheme + xlab("Treatment Combination") + ylim(1,5) + ggtitle("Re-Election Support, Study 2") +
  ylab("Re-Election Support")