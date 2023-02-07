## Tutorial 1 - Revision - Lily ##

# FIRST - LOAD LIBRARIES

library(tidyverse)

install.packages("stargazer")
library(stargazer)

?stargazer
# 1. IMPORT DATA:

data <- read_csv("data/tutorial1_data.csv", 
                 col_types = cols(
                   `Ease of doing business rank (1=most business-friendly regulations) [IC.BUS.EASE.XQ]` = col_double(),
                   `Tax revenue (% of GDP) [GC.TAX.TOTL.GD.ZS]` = col_double(),
                   `GDP per capita (current US$) [NY.GDP.PCAP.CD]` = col_double()))

data <- data %>%
  select(-(starts_with("Time")), -(`Country Code`))

names(data) <- sub(" \\[.*", "", names(data))

# DATA ANALYSIS: 
## 1. GGPLOT TO CREATE SCATTERPLOT OF GDP VS TAX REVENUE, ADDING SIMPLE LM LINE
data %>%
  ggplot(aes(`Tax revenue (% of GDP)`, `GDP per capita (current US$)`)) +
  geom_point() +
  geom_smooth(method = "lm")

data %>%
  ggplot(aes(`Ease of doing business rank (1=most business-friendly regulations)`, 
             `GDP per capita (current US$)`)) +
  geom_point() +
  geom_smooth(method = "lm")

data %>%
  ggplot(aes(`Ease of doing business rank (1=most business-friendly regulations)`, 
             `Tax revenue (% of GDP)`)) +
  geom_point() +
  geom_smooth(method = "lm")

#creates a value titled "formula" (see in Global Environment)
formula <- `GDP per capita (current US$)` ~ `Tax revenue (% of GDP)` + `Ease of doing business rank (1=most business-friendly regulations)`

#run the regression (linear regression, additive model)
reg <- lm(formula, data)

stargazer(reg, type = "latex")

#just labeling the graph
data %>%
  ggplot(aes(`Tax revenue (% of GDP)`, 
             `GDP per capita (current US$)`, 
             alpha = -`Ease of doing business rank (1=most business-friendly regulations)`)) +
  geom_text(aes(label = `Country Name`), show.legend = FALSE) +
  geom_smooth(method = "lm", show.legend = FALSE) +
  ylim(0, 150000) +
  labs(title = "GDP per capita and Tax Revenue (2019)",
       subtitle = "World Bank data - Europe and Central Asia",
       alpha = "Ease of doing\nbusiness")
