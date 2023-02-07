#####
#Tutorial 1#

#Code to drop excess columns --> select dplyr method)

# DATA WRANGLING:
data <- World_Bank_data

library(tidyverse)
library(stargazer)

#removes some unnecessary columns:
data <- data %>%
  select(-(starts_with("Time")), -(`Country Code`))

#code below removes everything after first square bracket, gets rid of the internal variable
#codes (in titles of columns):
names(data) <- sub("\\[.*","", names(data))

# ANALYSE DATA:
# Use ggplot to create scatter plot showing GDP p/c vs Tax Revenue

data %>% 
  ggplot(aes(`Tax revenue (% of GDP) `, `GDP per capita (current US$) `)) +
  geom_point() +
  geom_smooth(method = "lm")

# need to import as numeric, theyre reading in as characters --> so going to 
# reimport as numeric; this produces good graph

# HOW TO TURN CHARACTER TO NUMERIC = WITH CODE
#easy way to access specific column without using code --> eg the second column (ease of doing business)
#DO THIS WITH INDEXING: (problems with this in class, Martin working on in class)
data[,2] <- as.numeric(data[,2]) # not working, so martin just reimported as numeric as well

#trying to run the ggplot for other two variables
data %>% 
  ggplot(aes(`Tax revenue (% of GDP) `, `Ease of doing business score (0 = lowest performance to 100 = best performance)`)) +
  geom_point() +
  geom_smooth(method = "lm")

#note - this code uses diff name for ease of doing business RANK rather than score
#Martin will redo code
data %>% 
  ggplot(aes(`Tax revenue (% of GDP) `, 
             `GDP per capita (current US$)`,
             alpha = `Ease of doing business score (0 = lowest performance to 100 = best performance)`)) +
  geom_point() +
  geom_smooth(method = "lm", show,legend = FALSE) +
  ylim(0, 150000)




