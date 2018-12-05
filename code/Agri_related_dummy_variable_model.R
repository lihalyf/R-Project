#merge all the selected agri data
Agriculture <- merge(AGRICULTURE_2, AGRICULTURE_3, by = c('nh', 'clust'))
Agriculture <- merge(Agriculture, AGRICULTURE_6, by = c('nh', 'clust'))
Agriculture <- merge(Agriculture, AGRICULTURE_7, by = c('nh', 'clust'))
Agriculture <- merge(Agriculture, AGRICULTURE_9, by = c('nh', 'clust'))
Agriculture <- merge(Agriculture, AGRICULTURE_10, by = c('nh', 'clust'))
Agriculture <- merge(Agriculture, AGRICULTURE_11, by = c('nh', 'clust'))
Agriculture <- merge(Agriculture, AGRICULTURE_12, by = c('nh', 'clust'))

#factor dummy variables
Agriculture <- apply(Agriculture,2,factor)
#merge data with profit table
Agriculture <- merge(Agriculture, land_and_profit, by = c('nh', 'clust'))

#deleted unnecessary variables
Agriculture <- Agriculture %>%
  select(-nh, -clust, -agri1c, -land_in_acre)
summary(Agriculture)



library(MASS)
library(tidyverse)
library(caret)
library(leaps)
# Fit the full model 
full.model <- lm(profit_per_acre ~., data = Agriculture)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)
#final model
Agri_related_model <- step.model

