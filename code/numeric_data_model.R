numer <- merge(SAVINGS, AGRICULTURE_8, by = c('nh', 'clust'))

#merge data with profit table
numer <- merge(numer, land_and_profit, by = c('nh', 'clust'))
write.csv(numer, file = "~/econ_5100/Lecture_05/numeric_data.csv",quote=F,row.names = F)


#deleted unnecessary variables
numer <- numer %>%
  select(-nh, -clust, -agri1c)
summary(numer)



library(MASS)
library(tidyverse)
library(caret)
library(leaps)
# Fit the full model 
full.model <- lm(profit_per_acre ~., data = numer)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)
summary(full.model)
#final model
numer_model <- step.model