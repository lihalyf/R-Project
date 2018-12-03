df_sec2c<- subset(sec2c, pid == 1) #subset data(not required)
df_sec2c <- apply(df_sec2c,2,factor) #factor dummy variables
df_sec2c <- merge(df_sec2c, land_and_profit, by = c('nh', 'clust')) #merge df with profit
df_sec2c <- subset(df_sec2c[c(5,6,7,8,9,10,11,21)]) #select dummy variables and average profit

#linear model
library(MASS)
# Fit the full model 
full.model <- lm(profit_per_acre ~., data = df_sec2c)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)