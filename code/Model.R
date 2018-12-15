library(MASS)
library(tidyverse)
library(caret)
library(leaps)
# Fit the full model 
full.model <- lm(profit_per_acre ~., data = full_c)

#Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)


#Plot Diagnostics
par(mfrow=c(2,2))
plot(step.model)
par(mfrow=c(1,1))
plot(step.model)


#Hist Plot Standardized Residuals 
SR_plot <- qplot(rstandard(step.model), geom = "histogram", 
                 colour = I("black"), fill = I("white"),
                 xlab = "Standardized Residuals", ylab = "Count")
print(SR_plot + ggtitle("Hist Plot Standardized Residuals of Profit"))