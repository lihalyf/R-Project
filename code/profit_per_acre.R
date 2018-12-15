###1.1.3 profit per acre:
library(haven)
library(here)
library(ggplot2)

##original profit data
# (please make sure the current working directory is in the project folder)
# setwd('/Users/weili/Desktop/MSBA/1. Stats/glss4')
agg2 <- read_dta('aggregates/agg2.dta')

##original land data 
sec8a1 <- read_dta('sec8a1.dta')
land <- subset(sec8a1[c('clust', 'nh', 's8aq3', 's8aq4')]) 
#"s8aq3": unit of land; "s8aq4": land owned by household

## for the land owned by household column:
# 4.222000e+03 is the maximum reasonable value in the column 's8aq4',
valid_rows_land <- land['s8aq4'] <= 4.222000e+03  
# 4 is the largest number to represent unit. larger than 4 is wrong.
valid_rows_unit <- land['s8aq3'] < 4 

valid_land <- land[valid_rows_land, ]
valid_land <- land[valid_rows_unit, ]
# now we excluded the strange values '1.749801e+100' in 's8aq4' and wrong/empty unit data in 's8aq3'.

# now valid_land is the correct table that we can use.
##then we need to create a column for the land in the same unit
for (rows in 1:nrow(valid_land)) {
  if (valid_land[rows, 's8aq3'] == 1) {
    valid_land[rows, 'land_in_acre'] = valid_land[rows, 's8aq4']
  }
  if (valid_land[rows, 's8aq3'] == 2) {
    valid_land[rows, 'land_in_acre'] = valid_land[rows, 's8aq4'] * 0.00625 #1 square pole = 0.00625 ac 
  }
  if (valid_land[rows, 's8aq3'] == 3) {
    valid_land[rows, 'land_in_acre'] = valid_land[rows, 's8aq4'] * 0.00918274 #1 square rope = 0.00918274 ac
  }
}

## then we need to join the valid_land table with the profit table
land_and_profit <- merge(valid_land, agg2, by = c('clust','nh'))
land_and_profit['profit_per_acre'] = land_and_profit['agri1c'] / land_and_profit['land_in_acre']
land_and_profit = land_and_profit[c('clust', 'nh', 'land_in_acre', 'agri1c', 'profit_per_acre')]
write.csv(land_and_profit, file = 'land_and_profit.csv')

##as we can see in the csv file, there are extreme values in "profit_per_acre" column,
#especially the long right tail
#thus, we exclude the outliers
a <- land_and_profit[land_and_profit$profit_per_acre > -110000.3, ] # lower limit
c <- a[a$profit_per_acre <= 341986.8, ] # upper limit
nrow(c)
hist(c$profit_per_acre)
min(c$profit_per_acre)
