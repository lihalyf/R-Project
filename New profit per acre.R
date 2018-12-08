###Wei's part- profit per acre:
library(haven)
library(here)
##land data
sec8a1 <- read_dta('glss4/sec8a1.dta')
land <- subset(sec8a1[c('clust', 'nh', 's8aq3', 's8aq4')])
agg2 <- read_dta(here('glss4/aggregates/agg2.dta'))

valid_rows_land <- land['s8aq4'] <= 4.222000e+03 
# 4.222000e+03 is the maximum reasonable value in the column 's8aq4', then we excluded the strange value '1.749801e+100'

valid_rows_unit <- land['s8aq3'] < 4
# 4 is the largest number to represent unit. larger than 4 is wrong.

valid_land <- land[valid_rows_land, ]
valid_land <- land[valid_rows_unit, ]

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

View(land_and_profit)