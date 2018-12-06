# figure out the match between eanum and clust
sec0a <- read_dta("raw_data/sec0a.dta")
EAnumber<- sec0a %>%
  select (eanum,clust) %>%
  distinct()
# calculate the average unit profit for each EA
average_profit <- land_and_profit %>%
  group_by(clust) %>%
  summarise(mean_profit_per_acre = mean(profit_per_acre)) %>%
  merge(EAnumber,by = "clust")
### cs1 ###

## obtain mean value if there is more than 1 observation in the same ID
cs1 <- read_dta("community/cs1.dta")
cs1[cs1 >= 1.0e+100] <- NA_real_
cs1 <-aggregate(cs1[,-c(1:3)],by = list(cs1$eanum),mean, na.rm = TRUE)%>%
  rename(eanum = Group.1)
# factor and merge the catagory dataframe with average profit 
myFun <- function(x){
  factor(x, exclude = NA)
}
cs1_profit_per_acre.catagory<- cs1 %>%
  apply(2,myFun) %>% 
  merge(average_profit, by ="eanum")  %>% # merge with profit 
  distinct() %>%
  select(-s1q5,-s1q1,-clust,-eanum)
#linear regression 
cs1_catagory_model <- lm(mean_profit_per_acre ~ ., data =cs1_profit_per_acre.catagory)
summary(cs1_catagory_model)
# Stepwise regression model
cs1.catagory.step.model <- stepAIC(cs1_catagory_model, direction = "both", trace = FALSE)
summary(cs1.catagory.step.model)

# merge the numeric dataframe with average profit 
cs1_profit_per_acre.numeric<- cs1 %>%
  merge(average_profit, by ="eanum")  %>% # merge with profit 
  distinct() %>%
  select(s1q1,s1q5,mean_profit_per_acre) 
# linear regression 
cs1_numeric_model <- lm(mean_profit_per_acre ~., data =cs1_profit_per_acre.numeric,na.action = na.exclude)
summary(cs1_numeric_model)

# summary  : no good variables 
####################################################

### cs2 ###

# convert ALL THE NA'S NUMBER to 0 
cs2 <- read_dta("community/cs2.dta")
cs2[cs2 >= 1.0e+100] <- 0
## obtain mean value if there is more than 2 observation in the same ID
cs2 <-aggregate(cs2[,-c(1:3)],by = list(cs2$eanum),median, na.rm = TRUE)%>%
    rename(eanum = Group.1)
 
# all the catagory variables need to be factored 
cs2_profit_per_acre.catagory<- cs2 %>%
  apply(2,factor) %>% 
  merge(average_profit, by ="eanum")  %>% # merge with profit 
  distinct()  %>%
  select(-s2q5,-s2q7,-s2q16,-s2q18,-s2q21a,-s2q21b,-s2q22,-s2q25,-s2q26a,-s2q30a,-s2q35a,-clust) 

# merge the numeric dataframe with average profit 
cs2_all_variables <- cs2 %>%
  merge(average_profit, by ="eanum")  %>% # merge with profit 
  distinct()  %>%
  select(eanum,s2q7,s2q16,s2q18,s2q22,s2q26a,s2q30a,s2q35a,mean_profit_per_acre)  %>%
  merge(cs2_profit_per_acre.catagory, by = c("eanum","mean_profit_per_acre")) 

cs2_model <- lm(mean_profit_per_acre ~ ., data =cs2_all_variables)
summary(cs2_model)
# Stepwise regression model
cs2.step.model <- stepAIC(cs2_model, direction = "both", trace = FALSE)
summary(cs2.step.model)

cs2_relative_varibles <- cs2_all_variables %>%
  select(s2q7,s2q16,s2q1a,s2q1b,s2q1c,s2q6,s2q12,s2q14,s2q15,s2q18,s2q20,s2q24,s2q28,s2q29,s2q31,s2q34a,s2q34b,mean_profit_per_acre)

cs2_relative_model <- lm(mean_profit_per_acre ~ ., data =cs2_relative_varibles )
summary(cs2_model)
# Stepwise regression model
cs2.step.model <- stepAIC(cs2_relative_model, direction = "both", trace = FALSE)
summary(cs2.step.model)
####################################################
#s2q1,s2q28,s2q31 ,s3q12,s3q9,s3q19,s3q20,s3q32,s5bq9,s5bq11,s5bq23,s5bq24,s5bq13()
### cs3 ###

## obtain mean value if there is more than 2 observation in the same ID

# convert ALL THE NA'S NUMBER to 0 
cs3 <- read_dta("community/cs3.dta")
cs3[cs3 >= 1.0e+100] <- 0
## obtain mean value if there is more than 2 observation in the same ID
cs3 <-aggregate(cs3[,-c(1:3)],by = list(cs3$eanum),median, na.rm = TRUE)%>%
  rename(eanum = Group.1)

# all the catagory variables need to be factored 
cs3_profit_per_acre.catagory<- cs3 %>%
  apply(2,factor) %>% 
  merge(average_profit, by ="eanum")  %>% # merge with profit 
  distinct()  %>%
  select(-s3q2,-s3q5a,-s3q4a,-s3q4b,-s3q4c,-s3q5b,-s3q5c,-s3q6a,-s3q6b,-s3q6c,-s3q7a,-s3q7b,-s3q7c,-s3q12,-s3q17,-s3q18,-s3q27,-s3q21,-s3q26,-s3q30,-s3q31a,-s3q31b,-s3q31c,-clust)

# merge the numeric dataframe with average profit 
cs3_all_variables <- cs3 %>%
  merge(average_profit, by ="eanum")  %>% # merge with profit 
  distinct()  %>%
  select(eanum, s3q2,s3q12,s3q17,s3q21,s3q31a,s3q31b,s3q31c,mean_profit_per_acre) %>%
  merge(cs3_profit_per_acre.catagory, by = c("eanum","mean_profit_per_acre")) 

cs3_model <- lm(mean_profit_per_acre ~ ., data =cs3_all_variables)
summary(cs3_model)
# Stepwise regression model
cs3.step.model <- stepAIC(cs3_model, direction = "both", trace = FALSE)
summary(cs3.step.model)






### cs4a ###

## obtain mean value if there is more than 4a observation in the same ID
cs4a <- read_dta("community/cs4a.dta")
cs4a[cs4a >= 1.0e+100] <- NA_real_
cs4a <-aggregate(cs4a[,-c(1:3)],by = list(cs4a$eanum),mean, na.rm = TRUE)%>%
  rename(eanum = Group.1)
# factor and merge the catagory dataframe with average profit 
myFun <- function(x){
  factor(x, exclude = NA)
}
cs4a_profit_per_acre.catagory<- cs4a %>%
  apply(2,myFun) %>% 
  merge(average_profit, by ="eanum")  %>% # merge with profit 
  distinct() %>%
  select(-s4aq2,-s4aq4a,-s4aq4b,-clust,-eanum)

#linear regression 
cs4a_catagory_model <- lm(mean_profit_per_acre ~ ., data =cs4a_profit_per_acre.catagory)
summary(cs4a_catagory_model)
# Stepwise regression model
cs4a.catagory.step.model <- stepAIC(cs4a_catagory_model, direction = "both", trace = FALSE)
summary(cs4a.catagory.step.model)

# merge the numeric dataframe with average profit 
cs4a_profit_per_acre.numeric<- cs4a %>%
  merge(average_profit, by ="eanum")  %>% # merge with profit 
  distinct() %>%
  select(s4aq2,s4aq4a,s4aq4b,mean_profit_per_acre) 
# linear regression 
cs4a_numeric_model <- lm(mean_profit_per_acre ~., data =cs4a_profit_per_acre.numeric,na.action = na.exclude)
summary(cs4a_numeric_model)
# summary : no good variable  
####################################################

## cs4b 
## obtain mean value if there is more than 4b observation in the same ID
cs4b <- read_dta("community/cs4b.dta")
cs4b[cs4b >= 1.0e+100] <- NA_real_
cs4b <-aggregate(cs4b[,-c(1:3)],by = list(cs4b$eanum),mean, na.rm = TRUE)%>%
  rename(eanum = Group.1)

# factor and merge the catagory dataframe with average profit 
myFun <- function(x){
  factor(x, exclude = NA)
}
cs4b_profit_per_acre.catagory<- cs4b %>%
  apply(2,myFun) %>% 
  merge(average_profit, by ="eanum")  %>% # merge with profit 
  distinct() %>%
  select(-s4bq6,-s4bq8a,-s4bq8b,-s4bq9,-clust,-eanum)
summary(cs4b_profit_per_acre.catagory)
#linear regression 
cs4b_catagory_model <- lm(mean_profit_per_acre ~ ., data =cs4b_profit_per_acre.catagory)
summary(cs4b_catagory_model)
# Stepwise regression model
cs4b.catagory.step.model <- stepAIC(cs4b_catagory_model, direction = "both", trace = FALSE)
summary(cs4b.catagory.step.model)

# merge the numeric dataframe with average profit 
cs4b_profit_per_acre.numeric<- cs4b %>%
  merge(average_profit, by ="eanum")  %>% # merge with profit 
  distinct() %>%
  select(s4bq6,s4bq8a,s4bq8b,s4bq9,mean_profit_per_acre) 
# linear regression 
cs4b_numeric_model <- lm(mean_profit_per_acre ~., data =cs4b_profit_per_acre.numeric,na.action = na.exclude)
summary(cs4b_numeric_model)
## summary: no good variables 
####################################################


## cs4c 
# all the variables is catagorical 
## obtain mean value if there is more than 4c observation in the same ID
cs4c <- read_dta("community/cs4c.dta")
cs4c[cs4c >= 1.0e+100] <- NA_real_
cs4c <-aggregate(cs4c[,-c(1:3)],by = list(cs4c$eanum),mean, na.rm = TRUE)%>%
  rename(eanum = Group.1)
names(cs4c)
# factor and merge the catagory dataframe with average profit 
myFun <- function(x){
  factor(x, exclude = NA)
}
cs4c_profit_per_acre.catagory<- cs4c %>%
  apply(2,myFun) %>% 
  merge(average_profit, by ="eanum")  %>% # merge with profit 
  distinct() %>%
  select(-clust,-eanum)
summary(cs4c_profit_per_acre.catagory)
#linear regression 
cs4c_catagory_model <- lm(mean_profit_per_acre ~ ., data =cs4c_profit_per_acre.catagory)
summary(cs4c_catagory_model)
# Stepwise regression model
cs4c.catagory.step.model <- stepAIC(cs4c_catagory_model, direction = "both", trace = FALSE)
summary(cs4c.catagory.step.model)

# summary no good variables  
####################################################
## cs5a   ## all the variables is catagorical 
## obtain mean value if there is more than 5a observation in the same ID
cs5a <- read_dta("community/cs5a.dta")
cs5a[cs5a >= 1.0e+100] <- NA_real_
cs5a <-aggregate(cs5a[,-c(1:3)],by = list(cs5a$eanum),mean, na.rm = TRUE)%>%
  rename(eanum = Group.1)

# factor and merge the catagory dataframe with average profit 
myFun <- function(x){
  factor(x, exclude = NA)
}
cs5a_profit_per_acre.catagory<- cs5a %>%
  apply(2,myFun) %>% 
  merge(average_profit, by ="eanum")  %>% # merge with profit 
  distinct() %>%
  select(-s5aq3a,-s5aq3b,-s5aq3c,-s5aq3d,-clust,-eanum)

#linear regression 
cs5a_catagory_model <- lm(mean_profit_per_acre ~ ., data =cs5a_profit_per_acre.catagory)
summary(cs5a_catagory_model)
# Stepwise regression model
cs5a.catagory.step.model <- stepAIC(cs5a_catagory_model, direction = "both", trace = FALSE)
summary(cs5a.catagory.step.model)
#######################


# convert ALL THE NA'S NUMBER to 0 
cs5b <- read_dta("community/cs5b.dta")
cs5b[cs5b >= 1.0e+100] <- 0
## obtain mean value if there is more than 2 observation in the same ID
cs5b <-aggregate(cs5b[,-c(1:3)],by = list(cs5b$eanum),median, na.rm = TRUE)%>%
  rename(eanum = Group.1)

# all the catagory variables need to be factored 
cs5b_profit_per_acre.catagory<- cs5b %>%
  apply(2,factor) %>% 
  merge(average_profit, by ="eanum")  %>% # merge with profit 
  distinct()  %>%
  select(-s5bq6,-s5bq13,-s5bq22a,-s5bq22b,-s5bq22c,-s5bq22e,-s5bq22f,-s5bq22g,-s5bq22g,-s5bq22h,-s5bq22i,-s5bq22j,
         -s5bq22k,-s5bq22l,-s5bq25a,-s5bq25b,-s5bq25d,-clust)

# merge the numeric dataframe with average profit 
cs5b_all_variables <- cs5b %>%
  merge(average_profit, by ="eanum")  %>% # merge with profit 
  distinct()  %>%
  select(eanum,s5bq6,s5bq13,s5bq25a,s5bq25b,s5bq25d,mean_profit_per_acre) %>%
  merge(cs5b_profit_per_acre.catagory, by = c("eanum","mean_profit_per_acre"))  %>%
  select(-eanum)

cs5b_model <- lm(mean_profit_per_acre ~ ., data =cs5b_all_variables)
summary(cs5b_model)
# Stepwise regression model
cs5b.step.model <- stepAIC(cs5b_model, direction = "both", trace = FALSE)
summary(cs5b.step.model)

cs5b_relative_varibles <- cs5b_all_variables %>%
 
cs5b_relative_model <- lm(mean_profit_per_acre ~ ., data =cs5b_relative_varibles )
summary(cs5b_model)
# Stepwise regression model
cs5b.step.model <- stepAIC(cs5b_relative_model, direction = "both", trace = FALSE)
summary(cs5b.step.model)







## cs5b 
## obtain mean value if there is more than 5b observation in the same ID
cs5b <- read_dta("community/cs5b.dta")
cs5b[cs5b >= 1.0e+100] <- NA_real_
cs5b <-aggregate(cs5b[,-c(1:3)],by = list(cs5b$eanum),mean, na.rm = TRUE)%>%
  rename(eanum = Group.1)

# factor and merge the catagory dataframe with average profit 
myFun <- function(x){
  factor(x, exclude = NA)
}
cs5b_profit_per_acre.catagory<- cs5b %>%
  apply(2,myFun) %>% 
  merge(average_profit, by ="eanum")  %>% # merge with profit 
  distinct() %>%
cs5b_catagory <- cs5b_profit_per_acre.catagory %>%
  select(-s5bq6,-s5bq13,-s5bq22a,-s5bq22b,-s5bq22c,-s5bq22e,-s5bq22f,-s5bq22g,-s5bq22g,-s5bq22h,-s5bq22i,-s5bq25a,-s5bq25b,-s5bq25d,-clust,-eanum)
summary(cs5b_profit_per_acre.catagory)
#linear regression 
cs5b_catagory_model <- lm(mean_profit_per_acre ~ ., data =cs5b_profit_per_acre.catagory)
summary(cs5b_catagory_model)
# Stepwise regression model
cs5b.catagory.step.model <- stepAIC(cs5b_catagory_model, direction = "both", trace = FALSE)
summary(cs5b.catagory.step.model)

# merge the numeric dataframe with average profit 
cs5b_profit_per_acre.numeric<- cs5b %>%
  merge(average_profit, by ="eanum")  %>% # merge with profit 
  distinct() %>%
  select(s5bq6,s5bq13,s5bq22a,s5bq22b,s5bq22c,s5bq22e,s5bq22f,s5bq22g,s5bq22g,s5bq22h,s5bq22i,s5bq25a,s5bq25b,s5bq25d,mean_profit_per_acre) 
# linear regression 
cs5b_numeric_model <- lm(mean_profit_per_acre ~., data =cs5b_profit_per_acre.numeric,na.action = na.exclude)
summary(cs5b_numeric_model)
## summary: no good variables 
####################################################

