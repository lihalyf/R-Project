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


### cs2 ###

# convert ALL THE NA'S NUMBER to 0 
cs2 <- read_dta("community/cs2.dta")
cs2[cs2 >= 1.0e+100] <- 0
myfunc <- function(x){
  round(mean(x))
}
## obtain mean value if there is more than 2 observation in the same ID
cs2 <-aggregate(cs2[,-c(1:3)],by = list(cs2$eanum),FUN= myfunc)%>%
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
  merge(cs2_profit_per_acre.catagory, by = c("eanum","mean_profit_per_acre")) %>%
  select(-eanum,-s2q30b,-s2q31,-s2q32,-s2q29,-s2q33,-s2q11,-s2q9,-s2q8,-s2q12,-s2q13,-s2q38a,-s2q26b,-s2q38c,-s2q20,-s2q23,-s2q15)


cs2_model <- lm(mean_profit_per_acre ~ ., data =cs2_all_variables)
#summary(cs2_model)
# Stepwise regression model
cs2.step.model <- stepAIC(cs2_model, direction = "both", trace = FALSE)
summary(cs2.step.model)



###cs3 ### 
## obtain mean value if there is more than 2 observation in the same ID

# convert ALL THE NA'S NUMBER to 0 
cs3 <- read_dta("community/cs3.dta")
cs3[cs3 >= 1.0e+100] <- 0
## obtain mean value if there is more than 2 observation in the same ID
cs3 <-aggregate(cs3[,-c(1:3)],by = list(cs3$eanum),FUN = myfunc)%>%
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
  merge(cs3_profit_per_acre.catagory, by = c("eanum","mean_profit_per_acre")) %>%
  select(-eanum)

cs3_model <- lm(mean_profit_per_acre ~ ., data =cs3_all_variables)
#summary(cs3_model)
# Stepwise regression model
cs3.step.model <- stepAIC(cs3_model, direction = "both", trace = FALSE)
summary(cs3.step.model)


### cs5b###
# convert ALL THE NA'S NUMBER to 0 
cs5b <- read_dta("community/cs5b.dta")
cs5b[cs5b >= 1.0e+100] <- 0
## obtain mean value if there is more than 2 observation in the same ID
cs5b <-aggregate(cs5b[,-c(1:3)],by = list(cs5b$eanum),FUN = myfunc)%>%
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
  merge(cs5b_profit_per_acre.catagory, by = c("eanum","mean_profit_per_acre")) %>%
  select(-eanum)


cs5b_model <- lm(mean_profit_per_acre ~ ., data =cs5b_all_variables)
#summary(cs5b_model)
# Stepwise regression model
cs5b.step.model <- stepAIC(cs5b_model, direction = "both", trace = FALSE)
summary(cs5b.step.model)



