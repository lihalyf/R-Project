library(tidyverse)
library(haven)
library(MASS)
library(caret)
library(leaps)
library(here)
library(dplyr)
##community service###
#### variables from cummunity service ####
# figure out the match between eanum and clust
sec0a <- read_dta("raw_data/sec0a.dta")
EAnumber<- sec0a %>%
  select (eanum,clust) %>%
  distinct()


## read data 
cs1 <- read_dta("community/cs1.dta")
cs2 <- read_dta("community/cs2.dta")
cs3 <- read_dta("community/cs3.dta")
cs5b <- read_dta("community/cs5b.dta")
# convert outliers to 0 
cs1[cs1>= 1.0e+100] <- 0
cs2[cs2>= 1.0e+100] <- 0
cs3[cs3>= 1.0e+100] <- 0
cs5b[cs5b>= 1.0e+100] <- 0

#categorical variables and pick up all the categorical variables from cs1,cs2,cs3,cs5b
cs1_catagory  <- cs1 %>%
  select(eanum,s1q4) %>%
  rename(move = s1q4)
cs2_catagory  <- cs2 %>%
  select(eanum,s2q8,s2q10,s2q14,s2q17,
         s2q27,s2q33,s2q34a,s2q34b) %>%
  rename(has_restaurant = s2q14,has_pipe = s2q10,migrate1_activity= s2q34a, 
         migrate2_activity = s2q34b, has_electric = s2q8,migrant_from = s2q33)
cs3_catagory  <- cs3 %>%
  select(eanum,s3q1,s3q9,s3q14,s3q16,s3q20,s3q23,s3q24,s3q29) %>%
  rename(school_gender = s3q16,has_seniorsch = s3q20, sch_private = s3q24, 
         girl_prop = s3q9,junior_loc=s3q14,has_adultprogram = s3q29)
cs5b_catagory  <- cs5b %>%
  select(eanum,s5bq5,s5bq7,s5bq10,s5bq12,s5bq14,s5bq15,s5bq16,s5bq17,
         s5bq20,s5bq21,s5bq23)  %>%
  rename(cooperative = s5bq10, agriculture_coop = s5bq12, rice_husking = s5bq14,
         use_insecticides = s5bq16,sharecropper = s5bq20, sharecropper_prop = s5bq21, 
         agent_visit = s5bq7 )

## obtain mean value if there is more than 2 observation in the same ID 
# use  round(mean) function to aggregate categorical variables.
myfunc <- function(x){
  round(mean(x))
}
cs1_catagory <-aggregate(cs1_catagory[,-1],by = list(cs1_catagory$eanum),FUN= myfunc)%>%
  rename(eanum = Group.1)
cs2_catagory <-aggregate(cs2_catagory[,-1],by = list(cs2_catagory$eanum),FUN= myfunc)%>%
  rename(eanum = Group.1)
cs3_catagory <-aggregate(cs3_catagory[,-1],by = list(cs3_catagory$eanum),FUN= myfunc)%>%
  rename(eanum = Group.1)
cs5b_catagory <-aggregate(cs5b_catagory[,-1],by = list(cs5b_catagory$eanum),FUN= myfunc)%>%
  rename(eanum = Group.1)

# merge all the categorical variables 
cs_catagory <- merge(cs1_catagory,cs2_catagory,by = "eanum",all =TRUE) %>%
  distinct() %>%
  merge(cs3_catagory,by = "eanum",all =TRUE) %>%
  distinct() %>%
  merge(cs5b_catagory,by = "eanum",all =TRUE) %>%
  distinct() 

# create dummies for each categorical variables
cs_catagory  <- cs_catagory  %>%
  gather(question, answer,-eanum) %>%   
  unite("names",c("question","answer"))%>%   # unite 2 columns into one columns 
  mutate(value_1 =  1) %>%   # add a column with the value 1 
  spread(key = "names", value = value_1) %>%  # set 2 column to multiple columns, get dummy 
  select(-contains("_0"),-contains("_NA"),-move_4,-has_pipe_2,-has_electric_2,-has_restaurant_2,-s2q17_2,
         -s2q27_2,-migrate1_activity_9,-migrate2_activity_9,-s3q1_2,-girl_prop_6,-junior_loc_2,
         -has_seniorsch_2,-s3q23_2,-has_adultprogram_2, -s5bq5_2,-agent_visit_2,-cooperative_2,-agriculture_coop_2,-rice_husking_2,
         -s5bq15_2,-use_insecticides_2,-s5bq17_2,-sharecropper_2,-sharecropper_prop_4,-s5bq23_2 
  )   # delete one dummy of each categorical variable
cs_catagory [is.na(cs_catagory )] <- 0  # convert NA to 0 

# numeric variables and pick up all numeric varibales from cs1,cs2,cs3,cs5b
cs1_numeric <- select(cs1, eanum,s1q1)
cs2_numeric <- select(cs2, eanum,s2q22,s2q25)
cs3_numeric <- select(cs3, eanum,s3q12,s3q21)%>%
  rename(distance_seniorschool = s3q21)
cs5b_numeric <- select(cs5b, eanum,s5bq6,s5bq13) %>%
  rename(distance_agricenter = s5bq6,tracer_num = s5bq13)

## obtain mean value if there is more than 2 observation in the same ID
# use mean to aggregate 
cs1_numeric <-aggregate(cs1_numeric[,-1],by = list(cs1_numeric$eanum),mean)%>%
  rename(eanum = Group.1)
cs2_numeric <-aggregate(cs2_numeric[,-1],by = list(cs2_numeric$eanum),mean)%>%
  rename(eanum = Group.1)
cs3_numeric <-aggregate(cs3_numeric[,-1],by = list(cs3_numeric$eanum),mean)%>%
  rename(eanum = Group.1)
cs5b_numeric <-aggregate(cs5b_numeric[,-1],by = list(cs5b_numeric$eanum),mean)%>%
  rename(eanum = Group.1)

#merge all the numeric table 
cs_numeric <- merge(cs1_numeric,cs2_numeric,by = "eanum",all =TRUE) %>%
  distinct() %>%
  merge(cs3_numeric,by = "eanum",all =TRUE) %>%
  distinct() %>%
  merge(cs5b_numeric,by = "eanum",all =TRUE) %>%
  distinct() 

# merge all the variables and clust
cs <- left_join(cs_catagory,cs_numeric, by= "eanum") %>%
  merge(EAnumber,by = "eanum")
write_csv(cs,here("cleaned_data","cs1.csv")) # save into csv file 
# if you want to remove unnecessary data frame 
rm(cs1_catagory,cs1_numeric,cs1,cs2,cs2_catagory,cs2_numeric,cs3,cs3_catagory,cs3_numeric,
   cs5b,cs5b_catagory,cs5b_numeric,sec0a,EAnumber,cs_catagory,cs_numeric)