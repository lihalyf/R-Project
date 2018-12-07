#### variables from cummunity service ####
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
#catagory variables 
cs1_catagory  <- cs1 %>%
  select(eanum,s1q4)
cs2_catagory  <- cs2 %>%
  select(eanum,s2q6,s2q8,s2q10,s2q14,s2q15,s2q17,s2q19,s2q20,s2q23,
         s2q27,s2q28,s2q29,s2q32,s2q33,s2q34a,s2q34b,s2q37)
cs3_catagory  <- cs3 %>%
  select(eanum,s3q1,s3q8,s3q9,s3q11,s3q14,s3q15,s3q16,s3q20,s3q23,s3q24,s3q25,s3q29)
cs5b_catagory  <- cs5b %>%
  select(eanum,s5bq5,s5bq7,s5bq10,s5bq12,s5bq14,s5bq15,s5bq16,s5bq17,
         s5bq18,s5bq19,s5bq20,s5bq21,s5bq23)
myfunc <- function(x){
  round(mean(x))
}
## obtain mean value if there is more than 2 observation in the same ID
cs1_catagory <-aggregate(cs1_catagory[,-1],by = list(cs1_catagory$eanum),FUN= myfunc)%>%
  rename(eanum = Group.1)
cs2_catagory <-aggregate(cs2_catagory[,-1],by = list(cs2_catagory$eanum),FUN= myfunc)%>%
  rename(eanum = Group.1)
cs3_catagory <-aggregate(cs3_catagory[,-1],by = list(cs3_catagory$eanum),FUN= myfunc)%>%
  rename(eanum = Group.1)
cs5b_catagory <-aggregate(cs5b_catagory[,-1],by = list(cs5b_catagory$eanum),FUN= myfunc)%>%
  rename(eanum = Group.1)
# merge all the catagory variables 
cs_catagory <- merge(cs1_catagory,cs2_catagory,by = "eanum",all =TRUE) %>%
  distinct() %>%
  merge(cs3_catagory,by = "eanum",all =TRUE) %>%
  distinct() %>%
  merge(cs5b_catagory,by = "eanum",all =TRUE) %>%
  distinct() 
# convert dummies to 0 & 1
cs_catagory  <- cs_catagory  %>%
  gather(variable, var_value,-eanum) %>%
  unite("col",c("variable","var_value"))%>%
  mutate(var =  1) %>%
  spread(key = "col", value = var) %>%
  select(-contains("_0"),-contains("_NA"),-s1q4_4,-s2q10_2,-s2q6_2,-s2q8_2,-s2q14_2,-s2q15_2,-s2q17_2,-s2q19_2,-s2q20_2,-s2q23_2,
         -s2q27_2,-s2q28_3,-s2q29_5,-s2q32_2,-s2q34a_9,-s2q34b_9,-s2q37_3,-s3q1_2,-s3q8_5,-s3q9_6,-s3q11_2,-s3q14_2,
         -s3q20_2,-s3q23_2,-s3q29_2, -s5bq5_2,-s5bq7_2,-s5bq10_2,-s5bq12_2,-s5bq14_2,-s5bq15_2,-s5bq16_2,-s5bq17_2,-s5bq18_3,-s5bq19_2,-s5bq20_2,
         -s5bq21_4,-s5bq23_2 
  )
cs_catagory [is.na(cs_catagory )] <- 0

# numeric variables 
cs1_numeric <- select(cs1, eanum,s1q1)
cs2_numeric <- select(cs2, eanum,s2q5,s2q16,s2q18,s2q22,s2q25)
cs3_numeric <- select(cs3, eanum,s3q2,s3q12,s3q21,s3q31a,s3q31b,s3q31c)
cs5b_numeric <- select(cs5b, eanum,s5bq6,s5bq13)
## obtain mean value if there is more than 2 observation in the same ID
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

# merge all the variables and profit 
cs <- merge(cs_catagory,cs_numeric, by= "eanum") %>%
  merge(average_profit,by = "eanum")


