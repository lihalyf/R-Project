sec2a <- read_dta("~/Downloads/glss4/sec2a.dta")
sec2a <- subset(sec2a, pid == 1)
sec2a <- sec2a[c('nh', 'clust', 's2aq1')]
EDUCATION <- sec2a %>%
  rename(ever_attend_school = s2aq1)

sec2c <- read_dta("~/Downloads/glss4/sec2c.dta")
sec2c <- subset(sec2c, pid == 1)
sec2c <- sec2c[c('nh', 'clust', 's2cq2', 's2cq5')]
Literacy_Apprenticeship <- sec2c %>%
  rename(Read_Ghanaian_language = s2cq2,
         Can_do_written_calculation = s2cq5)

sec4b <- read_dta("~/Downloads/glss4/sec4b.dta")
sec4b <- subset(sec4b, pid == 1)
sec4b <- sec4b[c('nh', 'clust', 's4bq8', 's4bq9')]
EMPLOYMENT_AND_TIME_USE_A <- sec4b %>%
  rename(Employment_status = s4bq8,
         Work_for_whom_in_main_job= s4bq9)

sec4c <- read_dta("~/Downloads/glss4/sec4c.dta")
sec4c <- subset(sec4c, pid == 1)
sec4c <- sec4c[c('nh', 'clust', 's4cq3', 's4cq5', 's4cq12')]
EMPLOYMENT_AND_TIME_USE_B <- sec4c %>%
  rename(Doing_same_work = s4cq3,
         Father_do_same_work = s4cq5,
         Work_for_whom = s4cq12)

sec4j <- read_dta("~/Downloads/glss4/sec4j.dta")
sec4j <- subset(sec4j, pid == 1)
sec4j <- sec4j[c('nh', 'clust', 's4jq1', 's4jq13')]
EMPLOYMENT_AND_TIME_USE_C <- sec4j %>%
  rename(Fetching_wood = s4jq1,
         Dispose_garbage = s4jq13)

sec5 <- read_dta("~/Downloads/glss4/sec5.dta")
sec5 <- subset(sec5, pid == 1)
sec5 <- sec5[c('nh', 'clust', 's5q1')]
MIGRATION <- sec5 %>%
  rename(Born_here = s5q1)

sec6 <- read_dta("~/Downloads/glss4/sec6.dta")
sec6 <- sec6[c('nh', 'clust', 's6q1', 's6q4', 's6q8')]
Farm_Nonfarm_Enterprises <- sec6 %>%
  rename(Own_operate_farm = s6q1,
         Fish_process_for_sale = s6q4,
         Operate_own_business = s6q8)

sec10b <- read_dta("~/Downloads/glss4/sec10b.dta")
sec10b <- sec10b[c('nh', 'clust', 'bexpcd2')]
NONFARM_ENTERPRISES_1 <- sec10b %>%
  rename(Expenditure_item_code = bexpcd2)

sec10c <- read_dta("~/Downloads/glss4/sec10c.dta")
sec10c <- sec10c[c('nh', 'clust', 's10cq1c')]
NONFARM_ENTERPRISES_2 <- sec10c %>%
  rename(Does_enterprise3_own_item = s10cq1c)

sec7 <- read_dta("~/Downloads/glss4/sec7.dta")
sec7 <- sec7[c('nh', 'clust', 's7eq1')]
Housing <- sec7 %>%
  rename(Outside_wall_material = s7eq1)

sec8hid <- read_dta("~/Downloads/glss4/sec8hid.dta")
sec8hid <- sec8hid[c('nh', 'clust', 'region')]
AGRICULTURE_12 <- sec8hid %>%
  rename(Region_ID = region)


sec12a2 <- read_dta("~/Downloads/glss4/sec12a2.dta")
sec12a2 <- sec12a2[c('nh', 'clust', 's12aq5')]
INCOME_EXPENDITURE <- sec12a2 %>%
  rename(Source_of_loan = s12aq5)

#Combine all the variables we selected
all_dv_2 <- left_join(land_and_profit, EDUCATION, by = c('nh', 'clust'))
all_dv_2 <- left_join(all_dv_2, Literacy_Apprenticeship, by = c('nh', 'clust'))
all_dv_2 <-  left_join(all_dv_2, Literacy_Apprenticeship, by = c('nh', 'clust'))
all_dv_2 <-  left_join(all_dv_2, EMPLOYMENT_AND_TIME_USE_A, by = c('nh', 'clust'))
all_dv_2 <-  left_join(all_dv_2, EMPLOYMENT_AND_TIME_USE_B, by = c('nh', 'clust')) 
all_dv_2 <-  left_join(all_dv_2, EMPLOYMENT_AND_TIME_USE_C, by = c('nh', 'clust')) 
all_dv_2 <-  left_join(all_dv_2, MIGRATION, by = c('nh', 'clust')) 
all_dv_2 <-  merge(all_dv_2, Housing, by = c('nh', 'clust')) 
all_dv_2 <-  merge(all_dv_2, AGRICULTURE_12, by = c('nh', 'clust')) 

all_dv_3 <- left_join(all_dv, all_dv_2, by = c('nh', 'clust'))
all_dv_3[is.na(all_dv_3)] <- 0

write.csv(all_dv_3,file="~/econ_5100/all_dv_3.csv",quote=F,row.names = F)

for (col in c ('Food_01', 'Food_02', 'Food_08', 'ever_attend_school', 'Can_do_written_calculation.x',
               'Fetching_wood', 'Dispose_garbage', 'Born_here', 'Father_do_same_work', 'Doing_same_work')){
  for (row in 1:nrow(all_dv_3)) {
    if ((all_dv_3)[row, col] > 1) {
      all_dv_3[row, col] = 0
    }
  }
}



