library(haven)
library(dplyr)
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

sec7 <- read_dta("~/Downloads/glss4/sec7.dta")
sec7 <- sec7[c('nh', 'clust', 's7eq1', 's7eq5')]
Housing <- sec7 %>%
  rename(Outside_wall_material = s7eq1,
         Room_measurements= s7eq5)

sec8a1 <- read_dta("~/Downloads/glss4/sec8a1.dta")
AGRICULTURE_1 <- sec8a1 %>%
  select(c('nh', 'clust', 's8aq3', 's8aq4')) %>%
  rename(Unit_of_plot_area = s8aq3,
         Land_owned_by_HH = s8aq4)

sec8a2 <- read_dta("~/Downloads/glss4/sec8a2.dta")
sec8a2 <- sec8a2[c('nh', 'clust', 's8aq24', 's8aq27')]
AGRICULTURE_2 <- sec8a2 %>%
  rename(Any_livestock_been_sold = s8aq24,
         Any_livestock_been_bought = s8aq27)


sec8a3 <- read_dta("~/Downloads/glss4/sec8a3.dta")
sec8a3 <- sec8a3[c('nh', 'clust', 'eqcdown', 's8aq33')]
AGRICULTURE_3 <- sec8a3 %>%
  rename(agric_equipment_code = eqcdown,
         Member_own_any_equipment = s8aq33)

sec8b <- read_dta("~/Downloads/glss4/sec8b.dta")
sec8b <- sec8b[c('nh', 'clust', 's8bq5', 's8bq4a', 's8bq4b')]
AGRICULTURE_4 <- sec8b %>%
  rename(Farm_owned_by_HH_member = s8bq5,
         Farm_land_size = s8bq4a,
         Unit_of_measure = s8bq4b)

sec8c1 <- read_dta("~/Downloads/glss4/sec8c1.dta")
sec8c1 <- sec8c1[c('nh', 'clust', 's8cq14a', 's8cq14b')]
AGRICULTURE_5 <- sec8c1 %>%
  rename(Harvest_processed_by_HH = s8cq14a,
         Unit_of_measure = s8cq14b)

AGRICULTURE_6 <- read_dta("~/Downloads/glss4/sec8c2.dta")
AGRICULTURE_6 <- AGRICULTURE_6 %>%
  select(c('nh', 'clust', 's8cq20', 's8cq24')) %>%
  rename(Harvest_any_root_crops = s8cq20,
         Sell_any_root_harvested = s8cq24)

sec8d <- read_dta("~/Downloads/glss4/sec8d.dta")
sec8d <- sec8d[c('nh', 'clust', 'crpseacd')]
AGRICULTURE_7 <- sec8d %>%
  rename(Crop_code = crpseacd)

sec8e <- read_dta("~/Downloads/glss4/sec8e.dta")
sec8e <- sec8e[c('nh', 'clust', 's8eq2')]
AGRICULTURE_8 <- sec8e %>%
  rename(Honey = s8eq2)

sec8f <- read_dta("~/Downloads/glss4/sec8f.dta")
sec8f <- sec8f[c('nh', 'clust', 's8fq3')]
AGRICULTURE_9 <- sec8f %>%
  rename(Source_of_item = s8fq3)

sec8g <- read_dta("~/Downloads/glss4/sec8g.dta")
sec8g <- sec8g[c('nh', 'clust', 'proagrcd', 's8gq10')]
AGRICULTURE_10 <- sec8g%>%
  rename(Processed_food_code = proagrcd,
         Sell_any_processed_food = s8gq10)

sec8h <- read_dta("~/Downloads/glss4/sec8h.dta")
sec8h <- sec8h[c('nh', 'clust', 's8hq1')]
AGRICULTURE_11 <- sec8h %>%
  rename(HH_consume_any_home_produce= s8hq1)

sec8hid <- read_dta("~/Downloads/glss4/sec8hid.dta")
sec8hid <- sec8hid[c('nh', 'clust', 'region')]
AGRICULTURE_12 <- sec8hid %>%
  rename(Region_ID = region)

sec10b <- read_dta("~/Downloads/glss4/sec10b.dta")
sec10b <- sec10b[c('nh', 'clust', 'bexpcd2')]
NONFARM_ENTERPRISES_1 <- sec10b %>%
  rename(Expenditure_item_code = bexpcd2)

sec10c <- read_dta("~/Downloads/glss4/sec10c.dta")
sec10c <- sec10c[c('nh', 'clust', 's10cq1c')]
NONFARM_ENTERPRISES_2 <- sec10c %>%
  rename(Does_enterprise3_own_item = s10cq1c)

sec11d <- read_dta("~/Downloads/glss4/sec11d.dta")
sec11d <- sec11d[c('nh', 'clust', 's11dq2')]
INCOME_EXPENDITURE <- sec11d %>%
  rename(Amount_spend_on_self_help_project = s11dq2)

sec12a2 <- read_dta("~/Downloads/glss4/sec12a2.dta")
sec12a2 <- sec12a2[c('nh', 'clust', 's12aq5')]
INCOME_EXPENDITURE <- sec12a2 %>%
  rename(Source_of_loan = s12aq5)

sec12c <- read_dta("~/Downloads/glss4/sec12c.dta")
sec12c <- sec12c[c('nh', 'clust', 's12cq6')]
SAVINGS <- sec12c %>%
  rename(Amount_withdrawn_from_savings = s12cq6)




