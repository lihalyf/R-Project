#Please load all_dv_3.csv before run this code
# all_dv_3 is the aggreated dataframe we created before containing the well-done variables 

#select all the factor tidy variables to factor
fac_v <- all_dv_3 %>%
  select(nh, clust, Employment_status, Read_Ghanaian_language.x, Work_for_whom_in_main_job, Work_for_whom, Region_ID, Outside_wall_material, Sell_any_processed_food)
fac_v <- apply(fac_v,2,factor)

#select all the dummy variables
dummy_v <- all_dv_3 %>%
  select(-Employment_status, -Read_Ghanaian_language.x, -Work_for_whom_in_main_job, -Work_for_whom, -Region_ID, -Outside_wall_material, -Sell_any_processed_food)


#c is imported in profit_per_acre.r file
c <- c %>%
  select(-profit_per_acre, -agri1c, -land_in_acre)

#join community data
full_v_com <- left_join(dummy_v, cs1, by = c('clust'))
full_c <- left_join(c, full_v_com, by = c('nh', 'clust'))

#join numeric data
AGRICULTURE_8 <- unique(AGRICULTURE_8)
full_c <- left_join(full_c, AGRICULTURE_8, by = c('nh', 'clust'))
full_c <- left_join(full_c, numer, by = c('nh', 'clust'))
full_c <- left_join(full_c, numer_2, by = c('nh', 'clust'))
full_c[is.na(full_c)] <- 0

#the data frame with all the variables
summary(full_c$profit_per_acre)

#merge all the other variables with factor variables
full_c <- merge(full_c, fac_v, by =c('nh', 'clust'))

#deleted unnecessary and redundant variables
full_c <- full_c %>%
  select(-nh, -clust, -agri1c,-Can_do_written_calculation.y, -Read_Ghanaian_language.y, -land_in_acre, -Crop_06, -Other_fruits, -Food_01, -Equip_21)


#save the final data frame
write.csv(full_c,file="~/econ_5100/full_c.csv",quote=F,row.names = F)
