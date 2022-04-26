years <- as.numeric(substr(start_study_date2,1,4)):as.numeric(substr(end_study_date2,1,4))
years_st <- paste0(as.character(years),"0101")
years_en <- paste0(as.character(years),"1231")

for(j in 1:length(years)){
  
  Y <- years[j]
  YST2 <- years_st[j]
  YEN2 <- years_en[j]
  
  intv2 <- as.IDate(c(as.IDate(YST2,"%Y%m%d"), as.IDate(YEN2,"%Y%m%d")))
  
  print(Y)
  
  TEMP <- CountPersonTime(
  
    
    Dataset = STUDY_POPULATION,
    Person_id = "person_id", 
    Start_study_time = YST2, 
    End_study_time = YEN2, 
    Start_date = "start_follow_up", 
    End_date = "end_follow_up", 
    Birth_date = "birth_date",
    Strata = "sex_at_instance_creation", 
    Age_bands = c(0,10,20,30,40,50,60,70,80,90,100), 
    Unit_of_age = "year" , 
    Increment = "month", 
    include_remaning_ages = T, 
    Aggregate = F
    
  )
  
  
  TEMP <- TEMP[,Month := substr(month,6,8)]
  TEMP <- TEMP[,Year := substr(month,1,4)]
  
  if (j == 1) {PT1 <- TEMP[, .(Persontime = sum(Persontime)), keyby = list(Year,sex_at_instance_creation, Ageband)]}else{
    PT1 <- rbindlist(list(PT1,TEMP[, .(Persontime = sum(Persontime)), keyby = list(Year,sex_at_instance_creation, Ageband)]),fill = T, use.names = T)
  
  }
  
  if (j == 1) {PT2 <- TEMP[, .(Persontime = sum(Persontime)), keyby = list(Year,Month,sex_at_instance_creation)]}else{
    PT2 <- rbindlist(list(PT2,TEMP[, .(Persontime = sum(Persontime)), keyby = list(Year,Month,sex_at_instance_creation)]),fill = T, use.names = T)
  }
  
  rm(TEMP,Y,YST2,YEN2,intv2)
  gc()
  
  
}

rm(years,years_st,years_en)
gc()

