
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021


if(SUBP) SCHEME_07 <- subpopulation_meanings[, 
                                             
                                             ':=' 
                                             (file_in = paste0(subpopulations,"_study_population.rds"),
                                               file_out1 = paste0(subpopulations,"_PersonTime1.rds"),
                                               file_out2 = paste0(subpopulations,"_PersonTime2.rds"),
                                               folder_in = populations_dir, 
                                               folder_out = std_pop_tmp
                                               
                                               )
                                             ]




if(!SUBP) SCHEME_07 <- data.frame(subpopulations = c("ALL"),
                                  file_in = "ALL_study_population.rds", 
                                  file_out1 = "ALL_PersonTime1.rds", 
                                  file_out2 = "ALL_PersonTime2.rds",
                                  folder_in = populations_dir, 
                                  folder_out = std_pop_tmp
                                  )



for(i in 1:nrow(SCHEME_07)){
  
  print(paste0("Read Study population table for Run CountPersonTime ", SCHEME_07[["subpopulations"]][i]," from intermediate"))
  STUDY_POPULATION <- readRDS(paste0(SCHEME_07[["folder_in"]][i], SCHEME_07[["file_in"]][i]))
  
  print("Run CountPersonTime only on persontime for by month")
  
  years <- as.numeric(substr(start_study_date2,1,4)):as.numeric(substr(end_study_date2,1,4))
  #years <- years[1:3]
  years_st <- paste0(as.character(years),"0101")
  years_en <- paste0(as.character(years),"1231")
  
  #PT1 <- data.table()
  PT1 <- data.table(Year = character(),sex_at_instance_creation = character(), Ageband = character(), Persontime = numeric())                    
  #PT2 <- data.table()
  PT2 <- data.table(Year = character(),Month = character(),sex_at_instance_creation = character(),Persontime = numeric())
  
  for(j in 1:length(years)){
    
    Y <- years[j]
    YST2 <- years_st[j]
    YEN2 <- years_en[j]
    
    intv2 <- as.IDate(c(as.IDate(YST2,"%Y%m%d"), as.IDate(YEN2,"%Y%m%d")))
    
    print(Y)
    
    TEMP <- CountPersonTime2(
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
    
    if(!is.null(TEMP)){
    TEMP <- TEMP[,Month := substr(month,6,8)]
    TEMP <- TEMP[,Year := substr(month,1,4)]
    
    PT1 <- rbindlist(list(PT1,TEMP[, .(Persontime = sum(Persontime)), keyby = list(Year,sex_at_instance_creation, Ageband)]),fill = T, use.names = T)
    PT2 <- rbindlist(list(PT2,TEMP[, .(Persontime = sum(Persontime)), keyby = list(Year,Month,sex_at_instance_creation)]),fill = T, use.names = T)
    
    }
    
    rm(TEMP,Y,YST2,YEN2,intv2)
    gc()
    
    
  }
  
  rm(years,years_st,years_en)
  gc()
  

  ###################################################################################
  
  print(paste0("Write persontime tables for population ",SCHEME_07[["subpopulations"]][i]," to intermediate"))
  
  saveRDS(PT1,file = paste0(SCHEME_07[["folder_out"]][i],SCHEME_07[["file_out1"]][i]))
  saveRDS(PT2,file = paste0(SCHEME_07[["folder_out"]][i],SCHEME_07[["file_out2"]][i]))
  
  
  
  
  rm(STUDY_POPULATION,PT1,PT2)
  gc()
}


saveRDS(SCHEME_07,file = paste0(std_pop_tmp,"SCHEME_07.rds"))

rm(SCHEME_07)
gc()
