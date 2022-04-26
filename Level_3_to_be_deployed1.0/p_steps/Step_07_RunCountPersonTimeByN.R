
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

NB <- 200000

for(i in 1:nrow(SCHEME_07)){
  
  print(paste0("Read Study population table for Run CountPersonTime ", SCHEME_07[["subpopulations"]][i]," from intermediate"))
  STUDY_POPULATION <- readRDS(paste0(SCHEME_07[["folder_in"]][i], SCHEME_07[["file_in"]][i]))
  
  print("Run CountPersonTime only on persontime for by month")
  
  
  numchunks <- ceiling(nrow(STUDY_POPULATION) / NB)
  
  PT1 <- data.table(Year = character(),sex_at_instance_creation = character(), Ageband = character(), Persontime = numeric())                    
  PT2 <- data.table(Year = character(),Month = character(),sex_at_instance_creation = character(),Persontime = numeric())
  
  
  #Because CountPersonTime with increment year always want to start on 01/01 end end on 31/12 the start and end date are manupulated accordingly
  YST2 <- paste0(year(start_study_date),"0101")
  YEN2 <- paste0(year(end_study_date),"1231")
  
  for(j in 1:numchunks){
    
    
    #if(j < numchunks) 	aatest <- STUDY_POPULATION[((j - 1) * NB + 1):(j * NB),]
    #if(j == numchunks) 	aatest <- STUDY_POPULATION[((j - 1) * NB + 1):nrow(STUDY_POPULATION),]
    
    if(j < numchunks) 	rows <- c(((j - 1) * NB + 1):(j * NB))
    if(j == numchunks) 	rows <- ((j - 1) * NB + 1):nrow(STUDY_POPULATION)
    

    print(paste0(j," from ",numchunks," chunks"))
    
    TEMP <- CountPersonTime2(
      Dataset = STUDY_POPULATION[rows ,],
      Person_id = "person_id", 
      Start_study_time = YST2, 
      End_study_time = YEN2, 
      Start_date = "start_follow_up", 
      End_date = "end_follow_up", 
      Birth_date = "birth_date",
      Strata = "sex_at_instance_creation", 
      Age_bands = c(0,0,9,19,29,39,49,59,69,79,89,99), 
      Unit_of_age = "year" , 
      Increment = "month", 
      include_remaning_ages = T, 
      Aggregate = F
      
    )
    
    if(!is.null(TEMP)){
    TEMP <- TEMP[,Month := substr(month,6,8)]
    TEMP <- TEMP[,Year := substr(month,1,4)]
    
    TEMP <- TEMP[Ageband == "0-0", Ageband := "0"]
    
    PT1 <- rbindlist(list(PT1,TEMP[, .(Persontime = sum(Persontime)), keyby = list(Year,sex_at_instance_creation, Ageband)]),fill = T, use.names = T)
    PT2 <- rbindlist(list(PT2,TEMP[, .(Persontime = sum(Persontime)), keyby = list(Year,Month,sex_at_instance_creation)]),fill = T, use.names = T)
    
    PT1 <- PT1[, .(Persontime = sum(Persontime)), keyby = list(Year,sex_at_instance_creation, Ageband)] 
    PT2 <- PT2[, .(Persontime = sum(Persontime)), keyby = list(Year,Month,sex_at_instance_creation)]
    
    }
    
    rm(TEMP)
    gc()
    
    
  }
  
  rm(YST2,YEN2)
  gc()
  

  ###################################################################################
  
  print(paste0("Write persontime tables for population ",SCHEME_07[["subpopulations"]][i]," to intermediate"))
  
  saveRDS(PT1,file = paste0(SCHEME_07[["folder_out"]][i],SCHEME_07[["file_out1"]][i]))
  saveRDS(PT2,file = paste0(SCHEME_07[["folder_out"]][i],SCHEME_07[["file_out2"]][i]))
  
  
  
  
  rm(STUDY_POPULATION,PT1,PT2)
  gc()
}


saveRDS(SCHEME_07,file = paste0(std_pop_tmp,"SCHEME_07.rds"))

rm(SCHEME_07, NB)
gc()
