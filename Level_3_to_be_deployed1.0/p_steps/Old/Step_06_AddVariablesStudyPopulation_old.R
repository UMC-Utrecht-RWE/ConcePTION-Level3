
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021



if(SUBP) SCHEME_06 <- subpopulation_meanings[, ':=' (file_in = paste0(subpopulations,"_study_population.rds"), file_out = paste0(subpopulations,"_study_population.rds"), folder_in = populations_dir, folder_out = populations_dir) ]
if(!SUBP) SCHEME_06 <- data.frame(subpopulations = c("ALL"), file_in = "ALL_study_population.rds", file_out = "ALL_study_population.rds", folder_in = populations_dir, folder_out = populations_dir)

SCHEME_06$nrows <- as.integer(NA)
SCHEME_06$ncols <- as.integer(NA)
SCHEME_06$ncolsneeded <- 23

SCHEME_06$nrows_CPT <- as.integer(NA)
SCHEME_06$ncols_CPT <- as.integer(NA)
SCHEME_06$ncolsneeded_CPT <- 23


for(i in 1:nrow(SCHEME_06)){
  
  print(paste0("Read Study population table for population ",SCHEME_06[["subpopulations"]][i]," from intermediate"))
  STUDY_POPULATION <- readRDS(paste0(SCHEME_06[["folder_in"]][i],SCHEME_06[["file_in"]][i]))
  
  #Create new Columns here
  ###################################################################################
  
  print("Set date variables to day, month and year")
  for(j in Analyse_dates){
    STUDY_POPULATION <- STUDY_POPULATION[,  paste0(j,"_month") := month(get(j))]
    STUDY_POPULATION <- STUDY_POPULATION[,  paste0(j,"_year") := year(get(j))]
    STUDY_POPULATION <- STUDY_POPULATION[,  paste0(j,"_day") := day(get(j))]
  }
  
  ###
  
  T0 <- "birth_date"
  T1 <- "op_start_date"
  T2 <- "op_end_date"
  
  vars <- c("person_id",T0,T1,T2)
  
  print(paste0("Calculate time differences between ",T0,"/",T1,"/",T2))
  STUDY_POPULATION <- STUDY_POPULATION[,diff_T1_T0_W := floor((get(T1) - get(T0))/7)]
  STUDY_POPULATION <- STUDY_POPULATION[,diff_T2_T1_M := floor((get(T2) - get(T1))/30.4)]
  
  rm(T0,T1,T2,vars)
  gc()
  
  
  STUDY_POPULATION <- STUDY_POPULATION[,PY := round((end_follow_up - start_follow_up)/365.25,2) ]
  STUDY_POPULATION <- STUDY_POPULATION[,Year_op := year(op_start_date)]
  STUDY_POPULATION <- STUDY_POPULATION[,Year := year(start_follow_up)]
  
  
  
  
  print(paste0("Write Study population table for population ",SCHEME_06[["subpopulations"]][i]," to intermediate"))
  SCHEME_06[i,"nrows"] <- nrow(STUDY_POPULATION)
  SCHEME_06[i,"ncols"] <- ncol(STUDY_POPULATION) 
  saveRDS(STUDY_POPULATION,file = paste0(SCHEME_06[["folder_out"]][i],SCHEME_06[["file_out"]][i]))
  
  rm(STUDY_POPULATION)
  gc()
  

  
}

saveRDS(SCHEME_06,file = paste0(std_pop_tmp,"SCHEME_06.rds"))

rm(SCHEME_06)
gc()
