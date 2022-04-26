
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021


if(SUBP) SCHEME_01_0609 <- subpopulation_meanings[, 
                                                  ':=' 
                                                  (
                                                    file_in = paste0(subpopulations,"_study_population.rds"), 
                                                    file_out = paste0(subpopulations,"_R_01_0608_DT"), 
                                                    
                                                    file_out2 = paste0(subpopulations,"_R_01_09_DT_birth_date_day.csv"),
                                                    folder_in = populations_dir, 
                                                    folder_out = std_source_pop_dir)
                                                  
                                                  ]


if(!SUBP) SCHEME_01_0609 <- data.frame(subpopulations = c("ALL"), 
                                       
                                       file_in = paste0("ALL_study_population.rds"), 
                                       file_out = "ALL_R_01_0608_DT", 
                                       file_out2 = "ALL_R_01_09_DT_birth_date_day.csv",
                                       folder_in = populations_dir, 
                                       folder_out = std_source_pop_dir)




for(i in 1:nrow(SCHEME_01_0609)){
  
  STUDY_POPULATION <- readRDS(file = paste0(SCHEME_01_0609[["folder_in"]][i],SCHEME_01_0609[["file_in"]][i]))[sex_at_instance_creation %in% c("F","M"),]
  
  #[,.(person_id,start_follow_up,end_follow_up,birth_date)]
  
  for(j in Analyse_dates){
    
    TEMP <- STUDY_POPULATION[,.(count = sum(!is.na(person_id))), keyby = c(paste0(j,"_month"),paste0(j,"_year"))]
    
    
    if(j == "birth_date") {
      TEMP <- dcast(TEMP, get(paste0(j,"_year")) ~ get(paste0(j,"_month")), value.var = "count")
    }
    
    if(j != "birth_date") {
      
      TEMP2 <- INPUTMATRIX(
        d = TEMP,
        value = "count",
        type = "none",
        var = paste0(j,"_month"),
        var.v = c(1:12),
        cat = paste0(j,"_year"),
        cat.v = c(min(TEMP[[paste0(j,"_year")]]):max(TEMP[[paste0(j,"_year")]])), 
        per = F
        
        
      ) 
      
      saveRDS(TEMP2, file = paste0(SCHEME_01_0609[["folder_out"]][i],SCHEME_01_0609[["file_out"]][i],"_",j,"_PLOT.rds"))
      
      
      TEMP <- dcast(TEMP, get(paste0(j,"_month")) ~ get(paste0(j,"_year")), value.var = "count")
    }
    TEMP[is.na(TEMP)] <- 0
    if(j == "birth_date") setnames(TEMP,"j","year")
    if(j != "birth_date") setnames(TEMP,"j","month")
    
    fwrite(TEMP, file = paste0(SCHEME_01_0609[["folder_out"]][i],SCHEME_01_0609[["file_out"]][i],"_",j,".csv"), sep = ";")
    
    rm(TEMP)
    gc()
    
  }
  
  TEMP <- STUDY_POPULATION[,.(count = sum(!is.na(person_id))), keyby = c("birth_date_day","birth_date_month")]
  TEMP <- dcast(TEMP, birth_date_day ~ birth_date_month, value.var = "count")
  
  TEMP[is.na(TEMP)] <- 0
  setnames(TEMP,"birth_date_day","day")
  
  fwrite(TEMP, file = paste0(SCHEME_01_0609[["folder_out"]][i],SCHEME_01_0609[["file_out2"]][i]), sep = ";")
  
  rm(TEMP,STUDY_POPULATION)
  
  gc()
  
  
  
  
  
  
  
}

saveRDS(SCHEME_01_0609, file = paste0(std_pop_tmp,"SCHEME_01_0609.rds"))
rm(SCHEME_01_0609)





