
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021



if(SUBP) SCHEME_0103 <- subpopulation_meanings[, 
                                               ':=' 
                                               (
                                                 file_in = paste0(subpopulations,"_study_population.rds"), 
                                                 file_out = paste0(subpopulations,"_R_01_03_STUDYPOP.csv"), 
                                                 folder_in = populations_dir, 
                                                 folder_out = std_source_pop_dir)
                                               
                                               ]


if(!SUBP) SCHEME_0103 <- data.frame(subpopulations = c("ALL"), 
                                    
                                    file_in = paste0("ALL_study_population.rds"), 
                                    file_out = paste0("ALL_R_01_03_STUDYPOP.csv"), 
                                    folder_in = populations_dir, 
                                    folder_out = std_source_pop_dir)




for(i in 1:nrow(SCHEME_0103)){
  
  STUDY_POPULATION <- readRDS(file = paste0(SCHEME_0103[["folder_in"]][i],SCHEME_0103[["file_in"]][i]))[sex_at_instance_creation %in% c("F","M"),]
  
  Agebands <- CreateBands(seq(from = 0 , to = 20 + Age_max, by = 10))
  #Agebands <- CreateBands(seq(from = 0 , to = 10 + max(STUDY_POPULATION[["age_start_follow_up"]]), by = 10))
  
  TEMP <- merge(x = STUDY_POPULATION, y = Agebands, by.x = "age_start_follow_up",by.y = "INT", all.x = T )
  
  rm(Agebands,STUDY_POPULATION)
  gc()
  
  
  COUNT <- TEMP[,.(count = sum(!is.na(person_id)), mean = round(mean(PY),2), median = median(PY)), keyby = list(Year,band,sex_at_instance_creation)]
  COUNT_T <- TEMP[,.(count = sum(!is.na(person_id)), mean = round(mean(PY),2), median = median(PY)), keyby = list(Year,sex_at_instance_creation)][,band := "Total"]
  rm(TEMP)
  gc()
  
  COUNT <- rbind(COUNT_T,COUNT)
  
  
  ###
  if(nrow(COUNT) == 0){
    COUNT <- data.table(band = character(),Year = character(),count_F = numeric(),count_M = numeric(),mean_F = numeric(),mean_M = numeric(),median_F = numeric(),median_M = numeric())
  }else{
    COUNT <- dcast(COUNT, band + Year ~ sex_at_instance_creation, value.var = c("count","mean","median"))
    lapply(c("count_F","count_M","mean_F","mean_M","median_F","median_M")[!c("count_F","count_M","mean_F","mean_M","median_F","median_M") %in% colnames(COUNT)],function(x) COUNT <- COUNT[,eval(x) := 0] )
  }
  
  ###
  
  #COUNT <- dcast(COUNT, band + Year ~ sex_at_instance_creation, value.var = c("count","mean","median"))
  
  
  COUNT <- COUNT[,Total := count_F + count_M]
  
  order <- c("Year","band","Total","count_F","mean_F","median_F","count_M","mean_M","median_M")
  setcolorder(COUNT, neworder = order )
  new <- c("Year","Ageband","Total No","No Female","Mean Female","Median Female","No Male","Mean Male","Median Male")
  setnames(COUNT, order, new)
  setorder(COUNT,Year,Ageband)
  
  #saveRDS(COUNT_T, file = paste0(thisdir,"/g_intermediate/InDatabaseDuration.rds"))
  fwrite(COUNT, file = paste0(SCHEME_0103[["folder_out"]][i],SCHEME_0103[["file_out"]][i]), sep = ";")
  
  rm(COUNT,COUNT_T,order,new)
  gc()
  
  
  
  
}

saveRDS(SCHEME_0103, file = paste0(std_pop_tmp,"SCHEME_0103.rds"))
rm(SCHEME_0103)





