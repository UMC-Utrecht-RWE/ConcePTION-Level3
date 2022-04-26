
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021




if(SUBP) SCHEME_1011 <- subpopulation_meanings[, 
                                               ':=' 
                                               (
                                                 file_in = paste0(subpopulations,"_study_population.rds"), 
                                                 file_out = paste0(subpopulations,"_R_01_10_InDatabaseAtAfterBirth.rds"), 
                                                 file_out2 = paste0(subpopulations,"_R_01_11_InDatabaseDuration.csv"),
                                                 folder_in = populations_dir, 
                                                 folder_out = std_source_pop_dir)
                                               
                                               ]


if(!SUBP) SCHEME_1011 <- data.frame(subpopulations = c("ALL"), 
                                    
                                    file_in = paste0("ALL_study_population.rds"), 
                                    file_out = paste0("ALL_R_01_10_InDatabaseAtAfterBirth.rds"), 
                                    file_out2 = paste0("ALL_R_01_11_InDatabaseDuration.csv"),
                                    folder_in = populations_dir, 
                                    folder_out = std_source_pop_dir)



for(i in 1:nrow(SCHEME_1011)){
  
  STUDY_POPULATION <- readRDS(file = paste0(SCHEME_1011[["folder_in"]][i],SCHEME_1011[["file_in"]][i]))[sex_at_instance_creation %in% c("F","M"),]
  
  
  if(nrow(STUDY_POPULATION) > 0){
  Weekbands <- CreateBands(seq(from = 0, to = 2+ max(STUDY_POPULATION[["diff_T1_T0_W"]]), by = 2))
  STUDY_POPULATION <- merge(x = STUDY_POPULATION, y = Weekbands, by.x = "diff_T1_T0_W",by.y = "INT", all.x = T )
  rm(Weekbands)
  gc()
  
  setorder(STUDY_POPULATION, Order)
  
  TEMP <- INPUTMATRIX(
    
    d = STUDY_POPULATION,
    value = "person_id",
    type = "count",
    var = "diff_T1_T0_W",
    var.v = c(min(STUDY_POPULATION[["diff_T1_T0_W"]]):max(STUDY_POPULATION[["diff_T1_T0_W"]])),
    per = T
  
    )
  
  STUDY_POPULATION2 <- STUDY_POPULATION[band == "0-1",]
  
  }else{ 
    
    TEMP <- matrix(0,nrow = 1, ncol = 2000)
    rownames(TEMP) <- "ALL"
    colnames(TEMP) <- as.character(c(1:2000)) 
    STUDY_POPULATION2 <- STUDY_POPULATION[0]
    }
  #fwrite(TEMP, file = paste0(thisdir,SCHEME_1011[["folder_out"]][i],SCHEME_1011[["file_out"]][i]), sep = ";")
  saveRDS(TEMP, file = paste0(SCHEME_1011[["folder_out"]][i],SCHEME_1011[["file_out"]][i]))
  rm(TEMP)
  
  
  
  rm(STUDY_POPULATION)
  gc()
  
  if(nrow(STUDY_POPULATION2) > 0){
  Bands <- CreateBands(seq(from = 0, to = 6 + max(STUDY_POPULATION2[["diff_T2_T1_M"]]), by = 6))
  setnames(Bands,c("band","INT","Order"),c("band2","INT2","Order2"))
  STUDY_POPULATION2 <- merge(x = STUDY_POPULATION2, y = Bands, by.x = "diff_T2_T1_M",by.y = "INT2", all.x = T )
  STUDY_POPULATION2 <- STUDY_POPULATION2[, Year_birth := year(birth_date)]
  COUNT1 <- STUDY_POPULATION2[,.(No = sum(!is.na(person_id))), keyby = list(Year_birth,band2,Order2)]
  COUNT1 <- COUNT1[,Percentage := round((No/sum(No)*100),1)]
  
  new <- c("Year of birth","Distance in months","Order2","No","%")
  colnames(COUNT1) <- new
  
  COUNT2 <- STUDY_POPULATION2[,.(No = median(diff_T2_T1_M)), keyby = Year_birth ]
  COUNT2 <- COUNT2[,':=' (band2 = "Median", Percentage = "", Order2 = 0)]
  
  
  setcolorder(COUNT2,neworder =  c("Year_birth","band2","Order2","No","Percentage"))
  colnames(COUNT2) <- new
  
  COUNT3 <- STUDY_POPULATION2[,.(No = sum(!is.na(person_id))), keyby = Year_birth ]
  COUNT3 <- COUNT3[,':=' (band2 = "Total", Percentage = round((No/sum(No)*100),1), Order2 = max(COUNT1[["Order2"]]+1))]
  
  setcolorder(COUNT3,neworder =  c("Year_birth","band2","Order2","No","Percentage"))
  colnames(COUNT3) <- new
  
  COUNT_T <-rbind(COUNT1,COUNT2,COUNT3)
  rm(COUNT1,COUNT2,COUNT3,STUDY_POPULATION2,Bands)
  gc()
  setorderv(COUNT_T, c("Year of birth","Order2"))
  COUNT_T <- COUNT_T[,Order2 := NULL]
  
  }else{
    COUNT_T <- data.table("Year of birth" = character(),"Distance in months" = character(),No = numeric(),"%" = numeric())
  }
  
  #saveRDS(COUNT_T, file = paste0(thisdir,"/g_intermediate/InDatabaseDuration.rds"))
  fwrite(COUNT_T, file = paste0(SCHEME_1011[["folder_out"]][i],SCHEME_1011[["file_out2"]][i]), sep = ";")
  rm(COUNT_T)
  gc()
  
  
}

#saveRDS(SCHEME_1011, file = paste0(pre_dir,"SCHEME_1011.rds"))
saveRDS(SCHEME_1011,file = paste0(std_pop_tmp,"SCHEME_1011.rds"))
rm(SCHEME_1011)





