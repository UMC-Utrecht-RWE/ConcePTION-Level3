
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021



if(SUBP) SCHEME_01_0405 <- subpopulation_meanings[, 
                                                  ':=' 
                                                  (
                                                    file_in1 = paste0(subpopulations,"_PersonTime1.rds"), 
                                                    file_in2 = paste0(subpopulations,"_PersonTime2.rds"),
                                                    file_out = paste0(subpopulations,"_R_01_04_STUDYPOPPY.csv"), 
                                                    file_out2 = paste0(subpopulations,"_R_01_05_STUDYPOPPY2.csv"),
                                                    folder_in = std_pop_tmp, 
                                                    folder_out = std_source_pop_dir)
                                                  
                                                  ]


if(!SUBP) SCHEME_01_0405 <- data.frame(subpopulations = c("ALL"), 
                                       
                                       file_in1 = paste0("ALL_PersonTime1.rds"),
                                       file_in2 = paste0("ALL_PersonTime2.rds"),
                                       file_out = paste0("ALL_R_01_04_STUDYPOPPY.csv"), 
                                       file_out2 = paste0("ALL_R_01_05_STUDYPOPPY2.csv"),
                                       folder_in = std_pop_tmp, 
                                       folder_out = std_source_pop_dir)




for(i in 1:nrow(SCHEME_01_0405)){
  
  PT1 <- readRDS(file = paste0(SCHEME_01_0405[["folder_in"]][i],SCHEME_01_0405[["file_in1"]][i]))[sex_at_instance_creation %in% c("F","M"),]
  PT2 <- readRDS(file = paste0(SCHEME_01_0405[["folder_in"]][i],SCHEME_01_0405[["file_in2"]][i]))[sex_at_instance_creation %in% c("F","M"),]
  
  ###
  COUNT <- PT1[, PY := round(Persontime/365.25,2)][,Persontime := NULL]
  COUNT <- COUNT[,per := round(PY/sum(PY)*100,2),by = sex_at_instance_creation]
  
  COUNT_T <- COUNT[,.(PY = round(sum(PY),2)), keyby = list(Year,sex_at_instance_creation)][,Ageband := "Total"]
  COUNT_T <- COUNT_T[,per := round(PY/sum(PY)*100,2),by = sex_at_instance_creation]
  
  COUNT <- rbind(COUNT,COUNT_T)

  
  if(nrow(COUNT) == 0){
    
    COUNT <- data.table(Ageband = character(),Year = character(),PY_F = numeric(),PY_M = numeric(),per_F = numeric(),per_M = numeric(),Total = numeric()) 
  }else{
    COUNT <- dcast(COUNT, Ageband + Year ~ sex_at_instance_creation, value.var = c("PY","per"))
    lapply(c("PY_M","per_M","PY_F","per_F")[!c("PY_M","per_M","PY_F","per_F") %in% colnames(COUNT)],function(x) COUNT <- COUNT[,eval(x) := 0] )
    #COUNT <- COUNT[is.na(PY_F), PY_F := 0]
    #COUNT <- COUNT[is.na(PY_M), PY_F := 0]
    COUNT <- COUNT[,Total := PY_F+PY_M]
    }
   
  order <- c("Year","Ageband","PY_M","per_M","PY_F","per_F","Total")
  setcolorder(COUNT, neworder = order )
  new <- c("Year","Ageband","PY Male","% Male","PY Female","% Female","Total")
  setnames(COUNT, order, new)
  setorder(COUNT,Year,Ageband)
  
  fwrite(COUNT, file = paste0(SCHEME_01_0405[["folder_out"]][i],SCHEME_01_0405[["file_out"]][i]), sep = ";")
  
  rm(COUNT,COUNT_T,order,new,PT1)
  ###
  
  COUNT <- PT2[, PY := round(Persontime/365.25,2)][, Persontime := NULL]
  COUNT <- COUNT[, per := round(PY/sum(PY)*100,2), by = sex_at_instance_creation]
  
  if(nrow(COUNT) == 0){
    COUNT <- data.table(Year = character(),Month = character(),PY_F = numeric(),PY_M = numeric(),per_F = numeric(),per_M = numeric())
  }else{
  COUNT <- dcast(COUNT,  Year + Month  ~ sex_at_instance_creation, value.var = c("PY","per"))
  lapply(c("PY_M","per_M","PY_F","per_F")[!c("PY_M","per_M","PY_F","per_F") %in% colnames(COUNT)],function(x) COUNT <- COUNT[,eval(x) := 0] )
  }
  
  order <- c("Year","Month","PY_M","per_M","PY_F","per_F")
  setcolorder(COUNT, neworder = order )
  new <- c("Year","Month","PY Male","% Male","PY Female","% Female")
  setnames(COUNT, order, new)
  setorder(COUNT,Year,Month)
  
  fwrite(COUNT, file = paste0(SCHEME_01_0405[["folder_out"]][i],SCHEME_01_0405[["file_out2"]][i]), sep = ";")
  #saveRDS(COUNT, file = paste0(thisdir,"/g_intermediate/STUDYPOP_PY2_",i,".rds"))
  
  rm(COUNT,PT2,order,new)
  
  ###
  
  
  
  gc()
  
  
  
  
}

saveRDS(SCHEME_01_0405, file = paste0(std_pop_tmp,"SCHEME_01_0405.rds"))
rm(SCHEME_01_0405)





