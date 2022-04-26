

#STUDY_POPULATION[,.(person_id, start_follow_up, end_follow_up)]

#TEMP2 <- merge(x= STUDY_POPULATION[,.(person_id, start_follow_up, end_follow_up)], y = TEMP[, ':=' (start_follow_up = NULL, end_follow_up = NULL) ], by = "person_id", all.x = T)






#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021



print('Import and append persons files')

VISITS <- IMPORT_PATTERN(pat = "VISIT_OCCURRENCE", dir = path_dir)

if(!is.null(VISITS)){
  
  VISITS<-VISITS[,.(person_id,visit_occurrence_id,visit_start_date,visit_end_date,meaning_of_visit)]
  lapply(c("visit_start_date","visit_end_date"), function (x) VISITS <- VISITS[, eval(x) := as.IDate(as.character(get(x)),"%Y%m%d")]  ) 
  
}else{
  VISITS <- data.table(person_id = as.character(), visit_occurrence_id = as.character(),visit_start_date = as.character(),visit_end_date = as.character(),meaning_of_visit = as.character())
  
}


saveRDS(VISITS,file = paste0(std_pop_tmp,"VISITS.rds"))
rm(VISITS)



if(SUBP) SCHEME_0112 <- subpopulation_meanings[, 
                                               ':=' 
                                               (
                                                 file_in = paste0(subpopulations,"_study_population.rds"), 
                                                 file_in2 = "VISITS.rds",
                                                 file_out = paste0(subpopulations,"_R_01_12_VISITS.csv"), 
                                                 folder_in = populations_dir,
                                                 folder_in2 = std_pop_tmp, 
                                                 folder_out =paste0(std_source_pop_dir,subpopulations,"/"))
                                               
]


if(!SUBP){ 
  
  subpopulations <- "ALL"
  SCHEME_0112 <- data.frame(subpopulations = c("ALL"), 
                            
                            file_in = paste0(subpopulations,"_study_population.rds"), 
                            file_in2 = "VISITS.rds",
                            file_out = paste0(subpopulations,"_R_01_12_VISITS.csv"), 
                            folder_in = populations_dir,
                            folder_in2 = std_pop_tmp, 
                            folder_out = std_source_pop_dir)
  rm(subpopulations)
}





for(i in 1:nrow(SCHEME_0112)){
  
  STUDY_POPULATION <- readRDS(file = paste0(SCHEME_0112[["folder_in"]][i],SCHEME_0112[["file_in"]][i]))[, .(person_id, birth_date, start_follow_up, end_follow_up,PY)]
  VISITS <- readRDS(file = paste0(SCHEME_0112[["folder_in2"]][i],SCHEME_0112[["file_in2"]][i]))
  
  TEMP <- CountPersonTime2(
    Dataset = STUDY_POPULATION,
    Dataset_events = VISITS[,.(person_id, visit_start_date, meaning_of_visit)],
    
    Name_event = "meaning_of_visit",
    Date_event = "visit_start_date",
    Outcomes_rec = unique(VISITS[["meaning_of_visit"]]),
    Rec_period = rep(0, length(unique(VISITS[["meaning_of_visit"]]))),
    Person_id = "person_id", 
    Start_study_time = start_study_date2, 
    End_study_time = end_study_date2, 
    Start_date = "start_follow_up", 
    End_date = "end_follow_up", 
    Birth_date = "birth_date",
    Age_bands = c(0,0,9,19,29,39,49,59,69,79,89,99), 
    Unit_of_age = "year" , 
    Increment = "year", 
    include_remaning_ages = T, 
    Aggregate = T
    
  )
  
  
  
  
  
  TEMP <- merge(STUDY_POPULATION, VISITS, by = "person_id", allow.cartesian = T)
  
  TEMP <- TEMP[visit_start_date %between% list(start_follow_up,end_follow_up),]
  
  TEMP <- TEMP[,
               
               ':='
               (
                 Year_visit = year(visit_start_date),
                 Age_visit =  floor(time_length(interval(birth_date, visit_start_date),"year"))
                 
               )
               
               
  ]
  
  TEMP <- merge(x= STUDY_POPULATION[,.(person_id, start_follow_up, end_follow_up)], y = , all.x = "person_id")
  
  if(nrow(TEMP) > 0){
    Agebands <- CreateBands(seq(from = 0 , to = 10 + max(TEMP[["Age_visit"]]), by = 10))
  }else{
    Agebands <- CreateBands(seq(from = 0 , to = 30, by = 10))
  }
  
  TEMP <- merge(x = TEMP, y = Agebands, by.x = "Age_visit",by.y = "INT", all.x = T )
  
  rm(Agebands,STUDY_POPULATION,VISITS)
  gc()
  
  
  COUNT <- TEMP[,.(No = .N, NoU = uniqueN(person_id),   PY2 = round(sum(PY),2)), keyby = list(Year_visit,band,meaning_of_visit,Order)]
  COUNT <- COUNT[, NoPY := round(No/PY2*1000,2)]
  
  
  rm(TEMP)
  
  order <- c("Year_visit","band","PY2","meaning_of_visit","No","NoU","NoPY")
  setcolorder(COUNT, neworder = order )
  setorder(COUNT,Year_visit,Order,meaning_of_visit )
  new <- c("Calendar year","Age","PY","Visit meaning","No. of visits","No. of persons with at least one visit","Visit rate, No. of visits/1000 PY")
  setnames(COUNT, order, new)
  
  #COUNT <- COUNT[, Order := NULL]
  
  fwrite(COUNT, file = paste0(SCHEME_0112[["folder_out"]][i],SCHEME_0112[["file_out"]][i]), sep = ";")
  
  rm(COUNT,order,new)
  gc()
  
  
  
  
}

saveRDS(SCHEME_0112, file = paste0(std_pop_tmp,"SCHEME_0112.rds"))
rm(SCHEME_0112)





