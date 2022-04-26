

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
                                                 file_in3 =  paste0(subpopulations,"_PersonTime1.rds"),
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
                            file_in3 = paste0(subpopulations,"_PersonTime1.rds"),
                            file_out = paste0(subpopulations,"_R_01_12_VISITS.csv"), 
                            folder_in = populations_dir,
                            folder_in2 = std_pop_tmp, 
                            folder_out = std_source_pop_dir)
  rm(subpopulations)
}





for(i in 1:nrow(SCHEME_0112)){
  
  STUDY_POPULATION <- readRDS(file = paste0(SCHEME_0112[["folder_in"]][i],SCHEME_0112[["file_in"]][i]))[, .(person_id, birth_date, start_follow_up, end_follow_up,PY)]
  VISITS <- readRDS(file = paste0(SCHEME_0112[["folder_in2"]][i],SCHEME_0112[["file_in2"]][i]))
  PT1 <- readRDS(file = paste0(SCHEME_0112[["folder_in2"]][i],SCHEME_0112[["file_in3"]][i]))
  PT1 <- PT1[, .(Persontime = sum(Persontime)), by = c("Year", "Ageband")] 
  
  TEMP <- merge(STUDY_POPULATION, VISITS, by = "person_id", allow.cartesian = T)
  
  TEMP <- TEMP[visit_start_date %between% list(start_follow_up,end_follow_up),]
  
  TEMP <- TEMP[,
               
               ':='
               (
                 Year_visit = as.character(year(visit_start_date)),
                 Age_visit =  floor(time_length(interval(birth_date, visit_start_date),"year"))
                 
               )
               
               
  ]
  if(nrow(TEMP) > 0){
    Agebands <- CreateBands(seq(from = 0 , to = 10 + max(TEMP[["Age_visit"]]), by = 10))
  }else{
    Agebands <- CreateBands(seq(from = 0 , to = 30, by = 10))
  }
  
  TEMP <- merge(x = TEMP, y = Agebands, by.x = "Age_visit",by.y = "INT", all.x = T )
  
  rm(Agebands,STUDY_POPULATION,VISITS)
  gc()
  
  
  COUNT <- TEMP[,.(No = .N, NoU = uniqueN(person_id)), keyby = list(Year_visit,band,meaning_of_visit,Order)]
  setnames(PT1, c("Year", "Ageband","Persontime"), c("Year_visit", "band","PY2"))
  
  
  
  if(any(!sort(unique(COUNT[["band"]])) %in% sort(unique(PT1[["band"]])))){warning("Agebands do not match")}
  if(any(!sort(unique(COUNT[["Year_visit"]])) %in% sort(unique(PT1[["Year_visit"]])))){warning("Years do not match")}
  
  before <- nrow(COUNT)
  
  COUNT <- merge(x = COUNT, y = PT1, by = c("Year_visit", "band"), allow.cartesian = F)
  COUNT <- COUNT[, NoPY := round(No/(PY2/365.25)*1000,2)]
  
  after <- nrow(COUNT)
  
  if(before != after){warning("With adding person time to counts rows are not equal")}
  
  rm(TEMP, before, after)
  
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

# 
# v.visits <- unique(VISITS[["meaning_of_visit"]])
# 
# TEMP <- CountPersonTime2(
#   Dataset = STUDY_POPULATION,
#   Dataset_events = VISITS[,.(person_id, visit_start_date, meaning_of_visit)],
#   
#   Name_event = "meaning_of_visit",
#   Date_event = "visit_start_date",
#   Outcomes_rec = v.visits,
#   Rec_period = rep(0, length(v.visits)),
#   Person_id = "person_id", 
#   Start_study_time = start_study_date2, 
#   End_study_time = end_study_date2, 
#   Start_date = "start_follow_up", 
#   End_date = "end_follow_up", 
#   Birth_date = "birth_date",
#   Age_bands = c(0,0,9,19,29,39,49,59,69,79,89,99), 
#   Unit_of_age = "year" , 
#   Increment = "year", 
#   include_remaning_ages = T, 
#   Aggregate = T
#   
# )
# 
# PT_colls <- c("Persontime",paste0("Persontime_",v.visits),paste0(v.visits,"_b"))
# 
# #code <- paste0(PT_colls, "= sum(",PT_colls,")", collapse = ",")
# #eval(parse(text = code))
# 
# #TEMP3 <- TEMP[,  .(eval(parse(text = code))), by = c("year", "Ageband")]
# #TEMP3 <- TEMP[,  .(Persontime= sum(Persontime),Persontime_hospitalisation= sum(Persontime_hospitalisation)), by = c("year", "Ageband")]
# 
# TEMP2 <- TEMP[, lapply(.SD, sum), .SDcols = PT_colls, by = c("year", "Ageband")]
# TEMP3 <- TEMP[, .( NoU = uniqueN(person_id)), by = c("year", "Ageband")]
# 
# TEMP <- merge(TEMP2, TEMP3, by = c("year", "Ageband"))
# 
# TEMP2 <- dcast(TEMP,  Year + Month  ~ sex_at_instance_creation, value.var = c("PY","per"))
# 
# 
# 




