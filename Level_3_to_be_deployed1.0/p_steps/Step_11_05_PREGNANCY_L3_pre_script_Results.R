#Author: Vjola Hoxhaj Drs.
#email: v.hoxhaj@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/07/2021

###create age bands that will be used for counts####
source(paste0(pre_dir, "functions/create_age_band.R"))
age_bands_counts<-create_age_band(agebands_rates_pregnancy)
####flowchart pregnancy####
flowchart_preg<-data.table(flowchart_events_preg,flowchart_mo_preg[,2], flowchart_so_preg[,2], flowchart_si_preg[,2])
rm(flowchart_events_preg,flowchart_mo_preg,flowchart_so_preg, flowchart_si_preg)

if(subpopulations_present=="Yes"){
  fwrite(flowchart_preg, paste0(preg_dir, subpopulations_names[s], "/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_pregnancy_flowchart.csv"), row.names = F)
} else {
  fwrite(flowchart_preg, paste0(preg_dir,format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "pregnancy_flowchart.csv"), row.names = F)
}

if(length(actual_tables$EVENTS)>0){
  suppressWarnings(flowchart_preg[, EVENTS:= as.character(EVENTS)][as.numeric(EVENTS) > 0 & as.numeric(EVENTS) < 5, EVENTS := "<5"])
}
if(length(actual_tables$MEDICAL_OBSERVATIONS)>0){
  suppressWarnings(flowchart_preg[, MEDICAL_OBSERVATIONS:= as.character(MEDICAL_OBSERVATIONS)][as.numeric(MEDICAL_OBSERVATIONS) > 0 & as.numeric(MEDICAL_OBSERVATIONS) < 5, MEDICAL_OBSERVATIONS := "<5"])
}
if(length(actual_tables$SURVEY_OBSERVATIONS)>0){
  suppressWarnings(flowchart_preg[, SURVEY_OBSERVATIONS:= as.character(SURVEY_OBSERVATIONS)][as.numeric(SURVEY_OBSERVATIONS) > 0 & as.numeric(SURVEY_OBSERVATIONS) < 5, SURVEY_OBSERVATIONS := "<5"])
}
if(length(actual_tables$SURVEY_ID)>0){
  suppressWarnings(flowchart_preg[, SURVEY_ID:= as.character(SURVEY_ID)][as.numeric(SURVEY_ID) > 0 & as.numeric(SURVEY_ID) < 5, SURVEY_ID := "<5"])
}

if(subpopulations_present=="Yes"){
  fwrite(flowchart_preg, paste0(preg_dir,subpopulations_names[s], "/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_pregnancy_flowchart_masked.csv"), row.names = F)
} else {
  fwrite(flowchart_preg, paste0(preg_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "pregnancy_flowchart_masked.csv"), row.names = F)
}

rm(flowchart_preg)
####description pregnancy####
description_preg<-data.table(description_events_preg,description_mo_preg[,2], description_so_preg[,2], description_si_preg[,2])
rm(description_events_preg,description_mo_preg,description_so_preg, description_si_preg)

if(subpopulations_present=="Yes"){
  fwrite(description_preg, paste0(preg_dir, subpopulations_names[s], "/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_pregnancy_description.csv"), row.names = F)
} else {
  fwrite(description_preg, paste0(preg_dir,format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "pregnancy_description.csv"), row.names = F)
}

if(length(actual_tables$EVENTS)>0){
  if(description_preg[4, 2]<5 & description_preg[4, 2]>0) {description_preg[4, 2]<-"<5"}
}
if(length(actual_tables$MEDICAL_OBSERVATIONS)>0){
  if(description_preg[4, 3]<5 & description_preg[4, 3]>0) {description_preg[4, 3]<-"<5"} 
}
if(length(actual_tables$SURVEY_OBSERVATIONS)>0){
  if(description_preg[4, 4]<5 & description_preg[4, 4]>0) {description_preg[4, 4]<-"<5"} 
}
if(length(actual_tables$SURVEY_ID)>0){
  if(description_preg[4, 5]<5 & description_preg[4, 5]>0) {description_preg[4, 5]<-"<5"} 
}

if(subpopulations_present=="Yes"){
  fwrite(description_preg, paste0(preg_dir,subpopulations_names[s], "/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_pregnancy_description_masked.csv"), row.names = F)
} else {
  fwrite(description_preg, paste0(preg_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "pregnancy_description_masked.csv"), row.names = F)
}
rm(description_preg)
####Identify all subjects that never had a pregnancy code####
print("Calculating person time for women that do not have a pregnancy record.")

study_population[no_event_id==1 & no_mo_id==1 & no_so_id==1 & no_si_id==1, no_pregnancy:=1]
study_population[,no_event_id:=NULL][,no_mo_id:=NULL][,no_so_id:=NULL][,no_si_id:=NULL]

if(class(study_population[["start_follow_up_preg"]])[1] !="IDate"){
  study_population[,start_follow_up_preg:=as.IDate(start_follow_up_preg)]
}
if(class(study_population[["end_follow_up_preg"]])[1] != "IDate"){
  study_population[,end_follow_up_preg:=as.IDate(end_follow_up_preg)]
}

#apply count person time and save, in chunks
#Create chunks of 100.000
study_population_no_preg<-study_population[no_pregnancy==1]
study_population<-study_population[is.na(no_pregnancy)]

#####To be removed####
# if(study_population_no_preg[,.N]>0){
#   size<-100000
#   groups<-round(study_population_no_preg[,.N]/size)
#   if(groups==0){groups<-1}
#   index<-1
#   min<-1
#   max<-study_population_no_preg[,.N]
#   chunks<-rep(NA, groups)
#   for (size_ind in 1: groups){
#     if(index< groups){
#       chunks[index]<-paste0(min,":", size_ind*size)
#       min<-size_ind*size+1
#       index<-index+1
#     } else {
#       chunks[index]<-paste0(min,":", study_population_no_preg[,.N]) 
#     }
#   }
#   
#   #Apply countperson time by chunks
#   for (size_ind in 1:length(chunks)){
#     min<-unlist(str_split(chunks[size_ind],":"))[1]
#     max<-unlist(str_split(chunks[size_ind],":"))[2]
#     print(paste0("Analysing chunk ", min,":",max," of the study population with no pregnancy record in the study period." ))
#     ps_no_preg<-CountPersonTime2(Dataset = unique(study_population_no_preg[min:max,.(person_id, birth_date, start_follow_up_preg,end_follow_up_preg, sex_at_instance_creation)]),                                 
#                                  Person_id = "person_id",
#                                  Start_study_time =start_study_date2,
#                                  End_study_time = end_study_rates,
#                                  Start_date = "start_follow_up_preg",
#                                  End_date = "end_follow_up_preg",
#                                  Birth_date = "birth_date",
#                                  Increment = "year",
#                                  Unit_of_age = "year",
#                                  include_remaning_ages = TRUE,
#                                  Strata = c("sex_at_instance_creation"),
#                                  Aggregate = T,
#                                  Age_bands = agebands_rates,
#                                  print = F, 
#                                  check_overlap = F)
#     ps_no_preg<-as.data.table(ps_no_preg)  
#     names(ps_no_preg)<-c("sex", "year","age_band","person_years")    
#     #save results in preg_tmp
#     saveRDS(ps_no_preg, paste0(preg_tmp,size_ind,"_no_id_py.rds"))
#     rm(ps_no_preg)
#   }
# }
# rm(study_population_no_preg)
####Identify all people present in the study population(had at least one pregnancy record)####
#events
pers_events_files_preg<-list.files(preg_ev_tmp, "pers_events_preg")
if(length(pers_events_files_preg)>0){
  pers_events_preg<-lapply(paste0(preg_ev_tmp, pers_events_files_preg), readRDS)
  pers_events_preg<-do.call(rbind,pers_events_preg)
  pers_events_preg<-as.data.table(pers_events_preg)
  pers_events_preg[,pers_events_preg:=1]
  study_population<-merge.data.table(study_population,pers_events_preg, by=c("person_id","birth_date","start_follow_up_preg","end_follow_up_preg","sex_at_instance_creation"), all.x = T)
  rm(pers_events_preg)
} else {
  study_population[,pers_events_preg:=NA] 
}

#mo
pers_mo_files_preg<-list.files(preg_m_tmp, "pers_mo_preg")
if(length(pers_mo_files_preg)>0){
  pers_mo_preg<-lapply(paste0(preg_m_tmp, pers_mo_files_preg), readRDS)
  pers_mo_preg<-do.call(rbind,pers_mo_preg)
  pers_mo_preg<-as.data.table(pers_mo_preg)
  pers_mo_preg[,pers_mo_preg:=1]
  study_population<-merge.data.table(study_population,pers_mo_preg, by=c("person_id","birth_date","start_follow_up_preg","end_follow_up_preg","sex_at_instance_creation"), all.x = T)
  rm(pers_mo_preg)
} else {
  study_population[,pers_mo_preg:=NA] 
}

#so
pers_so_files_preg<-list.files(preg_s_tmp, "pers_so_preg")
if(length(pers_so_files_preg)>0){
  pers_so_preg<-lapply(paste0(preg_s_tmp, pers_so_files_preg), readRDS)
  pers_so_preg<-do.call(rbind,pers_so_preg)
  pers_so_preg<-as.data.table(pers_so_preg)
  pers_so_preg[,pers_so_preg:=1]
  study_population<-merge.data.table(study_population,pers_so_preg, by=c("person_id","birth_date","start_follow_up_preg","end_follow_up_preg","sex_at_instance_creation"), all.x = T)
  rm(pers_so_preg)
} else {
  study_population[,pers_so_preg:=NA] 
}

#si
pers_si_files_preg<-list.files(preg_si_tmp, "pers_si_preg")
if(length(pers_si_files_preg)>0){
  pers_si_preg<-lapply(paste0(preg_si_tmp, pers_si_files_preg), readRDS)
  pers_si_preg<-do.call(rbind,pers_si_preg)
  pers_si_preg<-as.data.table(pers_si_preg)
  pers_si_preg[,pers_si_preg:=1]
  study_population<-merge.data.table(study_population,pers_si_preg, by=c("person_id","birth_date","start_follow_up_preg","end_follow_up_preg","sex_at_instance_creation"), all.x = T)
  rm(pers_si_preg)
} else {
  study_population[,pers_si_preg:=NA] 
}

#if a person is present in one of the tables, then is present in the diagnoses_study_population
study_population[pers_events_preg==1 | pers_mo_preg==1 | pers_so_preg==1 | pers_si_preg==1, pers_preg:=1]
study_population[,pers_events_preg:=NULL][,pers_mo_preg:=NULL][,pers_so_preg:=NULL][,pers_si_preg:=NULL]

#remove people that where excluded from the diagnoses_study_population
excluded_people<-study_population[is.na(pers_preg),.N]
study_population<-study_population[pers_preg==1]

excluded_people<-data.table(Indicator="People removed due to exclusion criteria", Number=excluded_people)
if(subpopulations_present=="Yes"){
  fwrite(excluded_people, paste0(preg_dir, subpopulations_names[s], "/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_excluded_people_pregnancy.csv"), row.names = F)
} else {
  fwrite(excluded_people, paste0(preg_dir,format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "excluded_people_pregnancy.csv"), row.names = F)
}

rm(excluded_people)

#Combine the study_population with people that never had a pregnacy code
study_population[,pers_preg:=NULL][,no_pregnancy:=NULL]
study_population_no_preg[,no_pregnancy:=NULL]
study_population<-rbind(study_population,study_population_no_preg)
rm(study_population_no_preg)
#Apply count person time to save the person time for all people that had a pregnancy record 
#Apply countperson time in  chunks
if(study_population[,.N]>0){
  size<-100000
  groups<-round(study_population[,.N]/size)
  if(groups==0){groups<-1}
  index<-1
  min<-1
  max<-study_population[,.N]
  chunks<-rep(NA, groups)
  for (size_ind in 1: groups){
    if(index< groups){
      chunks[index]<-paste0(min,":", size_ind*size)
      min<-size_ind*size+1
      index<-index+1
    } else {
      chunks[index]<-paste0(min,":", study_population[,.N]) 
    }
  }
  
  #Apply countperson time by chunks
  for (size_ind in 1:length(chunks)){
    min<-unlist(str_split(chunks[size_ind],":"))[1]
    max<-unlist(str_split(chunks[size_ind],":"))[2]
    print(paste0("Analysing chunk ", min,":",max," of the study population with at least one preganncy record in the study period." ))
    
    ps_all_preg<-CountPersonTime2(Dataset = unique(study_population[min:max,.(person_id, birth_date, start_follow_up_preg,end_follow_up_preg, sex_at_instance_creation)]),
                                  Person_id = "person_id",
                                  Start_study_time =start_study_date2,
                                  End_study_time = end_study_rates,
                                  Start_date = "start_follow_up_preg",
                                  End_date = "end_follow_up_preg",
                                  Birth_date = "birth_date",
                                  Increment = "year",
                                  Unit_of_age = "year",
                                  include_remaning_ages = TRUE,
                                  Strata = c("sex_at_instance_creation"),
                                  Aggregate = T,
                                  Age_bands = agebands_rates_pregnancy,
                                  print = F, 
                                  check_overlap = F)
    
    ps_all_preg<-as.data.table(ps_all_preg)
    names(ps_all_preg)<-c("sex", "year","age_band","person_years")
    ps_all_preg[,person_years:=round(person_years/365.25,3)]
    saveRDS(ps_all_preg,paste0(preg_tmp, size_ind, "_py_pregnancy.rds"))
    rm(ps_all_preg)
  }
}


####Counts and rates####
print("Combine datasets for pooling results.")
####Retrieve files for all stages of pregnancy coming from events,mo,so,si####
pregnancy_files_events<-list()
pregnancy_files_events$start_of_pregnancy<-list.files(preg_ev_tmp, "start_of_pregnancy")
pregnancy_files_events$end_of_pregnancy<-list.files(preg_ev_tmp, "end_of_pregnancy")
pregnancy_files_events$ongoing_pregnancy<-list.files(preg_ev_tmp, "ongoing_pregnancy")
pregnancy_files_events$interruption_pregnancy<-list.files(preg_ev_tmp, "interruption_pregnancy")

pregnancy_files_mo<-list()
pregnancy_files_mo$start_of_pregnancy<-list.files(preg_m_tmp, "start_of_pregnancy")
pregnancy_files_mo$end_of_pregnancy<-list.files(preg_m_tmp, "end_of_pregnancy")
pregnancy_files_mo$ongoing_pregnancy<-list.files(preg_m_tmp, "ongoing_pregnancy")
pregnancy_files_mo$interruption_pregnancy<-list.files(preg_m_tmp, "interruption_pregnancy")

pregnancy_files_so<-list()
pregnancy_files_so$start_of_pregnancy<-list.files(preg_s_tmp, "start_of_pregnancy")
pregnancy_files_so$end_of_pregnancy<-list.files(preg_s_tmp, "end_of_pregnancy")
pregnancy_files_so$ongoing_pregnancy<-list.files(preg_s_tmp, "ongoing_pregnancy")
pregnancy_files_so$interruption_pregnancy<-list.files(preg_s_tmp, "interruption_pregnancy")

pregnancy_files_si<-list()
pregnancy_files_si$start_of_pregnancy<-list.files(preg_si_tmp,"start")
pregnancy_files_si$end_of_pregnancy<-list.files(preg_si_tmp, "end")
pregnancy_files_si$ongoing_pregnancy<-list.files(preg_si_tmp,"ongoing")
pregnancy_files_si$interruption_pregnancy<-list.files(preg_si_tmp,"interruption")

####Data cleanup, remove duplicate pregnancy records and pregnancy records with the time lag####
time_lag<-data.table(stage_of_pregnancy=c("start_of_pregnancy", "end_of_pregnancy", "ongoing_pregnancy", "interruption_pregnancy"),time_lag=c(6*7,23*7,23*7, 8*7))

#combine all files from all tables in one
#create a loop that goes through all stages of pregnancy
duplicated_preg_dates<-data.table(event_definition=c("start_of_pregnancy", "ongoing_pregnancy", "interruption_pregnancy","end_of_pregnancy"),original_rows=0,duplicates=0,duplicates_time_lag=as.character(0))
#duplicates: pregnancy record at the same date are counted and removed
#duplicates_time_lag: pregnancy record within the time lag are counted and removed


####start_of_pregnancy####
####start_of_pregnancy_events####
if(length(pregnancy_files_events$start_of_pregnancy)>0){
combined_start_ev<-readRDS(paste0(preg_ev_tmp,pregnancy_files_events$start_of_pregnancy[1]))
orig_no_row_events<-combined_start_ev[,.N]
#remove the duplicate dates and save number removed
combined_start_ev[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
combined_start_ev<-combined_start_ev[!duplicated(pers_date)]
combined_start_ev[,pers_date:=NULL]
no_duplicates_rows_events<-combined_start_ev[,.N]
duplicated_dates_rows_events<-orig_no_row_events - no_duplicates_rows_events
#remove within time lag dates
combined_start_ev[,lag:=time_lag[stage_of_pregnancy=="start_of_pregnancy",time_lag]]
if(combined_start_ev[!is.na(lag),.N]>0){
  #Step 1: Order the dataset by person_id, condition and date of event
  combined_start_ev<-combined_start_ev[order(person_id,condition,pregnancy_code_date)]
  #Step 2: Create date_2(by shifting the first date)
  combined_start_ev[,date_1:=shift(pregnancy_code_date)]
  combined_start_ev[,date_2:=pregnancy_code_date]
  #Step 3: Create rowid(which will give the number of rows for each person)
  combined_start_ev[,rowid:=rowid(person_id)]
  #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
  combined_start_ev[rowid==1,date_1:=NA]
  #Step 5: Create date_dif as difference between date 2 and date
  combined_start_ev[,date_dif:=date_2-date_1]
  #Step 6: Remove these rows 
  combined_start_ev<-combined_start_ev[date_dif>lag | is.na(date_dif)]
  #Step 7: Repeat until there are no more impossible dates in the dataset
  combined_start_ev[,date_dif:=date_2-date_1]
  while(combined_start_ev[date_dif<=lag,.N]>0){
    combined_start_ev<-combined_start_ev[date_dif>lag | is.na(date_dif)]
    combined_start_ev[,date_dif:=date_2-date_1]
  }
  
  rows_no_duplicated_events<-combined_start_ev[,.N]
  duplicates_time_lag_events<-no_duplicates_rows_events - rows_no_duplicated_events
  
  combined_start_ev[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
  
}

#Add other files one by one
start_ind<-2
while(start_ind<=length(pregnancy_files_events$start_of_pregnancy)){
  #load next file
  a<-readRDS(paste0(preg_ev_tmp,pregnancy_files_events$start_of_pregnancy[start_ind]))
  #create var lag
  a[,lag:=time_lag[stage_of_pregnancy=="start_of_pregnancy",time_lag]]
  #add number of rows to original no of rows
  orig_no_row_events<-orig_no_row_events+a[,.N]
  file_add_rows<-a[,.N]
  #Combine files together
  combined_start_ev<-rbind(combined_start_ev,a)
  rm(a)
  previous_rows<-combined_start_ev[,.N]
  #remove duplicates
  combined_start_ev[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
  combined_start_ev<-combined_start_ev[!duplicated(pers_date)]
  combined_start_ev[,pers_date:=NULL]
  no_duplicates_rows_events<-combined_start_ev[,.N]
  duplicated_dates_rows_events<-previous_rows - no_duplicates_rows_events
  #remove duplicates time lag
  if(combined_start_ev[!is.na(lag),.N]>0){
    #Step 1: Order the dataset by person_id, condition and date of event
    combined_start_ev<-combined_start_ev[order(person_id,condition,pregnancy_code_date)]
    #Step 2: Create date_2(by shifting the first date)
    combined_start_ev[,date_1:=shift(pregnancy_code_date)]
    combined_start_ev[,date_2:=pregnancy_code_date]
    #Step 3: Create rowid(which will give the number of rows for each person)
    combined_start_ev[,rowid:=rowid(person_id)]
    #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
    combined_start_ev[rowid==1,date_1:=NA]
    #Step 5: Create date_dif as difference between date 2 and date
    combined_start_ev[,date_dif:=date_2-date_1]
    #Step 6: Remove these rows 
    combined_start_ev<-combined_start_ev[date_dif>lag | is.na(date_dif)]
    #Step 7: Repeat until there are no more impossible dates in the dataset
    combined_start_ev[,date_dif:=date_2-date_1]
    while(combined_start_ev[date_dif<=lag,.N]>0){
      combined_start_ev<-combined_start_ev[date_dif>lag | is.na(date_dif)]
      combined_start_ev[,date_dif:=date_2-date_1]
    }
    
    rows_no_duplicated_events<-combined_start_ev[,.N]
    duplicates_time_lag_events<-no_duplicates_rows_events - rows_no_duplicated_events
    
    combined_start_ev[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
    
  }
  start_ind<-start_ind+1
}

#remove all events files and save the start_events at preg_tmp
for(i in 1: length(pregnancy_files_events$start_of_pregnancy)){
  file.remove(paste0(preg_ev_tmp,pregnancy_files_events$start_of_pregnancy[i]))
}

if(combined_start_ev[,.N]>0){
  years_preg<-sort(combined_start_ev[!duplicated(year),year])
  for(year_ind in 1:length(years_preg)){
    saveRDS(combined_start_ev[year==years_preg[year_ind]], paste0(preg_tmp,years_preg[year_ind], "_start_events.rds"))
  }
}
rm(combined_start_ev)
} else {
  orig_no_row_events<-0
  duplicated_dates_rows_events<-0
  duplicates_time_lag_events<-0
}
####start_of_pregnancy_mo####
if(length(pregnancy_files_mo$start_of_pregnancy)>0){
combined_start_mo<-readRDS(paste0(preg_m_tmp,pregnancy_files_mo$start_of_pregnancy[1]))
orig_no_row_mo<-combined_start_mo[,.N]
#remove the duplicate dates and save number removed
combined_start_mo[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
combined_start_mo<-combined_start_mo[!duplicated(pers_date)]
combined_start_mo[,pers_date:=NULL]
no_duplicates_rows_mo<-combined_start_mo[,.N]
duplicated_dates_rows_mo<-orig_no_row_mo - no_duplicates_rows_mo
#remove within time lag dates
combined_start_mo[,lag:=time_lag[stage_of_pregnancy=="start_of_pregnancy",time_lag]]
if(combined_start_mo[!is.na(lag),.N]>0){
  #Step 1: Order the dataset by person_id, condition and date of event
  combined_start_mo<-combined_start_mo[order(person_id,condition,pregnancy_code_date)]
  #Step 2: Create date_2(by shifting the first date)
  combined_start_mo[,date_1:=shift(pregnancy_code_date)]
  combined_start_mo[,date_2:=pregnancy_code_date]
  #Step 3: Create rowid(which will give the number of rows for each person)
  combined_start_mo[,rowid:=rowid(person_id)]
  #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
  combined_start_mo[rowid==1,date_1:=NA]
  #Step 5: Create date_dif as difference between date 2 and date
  combined_start_mo[,date_dif:=date_2-date_1]
  #Step 6: Remove these rows 
  combined_start_mo<-combined_start_mo[date_dif>lag | is.na(date_dif)]
  #Step 7: Repeat until there are no more impossible dates in the dataset
  combined_start_mo[,date_dif:=date_2-date_1]
  while(combined_start_mo[date_dif<=lag,.N]>0){
    combined_start_mo<-combined_start_mo[date_dif>lag | is.na(date_dif)]
    combined_start_mo[,date_dif:=date_2-date_1]
  }
  
  rows_no_duplicated_mo<-combined_start_mo[,.N]
  duplicates_time_lag_mo<-no_duplicates_rows_mo - rows_no_duplicated_mo
  
  combined_start_mo[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
  
}

#Add other files one by one
start_ind<-2
while(start_ind<=length(pregnancy_files_mo$start_of_pregnancy)){
  #load next file
  a<-readRDS(paste0(preg_m_tmp,pregnancy_files_mo$start_of_pregnancy[start_ind]))
  #create var lag
  a[,lag:=time_lag[stage_of_pregnancy=="start_of_pregnancy",time_lag]]
  #add number of rows to original no of rows
  orig_no_row_mo<-orig_no_row_mo+a[,.N]
  file_add_rows<-a[,.N]
  #Combine files together
  combined_start_mo<-rbind(combined_start_mo,a)
  rm(a)
  previous_rows<-combined_start_mo[,.N]
  #remove duplicates
  combined_start_mo[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
  combined_start_mo<-combined_start_mo[!duplicated(pers_date)]
  combined_start_mo[,pers_date:=NULL]
  no_duplicates_rows_mo<-combined_start_mo[,.N]
  duplicated_dates_rows_mo<-previous_rows - no_duplicates_rows_mo
  #remove duplicates time lag
  if(combined_start_mo[!is.na(lag),.N]>0){
    #Step 1: Order the dataset by person_id, condition and date of event
    combined_start_mo<-combined_start_mo[order(person_id,condition,pregnancy_code_date)]
    #Step 2: Create date_2(by shifting the first date)
    combined_start_mo[,date_1:=shift(pregnancy_code_date)]
    combined_start_mo[,date_2:=pregnancy_code_date]
    #Step 3: Create rowid(which will give the number of rows for each person)
    combined_start_mo[,rowid:=rowid(person_id)]
    #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
    combined_start_mo[rowid==1,date_1:=NA]
    #Step 5: Create date_dif as difference between date 2 and date
    combined_start_mo[,date_dif:=date_2-date_1]
    #Step 6: Remove these rows 
    combined_start_mo<-combined_start_mo[date_dif>lag | is.na(date_dif)]
    #Step 7: Repeat until there are no more impossible dates in the dataset
    combined_start_mo[,date_dif:=date_2-date_1]
    while(combined_start_mo[date_dif<=lag,.N]>0){
      combined_start_mo<-combined_start_mo[date_dif>lag | is.na(date_dif)]
      combined_start_mo[,date_dif:=date_2-date_1]
    }
    
    rows_no_duplicated_mo<-combined_start_mo[,.N]
    duplicates_time_lag_mo<-no_duplicates_rows_mo - rows_no_duplicated_mo
    
    combined_start_mo[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
    
  }
  start_ind<-start_ind+1
}

#remove all events files and save the start_events at preg_tmp
for(i in 1: length(pregnancy_files_mo$start_of_pregnancy)){
  file.remove(paste0(preg_m_tmp,pregnancy_files_mo$start_of_pregnancy[i]))
}

if(combined_start_mo[,.N]>0){
  years_preg<-sort(combined_start_mo[!duplicated(year),year])
  for(year_ind in 1:length(years_preg)){
    saveRDS(combined_start_mo[year==years_preg[year_ind]], paste0(preg_tmp,years_preg[year_ind], "_start_mo.rds"))
  }
}
rm(combined_start_mo)
} else {
  orig_no_row_mo<-0
  duplicated_dates_rows_mo<-0
  duplicates_time_lag_mo<-0
}
####start_of_pregnancy_so####
if(length(pregnancy_files_so$start_of_pregnancy)>0){
combined_start_so<-readRDS(paste0(preg_s_tmp,pregnancy_files_so$start_of_pregnancy[1]))
orig_no_row_so<-combined_start_so[,.N]
#remove the duplicate dates and save number removed
combined_start_so[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
combined_start_so<-combined_start_so[!duplicated(pers_date)]
combined_start_so[,pers_date:=NULL]
no_duplicates_rows_so<-combined_start_so[,.N]
duplicated_dates_rows_so<-orig_no_row_so - no_duplicates_rows_so
#remove within time lag dates
combined_start_so[,lag:=time_lag[stage_of_pregnancy=="start_of_pregnancy",time_lag]]
if(combined_start_so[!is.na(lag),.N]>0){
  #Step 1: Order the dataset by person_id, condition and date of event
  combined_start_so<-combined_start_so[order(person_id,condition,pregnancy_code_date)]
  #Step 2: Create date_2(by shifting the first date)
  combined_start_so[,date_1:=shift(pregnancy_code_date)]
  combined_start_so[,date_2:=pregnancy_code_date]
  #Step 3: Create rowid(which will give the number of rows for each person)
  combined_start_so[,rowid:=rowid(person_id)]
  #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
  combined_start_so[rowid==1,date_1:=NA]
  #Step 5: Create date_dif as difference between date 2 and date
  combined_start_so[,date_dif:=date_2-date_1]
  #Step 6: Remove these rows 
  combined_start_so<-combined_start_so[date_dif>lag | is.na(date_dif)]
  #Step 7: Repeat until there are no more impossible dates in the dataset
  combined_start_so[,date_dif:=date_2-date_1]
  while(combined_start_so[date_dif<=lag,.N]>0){
    combined_start_so<-combined_start_so[date_dif>lag | is.na(date_dif)]
    combined_start_so[,date_dif:=date_2-date_1]
  }
  
  rows_no_duplicated_so<-combined_start_so[,.N]
  duplicates_time_lag_so<-no_duplicates_rows_so - rows_no_duplicated_so
  
  combined_start_so[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
  
}

#Add other files one by one
start_ind<-2
while(start_ind<=length(pregnancy_files_so$start_of_pregnancy)){
  #load next file
  a<-readRDS(paste0(preg_s_tmp,pregnancy_files_so$start_of_pregnancy[start_ind]))
  #create var lag
  a[,lag:=time_lag[stage_of_pregnancy=="start_of_pregnancy",time_lag]]
  #add number of rows to original no of rows
  orig_no_row_so<-orig_no_row_so+a[,.N]
  file_add_rows<-a[,.N]
  #Combine files together
  combined_start_so<-rbind(combined_start_so,a)
  rm(a)
  previous_rows<-combined_start_so[,.N]
  #remove duplicates
  combined_start_so[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
  combined_start_so<-combined_start_so[!duplicated(pers_date)]
  combined_start_so[,pers_date:=NULL]
  no_duplicates_rows_so<-combined_start_so[,.N]
  duplicated_dates_rows_so<-previous_rows - no_duplicates_rows_so
  #remove duplicates time lag
  if(combined_start_so[!is.na(lag),.N]>0){
    #Step 1: Order the dataset by person_id, condition and date of event
    combined_start_so<-combined_start_so[order(person_id,condition,pregnancy_code_date)]
    #Step 2: Create date_2(by shifting the first date)
    combined_start_so[,date_1:=shift(pregnancy_code_date)]
    combined_start_so[,date_2:=pregnancy_code_date]
    #Step 3: Create rowid(which will give the number of rows for each person)
    combined_start_so[,rowid:=rowid(person_id)]
    #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
    combined_start_so[rowid==1,date_1:=NA]
    #Step 5: Create date_dif as difference between date 2 and date
    combined_start_so[,date_dif:=date_2-date_1]
    #Step 6: Remove these rows 
    combined_start_so<-combined_start_so[date_dif>lag | is.na(date_dif)]
    #Step 7: Repeat until there are no more impossible dates in the dataset
    combined_start_so[,date_dif:=date_2-date_1]
    while(combined_start_so[date_dif<=lag,.N]>0){
      combined_start_so<-combined_start_so[date_dif>lag | is.na(date_dif)]
      combined_start_so[,date_dif:=date_2-date_1]
    }
    
    rows_no_duplicated_so<-combined_start_so[,.N]
    duplicates_time_lag_so<-no_duplicates_rows_so - rows_no_duplicated_so
    
    combined_start_so[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
    
  }
  start_ind<-start_ind+1
}

#remove all events files and save the start_events at preg_tmp
for(i in 1: length(pregnancy_files_so$start_of_pregnancy)){
  file.remove(paste0(preg_s_tmp,pregnancy_files_so$start_of_pregnancy[i]))
}

if(combined_start_so[,.N]>0){
  years_preg<-sort(combined_start_so[!duplicated(year),year])
  for(year_ind in 1:length(years_preg)){
    saveRDS(combined_start_so[year==years_preg[year_ind]], paste0(preg_tmp,years_preg[year_ind], "_start_so.rds"))
  }
}
rm(combined_start_so)
} else {
  orig_no_row_so<-0
  duplicated_dates_rows_so<-0
  duplicates_time_lag_so<-0
}
####start_of_pregnancy_si####
if(length(pregnancy_files_si$start_of_pregnancy)>0){
combined_start_si<-readRDS(paste0(preg_si_tmp,pregnancy_files_si$start_of_pregnancy[1]))
orig_no_row_si<-combined_start_si[,.N]
#remove the duplicate dates and save number removed
combined_start_si[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
combined_start_si<-combined_start_si[!duplicated(pers_date)]
combined_start_si[,pers_date:=NULL]
no_duplicates_rows_si<-combined_start_si[,.N]
duplicated_dates_rows_si<-orig_no_row_si - no_duplicates_rows_si
#remove within time lag dates
combined_start_si[,lag:=time_lag[stage_of_pregnancy=="start_of_pregnancy",time_lag]]
if(combined_start_si[!is.na(lag),.N]>0){
  #Step 1: Order the dataset by person_id, condition and date of event
  combined_start_si<-combined_start_si[order(person_id,condition,pregnancy_code_date)]
  #Step 2: Create date_2(by shifting the first date)
  combined_start_si[,date_1:=shift(pregnancy_code_date)]
  combined_start_si[,date_2:=pregnancy_code_date]
  #Step 3: Create rowid(which will give the number of rows for each person)
  combined_start_si[,rowid:=rowid(person_id)]
  #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
  combined_start_si[rowid==1,date_1:=NA]
  #Step 5: Create date_dif as difference between date 2 and date
  combined_start_si[,date_dif:=date_2-date_1]
  #Step 6: Remove these rows 
  combined_start_si<-combined_start_si[date_dif>lag | is.na(date_dif)]
  #Step 7: Repeat until there are no more impossible dates in the dataset
  combined_start_si[,date_dif:=date_2-date_1]
  while(combined_start_si[date_dif<=lag,.N]>0){
    combined_start_si<-combined_start_si[date_dif>lag | is.na(date_dif)]
    combined_start_si[,date_dif:=date_2-date_1]
  }
  
  rows_no_duplicated_si<-combined_start_si[,.N]
  duplicates_time_lag_si<-no_duplicates_rows_si - rows_no_duplicated_si
  
  combined_start_si[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
  
}

#Add other files one by one
start_ind<-2
while(start_ind<=length(pregnancy_files_si$start_of_pregnancy)){
  #load next file
  a<-readRDS(paste0(preg_si_tmp,pregnancy_files_si$start_of_pregnancy[start_ind]))
  #create var lag
  a[,lag:=time_lag[stage_of_pregnancy=="start_of_pregnancy",time_lag]]
  #add number of rows to original no of rows
  orig_no_row_si<-orig_no_row_si+a[,.N]
  file_add_rows<-a[,.N]
  #Combine files together
  combined_start_si<-rbind(combined_start_si,a)
  rm(a)
  previous_rows<-combined_start_si[,.N]
  #remove duplicates
  combined_start_si[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
  combined_start_si<-combined_start_si[!duplicated(pers_date)]
  combined_start_si[,pers_date:=NULL]
  no_duplicates_rows_si<-combined_start_si[,.N]
  duplicated_dates_rows_si<-previous_rows - no_duplicates_rows_si
  #remove duplicates time lag
  if(combined_start_si[!is.na(lag),.N]>0){
    #Step 1: Order the dataset by person_id, condition and date of event
    combined_start_si<-combined_start_si[order(person_id,condition,pregnancy_code_date)]
    #Step 2: Create date_2(by shifting the first date)
    combined_start_si[,date_1:=shift(pregnancy_code_date)]
    combined_start_si[,date_2:=pregnancy_code_date]
    #Step 3: Create rowid(which will give the number of rows for each person)
    combined_start_si[,rowid:=rowid(person_id)]
    #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
    combined_start_si[rowid==1,date_1:=NA]
    #Step 5: Create date_dif as difference between date 2 and date
    combined_start_si[,date_dif:=date_2-date_1]
    #Step 6: Remove these rows 
    combined_start_si<-combined_start_si[date_dif>lag | is.na(date_dif)]
    #Step 7: Repeat until there are no more impossible dates in the dataset
    combined_start_si[,date_dif:=date_2-date_1]
    while(combined_start_si[date_dif<=lag,.N]>0){
      combined_start_si<-combined_start_si[date_dif>lag | is.na(date_dif)]
      combined_start_si[,date_dif:=date_2-date_1]
    }
    
    rows_no_duplicated_si<-combined_start_si[,.N]
    duplicates_time_lag_si<-no_duplicates_rows_si - rows_no_duplicated_si
    
    combined_start_si[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
    
  }
  start_ind<-start_ind+1
}

#remove all events files and save the start_events at preg_tmp
for(i in 1: length(pregnancy_files_si$start_of_pregnancy)){
  file.remove(paste0(preg_si_tmp,pregnancy_files_si$start_of_pregnancy[i]))
}

if(combined_start_si[,.N]>0){
  years_preg<-sort(combined_start_si[!duplicated(year),year])
  for(year_ind in 1:length(years_preg)){
    saveRDS(combined_start_si[year==years_preg[year_ind]], paste0(preg_tmp,years_preg[year_ind], "_start_si.rds"))
  }
}
rm(combined_start_si)
} else {
  orig_no_row_si<-0
  duplicated_dates_rows_si<-0
  duplicates_time_lag_si<-0
}
####start_of_pregnancy duplicates removal####
orig_no_row_start<-orig_no_row_events+orig_no_row_mo+orig_no_row_so+orig_no_row_si
duplicated_dates_rows_start<-duplicated_dates_rows_events+duplicated_dates_rows_mo+duplicated_dates_rows_so+duplicated_dates_rows_si
duplicates_time_lag_start<-duplicates_time_lag_events+duplicates_time_lag_mo+duplicates_time_lag_so+duplicates_time_lag_si
duplicated_preg_dates[event_definition=="start_of_pregnancy",original_rows:=orig_no_row_start]
duplicated_preg_dates[event_definition=="start_of_pregnancy",duplicates:=duplicated_dates_rows_start]
duplicated_preg_dates[event_definition=="start_of_pregnancy",duplicates_time_lag:=duplicates_time_lag_start]
rm(orig_no_row_start,orig_no_row_events,orig_no_row_mo,orig_no_row_so,orig_no_row_si)
rm(duplicated_dates_rows_start,duplicated_dates_rows_events,duplicated_dates_rows_mo,duplicated_dates_rows_so,duplicated_dates_rows_si)
rm(duplicates_time_lag_start,duplicates_time_lag_mo,duplicates_time_lag_so,duplicates_time_lag_si)
####interruption_pregnancy####
####interruption_pregnancy_events####
if(length(pregnancy_files_events$interruption_pregnancy)>0){
  combined_int_ev<-readRDS(paste0(preg_ev_tmp,pregnancy_files_events$interruption_pregnancy[1]))
  orig_no_row_events<-combined_int_ev[,.N]
  #remove the duplicate dates and save number removed
  combined_int_ev[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
  combined_int_ev<-combined_int_ev[!duplicated(pers_date)]
  combined_int_ev[,pers_date:=NULL]
  no_duplicates_rows_events<-combined_int_ev[,.N]
  duplicated_dates_rows_events<-orig_no_row_events - no_duplicates_rows_events
  #remove within time lag dates
  combined_int_ev[,lag:=time_lag[stage_of_pregnancy=="interruption_pregnancy",time_lag]]
  if(combined_int_ev[!is.na(lag),.N]>0){
    #Step 1: Order the dataset by person_id, condition and date of event
    combined_int_ev<-combined_int_ev[order(person_id,condition,pregnancy_code_date)]
    #Step 2: Create date_2(by shifting the first date)
    combined_int_ev[,date_1:=shift(pregnancy_code_date)]
    combined_int_ev[,date_2:=pregnancy_code_date]
    #Step 3: Create rowid(which will give the number of rows for each person)
    combined_int_ev[,rowid:=rowid(person_id)]
    #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
    combined_int_ev[rowid==1,date_1:=NA]
    #Step 5: Create date_dif as difference between date 2 and date
    combined_int_ev[,date_dif:=date_2-date_1]
    #Step 6: Remove these rows 
    combined_int_ev<-combined_int_ev[date_dif>lag | is.na(date_dif)]
    #Step 7: Repeat until there are no more impossible dates in the dataset
    combined_int_ev[,date_dif:=date_2-date_1]
    while(combined_int_ev[date_dif<=lag,.N]>0){
      combined_int_ev<-combined_int_ev[date_dif>lag | is.na(date_dif)]
      combined_int_ev[,date_dif:=date_2-date_1]
    }
    
    rows_no_duplicated_events<-combined_int_ev[,.N]
    duplicates_time_lag_events<-no_duplicates_rows_events - rows_no_duplicated_events
    
    combined_int_ev[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
    
  }
  
  #Add other files one by one
  int_ind<-2
  while(int_ind<=length(pregnancy_files_events$interruption_pregnancy)){
    #load next file
    a<-readRDS(paste0(preg_ev_tmp,pregnancy_files_events$interruption_pregnancy[int_ind]))
    #create var lag
    a[,lag:=time_lag[stage_of_pregnancy=="interruption_pregnancy",time_lag]]
    #add number of rows to original no of rows
    orig_no_row_events<-orig_no_row_events+a[,.N]
    file_add_rows<-a[,.N]
    #Combine files together
    combined_int_ev<-rbind(combined_int_ev,a)
    rm(a)
    previous_rows<-combined_int_ev[,.N]
    #remove duplicates
    combined_int_ev[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
    combined_int_ev<-combined_int_ev[!duplicated(pers_date)]
    combined_int_ev[,pers_date:=NULL]
    no_duplicates_rows_events<-combined_int_ev[,.N]
    duplicated_dates_rows_events<-previous_rows - no_duplicates_rows_events
    #remove duplicates time lag
    if(combined_int_ev[!is.na(lag),.N]>0){
      #Step 1: Order the dataset by person_id, condition and date of event
      combined_int_ev<-combined_int_ev[order(person_id,condition,pregnancy_code_date)]
      #Step 2: Create date_2(by shifting the first date)
      combined_int_ev[,date_1:=shift(pregnancy_code_date)]
      combined_int_ev[,date_2:=pregnancy_code_date]
      #Step 3: Create rowid(which will give the number of rows for each person)
      combined_int_ev[,rowid:=rowid(person_id)]
      #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
      combined_int_ev[rowid==1,date_1:=NA]
      #Step 5: Create date_dif as difference between date 2 and date
      combined_int_ev[,date_dif:=date_2-date_1]
      #Step 6: Remove these rows 
      combined_int_ev<-combined_int_ev[date_dif>lag | is.na(date_dif)]
      #Step 7: Repeat until there are no more impossible dates in the dataset
      combined_int_ev[,date_dif:=date_2-date_1]
      while(combined_int_ev[date_dif<=lag,.N]>0){
        combined_int_ev<-combined_int_ev[date_dif>lag | is.na(date_dif)]
        combined_int_ev[,date_dif:=date_2-date_1]
      }
      
      rows_no_duplicated_events<-combined_int_ev[,.N]
      duplicates_time_lag_events<-no_duplicates_rows_events - rows_no_duplicated_events
      
      combined_int_ev[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
      
    }
    int_ind<-int_ind+1
  }
  
  #remove all events files and save the int_events at preg_tmp
  for(i in 1: length(pregnancy_files_events$interruption_pregnancy)){
    file.remove(paste0(preg_ev_tmp,pregnancy_files_events$interruption_pregnancy[i]))
  }
  
  if(combined_int_ev[,.N]>0){
    years_preg<-sort(combined_int_ev[!duplicated(year),year])
    for(year_ind in 1:length(years_preg)){
      saveRDS(combined_int_ev[year==years_preg[year_ind]], paste0(preg_tmp,years_preg[year_ind], "_int_events.rds"))
    }
  }
  rm(combined_int_ev)
} else {
  orig_no_row_events<-0
  duplicated_dates_rows_events<-0
  duplicates_time_lag_events<-0
}
####interruption_pregnancy_mo####
if(length(pregnancy_files_mo$interruption_pregnancy)>0){
  combined_int_mo<-readRDS(paste0(preg_m_tmp,pregnancy_files_mo$interruption_pregnancy[1]))
  orig_no_row_mo<-combined_int_mo[,.N]
  #remove the duplicate dates and save number removed
  combined_int_mo[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
  combined_int_mo<-combined_int_mo[!duplicated(pers_date)]
  combined_int_mo[,pers_date:=NULL]
  no_duplicates_rows_mo<-combined_int_mo[,.N]
  duplicated_dates_rows_mo<-orig_no_row_mo - no_duplicates_rows_mo
  #remove within time lag dates
  combined_int_mo[,lag:=time_lag[stage_of_pregnancy=="interruption_pregnancy",time_lag]]
  if(combined_int_mo[!is.na(lag),.N]>0){
    #Step 1: Order the dataset by person_id, condition and date of event
    combined_int_mo<-combined_int_mo[order(person_id,condition,pregnancy_code_date)]
    #Step 2: Create date_2(by shifting the first date)
    combined_int_mo[,date_1:=shift(pregnancy_code_date)]
    combined_int_mo[,date_2:=pregnancy_code_date]
    #Step 3: Create rowid(which will give the number of rows for each person)
    combined_int_mo[,rowid:=rowid(person_id)]
    #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
    combined_int_mo[rowid==1,date_1:=NA]
    #Step 5: Create date_dif as difference between date 2 and date
    combined_int_mo[,date_dif:=date_2-date_1]
    #Step 6: Remove these rows 
    combined_int_mo<-combined_int_mo[date_dif>lag | is.na(date_dif)]
    #Step 7: Repeat until there are no more impossible dates in the dataset
    combined_int_mo[,date_dif:=date_2-date_1]
    while(combined_int_mo[date_dif<=lag,.N]>0){
      combined_int_mo<-combined_int_mo[date_dif>lag | is.na(date_dif)]
      combined_int_mo[,date_dif:=date_2-date_1]
    }
    
    rows_no_duplicated_mo<-combined_int_mo[,.N]
    duplicates_time_lag_mo<-no_duplicates_rows_mo - rows_no_duplicated_mo
    
    combined_int_mo[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
    
  }
  
  #Add other files one by one
  int_ind<-2
  while(int_ind<=length(pregnancy_files_mo$interruption_pregnancy)){
    #load next file
    a<-readRDS(paste0(preg_m_tmp,pregnancy_files_mo$interruption_pregnancy[int_ind]))
    #create var lag
    a[,lag:=time_lag[stage_of_pregnancy=="interruption_pregnancy",time_lag]]
    #add number of rows to original no of rows
    orig_no_row_mo<-orig_no_row_mo+a[,.N]
    file_add_rows<-a[,.N]
    #Combine files together
    combined_int_mo<-rbind(combined_int_mo,a)
    rm(a)
    previous_rows<-combined_int_mo[,.N]
    #remove duplicates
    combined_int_mo[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
    combined_int_mo<-combined_int_mo[!duplicated(pers_date)]
    combined_int_mo[,pers_date:=NULL]
    no_duplicates_rows_mo<-combined_int_mo[,.N]
    duplicated_dates_rows_mo<-previous_rows - no_duplicates_rows_mo
    #remove duplicates time lag
    if(combined_int_mo[!is.na(lag),.N]>0){
      #Step 1: Order the dataset by person_id, condition and date of event
      combined_int_mo<-combined_int_mo[order(person_id,condition,pregnancy_code_date)]
      #Step 2: Create date_2(by shifting the first date)
      combined_int_mo[,date_1:=shift(pregnancy_code_date)]
      combined_int_mo[,date_2:=pregnancy_code_date]
      #Step 3: Create rowid(which will give the number of rows for each person)
      combined_int_mo[,rowid:=rowid(person_id)]
      #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
      combined_int_mo[rowid==1,date_1:=NA]
      #Step 5: Create date_dif as difference between date 2 and date
      combined_int_mo[,date_dif:=date_2-date_1]
      #Step 6: Remove these rows 
      combined_int_mo<-combined_int_mo[date_dif>lag | is.na(date_dif)]
      #Step 7: Repeat until there are no more impossible dates in the dataset
      combined_int_mo[,date_dif:=date_2-date_1]
      while(combined_int_mo[date_dif<=lag,.N]>0){
        combined_int_mo<-combined_int_mo[date_dif>lag | is.na(date_dif)]
        combined_int_mo[,date_dif:=date_2-date_1]
      }
      
      rows_no_duplicated_mo<-combined_int_mo[,.N]
      duplicates_time_lag_mo<-no_duplicates_rows_mo - rows_no_duplicated_mo
      
      combined_int_mo[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
      
    }
    int_ind<-int_ind+1
  }
  
  #remove all events files and save the int_events at preg_tmp
  for(i in 1: length(pregnancy_files_mo$interruption_pregnancy)){
    file.remove(paste0(preg_m_tmp,pregnancy_files_mo$interruption_pregnancy[i]))
  }
  
  if(combined_int_mo[,.N]>0){
    years_preg<-sort(combined_int_mo[!duplicated(year),year])
    for(year_ind in 1:length(years_preg)){
      saveRDS(combined_int_mo[year==years_preg[year_ind]], paste0(preg_tmp,years_preg[year_ind], "_int_mo.rds"))
    }
  }
  rm(combined_int_mo)
} else {
  orig_no_row_mo<-0
  duplicated_dates_rows_mo<-0
  duplicates_time_lag_mo<-0
}
####interruption_pregnancy_so####
if(length(pregnancy_files_so$interruption_pregnancy)>0){
  combined_int_so<-readRDS(paste0(preg_s_tmp,pregnancy_files_so$interruption_pregnancy[1]))
  orig_no_row_so<-combined_int_so[,.N]
  #remove the duplicate dates and save number removed
  combined_int_so[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
  combined_int_so<-combined_int_so[!duplicated(pers_date)]
  combined_int_so[,pers_date:=NULL]
  no_duplicates_rows_so<-combined_int_so[,.N]
  duplicated_dates_rows_so<-orig_no_row_so - no_duplicates_rows_so
  #remove within time lag dates
  combined_int_so[,lag:=time_lag[stage_of_pregnancy=="interruption_pregnancy",time_lag]]
  if(combined_int_so[!is.na(lag),.N]>0){
    #Step 1: Order the dataset by person_id, condition and date of event
    combined_int_so<-combined_int_so[order(person_id,condition,pregnancy_code_date)]
    #Step 2: Create date_2(by shifting the first date)
    combined_int_so[,date_1:=shift(pregnancy_code_date)]
    combined_int_so[,date_2:=pregnancy_code_date]
    #Step 3: Create rowid(which will give the number of rows for each person)
    combined_int_so[,rowid:=rowid(person_id)]
    #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
    combined_int_so[rowid==1,date_1:=NA]
    #Step 5: Create date_dif as difference between date 2 and date
    combined_int_so[,date_dif:=date_2-date_1]
    #Step 6: Remove these rows 
    combined_int_so<-combined_int_so[date_dif>lag | is.na(date_dif)]
    #Step 7: Repeat until there are no more impossible dates in the dataset
    combined_int_so[,date_dif:=date_2-date_1]
    while(combined_int_so[date_dif<=lag,.N]>0){
      combined_int_so<-combined_int_so[date_dif>lag | is.na(date_dif)]
      combined_int_so[,date_dif:=date_2-date_1]
    }
    
    rows_no_duplicated_so<-combined_int_so[,.N]
    duplicates_time_lag_so<-no_duplicates_rows_so - rows_no_duplicated_so
    
    combined_int_so[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
    
  }
  
  #Add other files one by one
  int_ind<-2
  while(int_ind<=length(pregnancy_files_so$interruption_pregnancy)){
    #load next file
    a<-readRDS(paste0(preg_s_tmp,pregnancy_files_so$interruption_pregnancy[int_ind]))
    #create var lag
    a[,lag:=time_lag[stage_of_pregnancy=="interruption_pregnancy",time_lag]]
    #add number of rows to original no of rows
    orig_no_row_so<-orig_no_row_so+a[,.N]
    file_add_rows<-a[,.N]
    #Combine files together
    combined_int_so<-rbind(combined_int_so,a)
    rm(a)
    previous_rows<-combined_int_so[,.N]
    #remove duplicates
    combined_int_so[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
    combined_int_so<-combined_int_so[!duplicated(pers_date)]
    combined_int_so[,pers_date:=NULL]
    no_duplicates_rows_so<-combined_int_so[,.N]
    duplicated_dates_rows_so<-previous_rows - no_duplicates_rows_so
    #remove duplicates time lag
    if(combined_int_so[!is.na(lag),.N]>0){
      #Step 1: Order the dataset by person_id, condition and date of event
      combined_int_so<-combined_int_so[order(person_id,condition,pregnancy_code_date)]
      #Step 2: Create date_2(by shifting the first date)
      combined_int_so[,date_1:=shift(pregnancy_code_date)]
      combined_int_so[,date_2:=pregnancy_code_date]
      #Step 3: Create rowid(which will give the number of rows for each person)
      combined_int_so[,rowid:=rowid(person_id)]
      #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
      combined_int_so[rowid==1,date_1:=NA]
      #Step 5: Create date_dif as difference between date 2 and date
      combined_int_so[,date_dif:=date_2-date_1]
      #Step 6: Remove these rows 
      combined_int_so<-combined_int_so[date_dif>lag | is.na(date_dif)]
      #Step 7: Repeat until there are no more impossible dates in the dataset
      combined_int_so[,date_dif:=date_2-date_1]
      while(combined_int_so[date_dif<=lag,.N]>0){
        combined_int_so<-combined_int_so[date_dif>lag | is.na(date_dif)]
        combined_int_so[,date_dif:=date_2-date_1]
      }
      
      rows_no_duplicated_so<-combined_int_so[,.N]
      duplicates_time_lag_so<-no_duplicates_rows_so - rows_no_duplicated_so
      
      combined_int_so[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
      
    }
    int_ind<-int_ind+1
  }
  
  #remove all events files and save the int_events at preg_tmp
  for(i in 1: length(pregnancy_files_so$interruption_pregnancy)){
    file.remove(paste0(preg_s_tmp,pregnancy_files_so$interruption_pregnancy[i]))
  }
  
  if(combined_int_so[,.N]>0){
    years_preg<-sort(combined_int_so[!duplicated(year),year])
    for(year_ind in 1:length(years_preg)){
      saveRDS(combined_int_so[year==years_preg[year_ind]], paste0(preg_tmp,years_preg[year_ind], "_int_so.rds"))
    }
  }
  rm(combined_int_so)
} else {
  orig_no_row_so<-0
  duplicated_dates_rows_so<-0
  duplicates_time_lag_so<-0
}
####interruption_pregnancy_si####
if(length(pregnancy_files_si$interruption_pregnancy)>0){
  combined_int_si<-readRDS(paste0(preg_si_tmp,pregnancy_files_si$interruption_pregnancy[1]))
  orig_no_row_si<-combined_int_si[,.N]
  #remove the duplicate dates and save number removed
  combined_int_si[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
  combined_int_si<-combined_int_si[!duplicated(pers_date)]
  combined_int_si[,pers_date:=NULL]
  no_duplicates_rows_si<-combined_int_si[,.N]
  duplicated_dates_rows_si<-orig_no_row_si - no_duplicates_rows_si
  #remove within time lag dates
  combined_int_si[,lag:=time_lag[stage_of_pregnancy=="interruption_pregnancy",time_lag]]
  if(combined_int_si[!is.na(lag),.N]>0){
    #Step 1: Order the dataset by person_id, condition and date of event
    combined_int_si<-combined_int_si[order(person_id,condition,pregnancy_code_date)]
    #Step 2: Create date_2(by shifting the first date)
    combined_int_si[,date_1:=shift(pregnancy_code_date)]
    combined_int_si[,date_2:=pregnancy_code_date]
    #Step 3: Create rowid(which will give the number of rows for each person)
    combined_int_si[,rowid:=rowid(person_id)]
    #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
    combined_int_si[rowid==1,date_1:=NA]
    #Step 5: Create date_dif as difference between date 2 and date
    combined_int_si[,date_dif:=date_2-date_1]
    #Step 6: Remove these rows 
    combined_int_si<-combined_int_si[date_dif>lag | is.na(date_dif)]
    #Step 7: Repeat until there are no more impossible dates in the dataset
    combined_int_si[,date_dif:=date_2-date_1]
    while(combined_int_si[date_dif<=lag,.N]>0){
      combined_int_si<-combined_int_si[date_dif>lag | is.na(date_dif)]
      combined_int_si[,date_dif:=date_2-date_1]
    }
    
    rows_no_duplicated_si<-combined_int_si[,.N]
    duplicates_time_lag_si<-no_duplicates_rows_si - rows_no_duplicated_si
    
    combined_int_si[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
    
  }
  
  #Add other files one by one
  int_ind<-2
  while(int_ind<=length(pregnancy_files_si$interruption_pregnancy)){
    #load next file
    a<-readRDS(paste0(preg_si_tmp,pregnancy_files_si$interruption_pregnancy[int_ind]))
    #create var lag
    a[,lag:=time_lag[stage_of_pregnancy=="interruption_pregnancy",time_lag]]
    #add number of rows to original no of rows
    orig_no_row_si<-orig_no_row_si+a[,.N]
    file_add_rows<-a[,.N]
    #Combine files together
    combined_int_si<-rbind(combined_int_si,a)
    rm(a)
    previous_rows<-combined_int_si[,.N]
    #remove duplicates
    combined_int_si[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
    combined_int_si<-combined_int_si[!duplicated(pers_date)]
    combined_int_si[,pers_date:=NULL]
    no_duplicates_rows_si<-combined_int_si[,.N]
    duplicated_dates_rows_si<-previous_rows - no_duplicates_rows_si
    #remove duplicates time lag
    if(combined_int_si[!is.na(lag),.N]>0){
      #Step 1: Order the dataset by person_id, condition and date of event
      combined_int_si<-combined_int_si[order(person_id,condition,pregnancy_code_date)]
      #Step 2: Create date_2(by shifting the first date)
      combined_int_si[,date_1:=shift(pregnancy_code_date)]
      combined_int_si[,date_2:=pregnancy_code_date]
      #Step 3: Create rowid(which will give the number of rows for each person)
      combined_int_si[,rowid:=rowid(person_id)]
      #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
      combined_int_si[rowid==1,date_1:=NA]
      #Step 5: Create date_dif as difference between date 2 and date
      combined_int_si[,date_dif:=date_2-date_1]
      #Step 6: Remove these rows 
      combined_int_si<-combined_int_si[date_dif>lag | is.na(date_dif)]
      #Step 7: Repeat until there are no more impossible dates in the dataset
      combined_int_si[,date_dif:=date_2-date_1]
      while(combined_int_si[date_dif<=lag,.N]>0){
        combined_int_si<-combined_int_si[date_dif>lag | is.na(date_dif)]
        combined_int_si[,date_dif:=date_2-date_1]
      }
      
      rows_no_duplicated_si<-combined_int_si[,.N]
      duplicates_time_lag_si<-no_duplicates_rows_si - rows_no_duplicated_si
      
      combined_int_si[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
      
    }
    int_ind<-int_ind+1
  }
  
  #remove all events files and save the int_events at preg_tmp
  for(i in 1: length(pregnancy_files_si$interruption_pregnancy)){
    file.remove(paste0(preg_si_tmp,pregnancy_files_si$interruption_pregnancy[i]))
  }
  
  if(combined_int_si[,.N]>0){
    years_preg<-sort(combined_int_si[!duplicated(year),year])
    for(year_ind in 1:length(years_preg)){
      saveRDS(combined_int_si[year==years_preg[year_ind]], paste0(preg_tmp,years_preg[year_ind], "_int_si.rds"))
    }
  }
  rm(combined_int_si)
} else {
  orig_no_row_si<-0
  duplicated_dates_rows_si<-0
  duplicates_time_lag_si<-0
}
####interruption_pregnancy duplicates removal####
orig_no_row_interruption<-orig_no_row_events+orig_no_row_mo+orig_no_row_so+orig_no_row_si
duplicated_dates_rows_interruption<-duplicated_dates_rows_events+duplicated_dates_rows_mo+duplicated_dates_rows_so+duplicated_dates_rows_si
duplicates_time_lag_interruption<-duplicates_time_lag_events+duplicates_time_lag_mo+duplicates_time_lag_so+duplicates_time_lag_si
duplicated_preg_dates[event_definition=="interruption_pregnancy",original_rows:=orig_no_row_interruption]
duplicated_preg_dates[event_definition=="interruption_pregnancy",duplicates:=duplicated_dates_rows_interruption]
duplicated_preg_dates[event_definition=="interruption_pregnancy",duplicates_time_lag:=duplicates_time_lag_interruption]
rm(orig_no_row_interruption,orig_no_row_events,orig_no_row_mo,orig_no_row_so,orig_no_row_si)
rm(duplicated_dates_rows_interruption,duplicated_dates_rows_events,duplicated_dates_rows_mo,duplicated_dates_rows_so,duplicated_dates_rows_si)
rm(duplicates_time_lag_interruption,duplicates_time_lag_mo,duplicates_time_lag_so,duplicates_time_lag_si)
####ongoing_pregnancy####
####ongoing_pregnancy_events####
if(length(pregnancy_files_events$ongoing_pregnancy)>0){
  combined_ongoing_ev<-readRDS(paste0(preg_ev_tmp,pregnancy_files_events$ongoing_pregnancy[1]))
  orig_no_row_events<-combined_ongoing_ev[,.N]
  #remove the duplicate dates and save number removed
  combined_ongoing_ev[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
  combined_ongoing_ev<-combined_ongoing_ev[!duplicated(pers_date)]
  combined_ongoing_ev[,pers_date:=NULL]
  no_duplicates_rows_events<-combined_ongoing_ev[,.N]
  duplicated_dates_rows_events<-orig_no_row_events - no_duplicates_rows_events
  #remove within time lag dates
  combined_ongoing_ev[,lag:=time_lag[stage_of_pregnancy=="ongoing_pregnancy",time_lag]]
  if(combined_ongoing_ev[!is.na(lag),.N]>0){
    #Step 1: Order the dataset by person_id, condition and date of event
    combined_ongoing_ev<-combined_ongoing_ev[order(person_id,condition,pregnancy_code_date)]
    #Step 2: Create date_2(by shifting the first date)
    combined_ongoing_ev[,date_1:=shift(pregnancy_code_date)]
    combined_ongoing_ev[,date_2:=pregnancy_code_date]
    #Step 3: Create rowid(which will give the number of rows for each person)
    combined_ongoing_ev[,rowid:=rowid(person_id)]
    #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
    combined_ongoing_ev[rowid==1,date_1:=NA]
    #Step 5: Create date_dif as difference between date 2 and date
    combined_ongoing_ev[,date_dif:=date_2-date_1]
    #Step 6: Remove these rows 
    combined_ongoing_ev<-combined_ongoing_ev[date_dif>lag | is.na(date_dif)]
    #Step 7: Repeat until there are no more impossible dates in the dataset
    combined_ongoing_ev[,date_dif:=date_2-date_1]
    while(combined_ongoing_ev[date_dif<=lag,.N]>0){
      combined_ongoing_ev<-combined_ongoing_ev[date_dif>lag | is.na(date_dif)]
      combined_ongoing_ev[,date_dif:=date_2-date_1]
    }
    
    rows_no_duplicated_events<-combined_ongoing_ev[,.N]
    duplicates_time_lag_events<-no_duplicates_rows_events - rows_no_duplicated_events
    
    combined_ongoing_ev[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
    
  }
  
  #Add other files one by one
  ongoing_ind<-2
  while(ongoing_ind<=length(pregnancy_files_events$ongoing_pregnancy)){
    #load next file
    a<-readRDS(paste0(preg_ev_tmp,pregnancy_files_events$ongoing_pregnancy[ongoing_ind]))
    #create var lag
    a[,lag:=time_lag[stage_of_pregnancy=="ongoing_pregnancy",time_lag]]
    #add number of rows to original no of rows
    orig_no_row_events<-orig_no_row_events+a[,.N]
    file_add_rows<-a[,.N]
    #Combine files together
    combined_ongoing_ev<-rbind(combined_ongoing_ev,a)
    rm(a)
    previous_rows<-combined_ongoing_ev[,.N]
    #remove duplicates
    combined_ongoing_ev[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
    combined_ongoing_ev<-combined_ongoing_ev[!duplicated(pers_date)]
    combined_ongoing_ev[,pers_date:=NULL]
    no_duplicates_rows_events<-combined_ongoing_ev[,.N]
    duplicated_dates_rows_events<-previous_rows - no_duplicates_rows_events
    #remove duplicates time lag
    if(combined_ongoing_ev[!is.na(lag),.N]>0){
      #Step 1: Order the dataset by person_id, condition and date of event
      combined_ongoing_ev<-combined_ongoing_ev[order(person_id,condition,pregnancy_code_date)]
      #Step 2: Create date_2(by shifting the first date)
      combined_ongoing_ev[,date_1:=shift(pregnancy_code_date)]
      combined_ongoing_ev[,date_2:=pregnancy_code_date]
      #Step 3: Create rowid(which will give the number of rows for each person)
      combined_ongoing_ev[,rowid:=rowid(person_id)]
      #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
      combined_ongoing_ev[rowid==1,date_1:=NA]
      #Step 5: Create date_dif as difference between date 2 and date
      combined_ongoing_ev[,date_dif:=date_2-date_1]
      #Step 6: Remove these rows 
      combined_ongoing_ev<-combined_ongoing_ev[date_dif>lag | is.na(date_dif)]
      #Step 7: Repeat until there are no more impossible dates in the dataset
      combined_ongoing_ev[,date_dif:=date_2-date_1]
      while(combined_ongoing_ev[date_dif<=lag,.N]>0){
        combined_ongoing_ev<-combined_ongoing_ev[date_dif>lag | is.na(date_dif)]
        combined_ongoing_ev[,date_dif:=date_2-date_1]
      }
      
      rows_no_duplicated_events<-combined_ongoing_ev[,.N]
      duplicates_time_lag_events<-no_duplicates_rows_events - rows_no_duplicated_events
      
      combined_ongoing_ev[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
      
    }
    ongoing_ind<-ongoing_ind+1
  }
  
  #remove all events files and save the int_events at preg_tmp
  for(i in 1: length(pregnancy_files_events$ongoing_pregnancy)){
    file.remove(paste0(preg_ev_tmp,pregnancy_files_events$ongoing_pregnancy[i]))
  }
  
  if(combined_ongoing_ev[,.N]>0){
    years_preg<-sort(combined_ongoing_ev[!duplicated(year),year])
    for(year_ind in 1:length(years_preg)){
      saveRDS(combined_ongoing_ev[year==years_preg[year_ind]], paste0(preg_tmp,years_preg[year_ind], "_ongoing_events.rds"))
    }
  }
  rm(combined_ongoing_ev)
} else {
  orig_no_row_events<-0
  duplicated_dates_rows_events<-0
  duplicates_time_lag_events<-0
}
####ongoing_pregnancy_mo####
if(length(pregnancy_files_mo$ongoing_pregnancy)>0){
  combined_ongoing_mo<-readRDS(paste0(preg_m_tmp,pregnancy_files_mo$ongoing_pregnancy[1]))
  orig_no_row_mo<-combined_ongoing_mo[,.N]
  #remove the duplicate dates and save number removed
  combined_ongoing_mo[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
  combined_ongoing_mo<-combined_ongoing_mo[!duplicated(pers_date)]
  combined_ongoing_mo[,pers_date:=NULL]
  no_duplicates_rows_mo<-combined_ongoing_mo[,.N]
  duplicated_dates_rows_mo<-orig_no_row_mo - no_duplicates_rows_mo
  #remove within time lag dates
  combined_ongoing_mo[,lag:=time_lag[stage_of_pregnancy=="ongoing_pregnancy",time_lag]]
  if(combined_ongoing_mo[!is.na(lag),.N]>0){
    #Step 1: Order the dataset by person_id, condition and date of event
    combined_ongoing_mo<-combined_ongoing_mo[order(person_id,condition,pregnancy_code_date)]
    #Step 2: Create date_2(by shifting the first date)
    combined_ongoing_mo[,date_1:=shift(pregnancy_code_date)]
    combined_ongoing_mo[,date_2:=pregnancy_code_date]
    #Step 3: Create rowid(which will give the number of rows for each person)
    combined_ongoing_mo[,rowid:=rowid(person_id)]
    #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
    combined_ongoing_mo[rowid==1,date_1:=NA]
    #Step 5: Create date_dif as difference between date 2 and date
    combined_ongoing_mo[,date_dif:=date_2-date_1]
    #Step 6: Remove these rows 
    combined_ongoing_mo<-combined_ongoing_mo[date_dif>lag | is.na(date_dif)]
    #Step 7: Repeat until there are no more impossible dates in the dataset
    combined_ongoing_mo[,date_dif:=date_2-date_1]
    while(combined_ongoing_mo[date_dif<=lag,.N]>0){
      combined_ongoing_mo<-combined_ongoing_mo[date_dif>lag | is.na(date_dif)]
      combined_ongoing_mo[,date_dif:=date_2-date_1]
    }
    
    rows_no_duplicated_mo<-combined_ongoing_mo[,.N]
    duplicates_time_lag_mo<-no_duplicates_rows_mo - rows_no_duplicated_mo
    
    combined_ongoing_mo[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
    
  }
  
  #Add other files one by one
  ongoing_ind<-2
  while(ongoing_ind<=length(pregnancy_files_mo$ongoing_pregnancy)){
    #load next file
    a<-readRDS(paste0(preg_m_tmp,pregnancy_files_mo$ongoing_pregnancy[ongoing_ind]))
    #create var lag
    a[,lag:=time_lag[stage_of_pregnancy=="ongoing_pregnancy",time_lag]]
    #add number of rows to original no of rows
    orig_no_row_mo<-orig_no_row_mo+a[,.N]
    file_add_rows<-a[,.N]
    #Combine files together
    combined_ongoing_mo<-rbind(combined_ongoing_mo,a)
    rm(a)
    previous_rows<-combined_ongoing_mo[,.N]
    #remove duplicates
    combined_ongoing_mo[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
    combined_ongoing_mo<-combined_ongoing_mo[!duplicated(pers_date)]
    combined_ongoing_mo[,pers_date:=NULL]
    no_duplicates_rows_mo<-combined_ongoing_mo[,.N]
    duplicated_dates_rows_mo<-previous_rows - no_duplicates_rows_mo
    #remove duplicates time lag
    if(combined_ongoing_mo[!is.na(lag),.N]>0){
      #Step 1: Order the dataset by person_id, condition and date of event
      combined_ongoing_mo<-combined_ongoing_mo[order(person_id,condition,pregnancy_code_date)]
      #Step 2: Create date_2(by shifting the first date)
      combined_ongoing_mo[,date_1:=shift(pregnancy_code_date)]
      combined_ongoing_mo[,date_2:=pregnancy_code_date]
      #Step 3: Create rowid(which will give the number of rows for each person)
      combined_ongoing_mo[,rowid:=rowid(person_id)]
      #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
      combined_ongoing_mo[rowid==1,date_1:=NA]
      #Step 5: Create date_dif as difference between date 2 and date
      combined_ongoing_mo[,date_dif:=date_2-date_1]
      #Step 6: Remove these rows 
      combined_ongoing_mo<-combined_ongoing_mo[date_dif>lag | is.na(date_dif)]
      #Step 7: Repeat until there are no more impossible dates in the dataset
      combined_ongoing_mo[,date_dif:=date_2-date_1]
      while(combined_ongoing_mo[date_dif<=lag,.N]>0){
        combined_ongoing_mo<-combined_ongoing_mo[date_dif>lag | is.na(date_dif)]
        combined_ongoing_mo[,date_dif:=date_2-date_1]
      }
      
      rows_no_duplicated_mo<-combined_ongoing_mo[,.N]
      duplicates_time_lag_mo<-no_duplicates_rows_mo - rows_no_duplicated_mo
      
      combined_ongoing_mo[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
      
    }
    ongoing_ind<-ongoing_ind+1
  }
  
  #remove all events files and save the int_events at preg_tmp
  for(i in 1: length(pregnancy_files_mo$ongoing_pregnancy)){
    file.remove(paste0(preg_m_tmp,pregnancy_files_mo$ongoing_pregnancy[i]))
  }
  
  if(combined_ongoing_mo[,.N]>0){
    years_preg<-sort(combined_ongoing_mo[!duplicated(year),year])
    for(year_ind in 1:length(years_preg)){
      saveRDS(combined_ongoing_mo[year==years_preg[year_ind]], paste0(preg_tmp,years_preg[year_ind], "_ongoing_mo.rds"))
    }
  }
  rm(combined_ongoing_mo)
} else {
  orig_no_row_mo<-0
  duplicated_dates_rows_mo<-0
  duplicates_time_lag_mo<-0
}
####ongoing_pregnancy_so####
if(length(pregnancy_files_so$ongoing_pregnancy)>0){
  combined_ongoing_so<-readRDS(paste0(preg_s_tmp,pregnancy_files_so$ongoing_pregnancy[1]))
  orig_no_row_so<-combined_ongoing_so[,.N]
  #remove the duplicate dates and save number removed
  combined_ongoing_so[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
  combined_ongoing_so<-combined_ongoing_so[!duplicated(pers_date)]
  combined_ongoing_so[,pers_date:=NULL]
  no_duplicates_rows_so<-combined_ongoing_so[,.N]
  duplicated_dates_rows_so<-orig_no_row_so - no_duplicates_rows_so
  #remove within time lag dates
  combined_ongoing_so[,lag:=time_lag[stage_of_pregnancy=="ongoing_pregnancy",time_lag]]
  if(combined_ongoing_so[!is.na(lag),.N]>0){
    #Step 1: Order the dataset by person_id, condition and date of event
    combined_ongoing_so<-combined_ongoing_so[order(person_id,condition,pregnancy_code_date)]
    #Step 2: Create date_2(by shifting the first date)
    combined_ongoing_so[,date_1:=shift(pregnancy_code_date)]
    combined_ongoing_so[,date_2:=pregnancy_code_date]
    #Step 3: Create rowid(which will give the number of rows for each person)
    combined_ongoing_so[,rowid:=rowid(person_id)]
    #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
    combined_ongoing_so[rowid==1,date_1:=NA]
    #Step 5: Create date_dif as difference between date 2 and date
    combined_ongoing_so[,date_dif:=date_2-date_1]
    #Step 6: Remove these rows 
    combined_ongoing_so<-combined_ongoing_so[date_dif>lag | is.na(date_dif)]
    #Step 7: Repeat until there are no more impossible dates in the dataset
    combined_ongoing_so[,date_dif:=date_2-date_1]
    while(combined_ongoing_so[date_dif<=lag,.N]>0){
      combined_ongoing_so<-combined_ongoing_so[date_dif>lag | is.na(date_dif)]
      combined_ongoing_so[,date_dif:=date_2-date_1]
    }
    
    rows_no_duplicated_so<-combined_ongoing_so[,.N]
    duplicates_time_lag_so<-no_duplicates_rows_so - rows_no_duplicated_so
    
    combined_ongoing_so[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
    
  }
  
  #Add other files one by one
  ongoing_ind<-2
  while(ongoing_ind<=length(pregnancy_files_so$ongoing_pregnancy)){
    #load next file
    a<-readRDS(paste0(preg_s_tmp,pregnancy_files_so$ongoing_pregnancy[ongoing_ind]))
    #create var lag
    a[,lag:=time_lag[stage_of_pregnancy=="ongoing_pregnancy",time_lag]]
    #add number of rows to original no of rows
    orig_no_row_so<-orig_no_row_so+a[,.N]
    file_add_rows<-a[,.N]
    #Combine files together
    combined_ongoing_so<-rbind(combined_ongoing_so,a)
    rm(a)
    previous_rows<-combined_ongoing_so[,.N]
    #remove duplicates
    combined_ongoing_so[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
    combined_ongoing_so<-combined_ongoing_so[!duplicated(pers_date)]
    combined_ongoing_so[,pers_date:=NULL]
    no_duplicates_rows_so<-combined_ongoing_so[,.N]
    duplicated_dates_rows_so<-previous_rows - no_duplicates_rows_so
    #remove duplicates time lag
    if(combined_ongoing_so[!is.na(lag),.N]>0){
      #Step 1: Order the dataset by person_id, condition and date of event
      combined_ongoing_so<-combined_ongoing_so[order(person_id,condition,pregnancy_code_date)]
      #Step 2: Create date_2(by shifting the first date)
      combined_ongoing_so[,date_1:=shift(pregnancy_code_date)]
      combined_ongoing_so[,date_2:=pregnancy_code_date]
      #Step 3: Create rowid(which will give the number of rows for each person)
      combined_ongoing_so[,rowid:=rowid(person_id)]
      #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
      combined_ongoing_so[rowid==1,date_1:=NA]
      #Step 5: Create date_dif as difference between date 2 and date
      combined_ongoing_so[,date_dif:=date_2-date_1]
      #Step 6: Remove these rows 
      combined_ongoing_so<-combined_ongoing_so[date_dif>lag | is.na(date_dif)]
      #Step 7: Repeat until there are no more impossible dates in the dataset
      combined_ongoing_so[,date_dif:=date_2-date_1]
      while(combined_ongoing_so[date_dif<=lag,.N]>0){
        combined_ongoing_so<-combined_ongoing_so[date_dif>lag | is.na(date_dif)]
        combined_ongoing_so[,date_dif:=date_2-date_1]
      }
      
      rows_no_duplicated_so<-combined_ongoing_so[,.N]
      duplicates_time_lag_so<-no_duplicates_rows_so - rows_no_duplicated_so
      
      combined_ongoing_so[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
      
    }
    ongoing_ind<-ongoing_ind+1
  }
  
  #remove all events files and save the int_events at preg_tmp
  for(i in 1: length(pregnancy_files_so$ongoing_pregnancy)){
    file.remove(paste0(preg_s_tmp,pregnancy_files_so$ongoing_pregnancy[i]))
  }
  
  if(combined_ongoing_so[,.N]>0){
    years_preg<-sort(combined_ongoing_so[!duplicated(year),year])
    for(year_ind in 1:length(years_preg)){
      saveRDS(combined_ongoing_so[year==years_preg[year_ind]], paste0(preg_tmp,years_preg[year_ind], "_ongoing_so.rds"))
    }
  }
  rm(combined_ongoing_so)
} else {
  orig_no_row_so<-0
  duplicated_dates_rows_so<-0
  duplicates_time_lag_so<-0
}
####ongoing_pregnancy_si####
if(length(pregnancy_files_si$ongoing_pregnancy)>0){
  combined_ongoing_si<-readRDS(paste0(preg_si_tmp,pregnancy_files_si$ongoing_pregnancy[1]))
  orig_no_row_si<-combined_ongoing_si[,.N]
  #remove the duplicate dates and save number removed
  combined_ongoing_si[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
  combined_ongoing_si<-combined_ongoing_si[!duplicated(pers_date)]
  combined_ongoing_si[,pers_date:=NULL]
  no_duplicates_rows_si<-combined_ongoing_si[,.N]
  duplicated_dates_rows_si<-orig_no_row_si - no_duplicates_rows_si
  #remove within time lag dates
  combined_ongoing_si[,lag:=time_lag[stage_of_pregnancy=="ongoing_pregnancy",time_lag]]
  if(combined_ongoing_si[!is.na(lag),.N]>0){
    #Step 1: Order the dataset by person_id, condition and date of event
    combined_ongoing_si<-combined_ongoing_si[order(person_id,condition,pregnancy_code_date)]
    #Step 2: Create date_2(by shifting the first date)
    combined_ongoing_si[,date_1:=shift(pregnancy_code_date)]
    combined_ongoing_si[,date_2:=pregnancy_code_date]
    #Step 3: Create rowid(which will give the number of rows for each person)
    combined_ongoing_si[,rowid:=rowid(person_id)]
    #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
    combined_ongoing_si[rowid==1,date_1:=NA]
    #Step 5: Create date_dif as difference between date 2 and date
    combined_ongoing_si[,date_dif:=date_2-date_1]
    #Step 6: Remove these rows 
    combined_ongoing_si<-combined_ongoing_si[date_dif>lag | is.na(date_dif)]
    #Step 7: Repeat until there are no more impossible dates in the dataset
    combined_ongoing_si[,date_dif:=date_2-date_1]
    while(combined_ongoing_si[date_dif<=lag,.N]>0){
      combined_ongoing_si<-combined_ongoing_si[date_dif>lag | is.na(date_dif)]
      combined_ongoing_si[,date_dif:=date_2-date_1]
    }
    
    rows_no_duplicated_si<-combined_ongoing_si[,.N]
    duplicates_time_lag_si<-no_duplicates_rows_si - rows_no_duplicated_si
    
    combined_ongoing_si[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
    
  }
  
  #Add other files one by one
  ongoing_ind<-2
  while(ongoing_ind<=length(pregnancy_files_si$ongoing_pregnancy)){
    #load next file
    a<-readRDS(paste0(preg_si_tmp,pregnancy_files_si$ongoing_pregnancy[ongoing_ind]))
    #create var lag
    a[,lag:=time_lag[stage_of_pregnancy=="ongoing_pregnancy",time_lag]]
    #add number of rows to original no of rows
    orig_no_row_si<-orig_no_row_si+a[,.N]
    file_add_rows<-a[,.N]
    #Combine files together
    combined_ongoing_si<-rbind(combined_ongoing_si,a)
    rm(a)
    previous_rows<-combined_ongoing_si[,.N]
    #remove duplicates
    combined_ongoing_si[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
    combined_ongoing_si<-combined_ongoing_si[!duplicated(pers_date)]
    combined_ongoing_si[,pers_date:=NULL]
    no_duplicates_rows_si<-combined_ongoing_si[,.N]
    duplicated_dates_rows_si<-previous_rows - no_duplicates_rows_si
    #remove duplicates time lag
    if(combined_ongoing_si[!is.na(lag),.N]>0){
      #Step 1: Order the dataset by person_id, condition and date of event
      combined_ongoing_si<-combined_ongoing_si[order(person_id,condition,pregnancy_code_date)]
      #Step 2: Create date_2(by shifting the first date)
      combined_ongoing_si[,date_1:=shift(pregnancy_code_date)]
      combined_ongoing_si[,date_2:=pregnancy_code_date]
      #Step 3: Create rowid(which will give the number of rows for each person)
      combined_ongoing_si[,rowid:=rowid(person_id)]
      #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
      combined_ongoing_si[rowid==1,date_1:=NA]
      #Step 5: Create date_dif as difference between date 2 and date
      combined_ongoing_si[,date_dif:=date_2-date_1]
      #Step 6: Remove these rows 
      combined_ongoing_si<-combined_ongoing_si[date_dif>lag | is.na(date_dif)]
      #Step 7: Repeat until there are no more impossible dates in the dataset
      combined_ongoing_si[,date_dif:=date_2-date_1]
      while(combined_ongoing_si[date_dif<=lag,.N]>0){
        combined_ongoing_si<-combined_ongoing_si[date_dif>lag | is.na(date_dif)]
        combined_ongoing_si[,date_dif:=date_2-date_1]
      }
      
      rows_no_duplicated_si<-combined_ongoing_si[,.N]
      duplicates_time_lag_si<-no_duplicates_rows_si - rows_no_duplicated_si
      
      combined_ongoing_si[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
      
    }
    ongoing_ind<-ongoing_ind+1
  }
  
  #remove all events files and save the int_events at preg_tmp
  for(i in 1: length(pregnancy_files_si$ongoing_pregnancy)){
    file.remove(paste0(preg_si_tmp,pregnancy_files_si$ongoing_pregnancy[i]))
  }
  
  if(combined_ongoing_si[,.N]>0){
    years_preg<-sort(combined_ongoing_si[!duplicated(year),year])
    for(year_ind in 1:length(years_preg)){
      saveRDS(combined_ongoing_si[year==years_preg[year_ind]], paste0(preg_tmp,years_preg[year_ind], "_ongoing_si.rds"))
    }
  }
  rm(combined_ongoing_si)
} else {
  orig_no_row_si<-0
  duplicated_dates_rows_si<-0
  duplicates_time_lag_si<-0
}
####ongoing_pregnancy duplicates removal####
orig_no_row_ongoing<-orig_no_row_events+orig_no_row_mo+orig_no_row_so+orig_no_row_si
duplicated_dates_rows_ongoing<-duplicated_dates_rows_events+duplicated_dates_rows_mo+duplicated_dates_rows_so+duplicated_dates_rows_si
duplicates_time_lag_ongoing<-duplicates_time_lag_events+duplicates_time_lag_mo+duplicates_time_lag_so+duplicates_time_lag_si
duplicated_preg_dates[event_definition=="ongoing_pregnancy",original_rows:=orig_no_row_ongoing]
duplicated_preg_dates[event_definition=="ongoing_pregnancy",duplicates:=duplicated_dates_rows_ongoing]
duplicated_preg_dates[event_definition=="ongoing_pregnancy",duplicates_time_lag:=duplicates_time_lag_ongoing]
rm(orig_no_row_ongoing,orig_no_row_events,orig_no_row_mo,orig_no_row_so,orig_no_row_si)
rm(duplicated_dates_rows_ongoing,duplicated_dates_rows_events,duplicated_dates_rows_mo,duplicated_dates_rows_so,duplicated_dates_rows_si)
rm(duplicates_time_lag_ongoing,duplicates_time_lag_mo,duplicates_time_lag_so,duplicates_time_lag_si)
####end_of_pregnancy####
####end_of_pregnancy_events####
if(length(pregnancy_files_events$end_of_pregnancy)>0){
  combined_end_ev<-readRDS(paste0(preg_ev_tmp,pregnancy_files_events$end_of_pregnancy[1]))
  orig_no_row_events<-combined_end_ev[,.N]
  #remove the duplicate dates and save number removed
  combined_end_ev[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
  combined_end_ev<-combined_end_ev[!duplicated(pers_date)]
  combined_end_ev[,pers_date:=NULL]
  no_duplicates_rows_events<-combined_end_ev[,.N]
  duplicated_dates_rows_events<-orig_no_row_events - no_duplicates_rows_events
  #remove within time lag dates
  combined_end_ev[,lag:=time_lag[stage_of_pregnancy=="end_of_pregnancy",time_lag]]
  if(combined_end_ev[!is.na(lag),.N]>0){
    #Step 1: Order the dataset by person_id, condition and date of event
    combined_end_ev<-combined_end_ev[order(person_id,condition,pregnancy_code_date)]
    #Step 2: Create date_2(by shifting the first date)
    combined_end_ev[,date_1:=shift(pregnancy_code_date)]
    combined_end_ev[,date_2:=pregnancy_code_date]
    #Step 3: Create rowid(which will give the number of rows for each person)
    combined_end_ev[,rowid:=rowid(person_id)]
    #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
    combined_end_ev[rowid==1,date_1:=NA]
    #Step 5: Create date_dif as difference between date 2 and date
    combined_end_ev[,date_dif:=date_2-date_1]
    #Step 6: Remove these rows 
    combined_end_ev<-combined_end_ev[date_dif>lag | is.na(date_dif)]
    #Step 7: Repeat until there are no more impossible dates in the dataset
    combined_end_ev[,date_dif:=date_2-date_1]
    while(combined_end_ev[date_dif<=lag,.N]>0){
      combined_end_ev<-combined_end_ev[date_dif>lag | is.na(date_dif)]
      combined_end_ev[,date_dif:=date_2-date_1]
    }
    
    rows_no_duplicated_events<-combined_end_ev[,.N]
    duplicates_time_lag_events<-no_duplicates_rows_events - rows_no_duplicated_events
    
    combined_end_ev[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
    
  }
  
  #Add other files one by one
  end_ind<-2
  while(end_ind<=length(pregnancy_files_events$end_of_pregnancy)){
    #load next file
    a<-readRDS(paste0(preg_ev_tmp,pregnancy_files_events$end_of_pregnancy[end_ind]))
    #create var lag
    a[,lag:=time_lag[stage_of_pregnancy=="end_of_pregnancy",time_lag]]
    #add number of rows to original no of rows
    orig_no_row_events<-orig_no_row_events+a[,.N]
    file_add_rows<-a[,.N]
    #Combine files together
    combined_end_ev<-rbind(combined_end_ev,a)
    rm(a)
    previous_rows<-combined_end_ev[,.N]
    #remove duplicates
    combined_end_ev[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
    combined_end_ev<-combined_end_ev[!duplicated(pers_date)]
    combined_end_ev[,pers_date:=NULL]
    no_duplicates_rows_events<-combined_end_ev[,.N]
    duplicated_dates_rows_events<-previous_rows - no_duplicates_rows_events
    #remove duplicates time lag
    if(combined_end_ev[!is.na(lag),.N]>0){
      #Step 1: Order the dataset by person_id, condition and date of event
      combined_end_ev<-combined_end_ev[order(person_id,condition,pregnancy_code_date)]
      #Step 2: Create date_2(by shifting the first date)
      combined_end_ev[,date_1:=shift(pregnancy_code_date)]
      combined_end_ev[,date_2:=pregnancy_code_date]
      #Step 3: Create rowid(which will give the number of rows for each person)
      combined_end_ev[,rowid:=rowid(person_id)]
      #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
      combined_end_ev[rowid==1,date_1:=NA]
      #Step 5: Create date_dif as difference between date 2 and date
      combined_end_ev[,date_dif:=date_2-date_1]
      #Step 6: Remove these rows 
      combined_end_ev<-combined_end_ev[date_dif>lag | is.na(date_dif)]
      #Step 7: Repeat until there are no more impossible dates in the dataset
      combined_end_ev[,date_dif:=date_2-date_1]
      while(combined_end_ev[date_dif<=lag,.N]>0){
        combined_end_ev<-combined_end_ev[date_dif>lag | is.na(date_dif)]
        combined_end_ev[,date_dif:=date_2-date_1]
      }
      
      rows_no_duplicated_events<-combined_end_ev[,.N]
      duplicates_time_lag_events<-no_duplicates_rows_events - rows_no_duplicated_events
      
      combined_end_ev[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
      
    }
    end_ind<-end_ind+1
  }
  
  #remove all events files and save the int_events at preg_tmp
  for(i in 1: length(pregnancy_files_events$end_of_pregnancy)){
    file.remove(paste0(preg_ev_tmp,pregnancy_files_events$end_of_pregnancy[i]))
  }
  
  if(combined_end_ev[,.N]>0){
    years_preg<-sort(combined_end_ev[!duplicated(year),year])
    for(year_ind in 1:length(years_preg)){
      saveRDS(combined_end_ev[year==years_preg[year_ind]], paste0(preg_tmp,years_preg[year_ind], "_end_events.rds"))
    }
  }
  rm(combined_end_ev)
} else {
  orig_no_row_events<-0
  duplicated_dates_rows_events<-0
  duplicates_time_lag_events<-0
}
####end_of_pregnancy_mo####
if(length(pregnancy_files_mo$end_of_pregnancy)>0){
  combined_end_mo<-readRDS(paste0(preg_m_tmp,pregnancy_files_mo$end_of_pregnancy[1]))
  orig_no_row_mo<-combined_end_mo[,.N]
  #remove the duplicate dates and save number removed
  combined_end_mo[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
  combined_end_mo<-combined_end_mo[!duplicated(pers_date)]
  combined_end_mo[,pers_date:=NULL]
  no_duplicates_rows_mo<-combined_end_mo[,.N]
  duplicated_dates_rows_mo<-orig_no_row_mo - no_duplicates_rows_mo
  #remove within time lag dates
  combined_end_mo[,lag:=time_lag[stage_of_pregnancy=="end_of_pregnancy",time_lag]]
  if(combined_end_mo[!is.na(lag),.N]>0){
    #Step 1: Order the dataset by person_id, condition and date of event
    combined_end_mo<-combined_end_mo[order(person_id,condition,pregnancy_code_date)]
    #Step 2: Create date_2(by shifting the first date)
    combined_end_mo[,date_1:=shift(pregnancy_code_date)]
    combined_end_mo[,date_2:=pregnancy_code_date]
    #Step 3: Create rowid(which will give the number of rows for each person)
    combined_end_mo[,rowid:=rowid(person_id)]
    #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
    combined_end_mo[rowid==1,date_1:=NA]
    #Step 5: Create date_dif as difference between date 2 and date
    combined_end_mo[,date_dif:=date_2-date_1]
    #Step 6: Remove these rows 
    combined_end_mo<-combined_end_mo[date_dif>lag | is.na(date_dif)]
    #Step 7: Repeat until there are no more impossible dates in the dataset
    combined_end_mo[,date_dif:=date_2-date_1]
    while(combined_end_mo[date_dif<=lag,.N]>0){
      combined_end_mo<-combined_end_mo[date_dif>lag | is.na(date_dif)]
      combined_end_mo[,date_dif:=date_2-date_1]
    }
    
    rows_no_duplicated_mo<-combined_end_mo[,.N]
    duplicates_time_lag_mo<-no_duplicates_rows_mo - rows_no_duplicated_mo
    
    combined_end_mo[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
    
  }
  
  #Add other files one by one
  end_ind<-2
  while(end_ind<=length(pregnancy_files_mo$end_of_pregnancy)){
    #load next file
    a<-readRDS(paste0(preg_m_tmp,pregnancy_files_mo$end_of_pregnancy[end_ind]))
    #create var lag
    a[,lag:=time_lag[stage_of_pregnancy=="end_of_pregnancy",time_lag]]
    #add number of rows to original no of rows
    orig_no_row_mo<-orig_no_row_mo+a[,.N]
    file_add_rows<-a[,.N]
    #Combine files together
    combined_end_mo<-rbind(combined_end_mo,a)
    rm(a)
    previous_rows<-combined_end_mo[,.N]
    #remove duplicates
    combined_end_mo[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
    combined_end_mo<-combined_end_mo[!duplicated(pers_date)]
    combined_end_mo[,pers_date:=NULL]
    no_duplicates_rows_mo<-combined_end_mo[,.N]
    duplicated_dates_rows_mo<-previous_rows - no_duplicates_rows_mo
    #remove duplicates time lag
    if(combined_end_mo[!is.na(lag),.N]>0){
      #Step 1: Order the dataset by person_id, condition and date of event
      combined_end_mo<-combined_end_mo[order(person_id,condition,pregnancy_code_date)]
      #Step 2: Create date_2(by shifting the first date)
      combined_end_mo[,date_1:=shift(pregnancy_code_date)]
      combined_end_mo[,date_2:=pregnancy_code_date]
      #Step 3: Create rowid(which will give the number of rows for each person)
      combined_end_mo[,rowid:=rowid(person_id)]
      #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
      combined_end_mo[rowid==1,date_1:=NA]
      #Step 5: Create date_dif as difference between date 2 and date
      combined_end_mo[,date_dif:=date_2-date_1]
      #Step 6: Remove these rows 
      combined_end_mo<-combined_end_mo[date_dif>lag | is.na(date_dif)]
      #Step 7: Repeat until there are no more impossible dates in the dataset
      combined_end_mo[,date_dif:=date_2-date_1]
      while(combined_end_mo[date_dif<=lag,.N]>0){
        combined_end_mo<-combined_end_mo[date_dif>lag | is.na(date_dif)]
        combined_end_mo[,date_dif:=date_2-date_1]
      }
      
      rows_no_duplicated_mo<-combined_end_mo[,.N]
      duplicates_time_lag_mo<-no_duplicates_rows_mo - rows_no_duplicated_mo
      
      combined_end_mo[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
      
    }
    end_ind<-end_ind+1
  }
  
  #remove all events files and save the int_events at preg_tmp
  for(i in 1: length(pregnancy_files_mo$end_of_pregnancy)){
    file.remove(paste0(preg_m_tmp,pregnancy_files_mo$end_of_pregnancy[i]))
  }
  
  if(combined_end_mo[,.N]>0){
    years_preg<-sort(combined_end_mo[!duplicated(year),year])
    for(year_ind in 1:length(years_preg)){
      saveRDS(combined_end_mo[year==years_preg[year_ind]], paste0(preg_tmp,years_preg[year_ind], "_end_mo.rds"))
    }
  }
  rm(combined_end_mo)
} else {
  orig_no_row_mo<-0
  duplicated_dates_rows_mo<-0
  duplicates_time_lag_mo<-0
}
####end_of_pregnancy_so####
if(length(pregnancy_files_so$end_of_pregnancy)>0){
  combined_end_so<-readRDS(paste0(preg_s_tmp,pregnancy_files_so$end_of_pregnancy[1]))
  orig_no_row_so<-combined_end_so[,.N]
  #remove the duplicate dates and save number removed
  combined_end_so[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
  combined_end_so<-combined_end_so[!duplicated(pers_date)]
  combined_end_so[,pers_date:=NULL]
  no_duplicates_rows_so<-combined_end_so[,.N]
  duplicated_dates_rows_so<-orig_no_row_so - no_duplicates_rows_so
  #remove within time lag dates
  combined_end_so[,lag:=time_lag[stage_of_pregnancy=="end_of_pregnancy",time_lag]]
  if(combined_end_so[!is.na(lag),.N]>0){
    #Step 1: Order the dataset by person_id, condition and date of event
    combined_end_so<-combined_end_so[order(person_id,condition,pregnancy_code_date)]
    #Step 2: Create date_2(by shifting the first date)
    combined_end_so[,date_1:=shift(pregnancy_code_date)]
    combined_end_so[,date_2:=pregnancy_code_date]
    #Step 3: Create rowid(which will give the number of rows for each person)
    combined_end_so[,rowid:=rowid(person_id)]
    #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
    combined_end_so[rowid==1,date_1:=NA]
    #Step 5: Create date_dif as difference between date 2 and date
    combined_end_so[,date_dif:=date_2-date_1]
    #Step 6: Remove these rows 
    combined_end_so<-combined_end_so[date_dif>lag | is.na(date_dif)]
    #Step 7: Repeat until there are no more impossible dates in the dataset
    combined_end_so[,date_dif:=date_2-date_1]
    while(combined_end_so[date_dif<=lag,.N]>0){
      combined_end_so<-combined_end_so[date_dif>lag | is.na(date_dif)]
      combined_end_so[,date_dif:=date_2-date_1]
    }
    
    rows_no_duplicated_so<-combined_end_so[,.N]
    duplicates_time_lag_so<-no_duplicates_rows_so - rows_no_duplicated_so
    
    combined_end_so[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
    
  }
  
  #Add other files one by one
  end_ind<-2
  while(end_ind<=length(pregnancy_files_so$end_of_pregnancy)){
    #load next file
    a<-readRDS(paste0(preg_s_tmp,pregnancy_files_so$end_of_pregnancy[end_ind]))
    #create var lag
    a[,lag:=time_lag[stage_of_pregnancy=="end_of_pregnancy",time_lag]]
    #add number of rows to original no of rows
    orig_no_row_so<-orig_no_row_so+a[,.N]
    file_add_rows<-a[,.N]
    #Combine files together
    combined_end_so<-rbind(combined_end_so,a)
    rm(a)
    previous_rows<-combined_end_so[,.N]
    #remove duplicates
    combined_end_so[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
    combined_end_so<-combined_end_so[!duplicated(pers_date)]
    combined_end_so[,pers_date:=NULL]
    no_duplicates_rows_so<-combined_end_so[,.N]
    duplicated_dates_rows_so<-previous_rows - no_duplicates_rows_so
    #remove duplicates time lag
    if(combined_end_so[!is.na(lag),.N]>0){
      #Step 1: Order the dataset by person_id, condition and date of event
      combined_end_so<-combined_end_so[order(person_id,condition,pregnancy_code_date)]
      #Step 2: Create date_2(by shifting the first date)
      combined_end_so[,date_1:=shift(pregnancy_code_date)]
      combined_end_so[,date_2:=pregnancy_code_date]
      #Step 3: Create rowid(which will give the number of rows for each person)
      combined_end_so[,rowid:=rowid(person_id)]
      #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
      combined_end_so[rowid==1,date_1:=NA]
      #Step 5: Create date_dif as difference between date 2 and date
      combined_end_so[,date_dif:=date_2-date_1]
      #Step 6: Remove these rows 
      combined_end_so<-combined_end_so[date_dif>lag | is.na(date_dif)]
      #Step 7: Repeat until there are no more impossible dates in the dataset
      combined_end_so[,date_dif:=date_2-date_1]
      while(combined_end_so[date_dif<=lag,.N]>0){
        combined_end_so<-combined_end_so[date_dif>lag | is.na(date_dif)]
        combined_end_so[,date_dif:=date_2-date_1]
      }
      
      rows_no_duplicated_so<-combined_end_so[,.N]
      duplicates_time_lag_so<-no_duplicates_rows_so - rows_no_duplicated_so
      
      combined_end_so[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
      
    }
    end_ind<-end_ind+1
  }
  
  #remove all events files and save the int_events at preg_tmp
  for(i in 1: length(pregnancy_files_so$end_of_pregnancy)){
    file.remove(paste0(preg_s_tmp,pregnancy_files_so$end_of_pregnancy[i]))
  }
  
  if(combined_end_so[,.N]>0){
    years_preg<-sort(combined_end_so[!duplicated(year),year])
    for(year_ind in 1:length(years_preg)){
      saveRDS(combined_end_so[year==years_preg[year_ind]], paste0(preg_tmp,years_preg[year_ind], "_end_so.rds"))
    }
  }
  rm(combined_end_so)
} else {
  orig_no_row_so<-0
  duplicated_dates_rows_so<-0
  duplicates_time_lag_so<-0
}
####end_of_pregnancy_si####
if(length(pregnancy_files_si$end_of_pregnancy)>0){
  combined_end_si<-readRDS(paste0(preg_si_tmp,pregnancy_files_si$end_of_pregnancy[1]))
  orig_no_row_si<-combined_end_si[,.N]
  #remove the duplicate dates and save number removed
  combined_end_si[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
  combined_end_si<-combined_end_si[!duplicated(pers_date)]
  combined_end_si[,pers_date:=NULL]
  no_duplicates_rows_si<-combined_end_si[,.N]
  duplicated_dates_rows_si<-orig_no_row_si - no_duplicates_rows_si
  #remove within time lag dates
  combined_end_si[,lag:=time_lag[stage_of_pregnancy=="end_of_pregnancy",time_lag]]
  if(combined_end_si[!is.na(lag),.N]>0){
    #Step 1: Order the dataset by person_id, condition and date of event
    combined_end_si<-combined_end_si[order(person_id,condition,pregnancy_code_date)]
    #Step 2: Create date_2(by shifting the first date)
    combined_end_si[,date_1:=shift(pregnancy_code_date)]
    combined_end_si[,date_2:=pregnancy_code_date]
    #Step 3: Create rowid(which will give the number of rows for each person)
    combined_end_si[,rowid:=rowid(person_id)]
    #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
    combined_end_si[rowid==1,date_1:=NA]
    #Step 5: Create date_dif as difference between date 2 and date
    combined_end_si[,date_dif:=date_2-date_1]
    #Step 6: Remove these rows 
    combined_end_si<-combined_end_si[date_dif>lag | is.na(date_dif)]
    #Step 7: Repeat until there are no more impossible dates in the dataset
    combined_end_si[,date_dif:=date_2-date_1]
    while(combined_end_si[date_dif<=lag,.N]>0){
      combined_end_si<-combined_end_si[date_dif>lag | is.na(date_dif)]
      combined_end_si[,date_dif:=date_2-date_1]
    }
    
    rows_no_duplicated_si<-combined_end_si[,.N]
    duplicates_time_lag_si<-no_duplicates_rows_si - rows_no_duplicated_si
    
    combined_end_si[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
    
  }
  
  #Add other files one by one
  end_ind<-2
  while(end_ind<=length(pregnancy_files_si$end_of_pregnancy)){
    #load next file
    a<-readRDS(paste0(preg_si_tmp,pregnancy_files_si$end_of_pregnancy[end_ind]))
    #create var lag
    a[,lag:=time_lag[stage_of_pregnancy=="end_of_pregnancy",time_lag]]
    #add number of rows to original no of rows
    orig_no_row_si<-orig_no_row_si+a[,.N]
    file_add_rows<-a[,.N]
    #Combine files together
    combined_end_si<-rbind(combined_end_si,a)
    rm(a)
    previous_rows<-combined_end_si[,.N]
    #remove duplicates
    combined_end_si[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
    combined_end_si<-combined_end_si[!duplicated(pers_date)]
    combined_end_si[,pers_date:=NULL]
    no_duplicates_rows_si<-combined_end_si[,.N]
    duplicated_dates_rows_si<-previous_rows - no_duplicates_rows_si
    #remove duplicates time lag
    if(combined_end_si[!is.na(lag),.N]>0){
      #Step 1: Order the dataset by person_id, condition and date of event
      combined_end_si<-combined_end_si[order(person_id,condition,pregnancy_code_date)]
      #Step 2: Create date_2(by shifting the first date)
      combined_end_si[,date_1:=shift(pregnancy_code_date)]
      combined_end_si[,date_2:=pregnancy_code_date]
      #Step 3: Create rowid(which will give the number of rows for each person)
      combined_end_si[,rowid:=rowid(person_id)]
      #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
      combined_end_si[rowid==1,date_1:=NA]
      #Step 5: Create date_dif as difference between date 2 and date
      combined_end_si[,date_dif:=date_2-date_1]
      #Step 6: Remove these rows 
      combined_end_si<-combined_end_si[date_dif>lag | is.na(date_dif)]
      #Step 7: Repeat until there are no more impossible dates in the dataset
      combined_end_si[,date_dif:=date_2-date_1]
      while(combined_end_si[date_dif<=lag,.N]>0){
        combined_end_si<-combined_end_si[date_dif>lag | is.na(date_dif)]
        combined_end_si[,date_dif:=date_2-date_1]
      }
      
      rows_no_duplicated_si<-combined_end_si[,.N]
      duplicates_time_lag_si<-no_duplicates_rows_si - rows_no_duplicated_si
      
      combined_end_si[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
      
    }
    end_ind<-end_ind+1
  }
  
  #remove all events files and save the int_events at preg_tmp
  for(i in 1: length(pregnancy_files_si$end_of_pregnancy)){
    file.remove(paste0(preg_si_tmp,pregnancy_files_si$end_of_pregnancy[i]))
  }
  
  if(combined_end_si[,.N]>0){
    years_preg<-sort(combined_end_si[!duplicated(year),year])
    for(year_ind in 1:length(years_preg)){
      saveRDS(combined_end_si[year==years_preg[year_ind]], paste0(preg_tmp,years_preg[year_ind], "_end_si.rds"))
    }
  }
  rm(combined_end_si)
} else {
  orig_no_row_si<-0
  duplicated_dates_rows_si<-0
  duplicates_time_lag_si<-0
}
####end_pregnancy duplicates removal####
orig_no_row_end<-orig_no_row_events+orig_no_row_mo+orig_no_row_so+orig_no_row_si
duplicated_dates_rows_end<-duplicated_dates_rows_events+duplicated_dates_rows_mo+duplicated_dates_rows_so+duplicated_dates_rows_si
duplicates_time_lag_end<-duplicates_time_lag_events+duplicates_time_lag_mo+duplicates_time_lag_so+duplicates_time_lag_si

duplicated_preg_dates[event_definition=="end_of_pregnancy",original_rows:=orig_no_row_end]
duplicated_preg_dates[event_definition=="end_of_pregnancy",duplicates:=duplicated_dates_rows_end]
duplicated_preg_dates[event_definition=="end_of_pregnancy",duplicates_time_lag:=duplicates_time_lag_end]
rm(orig_no_row_end,orig_no_row_events,orig_no_row_mo,orig_no_row_so,orig_no_row_si)
rm(duplicated_dates_rows_end,duplicated_dates_rows_events,duplicated_dates_rows_mo,duplicated_dates_rows_so,duplicated_dates_rows_si)
rm(duplicates_time_lag_end,duplicates_time_lag_mo,duplicates_time_lag_so,duplicates_time_lag_si)

####To be removed:info for removed row, intermediate steps####
# orig_no_row_all_intermediate<-orig_no_row_all_start+orig_no_row_all_int+orig_no_row_all_ongoing+orig_no_row_all_end
# duplicated_dates_rows_all_intermediate<-duplicated_dates_rows_all_start+duplicated_dates_rows_all_int+duplicated_dates_rows_all_ongoing+duplicated_dates_rows_all_end
# duplicates_time_lag_all_intermediate<-duplicates_time_lag_all_start+duplicates_time_lag_all_int+duplicates_time_lag_all_ongoing+duplicates_time_lag_all_end

####Combine all files by stage of pregnancy and year, clean up duplicates and time lag duplicates####
start_files<-list.files(preg_tmp, "start")
if(length(start_files)>0){
list_years_start<-list()
years_ind<-1
for(i in 1:length(start_files)){
  list_years_start[[years_ind]]<-str_split(start_files[years_ind],"_")[[1]][1]
  years_ind<-years_ind+1
}
list_years_start<-do.call(rbind,list_years_start)
list_years_start<-sort(unique(list_years_start[,1]))

start_list<-vector(mode="list", length=length(list_years_start))
names(start_list)<-list_years_start
for(i in 1:length(start_list)){
  start_list[[i]]<-start_files[str_detect(start_files, names(start_list)[i])]
}

}else{
  start_list<-NULL
}

interruption_files<-list.files(preg_tmp, "int")
if(length(interruption_files)>0){
  list_years_int<-list()
  years_ind<-1
  for(i in 1:length(interruption_files)){
    list_years_int[[years_ind]]<-str_split(interruption_files[years_ind],"_")[[1]][1]
    years_ind<-years_ind+1
  }
  list_years_int<-do.call(rbind,list_years_int)
  list_years_int<-sort(unique(list_years_int[,1]))
  
  interruption_list<-vector(mode="list", length=length(list_years_int))
  names(interruption_list)<-list_years_int
  for(i in 1:length(interruption_list)){
    interruption_list[[i]]<-interruption_files[str_detect(interruption_files, names(interruption_list)[i])]
  }
  
}else{
  interruption_list<-NULL
}

ongoing_files<-list.files(preg_tmp, "ongoing")
if(length(ongoing_files)>0){
  list_years_ongoing<-list()
  years_ind<-1
  for(i in 1:length(ongoing_files)){
    list_years_ongoing[[years_ind]]<-str_split(ongoing_files[years_ind],"_")[[1]][1]
    years_ind<-years_ind+1
  }
  list_years_ongoing<-do.call(rbind,list_years_ongoing)
  list_years_ongoing<-sort(unique(list_years_ongoing[,1]))
  
  ongoing_list<-vector(mode="list", length=length(list_years_ongoing))
  names(ongoing_list)<-list_years_ongoing
  for(i in 1:length(ongoing_list)){
    ongoing_list[[i]]<-ongoing_files[str_detect(ongoing_files, names(ongoing_list)[i])]
  }
  
}else{
  ongoing_list<-NULL
}

end_files<-list.files(preg_tmp, "end")
if(length(end_files)>0){
  list_years_end<-list()
  years_ind<-1
  for(i in 1:length(end_files)){
    list_years_end[[years_ind]]<-str_split(end_files[years_ind],"_")[[1]][1]
    years_ind<-years_ind+1
  }
  list_years_end<-do.call(rbind,list_years_end)
  list_years_end<-sort(unique(list_years_end[,1]))
  
  end_list<-vector(mode="list", length=length(list_years_end))
  names(end_list)<-list_years_end
  for(i in 1:length(end_list)){
    end_list[[i]]<-end_files[str_detect(end_files, names(end_list)[i])]
  }
  
}else{
  end_list<-NULL
}

duplicated_preg_dates[,duplicates:=as.numeric(duplicates)]
duplicated_preg_dates[,duplicates_time_lag:=as.numeric(duplicates_time_lag)]
###start_of_pregnancy###
w<-1
graph_1_list_start<-list()
graph_1_age_list_start<-list()
graph_1m_list_start<-list()
graph_1m_365_list_start<-list()
graph_1m_age_list_start<-list()
graph_2_list_start<-list()

if(!is.null(start_list)){
  
  #Load the first year
  first_file<-lapply(paste0(preg_tmp,start_list[[1]]), readRDS)
  first_file<-as.data.table(do.call(rbind,first_file))
  
  if(length(start_list)>1){
    for(year_preg_ind in 2:length(start_list)){
      year_to_be_removed<-names(start_list)[year_preg_ind-1]
      #Load next year
      file<-lapply(paste0(preg_tmp,start_list[[year_preg_ind]]), readRDS)
      file<-as.data.table(do.call(rbind,file))
      #Combine first and next file
      file<-rbind(first_file,file)
      rm(first_file)
      rows_start<-file[,.N]
      #Clean up data
      #remove the duplicate dates and save number removed
      file[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
      file<-file[!duplicated(pers_date)]
      file[,pers_date:=NULL]
      duplicated_dates_new<-rows_start - file[,.N]
      duplicated_preg_dates[event_definition=="start_of_pregnancy", duplicates:=duplicates + duplicated_dates_new]
      rm(duplicated_dates_new)
      
      if(file[!is.na(lag),.N]>0){
        #Step 1: Order the dataset by person_id, condition and date of event
        file<-file[order(person_id,condition,pregnancy_code_date)]
        #Step 2: Create date_2(by shifting the first date)
        file[,date_1:=shift(pregnancy_code_date)]
        file[,date_2:=pregnancy_code_date]
        #Step 3: Create rowid(which will give the number of rows for each person)
        file[,rowid:=rowid(person_id)]
        #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
        file[rowid==1,date_1:=NA]
        #Step 5: Create date_dif as difference between date 2 and date
        file[,date_dif:=date_2-date_1]
        #Step 6: Remove these rows 
        file<-file[date_dif>lag | is.na(date_dif)]
        #Step 7: Repeat until there are no more impossible dates in the dataset
        file[,date_dif:=date_2-date_1]
        while(file[date_dif<=lag,.N]>0){
          file<-file[date_dif>lag | is.na(date_dif)]
          file[,date_dif:=date_2-date_1]
        }
        
        duplicates_lag_new<-rows_start-file[,.N]
        duplicated_preg_dates[event_definition=="start_of_pregnancy", duplicates_time_lag:=duplicates_time_lag + duplicates_lag_new]
        file[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
        rm(rows_start,duplicates_lag_new)
        
      }
      
      #Calculate age at pregnancy record date
      file[year==year_to_be_removed,age_at_preg_record:=floor((pregnancy_code_date-birth_date)/365.25)]
      #create age_band
      for(age_band_ind in 1:age_bands_counts[,.N]){
        file[age_at_preg_record>=age_bands_counts[age_band_ind,min] & age_at_preg_record<=age_bands_counts[age_band_ind,max], age_band:=age_bands_counts[age_band_ind,age_band]]
      }
      #Counts for the first file(there are no more duplicates for the first file)
      no_records_year_age<-file[year==year_to_be_removed,.N, by=c("condition", "year","age_band")]
      setnames(no_records_year_age, "N", "no_records")
      no_women_age<-file[year==year_to_be_removed,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","year","age_band")]
      setnames(no_women_age, "person_id", "no_women")
      
      no_records_year<-file[year==year_to_be_removed,.N, by=c("condition", "year")]
      setnames(no_records_year, "N", "no_records")
      no_women<-file[year==year_to_be_removed,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","year")]
      setnames(no_women, "person_id", "no_women")
      
      no_records_year_m_age<-file[year==year_to_be_removed,.N, by=c("condition", "meaning","year", "age_band")]
      setnames(no_records_year_m_age, "N", "no_records")
      no_women_m_age<-file[year==year_to_be_removed,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year","age_band")]
      setnames(no_women_m_age, "person_id", "no_women")
      
      no_records_year_m<-file[year==year_to_be_removed,.N, by=c("condition", "meaning","year")]
      setnames(no_records_year_m, "N", "no_records")
      no_women_m<-file[year==year_to_be_removed,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year")]
      setnames(no_women_m, "person_id", "no_women")
      
      #identify women with more than 365 days of follow up
      file[,date_dif:=end_follow_up_preg-start_follow_up_preg]
      no_records_year_m_365<-file[year==year_to_be_removed & date_dif>=365,.N, by=c("condition", "meaning","year")]
      setnames(no_records_year_m_365, "N", "no_records")
      no_women_m_365<-file[year==year_to_be_removed & date_dif>=365,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year")]
      setnames(no_women_m_365, "person_id", "no_women")
      file[,date_dif:=NULL]
      
      
      #no_records_year_age,no_women_age
      graph_1_age<-merge.data.table(no_records_year_age,no_women_age, by=c("condition","year","age_band"))
      graph_1_age_list_start[[w]]<-graph_1_age
      rm(graph_1_age)
      
      #no_records_year,no_women
      graph_1<-merge.data.table(no_records_year,no_women, by=c("condition","year"))
      graph_1_list_start[[w]]<-graph_1
      rm(graph_1)
      
      #no_records_year_m_age,no_women_m_age
      graph_1m_age<-merge.data.table(no_records_year_m_age,no_women_m_age, by=c("condition","year","meaning","age_band"))
      graph_1m_age_list_start[[w]]<-graph_1m_age
      rm(graph_1m_age)
      
      #no_records_year_m,no_women_m
      graph_1m<-merge.data.table(no_records_year_m,no_women_m, by=c("condition","year","meaning"))
      graph_1m_list_start[[w]]<-graph_1m
      rm(graph_1m)
      
      #no_records_year_m_365,no_women_m_365
      graph_1m_365<-merge.data.table(no_records_year_m_365,no_women_m_365, by=c("condition","year","meaning"))
      graph_1m_365_list_start[[w]]<-graph_1m_365
      rm(graph_1m_365)
      
      #graph_2:no of records per women
      no_rec_women<-file[year==year_to_be_removed,.N, by=c("condition", "person_id","year")]
      res_rec_1<-no_rec_women[N==1, .N, by=c("condition","year")]
      setnames(res_rec_1,"N","1_pregnancy_record")
      res_rec_2<-no_rec_women[N==2, .N, by=c("condition","year")]
      setnames(res_rec_2,"N","2_pregnancy_records")
      res_rec_3<-no_rec_women[N==3, .N, by=c("condition","year")]
      setnames(res_rec_3,"N","3_pregnancy_records")
      res_rec_4<-no_rec_women[N>=4, .N, by=c("condition","year")]
      setnames(res_rec_4,"N","more_than_4_pregnancy_records")
      rm(no_rec_women)
      graph_2<-merge.data.table(res_rec_1,res_rec_2, by=c("condition","year"), all=T)
      graph_2<-merge.data.table(graph_2,res_rec_3, by=c("condition","year"), all=T)
      graph_2<-merge.data.table(graph_2,res_rec_4, by=c("condition","year"), all=T)
      graph_2_list_start[[w]]<-graph_2
      rm(graph_2)
      
      #add all ids that had a start pregnancy code to the study population from the first file
      study_population[person_id %in% file[year==year_to_be_removed,person_id], start_id:=1]
      file[,age_at_preg_record:=NULL][,age_band:=NULL]
      #save the first file in preg_pop
      saveRDS(file[year==year_to_be_removed], paste0(preg_pop, year_to_be_removed,"_start.rds"))
      #remove all records from the first year
      file<-file[!year %in% year_to_be_removed]
      first_file<-file
      rm(file)
      if(year_preg_ind==length(start_list)){
        saveRDS(first_file, paste0(preg_pop, first_file[!duplicated(year),year],"_start.rds"))
        break
      }
      w<-w+1
      year_preg_ind<-year_preg_ind+1
      
    }
    } else{
    rows_start<-first_file[,.N]
    #Clean up data
    #remove the duplicate dates and save number removed
    first_file[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
    first_file<-first_file[!duplicated(pers_date)]
    first_file[,pers_date:=NULL]
    duplicated_dates_new<-rows_start - first_file[,.N]
    duplicated_preg_dates[event_definition=="start_of_pregnancy", duplicates:=duplicates + duplicated_dates_new]
    rm(duplicated_dates_new)
    
    if(first_file[!is.na(lag),.N]>0){
      #Step 1: Order the dataset by person_id, condition and date of event
      first_file<-first_file[order(person_id,condition,pregnancy_code_date)]
      #Step 2: Create date_2(by shifting the first date)
      first_file[,date_1:=shift(pregnancy_code_date)]
      first_file[,date_2:=pregnancy_code_date]
      #Step 3: Create rowid(which will give the number of rows for each person)
      first_file[,rowid:=rowid(person_id)]
      #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
      first_file[rowid==1,date_1:=NA]
      #Step 5: Create date_dif as difference between date 2 and date
      first_file[,date_dif:=date_2-date_1]
      #Step 6: Remove these rows 
      first_file<-first_file[date_dif>lag | is.na(date_dif)]
      #Step 7: Repeat until there are no more impossible dates in the dataset
      first_file[,date_dif:=date_2-date_1]
      while(first_file[date_dif<=lag,.N]>0){
        first_file<-first_file[date_dif>lag | is.na(date_dif)]
        first_file[,date_dif:=date_2-date_1]
      }
      
      duplicates_lag_new<-rows_start-first_file[,.N]
      duplicated_preg_dates[event_definition=="start_of_pregnancy", duplicates_time_lag:=duplicates_time_lag + duplicates_lag_new]
      first_file[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
      rm(rows_start,duplicates_lag_new)
      
    }
    
    #Calculate age at pregnancy record date
    first_file[,age_at_preg_record:=floor((pregnancy_code_date-birth_date)/365.25)]
    #create age_band
    for(age_band_ind in 1:age_bands_counts[,.N]){
      first_file[age_at_preg_record>=age_bands_counts[age_band_ind,min] & age_at_preg_record<=age_bands_counts[age_band_ind,max], age_band:=age_bands_counts[age_band_ind,age_band]]
    }
    
    #Counts for the first first_file(there are no more duplicates for the first first_file)
    no_records_year_age<-first_file[,.N, by=c("condition", "year","age_band")]
    setnames(no_records_year_age, "N", "no_records")
    no_women_age<-first_file[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","year","age_band")]
    setnames(no_women_age, "person_id", "no_women")
    
    no_records_year<-first_file[,.N, by=c("condition", "year")]
    setnames(no_records_year, "N", "no_records")
    no_women<-first_file[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","year")]
    setnames(no_women, "person_id", "no_women")
    
    no_records_year_m_age<-first_file[,.N, by=c("condition", "meaning","year", "age_band")]
    setnames(no_records_year_m_age, "N", "no_records")
    no_women_m_age<-first_file[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year","age_band")]
    setnames(no_women_m_age, "person_id", "no_women")
    
    no_records_year_m<-first_file[,.N, by=c("condition", "meaning","year")]
    setnames(no_records_year_m, "N", "no_records")
    no_women_m<-file[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year")]
    setnames(no_women_m, "person_id", "no_women")
    
    #identify women with more than 365 days of follow up
    first_file[,date_dif:=end_follow_up_preg-start_follow_up_preg]
    no_records_year_m_365<-first_file[date_dif>=365,.N, by=c("condition", "meaning","year")]
    setnames(no_records_year_m_365, "N", "no_records")
    no_women_m_365<-first_file[date_dif>=365,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year")]
    setnames(no_women_m_365, "person_id", "no_women")
    first_file[,date_dif:=NULL]
    
    #no_records_year_age,no_women_age
    graph_1_age<-merge.data.table(no_records_year_age,no_women_age, by=c("condition","year","age_band"))
    graph_1_age_list_start[[w]]<-graph_1_age
    rm(graph_1_age)
    
    #no_records_year,no_women
    graph_1<-merge.data.table(no_records_year,no_women, by=c("condition","year"))
    graph_1_list_start[[w]]<-graph_1
    rm(graph_1)
    
    #no_records_year_m_age,no_women_m_age
    graph_1m_age<-merge.data.table(no_records_year_m_age,no_women_m_age, by=c("condition","year","meaning","age_band"))
    graph_1m_age_list_start[[w]]<-graph_1m_age
    rm(graph_1m_age)
    
    #no_records_year_m,no_women_m
    graph_1m<-merge.data.table(no_records_year_m,no_women_m, by=c("condition","year","meaning"))
    graph_1m_list_start[[w]]<-graph_1m
    rm(graph_1m)
    
    #no_records_year_m_365,no_women_m_365
    graph_1m_365<-merge.data.table(no_records_year_m_365,no_women_m_365, by=c("condition","year","meaning"))
    graph_1m_365_list_start[[w]]<-graph_1m_365
    rm(graph_1m_365)
    
    #graph_2:no of records per women
    no_rec_women<-first_file[,.N, by=c("condition", "person_id","year")]
    res_rec_1<-no_rec_women[N==1, .N, by=c("condition","year")]
    setnames(res_rec_1,"N","1_pregnancy_record")
    res_rec_2<-no_rec_women[N==2, .N, by=c("condition","year")]
    setnames(res_rec_2,"N","2_pregnancy_records")
    res_rec_3<-no_rec_women[N==3, .N, by=c("condition","year")]
    setnames(res_rec_3,"N","3_pregnancy_records")
    res_rec_4<-no_rec_women[N>=4, .N, by=c("condition","year")]
    setnames(res_rec_4,"N","more_than_4_pregnancy_records")
    rm(no_rec_women)
    graph_2<-merge.data.table(res_rec_1,res_rec_2, by=c("condition","year"), all=T)
    graph_2<-merge.data.table(graph_2,res_rec_3, by=c("condition","year"), all=T)
    graph_2<-merge.data.table(graph_2,res_rec_4, by=c("condition","year"), all=T)
    graph_2_list_start[[w]]<-graph_2
    rm(graph_2)
    
    #add all ids that had a start pregnancy code to the study population from the first file
    study_population[person_id %in% first_file[,person_id], start_id:=1]
    first_file[,age_at_preg_record:=NULL][,age_band:=NULL]
    #save the first file in preg_pop
    saveRDS(first_file, paste0(preg_pop, first_file[!duplicated(year),year],"_start.rds"))
  }
  
  rm(first_file)
  for(i in 1:length(start_files)){
    file.remove(paste0(preg_tmp,start_files[[i]]))
  }
}else {
  study_population[,start_id:=NA]
}

###interruption_pregnancy###
w<-1
graph_1_list_interruption<-list()
graph_1_age_list_interruption<-list()
graph_1m_list_interruption<-list()
graph_1m_365_list_interruption<-list()
graph_1m_age_list_interruption<-list()
graph_2_list_interruption<-list()

if(!is.null(interruption_list)){
  
  #Load the first year
  first_file<-lapply(paste0(preg_tmp,interruption_list[[1]]), readRDS)
  first_file<-as.data.table(do.call(rbind,first_file))
  
  if(length(interruption_list)>1){
    for(year_preg_ind in 2:length(interruption_list)){
      year_to_be_removed<-names(interruption_list)[year_preg_ind-1]
      #Load next year
      file<-lapply(paste0(preg_tmp,interruption_list[[year_preg_ind]]), readRDS)
      file<-as.data.table(do.call(rbind,file))
      #Combine first and next file
      file<-rbind(first_file,file)
      rm(first_file)
      rows_start<-file[,.N]
      #Clean up data
      #remove the duplicate dates and save number removed
      file[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
      file<-file[!duplicated(pers_date)]
      file[,pers_date:=NULL]
      duplicated_dates_new<-rows_start - file[,.N]
      duplicated_preg_dates[event_definition=="interruption_pregnancy", duplicates:=duplicates + duplicated_dates_new]
      rm(duplicated_dates_new)
      
      if(file[!is.na(lag),.N]>0){
        #Step 1: Order the dataset by person_id, condition and date of event
        file<-file[order(person_id,condition,pregnancy_code_date)]
        #Step 2: Create date_2(by shifting the first date)
        file[,date_1:=shift(pregnancy_code_date)]
        file[,date_2:=pregnancy_code_date]
        #Step 3: Create rowid(which will give the number of rows for each person)
        file[,rowid:=rowid(person_id)]
        #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
        file[rowid==1,date_1:=NA]
        #Step 5: Create date_dif as difference between date 2 and date
        file[,date_dif:=date_2-date_1]
        #Step 6: Remove these rows 
        file<-file[date_dif>lag | is.na(date_dif)]
        #Step 7: Repeat until there are no more impossible dates in the dataset
        file[,date_dif:=date_2-date_1]
        while(file[date_dif<=lag,.N]>0){
          file<-file[date_dif>lag | is.na(date_dif)]
          file[,date_dif:=date_2-date_1]
        }
        
        duplicates_lag_new<-rows_start-file[,.N]
        duplicated_preg_dates[event_definition=="interruption_pregnancy", duplicates_time_lag:=duplicates_time_lag + duplicates_lag_new]
        file[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
        rm(rows_start,duplicates_lag_new)
        
      }
      #Calculate age at pregnancy record date
      file[year==year_to_be_removed,age_at_preg_record:=floor((pregnancy_code_date-birth_date)/365.25)]
      #create age_band
      for(age_band_ind in 1:age_bands_counts[,.N]){
        file[age_at_preg_record>=age_bands_counts[age_band_ind,min] & age_at_preg_record<=age_bands_counts[age_band_ind,max], age_band:=age_bands_counts[age_band_ind,age_band]]
      }
      
      #Counts for the first file(there are no more duplicates for the first file)
      no_records_year_age<-file[year==year_to_be_removed,.N, by=c("condition", "year","age_band")]
      setnames(no_records_year_age, "N", "no_records")
      no_women_age<-file[year==year_to_be_removed,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","year","age_band")]
      setnames(no_women_age, "person_id", "no_women")
      
      no_records_year<-file[year==year_to_be_removed,.N, by=c("condition", "year")]
      setnames(no_records_year, "N", "no_records")
      no_women<-file[year==year_to_be_removed,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","year")]
      setnames(no_women, "person_id", "no_women")
      
      no_records_year_m_age<-file[year==year_to_be_removed,.N, by=c("condition", "meaning","year", "age_band")]
      setnames(no_records_year_m_age, "N", "no_records")
      no_women_m_age<-file[year==year_to_be_removed,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year","age_band")]
      setnames(no_women_m_age, "person_id", "no_women")
      
      no_records_year_m<-file[year==year_to_be_removed,.N, by=c("condition", "meaning","year")]
      setnames(no_records_year_m, "N", "no_records")
      no_women_m<-file[year==year_to_be_removed,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year")]
      setnames(no_women_m, "person_id", "no_women")
      
      #identify women with more than 365 days of follow up
      file[,date_dif:=end_follow_up_preg-start_follow_up_preg]
      no_records_year_m_365<-file[year==year_to_be_removed & date_dif>=365,.N, by=c("condition", "meaning","year")]
      setnames(no_records_year_m_365, "N", "no_records")
      no_women_m_365<-file[year==year_to_be_removed & date_dif>=365,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year")]
      setnames(no_women_m_365, "person_id", "no_women")
      file[,date_dif:=NULL]
      
      #no_records_year_age,no_women_age
      graph_1_age<-merge.data.table(no_records_year_age,no_women_age, by=c("condition","year","age_band"))
      graph_1_age_list_interruption[[w]]<-graph_1_age
      rm(graph_1_age)
      
      #no_records_year,no_women
      graph_1<-merge.data.table(no_records_year,no_women, by=c("condition","year"))
      graph_1_list_interruption[[w]]<-graph_1
      rm(graph_1)
      
      #no_records_year_m_age,no_women_m_age
      graph_1m_age<-merge.data.table(no_records_year_m_age,no_women_m_age, by=c("condition","year","meaning","age_band"))
      graph_1m_age_list_interruption[[w]]<-graph_1m_age
      rm(graph_1m_age)
      
      #no_records_year_m,no_women_m
      graph_1m<-merge.data.table(no_records_year_m,no_women_m, by=c("condition","year","meaning"))
      graph_1m_list_interruption[[w]]<-graph_1m
      rm(graph_1m)
      
      #no_records_year_m_365,no_women_m_365
      graph_1m_365<-merge.data.table(no_records_year_m_365,no_women_m_365, by=c("condition","year","meaning"))
      graph_1m_365_list_interruption[[w]]<-graph_1m_365
      rm(graph_1m_365)
      
      
      #graph_2:no of records per women
      no_rec_women<-file[year==year_to_be_removed,.N, by=c("condition", "person_id","year")]
      res_rec_1<-no_rec_women[N==1, .N, by=c("condition","year")]
      setnames(res_rec_1,"N","1_pregnancy_record")
      res_rec_2<-no_rec_women[N==2, .N, by=c("condition","year")]
      setnames(res_rec_2,"N","2_pregnancy_records")
      res_rec_3<-no_rec_women[N==3, .N, by=c("condition","year")]
      setnames(res_rec_3,"N","3_pregnancy_records")
      res_rec_4<-no_rec_women[N>=4, .N, by=c("condition","year")]
      setnames(res_rec_4,"N","more_than_4_pregnancy_records")
      rm(no_rec_women)
      graph_2<-merge.data.table(res_rec_1,res_rec_2, by=c("condition","year"), all=T)
      graph_2<-merge.data.table(graph_2,res_rec_3, by=c("condition","year"), all=T)
      graph_2<-merge.data.table(graph_2,res_rec_4, by=c("condition","year"), all=T)
      graph_2_list_interruption[[w]]<-graph_2
      rm(graph_2)
      
      #add all ids that had a start pregnancy code to the study population from the first file
      study_population[person_id %in% file[year==year_to_be_removed,person_id], interruption_id:=1]
      file[,age_at_preg_record:=NULL][,age_band:=NULL]
      #save the first file in preg_pop
      saveRDS(file[year==year_to_be_removed], paste0(preg_pop, year_to_be_removed,"_interruption.rds"))
      #remove all records from the first year
      file<-file[!year %in% year_to_be_removed]
      first_file<-file
      rm(file)
      if(year_preg_ind==length(interruption_list)){
        saveRDS(first_file, paste0(preg_pop, first_file[!duplicated(year),year],"_interruption.rds"))
        break
      }
      w<-w+1
      year_preg_ind<-year_preg_ind+1
      
    }
    
  } else{
    rows_start<-first_file[,.N]
    #Clean up data
    #remove the duplicate dates and save number removed
    first_file[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
    first_file<-first_file[!duplicated(pers_date)]
    first_file[,pers_date:=NULL]
    duplicated_dates_new<-rows_start - first_file[,.N]
    duplicated_preg_dates[event_definition=="interruption_pregnancy", duplicates:=duplicates + duplicated_dates_new]
    rm(duplicated_dates_new)
    
    if(first_file[!is.na(lag),.N]>0){
      #Step 1: Order the dataset by person_id, condition and date of event
      first_file<-first_file[order(person_id,condition,pregnancy_code_date)]
      #Step 2: Create date_2(by shifting the first date)
      first_file[,date_1:=shift(pregnancy_code_date)]
      first_file[,date_2:=pregnancy_code_date]
      #Step 3: Create rowid(which will give the number of rows for each person)
      first_file[,rowid:=rowid(person_id)]
      #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
      first_file[rowid==1,date_1:=NA]
      #Step 5: Create date_dif as difference between date 2 and date
      first_file[,date_dif:=date_2-date_1]
      #Step 6: Remove these rows 
      first_file<-first_file[date_dif>lag | is.na(date_dif)]
      #Step 7: Repeat until there are no more impossible dates in the dataset
      first_file[,date_dif:=date_2-date_1]
      while(first_file[date_dif<=lag,.N]>0){
        first_file<-first_file[date_dif>lag | is.na(date_dif)]
        first_file[,date_dif:=date_2-date_1]
      }
      
      duplicates_lag_new<-rows_start-first_file[,.N]
      duplicated_preg_dates[event_definition=="interruption_pregnancy", duplicates_time_lag:=duplicates_time_lag + duplicates_lag_new]
      
      first_file[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
      rm(rows_start,duplicates_lag_new)
      
    }
    #Calculate age at pregnancy record date
    first_file[,age_at_preg_record:=floor((pregnancy_code_date-birth_date)/365.25)]
    #create age_band
    for(age_band_ind in 1:age_bands_counts[,.N]){
      first_file[age_at_preg_record>=age_bands_counts[age_band_ind,min] & age_at_preg_record<=age_bands_counts[age_band_ind,max], age_band:=age_bands_counts[age_band_ind,age_band]]
    }
    
    #Counts for the first first_file(there are no more duplicates for the first first_file)
    no_records_year_age<-first_file[,.N, by=c("condition", "year","age_band")]
    setnames(no_records_year_age, "N", "no_records")
    no_women_age<-first_file[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","year","age_band")]
    setnames(no_women_age, "person_id", "no_women")
    
    no_records_year<-first_file[,.N, by=c("condition", "year")]
    setnames(no_records_year, "N", "no_records")
    no_women<-first_file[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","year")]
    setnames(no_women, "person_id", "no_women")
    
    no_records_year_m_age<-first_file[,.N, by=c("condition", "meaning","year", "age_band")]
    setnames(no_records_year_m_age, "N", "no_records")
    no_women_m_age<-first_file[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year","age_band")]
    setnames(no_women_m_age, "person_id", "no_women")
    
    no_records_year_m<-first_file[,.N, by=c("condition", "meaning","year")]
    setnames(no_records_year_m, "N", "no_records")
    no_women_m<-file[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year")]
    setnames(no_women_m, "person_id", "no_women")
    
    #identify women with more than 365 days of follow up
    first_file[,date_dif:=end_follow_up_preg-start_follow_up_preg]
    no_records_year_m_365<-first_file[date_dif>=365,.N, by=c("condition", "meaning","year")]
    setnames(no_records_year_m_365, "N", "no_records")
    no_women_m_365<-first_file[date_dif>=365,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year")]
    setnames(no_women_m_365, "person_id", "no_women")
    first_file[,date_dif:=NULL]
    
    #no_records_year_age,no_women_age
    graph_1_age<-merge.data.table(no_records_year_age,no_women_age, by=c("condition","year","age_band"))
    graph_1_age_list_interruption[[w]]<-graph_1_age
    rm(graph_1_age)
    
    #no_records_year,no_women
    graph_1<-merge.data.table(no_records_year,no_women, by=c("condition","year"))
    graph_1_list_interruption[[w]]<-graph_1
    rm(graph_1)
    
    #no_records_year_m_age,no_women_m_age
    graph_1m_age<-merge.data.table(no_records_year_m_age,no_women_m_age, by=c("condition","year","meaning","age_band"))
    graph_1m_age_list_interruption[[w]]<-graph_1m_age
    rm(graph_1m_age)
    
    #no_records_year_m,no_women_m
    graph_1m<-merge.data.table(no_records_year_m,no_women_m, by=c("condition","year","meaning"))
    graph_1m_list_interruption[[w]]<-graph_1m
    rm(graph_1m)
    
    #no_records_year_m_365,no_women_m_365
    graph_1m_365<-merge.data.table(no_records_year_m_365,no_women_m_365, by=c("condition","year","meaning"))
    graph_1m_365_list_interruption[[w]]<-graph_1m_365
    rm(graph_1m_365)
    
    #graph_2:no of records per women
    no_rec_women<-first_file[,.N, by=c("condition", "person_id","year")]
    res_rec_1<-no_rec_women[N==1, .N, by=c("condition","year")]
    setnames(res_rec_1,"N","1_pregnancy_record")
    res_rec_2<-no_rec_women[N==2, .N, by=c("condition","year")]
    setnames(res_rec_2,"N","2_pregnancy_records")
    res_rec_3<-no_rec_women[N==3, .N, by=c("condition","year")]
    setnames(res_rec_3,"N","3_pregnancy_records")
    res_rec_4<-no_rec_women[N>=4, .N, by=c("condition","year")]
    setnames(res_rec_4,"N","more_than_4_pregnancy_records")
    rm(no_rec_women)
    graph_2<-merge.data.table(res_rec_1,res_rec_2, by=c("condition","year"), all=T)
    graph_2<-merge.data.table(graph_2,res_rec_3, by=c("condition","year"), all=T)
    graph_2<-merge.data.table(graph_2,res_rec_4, by=c("condition","year"), all=T)
    graph_2_list_interruption[[w]]<-graph_2
    rm(graph_2)
    #add all ids that had a start pregnancy code to the study population from the first file
    study_population[person_id %in% first_file[,person_id], interruption_id:=1]
    first_file[,age_at_preg_record:=NULL][,age_band:=NULL]
    #save the first file in preg_pop
    saveRDS(first_file, paste0(preg_pop, first_file[!duplicated(year),year],"_interruption.rds"))
  }
  
  rm(first_file)
  for(i in 1:length(interruption_files)){
    file.remove(paste0(preg_tmp,interruption_files[[i]]))
  }
}else {
  study_population[,interruption_id:=NA]
}

####ongoing_pregnancy###
w<-1
graph_1_list_ongoing<-list()
graph_1_age_list_ongoing<-list()
graph_1m_list_ongoing<-list()
graph_1m_365_list_ongoing<-list()
graph_1m_age_list_ongoing<-list()
graph_2_list_ongoing<-list()

if(!is.null(ongoing_list)){
  
  #Load the first year
  first_file<-lapply(paste0(preg_tmp,ongoing_list[[1]]), readRDS)
  first_file<-as.data.table(do.call(rbind,first_file))
  
  if(length(ongoing_list)>1){
    for(year_preg_ind in 2:length(ongoing_list)){
      year_to_be_removed<-names(ongoing_list)[year_preg_ind-1]
      #Load next year
      file<-lapply(paste0(preg_tmp,ongoing_list[[year_preg_ind]]), readRDS)
      file<-as.data.table(do.call(rbind,file))
      #Combine first and next file
      file<-rbind(first_file,file)
      rm(first_file)
      rows_start<-file[,.N]
      #Clean up data
      #remove the duplicate dates and save number removed
      file[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
      file<-file[!duplicated(pers_date)]
      file[,pers_date:=NULL]
      duplicated_dates_new<-rows_start - file[,.N]
      duplicated_preg_dates[event_definition=="ongoing_pregnancy", duplicates:=duplicates + duplicated_dates_new]
      rm(duplicated_dates_new)
      
      if(file[!is.na(lag),.N]>0){
        #Step 1: Order the dataset by person_id, condition and date of event
        file<-file[order(person_id,condition,pregnancy_code_date)]
        #Step 2: Create date_2(by shifting the first date)
        file[,date_1:=shift(pregnancy_code_date)]
        file[,date_2:=pregnancy_code_date]
        #Step 3: Create rowid(which will give the number of rows for each person)
        file[,rowid:=rowid(person_id)]
        #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
        file[rowid==1,date_1:=NA]
        #Step 5: Create date_dif as difference between date 2 and date
        file[,date_dif:=date_2-date_1]
        #Step 6: Remove these rows 
        file<-file[date_dif>lag | is.na(date_dif)]
        #Step 7: Repeat until there are no more impossible dates in the dataset
        file[,date_dif:=date_2-date_1]
        while(file[date_dif<=lag,.N]>0){
          file<-file[date_dif>lag | is.na(date_dif)]
          file[,date_dif:=date_2-date_1]
        }
        
        duplicates_lag_new<-rows_start-file[,.N]
        duplicated_preg_dates[event_definition=="ongoing_pregnancy", duplicates_time_lag:=duplicates_time_lag + duplicates_lag_new]
        file[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
        rm(rows_start,duplicates_lag_new)
        
      }
      #Calculate age at pregnancy record date
      file[year==year_to_be_removed,age_at_preg_record:=floor((pregnancy_code_date-birth_date)/365.25)]
      #create age_band
      for(age_band_ind in 1:age_bands_counts[,.N]){
        file[age_at_preg_record>=age_bands_counts[age_band_ind,min] & age_at_preg_record<=age_bands_counts[age_band_ind,max], age_band:=age_bands_counts[age_band_ind,age_band]]
      }
      
      #Counts for the first file(there are no more duplicates for the first file)
      no_records_year_age<-file[year==year_to_be_removed,.N, by=c("condition", "year","age_band")]
      setnames(no_records_year_age, "N", "no_records")
      no_women_age<-file[year==year_to_be_removed,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","year","age_band")]
      setnames(no_women_age, "person_id", "no_women")
      
      no_records_year<-file[year==year_to_be_removed,.N, by=c("condition", "year")]
      setnames(no_records_year, "N", "no_records")
      no_women<-file[year==year_to_be_removed,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","year")]
      setnames(no_women, "person_id", "no_women")
      
      no_records_year_m_age<-file[year==year_to_be_removed,.N, by=c("condition", "meaning","year", "age_band")]
      setnames(no_records_year_m_age, "N", "no_records")
      no_women_m_age<-file[year==year_to_be_removed,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year","age_band")]
      setnames(no_women_m_age, "person_id", "no_women")
      
      no_records_year_m<-file[year==year_to_be_removed,.N, by=c("condition", "meaning","year")]
      setnames(no_records_year_m, "N", "no_records")
      no_women_m<-file[year==year_to_be_removed,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year")]
      setnames(no_women_m, "person_id", "no_women")
      
      #identify women with more than 365 days of follow up
      file[,date_dif:=end_follow_up_preg-start_follow_up_preg]
      no_records_year_m_365<-file[year==year_to_be_removed & date_dif>=365,.N, by=c("condition", "meaning","year")]
      setnames(no_records_year_m_365, "N", "no_records")
      no_women_m_365<-file[year==year_to_be_removed & date_dif>=365,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year")]
      setnames(no_women_m_365, "person_id", "no_women")
      file[,date_dif:=NULL]
      
      #no_records_year_age,no_women_age
      graph_1_age<-merge.data.table(no_records_year_age,no_women_age, by=c("condition","year","age_band"))
      graph_1_age_list_ongoing[[w]]<-graph_1_age
      rm(graph_1_age)
      
      #no_records_year,no_women
      graph_1<-merge.data.table(no_records_year,no_women, by=c("condition","year"))
      graph_1_list_ongoing[[w]]<-graph_1
      rm(graph_1)
      
      #no_records_year_m_age,no_women_m_age
      graph_1m_age<-merge.data.table(no_records_year_m_age,no_women_m_age, by=c("condition","year","meaning","age_band"))
      graph_1m_age_list_ongoing[[w]]<-graph_1m_age
      rm(graph_1m_age)
      
      #no_records_year_m,no_women_m
      graph_1m<-merge.data.table(no_records_year_m,no_women_m, by=c("condition","year","meaning"))
      graph_1m_list_ongoing[[w]]<-graph_1m
      rm(graph_1m)
      
      #no_records_year_m_365,no_women_m_365
      graph_1m_365<-merge.data.table(no_records_year_m_365,no_women_m_365, by=c("condition","year","meaning"))
      graph_1m_365_list_ongoing[[w]]<-graph_1m_365
      rm(graph_1m_365)
      
      
      #graph_2:no of records per women
      no_rec_women<-file[year==year_to_be_removed,.N, by=c("condition", "person_id","year")]
      res_rec_1<-no_rec_women[N==1, .N, by=c("condition","year")]
      setnames(res_rec_1,"N","1_pregnancy_record")
      res_rec_2<-no_rec_women[N==2, .N, by=c("condition","year")]
      setnames(res_rec_2,"N","2_pregnancy_records")
      res_rec_3<-no_rec_women[N==3, .N, by=c("condition","year")]
      setnames(res_rec_3,"N","3_pregnancy_records")
      res_rec_4<-no_rec_women[N>=4, .N, by=c("condition","year")]
      setnames(res_rec_4,"N","more_than_4_pregnancy_records")
      rm(no_rec_women)
      graph_2<-merge.data.table(res_rec_1,res_rec_2, by=c("condition","year"), all=T)
      graph_2<-merge.data.table(graph_2,res_rec_3, by=c("condition","year"), all=T)
      graph_2<-merge.data.table(graph_2,res_rec_4, by=c("condition","year"), all=T)
      graph_2_list_ongoing[[w]]<-graph_2
      rm(graph_2)
      
      #add all ids that had a start pregnancy code to the study population from the first file
      study_population[person_id %in% file[year==year_to_be_removed,person_id], ongoing_id:=1]
      file[,age_at_preg_record:=NULL][,age_band:=NULL]
      #save the first file in preg_pop
      saveRDS(file[year==year_to_be_removed], paste0(preg_pop, year_to_be_removed,"_ongoing.rds"))
      #remove all records from the first year
      file<-file[!year %in% year_to_be_removed]
      first_file<-file
      rm(file)
      if(year_preg_ind==length(ongoing_list)){
        saveRDS(first_file, paste0(preg_pop, first_file[!duplicated(year),year],"_ongoing.rds"))
        break
      }
      w<-w+1
      year_preg_ind<-year_preg_ind+1
      
    }
    
  } else{
    rows_start<-first_file[,.N]
    #Clean up data
    #remove the duplicate dates and save number removed
    first_file[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
    first_file<-first_file[!duplicated(pers_date)]
    first_file[,pers_date:=NULL]
    duplicated_dates_new<-rows_start - first_file[,.N]
    duplicated_preg_dates[event_definition=="ongoing_pregnancy", duplicates:=duplicates + duplicated_dates_new]
    rm(duplicated_dates_new)
    
    if(first_file[!is.na(lag),.N]>0){
      #Step 1: Order the dataset by person_id, condition and date of event
      first_file<-first_file[order(person_id,condition,pregnancy_code_date)]
      #Step 2: Create date_2(by shifting the first date)
      first_file[,date_1:=shift(pregnancy_code_date)]
      first_file[,date_2:=pregnancy_code_date]
      #Step 3: Create rowid(which will give the number of rows for each person)
      first_file[,rowid:=rowid(person_id)]
      #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
      first_file[rowid==1,date_1:=NA]
      #Step 5: Create date_dif as difference between date 2 and date
      first_file[,date_dif:=date_2-date_1]
      #Step 6: Remove these rows 
      first_file<-first_file[date_dif>lag | is.na(date_dif)]
      #Step 7: Repeat until there are no more impossible dates in the dataset
      first_file[,date_dif:=date_2-date_1]
      while(first_file[date_dif<=lag,.N]>0){
        first_file<-first_file[date_dif>lag | is.na(date_dif)]
        first_file[,date_dif:=date_2-date_1]
      }
      
      duplicates_lag_new<-rows_start-first_file[,.N]
      duplicated_preg_dates[event_definition=="ongoing_pregnancy", duplicates_time_lag:=duplicates_time_lag + duplicates_lag_new]
      first_file[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
      rm(rows_start,duplicates_lag_new)
      
    }
    #Calculate age at pregnancy record date
    first_file[,age_at_preg_record:=floor((pregnancy_code_date-birth_date)/365.25)]
    #create age_band
    for(age_band_ind in 1:age_bands_counts[,.N]){
      first_file[age_at_preg_record>=age_bands_counts[age_band_ind,min] & age_at_preg_record<=age_bands_counts[age_band_ind,max], age_band:=age_bands_counts[age_band_ind,age_band]]
    }
    
    #Counts for the first first_file(there are no more duplicates for the first first_file)
    no_records_year_age<-first_file[,.N, by=c("condition", "year","age_band")]
    setnames(no_records_year_age, "N", "no_records")
    no_women_age<-first_file[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","year","age_band")]
    setnames(no_women_age, "person_id", "no_women")
    
    no_records_year<-first_file[,.N, by=c("condition", "year")]
    setnames(no_records_year, "N", "no_records")
    no_women<-first_file[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","year")]
    setnames(no_women, "person_id", "no_women")
    
    no_records_year_m_age<-first_file[,.N, by=c("condition", "meaning","year", "age_band")]
    setnames(no_records_year_m_age, "N", "no_records")
    no_women_m_age<-first_file[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year","age_band")]
    setnames(no_women_m_age, "person_id", "no_women")
    
    no_records_year_m<-first_file[,.N, by=c("condition", "meaning","year")]
    setnames(no_records_year_m, "N", "no_records")
    no_women_m<-file[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year")]
    setnames(no_women_m, "person_id", "no_women")
    
    #identify women with more than 365 days of follow up
    first_file[,date_dif:=end_follow_up_preg-start_follow_up_preg]
    no_records_year_m_365<-first_file[date_dif>=365,.N, by=c("condition", "meaning","year")]
    setnames(no_records_year_m_365, "N", "no_records")
    no_women_m_365<-first_file[date_dif>=365,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year")]
    setnames(no_women_m_365, "person_id", "no_women")
    first_file[,date_dif:=NULL]
    
    #no_records_year_age,no_women_age
    graph_1_age<-merge.data.table(no_records_year_age,no_women_age, by=c("condition","year","age_band"))
    graph_1_age_list_ongoing[[w]]<-graph_1_age
    rm(graph_1_age)
    
    #no_records_year,no_women
    graph_1<-merge.data.table(no_records_year,no_women, by=c("condition","year"))
    graph_1_list_ongoing[[w]]<-graph_1
    rm(graph_1)
    
    #no_records_year_m_age,no_women_m_age
    graph_1m_age<-merge.data.table(no_records_year_m_age,no_women_m_age, by=c("condition","year","meaning","age_band"))
    graph_1m_age_list_ongoing[[w]]<-graph_1m_age
    rm(graph_1m_age)
    
    #no_records_year_m,no_women_m
    graph_1m<-merge.data.table(no_records_year_m,no_women_m, by=c("condition","year","meaning"))
    graph_1m_list_ongoing[[w]]<-graph_1m
    rm(graph_1m)
    
    #no_records_year_m_365,no_women_m_365
    graph_1m_365<-merge.data.table(no_records_year_m_365,no_women_m_365, by=c("condition","year","meaning"))
    graph_1m_365_list_ongoing[[w]]<-graph_1m_365
    rm(graph_1m_365)
    
    #graph_2:no of records per women
    no_rec_women<-first_file[,.N, by=c("condition", "person_id","year")]
    res_rec_1<-no_rec_women[N==1, .N, by=c("condition","year")]
    setnames(res_rec_1,"N","1_pregnancy_record")
    res_rec_2<-no_rec_women[N==2, .N, by=c("condition","year")]
    setnames(res_rec_2,"N","2_pregnancy_records")
    res_rec_3<-no_rec_women[N==3, .N, by=c("condition","year")]
    setnames(res_rec_3,"N","3_pregnancy_records")
    res_rec_4<-no_rec_women[N>=4, .N, by=c("condition","year")]
    setnames(res_rec_4,"N","more_than_4_pregnancy_records")
    rm(no_rec_women)
    graph_2<-merge.data.table(res_rec_1,res_rec_2, by=c("condition","year"), all=T)
    graph_2<-merge.data.table(graph_2,res_rec_3, by=c("condition","year"), all=T)
    graph_2<-merge.data.table(graph_2,res_rec_4, by=c("condition","year"), all=T)
    graph_2_list_ongoing[[w]]<-graph_2
    rm(graph_2)
    #add all ids that had a start pregnancy code to the study population from the first file
    study_population[person_id %in% first_file[,person_id], ongoing_id:=1]
    first_file[,age_at_preg_record:=NULL][,age_band:=NULL]
    #save the first file in preg_pop
    saveRDS(first_file, paste0(preg_pop, first_file[!duplicated(year),year],"_ongoing.rds"))
  }
  
  rm(first_file)
  for(i in 1:length(ongoing_files)){
    file.remove(paste0(preg_tmp,ongoing_files[[i]]))
  }
}else {
  study_population[,ongoing_id:=NA]
}

###end_of_pregnancy###
w<-1
graph_1_list_end<-list()
graph_1_age_list_end<-list()
graph_1m_list_end<-list()
graph_1m_365_list_end<-list()
graph_1m_age_list_end<-list()
graph_2_list_end<-list()

if(!is.null(end_list)){
  
  #Load the first year
  first_file<-lapply(paste0(preg_tmp,end_list[[1]]), readRDS)
  first_file<-as.data.table(do.call(rbind,first_file))
  
  if(length(end_list)>1){
    for(year_preg_ind in 2:length(end_list)){
      year_to_be_removed<-names(end_list)[year_preg_ind-1]
      #Load next year
      file<-lapply(paste0(preg_tmp,end_list[[year_preg_ind]]), readRDS)
      file<-as.data.table(do.call(rbind,file))
      #Combine first and next file
      file<-rbind(first_file,file)
      rm(first_file)
      rows_start<-file[,.N]
      #Clean up data
      #remove the duplicate dates and save number removed
      file[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
      file<-file[!duplicated(pers_date)]
      file[,pers_date:=NULL]
      duplicated_dates_new<-rows_start - file[,.N]
      duplicated_preg_dates[event_definition=="end_of_pregnancy", duplicates:=duplicates + duplicated_dates_new]
      rm(duplicated_dates_new)
      
      if(file[!is.na(lag),.N]>0){
        #Step 1: Order the dataset by person_id, condition and date of event
        file<-file[order(person_id,condition,pregnancy_code_date)]
        #Step 2: Create date_2(by shifting the first date)
        file[,date_1:=shift(pregnancy_code_date)]
        file[,date_2:=pregnancy_code_date]
        #Step 3: Create rowid(which will give the number of rows for each person)
        file[,rowid:=rowid(person_id)]
        #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
        file[rowid==1,date_1:=NA]
        #Step 5: Create date_dif as difference between date 2 and date
        file[,date_dif:=date_2-date_1]
        #Step 6: Remove these rows 
        file<-file[date_dif>lag | is.na(date_dif)]
        #Step 7: Repeat until there are no more impossible dates in the dataset
        file[,date_dif:=date_2-date_1]
        while(file[date_dif<=lag,.N]>0){
          file<-file[date_dif>lag | is.na(date_dif)]
          file[,date_dif:=date_2-date_1]
        }
        
        duplicates_lag_new<-rows_start-file[,.N]
        duplicated_preg_dates[event_definition=="end_of_pregnancy", duplicates_time_lag:=duplicates_time_lag + duplicates_lag_new]
        file[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
        rm(rows_start,duplicates_lag_new)
        
      }
      #Calculate age at pregnancy record date
      file[year==year_to_be_removed,age_at_preg_record:=floor((pregnancy_code_date-birth_date)/365.25)]
      #create age_band
      for(age_band_ind in 1:age_bands_counts[,.N]){
        file[age_at_preg_record>=age_bands_counts[age_band_ind,min] & age_at_preg_record<=age_bands_counts[age_band_ind,max], age_band:=age_bands_counts[age_band_ind,age_band]]
      }
      
      #Counts for the first file(there are no more duplicates for the first file)
      no_records_year_age<-file[year==year_to_be_removed,.N, by=c("condition", "year","age_band")]
      setnames(no_records_year_age, "N", "no_records")
      no_women_age<-file[year==year_to_be_removed,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","year","age_band")]
      setnames(no_women_age, "person_id", "no_women")
      
      no_records_year<-file[year==year_to_be_removed,.N, by=c("condition", "year")]
      setnames(no_records_year, "N", "no_records")
      no_women<-file[year==year_to_be_removed,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","year")]
      setnames(no_women, "person_id", "no_women")
      
      no_records_year_m_age<-file[year==year_to_be_removed,.N, by=c("condition", "meaning","year", "age_band")]
      setnames(no_records_year_m_age, "N", "no_records")
      no_women_m_age<-file[year==year_to_be_removed,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year","age_band")]
      setnames(no_women_m_age, "person_id", "no_women")
      
      no_records_year_m<-file[year==year_to_be_removed,.N, by=c("condition", "meaning","year")]
      setnames(no_records_year_m, "N", "no_records")
      no_women_m<-file[year==year_to_be_removed,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year")]
      setnames(no_women_m, "person_id", "no_women")
      
      #identify women with more than 365 days of follow up
      file[,date_dif:=end_follow_up_preg-start_follow_up_preg]
      no_records_year_m_365<-file[year==year_to_be_removed & date_dif>=365,.N, by=c("condition", "meaning","year")]
      setnames(no_records_year_m_365, "N", "no_records")
      no_women_m_365<-file[year==year_to_be_removed & date_dif>=365,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year")]
      setnames(no_women_m_365, "person_id", "no_women")
      file[,date_dif:=NULL]
      
      #no_records_year_age,no_women_age
      graph_1_age<-merge.data.table(no_records_year_age,no_women_age, by=c("condition","year","age_band"))
      graph_1_age_list_end[[w]]<-graph_1_age
      rm(graph_1_age)
      
      #no_records_year,no_women
      graph_1<-merge.data.table(no_records_year,no_women, by=c("condition","year"))
      graph_1_list_end[[w]]<-graph_1
      rm(graph_1)
      
      #no_records_year_m_age,no_women_m_age
      graph_1m_age<-merge.data.table(no_records_year_m_age,no_women_m_age, by=c("condition","year","meaning","age_band"))
      graph_1m_age_list_end[[w]]<-graph_1m_age
      rm(graph_1m_age)
      
      #no_records_year_m,no_women_m
      graph_1m<-merge.data.table(no_records_year_m,no_women_m, by=c("condition","year","meaning"))
      graph_1m_list_end[[w]]<-graph_1m
      rm(graph_1m)
      
      #no_records_year_m_365,no_women_m_365
      graph_1m_365<-merge.data.table(no_records_year_m_365,no_women_m_365, by=c("condition","year","meaning"))
      graph_1m_365_list_end[[w]]<-graph_1m_365
      rm(graph_1m_365)
      
      
      #graph_2:no of records per women
      no_rec_women<-file[year==year_to_be_removed,.N, by=c("condition", "person_id","year")]
      res_rec_1<-no_rec_women[N==1, .N, by=c("condition","year")]
      setnames(res_rec_1,"N","1_pregnancy_record")
      res_rec_2<-no_rec_women[N==2, .N, by=c("condition","year")]
      setnames(res_rec_2,"N","2_pregnancy_records")
      res_rec_3<-no_rec_women[N==3, .N, by=c("condition","year")]
      setnames(res_rec_3,"N","3_pregnancy_records")
      res_rec_4<-no_rec_women[N>=4, .N, by=c("condition","year")]
      setnames(res_rec_4,"N","more_than_4_pregnancy_records")
      rm(no_rec_women)
      graph_2<-merge.data.table(res_rec_1,res_rec_2, by=c("condition","year"), all=T)
      graph_2<-merge.data.table(graph_2,res_rec_3, by=c("condition","year"), all=T)
      graph_2<-merge.data.table(graph_2,res_rec_4, by=c("condition","year"), all=T)
      graph_2_list_end[[w]]<-graph_2
      rm(graph_2)
      
      #add all ids that had a start pregnancy code to the study population from the first file
      study_population[person_id %in% file[year==year_to_be_removed,person_id], end_id:=1]
      file[,age_at_preg_record:=NULL][,age_band:=NULL]
      #save the first file in preg_pop
      saveRDS(file[year==year_to_be_removed], paste0(preg_pop, year_to_be_removed,"_end.rds"))
      #remove all records from the first year
      file<-file[!year %in% year_to_be_removed]
      first_file<-file
      rm(file)
      if(year_preg_ind==length(end_list)){
        saveRDS(first_file, paste0(preg_pop, first_file[!duplicated(year),year],"_end.rds"))
        break
      }
      w<-w+1
      year_preg_ind<-year_preg_ind+1
      
    }
    
  } else{
    rows_start<-first_file[,.N]
    #Clean up data
    #remove the duplicate dates and save number removed
    first_file[,pers_date:=paste0(person_id,"_", pregnancy_code_date)]
    first_file<-first_file[!duplicated(pers_date)]
    first_file[,pers_date:=NULL]
    duplicated_dates_new<-rows_start - first_file[,.N]
    duplicated_preg_dates[event_definition=="end_of_pregnancy", duplicates:=duplicates + duplicated_dates_new]
    rm(duplicated_dates_new)
    
    if(first_file[!is.na(lag),.N]>0){
      #Step 1: Order the dataset by person_id, condition and date of event
      first_file<-first_file[order(person_id,condition,pregnancy_code_date)]
      #Step 2: Create date_2(by shifting the first date)
      first_file[,date_1:=shift(pregnancy_code_date)]
      first_file[,date_2:=pregnancy_code_date]
      #Step 3: Create rowid(which will give the number of rows for each person)
      first_file[,rowid:=rowid(person_id)]
      #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
      first_file[rowid==1,date_1:=NA]
      #Step 5: Create date_dif as difference between date 2 and date
      first_file[,date_dif:=date_2-date_1]
      #Step 6: Remove these rows 
      first_file<-first_file[date_dif>lag | is.na(date_dif)]
      #Step 7: Repeat until there are no more impossible dates in the dataset
      first_file[,date_dif:=date_2-date_1]
      while(first_file[date_dif<=lag,.N]>0){
        first_file<-first_file[date_dif>lag | is.na(date_dif)]
        first_file[,date_dif:=date_2-date_1]
      }
      
      duplicates_lag_new<-rows_start-first_file[,.N]
      duplicated_preg_dates[event_definition=="end_of_pregnancy", duplicates_time_lag:=duplicates_time_lag + duplicates_lag_new]
      first_file[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
      rm(rows_start,duplicates_lag_new)
      
    }
    #Calculate age at pregnancy record date
    first_file[,age_at_preg_record:=floor((pregnancy_code_date-birth_date)/365.25)]
    #create age_band
    for(age_band_ind in 1:age_bands_counts[,.N]){
      first_file[age_at_preg_record>=age_bands_counts[age_band_ind,min] & age_at_preg_record<=age_bands_counts[age_band_ind,max], age_band:=age_bands_counts[age_band_ind,age_band]]
    }
    #Counts for the first first_file(there are no more duplicates for the first first_file)
    no_records_year_age<-first_file[,.N, by=c("condition", "year","age_band")]
    setnames(no_records_year_age, "N", "no_records")
    no_women_age<-first_file[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","year","age_band")]
    setnames(no_women_age, "person_id", "no_women")
    
    no_records_year<-first_file[,.N, by=c("condition", "year")]
    setnames(no_records_year, "N", "no_records")
    no_women<-first_file[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","year")]
    setnames(no_women, "person_id", "no_women")
    
    no_records_year_m_age<-first_file[,.N, by=c("condition", "meaning","year", "age_band")]
    setnames(no_records_year_m_age, "N", "no_records")
    no_women_m_age<-first_file[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year","age_band")]
    setnames(no_women_m_age, "person_id", "no_women")
    
    no_records_year_m<-first_file[,.N, by=c("condition", "meaning","year")]
    setnames(no_records_year_m, "N", "no_records")
    no_women_m<-file[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year")]
    setnames(no_women_m, "person_id", "no_women")
    
    #identify women with more than 365 days of follow up
    first_file[,date_dif:=end_follow_up_preg-start_follow_up_preg]
    no_records_year_m_365<-first_file[date_dif>=365,.N, by=c("condition", "meaning","year")]
    setnames(no_records_year_m_365, "N", "no_records")
    no_women_m_365<-first_file[date_dif>=365,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year")]
    setnames(no_women_m_365, "person_id", "no_women")
    first_file[,date_dif:=NULL]
    
    #no_records_year_age,no_women_age
    graph_1_age<-merge.data.table(no_records_year_age,no_women_age, by=c("condition","year","age_band"))
    graph_1_age_list_end[[w]]<-graph_1_age
    rm(graph_1_age)
    
    #no_records_year,no_women
    graph_1<-merge.data.table(no_records_year,no_women, by=c("condition","year"))
    graph_1_list_end[[w]]<-graph_1
    rm(graph_1)
    
    #no_records_year_m_age,no_women_m_age
    graph_1m_age<-merge.data.table(no_records_year_m_age,no_women_m_age, by=c("condition","year","meaning","age_band"))
    graph_1m_age_list_end[[w]]<-graph_1m_age
    rm(graph_1m_age)
    
    #no_records_year_m,no_women_m
    graph_1m<-merge.data.table(no_records_year_m,no_women_m, by=c("condition","year","meaning"))
    graph_1m_list_end[[w]]<-graph_1m
    rm(graph_1m)
    
    #no_records_year_m_365,no_women_m_365
    graph_1m_365<-merge.data.table(no_records_year_m_365,no_women_m_365, by=c("condition","year","meaning"))
    graph_1m_365_list_end[[w]]<-graph_1m_365
    rm(graph_1m_365)
    
    
    #graph_2:no of records per women
    no_rec_women<-first_file[,.N, by=c("condition", "person_id","year")]
    res_rec_1<-no_rec_women[N==1, .N, by=c("condition","year")]
    setnames(res_rec_1,"N","1_pregnancy_record")
    res_rec_2<-no_rec_women[N==2, .N, by=c("condition","year")]
    setnames(res_rec_2,"N","2_pregnancy_records")
    res_rec_3<-no_rec_women[N==3, .N, by=c("condition","year")]
    setnames(res_rec_3,"N","3_pregnancy_records")
    res_rec_4<-no_rec_women[N>=4, .N, by=c("condition","year")]
    setnames(res_rec_4,"N","more_than_4_pregnancy_records")
    rm(no_rec_women)
    graph_2<-merge.data.table(res_rec_1,res_rec_2, by=c("condition","year"), all=T)
    graph_2<-merge.data.table(graph_2,res_rec_3, by=c("condition","year"), all=T)
    graph_2<-merge.data.table(graph_2,res_rec_4, by=c("condition","year"), all=T)
    graph_2_list_end[[w]]<-graph_2
    rm(graph_2)
    #add all ids that had a start pregnancy code to the study population from the first file
    study_population[person_id %in% first_file[,person_id], end_id:=1]
    first_file[,age_at_preg_record:=NULL][,age_band:=NULL]
    #save the first file in preg_pop
    saveRDS(first_file, paste0(preg_pop, first_file[!duplicated(year),year],"_end.rds"))
  }
  
  rm(first_file)
  for(i in 1:length(end_files)){
    file.remove(paste0(preg_tmp,end_files[[i]]))
  }
}else {
  study_population[,end_id:=NA]
}

####Removed duplicates####

if(subpopulations_present=="Yes"){
  fwrite(duplicated_preg_dates, paste0(preg_dir, subpopulations_names[s], "/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_pregnancy_remove_duplicated_records.csv"), row.names = F)
} else {
  fwrite(duplicated_preg_dates, paste0(preg_dir,format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "pregnancy_remove_duplicated_records.csv"), row.names = F)
}

rm(duplicated_preg_dates)


####Records per women####
graph_2_list_start<-do.call(rbind,graph_2_list_start)
if(is.null(graph_2_list_start)){graph_2_list_start<-data.table(condition=NA,year=NA,"1_pregnancy_record"=NA,"2_pregnancy_records"=NA,"3_pregnancy_records"=NA,"more_than_4_pregnancy_records"=NA)}
graph_2_list_interruption<-do.call(rbind,graph_2_list_interruption)
if(is.null(graph_2_list_interruption)){graph_2_list_interruption<-data.table(condition=NA,year=NA,"1_pregnancy_record"=NA,"2_pregnancy_records"=NA,"3_pregnancy_records"=NA,"more_than_4_pregnancy_records"=NA)}
graph_2_list_ongoing<-do.call(rbind,graph_2_list_ongoing)
if(is.null(graph_2_list_ongoing)){graph_2_list_ongoing<-data.table(condition=NA,year=NA,"1_pregnancy_record"=NA,"2_pregnancy_records"=NA,"3_pregnancy_records"=NA,"more_than_4_pregnancy_records"=NA)}
graph_2_list_end<-do.call(rbind,graph_2_list_end)
if(is.null(graph_2_list_end)){graph_2_list_end<-data.table(condition=NA,year=NA,"1_pregnancy_record"=NA,"2_pregnancy_records"=NA,"3_pregnancy_records"=NA,"more_than_4_pregnancy_records"=NA)}

graph_2<-rbind(graph_2_list_start,graph_2_list_interruption,graph_2_list_ongoing,graph_2_list_end)
graph_2[is.na(`1_pregnancy_record`) & is.na(`2_pregnancy_records`) & is.na(`3_pregnancy_records`) & is.na(more_than_4_pregnancy_records), filter:=1]
graph_2<-graph_2[is.na(filter)]
graph_2[,filter:=NULL]
rm(graph_2_list_start,graph_2_list_interruption,graph_2_list_ongoing,graph_2_list_end)
graph_2[is.na(`1_pregnancy_record`),`1_pregnancy_record`:=0]
graph_2[is.na(`2_pregnancy_records`),`2_pregnancy_records`:=0]
graph_2[is.na(`3_pregnancy_records`),`3_pregnancy_records`:=0]
graph_2[is.na(`more_than_4_pregnancy_records`),`more_than_4_pregnancy_records`:=0]

setnames(graph_2,"condition","event_definition")
if(subpopulations_present=="Yes"){
  fwrite(graph_2, paste0(preg_dir, subpopulations_names[s], "/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_pregnancy_records_per_women.csv"), row.names = F)
} else {
  fwrite(graph_2, paste0(preg_dir,format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "pregnancy_records_per_women.csv"), row.names = F)
}

#Apply masking
suppressWarnings(graph_2[, `1_pregnancy_record`:= as.character(`1_pregnancy_record`)][as.numeric(`1_pregnancy_record`) > 0 & as.numeric(`1_pregnancy_record`) < 5, `1_pregnancy_record` := "<5"])
suppressWarnings(graph_2[, `2_pregnancy_records`:= as.character(`2_pregnancy_records`)][as.numeric(`2_pregnancy_records`) > 0 & as.numeric(`2_pregnancy_records`) < 5, `2_pregnancy_records` := "<5"])
suppressWarnings(graph_2[, `3_pregnancy_records`:= as.character(`3_pregnancy_records`)][as.numeric(`3_pregnancy_records`) > 0 & as.numeric(`3_pregnancy_records`) < 5, `3_pregnancy_records` := "<5"])
suppressWarnings(graph_2[, more_than_4_pregnancy_records:= as.character(more_than_4_pregnancy_records)][as.numeric(more_than_4_pregnancy_records) > 0 & as.numeric(more_than_4_pregnancy_records) < 5, more_than_4_pregnancy_records := "<5"])

if(subpopulations_present=="Yes"){
  fwrite(graph_2, paste0(preg_dir,subpopulations_names[s], "/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_pregnancy_records_per_women_masked.csv"), row.names = F)
} else {
  fwrite(graph_2, paste0(preg_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "pregnancy_records_per_women_masked.csv"), row.names = F)
}
rm(graph_2)


####Combine results:Counts by year, meaning, age band and stage of pregnancy####

graph_1m_age_list_start<-do.call(rbind,graph_1m_age_list_start)
if(is.null(graph_1m_age_list_start)){graph_1m_age_list_start<-data.table(condition=NA,meaning=NA,age_band=NA,year=NA,no_records=NA,no_women=NA)}
graph_1m_age_list_interruption<-do.call(rbind,graph_1m_age_list_interruption)
if(is.null(graph_1m_age_list_interruption)){graph_1m_age_list_interruption<-data.table(condition=NA,meaning=NA,age_band=NA,year=NA,no_records=NA,no_women=NA)}
graph_1m_age_list_ongoing<-do.call(rbind,graph_1m_age_list_ongoing)
if(is.null(graph_1m_age_list_ongoing)){graph_1m_age_list_ongoing<-data.table(condition=NA,meaning=NA,age_band=NA,year=NA,no_records=NA,no_women=NA)}
graph_1m_age_list_end<-do.call(rbind,graph_1m_age_list_end)
if(is.null(graph_1m_age_list_end)){graph_1m_age_list_end<-data.table(condition=NA,meaning=NA,age_band=NA,year=NA,no_records=NA,no_women=NA)}

graph_1m_age<-rbind(graph_1m_age_list_start,graph_1m_age_list_interruption,graph_1m_age_list_ongoing,graph_1m_age_list_end)
rm(graph_1m_age_list_start,graph_1m_age_list_interruption,graph_1m_age_list_ongoing,graph_1m_age_list_end)

setnames(graph_1m_age,"condition","event_definition")
graph_1m_age[,data_access_provider:=data_access_provider_name][,data_source:=data_source_name]
total<-graph_1m_age[,lapply(.SD,sum), by=c("event_definition","meaning"), .SDcols="no_records"]
setnames(total,"no_records","total_records")
graph_1m_age<-merge.data.table(graph_1m_age,total,all=T)
setcolorder(graph_1m_age, c("event_definition","meaning","age_band", "year", "no_records","no_women","total_records","data_access_provider"))
#no_records:no of pregnancy records by event definition, meaning, age_band and year
#no_women: no of unique persons(women) having a pregnancy code by event definition, meaning, age band and year
#total_records: no of pregnancy records by event definition and meaning

if(subpopulations_present=="Yes"){
  fwrite(graph_1m_age, paste0(preg_dir, subpopulations_names[s], "/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_pregnancy_counts_yam.csv"), row.names = F)
} else {
  fwrite(graph_1m_age, paste0(preg_dir,format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "pregnancy_counts_yam.csv"), row.names = F)
}

#Apply masking
suppressWarnings(graph_1m_age[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"])
suppressWarnings(graph_1m_age[, no_women:= as.character(no_women)][as.numeric(no_women) > 0 & as.numeric(no_women) < 5, no_women := "<5"])
suppressWarnings(graph_1m_age[, total_records:= as.character(total_records)][as.numeric(total_records) > 0 & as.numeric(total_records) < 5, total_records := "<5"])

if(subpopulations_present=="Yes"){
  fwrite(graph_1m_age, paste0(preg_dir,subpopulations_names[s], "/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_pregnancy_counts_yam_masked.csv"), row.names = F)
} else {
  fwrite(graph_1m_age, paste0(preg_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "pregnancy_counts_yam_masked.csv"), row.names = F)
}
rm(graph_1m_age)


####Combine results:Counts by year, meaning and stage of pregnancy####

graph_1m_list_start<-do.call(rbind,graph_1m_list_start)
if(is.null(graph_1m_list_start)){graph_1m_list_start<-data.table(condition=NA,meaning=NA,year=NA,no_records=NA,no_women=NA)}
graph_1m_list_interruption<-do.call(rbind,graph_1m_list_interruption)
if(is.null(graph_1m_list_interruption)){graph_1m_list_interruption<-data.table(condition=NA,meaning=NA,year=NA,no_records=NA,no_women=NA)}
graph_1m_list_ongoing<-do.call(rbind,graph_1m_list_ongoing)
if(is.null(graph_1m_list_ongoing)){graph_1m_list_ongoing<-data.table(condition=NA,meaning=NA,year=NA,no_records=NA,no_women=NA)}
graph_1m_list_end<-do.call(rbind,graph_1m_list_end)
if(is.null(graph_1m_list_end)){graph_1m_list_end<-data.table(condition=NA,meaning=NA,year=NA,no_records=NA,no_women=NA)}

graph_1m<-rbind(graph_1m_list_start,graph_1m_list_interruption,graph_1m_list_ongoing,graph_1m_list_end)
rm(graph_1m_list_start,graph_1m_list_interruption,graph_1m_list_ongoing,graph_1m_list_end)

setnames(graph_1m,"condition","event_definition")
graph_1m[,data_access_provider:=data_access_provider_name][,data_source:=data_source_name]
total<-graph_1m[,lapply(.SD,sum), by=c("event_definition","meaning"), .SDcols="no_records"]
setnames(total,"no_records","total_records")
graph_1m<-merge.data.table(graph_1m,total,all=T)
setcolorder(graph_1m, c("event_definition","meaning", "year", "no_records","no_women","total_records","data_access_provider"))
#no_records:no of pregnancy records by event definition, meaning and year
#no_women: no of unique persons(women) having a pregnancy code by event definition, meaning and year
#total_records: no of pregnancy records by event definition and meaning

if(subpopulations_present=="Yes"){
  fwrite(graph_1m, paste0(preg_dir, subpopulations_names[s], "/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_pregnancy_counts_ym.csv"), row.names = F)
} else {
  fwrite(graph_1m, paste0(preg_dir,format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "pregnancy_counts_ym.csv"), row.names = F)
}

#Apply masking
suppressWarnings(graph_1m[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"])
suppressWarnings(graph_1m[, no_women:= as.character(no_women)][as.numeric(no_women) > 0 & as.numeric(no_women) < 5, no_women := "<5"])
suppressWarnings(graph_1m[, total_records:= as.character(total_records)][as.numeric(total_records) > 0 & as.numeric(total_records) < 5, total_records := "<5"])

if(subpopulations_present=="Yes"){
  fwrite(graph_1m, paste0(preg_dir,subpopulations_names[s], "/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_pregnancy_counts_ym_masked.csv"), row.names = F)
} else {
  fwrite(graph_1m, paste0(preg_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "pregnancy_counts_ym_masked.csv"), row.names = F)
}
rm(graph_1m)

####Combine results:Counts by year, meaning and stage of pregnancy in women with at least 365 days of follow up####

graph_1m_365_list_start<-do.call(rbind,graph_1m_365_list_start)
if(is.null(graph_1m_365_list_start)){graph_1m_365_list_start<-data.table(condition=NA,meaning=NA,year=NA,no_records=NA,no_women=NA)}
graph_1m_365_list_interruption<-do.call(rbind,graph_1m_365_list_interruption)
if(is.null(graph_1m_365_list_interruption)){graph_1m_365_list_interruption<-data.table(condition=NA,meaning=NA,year=NA,no_records=NA,no_women=NA)}
graph_1m_365_list_ongoing<-do.call(rbind,graph_1m_365_list_ongoing)
if(is.null(graph_1m_365_list_ongoing)){graph_1m_365_list_ongoing<-data.table(condition=NA,meaning=NA,year=NA,no_records=NA,no_women=NA)}
graph_1m_365_list_end<-do.call(rbind,graph_1m_365_list_end)
if(is.null(graph_1m_365_list_end)){graph_1m_365_list_end<-data.table(condition=NA,meaning=NA,year=NA,no_records=NA,no_women=NA)}

graph_1m_365<-rbind(graph_1m_365_list_start,graph_1m_365_list_interruption,graph_1m_365_list_ongoing,graph_1m_365_list_end)
rm(graph_1m_365_list_start,graph_1m_365_list_interruption,graph_1m_365_list_ongoing,graph_1m_365_list_end)

setnames(graph_1m_365,"condition","event_definition")
graph_1m_365[,data_access_provider:=data_access_provider_name][,data_source:=data_source_name]
total<-graph_1m_365[,lapply(.SD,sum), by=c("event_definition","meaning"), .SDcols="no_records"]
setnames(total,"no_records","total_records")
graph_1m_365<-merge.data.table(graph_1m_365,total,all=T)
setcolorder(graph_1m_365, c("event_definition","meaning", "year", "no_records","no_women","total_records","data_access_provider"))
#no_records:no of pregnancy records by event definition, meaning and year
#no_women: no of unique persons(women) having a pregnancy code by event definition, meaning and year
#total_records: no of pregnancy records by event definition and meaning

if(subpopulations_present=="Yes"){
  fwrite(graph_1m_365, paste0(preg_dir, subpopulations_names[s], "/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_pregnancy_counts_ym_365.csv"), row.names = F)
} else {
  fwrite(graph_1m_365, paste0(preg_dir,format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "pregnancy_counts_ym_365.csv"), row.names = F)
}

#Apply masking
suppressWarnings(graph_1m_365[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"])
suppressWarnings(graph_1m_365[, no_women:= as.character(no_women)][as.numeric(no_women) > 0 & as.numeric(no_women) < 5, no_women := "<5"])
suppressWarnings(graph_1m_365[, total_records:= as.character(total_records)][as.numeric(total_records) > 0 & as.numeric(total_records) < 5, total_records := "<5"])

if(subpopulations_present=="Yes"){
  fwrite(graph_1m_365, paste0(preg_dir,subpopulations_names[s], "/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_pregnancy_counts_ym_365_masked.csv"), row.names = F)
} else {
  fwrite(graph_1m_365, paste0(preg_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "pregnancy_counts_ym_365_masked.csv"), row.names = F)
}
rm(graph_1m_365)

####Combine results:Counts by year, age band and stage of pregnancy####

graph_1_age_list_start<-do.call(rbind,graph_1_age_list_start)
if(is.null(graph_1_age_list_start)){graph_1_age_list_start<-data.table(condition=NA,age_band=NA,year=NA,no_records=NA,no_women=NA)}
graph_1_age_list_interruption<-do.call(rbind,graph_1_age_list_interruption)
if(is.null(graph_1_age_list_interruption)){graph_1_age_list_interruption<-data.table(condition=NA,age_band=NA,year=NA,no_records=NA,no_women=NA)}
graph_1_age_list_ongoing<-do.call(rbind,graph_1_age_list_ongoing)
if(is.null(graph_1_age_list_ongoing)){graph_1_age_list_ongoing<-data.table(condition=NA,age_band=NA,year=NA,no_records=NA,no_women=NA)}
graph_1_age_list_end<-do.call(rbind,graph_1_age_list_end)
if(is.null(graph_1_age_list_end)){graph_1_age_list_end<-data.table(condition=NA,age_band=NA,year=NA,no_records=NA,no_women=NA)}

graph_1_age<-rbind(graph_1_age_list_start,graph_1_age_list_interruption,graph_1_age_list_ongoing,graph_1_age_list_end)
rm(graph_1_age_list_start,graph_1_age_list_interruption,graph_1_age_list_ongoing,graph_1_age_list_end)


setnames(graph_1_age,"condition","event_definition")
graph_1_age[,data_access_provider:=data_access_provider_name][,data_source:=data_source_name]
total<-graph_1_age[,lapply(.SD,sum), by=c("event_definition"), .SDcols="no_records"]
setnames(total,"no_records","total_records")
graph_1_age<-merge.data.table(graph_1_age,total,all=T)
setcolorder(graph_1_age, c("event_definition","age_band", "year", "no_records","no_women","total_records","data_access_provider"))
#no_records:no of pregnancy records by event definition and year
#no_women: no of unique persons(women) having a pregnancy code by event definition and year
#total_records: no of pregnancy records by event definition

if(subpopulations_present=="Yes"){
  fwrite(graph_1_age, paste0(preg_dir, subpopulations_names[s], "/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_pregnancy_counts_ya.csv"), row.names = F)
} else {
  fwrite(graph_1_age, paste0(preg_dir,format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "pregnancy_counts_ya.csv"), row.names = F)
}

#Apply masking
suppressWarnings(graph_1_age[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"])
suppressWarnings(graph_1_age[, no_women:= as.character(no_women)][as.numeric(no_women) > 0 & as.numeric(no_women) < 5, no_women := "<5"])
suppressWarnings(graph_1_age[, total_records:= as.character(total_records)][as.numeric(total_records) > 0 & as.numeric(total_records) < 5, total_records := "<5"])

if(subpopulations_present=="Yes"){
  fwrite(graph_1_age, paste0(preg_dir,subpopulations_names[s], "/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_pregnancy_counts_ya_masked.csv"), row.names = F)
} else {
  fwrite(graph_1_age, paste0(preg_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "pregnancy_counts_ya_masked.csv"), row.names = F)
}

rm(graph_1_age)
####Combine results:Counts by year and stage of pregnancy####

graph_1_list_start<-do.call(rbind,graph_1_list_start)
if(is.null(graph_1_list_start)){graph_1_list_start<-data.table(condition=NA,year=NA,no_records=NA,no_women=NA)}
graph_1_list_interruption<-do.call(rbind,graph_1_list_interruption)
if(is.null(graph_1_list_interruption)){graph_1_list_interruption<-data.table(condition=NA,year=NA,no_records=NA,no_women=NA)}
graph_1_list_ongoing<-do.call(rbind,graph_1_list_ongoing)
if(is.null(graph_1_list_ongoing)){graph_1_list_ongoing<-data.table(condition=NA,year=NA,no_records=NA,no_women=NA)}
graph_1_list_end<-do.call(rbind,graph_1_list_end)
if(is.null(graph_1_list_end)){graph_1_list_end<-data.table(condition=NA,year=NA,no_records=NA,no_women=NA)}

graph_1<-rbind(graph_1_list_start,graph_1_list_interruption,graph_1_list_ongoing,graph_1_list_end)
rm(graph_1_list_start,graph_1_list_interruption,graph_1_list_ongoing,graph_1_list_end)


setnames(graph_1,"condition","event_definition")
graph_1[,data_access_provider:=data_access_provider_name][,data_source:=data_source_name]
total<-graph_1[,lapply(.SD,sum), by=c("event_definition"), .SDcols="no_records"]
setnames(total,"no_records","total_records")
graph_1<-merge.data.table(graph_1,total,all=T)
setcolorder(graph_1, c("event_definition", "year", "no_records","no_women","total_records","data_access_provider"))
#no_records:no of pregnancy records by event definition and year
#no_women: no of unique persons(women) having a pregnancy code by event definition and year
#total_records: no of pregnancy records by event definition

if(subpopulations_present=="Yes"){
  fwrite(graph_1, paste0(preg_dir, subpopulations_names[s], "/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_pregnancy_counts_y.csv"), row.names = F)
} else {
  fwrite(graph_1, paste0(preg_dir,format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "pregnancy_counts_y.csv"), row.names = F)
}

#Apply masking
suppressWarnings(graph_1[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"])
suppressWarnings(graph_1[, no_women:= as.character(no_women)][as.numeric(no_women) > 0 & as.numeric(no_women) < 5, no_women := "<5"])
suppressWarnings(graph_1[, total_records:= as.character(total_records)][as.numeric(total_records) > 0 & as.numeric(total_records) < 5, total_records := "<5"])

if(subpopulations_present=="Yes"){
  fwrite(graph_1, paste0(preg_dir,subpopulations_names[s], "/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_pregnancy_counts_y_masked.csv"), row.names = F)
} else {
  fwrite(graph_1, paste0(preg_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "pregnancy_counts_y_masked.csv"), row.names = F)
}
rm(graph_1)

####Rates of pregnancy records by year and age band####
#Load the person time
person_time_file<-list.files(preg_tmp, "py")
if(length(person_time_file)>0){
person_time_age<-lapply(paste0(preg_tmp,person_time_file), readRDS)
person_time_age<-as.data.table(do.call(rbind,person_time_age))
#Create person time by year only
person_time<-person_time_age[,lapply(.SD, sum), .SDcols="person_years", by=c("sex", "year")]
} else {
  person_time_age<-NULL  
  person_time<-NULL
}

#Load counts by year and age band
if(subpopulations_present=="Yes"){
  counts_ya_file<-fread(paste0(preg_dir,subpopulations_names[s], "/", list.files(preg_dir,"_pregnancy_counts_ya.csv")))
} else {
  counts_ya_file<-fread(paste0(preg_dir,list.files(preg_dir, "pregnancy_counts_ya.csv")))
}


#Combine counts and person time
counts_ya_file[,year:=as.character(year)]
person_time_age[,year:=as.character(year)]
rates_ya<-merge.data.table(counts_ya_file, person_time_age, by=c("year","age_band"),all=T, allow.cartesian = T)
rates_ya[,total_records:=NULL][,sex:=NULL]
rates_ya[is.na(no_records), no_records:=0]
rates_ya[is.na(no_women), no_women:=0]
rates_ya[is.na(data_access_provider), data_access_provider:=data_access_provider_name]
rates_ya[is.na(data_source), data_source:=data_source_name]
rates_ya[is.na(event_definition),event_definition:="N/A"]
rates_ya[,rate_per_100_py:=round((no_records/person_years)*100,2)]
setcolorder(rates_ya,c("event_definition","year","age_band","no_records","no_women","person_years","rate_per_100_py"))
rm(counts_ya_file,person_time_age)

####Rates of pregnancy records by year####
#Load counts by year and age band
if(subpopulations_present=="Yes"){
  counts_y_file<-fread(paste0(preg_dir,subpopulations_names[s], "/", list.files(preg_dir,"_pregnancy_counts_y.csv")))
} else {
  counts_y_file<-fread(paste0(preg_dir,list.files(preg_dir, "pregnancy_counts_y.csv")))
}

#Combine counts and person time
counts_y_file[,year:=as.character(year)]
person_time[,year:=as.character(year)]
rates_y<-merge.data.table(counts_y_file, person_time, by=c("year"),all=T, allow.cartesian = T)
rates_y[,total_records:=NULL][,sex:=NULL]
rates_y[is.na(no_records), no_records:=0]
rates_y[is.na(no_women), no_women:=0]
rates_y[is.na(data_access_provider), data_access_provider:=data_access_provider_name]
rates_y[is.na(data_source), data_source:=data_source_name]
rates_y[is.na(event_definition),event_definition:="N/A"]
rates_y[,rate_per_100_py:=round((no_records/person_years)*100,2)]
setcolorder(rates_y,c("event_definition","year","no_records","no_women","person_years","rate_per_100_py"))
rm(counts_y_file,person_time)

####Export rates####
if(subpopulations_present=="Yes"){
  fwrite(rates_ya, paste0(preg_dir, subpopulations_names[s], "/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_pregnancy_rates_ya.csv"), row.names = F)
  fwrite(rates_y, paste0(preg_dir, subpopulations_names[s], "/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_pregnancy_rates_y.csv"), row.names = F)
} else {
  fwrite(rates_ya, paste0(preg_dir,format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "pregnancy_rates_ya.csv"), row.names = F)
  fwrite(rates_y, paste0(preg_dir,format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "pregnancy_rates_y.csv"), row.names = F)
}

rates_y[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
rates_y[, no_women:= as.character(no_women)][as.numeric(no_women) > 0 & as.numeric(no_women) < 5, no_women := "<5"]
rates_y[, person_years:= as.character(person_years)][as.numeric(person_years) > 0 & as.numeric(person_years) < 5, person_years := "<5"]
rates_y[, rate_per_100_py:= as.character(rate_per_100_py)][no_records=="<5" | person_years=="<5", rate_per_100_py := "N/A"]

rates_ya[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
rates_ya[, no_women:= as.character(no_women)][as.numeric(no_women) > 0 & as.numeric(no_women) < 5, no_women := "<5"]
rates_ya[, person_years:= as.character(person_years)][as.numeric(person_years) > 0 & as.numeric(person_years) < 5, person_years := "<5"]
rates_ya[, rate_per_100_py:= as.character(rate_per_100_py)][no_records=="<5" | person_years=="<5", rate_per_100_py := "N/A"]

if(subpopulations_present=="Yes"){
  fwrite(rates_y, paste0(preg_dir,subpopulations_names[s], "/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_pregnancy_rates_y_masked.csv"), row.names = F)
  fwrite(rates_ya, paste0(preg_dir,subpopulations_names[s], "/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_pregnancy_rates_ya_masked.csv"), row.names = F)
} else {
  fwrite(rates_y, paste0(preg_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "pregnancy_rates_y_masked.csv"), row.names = F)
  fwrite(rates_ya, paste0(preg_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "pregnancy_rates_ya_masked.csv"), row.names = F)
}
rm(rates_y,rates_ya)

####Combination pregnancy records####
study_population[,event_definition:=NA]
study_population[,event_definition:=as.character(event_definition)]
study_population[!is.na(start_id) & !is.na(interruption_id) & !is.na(ongoing_id) & !is.na(end_id), event_definition:="S_I_O_E"]
study_population[!is.na(start_id) & !is.na(interruption_id) & !is.na(ongoing_id) & is.na(end_id), event_definition:="S_I_O"]
study_population[!is.na(start_id) & !is.na(interruption_id) & is.na(ongoing_id) & !is.na(end_id), event_definition:="S_I_E"]
study_population[!is.na(start_id) & is.na(interruption_id) & !is.na(ongoing_id) & !is.na(end_id), event_definition:="S_O_E"]
study_population[is.na(start_id) & !is.na(interruption_id) & !is.na(ongoing_id) & !is.na(end_id), event_definition:="I_O_E"]
study_population[!is.na(start_id) & !is.na(interruption_id) & is.na(ongoing_id) & is.na(end_id), event_definition:="S_I"]
study_population[!is.na(start_id) & is.na(interruption_id) & !is.na(ongoing_id) & is.na(end_id), event_definition:="S_O"]
study_population[!is.na(start_id) & is.na(interruption_id) & is.na(ongoing_id) & !is.na(end_id), event_definition:="S_E"]
study_population[!is.na(start_id) & is.na(interruption_id) & is.na(ongoing_id) & is.na(end_id), event_definition:="S"]
study_population[is.na(start_id) & !is.na(interruption_id) & !is.na(ongoing_id) & is.na(end_id), event_definition:="I_O"]
study_population[is.na(start_id) & !is.na(interruption_id) & is.na(ongoing_id) & !is.na(end_id), event_definition:="I_E"]
study_population[is.na(start_id) & !is.na(interruption_id) & is.na(ongoing_id) & is.na(end_id), event_definition:="I"]
study_population[is.na(start_id) & is.na(interruption_id) & !is.na(ongoing_id) & !is.na(end_id), event_definition:="O_E"]
study_population[is.na(start_id) & is.na(interruption_id) & !is.na(ongoing_id) & is.na(end_id), event_definition:="O"]
study_population[is.na(start_id) & is.na(interruption_id) & is.na(ongoing_id) & !is.na(end_id), event_definition:="E"]
study_population[is.na(start_id) & is.na(interruption_id) & is.na(ongoing_id) & is.na(end_id), event_definition:="no_pregnancy_record"]

comb_preg_rec<-study_population[,.N,by="event_definition"]
total<-study_population[,.N]
comb_preg_rec[,total_women:=total]
setnames(comb_preg_rec, "N", "no_women")
comb_preg_rec[,percentage_no_women:=round((no_women/total_women)*100,1)]

rm(study_population)

if(subpopulations_present=="Yes"){
  fwrite(comb_preg_rec, paste0(preg_dir, subpopulations_names[s], "/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s], "_pregnancy_combined_records.csv"), row.names = F)
} else {
  fwrite(comb_preg_rec, paste0(preg_dir,format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "pregnancy_combined_records.csv"), row.names = F)
}

comb_preg_rec[, no_women:= as.character(no_women)][as.numeric(no_women) > 0 & as.numeric(no_women) < 5, no_women := "<5"]
comb_preg_rec[, total_women:= as.character(total_women)][as.numeric(total_women) > 0 & as.numeric(total_women) < 5, total_women := "<5"]
comb_preg_rec[, percentage_no_women:= as.character(percentage_no_women)][no_women=="<5" | total_women=="<5", percentage_no_women := "N/A"]

if(subpopulations_present=="Yes"){
  fwrite(comb_preg_rec, paste0(preg_dir,subpopulations_names[s], "/","Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[s],"_pregnancy_combined_records_masked.csv"), row.names = F)
} else {
  fwrite(comb_preg_rec, paste0(preg_dir, "Masked/",format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "pregnancy_combined_records_masked.csv"), row.names = F)
}

rm(comb_preg_rec)


####To be removed(old version)####
# #Identify all people having a start_pregnancy code in the study population(will be used to create combination of counts)
# study_population[person_id %in% start_pregnancy[!duplicated(person_id),person_id],start_preg_code:=1]
# 
# if(start_pregnancy[,.N]>0){
# 
# #graph1
# 
# no_records_year<-start_pregnancy[,.N, by=c("condition", "year")]
# setnames(no_records_year, "N", "no_records")
# no_records_year_m<-start_pregnancy[,.N, by=c("condition", "meaning","year")]
# setnames(no_records_year_m, "N", "no_records")
# 
# no_records_tot<-start_pregnancy[,.N, by=c("condition")]
# setnames(no_records_tot, "N", "total_records")
# no_records_tot_m<-start_pregnancy[,.N, by=c("condition", "meaning")]
# setnames(no_records_tot_m, "N", "total_records")
# 
# no_women<-start_pregnancy[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","year")]
# setnames(no_women, "person_id", "no_women")
# no_women_m<-start_pregnancy[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year")]
# setnames(no_women_m, "person_id", "no_women")
# 
# tot_women<-start_pregnancy[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition")]
# setnames(tot_women, "person_id", "no_women_total")
# tot_women_m<-start_pregnancy[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning")]
# setnames(tot_women_m, "person_id", "no_women_total")
# 
# 
# graph1<-merge(no_records_year,no_women, by=c("condition","year"))
# rm(no_records_year,no_women)
# graph1<-merge(graph1,no_records_tot, by=c("condition"))
# rm(no_records_tot)
# graph1<-merge(graph1,tot_women, by=c("condition"))
# rm(tot_women)
# graph1[,meaning:="All"]
# 
# no_records_year_m<-merge(no_records_year_m,no_women_m, by=c("condition","meaning", "year"))
# rm(no_women_m)
# no_records_year_m<-merge(no_records_year_m,no_records_tot_m, by=c("condition","meaning"))
# rm(no_records_tot_m)
# no_records_year_m<-merge(no_records_year_m,tot_women_m, by=c("condition", "meaning"))
# rm(tot_women_m)
# graph1<-rbind(graph1,no_records_year_m)
# rm(no_records_year_m)
# 
# graph1[,percentage_records:=round((no_records/total_records)*100,2)]
# graph1[,mean_no_records:=round(no_records/no_women,2)]
# 
# # #get number of rows per unique combination
# # pregnancy_graph<-pregnancy_dt[,.N,by=c("person_id","year", "condition")]
# # setnames(pregnancy_graph, "N", "code_count")
# # #Create code_1 meaning each person has only one code per year(type of code not taken into account)
# # code_1_all<-pregnancy_graph[code_count==1 & !duplicated(person_id),.N, by="year"]
# # setnames(code_1_all,"N","code_1_per_year")
# # code_2_all<-pregnancy_graph[code_count==2 & !duplicated(person_id),.N, by="year"]
# # setnames(code_2_all,"N","codes_2_per_year")
# # code_3_all<-pregnancy_graph[code_count==3 & !duplicated(person_id),.N, by="year"]
# # setnames(code_3_all,"N","codes_3_per_year")
# # code_more_all<-pregnancy_graph[code_count>3 & !duplicated(person_id),.N, by="year"]
# # setnames(code_more_all,"N","codes_min_4_per_year")
# # total_women_by_year<-pregnancy_graph[!duplicated(person_id),.N, by="year"]
# # setnames(total_women_by_year,"N","total_women")
# 
# # graph1<-code_1_all
# # rm(code_1_all)
# # graph1<-merge(graph1, code_2_all, by="year", all=T)
# # rm(code_2_all)
# # graph1<-merge(graph1, code_3_all, by="year", all=T)
# # rm(code_3_all)
# # graph1<-merge(graph1, code_more_all, by="year", all=T)
# # rm(code_more_all)
# # graph1<-merge(graph1,total_women_by_year, by="year",all=T)
# # rm(total_women_by_year)
# # graph1[is.na(code_1_per_year),code_1_per_year:=0][is.na(codes_2_per_year),codes_2_per_year:=0][is.na(codes_3_per_year),codes_3_per_year:=0][is.na(codes_min_4_per_year),codes_min_4_per_year:=0]
# 
# saveRDS(graph1,paste0(preg_tmp, "graph1_start.rds"))
# rm(graph1)
# }
# rm(start_pregnancy)
# 
# #ongoing_pregnancy
# 
# if(length(pregnancy_files_events$ongoing_pregnancy)>0){
#   combined_ongoing_ev<-lapply(paste0(preg_ev_tmp,pregnancy_files_events$ongoing_pregnancy), readRDS) #combine all files for one pregnancy stage
#   combined_ongoing_ev<-do.call(rbind,combined_ongoing_ev)
#   combined_ongoing_ev[,lag:=time_lag[stage_of_pregnancy == "ongoing_pregnancy",time_lag]] #create lag variable based on condition
#   combined_ongoing_ev[,pregnancy_code:=NULL][,pregnancy_code_vocabulary:=NULL][,filter_preg:=NULL]
#   orig_no_row<-combined_ongoing_ev[,.N]
# } else {combined_ongoing_ev<-NULL
# orig_no_row<-0}
# if(length(pregnancy_files_mo$ongoing_pregnancy)>0){
#   combined_ongoing_mo<-lapply(paste0(preg_m_tmp,pregnancy_files_mo$ongoing_pregnancy), readRDS) #combine all files for one pregnancy stage
#   combined_ongoing_mo<-do.call(rbind,combined_ongoing_mo)
#   combined_ongoing_mo[,lag:=time_lag[stage_of_pregnancy == "ongoing_pregnancy",time_lag]] #create lag variable based on condition
#   combined_ongoing_mo[,pregnancy_code:=NULL][,pregnancy_code_vocabulary:=NULL][,filter_preg:=NULL]
#   orig_no_row<-orig_no_row+combined_ongoing_mo[,.N]
# } else {combined_ongoing_mo<-NULL
# orig_no_row<-orig_no_row+0}
# ongoing_pregnancy<-rbind(combined_ongoing_ev,combined_ongoing_mo)
# rm(combined_ongoing_ev,combined_ongoing_mo)
# #remove duplicate pregnancy dates between events and mo datasets
# if(!is.null(ongoing_pregnancy)){
#   ongoing_pregnancy[,dup:=paste(person_id, pregnancy_code_date, sep="_")]
#   ongoing_pregnancy<-ongoing_pregnancy[!duplicated(dup)]
#   ongoing_pregnancy[,dup:=NULL]
# }
# if(length(pregnancy_files_so$ongoing_pregnancy)>0){
#   combined_ongoing_so<-lapply(paste0(preg_s_tmp,pregnancy_files_so$ongoing_pregnancy), readRDS) #combine all files for one pregnancy stage
#   combined_ongoing_so<-do.call(rbind,combined_ongoing_so)
#   combined_ongoing_so[,lag:=time_lag[stage_of_pregnancy == "ongoing_pregnancy",time_lag]] #create lag variable based on condition
#   combined_ongoing_so[,pregnancy_code:=NULL][,pregnancy_code_vocabulary:=NULL][,filter_preg:=NULL]
#   orig_no_row<-orig_no_row+combined_ongoing_so[,.N]
# } else {combined_ongoing_so<-NULL
# orig_no_row<-orig_no_row+0}
# ongoing_pregnancy<-rbind(ongoing_pregnancy,combined_ongoing_so)
# rm(combined_ongoing_so)
# if(!is.null(ongoing_pregnancy)){
#   ongoing_pregnancy[,dup:=paste(person_id, pregnancy_code_date, sep="_")]
#   ongoing_pregnancy<-ongoing_pregnancy[!duplicated(dup)]
#   ongoing_pregnancy[,dup:=NULL]
# }
# if(!is.null(ongoing_pregnancy)){
#   dup<-orig_no_row-ongoing_pregnancy[,.N]
#   no_dup_lag<-ongoing_pregnancy[,.N]
# } else {dup<-0
# no_dup_lag<-0}
# 
# duplicated_preg_dates[pregnancy_stage=="ongoing_pregnancy", original_rows:=orig_no_row]
# duplicated_preg_dates[pregnancy_stage=="ongoing_pregnancy", duplicates:=dup]
# rm(orig_no_row,dup)
# 
# #Apply time lag check and remove not possible dates
# if(!is.null(ongoing_pregnancy)){
#   #Step 1: Order the dataset by person_id, condition and date of event
#   ongoing_pregnancy<-ongoing_pregnancy[order(person_id,condition,pregnancy_code_date)]
#   #Step 2: Create date_2(by shifting the first date)
#   ongoing_pregnancy[,date_1:=shift(pregnancy_code_date)]
#   ongoing_pregnancy[,date_2:=pregnancy_code_date]
#   #Step 3: Create rowid(whhcih will give the number of rows for each person)
#   ongoing_pregnancy[,rowid:=rowid(person_id)]
#   #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
#   ongoing_pregnancy[rowid==1,date_1:=NA]
#   #Step 5: Create date_dif as difference between date 2 and date
#   ongoing_pregnancy[,date_dif:=date_2-date_1]
#   #Step 6: Remove these rows 
#   ongoing_pregnancy<-ongoing_pregnancy[date_dif>lag | is.na(date_dif)]
#   #Step 7: Repeat until there are no more impossible dates in the dataset
#   ongoing_pregnancy[,date_dif:=date_2-date_1]
#   while(ongoing_pregnancy[date_dif<=lag,.N]>0){
#     ongoing_pregnancy<-ongoing_pregnancy[date_dif>lag | is.na(date_dif)]
#     ongoing_pregnancy[,ongoing_pregnancy:=date_2-date_1]
#   }
#   
#   #count number of duplicates
#   duplicated_preg_dates[pregnancy_stage=="ongoing_pregnancy",duplicates_time_lag:=no_dup_lag -ongoing_pregnancy[,.N]]
#   ongoing_pregnancy[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,lag:=NULL][,date_dif:=NULL]
#   
# } else {
#   duplicated_preg_dates[pregnancy_stage=="ongoing_pregnancy",duplicates_time_lag:=0]  
# }
# 
# 
# if(!is.null(ongoing_pregnancy)){
#   if (subpopulations_present=="Yes"){
#     saveRDS(ongoing_pregnancy, paste0(preg_pop, subpopulations_names[s], "/", subpopulations_names[s],"_","ongoing_pregnancy.rds"))
#   } else {
#     saveRDS(ongoing_pregnancy, paste0(preg_pop,"ongoing_pregnancy.rds"))
#   }
# }
# 
# #remove all ongoing files
# if(length(pregnancy_files_events$ongoing_pregnancy)>0){
#   for (i in 1:length(pregnancy_files_events$ongoing_pregnancy)){
#     file.remove(paste0(preg_ev_tmp, pregnancy_files_events$ongoing_pregnancy[[i]]))
#   }
# }
# if(length(pregnancy_files_mo$ongoing_pregnancy)>0){
#   for (i in 1:length(pregnancy_files_mo$ongoing_pregnancy)){
#     file.remove(paste0(preg_m_tmp, pregnancy_files_mo$ongoing_pregnancy[[i]]))
#   }
# }
# if(length(pregnancy_files_so$ongoing_pregnancy)>0){
#   for (i in 1:length(pregnancy_files_so$ongoing_pregnancy)){
#     file.remove(paste0(preg_s_tmp, pregnancy_files_so$ongoing_pregnancy[[i]]))
#   }
# }
# 
# #Identify all people having a ongoing_pregnancy code in the study population(will be used to create combination of counts)
# study_population[person_id %in% ongoing_pregnancy[!duplicated(person_id),person_id],ongoing_preg_code:=1]
# 
# if(ongoing_pregnancy[,.N]>0){
# 
# #graph1
# 
# no_records_year<-ongoing_pregnancy[,.N, by=c("condition", "year")]
# setnames(no_records_year, "N", "no_records")
# no_records_year_m<-ongoing_pregnancy[,.N, by=c("condition", "meaning","year")]
# setnames(no_records_year_m, "N", "no_records")
# 
# no_records_tot<-ongoing_pregnancy[,.N, by=c("condition")]
# setnames(no_records_tot, "N", "total_records")
# no_records_tot_m<-ongoing_pregnancy[,.N, by=c("condition", "meaning")]
# setnames(no_records_tot_m, "N", "total_records")
# 
# no_women<-ongoing_pregnancy[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","year")]
# setnames(no_women, "person_id", "no_women")
# no_women_m<-ongoing_pregnancy[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year")]
# setnames(no_women_m, "person_id", "no_women")
# 
# tot_women<-ongoing_pregnancy[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition")]
# setnames(tot_women, "person_id", "no_women_total")
# tot_women_m<-ongoing_pregnancy[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning")]
# setnames(tot_women_m, "person_id", "no_women_total")
# 
# 
# graph1<-merge(no_records_year,no_women, by=c("condition","year"))
# rm(no_records_year,no_women)
# graph1<-merge(graph1,no_records_tot, by=c("condition"))
# rm(no_records_tot)
# graph1<-merge(graph1,tot_women, by=c("condition"))
# rm(tot_women)
# graph1[,meaning:="All"]
# 
# no_records_year_m<-merge(no_records_year_m,no_women_m, by=c("condition","meaning", "year"))
# rm(no_women_m)
# no_records_year_m<-merge(no_records_year_m,no_records_tot_m, by=c("condition","meaning"))
# rm(no_records_tot_m)
# no_records_year_m<-merge(no_records_year_m,tot_women_m, by=c("condition", "meaning"))
# rm(tot_women_m)
# graph1<-rbind(graph1,no_records_year_m)
# rm(no_records_year_m)
# 
# graph1[,percentage_records:=round((no_records/total_records)*100,2)]
# graph1[,mean_no_records:=round(no_records/no_women,2)]
# 
# saveRDS(graph1,paste0(preg_tmp, "graph1_ongoing.rds"))
# rm(graph1)
# rm(ongoing_pregnancy)
# }
# 
# 
# 
# #interruption_pregnancy
# 
# if(length(pregnancy_files_events$interruption_pregnancy)>0){
#   combined_int_ev<-lapply(paste0(preg_ev_tmp,pregnancy_files_events$interruption_pregnancy), readRDS) #combine all files for one pregnancy stage
#   combined_int_ev<-do.call(rbind,combined_int_ev)
#   combined_int_ev[,lag:=time_lag[stage_of_pregnancy == "interruption_pregnancy",time_lag]] #create lag variable based on condition
#   combined_int_ev[,pregnancy_code:=NULL][,pregnancy_code_vocabulary:=NULL][,filter_preg:=NULL]
#   orig_no_row<-combined_int_ev[,.N]
# } else {combined_int_ev<-NULL
# orig_no_row<-0}
# if(length(pregnancy_files_mo$interruption_pregnancy)>0){
#   combined_int_mo<-lapply(paste0(preg_m_tmp,pregnancy_files_mo$interruption_pregnancy), readRDS) #combine all files for one pregnancy stage
#   combined_int_mo<-do.call(rbind,combined_int_mo)
#   combined_int_mo[,lag:=time_lag[stage_of_pregnancy == "interruption_pregnancy",time_lag]] #create lag variable based on condition
#   combined_int_mo[,pregnancy_code:=NULL][,pregnancy_code_vocabulary:=NULL][,filter_preg:=NULL]
#   orig_no_row<-orig_no_row+combined_int_mo[,.N]
# } else {combined_int_mo<-NULL
# orig_no_row<-orig_no_row+0}
# interruption_pregnancy<-rbind(combined_int_ev,combined_int_mo)
# rm(combined_int_ev,combined_int_mo)
# #remove duplicate pregnancy dates between events and mo datasets
# if(!is.null(interruption_pregnancy)){
#   interruption_pregnancy[,dup:=paste(person_id, pregnancy_code_date, sep="_")]
#   interruption_pregnancy<-interruption_pregnancy[!duplicated(dup)]
#   interruption_pregnancy[,dup:=NULL]
# }
# if(length(pregnancy_files_so$interruption_pregnancy)>0){
#   combined_int_so<-lapply(paste0(preg_s_tmp,pregnancy_files_so$interruption_pregnancy), readRDS) #combine all files for one pregnancy stage
#   combined_int_so<-do.call(rbind,combined_int_so)
#   combined_int_so[,lag:=time_lag[stage_of_pregnancy == "interruption_pregnancy",time_lag]] #create lag variable based on condition
#   combined_int_so[,pregnancy_code:=NULL][,pregnancy_code_vocabulary:=NULL][,filter_preg:=NULL]
#   orig_no_row<-orig_no_row+combined_int_so[,.N]
# } else {combined_int_so<-NULL
# orig_no_row<-orig_no_row+0}
# interruption_pregnancy<-rbind(interruption_pregnancy,combined_int_so)
# rm(combined_int_so)
# if(!is.null(interruption_pregnancy)){
#   interruption_pregnancy[,dup:=paste(person_id, pregnancy_code_date, sep="_")]
#   interruption_pregnancy<-interruption_pregnancy[!duplicated(dup)]
#   interruption_pregnancy[,dup:=NULL]
# }
# if(!is.null(interruption_pregnancy)){
#   dup<-orig_no_row-interruption_pregnancy[,.N]
#   no_dup_lag<-interruption_pregnancy[,.N]
# } else {dup<-0
# no_dup_lag<-0}
# 
# duplicated_preg_dates[pregnancy_stage=="interruption_pregnancy", original_rows:=orig_no_row]
# duplicated_preg_dates[pregnancy_stage=="interruption_pregnancy", duplicates:=dup]
# rm(orig_no_row,dup)
# 
# #Apply time lag check and remove not possible dates
# if(!is.null(interruption_pregnancy)){
#   #Step 1: Order the dataset by person_id, condition and date of event
#   interruption_pregnancy<-interruption_pregnancy[order(person_id,condition,pregnancy_code_date)]
#   #Step 2: Create date_2(by shifting the first date)
#   interruption_pregnancy[,date_1:=shift(pregnancy_code_date)]
#   interruption_pregnancy[,date_2:=pregnancy_code_date]
#   #Step 3: Create rowid(whhcih will give the number of rows for each person)
#   interruption_pregnancy[,rowid:=rowid(person_id)]
#   #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
#   interruption_pregnancy[rowid==1,date_1:=NA]
#   #Step 5: Create date_dif as difference between date 2 and date
#   interruption_pregnancy[,date_dif:=date_2-date_1]
#   #Step 6: Remove these rows 
#   interruption_pregnancy<-interruption_pregnancy[date_dif>lag | is.na(date_dif)]
#   #Step 7: Repeat until there are no more impossible dates in the dataset
#   interruption_pregnancy[,date_dif:=date_2-date_1]
#   while(interruption_pregnancy[date_dif<=lag,.N]>0){
#     interruption_pregnancy<-interruption_pregnancy[date_dif>lag | is.na(date_dif)]
#     interruption_pregnancy[,interruption_pregnancy:=date_2-date_1]
#   }
#   
#   #count number of duplicates
#   duplicated_preg_dates[pregnancy_stage=="interruption_pregnancy",duplicates_time_lag:=no_dup_lag -interruption_pregnancy[,.N]]
#   interruption_pregnancy[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,lag:=NULL][,date_dif:=NULL]
#   
# } else {
#   duplicated_preg_dates[pregnancy_stage=="interruption_pregnancy",duplicates_time_lag:=0]  
# }
# 
# 
# if(!is.null(interruption_pregnancy)){
#   if (subpopulations_present=="Yes"){
#     saveRDS(interruption_pregnancy, paste0(preg_pop, subpopulations_names[s], "/", subpopulations_names[s],"_","interruption_pregnancy.rds"))
#   } else {
#     saveRDS(interruption_pregnancy, paste0(preg_pop,"interruption_pregnancy.rds"))
#   }
# }
# 
# #remove all interruption files
# if(length(pregnancy_files_events$interruption_pregnancy)>0){
#   for (i in 1:length(pregnancy_files_events$interruption_pregnancy)){
#     file.remove(paste0(preg_ev_tmp, pregnancy_files_events$interruption_pregnancy[[i]]))
#   }
# }
# if(length(pregnancy_files_mo$interruption_pregnancy)>0){
#   for (i in 1:length(pregnancy_files_mo$interruption_pregnancy)){
#     file.remove(paste0(preg_m_tmp, pregnancy_files_mo$interruption_pregnancy[[i]]))
#   }
# }
# if(length(pregnancy_files_so$interruption_pregnancy)>0){
#   for (i in 1:length(pregnancy_files_so$interruption_pregnancy)){
#     file.remove(paste0(preg_s_tmp, pregnancy_files_so$interruption_pregnancy[[i]]))
#   }
# }
# 
# 
# #Identify all people having a interruption_pregnancy code in the study population(will be used to create combination of counts)
# study_population[person_id %in% interruption_pregnancy[!duplicated(person_id),person_id],interruption_preg_code:=1]
# 
# if(interruption_pregnancy[,.N]>0){
# 
#   #graph1
# 
#   no_records_year<-interruption_pregnancy[,.N, by=c("condition", "year")]
#   setnames(no_records_year, "N", "no_records")
#   no_records_year_m<-interruption_pregnancy[,.N, by=c("condition", "meaning","year")]
#   setnames(no_records_year_m, "N", "no_records")
#   
#   no_records_tot<-interruption_pregnancy[,.N, by=c("condition")]
#   setnames(no_records_tot, "N", "total_records")
#   no_records_tot_m<-interruption_pregnancy[,.N, by=c("condition", "meaning")]
#   setnames(no_records_tot_m, "N", "total_records")
#   
#   no_women<-interruption_pregnancy[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","year")]
#   setnames(no_women, "person_id", "no_women")
#   no_women_m<-interruption_pregnancy[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year")]
#   setnames(no_women_m, "person_id", "no_women")
#   
#   tot_women<-interruption_pregnancy[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition")]
#   setnames(tot_women, "person_id", "no_women_total")
#   tot_women_m<-interruption_pregnancy[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning")]
#   setnames(tot_women_m, "person_id", "no_women_total")
#   
#   
#   graph1<-merge(no_records_year,no_women, by=c("condition","year"))
#   rm(no_records_year,no_women)
#   graph1<-merge(graph1,no_records_tot, by=c("condition"))
#   rm(no_records_tot)
#   graph1<-merge(graph1,tot_women, by=c("condition"))
#   rm(tot_women)
#   graph1[,meaning:="All"]
#   
#   no_records_year_m<-merge(no_records_year_m,no_women_m, by=c("condition","meaning", "year"))
#   rm(no_women_m)
#   no_records_year_m<-merge(no_records_year_m,no_records_tot_m, by=c("condition","meaning"))
#   rm(no_records_tot_m)
#   no_records_year_m<-merge(no_records_year_m,tot_women_m, by=c("condition", "meaning"))
#   rm(tot_women_m)
#   graph1<-rbind(graph1,no_records_year_m)
#   rm(no_records_year_m)
#   
#   graph1[,percentage_records:=round((no_records/total_records)*100,2)]
#   graph1[,mean_no_records:=round(no_records/no_women,2)]
#   
#   saveRDS(graph1,paste0(preg_tmp, "graph1_interruption.rds"))
#   rm(graph1)
#   rm(interruption_pregnancy)
# }
# 
# 
# #end_of_pregnancy
# 
# if(length(pregnancy_files_events$end_of_pregnancy)>0){
#   combined_end_ev<-lapply(paste0(preg_ev_tmp,pregnancy_files_events$end_of_pregnancy), readRDS) #combine all files for one pregnancy stage
#   combined_end_ev<-do.call(rbind,combined_end_ev)
#   combined_end_ev[,lag:=time_lag[stage_of_pregnancy == "end_of_pregnancy",time_lag]] #create lag variable based on condition
#   combined_end_ev[,pregnancy_code:=NULL][,pregnancy_code_vocabulary:=NULL][,filter_preg:=NULL]
#   orig_no_row<-combined_end_ev[,.N]
# } else {combined_end_ev<-NULL
# orig_no_row<-0}
# if(length(pregnancy_files_mo$end_of_pregnancy)>0){
#   combined_end_mo<-lapply(paste0(preg_m_tmp,pregnancy_files_mo$end_of_pregnancy), readRDS) #combine all files for one pregnancy stage
#   combined_end_mo<-do.call(rbind,combined_end_mo)
#   combined_end_mo[,lag:=time_lag[stage_of_pregnancy == "end_of_pregnancy",time_lag]] #create lag variable based on condition
#   combined_end_mo[,pregnancy_code:=NULL][,pregnancy_code_vocabulary:=NULL][,filter_preg:=NULL]
#   orig_no_row<-orig_no_row+combined_end_mo[,.N]
# } else {combined_end_mo<-NULL
# orig_no_row<-orig_no_row+0}
# end_of_pregnancy<-rbind(combined_end_ev,combined_end_mo)
# rm(combined_end_ev,combined_end_mo)
# #remove duplicate pregnancy dates between events and mo datasets
# if(!is.null(end_of_pregnancy)){
#   end_of_pregnancy[,dup:=paste(person_id, pregnancy_code_date, sep="_")]
#   end_of_pregnancy<-end_of_pregnancy[!duplicated(dup)]
#   end_of_pregnancy[,dup:=NULL]
# }
# if(length(pregnancy_files_so$end_of_pregnancy)>0){
#   combined_end_so<-lapply(paste0(preg_s_tmp,pregnancy_files_so$end_of_pregnancy), readRDS) #combine all files for one pregnancy stage
#   combined_end_so<-do.call(rbind,combined_end_so)
#   combined_end_so[,lag:=time_lag[stage_of_pregnancy == "end_of_pregnancy",time_lag]] #create lag variable based on condition
#   combined_end_so[,pregnancy_code:=NULL][,pregnancy_code_vocabulary:=NULL][,filter_preg:=NULL]
#   orig_no_row<-orig_no_row+combined_end_so[,.N]
# } else {combined_end_so<-NULL
# orig_no_row<-orig_no_row+0}
# end_of_pregnancy<-rbind(end_of_pregnancy,combined_end_so)
# rm(combined_end_so)
# if(!is.null(end_of_pregnancy)){
#   end_of_pregnancy[,dup:=paste(person_id, pregnancy_code_date, sep="_")]
#   end_of_pregnancy<-end_of_pregnancy[!duplicated(dup)]
#   end_of_pregnancy[,dup:=NULL]
# }
# 
# if(length(pregnancy_files_si$end_of_pregnancy)>0){
#   combined_end_si<-lapply(paste0(preg_si_tmp,pregnancy_files_si$end_of_pregnancy), readRDS) #combine all files for one pregnancy stage
#   combined_end_si<-do.call(rbind,combined_end_si)
#   combined_end_si[,lag:=time_lag[stage_of_pregnancy == "end_of_pregnancy",time_lag]] #create lag variable based on condition
#   combined_end_si[,pregnancy_code:=NULL][,pregnancy_code_vocabulary:=NULL][,filter_preg:=NULL]
#   orig_no_row<-orig_no_row+combined_end_si[,.N]
# } else {combined_end_si<-NULL
# orig_no_row<-orig_no_row+0}
# end_of_pregnancy<-rbind(end_of_pregnancy,combined_end_si)
# rm(combined_end_si)
# if(!is.null(end_of_pregnancy)){
#   end_of_pregnancy[,dup:=paste(person_id, pregnancy_code_date, sep="_")]
#   end_of_pregnancy<-end_of_pregnancy[!duplicated(dup)]
#   end_of_pregnancy[,dup:=NULL]
# }
# 
# if(!is.null(end_of_pregnancy)){
#   dup<-orig_no_row-end_of_pregnancy[,.N]
#   no_dup_lag<-end_of_pregnancy[,.N]
# } else {dup<-0
# no_dup_lag<-0}
# 
# duplicated_preg_dates[pregnancy_stage=="end_of_pregnancy", original_rows:=orig_no_row]
# duplicated_preg_dates[pregnancy_stage=="end_of_pregnancy", duplicates:=dup]
# rm(orig_no_row,dup)
# 
# #Apply time lag check and remove not possible dates
# if(!is.null(end_of_pregnancy)){
#   #Step 1: Order the dataset by person_id, condition and date of event
#   end_of_pregnancy<-end_of_pregnancy[order(person_id,condition,pregnancy_code_date)]
#   #Step 2: Create date_2(by shifting the first date)
#   end_of_pregnancy[,date_1:=shift(pregnancy_code_date)]
#   end_of_pregnancy[,date_2:=pregnancy_code_date]
#   #Step 3: Create rowid(whhcih will give the number of rows for each person)
#   end_of_pregnancy[,rowid:=rowid(person_id)]
#   #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
#   end_of_pregnancy[rowid==1,date_1:=NA]
#   #Step 5: Create date_dif as difference between date 2 and date
#   end_of_pregnancy[,date_dif:=date_2-date_1]
#   #Step 6: Remove these rows 
#   end_of_pregnancy<-end_of_pregnancy[date_dif>lag | is.na(date_dif)]
#   #Step 7: Repeat until there are no more impossible dates in the dataset
#   end_of_pregnancy[,date_dif:=date_2-date_1]
#   while(end_of_pregnancy[date_dif<=lag,.N]>0){
#     end_of_pregnancy<-end_of_pregnancy[date_dif>lag | is.na(date_dif)]
#     end_of_pregnancy[,end_of_pregnancy:=date_2-date_1]
#   }
#   
#   #count number of duplicates
#   duplicated_preg_dates[pregnancy_stage=="end_of_pregnancy",duplicates_time_lag:=no_dup_lag -end_of_pregnancy[,.N]]
#   end_of_pregnancy[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,lag:=NULL][,date_dif:=NULL]
#   
# } else {
#   duplicated_preg_dates[pregnancy_stage=="end_of_pregnancy",duplicates_time_lag:=0]  
# }
# 
# if(!is.null(end_of_pregnancy)){
#   if (subpopulations_present=="Yes"){
#     saveRDS(end_of_pregnancy, paste0(preg_pop, subpopulations_names[s], "/", subpopulations_names[s],"_","end_of_pregnancy.rds"))
#   } else {
#     saveRDS(end_of_pregnancy, paste0(preg_pop,"end_of_pregnancy.rds"))
#   }
# }
# 
# #remove all start files
# if(length(pregnancy_files_events$end_of_pregnancy)>0){
#   for (i in 1:length(pregnancy_files_events$end_of_pregnancy)){
#     file.remove(paste0(preg_ev_tmp, pregnancy_files_events$end_of_pregnancy[[i]]))
#   }
# }
# if(length(pregnancy_files_mo$end_of_pregnancy)>0){
#   for (i in 1:length(pregnancy_files_mo$end_of_pregnancy)){
#     file.remove(paste0(preg_m_tmp, pregnancy_files_mo$end_of_pregnancy[[i]]))
#   }
# }
# if(length(pregnancy_files_so$end_of_pregnancy)>0){
#   for (i in 1:length(pregnancy_files_so$end_of_pregnancy)){
#     file.remove(paste0(preg_s_tmp, pregnancy_files_so$end_of_pregnancy[[i]]))
#   }
# }
# if(length(pregnancy_files_si$end_of_pregnancy)>0){
#   for (i in 1:length(pregnancy_files_si$end_of_pregnancy)){
#     file.remove(paste0(preg_si_tmp, pregnancy_files_si$end_of_pregnancy[[i]]))
#   }
# }
# 
# #Identify all people having a end_pregnancy code in the study population(will be used to create combination of counts)
# study_population[person_id %in% end_of_pregnancy[!duplicated(person_id),person_id], end_preg_code:=1]
# 
# if(end_of_pregnancy[,.N]>0){
# 
#   #graph1
# 
#   no_records_year<-end_of_pregnancy[,.N, by=c("condition", "year")]
#   setnames(no_records_year, "N", "no_records")
#   no_records_year_m<-end_of_pregnancy[,.N, by=c("condition", "meaning","year")]
#   setnames(no_records_year_m, "N", "no_records")
#   
#   no_records_tot<-end_of_pregnancy[,.N, by=c("condition")]
#   setnames(no_records_tot, "N", "total_records")
#   no_records_tot_m<-end_of_pregnancy[,.N, by=c("condition", "meaning")]
#   setnames(no_records_tot_m, "N", "total_records")
#   
#   no_women<-end_of_pregnancy[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","year")]
#   setnames(no_women, "person_id", "no_women")
#   no_women_m<-end_of_pregnancy[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning", "year")]
#   setnames(no_women_m, "person_id", "no_women")
#   
#   tot_women<-end_of_pregnancy[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition")]
#   setnames(tot_women, "person_id", "no_women_total")
#   tot_women_m<-end_of_pregnancy[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition","meaning")]
#   setnames(tot_women_m, "person_id", "no_women_total")
#   
#   
#   graph1<-merge(no_records_year,no_women, by=c("condition","year"))
#   rm(no_records_year,no_women)
#   graph1<-merge(graph1,no_records_tot, by=c("condition"))
#   rm(no_records_tot)
#   graph1<-merge(graph1,tot_women, by=c("condition"))
#   rm(tot_women)
#   graph1[,meaning:="All"]
#   
#   no_records_year_m<-merge(no_records_year_m,no_women_m, by=c("condition","meaning", "year"))
#   rm(no_women_m)
#   no_records_year_m<-merge(no_records_year_m,no_records_tot_m, by=c("condition","meaning"))
#   rm(no_records_tot_m)
#   no_records_year_m<-merge(no_records_year_m,tot_women_m, by=c("condition", "meaning"))
#   rm(tot_women_m)
#   graph1<-rbind(graph1,no_records_year_m)
#   rm(no_records_year_m)
#   
#   graph1[,percentage_records:=round((no_records/total_records)*100,2)]
#   graph1[,mean_no_records:=round(no_records/no_women,2)]
#   
#   saveRDS(graph1,paste0(preg_tmp, "graph1_interruption.rds"))
#   rm(graph1)
#   rm(end_of_pregnancy)
# }
# 
# #Create counts for women having more than 1 pregnancy code
# 
# #Having all 4 types of codes
# study_population[start_preg_code==1 & interruption_preg_code==1 & ongoing_preg_code==1 & end_preg_code==1, all_codes:=1]
# #s_i_o(start_interruption_ongoing)
# study_population[start_preg_code==1 & interruption_preg_code==1 & ongoing_preg_code==1 & is.na(end_preg_code), s_i_o:=1]
# #s_i_e(start_interruption_end)
# study_population[start_preg_code==1 & interruption_preg_code==1 & is.na(ongoing_preg_code) & end_preg_code==1, s_i_e:=1]
# #s_o_e(start_interruption_end)
# study_population[start_preg_code==1 & is.na(interruption_preg_code) & ongoing_preg_code==1 & end_preg_code==1, s_o_e:=1]
# #s_i(start_interruption)
# study_population[start_preg_code==1 & interruption_preg_code==1 & is.na(ongoing_preg_code) & is.na(end_preg_code), s_i:=1]
# #s_o(start_ongoing)
# study_population[start_preg_code==1 & is.na(interruption_preg_code) & ongoing_preg_code==1 & is.na(end_preg_code), s_o:=1]
# #s_e(start_end)
# study_population[start_preg_code==1 & is.na(interruption_preg_code) & is.na(ongoing_preg_code) & end_preg_code==1, s_e:=1]
# #s(start)
# study_population[start_preg_code==1 & is.na(interruption_preg_code) & is.na(ongoing_preg_code) & is.na(end_preg_code), s:=1]
# #i_o_e(interruption_ongoing_end)
# study_population[is.na(start_preg_code) & interruption_preg_code==1 & ongoing_preg_code==1 & end_preg_code==1, i_o_e:=1]
# #i_o(interruption_ongoing)
# study_population[is.na(start_preg_code) & interruption_preg_code==1 & ongoing_preg_code==1 & is.na(end_preg_code), i_o:=1]
# #i_e(interruption_end)
# study_population[is.na(start_preg_code) & interruption_preg_code==1 & is.na(ongoing_preg_code) & end_preg_code==1, i_e:=1]
# #i(interruption)
# study_population[is.na(start_preg_code) & interruption_preg_code==1 & is.na(ongoing_preg_code) & is.na(end_preg_code), i:=1]
# #o_e(ongoing_end)
# study_population[is.na(start_preg_code) & is.na(interruption_preg_code) & ongoing_preg_code==1 & end_preg_code==1, o_e:=1]
# #o(ongoing)
# study_population[is.na(start_preg_code) & is.na(interruption_preg_code) & ongoing_preg_code==1 & is.na(end_preg_code), o:=1]
# #e(end)
# study_population[is.na(start_preg_code) & is.na(interruption_preg_code) & is.na(ongoing_preg_code) & end_preg_code==1, e:=1]
# 
# 
# 
# #Lets load the start population 
# start_files<-list.files(preg_pop, "start_of_pregnancy")
# if(length(start_files)>0){
#   #Load database
# if(subpopulations_present=="Yes"){
# start_pregnancy<-readRDS(paste0(preg_pop, subpopulations_names[s], "/", subpopulations_names[s],"_","start_of_pregnancy.rds"))
# } else{
#   start_pregnancy<-readRDS(paste0(preg_pop, "start_of_pregnancy.rds"))
# }
# 
#   #Counts of women having multiple codes
#   #start only
#   if(study_population[s==1,.N]>0){
# #count number of subjects that have only a start preg
# start_only_year<-start_pregnancy[person_id %in% study_population[s==1, person_id],lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("year")]
# setnames(start_only_year, "person_id", "no_women")
# start_only_records<-start_pregnancy[person_id %in% study_population[s==1, person_id],.N, by=c("year")]
# setnames(start_only_records, "N", "no_records")
# start_only<-merge(start_only_records, start_only_year, by=c("year"))
# start_only[,start_of_pregnancy:=start_pregnancy[person_id %in% study_population[s==1, person_id],.N]]
# start_only[,interruption_pregnancy:=0][,ongoing_pregnancy:=0][,end_of_pregnancy:=0]
#   } else {
#   start_only<-NULL
# }
#   #Having all 4 types of codes
#   if(study_population[all_codes==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(start_pregnancy[person_id %in% study_population[all_codes==1, person_id]][,.N]>0){
#       saveRDS(start_pregnancy[person_id %in% study_population[all_codes==1, person_id]],paste0(preg_tmp, "start_all.rds"))
#     }
#   } 
#   #s_i_o(start_interruption_ongoing)
#   if(study_population[s_i_o==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(start_pregnancy[person_id %in% study_population[s_i_o==1, person_id]][,.N]>0){
#       saveRDS(start_pregnancy[person_id %in% study_population[s_i_o==1, person_id]],paste0(preg_tmp, "start_sio.rds"))
#     }
#   } 
#   #s_i_e(start_interruption_end)
#   if(study_population[s_i_e==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(start_pregnancy[person_id %in% study_population[s_i_e==1, person_id]][,.N]>0){
#       saveRDS(start_pregnancy[person_id %in% study_population[s_i_e==1, person_id]],paste0(preg_tmp, "start_sie.rds"))
#     }
#   }
#   
#   #s_o_e(start_ongoing_end)
#   if(study_population[s_o_e==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(start_pregnancy[person_id %in% study_population[s_o_e==1, person_id]][,.N]>0){
#       saveRDS(start_pregnancy[person_id %in% study_population[s_o_e==1, person_id]],paste0(preg_tmp, "start_soe.rds"))
#     }
#   }
#   
#   #s_i(start_interruption)
#   if(study_population[s_i==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(start_pregnancy[person_id %in% study_population[s_i==1, person_id]][,.N]>0){
#       saveRDS(start_pregnancy[person_id %in% study_population[s_i==1, person_id]],paste0(preg_tmp, "start_si.rds"))
#     }
#   }
#   
#   #s_o(start_ongoing)
#   if(study_population[s_o==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(start_pregnancy[person_id %in% study_population[s_o==1, person_id]][,.N]>0){
#       saveRDS(start_pregnancy[person_id %in% study_population[s_o==1, person_id]],paste0(preg_tmp, "start_so.rds"))
#     }
#   }
#   
#   #s_e(start_end)
#   if(study_population[s_e==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(start_pregnancy[person_id %in% study_population[s_e==1, person_id]][,.N]>0){
#       saveRDS(start_pregnancy[person_id %in% study_population[s_e==1, person_id]],paste0(preg_tmp, "start_se.rds"))
#     }
#   }
# 
#   #Rates:start of pregnancy
#
#   
#   start_pregnancy[,condition:="start_of_pregnancy"]
#   outcomes_list<-unique(start_pregnancy[,condition])
#   #apply count person time and save 
#     start_pregnancy<-CountPersonTime2(Dataset_events = start_pregnancy[,.(person_id, condition, pregnancy_code_date)],
#                                   Dataset = unique(start_pregnancy[,.(person_id, birth_date, start_follow_up_preg,end_follow_up_preg)]),
#                                   Person_id = "person_id",
#                                   Start_study_time =start_study_date2,
#                                   End_study_time =end_study_rates,
#                                   Start_date = "start_follow_up_preg",
#                                   End_date = "end_follow_up_preg",
#                                   Birth_date = "birth_date",
#                                   Increment = "year",
#                                   Unit_of_age = "year",
#                                   include_remaning_ages = TRUE,
#                                   Aggregate = T,
#                                   Outcomes_rec = outcomes_list,
#                                   Name_event = "condition",
#                                   Date_event = "pregnancy_code_date",
#                                   Rec_period = c(0),
#                                   Age_bands = c(12,29,39,49),
#                                   print = F, 
#                                   check_overlap = F)
#     
#     start_pregnancy[,Persontime:=NULL]
#     names(start_pregnancy)<-c("year","age_band","person_years","no_records")
#     start_pregnancy[,condition:="start_of_pregnancy"]
#     
#     
#     #Calculate person time for people that had a pregnancy but didnt havr a start code
#     #apply count person time and save 
#     if(study_population[pers_preg==1 & is.na(start_preg_code),.N]>0){
#       
#       ps_preg<-CountPersonTime2(Dataset = unique(study_population[pers_preg==1 & is.na(start_preg_code),.(person_id, birth_date, start_follow_up_preg,end_follow_up_preg)]),
#                                    Person_id = "person_id",
#                                    Start_study_time =start_study_date2,
#                                    End_study_time = end_study_rates,
#                                    Start_date = "start_follow_up_preg",
#                                    End_date = "end_follow_up_preg",
#                                    Birth_date = "birth_date",
#                                    Increment = "year",
#                                    Unit_of_age = "year",
#                                    include_remaning_ages = TRUE,
#                                    Strata = NULL,
#                                    Aggregate = T,
#                                    Age_bands = c(12,29,39,49),
#                                    print = F, 
#                                    check_overlap = F)
#       
#       names(ps_preg)<-c("year","age_band","person_years")
#       ps_preg[,no_records:=0][,condition:="start_of_pregnancy"]
#       
#       #Combine
#       start_pregnancy<-rbind(start_pregnancy,ps_preg)
#       start_pregnancy<-start_pregnancy[,lapply(.SD,sum), .SDcols=c("person_years","no_records"), by=c("condition","year","age_band")]
#       rm(ps_preg)
#       
#     }
#     
#     #Add person time from people that didnt have a pregnancy
#     no_preg_files<-list.files(preg_tmp, "no_id_py.rds")
#     if(length(no_preg_files)>0){
#       no_preg<-readRDS(paste0(preg_tmp,"no_id_py.rds"))
#       no_preg[,no_records:=0][,condition:="start_of_pregnancy"]
#     } else {
#       no_preg<-NULL
#     }
#     #Combine
#     start_pregnancy<-rbind(start_pregnancy,no_preg)
#     start_pregnancy<-start_pregnancy[,lapply(.SD,sum), .SDcols=c("person_years","no_records"), by=c("condition","year","age_band")]
#     rm(no_preg)
#     
#     #save results in preg_tmp
#     saveRDS(start_pregnancy, paste0(preg_tmp,"start_of_pregnancy_rates.rds"))
#     
#     #by year, age_band==All
#     start_pregnancy<-start_pregnancy[,lapply(.SD,sum), .SDcols=c("person_years","no_records"), by=c("condition","year")]
#     start_pregnancy[,age_band:="All"]
#   
#     #save results in preg_tmp
#     saveRDS(start_pregnancy, paste0(preg_tmp,"start_of_pregnancy_year_rates.rds"))
#     rm(start_pregnancy)
# } else {
#   start_only<-NULL
#   start_all<-NULL
# } 
# #Lets load the interruption population 
# interruption_files<-list.files(preg_pop, "interruption_pregnancy")
# if(length(interruption_files)>0){
#   #Load database
#   if(subpopulations_present=="Yes"){
#     interruption_pregnancy<-readRDS(paste0(preg_pop, subpopulations_names[i], "/", subpopulations_names[i],"_","interruption_pregnancy.rds"))
#   } else{
#     interruption_pregnancy<-readRDS(paste0(preg_pop, "interruption_pregnancy.rds"))
#   }
#   
#   #Counts of women having multiple codes
#   #Having all 4 types of codes
#   if(study_population[all_codes==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(interruption_pregnancy[person_id %in% study_population[all_codes==1, person_id]][,.N]>0){
#       saveRDS(interruption_pregnancy[person_id %in% study_population[all_codes==1, person_id]],paste0(preg_tmp, "interruption_all.rds"))
#     }
#   }
#   #interruption only
#   if(study_population[i==1,.N]>0){
#     #count number of subjects that have only a interruption preg
#     interruption_only_year<-interruption_pregnancy[person_id %in% study_population[i==1, person_id],lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("year")]
#     setnames(interruption_only_year, "person_id", "no_women")
#     interruption_only_records<-interruption_pregnancy[person_id %in% study_population[i==1, person_id],.N, by=c("year")]
#     setnames(interruption_only_records, "N", "no_records")
#     interruption_only<-merge(interruption_only_records, interruption_only_year, by=c("year"))
#     interruption_only[,interruption_pregnancy:=interruption_pregnancy[person_id %in% study_population[i==1, person_id],.N]]
#     interruption_only[,start_of_pregnancy:=0][,ongoing_pregnancy:=0][,end_of_pregnancy:=0]
#   } else {
#     interruption_only<-NULL
#   }
#   #i_o_e(interruption_ongoing_end)  
#   if(study_population[i_o_e==1,.N]>0){
#     #Save the subtable from the interruption_pregnancy to preg_tmp
#     if(interruption_pregnancy[person_id %in% study_population[i_o_e==1, person_id]][,.N]>0){
#       saveRDS(interruption_pregnancy[person_id %in% study_population[i_o_e==1, person_id]],paste0(preg_tmp, "interruption_ioe.rds"))
#     }
#   }
#   
#   #i_o(interruption_ongoing)
#   if(study_population[i_o==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(interruption_pregnancy[person_id %in% study_population[i_o==1, person_id]][,.N]>0){
#       saveRDS(interruption_pregnancy[person_id %in% study_population[i_o==1, person_id]],paste0(preg_tmp, "interruption_io.rds"))
#     }
#   }
#   
#   #i_e(interruption_end)
#   if(study_population[i_e==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(interruption_pregnancy[person_id %in% study_population[i_e==1, person_id]][,.N]>0){
#       saveRDS(interruption_pregnancy[person_id %in% study_population[i_e==1, person_id]],paste0(preg_tmp, "interruption_ie.rds"))
#     }
#   }
#   
#   #s_i_e(start_interruption_end)  
#   if(study_population[s_i_e==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(interruption_pregnancy[person_id %in% study_population[s_i_e==1, person_id]][,.N]>0){
#       saveRDS(interruption_pregnancy[person_id %in% study_population[s_i_e==1, person_id]],paste0(preg_tmp, "interruption_sie.rds"))
#     }
#   }
#   
#   #s_i(start_interruption)
#   if(study_population[s_i==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(interruption_pregnancy[person_id %in% study_population[s_i==1, person_id]][,.N]>0){
#       saveRDS(interruption_pregnancy[person_id %in% study_population[s_i==1, person_id]],paste0(preg_tmp, "interruption_si.rds"))
#     }
#   }
#   
#   
#   #Rates:interruption of pregnancy
#   
#   
#   interruption_pregnancy[,condition:="interruption_pregnancy"]
#   outcomes_list<-unique(interruption_pregnancy[,condition])
#   #apply count person time and save 
#   interruption_pregnancy<-CountPersonTime2(Dataset_events = interruption_pregnancy[,.(person_id, condition, pregnancy_code_date)],
#                                            Dataset = unique(interruption_pregnancy[,.(person_id, birth_date, start_follow_up_preg,end_follow_up_preg)]),
#                                            Person_id = "person_id",
#                                            Start_study_time =start_study_date2,
#                                            End_study_time =end_study_rates,
#                                            Start_date = "start_follow_up_preg",
#                                            End_date = "end_follow_up_preg",
#                                            Birth_date = "birth_date",
#                                            Increment = "year",
#                                            Unit_of_age = "year",
#                                            include_remaning_ages = TRUE,
#                                            Aggregate = T,
#                                            Outcomes_rec = outcomes_list,
#                                            Name_event = "condition",
#                                            Date_event = "pregnancy_code_date",
#                                            Rec_period = c(0),
#                                            Age_bands = c(12,29,39,49),
#                                            print = F, 
#                                            check_overlap = F)
#   
#   interruption_pregnancy[,Persontime:=NULL]
#   names(interruption_pregnancy)<-c("year","age_band","person_years","no_records")
#   interruption_pregnancy[,condition:="interruption_pregnancy"]
#   
#   
#   #Calculate person time for people that had a pregnancy but didnt havr a interruption code
#   #apply count person time and save 
#   if(study_population[pers_preg==1 & is.na(interruption_preg_code),.N]>0){
#     
#     ps_preg<-CountPersonTime2(Dataset = unique(study_population[pers_preg==1 & is.na(interruption_preg_code),.(person_id, birth_date, start_follow_up_preg,end_follow_up_preg)]),
#                               Person_id = "person_id",
#                               Start_study_time =start_study_date2,
#                               End_study_time = end_study_rates,
#                               Start_date = "start_follow_up_preg",
#                               End_date = "end_follow_up_preg",
#                               Birth_date = "birth_date",
#                               Increment = "year",
#                               Unit_of_age = "year",
#                               include_remaning_ages = TRUE,
#                               Strata = NULL,
#                               Aggregate = T,
#                               Age_bands = c(12,29,39,49),
#                               print = F, 
#                               check_overlap = F)
#     
#     names(ps_preg)<-c("year","age_band","person_years")
#     ps_preg[,no_records:=0][,condition:="interruption_pregnancy"]
#     
#     #Combine
#     interruption_pregnancy<-rbind(interruption_pregnancy,ps_preg)
#     interruption_pregnancy<-interruption_pregnancy[,lapply(.SD,sum), .SDcols=c("person_years","no_records"), by=c("condition","year","age_band")]
#     rm(ps_preg)
#     
#   }
#   
#   #Add person time from people that didnt have a pregnancy
#   no_preg_files<-list.files(preg_tmp, "no_id_py.rds")
#   if(length(no_preg_files)>0){
#     no_preg<-readRDS(paste0(preg_tmp,"no_id_py.rds"))
#     no_preg[,no_records:=0][,condition:="interruption_pregnancy"]
#   } else {
#     no_preg<-NULL
#   }
#   #Combine
#   interruption_pregnancy<-rbind(interruption_pregnancy,no_preg)
#   interruption_pregnancy<-interruption_pregnancy[,lapply(.SD,sum), .SDcols=c("person_years","no_records"), by=c("condition","year","age_band")]
#   rm(no_preg)
#   
#   #save results in preg_tmp
#   saveRDS(interruption_pregnancy, paste0(preg_tmp,"interruption_pregnancy_rates.rds"))
#   
#   #by year, age_band==All
#   interruption_pregnancy<-interruption_pregnancy[,lapply(.SD,sum), .SDcols=c("person_years","no_records"), by=c("condition","year")]
#   interruption_pregnancy[,age_band:="All"]
#   
#   #save results in preg_tmp
#   saveRDS(interruption_pregnancy, paste0(preg_tmp,"interruption_pregnancy_year_rates.rds"))
#   rm(interruption_pregnancy)
# }  else {
#   interruption_only<-NULL
# }
# #Lets load the ongoing population 
# ongoing_files<-list.files(preg_pop, "ongoing_pregnancy")
# if(length(ongoing_files)>0){
#   #Load database
#   if(subpopulations_present=="Yes"){
#     ongoing_pregnancy<-readRDS(paste0(preg_pop, subpopulations_names[i], "/", subpopulations_names[i],"_","ongoing_pregnancy.rds"))
#   } else{
#     ongoing_pregnancy<-readRDS(paste0(preg_pop, "ongoing_pregnancy.rds"))
#   }
#   
#   #Counts of women having multiple codes
#   #Having all 4 types of codes
#   if(study_population[all_codes==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(ongoing_pregnancy[person_id %in% study_population[all_codes==1, person_id]][,.N]>0){
#       saveRDS(ongoing_pregnancy[person_id %in% study_population[all_codes==1, person_id]],paste0(preg_tmp, "ongoing_all.rds"))
#     }
#   }
#   #ongoing only
#   if(study_population[o==1,.N]>0){
#     #count number of subjects that have only a ongoing preg
#     ongoing_only_year<-ongoing_pregnancy[person_id %in% study_population[o==1, person_id],lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("year")]
#     setnames(ongoing_only_year, "person_id", "no_women")
#     ongoing_only_records<-ongoing_pregnancy[person_id %in% study_population[o==1, person_id],.N, by=c("year")]
#     setnames(ongoing_only_records, "N", "no_records")
#     ongoing_only<-merge(ongoing_only_records, ongoing_only_year, by=c("year"))
#     ongoing_only[,ongoing_pregnancy:=ongoing_pregnancy[person_id %in% study_population[o==1, person_id],.N]]
#     ongoing_only[,start_of_pregnancy:=0][,interruption_pregnancy:=0][,end_of_pregnancy:=0]
#   } else {
#     ongoing_only<-NULL
#   }
# 
#   #o_e(ongoing_end)
#   if(study_population[o_e==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(ongoing_pregnancy[person_id %in% study_population[o_e==1, person_id]][,.N]>0){
#       saveRDS(ongoing_pregnancy[person_id %in% study_population[o_e==1, person_id]],paste0(preg_tmp, "ongoing_oe.rds"))
#     }
#   }
#   
#   #s_o_e(start_ongoing_end)
#   if(study_population[s_o_e==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(ongoing_pregnancy[person_id %in% study_population[s_o_e==1, person_id]][,.N]>0){
#       saveRDS(ongoing_pregnancy[person_id %in% study_population[s_o_e==1, person_id]],paste0(preg_tmp, "ongoing_soe.rds"))
#     }
#   }
#   
#   #s_o(start_ongoing)
#   if(study_population[s_o==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(ongoing_pregnancy[person_id %in% study_population[s_o==1, person_id]][,.N]>0){
#       saveRDS(ongoing_pregnancy[person_id %in% study_population[s_o==1, person_id]],paste0(preg_tmp, "ongoing_so.rds"))
#     }
#   }
#   
#   #i_o_e(interruption_ongoing_end)  
#   if(study_population[i_o_e==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(ongoing_pregnancy[person_id %in% study_population[i_o_e==1, person_id]][,.N]>0){
#       saveRDS(ongoing_pregnancy[person_id %in% study_population[i_o_e==1, person_id]],paste0(preg_tmp, "ongoing_ioe.rds"))
#     }
#   }
#   
#   #i_o(interruption_ongoing)
#   if(study_population[i_o==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(ongoing_pregnancy[person_id %in% study_population[i_o==1, person_id]][,.N]>0){
#       saveRDS(ongoing_pregnancy[person_id %in% study_population[i_o==1, person_id]],paste0(preg_tmp, "ongoing_io.rds"))
#     }
#   }
#   
# 
#   #Rates:ongoing of pregnancy
#   
#   
#   ongoing_pregnancy[,condition:="ongoing_pregnancy"]
#   outcomes_list<-unique(ongoing_pregnancy[,condition])
#   #apply count person time and save 
#   ongoing_pregnancy<-CountPersonTime2(Dataset_events = ongoing_pregnancy[,.(person_id, condition, pregnancy_code_date)],
#                                       Dataset = unique(ongoing_pregnancy[,.(person_id, birth_date, start_follow_up_preg,end_follow_up_preg)]),
#                                       Person_id = "person_id",
#                                       Start_study_time =start_study_date2,
#                                       End_study_time =end_study_rates,
#                                       Start_date = "start_follow_up_preg",
#                                       End_date = "end_follow_up_preg",
#                                       Birth_date = "birth_date",
#                                       Increment = "year",
#                                       Unit_of_age = "year",
#                                       include_remaning_ages = TRUE,
#                                       Aggregate = T,
#                                       Outcomes_rec = outcomes_list,
#                                       Name_event = "condition",
#                                       Date_event = "pregnancy_code_date",
#                                       Rec_period = c(0),
#                                       Age_bands = c(12,29,39,49),
#                                       print = F, 
#                                       check_overlap = F)
#   
#   ongoing_pregnancy[,Persontime:=NULL]
#   names(ongoing_pregnancy)<-c("year","age_band","person_years","no_records")
#   ongoing_pregnancy[,condition:="ongoing_pregnancy"]
#   
#   
#   #Calculate person time for people that had a pregnancy but didnt havr a ongoing code
#   #apply count person time and save 
#   if(study_population[pers_preg==1 & is.na(ongoing_preg_code),.N]>0){
#     
#     ps_preg<-CountPersonTime2(Dataset = unique(study_population[pers_preg==1 & is.na(ongoing_preg_code),.(person_id, birth_date, start_follow_up_preg,end_follow_up_preg)]),
#                               Person_id = "person_id",
#                               Start_study_time =start_study_date2,
#                               End_study_time = end_study_rates,
#                               Start_date = "start_follow_up_preg",
#                               End_date = "end_follow_up_preg",
#                               Birth_date = "birth_date",
#                               Increment = "year",
#                               Unit_of_age = "year",
#                               include_remaning_ages = TRUE,
#                               Strata = NULL,
#                               Aggregate = T,
#                               Age_bands = c(12,29,39,49),
#                               print = F, 
#                               check_overlap = F)
#     
#     names(ps_preg)<-c("year","age_band","person_years")
#     ps_preg[,no_records:=0][,condition:="ongoing_pregnancy"]
#     
#     #Combine
#     ongoing_pregnancy<-rbind(ongoing_pregnancy,ps_preg)
#     ongoing_pregnancy<-ongoing_pregnancy[,lapply(.SD,sum), .SDcols=c("person_years","no_records"), by=c("condition","year","age_band")]
#     rm(ps_preg)
#   }
#   
#   
#   #Add person time from people that didnt have a pregnancy
#   no_preg_files<-list.files(preg_tmp, "no_id_py.rds")
#   if(length(no_preg_files)>0){
#     no_preg<-readRDS(paste0(preg_tmp,"no_id_py.rds"))
#     no_preg[,no_records:=0][,condition:="ongoing_pregnancy"]
#   } else {
#     no_preg<-NULL
#   }
#   #Combine
#   ongoing_pregnancy<-rbind(ongoing_pregnancy,no_preg)
#   ongoing_pregnancy<-ongoing_pregnancy[,lapply(.SD,sum), .SDcols=c("person_years","no_records"), by=c("condition","year","age_band")]
#   rm(no_preg)
#   
#   #save results in preg_tmp
#   saveRDS(ongoing_pregnancy, paste0(preg_tmp,"ongoing_pregnancy_rates.rds"))
#   
#   #by year, age_band==All
#   ongoing_pregnancy<-ongoing_pregnancy[,lapply(.SD,sum), .SDcols=c("person_years","no_records"), by=c("condition","year")]
#   ongoing_pregnancy[,age_band:="All"]
#   
#   #save results in preg_tmp
#   saveRDS(ongoing_pregnancy, paste0(preg_tmp,"ongoing_pregnancy_year_rates.rds"))
#   rm(ongoing_pregnancy)
# } else {
#   ongoing_only<-NULL
# } 
# #Lets load the end population 
# end_files<-list.files(preg_pop, "end_of_pregnancy")
# if(length(end_files)>0){
#   #Load database
#   if(subpopulations_present=="Yes"){
#     end_pregnancy<-readRDS(paste0(preg_pop, subpopulations_names[i], "/", subpopulations_names[i],"_","end_of_pregnancy.rds"))
#   } else{
#     end_pregnancy<-readRDS(paste0(preg_pop, "end_of_pregnancy.rds"))
#   }
#   
#   #Counts of women having multiple codes
#   #Having all 4 types of codes
#   if(study_population[all_codes==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(end_pregnancy[person_id %in% study_population[all_codes==1, person_id]][,.N]>0){
#       saveRDS(end_pregnancy[person_id %in% study_population[all_codes==1, person_id]],paste0(preg_tmp, "end_all.rds"))
#     }
#   }
#   #end only
#   if(study_population[e==1,.N]>0){
#     #count number of subjects that have only a end preg
#     end_only_year<-end_pregnancy[person_id %in% study_population[e==1, person_id],lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("year")]
#     setnames(end_only_year, "person_id", "no_women")
#     end_only_records<-end_pregnancy[person_id %in% study_population[e==1, person_id],.N, by=c("year")]
#     setnames(end_only_records, "N", "no_records")
#     end_only<-merge(end_only_records, end_only_year, by=c("year"))
#     end_only[,end_of_pregnancy:=end_pregnancy[person_id %in% study_population[e==1, person_id],.N]]
#     end_only[,start_of_pregnancy:=0][,interruption_pregnancy:=0][,ongoing_pregnancy:=0]
#   } else {
#     end_only<-NULL
#   }
#   
#   #s_i_e(start_interruption_end)  
#   if(study_population[s_i_e==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(end_pregnancy[person_id %in% study_population[s_i_e==1, person_id]][,.N]>0){
#       saveRDS(end_pregnancy[person_id %in% study_population[s_i_e==1, person_id]],paste0(preg_tmp, "end_sie.rds"))
#     }
#   }
#   
#   #s_o_e(start_ongoing_end)
#   if(study_population[s_o_e==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(end_pregnancy[person_id %in% study_population[s_o_e==1, person_id]][,.N]>0){
#       saveRDS(end_pregnancy[person_id %in% study_population[s_o_e==1, person_id]],paste0(preg_tmp, "end_soe.rds"))
#     }
#   }
#   
#   #s_e(start_end)
#   if(study_population[s_e==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(end_pregnancy[person_id %in% study_population[s_e==1, person_id]][,.N]>0){
#       saveRDS(end_pregnancy[person_id %in% study_population[s_e==1, person_id]],paste0(preg_tmp, "end_se.rds"))
#     }
#   }
#   
#   #i_o_e(interruption_ongoing_end)  
#   if(study_population[i_o_e==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(end_pregnancy[person_id %in% study_population[i_o_e==1, person_id]][,.N]>0){
#       saveRDS(end_pregnancy[person_id %in% study_population[i_o_e==1, person_id]],paste0(preg_tmp, "end_ioe.rds"))
#     }
#   }
#   
#   #i_e(interruption_end)
#   if(study_population[i_e==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(end_pregnancy[person_id %in% study_population[i_e==1, person_id]][,.N]>0){
#       saveRDS(end_pregnancy[person_id %in% study_population[i_e==1, person_id]],paste0(preg_tmp, "end_ie.rds"))
#     }
#   }
#   
#   #o_e(ongoing_end)
#   if(study_population[o_e==1,.N]>0){
#     #Save the subtable from the start_pregnancy to preg_tmp
#     if(end_pregnancy[person_id %in% study_population[o_e==1, person_id]][,.N]>0){
#       saveRDS(end_pregnancy[person_id %in% study_population[o_e==1, person_id]],paste0(preg_tmp, "end_oe.rds"))
#     }
#   }
#   
# 
#   #Rates:end of pregnancy
#   
#   
#   end_pregnancy[,condition:="end_of_pregnancy"]
#   outcomes_list<-unique(end_pregnancy[,condition])
#   #apply count person time and save 
#   end_pregnancy<-CountPersonTime2(Dataset_events = end_pregnancy[,.(person_id, condition, pregnancy_code_date)],
#                                   Dataset = unique(end_pregnancy[,.(person_id, birth_date, start_follow_up_preg,end_follow_up_preg)]),
#                                   Person_id = "person_id",
#                                   Start_study_time =start_study_date2,
#                                   End_study_time =end_study_rates,
#                                   Start_date = "start_follow_up_preg",
#                                   End_date = "end_follow_up_preg",
#                                   Birth_date = "birth_date",
#                                   Increment = "year",
#                                   Unit_of_age = "year",
#                                   include_remaning_ages = TRUE,
#                                   Aggregate = T,
#                                   Outcomes_rec = outcomes_list,
#                                   Name_event = "condition",
#                                   Date_event = "pregnancy_code_date",
#                                   Rec_period = c(0),
#                                   Age_bands = c(12,29,39,49),
#                                   print = F, 
#                                   check_overlap = F)
#   
#   end_pregnancy[,Persontime:=NULL]
#   names(end_pregnancy)<-c("year","age_band","person_years","no_records")
#   end_pregnancy[,condition:="end_of_pregnancy"]
#   
#   
#   #Calculate person time for people that had a pregnancy but didnt havr a end code
#   #apply count person time and save 
#   if(study_population[pers_preg==1 & is.na(end_preg_code),.N]>0){
#     
#     ps_preg<-CountPersonTime2(Dataset = unique(study_population[pers_preg==1 & is.na(end_preg_code),.(person_id, birth_date, start_follow_up_preg,end_follow_up_preg)]),
#                               Person_id = "person_id",
#                               Start_study_time =start_study_date2,
#                               End_study_time = end_study_rates,
#                               Start_date = "start_follow_up_preg",
#                               End_date = "end_follow_up_preg",
#                               Birth_date = "birth_date",
#                               Increment = "year",
#                               Unit_of_age = "year",
#                               include_remaning_ages = TRUE,
#                               Strata = NULL,
#                               Aggregate = T,
#                               Age_bands = c(12,29,39,49),
#                               print = F, 
#                               check_overlap = F)
#     
#     names(ps_preg)<-c("year","age_band","person_years")
#     ps_preg[,no_records:=0][,condition:="end_of_pregnancy"]
#     
#     #Combine
#     end_pregnancy<-rbind(end_pregnancy,ps_preg)
#     end_pregnancy<-end_pregnancy[,lapply(.SD,sum), .SDcols=c("person_years","no_records"), by=c("condition","year","age_band")]
#     rm(ps_preg)
#   }
#   
#   
#   #Add person time from people that didnt have a pregnancy
#   no_preg_files<-list.files(preg_tmp, "no_id_py.rds")
#   if(length(no_preg_files)>0){
#     no_preg<-readRDS(paste0(preg_tmp,"no_id_py.rds"))
#     no_preg[,no_records:=0][,condition:="end_of_pregnancy"]
#   } else {
#     no_preg<-NULL
#   }
#   #Combine
#   end_pregnancy<-rbind(end_pregnancy,no_preg)
#   end_pregnancy<-end_pregnancy[,lapply(.SD,sum), .SDcols=c("person_years","no_records"), by=c("condition","year","age_band")]
#   rm(no_preg)
#   
#   #save results in preg_tmp
#   saveRDS(end_pregnancy, paste0(preg_tmp,"end_pregnancy_rates.rds"))
#   
#   #by year, age_band==All
#   end_pregnancy<-end_pregnancy[,lapply(.SD,sum), .SDcols=c("person_years","no_records"), by=c("condition","year")]
#   end_pregnancy[,age_band:="All"]
#   
#   #save results in preg_tmp
#   saveRDS(end_pregnancy, paste0(preg_tmp,"end_of_pregnancy_year_rates.rds"))
#   rm(end_pregnancy)
# } else {
#   end_only<-NULL
# }
# 
# 
# 
# #Combine only codes
# 
# #start_only, interruption_only,ongoing_only, end_only
# preg_only<-rbind(start_only,interruption_only,ongoing_only,end_only)
# rm(start_only,interruption_only, ongoing_only, end_only)
# 
# #Combine all codes
# 
# #start_all, interruption_all,ongoing_all, end_all
# all_codes<-list.files(preg_tmp, "all.rds")
# if(length(all_codes)>0){
#   all_fl<-lapply(paste0(preg_tmp, all_codes), readRDS)
#   all_fl<-do.call(rbind,all_fl)
#   start_all<-all_fl[condition=="start_of_pregnancy",.N]
#   interruption_all<-all_fl[condition=="interruption_pregnancy",.N]
#   ongoing_all<-all_fl[condition=="ongoing_pregnancy",.N]
#   end_all<-all_fl[condition=="end_of_pregnancy",.N]
#   
#   all_year<-all_fl[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("year")]
#   setnames(all_year, "person_id", "no_women")
#   all_records<-all_fl[,.N, by=c("year")]
#   setnames(all_records, "N", "no_records")
#   all<-merge(all_records, all_year, by=c("year"))
#   all[,start_of_pregnancy:=start_all][,interruption_pregnancy:=interruption_all][,ongoing_pregnancy:=ongoing_all][,end_of_pregnancy:=end_all]
#   rm(all_fl,start_all,interruption_all,ongoing_all,end_all)
# }
# 
# 
# preg_only<-rbind(start_only,interruption_only,ongoing_only,end_only)
# rm(start_only,interruption_only, ongoing_only, end_only)
# 
# 
# #Combine sio codes
# sio_codes<-list.files(preg_tmp, "_sio.rds")
# if(length(sio_codes)>0){
#   sio_fl<-lapply(paste0(preg_tmp, sio_codes), readRDS)
#   sio_fl<-do.call(rbind,sio_fl)
# }
# 
# #Combine sie codes
# sie_codes<-list.files(preg_tmp, "_sie.rds")
# if(length(sie_codes)>0){
#   sie_fl<-lapply(paste0(preg_tmp, sie_codes), readRDS)
#   sie_fl<-do.call(rbind,sie_fl)
# }
# 
# #Load interruption database
# if(subpopulations_present=="Yes"){
#   interruption_pregnancy<-readRDS(paste0(preg_pop, subpopulations_names[s], "/", subpopulations_names[s],"_","interruption_pregnancy.rds"))
# } else{
#   interruption_pregnancy<-readRDS(paste0(preg_pop, "interruption_pregnancy.rds"))
# }
# 
# 
# 
# 
# 
# #Load all pregnancy files
# if(subpopulations_present=="Yes"){
#   pregnancy_files<-list()
#   pregnancy_files$start_of_pregnancy<-list.files(preg_pop,subpopulations_names[s], "start")
#   pregnancy_files$ongoing_pregnancy<-list.files(preg_pop,subpopulations_names[s], "ongoing")
#   pregnancy_files$interruption_pregnancy<-list.files(preg_pop, subpopulations_names[s], "interruption")
#   pregnancy_files$end_of_pregnancy<-list.files(preg_pop, subpopulations_names[s], "end")
#   
# } else {
#   pregnancy_files<-list()
#   pregnancy_files$start_of_pregnancy<-list.files(preg_pop,"start")
#   pregnancy_files$ongoing_pregnancy<-list.files(preg_pop,"ongoing")
#   pregnancy_files$interruption_pregnancy<-list.files(preg_pop,"interruption")
#   pregnancy_files$end_of_pregnancy<-list.files(preg_pop,"end")
# }
# 
# #remove empty list elements
# pregnancy_files<-Filter(length,pregnancy_files)
# pregnancy_files<-Filter(function(k) length(k)>0, pregnancy_files)
# 
# for (preg_ind in 1:length(pregnancy_files)){
#   #Load file
#   if(subpopulations_present=="Yes"){
#     pregnancy_dt<-readRDS(paste0(preg_pop,subpopulations_names[s],  pregnancy_files[[preg_ind]]))
#   } else {
#     pregnancy_dt<-readRDS(paste0(preg_pop, pregnancy_files[[preg_ind]])) 
#   }
#   
#   
#   #graph1
#   
#   no_records_year<-pregnancy_dt[,.N, by=c("year", "condition")]
#   setnames(no_records_year, "N", "no_records")
#   no_records_tot<-pregnancy_dt[,.N, by=c("condition")]
#   setnames(no_records_tot, "N", "total_records")
#   no_women<-pregnancy_dt[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("year", "condition")]
#   setnames(no_women, "person_id", "no_women")
#   tot_women<-pregnancy_dt[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition")]
#   setnames(tot_women, "person_id", "no_women_total")
#   
#   graph1<-merge(no_records_year,no_women, by=c("condition","year"))
#   rm(no_records_year,no_women)
#   graph1<-merge(graph1,no_records_tot, by=c("condition"))
#   rm(no_records_tot)
#   graph1<-merge(graph1,tot_women, by=c("condition"))
#   rm(tot_women)
#   graph1[,percentage_records:=round((no_records/total_records)*100,2)]
#   graph1[,mean_no_records:=round(no_records/no_women,2)]
#   
#   # #get number of rows per unique combination
#   # pregnancy_graph<-pregnancy_dt[,.N,by=c("person_id","year", "condition")]
#   # setnames(pregnancy_graph, "N", "code_count")
#   # #Create code_1 meaning each person has only one code per year(type of code not taken into account)
#   # code_1_all<-pregnancy_graph[code_count==1 & !duplicated(person_id),.N, by="year"]
#   # setnames(code_1_all,"N","code_1_per_year")
#   # code_2_all<-pregnancy_graph[code_count==2 & !duplicated(person_id),.N, by="year"]
#   # setnames(code_2_all,"N","codes_2_per_year")
#   # code_3_all<-pregnancy_graph[code_count==3 & !duplicated(person_id),.N, by="year"]
#   # setnames(code_3_all,"N","codes_3_per_year")
#   # code_more_all<-pregnancy_graph[code_count>3 & !duplicated(person_id),.N, by="year"]
#   # setnames(code_more_all,"N","codes_min_4_per_year")
#   # total_women_by_year<-pregnancy_graph[!duplicated(person_id),.N, by="year"]
#   # setnames(total_women_by_year,"N","total_women")
# 
#   # graph1<-code_1_all
#   # rm(code_1_all)
#   # graph1<-merge(graph1, code_2_all, by="year", all=T)
#   # rm(code_2_all)
#   # graph1<-merge(graph1, code_3_all, by="year", all=T)
#   # rm(code_3_all)
#   # graph1<-merge(graph1, code_more_all, by="year", all=T)
#   # rm(code_more_all)
#   # graph1<-merge(graph1,total_women_by_year, by="year",all=T)
#   # rm(total_women_by_year)
#   # graph1[is.na(code_1_per_year),code_1_per_year:=0][is.na(codes_2_per_year),codes_2_per_year:=0][is.na(codes_3_per_year),codes_3_per_year:=0][is.na(codes_min_4_per_year),codes_min_4_per_year:=0]
#   
#   saveRDS(graph1,paste0(preg_tmp, "graph1_",  pregnancy_files[[preg_ind]]))
#   rm(graph1)
#   
#   #graph2
#   
#   #save all unique ids that had a pregnancy code by year
#   
#   pregnancy_dt[,rowid:=rowid(person_id), by="year"]
#   pregnancy_dt[,counts_id:=.N,by=c("person_id","year")]
#   saveRDS(pregnancy_dt[rowid==1],paste0(preg_tmp, "graph2_",  pregnancy_files[[preg_ind]]))
#   pregnancy_dt[,rowid:=NULL][,counts_id:=NULL]
#   
#   
#   # #Create code_1 meaning each person has only one code per year(type of code not taken into account)
#   # code_1_all<-pregnancy_graph[code_count==1 & !duplicated(person_id) ,.N, by=c("year","condition")]
#   # setnames(code_1_all,"N","code_1_per_year")
#   # code_2_all<-pregnancy_graph[code_count==2 & !duplicated(person_id),.N, by=c("year","condition")]
#   # setnames(code_2_all,"N","codes_2_per_year")
#   # code_3_all<-pregnancy_graph[code_count==3 & !duplicated(person_id),.N, by=c("year","condition")]
#   # setnames(code_3_all,"N","codes_3_per_year")
#   # code_more_all<-pregnancy_graph[code_count>3 & !duplicated(person_id),.N, by=c("year","condition")]
#   # setnames(code_more_all,"N","codes_min_4_per_year")
#   # total_women_by_year<-pregnancy_graph[!duplicated(person_id),.N, by=c("year", "condition")]
#   # setnames(total_women_by_year,"N","total_women")
#   # rm(pregnancy_graph)
#   # 
#   # graph2<-code_1_all
#   # rm(code_1_all)
#   # graph2<-merge(graph2, code_2_all, by=c("year","condition"), all=T)
#   # rm(code_2_all)
#   # graph2<-merge(graph2, code_3_all, by=c("year","condition"), all=T)
#   # rm(code_3_all)
#   # graph2<-merge(graph2, code_more_all, by=c("year","condition"), all=T)
#   # rm(code_more_all)
#   # graph2<-merge(graph2, total_women_by_year, by=c("year","condition"), all=T)
#   # graph2[is.na(code_1_per_year),code_1_per_year:=0][is.na(codes_2_per_year),codes_2_per_year:=0][is.na(codes_3_per_year),codes_3_per_year:=0][is.na(codes_min_4_per_year),codes_min_4_per_year:=0]
#   # setnames(graph2, "condition", "stage_of_pregnancy")
#   
#   
#   #tab19
#   
#   #counts in women that have a follow up of 365 days after their first pregnancy code
#   
#   #sort dataset by person id and date of pregnancy code to select the first one
#   pregnancy_dt<-pregnancy_dt[order(person_id, pregnancy_code_date),]
#   #create variable rowid by person id to identify all records for each person
#   pregnancy_dt[,rowid:=rowid(person_id)]
#   #do the date difference between the first code and end of follow up for each person
#   pregnancy_dt[,end_follow_up_preg:=as.IDate(end_follow_up_preg, "%Y%m%d")][,pregnancy_code_date:=as.IDate(pregnancy_code_date, "%Y%m%d")]
#   pregnancy_dt[rowid==1, date_dif:=end_follow_up_preg-pregnancy_code_date]
#   #if the date diff is more than 365 days create filter==1
#   pregnancy_dt[date_dif>365, filter:=1]
#   
#   #create table
#   no_records_365<-pregnancy_dt[filter==1,.N, by=c("year", "condition")]
#   setnames(no_records_365, "N", "no_records_365")
#   no_records<-pregnancy_dt[,.N, by=c("year", "condition")]
#   setnames(no_records, "N", "no_records")
#   no_records<-merge(no_records_365,no_records, by=c("condition","year"), all=T)
#   rm(no_records_365)
#   
#   no_women_365<-pregnancy_dt[filter==1,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("year", "condition")]
#   setnames(no_women_365, "person_id", "no_women_365")
#   no_women<-pregnancy_dt[,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("year", "condition")]
#   setnames(no_women, "person_id", "no_women")
#   no_women<-merge(no_women_365,no_women, by=c("condition","year"),all=T)
#   rm(no_women_365)
#   
#   #combine
#   tab19<-merge(no_records,no_women,by=c("condition","year"),all=T)
#   rm(no_records, no_women)
#   
#   tot_women_365<-pregnancy_dt[filter==1,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("condition")]
#   setnames(tot_women_365, "person_id", "women_total_365")
#   tab19<-merge(tab19,tot_women_365, by="condition")
#   rm(tot_women_365)
#   
#   median_fup<-pregnancy_dt[filter==1,lapply(.SD, function(x) round(median(na.omit(x))/365.25,2)), .SDcols="date_dif", by=c("condition","year")]
#   setnames(median_fup,"date_dif", "median_fup_365")
#   tab19<-merge(tab19,median_fup, by=c("condition","year"), all=T)
#   rm(median_fup)
#   #replace NA with 0
#   tab19[is.na(no_records_365), no_records_365:=0]
#   tab19[is.na(no_records), no_records:=0]
#   tab19[is.na(no_women_365), no_women_365:=0]
#   tab19[is.na(no_women), no_women:=0]
#   tab19[is.na(women_total_365), women_total_365:=0]
#   tab19[is.na(median_fup_365), median_fup_365:=0]
#   
#   #removed on 15 feb 2022
#   # no_women_365<-pregnancy_dt[filter==1,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("year", "condition"), .SDcols="person_id"]
#   # setnames(no_women_365, "person_id", "no_women_365")
#   # no_women_total<-pregnancy_dt[,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("year", "condition"), .SDcols="person_id"]
#   # setnames(no_women_total, "person_id", "no_women")
#   # no_women_365<-merge(no_women_365, no_women_total, by=c("year","condition"), all=T)
#   # no_women_365<-no_women_365[is.na(no_women_365), no_women_365:=0]
#   # rm(no_women_total)
#   # pregnancy_dt[,date_dif:=as.numeric(date_dif)]
#   # median<-pregnancy_dt[rowid==1 & filter==1, lapply(.SD, function(x) median(x, na.rm = T)), by=c("year", "condition"), .SDcols="date_dif"]
#   # setnames(median, "date_dif", "median_follow_up")
#   # no_women_365<-merge(no_women_365,median, by=c("year", "condition"), all=T)
#   # no_women_365[is.na(median_follow_up),median_follow_up:=0]
#   # rm(median)
#   # min<-pregnancy_dt[rowid==1 & filter==1, lapply(.SD, function(x) min(x, na.rm = T)), by=c("year", "condition"), .SDcols="date_dif"]
#   # setnames(min,"date_dif","min")
#   # no_women_365<-merge(no_women_365,min, by=c("year","condition"), all=T)
#   # no_women_365[is.na(min),min:=0]
#   # rm(min)
#   # max<-pregnancy_dt[rowid==1 & filter==1, lapply(.SD, function(x) max(x, na.rm = T)), by=c("year", "condition"), .SDcols="date_dif"]
#   # setnames(max,"date_dif","max")
#   # no_women_365<-merge(no_women_365,max, by=c("year","condition"), all=T)
#   # no_women_365[is.na(max),max:=0]
#   # rm(max)
#   # no_women_365[,range_follow_up:=paste(min,max, sep="-")]
#   # no_women_365[,min:=NULL][,max:=NULL]
#   # no_women_365[,percentage_women_365_fup:=round((no_women_365/no_women)*100,2)]
#   
#   saveRDS(tab19, paste0(preg_tmp, "tab19_", names(pregnancy_files)[preg_ind], ".rds"))
#   rm(tab19)
#   pregnancy_dt[,date_dif:=NULL][,filter:=NULL][,rowid:=NULL][,meaning:=NULL][,age_start_follow_up:=NULL]
#   
#   
#   #rates
#   
#   pregnancy_dt[,lag:=time_lag[stage_of_pregnancy==pregnancy_dt[!duplicated(condition),condition],time_lag]]
#   
#   #find all ids that are part of preganncy_study_population but did not have a start preganncy code
#   study_population[!(person_id %in% pregnancy_dt[!duplicated(person_id),person_id]) & pers_preg==1, to_include:=1]
#   
#   #Apply count person time
#   pregnancy_dt<-CountPersonTime2(Dataset_events = pregnancy_dt[,.(person_id, condition, pregnancy_code_date)],
#                                  Dataset = unique(pregnancy_dt[,.(person_id, birth_date, start_follow_up_preg, end_follow_up_preg)]),
#                                  Person_id = "person_id",
#                                  Start_study_time =start_study_date2,
#                                  End_study_time =end_study_date2,
#                                  Start_date = "start_follow_up_preg",
#                                  End_date = "end_follow_up_preg",
#                                  Birth_date = "birth_date",
#                                  Increment = "year",
#                                  Unit_of_age = "year",
#                                  Strata = NULL,
#                                  include_remaning_ages = TRUE,
#                                  Aggregate = T,
#                                  Outcomes_rec = unique(pregnancy_dt[,condition]),
#                                  Name_event = "condition",
#                                  Date_event = "pregnancy_code_date",
#                                  Rec_period =  unique(pregnancy_dt[,lag]),
#                                  Age_bands = c(12,29,39,49),
#                                  print = F, 
#                                  check_overlap = F)
#   pregnancy_dt[,Persontime:=NULL]
#   names(pregnancy_dt)<-c("year", "age_band", "person_years","no_records")
#   #add condition name
#   pregnancy_dt[,stage_pregnancy:=names(pregnancy_files)[preg_ind]]
#   
#   #save to preg_tmp
#   saveRDS(pregnancy_dt, paste0(preg_tmp, "rates_",names(pregnancy_files)[preg_ind],".rds"))
#   rm(pregnancy_dt)
#   
#   #calculate person time for other women part of the pregnancy std population
#   ps_preg_other<-CountPersonTime2(Dataset = unique(study_population[to_include==1,.(person_id, birth_date, start_follow_up_preg,end_follow_up_preg)]),
#                                Person_id = "person_id",
#                                Start_study_time =start_study_date2,
#                                End_study_time = end_study_date2,
#                                Start_date = "start_follow_up_preg",
#                                End_date = "end_follow_up_preg",
#                                Birth_date = "birth_date",
#                                Increment = "year",
#                                Unit_of_age = "year",
#                                include_remaning_ages = TRUE,
#                                Strata = NULL,
#                                Aggregate = T,
#                                Age_bands = agebands_rates_pregnancy,
#                                print = F, 
#                                check_overlap = F)
#   
#   #tab18
#   no_women<-pregnancy_dt[,lapply(.SD,function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("year","age_band")]
#   setnames(no_women, "person_id","no_women")
#   person_years<-pregnancy_dt[,lapply(.SD,sum), .SDcols="person_years", by=c("year","age_band")]
#   tab18<-no_women
#   rm(no_women)
#   tab18<-merge(tab18, person_years, by=c("year","age_band"))
#   rm(person_years)
#   tab18[,women_per_1000_py:=round((no_women/person_years)*1000,2)]
#   tab18[,stage_of_pregnancy:=names(pregnancy_files)[preg_ind]]
#   
#   saveRDS(tab18, paste0(preg_tmp, "tab18_", names(pregnancy_files)[preg_ind], ".rds"))
#   rm(tab18)
#   
# 
#   no_women<-pregnancy_dt[,lapply(.SD,function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("year")]
#   setnames(no_women, "person_id","no_women")
#   person_years<-pregnancy_dt[,lapply(.SD,sum), .SDcols="person_years", by=c("year")]
#   tab17<-no_women
#   rm(no_women)
#   tab17<-merge(tab17, person_years, by=c("year"))
#   rm(person_years)
#   tab17[,women_per_1000_py:=round((no_women/person_years)*1000,2)]
#   tab17[,stage_of_pregnancy:=names(pregnancy_files)[preg_ind]]
#   
#   saveRDS(tab17, paste0(preg_tmp, "tab17_", names(pregnancy_files)[preg_ind], ".rds"))
#   rm(tab17)
#   rm(pregnancy_dt)
# }
# 
# #tab17
# tab17_files<-list.files(preg_tmp, "tab17")
# tab17<-lapply(paste0(preg_tmp, tab17_files), readRDS)
# tab17<-do.call(rbind,tab17)
# setcolorder(tab17,c("stage_of_pregnancy","year","no_women","person_years","women_per_1000_py"))
# 
# if(subpopulations_present=="Yes"){
#   fwrite(tab17, paste0(preg_dir, subpopulations_names[s], "/", subpopulations_names[s], "_pregnancy_rates_y.csv"), row.names = F)
# } else {
#   fwrite(tab17, paste0(preg_dir, "pregnancy_rates_y.csv"), row.names = F)
# }
# 
# for (i in 1:length(tab17_files)){
#   file.remove(paste0(preg_tmp, tab17_files[[i]]))
# }
# rm(tab17_files)
# 
# 
# tab17[, no_women:= as.character(no_women)][as.numeric(no_women) > 0 & as.numeric(no_women) < 5, no_women := "<5"]
# tab17[, person_years:= as.character(person_years)][as.numeric(person_years) > 0 & as.numeric(person_years) < 5, person_years := "<5"]
# tab17[, women_per_1000_py:= as.character(women_per_1000_py)][no_women=="<5" | person_years=="<5", women_per_1000_py := "N/A"]
# 
# if(subpopulations_present=="Yes"){
#   fwrite(tab17, paste0(preg_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_pregnancy_rates_y_masked.csv"), row.names = F)
# } else {
#   fwrite(tab17, paste0(preg_dir, "Masked/", "pregnancy_rates_y_masked.csv"), row.names = F)
# }
# 
# rm(tab17)
# 
# #tab18
# tab18_files<-list.files(preg_tmp, "tab18")
# tab18<-lapply(paste0(preg_tmp, tab18_files), readRDS)
# tab18<-do.call(rbind,tab18)
# setcolorder(tab18,c("stage_of_pregnancy","year","age_band", "no_women","person_years","women_per_1000_py"))
# 
# if(subpopulations_present=="Yes"){
#   fwrite(tab18, paste0(preg_dir, subpopulations_names[s], "/", subpopulations_names[s], "_pregnancy_rates_y_age.csv"), row.names = F)
# } else {
#   fwrite(tab18, paste0(preg_dir, "pregnancy_rates_y_age.csv"), row.names = F)
# }
# 
# for (i in 1:length(tab18_files)){
#   file.remove(paste0(preg_tmp, tab18_files[[i]]))
# }
# rm(tab18_files)
# 
# 
# tab18[, no_women:= as.character(no_women)][as.numeric(no_women) > 0 & as.numeric(no_women) < 5, no_women := "<5"]
# tab18[, person_years:= as.character(person_years)][as.numeric(person_years) > 0 & as.numeric(person_years) < 5, person_years := "<5"]
# tab18[, women_per_1000_py:= as.character(women_per_1000_py)][no_women=="<5" | person_years=="<5", women_per_1000_py := "N/A"]
# 
# if(subpopulations_present=="Yes"){
#   fwrite(tab18, paste0(preg_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_pregnancy_rates_y_age_masked.csv"), row.names = F)
# } else {
#   fwrite(tab18, paste0(preg_dir, "Masked/", "pregnancy_rates_y_age_masked.csv"), row.names = F)
# }
# 
# rm(tab18)
# 
# #tab19
# 
# tab19_files<-list.files(preg_tmp, "tab19")
# tab19<-lapply(paste0(preg_tmp, tab19_files), readRDS)
# tab19<-do.call(rbind,tab19)
# setnames(tab19,"condition","stage_of_pregnancy")
# 
# if(subpopulations_present=="Yes"){
#   fwrite(tab19, paste0(preg_dir, subpopulations_names[s], "/", subpopulations_names[s], "_pregnancy_follow_up.csv"), row.names = F)
# } else {
#   fwrite(tab19, paste0(preg_dir, "pregnancy_follow_up.csv"), row.names = F)
# }
# 
# for (i in 1:length(tab19_files)){
#   file.remove(paste0(preg_tmp, tab19_files[[i]]))
# }
# rm(tab19_files)
# 
# 
# tab19[, no_women_365:= as.character(no_women_365)][as.numeric(no_women_365) > 0 & as.numeric(no_women_365) < 5, no_women_365 := "<5"]
# tab19[, no_women:= as.character(no_women)][as.numeric(no_women) > 0 & as.numeric(no_women) < 5, no_women := "<5"]
# tab19[, median_follow_up:= as.character(median_follow_up)][as.numeric(median_follow_up) > 0 & as.numeric(median_follow_up) < 5, median_follow_up := "<5"]
# tab19[, percentage_women_365_fup:= as.character(percentage_women_365_fup)][no_women_365=="<5" | median_follow_up=="<5", percentage_women_365_fup := "N/A"]
# 
# if(subpopulations_present=="Yes"){
#   fwrite(tab19, paste0(preg_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_pregnancy_follow_up_masked.csv"), row.names = F)
# } else {
#   fwrite(tab19, paste0(preg_dir, "Masked/", "pregnancy_follow_up_masked.csv"), row.names = F)
# }
# 
# rm(tab19)
# 
# 
# #graph1
# 
# graph1_files<-list.files(preg_tmp,"graph1")
# graph1<-lapply(paste0(preg_tmp, graph1_files), readRDS)
# graph1<-do.call(rbind,graph1)
# rm(graph1_files)
# 
# if (subpopulations_present=="Yes"){
#   fwrite(graph1, paste0(preg_dir,subpopulations_names[s], "/", subpopulations_names[s],"_","pregnancy_graph_1.csv"))
# } else {
#   fwrite(graph1, paste0(preg_dir,"pregnancy_graph_1.csv"))
# }
# 
# #apply masking
# graph1[, code_1_per_year:= as.character(code_1_per_year)][as.numeric(code_1_per_year) > 0 & as.numeric(code_1_per_year) < 5, code_1_per_year := "<5"]
# graph1[, codes_2_per_year:= as.character(codes_2_per_year)][as.numeric(codes_2_per_year) > 0 & as.numeric(codes_2_per_year) < 5, codes_2_per_year := "<5"]
# graph1[, codes_3_per_year:= as.character(codes_3_per_year)][as.numeric(codes_3_per_year) > 0 & as.numeric(codes_3_per_year) < 5, codes_3_per_year := "<5"]
# graph1[, codes_min_4_per_year:= as.character(codes_min_4_per_year)][as.numeric(codes_min_4_per_year) > 0 & as.numeric(codes_min_4_per_year) < 5, codes_min_4_per_year := "<5"]
# graph1[, total_women:= as.character(total_women)][as.numeric(total_women) > 0 & as.numeric(total_women) < 5, total_women := "<5"]
# 
# if (subpopulations_present=="Yes"){
#   fwrite(graph1, paste0(preg_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_graph_1_masked.csv"), row.names = F)
# } else {
#   fwrite(graph1, paste0(preg_dir,"Masked/", "graph_1_masked.csv"), row.names = F)
# }
# rm(graph1)
# 
# 
# #graph2
# 
# graph2_files<-list.files(preg_tmp,"graph2")
# graph2<-lapply(paste0(preg_tmp, graph2_files), readRDS)
# graph2<-do.call(rbind,graph2)
# rm(graph2_files)
# 
# if (subpopulations_present=="Yes"){
#   fwrite(graph2, paste0(preg_dir,subpopulations_names[s], "/", subpopulations_names[s],"_","pregnancy_graph_2.csv"))
# } else {
#   fwrite(graph2, paste0(preg_dir,"pregnancy_graph_2.csv"))
# }
# 
# #apply masking
# graph2[, code_1_per_year:= as.character(code_1_per_year)][as.numeric(code_1_per_year) > 0 & as.numeric(code_1_per_year) < 5, code_1_per_year := "<5"]
# graph2[, codes_2_per_year:= as.character(codes_2_per_year)][as.numeric(codes_2_per_year) > 0 & as.numeric(codes_2_per_year) < 5, codes_2_per_year := "<5"]
# graph2[, codes_3_per_year:= as.character(codes_3_per_year)][as.numeric(codes_3_per_year) > 0 & as.numeric(codes_3_per_year) < 5, codes_3_per_year := "<5"]
# graph2[, codes_min_4_per_year:= as.character(codes_min_4_per_year)][as.numeric(codes_min_4_per_year) > 0 & as.numeric(codes_min_4_per_year) < 5, codes_min_4_per_year := "<5"]
# graph2[, total_women:= as.character(total_women)][as.numeric(total_women) > 0 & as.numeric(total_women) < 5, total_women := "<5"]
# 
# if (subpopulations_present=="Yes"){
#   fwrite(graph2, paste0(preg_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_graph_2_masked.csv"), row.names = F)
# } else {
#   fwrite(graph2, paste0(preg_dir,"Masked/", "graph_2_masked.csv"), row.names = F)
# }
# rm(graph2)
# 
