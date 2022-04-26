###########################################################################################
#Combine results 
##########################################################################################

#########################
#Identify all subjects that never had a diagnosis
#########################
# ids_no_diagnosis_events_fl<-list.files(events_tmp,"events_not_ids_stdpop")
# if(length(ids_no_diagnosis_events_fl)>0){
#   ids_no_diagnosis_events<-lapply(paste0(events_tmp,ids_no_diagnosis_events_fl),readRDS)  
#   ids_no_diagnosis_events<-do.call(rbind,ids_no_diagnosis_events)
#   
#   for (i in 1:length(ids_no_diagnosis_events_fl)){
#     unlink(paste0(events_tmp,ids_no_diagnosis_events_fl[i]))
#   }
#   rm(ids_no_diagnosis_events_fl)
# } else {
#   ids_no_diagnosis_events<-NULL 
#   rm(ids_no_diagnosis_events_fl)
# }
# 
# ids_no_diagnosis_mo_fl<-list.files(mo_tmp,"mo_not_ids_stdpop")
# if(length(ids_no_diagnosis_mo_fl)>0){
#   ids_no_diagnosis_mo<-lapply(paste0(mo_tmp,ids_no_diagnosis_mo_fl),readRDS)  
#   ids_no_diagnosis_mo<-do.call(rbind,ids_no_diagnosis_mo)
#   
#   for (i in 1:length(ids_no_diagnosis_mo_fl)){
#     unlink(paste0(mo_tmp,ids_no_diagnosis_mo_fl[i]))
#   }
#   rm(ids_no_diagnosis_mo_fl)
# } else {
#   ids_no_diagnosis_mo<-NULL 
#   rm(ids_no_diagnosis_mo_fl)
# }
# 
# ids_no_diagnosis_so_fl<-list.files(so_tmp,"so_not_ids_stdpop")
# if(length(ids_no_diagnosis_so_fl)>0){
#   ids_no_diagnosis_so<-lapply(paste0(so_tmp,ids_no_diagnosis_so_fl),readRDS)  
#   ids_no_diagnosis_so<-do.call(rbind,ids_no_diagnosis_so)
#   
#   for (i in 1:length(ids_no_diagnosis_so_fl)){
#     unlink(paste0(so_tmp,ids_no_diagnosis_so_fl[i]))
#   }
#   rm(ids_no_diagnosis_so_fl)
# } else {
#   ids_no_diagnosis_so<-NULL 
#   rm(ids_no_diagnosis_so_fl)
# }
# 
# #Find duplicates between all of the tables(they shouldn't show in any of the tables)
# if(!is.null(ids_no_diagnosis_events) | !is.null(ids_no_diagnosis_mo) | !is.null(ids_no_diagnosis_so)){
# ids_no_diagnosis_events<-rbind(ids_no_diagnosis_events,ids_no_diagnosis_mo)
# rm(ids_no_diagnosis_mo)
# ids_no_diagnosis_events<-ids_no_diagnosis_events[duplicated(person_id)]
# ids_no_diagnosis_events<-rbind(ids_no_diagnosis_events,ids_no_diagnosis_so)
# rm(ids_no_diagnosis_so)
# ids_no_diagnosis_events<-ids_no_diagnosis_events[duplicated(person_id)]
# } else {
#   ids_no_diagnosis_events<-NULL
#   rm(ids_no_diagnosis_mo)
#   rm(ids_no_diagnosis_so)
# }

print("Calculating person time for subject that do not have a diagnoses.")
study_population[no_event_id==1 & no_mo_id==1 & no_so_id==1, no_diagosis:=1]
study_population[,no_event_id:=NULL][,no_mo_id:=NULL][,no_so_id:=NULL]

#apply count person time and save, in chunks
#Create chunks of 100.000
study_population_no_diag<-study_population[no_diagosis==1]
study_population<-study_population[is.na(no_diagosis)]
if(study_population_no_diag[,.N]>0){
  size<-100000
  groups<-round(study_population_no_diag[,.N]/size)
  index<-1
  min<-1
  max<-study_population_no_diag[,.N]
  chunks<-rep(NA, groups)
  for (size_ind in 1: groups){
    if(index< groups){
      chunks[index]<-paste0(min,":", size_ind*size)
      min<-size_ind*size+1
      index<-index+1
    } else {
      chunks[index]<-paste0(min,":", study_population_no_diag[,.N]) 
  }
}

#Apply countperson time by chunks
for (size_ind in 1:length(chunks)){
  min<-unlist(str_split(chunks[size_ind],":"))[1]
  max<-unlist(str_split(chunks[size_ind],":"))[2]
  print(paste0("Analysing chunk ", min,":",max," of the study population with no diagnosis in the study period." ))
  ps_no_diag<-CountPersonTime2(Dataset = unique(study_population_no_diag[min:max,.(person_id, birth_date, start_follow_up,end_follow_up, sex_at_instance_creation)]),                                 
                               Person_id = "person_id",
                                Start_study_time =start_study_date2,
                                End_study_time = end_study_rates,
                                Start_date = "start_follow_up",
                                End_date = "end_follow_up",
                                Birth_date = "birth_date",
                                Increment = "year",
                                Unit_of_age = "year",
                                include_remaning_ages = TRUE,
                                Strata = c("sex_at_instance_creation"),
                                Aggregate = T,
                                Age_bands = agebands_rates,
                                print = F, 
                                check_overlap = F)
ps_no_diag<-as.data.table(ps_no_diag)  
names(ps_no_diag)<-c("sex", "year","age_band","person_years")    
#save results in diag_tmp
saveRDS(ps_no_diag, paste0(diag_tmp,size_ind,"_no_id_py.rds"))
rm(ps_no_diag)
  }
}
rm(study_population_no_diag)

#Combine all results of the study_population_no_diag
#person_years are in days
ps_no_diag_fl<-list.files(diag_tmp,"no_id_py.rds")
if(length(ps_no_diag_fl)>0){
  ps_no_diag<-lapply(paste0(diag_tmp, ps_no_diag_fl), readRDS)  
  ps_no_diag<-do.call(rbind,ps_no_diag)
  ps_no_diag<-as.data.table(ps_no_diag)
  
  for (i in 1:length(ps_no_diag_fl)){
    file.remove(paste0(diag_tmp, ps_no_diag_fl[i]))
  }
  saveRDS(ps_no_diag, paste0(diag_tmp,"no_id_py.rds"))
}
rm(ps_no_diag_fl)
##########################
#Identify all people present in the study population
##########################
#events
pers_events_files<-list.files(events_tmp, "pers_events")
if(length(pers_events_files)>0){
  pers_events<-lapply(paste0(events_tmp, pers_events_files), readRDS)
  pers_events<-do.call(rbind,pers_events)
  pers_events<-as.data.table(pers_events)
  pers_events[,pers_events:=1]
  study_population<-merge.data.table(study_population,pers_events, by=c("person_id","birth_date","start_follow_up","end_follow_up","sex_at_instance_creation"), all.x = T)
  rm(pers_events)
} else {
  study_population[,pers_events:=NA] 
}

pers_mo_files<-list.files(mo_tmp, "pers_mo")
if(length(pers_mo_files)>0){
  pers_mo<-lapply(paste0(mo_tmp, pers_mo_files), readRDS)
  pers_mo<-do.call(rbind,pers_mo)
  pers_mo<-as.data.table(pers_mo)
  pers_mo[,pers_mo:=1]
  study_population<-merge.data.table(study_population,pers_mo, by=c("person_id","birth_date","start_follow_up","end_follow_up","sex_at_instance_creation"), all.x = T)
  rm(pers_mo)
} else {
  study_population[,pers_mo:=NA] 
}

pers_so_files<-list.files(so_tmp, "pers_so")
if(length(pers_so_files)>0){
  pers_so<-lapply(paste0(so_tmp, pers_so_files), readRDS)
  pers_so<-do.call(rbind,pers_so)
  pers_so<-as.data.table(pers_so)
  pers_so[,pers_so:=1]
  study_population<-merge.data.table(study_population,pers_so, by=c("person_id","birth_date","start_follow_up","end_follow_up","sex_at_instance_creation"), all.x = T)
  rm(pers_so)
} else {
  study_population[,pers_so:=NA] 
}

#if a person is present in one of the tables, then is present in the diagnoses_study_population
study_population[pers_events==1 | pers_mo==1 | pers_so==1, pers_diagnosis:=1]
study_population[,pers_events:=NULL][,pers_mo:=NULL][,pers_so:=NULL]

#remove people that where excluded from the diagnoses_study_population
excluded_people<-study_population[is.na(pers_diagnosis),.N]
study_population<-study_population[pers_diagnosis==1]
#Apply count person time to save the person time for all people that had a diagnosis 
#Apply countperson time in  chunks
if(study_population[,.N]>0){
  size<-100000
  groups<-round(study_population[,.N]/size)
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
    print(paste0("Analysing chunk ", min,":",max," of the study population with at least one diagnosis in the study period." ))
    
    ps_all_diag<-CountPersonTime2(Dataset = unique(study_population[min:max,.(person_id, birth_date, start_follow_up,end_follow_up, sex_at_instance_creation)]),
                                  Person_id = "person_id",
                                  Start_study_time =start_study_date2,
                                  End_study_time = end_study_rates,
                                  Start_date = "start_follow_up",
                                  End_date = "end_follow_up",
                                  Birth_date = "birth_date",
                                  Increment = "year",
                                  Unit_of_age = "year",
                                  include_remaning_ages = TRUE,
                                  Strata = c("sex_at_instance_creation"),
                                  Aggregate = F,
                                  Age_bands = agebands_rates,
                                  print = F, 
                                  check_overlap = F)
    
    ps_all_diag<-as.data.table(ps_all_diag)
    names(ps_all_diag)<-c("person_id", "sex", "age_band","year","person_years")
    saveRDS(ps_all_diag,paste0(diag_tmp, size_ind, "_py_diagnosis.rds"))
    rm(ps_all_diag)
  }
}
#remove study_population
rm(study_population)

#Load all files for py and combine
py_fl<-list.files(diag_tmp, "py_diagnosis.rds")
if(length(py_fl)>0){
  py_diag<-lapply(paste0(diag_tmp,py_fl), readRDS)  
  py_diag<-do.call(rbind,py_diag)
  py_diag<-as.data.table(py_diag)
}

###################
#flowchart
###################
flowchart<-data.table(flowchart_events,flowchart_mo[,2], flowchart_so[,2])
rm(flowchart_events,flowchart_mo,flowchart_so)

if(subpopulations_present=="Yes"){
  write.csv(flowchart, paste0(diag_dir, subpopulations_names[s], "/", subpopulations_names[s], "_diagnoses_flowchart.csv"), row.names = F)
} else {
  write.csv(flowchart, paste0(diag_dir, "diagnoses_flowchart.csv"), row.names = F)
}

########
#Apply masking
########
if(length(actual_tables$EVENTS)>0){
  flowchart[, EVENTS:= as.character(EVENTS)][as.numeric(EVENTS) > 0 & as.numeric(EVENTS) < 5, EVENTS := "<5"]
}
if(length(actual_tables$MEDICAL_OBSERVATIONS)>0){
  flowchart[, MEDICAL_OBSERVATIONS:= as.character(MEDICAL_OBSERVATIONS)][as.numeric(MEDICAL_OBSERVATIONS) > 0 & as.numeric(MEDICAL_OBSERVATIONS) < 5, MEDICAL_OBSERVATIONS := "<5"]
}
if(length(actual_tables$SURVEY_OBSERVATIONS)>0){
  flowchart[, SURVEY_OBSERVATIONS:= as.character(SURVEY_OBSERVATIONS)][as.numeric(SURVEY_OBSERVATIONS) > 0 & as.numeric(SURVEY_OBSERVATIONS) < 5, SURVEY_OBSERVATIONS := "<5"]
}

if(subpopulations_present=="Yes"){
  write.csv(flowchart, paste0(diag_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_diagnoses_flowchart_masked.csv"), row.names = F)
} else {
  write.csv(flowchart, paste0(diag_dir, "Masked/", "diagnoses_flowchart_masked.csv"), row.names = F)
}

rm(flowchart)
###################
#description
###################
description<-data.table(description_events,description_mo[,2], description_so[,2])
rm(description_events,description_mo,description_so)

if(subpopulations_present=="Yes"){
  write.csv(description, paste0(diag_dir, subpopulations_names[s], "/", subpopulations_names[s], "_diagnoses_description.csv"), row.names = F)
} else {
  write.csv(description, paste0(diag_dir, "diagnoses_description.csv"), row.names = F)
}

##################
#Apply masking
##################
if(length(actual_tables$EVENTS)>0){
  if(description[5, 2]<5 & description[5, 2]>0) {description[5, 2]<-"<5"}
}
if(length(actual_tables$MEDICAL_OBSERVATIONS)>0){
  if(description[5, 3]<5 & description[5, 3]>0) {description[5, 3]<-"<5"} 
}
if(length(actual_tables$SURVEY_OBSERVATIONS)>0){
  if(description[5, 4]<5 & description[5, 4]>0) {description[5, 4]<-"<5"} 
}

if(subpopulations_present=="Yes"){
  write.csv(description, paste0(diag_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_diagnoses_description_masked.csv"), row.names = F)
} else {
  write.csv(description, paste0(diag_dir, "Masked/", "diagnoses_description_masked.csv"), row.names = F)
}
rm(description)

###################
#tab20
##################
tab20<-rbind(tab20_events, tab20_mo, tab20_so)
tab20<-as.data.table(tab20)
rm(tab20_events,tab20_mo,tab20_so)
#combine results if same meaning+year combination exists
tab20<-tab20[,lapply(.SD,sum), .SDcols=c("no_records", "no_empty_code"), by=.(meaning,year)]
tab20<-tab20[,percentage_empty_code:=round((no_empty_code/no_records)*100, digits=2)]
tab20<-data.table(tab20, data_access_provider= data_access_provider_name, data_source=data_source_name)


if(subpopulations_present=="Yes"){
  write.csv(tab20, paste0(diag_dir, subpopulations_names[s], "/", subpopulations_names[s], "_diagnoses_completeness.csv"), row.names = F)
} else {
  write.csv(tab20, paste0(diag_dir, "diagnoses_completeness.csv"), row.names = F)
}

tab20[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
tab20[, no_empty_code:= as.character(no_empty_code)][as.numeric(no_empty_code) > 0 & as.numeric(no_empty_code) < 5, no_empty_code := "<5"]
tab20[, percentage_empty_code:= as.character(percentage_empty_code)][no_empty_code == "<5", percentage_empty_code := "N/A"]

if(subpopulations_present=="Yes"){
  write.csv(tab20, paste0(diag_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_diagnoses_completeness_masked.csv"), row.names = F)
} else {
  write.csv(tab20, paste0(diag_dir, "Masked/", "diagnoses_completeness_masked.csv"), row.names = F)
}

rm(tab20)

###########################
#Rates of recurrent events and data cleanup
###########################
diagnoses_files<-list.files(paste0(populations_dir, "DIAGNOSES/"))
files<-list()
for (i in 1: length(diagnoses_files)){
  files<-append(files,unique(list(unlist(str_split(diagnoses_files[i],"_"))[2])))
}
files<-do.call(c,files)
#remove duplicates 
files<-files[!duplicated(files)]
#create list with names year_condition
diagnoses_list<-vector(mode="list", length=length(files))
names(diagnoses_list)<-files
rm(files)
#separate all files into the right category
for (i in 1:length(diagnoses_list)){
  diagnoses_list[[i]]<-diagnoses_files[str_detect(diagnoses_files,paste0("_", names(diagnoses_list)[i], "_"))]
}
rm(diagnoses_files)
diagnoses_files<-diagnoses_list
rm(diagnoses_list)

#Each file is separated by type of event and year
#count person time to be applied to each file
#time lags more than 1 year will not be considered
time_lag<-data.table(condition=c("Depression", "Elective abortion",
                                 "Gestational diabetes","Multiple gestation"," Preeclampsia", "Spontaneous abortion",
                                 "TOPFA", "Breast cancer"),
                     time_lag=c(3*30, 8*7, 23*7, 23*7, 8*7, 8*7, 8*7,0))

#create a loop that woud run count person time for each diagnoses separately
duplicated_event_dates<-data.table(event_definition=names(diagnoses_files),original_rows=0,duplicates=0,duplicates_time_lag=as.character(0))
#duplicates: diagnosis at the same date are counted and removed
#duplicates_time_lag: diagnosis within the time lag are counted and removed


for (condition_ind in 1:length(diagnoses_files)){
  diag_file<-lapply(paste0(populations_dir,"DIAGNOSES/", diagnoses_files[[condition_ind]]), readRDS)
  diag_file<-do.call(rbind,diag_file)
  diag_file<-as.data.table(diag_file)
  #remove all records at the same day for the same individual
  diag_file[,pers_date:=paste0(person_id,"_", event_date)]
  orig_no_rows<-diag_file[,.N]
  duplicated_event_dates[event_definition==names(diagnoses_files)[condition_ind],original_rows:=orig_no_rows]
  
  #remove duplicates
  diag_file<-diag_file[!duplicated(pers_date)]
  diag_file[,pers_date:=NULL]
  no_duplicates_rows<-diag_file[,.N]
  #count number of duplicates
  duplicated_event_dates[event_definition==names(diagnoses_files)[condition_ind],duplicates:=orig_no_rows-no_duplicates_rows]
  
  #remove all records that are over the time lag
  if(diag_file[!duplicated(condition), condition] %in% time_lag[,condition]){
  diag_file[,lag:=time_lag[condition == names(diagnoses_files)[condition_ind],time_lag]]
    } else {
  diag_file[,lag:=NA]
  }
      #create lag variable based on condition
  if(diag_file[!is.na(lag),.N]>0){
    #Step 1: Order the dataset by person_id, condition and date of event
    diag_file<-diag_file[order(person_id,condition,event_date)]
    #Step 2: Create date_2(by shifting the first date)
    diag_file[,date_1:=shift(event_date)]
    diag_file[,date_2:=event_date]
    #Step 3: Create rowid(whhcih will give the number of rows for each person)
    diag_file[,rowid:=rowid(person_id)]
    #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
    diag_file[rowid==1,date_1:=NA]
    #Step 5: Create date_dif as difference between date 2 and date
    diag_file[,date_dif:=date_2-date_1]
    #Step 6: Remove these rows 
    diag_file<-diag_file[date_dif>lag | is.na(date_dif)]
    #Step 7: Repeat until there are no more impossible dates in the dataset
    diag_file[,date_dif:=date_2-date_1]
    while(diag_file[date_dif<=lag,.N]>0){
      diag_file<-diag_file[date_dif>lag | is.na(date_dif)]
      diag_file[,date_dif:=date_2-date_1]
    }
    #count number of duplicates
    duplicated_event_dates[event_definition==names(diagnoses_files)[condition_ind],duplicates_time_lag:=no_duplicates_rows -diag_file[,.N]]
    diag_file[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,lag:=NULL][,date_dif:=NULL]
    
  } else {
    #Step 1: Order the dataset by person_id, condition and date of event
    diag_file<-diag_file[order(person_id,condition,event_date)]
    #Step 2: Create rowid(whhcih will give the number of rows for each person)
    diag_file[,rowid:=rowid(person_id)]
    #Step 3: keep only one row per person
    diag_file<-diag_file[rowid==1] 
    diag_file[,rowid:=NULL]
    
    #count number of duplicates
    duplicated_event_dates[event_definition==names(diagnoses_files)[condition_ind],duplicates_time_lag:=no_duplicates_rows -diag_file[,.N]]
  }
  rm(orig_no_rows,no_duplicates_rows)
 
   #remove all files in population_diagnoses
  for (i in 1:length(diagnoses_files[[condition_ind]])){
    file.remove(paste0(diag_pop, diagnoses_files[[condition_ind]][[i]]))
  }
  #save the new files by year
  for (i in 1:length(unique(diag_file[["year"]]))){
    saveRDS(diag_file[year==unique(diag_file[["year"]])[i]], paste0(diag_pop, unique(diag_file[["year"]])[i],"_", unique(diag_file[["condition"]]),"_","diagnoses.rds"))
  }
  
  
  diag_file[,combined_diagnoses:=paste(condition,truncated_code,event_vocabulary,sep=":")]
  #create tab 21(code count by code)
  tab21_counts<-diag_file[,.N, by=c("meaning","year", "combined_diagnoses")]
  setnames(tab21_counts,"N","no_records")
  #separate the combined diagnoses file
  tab21_counts[,c("event_definition","truncated_code","vocabulary") := tstrsplit(combined_diagnoses, ":")]
  tab21_counts[,combined_diagnoses:=NULL]
  tab21_counts[,total_records:=lapply(.SD,sum), .SDcols="no_records", by="event_definition"]
  setcolorder(tab21_counts, c("event_definition","meaning","year", "truncated_code","vocabulary"))
  
  saveRDS(tab21_counts, paste0(diag_tmp, tab21_counts[!duplicated(event_definition), event_definition], "_tab21.rds"))
  rm(tab21_counts)
  
  #remove event_code and vocabulary, condition will be used as event count and remove all other uneccessary columns
  diag_file[,event_code:=NULL][,event_vocabulary:=NULL][,meaning:=NULL][,age_start_follow_up:=NULL][,obs_out:=NULL][,filter:=NULL][,code_nodot:=NULL][,truncated_code:=NULL]
  
  if(recurrent_event_analysis %in% c("Yes","yes")){
    print("Calculating rates of recurrent events")
  #grab and combine person time for all people that had a diagnoses that are not part of the diagoses of interest
  py_other<-py_diag[!(person_id %in% diag_file[!duplicated(person_id),person_id])][,lapply(.SD, sum), by=c("sex", "year", "age_band"), .SDcols="person_years"]
  py_other[,no_records:=0]  
  #calculate counts
  if(diag_file[,.N]!=0){
    if (names(diagnoses_files)[condition_ind] %in% time_lag[,condition]){
      outcomes_list<-unique(diag_file[,condition])
      #apply count person time
      output<-CountPersonTime2(Dataset_events = diag_file[,.(person_id, condition, event_date)],
                               Dataset = unique(diag_file[,.(person_id, birth_date, start_follow_up, end_follow_up, sex_at_instance_creation)]),
                               Person_id = "person_id",
                               Start_study_time =start_study_date2,
                               End_study_time =end_study_rates,
                               Start_date = "start_follow_up",
                               End_date = "end_follow_up",
                               Birth_date = "birth_date",
                               Increment = "year",
                               Unit_of_age = "year",
                               Strata = c("sex_at_instance_creation"),
                               include_remaning_ages = TRUE,
                               Aggregate = T,
                               Outcomes_rec = outcomes_list,
                               Name_event = "condition",
                               Date_event = "event_date",
                               Rec_period = rep(time_lag[condition==names(diagnoses_files)[[condition_ind]],time_lag], length(outcomes_list)),
                               Age_bands = agebands_rates,
                               print = F, 
                               check_overlap = F)
      output<-as.data.table(output)
      
    } else {
      outcomes_list<-unique(diag_file[,condition])
      #apply count person time
      output<-CountPersonTime2(Dataset_events = diag_file[,.(person_id, condition, event_date)],
                               Dataset = unique(diag_file[,.(person_id, birth_date, start_follow_up, end_follow_up, sex_at_instance_creation)]),
                               Person_id = "person_id",
                               Start_study_time =start_study_date2,
                               End_study_time =end_study_rates,
                               Start_date = "start_follow_up",
                               End_date = "end_follow_up",
                               Birth_date = "birth_date",
                               Increment = "year",
                               Unit_of_age = "year",
                               Strata = c("sex_at_instance_creation"),
                               include_remaning_ages = TRUE,
                               Aggregate = T,
                               Outcomes_rec = outcomes_list,
                               Name_event = "condition",
                               Date_event = "event_date",
                               Rec_period = rep(0, length(outcomes_list)),
                               Age_bands = agebands_rates,
                               print = F, 
                               check_overlap = F)
      output<-as.data.table(output)
    }
    
    output[,Persontime:=NULL]
    #from wide to long(remove all person time)
    #ps<-output[,colnames(output)[str_detect(colnames(output), "Persontime_")]]
    #person_years_df<-output[,..ps]
    #output[,colnames(output)[str_detect(colnames(output), "Persontime_")]]<-NULL
    #ps<-unlist(lapply(ps, function(x) str_replace(x,"Persontime_","")))
    #names(person_years_df)<-ps
    #person_years_df<-data.table(person_years_df,output[,c("sex_at_instance_creation","meaning","year","Ageband")])
    
    # person_years_df<-person_years_df[,lapply(.SD,function(x) as.numeric(x)), .SDcols=ps, by=c("sex_at_instance_creation","meaning","year","Ageband")]
    # person_years_df<-melt(person_years_df, id.vars=c("sex_at_instance_creation","meaning", "Ageband","year"), measure.vars = colnames(person_years_df)[!colnames(person_years_df) %in% c("sex_at_instance_creation","meaning", "Ageband","year")], variable.name = "combined_diagnoses")        
    # setnames(person_years_df,"value","person_years")
    # #sum over meaning(person years by year, sex and age band)
    # person_years_df<-person_years_df[,lapply(.SD, sum), by=c("year","Ageband","sex_at_instance_creation"), .SDcols="person_years"]
    # 
    # output<-melt(output, id.vars=c("sex_at_instance_creation","meaning", "Ageband","year"), measure.vars = colnames(output)[!colnames(output) %in% c("sex_at_instance_creation","meaning", "Ageband","year")], variable.name = "combined_diagnoses")        
    # output[,combined_diagnoses:= gsub('.{2}$','',combined_diagnoses)]
    # setnames(output,"value","no_records")
    # person_years_df[,sex_at_instance_creation:=as.character(sex_at_instance_creation)][,Ageband:=as.character(Ageband)][,year:=as.character(year)]
    # output[,sex_at_instance_creation:=as.character(sex_at_instance_creation)][,meaning:=as.character(meaning)][,Ageband:=as.character(Ageband)][,year:=as.character(year)][,combined_diagnoses:=as.character(combined_diagnoses)]
    # output<-merge(person_years_df,output,by=c("sex_at_instance_creation","Ageband","year"), all=T)
    # rm(person_years_df)
    # setnames(output, "Ageband", "age_band")
    # setnames(output, "sex_at_instance_creation", "sex")
    # # output[,person_years:=round(person_years/365.25,3)]
    # output[age_band=="0-0",age_band:=0]
    # output<-output[person_years!=0]
    #will be used to count users
    #output<-output[no_records!=0]
    
    names(output)<-c("sex", "year", "age_band", "person_years","no_records") 
    #Add person time from other people in the study population
    output<-rbind(output,py_other)
    output<-as.data.table(output)
    rm(py_other)
    #sum the person time
    output<-output[,lapply(.SD, sum), by=c("sex","year","age_band"), .SDcols=c("no_records","person_years")]
    output[,event_definition:=names(diagnoses_files)[condition_ind]]
    
  } else {
    output<-NULL
  }
  
  #Load person time for all people with no diagnosis
  no_diag_py_fl<-list.files(diag_tmp, "no_id_py.rds")
  if(length(no_diag_py_fl)>0){
    no_diag_py<-readRDS(paste0(diag_tmp, "no_id_py.rds"))
    no_diag_py<-as.data.table(no_diag_py)
    no_diag_py[,no_records:=0]
    no_diag_py[,event_definition:=names(diagnoses_files)[condition_ind]]
    #combine py
    output<-rbind(output,no_diag_py)
    output<-as.data.table(output)
    rm(no_diag_py)
  }
  rm(no_diag_py_fl)
  
  output<-output[,lapply(.SD, sum), by=c("event_definition", "sex","year","age_band"), .SDcols=c("no_records","person_years")]
  output<-output[,person_years:=round(person_years/365.25,3)]
  
  saveRDS(output, paste0(diag_tmp, names(diagnoses_files)[condition_ind], "_rates_rec.rds"))
    rm(output)
  }
}
rm(diagnoses_files)
####################################################################################################
 
  ############################
  #tab21
  ############################
  tab21_files<-list.files(diag_tmp,"_tab21")
  tab21<-lapply(paste0(diag_tmp, tab21_files), readRDS)
  tab21<-do.call(rbind, tab21)
  tab21<-as.data.table(tab21)
  setcolorder(tab21, c("event_definition","meaning","year", "truncated_code", "vocabulary", "no_records", "total_records"))
  tab21<-data.table(tab21, data_access_provider= data_access_provider_name, data_source=data_source_name)
  
  if(subpopulations_present=="Yes"){
    write.csv(tab21, paste0(diag_dir, subpopulations_names[s], "/", subpopulations_names[s], "_diagnoses_counts_m.csv"), row.names = F)
  } else {
    write.csv(tab21, paste0(diag_dir, "diagnoses_counts_m.csv"), row.names = F)
  }
  
  for (i in 1:length(tab21_files)){
    file.remove(paste0(diag_tmp, tab21_files[[i]]))
  }
  rm(tab21_files)
  
  tab21[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
  tab21[, total_records:= as.character(total_records)][as.numeric(total_records) > 0 & as.numeric(total_records) < 5, total_records := "<5"]
  
  if(subpopulations_present=="Yes"){
    write.csv(tab21, paste0(diag_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_diagnoses_counts_m_masked.csv"), row.names = F)
  } else {
    write.csv(tab21, paste0(diag_dir, "Masked/", "diagnoses_counts_m_masked.csv"), row.names = F)
  }
  
  rm(tab21)
  
  #old
  #  tab21_counts<-diag[,lapply(.SD,sum), by=c("event_definition","meaning","vocabulary","truncated_code"), .SDcols="no_records"]
  # tab21_total<-tab21_counts[,lapply(.SD,sum), by="event_definition", .SDcols="no_records"]
  # setnames(tab21_total,"no_records","total_records")
  # #counts by event_definition, meaning, vocabulary and truncate_code
  # #total by event_definition
  # tab21_counts[,event_definition:=as.character(event_definition)]
  # tab21_total[,event_definition:=as.character(event_definition)]
  # tab21_counts<-merge(tab21_counts, tab21_total, by="event_definition")
  # rm(tab21_total)
  # 
  # saveRDS(tab21_counts,paste0(diag_tmp, unlist(str_split(diagnoses_files[cond_ind],"_"))[1], "_tab21.rds"))
  # rm(tab21_counts)
  
  ##############################
  #removed subjects
  #############################

  if(subpopulations_present=="Yes"){
    write.csv(duplicated_event_dates, paste0(diag_dir, subpopulations_names[s], "/", subpopulations_names[s], "_diagnoses_remove_duplicated_diagoses.csv"), row.names = F)
  } else {
    write.csv(duplicated_event_dates, paste0(diag_dir, "diagnoses_remove_duplicated_diagoses.csv"), row.names = F)
  }
  
  rm(duplicated_event_dates)
  
  excluded_people<-data.table(Indicator="People removed due to exclusion criteria", Number=excluded_people)
  if(subpopulations_present=="Yes"){
    write.csv(excluded_people, paste0(diag_dir, subpopulations_names[s], "/", subpopulations_names[s], "_excluded_people_diagoses.csv"), row.names = F)
  } else {
    write.csv(excluded_people, paste0(diag_dir, "excluded_people_diagoses.csv"), row.names = F)
  }
  
  rm(excluded_people)
  
  ############################
  #tab22
  ############################
  if(recurrent_event_analysis %in% c("Yes","yes")){
  diagnoses_files<-list.files(diag_tmp, "rates_rec")
  tab22<-lapply(paste0(diag_tmp, diagnoses_files), readRDS)
  tab22<-do.call(rbind, tab22)
  tab22<-as.data.table(tab22)
  
  #combine counts by year and sex
  tab22_counts_year_sex<-tab22[,lapply(.SD,sum), by=c("event_definition","year", "sex"), .SDcols=c("no_records", "person_years")]
  # tab22_counts_year_sex[,age_band:="All"]
  
  
  tab22_counts_year<-tab22[,lapply(.SD,sum), by=c("event_definition","year"), .SDcols=c("no_records", "person_years")]
  # tab22_counts_year[,age_band:="All"][,sex:="All"]
  
  
  # ######table 22a
  # tab22a_py<-tab22[,lapply(.SD,sum), by=c("event_definition","year"), .SDcols=c("person_years")]
  # tab22a_counts<-tab22[,-c("person_years")]
  # tab22a_counts<-merge(tab22a_counts, tab22a_py, by=c("event_definition","year"))

  
  # tab22a_counts_year_sex<-tab22a_counts[,lapply(.SD,sum), by=c("event_definition","year", "sex"), .SDcols=c("no_records")]
  # tab22a_counts_year_sex[,age_band:="All"]
  # tab22a_counts_year_sex<-merge(tab22a_counts_year_sex, tab22a_py, by=c("event_definition","year"))
  # rm(tab22a_py)
 
  
  #combine tab22
  # tab22<-rbind(tab22, tab22_counts_year_sex,tab22_counts_year)
  # rm(tab22_counts_year_sex,tab22_counts_year)
  # # #combine tab22a
  # tab22a<-rbind(tab22a_counts,tab22a_counts_year_sex)
  # rm(tab22a_counts_year_sex)
  
for (i in 1:length(diagnoses_files)){
  file.remove(paste0(diag_tmp, diagnoses_files[[i]]))
}
  
  
  tab22<-data.table(tab22, data_access_provider= data_access_provider_name, data_source=data_source_name)
  tab22_counts_year_sex<-data.table(tab22_counts_year_sex, data_access_provider= data_access_provider_name, data_source=data_source_name)
  tab22_counts_year<-data.table(tab22_counts_year, data_access_provider= data_access_provider_name, data_source=data_source_name)
  # tab22a<-data.table(tab22a, data_access_provider= data_access_provider_name, data_source=data_source_name)
  tab22[,rate_per_100_py:=round((no_records/person_years)*100,2)]
  # tab22a[,rate_per_100_py:=round((no_records/person_years)*100,2)]
  tab22_counts_year_sex[,rate_per_100_py:=round((no_records/person_years)*100,2)]
  tab22_counts_year[,rate_per_100_py:=round((no_records/person_years)*100,2)]
  
  if(subpopulations_present=="Yes"){
    write.csv(tab22, paste0(diag_dir, subpopulations_names[s], "/", subpopulations_names[s], "_diagnoses_rates_yas_recurrent.csv"), row.names = F)
    write.csv(tab22_counts_year_sex, paste0(diag_dir, subpopulations_names[s], "/", subpopulations_names[s], "_diagnoses_rates_ys_recurrent.csv"), row.names = F)
    write.csv(tab22_counts_year, paste0(diag_dir, subpopulations_names[s], "/", subpopulations_names[s], "_diagnoses_rates_y_recurrent.csv"), row.names = F)
  } else {
    write.csv(tab22, paste0(diag_dir, "diagnoses_rates_yas_recurrent.csv"), row.names = F)
    write.csv(tab22_counts_year_sex, paste0(diag_dir, "diagnoses_rates_ys_recurrent.csv"), row.names = F)
    write.csv(tab22_counts_year, paste0(diag_dir, "diagnoses_rates_y_recurrent.csv"), row.names = F)
  }
  
  # if(subpopulations_present=="Yes"){
  #   write.csv(tab22a, paste0(diag_dir, subpopulations_names[s], "/", subpopulations_names[s], "_diagnoses_rates_agg_recurrent.csv"), row.names = F)
  # } else {
  #   write.csv(tab22a, paste0(diag_dir, "diagnoses_rates_agg_recurrent.csv"), row.names = F)
  # }
  
  tab22[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
  tab22[, person_years:= as.character(person_years)][as.numeric(person_years) > 0 & as.numeric(person_years) < 5, person_years := "<5"]
  tab22[, rate_per_100_py:= as.character(rate_per_100_py)][no_records=="<5" | person_years=="<5", rate_per_100_py := "N/A"]
  
  tab22_counts_year_sex[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
  tab22_counts_year_sex[, person_years:= as.character(person_years)][as.numeric(person_years) > 0 & as.numeric(person_years) < 5, person_years := "<5"]
  tab22_counts_year_sex[, rate_per_100_py:= as.character(rate_per_100_py)][no_records=="<5" | person_years=="<5", rate_per_100_py := "N/A"]
  
  tab22_counts_year[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
  tab22_counts_year[, person_years:= as.character(person_years)][as.numeric(person_years) > 0 & as.numeric(person_years) < 5, person_years := "<5"]
  tab22_counts_year[, rate_per_100_py:= as.character(rate_per_100_py)][no_records=="<5" | person_years=="<5", rate_per_100_py := "N/A"]
  
  # tab22a[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
  # tab22a[, person_years:= as.character(person_years)][as.numeric(person_years) > 0 & as.numeric(person_years) < 5, person_years := "<5"]
  # tab22a[, rate_per_100_py:= as.character(rate_per_100_py)][no_records=="<5" | person_years=="<5", rate_per_100_py := "N/A"]
  # 
  if(subpopulations_present=="Yes"){
    write.csv(tab22, paste0(diag_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_diagnoses_rates_yas_recurrent_masked.csv"), row.names = F)
    write.csv(tab22_counts_year_sex, paste0(diag_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_diagnoses_rates_ys_recurrent_masked.csv"), row.names = F)
    write.csv(tab22_counts_year, paste0(diag_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_diagnoses_rates_y_recurrent_masked.csv"), row.names = F)
  } else {
    write.csv(tab22, paste0(diag_dir, "Masked/", "diagnoses_rates_yas_recurrent_masked.csv"), row.names = F)
    write.csv(tab22_counts_year_sex, paste0(diag_dir, "Masked/", "diagnoses_rates_ys_recurrent_masked.csv"), row.names = F)
    write.csv(tab22_counts_year, paste0(diag_dir, "Masked/", "diagnoses_rates_y_recurrent_masked.csv"), row.names = F)
  }
  # 
  # if(subpopulations_present=="Yes"){
  #   write.csv(tab22a, paste0(diag_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_diagnoses_rates_agg_recurrent_masked.csv"), row.names = F)
  # } else {
  #   write.csv(tab22a, paste0(diag_dir, "Masked/", "diagnoses_rates_agg_recurrent_masked.csv"), row.names = F)
  # }
  
  
  rm(tab22, tab22_counts_year_sex, tab22_counts_year)
  }
  
######################################################
#first event
######################################################
  print("Calculating rates of first events")
diagnoses_files<-list.files(paste0(populations_dir, "DIAGNOSES/"))
files<-list()
for (i in 1: length(diagnoses_files)){
  files<-append(files,unique(list(unlist(str_split(diagnoses_files[i],"_"))[2])))
}
files<-do.call(c,files)
#remove duplicates 
files<-files[!duplicated(files)]
#create list with names year_condition
diagnoses_list<-vector(mode="list", length=length(files))
names(diagnoses_list)<-files
rm(files)
#separate all files into the right category
for (i in 1:length(diagnoses_list)){
  diagnoses_list[[i]]<-diagnoses_files[str_detect(diagnoses_files, paste0("_", names(diagnoses_list)[i], "_"))]
}
rm(diagnoses_files)
diagnoses_files<-diagnoses_list
rm(diagnoses_list)


##################
#get all subjects with prior events
##################
prior_events_fl<-list.files(events_tmp, "_events_prior")
prior_mo_fl<-list.files(mo_tmp, "_mo_prior")
prior_so_fl<-list.files(mo_tmp, "_so_prior")  

#combine all files into one
if(length(prior_events_fl)>0){
prior_events<-lapply(paste0(events_tmp, prior_events_fl), readRDS)
prior_events<-do.call(rbind,prior_events)
prior_events<-as.data.table(prior_events)

#remove files
for(i in 1:length(prior_events_fl)){
  unlink(paste0(events_tmp,prior_events_fl[[i]]))}
}else {
prior_events<- NULL
}
if(length(prior_mo_fl)>0){
  prior_mo<-lapply(paste0(mo_tmp, prior_mo_fl), readRDS)
  prior_mo<-do.call(rbind,prior_mo)
  prior_mo<-as.data.table(prior_mo)
  
  for(i in 1:length(prior_mo_fl)){
    unlink(paste0(mo_tmp,prior_mo_fl[[i]]))
  }
  
} else {
  prior_mo<- NULL
}
if(length(prior_so_fl)>0){
  prior_so<-lapply(paste0(so_tmp, prior_so_fl), readRDS)
  prior_so<-do.call(rbind,prior_so)
  prior_so<-as.data.table(prior_so)
  
  for(i in 1:length(prior_so_fl)){
    unlink(paste0(so_tmp,prior_so_fl[[i]]))
  }
  
} else {
  prior_so<- NULL
}
 
rm(prior_events_fl,prior_mo_fl,prior_so_fl)

prior<-rbind(prior_events,prior_mo,prior_so)
prior<-as.data.table(prior)
if(!is.null(prior)){
  prior[,comb:=paste(person_id,prior,condition)]
  prior<-prior[!duplicated(comb)]
  prior[,comb:= NULL]
}

###################
#combine
###################
remove_subj<-list()

for (condition_ind in 1:length(diagnoses_files)){
  diag_file<-lapply(paste0(populations_dir,"DIAGNOSES/", diagnoses_files[[condition_ind]]), readRDS)
  diag_file<-do.call(rbind,diag_file)
  diag_file<-as.data.table(diag_file)
  
  if(!is.null(prior)){
  ids_remove<-prior[condition==names(diagnoses_files)[condition_ind],person_id]
  #remove subject who had an event in the year prior to start of follow up
  remove_subj[[condition_ind]]<-data.table(event_definition=names(diagnoses_files)[condition_ind], diag_file[person_id %in% ids_remove,.N])
  #remove all subjects that had a prior event
  diag_file<-diag_file[!(person_id %in% ids_remove)]
  } else {
    remove_subj[[condition_ind]]<-data.table(event_definition=names(diagnoses_files)[condition_ind], 0)
  }
  
  #remove event_code and vocabulary, condition will be used as event count and remove all other uneccessary columns
  diag_file[,event_code:=NULL][,event_vocabulary:=NULL][,meaning:=NULL][,age_start_follow_up:=NULL][,obs_out:=NULL][,filter:=NULL][,code_nodot:=NULL][,truncated_code:=NULL]
  
  #grab and combine person time for all people that had a diagnoses that are not part of the diagoses of interest
  py_other<-py_diag[!(person_id %in% diag_file[!duplicated(person_id),person_id])][,lapply(.SD, sum), by=c("sex", "year", "age_band"), .SDcols="person_years"]
  py_other[,no_records:=0]  
  
  
  outcomes_list<-unique(diag_file[,condition])
  #apply count person time
  output<-CountPersonTime2(Dataset_events = diag_file[,.(person_id, condition, event_date)],
                           Dataset = unique(diag_file[,.(person_id, birth_date, start_follow_up, end_follow_up, sex_at_instance_creation)]),
                           Person_id = "person_id",
                           Start_study_time =start_study_date2,
                           End_study_time =end_study_rates,
                           Start_date = "start_follow_up",
                           End_date = "end_follow_up",
                           Birth_date = "birth_date",
                           Increment = "year",
                           Unit_of_age = "year",
                           Strata = c("sex_at_instance_creation"),
                           include_remaning_ages = TRUE,
                           Aggregate = T,
                           Outcomes_nrec = outcomes_list,
                           Name_event = "condition",
                           Date_event = "event_date",
                           Rec_period = NULL,
                           Age_bands = agebands_rates,
                           print = F, 
                           check_overlap = F)
  
  output<-as.data.table(output)
  output[,Persontime:=NULL]
  names(output)<-c("sex", "year", "age_band", "person_years","no_records") 
  #Add person time from other people in the study population
  output<-rbind(output,py_other)
  output<-as.data.table(output)
  rm(py_other)
  #sum the person time
  output<-output[,lapply(.SD, sum), by=c("sex","year","age_band"), .SDcols=c("no_records","person_years")]
  output[,event_definition:=names(diagnoses_files)[condition_ind]]
  
  #Load person time for all people with no diagnosis
  no_diag_py_fl<-list.files(diag_tmp, "no_id_py.rds")
  if(length(no_diag_py_fl)>0){
    no_diag_py<-readRDS(paste0(diag_tmp, "no_id_py.rds"))
    no_diag_py<-as.data.table(no_diag_py)
    no_diag_py[,no_records:=0]
    no_diag_py[,event_definition:=names(diagnoses_files)[condition_ind]]
    #combine py
    output<-rbind(output,no_diag_py)
    output<-as.data.table(output)
    rm(no_diag_py)
  }
  rm(no_diag_py_fl)
  
  output<-output[,lapply(.SD, sum), by=c("event_definition", "sex","year","age_band"), .SDcols=c("no_records","person_years")]
  output<-output[,person_years:=round(person_years/365.25,3)]

  #will be used to count users
  saveRDS(output, paste0(diag_tmp, names(diagnoses_files)[condition_ind], "_rates_first.rds"))
  rm(output)

}
rm(diagnoses_files)

##############################
#tab23
#############################
remove_subj<-do.call(rbind,remove_subj)
if(remove_subj[,.N]>0){
  names(remove_subj)<-c("event_definition","persons_with_prior_events_removed")
  
if(subpopulations_present=="Yes"){
  write.csv(remove_subj, paste0(diag_dir, subpopulations_names[s], "/", subpopulations_names[s], "_diagnoses_remove_subj_prior_events.csv"), row.names = F)
} else {
  write.csv(remove_subj, paste0(diag_dir, "diagnoses_remove_subj_prior_events.csv"), row.names = F)
}
}


diagnoses_files<-list.files(diag_tmp, "rates_first")
tab23<-lapply(paste0(diag_tmp, diagnoses_files), readRDS)
tab23<-do.call(rbind, tab23)
tab23<-as.data.table(tab23)

#combine counts by year and sex
tab23_counts_year_sex<-tab23[,lapply(.SD,sum), by=c("event_definition","year", "sex"), .SDcols=c("no_records", "person_years")]
# tab23_counts_year_sex[,age_band:="All"]


tab23_counts_year<-tab23[,lapply(.SD,sum), by=c("event_definition","year"), .SDcols=c("no_records", "person_years")]
# tab23_counts_year[,age_band:="All"][,sex:="All"]


# ######table 23a
# tab23a_py<-tab23[,lapply(.SD,sum), by=c("event_definition","year"), .SDcols=c("person_years")]
# tab23a_counts<-tab23[,-c("person_years")]
# tab23a_counts<-merge(tab23a_counts, tab23a_py, by=c("event_definition","year"))
# 
# 
# tab23a_counts_year_sex<-tab23a_counts[,lapply(.SD,sum), by=c("event_definition","year", "sex"), .SDcols=c("no_records")]
# tab23a_counts_year_sex[,age_band:="All"]
# tab23a_counts_year_sex<-merge(tab23a_counts_year_sex, tab23a_py, by=c("event_definition","year"))
# rm(tab23a_py)
# 
# 
# #combine tab23
# tab23<-rbind(tab23, tab23_counts_year_sex,tab23_counts_year)
# rm(tab23_counts_year_sex,tab23_counts_year)
# #combine tab23a
# tab23a<-rbind(tab23a_counts,tab23a_counts_year_sex)
# rm(tab23a_counts_year_sex)

for (i in 1:length(diagnoses_files)){
  file.remove(paste0(diag_tmp, diagnoses_files[[i]]))
}


tab23<-data.table(tab23, data_access_provider= data_access_provider_name, data_source=data_source_name)
tab23_counts_year_sex<-data.table(tab23_counts_year_sex, data_access_provider= data_access_provider_name, data_source=data_source_name)
tab23_counts_year<-data.table(tab23_counts_year, data_access_provider= data_access_provider_name, data_source=data_source_name)
# tab23a<-data.table(tab23a, data_access_provider= data_access_provider_name, data_source=data_source_name)

tab23[,rate_per_100_py:=round((no_records/person_years)*100,2)]
tab23_counts_year_sex[,rate_per_100_py:=round((no_records/person_years)*100,2)]
tab23_counts_year[,rate_per_100_py:=round((no_records/person_years)*100,2)]
# tab23a[,rate_per_100_py:=round((no_records/person_years)*100,2)]

if(subpopulations_present=="Yes"){
  write.csv(tab23, paste0(diag_dir, subpopulations_names[s], "/", subpopulations_names[s], "_diagnoses_rates_yas_first.csv"), row.names = F)
  write.csv(tab23_counts_year_sex, paste0(diag_dir, subpopulations_names[s], "/", subpopulations_names[s], "_diagnoses_rates_ys_first.csv"), row.names = F)
  write.csv(tab23_counts_year, paste0(diag_dir, subpopulations_names[s], "/", subpopulations_names[s], "_diagnoses_rates_y_first.csv"), row.names = F)
} else {
  write.csv(tab23, paste0(diag_dir, "diagnoses_rates_yas_first.csv"), row.names = F)
  write.csv(tab23_counts_year_sex, paste0(diag_dir, "diagnoses_rates_ys_first.csv"), row.names = F)
  write.csv(tab23_counts_year, paste0(diag_dir, "diagnoses_rates_y_first.csv"), row.names = F)
}

# if(subpopulations_present=="Yes"){
#   write.csv(tab23a, paste0(diag_dir, subpopulations_names[s], "/", subpopulations_names[s], "_diagnoses_rates_agg_first.csv"), row.names = F)
# } else {
#   write.csv(tab23a, paste0(diag_dir, "diagnoses_rates_agg_first.csv"), row.names = F)
# }

tab23[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
tab23[, person_years:= as.character(person_years)][as.numeric(person_years) > 0 & as.numeric(person_years) < 5, person_years := "<5"]
tab23[, rate_per_100_py:= as.character(rate_per_100_py)][no_records=="<5" | person_years=="<5", rate_per_100_py := "N/A"]

tab23_counts_year_sex[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
tab23_counts_year_sex[, person_years:= as.character(person_years)][as.numeric(person_years) > 0 & as.numeric(person_years) < 5, person_years := "<5"]
tab23_counts_year_sex[, rate_per_100_py:= as.character(rate_per_100_py)][no_records=="<5" | person_years=="<5", rate_per_100_py := "N/A"]

tab23_counts_year[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
tab23_counts_year[, person_years:= as.character(person_years)][as.numeric(person_years) > 0 & as.numeric(person_years) < 5, person_years := "<5"]
tab23_counts_year[, rate_per_100_py:= as.character(rate_per_100_py)][no_records=="<5" | person_years=="<5", rate_per_100_py := "N/A"]

# tab23a[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
# tab23a[, person_years:= as.character(person_years)][as.numeric(person_years) > 0 & as.numeric(person_years) < 5, person_years := "<5"]
# tab23a[, rate_per_100_py:= as.character(rate_per_100_py)][no_records=="<5" | person_years=="<5", rate_per_100_py := "N/A"]

if(subpopulations_present=="Yes"){
  write.csv(tab23, paste0(diag_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_diagnoses_rates_yas_first_masked.csv"), row.names = F)
  write.csv(tab23_counts_year_sex, paste0(diag_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_diagnoses_rates_ys_first_masked.csv"), row.names = F)
  write.csv(tab23_counts_year, paste0(diag_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_diagnoses_rates_y_first_masked.csv"), row.names = F)
} else {
  write.csv(tab23, paste0(diag_dir, "Masked/", "diagnoses_rates_yas_first_masked.csv"), row.names = F)
  write.csv(tab23_counts_year_sex, paste0(diag_dir, "Masked/", "diagnoses_rates_ys_first_masked.csv"), row.names = F)
  write.csv(tab23_counts_year, paste0(diag_dir, "Masked/", "diagnoses_rates_y_first_masked.csv"), row.names = F)
}


# if(subpopulations_present=="Yes"){
#   write.csv(tab23a, paste0(diag_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_diagnoses_rates_agg_first_masked.csv"), row.names = F)
# } else {
#   write.csv(tab23a, paste0(diag_dir, "Masked/", "diagnoses_rates_agg_first_masked.csv"), row.names = F)
# }


rm(tab23,tab23_counts_year_sex,tab23_counts_year)

###############################
