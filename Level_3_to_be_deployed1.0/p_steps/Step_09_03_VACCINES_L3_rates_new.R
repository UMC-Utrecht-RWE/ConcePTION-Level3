########################################
#Combine counts
########################################

#Counts by year, sex and age_band
counts_yas_fl<-list.files(vaccines_tmp, "counts_rates_yas")
if(length(counts_yas_fl)>0){
  counts_yas<-lapply(paste0(vaccines_tmp, counts_yas_fl), readRDS)
  counts_yas<-as.data.table(do.call(rbind,counts_yas))
  
  for (i in 1:length(counts_yas_fl)){
    file.remove(paste0(vaccines_tmp,counts_yas_fl[i]))
  }
} else {
  counts_yas<-NULL
}


#Counts by year, sex 
counts_ys_fl<-list.files(vaccines_tmp, "counts_rates_ys")
if(length(counts_ys_fl)>0){
  counts_ys<-lapply(paste0(vaccines_tmp, counts_ys_fl), readRDS)
  counts_ys<-as.data.table(do.call(rbind,counts_ys))
  
  for (i in 1:length(counts_ys_fl)){
    file.remove(paste0(vaccines_tmp,counts_ys_fl[i]))
  }
} else {
  counts_ys<-NULL
}



#Calculate person time for the study population by year, sex and age band
#split the study populationin chunks of 100000

#Create chunks of 200.000
size<-200000
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
  print(paste0("Analysing chunk ", min,":",max," of the study population." ))
  output<-CountPersonTime2(Dataset = study_population[min:max,.(person_id, birth_date, start_follow_up,end_follow_up, sex_at_instance_creation)],
                           Person_id = "person_id",
                           Start_study_time =start_study_date2,
                           End_study_time = end_study_rates,
                           Start_date = "start_follow_up",
                           End_date = "end_follow_up",
                           Birth_date = "birth_date",
                           Increment = "year",
                           Unit_of_age = "year",
                           Strata = c("sex_at_instance_creation"),
                           include_remaning_ages = TRUE,
                           Aggregate = F,
                           Age_bands = agebands_rates,
                           print = F, 
                           check_overlap = F)
  
  #trasform from days into person-years
  output<-output[,Persontime:=round(Persontime/365.25,3)]
  names(output)<-c("person_id","sex","age_band","year","person_years")
  no_subjects_yas<-output[,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("sex","year","age_band"), .SDcols="person_id"]
  saveRDS(no_subjects_yas,paste0(vaccines_tmp, size_ind,"subjects_yas.rds"))
  rm(no_subjects_yas)
  
  no_subjects_ys<-output[,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("sex","year"), .SDcols="person_id"]
  saveRDS(no_subjects_ys,paste0(vaccines_tmp, size_ind,"subjects_ys.rds"))
  rm(no_subjects_ys)
  
  # no_subjects_s<-output[,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("sex"), .SDcols="person_id"]
  # saveRDS(no_subjects_s,paste0(vaccines_tmp, size_ind,"subjects_s.rds"))
  # rm(no_subjects_s)
  # 
  output<-output[,lapply(.SD, sum), by=c("sex", "year","age_band"), .SDcols="person_years"]
  
  #results will be used only for py
  
  saveRDS(output,paste0(vaccines_tmp, size_ind,"py_yas.rds"))
  
  py_ys<-output[,lapply(.SD, sum), by=c("sex", "year"), .SDcols="person_years"]
  saveRDS(py_ys,paste0(vaccines_tmp, size_ind,"py_ys.rds"))
  rm(py_ys)
  
  # py_s<-output[,lapply(.SD, sum), by=c("sex"), .SDcols="person_years"]
  # saveRDS(py_s,paste0(vaccines_tmp, size_ind,"py_s.rds"))
  # rm(py_s)
  
  rm(output)
  
}

#Combine results for number of subjects and person time
#########
#subjects:sex, year, age_band
#########
#Number of subjects by year, age band and sex
subj_yas_fl<-list.files(vaccines_tmp, "subjects_yas.rds")
if(length(subj_yas_fl)>0){
  subj_yas<-lapply(paste0(vaccines_tmp,subj_yas_fl), readRDS)
  subj_yas<-do.call(rbind,subj_yas)
  subj_yas<-subj_yas[,lapply(.SD,sum), by=c("sex","year","age_band"), .SDcols="person_id"]
  setnames(subj_yas, "person_id", "no_subjects")
  
  #delete files
  for (i in 1:length(subj_yas_fl)){
    file.remove(paste0(vaccines_tmp, subj_yas_fl[i]))
  }
} else{
  subj_yas<-NULL
}
rm(subj_yas_fl)

#Person time by year, age band and sex
py_yas_fl<-list.files(vaccines_tmp, "py_yas.rds")
if(length(py_yas_fl)>0){
  py_yas<-lapply(paste0(vaccines_tmp,py_yas_fl), readRDS)
  py_yas<-do.call(rbind,py_yas)
  py_yas<-py_yas[,lapply(.SD,sum), by=c("sex","year","age_band"), .SDcols="person_years"]
  
  #delete files
  for (i in 1:length(py_yas_fl)){
    file.remove(paste0(vaccines_tmp, py_yas_fl[i]))
  }
} else {
  py_yas<-NULL
}
rm(py_yas_fl)
#Combine results
if(!is.null(subj_yas) & !is.null(py_yas)){
  rates_yas<-as.data.table(merge(subj_yas,py_yas, by=c("sex","year","age_band"), all=T))
  rates_yas[,subjects_per_100_py:=round((no_subjects/person_years)*100,0)]
  saveRDS(rates_yas,paste0(vaccines_tmp,"subjects_rates_yas.rds"))
  rm(rates_yas)
}
rm(subj_yas,py_yas)

#########
#subjects:sex, year
#########
#Number of subjects by year, and sex
subj_ys_fl<-list.files(vaccines_tmp, "subjects_ys.rds")
if(length(subj_ys_fl)>0){
  subj_ys<-lapply(paste0(vaccines_tmp,subj_ys_fl), readRDS)
  subj_ys<-do.call(rbind,subj_ys)
  subj_ys<-subj_ys[,lapply(.SD,sum), by=c("sex","year"), .SDcols="person_id"]
  setnames(subj_ys, "person_id", "no_subjects")
  
  #delete files
  for (i in 1:length(subj_ys_fl)){
    file.remove(paste0(vaccines_tmp, subj_ys_fl[i]))
  }
} else {
  subj_ys<-NULL  
}
rm(subj_ys_fl)

#Person time by year,and sex
py_ys_fl<-list.files(vaccines_tmp, "py_ys.rds")
if(length(py_ys_fl)>0){
  py_ys<-lapply(paste0(vaccines_tmp,py_ys_fl), readRDS)
  py_ys<-do.call(rbind,py_ys)
  py_ys<-py_ys[,lapply(.SD,sum), by=c("sex","year"), .SDcols="person_years"]
  
  #delete files
  for (i in 1:length(py_ys_fl)){
    file.remove(paste0(vaccines_tmp, py_ys_fl[i]))
  }
} else {
  py_ys<-NULL
}
rm(py_ys_fl)

#Combine results
if(!is.null(subj_ys) & !is.null(py_ys)){
  rates_ys<-as.data.table(merge(subj_ys,py_ys, by=c("sex","year"), all=T))
  rates_ys[,subjects_per_100_py:=round((no_subjects/person_years)*100,0)]
  saveRDS(rates_ys,paste0(vaccines_tmp,"subjects_rates_ys.rds"))
  rm(rates_ys)
} 
rm(subj_ys, py_ys)


# #########
# #subjects:sex
# #########
# #Number of subjects by year, and sex
# subj_s_fl<-list.files(vaccines_tmp, "subjects_s.rds")
# if(length(subj_s_fl)>0){
#   subj_s<-lapply(paste0(vaccines_tmp,subj_s_fl), readRDS)
#   subj_s<-do.call(rbind,subj_s)
#   subj_s<-subj_s[,lapply(.SD,sum), by=c("sex"), .SDcols="person_id"]
#   setnames(subj_s, "person_id", "no_subjects")
#   
#   #delete files
#   for (i in 1:length(subj_s_fl)){
#     file.remove(paste0(vaccines_tmp, subj_s_fl[i]))
#   }
# } else {
#   subj_s<-NULL  
# }
# rm(subj_s_fl)

# #Person time by year,and sex
# py_s_fl<-list.files(vaccines_tmp, "py_s.rds")
# if(length(py_s_fl)>0){
#   py_s<-lapply(paste0(vaccines_tmp,py_s_fl), readRDS)
#   py_s<-do.call(rbind,py_s)
#   py_s<-py_s[,lapply(.SD,sum), by=c("sex"), .SDcols="person_years"]
#   
#   #delete files
#   for (i in 1:length(py_s_fl)){
#     file.remove(paste0(vaccines_tmp, py_s_fl[i]))
#   }
# } else {
#   py_s<-NULL
# }
# rm(py_s_fl)
# 
# #Combine results
# if(!is.null(subj_s) & !is.null(py_s)){
#   rates_s<-as.data.table(merge(subj_s,py_s, by=c("sex"), all=T))
#   rates_s[,subjects_per_100_py:=round((no_subjects/person_years)*100,0)]
#   saveRDS(rates_s,paste0(vaccines_tmp,"subjects_rates_s.rds"))
#   rm(rates_s)
# } 
# rm(subj_s, py_s)

#############################################################################################
#Combine counts and person time, calculate rates of vaccine use
#############################################################################
rm(study_population)
subjects_yas_fl<-list.files(vaccines_tmp,"subjects_rates_yas")
if(length(subjects_yas_fl)>0){
  subjects_yas<-lapply(paste0(vaccines_tmp, subjects_yas_fl), readRDS)
  subjects_yas<-as.data.table(do.call(rbind,subjects_yas))
  
  for (i in 1:length(subjects_yas_fl)){
    file.remove(paste0(vaccines_tmp,subjects_yas_fl[i]))
  }
} else {
  subjects_yas<-NULL
}
rm(subjects_yas_fl)
#rates by year, sex and ageband
if(!is.null(counts_yas) & !is.null(subjects_yas)){
  counts_yas[,year:=as.numeric(year)]
  subjects_yas[,year:=as.numeric(year)]
  rates_yas<-as.data.table(merge(counts_yas,subjects_yas, by=c("sex", "year","age_band"), all=T))
  #If there are empty person-years remove those rows
  rates_yas<-rates_yas[!is.na(person_years)]
  rates_yas[is.na(truncated_atc_code), truncated_atc_code:="N/A"]
  rates_yas[is.na(no_records), no_records:=0]
  rates_yas[is.na(no_users), no_users:=0]
  
  rates_yas[,vaccines_per_100_py:=round((no_records/person_years)*100,2)]
  rates_yas[,users_per_100_py:=round((no_users/person_years)*100,0)]
  rates_yas[,person_years:=round(person_years,3)]
  
  
  if(subpopulations_present=="Yes"){
    fwrite(rates_yas, paste0(vacc_dir, subpopulations_names[s], "/", subpopulations_names[s], "_vaccines_rates_year_age_atc.csv"), row.names = F)
  } else {
    fwrite(rates_yas, paste0(vacc_dir, "vaccines_rates_year_age_atc.csv"), row.names = F)
  }
  
  ################
  #Apply masking
  ################
  
  rates_yas[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
  rates_yas[, no_users:= as.character(no_users)][as.numeric(no_users) > 0 & as.numeric(no_users) < 5, no_users := "<5"]
  rates_yas[, no_subjects:= as.character(no_subjects)][as.numeric(no_subjects) > 0 & as.numeric(no_subjects) < 5, no_subjects := "<5"]
  rates_yas[, person_years:= as.character(person_years)][as.numeric(person_years) > 0 & as.numeric(person_years) < 5, person_years := "<5"]
  rates_yas[, vaccines_per_100_py:= as.character(vaccines_per_100_py)][no_records=="<5" | person_years=="<5", vaccines_per_100_py := "N/A"]
  rates_yas[, users_per_100_py:= as.character(users_per_100_py)][no_users=="<5" | person_years=="<5", users_per_100_py := "N/A"]
  rates_yas[, subjects_per_100_py:= as.character(subjects_per_100_py)][no_subjects=="<5" | person_years=="<5", subjects_per_100_py := "N/A"]
  
  if(subpopulations_present=="Yes"){
    fwrite(rates_yas, paste0(vacc_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_vaccines_rates_year_age_atc_masked.csv"), row.names = F)
  } else {
    fwrite(rates_yas, paste0(vacc_dir, "Masked/", "vaccines_rates_year_age_atc_masked.csv"), row.names = F)
  }
  
  rm(rates_yas) 
  
  
}


#results by year and sex
subjects_ys_fl<-list.files(vaccines_tmp,"subjects_rates_ys")
if(length(subjects_ys_fl)>0){
  subjects_ys<-lapply(paste0(vaccines_tmp, subjects_ys_fl), readRDS)
  subjects_ys<-as.data.table(do.call(rbind,subjects_ys))
  
  for (i in 1:length(subjects_ys_fl)){
    file.remove(paste0(vaccines_tmp,subjects_ys_fl[i]))
  }
} else {
  subjects_ys<-NULL
}
rm(subjects_ys_fl)
#rates by year, sex and ageband
if(!is.null(counts_ys) & !is.null(subjects_ys)){
  counts_ys[,year:=as.numeric(year)]
  subjects_ys[,year:=as.numeric(year)]
  rates_ys<-as.data.table(merge(counts_ys,subjects_ys, by=c("sex", "year"), all=T))
  rates_ys[is.na(truncated_atc_code), truncated_atc_code:="N/A"]
  rates_ys[is.na(no_records), no_records:=0]
  rates_ys[is.na(no_users), no_users:=0]
  
  rates_ys[,vaccines_per_100_py:=round((no_records/person_years)*100,2)]
  rates_ys[,users_per_100_py:=round((no_users/person_years)*100,0)]
  rates_ys[,person_years:=round(person_years,3)]
  
  
  if(subpopulations_present=="Yes"){
    fwrite(rates_ys, paste0(vacc_dir, subpopulations_names[s], "/", subpopulations_names[s], "_vaccines_rates_year_atc.csv"), row.names = F)
  } else {
    fwrite(rates_ys, paste0(vacc_dir, "vaccines_rates_year_atc.csv"), row.names = F)
  }
  
  ################
  #Apply masking
  ################
  
  rates_ys[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
  rates_ys[, no_users:= as.character(no_users)][as.numeric(no_users) > 0 & as.numeric(no_users) < 5, no_users := "<5"]
  rates_ys[, no_subjects:= as.character(no_subjects)][as.numeric(no_subjects) > 0 & as.numeric(no_subjects) < 5, no_subjects := "<5"]
  rates_ys[, person_years:= as.character(person_years)][as.numeric(person_years) > 0 & as.numeric(person_years) < 5, person_years := "<5"]
  rates_ys[, vaccines_per_100_py:= as.character(vaccines_per_100_py)][no_records=="<5" | person_years=="<5", vaccines_per_100_py := "N/A"]
  rates_ys[, users_per_100_py:= as.character(users_per_100_py)][no_users=="<5" | person_years=="<5", users_per_100_py := "N/A"]
  rates_ys[, subjects_per_100_py:= as.character(subjects_per_100_py)][no_subjects=="<5" | person_years=="<5", subjects_per_100_py := "N/A"]
  
  if(subpopulations_present=="Yes"){
    fwrite(rates_ys, paste0(vacc_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_vaccines_rates_year_atc_masked.csv"), row.names = F)
  } else {
    fwrite(rates_ys, paste0(vacc_dir, "Masked/", "vaccines_rates_year_atc_masked.csv"), row.names = F)
  }
  
  rm(rates_ys) 
  
  
}
