
#Author: Vjola Hoxhaj Drs.
#email: v.hoxhaj@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/07/2021

###############################################################################################
#EVENTS_PREGNANCY    #MEDICINES_PREGNANCY     #VACCINES_PREGNANCY
#############################################################################################
if(length(pregnancy_files)>0){
for (preg_ind in 1:length(pregnancy_files)){
  if (subpopulations_present=="Yes"){
    pregnancy_pop<-readRDS(paste0(preg_pop, subpopulations_names[s],"/", pregnancy_files[[preg_ind]]))
    pregnancy_pop<-pregnancy_pop[,c("person_id","birth_date", "end_follow_up","start_follow_up","age_start_follow_up","condition", "pregnancy_code_date")]
  } else {
    pregnancy_pop<-readRDS(paste0(preg_pop,pregnancy_files[[preg_ind]]))
    pregnancy_pop<-pregnancy_pop[,c("person_id","birth_date", "end_follow_up","start_follow_up","age_start_follow_up","condition", "pregnancy_code_date")]
  }
  setnames(pregnancy_pop, "condition", "stage_of_pregnancy")
  #apply lag time
  time_lag<-data.table(stage_of_pregnancy=c("start_of_pregnancy", "end_of_pregnancy", "ongoing_pregnancy", "interruption_pregnancy"),time_lag=c(6*7,23*7,23*7, 8*7))
  pregnancy_pop[,lag:=time_lag[stage_of_pregnancy == pregnancy_pop[!duplicated(stage_of_pregnancy),stage_of_pregnancy],time_lag]] #create lag variable based on condition
  #Step 1: Order the dataset by person_id, stage_of_pregnancy and date of event
  pregnancy_pop<-pregnancy_pop[order(person_id,stage_of_pregnancy,pregnancy_code_date)]
  #Step 2: Create date_2(by shifting the first date)
  pregnancy_pop[,date_1:=shift(pregnancy_code_date)]
  pregnancy_pop[,date_2:=pregnancy_code_date]
  #Step 3: Create rowid(whhcih will give the number of rows for each person)
  pregnancy_pop[,rowid:=rowid(person_id)]
  #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
  pregnancy_pop[rowid==1,date_1:=NA]
  #Step 5: Create date_dif as difference between date 2 and date
  pregnancy_pop[,date_dif:=date_2-date_1]
  #Step 6: Remove these rows 
  pregnancy_pop<-pregnancy_pop[date_dif>lag | is.na(date_dif)]
  #Step 7: Repeat until there are no more impossible dates in the dataset
  pregnancy_pop[,date_dif:=date_2-date_1]
  while(pregnancy_pop[date_dif<=lag,.N]>0){
    pregnancy_pop<-pregnancy_pop[date_dif>lag | is.na(date_dif)]
    pregnancy_pop[,date_dif:=date_2-date_1]
  }
  pregnancy_pop[,lag:=NULL][,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
  
  pregnancy_pop[,year:=year(pregnancy_code_date)]
  pregnancy_pop[,comb:=paste(person_id,year, sep="_")]
  pregnancy_pop<-pregnancy_pop[!duplicated(comb)]
  pregnancy_pop[,comb:=NULL]

  setkey(pregnancy_pop, person_id, year,birth_date,start_follow_up,age_start_follow_up, end_follow_up)
  preg_stage<-pregnancy_pop[!duplicated(stage_of_pregnancy),stage_of_pregnancy]
  
  #####################################################
  #PREGNANCY_EVENTS
  #####################################################
  if (length(conditions_files_chronic)>0){
  no_women<-list()
    w<-1
  for (j in 1: length(conditions_files_chronic)){
    #load file
    diagnoses_df<-readRDS(paste0(poi_tmp, conditions_files_chronic[[j]]))
    diagnoses_df[,year:=NULL]
    diagnoses_df[,year:=unlist(str_split(names(conditions_files_chronic)[j],"_"))[1]][,year:=as.numeric(year)]
    setkey(diagnoses_df,person_id,year,birth_date,start_follow_up,age_start_follow_up, end_follow_up)
    #merge databases: left join
    diagnoses_df[,person_id:=as.character(person_id)][,year:=as.character(year)]
    pregnancy_pop[,person_id:=as.character(person_id)][,year:=as.character(year)]
    diagnoses_df<-merge(diagnoses_df,pregnancy_pop,all.x=T)
    
    if(diagnoses_df[!is.na(pregnancy_code_date),.N]>0){
    if (subpopulations_present=="Yes"){
      saveRDS(diagnoses_df[!is.na(pregnancy_code_date)], paste0(ev_preg_pop, subpopulations_names[s],"/", subpopulations_names[s],"_",diagnoses_df[!duplicated(year),"year"],"_",diagnoses_df[!duplicated(condition),"condition"],  "_", preg_stage, ".rds"))
    } else {
      saveRDS(diagnoses_df[!is.na(pregnancy_code_date)], paste0(ev_preg_pop, diagnoses_df[!duplicated(year),"year"],"_",diagnoses_df[!duplicated(condition),"condition"], "_", preg_stage, ".rds"))
    }
    }
    
    no_women_sub<-diagnoses_df[,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("year", "condition", "truncated_code", "event_vocabulary"),.SDcols="person_id"]
    if (no_women_sub[,.N]>0){
      setnames(no_women_sub, "person_id", "no_women")
      no_pregnant_women<-diagnoses_df[!is.na(stage_of_pregnancy), lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("year","stage_of_pregnancy"),.SDcols="person_id"]
      setnames(no_pregnant_women,"person_id", "no_pregnant_women")
      no_women_sub[,year:as.character(year)]
      no_pregnant_women[,year:=as.character(year)]
      no_women_sub<-merge(no_women_sub,no_pregnant_women, by="year", all=T)
    } else {
      no_women_sub<-NULL
    }
  no_women[[w]]<-no_women_sub
  rm(no_women_sub,diagnoses_df)
    w<-w+1
    }   
  no_women<-do.call(rbind,no_women)
  no_women[,stage_of_pregnancy:=preg_stage]
  no_women[is.na(no_pregnant_women),no_pregnant_women:=0]
  
  if(!is.null(no_women)){
    saveRDS(no_women, paste0(poi_tmp, "poi_res1_ev_preg_",preg_stage, ".rds"))
  }
    rm(no_women)
  }

 
#####################################################
#PREGNANCY_MEDICINES
#####################################################
if(length(medicines_files)>0){
  w<-1
for (j in 1: length(medicines_files)){
  if (subpopulations_present=="Yes"){
    medicines<-lapply(paste0(medicines_pop, subpopulations_names[s],"/",medicines_files[[j]]), readRDS) #combine all files for one pregnancy stage
  } else {
    medicines<-lapply(paste0(medicines_pop, medicines_files[[j]]), readRDS) #combine all files for one pregnancy stage
  }
  medicines<-do.call(rbind,medicines)
  
  medicines<-medicines[age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,c("person_id","year", "medicinal_product_atc_code","age_start_follow_up")]

  #merge databases
  pregnancy_pop[,person_id:=as.character(person_id)][,year:=as.character(year)][,age_start_follow_up:=as.numeric(age_start_follow_up)]
  medicines[,person_id:=as.character(person_id)][,year:=as.character(year)][,age_start_follow_up:=as.numeric(age_start_follow_up)]
  medicines<-merge(pregnancy_pop,medicines, by=c("person_id","year", "age_start_follow_up"), all=F) #inner join
 
  if(medicines[!is.na(medicinal_product_atc_code),.N]>0){
   if (subpopulations_present=="Yes"){
    saveRDS(medicines[!is.na(medicinal_product_atc_code)], paste0(med_preg_pop, subpopulations_names[s],"/", subpopulations_names[s], "_", names(medicines_files)[j], "_",preg_stage, ".rds" ))
  } else {
    saveRDS(medicines[!is.na(medicinal_product_atc_code)], paste0(med_preg_pop, names(medicines_files)[j], "_",preg_stage, ".rds" ))
  } 
  }
  
  if(medicines[,.N]>0){
    
    no_records<-medicines[, .N, by=c("year", "medicinal_product_atc_code","stage_of_pregnancy")]
    setnames(no_records,"N","no_records") #total number of records, one women multiple prescriptios is possible
    no_women<-medicines[,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("year", "medicinal_product_atc_code","stage_of_pregnancy"),.SDcols="person_id"]
    setnames(no_women,"person_id","no_women")  #number of pregnant women
    no_total_women<-medicines[,lapply(.SD, function(x) length(unique(na.omit(x)))), by="year",.SDcols="person_id"]
    setnames(no_total_women,"person_id","no_total_women")
    no_records[,year:=as.character(year)][,medicinal_product_atc_code:=as.character(medicinal_product_atc_code)][,stage_of_pregnancy:=as.character(stage_of_pregnancy)]
    no_women[,year:=as.character(year)][,medicinal_product_atc_code:=as.character(medicinal_product_atc_code)][,stage_of_pregnancy:=as.character(stage_of_pregnancy)]
    no_records<-merge(no_records,no_women,by=c("year", "medicinal_product_atc_code","stage_of_pregnancy"))
    rm(no_women)
    no_records[,year:=as.character(year)]
    no_total_women[,year:=as.character(year)]
    no_records<-merge(no_records,no_total_women,by="year") #number of pregannt women by year
    rm(no_total_women)

    if(no_records[,.N]>0){
      saveRDS(no_records, paste0(poi_tmp, "poi_res2_med_preg_", preg_stage, ".rds" ))
    }
      rm(no_records)

  }
  
  rm(medicines)
  w<-w+1
}
}
  
#####################################################
#PREGNANCY_VACCINES
#####################################################
if(length(vaccines_files)>0){
  w<-1
  for (j in 1: length(vaccines_files)){
    if (subpopulations_present=="Yes"){
      vaccines<-lapply(paste0(vaccines_pop, subpopulations_names[s],"/",vaccines_files[[j]]), readRDS) #combine all files for one pregnancy stage
    } else {
      vaccines<-lapply(paste0(vaccines_pop, vaccines_files[[j]]), readRDS) #combine all files for one pregnancy stage
    }
    vaccines<-do.call(rbind,vaccines)
    
    vaccines<-vaccines[age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,c("person_id","year", "vx_atc","age_start_follow_up")]
    
    #merge databases
    pregnancy_pop[,person_id:=as.character(person_id)][,year:=as.character(year)][,age_start_follow_up:=as.numeric(age_start_follow_up)]
    vaccines[,person_id:=as.character(person_id)][,year:=as.character(year)][,age_start_follow_up:=as.numeric(age_start_follow_up)]
    vaccines<-merge(pregnancy_pop,vaccines, by=c("person_id","year", "age_start_follow_up"), all=F) #inner join
    
    if (vaccines[!is.na(vx_atc),.N]>0){
    if (subpopulations_present=="Yes"){
      saveRDS(vaccines[!is.na(vx_atc)], paste0(vacc_preg_pop, subpopulations_names[s],"/", subpopulations_names[s], "_", names(vaccines_files)[j], "_",preg_stage, ".rds" ))
    } else {
      saveRDS(vaccines[!is.na(vx_atc)], paste0(vacc_preg_pop, names(vaccines_files)[j], "_",preg_stage, ".rds" ))
    }
    }
    if(vaccines[,.N]>0){
      
      no_records<-vaccines[, .N, by=c("year", "vx_atc","stage_of_pregnancy")]
      setnames(no_records,"N","no_records")
      no_women<-vaccines[,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("year", "vx_atc","stage_of_pregnancy"),.SDcols="person_id"]
      setnames(no_women,"person_id","no_women")
      no_total_women<-vaccines[,lapply(.SD, function(x) length(unique(na.omit(x)))), by="year",.SDcols="person_id"]
      setnames(no_total_women,"person_id","no_total_women")
      no_records[,year:=as.character(year)][,vx_atc:=as.character(vx_atc)][,stage_of_pregnancy:=as.character(stage_of_pregnancy)]
      no_women[,year:=as.character(year)][,vx_atc:=as.character(vx_atc)][,stage_of_pregnancy:=as.character(stage_of_pregnancy)]
      no_records<-merge(no_records,no_women,by=c("year", "vx_atc","stage_of_pregnancy"))
      rm(no_women)
      no_records[,year:=as.character(year)]
      no_total_women[,year:=as.character(year)]
      no_records<-merge(no_records,no_total_women,by="year")
      rm(no_total_women)
      
      if(no_records[,.N]>0){
        saveRDS(no_records, paste0(poi_tmp, "poi_res3_vacc_preg_",preg_stage, ".rds" ))
      }
        rm(no_records)
    }
    
    rm(vaccines)
    w<-w+1
  }
}
}
}

###############################################################################################
#EVENTS_MEDICINES    #EVENTS_VACCINES
#############################################################################################
if(length(conditions_files_chronic)>0){
for (chr_fl_ind in 1: length(conditions_files_chronic)){
  #load file
  diagnoses_df<-readRDS(paste0(poi_tmp, conditions_files_chronic[[chr_fl_ind]]))
  diagnoses_df[,year:=NULL]
  diagnoses_df[,year:=unlist(str_split(names(conditions_files_chronic)[chr_fl_ind],"_"))[1]][,year:=as.numeric(year)]
  setkey(diagnoses_df,person_id,year,age_start_follow_up)
  
  ###############################################
  #EVENTS_MEDICINES
  ################################################
  if(length(medicines_files)>0){
  no_records<-list()
  w<-1
  for (l in 1: length(medicines_files)){
    if (subpopulations_present=="Yes"){
      medicines<-lapply(paste0(medicines_pop, subpopulations_names[s],"/",medicines_files[[l]]), readRDS) #combine all files for one pregnancy stage
    } else {
      medicines<-lapply(paste0(medicines_pop,medicines_files[[l]]), readRDS) #combine all files for one pregnancy stage
    }
    medicines<-do.call(rbind,medicines)
    
    medicines<-medicines[age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,c("person_id","year", "medicinal_product_atc_code","age_start_follow_up")]
    
    #merge databases
    diagnoses_df[,person_id:=as.character(person_id)][,year:=as.character(year)][,age_start_follow_up:=as.numeric(age_start_follow_up)]
    medicines[,person_id:=as.character(person_id)][,year:=as.character(year)][,age_start_follow_up:=as.numeric(age_start_follow_up)]
    medicines<-merge(diagnoses_df,medicines, by=c("person_id","year", "age_start_follow_up"), all=F) #inner join
    
    if(medicines[!is.na(medicinal_product_atc_code),.N]>0){
    if (subpopulations_present=="Yes"){
      saveRDS(medicines[!is.na(medicinal_product_atc_code)], paste0(ev_med_pop, subpopulations_names[s],"/", subpopulations_names[s], "_",names(conditions_files_chronic)[chr_fl_ind],"_", unlist(str_split(names(medicines_files)[l],"_"))[1],  ".rds" ))
    } else {
      saveRDS(medicines[!is.na(medicinal_product_atc_code)], paste0(ev_med_pop, names(conditions_files_chronic)[chr_fl_ind],"_", unlist(str_split(names(medicines_files)[l],"_"))[1], ".rds" ))
    }
    } 
    
    if(medicines[,.N]>0){
      no_records_sub<-medicines[, .N, by=c("year", "medicinal_product_atc_code","condition")]
      setnames(no_records_sub,"N","no_records") #total number of records, one women multiple prescriptions is possible
      no_women<-medicines[,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("year", "medicinal_product_atc_code","condition"),.SDcols="person_id"]
      setnames(no_women,"person_id","no_women")  #number of  women with condition
      no_total_women<-medicines[,lapply(.SD, function(x) length(unique(na.omit(x)))), by="year",.SDcols="person_id"]
      setnames(no_total_women,"person_id","no_total_women")
      no_records_sub[,year:=as.character(year)][,medicinal_product_atc_code:=as.character(medicinal_product_atc_code)][,condition:=as.character(condition)]
      no_women[,year:=as.character(year)][,medicinal_product_atc_code:=as.character(medicinal_product_atc_code)][,condition:=as.character(condition)]
      no_records_sub<-merge(no_records_sub,no_women,by=c("year", "medicinal_product_atc_code","condition"))
      rm(no_women)
      no_records_sub[,year:=as.character(year)]
      no_total_women[,year:=as.character(year)]
      no_records_sub<-merge(no_records_sub,no_total_women,by="year") #number of pregannt women by year
      rm(no_total_women)
    } else {
      no_records_sub<-NULL
    }
   if(!is.null(no_records_sub)){
     no_records_sub[,condition:=unlist(str_split(names(conditions_files_chronic)[chr_fl_ind], "_"))[2]]
     no_records_sub[is.na(no_records), no_records:=0]
   }
    
    no_records[[w]]<-no_records_sub
    
    rm(medicines,no_records_sub)
    w<-w+1
  }
  
  no_records<-do.call(rbind,no_records)

  if(!is.null(no_records)){
  saveRDS(no_records, paste0(poi_tmp, "poi_res4_ev_med_", names(conditions_files_chronic)[chr_fl_ind], ".rds" ))
  }
    rm(no_records)
  }
  ###############################################
  #EVENTS_VACCINES
  ###############################################
  if(length(vaccines_files)>0){
  no_records<-list()
  w<-1
  for (l in 1: length(vaccines_files)){
    if (subpopulations_present=="Yes"){
      vaccines<-lapply(paste0(vaccines_pop, subpopulations_names[s],"/",vaccines_files[[l]]), readRDS) #combine all files for one pregnancy stage
    } else {
      vaccines<-lapply(paste0(vaccines_pop, vaccines_files[[l]]), readRDS) #combine all files for one pregnancy stage
    }
    vaccines<-do.call(rbind,vaccines)
    
    vaccines<-vaccines[age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,c("person_id","year", "vx_atc","age_start_follow_up")]
    
    #merge databases
    diagnoses_df[,person_id:=as.character(person_id)][,year:=as.character(year)][,age_start_follow_up:=as.numeric(age_start_follow_up)]
    vaccines[,person_id:=as.character(person_id)][,year:=as.character(year)][,age_start_follow_up:=as.numeric(age_start_follow_up)]
    vaccines<-merge(diagnoses_df,vaccines, by=c("person_id","year", "age_start_follow_up"), all=F) #inner join
    
    if(vaccines[!is.na(vx_atc),.N]>0){
      if (subpopulations_present=="Yes"){
        saveRDS(vaccines[!is.na(vx_atc)], paste0(ev_vacc_pop, subpopulations_names[s],"/", subpopulations_names[s], "_",names(conditions_files_chronic)[chr_fl_ind],"_", unlist(str_split(names(vaccines_files)[l],"_"))[1],  ".rds" ))
      } else {
        saveRDS(vaccines[!is.na(vx_atc)], paste0(ev_vacc_pop, names(conditions_files_chronic)[chr_fl_ind],"_", unlist(str_split(names(vaccines_files)[l],"_"))[1], ".rds" ))
      }
    }
    if(vaccines[,.N]>0){
      no_records_sub<-vaccines[, .N, by=c("year", "vx_atc","condition")]
      setnames(no_records_sub,"N","no_records") #total number of records, one women multiple prescriptions is possible
      no_women<-vaccines[,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("year", "vx_atc","condition"),.SDcols="person_id"]
      setnames(no_women,"person_id","no_women")  #number of  women with condition
      no_total_women<-vaccines[,lapply(.SD, function(x) length(unique(na.omit(x)))), by="year",.SDcols="person_id"]
      setnames(no_total_women,"person_id","no_total_women")
      no_records_sub[,year:=as.character(year)][,vx_atc:=as.character(vx_atc)][,condition:=as.character(condition)]
      no_women[,year:=as.character(year)][,vx_atc:=as.character(vx_atc)][,condition:=as.character(condition)]
      no_records_sub<-merge(no_records_sub,no_women,by=c("year", "vx_atc","condition"))
      rm(no_women)
      no_records_sub[,year:=as.character(year)]
      no_total_women[,year:=as.character(year)]
      no_records_sub<-merge(no_records_sub,no_total_women,by="year") #number of pregannt women by year
      rm(no_total_women)
    } else {
      no_records_sub<-NULL
    }
    if(!is.null(no_records_sub)){
      no_records_sub[,condition:=unlist(str_split(names(conditions_files_chronic)[chr_fl_ind], "_"))[2]]
      no_records_sub[is.na(no_records), no_records:=0]
    }
    
    no_records[[w]]<-no_records_sub
    
    rm(vaccines,no_records_sub)
    w<-w+1
  }
  
  no_records<-do.call(rbind,no_records)
  if(!is.null(no_records)){
  saveRDS(no_records, paste0(poi_tmp, "poi_res5_ev_vacc_", names(conditions_files_chronic)[chr_fl_ind], ".rds" ))
  }
    rm(no_records)
  
  }
}  
}
###############################################################################################
#EVENTS_MEDICINES_PREGNANCY    #EVENTS_VACCINES_PREGNANCY
###############################################################################################
if(subpopulations_present=="Yes"){
events_medicines_files<-list.files(paste0(ev_med_pop, subpopulations_names[s]))
events_pregnancy_files<-list.files(paste0(ev_preg_pop, subpopulations_names[s]))
events_vaccines_files<-list.files(paste0(ev_vacc_pop, subpopulations_names[s]))
} else {
  events_medicines_files<-list.files(ev_med_pop)
  events_pregnancy_files<-list.files(ev_preg_pop)  
  events_vaccines_files<-list.files(ev_vacc_pop)
}

if (length(events_pregnancy_files)>0){
for (ev_preg_fl_ind in 1 :length(events_pregnancy_files)){
  
  if (subpopulations_present=="Yes"){
    pregnancy_event<-readRDS(paste0(ev_preg_pop, subpopulations_names[s],"/", events_pregnancy_files[ev_preg_fl_ind]))
   } else {
    pregnancy_event<-readRDS(paste0(ev_preg_pop,events_pregnancy_files[ev_preg_fl_ind]))
   }
setkey(pregnancy_event, person_id, year, birth_date,age_start_follow_up, start_follow_up, end_follow_up, condition, truncated_code, event_vocabulary, event_date)
  
############################################
#EVENTS_PREGNANCY_MEDICINES
############################################
if (length(events_medicines_files)>0){  
no_records<-list()
w<-1
  for (ev_med_fl_ind in 1: length(events_medicines_files)){
  if (subpopulations_present=="Yes"){
   medicines_event<-readRDS(paste0(ev_med_pop, subpopulations_names[s],"/", events_medicines_files[ev_med_fl_ind]))
  } else {
    medicines_event<-readRDS(paste0(ev_med_pop,events_medicines_files[ev_med_fl_ind]))
  }
  setkey(medicines_event, person_id, year, birth_date,age_start_follow_up, start_follow_up, end_follow_up, condition, truncated_code, event_vocabulary, event_date)
  
  #merge pregnancy_event with medicines_event
  pregnancy_event[,person_id:=as.character(person_id)][,year:=as.character(year)][,age_start_follow_up:=as.character(age_start_follow_up)][,condition:=as.character(condition)][,truncated_code:=as.character(truncated_code)][,event_vocabulary:=as.character(event_vocabulary)]
  medicines_event[,person_id:=as.character(person_id)][,year:=as.character(year)][,age_start_follow_up:=as.character(age_start_follow_up)][,condition:=as.character(condition)][,truncated_code:=as.character(truncated_code)][,event_vocabulary:=as.character(event_vocabulary)]
  medicines_event<-merge(pregnancy_event, medicines_event, all = F)
  
  if(medicines_event[,.N]>0){
    if (subpopulations_present=="Yes"){
      saveRDS(medicines_event, paste0(ev_med_preg_pop, subpopulations_names[s],"/", unlist(str_split(events_pregnancy_files[ev_preg_fl_ind],".rds"))[1],"_", unlist(str_split(events_medicines_files[ev_med_fl_ind],"_"))[3] ))
    } else {
      saveRDS(medicines_event, paste0(ev_med_preg_pop, unlist(str_split(events_pregnancy_files[ev_preg_fl_ind],".rds"))[1],"_", unlist(str_split(events_medicines_files[ev_med_fl_ind],"_"))[3] ))
    }
  }
  
  if(medicines_event[,.N]>0){
    no_records_sub<-medicines_event[,.N, by=c("year","condition","stage_of_pregnancy", "medicinal_product_atc_code")]
    setnames(no_records_sub,"N", "no_records")
    no_women<-medicines_event[,lapply(.SD, function(x) length(unique(na.omit(x)))),by=c("year","condition","stage_of_pregnancy", "medicinal_product_atc_code"), .SDcols="person_id"]
    setnames(no_women, "person_id", "no_women")
    no_total_women<-medicines_event[,lapply(.SD, function(x) length(unique(na.omit(x)))),by="year", .SDcols="person_id"]
    setnames(no_total_women,"person_id","no_total_women")
    no_records_sub[,year:=as.character(year)][,condition:=as.character(condition)][,stage_of_pregnancy:=as.character(stage_of_pregnancy)][,medicinal_product_atc_code:=as.character(medicinal_product_atc_code)]
    no_women[,year:=as.character(year)][,condition:=as.character(condition)][,stage_of_pregnancy:=as.character(stage_of_pregnancy)][,medicinal_product_atc_code:=as.character(medicinal_product_atc_code)]
    no_records_sub<-merge(no_records_sub,no_women,by=c("year","condition","stage_of_pregnancy", "medicinal_product_atc_code"))
    rm(no_women)
    no_records_sub[,year:=as.character(year)]
    no_total_women[,year:=as.character(year)]
    no_records_sub<-merge(no_records_sub,no_total_women,by="year") #number of pregannt women by year
    rm(no_total_women)
    } else {
      no_records_sub<-NULL
    }
    
    no_records[[w]]<-no_records_sub
    rm(medicines_event, no_records_sub)
    w<-w+1
}  

no_records<-do.call(rbind,no_records)
if(!is.null(no_records)){
saveRDS(no_records, paste0(poi_tmp, "poi_res6_ev_preg_med_", events_pregnancy_files[ev_preg_fl_ind]))
}
rm(no_records)

}

###########################################
#EVENTS_PREGNANCY_VACCINES
##########################################
if (length(events_vaccines_files)>0){  
  no_records<-list()
  w<-1
  for (ev_vacc_fl_ind in 1: length(events_vaccines_files)){
    if (subpopulations_present=="Yes"){
      vaccines_event<-readRDS(paste0(ev_vacc_pop, subpopulations_names[s],"/", events_vaccines_files[ev_vacc_fl_ind]))
    } else {
      vaccines_event<-readRDS(paste0(ev_vacc_pop,events_vaccines_files[ev_vacc_fl_ind]))
    }
    setkey(vaccines_event, person_id, year, birth_date,age_start_follow_up, start_follow_up, end_follow_up, condition, truncated_code, event_vocabulary, event_date)
    
    #merge pregnancy_event with vaccines_event
    pregnancy_event[,person_id:=as.character(person_id)][,year:=as.character(year)][,age_start_follow_up:=as.character(age_start_follow_up)][,condition:=as.character(condition)][,truncated_code:=as.character(truncated_code)][,event_vocabulary:=as.character(event_vocabulary)]
    vaccines_event[,person_id:=as.character(person_id)][,year:=as.character(year)][,age_start_follow_up:=as.character(age_start_follow_up)][,condition:=as.character(condition)][,truncated_code:=as.character(truncated_code)][,event_vocabulary:=as.character(event_vocabulary)]
    vaccines_event<-merge(pregnancy_event, vaccines_event, all = F)
    
    if(vaccines_event[,.N]>0){
      if (subpopulations_present=="Yes"){
        saveRDS(vaccines_event, paste0(ev_vacc_preg_pop, subpopulations_names[s],"/", unlist(str_split(events_pregnancy_files[ev_preg_fl_ind],".rds"))[1],"_", unlist(str_split(events_vaccines_files[ev_vacc_fl_ind],"_"))[3] ))
      } else {
        saveRDS(vaccines_event, paste0(ev_vacc_preg_pop, unlist(str_split(events_pregnancy_files[ev_preg_fl_ind],".rds"))[1],"_", unlist(str_split(events_vaccines_files[ev_vacc_fl_ind],"_"))[3] ))
      }
    }
    
    if(vaccines_event[,.N]>0){
      no_records_sub<-vaccines_event[,.N, by=c("year","condition","stage_of_pregnancy", "vx_atc")]
      setnames(no_records_sub,"N", "no_records")
      no_women<-vaccines_event[,lapply(.SD, function(x) length(unique(na.omit(x)))),by=c("year","condition","stage_of_pregnancy", "vx_atc"), .SDcols="person_id"]
      setnames(no_women, "person_id", "no_women")
      no_total_women<-vaccines_event[,lapply(.SD, function(x) length(unique(na.omit(x)))),by="year", .SDcols="person_id"]
      setnames(no_total_women,"person_id","no_total_women")
      no_records_sub[,year:=as.character(year)][,condition:=as.character(condition)][,stage_of_pregnancy:=as.character(stage_of_pregnancy)][,vx_atc:=as.character(vx_atc)]
      no_women[,year:=as.character(year)][,condition:=as.character(condition)][,stage_of_pregnancy:=as.character(stage_of_pregnancy)][,vx_atc:=as.character(vx_atc)]
      no_records_sub<-merge(no_records_sub,no_women,by=c("year","condition","stage_of_pregnancy", "vx_atc"))
      rm(no_women)
      no_records_sub[,year:=as.character(year)]
      no_total_women[,year:=as.character(year)]
      no_records_sub<-merge(no_records_sub,no_total_women,by="year") #number of pregannt women by year
      rm(no_total_women)
    } else {
      no_records_sub<-NULL
    }
    
    no_records[[w]]<-no_records_sub
    rm(vaccines_event, no_records_sub)
    w<-w+1
  }  
  
  no_records<-do.call(rbind,no_records)
  if(!is.null(no_records)){
  saveRDS(no_records, paste0(poi_tmp, "poi_res7_ev_preg_vacc_", events_pregnancy_files[ev_preg_fl_ind]))
  }
  rm(no_records)
  
}
}
}

#####################################################
#Combine results
#####################################################
#EVENTS_PREGNANCY(res1)
#####################################################
events_pregnancy<-list.files(poi_tmp, "poi_res1_ev_preg_")

if (length(events_pregnancy)>0){

ev_preg<-lapply(paste0(poi_tmp, events_pregnancy), readRDS)

ev_preg<-do.call(rbind,ev_preg)
setcolorder(ev_preg, c("condition", "year", "no_women", "no_pregnant_women","stage_of_pregnancy", "truncated_code", "event_vocabulary"))

ev_preg<-data.table(ev_preg, data_access_provider=data_access_provider_name, data_source=data_source_name)
setnames(ev_preg,"condition", "event_definition")

if (subpopulations_present=="Yes"){
  write.csv(ev_preg, paste0(ev_preg_dir,subpopulations_names[s],"/", subpopulations_names[s], "_diagnoses_pregnancy_counts.csv"), row.names = F)
} else {
  write.csv(ev_preg, paste0(ev_preg_dir,"diagnoses_pregnancy_counts.csv"), row.names = F)
}

#######
#Create masked file
#####
ev_preg[, no_women:= as.character(no_women)][as.numeric(no_women) > 0 & as.numeric(no_women) < 5, no_women := "<5"]
ev_preg[, no_pregnant_women:= as.character(no_pregnant_women)][as.numeric(no_pregnant_women) > 0 & as.numeric(no_pregnant_women) < 5, no_pregnant_women := "<5"]
if (subpopulations_present=="Yes"){
write.csv(ev_preg, paste0(ev_preg_dir,subpopulations_names[s],"/", "Masked/", subpopulations_names[s], "_diagnoses_pregnancy_counts_masked.csv"), row.names = F)
} else {
write.csv(ev_preg, paste0(ev_preg_dir,"Masked/", "diagnoses_pregnancy_counts_masked.csv"), row.names = F)
}

for (i in 1:length(events_pregnancy)){
  file.remove(paste0(poi_tmp, events_pregnancy[[i]]))
}

rm(ev_preg, events_pregnancy)
} else {
  rm(events_pregnancy)
}
#####################################################
#MEDICINES_PREGNANCY(res2)
#####################################################
medicines_pregnancy<-list.files(poi_tmp, "poi_res2_med_preg_")

if(length(medicines_pregnancy)>0){

med_preg<-lapply(paste0(poi_tmp, medicines_pregnancy), readRDS)
med_preg<-do.call(rbind,med_preg)
setcolorder(med_preg, c("stage_of_pregnancy", "medicinal_product_atc_code", "year","no_records", "no_women", "no_total_women"))
setnames(med_preg,"medicinal_product_atc_code", "atc_code")

med_preg<-data.table(med_preg, data_access_provider=data_access_provider_name, data_source=data_source_name)


if (subpopulations_present=="Yes"){
  write.csv(med_preg, paste0(med_preg_dir,subpopulations_names[s],"/", subpopulations_names[s], "_medicines_pregnancy_counts.csv"), row.names = F)
} else {
  write.csv(med_preg, paste0(med_preg_dir,"medicines_pregnancy_counts.csv"), row.names = F)
}

#######
#Create masked file
#####
med_preg[, no_women:= as.character(no_women)][as.numeric(no_women) > 0 & as.numeric(no_women) < 5, no_women := "<5"]
med_preg[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
med_preg[, no_total_women:= as.character(no_total_women)][as.numeric(no_total_women) > 0 & as.numeric(no_total_women) < 5, no_total_women := "<5"]
if (subpopulations_present=="Yes"){
  write.csv(med_preg, paste0(med_preg_dir,subpopulations_names[s],"/", "Masked/", subpopulations_names[s], "_medicines_pregnancy_counts_masked.csv"), row.names = F)
} else {
  write.csv(med_preg, paste0(med_preg_dir,"Masked/", "medicines_pregnancy_counts_masked.csv"), row.names = F)
}

for (i in 1:length(medicines_pregnancy)){
  file.remove(paste0(poi_tmp, medicines_pregnancy[[i]]))
}

rm(med_preg, medicines_pregnancy)
} else {rm(medicines_pregnancy)}

#######################################
#VACCINES_PREGNANCY(res3)
########################################

vaccines_pregnancy<-list.files(poi_tmp, "poi_res3_vacc_preg_")

if(length(vaccines_pregnancy)>0){
    vacc_preg<-lapply(paste0(poi_tmp, vaccines_pregnancy), readRDS)
  vacc_preg<-do.call(rbind,vacc_preg)
  setcolorder(vacc_preg, c("stage_of_pregnancy", "vx_atc", "year","no_records", "no_women", "no_total_women"))
  setnames(vacc_preg,"vx_atc", "atc_code")
  
  vacc_preg<-data.table(vacc_preg, data_access_provider=data_access_provider_name, data_source=data_source_name)
  
  
  if (subpopulations_present=="Yes"){
    write.csv(vacc_preg, paste0(vacc_preg_dir,subpopulations_names[s],"/", subpopulations_names[s], "_vaccines_pregnancy_counts.csv"), row.names = F)
  } else {
    write.csv(vacc_preg, paste0(vacc_preg_dir,"vaccines_pregnancy_counts.csv"), row.names = F)
  }
  
  #######
  #Create masked file
  #####
  vacc_preg[, no_women:= as.character(no_women)][as.numeric(no_women) > 0 & as.numeric(no_women) < 5, no_women := "<5"]
  vacc_preg[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
  vacc_preg[, no_total_women:= as.character(no_total_women)][as.numeric(no_total_women) > 0 & as.numeric(no_total_women) < 5, no_total_women := "<5"]
  if (subpopulations_present=="Yes"){
    write.csv(vacc_preg, paste0(vacc_preg_dir,subpopulations_names[s],"/", "Masked/", subpopulations_names[s], "_vaccines_pregnancy_counts_masked.csv"), row.names = F)
  } else {
    write.csv(vacc_preg, paste0(vacc_preg_dir,"Masked/", "vaccines_pregnancy_counts_masked.csv"), row.names = F)
  }
  
  for (i in 1:length(vaccines_pregnancy)){
    file.remove(paste0(poi_tmp, vaccines_pregnancy[[i]]))
  }
  
  rm(vacc_preg, vaccines_pregnancy)
} else {
  rm(vaccines_pregnancy)
}

###########################
#EVENTS_MEDICINES(res4)
##########################

events_medicines<-list.files(poi_tmp, "poi_res4_ev_med_")

if(length(events_medicines)>0){
    ev_med<-lapply(paste0(poi_tmp, events_medicines), readRDS)
  ev_med<-do.call(rbind,ev_med)
  setcolorder(ev_med, c("condition", "medicinal_product_atc_code", "year","no_records", "no_women", "no_total_women"))
  setnames(ev_med,"medicinal_product_atc_code", "atc_code")
  setnames(ev_med,"condition", "event_definition")
  ev_med<-data.table(ev_med, data_access_provider=data_access_provider_name, data_source=data_source_name)
  
  
  if (subpopulations_present=="Yes"){
    write.csv(ev_med, paste0(ev_med_dir,subpopulations_names[s],"/", subpopulations_names[s], "_events_medicines_counts.csv"), row.names = F)
  } else {
    write.csv(ev_med, paste0(ev_med_dir,"events_medicines_counts.csv"), row.names = F)
  }
  
  #######
  #Create masked file
  #####
  ev_med[, no_women:= as.character(no_women)][as.numeric(no_women) > 0 & as.numeric(no_women) < 5, no_women := "<5"]
  ev_med[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
  ev_med[, no_total_women:= as.character(no_total_women)][as.numeric(no_total_women) > 0 & as.numeric(no_total_women) < 5, no_total_women := "<5"]
  if (subpopulations_present=="Yes"){
    write.csv(ev_med, paste0(ev_med_dir,subpopulations_names[s],"/", "Masked/", subpopulations_names[s], "_events_medicines_counts_masked.csv"), row.names = F)
  } else {
    write.csv(ev_med, paste0(ev_med_dir,"Masked/", "events_medicines_counts_masked.csv"), row.names = F)
  }
  
  for (i in 1:length(events_medicines)){
    file.remove(paste0(poi_tmp, events_medicines[[i]]))
  }
  
  rm(ev_med, events_medicines)
} else {
  rm(events_medicines)
} 

###########################
#EVENTS_VACCINES(res5)
##########################

events_vaccines<-list.files(poi_tmp, "poi_res5_ev_vacc_")

if(length(events_vaccines)>0){
    ev_vacc<-lapply(paste0(poi_tmp, events_vaccines), readRDS)
  ev_vacc<-do.call(rbind,ev_vacc)
  setcolorder(ev_vacc, c("condition", "vx_atc", "year","no_records", "no_women", "no_total_women"))
  setnames(ev_vacc,"vx_atc", "atc_code")
  setnames(ev_vacc,"condition", "event_definition")
  ev_vacc<-data.table(ev_vacc, data_access_provider=data_access_provider_name, data_source=data_source_name)
  
  
  if (subpopulations_present=="Yes"){
    write.csv(ev_vacc, paste0(ev_vacc_dir,subpopulations_names[s],"/", subpopulations_names[s], "_events_vaccines_counts.csv"), row.names = F)
  } else {
    write.csv(ev_vacc, paste0(ev_vacc_dir,"events_vaccines_counts.csv"), row.names = F)
  }
  
  #######
  #Create masked file
  #####
  ev_vacc[, no_women:= as.character(no_women)][as.numeric(no_women) > 0 & as.numeric(no_women) < 5, no_women := "<5"]
  ev_vacc[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
  ev_vacc[, no_total_women:= as.character(no_total_women)][as.numeric(no_total_women) > 0 & as.numeric(no_total_women) < 5, no_total_women := "<5"]
  if (subpopulations_present=="Yes"){
    write.csv(ev_vacc, paste0(ev_vacc_dir,subpopulations_names[s],"/", "Masked/", subpopulations_names[s], "_events_vaccines_counts_masked.csv"), row.names = F)
  } else {
    write.csv(ev_vacc, paste0(ev_vacc_dir,"Masked/", "events_vaccines_counts_masked.csv"), row.names = F)
  }
  
  for (i in 1:length(events_vaccines)){
    file.remove(paste0(poi_tmp, events_vaccines[[i]]))
  }
  
  rm(ev_vacc, events_vaccines)
} else {
  rm(events_vaccines)
} 

##################################
#EVENTS_PREGNANCY_MEDICINES(res6)
###################################

events_medicines_pregnancy<-list.files(poi_tmp, "poi_res6_ev_preg_med_")

if(length(events_medicines_pregnancy)>0){
    ev_med_preg<-lapply(paste0(poi_tmp, events_medicines_pregnancy), readRDS)
  ev_med_preg<-do.call(rbind,ev_med_preg)
  setcolorder(ev_med_preg, c("condition","stage_of_pregnancy", "medicinal_product_atc_code", "year","no_records", "no_women", "no_total_women"))
  setnames(ev_med_preg,"medicinal_product_atc_code", "atc_code")
  
  ev_med_preg<-data.table(ev_med_preg, data_access_provider=data_access_provider_name, data_source=data_source_name)
  setnames(ev_med_preg,"condition", "event_definition")
  
  if (subpopulations_present=="Yes"){
    write.csv(ev_med_preg, paste0(ev_med_preg_dir,subpopulations_names[s],"/", subpopulations_names[s], "_events_medicines_pregnancy_counts.csv"), row.names = F)
  } else {
    write.csv(ev_med_preg, paste0(ev_med_preg_dir,"events_medicines_pregnancy_counts.csv"), row.names = F)
  }
  
  #######
  #Create masked file
  #####
  ev_med_preg[, no_women:= as.character(no_women)][as.numeric(no_women) > 0 & as.numeric(no_women) < 5, no_women := "<5"]
  ev_med_preg[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
  ev_med_preg[, no_total_women:= as.character(no_total_women)][as.numeric(no_total_women) > 0 & as.numeric(no_total_women) < 5, no_total_women := "<5"]
  if (subpopulations_present=="Yes"){
    write.csv(ev_med_preg, paste0(ev_med_preg_dir,subpopulations_names[s],"/", "Masked/", subpopulations_names[s], "_events_medicines_pregnancy_counts_masked.csv"), row.names = F)
  } else {
    write.csv(ev_med_preg, paste0(ev_med_preg_dir,"Masked/", "events_medicines_pregnancy_counts_masked.csv"), row.names = F)
  }
  
  for (i in 1:length(events_medicines_pregnancy)){
    file.remove(paste0(poi_tmp, events_medicines_pregnancy[[i]]))
  }
  
  rm(ev_med_preg, events_medicines_pregnancy)
} else {
  rm(events_medicines_pregnancy)
} 

##################################
#EVENTS_PREGNANCY_VACCINES(res7)
###################################

events_vaccines_pregnancy<-list.files(poi_tmp, "poi_res7_ev_preg_vacc_")

if(length(events_vaccines_pregnancy)>0){
    ev_vacc_preg<-lapply(paste0(poi_tmp, events_vaccines_pregnancy), readRDS)
  ev_vacc_preg<-do.call(rbind,ev_vacc_preg)
  setcolorder(ev_vacc_preg, c("condition","stage_of_pregnancy", "vx_atc", "year","no_records", "no_women", "no_total_women"))
  setnames(ev_vacc_preg,"vx_atc", "atc_code")
  
  ev_vacc_preg<-data.table(ev_vacc_preg, data_access_provider=data_access_provider_name, data_source=data_source_name)
  
  
  if (subpopulations_present=="Yes"){
    write.csv(ev_vacc_preg, paste0(ev_vacc_preg_dir,subpopulations_names[s],"/", subpopulations_names[s], "_events_vaccines_pregnancy_counts.csv"), row.names = F)
  } else {
    write.csv(ev_vacc_preg, paste0(ev_vacc_preg_dir,"events_vaccines_pregnancy_counts.csv"), row.names = F)
  }
  
  #######
  #Create masked file
  #####
  ev_vacc_preg[, no_women:= as.character(no_women)][as.numeric(no_women) > 0 & as.numeric(no_women) < 5, no_women := "<5"]
  ev_vacc_preg[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
  ev_vacc_preg[, no_total_women:= as.character(no_total_women)][as.numeric(no_total_women) > 0 & as.numeric(no_total_women) < 5, no_total_women := "<5"]
  if (subpopulations_present=="Yes"){
    write.csv(ev_vacc_preg, paste0(ev_med_preg_dir,subpopulations_names[s],"/", "Masked/", subpopulations_names[s], "_events_vaccines_pregnancy_counts_masked.csv"), row.names = F)
  } else {
    write.csv(ev_vacc_preg, paste0(ev_med_preg_dir,"Masked/", "events_vaccines_pregnancy_counts_masked.csv"), row.names = F)
  }
  
  for (i in 1:length(events_vaccines_pregnancy)){
    file.remove(paste0(poi_tmp, events_vaccines_pregnancy[[i]]))
  }
  
  rm(ev_vacc_preg, events_vaccines_pregnancy)
} else {
  rm(events_vaccines_pregnancy)
} 

files_tmp<-list.files(poi_tmp) 
if(length(files_tmp)>0){
for (i in 1:length(files_tmp)){
  file.remove(paste0(poi_tmp,files_tmp[[i]]))
}
}