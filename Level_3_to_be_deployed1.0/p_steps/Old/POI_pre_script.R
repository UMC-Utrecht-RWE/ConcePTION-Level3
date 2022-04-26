######################################################################################
#ev_preg_res_subults lists
######################################################################################
######################################################################################
#EVENTS_MEDICINES_PREGNANCY
#####################################################################################
flowchart_ev_med_preg<-list()
ev_med_preg_ev_preg_res_sub<-list() 
ev_med_preg_ev_preg_res_sub_365<-list()
######################################################################################
#EVENTS_MEDICINES_PREGNANCY
#####################################################################################
flowchart_ev_med_preg<-list()
ev_med_preg_ev_preg_res_sub<-list() 
ev_med_preg_ev_preg_res_sub_365<-list()
######################################################################################



##################################################################
#EVENTS_PREGNANCY #MEDICINES_PREGNANCY #VACCINES_PREGNANCY
###################################################################
w<-1
z<-1
for (i in 1:length(pregnancy_files)){
  if (subpopulations_present=="Yes"){
  pregnancy_pop<-readRDS(paste0(preg_pop, subpopulations_names[s],"/", pregnancy_files[[i]]))
  pregnancy_pop<-pregnancy_pop[,c("person_id","birth_date", "exit_spell_category","start_follow_up","age_fup","condition", "pregnancy_code_date")]
  } else {
    pregnancy_pop<-readRDS(paste0(preg_pop, pregnancy_files[[i]]))
    pregnancy_pop<-pregnancy_pop[,c("person_id","birth_date", "exit_spell_category","start_follow_up","age_fup","condition", "pregnancy_code_date")]
    }
  setnames(pregnancy_pop, "condition", "stage_of_pregnancy")
  pregnancy_pop[,year:=year(pregnancy_code_date)]
  setkey(pregnancy_pop, person_id, year)
  no_orig_pregnancy<-pregnancy_pop[,.N]
  preg_flowchart<-data.table(table=pregnancy_files[[i]], no_before=no_orig_pregnancy, no_after="N/A")
  preg_stage<-pregnancy_pop[!duplicated(stage_of_pregnancy),stage_of_pregnancy]
  
  
  #####################################################
  #PREGNANCY_EVENTS
  #####################################################
  w<-1
  flowchart_ev_preg_sub<-list()
  ev_preg_res_sub<-list()
  ev_preg_res_sub_365<-list()
  
  for (j in 1: length(conditions)){
    if (subpopulations_present=="Yes"){
    diagnoses<-lapply(paste0(diag_pop, subpopulations_names[s],"/",conditions[[j]]), readRDS) #combine all files for one pregnancy stage
    } else {
      diagnoses<-lapply(paste0(diag_pop, subpopulations_names[s],"/",conditions[[j]]), readRDS) #combine all files for one pregnancy stage
    }
    diagnoses<-do.call(rbind,diagnoses)
    diagnoses<-diagnoses[,c("person_id","event_date","condition","event_code","truncated_code","event_vocabulary")]
    diagnoses[,year:=year(event_date)]
    no_orig_diagnoses<-diagnoses[,.N]
    setkey(diagnoses, person_id, year)
    end_year<-as.IDate(paste0(substr(names(conditions)[j],1,4),12,31), "%Y%m%d")
    #combine on person_id keep only files in both tables
    diagnoses<-merge(pregnancy_pop[year(pregnancy_code_date)==substr(names(conditions)[j],1,4)],diagnoses, all=F, by=c("person_id","year")) #inner join
    no_merged<-diagnoses[,.N]
    no_records<-diagnoses[,.N]
    no_women<-diagnoses[!duplicated(person_id),.N]

    if (diagnoses[,.N]>0){
    diagnoses[,end_year:=end_year][,person_years:=end_year-start_follow_up]
    diagnoses[,fup_filter:=exit_spell_category-start_follow_up][,person_years:=end_year-start_follow_up]
    #if person_years>365 replace with 365, if smaller replace with 0
    diagnoses[person_years>365, person_years:=365][person_years<0, person_years:=0]
   
    person_years<-sum(diagnoses[,person_years])
    diagnoses<-diagnoses[fup_filter>365]
    no_records_365<-diagnoses[,.N]
    no_women_365<-diagnoses[!duplicated(person_id),.N]
    person_years_365<-sum(diagnoses[,person_years])
    
    } else {
      no_records_365<-0
      no_women_365<-0
      person_years_365<-0
      person_years<-0
    }
    
    
    flowchart_ev_preg_sub[[w]]<-data.table(table=names(conditions)[j], 
                                   no_before=no_orig_diagnoses,
                                   no_after=no_merged)
    
    ev_preg_res_sub[[w]]<-data.table(condition=names(conditions)[j], stage_of_pregnancy=preg_stage, no_records= no_records, no_women=no_women, person_years=person_years)
    ev_preg_res_sub_365[[w]]<-data.table(condition=names(conditions)[j], stage_of_pregnancy=preg_stage, no_records= no_records_365, no_women=no_women_365, person_years=person_years_365)
    
    w<-w+1
  }
  
  saveRDS(rbind(preg_flowchart, do.call(rbind,flowchart_ev_preg_sub)), paste0(poi_tmp,"flowchart_ev_preg_", pregnancy_files[i]))
  saveRDS(do.call(rbind,ev_preg_res_sub), paste0(poi_tmp,"ev_preg_res_", pregnancy_files[i]))
  saveRDS(do.call(rbind,ev_preg_res_sub_365), paste0(poi_tmp,"ev_preg_res_365_", pregnancy_files[i]))
  
  rm(flowchart_ev_preg_sub, ev_preg_res_sub,ev_preg_res_sub_365)

  #######################################################################
  #PREGNANCY_MEDICINES
  #######################################################################
  flowchart_med_preg_sub<-list()
  med_preg_res_sub<-list()
  med_preg_res_sub_365<-list()
  w<-1
  for (k in 1: length(medicines_list)){
    if (subpopulations_present=="Yes"){
      medicines<-lapply(paste0(medicines_pop, subpopulations_names[s],"/",medicines_list[[k]]), readRDS) #combine all files for one pregnancy stage
    } else {
      medicines<-lapply(paste0(medicines_pop, subpopulations_names[s],"/",medicines_list[[k]]), readRDS) #combine all files for one pregnancy stage
    }
    medicines<-do.call(rbind,medicines)
    no_orig_medicines<-medicines[,.N]
    setkey(medicines, person_id, year)
    end_year<-as.IDate(paste0(substr(names(medicines_list)[k],3,6),12,31), "%Y%m%d")
    #combine on person_id keep only files in both tables
    medicines<-merge(pregnancy_pop[year(pregnancy_code_date)==substr(names(medicines_list)[k],1,4)],medicines, all=F, by=c("person_id","year")) #inner join
    no_merged<-medicines[,.N]
    no_records<-medicines[,.N]
    no_women<-medicines[!duplicated(person_id),.N]
    
    if (medicines[,.N]>0){
      medicines[,end_year:=end_year][,person_years:=end_year-start_follow_up]
      medicines[,fup_filter:=exit_spell_category-start_follow_up][,person_years:=end_year-start_follow_up]
      #if person_years>365 replace with 365, if smaller replace with 0
      medicines[person_years>365, person_years:=365][person_years<0, person_years:=0]
      
      person_years<-sum(medicines[,person_years])
      medicines<-medicines[fup_filter>365]
      no_records_365<-medicines[,.N]
      no_women_365<-medicines[!duplicated(person_id),.N]
      person_years_365<-sum(medicines[,person_years])
      
    } else {
      no_records_365<-0
      no_women_365<-0
      person_years_365<-0
      person_years<-0
    }
    
    
    flowchart_med_preg_sub[[w]]<-data.table(table=names(medicines_list)[k], 
                                           no_before=no_orig_medicines,
                                           no_after=no_merged)
    
    med_preg_res_sub[[w]]<-data.table(condition=names(medicines_list)[k], stage_of_pregnancy=preg_stage, no_records= no_records, no_women=no_women, person_years=person_years)
    med_preg_res_sub_365[[w]]<-data.table(condition=names(medicines_list)[k], stage_of_pregnancy=preg_stage, no_records= no_records_365, no_women=no_women_365, person_years=person_years_365)
    
    
    w<-w+1
  }
  
  saveRDS(rbind(preg_flowchart, do.call(rbind,flowchart_med_preg_sub)), paste0(poi_tmp,"flowchart_med_preg_", pregnancy_files[i]))
  saveRDS(do.call(rbind,med_preg_res_sub), paste0(poi_tmp,"med_preg_res_", pregnancy_files[i]))
  saveRDS(do.call(rbind,med_preg_res_sub_365), paste0(poi_tmp,"med_preg_res_365_", pregnancy_files[i]))
  
  rm(flowchart_med_preg_sub, med_preg_res_sub,med_preg_res_sub_365)
  ########################################################################
  #PREGNANCY_VACCINES
  ########################################################################
  flowchart_vacc_preg_sub<-list()
  vacc_preg_res_sub<-list()
  vacc_preg_res_sub_365<-list()
  w<-1
  for (k in 1: length(vaccines_list)){
    if (subpopulations_present=="Yes"){
      vaccines<-lapply(paste0(vaccines_pop, subpopulations_names[s],"/",vaccines_list[[k]]), readRDS) #combine all files for one pregnancy stage
    } else {
      vaccines<-lapply(paste0(vaccines_pop, subpopulations_names[s],"/",vaccines_list[[k]]), readRDS) #combine all files for one pregnancy stage
    }
    vaccines<-do.call(rbind,vaccines)
    no_orig_vaccines<-vaccines[,.N]
    setkey(vaccines, person_id, year)
    end_year<-as.IDate(paste0(substr(names(vaccines_list)[k],3,6),12,31), "%Y%m%d")
    #combine on person_id keep only files in both tables
    vaccines<-merge(pregnancy_pop[year(pregnancy_code_date)==substr(names(vaccines_list)[k],1,4)],vaccines, all=F, by=c("person_id","year")) #inner join
    no_merged<-vaccines[,.N]
    no_records<-vaccines[,.N]
    no_women<-vaccines[!duplicated(person_id),.N]
    
    if (vaccines[,.N]>0){
      vaccines[,end_year:=end_year][,person_years:=end_year-start_follow_up]
      vaccines[,fup_filter:=exit_spell_category-start_follow_up][,person_years:=end_year-start_follow_up]
      #if person_years>365 replace with 365, if smaller replace with 0
      vaccines[person_years>365, person_years:=365][person_years<0, person_years:=0]
      
      person_years<-sum(vaccines[,person_years])
      vaccines<-vaccines[fup_filter>365]
      no_records_365<-vaccines[,.N]
      no_women_365<-vaccines[!duplicated(person_id),.N]
      person_years_365<-sum(vaccines[,person_years])
      
    } else {
      no_records_365<-0
      no_women_365<-0
      person_years_365<-0
      person_years<-0
    }
    
    
    flowchart_vacc_preg_sub[[w]]<-data.table(table=names(vaccines_list)[k], 
                                            no_before=no_orig_vaccines,
                                            no_after=no_merged)
    
    vacc_preg_res_sub[[w]]<-data.table(condition=names(medicines_list)[k], stage_of_pregnancy=preg_stage, no_records= no_records, no_women=no_women, person_years=person_years)
    vacc_preg_res_sub_365[[w]]<-data.table(condition=names(medicines_list)[k], stage_of_pregnancy=preg_stage, no_records= no_records_365, no_women=no_women_365, person_years=person_years_365)
    
    
    w<-w+1
  }
  
  saveRDS(rbind(preg_flowchart, do.call(rbind,flowchart_vacc_preg_sub)), paste0(poi_tmp,"flowchart_vacc_preg_", pregnancy_files[i]))
  saveRDS(do.call(rbind,vacc_preg_res_sub), paste0(poi_tmp,"vacc_preg_res_", pregnancy_files[i]))
  saveRDS(do.call(rbind,vacc_preg_res_sub_365), paste0(poi_tmp,"vacc_preg_res_365_", pregnancy_files[i]))
  
  rm(flowchart_vacc_preg_sub, vacc_preg_res_sub, vacc_preg_res_sub_365)
  
}

##################################################################
#EVENTS_MEDICINES #EVENTS_VACCINES
###################################################################
w<-1
z<-1
for (i in 1:length(conditions)){
  if (subpopulations_present=="Yes"){
    diagnoses<-readRDS(paste0(diag_pop, subpopulations_names[s],"/", conditions[[i]]))
    diagnoses<-diagnoses[,c("person_id","birth_date", "exit_spell_category","start_follow_up","age_fup","condition", "event_code", "truncated_code","event_vocabulary", "event_date")]
  } else {
    diagnoses<-readRDS(paste0(diag_pop, conditions[[i]]))
    diagnoses<-diagnoses[,c("person_id","birth_date", "exit_spell_category","start_follow_up","age_fup","condition", "event_code", "truncated_code","event_vocabulary","event_date")]
  }
  diagnoses[,year:=year(event_date)]
  setkey(diagnoses, person_id, year)
  no_orig_diagnoses<-diagnoses[,.N]
  diagnoses_flowchart<-data.table(table=names(conditions)[i], no_before=no_orig_diagnoses, no_after="N/A")
  condition_filter<-diagnoses[!duplicated(condition),condition]
  
  
  #####################################################
  #EVENTS_MEDICINES
  #####################################################
  w<-1
  flowchart_ev_preg_sub<-list()
  ev_preg_res_sub<-list()
  ev_preg_res_sub_365<-list()
  
  for (j in 1: length(conditions)){
    if (subpopulations_present=="Yes"){
      diagnoses<-lapply(paste0(diag_pop, subpopulations_names[s],"/",conditions[[j]]), readRDS) #combine all files for one pregnancy stage
    } else {
      diagnoses<-lapply(paste0(diag_pop, subpopulations_names[s],"/",conditions[[j]]), readRDS) #combine all files for one pregnancy stage
    }
    diagnoses<-do.call(rbind,diagnoses)
    diagnoses<-diagnoses[,c("person_id","event_date","condition","event_code","truncated_code","event_vocabulary")]
    diagnoses[,year:=year(event_date)]
    no_orig_diagnoses<-diagnoses[,.N]
    setkey(diagnoses, person_id, year)
    end_year<-as.IDate(paste0(substr(names(conditions)[j],1,4),12,31), "%Y%m%d")
    #combine on person_id keep only files in both tables
    diagnoses<-merge(pregnancy_pop[year(pregnancy_code_date)==substr(names(conditions)[j],1,4)],diagnoses, all=F, by=c("person_id","year")) #inner join
    no_merged<-diagnoses[,.N]
    no_records<-diagnoses[,.N]
    no_women<-diagnoses[!duplicated(person_id),.N]
    
    if (diagnoses[,.N]>0){
      diagnoses[,end_year:=end_year][,person_years:=end_year-start_follow_up]
      diagnoses[,fup_filter:=exit_spell_category-start_follow_up][,person_years:=end_year-start_follow_up]
      #if person_years>365 replace with 365, if smaller replace with 0
      diagnoses[person_years>365, person_years:=365][person_years<0, person_years:=0]
      
      person_years<-sum(diagnoses[,person_years])
      diagnoses<-diagnoses[fup_filter>365]
      no_records_365<-diagnoses[,.N]
      no_women_365<-diagnoses[!duplicated(person_id),.N]
      person_years_365<-sum(diagnoses[,person_years])
      
    } else {
      no_records_365<-0
      no_women_365<-0
      person_years_365<-0
      person_years<-0
    }
    
    
    flowchart_ev_preg_sub[[w]]<-data.table(table=names(conditions)[j], 
                                           no_before=no_orig_diagnoses,
                                           no_after=no_merged)
    
    ev_preg_res_sub[[w]]<-data.table(condition=names(conditions)[j], stage_of_pregnancy=preg_stage, no_records= no_records, no_women=no_women, person_years=person_years)
    ev_preg_res_sub_365[[w]]<-data.table(condition=names(conditions)[j], stage_of_pregnancy=preg_stage, no_records= no_records_365, no_women=no_women_365, person_years=person_years_365)
    
    w<-w+1
  }
  
  saveRDS(rbind(preg_flowchart, do.call(rbind,flowchart_ev_preg_sub)), paste0(poi_tmp,"flowchart_ev_preg_", pregnancy_files[i]))
  saveRDS(do.call(rbind,ev_preg_res_sub), paste0(poi_tmp,"ev_preg_res_", pregnancy_files[i]))
  saveRDS(do.call(rbind,ev_preg_res_sub_365), paste0(poi_tmp,"ev_preg_res_365_", pregnancy_files[i]))
  
  rm(flowchart_ev_preg_sub, ev_preg_res_sub,ev_preg_res_sub_365)
  
  #######################################################################
  #PREGNANCY_MEDICINES
  #######################################################################
  flowchart_med_preg_sub<-list()
  med_preg_res_sub<-list()
  med_preg_res_sub_365<-list()
  w<-1
  for (k in 1: length(medicines_list)){
    if (subpopulations_present=="Yes"){
      medicines<-lapply(paste0(medicines_pop, subpopulations_names[s],"/",medicines_list[[k]]), readRDS) #combine all files for one pregnancy stage
    } else {
      medicines<-lapply(paste0(medicines_pop, subpopulations_names[s],"/",medicines_list[[k]]), readRDS) #combine all files for one pregnancy stage
    }
    medicines<-do.call(rbind,medicines)
    no_orig_medicines<-medicines[,.N]
    setkey(medicines, person_id, year)
    end_year<-as.IDate(paste0(substr(names(medicines_list)[k],3,6),12,31), "%Y%m%d")
    #combine on person_id keep only files in both tables
    medicines<-merge(pregnancy_pop[year(pregnancy_code_date)==substr(names(medicines_list)[k],1,4)],medicines, all=F, by=c("person_id","year")) #inner join
    no_merged<-medicines[,.N]
    no_records<-medicines[,.N]
    no_women<-medicines[!duplicated(person_id),.N]
    
    if (medicines[,.N]>0){
      medicines[,end_year:=end_year][,person_years:=end_year-start_follow_up]
      medicines[,fup_filter:=exit_spell_category-start_follow_up][,person_years:=end_year-start_follow_up]
      #if person_years>365 replace with 365, if smaller replace with 0
      medicines[person_years>365, person_years:=365][person_years<0, person_years:=0]
      
      person_years<-sum(medicines[,person_years])
      medicines<-medicines[fup_filter>365]
      no_records_365<-medicines[,.N]
      no_women_365<-medicines[!duplicated(person_id),.N]
      person_years_365<-sum(medicines[,person_years])
      
    } else {
      no_records_365<-0
      no_women_365<-0
      person_years_365<-0
      person_years<-0
    }
    
    
    flowchart_med_preg_sub[[w]]<-data.table(table=names(medicines_list)[k], 
                                            no_before=no_orig_medicines,
                                            no_after=no_merged)
    
    med_preg_res_sub[[w]]<-data.table(condition=names(medicines_list)[k], stage_of_pregnancy=preg_stage, no_records= no_records, no_women=no_women, person_years=person_years)
    med_preg_res_sub_365[[w]]<-data.table(condition=names(medicines_list)[k], stage_of_pregnancy=preg_stage, no_records= no_records_365, no_women=no_women_365, person_years=person_years_365)
    
    
    w<-w+1
  }
  
  saveRDS(rbind(preg_flowchart, do.call(rbind,flowchart_med_preg_sub)), paste0(poi_tmp,"flowchart_med_preg_", pregnancy_files[i]))
  saveRDS(do.call(rbind,med_preg_res_sub), paste0(poi_tmp,"med_preg_res_", pregnancy_files[i]))
  saveRDS(do.call(rbind,med_preg_res_sub_365), paste0(poi_tmp,"med_preg_res_365_", pregnancy_files[i]))
  
  rm(flowchart_med_preg_sub, med_preg_res_sub,med_preg_res_sub_365)
  ########################################################################
  #PREGNANCY_VACCINES
  ########################################################################
  flowchart_vacc_preg_sub<-list()
  vacc_preg_res_sub<-list()
  vacc_preg_res_sub_365<-list()
  w<-1
  for (k in 1: length(vaccines_list)){
    if (subpopulations_present=="Yes"){
      vaccines<-lapply(paste0(vaccines_pop, subpopulations_names[s],"/",vaccines_list[[k]]), readRDS) #combine all files for one pregnancy stage
    } else {
      vaccines<-lapply(paste0(vaccines_pop, subpopulations_names[s],"/",vaccines_list[[k]]), readRDS) #combine all files for one pregnancy stage
    }
    vaccines<-do.call(rbind,vaccines)
    no_orig_vaccines<-vaccines[,.N]
    setkey(vaccines, person_id, year)
    end_year<-as.IDate(paste0(substr(names(vaccines_list)[k],3,6),12,31), "%Y%m%d")
    #combine on person_id keep only files in both tables
    vaccines<-merge(pregnancy_pop[year(pregnancy_code_date)==substr(names(vaccines_list)[k],1,4)],vaccines, all=F, by=c("person_id","year")) #inner join
    no_merged<-vaccines[,.N]
    no_records<-vaccines[,.N]
    no_women<-vaccines[!duplicated(person_id),.N]
    
    if (vaccines[,.N]>0){
      vaccines[,end_year:=end_year][,person_years:=end_year-start_follow_up]
      vaccines[,fup_filter:=exit_spell_category-start_follow_up][,person_years:=end_year-start_follow_up]
      #if person_years>365 replace with 365, if smaller replace with 0
      vaccines[person_years>365, person_years:=365][person_years<0, person_years:=0]
      
      person_years<-sum(vaccines[,person_years])
      vaccines<-vaccines[fup_filter>365]
      no_records_365<-vaccines[,.N]
      no_women_365<-vaccines[!duplicated(person_id),.N]
      person_years_365<-sum(vaccines[,person_years])
      
    } else {
      no_records_365<-0
      no_women_365<-0
      person_years_365<-0
      person_years<-0
    }
    
    
    flowchart_vacc_preg_sub[[w]]<-data.table(table=names(vaccines_list)[k], 
                                             no_before=no_orig_vaccines,
                                             no_after=no_merged)
    
    vacc_preg_res_sub[[w]]<-data.table(condition=names(medicines_list)[k], stage_of_pregnancy=preg_stage, no_records= no_records, no_women=no_women, person_years=person_years)
    vacc_preg_res_sub_365[[w]]<-data.table(condition=names(medicines_list)[k], stage_of_pregnancy=preg_stage, no_records= no_records_365, no_women=no_women_365, person_years=person_years_365)
    
    
    w<-w+1
  }
  
  saveRDS(rbind(preg_flowchart, do.call(rbind,flowchart_vacc_preg_sub)), paste0(poi_tmp,"flowchart_vacc_preg_", pregnancy_files[i]))
  saveRDS(do.call(rbind,vacc_preg_res_sub), paste0(poi_tmp,"vacc_preg_res_", pregnancy_files[i]))
  saveRDS(do.call(rbind,vacc_preg_res_sub_365), paste0(poi_tmp,"vacc_preg_res_365_", pregnancy_files[i]))
  
  rm(flowchart_vacc_preg_sub, vacc_preg_res_sub, vacc_preg_res_sub_365)
  
}


