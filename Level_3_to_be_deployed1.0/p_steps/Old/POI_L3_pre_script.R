
for (i in 1:length(pregnancy_files)){
  if (subpopulations_present=="Yes"){
    pregnancy_pop<-readRDS(paste0(preg_pop, subpopulations_names[s],"/", pregnancy_files[[i]]))
    pregnancy_pop<-pregnancy_pop[,c("person_id","birth_date", "op_end_date","start_follow_up","age_start_follow_up","condition", "pregnancy_code_date")]
  } else {
    pregnancy_pop<-readRDS(paste0(preg_pop, pregnancy_files[[i]]))
    pregnancy_pop<-pregnancy_pop[,c("person_id","birth_date", "op_end_date","start_follow_up","age_start_follow_up","condition", "pregnancy_code_date")]
  }
  setnames(pregnancy_pop, "condition", "stage_of_pregnancy")
  pregnancy_pop[,year:=year(pregnancy_code_date)]
  #first we aggregate data over year(if a women has the same stage of pregnancy over the same year more than once that will be aggregated)
  colnames_preg<-names(pregnancy_pop)
  colnames_preg<-colnames_preg[!colnames_preg %in% "pregnancy_code_date"]
  pregnancy_pop<-pregnancy_pop[,.N,by=colnames_preg]
  setnames(pregnancy_pop,"N","counts_pregnancy")
  setkey(pregnancy_pop, person_id, year)
  preg_stage<-pregnancy_pop[!duplicated(stage_of_pregnancy),stage_of_pregnancy]
  
  #####################################################
  #PREGNANCY_EVENTS
  #####################################################
  w<-1
  for (j in 1: length(conditions_files)){
    if (subpopulations_present=="Yes"){
      diagnoses<-lapply(paste0(diag_pop, subpopulations_names[s],"/",conditions_files[[j]]), readRDS) #combine all files for one pregnancy stage
    } else {
      diagnoses<-lapply(paste0(diag_pop, subpopulations_names[s],"/",conditions_files[[j]]), readRDS) #combine all files for one pregnancy stage
    }
    diagnoses<-do.call(rbind,diagnoses)
    diagnoses<-diagnoses[sex_at_instance_creation=="F" & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,c("person_id","condition", "truncated_code","event_vocabulary","birth_date","op_end_date","start_follow_up","age_start_follow_up","year")]

    #aggregate data
    colnames_diag<-names(diagnoses)
    diagnoses<-diagnoses[, .N, by=colnames_diag]
    setnames(diagnoses, "N", "counts_diagnoses")
    
    #merge databases
    diagnoses<-merge(pregnancy_pop,diagnoses, all=T, by=c("person_id","year","birth_date","op_end_date","start_follow_up","age_start_follow_up")) #inner join
    #replace NA in counts_diagnoses and counts_pregnancy with 0
    diagnoses[is.na(counts_pregnancy),counts_pregnancy:=0][is.na(counts_diagnoses),counts_diagnoses:=0]
    
    if(diagnoses[,.N]>0){
    if (subpopulations_present=="Yes"){
      saveRDS(diagnoses, paste0(ev_preg_pop, subpopulations_names[s],"/", subpopulations_names[s], "_", names(conditions_files)[j], "_",preg_stage, ".rds" ))
    } else {
      saveRDS(diagnoses, paste0(ev_preg_pop, names(conditions_files)[j], "_",preg_stage, ".rds" ))
      
    }
    }
    rm(diagnoses)
     w<-w+1
  }
  
  #####################################################
  #PREGNANCY_MEDICINES
  #####################################################
  w<-1
  for (j in 1: length(medicines_files)){
    if (subpopulations_present=="Yes"){
      medicines<-lapply(paste0(medicines_pop, subpopulations_names[s],"/",medicines_files[[j]]), readRDS) #combine all files for one pregnancy stage
    } else {
      medicines<-lapply(paste0(medicines_pop, subpopulations_names[s],"/",medicines_files[[j]]), readRDS) #combine all files for one pregnancy stage
    }
    medicines<-do.call(rbind,medicines)
    
    medicines<-medicines[age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,c("person_id","year", "medicinal_product_atc_code","age_start_follow_up")]

    #aggregate data(keep only data for pregnant women, if no pregnancy code in a given year the discard of those observations)
    colnames_medicines<-names(medicines)
    medicines<-medicines[, .N, by=colnames_medicines]
    setnames(medicines, "N", "counts_medicines")
    
    #merge databases
    medicines<-merge(pregnancy_pop,medicines, by=c("person_id","year", "age_start_follow_up")) #left join
     #replace NA in counts_medicines with 0
    medicines[is.na(counts_medicines),counts_medicines:=0]
    
    if(medicines[,.N]>0){
    if (subpopulations_present=="Yes"){
      saveRDS(medicines, paste0(med_preg_pop, subpopulations_names[s],"/", subpopulations_names[s], "_", names(conditions_files)[j], "_",preg_stage, ".rds" ))
    } else {
      saveRDS(medicines, paste0(med_preg_pop, names(conditions_files)[j], "_",preg_stage, ".rds" ))
      
    }
    }

    rm(medicines)
    w<-w+1
  }
  
  #####################################################
  #PREGNANCY_VACCINES
  #####################################################
  w<-1
  for (j in 1: length(vaccines_files)){
    if (subpopulations_present=="Yes"){
      vaccines<-lapply(paste0(vaccines_pop, subpopulations_names[s],"/",vaccines_files[[j]]), readRDS) #combine all files for one pregnancy stage
    } else {
      vaccines<-lapply(paste0(vaccines_pop, subpopulations_names[s],"/",vaccines_files[[j]]), readRDS) #combine all files for one pregnancy stage
    }
    vaccines<-do.call(rbind,vaccines)
    
    vaccines<-vaccines[age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,c("person_id","year", "vx_atc","age_start_follow_up")]
    
    #aggregate data(keep only data for pregnant women, if no pregnancy code in a given year the discard of those observations)
    colnames_vaccines<-names(vaccines)
    vaccines<-vaccines[, .N, by=colnames_vaccines]
    setnames(vaccines, "N", "counts_vaccines")
    
    #merge databases
    vaccines<-merge(pregnancy_pop,vaccines, by=c("person_id","year", "age_start_follow_up")) #left join
    #replace NA in counts_medicines with 0
    vaccines[is.na(counts_vaccines),counts_vaccines:=0]
    
    if(vaccines[,.N]>0){
    if (subpopulations_present=="Yes"){
      saveRDS(vaccines, paste0(vacc_preg_pop, subpopulations_names[s],"/", subpopulations_names[s], "_", names(conditions_files)[j], "_",preg_stage, ".rds" ))
    } else {
      saveRDS(vaccines, paste0(vacc_preg_pop, names(conditions_files)[j], "_",preg_stage, ".rds" ))
      
    }
    }

    rm(vaccines)
    w<-w+1
  }
  
}

###############################################################################################
#EVENTS_MEDICINES    #EVENTS_VACCINES
#############################################################################################
for (i in 1:length(conditions_files)){
  if (subpopulations_present=="Yes"){
    diagnoses<-lapply(paste0(diag_pop, subpopulations_names[s],"/",conditions_files[[i]]), readRDS)
    diagnoses<-do.call(rbind,diagnoses)
    diagnoses<-diagnoses[sex_at_instance_creation=="F" & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,c("person_id","year", "birth_date", "op_end_date","start_follow_up","age_start_follow_up","condition", "truncated_code","event_vocabulary")]
  } else {
    diagnoses<-lapply(paste0(diag_pop, conditions_files[[i]]), readRDS)
    diagnoses<-do.call(rbind,diagnoses)
    diagnoses<-diagnoses[sex_at_instance_creation=="F" & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,c("person_id","year", "birth_date", "op_end_date","start_follow_up","age_start_follow_up","condition", "truncated_code","event_vocabulary")]
  }
  

  #first we aggregate data over year
  colnames_diag<-names(diagnoses)
  diagnoses<-diagnoses[,.N,by=colnames_diag]
  setnames(diagnoses,"N","counts_diagnoses")
  setkey(diagnoses, person_id, year)
  
####################################################
#EVENTS_MEDICINES
###################################################
w<-1
  for (j in 1: length(medicines_files)){
    if (subpopulations_present=="Yes"){
      medicines<-lapply(paste0(medicines_pop, subpopulations_names[s],"/",medicines_files[[j]]), readRDS) #combine all files for one pregnancy stage
    } else {
      medicines<-lapply(paste0(medicines_pop, subpopulations_names[s],"/",medicines_files[[j]]), readRDS) #combine all files for one pregnancy stage
    }
    medicines<-do.call(rbind,medicines)
    
    medicines<-medicines[age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,c("person_id","year", "medicinal_product_atc_code","age_start_follow_up")]
    
    #aggregate data(keep only data for pregnant women, if no pregnancy code in a given year the discard of those observations)
    colnames_medicines<-names(medicines)
    medicines<-medicines[, .N, by=colnames_medicines]
    setnames(medicines, "N", "counts_medicines")
    
    #merge databases
    medicines<-merge(diagnoses,medicines, by=c("person_id","year", "age_start_follow_up")) #left join
    #replace NA in counts_medicines with 0
    medicines[is.na(counts_medicines),counts_medicines:=0]
    
    if(medicines[,.N]>0){
      if (subpopulations_present=="Yes"){
        saveRDS(medicines, paste0(ev_med_pop, subpopulations_names[s],"/", subpopulations_names[s], "_", names(conditions_files)[j], "_",substr(names(medicines_files)[j],1,1), ".rds" ))
      } else {
        saveRDS(medicines, paste0(ev_med_pop, names(conditions_files)[j], "_",substr(names(medicines_files)[j],1,1), ".rds" ))
        
      }
    }
    
    rm(medicines)
    w<-w+1
  }

  ####################################################
  #EVENTS_VACCINES
  ###################################################
w<-1
  for (j in 1: length(vaccines_files)){
    if (subpopulations_present=="Yes"){
      vaccines<-lapply(paste0(vaccines_pop, subpopulations_names[s],"/",vaccines_files[[j]]), readRDS) #combine all files for one pregnancy stage
    } else {
      vaccines<-lapply(paste0(vaccines_pop, subpopulations_names[s],"/",vaccines_files[[j]]), readRDS) #combine all files for one pregnancy stage
    }
    vaccines<-do.call(rbind,vaccines)
    
    vaccines<-vaccines[age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,c("person_id","year", "vx_atc","age_start_follow_up")]
    
    #aggregate data(keep only data for pregnant women, if no pregnancy code in a given year the discard of those observations)
    colnames_vaccines<-names(vaccines)
    vaccines<-vaccines[, .N, by=colnames_vaccines]
    setnames(vaccines, "N", "counts_vaccines")
    
    #merge databases
    vaccines<-merge(diagnoses,vaccines, by=c("person_id","year", "age_start_follow_up")) #left join
    #replace NA in counts_medicines with 0
    vaccines[is.na(counts_vaccines),counts_vaccines:=0]
    
    if(vaccines[,.N]>0){
      if (subpopulations_present=="Yes"){
        saveRDS(vaccines, paste0(ev_vacc_pop, subpopulations_names[s],"/", subpopulations_names[s], "_", names(conditions_files)[j], "_",substr(names(vaccines_files)[j],1,1), ".rds" ))
      } else {
        saveRDS(vaccines, paste0(ev_vacc_pop, names(conditions_files)[j], "_",substr(names(vaccines_files)[j],1,1), ".rds" ))
        
      }
    }
    
    rm(vaccines)
    w<-w+1
  }
}



  