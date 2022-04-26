person_time<-function(x,dt,year_index){
  res_py<-vector(mode="list", length(x))
  names(res_py)<-x
  dt[year_start==x[year_index],year_diff:=exit_spell_category-(start_follow_up+ 1*365 - 365)]
  dt[year_diff>365, year_diff:=365]
  dt[is.na(year_diff), year_diff:=0]
  dt[year_diff<0, year_diff:=0]
  res_py[names(res_py)==x[year_index]]<-sum(dt[["year_diff"]])#only years that start with specific year
  dt[,year_diff:=NULL]
  x<-x[x>=x[year_index]]
  #increment year 1 by 1 and append results
  for(i in 1:length(x)-1){
    dt[year_start==x[year_index],year_diff:=exit_spell_category - as.IDate(paste0(year(start_follow_up)+i, "0101"), format="%Y%m%d")]#gives the start of next year
    dt[year_diff>365, year_diff:=365]
    dt[is.na(year_diff), year_diff:=0]
    dt[year_diff<0, year_diff:=0]
    res_py[names(res_py)==x[i+1]]<-sum(dt[["year_diff"]])#only years that start with specific year
    dt[,year_diff:=NULL]
  }
  res_py<-lapply(res_py,function(y) {
    if(length(y) == 0) {
      0
    } else {
      y
    }
  })
  res_py<-cbind(year=names(res_py), as.data.table(do.call(rbind,res_py)))
  setnames(res_py,"V1","person_years")
  
  return(res_py)
}


############################################
#EVENTS
############################################
if("EVENTS" %in% names(actual_tables)){
  ###################################################
  #List for saving info
  ###################################################
  print("Creating lists to save the information.")
  ################################
  #Flowchart
  ################################
  orig_no_rows_events_preg<-list() #original number of records in the EVENTS table
  events_excluded_meanings_preg<-list() #number of recorded with excluded meanings
  #nr_std number of records in the original study population
  events_excluded_males<-list() #number of records for male subjects
  events_sex_not_specified_preg<-list() #number of records for subjects with unspecified sex
  females_outside_age_events<-list() #number of records where age outside limits
  events_study_pop_preg<-list() #number of records for the study population, no selection criteria for time applied
  events_date_miss_preg<-list() #number of record with missing start_date_record
  events_out_st_per_preg<-list() #number of EVENTS records outside the observation period(check is done on individual level)
  events_study_pop_obsper_preg<-list() #number of records in the study population inside study period
  events_stdpop_no_meaning_preg<-list() #number of records in the study population with no meaning
  events_code_vocabulary_miss_preg<-list() #number of records with both event code and event record vocabulary missing
  events_code_pres_voc_miss_preg<-list() #number of records with missing vocabularies
  events_not_vocabularies_preg<-list() #number of records where event_record_vocabulary not of interest
  empty_event_code_preg<-list()#number of records with empty event code in the study population 
  
  #######################
  #description
  #######################
  #data access provider name(data source name)
  meanings_events_preg<-list() #all meanings present
  years_events_preg<-list() #all years present
  #pers_stdpop_not_events_preg #number of people in the study population without a pregnancy
  #######################

  #############################################################################
  #Description of the dataset
  #############################################################################
  events_study_population_preg<-list() #number of records in the study population
  events_study_pop_m_preg<-list() #number of records in the study population by meaning
  events_study_pop_my_preg<-list() #number of records in the study population by meaning and year
  ###############################
  w<-1
  for (y in 1:length(actual_tables$EVENTS)){
    #Load the table
    df<-fread(paste(path_dir, actual_tables$EVENTS[y], sep=""), stringsAsFactors = FALSE)
    df<-df[,c("person_id", "start_date_record", "event_code", "event_record_vocabulary", "meaning_of_event")]
    df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately
    setnames(df,"meaning_of_event","meaning")
    colnames_events<-names(df)
    std_names_events<-names(study_population)
    colnames_events<-colnames_events[!colnames_events %in% "person_id"]
    #get the total number of records(a)
    orig_no_rows_events_preg[[w]]<-df[,.N]
    #get the total number of records with excluded meanings
    events_excluded_meanings_preg[[w]]<-df[meaning %in% meanings_exclude_events,.N]
    #remove all records for which the meaning is in excluded meanings
    df<-df[meaning %!in% meanings_exclude_events]
    #merge with the study_population table(there is no missing data in this table)
    df<-df[study_population,on=.(person_id)]#left join, keeps all people in the study population even if they didn't have an event
    df<-df[,age_fup:=as.numeric(age_fup)]
    pers_stdpop_not_events_preg<-df[rowSums(is.na(df[,..colnames_events]))==length(colnames_events), ..std_names_events] #subjects id present in the study population but that do not have an event
    pers_stdpop_not_events_preg<-pers_stdpop_not_events_preg[!duplicated(person_id)]
    df<-df[!rowSums(is.na(df[,..colnames_events]))==length(colnames_events)]
    if(pers_stdpop_not_events_preg[,.N]>0){
      saveRDS(pers_stdpop_not_events_preg, paste0(preg_ev_tmp, paste0("stdpop_not_events_preg_", actual_tables$EVENTS[y], ".rds"))) #to give the results we will merge the files by keeping only person_id that are in all saved files
    } else {pers_stdpop_not_events_preg<-NULL}
    rm(pers_stdpop_not_events_preg)
    #number of records for male subjects
    events_excluded_males[[w]]<-df[sex_at_instance_creation=="M",.N]
    #remove males
    df<-df[sex_at_instance_creation != "M"]
    #number of records with unspecified sex
    events_sex_not_specified_preg[[w]]<-df[sex_at_instance_creation =="U" | sex_at_instance_creation == "O",.N] #number of records with unspecified sex
    #remove unspecified sex
    df<-df[sex_at_instance_creation == "F"]#remove unspecified sex
    #number of females outside chidbearing age (based on age at follow up)
    females_outside_age_events[[w]]<-df[age_fup< min_age_preg | age_fup> max_age_preg, .N]
    df<-df[age_fup>= min_age_preg & age_fup<=max_age_preg]
    
    events_study_pop_preg[[w]]<-df[,.N] #number of records for the study population, no selection criteria for time applied
     #transform into date variables
    df[,start_date_record:=as.Date(start_date_record,"%Y%m%d")] #transform to date variables
    #create year variable
    df[,year:=year(start_date_record)]
    #number of records with both start_date_record missing
    events_date_miss_preg[[w]]<-df[is.na(year),.N]
    #remove records with both dates missing
    df<-df[!is.na(year)]
    #remove records that are outside the obs_period for all subjects
    events_out_st_per_preg[[w]]<-df[start_date_record<start_follow_up | start_date_record>exit_spell_category,.N] #number of records outside study population
    df[(start_date_record<start_follow_up | start_date_record>exit_spell_category), obs_out:=1]
    df<-df[is.na(obs_out)] #remove records outside study period
    df[,obs_out:=NULL] 
    events_study_pop_obsper_preg[[w]]<-df[,.N] #number of records after removing records outside study period
    events_stdpop_no_meaning_preg[[w]]<-df[is.na(meaning),.N] #number of records with empty meaning
    df<-df[!is.na(meaning)] #remove records with empty meaning
    events_code_vocabulary_miss_preg[[w]]<-df[is.na(event_code) & is.na(event_record_vocabulary),.N]#numbe rof records with both event code and vocabulary missing
    df<-df[!is.na(event_code) | !is.na(event_record_vocabulary)]# remove records with both event code and event record vocabulary missing
    events_code_pres_voc_miss_preg[[w]]<-df[!is.na(event_code) & is.na(event_record_vocabulary),.N] #number of records where event code present but vocabulary missing
    df<-df[!is.na(event_record_vocabulary)] #remove empty vocabularies
    events_not_vocabularies_preg[[w]]<-df[event_record_vocabulary %!in% vocabularies_list,.N] #number of records where vocabularies doesn't match the codelist
    df<-df[event_record_vocabulary %in% vocabularies_list] #remove records where vocabularies are not of interest
    empty_event_code_preg[[w]]<-df[is.na(event_code),.N] #number of records with empty codes
    df<-df[!is.na(event_code)] #remove records with missing event code
    females_childbearing_preg<-ifelse(df[,.N]!=0,"Yes","No")
    events_study_population_preg[[w]]<-df[,.N] #number of records in the study population(final)
    #########################################################################################
    #description
    #########################################################################################
    meanings_events_preg[[w]]<-unique(na.omit(df[, meaning])) #will be used for description
    years_events_preg[[w]]<-unique(na.omit(df[, year])) #will be used for description

    if(females_childbearing_preg=="Yes"){
    ############################
    #Description of the database
    ###########################
    events_study_pop_m_preg[[w]]<-df[,.N, by="meaning"] #number of records in the study population by meaning
    events_study_pop_my_preg[[w]]<-df[,.N, by=.(meaning,year)] #number of records in the study population by meaning and year
     ##################################################################
    #match codes based on coding system and code: algorithm start with
    #################################################################
    years_study_events_preg<-df[!duplicated(year), year]#years present in this table
    
      if(sum(df[!duplicated(event_record_vocabulary), event_record_vocabulary] %in% c("ICD10CM","ICD9CM", "ICPC2P"))>0){
        for (i in 1:length(stage_pregnancy_start)){
          for(j in 1:length(stage_pregnancy_start[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(stage_pregnancy_start[[i]][[j]][z])), df[["event_code"]]) & event_record_vocabulary==names(stage_pregnancy_start[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(stage_pregnancy_start[[i]][[j]][z])), df[["event_code"]]) & event_record_vocabulary==names(stage_pregnancy_start[[i]])[j],filter_preg:=1]
              }
              z<-z+1
              if(z>length(stage_pregnancy_start[[i]][[j]])){
                break
              }
            }
            if("filter_preg" %!in% names(df)){df[,filter_preg:=0]}
            m<-1
            repeat{
              if(df[filter_preg==1 & year==years_study_events_preg[m],.N]>0){
                saveRDS(data.table(df[filter_preg==1 & year==years_study_events_preg[m]], condition=names(stage_pregnancy_start[i])), paste0(preg_ev_tmp,years_study_events_preg[m],"_", names(stage_pregnancy_start[i]), "_",actual_tables$EVENTS[y], "_start.rds"))
              }
              m<-m+1
              if(m >length(years_study_events_preg)){
                break
              }
            }
            df[,filter_preg:=NULL]
          }
        }
      }
      
      #output to g_intermediate/tmp/EVENTS datasets splitted by condition, year, type of codes(start with:ICD10,ICD10CM,ICPC,ICD9,ICD9CM)
      
      if(sum(df[!duplicated(event_record_vocabulary), event_record_vocabulary] %in% c("RCD2", "RCD"))>0){
        for (i in 1:length(stage_pregnancy_read)){
          for(j in 1:length(stage_pregnancy_read[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(stage_pregnancy_read[[i]][[j]][z])), df[["event_code"]]) & event_record_vocabulary==names(stage_pregnancy_read[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(stage_pregnancy_read[[i]][[j]][z])), df[["event_code"]]) & event_record_vocabulary==names(stage_pregnancy_read[[i]])[j],filter_preg:=1]
              }
              z<-z+1
              if(z>length(stage_pregnancy_read[[i]][[j]])){
                break
              }
            }
            if("filter_preg" %!in% names(df)){df[,filter_preg:=0]}
            m<-1
            repeat{
              if(df[filter_preg==1 & year==years_study_events_preg[m],.N]>0){
                saveRDS(data.table(df[filter_preg==1 & year==years_study_events_preg[m]], condition=names(conditions_read[i])), paste0(preg_ev_tmp,years_study_events_preg[m],"_", names(stage_pregnancy_read[i]), "_",actual_tables$EVENTS[y], "_RCD.rds"))
              }
              m<-m+1
              if(m >length(years_study_events_preg)){
                break
              }
            }
            df[,filter_preg:=NULL]
          }
        }
      }
      #output to g_intermediate/tmp/EVENTS datasets splitted by condition, year, type of codes(start with:Read codes)
      
      if(sum(df[!duplicated(event_record_vocabulary), event_record_vocabulary] %in% c("SNOMEDCT_US"))>0){
        for (i in 1:length(stage_pregnancy_snomed)){
          for(j in 1:length(stage_pregnancy_snomed[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(stage_pregnancy_snomed[[i]][[j]][z])), df[["event_code"]]) & event_record_vocabulary==names(stage_pregnancy_snomed[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(stage_pregnancy_snomed[[i]][[j]][z])), df[["event_code"]]) & event_record_vocabulary==names(stage_pregnancy_snomed[[i]])[j],filter_preg:=1]
              }
              z<-z+1
              if(z>length(stage_pregnancy_snomed[[i]][[j]])){
                break
              }
            }
            if("filter_preg" %!in% names(df)){df[,filter_preg:=0]}
            m<-1
            repeat{
              if(df[filter_preg==1 & year==years_study_events_preg[m],.N]>0){
                saveRDS(data.table(df[filter_preg==1 & year==years_study_events_preg[m]], condition=names(stage_pregnancy_snomed[i])), paste0(preg_ev_tmp,years_study_events_preg[m],"_", names(stage_pregnancy_snomed[i]), "_",actual_tables$EVENTS[y], "_SNOMED.rds"))
              }
              m<-m+1
              if(m >length(years_study_events_preg)){
                break
              }
            }
            df[,filter_preg:=NULL]
          }
        }
      
      #output to g_intermediate/tmp/EVENTS datasets splitted by condition, year, type of codes(exact match: SNOMED)
    }
    
    }
    
    w<-w+1
    rm(df)
  }
  
  #number of subjects in the study population that have not had an event
  stdpop_not_events_preg_files<-list.files(preg_ev_tmp, pattern = "stdpop_not_events_preg")
  if (length(stdpop_not_events_preg_files)>0){
    stdpop_not_events_preg<-readRDS(paste0(preg_ev_tmp, stdpop_not_events_preg_files[1]))
    i<-2
    while(i <= length(stdpop_not_events_preg_files)){
      a<-readRDS(paste0(preg_ev_tmp, stdpop_not_events_preg_files[i]))
      stdpop_not_events_preg<-rbind(stdpop_not_events_preg, a)
      stdpop_not_events_preg<-stdpop_not_events_preg[duplicated(person_id)]
      i<-i+1
      rm(a)
    }
    stdpop_not_events_preg<-stdpop_not_events_preg[,.N]
    
    
    for(i in 1:length(stdpop_not_events_preg_files)){
      unlink(paste0(preg_ev_tmp,stdpop_not_events_preg_files[i]))
    }
    rm(stdpop_not_events_preg_files)
  } else {stdpop_not_events_preg<-0}
  
  
  #################################################################################################
  #Flowchart
  ################################################################################################
  print("Creating flowchart.")
  print("Get number of records in the original table.")
  #original number of records in the EVENTS table(flowchart 1)
  orig_no_rows_events_preg<-do.call(rbind,orig_no_rows_events_preg)
  orig_no_rows_events_preg<-sum(orig_no_rows_events_preg)
  #number of records with excluded meanings(flowchart 2)
  print("Get number of records with excluded meanings.")
  events_excluded_meanings_preg<-do.call(rbind, events_excluded_meanings_preg)
  events_excluded_meanings_preg<-sum(events_excluded_meanings_preg)
  #original number table (flowchart 1)
  #excluded meanings (flowchart 2)
  #original study pop flowchart(3)
  #male excluded(flowchart 4)
  events_excluded_males<-do.call(rbind,events_excluded_males)
  events_excluded_males<-sum(events_excluded_males)
  #females outside age excluded(flowchart 5)
  females_outside_age_events<-do.call(rbind,females_outside_age_events)
  females_outside_age_events<-sum(females_outside_age_events)
  #sex not specified(flowchart 6)
  events_sex_not_specified_preg<-do.call(rbind, events_sex_not_specified_preg)
  events_sex_not_specified_preg<-sum(events_sex_not_specified_preg)
  #number of records for the study population, no selection criteria for time applied (flowchart 7)
  print("Get number of records for the study population (no time criteria applied).")
  events_study_pop_preg<-do.call(rbind,events_study_pop_preg)
  events_study_pop_preg<-sum(events_study_pop_preg)
  #Number of records with date record missing(flowchart 8)
  print("Get number of records with date record missing.")
  events_date_miss_preg<-do.call(rbind,events_date_miss_preg)
  events_date_miss_preg<-sum(events_date_miss_preg)
  #number of medicines records outside the observation period(check is done on individual level) (flowchart 9)
  print("Get number of records outside observation period.")
  events_out_st_per_preg<-do.call(rbind,events_out_st_per_preg) 
  events_out_st_per_preg<-sum(events_out_st_per_preg)
  #number of records in the study population with start_date_record inside study period (flowchart 10)
  print("Get number of records for the study population(time criteria applied).")
  events_study_pop_obsper_preg<-do.call(rbind,events_study_pop_obsper_preg) 
  events_study_pop_obsper_preg<-sum(events_study_pop_obsper_preg)
  #number of records in the study population with no meaning (flowchart 11)
  print("Get number of records with no meaning.")
  events_stdpop_no_meaning_preg<-do.call(rbind,events_stdpop_no_meaning_preg) 
  events_stdpop_no_meaning_preg<-sum(events_stdpop_no_meaning_preg) 
  #Number of records with both code and vocabulary variables missing (flowchart 12)
  print("Get number of records with both code and vocabulary variables missing")
  events_code_vocabulary_miss_preg<-do.call(rbind,events_code_vocabulary_miss_preg)
  events_code_vocabulary_miss_preg<-sum(events_code_vocabulary_miss_preg)
  #Number of records with empty vocabulary when code is present (flowchart 13)
  print("Get number of records with empty vocabulary when code is present")
  events_code_pres_voc_miss_preg<-do.call(rbind,events_code_pres_voc_miss_preg)
  events_code_pres_voc_miss_preg<-sum(events_code_pres_voc_miss_preg)
  #Number of records with vocabularies not in the codelist
  events_not_vocabularies_preg<-do.call(rbind,events_not_vocabularies_preg)
  events_not_vocabularies_preg<-sum(events_not_vocabularies_preg)
  #Number of records with empty codes
  empty_event_code_preg<-do.call(rbind,empty_event_code_preg)
  empty_event_code_preg<-sum(empty_event_code_preg)
  #number of records in the study population (flowchart 14)
  print("Get number of records for study population.")
  events_study_population_preg<-do.call(rbind,events_study_population_preg) 
  events_study_population_preg<-sum(events_study_population_preg) 
  
  flowchart_events_preg<-data.table(INDICATOR=c("Number of records in the original table",
                                                "Exclude:Number of records with excluded meanings",
                                                "Number of subjects in the original study population table",
                                                "Exclude: Number of records for male subjects",
                                                "Exclude: Number of records with unknown or other sex",
                                                "Exclude: Number of records for females younger than 12 years old or older than 55 years old at start of follow up ",
                                                "Exclude: Number of subjects with meanings not of interest(to identify birth registry)",
                                                "Number of records for the study_population(no time criteria)",
                                                "Exclude: Number of records with date record missing",
                                                "Exclude: Number of records with date record outside study period",
                                                "Number of records for the study_population(time criteria applied)",
                                                "Exclude:Number of records with empty meaning",
                                                "Exclude: Number of records with both code and vocabulary variables missing",
                                                "Exclude: Number of records with empty vocabulary when code is present",
                                                "Exclude: Number of records with vocabularies not in the codelist",
                                                "Exclude: Number of records with empty code",
                                                "Number of records for study_population"), 
                                    EVENTS=c(orig_no_rows_events_preg,
                                             events_excluded_meanings_preg,
                                             nr_std,
                                             events_excluded_males,
                                             events_sex_not_specified_preg,
                                             females_outside_age_events,
                                             "N/A",
                                             events_study_pop_preg,
                                             events_date_miss_preg,
                                             events_out_st_per_preg,
                                             events_study_pop_obsper_preg,
                                             events_stdpop_no_meaning_preg,
                                             events_code_vocabulary_miss_preg,
                                             events_code_pres_voc_miss_preg,
                                             events_not_vocabularies_preg,
                                             empty_event_code_preg,
                                             events_study_population_preg))
  
  
  rm(orig_no_rows_events_preg, events_excluded_meanings_preg,events_excluded_males,events_sex_not_specified_preg,
     events_study_pop_preg,females_outside_age_events,events_date_miss_preg,events_out_st_per_preg,
     events_study_pop_obsper_preg,events_stdpop_no_meaning_preg,events_code_vocabulary_miss_preg,events_code_pres_voc_miss_preg,
     events_not_vocabularies_preg,empty_event_code_preg)  
  
  ##################################################################################################
  #Description
  ##################################################################################################
  print("Creating description of study population.")
  #meanings
  print("Get list of meanings.")
  meanings_events_preg<-Filter(length,meanings_events_preg)
  meanings_events_preg<-suppressWarnings(do.call(rbind,meanings_events_preg))
  meanings_events_preg<-unique(c(meanings_events_preg))
  meanings_events_preg_des<-paste(meanings_events_preg, collapse = ", ")
  #study years
  years_events_preg<-Filter(length,years_events_preg)
  years_events_preg<-suppressWarnings(do.call(rbind, years_events_preg))
  years_events_preg<-unique(c(years_events_preg))
  years_events_des_preg<-paste(sort(years_events_preg), collapse=", ")
 

  print("Create description.")
  description_events_preg<-data.table(INDICATOR=c("Data access provider(data source name)",
                                             "List of meanings present",
                                             "Years included in the study period",
                                             "Number of subjects in the study population without a recorded pregnancy"), 
                                 EVENTS=c(paste0(data_access_provider_name,"(",data_source_name,")"),
                                          meanings_events_preg_des,
                                          years_events_des_preg,
                                          stdpop_not_events_preg))
  rm(meanings_events_preg_des,years_events_des_preg,stdpop_not_events_preg)
} else {
  flowchart_events_preg<-data.table(INDICATOR=c("Number of records in the original table",
                                                "Exclude:Number of records with excluded meanings",
                                                "Number of subjects in the original study population table",
                                                "Exclude: Number of records for male subjects",
                                                "Exclude: Number of records with unknown or other sex",
                                                "Exclude: Number of records for females younger than 12 years old or older than 55 years old at start of follow up ",
                                                "Exclude: Number of subjects with meanings not of interest(to identify birth registry)",
                                                "Number of records for the study_population(no time criteria)",
                                                "Exclude: Number of records with date record missing",
                                                "Exclude: Number of records with date record outside study period",
                                                "Number of records for the study_population(time criteria applied)",
                                                "Exclude:Number of records with empty meaning",
                                                "Exclude: Number of records with both code and vocabulary variables missing",
                                                "Exclude: Number of records with empty vocabulary when code is present",
                                                "Exclude: Number of records with vocabularies not in the codelist",
                                                "Exclude: Number of records with empty code",
                                                "Number of records for study_population"), 
                                    EVENTS="N/A")
  
  description_events_preg<-data.table(INDICATOR=c("Data access provider(data source name)",
                                                  "List of meanings present",
                                                  "Years included in the study period",
                                                  "Number of subjects in the study population without a recorded pregnancy"), 
                                      EVENTS="N/A")
}

############################################
#MEDICAL_OBSERVATIONS
###########################################
if("MEDICAL_OBSERVATIONS" %in% names(actual_tables)){
  ###################################################
  #List for saving info
  ###################################################
  print("Creating lists to save the information.")
  ################################
  #Flowchart
  ################################
  orig_no_rows_mo_preg<-list() #original number of records in the MEDICAL_OBSERVATIONS table
  mo_excluded_meanings_preg<-list() #number of recorded with excluded meanings
  #nr_std number of records in the original study population
  mo_excluded_males<-list() #number of records for male subjects
  mo_sex_not_specified_preg<-list() #number of records for subjects with unspecified sex
  females_outside_age_mo<-list() #number of records where age outside limits
  mo_study_pop_preg<-list() #number of records for the study population, no selection criteria for time applied
  mo_date_miss_preg<-list() #number of record with missing mo_date
  mo_out_st_per_preg<-list() #number of MEDICAL_OBSERVATIONS records outside the observation period(check is done on individual level)
  mo_study_pop_obsper_preg<-list() #number of records in the study population inside study period
  mo_stdpop_no_meaning_preg<-list() #number of records in the study population with no meaning
  mo_code_vocabulary_miss_preg<-list() #number of records with both mo code and mo record vocabulary missing
  mo_code_pres_voc_miss_preg<-list() #number of records with missing vocabularies
  mo_not_vocabularies_preg<-list() #number of records where mo_record_vocabulary not of interest
  empty_mo_code_preg<-list()#number of records with empty mo code in the study population 
  
  #######################
  #description
  #######################
  #data access provider name(data source name)
  meanings_mo_preg<-list() #all meanings present
  years_mo_preg<-list() #all years present
  #pers_stdpop_not_mo_preg #number of people in the study population without a pregnancy
  #######################
  
  #############################################################################
  #Description of the dataset
  #############################################################################
  mo_study_population_preg<-list() #number of records in the study population
  mo_study_pop_m_preg<-list() #number of records in the study population by meaning
  mo_study_pop_my_preg<-list() #number of records in the study population by meaning and year
  ###############################
  w<-1
  for (y in 1:length(actual_tables$MEDICAL_OBSERVATIONS)){
    #Load the table
    df<-fread(paste(path_dir, actual_tables$MEDICAL_OBSERVATIONS[y], sep=""), stringsAsFactors = FALSE)
    df<-df[,c("person_id", "mo_date", "mo_code", "mo_record_vocabulary", "mo_meaning")]
    df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately
    setnames(df,"mo_meaning","meaning")
    colnames_mo<-names(df)
    std_names_mo<-names(study_population)
    colnames_mo<-colnames_mo[!colnames_mo %in% "person_id"]
    #get the total number of records(a)
    orig_no_rows_mo_preg[[w]]<-df[,.N]
    #get the total number of records with excluded meanings
    mo_excluded_meanings_preg[[w]]<-df[meaning %in% meanings_exclude_mo,.N]
    #remove all records for which the meaning is in excluded meanings
    df<-df[meaning %!in% meanings_exclude_mo]
    #merge with the study_population table(there is no missing data in this table)
    df<-df[study_population,on=.(person_id)]#left join, keeps all people in the study population even if they didn't have an mo
    df<-df[,age_fup:=as.numeric(age_fup)]
    pers_stdpop_not_mo_preg<-df[rowSums(is.na(df[,..colnames_mo]))==length(colnames_mo), ..std_names_mo] #subjects id present in the study population but that do not have an mo
    pers_stdpop_not_mo_preg<-pers_stdpop_not_mo_preg[!duplicated(person_id)]
    df<-df[!rowSums(is.na(df[,..colnames_mo]))==length(colnames_mo)]
    if(pers_stdpop_not_mo_preg[,.N]>0){
      saveRDS(pers_stdpop_not_mo_preg, paste0(preg_m_tmp, paste0("stdpop_not_mo_preg_", actual_tables$MEDICAL_OBSERVATIONS[y], ".rds"))) #to give the results we will merge the files by keeping only person_id that are in all saved files
    } else {pers_stdpop_not_mo_preg<-NULL}
    rm(pers_stdpop_not_mo_preg)
    #number of records for male subjects
    mo_excluded_males[[w]]<-df[sex_at_instance_creation=="M",.N]
    #remove males
    df<-df[sex_at_instance_creation != "M"]
    #number of records with unspecified sex
    mo_sex_not_specified_preg[[w]]<-df[sex_at_instance_creation =="U" | sex_at_instance_creation == "O",.N] #number of records with unspecified sex
    #remove unspecified sex
    df<-df[sex_at_instance_creation == "F"]#remove unspecified sex
    #number of females outside chidbearing age (based on age at follow up)
    females_outside_age_mo[[w]]<-df[age_fup< min_age_preg | age_fup> max_age_preg, .N]
    df<-df[age_fup>= min_age_preg & age_fup<=max_age_preg]
    
    mo_study_pop_preg[[w]]<-df[,.N] #number of records for the study population, no selection criteria for time applied
    #transform into date variables
    df[,mo_date:=as.Date(mo_date,"%Y%m%d")] #transform to date variables
    #create year variable
    df[,year:=year(mo_date)]
    #number of records with both mo_date missing
    mo_date_miss_preg[[w]]<-df[is.na(year),.N]
    #remove records with both dates missing
    df<-df[!is.na(year)]
    #remove records that are outside the obs_period for all subjects
    mo_out_st_per_preg[[w]]<-df[mo_date<start_follow_up | mo_date>exit_spell_category,.N] #number of records outside study population
    df[(mo_date<start_follow_up | mo_date>exit_spell_category), obs_out:=1]
    df<-df[is.na(obs_out)] #remove records outside study period
    df[,obs_out:=NULL] 
    mo_study_pop_obsper_preg[[w]]<-df[,.N] #number of records after removing records outside study period
    mo_stdpop_no_meaning_preg[[w]]<-df[is.na(meaning),.N] #number of records with empty meaning
    df<-df[!is.na(meaning)] #remove records with empty meaning
    mo_code_vocabulary_miss_preg[[w]]<-df[is.na(mo_code) & is.na(mo_record_vocabulary),.N]#numbe rof records with both mo code and vocabulary missing
    df<-df[!is.na(mo_code) | !is.na(mo_record_vocabulary)]# remove records with both mo code and mo record vocabulary missing
    mo_code_pres_voc_miss_preg[[w]]<-df[!is.na(mo_code) & is.na(mo_record_vocabulary),.N] #number of records where mo code present but vocabulary missing
    df<-df[!is.na(mo_record_vocabulary)] #remove empty vocabularies
    mo_not_vocabularies_preg[[w]]<-df[mo_record_vocabulary %!in% vocabularies_list,.N] #number of records where vocabularies doesn't match the codelist
    df<-df[mo_record_vocabulary %in% vocabularies_list] #remove records where vocabularies are not of interest
    empty_mo_code_preg[[w]]<-df[is.na(mo_code),.N] #number of records with empty codes
    df<-df[!is.na(mo_code)] #remove records with missing mo code
    females_childbearing_preg<-ifelse(df[,.N]!=0,"Yes","No")
    mo_study_population_preg[[w]]<-df[,.N] #number of records in the study population(final)
    #########################################################################################
    #description
    #########################################################################################
    meanings_mo_preg[[w]]<-unique(na.omit(df[, meaning])) #will be used for description
    years_mo_preg[[w]]<-unique(na.omit(df[, year])) #will be used for description
    
    if(females_childbearing_preg=="Yes"){
      ############################
      #Description of the database
      ###########################
      mo_study_pop_m_preg[[w]]<-df[,.N, by="meaning"] #number of records in the study population by meaning
      mo_study_pop_my_preg[[w]]<-df[,.N, by=.(meaning,year)] #number of records in the study population by meaning and year
      ##################################################################
      #match codes based on coding system and code: algorithm start with
      #################################################################
      years_study_mo_preg<-df[!duplicated(year), year]#years present in this table
      
      if(sum(df[!duplicated(mo_record_vocabulary), mo_record_vocabulary] %in% c("ICD10CM","ICD9CM", "ICPC2P"))>0){
        for (i in 1:length(stage_pregnancy_start)){
          for(j in 1:length(stage_pregnancy_start[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(stage_pregnancy_start[[i]][[j]][z])), df[["mo_code"]]) & mo_record_vocabulary==names(stage_pregnancy_start[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(stage_pregnancy_start[[i]][[j]][z])), df[["mo_code"]]) & mo_record_vocabulary==names(stage_pregnancy_start[[i]])[j],filter_preg:=1]
              }
              z<-z+1
              if(z>length(stage_pregnancy_start[[i]][[j]])){
                break
              }
            }
            if("filter_preg" %!in% names(df)){df[,filter_preg:=0]}
            m<-1
            repeat{
              if(df[filter_preg==1 & year==years_study_mo_preg[m],.N]>0){
                saveRDS(data.table(df[filter_preg==1 & year==years_study_mo_preg[m]], condition=names(stage_pregnancy_start[i])), paste0(preg_m_tmp,years_study_mo_preg[m],"_", names(stage_pregnancy_start[i]), "_",actual_tables$MEDICAL_OBSERVATIONS[y], "_start.rds"))
              }
              m<-m+1
              if(m >length(years_study_mo_preg)){
                break
              }
            }
            df[,filter_preg:=NULL]
          }
        }
      }
      
      #output to g_intermediate/tmp/MEDICAL_OBSERVATIONS datasets splitted by condition, year, type of codes(start with:ICD10,ICD10CM,ICPC,ICD9,ICD9CM)
      
      if(sum(df[!duplicated(mo_record_vocabulary), mo_record_vocabulary] %in% c("RCD2", "RCD"))>0){
        for (i in 1:length(stage_pregnancy_read)){
          for(j in 1:length(stage_pregnancy_read[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(stage_pregnancy_read[[i]][[j]][z])), df[["mo_code"]]) & mo_record_vocabulary==names(stage_pregnancy_read[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(stage_pregnancy_read[[i]][[j]][z])), df[["mo_code"]]) & mo_record_vocabulary==names(stage_pregnancy_read[[i]])[j],filter_preg:=1]
              }
              z<-z+1
              if(z>length(stage_pregnancy_read[[i]][[j]])){
                break
              }
            }
            if("filter_preg" %!in% names(df)){df[,filter_preg:=0]}
            m<-1
            repeat{
              if(df[filter_preg==1 & year==years_study_mo_preg[m],.N]>0){
                saveRDS(data.table(df[filter_preg==1 & year==years_study_mo_preg[m]], condition=names(conditions_read[i])), paste0(preg_m_tmp,years_study_mo_preg[m],"_", names(stage_pregnancy_read[i]), "_",actual_tables$MEDICAL_OBSERVATIONS[y], "_RCD.rds"))
              }
              m<-m+1
              if(m >length(years_study_mo_preg)){
                break
              }
            }
            df[,filter_preg:=NULL]
          }
        }
      }
      #output to g_intermediate/tmp/MEDICAL_OBSERVATIONS datasets splitted by condition, year, type of codes(start with:Read codes)
      
      if(sum(df[!duplicated(mo_record_vocabulary), mo_record_vocabulary] %in% c("SNOMEDCT_US"))>0){
        for (i in 1:length(stage_pregnancy_snomed)){
          for(j in 1:length(stage_pregnancy_snomed[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(stage_pregnancy_snomed[[i]][[j]][z])), df[["mo_code"]]) & mo_record_vocabulary==names(stage_pregnancy_snomed[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(stage_pregnancy_snomed[[i]][[j]][z])), df[["mo_code"]]) & mo_record_vocabulary==names(stage_pregnancy_snomed[[i]])[j],filter_preg:=1]
              }
              z<-z+1
              if(z>length(stage_pregnancy_snomed[[i]][[j]])){
                break
              }
            }
            if("filter_preg" %!in% names(df)){df[,filter_preg:=0]}
            m<-1
            repeat{
              if(df[filter_preg==1 & year==years_study_mo_preg[m],.N]>0){
                saveRDS(data.table(df[filter_preg==1 & year==years_study_mo_preg[m]], condition=names(stage_pregnancy_snomed[i])), paste0(preg_m_tmp,years_study_mo_preg[m],"_", names(stage_pregnancy_snomed[i]), "_",actual_tables$MEDICAL_OBSERVATIONS[y], "_SNOMED.rds"))
              }
              m<-m+1
              if(m >length(years_study_mo_preg)){
                break
              }
            }
            df[,filter_preg:=NULL]
          }
        }
        
        #output to g_intermediate/tmp/MEDICAL_OBSERVATIONS datasets splitted by condition, year, type of codes(exact match: SNOMED)
      }
      
    }
    
    w<-w+1
    rm(df)
  }
  
  #number of subjects in the study population that have not had an mo
  stdpop_not_mo_preg_files<-list.files(preg_m_tmp, pattern = "stdpop_not_mo_preg")
  if (length(stdpop_not_mo_preg_files)>0){
    stdpop_not_mo_preg<-readRDS(paste0(preg_m_tmp, stdpop_not_mo_preg_files[1]))
    i<-2
    while(i <= length(stdpop_not_mo_preg_files)){
      a<-readRDS(paste0(preg_m_tmp, stdpop_not_mo_preg_files[i]))
      stdpop_not_mo_preg<-rbind(stdpop_not_mo_preg, a)
      stdpop_not_mo_preg<-stdpop_not_mo_preg[duplicated(person_id)]
      i<-i+1
      rm(a)
    }
    stdpop_not_mo_preg<-stdpop_not_mo_preg[,.N]
    
    
    for(i in 1:length(stdpop_not_mo_preg_files)){
      unlink(paste0(preg_m_tmp,stdpop_not_mo_preg_files[i]))
    }
    rm(stdpop_not_mo_preg_files)
  } else {stdpop_not_mo_preg<-0}
  
  
  #################################################################################################
  #Flowchart
  ################################################################################################
  print("Creating flowchart.")
  print("Get number of records in the original table.")
  #original number of records in the MEDICAL_OBSERVATIONS table(flowchart 1)
  orig_no_rows_mo_preg<-do.call(rbind,orig_no_rows_mo_preg)
  orig_no_rows_mo_preg<-sum(orig_no_rows_mo_preg)
  #number of records with excluded meanings(flowchart 2)
  print("Get number of records with excluded meanings.")
  mo_excluded_meanings_preg<-do.call(rbind, mo_excluded_meanings_preg)
  mo_excluded_meanings_preg<-sum(mo_excluded_meanings_preg)
  #original number table (flowchart 1)
  #excluded meanings (flowchart 2)
  #original study pop flowchart(3)
  #male excluded(flowchart 4)
  mo_excluded_males<-do.call(rbind,mo_excluded_males)
  mo_excluded_males<-sum(mo_excluded_males)
  #females outside age excluded(flowchart 5)
  females_outside_age_mo<-do.call(rbind,females_outside_age_mo)
  females_outside_age_mo<-sum(females_outside_age_mo)
  #sex not specified(flowchart 6)
  mo_sex_not_specified_preg<-do.call(rbind, mo_sex_not_specified_preg)
  mo_sex_not_specified_preg<-sum(mo_sex_not_specified_preg)
  #number of records for the study population, no selection criteria for time applied (flowchart 7)
  print("Get number of records for the study population (no time criteria applied).")
  mo_study_pop_preg<-do.call(rbind,mo_study_pop_preg)
  mo_study_pop_preg<-sum(mo_study_pop_preg)
  #Number of records with date record missing(flowchart 8)
  print("Get number of records with date record missing.")
  mo_date_miss_preg<-do.call(rbind,mo_date_miss_preg)
  mo_date_miss_preg<-sum(mo_date_miss_preg)
  #number of medicines records outside the observation period(check is done on individual level) (flowchart 9)
  print("Get number of records outside observation period.")
  mo_out_st_per_preg<-do.call(rbind,mo_out_st_per_preg) 
  mo_out_st_per_preg<-sum(mo_out_st_per_preg)
  #number of records in the study population with mo_date inside study period (flowchart 10)
  print("Get number of records for the study population(time criteria applied).")
  mo_study_pop_obsper_preg<-do.call(rbind,mo_study_pop_obsper_preg) 
  mo_study_pop_obsper_preg<-sum(mo_study_pop_obsper_preg)
  #number of records in the study population with no meaning (flowchart 11)
  print("Get number of records with no meaning.")
  mo_stdpop_no_meaning_preg<-do.call(rbind,mo_stdpop_no_meaning_preg) 
  mo_stdpop_no_meaning_preg<-sum(mo_stdpop_no_meaning_preg) 
  #Number of records with both code and vocabulary variables missing (flowchart 12)
  print("Get number of records with both code and vocabulary variables missing")
  mo_code_vocabulary_miss_preg<-do.call(rbind,mo_code_vocabulary_miss_preg)
  mo_code_vocabulary_miss_preg<-sum(mo_code_vocabulary_miss_preg)
  #Number of records with empty vocabulary when code is present (flowchart 13)
  print("Get number of records with empty vocabulary when code is present")
  mo_code_pres_voc_miss_preg<-do.call(rbind,mo_code_pres_voc_miss_preg)
  mo_code_pres_voc_miss_preg<-sum(mo_code_pres_voc_miss_preg)
  #Number of records with vocabularies not in the codelist
  mo_not_vocabularies_preg<-do.call(rbind,mo_not_vocabularies_preg)
  mo_not_vocabularies_preg<-sum(mo_not_vocabularies_preg)
  #Number of records with empty codes
  empty_mo_code_preg<-do.call(rbind,empty_mo_code_preg)
  empty_mo_code_preg<-sum(empty_mo_code_preg)
  #number of records in the study population (flowchart 14)
  print("Get number of records for study population.")
  mo_study_population_preg<-do.call(rbind,mo_study_population_preg) 
  mo_study_population_preg<-sum(mo_study_population_preg) 
  
  flowchart_mo_preg<-data.table(INDICATOR=c("Number of records in the original table",
                                            "Exclude:Number of records with excluded meanings",
                                            "Number of subjects in the original study population table",
                                            "Exclude: Number of records for male subjects",
                                            "Exclude: Number of records with unknown or other sex",
                                            "Exclude: Number of records for females younger than 12 years old or older than 55 years old at start of follow up ",
                                            "Exclude: Number of subjects with meanings not of interest(to identify birth registry)",
                                            "Number of records for the study_population(no time criteria)",
                                            "Exclude: Number of records with date record missing",
                                            "Exclude: Number of records with date record outside study period",
                                            "Number of records for the study_population(time criteria applied)",
                                            "Exclude:Number of records with empty meaning",
                                            "Exclude: Number of records with both code and vocabulary variables missing",
                                            "Exclude: Number of records with empty vocabulary when code is present",
                                            "Exclude: Number of records with vocabularies not in the codelist",
                                            "Exclude: Number of records with empty code",
                                            "Number of records for study_population"), 
                                MEDICAL_OBSERVATIONS=c(orig_no_rows_mo_preg,
                                                       mo_excluded_meanings_preg,
                                                       nr_std,
                                                       mo_excluded_males,
                                                       mo_sex_not_specified_preg,
                                                       females_outside_age_mo,
                                                       "N/A",
                                                       mo_study_pop_preg,
                                                       mo_date_miss_preg,
                                                       mo_out_st_per_preg,
                                                       mo_study_pop_obsper_preg,
                                                       mo_stdpop_no_meaning_preg,
                                                       mo_code_vocabulary_miss_preg,
                                                       mo_code_pres_voc_miss_preg,
                                                       mo_not_vocabularies_preg,
                                                       empty_mo_code_preg,
                                                       mo_study_population_preg))
  
  
  rm(orig_no_rows_mo_preg, mo_excluded_meanings_preg,mo_excluded_males,mo_sex_not_specified_preg,
     mo_study_pop_preg,females_outside_age_mo,mo_date_miss_preg,mo_out_st_per_preg,
     mo_study_pop_obsper_preg,mo_stdpop_no_meaning_preg,mo_code_vocabulary_miss_preg,mo_code_pres_voc_miss_preg,
     mo_not_vocabularies_preg,empty_mo_code_preg)  
  
  ##################################################################################################
  #Description
  ##################################################################################################
  print("Creating description of study population.")
  #meanings
  print("Get list of meanings.")
  meanings_mo_preg<-Filter(length,meanings_mo_preg)
  meanings_mo_preg<-suppressWarnings(do.call(rbind,meanings_mo_preg))
  meanings_mo_preg<-unique(c(meanings_mo_preg))
  meanings_mo_preg_des<-paste(meanings_mo_preg, collapse = ", ")
  #study years
  years_mo_preg<-Filter(length,years_mo_preg)
  years_mo_preg<-suppressWarnings(do.call(rbind, years_mo_preg))
  years_mo_preg<-unique(c(years_mo_preg))
  years_mo_des_preg<-paste(sort(years_mo_preg), collapse=", ")
  
  
  print("Create description.")
  description_mo_preg<-data.table(INDICATOR=c("Data access provider(data source name)",
                                              "List of meanings present",
                                              "Years included in the study period",
                                              "Number of subjects in the study population without a recorded pregnancy"), 
                                  MEDICAL_OBSERVATIONS=c(paste0(data_access_provider_name,"(",data_source_name,")"),
                                                         meanings_mo_preg_des,
                                                         years_mo_des_preg,
                                                         stdpop_not_mo_preg))
  rm(meanings_mo_preg_des,years_mo_des_preg,stdpop_not_mo_preg)
} else {
  flowchart_mo_preg<-data.table(INDICATOR=c("Number of records in the original table",
                                            "Exclude:Number of records with excluded meanings",
                                            "Number of subjects in the original study population table",
                                            "Exclude: Number of records for male subjects",
                                            "Exclude: Number of records with unknown or other sex",
                                            "Exclude: Number of records for females younger than 12 years old or older than 55 years old at start of follow up ",
                                            "Exclude: Number of subjects with meanings not of interest(to identify birth registry)",
                                            "Number of records for the study_population(no time criteria)",
                                            "Exclude: Number of records with date record missing",
                                            "Exclude: Number of records with date record outside study period",
                                            "Number of records for the study_population(time criteria applied)",
                                            "Exclude:Number of records with empty meaning",
                                            "Exclude: Number of records with both code and vocabulary variables missing",
                                            "Exclude: Number of records with empty vocabulary when code is present",
                                            "Exclude: Number of records with vocabularies not in the codelist",
                                            "Exclude: Number of records with empty code",
                                            "Number of records for study_population"), 
                                MEDICAL_OBSERVATIONS="N/A")
  
  description_mo_preg<-data.table(INDICATOR=c("Data access provider(data source name)",
                                              "List of meanings present",
                                              "Years included in the study period",
                                              "Number of subjects in the study population without a recorded pregnancy"), 
                                  MEDICAL_OBSERVATIONS="N/A")

}

############################################
#SURVEY_OBSERVATIONS
###########################################
if("SURVEY_OBSERVATIONS" %in% names(actual_tables)){
  ###################################################
  #List for saving info
  ###################################################
  print("Creating lists to save the information.")
  ################################
  #Flowchart
  ################################
  orig_no_rows_so_preg<-list() #original number of records in the SURVEY_OBSERVATIONS table
  so_excluded_meanings_preg<-list() #number of recorded with excluded meanings
  #nr_std number of records in the original study population
  so_excluded_males<-list() #number of records for male subjects
  so_sex_not_specified_preg<-list() #number of records for subjects with unspecified sex
  females_outside_age_so<-list() #number of records where age outside limits
  so_study_pop_preg<-list() #number of records for the study population, no selection criteria for time applied
  so_date_miss_preg<-list() #number of record with missing so_date
  so_out_st_per_preg<-list() #number of SURVEY_OBSERVATIONS records outside the observation period(check is done on individual level)
  so_study_pop_obsper_preg<-list() #number of records in the study population inside study period
  so_stdpop_no_meaning_preg<-list() #number of records in the study population with no meaning
  so_source_value_vocabulary_miss_preg<-list() #number of records with both mo code and mo record vocabulary missing
  so_source_value_pres_voc_miss_preg<-list() #number of records with missing vocabularies
  so_not_vocabularies_preg<-list() #number of records where so_unit not of interest
  empty_so_source_value_preg<-list()#number of records with empty mo code in the study population 
  
  #######################
  #description
  #######################
  #data access provider name(data source name)
  meanings_so_preg<-list() #all meanings present
  years_so_preg<-list() #all years present
  #pers_stdpop_not_so_preg #number of people in the study population without a pregnancy
  #######################
  
  #############################################################################
  #Description of the dataset
  #############################################################################
  so_study_population_preg<-list() #number of records in the study population
  so_study_pop_m_preg<-list() #number of records in the study population by meaning
  so_study_pop_my_preg<-list() #number of records in the study population by meaning and year
  ###############################
  w<-1
  for (y in 1:length(actual_tables$SURVEY_OBSERVATIONS)){
    #Load the table
    df<-fread(paste(path_dir, actual_tables$SURVEY_OBSERVATIONS[y], sep=""), stringsAsFactors = FALSE)
    df<-df[,c("person_id", "so_date", "so_source_value", "so_unit", "so_meaning")]
    df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately
    setnames(df,"so_meaning","meaning")
    colnames_so<-names(df)
    std_names_so<-names(study_population)
    colnames_so<-colnames_so[!colnames_so %in% "person_id"]
    #get the total number of records(a)
    orig_no_rows_so_preg[[w]]<-df[,.N]
    #get the total number of records with excluded meanings
    so_excluded_meanings_preg[[w]]<-df[meaning %in% meanings_exclude_so,.N]
    #remove all records for which the meaning is in excluded meanings
    df<-df[meaning %!in% meanings_exclude_so]
    #merge with the study_population table(there is no missing data in this table)
    df<-df[study_population,on=.(person_id)]#left join, keeps all people in the study population even if they didn't have an mo
    df<-df[,age_fup:=as.numeric(age_fup)]
    pers_stdpop_not_so_preg<-df[rowSums(is.na(df[,..colnames_so]))==length(colnames_so), ..std_names_so] #subjects id present in the study population but that do not have an mo
    pers_stdpop_not_so_preg<-pers_stdpop_not_so_preg[!duplicated(person_id)]
    df<-df[!rowSums(is.na(df[,..colnames_so]))==length(colnames_so)]
    if(pers_stdpop_not_so_preg[,.N]>0){
      saveRDS(pers_stdpop_not_so_preg, paste0(preg_s_tmp, paste0("stdpop_not_so_preg_", actual_tables$SURVEY_OBSERVATIONS[y], ".rds"))) #to give the results we will merge the files by keeping only person_id that are in all saved files
    } else {pers_stdpop_not_so_preg<-NULL}
    rm(pers_stdpop_not_so_preg)
    #number of records for male subjects
    so_excluded_males[[w]]<-df[sex_at_instance_creation=="M",.N]
    #remove males
    df<-df[sex_at_instance_creation != "M"]
    #number of records with unspecified sex
    so_sex_not_specified_preg[[w]]<-df[sex_at_instance_creation =="U" | sex_at_instance_creation == "O",.N] #number of records with unspecified sex
    #remove unspecified sex
    df<-df[sex_at_instance_creation == "F"]#remove unspecified sex
    #number of females outside chidbearing age (based on age at follow up)
    females_outside_age_so[[w]]<-df[age_fup< min_age_preg | age_fup> max_age_preg, .N]
    df<-df[age_fup>= min_age_preg & age_fup<=max_age_preg]
    
    so_study_pop_preg[[w]]<-df[,.N] #number of records for the study population, no selection criteria for time applied
    #transform into date variables
    df[,so_date:=as.Date(so_date,"%Y%m%d")] #transform to date variables
    #create year variable
    df[,year:=year(so_date)]
    #number of records with both so_date missing
    so_date_miss_preg[[w]]<-df[is.na(year),.N]
    #remove records with both dates missing
    df<-df[!is.na(year)]
    #remove records that are outside the obs_period for all subjects
    so_out_st_per_preg[[w]]<-df[so_date<start_follow_up | so_date>exit_spell_category,.N] #number of records outside study population
    df[(so_date<start_follow_up | so_date>exit_spell_category), obs_out:=1]
    df<-df[is.na(obs_out)] #remove records outside study period
    df[,obs_out:=NULL] 
    so_study_pop_obsper_preg[[w]]<-df[,.N] #number of records after removing records outside study period
    so_stdpop_no_meaning_preg[[w]]<-df[is.na(meaning),.N] #number of records with empty meaning
    df<-df[!is.na(meaning)] #remove records with empty meaning
    so_source_value_vocabulary_miss_preg[[w]]<-df[is.na(so_source_value) & is.na(so_unit),.N]#numbe rof records with both mo code and vocabulary missing
    df<-df[!is.na(so_source_value) | !is.na(so_unit)]# remove records with both mo code and mo record vocabulary missing
    so_source_value_pres_voc_miss_preg[[w]]<-df[!is.na(so_source_value) & is.na(so_unit),.N] #number of records where mo code present but vocabulary missing
    df<-df[!is.na(so_unit)] #remove empty vocabularies
    so_not_vocabularies_preg[[w]]<-df[so_unit %!in% vocabularies_list,.N] #number of records where vocabularies doesn't match the codelist
    df<-df[so_unit %in% vocabularies_list] #remove records where vocabularies are not of interest
    empty_so_source_value_preg[[w]]<-df[is.na(so_source_value),.N] #number of records with empty codes
    df<-df[!is.na(so_source_value)] #remove records with missing mo code
    females_childbearing_preg<-ifelse(df[,.N]!=0,"Yes","No")
    so_study_population_preg[[w]]<-df[,.N] #number of records in the study population(final)
    #########################################################################################
    #description
    #########################################################################################
    meanings_so_preg[[w]]<-unique(na.omit(df[, meaning])) #will be used for description
    years_so_preg[[w]]<-unique(na.omit(df[, year])) #will be used for description
    
    if(females_childbearing_preg=="Yes"){
      ############################
      #Description of the database
      ###########################
      so_study_pop_m_preg[[w]]<-df[,.N, by="meaning"] #number of records in the study population by meaning
      so_study_pop_my_preg[[w]]<-df[,.N, by=.(meaning,year)] #number of records in the study population by meaning and year
      ##################################################################
      #match codes based on coding system and code: algorithm start with
      #################################################################
      years_study_so_preg<-df[!duplicated(year), year]#years present in this table
      
      if(sum(df[!duplicated(so_unit), so_unit] %in% c("ICD10CM","ICD9CM", "ICPC2P"))>0){
        for (i in 1:length(stage_pregnancy_start)){
          for(j in 1:length(stage_pregnancy_start[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(stage_pregnancy_start[[i]][[j]][z])), df[["so_source_value"]]) & so_unit==names(stage_pregnancy_start[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(stage_pregnancy_start[[i]][[j]][z])), df[["so_source_value"]]) & so_unit==names(stage_pregnancy_start[[i]])[j],filter_preg:=1]
              }
              z<-z+1
              if(z>length(stage_pregnancy_start[[i]][[j]])){
                break
              }
            }
            if("filter_preg" %!in% names(df)){df[,filter_preg:=0]}
            m<-1
            repeat{
              if(df[filter_preg==1 & year==years_study_so_preg[m],.N]>0){
                saveRDS(data.table(df[filter_preg==1 & year==years_study_so_preg[m]], condition=names(stage_pregnancy_start[i])), paste0(preg_s_tmp,years_study_so_preg[m],"_", names(stage_pregnancy_start[i]), "_",actual_tables$SURVEY_OBSERVATIONS[y], "_start.rds"))
              }
              m<-m+1
              if(m >length(years_study_so_preg)){
                break
              }
            }
            df[,filter_preg:=NULL]
          }
        }
      }
      
      #output to g_intermediate/tmp/SURVEY_OBSERVATIONS datasets splitted by condition, year, type of codes(start with:ICD10,ICD10CM,ICPC,ICD9,ICD9CM)
      
      if(sum(df[!duplicated(so_unit), so_unit] %in% c("RCD2", "RCD"))>0){
        for (i in 1:length(stage_pregnancy_read)){
          for(j in 1:length(stage_pregnancy_read[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(stage_pregnancy_read[[i]][[j]][z])), df[["so_source_value"]]) & so_unit==names(stage_pregnancy_read[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(stage_pregnancy_read[[i]][[j]][z])), df[["so_source_value"]]) & so_unit==names(stage_pregnancy_read[[i]])[j],filter_preg:=1]
              }
              z<-z+1
              if(z>length(stage_pregnancy_read[[i]][[j]])){
                break
              }
            }
            if("filter_preg" %!in% names(df)){df[,filter_preg:=0]}
            m<-1
            repeat{
              if(df[filter_preg==1 & year==years_study_so_preg[m],.N]>0){
                saveRDS(data.table(df[filter_preg==1 & year==years_study_so_preg[m]], condition=names(conditions_read[i])), paste0(preg_s_tmp,years_study_so_preg[m],"_", names(stage_pregnancy_read[i]), "_",actual_tables$SURVEY_OBSERVATIONS[y], "_RCD.rds"))
              }
              m<-m+1
              if(m >length(years_study_so_preg)){
                break
              }
            }
            df[,filter_preg:=NULL]
          }
        }
      }
      #output to g_intermediate/tmp/SURVEY_OBSERVATIONS datasets splitted by condition, year, type of codes(start with:Read codes)
      
      if(sum(df[!duplicated(so_unit), so_unit] %in% c("SNOMEDCT_US"))>0){
        for (i in 1:length(stage_pregnancy_snomed)){
          for(j in 1:length(stage_pregnancy_snomed[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(stage_pregnancy_snomed[[i]][[j]][z])), df[["so_source_value"]]) & so_unit==names(stage_pregnancy_snomed[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(stage_pregnancy_snomed[[i]][[j]][z])), df[["so_source_value"]]) & so_unit==names(stage_pregnancy_snomed[[i]])[j],filter_preg:=1]
              }
              z<-z+1
              if(z>length(stage_pregnancy_snomed[[i]][[j]])){
                break
              }
            }
            if("filter_preg" %!in% names(df)){df[,filter_preg:=0]}
            m<-1
            repeat{
              if(df[filter_preg==1 & year==years_study_so_preg[m],.N]>0){
                saveRDS(data.table(df[filter_preg==1 & year==years_study_so_preg[m]], condition=names(stage_pregnancy_snomed[i])), paste0(preg_s_tmp,years_study_so_preg[m],"_", names(stage_pregnancy_snomed[i]), "_",actual_tables$SURVEY_OBSERVATIONS[y], "_SNOMED.rds"))
              }
              m<-m+1
              if(m >length(years_study_so_preg)){
                break
              }
            }
            df[,filter_preg:=NULL]
          }
        }
        
        #output to g_intermediate/tmp/SURVEY_OBSERVATIONS datasets splitted by condition, year, type of codes(exact match: SNOMED)
      }
      
    }
    
    w<-w+1
    rm(df)
  }
  
  #number of subjects in the study population that have not had an mo
  stdpop_not_so_preg_files<-list.files(preg_s_tmp, pattern = "stdpop_not_so_preg")
  if (length(stdpop_not_so_preg_files)>0){
    stdpop_not_so_preg<-readRDS(paste0(preg_s_tmp, stdpop_not_so_preg_files[1]))
    i<-2
    while(i <= length(stdpop_not_so_preg_files)){
      a<-readRDS(paste0(preg_s_tmp, stdpop_not_so_preg_files[i]))
      stdpop_not_so_preg<-rbind(stdpop_not_so_preg, a)
      stdpop_not_so_preg<-stdpop_not_so_preg[duplicated(person_id)]
      i<-i+1
      rm(a)
    }
    stdpop_not_so_preg<-stdpop_not_so_preg[,.N]
    
    
    for(i in 1:length(stdpop_not_so_preg_files)){
      unlink(paste0(preg_s_tmp,stdpop_not_so_preg_files[i]))
    }
    rm(stdpop_not_so_preg_files)
  } else {stdpop_not_so_preg<-0}
  
  
  #################################################################################################
  #Flowchart
  ################################################################################################
  print("Creating flowchart.")
  print("Get number of records in the original table.")
  #original number of records in the SURVEY_OBSERVATIONS table(flowchart 1)
  orig_no_rows_so_preg<-do.call(rbind,orig_no_rows_so_preg)
  orig_no_rows_so_preg<-sum(orig_no_rows_so_preg)
  #number of records with excluded meanings(flowchart 2)
  print("Get number of records with excluded meanings.")
  so_excluded_meanings_preg<-do.call(rbind, so_excluded_meanings_preg)
  so_excluded_meanings_preg<-sum(so_excluded_meanings_preg)
  #original number table (flowchart 1)
  #excluded meanings (flowchart 2)
  #original study pop flowchart(3)
  #male excluded(flowchart 4)
  so_excluded_males<-do.call(rbind,so_excluded_males)
  so_excluded_males<-sum(so_excluded_males)
  #females outside age excluded(flowchart 5)
  females_outside_age_so<-do.call(rbind,females_outside_age_so)
  females_outside_age_so<-sum(females_outside_age_so)
  #sex not specified(flowchart 6)
  so_sex_not_specified_preg<-do.call(rbind, so_sex_not_specified_preg)
  so_sex_not_specified_preg<-sum(so_sex_not_specified_preg)
  #number of records for the study population, no selection criteria for time applied (flowchart 7)
  print("Get number of records for the study population (no time criteria applied).")
  so_study_pop_preg<-do.call(rbind,so_study_pop_preg)
  so_study_pop_preg<-sum(so_study_pop_preg)
  #Number of records with date record missing(flowchart 8)
  print("Get number of records with date record missing.")
  so_date_miss_preg<-do.call(rbind,so_date_miss_preg)
  so_date_miss_preg<-sum(so_date_miss_preg)
  #number of medicines records outside the observation period(check is done on individual level) (flowchart 9)
  print("Get number of records outside observation period.")
  so_out_st_per_preg<-do.call(rbind,so_out_st_per_preg) 
  so_out_st_per_preg<-sum(so_out_st_per_preg)
  #number of records in the study population with so_date inside study period (flowchart 10)
  print("Get number of records for the study population(time criteria applied).")
  so_study_pop_obsper_preg<-do.call(rbind,so_study_pop_obsper_preg) 
  so_study_pop_obsper_preg<-sum(so_study_pop_obsper_preg)
  #number of records in the study population with no meaning (flowchart 11)
  print("Get number of records with no meaning.")
  so_stdpop_no_meaning_preg<-do.call(rbind,so_stdpop_no_meaning_preg) 
  so_stdpop_no_meaning_preg<-sum(so_stdpop_no_meaning_preg) 
  #Number of records with both code and vocabulary variables missing (flowchart 12)
  print("Get number of records with both code and vocabulary variables missing")
  so_source_value_vocabulary_miss_preg<-do.call(rbind,so_source_value_vocabulary_miss_preg)
  so_source_value_vocabulary_miss_preg<-sum(so_source_value_vocabulary_miss_preg)
  #Number of records with empty vocabulary when code is present (flowchart 13)
  print("Get number of records with empty vocabulary when code is present")
  so_source_value_pres_voc_miss_preg<-do.call(rbind,so_source_value_pres_voc_miss_preg)
  so_source_value_pres_voc_miss_preg<-sum(so_source_value_pres_voc_miss_preg)
  #Number of records with vocabularies not in the codelist
  so_not_vocabularies_preg<-do.call(rbind,so_not_vocabularies_preg)
  so_not_vocabularies_preg<-sum(so_not_vocabularies_preg)
  #Number of records with empty codes
  empty_so_source_value_preg<-do.call(rbind,empty_so_source_value_preg)
  empty_so_source_value_preg<-sum(empty_so_source_value_preg)
  #number of records in the study population (flowchart 14)
  print("Get number of records for study population.")
  so_study_population_preg<-do.call(rbind,so_study_population_preg) 
  so_study_population_preg<-sum(so_study_population_preg) 
  
  flowchart_so_preg<-data.table(INDICATOR=c("Number of records in the original table",
                                            "Exclude:Number of records with excluded meanings",
                                            "Number of subjects in the original study population table",
                                            "Exclude: Number of records for male subjects",
                                            "Exclude: Number of records with unknown or other sex",
                                            "Exclude: Number of records for females younger than 12 years old or older than 55 years old at start of follow up ",
                                            "Exclude: Number of subjects with meanings not of interest(to identify birth registry)",
                                            "Number of records for the study_population(no time criteria)",
                                            "Exclude: Number of records with date record missing",
                                            "Exclude: Number of records with date record outside study period",
                                            "Number of records for the study_population(time criteria applied)",
                                            "Exclude:Number of records with empty meaning",
                                            "Exclude: Number of records with both code and vocabulary variables missing",
                                            "Exclude: Number of records with empty vocabulary when code is present",
                                            "Exclude: Number of records with vocabularies not in the codelist",
                                            "Exclude: Number of records with empty code",
                                            "Number of records for study_population"), 
                                SURVEY_OBSERVATIONS=c(orig_no_rows_so_preg,
                                                      so_excluded_meanings_preg,
                                                      nr_std,
                                                      so_excluded_males,
                                                      so_sex_not_specified_preg,
                                                      females_outside_age_so,
                                                      "N/A",
                                                      so_study_pop_preg,
                                                      so_date_miss_preg,
                                                      so_out_st_per_preg,
                                                      so_study_pop_obsper_preg,
                                                      so_stdpop_no_meaning_preg,
                                                      so_source_value_vocabulary_miss_preg,
                                                      so_source_value_pres_voc_miss_preg,
                                                      so_not_vocabularies_preg,
                                                      empty_so_source_value_preg,
                                                      so_study_population_preg))
  
  
  rm(orig_no_rows_so_preg, so_excluded_meanings_preg,so_excluded_males,so_sex_not_specified_preg,
     so_study_pop_preg,females_outside_age_so,so_date_miss_preg,so_out_st_per_preg,
     so_study_pop_obsper_preg,so_stdpop_no_meaning_preg,so_source_value_vocabulary_miss_preg,so_source_value_pres_voc_miss_preg,
     so_not_vocabularies_preg,empty_so_source_value_preg)  
  
  ##################################################################################################
  #Description
  ##################################################################################################
  print("Creating description of study population.")
  #meanings
  print("Get list of meanings.")
  meanings_so_preg<-Filter(length,meanings_so_preg)
  meanings_so_preg<-suppressWarnings(do.call(rbind,meanings_so_preg))
  meanings_so_preg<-unique(c(meanings_so_preg))
  meanings_so_preg_des<-paste(meanings_so_preg, collapse = ", ")
  #study years
  years_so_preg<-Filter(length,years_so_preg)
  years_so_preg<-suppressWarnings(do.call(rbind, years_so_preg))
  years_so_preg<-unique(c(years_so_preg))
  years_so_des_preg<-paste(sort(years_so_preg), collapse=", ")
  
  
  print("Create description.")
  description_so_preg<-data.table(INDICATOR=c("Data access provider(data source name)",
                                              "List of meanings present",
                                              "Years included in the study period",
                                              "Number of subjects in the study population without a recorded pregnancy"), 
                                  SURVEY_OBSERVATIONS=c(paste0(data_access_provider_name,"(",data_source_name,")"),
                                                        meanings_so_preg_des,
                                                        years_so_des_preg,
                                                        stdpop_not_so_preg))
  rm(meanings_so_preg_des,years_so_des_preg,stdpop_not_so_preg)
} else {
  flowchart_so_preg<-data.table(INDICATOR=c("Number of records in the original table",
                                            "Exclude:Number of records with excluded meanings",
                                            "Number of subjects in the original study population table",
                                            "Exclude: Number of records for male subjects",
                                            "Exclude: Number of records with unknown or other sex",
                                            "Exclude: Number of records for females younger than 12 years old or older than 55 years old at start of follow up ",
                                            "Exclude: Number of subjects with meanings not of interest(to identify birth registry)",
                                            "Number of records for the study_population(no time criteria)",
                                            "Exclude: Number of records with date record missing",
                                            "Exclude: Number of records with date record outside study period",
                                            "Number of records for the study_population(time criteria applied)",
                                            "Exclude:Number of records with empty meaning",
                                            "Exclude: Number of records with both code and vocabulary variables missing",
                                            "Exclude: Number of records with empty vocabulary when code is present",
                                            "Exclude: Number of records with vocabularies not in the codelist",
                                            "Exclude: Number of records with empty code",
                                            "Number of records for study_population"), 
                                SURVEY_OBSERVATIONS="N/A")
  
  description_so_preg<-data.table(INDICATOR=c("Data access provider(data source name)",
                                              "List of meanings present",
                                              "Years included in the study period",
                                              "Number of subjects in the study population without a recorded pregnancy"), 
                                  SURVEY_OBSERVATIONS="N/A")

}

############################################
#SURVEY_ID
###########################################
if("SURVEY_ID" %in% names(actual_tables)){
  ###################################################
  #List for saving info
  ###################################################
  print("Creating lists to save the information.")
  ################################
  #Flowchart
  ################################
  orig_no_rows_si_preg<-list() #original number of records in the SURVEY_ID table
  si_excluded_meanings_preg<-list() #number of recorded with excluded meanings
  #nr_std number of records in the original study population
  si_excluded_males<-list() #number of records for male subjects
  si_sex_not_specified_preg<-list() #number of records for subjects with unspecified sex
  females_outside_age_si<-list() #number of records where age outside limits
  si_meanings_not_of_interest<-list() #number of records excluded that do not match meanings of interest ex birth_registry needs to be included
  si_study_pop_preg<-list() #number of records for the study population, no selection criteria for time applied
  si_date_miss_preg<-list() #number of record with missing si_date
  si_out_st_per_preg<-list() #number of SURVEY_ID records outside the observation period(check is done on individual level)
  si_study_pop_obsper_preg<-list() #number of records in the study population inside study period
  si_stdpop_no_meaning_preg<-list() #number of records in the study population with no meaning
  
  #######################
  #description
  #######################
  #data access provider name(data source name)
  meanings_si_preg<-list() #all meanings present
  years_si_preg<-list() #all years present
  #pers_stdpop_not_si_preg #number of people in the study population without a pregnancy
  #######################
  
  #############################################################################
  #Description of the dataset
  #############################################################################
  si_study_population_preg<-list() #number of records in the study population
  si_study_pop_m_preg<-list() #number of records in the study population by meaning
  si_study_pop_my_preg<-list() #number of records in the study population by meaning and year
  ###############################
  w<-1
  for (y in 1:length(actual_tables$SURVEY_ID)){
    #Load the table
    df<-fread(paste(path_dir, actual_tables$SURVEY_ID[y], sep=""), stringsAsFactors = FALSE)
    df<-df[,c("person_id", "survey_date",  "survey_meaning")]
    df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately
    setnames(df,"survey_meaning","meaning")
    colnames_si<-names(df)
    std_names_si<-names(study_population)
    colnames_si<-colnames_si[!colnames_si %in% "person_id"]
    #get the total number of records(a)
    orig_no_rows_si_preg[[w]]<-df[,.N]
    #get the total number of records with excluded meanings
    si_excluded_meanings_preg[[w]]<-df[meaning %in% meanings_exclude_si,.N]
    #remove all records for which the meaning is in excluded meanings
    df<-df[meaning %!in% meanings_exclude_si]
    #merge with the study_population table(there is no missing data in this table)
    df<-df[study_population,on=.(person_id)]#left join, keeps all people in the study population even if they didn't have an mo
    df<-df[,age_fup:=as.numeric(age_fup)]
    pers_stdpop_not_si_preg<-df[rowSums(is.na(df[,..colnames_si]))==length(colnames_si), ..std_names_si] #subjects id present in the study population but that do not have an mo
    pers_stdpop_not_si_preg<-pers_stdpop_not_si_preg[!duplicated(person_id)]
    df<-df[!rowSums(is.na(df[,..colnames_si]))==length(colnames_si)]
    if(pers_stdpop_not_si_preg[,.N]>0){
      saveRDS(pers_stdpop_not_si_preg, paste0(preg_si_tmp, paste0("stdpop_not_si_preg_", actual_tables$SURVEY_ID[y], ".rds"))) #to give the results we will merge the files by keeping only person_id that are in all saved files
    } else {pers_stdpop_not_si_preg<-NULL}
    rm(pers_stdpop_not_si_preg)
    #number of records for male subjects
    si_excluded_males[[w]]<-df[sex_at_instance_creation=="M",.N]
    #remove males
    df<-df[sex_at_instance_creation != "M"]
    #number of records with unspecified sex
    si_sex_not_specified_preg[[w]]<-df[sex_at_instance_creation =="U" | sex_at_instance_creation == "O",.N] #number of records with unspecified sex
    #remove unspecified sex
    df<-df[sex_at_instance_creation == "F"]#remove unspecified sex
    #number of females outside chidbearing age (based on age at follow up)
    females_outside_age_si[[w]]<-df[age_fup< min_age_preg | age_fup> max_age_preg, .N]
    df<-df[age_fup>= min_age_preg & age_fup<=max_age_preg]
    #number of records with meanings not of interest
    si_meanings_not_of_interest[[w]]<-df[meaning %!in% meanings_birth_registry,.N]
    #select only meaning refferring to birth_registry
    df<-df[meaning %in% meanings_birth_registry]
    
    si_study_pop_preg[[w]]<-df[,.N] #number of records for the study population, no selection criteria for time applied
    #transform into date variables
    df[,survey_date:=as.Date(survey_date,"%Y%m%d")] #transform to date variables
    #create year variable
    df[,year:=year(survey_date)]
    #number of records with both survey_date missing
    si_date_miss_preg[[w]]<-df[is.na(year),.N]
    #remove records with both dates missing
    df<-df[!is.na(year)]
    #remove records that are outside the obs_period for all subjects
    si_out_st_per_preg[[w]]<-df[survey_date<start_follow_up | survey_date>exit_spell_category,.N] #number of records outside study population
    df[(survey_date<start_follow_up | survey_date>exit_spell_category), obs_out:=1]
    df<-df[is.na(obs_out)] #remove records outside study period
    df[,obs_out:=NULL] 
    si_study_pop_obsper_preg[[w]]<-df[,.N] #number of records after removing records outside study period
    si_stdpop_no_meaning_preg[[w]]<-df[is.na(meaning),.N] #number of records with empty meaning
    df<-df[!is.na(meaning)] #remove records with empty meaning
    females_childbearing_preg<-ifelse(df[,.N]!=0,"Yes","No")
    si_study_population_preg[[w]]<-df[,.N] #number of records in the study population(final)
    #########################################################################################
    #description
    #########################################################################################
    meanings_si_preg[[w]]<-unique(na.omit(df[, meaning])) #will be used for description
    years_si_preg[[w]]<-unique(na.omit(df[, year])) #will be used for description
    
    if(females_childbearing_preg=="Yes"){
      ############################
      #Description of the database
      ###########################
      
      si_study_pop_m_preg[[w]]<-df[,.N, by="meaning"] #number of records in the study population by meaning
      si_study_pop_my_preg[[w]]<-df[,.N, by=.(meaning,year)] #number of records in the study population by meaning and year
      ##################################################################
      #match codes based on coding system and code: algorithm start with
      #################################################################
      years_study_si_preg<-df[!duplicated(year), year]#years present in this table
      
      
      for (m in 1: length(years_study_si_preg)){
        saveRDS(data.table(df[year==years_study_si_preg[m]], condition="end_of_pregnancy"), paste0(preg_si_tmp,years_study_si_preg[m],"_", "end_of_pregnancy", "_",actual_tables$SURVEY_ID[y], ".rds"))
      }
    }
    
    w<-w+1
    rm(df)
  }
  
  #number of subjects in the study population that have not had an mo
  stdpop_not_si_preg_files<-list.files(preg_si_tmp, pattern = "stdpop_not_si_preg")
  if (length(stdpop_not_si_preg_files)>0){
    stdpop_not_si_preg<-readRDS(paste0(preg_si_tmp, stdpop_not_si_preg_files[1]))
    i<-2
    while(i <= length(stdpop_not_si_preg_files)){
      a<-readRDS(paste0(preg_si_tmp, stdpop_not_si_preg_files[i]))
      stdpop_not_si_preg<-rbind(stdpop_not_si_preg, a)
      stdpop_not_si_preg<-stdpop_not_si_preg[duplicated(person_id)]
      i<-i+1
      rm(a)
    }
    stdpop_not_si_preg<-stdpop_not_si_preg[,.N]
    
    
    for(i in 1:length(stdpop_not_si_preg_files)){
      unlink(paste0(preg_si_tmp,stdpop_not_si_preg_files[i]))
    }
    rm(stdpop_not_si_preg_files)
  } else {stdpop_not_si_preg<-0}
  
  
  #################################################################################################
  #Flowchart
  ################################################################################################
  print("Creating flowchart.")
  print("Get number of records in the original table.")
  #original number of records in the SURVEY_ID table(flowchart 1)
  orig_no_rows_si_preg<-do.call(rbind,orig_no_rows_si_preg)
  orig_no_rows_si_preg<-sum(orig_no_rows_si_preg)
  #number of records with excluded meanings(flowchart 2)
  print("Get number of records with excluded meanings.")
  si_excluded_meanings_preg<-do.call(rbind, si_excluded_meanings_preg)
  si_excluded_meanings_preg<-sum(si_excluded_meanings_preg)
  #original number table (flowchart 1)
  #excluded meanings (flowchart 2)
  #original study pop flowchart(3)
  #male excluded(flowchart 4)
  si_excluded_males<-do.call(rbind,si_excluded_males)
  si_excluded_males<-sum(si_excluded_males)
  #females outside age excluded(flowchart 5)
  females_outside_age_si<-do.call(rbind,females_outside_age_si)
  females_outside_age_si<-sum(females_outside_age_si)
  #sex not specified(flowchart 6)
  si_sex_not_specified_preg<-do.call(rbind, si_sex_not_specified_preg)
  si_sex_not_specified_preg<-sum(si_sex_not_specified_preg)
  #number of records with meanings not of interest
  si_meanings_not_of_interest<-do.call(rbind, si_meanings_not_of_interest)
  si_meanings_not_of_interest<-sum(si_meanings_not_of_interest)
  #number of records for the study population, no selection criteria for time applied (flowchart 7)
  print("Get number of records for the study population (no time criteria applied).")
  si_study_pop_preg<-do.call(rbind,si_study_pop_preg)
  si_study_pop_preg<-sum(si_study_pop_preg)
  #Number of records with date record missing(flowchart 8)
  print("Get number of records with date record missing.")
  si_date_miss_preg<-do.call(rbind,si_date_miss_preg)
  si_date_miss_preg<-sum(si_date_miss_preg)
  #number of medicines records outside the observation period(check is done on individual level) (flowchart 9)
  print("Get number of records outside observation period.")
  si_out_st_per_preg<-do.call(rbind,si_out_st_per_preg) 
  si_out_st_per_preg<-sum(si_out_st_per_preg)
  #number of records in the study population with si_date inside study period (flowchart 10)
  print("Get number of records for the study population(time criteria applied).")
  si_study_pop_obsper_preg<-do.call(rbind,si_study_pop_obsper_preg) 
  si_study_pop_obsper_preg<-sum(si_study_pop_obsper_preg)
  #number of records in the study population with no meaning (flowchart 11)
  print("Get number of records with no meaning.")
  si_stdpop_no_meaning_preg<-do.call(rbind,si_stdpop_no_meaning_preg) 
  si_stdpop_no_meaning_preg<-sum(si_stdpop_no_meaning_preg) 
  #Number of records with both code and vocabulary variables missing (flowchart 12)
  #number of records in the study population (flowchart 14)
  print("Get number of records for study population.")
  si_study_population_preg<-do.call(rbind,si_study_population_preg) 
  si_study_population_preg<-sum(si_study_population_preg) 
  
  flowchart_si_preg<-data.table(INDICATOR=c("Number of records in the original table",
                                            "Exclude:Number of records with excluded meanings",
                                            "Number of subjects in the original study population table",
                                            "Exclude: Number of records for male subjects",
                                            "Exclude: Number of records with unknown or other sex",
                                            "Exclude: Number of records for females younger than 12 years old or older than 55 years old at start of follow up ",
                                            "Exclude: Number of subjects with meanings not of interest(to identify birth registry)",
                                            "Number of records for the study_population(no time criteria)",
                                            "Exclude: Number of records with date record missing",
                                            "Exclude: Number of records with date record outside study period",
                                            "Number of records for the study_population(time criteria applied)",
                                            "Exclude:Number of records with empty meaning",
                                            "Exclude: Number of records with both code and vocabulary variables missing",
                                            "Exclude: Number of records with empty vocabulary when code is present",
                                            "Exclude: Number of records with vocabularies not in the codelist",
                                            "Exclude: Number of records with empty code",
                                            "Number of records for study_population"), 
                                SURVEY_ID=c(orig_no_rows_si_preg,
                                            si_excluded_meanings_preg,
                                            nr_std,
                                            si_excluded_males,
                                            si_sex_not_specified_preg,
                                            females_outside_age_si,
                                            si_meanings_not_of_interest,
                                            si_study_pop_preg,
                                            si_date_miss_preg,
                                            si_out_st_per_preg,
                                            si_study_pop_obsper_preg,
                                            si_stdpop_no_meaning_preg,
                                            "N/A",
                                            "N/A",
                                            "N/A",
                                            "N/A",
                                            si_study_population_preg))
  
  
  rm(orig_no_rows_si_preg, si_excluded_meanings_preg,si_excluded_males,si_sex_not_specified_preg,
     si_study_pop_preg,females_outside_age_si,si_date_miss_preg,si_out_st_per_preg,
     si_study_pop_obsper_preg,si_stdpop_no_meaning_preg,si_meanings_not_of_interest)  
  
  ##################################################################################################
  #Description
  ##################################################################################################
  print("Creating description of study population.")
  #meanings
  print("Get list of meanings.")
  meanings_si_preg<-Filter(length,meanings_si_preg)
  meanings_si_preg<-suppressWarnings(do.call(rbind,meanings_si_preg))
  meanings_si_preg<-unique(c(meanings_si_preg))
  meanings_si_preg_des<-paste(meanings_si_preg, collapse = ", ")
  #study years
  years_si_preg<-Filter(length,years_si_preg)
  years_si_preg<-suppressWarnings(do.call(rbind, years_si_preg))
  years_si_preg<-unique(c(years_si_preg))
  years_si_des_preg<-paste(sort(years_si_preg), collapse=", ")
  
  
  print("Create description.")
  description_si_preg<-data.table(INDICATOR=c("Data access provider(data source name)",
                                              "List of meanings present",
                                              "Years included in the study period",
                                              "Number of subjects in the study population without a recorded pregnancy"), 
                                  SURVEY_ID=c(paste0(data_access_provider_name,"(",data_source_name,")"),
                                              meanings_si_preg_des,
                                              years_si_des_preg,
                                              stdpop_not_si_preg))
  rm(meanings_si_preg_des,years_si_des_preg,stdpop_not_si_preg)
} else {
  flowchart_si_preg<-data.table(INDICATOR=c("Number of records in the original table",
                                            "Exclude:Number of records with excluded meanings",
                                            "Number of subjects in the original study population table",
                                            "Exclude: Number of records for male subjects",
                                            "Exclude: Number of records with unknown or other sex",
                                            "Exclude: Number of records for females younger than 12 years old or older than 55 years old at start of follow up ",
                                            "Exclude: Number of subjects with meanings not of interest(to identify birth registry)",
                                            "Number of records for the study_population(no time criteria)",
                                            "Exclude: Number of records with date record missing",
                                            "Exclude: Number of records with date record outside study period",
                                            "Number of records for the study_population(time criteria applied)",
                                            "Exclude:Number of records with empty meaning",
                                            "Exclude: Number of records with both code and vocabulary variables missing",
                                            "Exclude: Number of records with empty vocabulary when code is present",
                                            "Exclude: Number of records with vocabularies not in the codelist",
                                            "Exclude: Number of records with empty code",
                                            "Number of records for study_population"), 
                                SURVEY_ID="N/A")
  
  description_si_preg<-data.table(INDICATOR=c("Data access provider(data source name)",
                                              "List of meanings present",
                                              "Years included in the study period",
                                              "Number of subjects in the study population without a recorded pregnancy"), 
                                  SURVEY_ID="N/A")
  
}


###########################################################################################
#Combine results 
##########################################################################################
###################
#flowchart
###################
flowchart_preg<-data.table(flowchart_events_preg,flowchart_mo_preg[,2], flowchart_so_preg[,2], flowchart_si_preg[,2])
rm(flowchart_events_preg,flowchart_mo_preg,flowchart_so_preg, flowchart_si_preg)

###################
#description
###################
description_preg<-data.table(description_events_preg,description_mo_preg[,2], description_so_preg[,2], description_si_preg[,2])
rm(description_events_preg,description_mo_preg,description_so_preg, description_si_preg)
############################################################################################
#Counts and rates
############################################################################################
#Combine dataset by year and stage of pregnancy
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
pregnancy_files_si$start_of_pregnancy<-list()
pregnancy_files_si$end_of_pregnancy<-list.files(preg_si_tmp, "end_of_pregnancy")
pregnancy_files_si$ongoing_pregnancy<-list()
pregnancy_files_si$interruption_pregnancy<-list()


#########################################
time_lag<-data.table(stage_of_pregnancy=c("start_of_pregnancy", "end_of_pregnancy", "ongoing_pregnancy", "interruption_pregnancy"),time_lag=c(6*7,23*7,23*7, 8*7))
pregnancy_files<-vector(mode="list", length=4)
names(pregnancy_files)<-c("start_of_pregnancy", "end_of_pregnancy", "ongoing_pregnancy", "interruption_pregnancy")

#create a loop that goes through all stages of pregnancy
for (t in 1:length(pregnancy_files_events)){
  if(length(pregnancy_files_events[[t]])>0){
    combined_preg_stage_ev<-lapply(paste0(preg_ev_tmp,pregnancy_files_events[[t]]), readRDS) #combine all files for one pregnancy stage
    combined_preg_stage_ev<-do.call(rbind,combined_preg_stage_ev)
    combined_preg_stage_ev[,lag:=time_lag[stage_of_pregnancy == names(pregnancy_files_events)[t],time_lag]] #create lag variable based on condition
    #Step 1: Order the dataset by person_id, stage_of_pregnancy and date of event
    combined_preg_stage_ev<-combined_preg_stage_ev[order(person_id,condition,start_date_record)]
    #Step 2: Create date_2(by shifting the first date)
    combined_preg_stage_ev[,date_1:=shift(start_date_record)]
    combined_preg_stage_ev[,date_2:=start_date_record]
    #Step 3: Create rowid(whhcih will give the number of rows for each person)
    combined_preg_stage_ev[,rowid:=rowid(person_id)]
    #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
    combined_preg_stage_ev[rowid==1,date_1:=NA]
    #Step 5: Create date_dif as difference between date 2 and date
    combined_preg_stage_ev[,date_dif:=date_2-date_1]
    #Step 6: Remove these rows 
    combined_preg_stage_ev<-combined_preg_stage_ev[date_dif>lag | is.na(date_dif)]
    #Step 7: Repeat until there are no more impossible dates in the dataset
    combined_preg_stage_ev[,date_dif:=date_2-date_1]
    while(combined_preg_stage_ev[date_dif<=lag,.N]>0){
      combined_preg_stage_ev<-combined_preg_stage_ev[date_dif>lag | is.na(date_dif)]
      combined_preg_stage_ev[,date_dif:=date_2-date_1]
    }
    combined_preg_stage_ev[,event_code:=NULL][,event_record_vocabulary:=NULL][,filter_preg:=NULL][,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
    setnames(combined_preg_stage_ev,"start_date_record","pregnancy_code_date")
  } else {combined_preg_stage_ev<-NULL}
  
  pregnancy_files[[t]]<-append(pregnancy_files[[t]],list(combined_preg_stage_ev))
} 
rm(combined_preg_stage_ev, pregnancy_files_events)
##############################################################################
for (t in 1:length(pregnancy_files_mo)){
  if(length(pregnancy_files_mo[[t]])>0){
    combined_preg_stage_mo<-lapply(paste0(preg_m_tmp,pregnancy_files_mo[[t]]), readRDS) #combine all files for one pregnancy stage
    combined_preg_stage_mo<-do.call(rbind,combined_preg_stage_mo)
    combined_preg_stage_mo[,lag:=time_lag[stage_of_pregnancy == names(pregnancy_files_mo)[t],time_lag]] #create lag variable based on condition
    #Step 1: Order the dataset by person_id, stage_of_pregnancy and date of mo
    combined_preg_stage_mo<-combined_preg_stage_mo[order(person_id,condition,mo_date)]
    #Step 2: Create date_2(by shifting the first date)
    combined_preg_stage_mo[,date_1:=shift(mo_date)]
    combined_preg_stage_mo[,date_2:=mo_date]
    #Step 3: Create rowid(whhcih will give the number of rows for each person)
    combined_preg_stage_mo[,rowid:=rowid(person_id)]
    #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
    combined_preg_stage_mo[rowid==1,date_1:=NA]
    #Step 5: Create date_dif as difference between date 2 and date
    combined_preg_stage_mo[,date_dif:=date_2-date_1]
    #Step 6: Remove these rows 
    combined_preg_stage_mo<-combined_preg_stage_mo[date_dif>lag | is.na(date_dif)]
    #Step 7: Repeat until there are no more impossible dates in the dataset
    combined_preg_stage_mo[,date_dif:=date_2-date_1]
    while(combined_preg_stage_mo[date_dif<=lag,.N]>0){
      combined_preg_stage_mo<-combined_preg_stage_mo[date_dif>lag | is.na(date_dif)]
      combined_preg_stage_mo[,date_dif:=date_2-date_1]
    }
    combined_preg_stage_mo[,mo_code:=NULL][,mo_record_vocabulary:=NULL][,filter_preg:=NULL][,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
    setnames(combined_preg_stage_mo,"mo_date","pregnancy_code_date")
    
  } else {combined_preg_stage_mo<-NULL}
  pregnancy_files[[t]]<-append(pregnancy_files[[t]],list(combined_preg_stage_mo))
}
rm(combined_preg_stage_mo,pregnancy_files_mo)
###############################################################
for (t in 1:length(pregnancy_files)){
  if(length(pregnancy_files_so[[t]])>0){
    combined_preg_stage_so<-lapply(paste0(preg_s_tmp,pregnancy_files_so[[t]]), readRDS) #combine all files for one pregnancy stage
    combined_preg_stage_so<-do.call(rbind,combined_preg_stage_so)
    combined_preg_stage_so[,lag:=time_lag[stage_of_pregnancy == names(pregnancy_files_so)[t],time_lag]] #create lag variable based on condition
    #Step 1: Order the dataset by person_id, stage_of_pregnancy and date of mo
    combined_preg_stage_so<-combined_preg_stage_so[order(person_id,condition,so_date)]
    #Step 2: Create date_2(by shifting the first date)
    combined_preg_stage_so[,date_1:=shift(so_date)]
    combined_preg_stage_so[,date_2:=so_date]
    #Step 3: Create rowid(whhcih will give the number of rows for each person)
    combined_preg_stage_so[,rowid:=rowid(person_id)]
    #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
    combined_preg_stage_so[rowid==1,date_1:=NA]
    #Step 5: Create date_dif as difference between date 2 and date
    combined_preg_stage_so[,date_dif:=date_2-date_1]
    #Step 6: Remove these rows 
    combined_preg_stage_so<-combined_preg_stage_so[date_dif>lag | is.na(date_dif)]
    #Step 7: Repeat until there are no more impossible dates in the dataset
    combined_preg_stage_so[,date_dif:=date_2-date_1]
    while(combined_preg_stage_so[date_dif<=lag,.N]>0){
      combined_preg_stage_so<-combined_preg_stage_so[date_dif>lag | is.na(date_dif)]
      combined_preg_stage_so[,date_dif:=date_2-date_1]
    }
    combined_preg_stage_so[,so_source_value:=NULL][,so_unit:=NULL][,filter_preg:=NULL][,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
    setnames(combined_preg_stage_so,"so_date","pregnancy_code_date")
    
  } else {combined_preg_stage_so<-NULL}
  pregnancy_files[[t]]<-append(pregnancy_files[[t]],list(combined_preg_stage_so))
}
rm(combined_preg_stage_so,pregnancy_files_so)
################################################################
for (t in 1:length(pregnancy_files)){
  if(length(pregnancy_files_si[[t]])>0){
    combined_preg_stage_si<-lapply(paste0(preg_si_tmp,pregnancy_files_si[[t]]), readRDS) #combine all files for one pregnancy stage
    combined_preg_stage_si<-do.call(rbind,combined_preg_stage_si)
    combined_preg_stage_si[,lag:=time_lag[stage_of_pregnancy == names(pregnancy_files_si)[t],time_lag]] #create lag variable based on condition
    #Step 1: Order the dataset by person_id, stage_of_pregnancy and date of mo
    combined_preg_stage_si<-combined_preg_stage_si[order(person_id,condition,survey_date)]
    #Step 2: Create date_2(by shifting the first date)
    combined_preg_stage_si[,date_1:=shift(survey_date)]
    combined_preg_stage_si[,date_2:=survey_date]
    #Step 3: Create rowid(whhcih will give the number of rows for each person)
    combined_preg_stage_si[,rowid:=rowid(person_id)]
    #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
    combined_preg_stage_si[rowid==1,date_1:=NA]
    #Step 5: Create date_dif as difference between date 2 and date
    combined_preg_stage_si[,date_dif:=date_2-date_1]
    #Step 6: Remove these rows 
    combined_preg_stage_si<-combined_preg_stage_si[date_dif>lag | is.na(date_dif)]
    #Step 7: Repeat until there are no more impossible dates in the dataset
    combined_preg_stage_si[,date_dif:=date_2-date_1]
    while(combined_preg_stage_si[date_dif<=lag,.N]>0){
      combined_preg_stage_si<-combined_preg_stage_si[date_dif>lag | is.na(date_dif)]
      combined_preg_stage_si[,date_dif:=date_2-date_1]
    }
    combined_preg_stage_si[,date_1:=NULL][,date_2:=NULL][,rowid:=NULL][,date_dif:=NULL]
    setnames(combined_preg_stage_si,"survey_date","pregnancy_code_date")
    
  } else {combined_preg_stage_si<-NULL}
  pregnancy_files[[t]]<-append(pregnancy_files[[t]],list(combined_preg_stage_si))
}
rm(combined_preg_stage_si,pregnancy_files_si)
#######
for (t in 1: length(pregnancy_files)){
  pregnancy_files[[t]]<-do.call(rbind,pregnancy_files[[t]])
}
pregnancy_files<-as.data.table(do.call(rbind,pregnancy_files))

#######################################
if (subpopulations_present=="Yes"){
  conditions<-unique(na.omit(pregnancy_files[["condition"]]))
  for (i in 1:length(conditions)){
    saveRDS(pregnancy_files[condition==conditions[i]], paste0(preg_pop,subpopulations_names[s], "/", subpopulations_names[s],"_",conditions[i],".rds"))
  }
} else {
  conditions<-unique(na.omit(pregnancy_files[["condition"]]))
  for (i in 1:length(conditions)){
    saveRDS(pregnancy_files[condition==conditions[i]], paste0(preg_pop,conditions[i],".rds"))
  }
}
######################################

#Create fup variable
pregnancy_files[,fup_time:=exit_spell_category-start_follow_up][,rowid:=rowid(person_id)]

#################################################
#tab17
################################################
counts_events_preg_rec<-pregnancy_files[,.N,by=.(condition,year)]
setnames(counts_events_preg_rec,"N", "no_records")
no_women_code_1<-pregnancy_files[!duplicated(person_id),.N, by=.(condition,year)] #number of women with at least one record
setnames(no_women_code_1,"N", "no_women")
tab17_preg<-counts_events_preg_rec
rm(counts_events_preg_rec)
tab17_preg<-merge(tab17_preg,no_women_code_1, by=c("condition","year"))
rm(no_women_code_1)
##################################################

#################################################
#tab19
################################################
#no of women with at least one code, no of women with at least one code and 365 years
#of follow up, %of women with one code+365 days, median follow up for these women(not possible)
#instead py for women at least 365 years is provided
#rowid==1 will be used as filter
#Create fup variable
pregnancy_files[,rowid:=rowid(person_id,year)][,fup_time:=exit_spell_category - pregnancy_code_date]#get the first record by year for each person
pregnancy_files[,pregnancy_code_date:=as.IDate(pregnancy_code_date)][rowid==1, fup_time:= exit_spell_category - pregnancy_code_date]
no_women_first_code<-pregnancy_files[!duplicated(person_id) & rowid==1,.N, by=.(condition,year)]
setnames(no_women_first_code, "N", "no_first_code")
no_women_first_code_365_fup<-pregnancy_files[!duplicated(person_id) & rowid==1 & fup_time>=365,.N, by=.(condition,year)]
setnames(no_women_first_code_365_fup, "N", "no_first_code_min_365_fup")
tab19_preg<-no_women_first_code
rm(no_women_first_code)
tab19_preg<-merge(tab19_preg,no_women_first_code_365_fup, by=c("condition","year"))
rm(no_women_first_code_365_fup)

#########################
#calculate person time by year
#########################
conditions<-unique(na.omit(pregnancy_files[,condition]))
final_res_py_17<-list()
for (z in 1: length(conditions)){
  pregnancy_files[condition==conditions[z] ,year_start:=year(start_follow_up)]
  pregnancy_files[condition==conditions[z] ,year_end:=year(exit_spell_category)]
  min_year<-min(pregnancy_files[condition==conditions[z] ,"year_start"])
  max_year<-max(pregnancy_files[condition==conditions[z] ,"year_end"])
  years_interval<-seq(min_year,max_year,by=1)
  
  res_py<-vector(mode="list",length(years_interval))
  for (i in 1:length(years_interval)){
    res_py[[i]]<-as.data.table(person_time(x=years_interval,dt=pregnancy_files[condition==conditions[z]],year_index = i))
  }
  res_py<-do.call(rbind,res_py)
  res_py<-res_py[person_years!=0]
  res_py<-data.table(condition=conditions[z],res_py)
  final_res_py_17<-append(final_res_py_17,list(res_py))
  rm(min_year, max_year, years_interval, res_py)
  pregnancy_files[,year_start:=NULL][,year_end:=NULL]
}

final_res_py_17<-do.call(rbind,final_res_py_17)
tab17_py<-final_res_py_17
rm(final_res_py_17)
tab17_py<-tab17_py[,lapply(.SD, sum), .SDcols=c("person_years"), by=c("year","condition")]
tab17_py[,year:=as.numeric(year)]
tab17_preg<-merge(tab17_preg, tab17_py, by=c("year","condition"))
rm(tab17_py, conditions)
tab17_preg[,rate_min_1_code_per_1000_py:=round((no_records/person_years)*1000,digits=2)]

tab17_preg<-data.table(tab17_preg, data_access_provider= data_access_provider_name, data_source=data_source_name)


if (subpopulations_present=="Yes"){
  write.csv(tab17_preg, paste0(preg_dir,subpopulations_names[s], "/", subpopulations_names[s],"_","tab17_pregnancy.csv"))
} else {
  write.csv(tab17_preg, paste0(preg_dir,"tab17_pregnancy.csv"))
}

  tab17_preg[, person_years:= as.character(person_years)][as.numeric(person_years) > 0 & as.numeric(person_years) < 5, person_years := "<5"]
  tab17_preg[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
  tab17_preg[, rate_min_1_code_per_1000_py:= as.character(rate_min_1_code_per_1000_py)][no_records == "<5", rate_min_1_code_per_1000_py := "N/A"]
  if (subpopulations_present=="Yes"){
  write.csv(tab17_preg, paste0(preg_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_tab17_pregnancy_masked.csv"), row.names = F)
  } else {
    write.csv(tab17_preg, paste0(preg_dir,"Masked/", "tab17_pregnancy_masked.csv"), row.names = F)
}

##############################################################
conditions<-unique(na.omit(pregnancy_files[rowid==1 & fup_time>=365,condition]))
final_res_py_19<-list()
for (z in 1: length(conditions)){
  pregnancy_files[condition==conditions[z] & rowid==1 & fup_time>=365 ,year_start:=year(start_follow_up)]
  pregnancy_files[condition==conditions[z] & rowid==1 & fup_time>=365 ,year_end:=year(exit_spell_category)]
  min_year<-min(pregnancy_files[condition==conditions[z] & rowid==1 & fup_time>=365,"year_start"])
  max_year<-max(pregnancy_files[condition==conditions[z] & rowid==1 & fup_time>=365,"year_end"])
  years_interval<-seq(min_year,max_year,by=1)
  
  res_py<-vector(mode="list",length(years_interval))
  for (i in 1:length(years_interval)){
    res_py[[i]]<-as.data.table(person_time(x=years_interval,dt=pregnancy_files[condition==conditions[z] & rowid==1 & fup_time>=365],year_index = i))
  }
  res_py<-do.call(rbind,res_py)
  res_py<-res_py[person_years!=0]
  res_py<-data.table(condition=conditions[z],res_py)
  final_res_py_19<-append(final_res_py_19,list(res_py))
  rm(min_year, max_year, years_interval, res_py)
  pregnancy_files[,year_start:=NULL][,year_end:=NULL]
}

final_res_py_19<-do.call(rbind,final_res_py_19)
tab19_py<-final_res_py_19
rm(final_res_py_19)
tab19_py<-tab19_py[,lapply(.SD, sum), .SDcols=c("person_years"), by=c("year","condition")]
tab19_py[,year:=as.numeric(year)]
tab19_preg<-merge(tab19_preg, tab19_py, by=c("year","condition"))
rm(tab19_py)
tab19_preg[,percentage_first_code_min_365_fup:=round((no_first_code_min_365_fup/no_first_code)*100, 2)]

tab19_preg<-data.table(tab19_preg, data_access_provider= data_access_provider_name, data_source=data_source_name)


if (subpopulations_present=="Yes"){
  write.csv(tab19_preg, paste0(preg_dir,subpopulations_names[s], "/", subpopulations_names[s],"_","tab19_pregnancy.csv"))
} else {
  write.csv(tab19_preg, paste0(preg_dir,"tab19_pregnancy.csv"))
}

tab19_preg[, person_years:= as.character(person_years)][as.numeric(person_years) > 0 & as.numeric(person_years) < 5, person_years := "<5"]
tab19_preg[, no_first_code:= as.character(no_first_code)][as.numeric(no_first_code) > 0 & as.numeric(no_first_code) < 5, no_first_code := "<5"]
tab19_preg[, no_first_code_min_365_fup:= as.character(no_first_code_min_365_fup)][as.numeric(no_first_code_min_365_fup) > 0 & as.numeric(no_first_code_min_365_fup) < 5, no_first_code_min_365_fup := "<5"]
tab19_preg[, percentage_first_code_min_365_fup:= as.character(percentage_first_code_min_365_fup)][no_first_code_min_365_fup == "<5", percentage_first_code_min_365_fup := "N/A"]
if (subpopulations_present=="Yes"){
write.csv(tab19_preg, paste0(preg_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_tab19_pregnancy_masked.csv"), row.names = F)
} else {
  write.csv(tab19_preg, paste0(preg_dir,"Masked/", "tab19_pregnancy_masked.csv"), row.names = F)
}
  rm(tab19_preg)

#############################################################


#####################################################
#create graph
#####################################################
#get number of rows per unique combination
pregnancy_files<-pregnancy_files[,.N,by=c("person_id","year", "condition")]
setnames(pregnancy_files, "N", "code_count")
#Create code_1 meaning each person has only one code per year(type of code not taken into account)
code_1_all<-pregnancy_files[code_count==1 & !duplicated(person_id),.N, by="year"]
setnames(code_1_all,"N","code_1_per_year")
code_2_all<-pregnancy_files[code_count==2 & !duplicated(person_id),.N, by="year"]
setnames(code_2_all,"N","codes_2_per_year")
code_3_all<-pregnancy_files[code_count==3 & !duplicated(person_id),.N, by="year"]
setnames(code_3_all,"N","codes_3_per_year")
code_more_all<-pregnancy_files[code_count>3 & !duplicated(person_id),.N, by="year"]
setnames(code_more_all,"N","codes_min_4_per_year")
total_women_by_year<-pregnancy_files[!duplicated(person_id),.N, by="year"]
###########
graph1<-code_1_all
rm(code_1_all)
graph1<-merge(graph1, code_2_all, by="year", all=T)
rm(code_2_all)
graph1<-merge(graph1, code_3_all, by="year", all=T)
rm(code_3_all)
graph1<-merge(graph1, code_more_all, by="year", all=T)
rm(code_more_all)
graph1[is.na(code_1_per_year),code_1_per_year:=0][is.na(codes_2_per_year),codes_2_per_year:=0][is.na(codes_3_per_year),codes_3_per_year:=0][is.na(codes_min_4_per_year),codes_min_4_per_year:=0]

if (subpopulations_present=="Yes"){
  write.csv(graph1, paste0(preg_dir,subpopulations_names[s], "/", subpopulations_names[s],"_","graph_1.csv"))
} else {
  write.csv(graph1, paste0(preg_dir,"graph_1.csv"))
}

#apply masking
graph1[, code_1_per_year:= as.character(code_1_per_year)][as.numeric(code_1_per_year) > 0 & as.numeric(code_1_per_year) < 5, code_1_per_year := "<5"]
graph1[, codes_2_per_year:= as.character(codes_2_per_year)][as.numeric(codes_2_per_year) > 0 & as.numeric(codes_2_per_year) < 5, codes_2_per_year := "<5"]
graph1[, codes_3_per_year:= as.character(codes_3_per_year)][as.numeric(codes_3_per_year) > 0 & as.numeric(codes_3_per_year) < 5, codes_3_per_year := "<5"]
graph1[, codes_min_4_per_year:= as.character(codes_min_4_per_year)][as.numeric(codes_min_4_per_year) > 0 & as.numeric(codes_min_4_per_year) < 5, codes_min_4_per_year := "<5"]

if (subpopulations_present=="Yes"){
  write.csv(graph1, paste0(preg_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_graph_1_masked.csv"), row.names = F)
} else {
  write.csv(graph1, paste0(preg_dir,"Masked/", "graph_1_masked.csv"), row.names = F)
}
rm(graph1)


##############################################################
#same calculation for each condition
#################################
#Create code_1 meaning each person has only one code per year(type of code not taken into account)
code_1_all<-pregnancy_files[code_count==1 & !duplicated(person_id) ,.N, by=c("year","condition")]
setnames(code_1_all,"N","code_1_per_year")
code_2_all<-pregnancy_files[code_count==2 & !duplicated(person_id),.N, by=c("year","condition")]
setnames(code_2_all,"N","codes_2_per_year")
code_3_all<-pregnancy_files[code_count==3 & !duplicated(person_id),.N, by=c("year","condition")]
setnames(code_3_all,"N","codes_3_per_year")
code_more_all<-pregnancy_files[code_count>3 & !duplicated(person_id),.N, by=c("year","condition")]
setnames(code_more_all,"N","codes_min_4_per_year")
total_women_by_year<-pregnancy_files[!duplicated(person_id),.N, by=c("year", "condition")]

rm(pregnancy_files)

graph2<-code_1_all
rm(code_1_all)
graph2<-merge(graph2, code_2_all, by=c("year","condition"), all=T)
rm(code_2_all)
graph2<-merge(graph2, code_3_all, by=c("year","condition"), all=T)
rm(code_3_all)
graph2<-merge(graph2, code_more_all, by=c("year","condition"), all=T)
rm(code_more_all)
graph2[is.na(code_1_per_year),code_1_per_year:=0][is.na(codes_2_per_year),codes_2_per_year:=0][is.na(codes_3_per_year),codes_3_per_year:=0][is.na(codes_min_4_per_year),codes_min_4_per_year:=0]
setnames(graph2, "condition", "stage_of_pregnancy")

if (subpopulations_present=="Yes"){
  write.csv(graph2, paste0(preg_dir,subpopulations_names[s], "/", subpopulations_names[s],"_","graph_2.csv"))
} else {
  write.csv(graph2, paste0(preg_dir,"graph_2.csv"))
}

graph2[, code_1_per_year:= as.character(code_1_per_year)][as.numeric(code_1_per_year) > 0 & as.numeric(code_1_per_year) < 5, code_1_per_year := "<5"]
graph2[, codes_2_per_year:= as.character(codes_2_per_year)][as.numeric(codes_2_per_year) > 0 & as.numeric(codes_2_per_year) < 5, codes_2_per_year := "<5"]
graph2[, codes_3_per_year:= as.character(codes_3_per_year)][as.numeric(codes_3_per_year) > 0 & as.numeric(codes_3_per_year) < 5, codes_3_per_year := "<5"]
graph2[, codes_min_4_per_year:= as.character(codes_min_4_per_year)][as.numeric(codes_min_4_per_year) > 0 & as.numeric(codes_min_4_per_year) < 5, codes_min_4_per_year := "<5"]

if (subpopulations_present=="Yes"){
  write.csv(graph2, paste0(preg_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_graph_2_masked.csv"), row.names = F)
} else {
  write.csv(graph2, paste0(preg_dir,"Masked/", "graph_2_masked.csv"), row.names = F)
}
rm(graph2)



