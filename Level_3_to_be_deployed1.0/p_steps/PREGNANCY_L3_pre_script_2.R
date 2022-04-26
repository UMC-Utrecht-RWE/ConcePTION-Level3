#Author: Vjola Hoxhaj Drs.
#email: v.hoxhaj@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/07/2021

############################################
#EVENTS
############################################
if(length(actual_tables$EVENTS)>0){
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
  events_date_miss_preg<-list() #number of record with missing pregnancy_code_date
  events_out_st_per_preg<-list() #number of EVENTS records outside the observation period(check is done on individual level)
  events_study_pop_obsper_preg<-list() #number of records in the study population inside study period
  events_stdpop_no_meaning_preg<-list() #number of records in the study population with no meaning
  events_code_vocabulary_miss_preg<-list() #number of records with both event code and event record vocabulary missing
  events_code_pres_voc_miss_preg<-list() #number of records with missing vocabularies
  events_not_vocabularies_preg<-list() #number of records where pregnancy_code_vocabulary not of interest
  empty_pregnancy_code_preg<-list()#number of records with empty event code in the study population 
  
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
    setnames(df,"event_record_vocabulary","pregnancy_code_vocabulary")
    setnames(df,"start_date_record","pregnancy_code_date")
    setnames(df,"event_code","pregnancy_code")
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
    df<-df[,age_start_follow_up:=as.numeric(age_start_follow_up)]
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
    females_outside_age_events[[w]]<-df[age_start_follow_up< min_age_preg | age_start_follow_up> max_age_preg, .N]
    df<-df[age_start_follow_up>= min_age_preg & age_start_follow_up<=max_age_preg]
    
    events_study_pop_preg[[w]]<-df[,.N] #number of records for the study population, no selection criteria for time applied
    #transform into date variables
    df[,pregnancy_code_date:=as.Date(pregnancy_code_date,"%Y%m%d")] #transform to date variables
    #create year variable
    df[,year:=year(pregnancy_code_date)]
    #number of records with both pregnancy_code_date missing
    events_date_miss_preg[[w]]<-df[is.na(year),.N]
    #remove records with both dates missing
    df<-df[!is.na(year)]
    #remove records that are outside the obs_period for all subjects
    events_out_st_per_preg[[w]]<-df[pregnancy_code_date<start_follow_up | pregnancy_code_date>end_follow_up,.N] #number of records outside study population
    df[(pregnancy_code_date<start_follow_up | pregnancy_code_date>end_follow_up), obs_out:=1]
    df<-df[is.na(obs_out)] #remove records outside study period
    df[,obs_out:=NULL] 
    events_study_pop_obsper_preg[[w]]<-df[,.N] #number of records after removing records outside study period
    events_stdpop_no_meaning_preg[[w]]<-df[is.na(meaning),.N] #number of records with empty meaning
    df<-df[!is.na(meaning)] #remove records with empty meaning
    events_code_vocabulary_miss_preg[[w]]<-df[is.na(pregnancy_code) & is.na(pregnancy_code_vocabulary),.N]#numbe rof records with both event code and vocabulary missing
    df<-df[!is.na(pregnancy_code) | !is.na(pregnancy_code_vocabulary)]# remove records with both event code and event record vocabulary missing
    events_code_pres_voc_miss_preg[[w]]<-df[!is.na(pregnancy_code) & is.na(pregnancy_code_vocabulary),.N] #number of records where event code present but vocabulary missing
    df<-df[!is.na(pregnancy_code_vocabulary)] #remove empty vocabularies
    events_not_vocabularies_preg[[w]]<-df[pregnancy_code_vocabulary %!in% vocabularies_list,.N] #number of records where vocabularies doesn't match the codelist
    df<-df[pregnancy_code_vocabulary %in% vocabularies_list] #remove records where vocabularies are not of interest
    empty_pregnancy_code_preg[[w]]<-df[is.na(pregnancy_code),.N] #number of records with empty codes
    df<-df[!is.na(pregnancy_code)] #remove records with missing event code
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
      
      if(sum(df[!duplicated(pregnancy_code_vocabulary), pregnancy_code_vocabulary] %in% c("ICD10CM","ICD9CM", "ICPC2P", "ICD9", "ICD10","ICPC"))>0){
        for (i in 1:length(stage_pregnancy_start)){
          for(j in 1:length(stage_pregnancy_start[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(stage_pregnancy_start[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_start[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(stage_pregnancy_start[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_start[[i]])[j],filter_preg:=1]
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
      
      if(sum(df[!duplicated(pregnancy_code_vocabulary), pregnancy_code_vocabulary] %in% c("RCD2", "RCD"))>0){
        for (i in 1:length(stage_pregnancy_read)){
          for(j in 1:length(stage_pregnancy_read[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(stage_pregnancy_read[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_read[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(stage_pregnancy_read[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_read[[i]])[j],filter_preg:=1]
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
      
      if(sum(df[!duplicated(pregnancy_code_vocabulary), pregnancy_code_vocabulary] %in% c("SNOMEDCT_US"))>0){
        for (i in 1:length(stage_pregnancy_snomed)){
          for(j in 1:length(stage_pregnancy_snomed[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(stage_pregnancy_snomed[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_snomed[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(stage_pregnancy_snomed[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_snomed[[i]])[j],filter_preg:=1]
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
  #number of records in the study population with pregnancy_code_date inside study period (flowchart 10)
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
  empty_pregnancy_code_preg<-do.call(rbind,empty_pregnancy_code_preg)
  empty_pregnancy_code_preg<-sum(empty_pregnancy_code_preg)
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
                                             empty_pregnancy_code_preg,
                                             events_study_population_preg))
  
  
  rm(orig_no_rows_events_preg, events_excluded_meanings_preg,events_excluded_males,events_sex_not_specified_preg,
     events_study_pop_preg,females_outside_age_events,events_date_miss_preg,events_out_st_per_preg,
     events_study_pop_obsper_preg,events_stdpop_no_meaning_preg,events_code_vocabulary_miss_preg,events_code_pres_voc_miss_preg,
     events_not_vocabularies_preg,empty_pregnancy_code_preg)  
  
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
if(length(actual_tables$MEDICAL_OBSERVATIONS)>0){
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
  pregnancy_code_date_miss_preg<-list() #number of record with missing pregnancy_code_date
  mo_out_st_per_preg<-list() #number of MEDICAL_OBSERVATIONS records outside the observation period(check is done on individual level)
  mo_study_pop_obsper_preg<-list() #number of records in the study population inside study period
  mo_stdpop_no_meaning_preg<-list() #number of records in the study population with no meaning
  pregnancy_code_vocabulary_miss_preg<-list() #number of records with both mo code and mo record vocabulary missing
  pregnancy_code_pres_voc_miss_preg<-list() #number of records with missing vocabularies
  mo_not_vocabularies_preg<-list() #number of records where pregnancy_code_vocabulary not of interest
  empty_pregnancy_code_preg<-list()#number of records with empty mo code in the study population 
  
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
    setnames(df,"mo_date","pregnancy_code_date")
    setnames(df,"mo_code","pregnancy_code")
    setnames(df,"mo_record_vocabulary","pregnancy_code_vocabulary")
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
    df<-df[,age_start_follow_up:=as.numeric(age_start_follow_up)]
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
    females_outside_age_mo[[w]]<-df[age_start_follow_up< min_age_preg | age_start_follow_up> max_age_preg, .N]
    df<-df[age_start_follow_up>= min_age_preg & age_start_follow_up<=max_age_preg]
    
    mo_study_pop_preg[[w]]<-df[,.N] #number of records for the study population, no selection criteria for time applied
    #transform into date variables
    df[,pregnancy_code_date:=as.Date(pregnancy_code_date,"%Y%m%d")] #transform to date variables
    #create year variable
    df[,year:=year(pregnancy_code_date)]
    #number of records with both pregnancy_code_date missing
    pregnancy_code_date_miss_preg[[w]]<-df[is.na(year),.N]
    #remove records with both dates missing
    df<-df[!is.na(year)]
    #remove records that are outside the obs_period for all subjects
    mo_out_st_per_preg[[w]]<-df[pregnancy_code_date<start_follow_up | pregnancy_code_date>end_follow_up,.N] #number of records outside study population
    df[(pregnancy_code_date<start_follow_up | pregnancy_code_date>end_follow_up), obs_out:=1]
    df<-df[is.na(obs_out)] #remove records outside study period
    df[,obs_out:=NULL] 
    mo_study_pop_obsper_preg[[w]]<-df[,.N] #number of records after removing records outside study period
    mo_stdpop_no_meaning_preg[[w]]<-df[is.na(meaning),.N] #number of records with empty meaning
    df<-df[!is.na(meaning)] #remove records with empty meaning
    pregnancy_code_vocabulary_miss_preg[[w]]<-df[is.na(pregnancy_code) & is.na(pregnancy_code_vocabulary),.N]#numbe rof records with both mo code and vocabulary missing
    df<-df[!is.na(pregnancy_code) | !is.na(pregnancy_code_vocabulary)]# remove records with both mo code and mo record vocabulary missing
    pregnancy_code_pres_voc_miss_preg[[w]]<-df[!is.na(pregnancy_code) & is.na(pregnancy_code_vocabulary),.N] #number of records where mo code present but vocabulary missing
    df<-df[!is.na(pregnancy_code_vocabulary)] #remove empty vocabularies
    mo_not_vocabularies_preg[[w]]<-df[pregnancy_code_vocabulary %!in% vocabularies_list,.N] #number of records where vocabularies doesn't match the codelist
    df<-df[pregnancy_code_vocabulary %in% vocabularies_list] #remove records where vocabularies are not of interest
    empty_pregnancy_code_preg[[w]]<-df[is.na(pregnancy_code),.N] #number of records with empty codes
    df<-df[!is.na(pregnancy_code)] #remove records with missing mo code
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
      
      if(sum(df[!duplicated(pregnancy_code_vocabulary), pregnancy_code_vocabulary] %in% c("ICD10CM","ICD9CM", "ICPC2P"))>0){
        for (i in 1:length(stage_pregnancy_start)){
          for(j in 1:length(stage_pregnancy_start[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(stage_pregnancy_start[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_start[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(stage_pregnancy_start[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_start[[i]])[j],filter_preg:=1]
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
      
      if(sum(df[!duplicated(pregnancy_code_vocabulary), pregnancy_code_vocabulary] %in% c("RCD2", "RCD"))>0){
        for (i in 1:length(stage_pregnancy_read)){
          for(j in 1:length(stage_pregnancy_read[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(stage_pregnancy_read[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_read[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(stage_pregnancy_read[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_read[[i]])[j],filter_preg:=1]
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
      
      if(sum(df[!duplicated(pregnancy_code_vocabulary), pregnancy_code_vocabulary] %in% c("SNOMEDCT_US"))>0){
        for (i in 1:length(stage_pregnancy_snomed)){
          for(j in 1:length(stage_pregnancy_snomed[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(stage_pregnancy_snomed[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_snomed[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(stage_pregnancy_snomed[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_snomed[[i]])[j],filter_preg:=1]
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
  pregnancy_code_date_miss_preg<-do.call(rbind,pregnancy_code_date_miss_preg)
  pregnancy_code_date_miss_preg<-sum(pregnancy_code_date_miss_preg)
  #number of medicines records outside the observation period(check is done on individual level) (flowchart 9)
  print("Get number of records outside observation period.")
  mo_out_st_per_preg<-do.call(rbind,mo_out_st_per_preg) 
  mo_out_st_per_preg<-sum(mo_out_st_per_preg)
  #number of records in the study population with pregnancy_code_date inside study period (flowchart 10)
  print("Get number of records for the study population(time criteria applied).")
  mo_study_pop_obsper_preg<-do.call(rbind,mo_study_pop_obsper_preg) 
  mo_study_pop_obsper_preg<-sum(mo_study_pop_obsper_preg)
  #number of records in the study population with no meaning (flowchart 11)
  print("Get number of records with no meaning.")
  mo_stdpop_no_meaning_preg<-do.call(rbind,mo_stdpop_no_meaning_preg) 
  mo_stdpop_no_meaning_preg<-sum(mo_stdpop_no_meaning_preg) 
  #Number of records with both code and vocabulary variables missing (flowchart 12)
  print("Get number of records with both code and vocabulary variables missing")
  pregnancy_code_vocabulary_miss_preg<-do.call(rbind,pregnancy_code_vocabulary_miss_preg)
  pregnancy_code_vocabulary_miss_preg<-sum(pregnancy_code_vocabulary_miss_preg)
  #Number of records with empty vocabulary when code is present (flowchart 13)
  print("Get number of records with empty vocabulary when code is present")
  pregnancy_code_pres_voc_miss_preg<-do.call(rbind,pregnancy_code_pres_voc_miss_preg)
  pregnancy_code_pres_voc_miss_preg<-sum(pregnancy_code_pres_voc_miss_preg)
  #Number of records with vocabularies not in the codelist
  mo_not_vocabularies_preg<-do.call(rbind,mo_not_vocabularies_preg)
  mo_not_vocabularies_preg<-sum(mo_not_vocabularies_preg)
  #Number of records with empty codes
  empty_pregnancy_code_preg<-do.call(rbind,empty_pregnancy_code_preg)
  empty_pregnancy_code_preg<-sum(empty_pregnancy_code_preg)
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
                                                       pregnancy_code_date_miss_preg,
                                                       mo_out_st_per_preg,
                                                       mo_study_pop_obsper_preg,
                                                       mo_stdpop_no_meaning_preg,
                                                       pregnancy_code_vocabulary_miss_preg,
                                                       pregnancy_code_pres_voc_miss_preg,
                                                       mo_not_vocabularies_preg,
                                                       empty_pregnancy_code_preg,
                                                       mo_study_population_preg))
  
  
  rm(orig_no_rows_mo_preg, mo_excluded_meanings_preg,mo_excluded_males,mo_sex_not_specified_preg,
     mo_study_pop_preg,females_outside_age_mo,pregnancy_code_date_miss_preg,mo_out_st_per_preg,
     mo_study_pop_obsper_preg,mo_stdpop_no_meaning_preg,pregnancy_code_vocabulary_miss_preg,pregnancy_code_pres_voc_miss_preg,
     mo_not_vocabularies_preg,empty_pregnancy_code_preg)  
  
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
if(length(actual_tables$SURVEY_OBSERVATIONS)>0){
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
  pregnancy_code_date_miss_preg<-list() #number of record with missing pregnancy_code_date
  so_out_st_per_preg<-list() #number of SURVEY_OBSERVATIONS records outside the observation period(check is done on individual level)
  so_study_pop_obsper_preg<-list() #number of records in the study population inside study period
  so_stdpop_no_meaning_preg<-list() #number of records in the study population with no meaning
  pregnancy_code_vocabulary_miss_preg<-list() #number of records with both mo code and mo record vocabulary missing
  pregnancy_code_pres_voc_miss_preg<-list() #number of records with missing vocabularies
  so_not_vocabularies_preg<-list() #number of records where pregnancy_code_vocabulary not of interest
  empty_pregnancy_code_preg<-list()#number of records with empty mo code in the study population 
  
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
    setnames(df,"so_date","pregnancy_code_date")
    setnames(df,"so_source_value","pregnancy_code")
    setnames(df,"so_unit","pregnancy_code_vocabulary")
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
    df<-df[,age_start_follow_up:=as.numeric(age_start_follow_up)]
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
    females_outside_age_so[[w]]<-df[age_start_follow_up< min_age_preg | age_start_follow_up> max_age_preg, .N]
    df<-df[age_start_follow_up>= min_age_preg & age_start_follow_up<=max_age_preg]
    
    so_study_pop_preg[[w]]<-df[,.N] #number of records for the study population, no selection criteria for time applied
    #transform into date variables
    df[,pregnancy_code_date:=as.Date(pregnancy_code_date,"%Y%m%d")] #transform to date variables
    #create year variable
    df[,year:=year(pregnancy_code_date)]
    #number of records with both pregnancy_code_date missing
    pregnancy_code_date_miss_preg[[w]]<-df[is.na(year),.N]
    #remove records with both dates missing
    df<-df[!is.na(year)]
    #remove records that are outside the obs_period for all subjects
    so_out_st_per_preg[[w]]<-df[pregnancy_code_date<start_follow_up | pregnancy_code_date>end_follow_up,.N] #number of records outside study population
    df[(pregnancy_code_date<start_follow_up | pregnancy_code_date>end_follow_up), obs_out:=1]
    df<-df[is.na(obs_out)] #remove records outside study period
    df[,obs_out:=NULL] 
    so_study_pop_obsper_preg[[w]]<-df[,.N] #number of records after removing records outside study period
    so_stdpop_no_meaning_preg[[w]]<-df[is.na(meaning),.N] #number of records with empty meaning
    df<-df[!is.na(meaning)] #remove records with empty meaning
    pregnancy_code_vocabulary_miss_preg[[w]]<-df[is.na(pregnancy_code) & is.na(pregnancy_code_vocabulary),.N]#numbe rof records with both mo code and vocabulary missing
    df<-df[!is.na(pregnancy_code) | !is.na(pregnancy_code_vocabulary)]# remove records with both mo code and mo record vocabulary missing
    pregnancy_code_pres_voc_miss_preg[[w]]<-df[!is.na(pregnancy_code) & is.na(pregnancy_code_vocabulary),.N] #number of records where mo code present but vocabulary missing
    df<-df[!is.na(pregnancy_code_vocabulary)] #remove empty vocabularies
    so_not_vocabularies_preg[[w]]<-df[pregnancy_code_vocabulary %!in% vocabularies_list,.N] #number of records where vocabularies doesn't match the codelist
    df<-df[pregnancy_code_vocabulary %in% vocabularies_list] #remove records where vocabularies are not of interest
    empty_pregnancy_code_preg[[w]]<-df[is.na(pregnancy_code),.N] #number of records with empty codes
    df<-df[!is.na(pregnancy_code)] #remove records with missing mo code
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
      
      if(sum(df[!duplicated(pregnancy_code_vocabulary), pregnancy_code_vocabulary] %in% c("ICD10CM","ICD9CM", "ICPC2P"))>0){
        for (i in 1:length(stage_pregnancy_start)){
          for(j in 1:length(stage_pregnancy_start[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(stage_pregnancy_start[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_start[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(stage_pregnancy_start[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_start[[i]])[j],filter_preg:=1]
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
      
      if(sum(df[!duplicated(pregnancy_code_vocabulary), pregnancy_code_vocabulary] %in% c("RCD2", "RCD"))>0){
        for (i in 1:length(stage_pregnancy_read)){
          for(j in 1:length(stage_pregnancy_read[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(stage_pregnancy_read[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_read[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(stage_pregnancy_read[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_read[[i]])[j],filter_preg:=1]
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
      
      if(sum(df[!duplicated(pregnancy_code_vocabulary), pregnancy_code_vocabulary] %in% c("SNOMEDCT_US"))>0){
        for (i in 1:length(stage_pregnancy_snomed)){
          for(j in 1:length(stage_pregnancy_snomed[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(stage_pregnancy_snomed[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_snomed[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(stage_pregnancy_snomed[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_snomed[[i]])[j],filter_preg:=1]
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
  pregnancy_code_date_miss_preg<-do.call(rbind,pregnancy_code_date_miss_preg)
  pregnancy_code_date_miss_preg<-sum(pregnancy_code_date_miss_preg)
  #number of medicines records outside the observation period(check is done on individual level) (flowchart 9)
  print("Get number of records outside observation period.")
  so_out_st_per_preg<-do.call(rbind,so_out_st_per_preg) 
  so_out_st_per_preg<-sum(so_out_st_per_preg)
  #number of records in the study population with pregnancy_code_date inside study period (flowchart 10)
  print("Get number of records for the study population(time criteria applied).")
  so_study_pop_obsper_preg<-do.call(rbind,so_study_pop_obsper_preg) 
  so_study_pop_obsper_preg<-sum(so_study_pop_obsper_preg)
  #number of records in the study population with no meaning (flowchart 11)
  print("Get number of records with no meaning.")
  so_stdpop_no_meaning_preg<-do.call(rbind,so_stdpop_no_meaning_preg) 
  so_stdpop_no_meaning_preg<-sum(so_stdpop_no_meaning_preg) 
  #Number of records with both code and vocabulary variables missing (flowchart 12)
  print("Get number of records with both code and vocabulary variables missing")
  pregnancy_code_vocabulary_miss_preg<-do.call(rbind,pregnancy_code_vocabulary_miss_preg)
  pregnancy_code_vocabulary_miss_preg<-sum(pregnancy_code_vocabulary_miss_preg)
  #Number of records with empty vocabulary when code is present (flowchart 13)
  print("Get number of records with empty vocabulary when code is present")
  pregnancy_code_pres_voc_miss_preg<-do.call(rbind,pregnancy_code_pres_voc_miss_preg)
  pregnancy_code_pres_voc_miss_preg<-sum(pregnancy_code_pres_voc_miss_preg)
  #Number of records with vocabularies not in the codelist
  so_not_vocabularies_preg<-do.call(rbind,so_not_vocabularies_preg)
  so_not_vocabularies_preg<-sum(so_not_vocabularies_preg)
  #Number of records with empty codes
  empty_pregnancy_code_preg<-do.call(rbind,empty_pregnancy_code_preg)
  empty_pregnancy_code_preg<-sum(empty_pregnancy_code_preg)
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
                                                      pregnancy_code_date_miss_preg,
                                                      so_out_st_per_preg,
                                                      so_study_pop_obsper_preg,
                                                      so_stdpop_no_meaning_preg,
                                                      pregnancy_code_vocabulary_miss_preg,
                                                      pregnancy_code_pres_voc_miss_preg,
                                                      so_not_vocabularies_preg,
                                                      empty_pregnancy_code_preg,
                                                      so_study_population_preg))
  
  
  rm(orig_no_rows_so_preg, so_excluded_meanings_preg,so_excluded_males,so_sex_not_specified_preg,
     so_study_pop_preg,females_outside_age_so,pregnancy_code_date_miss_preg,so_out_st_per_preg,
     so_study_pop_obsper_preg,so_stdpop_no_meaning_preg,pregnancy_code_vocabulary_miss_preg,pregnancy_code_pres_voc_miss_preg,
     so_not_vocabularies_preg,empty_pregnancy_code_preg)  
  
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
if(length(actual_tables$SURVEY_ID)>0){
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
    setnames(df,"survey_date","pregnancy_code_date")
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
    df<-df[,age_start_follow_up:=as.numeric(age_start_follow_up)]
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
    females_outside_age_si[[w]]<-df[age_start_follow_up< min_age_preg | age_start_follow_up> max_age_preg, .N]
    df<-df[age_start_follow_up>= min_age_preg & age_start_follow_up<=max_age_preg]
    #number of records with meanings not of interest
    si_meanings_not_of_interest[[w]]<-df[meaning %!in% meanings_birth_registry,.N]
    #select only meaning refferring to birth_registry
    df<-df[meaning %in% meanings_birth_registry]
    
    si_study_pop_preg[[w]]<-df[,.N] #number of records for the study population, no selection criteria for time applied
    #transform into date variables
    df[,pregnancy_code_date:=as.Date(pregnancy_code_date,"%Y%m%d")] #transform to date variables
    #create year variable
    df[,year:=year(pregnancy_code_date)]
    #number of records with both pregnancy_code_date missing
    si_date_miss_preg[[w]]<-df[is.na(year),.N]
    #remove records with both dates missing
    df<-df[!is.na(year)]
    #remove records that are outside the obs_period for all subjects
    si_out_st_per_preg[[w]]<-df[pregnancy_code_date<start_follow_up | pregnancy_code_date>end_follow_up,.N] #number of records outside study population
    df[(pregnancy_code_date<start_follow_up | pregnancy_code_date>end_follow_up), obs_out:=1]
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

if(subpopulations_present=="Yes"){
  write.csv(flowchart_preg, paste0(preg_dir, subpopulations_names[s], "/", subpopulations_names[s], "_pregnancy_flowchart.csv"), row.names = F)
} else {
  write.csv(flowchart_preg, paste0(preg_dir, "pregnancy_flowchart.csv"), row.names = F)
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
  write.csv(flowchart_preg, paste0(preg_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_pregnancy_flowchart_masked.csv"), row.names = F)
} else {
  write.csv(flowchart_preg, paste0(preg_dir, "Masked/", "pregnancy_flowchart_masked.csv"), row.names = F)
}

###################
#description
###################
description_preg<-data.table(description_events_preg,description_mo_preg[,2], description_so_preg[,2], description_si_preg[,2])
rm(description_events_preg,description_mo_preg,description_so_preg, description_si_preg)

if(subpopulations_present=="Yes"){
  write.csv(description_preg, paste0(preg_dir, subpopulations_names[s], "/", subpopulations_names[s], "_pregnancy_description.csv"), row.names = F)
} else {
  write.csv(description_preg, paste0(preg_dir, "pregnancy_description.csv"), row.names = F)
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
  write.csv(description_preg, paste0(preg_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_pregnancy_description_masked.csv"), row.names = F)
} else {
  write.csv(description_preg, paste0(preg_dir, "Masked/", "pregnancy_description_masked.csv"), row.names = F)
}

############################################################################################
#Counts and rates
############################################################################################
print("Combine datasets for pooling results.")
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
##########################################
#combine all files from all tables in one
#create a loop that goes through all stages of pregnancy
###########
#start_of_pregnancy
##########
if(length(pregnancy_files_events$start_of_pregnancy)>0){
  combined_start_ev<-lapply(paste0(preg_ev_tmp,pregnancy_files_events$start_of_pregnancy), readRDS) #combine all files for one pregnancy stage
  combined_start_ev<-do.call(rbind,combined_start_ev)
  combined_start_ev[,lag:=time_lag[stage_of_pregnancy == "start_of_pregnancy",time_lag]] #create lag variable based on condition
  combined_start_ev[,pregnancy_code:=NULL][,pregnancy_code_vocabulary:=NULL][,filter_preg:=NULL]
  
} else {combined_start_ev<-NULL}
if(length(pregnancy_files_mo$start_of_pregnancy)>0){
  combined_start_mo<-lapply(paste0(preg_m_tmp,pregnancy_files_mo$start_of_pregnancy), readRDS) #combine all files for one pregnancy stage
  combined_start_mo<-do.call(rbind,combined_start_mo)
  combined_start_mo[,lag:=time_lag[stage_of_pregnancy == "start_of_pregnancy",time_lag]] #create lag variable based on condition
  combined_start_mo[,pregnancy_code:=NULL][,pregnancy_code_vocabulary:=NULL][,filter_preg:=NULL]
  
} else {combined_start_mo<-NULL}
start_pregnancy<-rbind(combined_start_ev,combined_start_mo)
rm(combined_start_ev,combined_start_mo)
if(length(pregnancy_files_so$start_of_pregnancy)>0){
  combined_start_so<-lapply(paste0(preg_s_tmp,pregnancy_files_so$start_of_pregnancy), readRDS) #combine all files for one pregnancy stage
  combined_start_so<-do.call(rbind,combined_start_so)
  combined_start_so[,lag:=time_lag[stage_of_pregnancy == "start_of_pregnancy",time_lag]] #create lag variable based on condition
  combined_start_so[,pregnancy_code:=NULL][,pregnancy_code_vocabulary:=NULL][,filter_preg:=NULL]
  
} else {combined_start_so<-NULL}
start_pregnancy<-rbind(start_pregnancy, combined_start_so)
rm(combined_start_so)
if(!is.null(start_pregnancy)){
  if (subpopulations_present=="Yes"){
    saveRDS(start_pregnancy, paste0(preg_pop, subpopulations_names[s], "/", subpopulations_names[s],"_","start_of_pregnancy.rds"))
  } else {
    saveRDS(start_pregnancy, paste0(preg_pop,"start_of_pregnancy.rds"))
  }
}

#remove all start files
if(length(pregnancy_files_events$start_of_pregnancy)>0){
  for (i in 1:length(pregnancy_files_events$start_of_pregnancy)){
    file.remove(paste0(preg_ev_tmp, pregnancy_files_events$start_of_pregnancy[[i]]))
  }
}
if(length(pregnancy_files_mo$start_of_pregnancy)>0){
  for (i in 1:length(pregnancy_files_mo$start_of_pregnancy)){
    file.remove(paste0(preg_m_tmp, pregnancy_files_mo$start_of_pregnancy[[i]]))
  }
}
if(length(pregnancy_files_so$start_of_pregnancy)>0){
  for (i in 1:length(pregnancy_files_so$start_of_pregnancy)){
    file.remove(paste0(preg_s_tmp, pregnancy_files_so$start_of_pregnancy[[i]]))
  }
}


###########
#ongoing_pregnancy
##########
if(length(pregnancy_files_events$ongoing_pregnancy)>0){
  combined_ongoing_ev<-lapply(paste0(preg_ev_tmp,pregnancy_files_events$ongoing_pregnancy), readRDS) #combine all files for one pregnancy stage
  combined_ongoing_ev<-do.call(rbind,combined_ongoing_ev)
  combined_ongoing_ev[,lag:=time_lag[stage_of_pregnancy == "ongoing_pregnancy",time_lag]] #create lag variable based on condition
  combined_ongoing_ev[,pregnancy_code:=NULL][,pregnancy_code_vocabulary:=NULL][,filter_preg:=NULL]
  
} else {combined_ongoing_ev<-NULL}
if(length(pregnancy_files_mo$ongoing_pregnancy)>0){
  combined_ongoing_mo<-lapply(paste0(preg_m_tmp,pregnancy_files_mo$ongoing_pregnancy), readRDS) #combine all files for one pregnancy stage
  combined_ongoing_mo<-do.call(rbind,combined_ongoing_mo)
  combined_ongoing_mo[,lag:=time_lag[stage_of_pregnancy == "ongoing_pregnancy",time_lag]] #create lag variable based on condition
  combined_ongoing_mo[,pregnancy_code:=NULL][,pregnancy_code_vocabulary:=NULL][,filter_preg:=NULL]
  
} else {combined_ongoing_mo<-NULL}
ongoing_pregnancy<-rbind(combined_ongoing_ev,combined_ongoing_mo)
rm(combined_ongoing_ev,combined_ongoing_mo)
if(length(pregnancy_files_so$ongoing_pregnancy)>0){
  combined_ongoing_so<-lapply(paste0(preg_s_tmp,pregnancy_files_so$ongoing_pregnancy), readRDS) #combine all files for one pregnancy stage
  combined_ongoing_so<-do.call(rbind,combined_ongoing_so)
  combined_ongoing_so[,lag:=time_lag[stage_of_pregnancy == "ongoing_pregnancy",time_lag]] #create lag variable based on condition
  combined_ongoing_so[,pregnancy_code:=NULL][,pregnancy_code_vocabulary:=NULL][,filter_preg:=NULL]
  
} else {combined_ongoing_so<-NULL}
ongoing_pregnancy<-rbind(ongoing_pregnancy,combined_ongoing_so)
rm(combined_ongoing_so)
if(!is.null(ongoing_pregnancy)){
  if (subpopulations_present=="Yes"){
    saveRDS(ongoing_pregnancy, paste0(preg_pop, subpopulations_names[s], "/", subpopulations_names[s],"_","ongoing_pregnancy.rds"))
  } else {
    saveRDS(ongoing_pregnancy, paste0(preg_pop,"ongoing_pregnancy.rds"))
  }
}

#remove all start files
if(length(pregnancy_files_events$ongoing_pregnancy)>0){
  for (i in 1:length(pregnancy_files_events$ongoing_pregnancy)){
    file.remove(paste0(preg_ev_tmp, pregnancy_files_events$ongoing_pregnancy[[i]]))
  }
}
if(length(pregnancy_files_mo$ongoing_pregnancy)>0){
  for (i in 1:length(pregnancy_files_mo$ongoing_pregnancy)){
    file.remove(paste0(preg_m_tmp, pregnancy_files_mo$ongoing_pregnancy[[i]]))
  }
}
if(length(pregnancy_files_so$ongoing_pregnancy)>0){
  for (i in 1:length(pregnancy_files_so$ongoing_pregnancy)){
    file.remove(paste0(preg_s_tmp, pregnancy_files_so$ongoing_pregnancy[[i]]))
  }
}

###########
#interruption_pregnancy
##########
if(length(pregnancy_files_events$interruption_pregnancy)>0){
  combined_int_ev<-lapply(paste0(preg_ev_tmp,pregnancy_files_events$interruption_pregnancy), readRDS) #combine all files for one pregnancy stage
  combined_int_ev<-do.call(rbind,combined_int_ev)
  combined_int_ev[,lag:=time_lag[stage_of_pregnancy == "interruption_pregnancy",time_lag]] #create lag variable based on condition
  combined_int_ev[,pregnancy_code:=NULL][,pregnancy_code_vocabulary:=NULL][,filter_preg:=NULL]
  
} else {combined_int_ev<-NULL}
if(length(pregnancy_files_mo$interruption_pregnancy)>0){
  combined_int_mo<-lapply(paste0(preg_m_tmp,pregnancy_files_mo$interruption_pregnancy), readRDS) #combine all files for one pregnancy stage
  combined_int_mo<-do.call(rbind,combined_int_mo)
  combined_int_mo[,lag:=time_lag[stage_of_pregnancy == "interruption_pregnancy",time_lag]] #create lag variable based on condition
  combined_int_mo[,pregnancy_code:=NULL][,pregnancy_code_vocabulary:=NULL][,filter_preg:=NULL]
} else {combined_int_mo<-NULL}
interruption_pregnancy<-rbind(combined_int_ev,combined_int_mo)
rm(combined_int_ev,combined_int_mo)
if(length(pregnancy_files_so$interruption_pregnancy)>0){
  combined_int_so<-lapply(paste0(preg_s_tmp,pregnancy_files_so$interruption_pregnancy), readRDS) #combine all files for one pregnancy stage
  combined_int_so<-do.call(rbind,combined_int_so)
  combined_int_so[,lag:=time_lag[stage_of_pregnancy == "interruption_pregnancy",time_lag]] #create lag variable based on condition
  combined_int_so[,pregnancy_code:=NULL][,pregnancy_code_vocabulary:=NULL][,filter_preg:=NULL]
  
} else {combined_int_so<-NULL}
interruption_pregnancy<-rbind(interruption_pregnancy,combined_int_so)
rm(combined_int_so)
if(!is.null(interruption_pregnancy)){
  if (subpopulations_present=="Yes"){
    saveRDS(interruption_pregnancy, paste0(preg_pop, subpopulations_names[s], "/", subpopulations_names[s],"_","interruption_pregnancy.rds"))
  } else {
    saveRDS(interruption_pregnancy, paste0(preg_pop,"interruption_pregnancy.rds"))
  }
}

#remove all start files
if(length(pregnancy_files_events$interruption_pregnancy)>0){
  for (i in 1:length(pregnancy_files_events$interruption_pregnancy)){
    file.remove(paste0(preg_ev_tmp, pregnancy_files_events$interruption_pregnancy[[i]]))
  }
}
if(length(pregnancy_files_mo$interruption_pregnancy)>0){
  for (i in 1:length(pregnancy_files_mo$interruption_pregnancy)){
    file.remove(paste0(preg_m_tmp, pregnancy_files_mo$interruption_pregnancy[[i]]))
  }
}
if(length(pregnancy_files_so$interruption_pregnancy)>0){
  for (i in 1:length(pregnancy_files_so$interruption_pregnancy)){
    file.remove(paste0(preg_s_tmp, pregnancy_files_so$interruption_pregnancy[[i]]))
  }
}

###########
#end_of_pregnancy
##########
if(length(pregnancy_files_events$end_of_pregnancy)>0){
  combined_end_ev<-lapply(paste0(preg_ev_tmp,pregnancy_files_events$end_of_pregnancy), readRDS) #combine all files for one pregnancy stage
  combined_end_ev<-do.call(rbind,combined_end_ev)
  combined_end_ev[,lag:=time_lag[stage_of_pregnancy == "end_of_pregnancy",time_lag]] #create lag variable based on condition
  combined_end_ev[,pregnancy_code:=NULL][,pregnancy_code_vocabulary:=NULL][,filter_preg:=NULL]
} else {combined_end_ev<-NULL}
if(length(pregnancy_files_mo$end_of_pregnancy)>0){
  combined_end_mo<-lapply(paste0(preg_m_tmp,pregnancy_files_mo$end_of_pregnancy), readRDS) #combine all files for one pregnancy stage
  combined_end_mo<-do.call(rbind,combined_end_mo)
  combined_end_mo[,lag:=time_lag[stage_of_pregnancy == "end_of_pregnancy",time_lag]] #create lag variable based on condition
  combined_end_mo[,pregnancy_code:=NULL][,pregnancy_code_vocabulary:=NULL][,filter_preg:=NULL]
} else {combined_end_mo<-NULL}
end_of_pregnancy<-rbind(combined_end_ev,combined_end_mo)
rm(combined_end_ev,combined_end_mo)
if(length(pregnancy_files_so$end_of_pregnancy)>0){
  combined_end_so<-lapply(paste0(preg_s_tmp,pregnancy_files_so$end_of_pregnancy), readRDS) #combine all files for one pregnancy stage
  combined_end_so<-do.call(rbind,combined_end_so)
  combined_end_so[,lag:=time_lag[stage_of_pregnancy == "end_of_pregnancy",time_lag]] #create lag variable based on condition
  combined_end_so[,pregnancy_code:=NULL][,pregnancy_code_vocabulary:=NULL][,filter_preg:=NULL]
} else {combined_end_so<-NULL}
end_of_pregnancy<-rbind(end_of_pregnancy,combined_end_so)
rm(combined_end_so)
if(length(pregnancy_files_si$end_of_pregnancy)>0){
  combined_end_si<-lapply(paste0(preg_si_tmp,pregnancy_files_si$end_of_pregnancy), readRDS) #combine all files for one pregnancy stage
  combined_end_si<-do.call(rbind,combined_end_si)
  combined_end_si[,lag:=time_lag[stage_of_pregnancy == "end_of_pregnancy",time_lag]] #create lag variable based on condition
} else {combined_end_si<-NULL}
end_of_pregnancy<-rbind(end_of_pregnancy,combined_end_si)
rm(combined_end_si)

if(!is.null(end_of_pregnancy)){
  if (subpopulations_present=="Yes"){
    saveRDS(end_of_pregnancy, paste0(preg_pop, subpopulations_names[s], "/", subpopulations_names[s],"_","end_of_pregnancy.rds"))
  } else {
    saveRDS(end_of_pregnancy, paste0(preg_pop,"end_of_pregnancy.rds"))
  }
}

#remove all start files
if(length(pregnancy_files_events$end_of_pregnancy)>0){
  for (i in 1:length(pregnancy_files_events$end_of_pregnancy)){
    file.remove(paste0(preg_ev_tmp, pregnancy_files_events$end_of_pregnancy[[i]]))
  }
}
if(length(pregnancy_files_mo$end_of_pregnancy)>0){
  for (i in 1:length(pregnancy_files_mo$end_of_pregnancy)){
    file.remove(paste0(preg_m_tmp, pregnancy_files_mo$end_of_pregnancy[[i]]))
  }
}
if(length(pregnancy_files_so$end_of_pregnancy)>0){
  for (i in 1:length(pregnancy_files_so$end_of_pregnancy)){
    file.remove(paste0(preg_s_tmp, pregnancy_files_so$end_of_pregnancy[[i]]))
  }
}
if(length(pregnancy_files_si$end_of_pregnancy)>0){
  for (i in 1:length(pregnancy_files_si$end_of_pregnancy)){
    file.remove(paste0(preg_si_tmp, pregnancy_files_si$end_of_pregnancy[[i]]))
  }
}

##############
if(subpopulations_present=="Yes"){
  pregnancy_files<-list()
  pregnancy_files$start_of_pregnancy<-list.files(preg_pop,subpopulations_names[s], "start")
  pregnancy_files$ongoing_pregnancy<-list.files(preg_pop,subpopulations_names[s], "ongoing")
  pregnancy_files$interruption_pregnancy<-list.files(preg_pop, subpopulations_names[s], "interruption")
  pregnancy_files$end_of_pregnancy<-list.files(preg_pop, subpopulations_names[s], "end")
  
} else {
  pregnancy_files<-list()
  pregnancy_files$start_of_pregnancy<-list.files(preg_pop,"start")
  pregnancy_files$ongoing_pregnancy<-list.files(preg_pop,"ongoing")
  pregnancy_files$interruption_pregnancy<-list.files(preg_pop,"interruption")
  pregnancy_files$end_of_pregnancy<-list.files(preg_pop,"end")
}
#remove empty list elements
pregnancy_files<-Filter(length,pregnancy_files)

for (preg_ind in 1:length(pregnancy_files)){
  #Load file
  if(subpopulations_present=="Yes"){
    pregnancy_dt<-readRDS(paste0(preg_pop,subpopulations_names[s],  pregnancy_files[[preg_ind]]))
  } else {
    pregnancy_dt<-readRDS(paste0(preg_pop, pregnancy_files[[preg_ind]])) 
  }
  
  ####################
  #graph1
  ####################
  
  #get number of rows per unique combination
  pregnancy_graph<-pregnancy_dt[,.N,by=c("person_id","year", "condition")]
  setnames(pregnancy_graph, "N", "code_count")
  #Create code_1 meaning each person has only one code per year(type of code not taken into account)
  code_1_all<-pregnancy_graph[code_count==1 & !duplicated(person_id),.N, by="year"]
  setnames(code_1_all,"N","code_1_per_year")
  code_2_all<-pregnancy_graph[code_count==2 & !duplicated(person_id),.N, by="year"]
  setnames(code_2_all,"N","codes_2_per_year")
  code_3_all<-pregnancy_graph[code_count==3 & !duplicated(person_id),.N, by="year"]
  setnames(code_3_all,"N","codes_3_per_year")
  code_more_all<-pregnancy_graph[code_count>3 & !duplicated(person_id),.N, by="year"]
  setnames(code_more_all,"N","codes_min_4_per_year")
  total_women_by_year<-pregnancy_graph[!duplicated(person_id),.N, by="year"]
  setnames(total_women_by_year,"N","total_women")
  ###########
  graph1<-code_1_all
  rm(code_1_all)
  graph1<-merge(graph1, code_2_all, by="year", all=T)
  rm(code_2_all)
  graph1<-merge(graph1, code_3_all, by="year", all=T)
  rm(code_3_all)
  graph1<-merge(graph1, code_more_all, by="year", all=T)
  rm(code_more_all)
  graph1<-merge(graph1,total_women_by_year, by="year",all=T)
  rm(total_women_by_year)
  graph1[is.na(code_1_per_year),code_1_per_year:=0][is.na(codes_2_per_year),codes_2_per_year:=0][is.na(codes_3_per_year),codes_3_per_year:=0][is.na(codes_min_4_per_year),codes_min_4_per_year:=0]
  
  saveRDS(graph1,paste0(preg_tmp, "graph1_",  pregnancy_files[[preg_ind]]))
  rm(graph1)
  ##################
  #graph2
  #Create code_1 meaning each person has only one code per year(type of code not taken into account)
  code_1_all<-pregnancy_graph[code_count==1 & !duplicated(person_id) ,.N, by=c("year","condition")]
  setnames(code_1_all,"N","code_1_per_year")
  code_2_all<-pregnancy_graph[code_count==2 & !duplicated(person_id),.N, by=c("year","condition")]
  setnames(code_2_all,"N","codes_2_per_year")
  code_3_all<-pregnancy_graph[code_count==3 & !duplicated(person_id),.N, by=c("year","condition")]
  setnames(code_3_all,"N","codes_3_per_year")
  code_more_all<-pregnancy_graph[code_count>3 & !duplicated(person_id),.N, by=c("year","condition")]
  setnames(code_more_all,"N","codes_min_4_per_year")
  total_women_by_year<-pregnancy_graph[!duplicated(person_id),.N, by=c("year", "condition")]
  setnames(total_women_by_year,"N","total_women")
  rm(pregnancy_graph)
  
  graph2<-code_1_all
  rm(code_1_all)
  graph2<-merge(graph2, code_2_all, by=c("year","condition"), all=T)
  rm(code_2_all)
  graph2<-merge(graph2, code_3_all, by=c("year","condition"), all=T)
  rm(code_3_all)
  graph2<-merge(graph2, code_more_all, by=c("year","condition"), all=T)
  rm(code_more_all)
  graph2<-merge(graph2, total_women_by_year, by=c("year","condition"), all=T)
  graph2[is.na(code_1_per_year),code_1_per_year:=0][is.na(codes_2_per_year),codes_2_per_year:=0][is.na(codes_3_per_year),codes_3_per_year:=0][is.na(codes_min_4_per_year),codes_min_4_per_year:=0]
  setnames(graph2, "condition", "stage_of_pregnancy")
  
  saveRDS(graph2,paste0(preg_tmp, "graph2_",  pregnancy_files[[preg_ind]]))
  rm(graph2)
  
  
  #########
  #tab19
  ########
  #sort dataset by person id and date of pregnancy code to sleect the first one
  pregnancy_dt<-pregnancy_dt[order(person_id, pregnancy_code_date),]
  #create variable rowid by person id to identify all records for each person
  pregnancy_dt[,rowid:=rowid(person_id)]
  #do the date difference between the first code and end of follow up for each person
  pregnancy_dt[,end_follow_up:=as.IDate(end_follow_up, "%Y%m%d")][,pregnancy_code_date:=as.IDate(pregnancy_code_date, "%Y%m%d")]
  pregnancy_dt[rowid==1, date_dif:=end_follow_up-pregnancy_code_date]
  #if the date diff is more than 365 days create filter==1
  pregnancy_dt[date_dif>365, filter:=1]
  
  #create table
  no_women_365<-pregnancy_dt[filter==1,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("year", "condition"), .SDcols="person_id"]
  setnames(no_women_365, "person_id", "no_women_365")
  no_women_total<-pregnancy_dt[,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("year", "condition"), .SDcols="person_id"]
  setnames(no_women_total, "person_id", "no_women")
  no_women_365<-merge(no_women_365, no_women_total, by=c("year","condition"), all=T)
  no_women_365<-no_women_365[is.na(no_women_365), no_women_365:=0]
  rm(no_women_total)
  pregnancy_dt[,date_dif:=as.numeric(date_dif)]
  median<-pregnancy_dt[rowid==1 & filter==1, lapply(.SD, function(x) median(x, na.rm = T)), by=c("year", "condition"), .SDcols="date_dif"]
  setnames(median, "date_dif", "median_follow_up")
  no_women_365<-merge(no_women_365,median, by=c("year", "condition"), all=T)
  no_women_365[is.na(median_follow_up),median_follow_up:=0]
  rm(median)
  min<-pregnancy_dt[rowid==1 & filter==1, lapply(.SD, function(x) min(x, na.rm = T)), by=c("year", "condition"), .SDcols="date_dif"]
  setnames(min,"date_dif","min")
  no_women_365<-merge(no_women_365,min, by=c("year","condition"), all=T)
  no_women_365[is.na(min),min:=0]
  rm(min)
  max<-pregnancy_dt[rowid==1 & filter==1, lapply(.SD, function(x) max(x, na.rm = T)), by=c("year", "condition"), .SDcols="date_dif"]
  setnames(max,"date_dif","max")
  no_women_365<-merge(no_women_365,max, by=c("year","condition"), all=T)
  no_women_365[is.na(max),max:=0]
  rm(max)
  no_women_365[,range_follow_up:=paste(min,max, sep="-")]
  no_women_365[,min:=NULL][,max:=NULL]
  no_women_365[,percentage_women_365_fup:=round((no_women_365/no_women)*100,2)]
  
  saveRDS(no_women_365, paste0(preg_tmp, "tab19_", names(pregnancy_files)[preg_ind], ".rds"))
  rm(no_women_365)
  pregnancy_dt[,date_dif:=NULL][,filter:=NULL][,rowid:=NULL]
  
  #Apply count person time(not aggregated)
  pregnancy_dt<-CountPersonTime2(Dataset_events = unique(pregnancy_dt[,.(person_id, condition, pregnancy_code_date)]),
                                 Dataset = unique(pregnancy_dt[,.(person_id, birth_date, start_follow_up, end_follow_up)]),
                                 Person_id = "person_id",
                                 Start_study_time =start_study_date2,
                                 End_study_time =end_study_date2,
                                 Start_date = "start_follow_up",
                                 End_date = "end_follow_up",
                                 Birth_date = "birth_date",
                                 Increment = "year",
                                 Unit_of_age = "year",
                                 Strata = NULL,
                                 include_remaning_ages = TRUE,
                                 Aggregate = F,
                                 Outcomes_rec = unique(pregnancy_dt[,condition]),
                                 Name_event = "condition",
                                 Date_event = "pregnancy_code_date",
                                 Rec_period =  unique(pregnancy_dt[,lag]),
                                 Age_bands = c(12,29,39,49),
                                 print = F, 
                                 check_overlap = F)
  pregnancy_dt[,Persontime:=NULL]
  names(pregnancy_dt)<-c("person_id", "age_band","year", "person_years","no_records")
  
  #tab18
  no_women<-pregnancy_dt[,lapply(.SD,function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("year","age_band")]
  setnames(no_women, "person_id","no_women")
  person_years<-pregnancy_dt[,lapply(.SD,sum), .SDcols="person_years", by=c("year","age_band")]
  tab18<-no_women
  rm(no_women)
  tab18<-merge(tab18, person_years, by=c("year","age_band"))
  rm(person_years)
  tab18[,women_per_1000_py:=round((no_women/person_years)*1000,2)]
  tab18[,stage_of_pregnancy:=names(pregnancy_files)[preg_ind]]
  
  saveRDS(tab18, paste0(preg_tmp, "tab18_", names(pregnancy_files)[preg_ind], ".rds"))
  rm(tab18)
  
  #######
  no_women<-pregnancy_dt[,lapply(.SD,function(x) length(unique(na.omit(x)))), .SDcols="person_id", by=c("year")]
  setnames(no_women, "person_id","no_women")
  person_years<-pregnancy_dt[,lapply(.SD,sum), .SDcols="person_years", by=c("year")]
  tab17<-no_women
  rm(no_women)
  tab17<-merge(tab17, person_years, by=c("year"))
  rm(person_years)
  tab17[,women_per_1000_py:=round((no_women/person_years)*1000,2)]
  tab17[,stage_of_pregnancy:=names(pregnancy_files)[preg_ind]]
  
  saveRDS(tab17, paste0(preg_tmp, "tab17_", names(pregnancy_files)[preg_ind], ".rds"))
  rm(tab17)
  rm(pregnancy_dt)
}

#############################################
#tab17
############################################
tab17_files<-list.files(preg_tmp, "tab17")
tab17<-lapply(paste0(preg_tmp, tab17_files), readRDS)
tab17<-do.call(rbind,tab17)
setcolorder(tab17,c("stage_of_pregnancy","year","no_women","person_years","women_per_1000_py"))

if(subpopulations_present=="Yes"){
  write.csv(tab17, paste0(preg_dir, subpopulations_names[s], "/", subpopulations_names[s], "_pregnancy_rates_y.csv"), row.names = F)
} else {
  write.csv(tab17, paste0(preg_dir, "pregnancy_rates_y.csv"), row.names = F)
}

for (i in 1:length(tab17_files)){
  file.remove(paste0(preg_tmp, tab17_files[[i]]))
}
rm(tab17_files)


tab17[, no_women:= as.character(no_women)][as.numeric(no_women) > 0 & as.numeric(no_women) < 5, no_women := "<5"]
tab17[, person_years:= as.character(person_years)][as.numeric(person_years) > 0 & as.numeric(person_years) < 5, person_years := "<5"]
tab17[, women_per_1000_py:= as.character(women_per_1000_py)][no_women=="<5" | person_years=="<5", women_per_1000_py := "N/A"]

if(subpopulations_present=="Yes"){
  write.csv(tab17, paste0(preg_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_pregnancy_rates_y_masked.csv"), row.names = F)
} else {
  write.csv(tab17, paste0(preg_dir, "Masked/", "pregnancy_rates_y_masked.csv"), row.names = F)
}

rm(tab17)
##################################

#############################################
#tab18
############################################
tab18_files<-list.files(preg_tmp, "tab18")
tab18<-lapply(paste0(preg_tmp, tab18_files), readRDS)
tab18<-do.call(rbind,tab18)
setcolorder(tab18,c("stage_of_pregnancy","year","age_band", "no_women","person_years","women_per_1000_py"))

if(subpopulations_present=="Yes"){
  write.csv(tab18, paste0(preg_dir, subpopulations_names[s], "/", subpopulations_names[s], "_pregnancy_rates_y_age.csv"), row.names = F)
} else {
  write.csv(tab18, paste0(preg_dir, "pregnancy_rates_y_age.csv"), row.names = F)
}

for (i in 1:length(tab18_files)){
  file.remove(paste0(preg_tmp, tab18_files[[i]]))
}
rm(tab18_files)


tab18[, no_women:= as.character(no_women)][as.numeric(no_women) > 0 & as.numeric(no_women) < 5, no_women := "<5"]
tab18[, person_years:= as.character(person_years)][as.numeric(person_years) > 0 & as.numeric(person_years) < 5, person_years := "<5"]
tab18[, women_per_1000_py:= as.character(women_per_1000_py)][no_women=="<5" | person_years=="<5", women_per_1000_py := "N/A"]

if(subpopulations_present=="Yes"){
  write.csv(tab18, paste0(preg_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_pregnancy_rates_y_age_masked.csv"), row.names = F)
} else {
  write.csv(tab18, paste0(preg_dir, "Masked/", "pregnancy_rates_y_age_masked.csv"), row.names = F)
}

rm(tab18)
##################################

##################################
#tab19
###################################
tab19_files<-list.files(preg_tmp, "tab19")
tab19<-lapply(paste0(preg_tmp, tab19_files), readRDS)
tab19<-do.call(rbind,tab19)
setnames(tab19,"condition","stage_of_pregnancy")

if(subpopulations_present=="Yes"){
  write.csv(tab19, paste0(preg_dir, subpopulations_names[s], "/", subpopulations_names[s], "_pregnancy_follow_up.csv"), row.names = F)
} else {
  write.csv(tab19, paste0(preg_dir, "pregnancy_follow_up.csv"), row.names = F)
}

for (i in 1:length(tab19_files)){
  file.remove(paste0(preg_tmp, tab19_files[[i]]))
}
rm(tab19_files)


tab19[, no_women_365:= as.character(no_women_365)][as.numeric(no_women_365) > 0 & as.numeric(no_women_365) < 5, no_women_365 := "<5"]
tab19[, no_women:= as.character(no_women)][as.numeric(no_women) > 0 & as.numeric(no_women) < 5, no_women := "<5"]
tab19[, median_follow_up:= as.character(median_follow_up)][as.numeric(median_follow_up) > 0 & as.numeric(median_follow_up) < 5, median_follow_up := "<5"]
tab19[, percentage_women_365_fup:= as.character(percentage_women_365_fup)][no_women_365=="<5" | median_follow_up=="<5", percentage_women_365_fup := "N/A"]

if(subpopulations_present=="Yes"){
  write.csv(tab19, paste0(preg_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_pregnancy_follow_up_masked.csv"), row.names = F)
} else {
  write.csv(tab19, paste0(preg_dir, "Masked/", "pregnancy_follow_up_masked.csv"), row.names = F)
}

rm(tab19)
###############################

#####################################################
#graph1
#####################################################
graph1_files<-list.files(preg_tmp,"graph1")
graph1<-lapply(paste0(preg_tmp, graph1_files), readRDS)
graph1<-do.call(rbind,graph1)
rm(graph1_files)

if (subpopulations_present=="Yes"){
  write.csv(graph1, paste0(preg_dir,subpopulations_names[s], "/", subpopulations_names[s],"_","pregnancy_graph_1.csv"))
} else {
  write.csv(graph1, paste0(preg_dir,"pregnancy_graph_1.csv"))
}

#apply masking
graph1[, code_1_per_year:= as.character(code_1_per_year)][as.numeric(code_1_per_year) > 0 & as.numeric(code_1_per_year) < 5, code_1_per_year := "<5"]
graph1[, codes_2_per_year:= as.character(codes_2_per_year)][as.numeric(codes_2_per_year) > 0 & as.numeric(codes_2_per_year) < 5, codes_2_per_year := "<5"]
graph1[, codes_3_per_year:= as.character(codes_3_per_year)][as.numeric(codes_3_per_year) > 0 & as.numeric(codes_3_per_year) < 5, codes_3_per_year := "<5"]
graph1[, codes_min_4_per_year:= as.character(codes_min_4_per_year)][as.numeric(codes_min_4_per_year) > 0 & as.numeric(codes_min_4_per_year) < 5, codes_min_4_per_year := "<5"]
graph1[, total_women:= as.character(total_women)][as.numeric(total_women) > 0 & as.numeric(total_women) < 5, total_women := "<5"]

if (subpopulations_present=="Yes"){
  write.csv(graph1, paste0(preg_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_graph_1_masked.csv"), row.names = F)
} else {
  write.csv(graph1, paste0(preg_dir,"Masked/", "graph_1_masked.csv"), row.names = F)
}
rm(graph1)

######################################
#graph2
#####################################
graph2_files<-list.files(preg_tmp,"graph2")
graph2<-lapply(paste0(preg_tmp, graph2_files), readRDS)
graph2<-do.call(rbind,graph2)
rm(graph2_files)

if (subpopulations_present=="Yes"){
  write.csv(graph2, paste0(preg_dir,subpopulations_names[s], "/", subpopulations_names[s],"_","pregnancy_graph_2.csv"))
} else {
  write.csv(graph2, paste0(preg_dir,"pregnancy_graph_2.csv"))
}

#apply masking
graph2[, code_1_per_year:= as.character(code_1_per_year)][as.numeric(code_1_per_year) > 0 & as.numeric(code_1_per_year) < 5, code_1_per_year := "<5"]
graph2[, codes_2_per_year:= as.character(codes_2_per_year)][as.numeric(codes_2_per_year) > 0 & as.numeric(codes_2_per_year) < 5, codes_2_per_year := "<5"]
graph2[, codes_3_per_year:= as.character(codes_3_per_year)][as.numeric(codes_3_per_year) > 0 & as.numeric(codes_3_per_year) < 5, codes_3_per_year := "<5"]
graph2[, codes_min_4_per_year:= as.character(codes_min_4_per_year)][as.numeric(codes_min_4_per_year) > 0 & as.numeric(codes_min_4_per_year) < 5, codes_min_4_per_year := "<5"]
graph2[, total_women:= as.character(total_women)][as.numeric(total_women) > 0 & as.numeric(total_women) < 5, total_women := "<5"]

if (subpopulations_present=="Yes"){
  write.csv(graph2, paste0(preg_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_graph_2_masked.csv"), row.names = F)
} else {
  write.csv(graph2, paste0(preg_dir,"Masked/", "graph_2_masked.csv"), row.names = F)
}
rm(graph2)

