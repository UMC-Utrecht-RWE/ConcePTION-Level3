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
  orig_no_rows_events<-list() #original number of records in the EVENTS table
  #######################
  #pers_stdpop_not_events
  events_study_pop<-list() #number of records for the study population, no selection criteria for time applied
  events_date_miss<-list() #number of record with missing event_date
  years_events<-list()
  events_not_vocabularies<-list() #number of records where event_vocabulary not of interest
  events_code_vocabulary_miss<-list() #number of records with both event code and event record vocabulary missing
  events_code_pres_voc_miss<-list() #number of records with missing vocabularies
  events_sex_not_specified<-list()
  ######################
  events_out_st_per<-list() #number of EVENTS records outside the observation period(check is done on individual level)
  events_study_pop_obsper<-list() #number of records in the study population inside study period
  ######################
  events_stdpop_no_meaning<-list() #number of records in the study population with no meaning
  events_excluded_meanings<-list() #number of recorded with excluded meanings
  meanings_events<-list() #all meanings present
  #############################################################################
  #Table 20: Missingness of event codes
  #############################################################################
  events_study_population<-list() #number of records in the study population
  events_study_population_meaning<-list() #number of records in the study population by meaning
  events_study_population_my<-list() #number of records in the study population by meaning and year
  empty_event_code.my<-list()#number of records with empty event code in the study population by meaning and year
  ##############################################################################
  male_population_events<-list() #save whether males are included
  female_population_events<-list() #save whether females are included
  ##############################
  events_study_population_meaning_f<-list() #number of records in females [12-55] years old by meaning
  events_study_population_f<-list() #number of records in females [12-55] years old
  ##############################
  females_childbearing_events<-list() #check if females of childbearing age are available
  ###############################
  w<-1
  for (y in 1:length(actual_tables$EVENTS)){
    #Load the table
    df<-fread(paste(path_dir, actual_tables$EVENTS[y], sep=""), stringsAsFactors = FALSE)
    df<-df[,c("person_id", "start_date_record", "event_code", "event_record_vocabulary", "meaning_of_event")]
    df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately
    setnames(df,"meaning_of_event","meaning")
    setnames(df,"start_date_record","event_date")
    setnames(df,"event_record_vocabulary","event_vocabulary")
    colnames_events<-names(df)
    std_names_events<-names(study_population)
    colnames_events<-colnames_events[!colnames_events %in% "person_id"]
    #get the total number of records(a)
    orig_no_rows_events[[w]]<-df[,.N]
    #get the total number of records with excluded meanings
    events_excluded_meanings[[w]]<-df[meaning %in% meanings_exclude_events,.N]
    #remove all records for which the meaning is in excluded meanings
    df<-df[meaning %!in% meanings_exclude_events]
    #merge with the study_population table(there is no missing data in this table)
    df[,person_id:=as.character(person_id)]
    study_population[,person_id:=as.character(person_id)]
    df<-df[study_population,on=.(person_id)]#left join, keeps all people in the study population even if they didn't have an event
    df<-df[,age_start_follow_up:=as.numeric(age_start_follow_up)]
    pers_stdpop_not_events<-df[rowSums(is.na(df[,..colnames_events]))==length(colnames_events), ..std_names_events] #subjects id present in the study population but that do not have an event
    pers_stdpop_not_events<-pers_stdpop_not_events[!duplicated(person_id)]
    df<-df[!rowSums(is.na(df[,..colnames_events]))==length(colnames_events)]
    if(pers_stdpop_not_events[,.N]>0){
      saveRDS(pers_stdpop_not_events, paste0(events_tmp, paste0("stdpop_not_events_", actual_tables$EVENTS[y], ".rds"))) #to give the results we will merge the files by keeping only person_id that are in all saved files
    }
    rm(pers_stdpop_not_events)
    events_study_pop[[w]]<-df[,.N] #number of records for the study population, no selection criteria for time applied
    #transform into date variables
    df[,event_date:=as.IDate(event_date,"%Y%m%d")] #transform to date variables
    #create year variable
    df[,year:=year(event_date)]
    #number of records with both event_date missing
    events_date_miss[[w]]<-df[is.na(year),.N]
    #remove records with both dates missing
    df<-df[!is.na(year)]
    #identify persons that have an event before start_of_follow_up
    df[,date_dif:=start_follow_up-event_date][,filter:=fifelse(date_dif<=365 & date_dif>=1,1,0)]
    #get person_id
    persons_event_prior<-unique(na.omit(df[filter==1,person_id]))
    df[,date_dif:=NULL][,filter:=NULL]
    #remove records that are outside the obs_period for all subjects
    events_out_st_per[[w]]<-df[event_date<start_follow_up | event_date>end_follow_up,.N] #number of records outside study population
    df[(event_date<start_follow_up | event_date>end_follow_up), obs_out:=1]
    df<-df[is.na(obs_out)] #remove records outside study period
    df[,obs_out:=NULL] 
    events_study_pop_obsper[[w]]<-df[,.N] #number of records after removing records outside study period
    events_stdpop_no_meaning[[w]]<-df[is.na(meaning),.N] #number of records with empty meaning
    df<-df[!is.na(meaning)] #remove records with empty meaning
    events_code_vocabulary_miss[[w]]<-df[is.na(event_code) & is.na(event_vocabulary),.N]#numbe rof records with both event code and vocabulary missing
    df<-df[!is.na(event_code) | !is.na(event_vocabulary)]# remove records with both event code and event record vocabulary missing
    events_code_pres_voc_miss[[w]]<-df[!is.na(event_code) & is.na(event_vocabulary),.N] #number of records where event code present but vocabulary missing
    df<-df[!is.na(event_vocabulary)] #remove empty vocabularies
    events_not_vocabularies[[w]]<-df[event_vocabulary %!in% vocabularies_list,.N] #number of records where vocabularies doesn't match the codelist
    df<-df[is.na(event_vocabulary) | event_vocabulary %in% vocabularies_list] #remove records where vocabularies are not of interest
    events_sex_not_specified[[w]]<-df[sex_at_instance_creation =="U" | sex_at_instance_creation == "O",.N] #number of records with unspecified sex
    df<-df[sex_at_instance_creation == "M" | sex_at_instance_creation == "F"]#remove unspecified sex
    df[person_id %in% persons_event_prior, event_prior:=1]
    df[person_id %!in% persons_event_prior, event_prior:=0]
    #########
      meanings_events[[w]]<-unique(na.omit(df[, meaning])) #will be used for description
      years_events[[w]]<-unique(na.omit(df[, year])) #will be used for description
      male_population_events[[w]]<-ifelse(df[sex_at_instance_creation=="M",.N]>0,1,0)
      female_population_events[[w]]<-ifelse(df[sex_at_instance_creation=="F",.N]>0,1,0)
      females_childbearing_events[[w]]<-ifelse(df[sex_at_instance_creation=="F" & age_start_follow_up>=12 & age_start_follow_up<=55,.N]>0,1,0)
      ############################
      #Table 20
      ###########################
      events_study_population[[w]]<-df[,.N] #number of records in the study population
      events_study_population_meaning[[w]]<-df[,.N, by="meaning"] #number of records in the study population by meaning
      events_study_population_my[[w]]<-df[,.N, by=.(meaning,year)] #number of records in the study population by meaning and year
      empty_event_code.my[[w]]<-df[is.na(event_code), .N, by=.(meaning,year)] #number of records with missing event code when date disp/presc is present
      ##################################################################
      #match codes based on coding system and code: algorithm start with
      #################################################################
     if(df[,.N]>0){
       years_study_events<-df[!duplicated(year), year]#years present in this table
      
      if(sum(df[!duplicated(event_vocabulary), event_vocabulary] %in% c("ICD10", "ICD10CM","ICD9","ICD9CM","ICPC"))>0){
        for (i in 1:length(conditions_start)){
          for(j in 1:length(conditions_start[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(conditions_start[[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_start[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(conditions_start[[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_start[[i]])[j],filter:=1]
              }
              z<-z+1
              if(z>length(conditions_start[[i]][[j]])){
                break
              }
            }
            if("filter" %!in% names(df)){df[,filter:=0]}
            m<-1
            repeat{
              if(df[filter==1 & year==years_study_events[m],.N]>0){
                saveRDS(data.table(df[filter==1 & year==years_study_events[m]], condition=names(conditions_start[i])), paste0(events_tmp,years_study_events[m],"_", names(conditions_start[i]), "_",actual_tables$EVENTS[y], "_start.rds"))
              }
              m<-m+1
              if(m >length(years_study_events)){
                break
              }
            }
            df[,filter:=NULL]
          }
        }
      }
      #output to g_intermediate/tmp/EVENTS datasets splitted by condition, year, type of codes(start with:ICD10,ICD10CM,ICPC,ICD9,ICD9CM)
      
      if(sum(df[!duplicated(event_vocabulary), event_vocabulary] %in% c("RCD2"))>0){
        for (i in 1:length(conditions_read)){
          for(j in 1:length(conditions_read[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(conditions_read[[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_read[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(conditions_read[[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_read[[i]])[j],filter:=1]
              }
              z<-z+1
              if(z>length(conditions_read[[i]][[j]])){
                break
              }
            }
            if("filter" %!in% names(df)){df[,filter:=0]}
            m<-1
            repeat{
              if(df[filter==1 & year==years_study_events[m],.N]>0){
                saveRDS(data.table(df[filter==1 & year==years_study_events[m]], condition=names(conditions_read[i])), paste0(events_tmp,years_study_events[m],"_", names(conditions_read[i]), "_",actual_tables$EVENTS[y], "_RCD.rds"))
              }
              m<-m+1
              if(m >length(years_study_events)){
                break
              }
            }
            df[,filter:=NULL]
          }
        }
      }
      #output to g_intermediate/tmp/EVENTS datasets splitted by condition, year, type of codes(start with:Read codes)
      
      if(sum(df[!duplicated(event_vocabulary), event_vocabulary] %in% c("SNOMEDCT_US"))>0){
        for (i in 1:length(conditions_snomed)){
          for(j in 1:length(conditions_snomed[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(conditions_snomed[[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_snomed[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(conditions_snomed[[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_snomed[[i]])[j],filter:=1]
              }
              z<-z+1
              if(z>length(conditions_snomed[[i]][[j]])){
                break
              }
            }
            if("filter" %!in% names(df)){df[,filter:=0]}
            m<-1
            repeat{
              if(df[filter==1 & year==years_study_events[m],.N]>0){
                saveRDS(data.table(df[filter==1 & year==years_study_events[m]], condition=names(conditions_snomed[i])), paste0(events_tmp,years_study_events[m],"_", names(conditions_snomed[i]), "_",actual_tables$EVENTS[y], "_SNOMED.rds"))
              }
              m<-m+1
              if(m >length(years_study_events)){
                break
              }
            }
            df[,filter:=NULL]
          }
        }
      }
      #output to g_intermediate/tmp/EVENTS datasets splitted by condition, year, type of codes(exact match: SNOMED)
}
    w<-w+1
    rm(df)
  }
  
  #number of subjects in the study population that have not had an event
  stdpop_not_events_files<-list.files(events_tmp, pattern = "stdpop_not_events")
  if (length(stdpop_not_events_files)>0){
    stdpop_not_events<-readRDS(paste0(events_tmp, stdpop_not_events_files[1]))
    i<-2
    while(i <= length(stdpop_not_events_files)){
      a<-readRDS(paste0(events_tmp, stdpop_not_events_files[i]))
      stdpop_not_events<-rbind(stdpop_not_events, a)
      stdpop_not_events<-stdpop_not_events[duplicated(person_id)]
      i<-i+1
      rm(a)
    }
    stdpop_not_events<-stdpop_not_events[,.N]
    
    
    for(i in 1:length(stdpop_not_events_files)){
      unlink(paste0(events_tmp,stdpop_not_events_files[i]))
    }
    rm(stdpop_not_events_files)
  } else {stdpop_not_events<-0}
  

  #################################################################################################
  #Flowchart
  ################################################################################################
  print("Creating flowchart.")
  print("Get number of records in the original table.")
  #original number of records in the EVENTS table(flowchart 1)
  orig_no_rows_events<-do.call(rbind,orig_no_rows_events)
  orig_no_rows_events<-sum(orig_no_rows_events)
  #number of records with excluded meanings(flowchart 2)
  print("Get number of records with excluded meanings.")
  events_excluded_meanings<-do.call(rbind, events_excluded_meanings)
  events_excluded_meanings<-sum(events_excluded_meanings)
  #number of records for the study population, no selection criteria for time applied (flowchart 3)
  print("Get number of records for the study population (no time criteria applied).")
  events_study_pop<-do.call(rbind,events_study_pop)
  events_study_pop<-sum(events_study_pop)
  #Number of records with date record missing(flowchart 4)
  print("Get number of records with date record missing.")
  events_date_miss<-do.call(rbind,events_date_miss)
  events_date_miss<-sum(events_date_miss)
  #number of medicines records outside the observation period(check is done on individual level) (flowchart 5)
  print("Get number of records outside observation period.")
  events_out_st_per<-do.call(rbind,events_out_st_per) 
  events_out_st_per<-sum(events_out_st_per)
  #number of records in the study population with event_date inside study period (flowchart 6)
  print("Get number of records for the study population(time criteria applied).")
  events_study_pop_obsper<-do.call(rbind,events_study_pop_obsper) 
  events_study_pop_obsper<-sum(events_study_pop_obsper)
  #number of records in the study population with no meaning (flowchart 7)
  print("Get number of records with no meaning.")
  events_stdpop_no_meaning<-do.call(rbind,events_stdpop_no_meaning) 
  events_stdpop_no_meaning<-sum(events_stdpop_no_meaning) 
  #Number of records with both code and vocabulary variables missing
  print("Get number of records with both code and vocabulary variables missing")
  events_code_vocabulary_miss<-do.call(rbind,events_code_vocabulary_miss)
  events_code_vocabulary_miss<-sum(events_code_vocabulary_miss)
  #Number of records with empty vocabulary when code is present
  print("Get number of records with empty vocabulary when code is present")
  events_code_pres_voc_miss<-do.call(rbind,events_code_pres_voc_miss)
  events_code_pres_voc_miss<-sum(events_code_pres_voc_miss)
  #Number of records with vocabularies not present in the codelist
  print("Get number of records with vocabularies not present in the codelist")
  events_not_vocabularies<-do.call(rbind,events_not_vocabularies)
  events_not_vocabularies<-sum(events_not_vocabularies)
  #number of records with unspecified sex
  print("Get number of records with unspecified sex.")
  events_sex_not_specified<-do.call(rbind,events_sex_not_specified)
  events_sex_not_specified<-sum(events_sex_not_specified)
  #number of records in the study population
  print("Get number of records for study population.")
  events_study_population<-do.call(rbind,events_study_population) 
  events_study_population<-sum(events_study_population) 
  
  flowchart_events<-data.table(INDICATOR=c("Number of records in the original table", 
                                           "Number of subjects in the original study population table",
                                           "Exclude:Number of records with excluded meanings",
                                           "Number of records for the study_population(no time criteria)",
                                           "Exclude: Number of records with date record missing",
                                           "Exclude: Number of records with date record outside study period",
                                           "Number of records for the study_population(time criteria applied)",
                                           "Exclude:Number of records with empty meaning",
                                           "Exclude: Number of records with both code and vocabulary variables missing",
                                           "Exclude: Number of records with empty vocabulary when code is present",
                                           "Exclude: Number of records with vocabularies not present in the codelist",
                                           "Exclude: Number of records with unknown or other sex",
                                           "Number of records for study_population"), 
                               EVENTS=c(orig_no_rows_events,
                                        nr_std,
                                        events_excluded_meanings,
                                        events_study_pop,
                                        events_date_miss,
                                        events_out_st_per,
                                        events_study_pop_obsper,
                                        events_stdpop_no_meaning,
                                        events_code_vocabulary_miss,
                                        events_code_pres_voc_miss,
                                        events_not_vocabularies,
                                        events_sex_not_specified,
                                        events_study_population))
  
  rm(orig_no_rows_events,events_excluded_meanings,events_study_pop,events_date_miss,events_out_st_per,
     events_study_pop_obsper,events_stdpop_no_meaning,events_code_vocabulary_miss,events_code_pres_voc_miss,
     events_not_vocabularies,events_sex_not_specified,events_study_population)
  
  ##################################################################################################
  #Description
  ##################################################################################################
  print("Creating description of study population.")
  #meanings
  print("Get list of meanings.")
  meanings_events<-Filter(length,meanings_events)
  meanings_events<-suppressWarnings(do.call(rbind,meanings_events))
  meanings_events<-unique(c(meanings_events))
  meanings_events_des<-paste(meanings_events, collapse = ", ")
  #study years
  years_events<-Filter(length,years_events)
  years_events<-suppressWarnings(do.call(rbind, years_events))
  years_events<-unique(c(years_events))
  years_events_des<-paste(sort(years_events), collapse=", ")
  #
  male_population_events<-do.call(rbind, male_population_events)
  male_population_events<-sum(male_population_events)
  female_population_events<-do.call(rbind, female_population_events)
  female_population_events<-sum(female_population_events)
  if(male_population_events>0 & female_population_events>0){sex_included_events<-c("Males, Females")}
  if(male_population_events==0 & female_population_events>0){sex_included_events<-c("Females")}
  if(male_population_events>0 & female_population_events==0){sex_included_events<-c("Males")}
  if(male_population_events==0 & female_population_events==0){sex_included_events<-c("None")}
  females_childbearing_events<-do.call(rbind,females_childbearing_events)
  females_childbearing_events<-sum(females_childbearing_events)
  females_childbearing_events<-ifelse(females_childbearing_events>0,"Yes","No")
  
  print("Create description.")
  description_events<-data.table(INDICATOR=c("Data access provider(data source name)",
                                             "List of meanings present",
                                             "Years included in the study period",
                                             "Sex included in the study population",
                                             "Number of subjects in the study population without a recorded diagnosis",
                                             "Presence of females of child-bearing age 12-55 years old (based on age at start follow up)"), 
                                 EVENTS=c(paste0(data_access_provider_name,"(",data_source_name,")"),
                                          meanings_events_des,
                                          years_events_des,
                                          sex_included_events,
                                          stdpop_not_events,
                                          females_childbearing_events))
  rm(meanings_events_des,years_events_des,sex_included_events,stdpop_not_events)
  
  ##############################################################################
  #tab20
  ##############################################################################
  events_study_population_my<-do.call(rbind,events_study_population_my)
  setnames(events_study_population_my,"N","no_records")
  if(events_study_population_my[,.N]>0){
  events_study_population_my<-events_study_population_my[,lapply(.SD,sum),by=c("meaning", "year"), .SDcols="no_records"]
  }
  empty_event_code.my<-do.call(rbind,empty_event_code.my)
  setnames(empty_event_code.my,"N", "no_empty_code")
  if(empty_event_code.my[,.N]>0){
  empty_event_code.my<-empty_event_code.my[,lapply(.SD,sum),by=c("meaning", "year"), .SDcols="no_empty_code"]
  }
  if(empty_event_code.my[,.N]==0){
    tab20_events<-data.table(events_study_population_my, no_empty_code=0)
  } else {
    events_study_population_my[,meaning:=as.character(meaning)][,year:=as.character(year)]
    empty_event_code.my[,meaning:=as.character(meaning)][,year:=as.character(year)]
    tab20_events<-merge(events_study_population_my,empty_event_code.my, by=c("meaning","year"), all=T)
    tab20_events[is.na(no_empty_code),no_empty_code:=0]
  }
  if(tab20_events[is.na(meaning),.N]==1 & tab20_events[,.N]==1){tab20_events<-NULL}
  
  rm(empty_event_code.my)
  
  ######################
  #Combine populations
  #####################
  conditions_codelist<-names(conditions)
  
  ############################################################
  #Combine dataset by year and condition
  conditions_files<-c(list.files(events_tmp, "\\_start.rds$"),list.files(events_tmp, "\\_RCD.rds$"),list.files(events_tmp, "\\_SNOMED.rds$"))
  if (length(conditions_files)>0){
    #create combination year_condition from years(years present in the study) and all names of conditions in the codelist
    filter_var<-as.data.table(expand.grid(years_events,conditions_codelist))
    names(filter_var)<-c("year","diagnosis")
    filter_var[, comb:= paste0(year, "_", diagnosis)]
    filter_var<-filter_var[!duplicated(comb)]
    #Create list by conditions and years
    files<-vector(mode="list", length=filter_var[,.N])
    names(files)<-filter_var[["comb"]]
    for (i in 1:length(files)){
      files[[i]]<-conditions_files[grepl(names(files)[i], conditions_files)]
    }
    files<-Filter(length,files) #all files are separated based on year and diagnosis
    
    ############################################################################
    #Load each list element by combining all files inside one element
    #perform the necessary counts
    #export the data in populations named by events_year_condition_sex_population where sex==female
    #remove all files from tmp
    ############################################################################
    for (i in 1:length(files)){
      combined_diagnosis_events<-lapply(paste0(events_tmp,files[[i]]), readRDS)
      combined_diagnosis_events<-do.call(rbind,combined_diagnosis_events)
      combined_diagnosis_events[,code_nodot:=gsub("\\.","",event_code)]
      combined_diagnosis_events[event_vocabulary!="SNOMEDCT_US",truncated_code:=substr(code_nodot,1,4)]
      combined_diagnosis_events[event_vocabulary=="SNOMEDCT_US",truncated_code:=event_code]
      
      if (subpopulations_present=="Yes"){
        if(combined_diagnosis_events[,.N]>0){
          saveRDS(combined_diagnosis_events, paste0(diag_pop,subpopulations_names[s], "/", names(files)[i],"_events_diagnoses.rds"))
        }
      } else {
        if(combined_diagnosis_events[,.N]>0){
          saveRDS(combined_diagnosis_events, paste0(diag_pop,names(files)[i],"_events_diagnoses.rds"))
        }
      }
    }
    rm(files)
    
    for(i in 1:length(conditions_files)){
      unlink(paste0(events_tmp,conditions_files[i]))
    }
  }
  
  
} else {
  flowchart_events<-data.table(indicator=c("Number of records in the original table", 
                                           "Number of subjects in the original study population table",
                                           "Exclude:Number of records with excluded meanings",
                                           "Number of records for the study_population(no time criteria)",
                                           "Exclude: Number of records with date record missing",
                                           "Exclude: Number of records with date record outside study period",
                                           "Number of records for the study_population(time criteria applied)",
                                           "Exclude:Number of records with empty meaning",
                                           "Exclude: Number of records with both code and vocabulary variables missing",
                                           "Exclude: Number of records with empty vocabulary when code is present",
                                           "Exclude: Number of records with vocabularies not present in the codelist",
                                           "Exclude: Number of records with unknown or other sex",
                                           "Number of records for study_population"), 
                               EVENTS="N/A")
  
  description_events<-data.table(INDICATOR=c("Data access provider(data source name)",
                                             "List of meanings present",
                                             "Years included in the study period",
                                             "Sex included in the study population",
                                             "Number of subjects in the study population without a recorded diagnosis",
                                             "Presence of females of child-bearing age 12-55 years old (based on age at start follow up)"), 
                                 EVENTS="N/A")
  tab20_events<-NULL
}

############################################
#MEDICAL_OBSERVATIONS
###########################################
if(length(actual_tables$MEDICAL_OBSERVATIONS)>0){
  ###################################################
  #List for saving info
  ###################################################
  print("Creating lists to save the information.")
  orig_no_rows_mo<-list() #original number of records in the mo table
  #######################
  #pers_stdpop_not_mo
  mo_study_pop<-list() #number of records for the study population, no selection criteria for time applied
  mo_date_miss<-list() #number of record with missing event_date
  years_mo<-list()
  mo_not_vocabularies<-list() #number of records where  c not of interest
  mo_code_vocabulary_miss<-list() #number of records with both mo code and mo record vocabulary missing
  mo_code_pres_voc_miss<-list() #numb
  mo_sex_not_specified<-list()
  ######################
  mo_out_st_per<-list() #number of mo records outside the observation period(check is done on individual level)
  mo_study_pop_obsper<-list() #number of records in the study population inside study period
  ######################
  mo_stdpop_no_meaning<-list() #number of records in the study population with no meaning
  mo_excluded_meanings<-list() #number of recorded with excluded meanings
  meanings_mo<-list() #all meanings present
  #############################################################################
  #Table 20: Missingness of event codes
  #############################################################################
  mo_study_population<-list() #number of records in the study population
  mo_study_population_meaning<-list() #number of records in the study population by meaning
  mo_study_population_my<-list() #number of records in the study population by meaning and year
  empty_mo_code.my<-list()#number of records with empty event code in the study population by meaning and year
  ##############################################################################
  male_population_mo<-list() #save whether males are included
  female_population_mo<-list() #save whether females are included
  ##############################
  mo_study_population_meaning_f<-list() #number of records in females [12-55] years old by meaning
  mo_study_population_f<-list() #number of records in females [12-55] years old
  ##############################
  females_childbearing_mo<-list() #check if females of childbearing age are available
  ###############################
  w<-1
  for (y in 1:length(actual_tables$MEDICAL_OBSERVATIONS)){
    #Load the table
    df<-fread(paste(path_dir, actual_tables$MEDICAL_OBSERVATIONS[y], sep=""), stringsAsFactors = FALSE)
    df<-df[,c("person_id", "mo_date", "mo_code", "mo_record_vocabulary", "mo_meaning")]
    df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately
    setnames(df,"mo_meaning","meaning")
    setnames(df,"mo_date","event_date")
    setnames(df,"mo_code","event_code")
    setnames(df,"mo_record_vocabulary","event_vocabulary")
    colnames_mo<-names(df)
    std_names_mo<-names(study_population)
    colnames_mo<-colnames_mo[!colnames_mo %in% "person_id"]
    #get the total number of records(a)
    orig_no_rows_mo[[w]]<-df[,.N]
    #get the total number of records with excluded meanings
    mo_excluded_meanings[[w]]<-df[meaning %in% meanings_exclude_mo,.N]
    #remove all records for which the meaning is in excluded meanings
    df<-df[meaning %!in% meanings_exclude_mo]
    #merge with the study_population table(there is no missing data in this table)
    df[,person_id:=as.character(person_id)]
    study_population[,person_id:=as.character(person_id)]
    df<-df[study_population,on=.(person_id)]#left join, keeps all people in the study population even if they didn't have an event
    df<-df[,age_start_follow_up:=as.numeric(age_start_follow_up)]
    pers_stdpop_not_mo<-df[rowSums(is.na(df[,..colnames_mo]))==length(colnames_mo), ..std_names_mo] #subjects id present in the study population but that do not have an event
    pers_stdpop_not_mo<-pers_stdpop_not_mo[!duplicated(person_id)]
    df<-df[!rowSums(is.na(df[,..colnames_mo]))==length(colnames_mo)]
    if(pers_stdpop_not_mo[,.N]>0){
      saveRDS(pers_stdpop_not_mo, paste0(mo_tmp, paste0("stdpop_not_mo_", actual_tables$MEDICAL_OBSERVATIONS[y], ".rds"))) #to give the results we will merge the files by keeping only person_id that are in all saved files
    }
    rm(pers_stdpop_not_mo)
    mo_study_pop[[w]]<-df[,.N] #number of records for the study population, no selection criteria for time applied
    #transform into date variables
    df[,event_date:=as.IDate(event_date,"%Y%m%d")] #transform to date variables
    #create year variable
    df[,year:=year(event_date)]
    #number of records with both event_date missing
    mo_date_miss[[w]]<-df[is.na(year),.N]
    #remove records with both dates missing
    df<-df[!is.na(year)]
    #identify persons that have an event before start_of_follow_up
    df[,date_dif:=start_follow_up-event_date][,filter:=fifelse(date_dif<=365 & date_dif>=1,1,0)]
    #get person_id
    persons_event_prior<-unique(na.omit(df[filter==1,person_id]))
    df[,date_dif:=NULL][,filter:=NULL]
    #remove records that are outside the obs_period for all subjects
    mo_out_st_per[[w]]<-df[event_date<start_follow_up | event_date>end_follow_up,.N] #number of records outside study population
    df[(event_date<start_follow_up | event_date>end_follow_up), obs_out:=1]
    df<-df[is.na(obs_out)] #remove records outside study period
    df[,obs_out:=NULL] 
    mo_study_pop_obsper[[w]]<-df[,.N] #number of records after removing records outside study period
    mo_stdpop_no_meaning[[w]]<-df[is.na(meaning),.N] #number of records with empty meaning
    df<-df[!is.na(meaning)] #remove records with empty meaning
    mo_code_vocabulary_miss[[w]]<-df[is.na(event_code) & is.na( event_vocabulary),.N]#numbe rof records with both event code and vocabulary missing
    df<-df[!is.na(event_code) | !is.na( event_vocabulary)]# remove records with both event code and event record vocabulary missing
    mo_code_pres_voc_miss[[w]]<-df[!is.na(event_code) & is.na( event_vocabulary),.N] #number of records where event code present but vocabulary missing
    df<-df[!is.na( event_vocabulary)] #remove empty vocabularies
    mo_not_vocabularies[[w]]<-df[ event_vocabulary %!in% vocabularies_list,.N] #number of records where vocabularies doesn't match the codelist
    df<-df[is.na( event_vocabulary) |  event_vocabulary %in% vocabularies_list] #remove records where vocabularies are not of interest
    mo_sex_not_specified[[w]]<-df[sex_at_instance_creation =="U" | sex_at_instance_creation == "O",.N] #number of records with unspecified sex
    df<-df[sex_at_instance_creation == "M" | sex_at_instance_creation == "F"]#remove unspecified sex
    df[person_id %in% persons_event_prior, event_prior:=1]
    df[person_id %!in% persons_event_prior, event_prior:=0]
    #########
      meanings_mo[[w]]<-df[!duplicated(meaning)][["meaning"]] #will be used for description
      years_mo[[w]]<-df[!duplicated(year)][["year"]] #will be used for description
      male_population_mo[[w]]<-ifelse(df[sex_at_instance_creation=="M",.N]>0,1,0)
      female_population_mo[[w]]<-ifelse(df[sex_at_instance_creation=="F",.N]>0,1,0)
      females_childbearing_mo[[w]]<-ifelse(df[sex_at_instance_creation=="F" & age_start_follow_up>=12 & age_start_follow_up<=55,.N]>0,1,0)
      ############################
      #Table 20
      ###########################
      mo_study_population[[w]]<-df[,.N] #number of records in the study population
      mo_study_population_meaning[[w]]<-df[,.N, by="meaning"] #number of records in the study population by meaning
      mo_study_population_my[[w]]<-df[,.N, by=.(meaning,year)] #number of records in the study population by meaning and year
      empty_mo_code.my[[w]]<-df[is.na(event_code), .N, by=.(meaning,year)] #number of records with missing event code when date disp/presc is present
      ##################################################################
      #match codes based on coding system and code: algorithm start with
      #################################################################
      if(df[,.N]>0){
      years_study_mo<-df[!duplicated(year), year]#years present in this table
      
      if(sum(df[!duplicated( event_vocabulary),  event_vocabulary] %in% c("ICD10", "ICD10CM","ICD9","ICD9CM","ICPC"))>0){
        for (i in 1:length(conditions_start)){
          for(j in 1:length(conditions_start[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(conditions_start[[i]][[j]][z])), df[["event_code"]]) &  event_vocabulary==names(conditions_start[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(conditions_start[[i]][[j]][z])), df[["event_code"]]) &  event_vocabulary==names(conditions_start[[i]])[j],filter:=1]
              }
              z<-z+1
              if(z>length(conditions_start[[i]][[j]])){
                break
              }
            }
            if("filter" %!in% names(df)){df[,filter:=0]}
            m<-1
            repeat{
              if(df[filter==1 & year==years_study_mo[m],.N]>0){
                saveRDS(data.table(df[filter==1 & year==years_study_mo[m]], condition=names(conditions_start[i])), paste0(mo_tmp,years_study_mo[m],"_", names(conditions_start[i]), "_",actual_tables$MEDICAL_OBSERVATIONS[y], "_start.rds"))
              }
              m<-m+1
              if(m >length(years_study_mo)){
                break
              }
            }
            df[,filter:=NULL]
          }
        }
      }
      #output to g_intermediate/tmp/mo datasets splitted by condition, year, type of codes(start with:ICD10,ICD10CM,ICPC,ICD9,ICD9CM)
      
      if(sum(df[!duplicated( event_vocabulary),  event_vocabulary] %in% c("RCD2"))>0){
        for (i in 1:length(conditions_read)){
          for(j in 1:length(conditions_read[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(conditions_read[[i]][[j]][z])), df[["event_code"]]) &  event_vocabulary==names(conditions_read[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(conditions_read[[i]][[j]][z])), df[["event_code"]]) &  event_vocabulary==names(conditions_read[[i]])[j],filter:=1]
              }
              z<-z+1
              if(z>length(conditions_read[[i]][[j]])){
                break
              }
            }
            if("filter" %!in% names(df)){df[,filter:=0]}
            m<-1
            repeat{
              if(df[filter==1 & year==years_study_mo[m],.N]>0){
                saveRDS(data.table(df[filter==1 & year==years_study_mo[m]], condition=names(conditions_read[i])), paste0(mo_tmp,years_study_mo[m],"_", names(conditions_read[i]), "_",actual_tables$MEDICAL_OBSERVATIONS[y], "_RCD.rds"))
              }
              m<-m+1
              if(m >length(years_study_mo)){
                break
              }
            }
            df[,filter:=NULL]
          }
        }
      }
      #output to g_intermediate/tmp/mo datasets splitted by condition, year, type of codes(start with:Read codes)
      
      if(sum(df[!duplicated( event_vocabulary),  event_vocabulary] %in% c("SNOMEDCT_US"))>0){
        for (i in 1:length(conditions_snomed)){
          for(j in 1:length(conditions_snomed[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(conditions_snomed[[i]][[j]][z])), df[["event_code"]]) &  event_vocabulary==names(conditions_snomed[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(conditions_snomed[[i]][[j]][z])), df[["event_code"]]) &  event_vocabulary==names(conditions_snomed[[i]])[j],filter:=1]
              }
              z<-z+1
              if(z>length(conditions_snomed[[i]][[j]])){
                break
              }
            }
            if("filter" %!in% names(df)){df[,filter:=0]}
            m<-1
            repeat{
              if(df[filter==1 & year==years_study_mo[m],.N]>0){
                saveRDS(data.table(df[filter==1 & year==years_study_mo[m]], condition=names(conditions_snomed[i])), paste0(mo_tmp,years_study_mo[m],"_", names(conditions_snomed[i]), "_",actual_tables$MEDICAL_OBSERVATIONS[y], "_SNOMED.rds"))
              }
              m<-m+1
              if(m >length(years_study_mo)){
                break
              }
            }
            df[,filter:=NULL]
          }
        }
      }
      #output to g_intermediate/tmp/mo datasets splitted by condition, year, type of codes(exact match: SNOMED)
      }
      w<-w+1
      rm(df)
      ########
    }

  
  #number of subjects in the study population that have not had an mo record
  stdpop_not_mo_files<-list.files(mo_tmp, pattern = "stdpop_not_mo")
  if (length(stdpop_not_mo_files)>0){
    stdpop_not_mo<-readRDS(paste0(mo_tmp, stdpop_not_mo_files[1]))
    i<-2
    while(i <= length(stdpop_not_mo_files)){
      a<-readRDS(paste0(mo_tmp, stdpop_not_mo_files[i]))
      stdpop_not_mo<-rbind(stdpop_not_mo, a)
      stdpop_not_mo<-stdpop_not_mo[duplicated(person_id)]
      i<-i+1
      rm(a)
    }
    stdpop_not_mo<-stdpop_not_mo[,.N]
    
    
    for(i in 1:length(stdpop_not_mo_files)){
      unlink(paste0(mo_tmp,stdpop_not_mo_files[i]))
    }
    rm(stdpop_not_mo_files)
  } else {stdpop_not_mo<-0}
  
  #################################################################################################
  #Flowchart
  ################################################################################################
  print("Creating flowchart.")
  print("Get number of records in the original table.")
  #original number of records in the MEDICAL_OBSERVATIONS table(flowchart 1)
  orig_no_rows_mo<-do.call(rbind,orig_no_rows_mo)
  orig_no_rows_mo<-sum(orig_no_rows_mo)
  #number of records with excluded meanings(flowchart 2)
  print("Get number of records with excluded meanings.")
  mo_excluded_meanings<-do.call(rbind, mo_excluded_meanings)
  mo_excluded_meanings<-sum(mo_excluded_meanings)
  #number of records for the study population, no selection criteria for time applied (flowchart 3)
  print("Get number of records for the study population (no time criteria applied).")
  mo_study_pop<-do.call(rbind,mo_study_pop)
  mo_study_pop<-sum(mo_study_pop)
  #Number of records with date record missing(flowchart 4)
  print("Get number of records with date record missing.")
  mo_date_miss<-do.call(rbind,mo_date_miss)
  mo_date_miss<-sum(mo_date_miss)
  #number of medicines records outside the observation period(check is done on individual level) (flowchart 5)
  print("Get number of records outside observation period.")
  mo_out_st_per<-do.call(rbind,mo_out_st_per) 
  mo_out_st_per<-sum(mo_out_st_per)
  #number of records in the study population with date dispensing/prescription inside study period (flowchart 6)
  print("Get number of records for the study population(time criteria applied).")
  mo_study_pop_obsper<-do.call(rbind,mo_study_pop_obsper) 
  mo_study_pop_obsper<-sum(mo_study_pop_obsper)
  #number of records in the study population with no meaning (flowchart 7)
  print("Get number of records with no meaning.")
  mo_stdpop_no_meaning<-do.call(rbind,mo_stdpop_no_meaning) 
  mo_stdpop_no_meaning<-sum(mo_stdpop_no_meaning) 
  #Number of records with both code and vocabulary variables missing
  print("Get number of records with both code and vocabulary variables missing")
  mo_code_vocabulary_miss<-do.call(rbind,mo_code_vocabulary_miss)
  mo_code_vocabulary_miss<-sum(mo_code_vocabulary_miss)
  #Number of records with empty vocabulary when code is present
  print("Get number of records with empty vocabulary when code is present")
  mo_code_pres_voc_miss<-do.call(rbind,mo_code_pres_voc_miss)
  mo_code_pres_voc_miss<-sum(mo_code_pres_voc_miss)
  #Number of records with vocabularies not present in the codelist
  print("Get number of records with vocabularies not present in the codelist")
  mo_not_vocabularies<-do.call(rbind,mo_not_vocabularies)
  mo_not_vocabularies<-sum(mo_not_vocabularies)
  #number of records with unspecified sex
  print("Get number of records with unspecified sex.")
  mo_sex_not_specified<-do.call(rbind,mo_sex_not_specified)
  mo_sex_not_specified<-sum(mo_sex_not_specified)
  #number of records in the study population
  print("Get number of records for study population.")
  mo_study_population<-do.call(rbind,mo_study_population) 
  mo_study_population<-sum(mo_study_population) 
  
  flowchart_mo<-data.table(INDICATOR=c("Number of records in the original table",
                                       "Number of subjects in the original study population table",
                                           "Exclude:Number of records with excluded meanings",
                                           "Number of records for the study_population(no time criteria)",
                                           "Exclude: Number of records with date record missing",
                                           "Exclude: Number of records with date record outside study period",
                                           "Number of records for the study_population(time criteria applied)",
                                           "Exclude:Number of records with empty meaning",
                                           "Exclude: Number of records with both code and vocabulary variables missing",
                                           "Exclude: Number of records with empty vocabulary when code is present",
                                           "Exclude: Number of records with vocabularies not present in the codelist",
                                           "Exclude: Number of records with unknown or other sex",
                                           "Number of records for study_population"), 
                               MEDICAL_OBSERVATIONS=c(orig_no_rows_mo,
                                                      nr_std,
                                        mo_excluded_meanings,
                                        mo_study_pop,
                                        mo_date_miss,
                                        mo_out_st_per,
                                        mo_study_pop_obsper,
                                        mo_stdpop_no_meaning,
                                        mo_code_vocabulary_miss,
                                        mo_code_pres_voc_miss,
                                        mo_not_vocabularies,
                                        mo_sex_not_specified,
                                        mo_study_population))
  
  rm(orig_no_rows_mo,mo_excluded_meanings,mo_study_pop,mo_date_miss,mo_out_st_per,
     mo_study_pop_obsper,mo_stdpop_no_meaning,mo_code_vocabulary_miss,mo_code_pres_voc_miss,
     mo_not_vocabularies,mo_sex_not_specified,mo_study_population)
  
  
  ##################################################################################################
  #Description
  ##################################################################################################
  print("Creating description of study population.")
  #meanings
  print("Get list of meanings.")
  meanings_mo<-Filter(length,meanings_mo)
  meanings_mo<-suppressWarnings(do.call(rbind,meanings_mo))
  meanings_mo<-unique(c(meanings_mo))
  meanings_mo_des<-paste(meanings_mo, collapse = ", ")
  #study years
  years_mo<-Filter(length,years_mo)
  years_mo<-suppressWarnings(do.call(rbind,years_mo))
  years_mo<-unique(c(years_mo))
  years_mo_des<-paste(sort(years_mo), collapse=", ")
  #
  male_population_mo<-do.call(rbind, male_population_mo)
  male_population_mo<-sum(male_population_mo)
  female_population_mo<-do.call(rbind, female_population_mo)
  female_population_mo<-sum(female_population_mo)
  if(male_population_mo>0 & female_population_mo>0){sex_included_mo<-c("Males, Females")}
  if(male_population_mo==0 & female_population_mo>0){sex_included_mo<-c("Females")}
  if(male_population_mo>0 & female_population_mo==0){sex_included_mo<-c("Males")}
  if(male_population_mo==0 & female_population_mo==0){sex_included_mo<-c("None")}
  females_childbearing_mo<-do.call(rbind,females_childbearing_mo)
  females_childbearing_mo<-sum(females_childbearing_mo)
  females_childbearing_mo<-ifelse(females_childbearing_mo>0,"Yes","No")
  
  print("Create description.")
  description_mo<-data.table(INDICATOR=c("Data access provider(data source name)",
                                             "List of meanings present",
                                             "Years included in the study period",
                                             "Sex included in the study population",
                                             "Number of subjects in the study population without a recorded diagnosis",
                                             "Presence of females of child-bearing age 12-55 years old (based on age at start follow up)"), 
                                 MEDICAL_OBSERVATIONS=c(paste0(data_access_provider_name,"(",data_source_name,")"),
                                          meanings_mo_des,
                                          years_mo_des,
                                          sex_included_mo,
                                          stdpop_not_mo,
                                          females_childbearing_mo))
  rm(meanings_mo_des,years_mo_des,sex_included_mo,stdpop_not_mo)
  
  ####################################################################################################
  #tab20
  #################################################################################################
  mo_study_population_my<-do.call(rbind,mo_study_population_my)
  setnames(mo_study_population_my,"N","no_records")
  if(mo_study_population_my[,.N]>0){
    mo_study_population_my<-mo_study_population_my[,lapply(.SD,sum),by=c("meaning", "year"), .SDcols="no_records"]
  }
  empty_mo_code.my<-do.call(rbind,empty_mo_code.my)
  setnames(empty_mo_code.my,"N", "no_empty_code")
  if(empty_mo_code.my[,.N]>0){
    empty_mo_code.my<-empty_mo_code.my[,lapply(.SD,sum),by=c("meaning", "year"), .SDcols="no_records"]
  }
  if(empty_mo_code.my[,.N]==0){
    tab20_mo<-data.table(mo_study_population_my, no_empty_code=0)
  } else {
    mo_study_population_my[,meaning:=as.character(meaning)][,year:=as.character(year)]
    empty_mo_code.my[,meaning:=as.character(meaning)][,year:=as.character(year)]
    tab20_mo<-merge(mo_study_population_my,empty_mo_code.my, by=c("meaning","year"),all=T)
    tab20_mo[is.na(no_empty_code),no_empty_code:=0]
  }
  if(tab20_mo[is.na(meaning),.N]==1 & tab20_mo[,.N]==1){tab20_mo<-NULL}
  
  ####################
  #Combine populations
  ####################
  conditions_codelist<-names(conditions)
  
  ############################################################
  #Combine dataset by year and condition
  conditions_files<-c(list.files(mo_tmp, "\\_start.rds$"),list.files(mo_tmp, "\\_RCD.rds$"),list.files(mo_tmp, "\\_SNOMED.rds$"))
  if (length(conditions_files)>0){
    #create combination year_condition from years(years present in the study) and all names of conditions in the codelist
    filter_var<-as.data.table(expand.grid(years_mo,conditions_codelist))
    names(filter_var)<-c("year","diagnosis")
    filter_var[, comb:= paste0(year, "_", diagnosis)]
    filter_var<-filter_var[!duplicated(comb)]
    #Create list by conditions and years
    files<-vector(mode="list", length=filter_var[,.N])
    names(files)<-filter_var[["comb"]]
    for (i in 1:length(files)){
      files[[i]]<-conditions_files[grepl(names(files)[i], conditions_files)]
    }
    files<-Filter(length,files) #all files are separated based on year and diagnosis
    
    ############################################################################
    #Load each list element by combining all files inside one element
    #perform the necessary counts
    #export the data in populations named by mo_year_condition_sex_population where sex==female
    #remove all files from tmp
    ############################################################################
    for (i in 1:length(files)){
      combined_diagnosis_mo<-lapply(paste0(mo_tmp,files[[i]]), readRDS)
      combined_diagnosis_mo<-do.call(rbind,combined_diagnosis_mo)
      combined_diagnosis_mo[,code_nodot:=gsub("\\.","",event_code)]
      combined_diagnosis_mo[event_vocabulary!="SNOMEDCT_US",truncated_code:=substr(code_nodot,1,4)]
      combined_diagnosis_mo[event_vocabulary=="SNOMEDCT_US",truncated_code:=event_code]
      if (subpopulations_present=="Yes"){
        if(combined_diagnosis_mo[,.N]>0){
          saveRDS(combined_diagnosis_mo, paste0(diag_pop,subpopulations_names[s], "/", names(files)[i],"_mo_diagnoses.rds"))
        } 
      } else {
        if(combined_diagnosis_mo[,.N]>0){
          saveRDS(combined_diagnosis_mo, paste0(diag_pop, names(files)[i],"_mo_diagnoses.rds"))
        }
      }
    }
    
    rm(files)
    for(i in 1:length(conditions_files)){
      unlink(paste0(mo_tmp,conditions_files[i]))
    }
  }
  
   } else {
  flowchart_mo<-data.table(indicator=c("Number of records in the original table", 
                                       "Number of subjects in the original study population table",
                                       "Exclude:Number of records with excluded meanings",
                                       "Number of records for the study_population(no time criteria)",
                                       "Exclude: Number of records with date record missing",
                                       "Exclude: Number of records with date record outside study period",
                                       "Number of records for the study_population(time criteria applied)",
                                       "Exclude:Number of records with empty meaning",
                                       "Exclude: Number of records with both code and vocabulary variables missing",
                                       "Exclude: Number of records with empty vocabulary when code is present",
                                       "Exclude: Number of records with vocabularies not present in the codelist",
                                       "Exclude: Number of records with unknown or other sex",
                                       "Number of records for study_population"), 
                               MEDICAL_OBSERVATIONS="N/A")
  
  description_mo<-data.table(INDICATOR=c("Data access provider(data source name)",
                                         "List of meanings present",
                                         "Years included in the study period",
                                         "Sex included in the study population",
                                         "Number of subjects in the study population without a recorded diagnosis",
                                         "Presence of females of child-bearing age 12-55 years old (based on age at start follow up)"), 
                             MEDICAL_OBSERVATIONS="N/A")
  tab20_mo<-NULL
  }


############################################
#SURVEY_OBSERVATIONS
###########################################
if(length(actual_tables$SURVEY_OBSERVATIONS)>0){
  ###################################################
  #List for saving info
  ###################################################
  print("Creating lists to save the information.")
  orig_no_rows_so<-list() #original number of records in the mo table
  #######################
  #pers_stdpop_not_so
  so_study_pop<-list() #number of records for the study population, no selection criteria for time applied
  so_date_miss<-list() #number of record with missing event_date
  years_so<-list()
  so_not_vocabularies<-list() #number of records where event_vocabulary not of interest
  so_event_vocabulary_miss<-list() #number of records with both event_code and event_vocabulary missing
  so_event_pres_voc_miss<-list() #numb
  so_sex_not_specified<-list()
  ######################
  so_out_st_per<-list() #number of mo records outside the observation period(check is done on individual level)
  so_study_pop_obsper<-list() #number of records in the study population inside study period
  ######################
  so_stdpop_no_meaning<-list() #number of records in the study population with no meaning
  so_excluded_meanings<-list() #number of recorded with excluded meanings
  meanings_so<-list() #all meanings present
  #############################################################################
  #Table 20: Missingness of event codes
  #############################################################################
  so_study_population<-list() #number of records in the study population
  so_study_population_meaning<-list() #number of records in the study population by meaning
  so_study_population_my<-list() #number of records in the study population by meaning and year
  empty_so_code.my<-list()#number of records with empty so source value in the study population by meaning and year
  ##############################################################################
  male_population_so<-list() #save whether males are included
  female_population_so<-list() #save whether females are included
  ##############################
  so_study_population_meaning_f<-list() #number of records in females [12-55] years old by meaning
  so_study_population_f<-list() #number of records in females [12-55] years old
  ##############################
  females_childbearing_so<-list() #check if females of childbearing age are available
  ###############################
  w<-1
  for (y in 1:length(actual_tables$SURVEY_OBSERVATIONS)){
    #Load the table
    df<-fread(paste(path_dir, actual_tables$SURVEY_OBSERVATIONS[y], sep=""), stringsAsFactors = FALSE)
    df<-df[,c("person_id", "so_date", "so_source_value", "so_unit", "so_meaning")]
    df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately
    setnames(df,"so_meaning","meaning")
    setnames(df,"so_date","event_date")
    setnames(df,"so_source_value","event_code")
    setnames(df,"so_unit","event_vocabulary")
    colnames_so<-names(df)
    std_names_so<-names(study_population)
    colnames_so<-colnames_so[!colnames_so %in% "person_id"]
    #get the total number of records(a)
    orig_no_rows_so[[w]]<-df[,.N]
    #get the total number of records with excluded meanings
    so_excluded_meanings[[w]]<-df[meaning %in% meanings_exclude_so,.N]
    #remove all records for which the meaning is in excluded meanings
    df<-df[meaning %!in% meanings_exclude_so]
    #merge with the study_population table(there is no missing data in this table)
    df[,person_id:=as.character(person_id)]
    study_population[,person_id:=as.character(person_id)]
    df<-df[study_population,on=.(person_id)]#left join, keeps all people in the study population even if they didn't have an event
    df<-df[,age_start_follow_up:=as.numeric(age_start_follow_up)]
    pers_stdpop_not_so<-df[rowSums(is.na(df[,..colnames_so]))==length(colnames_so), ..std_names_so] #subjects id present in the study population but that do not have an event
    pers_stdpop_not_so<-pers_stdpop_not_so[!duplicated(person_id)]
    df<-df[!rowSums(is.na(df[,..colnames_so]))==length(colnames_so)]
    if(pers_stdpop_not_so[,.N]>0){
      saveRDS(pers_stdpop_not_so, paste0(so_tmp, paste0("stdpop_not_so_", actual_tables$SURVEY_OBSERVATIONS[y], ".rds"))) #to give the results we will merge the files by keeping only person_id that are in all saved files
    }
    rm(pers_stdpop_not_so)
    so_study_pop[[w]]<-df[,.N] #number of records for the study population, no selection criteria for time applied
    #transform into date variables
    df[,event_date:=as.IDate(event_date,"%Y%m%d")] #transform to date variables
    #create year variable
    df[,year:=year(event_date)]
    #number of records with both event_date missing
    so_date_miss[[w]]<-df[is.na(year),.N]
    #remove records with both dates missing
    df<-df[!is.na(year)]
    #identify persons that have an event before start_of_follow_up
    df[,date_dif:=start_follow_up-event_date][,filter:=fifelse(date_dif<=365 & date_dif>=1,1,0)]
    #get person_id
    persons_event_prior<-unique(na.omit(df[filter==1,person_id]))
    df[,date_dif:=NULL][,filter:=NULL]
    #remove records that are outside the obs_period for all subjects
    so_out_st_per[[w]]<-df[event_date<start_follow_up | event_date>end_follow_up,.N] #number of records outside study population
    df[(event_date<start_follow_up | event_date>end_follow_up), obs_out:=1]
    df<-df[is.na(obs_out)] #remove records outside study period
    df[,obs_out:=NULL] 
    so_study_pop_obsper[[w]]<-df[,.N] #number of records after removing records outside study period
    so_stdpop_no_meaning[[w]]<-df[is.na(meaning),.N] #number of records with empty meaning
    df<-df[!is.na(meaning)] #remove records with empty meaning
    so_event_vocabulary_miss[[w]]<-df[is.na(event_code) & is.na(event_vocabulary),.N]#numbe rof records with both event code and vocabulary missing
    df<-df[!is.na(event_code) | !is.na(event_vocabulary)]# remove records with both event code and event record vocabulary missing
    so_event_pres_voc_miss[[w]]<-df[!is.na(event_code) & is.na(event_vocabulary),.N] #number of records where event code present but vocabulary missing
    df<-df[!is.na(event_vocabulary)] #remove empty vocabularies
    so_not_vocabularies[[w]]<-df[event_vocabulary %!in% vocabularies_list,.N] #number of records where vocabularies doesn't match the codelist
    df<-df[is.na(event_vocabulary) | event_vocabulary %in% vocabularies_list] #remove records where vocabularies are not of interest
    so_sex_not_specified[[w]]<-df[sex_at_instance_creation =="U" | sex_at_instance_creation == "O",.N] #number of records with unspecified sex
    df<-df[sex_at_instance_creation == "M" | sex_at_instance_creation == "F"]#remove unspecified sex
    df[person_id %in% persons_event_prior, event_prior:=1]
    df[person_id %!in% persons_event_prior, event_prior:=0]
    #########
      meanings_so[[w]]<-df[!duplicated(meaning)][["meaning"]] #will be used for description
      years_so[[w]]<-df[!duplicated(year)][["year"]] #will be used for description
      male_population_so[[w]]<-ifelse(df[sex_at_instance_creation=="M",.N]>0,1,0)
      female_population_so[[w]]<-ifelse(df[sex_at_instance_creation=="F",.N]>0,1,0)
      females_childbearing_so[[w]]<-ifelse(df[sex_at_instance_creation=="F" & age_start_follow_up>=12 & age_start_follow_up<=55,.N]>0,1,0)
      ############################
      #Table 20
      ###########################
      so_study_population[[w]]<-df[,.N] #number of records in the study population
      so_study_population_meaning[[w]]<-df[,.N, by="meaning"] #number of records in the study population by meaning
      so_study_population_my[[w]]<-df[,.N, by=.(meaning,year)] #number of records in the study population by meaning and year
      empty_so_code.my[[w]]<-df[is.na(event_code), .N, by=.(meaning,year)] #number of records with missing event code when date disp/presc is present
      ##################################################################
      #match codes based on coding system and code: algorithm start with
      #################################################################
      if(df[,.N]>0){
      years_study_so<-df[!duplicated(year), year]#years present in this table
      
      if(sum(df[!duplicated(event_vocabulary), event_vocabulary] %in% c("ICD10", "ICD10CM","ICD9","ICD9CM","ICPC"))>0){
        for (i in 1:length(conditions_start)){
          for(j in 1:length(conditions_start[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(conditions_start[[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_start[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(conditions_start[[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_start[[i]])[j],filter:=1]
              }
              z<-z+1
              if(z>length(conditions_start[[i]][[j]])){
                break
              }
            }
            if("filter" %!in% names(df)){df[,filter:=0]}
            m<-1
            repeat{
              if(df[filter==1 & year==years_study_so[m],.N]>0){
                saveRDS(data.table(df[filter==1 & year==years_study_so[m]], condition=names(conditions_start[i])), paste0(so_tmp,years_study_so[m],"_", names(conditions_start[i]), "_",actual_tables$SURVEY_OBSERVATIONS[y], "_start.rds"))
              }
              m<-m+1
              if(m >length(years_study_so)){
                break
              }
            }
            df[,filter:=NULL]
          }
        }
      }
      #output to g_intermediate/tmp/mo datasets splitted by condition, year, type of codes(start with:ICD10,ICD10CM,ICPC,ICD9,ICD9CM)
      
      if(sum(df[!duplicated(event_vocabulary), event_vocabulary] %in% c("RCD2"))>0){
        for (i in 1:length(conditions_read)){
          for(j in 1:length(conditions_read[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(conditions_read[[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_read[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(conditions_read[[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_read[[i]])[j],filter:=1]
              }
              z<-z+1
              if(z>length(conditions_read[[i]][[j]])){
                break
              }
            }
            if("filter" %!in% names(df)){df[,filter:=0]}
            m<-1
            repeat{
              if(df[filter==1 & year==years_study_so[m],.N]>0){
                saveRDS(data.table(df[filter==1 & year==years_study_so[m]], condition=names(conditions_read[i])), paste0(so_tmp,years_study_so[m],"_", names(conditions_read[i]), "_",actual_tables$SURVEY_OBSERVATIONS[y], "_RCD.rds"))
              }
              m<-m+1
              if(m >length(years_study_so)){
                break
              }
            }
            df[,filter:=NULL]
          }
        }
      }
      #output to g_intermediate/tmp/mo datasets splitted by condition, year, type of codes(start with:Read codes)
      
      if(sum(df[!duplicated(event_vocabulary), event_vocabulary] %in% c("SNOMEDCT_US"))>0){
        for (i in 1:length(conditions_snomed)){
          for(j in 1:length(conditions_snomed[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(conditions_snomed[[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_snomed[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(conditions_snomed[[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_snomed[[i]])[j],filter:=1]
              }
              z<-z+1
              if(z>length(conditions_snomed[[i]][[j]])){
                break
              }
            }
            if("filter" %!in% names(df)){df[,filter:=0]}
            m<-1
            repeat{
              if(df[filter==1 & year==years_study_so[m],.N]>0){
                saveRDS(data.table(df[filter==1 & year==years_study_so[m]], condition=names(conditions_snomed[i])), paste0(so_tmp,years_study_so[m],"_", names(conditions_snomed[i]), "_",actual_tables$SURVEY_OBSERVATIONS[y], "_SNOMED.rds"))
              }
              m<-m+1
              if(m >length(years_study_so)){
                break
              }
            }
            df[,filter:=NULL]
          }
        }
      }
      #output to g_intermediate/tmp/mo datasets splitted by condition, year, type of codes(exact match: SNOMED)
      }
      w<-w+1
      rm(df)
      ########
  }
  
  #number of subjects in the study population that have not had an so record
  stdpop_not_so_files<-list.files(so_tmp, pattern = "stdpop_not_so")
  if (length(stdpop_not_so_files)>0){
    stdpop_not_so<-readRDS(paste0(so_tmp, stdpop_not_so_files[1]))
    i<-2
    while(i <= length(stdpop_not_so_files)){
      a<-readRDS(paste0(so_tmp, stdpop_not_so_files[i]))
      stdpop_not_so<-rbind(stdpop_not_so, a)
      stdpop_not_so<-stdpop_not_so[duplicated(person_id)]
      i<-i+1
      rm(a)
    }
    stdpop_not_so<-stdpop_not_so[,.N]
    
    
    for(i in 1:length(stdpop_not_so_files)){
      unlink(paste0(so_tmp,stdpop_not_so_files[i]))
    }
    rm(stdpop_not_so_files)
  } else {stdpop_not_so<-0}
  
  #################################################################################################
  #Flowchart
  ################################################################################################
  print("Creating flowchart.")
  print("Get number of records in the original table.")
  #original number of records in the SURVEY_OBSERVATIONS table(flowchart 1)
  orig_no_rows_so<-do.call(rbind,orig_no_rows_so)
  orig_no_rows_so<-sum(orig_no_rows_so)
  #number of records with excluded meanings(flowchart 2)
  print("Get number of records with excluded meanings.")
  so_excluded_meanings<-do.call(rbind, so_excluded_meanings)
  so_excluded_meanings<-sum(so_excluded_meanings)
  #number of records for the study population, no selection criteria for time applied (flowchart 3)
  print("Get number of records for the study population (no time criteria applied).")
  so_study_pop<-do.call(rbind,so_study_pop)
  so_study_pop<-sum(so_study_pop)
  #Number of records with date record missing(flowchart 4)
  print("Get number of records with date record missing.")
  so_date_miss<-do.call(rbind,so_date_miss)
  so_date_miss<-sum(so_date_miss)
  #number of medicines records outside the observation period(check is done on individual level) (flowchart 5)
  print("Get number of records outside observation period.")
  so_out_st_per<-do.call(rbind,so_out_st_per) 
  so_out_st_per<-sum(so_out_st_per)
  #number of records in the study population with date dispensing/prescription inside study period (flowchart 6)
  print("Get number of records for the study population(time criteria applied).")
  so_study_pop_obsper<-do.call(rbind,so_study_pop_obsper) 
  so_study_pop_obsper<-sum(so_study_pop_obsper)
  #number of records in the study population with no meaning (flowchart 7)
  print("Get number of records with no meaning.")
  so_stdpop_no_meaning<-do.call(rbind,so_stdpop_no_meaning) 
  so_stdpop_no_meaning<-sum(so_stdpop_no_meaning) 
  #Number of records with both code and vocabulary variables missing
  print("Get number of records with both code and vocabulary variables missing")
  so_event_vocabulary_miss<-do.call(rbind,so_event_vocabulary_miss)
  so_event_vocabulary_miss<-sum(so_event_vocabulary_miss)
  #Number of records with empty vocabulary when code is present
  print("Get number of records with empty vocabulary when code is present")
  so_event_pres_voc_miss<-do.call(rbind,so_event_pres_voc_miss)
  so_event_pres_voc_miss<-sum(so_event_pres_voc_miss)
  #Number of records with vocabularies not present in the codelist
  print("Get number of records with vocabularies not present in the codelist")
  so_not_vocabularies<-do.call(rbind,so_not_vocabularies)
  so_not_vocabularies<-sum(so_not_vocabularies)
  #number of records with unspecified sex
  print("Get number of records with unspecified sex.")
  so_sex_not_specified<-do.call(rbind,so_sex_not_specified)
  so_sex_not_specified<-sum(so_sex_not_specified)
  #number of records in the study population
  print("Get number of records for study population.")
  so_study_population<-do.call(rbind,so_study_population) 
  so_study_population<-sum(so_study_population) 
  
  flowchart_so<-data.table(INDICATOR=c("Number of records in the original table",
                                       "Number of subjects in the original study population table",
                                       "Exclude:Number of records with excluded meanings",
                                       "Number of records for the study_population(no time criteria)",
                                       "Exclude: Number of records with date record missing",
                                       "Exclude: Number of records with date record outside study period",
                                       "Number of records for the study_population(time criteria applied)",
                                       "Exclude:Number of records with empty meaning",
                                       "Exclude: Number of records with both code and vocabulary variables missing",
                                       "Exclude: Number of records with empty vocabulary when code is present",
                                       "Exclude: Number of records with vocabularies not present in the codelist",
                                       "Exclude: Number of records with unknown or other sex",
                                       "Number of records for study_population"), 
                           SURVEY_OBSERVATIONS=c(orig_no_rows_so,
                                                 nr_std,
                                                  so_excluded_meanings,
                                                  so_study_pop,
                                                  so_date_miss,
                                                  so_out_st_per,
                                                  so_study_pop_obsper,
                                                  so_stdpop_no_meaning,
                                                  so_event_vocabulary_miss,
                                                  so_event_pres_voc_miss,
                                                  so_not_vocabularies,
                                                  so_sex_not_specified,
                                                  so_study_population))
  
  rm(orig_no_rows_so,so_excluded_meanings,so_study_pop,so_date_miss,so_out_st_per,
     so_study_pop_obsper,so_stdpop_no_meaning,so_event_vocabulary_miss,so_event_pres_voc_miss,
     so_not_vocabularies,so_sex_not_specified,so_study_population)
  
  ##################################################################################################
  #Description
  ##################################################################################################
  print("Creating description of study population.")
  #meanings
  print("Get list of meanings.")
  meanings_so<-Filter(length,meanings_so)
  meanings_so<-suppressWarnings(do.call(rbind,meanings_so))
  meanings_so<-unique(c(meanings_so))
  meanings_so_des<-paste(meanings_so, collapse = ", ")
  #study years
  years_so<-Filter(length,years_so)
  years_so<-suppressWarnings(do.call(rbind,years_so))
  years_so<-unique(c(years_so))
  years_so_des<-paste(sort(years_so), collapse=", ")
  #
  male_population_so<-do.call(rbind, male_population_so)
  male_population_so<-sum(male_population_so)
  female_population_so<-do.call(rbind, female_population_so)
  female_population_so<-sum(female_population_so)
  if(male_population_so>0 & female_population_so>0){sex_included_so<-c("Males, Females")}
  if(male_population_so==0 & female_population_so>0){sex_included_so<-c("Females")}
  if(male_population_so>0 & female_population_so==0){sex_included_so<-c("Males")}
  if(male_population_so==0 & female_population_so==0){sex_included_so<-c("None")}
  females_childbearing_so<-do.call(rbind,females_childbearing_so)
  females_childbearing_so<-sum(females_childbearing_so)
  females_childbearing_so<-ifelse(females_childbearing_so>0,"Yes","No")
  
  print("Create description.")
  description_so<-data.table(INDICATOR=c("Data access provider(data source name)",
                                         "List of meanings present",
                                         "Years included in the study period",
                                         "Sex included in the study population",
                                         "Number of subjects in the study population without a recorded diagnosis",
                                         "Presence of females of child-bearing age 12-55 years old (based on age at start follow up)"), 
                             SURVEY_OBSERVATIONS=c(paste0(data_access_provider_name,"(",data_source_name,")"),
                                                    meanings_so_des,
                                                    years_so_des,
                                                    sex_included_so,
                                                    stdpop_not_so,
                                                    females_childbearing_so))
  rm(meanings_so_des,years_so_des,sex_included_so,stdpop_not_so)
  
  #########################################################################################
  #tab20
  ########################################################################################
  so_study_population_my<-do.call(rbind,so_study_population_my)
  setnames(so_study_population_my,"N","no_records")
  if(so_study_population_my[,.N]>0){
    so_study_population_my<-so_study_population_my[,lapply(.SD,sum),by=c("meaning", "year"), .SDcols="no_records"]
  }
  empty_so_code.my<-do.call(rbind,empty_so_code.my)
  setnames(empty_so_code.my,"N", "no_empty_code")
  if(empty_so_code.my[,.N]>0){
    empty_so_code.my<-empty_so_code.my[,lapply(.SD,sum),by=c("meaning", "year"), .SDcols="no_empty_code"]
  }
  if(empty_so_code.my[,.N]==0){
    tab20_so<-data.table(so_study_population_my, no_empty_code=0)
  } else {
    so_study_population_my[,meaning:=as.character(meaning)][,year:=as.character(year)]
    empty_so_code.my[,meaning:=as.character(meaning)][,year:=as.character(year)]
    tab20_so<-merge(so_study_population_my,empty_so_code.my, by=c("meaning","year"))
    tab20_so[is.na(no_empty_code),no_empty_code:=0]
  }
  if(tab20_so[is.na(meaning),.N]==1 & tab20_so[,.N]==1){tab20_so<-NULL}
  
  ##################
  #combine populations
  ##################
  conditions_codelist<-names(conditions)
  
  ############################################################
  #Combine dataset by year and condition
  conditions_files<-c(list.files(so_tmp, "\\_start.rds$"),list.files(so_tmp, "\\_RCD.rds$"),list.files(so_tmp, "\\_SNOMED.rds$"))
  if (length(conditions_files)>0){
    #create combination year_condition from years(years present in the study) and all names of conditions in the codelist
    filter_var<-as.data.table(expand.grid(years_so,conditions_codelist))
    names(filter_var)<-c("year","diagnosis")
    filter_var[, comb:= paste0(year, "_", diagnosis)]
    filter_var<-filter_var[!duplicated(comb)]
    #Create list by conditions and years
    files<-vector(mode="list", length=filter_var[,.N])
    names(files)<-filter_var[["comb"]]
    for (i in 1:length(files)){
      files[[i]]<-conditions_files[grepl(names(files)[i], conditions_files)]
    }
    files<-Filter(length,files) #all files are separated based on year and diagnosis
    
    ############################################################################
    #Load each list element by combining all files inside one element
    #perform the necessary counts
    #export the data in populations named by mo_year_condition_sex_population where sex==female
    #remove all files from tmp
    ############################################################################
    for (i in 1:length(files)){
      combined_diagnosis_so<-lapply(paste0(so_tmp,files[[i]]), readRDS)
      combined_diagnosis_so<-do.call(rbind,combined_diagnosis_so)
      combined_diagnosis_so[,code_nodot:=gsub("\\.","",event_code)]
      combined_diagnosis_so[event_vocabulary!="SNOMEDCT_US",truncated_code:=substr(code_nodot,1,4)]
      combined_diagnosis_so[event_vocabulary=="SNOMEDCT_US",truncated_code:=event_code]
      if (subpopulations_present=="Yes"){
        if(combined_diagnosis_so[,.N]>0){
          saveRDS(combined_diagnosis_so, paste0(diag_pop,subpopulations_names[s], "/", names(files)[i],"_so_diagnoses.rds"))
        } 
      } else {
        if(combined_diagnosis_so[,.N]>0){
          saveRDS(combined_diagnosis_so, paste0(diag_pop, names(files)[i],"_so_diagnoses.rds"))
        }
      }
    }
    rm(files)
    for(i in 1:length(conditions_files)){
      unlink(paste0(so_tmp,conditions_files[i]))
    }
   } 
  
  
} else {
  flowchart_so<-data.table(indicator=c("Number of records in the original table", 
                                       "Number of subjects in the original study population table",
                                       "Exclude:Number of records with excluded meanings",
                                       "Number of records for the study_population(no time criteria)",
                                       "Exclude: Number of records with date record missing",
                                       "Exclude: Number of records with date record outside study period",
                                       "Number of records for the study_population(time criteria applied)",
                                       "Exclude:Number of records with empty meaning",
                                       "Exclude: Number of records with both code and vocabulary variables missing",
                                       "Exclude: Number of records with empty vocabulary when code is present",
                                       "Exclude: Number of records with vocabularies not present in the codelist",
                                       "Exclude: Number of records with unknown or other sex",
                                       "Number of records for study_population"), 
                           SURVEY_OBSERVATIONS="N/A")
  
  description_so<-data.table(INDICATOR=c("Data access provider(data source name)",
                                         "List of meanings present",
                                         "Years included in the study period",
                                         "Sex included in the study population",
                                         "Number of subjects in the study population without a recorded diagnosis",
                                         "Presence of females of child-bearing age 12-55 years old (based on age at start follow up)"), 
                             SURVEY_OBSERVATIONS="N/A")
  
  tab20_so<-NULL
}

###########################################################################################
#Combine results 
##########################################################################################
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
#Rates of recurrent events
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
  diagnoses_list[[i]]<-diagnoses_files[str_detect(diagnoses_files,names(diagnoses_list)[i])]
}
rm(diagnoses_files)
diagnoses_files<-diagnoses_list
rm(diagnoses_list)

#Each file is separated by type of event and year
#count person time to be applied to each file
time_lag<-data.table(condition=c("Breast cancer", "Depression", "Elective abortion",
                                 "Gestational diabetes","Multiple gestation"," Preeclampsia", "Spontaneous abortion",
                                 "TOPFA"),
                     time_lag=c(5*365, 3*30, 8*7, 23*7, 23*7, 8*7, 8*7, 8*7))


#create a loop that woud run count person time for each diagnoses separately

for (condition_ind in 1:length(diagnoses_files)){
  diag_file<-lapply(paste0(populations_dir,"DIAGNOSES/", diagnoses_files[[condition_ind]]), readRDS)
  diag_file<-do.call(rbind,diag_file)
  diag_file[,comb:=paste(condition, truncated_code, sep=":")]
  diag_file[,comb:=paste(comb, event_vocabulary, sep=":")]
  
  if (names(diagnoses_files)[condition_ind] %in% time_lag[,condition]){
    outcomes_list<-unique(diag_file[,comb])
   #apply count person time
  output<-CountPersonTime2(Dataset_events = unique(diag_file[,.(person_id, comb, event_date)]),
                           Dataset = unique(diag_file[,.(person_id, birth_date, start_follow_up, end_follow_up, sex_at_instance_creation, meaning)]),
                           Person_id = "person_id",
                           Start_study_time =start_study_date2,
                           End_study_time =end_study_date2,
                           Start_date = "start_follow_up",
                           End_date = "end_follow_up",
                           Birth_date = "birth_date",
                           Increment = "year",
                           Unit_of_age = "year",
                           Strata = c("sex_at_instance_creation","meaning"),
                           include_remaning_ages = TRUE,
                           Aggregate = T,
                           Outcomes_rec = outcomes_list,
                           Name_event = "comb",
                           Date_event = "event_date",
                           Rec_period = rep(time_lag[condition==names(diagnoses_files)[[condition_ind]],time_lag], length(outcomes_list)),
                           Age_bands = c(0,11,55),
                           print = F, 
                           check_overlap = F)
  
  
  } else {
  outcomes_list<-unique(diag_file[,comb])
  #apply count person time
  output<-CountPersonTime2(Dataset_events = unique(diag_file[,.(person_id, comb, event_date)]),
                           Dataset = unique(diag_file[,.(person_id, birth_date, start_follow_up, end_follow_up, sex_at_instance_creation, meaning)]),
                           Person_id = "person_id",
                           Start_study_time =start_study_date2,
                           End_study_time =end_study_date2,
                           Start_date = "start_follow_up",
                           End_date = "end_follow_up",
                           Birth_date = "birth_date",
                           Increment = "year",
                           Unit_of_age = "year",
                           Strata = c("sex_at_instance_creation","meaning"),
                           include_remaning_ages = TRUE,
                           Aggregate = T,
                           Outcomes_nrec = outcomes_list,
                           Name_event = "comb",
                           Date_event = "event_date",
                           Rec_period = NULL,
                           Age_bands = c(0,11,55),
                           print = F, 
                           check_overlap = F)
}

  output[,Persontime:=NULL]
  #from wide to long(remove all person time)
  ps<-output[,colnames(output)[str_detect(colnames(output), "Persontime_")]]
  person_years_df<-output[,..ps]
  output[,colnames(output)[str_detect(colnames(output), "Persontime_")]]<-NULL
  ps<-unlist(lapply(ps, function(x) str_replace(x,"Persontime_","")))
  names(person_years_df)<-ps
  person_years_df<-data.table(person_years_df,output[,c("sex_at_instance_creation","meaning","year","Ageband")])
  
  person_years_df<-person_years_df[,lapply(.SD,function(x) as.numeric(x)), .SDcols=ps, by=c("sex_at_instance_creation","meaning","year","Ageband")]
  person_years_df<-melt(person_years_df, id.vars=c("sex_at_instance_creation","meaning", "Ageband","year"), measure.vars = colnames(person_years_df)[!colnames(person_years_df) %in% c("sex_at_instance_creation","meaning", "Ageband","year")], variable.name = "combined_diagnoses")        
  setnames(person_years_df,"value","person_years")
  output<-melt(output, id.vars=c("sex_at_instance_creation","meaning", "Ageband","year"), measure.vars = colnames(output)[!colnames(output) %in% c("sex_at_instance_creation","meaning", "Ageband","year")], variable.name = "combined_diagnoses")        
  output[,combined_diagnoses:= gsub('.{2}$','',combined_diagnoses)]
  setnames(output,"value","no_records")
  person_years_df[,sex_at_instance_creation:=as.character(sex_at_instance_creation)][,meaning:=as.character(meaning)][,Ageband:=as.character(Ageband)][,year:=as.character(year)][,combined_diagnoses:=as.character(combined_diagnoses)]
  output[,sex_at_instance_creation:=as.character(sex_at_instance_creation)][,meaning:=as.character(meaning)][,Ageband:=as.character(Ageband)][,year:=as.character(year)][,combined_diagnoses:=as.character(combined_diagnoses)]
  output<-merge(person_years_df,output,by=c("sex_at_instance_creation","meaning", "Ageband","year", "combined_diagnoses"), all=T)
  rm(person_years_df)
  setnames(output, "Ageband", "age_band")
  setnames(output, "sex_at_instance_creation", "sex")
  
  #will be used to count users
  saveRDS(output, paste0(diag_tmp, names(diagnoses_files)[condition_ind], "_rates_rec.rds"))
  rm(output)
}
rm(diagnoses_files)


############################
#tab21/tab22
############################
diagnoses_files<-list.files(diag_tmp, "rates_rec")
for (cond_ind in 1:length(diagnoses_files)){
  #load the file
  diag<-readRDS(paste0(diag_tmp, diagnoses_files[cond_ind]))
  #split the comb variable into event_definition, truncated_code and vocabulary
  
  diag[,c("event_definition","truncated_code","vocabulary") := tstrsplit(combined_diagnoses, ":")]
  diag[,combined_diagnoses:=NULL]
 
  ############################
  #tab21
  ############################
  tab21_counts<-diag[,lapply(.SD,sum), by=c("event_definition","meaning","vocabulary","truncated_code"), .SDcols="no_records"]
  tab21_total<-tab21_counts[,lapply(.SD,sum), by="event_definition", .SDcols="no_records"]
  setnames(tab21_total,"no_records","total_records")
  #counts by event_definition, meaning, vocabulary and truncate_code
  #total by event_definition
  tab21_counts[,event_definition:=as.character(event_definition)]
  tab21_total[,event_definition:=as.character(event_definition)]
  tab21_counts<-merge(tab21_counts, tab21_total, by="event_definition")
  rm(tab21_total)
  
  saveRDS(tab21_counts,paste0(diag_tmp, unlist(str_split(diagnoses_files[cond_ind],"_"))[1], "_tab21.rds"))
  rm(tab21_counts)
  
  ##############################
  #tab22
  #############################
  tab22_sex<-diag[,lapply(.SD,sum), by=c("event_definition","sex","year"), .SDcols=c("no_records","person_years")]
  tab22_sex<-data.table(tab22_sex, meaning="All",age_band="All")
  tab22_sex_age<-diag[,lapply(.SD,sum), by=c("event_definition","sex","year","age_band"), .SDcols=c("no_records","person_years")]
  tab22_sex_age<-data.table(tab22_sex_age, meaning="All")
  tab22_sex<-rbind(tab22_sex,tab22_sex_age)
  rm(tab22_sex_age)
  tab22_sex_m<-diag[,lapply(.SD,sum), by=c("event_definition","sex","year","meaning"), .SDcols=c("no_records","person_years")]
  tab22_sex_m<-data.table(tab22_sex_m, age_band="All")
  tab22_sex<-rbind(tab22_sex,tab22_sex_m)
  rm(tab22_sex_m)
  tab22_sex_age_m<-diag[,lapply(.SD,sum), by=c("event_definition","sex","year","meaning","age_band"), .SDcols=c("no_records","person_years")]
  tab22_sex<-rbind(tab22_sex,tab22_sex_age_m)
  rm(tab22_sex_age_m)
  tab22<-tab22_sex
  rm(tab22_sex)
  tab22[,rate_per_1000_py:=round((no_records/person_years)*1000,4)]
  
  saveRDS(tab22,paste0(diag_tmp, unlist(str_split(diagnoses_files[cond_ind],"_"))[1], "_tab22.rds"))
  rm(tab22)
  
  
}

for (i in 1:length(diagnoses_files)){
  file.remove(paste0(diag_tmp, diagnoses_files[[i]]))
}

############
#combine tab21
############
tab21_files<-list.files(diag_tmp, "tab21")
tab21<-lapply(paste0(diag_tmp, tab21_files), readRDS)
tab21<-do.call(rbind,tab21)
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
#################
#tab22
#################
tab22_files<-list.files(diag_tmp, "tab22")
tab22<-lapply(paste0(diag_tmp, tab22_files), readRDS)
tab22<-do.call(rbind,tab22)
tab22<-data.table(tab22, data_access_provider= data_access_provider_name, data_source=data_source_name)

if(subpopulations_present=="Yes"){
  write.csv(tab22, paste0(diag_dir, subpopulations_names[s], "/", subpopulations_names[s], "_diagnoses_rates_recurrent.csv"), row.names = F)
} else {
  write.csv(tab22, paste0(diag_dir, "diagnoses_rates_recurrent.csv"), row.names = F)
}

for (i in 1:length(tab22_files)){
  file.remove(paste0(diag_tmp, tab22_files[[i]]))
}


rm(tab22_files)

tab22[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
tab22[, person_years:= as.character(person_years)][as.numeric(person_years) > 0 & as.numeric(person_years) < 5, person_years := "<5"]
tab22[, rate_per_1000_py:= as.character(rate_per_1000_py)][no_records=="<5" | person_years=="<5", rate_per_1000_py := "N/A"]

if(subpopulations_present=="Yes"){
  write.csv(tab22, paste0(diag_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_diagnoses_rates_recurrent_masked.csv"), row.names = F)
} else {
  write.csv(tab22, paste0(diag_dir, "Masked/", "diagnoses_rates_recurrent_masked.csv"), row.names = F)
}

rm(tab22)
######################################################
#first event
######################################################
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
  diagnoses_list[[i]]<-diagnoses_files[str_detect(diagnoses_files,names(diagnoses_list)[i])]
}
rm(diagnoses_files)
diagnoses_files<-diagnoses_list
rm(diagnoses_list)

###################
#combine
###################
remove_subj<-list()
subj_ind<-1
for (condition_ind in 1:length(diagnoses_files)){
  diag_file<-lapply(paste0(populations_dir,"DIAGNOSES/", diagnoses_files[[condition_ind]]), readRDS)
  diag_file<-do.call(rbind,diag_file)
  
  #remove subject who had an event in the year prior to start of follow up
  remove_subj[[subj_ind]]<-diag_file[event_prior==1,lapply(.SD, function(x) length(unique(na.omit(x)))), .SDcols="person_id", by="condition"]
  outcomes_list<-unique(diag_file[,condition])
    #apply count person time
    output<-CountPersonTime2(Dataset_events = unique(diag_file[,.(person_id, condition, event_date)]),
                             Dataset = unique(diag_file[,.(person_id, birth_date, start_follow_up, end_follow_up, sex_at_instance_creation, meaning)]),
                             Person_id = "person_id",
                             Start_study_time =start_study_date2,
                             End_study_time =end_study_date2,
                             Start_date = "start_follow_up",
                             End_date = "end_follow_up",
                             Birth_date = "birth_date",
                             Increment = "year",
                             Unit_of_age = "year",
                             Strata = c("sex_at_instance_creation","meaning"),
                             include_remaning_ages = TRUE,
                             Aggregate = T,
                             Outcomes_nrec = outcomes_list,
                             Name_event = "condition",
                             Date_event = "event_date",
                             Rec_period = NULL,
                             Age_bands = c(0,11,55),
                             print = F, 
                             check_overlap = F)

  
  output[,Persontime:=NULL]
  output<-data.table(event_definition=outcomes_list,output)
  #from wide to long(remove all person time)
  names(output)<-c("event_definition", "sex","meaning","year","age_band","person_years","no_records")
 
  #will be used to count users
  saveRDS(output, paste0(diag_tmp, names(diagnoses_files)[condition_ind], "_rates_first.rds"))
  rm(output)
}
rm(diagnoses_files)

#####################
#table23
#####################
remove_subj<-do.call(rbind,remove_subj)

diagnoses_files<-list.files(diag_tmp, "rates_first")
for (cond_ind in 1:length(diagnoses_files)){
  #load the file
  diag<-readRDS(paste0(diag_tmp, diagnoses_files[cond_ind]))
  #split the comb variable into event_definition, truncated_code and vocabulary
  
  ##############################
  #tab23
  #############################
  tab23_sex<-diag[,lapply(.SD,sum), by=c("event_definition","sex","year"), .SDcols=c("no_records","person_years")]
  tab23_sex<-data.table(tab23_sex, meaning="All",age_band="All")
  tab23_sex_age<-diag[,lapply(.SD,sum), by=c("event_definition","sex","year","age_band"), .SDcols=c("no_records","person_years")]
  tab23_sex_age<-data.table(tab23_sex_age, meaning="All")
  tab23_sex<-rbind(tab23_sex,tab23_sex_age)
  rm(tab23_sex_age)
  tab23_sex_m<-diag[,lapply(.SD,sum), by=c("event_definition","sex","year","meaning"), .SDcols=c("no_records","person_years")]
  tab23_sex_m<-data.table(tab23_sex_m, age_band="All")
  tab23_sex<-rbind(tab23_sex,tab23_sex_m)
  rm(tab23_sex_m)
  tab23_sex_age_m<-diag[,lapply(.SD,sum), by=c("event_definition","sex","year","meaning","age_band"), .SDcols=c("no_records","person_years")]
  tab23_sex<-rbind(tab23_sex,tab23_sex_age_m)
  rm(tab23_sex_age_m)
  tab23<-tab23_sex
  rm(tab23_sex)
  tab23<-tab23[person_years!=0]
  tab23[,rate_per_1000_py:=round((no_records/person_years)*1000,4)]
  
  saveRDS(tab23,paste0(diag_tmp, unlist(str_split(diagnoses_files[cond_ind],"_"))[1], "_tab23.rds"))
  rm(tab23)
  
  
}

for (i in 1:length(diagnoses_files)){
  file.remove(paste0(diag_tmp, diagnoses_files[[i]]))
}

tab23_files<-list.files(diag_tmp, "tab23")
tab23<-lapply(paste0(diag_tmp, tab23_files), readRDS)
tab23<-do.call(rbind,tab23)
tab23<-data.table(tab23, data_access_provider= data_access_provider_name, data_source=data_source_name)

if(subpopulations_present=="Yes"){
  write.csv(tab23, paste0(diag_dir, subpopulations_names[s], "/", subpopulations_names[s], "_diagnoses_rates_first.csv"), row.names = F)
} else {
  write.csv(tab23, paste0(diag_dir, "diagnoses_rates_first.csv"), row.names = F)
}

for (i in 1:length(tab23_files)){
  file.remove(paste0(diag_tmp, tab23_files[[i]]))
}


rm(tab23_files)

tab23[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
tab23[, person_years:= as.character(person_years)][as.numeric(person_years) > 0 & as.numeric(person_years) < 5, person_years := "<5"]
tab23[, rate_per_1000_py:= as.character(rate_per_1000_py)][no_records=="<5" | person_years=="<5", rate_per_1000_py := "N/A"]

if(subpopulations_present=="Yes"){
  write.csv(tab23, paste0(diag_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_diagnoses_rates_first_masked.csv"), row.names = F)
} else {
  write.csv(tab23, paste0(diag_dir, "Masked/", "diagnoses_rates_first_masked.csv"), row.names = F)
}

rm(tab23)

