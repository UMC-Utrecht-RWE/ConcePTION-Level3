############################################
#EVENTS
############################################
if("EVENTS" %in% names(actual_tables)){
  ###################################################
  #List for saving info
  ###################################################
  print("Creating lists to save the information.")
  orig_no_rows_events<-list() #original number of records in the EVENTS table
  #######################
  #pers_stdpop_not_events
  events_study_pop<-list() #number of records for the study population, no selection criteria for time applied
  events_date_miss<-list() #number of record with missing start_date_record
  years_events<-list()
  events_not_vocabularies<-list() #number of records where event_record_vocabulary not of interest
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
    df<-df[study_population,on=.(person_id)]#left join, keeps all people in the study population even if they didn't have an event
    df<-df[,age_fup:=as.numeric(age_fup)]
    pers_stdpop_not_events<-df[rowSums(is.na(df[,..colnames_events]))==length(colnames_events), ..std_names_events] #subjects id present in the study population but that do not have an event
    pers_stdpop_not_events<-pers_stdpop_not_events[!duplicated(person_id)]
    df<-df[!rowSums(is.na(df[,..colnames_events]))==length(colnames_events)]
    if(pers_stdpop_not_events[,.N]>0){
      saveRDS(pers_stdpop_not_events, paste0(events_tmp, paste0("stdpop_not_events_", actual_tables$EVENTS[y], ".rds"))) #to give the results we will merge the files by keeping only person_id that are in all saved files
    }
    rm(pers_stdpop_not_events)
    events_study_pop[[w]]<-df[,.N] #number of records for the study population, no selection criteria for time applied
    #transform into date variables
    df[,start_date_record:=as.Date(start_date_record,"%Y%m%d")] #transform to date variables
    #create year variable
    df[,year:=year(start_date_record)]
    #number of records with both start_date_record missing
    events_date_miss[[w]]<-df[is.na(year),.N]
    #remove records with both dates missing
    df<-df[!is.na(year)]
    #remove records that are outside the obs_period for all subjects
    events_out_st_per[[w]]<-df[start_date_record<start_follow_up | start_date_record>exit_spell_category,.N] #number of records outside study population
    df[(start_date_record<start_follow_up | start_date_record>exit_spell_category), obs_out:=1]
    df<-df[is.na(obs_out)] #remove records outside study period
    df[,obs_out:=NULL] 
    events_study_pop_obsper[[w]]<-df[,.N] #number of records after removing records outside study period
    events_stdpop_no_meaning[[w]]<-df[is.na(meaning),.N] #number of records with empty meaning
    df<-df[!is.na(meaning)] #remove records with empty meaning
    events_code_vocabulary_miss[[w]]<-df[is.na(event_code) & is.na(event_record_vocabulary),.N]#numbe rof records with both event code and vocabulary missing
    df<-df[!is.na(event_code) | !is.na(event_record_vocabulary)]# remove records with both event code and event record vocabulary missing
    events_code_pres_voc_miss[[w]]<-df[!is.na(event_code) & is.na(event_record_vocabulary),.N] #number of records where event code present but vocabulary missing
    df<-df[!is.na(event_record_vocabulary)] #remove empty vocabularies
    events_not_vocabularies[[w]]<-df[event_record_vocabulary %!in% vocabularies_list,.N] #number of records where vocabularies doesn't match the codelist
    df<-df[is.na(event_record_vocabulary) | event_record_vocabulary %in% vocabularies_list] #remove records where vocabularies are not of interest
    events_sex_not_specified[[w]]<-df[sex_at_instance_creation =="U" | sex_at_instance_creation == "O",.N] #number of records with unspecified sex
    df<-df[sex_at_instance_creation != "U" | sex_at_instance_creation != "O"]#remove unspecified sex
    #########
      meanings_events[[w]]<-unique(na.omit(df[, meaning])) #will be used for description
      years_events[[w]]<-unique(na.omit(df[, year])) #will be used for description
      male_population_events[[w]]<-ifelse(df[sex_at_instance_creation=="M",.N]>0,1,0)
      female_population_events[[w]]<-ifelse(df[sex_at_instance_creation=="F",.N]>0,1,0)
      females_childbearing_events[[w]]<-ifelse(df[sex_at_instance_creation=="F" & age_fup>=12 & age_fup<=55,.N]>0,1,0)
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
      years_study_events<-df[!duplicated(year), year]#years present in this table
      
      if(sum(df[!duplicated(event_record_vocabulary), event_record_vocabulary] %in% c("ICD10", "ICD10CM","ICD9","ICD9CM","ICPC"))>0){
        for (i in 1:length(conditions_start)){
          for(j in 1:length(conditions_start[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(conditions_start[[i]][[j]][z])), df[["event_code"]]) & event_record_vocabulary==names(conditions_start[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(conditions_start[[i]][[j]][z])), df[["event_code"]]) & event_record_vocabulary==names(conditions_start[[i]])[j],filter:=1]
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
      
      if(sum(df[!duplicated(event_record_vocabulary), event_record_vocabulary] %in% c("RCD2"))>0){
        for (i in 1:length(conditions_read)){
          for(j in 1:length(conditions_read[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(conditions_read[[i]][[j]][z])), df[["event_code"]]) & event_record_vocabulary==names(conditions_read[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(conditions_read[[i]][[j]][z])), df[["event_code"]]) & event_record_vocabulary==names(conditions_read[[i]])[j],filter:=1]
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
      
      if(sum(df[!duplicated(event_record_vocabulary), event_record_vocabulary] %in% c("SNOMEDCT_US"))>0){
        for (i in 1:length(conditions_snomed)){
          for(j in 1:length(conditions_snomed[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(conditions_snomed[[i]][[j]][z])), df[["event_code"]]) & event_record_vocabulary==names(conditions_snomed[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(conditions_snomed[[i]][[j]][z])), df[["event_code"]]) & event_record_vocabulary==names(conditions_snomed[[i]])[j],filter:=1]
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
  #number of records in the study population with start_date_record inside study period (flowchart 6)
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
  empty_event_code.my<-do.call(rbind,empty_event_code.my)
  setnames(empty_event_code.my,"N", "no_empty_code")
  if(empty_event_code.my[,.N]==0){
    tab20_events<-data.table(events_study_population_my, no_empty_code=0)
  } else {
    tab20_events<-merge(events_study_population_my,empty_event_code.my, by=c("meaning","year"))
    tab20_events[is.na(no_empty_code),no_empty_code:=0]
  }
  if(tab20_events[is.na(meaning),.N]==1 & tab20_events[,.N]==1){tab20_events<-NULL}
  
  rm(empty_event_code.my)
  
  ##############################################################################
  #tab21
  ##############################################################################
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
  counts_events<-vector(mode="list", length(files))
  total_events<-vector(mode="list", length(files))
  for (i in 1:length(files)){
    combined_diagnosis_events<-lapply(paste0(events_tmp,files[[i]]), readRDS)
    combined_diagnosis_events<-do.call(rbind,combined_diagnosis_events)
    combined_diagnosis_events[,truncated_code:=substr(event_code,1,5)]
    if (subpopulations_present=="Yes"){
    saveRDS(combined_diagnosis_events, paste0(diag_pop,subpopulations_names[s], "/", names(files)[i],"_events_diagnoses.rds"))
    } else {
      saveRDS(combined_diagnosis_events, paste0(diag_pop,names(files)[i],"_events_diagnoses.rds"))
    }
      counts_events[[i]]<-combined_diagnosis_events[,.N,by=.(condition,event_record_vocabulary,meaning,truncated_code)]
    total_events[[i]]<-combined_diagnosis_events[,.N,by=.(condition)]
  }
  rm(files)
  for(i in 1:length(conditions_files)){
    unlink(paste0(events_tmp,conditions_files[i]))
  }
  
  counts_events<-do.call(rbind,counts_events)
  names(counts_events)<-c("UMLS_concept","vocabulary","meaning","code_truncated","count")
  counts_events<-counts_events[,lapply(.SD,sum), by=.(UMLS_concept,vocabulary,meaning,code_truncated),.SDcols="count"]
  total_events<-do.call(rbind,total_events)
  names(total_events)<-c("UMLS_concept","total")
  total_events<-total_events[,lapply(.SD,sum), by=.(UMLS_concept),.SDcols="total"]

  } else {
    counts_events<-NULL
    total_events<-NULL
  }
  
} else {
  flowchart_events<-data.table(indicator=c("Number of records in the original table", 
                                           "Exclude:Number of records with excluded meanings",
                                           "Number of records for the study_population(no time criteria)",
                                           "Exclude: Number of records with date record missing",
                                           "Exclude: Number of records with date record outside study period",
                                           "Number of records for the study_population(time criteria applied)",
                                           "Exclude:Number of records with empty meaning",
                                           "Exclude: Number of records with both code and vocabulary variables missing",
                                           "Exclude: Number of records with empty vocabulary when code is present",
                                           "Exclude: Number of records with vocabularies not present in the codelist",
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
  counts_events<-NULL
  total_events<-NULL
}

############################################
#MEDICAL_OBSERVATIONS
###########################################
if("MEDICAL_OBSERVATIONS" %in% names(actual_tables)){
  ###################################################
  #List for saving info
  ###################################################
  print("Creating lists to save the information.")
  orig_no_rows_mo<-list() #original number of records in the mo table
  #######################
  #pers_stdpop_not_mo
  mo_study_pop<-list() #number of records for the study population, no selection criteria for time applied
  mo_date_miss<-list() #number of record with missing mo_date
  years_mo<-list()
  mo_not_vocabularies<-list() #number of records where  mo_record_vocabulary not of interest
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
    df<-df[study_population,on=.(person_id)]#left join, keeps all people in the study population even if they didn't have an event
    df<-df[,age_fup:=as.numeric(age_fup)]
    pers_stdpop_not_mo<-df[rowSums(is.na(df[,..colnames_mo]))==length(colnames_mo), ..std_names_mo] #subjects id present in the study population but that do not have an event
    pers_stdpop_not_mo<-pers_stdpop_not_mo[!duplicated(person_id)]
    df<-df[!rowSums(is.na(df[,..colnames_mo]))==length(colnames_mo)]
    if(pers_stdpop_not_mo[,.N]>0){
      saveRDS(pers_stdpop_not_mo, paste0(mo_tmp, paste0("stdpop_not_mo_", actual_tables$MEDICAL_OBSERVATIONS[y], ".rds"))) #to give the results we will merge the files by keeping only person_id that are in all saved files
    }
    rm(pers_stdpop_not_mo)
    mo_study_pop[[w]]<-df[,.N] #number of records for the study population, no selection criteria for time applied
    #transform into date variables
    df[,mo_date:=as.Date(mo_date,"%Y%m%d")] #transform to date variables
    #create year variable
    df[,year:=year(mo_date)]
    #number of records with both mo_date missing
    mo_date_miss[[w]]<-df[is.na(year),.N]
    #remove records with both dates missing
    df<-df[!is.na(year)]
    #remove records that are outside the obs_period for all subjects
    mo_out_st_per[[w]]<-df[mo_date<start_follow_up | mo_date>exit_spell_category,.N] #number of records outside study population
    df[(mo_date<start_follow_up | mo_date>exit_spell_category), obs_out:=1]
    df<-df[is.na(obs_out)] #remove records outside study period
    df[,obs_out:=NULL] 
    mo_study_pop_obsper[[w]]<-df[,.N] #number of records after removing records outside study period
    mo_stdpop_no_meaning[[w]]<-df[is.na(meaning),.N] #number of records with empty meaning
    df<-df[!is.na(meaning)] #remove records with empty meaning
    mo_code_vocabulary_miss[[w]]<-df[is.na(mo_code) & is.na( mo_record_vocabulary),.N]#numbe rof records with both event code and vocabulary missing
    df<-df[!is.na(mo_code) | !is.na( mo_record_vocabulary)]# remove records with both event code and event record vocabulary missing
    mo_code_pres_voc_miss[[w]]<-df[!is.na(mo_code) & is.na( mo_record_vocabulary),.N] #number of records where event code present but vocabulary missing
    df<-df[!is.na( mo_record_vocabulary)] #remove empty vocabularies
    mo_not_vocabularies[[w]]<-df[ mo_record_vocabulary %!in% vocabularies_list,.N] #number of records where vocabularies doesn't match the codelist
    df<-df[is.na( mo_record_vocabulary) |  mo_record_vocabulary %in% vocabularies_list] #remove records where vocabularies are not of interest
    mo_sex_not_specified[[w]]<-df[sex_at_instance_creation =="U" | sex_at_instance_creation == "O",.N] #number of records with unspecified sex
    df<-df[sex_at_instance_creation != "U" | sex_at_instance_creation != "O"]#remove unspecified sex
    
    #########
      meanings_mo[[w]]<-df[!duplicated(meaning)][["meaning"]] #will be used for description
      years_mo[[w]]<-df[!duplicated(year)][["year"]] #will be used for description
      male_population_mo[[w]]<-ifelse(df[sex_at_instance_creation=="M",.N]>0,1,0)
      female_population_mo[[w]]<-ifelse(df[sex_at_instance_creation=="F",.N]>0,1,0)
      females_childbearing_mo[[w]]<-ifelse(df[sex_at_instance_creation=="F" & age_fup>=12 & age_fup<=55,.N]>0,1,0)
      ############################
      #Table 20
      ###########################
      mo_study_population[[w]]<-df[,.N] #number of records in the study population
      mo_study_population_meaning[[w]]<-df[,.N, by="meaning"] #number of records in the study population by meaning
      mo_study_population_my[[w]]<-df[,.N, by=.(meaning,year)] #number of records in the study population by meaning and year
      empty_mo_code.my[[w]]<-df[is.na(mo_code), .N, by=.(meaning,year)] #number of records with missing event code when date disp/presc is present
      ##################################################################
      #match codes based on coding system and code: algorithm start with
      #################################################################
      years_study_mo<-df[!duplicated(year), year]#years present in this table
      
      if(sum(df[!duplicated( mo_record_vocabulary),  mo_record_vocabulary] %in% c("ICD10", "ICD10CM","ICD9","ICD9CM","ICPC"))>0){
        for (i in 1:length(conditions_start)){
          for(j in 1:length(conditions_start[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(conditions_start[[i]][[j]][z])), df[["mo_code"]]) &  mo_record_vocabulary==names(conditions_start[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(conditions_start[[i]][[j]][z])), df[["mo_code"]]) &  mo_record_vocabulary==names(conditions_start[[i]])[j],filter:=1]
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
      
      if(sum(df[!duplicated( mo_record_vocabulary),  mo_record_vocabulary] %in% c("RCD2"))>0){
        for (i in 1:length(conditions_read)){
          for(j in 1:length(conditions_read[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(conditions_read[[i]][[j]][z])), df[["mo_code"]]) &  mo_record_vocabulary==names(conditions_read[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(conditions_read[[i]][[j]][z])), df[["mo_code"]]) &  mo_record_vocabulary==names(conditions_read[[i]])[j],filter:=1]
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
      
      if(sum(df[!duplicated( mo_record_vocabulary),  mo_record_vocabulary] %in% c("SNOMEDCT_US"))>0){
        for (i in 1:length(conditions_snomed)){
          for(j in 1:length(conditions_snomed[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(conditions_snomed[[i]][[j]][z])), df[["mo_code"]]) &  mo_record_vocabulary==names(conditions_snomed[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(conditions_snomed[[i]][[j]][z])), df[["mo_code"]]) &  mo_record_vocabulary==names(conditions_snomed[[i]])[j],filter:=1]
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
  empty_mo_code.my<-do.call(rbind,empty_mo_code.my)
  setnames(empty_mo_code.my,"N", "no_empty_code")
  if(empty_mo_code.my[,.N]==0){
    tab20_mo<-data.table(mo_study_population_my, no_empty_code=0)
  } else {
    tab20_mo<-merge(mo_study_population_my,empty_mo_code.my, by=c("meaning","year"))
    tab20_mo[is.na(no_empty_code),no_empty_code:=0]
  }
  if(tab20_mo[is.na(meaning),.N]==1 & tab20_mo[,.N]==1){tab20_mo<-NULL}
  
  ##############################################################################
  #tab21
  ##############################################################################
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
  counts_mo<-vector(mode="list", length(files))
  total_mo<-vector(mode="list", length(files))
  for (i in 1:length(files)){
    combined_diagnosis_mo<-lapply(paste0(mo_tmp,files[[i]]), readRDS)
    combined_diagnosis_mo<-do.call(rbind,combined_diagnosis_mo)
    combined_diagnosis_mo[,truncated_code:=substr(event_code,1,5)]
    if (subpopulations_present=="Yes"){
    saveRDS(combined_diagnosis_mo, paste0(diag_pop,subpopulations_names[s], "/", names(files)[i],"_mo_diagnoses.rds"))
    } else {
      saveRDS(combined_diagnosis_mo, paste0(diag_pop, names(files)[i],"_mo_diagnoses.rds"))
    }
      counts_mo[[i]]<-combined_diagnosis_mo[,.N,by=.(condition,mo_record_vocabulary,meaning,truncated_code)]
    total_mo[[i]]<-combined_diagnosis_mo[,.N,by=.(condition)]
  }
  rm(files)
  for(i in 1:length(conditions_files)){
    unlink(paste0(mo_tmp,conditions_files[i]))
  }
  
  counts_mo<-do.call(rbind,counts_mo)
  names(counts_mo)<-c("UMLS_concept","vocabulary","meaning","code_truncated","count")
  counts_mo<-counts_mo[,lapply(.SD,sum), by=.(UMLS_concept,vocabulary,meaning,code_truncated),.SDcols="count"]
  total_mo<-do.call(rbind,total_mo)
  names(total_mo)<-c("UMLS_concept","total")
  total_mo<-total_mo[,lapply(.SD,sum), by=.(UMLS_concept),.SDcols="total"]

  } else {
    counts_mo<-NULL
    total_mo<-NULL
  }
  } else {
  flowchart_mo<-data.table(indicator=c("Number of records in the original table", 
                                           "Exclude:Number of records with excluded meanings",
                                           "Number of records for the study_population(no time criteria)",
                                           "Exclude: Number of records with date record missing",
                                           "Exclude: Number of records with date record outside study period",
                                           "Number of records for the study_population(time criteria applied)",
                                           "Exclude:Number of records with empty meaning",
                                           "Exclude: Number of records with both code and vocabulary variables missing",
                                           "Exclude: Number of records with empty vocabulary when code is present",
                                           "Exclude: Number of records with vocabularies not present in the codelist",
                                           "Number of records for study_population"), 
                               MEDICAL_OBSERVATIONS="N/A")
  
  description_mo<-data.table(INDICATOR=c("Data access provider(data source name)",
                                         "List of meanings present",
                                         "Years included in the study period",
                                         "Sex included in the study population",
                                         "Number of subjects in the study population without a recorded diagnosis",
                                         "Presence of females of child-bearing age 12-55 years old (based on age at start follow up)"), 
                             MEDICAL_OBSERVATIONS="N/A")
  
  counts_mo<-NULL
  total_mo<-NULL
}

############################################
#SURVEY_OBSERVATIONS
###########################################
if("SURVEY_OBSERVATIONS" %in% names(actual_tables)){
  ###################################################
  #List for saving info
  ###################################################
  print("Creating lists to save the information.")
  orig_no_rows_so<-list() #original number of records in the mo table
  #######################
  #pers_stdpop_not_so
  so_study_pop<-list() #number of records for the study population, no selection criteria for time applied
  so_date_miss<-list() #number of record with missing so_date
  years_so<-list()
  so_not_vocabularies<-list() #number of records where so_unit not of interest
  so_source_value_vocabulary_miss<-list() #number of records with both so_source_value and so_unit missing
  so_source_value_pres_voc_miss<-list() #numb
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
  empty_so_source_value.my<-list()#number of records with empty so source value in the study population by meaning and year
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
    df<-df[study_population,on=.(person_id)]#left join, keeps all people in the study population even if they didn't have an event
    df<-df[,age_fup:=as.numeric(age_fup)]
    pers_stdpop_not_so<-df[rowSums(is.na(df[,..colnames_so]))==length(colnames_so), ..std_names_so] #subjects id present in the study population but that do not have an event
    pers_stdpop_not_so<-pers_stdpop_not_so[!duplicated(person_id)]
    df<-df[!rowSums(is.na(df[,..colnames_so]))==length(colnames_so)]
    if(pers_stdpop_not_so[,.N]>0){
      saveRDS(pers_stdpop_not_so, paste0(so_tmp, paste0("stdpop_not_so_", actual_tables$SURVEY_OBSERVATIONS[y], ".rds"))) #to give the results we will merge the files by keeping only person_id that are in all saved files
    }
    rm(pers_stdpop_not_so)
    so_study_pop[[w]]<-df[,.N] #number of records for the study population, no selection criteria for time applied
    #transform into date variables
    df[,so_date:=as.Date(so_date,"%Y%m%d")] #transform to date variables
    #create year variable
    df[,year:=year(so_date)]
    #number of records with both so_date missing
    so_date_miss[[w]]<-df[is.na(year),.N]
    #remove records with both dates missing
    df<-df[!is.na(year)]
    #remove records that are outside the obs_period for all subjects
    so_out_st_per[[w]]<-df[so_date<start_follow_up | so_date>exit_spell_category,.N] #number of records outside study population
    df[(so_date<start_follow_up | so_date>exit_spell_category), obs_out:=1]
    df<-df[is.na(obs_out)] #remove records outside study period
    df[,obs_out:=NULL] 
    so_study_pop_obsper[[w]]<-df[,.N] #number of records after removing records outside study period
    so_stdpop_no_meaning[[w]]<-df[is.na(meaning),.N] #number of records with empty meaning
    df<-df[!is.na(meaning)] #remove records with empty meaning
    so_source_value_vocabulary_miss[[w]]<-df[is.na(so_source_value) & is.na(so_unit),.N]#numbe rof records with both event code and vocabulary missing
    df<-df[!is.na(so_source_value) | !is.na(so_unit)]# remove records with both event code and event record vocabulary missing
    so_source_value_pres_voc_miss[[w]]<-df[!is.na(so_source_value) & is.na(so_unit),.N] #number of records where event code present but vocabulary missing
    df<-df[!is.na(so_unit)] #remove empty vocabularies
    so_not_vocabularies[[w]]<-df[so_unit %!in% vocabularies_list,.N] #number of records where vocabularies doesn't match the codelist
    df<-df[is.na(so_unit) | so_unit %in% vocabularies_list] #remove records where vocabularies are not of interest
    so_sex_not_specified[[w]]<-df[sex_at_instance_creation =="U" | sex_at_instance_creation == "O",.N] #number of records with unspecified sex
    df<-df[sex_at_instance_creation != "U" | sex_at_instance_creation != "O"]#remove unspecified sex
    
    #########
      meanings_so[[w]]<-df[!duplicated(meaning)][["meaning"]] #will be used for description
      years_so[[w]]<-df[!duplicated(year)][["year"]] #will be used for description
      male_population_so[[w]]<-ifelse(df[sex_at_instance_creation=="M",.N]>0,1,0)
      female_population_so[[w]]<-ifelse(df[sex_at_instance_creation=="F",.N]>0,1,0)
      females_childbearing_so[[w]]<-ifelse(df[sex_at_instance_creation=="F" & age_fup>=12 & age_fup<=55,.N]>0,1,0)
      ############################
      #Table 20
      ###########################
      so_study_population[[w]]<-df[,.N] #number of records in the study population
      so_study_population_meaning[[w]]<-df[,.N, by="meaning"] #number of records in the study population by meaning
      so_study_population_my[[w]]<-df[,.N, by=.(meaning,year)] #number of records in the study population by meaning and year
      empty_so_source_value.my[[w]]<-df[is.na(so_source_value), .N, by=.(meaning,year)] #number of records with missing event code when date disp/presc is present
      ##################################################################
      #match codes based on coding system and code: algorithm start with
      #################################################################
      years_study_so<-df[!duplicated(year), year]#years present in this table
      
      if(sum(df[!duplicated(so_unit), so_unit] %in% c("ICD10", "ICD10CM","ICD9","ICD9CM","ICPC"))>0){
        for (i in 1:length(conditions_start)){
          for(j in 1:length(conditions_start[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(conditions_start[[i]][[j]][z])), df[["so_source_value"]]) & so_unit==names(conditions_start[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(conditions_start[[i]][[j]][z])), df[["so_source_value"]]) & so_unit==names(conditions_start[[i]])[j],filter:=1]
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
      
      if(sum(df[!duplicated(so_unit), so_unit] %in% c("RCD2"))>0){
        for (i in 1:length(conditions_read)){
          for(j in 1:length(conditions_read[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(conditions_read[[i]][[j]][z])), df[["so_source_value"]]) & so_unit==names(conditions_read[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(conditions_read[[i]][[j]][z])), df[["so_source_value"]]) & so_unit==names(conditions_read[[i]])[j],filter:=1]
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
      
      if(sum(df[!duplicated(so_unit), so_unit] %in% c("SNOMEDCT_US"))>0){
        for (i in 1:length(conditions_snomed)){
          for(j in 1:length(conditions_snomed[[i]])){
            z<-1
            repeat{
              if(df[grepl(paste0("^",paste(conditions_snomed[[i]][[j]][z])), df[["so_source_value"]]) & so_unit==names(conditions_snomed[[i]])[j]][,.N]>0){
                df[grepl(paste0("^",paste(conditions_snomed[[i]][[j]][z])), df[["so_source_value"]]) & so_unit==names(conditions_snomed[[i]])[j],filter:=1]
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
  so_source_value_vocabulary_miss<-do.call(rbind,so_source_value_vocabulary_miss)
  so_source_value_vocabulary_miss<-sum(so_source_value_vocabulary_miss)
  #Number of records with empty vocabulary when code is present
  print("Get number of records with empty vocabulary when code is present")
  so_source_value_pres_voc_miss<-do.call(rbind,so_source_value_pres_voc_miss)
  so_source_value_pres_voc_miss<-sum(so_source_value_pres_voc_miss)
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
                                                  so_source_value_vocabulary_miss,
                                                  so_source_value_pres_voc_miss,
                                                  so_not_vocabularies,
                                                  so_sex_not_specified,
                                                  so_study_population))
  
  rm(orig_no_rows_so,so_excluded_meanings,so_study_pop,so_date_miss,so_out_st_per,
     so_study_pop_obsper,so_stdpop_no_meaning,so_source_value_vocabulary_miss,so_source_value_pres_voc_miss,
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
  empty_so_source_value.my<-do.call(rbind,empty_so_source_value.my)
  setnames(empty_so_source_value.my,"N", "no_empty_code")
  if(empty_so_source_value.my[,.N]==0){
    tab20_so<-data.table(so_study_population_my, no_empty_code=0)
  } else {
    tab20_so<-merge(so_study_population_my,empty_so_source_value.my, by=c("meaning","year"))
    tab20_so[is.na(no_empty_code),no_empty_code:=0]
  }
  if(tab20_so[is.na(meaning),.N]==1 & tab20_so[,.N]==1){tab20_so<-NULL}
  
  rm(empty_so_source_value.my, empty_mo_code.my)
  ##############################################################################
  #tab21
  ##############################################################################
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
    counts_so<-vector(mode="list", length(files))
    total_so<-vector(mode="list", length(files))
    for (i in 1:length(files)){
      combined_diagnosis_so<-lapply(paste0(smo_tmp,files[[i]]), readRDS)
      combined_diagnosis_so<-do.call(rbind,combined_diagnosis_so)
      combined_diagnosis_so[,truncated_code:=substr(event_code,1,5)]
      if (subpopulations_present=="Yes"){
      saveRDS(combined_diagnosis_so, paste0(diag_pop,subpopulations_names[s], "/", names(files)[i],"_so_diagnoses.rds"))
      } else {
        saveRDS(combined_diagnosis_so, paste0(diag_pop, names(files)[i],"_so_diagnoses.rds"))
        
      }
        counts_so[[i]]<-combined_diagnosis_so[,.N,by=.(condition,so_unit,meaning,truncated_code)]
      total_so[[i]]<-combined_diagnosis_so[,.N,by=.(condition)]
    }
    rm(files)
    for(i in 1:length(conditions_files)){
      unlink(paste0(so_tmp,conditions_files[i]))
    }
    
    counts_so<-do.call(rbind,counts_so)
    names(counts_so)<-c("UMLS_concept","vocabulary","meaning","code_truncated","count")
    counts_so<-counts_so[,lapply(.SD,sum), by=.(UMLS_concept,vocabulary,meaning,code_truncated),.SDcols="count"]
    total_so<-do.call(rbind,total_so)
    names(total_so)<-c("UMLS_concept","total")
    total_so<-total_so[,lapply(.SD,sum), by=.(UMLS_concept),.SDcols="total"]

  } else {
    counts_so<-NULL
    total_so<-NULL
  }
  
  
} else {
  flowchart_so<-data.table(indicator=c("Number of records in the original table", 
                                       "Exclude:Number of records with excluded meanings",
                                       "Number of records for the study_population(no time criteria)",
                                       "Exclude: Number of records with date record missing",
                                       "Exclude: Number of records with date record outside study period",
                                       "Number of records for the study_population(time criteria applied)",
                                       "Exclude:Number of records with empty meaning",
                                       "Exclude: Number of records with both code and vocabulary variables missing",
                                       "Exclude: Number of records with empty vocabulary when code is present",
                                       "Exclude: Number of records with vocabularies not present in the codelist",
                                       "Number of records for study_population"), 
                           SURVEY_OBSERVATIONS="N/A")
  
  description_so<-data.table(INDICATOR=c("Data access provider(data source name)",
                                         "List of meanings present",
                                         "Years included in the study period",
                                         "Sex included in the study population",
                                         "Number of subjects in the study population without a recorded diagnosis",
                                         "Presence of females of child-bearing age 12-55 years old (based on age at start follow up)"), 
                             SURVEY_OBSERVATIONS="N/A")
  
  counts_so<-NULL
  total_so<-NULL
}

###########################################################################################
#Combine results 
##########################################################################################
###################
#flowchart
###################
flowchart<-data.table(flowchart_events,flowchart_mo[,2], flowchart_so[,2])
rm(flowchart_events,flowchart_mo,flowchart_so)

###################
#description
###################
description<-data.table(description_events,description_mo[,2], description_so[,2])
rm(description_events,description_mo,description_so)

###################
#tab20
##################
tab20<-rbind(tab20_events, tab20_mo, tab20_so)
rm(tab20_events,tab20_mo,tab20_so)
#combine results if same meaning+year combination exists
tab20<-tab20[,lapply(.SD,sum), .SDcols=c("no_records", "no_empty_code"), by=.(meaning,year)]
tab20<-tab20[,percentage_empty_code:=round(no_empty_code/no_records, digits=2)]


###################
#tab21
###################
tab21<-rbind(counts_events, counts_mo, counts_so)
rm(counts_events, counts_mo, counts_so)
total<-rbind(total_events, total_mo, total_so)
rm(total_events,total_mo,total_so)
tab21<-tab21[,lapply(.SD,sum), .SDcols=c("count"), by=.(UMLS_concept, vocabulary, meaning,code_truncated)]
total<-total[,lapply(.SD,sum), .SDcols=c("total"), by=.(UMLS_concept)]
#combine results if same UMLS_concept+vocabulary+meaning+code_truncated combination exists
tab21<-merge(tab21,total,by="UMLS_concept")


