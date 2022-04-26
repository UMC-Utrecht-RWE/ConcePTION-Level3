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
  ############################################################################################
  #Counts and rates
  ############################################################################################
  #Combine dataset by year and stage of pregnancy
  pregnancy_files_events<-list()
  pregnancy_files_events$start_of_pregnancy<-list.files(preg_ev_tmp, "start_of_pregnancy")
  pregnancy_files_events$end_of_pregnancy<-list.files(preg_ev_tmp, "end_of_pregnancy")
  pregnancy_files_events$ongoing_pregnancy<-list.files(preg_ev_tmp, "ongoing_pregnancy")
  pregnancy_files_events$interruption_pregnancy<-list.files(preg_ev_tmp, "interruption_pregnancy")
  
  pregnancy_files_events<-Filter(length,pregnancy_files_events) #all files are separated based on year and stage of pregnancy
  if(sum(do.call(rbind,lapply(pregnancy_files_events, length)))>0){
  time_lag<-data.table(stage_of_pregnancy=c("start_of_pregnancy", "end_of_pregnancy", "interruption_pregnancy"),time_lag=c(6*7,28*7,11*7))
  counts_events_preg_rec_ev<-list()
  counts_events_preg_subj_ev<-list()
  counts_events_preg_py_ev<-list()
  no_women_code_1_ev<-list()
  no_women_code_1_ev_fup<-list()
  py_code_1_fup_ev<-list()
  z<-1
  for (t in 1:length(pregnancy_files_events)){
    combined_preg_stage<-lapply(paste0(preg_ev_tmp,pregnancy_files_events[[t]]), readRDS) #combine all files for one pregnancy stage
    combined_preg_stage<-do.call(rbind,combined_preg_stage)
    combined_preg_stage[,lag:=time_lag[stage_of_pregnancy == names(pregnancy_files_events)[t],time_lag]] #create lag variable based on condition
    #Step 1: Order the dataset by person_id, stage_of_pregnancy and date of event
    combined_preg_stage<-combined_preg_stage[order(person_id,condition,start_date_record)]
    #Step 2: Create date_2(by shifting the first date)
    combined_preg_stage[,date_1:=shift(start_date_record)]
    combined_preg_stage[,date_2:=start_date_record]
    #Step 3: Create rowid(whhcih will give the number of rows for each person)
    combined_preg_stage[,rowid:=rowid(person_id)]
    #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
    combined_preg_stage[rowid==1,date_1:=NA]
    #Step 5: Create date_dif as difference between date 2 and date
    combined_preg_stage[,date_dif:=date_2-date_1]
    #Step 6: Remove these rows 
    combined_preg_stage<-combined_preg_stage[date_dif>lag | is.na(date_dif)]
    #Step 7: Repeat until there are no more impossible dates in the dataset
    combined_preg_stage[,date_dif:=date_2-date_1]
    while(combined_preg_stage[date_dif<=lag,.N]>0){
      combined_preg_stage<-combined_preg_stage[date_dif>lag | is.na(date_dif)]
      combined_preg_stage[,date_dif:=date_2-date_1]
    }
    
    ###############################################
    #tab19_events
    ###############################################
    #no of women with at least one code, no of women with at least one code and 365 years
    #of follow up, %of women with one code+365 days, median follow up for these women(not possible)
    #insted py for women at least 365 years is provided
    #rowid==1 will be used as filter
    combined_preg_stage[,fup_time:=exit_spell_category-start_follow_up]
    no_women_code_1_ev[[z]]<-combined_preg_stage[rowid==1 & !duplicated(person_id),.N, by=.(condition,year)] #number of women with at least one record
    no_women_code_1_ev_fup[[z]]<-combined_preg_stage[rowid==1 & !duplicated(person_id) & fup_time>=365,.N, by=.(condition,year)]
    #########################
    #calculate person time by year
    #########################
    combined_preg_stage[fup_time>=365,year_start:=year(start_follow_up)]
    combined_preg_stage[fup_time>=365,year_end:=year(exit_spell_category)]
    min_year<-min(combined_preg_stage[["year_start"]])
    max_year<-max(combined_preg_stage[["year_end"]])
    years_interval<-seq(min_year,max_year,by=1)
    
    res_py<-vector(mode="list",length(years_interval))
    for (i in 1:length(years_interval)){
      res_py[[i]]<-as.data.table(person_time(x=years_interval,dt=combined_preg_stage[fup_time>=365],year_index = i))
    }
    res_py<-do.call(rbind,res_py)
    #apply sum by year
    res_py<-res_py[,lapply(.SD, sum), .SDcols=c("person_years"), by="year"]
    res_py<-data.table(condition=names(pregnancy_files_events)[t],res_py)
    py_code_1_fup_ev[[z]]<-res_py
    rm(res_py)
    
     #################################################
    combined_preg_stage[,date_1:=NULL][,date_2:=NULL][,filter_preg:=NULL][,lag:=NULL][,rowid:=NULL][,date_dif:=NULL][,fup_time:=NULL][,year_start:=NULL][,year_end:=NULL]
    
    if (subpopulations_present=="Yes"){
      saveRDS(combined_preg_stage, paste0(preg_pop,subpopulations_names[s], "/", names(pregnancy_files_events)[t],"_events.rds"))
    } else {
      saveRDS(combined_preg_stage, paste0(preg_pop,names(pregnancy_files_events)[t],"_events.rds"))
    }
    
    #################################################
    #tab17_events
    #################################################
    combined_preg_stage[,fup_time:=exit_spell_category-start_follow_up]
    combined_preg_stage[,rowid:=rowid(person_id)]
    counts_events_preg_rec_ev[[z]]<-combined_preg_stage[,.N,by=.(condition,year)]
    counts_events_preg_subj_ev[[z]]<-combined_preg_stage[!duplicated(person_id) & rowid==1,.N,by=.(condition,year)]
    combined_preg_stage[,year_start:=year(start_follow_up)]
    combined_preg_stage[,year_end:=year(exit_spell_category)]
    min_year<-min(combined_preg_stage[["year_start"]])
    max_year<-max(combined_preg_stage[["year_end"]])
    years_interval<-seq(min_year,max_year,by=1)
    
    res_py<-vector(mode="list",length(years_interval))
    for (i in 1:length(years_interval)){
      res_py[[i]]<-as.data.table(person_time(x=years_interval,dt=combined_preg_stage,year_index = i))
    }
    res_py<-do.call(rbind,res_py)
    #apply sum by year
    res_py<-res_py[,lapply(.SD, sum), .SDcols=c("person_years"), by="year"]
    res_py<-data.table(condition=names(pregnancy_files_events)[t],res_py)
    counts_events_preg_py_ev[[z]]<-res_py
    rm(res_py)
    
    
    #################################################
    #tab18(need to find how to incorporate time increment)
    #################################################
    rm(combined_preg_stage)
    z<-z+1
  }
  
  #remove all files from the tmp folder
  for (i in 1:length(pregnancy_files_events)){
    for(j in 1:length(pregnancy_files_events[[i]])){
      unlink(paste0(preg_ev_tmp,pregnancy_files_events[[i]][j]))
    }
  }
  
  ###########################################################
  #combine results for table 17
  ###########################################################
  counts_events_preg_rec_ev<-do.call(rbind,counts_events_preg_rec_ev)
  setnames(counts_events_preg_rec_ev, "N", "no_records")
  counts_events_preg_subj_ev<-do.call(rbind,counts_events_preg_subj_ev)
  setnames(counts_events_preg_subj_ev, "N", "no_women_at_least_1_code")
  counts_events_preg_py_ev<-do.call(rbind,counts_events_preg_py_ev)
  counts_events_preg_rec_ev[,year:=as.numeric(year)]
  counts_events_preg_subj_ev[,year:=as.numeric(year)]
  counts_events_preg_py_ev[,year:=as.numeric(year)]
  #put together
  counts_events_preg_rec_ev<-merge(counts_events_preg_py_ev,counts_events_preg_rec_ev, by=c("condition", "year"),all=T)
  counts_events_preg_rec_ev<-counts_events_preg_rec_ev[is.na(no_records), no_records:=0]
  counts_events_preg_rec_ev<-merge(counts_events_preg_rec_ev,counts_events_preg_subj_ev, by=c("condition", "year"),all=T)
  counts_events_preg_rec_ev<-counts_events_preg_rec_ev[is.na(no_women_at_least_1_code),no_women_at_least_1_code:=0]
  tab17_events<-counts_events_preg_rec_ev
  rm(counts_events_preg_rec_ev, counts_events_preg_subj_ev,counts_events_preg_py_ev)
  #############################################################
  #tab 19
  #############################################################
  no_women_code_1_ev<-do.call(rbind,no_women_code_1_ev)
  setnames(no_women_code_1_ev, "N", "no_min_1_code")
  no_women_code_1_ev_fup<-do.call(rbind,no_women_code_1_ev_fup)
  setnames(no_women_code_1_ev_fup, "N", "no_min_1_code_fup")
  py_code_1_fup_ev<-do.call(rbind,py_code_1_fup_ev)
  setnames(py_code_1_fup_ev, "person_years", "min_1_code_fup_py")
  py_code_1_fup_ev[,year:=as.numeric(year)]

  tab19_events<-no_women_code_1_ev
  tab19_events<-merge(tab19_events, no_women_code_1_ev_fup,  by=c("condition", "year"),all=T)
  tab19_events<-tab19_events[is.na(no_min_1_code_fup), no_min_1_code_fup:=0]
  tab19_events<-merge(tab19_events, py_code_1_fup_ev,  by=c("condition", "year"),all=T)
  tab19_events<-tab19_events[is.na(min_1_code_fup_py), min_1_code_fup_py:=0]
  tab19_events<-tab19_events[is.na(no_min_1_code), no_min_1_code:=0]
  tab19_events<-tab19_events[is.na(no_min_1_code_fup), no_min_1_code_fup:=0]
  rm(no_women_code_1_ev,no_women_code_1_ev_fup,py_code_1_fup_ev)
  } else {
    tab17_events<-NULL
    tab19_events<-NULL
  }
} else {
  flowchart_events_preg<-data.table(INDICATOR=c("Number of records in the original table",
                                                "Exclude:Number of records with excluded meanings",
                                                "Number of subjects in the original study population table",
                                                "Exclude: Number of records for male subjects",
                                                "Exclude: Number of records with unknown or other sex",
                                                "Exclude: Number of records for females younger than 12 years old or older than 55 years old at start of follow up ",
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
  tab17_events<-NULL
  tab19_events<-NULL
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
  ############################################################################################
  #Counts and rates
  ############################################################################################
  #Combine dataset by year and stage of pregnancy
  pregnancy_files_mo<-list()
  pregnancy_files_mo$start_of_pregnancy<-list.files(preg_m_tmp, "start_of_pregnancy")
  pregnancy_files_mo$end_of_pregnancy<-list.files(preg_m_tmp, "end_of_pregnancy")
  pregnancy_files_mo$ongoing_pregnancy<-list.files(preg_m_tmp, "ongoing_pregnancy")
  pregnancy_files_mo$interruption_pregnancy<-list.files(preg_m_tmp, "interruption_pregnancy")
  
  pregnancy_files_mo<-Filter(length,pregnancy_files_mo) #all files are separated based on year and stage of pregnancy
  if(sum(do.call(rbind,lapply(pregnancy_files_mo, length)))>0){
  time_lag<-data.table(stage_of_pregnancy=c("start_of_pregnancy", "end_of_pregnancy", "interruption_pregnancy"),time_lag=c(6*7,28*7,11*7))
  counts_mo_preg_rec_mo<-list()
  counts_mo_preg_subj_mo<-list()
  counts_mo_preg_py_mo<-list()
  no_women_code_1_mo<-list()
  no_women_code_1_mo_fup<-list()
  py_code_1_fup_mo<-list()
  z<-1
  for (t in 1:length(pregnancy_files_mo)){
    combined_preg_stage<-lapply(paste0(preg_m_tmp,pregnancy_files_mo[[t]]), readRDS) #combine all files for one pregnancy stage
    combined_preg_stage<-do.call(rbind,combined_preg_stage)
    combined_preg_stage[,lag:=time_lag[stage_of_pregnancy == names(pregnancy_files_mo)[t],time_lag]] #create lag variable based on condition
    #Step 1: Order the dataset by person_id, stage_of_pregnancy and date of mo
    combined_preg_stage<-combined_preg_stage[order(person_id,condition,mo_date)]
    #Step 2: Create date_2(by shifting the first date)
    combined_preg_stage[,date_1:=shift(mo_date)]
    combined_preg_stage[,date_2:=mo_date]
    #Step 3: Create rowid(whhcih will give the number of rows for each person)
    combined_preg_stage[,rowid:=rowid(person_id)]
    #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
    combined_preg_stage[rowid==1,date_1:=NA]
    #Step 5: Create date_dif as difference between date 2 and date
    combined_preg_stage[,date_dif:=date_2-date_1]
    #Step 6: Remove these rows 
    combined_preg_stage<-combined_preg_stage[date_dif>lag | is.na(date_dif)]
    #Step 7: Repeat until there are no more impossible dates in the dataset
    combined_preg_stage[,date_dif:=date_2-date_1]
    while(combined_preg_stage[date_dif<=lag,.N]>0){
      combined_preg_stage<-combined_preg_stage[date_dif>lag | is.na(date_dif)]
      combined_preg_stage[,date_dif:=date_2-date_1]
    }
    
    ###############################################
    #tab19_mo
    ###############################################
    #no of women with at least one code, no of women with at least one code and 365 years
    #of follow up, %of women with one code+365 days, median follow up for these women(not possible)
    #insted py for women at least 365 years is provided
    #rowid==1 will be used as filter
    combined_preg_stage[,fup_time:=exit_spell_category-start_follow_up]
    no_women_code_1_mo[[z]]<-combined_preg_stage[rowid==1 & !duplicated(person_id),.N, by=.(condition,year)] #number of women with at least one record
    no_women_code_1_mo_fup[[z]]<-combined_preg_stage[rowid==1 & !duplicated(person_id) & fup_time>=365,.N, by=.(condition,year)]
    #########################
    #calculate person time by year
    #########################
    combined_preg_stage[fup_time>=365,year_start:=year(start_follow_up)]
    combined_preg_stage[fup_time>=365,year_end:=year(exit_spell_category)]
    min_year<-min(combined_preg_stage[["year_start"]])
    max_year<-max(combined_preg_stage[["year_end"]])
    years_interval<-seq(min_year,max_year,by=1)
    
    res_py<-vector(mode="list",length(years_interval))
    for (i in 1:length(years_interval)){
      res_py[[i]]<-as.data.table(person_time(x=years_interval,dt=combined_preg_stage[fup_time>=365],year_index = i))
    }
    res_py<-do.call(rbind,res_py)
    #apply sum by year
    res_py<-res_py[,lapply(.SD, sum), .SDcols=c("person_years"), by="year"]
    res_py<-data.table(condition=names(pregnancy_files_mo)[t],res_py)
    py_code_1_fup_mo[[z]]<-res_py
    rm(res_py)
    
    #################################################
    combined_preg_stage[,date_1:=NULL][,date_2:=NULL][,filter_preg:=NULL][,lag:=NULL][,rowid:=NULL][,date_dif:=NULL][,fup_time:=NULL][,year_start:=NULL][,year_end:=NULL]
    
    if (subpopulations_present=="Yes"){
      saveRDS(combined_preg_stage, paste0(preg_pop,subpopulations_names[s], "/", names(pregnancy_files_mo)[t],"_mo.rds"))
    } else {
      saveRDS(combined_preg_stage, paste0(preg_pop,names(pregnancy_files_mo)[t],"_mo.rds"))
    }
    
    #################################################
    #tab17_mo
    #################################################
    combined_preg_stage[,fup_time:=exit_spell_category-start_follow_up]
    combined_preg_stage[,rowid:=rowid(person_id)]
    counts_mo_preg_rec_mo[[z]]<-combined_preg_stage[,.N,by=.(condition,year)]
    counts_mo_preg_subj_mo[[z]]<-combined_preg_stage[!duplicated(person_id) & rowid==1,.N,by=.(condition,year)]
    combined_preg_stage[,year_start:=year(start_follow_up)]
    combined_preg_stage[,year_end:=year(exit_spell_category)]
    min_year<-min(combined_preg_stage[["year_start"]])
    max_year<-max(combined_preg_stage[["year_end"]])
    years_interval<-seq(min_year,max_year,by=1)
    
    res_py<-vector(mode="list",length(years_interval))
    for (i in 1:length(years_interval)){
      res_py[[i]]<-as.data.table(person_time(x=years_interval,dt=combined_preg_stage,year_index = i))
    }
    res_py<-do.call(rbind,res_py)
    #apply sum by year
    res_py<-res_py[,lapply(.SD, sum), .SDcols=c("person_years"), by="year"]
    res_py<-data.table(condition=names(pregnancy_files_mo)[t],res_py)
    counts_mo_preg_py_mo[[z]]<-res_py
    rm(res_py)
    
    
    #################################################
    #tab18(need to find how to incorporate time increment)
    #################################################
    rm(combined_preg_stage)
    z<-z+1
  }
  
  #remove all files from the tmp folder
  for (i in 1:length(pregnancy_files_mo)){
    for(j in 1:length(pregnancy_files_mo[[i]])){
      unlink(paste0(preg_m_tmp,pregnancy_files_mo[[i]][j]))
    }
  }
  
  ###########################################################
  #combine results for table 17
  ###########################################################
  counts_mo_preg_rec_mo<-do.call(rbind,counts_mo_preg_rec_mo)
  setnames(counts_mo_preg_rec_mo, "N", "no_records")
  counts_mo_preg_subj_mo<-do.call(rbind,counts_mo_preg_subj_mo)
  setnames(counts_mo_preg_subj_mo, "N", "no_women_at_least_1_code")
  counts_mo_preg_py_mo<-do.call(rbind,counts_mo_preg_py_mo)
  counts_mo_preg_rec_mo[,year:=as.numeric(year)]
  counts_mo_preg_subj_mo[,year:=as.numeric(year)]
  counts_mo_preg_py_mo[,year:=as.numeric(year)]
  #put together
  counts_mo_preg_rec_mo<-merge(counts_mo_preg_py_mo,counts_mo_preg_rec_mo, by=c("condition", "year"),all=T)
  counts_mo_preg_rec_mo<-counts_mo_preg_rec_mo[is.na(no_records), no_records:=0]
  counts_mo_preg_rec_mo<-merge(counts_mo_preg_rec_mo,counts_mo_preg_subj_mo, by=c("condition", "year"),all=T)
  counts_mo_preg_rec_mo<-counts_mo_preg_rec_mo[is.na(no_women_at_least_1_code),no_women_at_least_1_code:=0]
  tab17_mo<-counts_mo_preg_rec_mo
  rm(counts_mo_preg_rec_mo, counts_mo_preg_subj_mo,counts_mo_preg_py_mo)
  #############################################################
  #tab 19
  #############################################################
  no_women_code_1_mo<-do.call(rbind,no_women_code_1_mo)
  setnames(no_women_code_1_mo, "N", "no_min_1_code")
  no_women_code_1_mo_fup<-do.call(rbind,no_women_code_1_mo_fup)
  setnames(no_women_code_1_mo_fup, "N", "no_min_1_code_fup")
  py_code_1_fup_mo<-do.call(rbind,py_code_1_fup_mo)
  setnames(py_code_1_fup_mo, "person_years", "min_1_code_fup_py")
  py_code_1_fup_mo[,year:=as.numeric(year)]
  
  tab19_mo<-no_women_code_1_mo
  tab19_mo<-merge(tab19_mo, no_women_code_1_mo_fup,  by=c("condition", "year"),all=T)
  tab19_mo<-tab19_mo[is.na(no_min_1_code_fup), no_min_1_code_fup:=0]
  tab19_mo<-merge(tab19_mo, py_code_1_fup_mo,  by=c("condition", "year"),all=T)
  tab19_mo<-tab19_mo[is.na(min_1_code_fup_py), min_1_code_fup_py:=0]
  tab19_mo<-tab19_mo[is.na(no_min_1_code), no_min_1_code:=0]
  tab19_mo<-tab19_mo[is.na(no_min_1_code_fup), no_min_1_code_fup:=0]
  rm(no_women_code_1_mo,no_women_code_1_mo_fup,py_code_1_fup_mo)
  } else {
    tab17_mo<-NULL
    tab19_mo<-NULL
  }
} else {
  flowchart_mo_preg<-data.table(INDICATOR=c("Number of records in the original table",
                                            "Exclude:Number of records with excluded meanings",
                                            "Number of subjects in the original study population table",
                                            "Exclude: Number of records for male subjects",
                                            "Exclude: Number of records with unknown or other sex",
                                            "Exclude: Number of records for females younger than 12 years old or older than 55 years old at start of follow up ",
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
  tab17_mo<-NULL
  tab19_mo<-NULL
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
  ############################################################################################
  #Counts and rates
  ############################################################################################
  #Combine dataset by year and stage of pregnancy
  pregnancy_files_so<-list()
  pregnancy_files_so$start_of_pregnancy<-list.files(preg_s_tmp, "start_of_pregnancy")
  pregnancy_files_so$end_of_pregnancy<-list.files(preg_s_tmp, "end_of_pregnancy")
  pregnancy_files_so$ongoing_pregnancy<-list.files(preg_s_tmp, "ongoing_pregnancy")
  pregnancy_files_so$interruption_pregnancy<-list.files(preg_s_tmp, "interruption_pregnancy")
  
  pregnancy_files_so<-Filter(length,pregnancy_files_so) #all files are separated based on year and stage of pregnancy

  
  if(sum(do.call(rbind,lapply(pregnancy_files_so, length)))>0){
  time_lag<-data.table(stage_of_pregnancy=c("start_of_pregnancy", "end_of_pregnancy", "interruption_pregnancy"),time_lag=c(6*7,28*7,11*7))
  counts_so_preg_rec_so<-list()
  counts_so_preg_subj_so<-list()
  counts_so_preg_py_so<-list()
  no_women_code_1_so<-list()
  no_women_code_1_so_fup<-list()
  py_code_1_fup_so<-list()
  z<-1
  for (t in 1:length(pregnancy_files_so)){
    combined_preg_stage<-lapply(paste0(preg_s_tmp,pregnancy_files_so[[t]]), readRDS) #combine all files for one pregnancy stage
    combined_preg_stage<-do.call(rbind,combined_preg_stage)
    combined_preg_stage[,lag:=time_lag[stage_of_pregnancy == names(pregnancy_files_so)[t],time_lag]] #create lag variable based on condition
    #Step 1: Order the dataset by person_id, stage_of_pregnancy and date of mo
    combined_preg_stage<-combined_preg_stage[order(person_id,condition,so_date)]
    #Step 2: Create date_2(by shifting the first date)
    combined_preg_stage[,date_1:=shift(so_date)]
    combined_preg_stage[,date_2:=so_date]
    #Step 3: Create rowid(whhcih will give the number of rows for each person)
    combined_preg_stage[,rowid:=rowid(person_id)]
    #Step 4: If rowid==1 then date_1 should be NA(because every time rowid is 1 we are considering a new person)
    combined_preg_stage[rowid==1,date_1:=NA]
    #Step 5: Create date_dif as difference between date 2 and date
    combined_preg_stage[,date_dif:=date_2-date_1]
    #Step 6: Remove these rows 
    combined_preg_stage<-combined_preg_stage[date_dif>lag | is.na(date_dif)]
    #Step 7: Repeat until there are no more impossible dates in the dataset
    combined_preg_stage[,date_dif:=date_2-date_1]
    while(combined_preg_stage[date_dif<=lag,.N]>0){
      combined_preg_stage<-combined_preg_stage[date_dif>lag | is.na(date_dif)]
      combined_preg_stage[,date_dif:=date_2-date_1]
    }
    
    ###############################################
    #tab19_so
    ###############################################
    #no of women with at least one code, no of women with at least one code and 365 years
    #of follow up, %of women with one code+365 days, median follow up for these women(not possible)
    #insted py for women at least 365 years is provided
    #rowid==1 will be used as filter
    combined_preg_stage[,fup_time:=exit_spell_category-start_follow_up]
    no_women_code_1_so[[z]]<-combined_preg_stage[rowid==1 & !duplicated(person_id),.N, by=.(condition,year)] #number of women with at least one record
    no_women_code_1_so_fup[[z]]<-combined_preg_stage[rowid==1 & !duplicated(person_id) & fup_time>=365,.N, by=.(condition,year)]
    #########################
    #calculate person time by year
    #########################
    combined_preg_stage[fup_time>=365,year_start:=year(start_follow_up)]
    combined_preg_stage[fup_time>=365,year_end:=year(exit_spell_category)]
    min_year<-min(combined_preg_stage[["year_start"]])
    max_year<-max(combined_preg_stage[["year_end"]])
    years_interval<-seq(min_year,max_year,by=1)
    
    res_py<-vector(mode="list",length(years_interval))
    for (i in 1:length(years_interval)){
      res_py[[i]]<-as.data.table(person_time(x=years_interval,dt=combined_preg_stage[fup_time>=365],year_index = i))
    }
    res_py<-do.call(rbind,res_py)
    #apply sum by year
    res_py<-res_py[,lapply(.SD, sum), .SDcols=c("person_years"), by="year"]
    res_py<-data.table(condition=names(pregnancy_files_so)[t],res_py)
    py_code_1_fup_so[[z]]<-res_py
    rm(res_py)
    
    #################################################
    combined_preg_stage[,date_1:=NULL][,date_2:=NULL][,filter_preg:=NULL][,lag:=NULL][,rowid:=NULL][,date_dif:=NULL][,fup_time:=NULL][,year_start:=NULL][,year_end:=NULL]
    
    if (subpopulations_present=="Yes"){
      saveRDS(combined_preg_stage, paste0(preg_pop,subpopulations_names[s], "/", names(pregnancy_files_so)[t],"_so.rds"))
    } else {
      saveRDS(combined_preg_stage, paste0(preg_pop,names(pregnancy_files_so)[t],"_so.rds"))
    }
    
    #################################################
    #tab17_so
    #################################################
    combined_preg_stage[,fup_time:=exit_spell_category-start_follow_up]
    combined_preg_stage[,rowid:=rowid(person_id)]
    counts_so_preg_rec_so[[z]]<-combined_preg_stage[,.N,by=.(condition,year)]
    counts_so_preg_subj_so[[z]]<-combined_preg_stage[!duplicated(person_id) & rowid==1,.N,by=.(condition,year)]
    combined_preg_stage[,year_start:=year(start_follow_up)]
    combined_preg_stage[,year_end:=year(exit_spell_category)]
    min_year<-min(combined_preg_stage[["year_start"]])
    max_year<-max(combined_preg_stage[["year_end"]])
    years_interval<-seq(min_year,max_year,by=1)
    
    res_py<-vector(mode="list",length(years_interval))
    for (i in 1:length(years_interval)){
      res_py[[i]]<-as.data.table(person_time(x=years_interval,dt=combined_preg_stage,year_index = i))
    }
    res_py<-do.call(rbind,res_py)
    #apply sum by year
    res_py<-res_py[,lapply(.SD, sum), .SDcols=c("person_years"), by="year"]
    res_py<-data.table(condition=names(pregnancy_files_so)[t],res_py)
    counts_so_preg_py_so[[z]]<-res_py
    rm(res_py)
    
    
    #################################################
    #tab18(need to find how to incorporate time increment)
    #################################################
    rm(combined_preg_stage)
    z<-z+1
  }
  
  #remove all files from the tmp folder
  for (i in 1:length(pregnancy_files_so)){
    for(j in 1:length(pregnancy_files_so[[i]])){
      unlink(paste0(preg_s_tmp,pregnancy_files_so[[i]][j]))
    }
  }
  
  ###########################################################
  #combine results for table 17
  ###########################################################
  counts_so_preg_rec_so<-do.call(rbind,counts_so_preg_rec_so)
  setnames(counts_so_preg_rec_so, "N", "no_records")
  counts_so_preg_subj_so<-do.call(rbind,counts_so_preg_subj_so)
  setnames(counts_so_preg_subj_so, "N", "no_women_at_least_1_code")
  counts_so_preg_py_so<-do.call(rbind,counts_so_preg_py_so)
  counts_so_preg_rec_so[,year:=as.numeric(year)]
  counts_so_preg_subj_so[,year:=as.numeric(year)]
  counts_so_preg_py_so[,year:=as.numeric(year)]
  #put together
  counts_so_preg_rec_so<-merge(counts_so_preg_py_so,counts_so_preg_rec_so, by=c("condition", "year"),all=T)
  counts_so_preg_rec_so<-counts_so_preg_rec_so[is.na(no_records), no_records:=0]
  counts_so_preg_rec_so<-merge(counts_so_preg_rec_so,counts_so_preg_subj_so, by=c("condition", "year"),all=T)
  counts_so_preg_rec_so<-counts_so_preg_rec_so[is.na(no_women_at_least_1_code),no_women_at_least_1_code:=0]
  tab17_so<-counts_so_preg_rec_so
  rm(counts_so_preg_rec_so, counts_so_preg_subj_so,counts_so_preg_py_so)
  #############################################################
  #tab 19
  #############################################################
  no_women_code_1_so<-do.call(rbind,no_women_code_1_so)
  setnames(no_women_code_1_so, "N", "no_min_1_code")
  no_women_code_1_so_fup<-do.call(rbind,no_women_code_1_so_fup)
  setnames(no_women_code_1_so_fup, "N", "no_min_1_code_fup")
  py_code_1_fup_so<-do.call(rbind,py_code_1_fup_so)
  setnames(py_code_1_fup_so, "person_years", "min_1_code_fup_py")
  py_code_1_fup_so[,year:=as.numeric(year)]
  
  tab19_so<-no_women_code_1_so
  tab19_so<-merge(tab19_so, no_women_code_1_so_fup,  by=c("condition", "year"),all=T)
  tab19_so<-tab19_so[is.na(no_min_1_code_fup), no_min_1_code_fup:=0]
  tab19_so<-merge(tab19_so, py_code_1_fup_so,  by=c("condition", "year"),all=T)
  tab19_so<-tab19_so[is.na(min_1_code_fup_py), min_1_code_fup_py:=0]
  tab19_so<-tab19_so[is.na(no_min_1_code), no_min_1_code:=0]
  tab19_so<-tab19_so[is.na(no_min_1_code_fup), no_min_1_code_fup:=0]
  rm(no_women_code_1_so,no_women_code_1_so_fup,py_code_1_fup_so)
  } else {
    tab17_so<-NULL
    tab19_so<-NULL
  }
} else {
  flowchart_so_preg<-data.table(INDICATOR=c("Number of records in the original table",
                                            "Exclude:Number of records with excluded meanings",
                                            "Number of subjects in the original study population table",
                                            "Exclude: Number of records for male subjects",
                                            "Exclude: Number of records with unknown or other sex",
                                            "Exclude: Number of records for females younger than 12 years old or older than 55 years old at start of follow up ",
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
  tab17_so<-NULL
  tab19_so<-NULL
}


###########################################################################################
#Combine results 
##########################################################################################
###################
#flowchart
###################
flowchart_preg<-data.table(flowchart_events_preg,flowchart_mo_preg[,2], flowchart_so_preg[,2])
rm(flowchart_events_preg,flowchart_mo_preg,flowchart_so_preg)

###################
#description
###################
description_preg<-data.table(description_events_preg,description_mo_preg[,2], description_so_preg[,2])
rm(description_events_preg,description_mo_preg,description_so_preg)

###################
#tab17
##################
tab17_preg<-rbind(tab17_events, tab17_mo, tab17_so)
rm(tab17_events,tab17_mo,tab17_so)
#combine results if same condition+year combination exists
tab17_preg<-tab17_preg[,lapply(.SD,sum), .SDcols=c("person_years", "no_records","no_women_at_least_1_code"), by=.(condition,year)]
setnames(tab17_preg,"no_women_at_least_1_code", "no_women_min_1_code")
tab17_preg<-tab17_preg[,rate_min_1_code_per_100_py:=round(no_records/person_years, digits=6)]
setnames(tab17_preg,"condition", "stage_of_pregnancy")

###################
#tab19
##################
tab19_preg<-rbind(tab19_events, tab19_mo, tab19_so)
rm(tab19_events,tab19_events,tab19_events)
#combine results if same condition+year combination exists
tab19_preg<-tab19_preg[,lapply(.SD,sum), .SDcols=c("no_min_1_code", "no_min_1_code_fup","min_1_code_fup_py"), by=.(condition,year)]
tab19_preg<-tab19_preg[,percentage_min_1_code_fup:=round((no_min_1_code_fup/no_min_1_code)*100, digits=2)]
setnames(tab19_preg,"condition", "stage_of_pregnancy")

