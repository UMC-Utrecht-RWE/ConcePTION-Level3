#Author: Vjola Hoxhaj Drs.
#email: v.hoxhaj@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/07/2021

print("Analyse SURVEY ID table for the PREGNANCY script.")
############################################
#SURVEY_ID
###########################################
if(length(actual_tables$SURVEY_ID)>0){
  #####List for saving info####
  print("Creating lists to save the information.")
  ####Flowchart parameters######
  orig_no_rows_si_preg<-list() #original number of records in the SURVEY_ID table
  si_excluded_meanings_preg<-list() #number of recorded with excluded meanings
  #nr_std number of records in the original study population
  # si_excluded_males<-list() #number of records for male subjects
  # si_sex_not_specified_preg<-list() #number of records for subjects with unspecified sex
  # females_outside_age_si<-list() #number of records where age outside limits
  si_meanings_not_of_interest<-list() #number of records excluded that do not match meanings of interest ex birth_registry needs to be included
  si_study_pop_preg<-list() #number of records for the study population, no selection criteria for time applied
  si_date_miss_preg<-list() #number of record with missing si_date
  si_out_st_per_preg<-list() #number of SURVEY_ID records outside the observation period(check is done on individual level)
  si_study_pop_obsper_preg<-list() #number of records in the study population inside study period
  si_stdpop_no_meaning_preg<-list() #number of records in the study population with no meaning
  ####description parameters####
  meanings_si_preg<-list() #all meanings present
  years_si_preg<-list() #all years present
  #pers_stdpop_not_si_preg #number of people in the study population without a pregnancy
  ####Description of the data set parameters####  
  si_study_population_preg<-list() #number of records in the study population
  si_study_pop_m_preg<-list() #number of records in the study population by meaning
  si_study_pop_my_preg<-list() #number of records in the study population by meaning and year
  ####Loop to filter data####
  w<-1
  for (y in 1:length(actual_tables$SURVEY_ID)){
    ####Load the table and apply exclusions####
    df<-fread(paste(path_dir, actual_tables$SURVEY_ID[y], sep=""), stringsAsFactors = FALSE, colClasses = "character")
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
    df<-df[!meaning %in% meanings_exclude_si]
    #merge with the study_population table(there is no missing data in this table)
    df[,person_id:=as.character(person_id)]
    study_population[,person_id:=as.character(person_id)]
    df<-df[study_population,on=.(person_id)]#left join, keeps all people in the study population even if they didn't have an mo
    df<-df[sex_at_instance_creation == "F"]#keep only females
    pers_stdpop_not_si_preg<-df[rowSums(is.na(df[,..colnames_si]))==length(colnames_si), ..std_names_si] #subjects id present in the study population but that do not have an mo
    pers_stdpop_not_si_preg<-pers_stdpop_not_si_preg[!duplicated(person_id)]
    df<-df[!rowSums(is.na(df[,..colnames_si]))==length(colnames_si)]
    if(pers_stdpop_not_si_preg[,.N]>0){
      saveRDS(pers_stdpop_not_si_preg, paste0(preg_si_tmp, paste0("stdpop_not_si_preg_", actual_tables$SURVEY_ID[y], ".rds"))) #to give the results we will merge the files by keeping only person_id that are in all saved files
    }
    rm(pers_stdpop_not_si_preg)
    #change applied 13.02.2022
    # #number of records for male subjects
    # si_excluded_males[[w]]<-df[sex_at_instance_creation=="M",.N]
    # #remove males
    # df<-df[sex_at_instance_creation != "M"]
    # #number of records with unspecified sex
    # si_sex_not_specified_preg[[w]]<-df[sex_at_instance_creation =="U" | sex_at_instance_creation == "O",.N] #number of records with unspecified sex
    # #remove unspecified sex
    # df<-df[sex_at_instance_creation == "F"]#remove unspecified sex
    # #number of females outside chidbearing age (based on age at follow up)
    # females_outside_age_si[[w]]<-df[age_start_follow_up_preg< min_age_preg | age_start_follow_up_preg> max_age_preg, .N]
    # df<-df[age_start_follow_up_preg>= min_age_preg & age_start_follow_up_preg<=max_age_preg]
    #number of records with meanings not of interest
    si_meanings_not_of_interest[[w]]<-df[!meaning %in% meanings_birth_registry,.N]
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
    si_out_st_per_preg[[w]]<-df[pregnancy_code_date<start_follow_up_preg | pregnancy_code_date>end_follow_up_preg,.N] #number of records outside study population
    df[(pregnancy_code_date<start_follow_up_preg | pregnancy_code_date>end_follow_up_preg), obs_out:=1]
    df<-df[is.na(obs_out)] #remove records outside study period
    df[,obs_out:=NULL] 
    si_study_pop_obsper_preg[[w]]<-df[,.N] #number of records after removing records outside study period
    si_stdpop_no_meaning_preg[[w]]<-df[is.na(meaning),.N] #number of records with empty meaning
    df<-df[!is.na(meaning)] #remove records with empty meaning
    si_study_population_preg[[w]]<-df[,.N] #number of records in the study population(final)
    ####description####
    meanings_si_preg[[w]]<-df[!duplicated(meaning), "meaning"] #will be used for description
    years_si_preg[[w]]<-df[!duplicated(year), "year"] #will be used for description
    
    if("no_event_id" %in% names(df)){df[,no_event_id:=NULL]}
    if("no_mo_id" %in% names(df)){df[,no_mo_id:=NULL]}
    if("no_so_id" %in% names(df)){df[,no_so_id:=NULL]}
    if("no_si_id" %in% names(df)){df[,no_si_id:=NULL]}
    ####start data filtering####
    if(df[,.N]>0){
      if(class(df[["pregnancy_code_date"]])[1] !="IDate"){
        df[,pregnancy_code_date:=as.IDate(pregnancy_code_date)]
      }
      ####Description of the database####
      si_study_pop_m_preg[[w]]<-df[,.N, by="meaning"] #number of records in the study population by meaning
      si_study_pop_my_preg[[w]]<-df[,.N, by=.(meaning,year)] #number of records in the study population by meaning and year
      
      years_study_si_preg<-df[!duplicated(year), year]#years present in this table
      
      ####save all subjects id that had a recorded pregnancy####
      
      #will be used to add people having another diagnosis when calculating rates recurrent/first event
      pers_included_si<-df[!duplicated(person_id), c("person_id","birth_date","start_follow_up_preg","end_follow_up_preg","sex_at_instance_creation")]
      
      if(pers_included_si[,.N]>0){
        saveRDS(pers_included_si, paste0(preg_si_tmp, paste0("pers_included_si_preg_", actual_tables$SURVEY_ID[y], ".rds"))) #to give the results we will merge the files by keeping only person_id that are in all saved files
      }
      rm(pers_included_si)
      
      ####match codes based on start_of_pregnancy meanings####
      if("filter" %in% names(df)){df[,filter:=NULL]}
      if("condition" %in% names(df)){df[,condition:=NULL]}
      if(sum(df[!duplicated(meaning),meaning] %in% meanings_start_pregnancy)>0){
        df[meaning %in% meanings_start_pregnancy, filter:=T]
        df[,condition:="start_of_pregnancy"]
        years_this_event<-sort(df[filter==T][!duplicated(year),year])
        for(year_ind in 1:length(years_this_event)){
          saveRDS(data.table(df[,c("person_id","pregnancy_code_date","meaning","sex_at_instance_creation","birth_date","start_follow_up_preg","end_follow_up_preg","year","condition","filter")][filter==T & year==years_this_event[year_ind]], condition="start_of_pregnancy"), paste0(preg_si_tmp,years_this_event[year_ind],"_", "start_of_pregnancy", "_",actual_tables$SURVEY_ID[y], "_start.rds"))
        }
        
        if("filter" %in% names(df)){df[,filter:=NULL]}
        rm(years_this_event)
      }
      
      ####match codes based on interruption_pregnancy meanings####
      if("filter" %in% names(df)){df[,filter:=NULL]}
      if("condition" %in% names(df)){df[,condition:=NULL]}
      if(sum(df[!duplicated(meaning),meaning] %in% meanings_interruption_pregnancy)>0){
        df[meaning %in% meanings_interruption_pregnancy, filter:=T]
        df[,condition:="interruption_pregnancy"]
        years_this_event<-sort(df[filter==T][!duplicated(year),year])
        for(year_ind in 1:length(years_this_event)){
          saveRDS(data.table(df[,c("person_id","pregnancy_code_date","meaning","sex_at_instance_creation","birth_date","start_follow_up_preg","end_follow_up_preg","year","condition","filter")][filter==T & year==years_this_event[year_ind]], condition="interruption_pregnancy"), paste0(preg_si_tmp,years_this_event[year_ind],"_", "interruption_pregnancy", "_",actual_tables$SURVEY_ID[y], "_interruption.rds"))
        }
        
        if("filter" %in% names(df)){df[,filter:=NULL]}
        rm(years_this_event)
      }
     
      ####match codes based on ongoing_pregnancy meanings####
      if("filter" %in% names(df)){df[,filter:=NULL]}
      if("condition" %in% names(df)){df[,condition:=NULL]}
      if(sum(df[!duplicated(meaning),meaning] %in% meanings_ongoing_pregnancy)>0){
        df[meaning %in% meanings_ongoing_pregnancy, filter:=T]
        df[,condition:="ongoing_pregnancy"]
        years_this_event<-sort(df[filter==T][!duplicated(year),year])
        for(year_ind in 1:length(years_this_event)){
          saveRDS(data.table(df[,c("person_id","pregnancy_code_date","meaning","sex_at_instance_creation","birth_date","start_follow_up_preg","end_follow_up_preg","year","condition","filter")][filter==T & year==years_this_event[year_ind]], condition="ongoing_pregnancy"), paste0(preg_si_tmp,years_this_event[year_ind],"_", "ongoing_pregnancy", "_",actual_tables$SURVEY_ID[y], "_ongoing.rds"))
        }
        
        if("filter" %in% names(df)){df[,filter:=NULL]}
        rm(years_this_event)
      }
      
      ####match codes based on end_of_pregnancy meanings####
      if("filter" %in% names(df)){df[,filter:=NULL]}
      if("condition" %in% names(df)){df[,condition:=NULL]}
      if(sum(df[!duplicated(meaning),meaning] %in% meanings_end_pregnancy)>0){
        df[meaning %in% meanings_end_pregnancy, filter:=T]
        df[,condition:="end_of_pregnancy"]
        years_this_event<-sort(df[filter==T][!duplicated(year),year])
        for(year_ind in 1:length(years_this_event)){
          saveRDS(data.table(df[,c("person_id","pregnancy_code_date","meaning","sex_at_instance_creation","birth_date","start_follow_up_preg","end_follow_up_preg","year","condition","filter")][filter==T & year==years_this_event[year_ind]], condition="end_of_pregnancy"), paste0(preg_si_tmp,years_this_event[year_ind],"_", "end_of_pregnancy", "_",actual_tables$SURVEY_ID[y], "_end.rds"))
        }
        
        if("filter" %in% names(df)){df[,filter:=NULL]}
        rm(years_this_event)
      }
      
    }
    
    ####repeat loop####
    w<-w+1
    rm(df)
  }
  
  ####number of subjects in the study population that have not had an event####
  stdpop_not_si_files_preg<-list.files(preg_si_tmp, pattern = "stdpop_not_si_preg_")
  if (length(stdpop_not_si_files_preg)>0){
    si_not_id<-readRDS(paste0(preg_si_tmp, stdpop_not_si_files_preg[1]))
    i<-2
    while(i <= length(stdpop_not_si_files_preg)){
      a<-readRDS(paste0(preg_si_tmp, stdpop_not_si_files_preg[i]))
      si_not_id<-rbind(si_not_id, a)
      si_not_id<-si_not_id[duplicated(person_id)]
      i<-i+1
      rm(a)
    }
    
    for(i in 1:length(stdpop_not_si_files_preg)){
      unlink(paste0(preg_si_tmp,stdpop_not_si_files_preg[i]))
    }
    
    study_population[person_id %in% si_not_id[,person_id], no_si_id:=1]
    
    if (si_not_id[,.N]>0){
      saveRDS(si_not_id, paste0(preg_si_tmp, "si_not_ids_stdpop_preg.rds"))
    }
    stdpop_not_si_preg<-si_not_id[,.N]
    rm(si_not_id)
  } else {
    stdpop_not_si_preg<-0
  }
  rm(stdpop_not_si_files_preg)
  ####subjects included in the si_study population####  
  stdpop_si_files_preg<-list.files(preg_si_tmp, pattern = "pers_included_si_preg_")
  if (length(stdpop_si_files_preg)>0){
    si_included_id<-readRDS(paste0(preg_si_tmp, stdpop_si_files_preg[1]))
    i<-2
    while(i <= length(stdpop_si_files_preg)){
      a<-readRDS(paste0(preg_si_tmp, stdpop_si_files_preg[i]))
      si_included_id<-rbind(si_included_id, a)
      si_included_id<-si_included_id[!duplicated(person_id)]
      i<-i+1
      rm(a)
    }
    
    for(i in 1:length(stdpop_si_files_preg)){
      unlink(paste0(preg_si_tmp,stdpop_si_files_preg[i]))
    }
    
    
    if (si_included_id[,.N]>0){
      saveRDS(si_included_id, paste0(preg_si_tmp, "pers_si_preg.rds"))
    }
    rm(si_included_id)
  }
  rm(stdpop_si_files_preg)
  
  
  ####Flowchart####  
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
  # si_excluded_males<-do.call(rbind,si_excluded_males)
  # si_excluded_males<-sum(si_excluded_males)
  # #females outside age excluded(flowchart 5)
  # females_outside_age_si<-do.call(rbind,females_outside_age_si)
  # females_outside_age_si<-sum(females_outside_age_si)
  # #sex not specified(flowchart 6)
  # si_sex_not_specified_preg<-do.call(rbind, si_sex_not_specified_preg)
  # si_sex_not_specified_preg<-sum(si_sex_not_specified_preg)
  # #number of records with meanings not of interest
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
  
  
  rm(orig_no_rows_si_preg, si_excluded_meanings_preg,si_study_pop_preg,si_date_miss_preg,si_out_st_per_preg,
     si_study_pop_obsper_preg,si_stdpop_no_meaning_preg,si_meanings_not_of_interest)  
  
  ####Description####
  print("Creating description of study population.")
  #meanings
  print("Get list of meanings.")
  meanings_si_preg<-Filter(length,meanings_si_preg)
  meanings_si_preg<-suppressWarnings(do.call(rbind,meanings_si_preg))
  meanings_si_preg<-meanings_si_preg[!duplicated(meaning),meaning]
  meanings_si_preg_des<-paste(meanings_si_preg, collapse = ", ")
  #study years
  years_si_preg<-Filter(length,years_si_preg)
  years_si_preg<-suppressWarnings(do.call(rbind, years_si_preg))
  years_si_preg<-sort(years_si_preg[!duplicated(year),year])
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
  study_population[,no_si_id:=1]
  
}
