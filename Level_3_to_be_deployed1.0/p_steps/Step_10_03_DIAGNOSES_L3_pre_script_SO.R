print("Analyse SURVEY OBSERVATIONS table.")
############################################
#SURVEY_OBSERVATIONS
###########################################
if(length(actual_tables$SURVEY_OBSERVATIONS)>0){
  ###################################################
  #List for saving info
  ###################################################
  print("Creating lists to save the information.")
  orig_no_rows_so<-list() #original number of records in the SURVEY_OBSERVATIONS table
  #######################
  #pers_stdpop_not_so
  so_study_pop<-list() #number of records for the study population, no selection criteria for time applied
  so_date_miss<-list() #number of record with missing event_date
  years_so<-list()
  years_so_filter<-list()
  so_not_vocabularies<-list() #number of records where event_vocabulary not of interest
  so_code_vocabulary_miss<-list() #number of records with both event code and event record vocabulary missing
  so_code_pres_voc_miss<-list() #number of records with missing vocabularies
  so_sex_not_specified<-list()
  ######################
  so_out_st_per<-list() #number of SURVEY_OBSERVATIONS records outside the observation period(check is done on individual level)
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
  empty_so_code.my<-list()#number of records with empty event code in the study population by meaning and year
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
    df<-fread(paste(path_dir, actual_tables$SURVEY_OBSERVATIONS[y], sep=""), stringsAsFactors = FALSE, colClasses = "character")
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
    #number of records with event_date missing
    so_date_miss[[w]]<-df[is.na(year),.N]
    #remove records with both dates missing
    df<-df[!is.na(year)]
    #identify persons that have an event one year before start_of_follow_up
    df[,date_dif:=start_follow_up-event_date][,filter:=fifelse(date_dif<=365 & date_dif>=1,1,0)]
    #save the persons who had a bdz x v byctxrB" prior event
    persons_so_prior<-df[filter==1]
    setnames(persons_so_prior,"filter","prior")
    persons_so_prior[,date_dif:=NULL]
    df[,date_dif:=NULL]
    #create variable person_id with event_code in order to remove the same record for the same person for the same event
    if(persons_so_prior[,.N]>0){
      persons_so_prior[,combined_code:=paste(person_id, event_code, sep="_")]
      persons_so_prior<-persons_so_prior[!duplicated(combined_code)]
      persons_so_prior[,combined_code:=NULL]
    }
    #remove records that are outside the obs_period for all subjects
    so_out_st_per[[w]]<-df[event_date<start_follow_up | event_date>end_follow_up,.N] #number of records outside study population
    df[(event_date<start_follow_up | event_date>end_follow_up), obs_out:=1]
    df<-df[is.na(obs_out)] #remove records outside study period
    so_study_pop_obsper[[w]]<-df[is.na(obs_out),.N] #number of records after removing records outside study period
    so_stdpop_no_meaning[[w]]<-df[is.na(meaning) & is.na(obs_out),.N] #number of records with empty meaning
    df<-df[!is.na(meaning)] #remove records with empty meaning
    so_code_vocabulary_miss[[w]]<-df[is.na(event_code) & is.na(event_vocabulary) & is.na(obs_out),.N]#number of records with both event code and vocabulary missing
    df<-df[!is.na(event_code) | !is.na(event_vocabulary)]# remove records with both event code and event record vocabulary missing
    so_code_pres_voc_miss[[w]]<-df[!is.na(event_code) & is.na(event_vocabulary) & is.na(obs_out),.N] #number of records where event code present but vocabulary missing
    df<-df[!is.na(event_vocabulary)] #remove empty vocabularies
    so_not_vocabularies[[w]]<-df[event_vocabulary %!in% vocabularies_list & is.na(obs_out),.N] #number of records where vocabularies doesn't match the codelist
    df<-df[is.na(event_vocabulary) | event_vocabulary %in% vocabularies_list] #remove records where vocabularies are not of interest
    so_sex_not_specified[[w]]<-df[is.na(obs_out)][sex_at_instance_creation =="U" | sex_at_instance_creation == "O",.N] #number of records with unspecified sex
    df<-df[sex_at_instance_creation == "M" | sex_at_instance_creation == "F"]#remove unspecified sex
    #########
    meanings_so[[w]]<-unique(na.omit(df[is.na(obs_out)][, meaning])) #will be used for description
    years_so[[w]]<-unique(na.omit(df[is.na(obs_out)][, year])) #will be used for description
    years_so_filter[[w]]<-unique(na.omit(df[, year])) #will be used for description
    male_population_so[[w]]<-ifelse(df[is.na(obs_out)][sex_at_instance_creation=="M",.N]>0,1,0)
    female_population_so[[w]]<-ifelse(df[is.na(obs_out)][sex_at_instance_creation=="F",.N]>0,1,0)
    females_childbearing_so[[w]]<-ifelse(df[is.na(obs_out)][sex_at_instance_creation=="F" & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,.N]>0,1,0)
    ############################
    #Table 20
    ###########################
    so_study_population[[w]]<-df[is.na(obs_out),.N] #number of records in the study population
    so_study_population_meaning[[w]]<-df[is.na(obs_out),.N, by="meaning"] #number of records in the study population by meaning
    so_study_population_my[[w]]<-df[is.na(obs_out),.N, by=.(meaning,year)] #number of records in the study population by meaning and year
    empty_so_code.my[[w]]<-df[is.na(event_code) & is.na(obs_out), .N, by=.(meaning,year)] #number of records with missing event code when date disp/presc is present
    #############################
    if("no_so_id" %in% names(df)){
      df[,no_so_id:=NULL]
    }
    ##################################################################
    #match codes based on coding system and code: algorithm start with, rcd, snomed
    #################################################################
    if(df[is.na(obs_out),.N]>0){
      years_study_so<-sort(df[!duplicated(year), year])#years present in this table
      pers_included_so<-df[!duplicated(person_id), c("person_id","birth_date","start_follow_up","end_follow_up","sex_at_instance_creation")]
      
      if(pers_included_so[,.N]>0){
        saveRDS(pers_included_so, paste0(so_tmp, paste0("pers_included_so_", actual_tables$SURVEY_OBSERVATIONS[y], ".rds"))) #to give the results we will merge the files by keeping only person_id that are in all saved files
      }
      rm(pers_included_so)
      
      print(paste0("Extracting data for conditions_to_start_with:",actual_tables$SURVEY_OBSERVATIONS[y]))
      if(sum(df[!duplicated(event_vocabulary), event_vocabulary] %in% conditions_to_start_with)>0){
        df[,code_no_dot:=as.character(gsub("\\.","", event_code))]
        for (i in 1:length(conditions_start)){
          for(j in 1:length(conditions_start[[i]])){
            # z<-1
            # repeat{
            #   if(df[grepl(paste0("^",paste(conditions_start[[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_start[[i]])[j]][,.N]>0){
            #     df[grepl(paste0("^",paste(conditions_start[[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_start[[i]])[j],filter:=1]
            #   }
            #   z<-z+1
            #   if(z>length(conditions_start[[i]][[j]])){
            #     break
            #   }
            # }
            
            #remove dots before filtering
            codes_no_dot<-gsub("\\.","", conditions_start[[i]][[j]])
            
            pattern_to_search<-paste(paste0("^",codes_no_dot), collapse = "|")
            df[grepl(pattern_to_search, df[["code_no_dot"]]) & event_vocabulary==names(conditions_start[[i]])[j],filter:=1]
            rm(pattern_to_search,codes_no_dot)
            
            if("filter" %!in% names(df)){df[,filter:=0]}
            m<-1
            repeat{
              if(df[filter==1 & year==years_study_so[m],.N]>0){
                if("code_no_dot" %in% names(df)){
                  df[,code_no_dot:=NULL]
                }
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
      #output to g_intermediate/tmp/SURVEY_OBSERVATIONS datasets splitted by condition, year, type of codes(start with:ICD10,ICD10CM,ICPC,ICD9,ICD9CM)
      
      print(paste0("Extracting data for conditions_rcd:",actual_tables$SURVEY_OBSERVATIONS[y]))
      if(sum(df[!duplicated(event_vocabulary), event_vocabulary] %in% conditions_rcd)>0){
        for (i in 1:length(conditions_read)){
          for(j in 1:length(conditions_read[[i]])){
            # z<-1
            # repeat{
            #   if(df[grepl(paste0("^",paste(conditions_read[[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_read[[i]])[j]][,.N]>0){
            #     df[grepl(paste0("^",paste(conditions_read[[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_read[[i]])[j],filter:=1]
            #   }
            #   z<-z+1
            #   if(z>length(conditions_read[[i]][[j]])){
            #     break
            #   }
            # }
            
            pattern_to_search<-paste(paste0("^",conditions_read[[i]][[j]]), collapse = "|")
            df[grepl(pattern_to_search, df[["event_code"]]) & event_vocabulary==names(conditions_read[[i]])[j],filter:=1]
            rm(pattern_to_search)
            
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
      #output to g_intermediate/tmp/SURVEY_OBSERVATIONS datasets splitted by condition, year, type of codes(start with:Read codes)
      
      print(paste0("Extracting data for conditions_snomed:",actual_tables$SURVEY_OBSERVATIONS[y]))
      if(sum(df[!duplicated(event_vocabulary), event_vocabulary] %in% conditions_snomed_codes)>0){
        for (i in 1:length(conditions_snomed)){
          for(j in 1:length(conditions_snomed[[i]])){
            # z<-1
            # repeat{
            #   if(df[grepl(paste0("^",paste(conditions_snomed[[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_snomed[[i]])[j]][,.N]>0){
            #     df[grepl(paste0("^",paste(conditions_snomed[[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_snomed[[i]])[j],filter:=1]
            #   }
            #   z<-z+1
            #   if(z>length(conditions_snomed[[i]][[j]])){
            #     break
            #   }
            # }
            
            if("filter" %in% names(df)){
              df[,filter:=NULL]
            }
            codes<-data.table(event_vocabulary=names(conditions_snomed[[i]])[j], event_code=conditions_snomed[[i]][[j]], filter=1)
            df<-merge.data.table(df,codes,by=c("event_vocabulary","event_code"),all.x = T,allow.cartesian = T)
            
            
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
      #output to g_intermediate/tmp/SURVEY_OBSERVATIONS datasets splitted by condition, year, type of codes(exact match: SNOMED)
      
      print(paste0("Extracting data for conditions_other:",actual_tables$SURVEY_OBSERVATIONS[y]))
      if(sum(df[!duplicated(event_vocabulary), event_vocabulary] %in% conditions_other_codes)>0){
        for (i in 1:length(conditions_other)){
          for(j in 1:length(conditions_other[[i]])){
            # z<-1
            # repeat{
            #   if(df[grepl(paste0("^",paste(conditions_other[[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_other[[i]])[j]][,.N]>0){
            #     df[grepl(paste0("^",paste(conditions_other[[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_other[[i]])[j],filter:=1]
            #   }
            #   z<-z+1
            #   if(z>length(conditions_other[[i]][[j]])){
            #     break
            #   }
            # }
            
            if("filter" %in% names(df)){
              df[,filter:=NULL]
            }
            codes<-data.table(event_vocabulary=names(conditions_other[[i]])[j], event_code=conditions_other[[i]][[j]], filter=1)
            df<-merge.data.table(df,codes,by=c("event_vocabulary","event_code"),all.x = T,allow.cartesian = T)
            
            if("filter" %!in% names(df)){df[,filter:=0]}
            m<-1
            repeat{
              if(df[filter==1 & year==years_study_so[m],.N]>0){
                saveRDS(data.table(df[filter==1 & year==years_study_so[m]], condition=names(conditions_other[i])), paste0(so_tmp,years_study_so[m],"_", names(conditions_other[i]), "_",actual_tables$SURVEY_OBSERVATIONS[y], "_other.rds"))
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
      #output to g_intermediate/tmp/SURVEY_OBSERVATIONS datasets splitted by condition, year, type of codes(exact match: other)
    }
    
    ##################################################################
    #match codes based on coding system and code: persons with prior so
    #################################################################
    if(persons_so_prior[,.N]>0){
      years_study_prior_so<-sort(persons_so_prior[!duplicated(year), year])#years present in this table
      
      print(paste0("Extracting data for prior events, conditions_to_start_with:",actual_tables$SURVEY_OBSERVATIONS[y]))
      if(sum(persons_so_prior[!duplicated(event_vocabulary), event_vocabulary] %in% conditions_to_start_with)>0){
        persons_so_prior[,code_no_dot:=as.character(gsub("\\.","", event_code))]
        for (i in 1:length(conditions_start)){
          for(j in 1:length(conditions_start[[i]])){
            # z<-1
            # repeat{
            #   if(persons_so_prior[grepl(paste0("^",paste(conditions_start[[i]][[j]][z])), persons_so_prior[["event_code"]]) & event_vocabulary==names(conditions_start[[i]])[j]][,.N]>0){
            #     persons_so_prior[grepl(paste0("^",paste(conditions_start[[i]][[j]][z])), persons_so_prior[["event_code"]]) & event_vocabulary==names(conditions_start[[i]])[j],filter:=1]
            #   }
            #   z<-z+1
            #   if(z>length(conditions_start[[i]][[j]])){
            #     break
            #   }
            # }
            
            #remove dots before filtering
            codes_no_dot<-gsub("\\.","", conditions_start[[i]][[j]])
            
            pattern_to_search<-paste(paste0("^",codes_no_dot), collapse = "|")
            persons_so_prior[grepl(pattern_to_search, persons_so_prior[["code_no_dot"]]) & event_vocabulary==names(conditions_start[[i]])[j],filter:=1]
            rm(pattern_to_search,codes_no_dot)
            
            if("filter" %!in% names(persons_so_prior)){persons_so_prior[,filter:=0]}
            m<-1
            repeat{
              if(persons_so_prior[filter==1 & year==years_study_prior_so[m],.N]>0){
                if("code_no_dot" %in% names(persons_so_prior)){
                  persons_so_prior[,code_no_dot:=NULL]
                }
                saveRDS(data.table(persons_so_prior[filter==1 & year==years_study_prior_so[m],c("person_id","event_date","event_code","prior")], condition=names(conditions_start[i])), paste0(so_tmp,years_study_prior_so[m],"_", names(conditions_start[i]), "_",actual_tables$SURVEY_OBSERVATIONS[y], "_prior_start.rds"))
              }
              m<-m+1
              if(m >length(years_study_prior_so)){
                break
              }
            }
            persons_so_prior[,filter:=NULL]
          }
        }
      }
      #output to g_intermediate/tmp/SURVEY_OBSERVATIONS datasets splitted by condition, year, type of codes(start with:ICD10,ICD10CM,ICPC,ICD9,ICD9CM)
      
      print(paste0("Extracting data for prior events, conditions_rcd:",actual_tables$SURVEY_OBSERVATIONS[y]))
      if(sum(persons_so_prior[!duplicated(event_vocabulary), event_vocabulary] %in% conditions_rcd)>0){
        for (i in 1:length(conditions_read)){
          for(j in 1:length(conditions_read[[i]])){
            # z<-1
            # repeat{
            #   if(persons_so_prior[grepl(paste0("^",paste(conditions_read[[i]][[j]][z])), persons_so_prior[["event_code"]]) & event_vocabulary==names(conditions_read[[i]])[j]][,.N]>0){
            #     persons_so_prior[grepl(paste0("^",paste(conditions_read[[i]][[j]][z])), persons_so_prior[["event_code"]]) & event_vocabulary==names(conditions_read[[i]])[j],filter:=1]
            #   }
            #   z<-z+1
            #   if(z>length(conditions_read[[i]][[j]])){
            #     break
            #   }
            # }
            
            pattern_to_search<-paste(paste0("^",conditions_read[[i]][[j]]), collapse = "|")
            persons_so_prior[grepl(pattern_to_search, persons_so_prior[["event_code"]]) & event_vocabulary==names(conditions_read[[i]])[j],filter:=1]
            rm(pattern_to_search)
            
            if("filter" %!in% names(persons_so_prior)){persons_so_prior[,filter:=0]}
            m<-1
            repeat{
              if(persons_so_prior[filter==1 & year==years_study_prior_so[m],.N]>0){
                saveRDS(data.table(persons_so_prior[filter==1 & year==years_study_prior_so[m],c("person_id","event_date","event_code","prior")], condition=names(conditions_read[i])), paste0(so_tmp,years_study_prior_so[m],"_", names(conditions_read[i]), "_",actual_tables$SURVEY_OBSERVATIONS[y], "_prior_RCD.rds"))
              }
              m<-m+1
              if(m >length(years_study_prior_so)){
                break
              }
            }
            persons_so_prior[,filter:=NULL]
          }
        }
      }
      #output to g_intermediate/tmp/SURVEY_OBSERVATIONS datasets splitted by condition, year, type of codes(start with:Read codes)
      
      print(paste0("Extracting data for prior events, conditions_snomed:",actual_tables$SURVEY_OBSERVATIONS[y]))
      if(sum(persons_so_prior[!duplicated(event_vocabulary), event_vocabulary] %in% conditions_snomed_codes)>0){
        for (i in 1:length(conditions_snomed)){
          for(j in 1:length(conditions_snomed[[i]])){
            # z<-1
            # repeat{
            #   if(persons_so_prior[grepl(paste0("^",paste(conditions_snomed[[i]][[j]][z])), persons_so_prior[["event_code"]]) & event_vocabulary==names(conditions_snomed[[i]])[j]][,.N]>0){
            #     persons_so_prior[grepl(paste0("^",paste(conditions_snomed[[i]][[j]][z])), persons_so_prior[["event_code"]]) & event_vocabulary==names(conditions_snomed[[i]])[j],filter:=1]
            #   }
            #   z<-z+1
            #   if(z>length(conditions_snomed[[i]][[j]])){
            #     break
            #   }
            # }
            
            if("filter" %in% names(persons_so_prior)){
              persons_so_prior[,filter:=NULL]
            }
            codes<-data.table(event_vocabulary=names(conditions_snomed[[i]])[j], event_code=conditions_snomed[[i]][[j]], filter=1)
            persons_so_prior<-merge.data.table(persons_so_prior,codes,by=c("event_vocabulary","event_code"),all.x = T,allow.cartesian = T)
            
            if("filter" %!in% names(persons_so_prior)){persons_so_prior[,filter:=0]}
            m<-1
            repeat{
              if(persons_so_prior[filter==1 & year==years_study_prior_so[m],.N]>0){
                saveRDS(data.table(persons_so_prior[filter==1 & year==years_study_prior_so[m],c("person_id","event_date","event_code","prior")], condition=names(conditions_snomed[i])), paste0(so_tmp,years_study_prior_so[m],"_", names(conditions_snomed[i]), "_",actual_tables$SURVEY_OBSERVATIONS[y], "_prior_SNOMED.rds"))
              }
              m<-m+1
              if(m >length(years_study_prior_so)){
                break
              }
            }
            persons_so_prior[,filter:=NULL]
          }
        }
      }
      #output to g_intermediate/tmp/SURVEY_OBSERVATIONS datasets splitted by condition, year, type of codes(exact match: SNOMED)
    
      print(paste0("Extracting data for prior events, conditions_other:",actual_tables$SURVEY_OBSERVATIONS[y]))
      if(sum(persons_so_prior[!duplicated(event_vocabulary), event_vocabulary] %in% conditions_other_codes)>0){
        for (i in 1:length(conditions_other)){
          for(j in 1:length(conditions_other[[i]])){
            # z<-1
            # repeat{
            #   if(persons_so_prior[grepl(paste0("^",paste(conditions_other[[i]][[j]][z])), persons_so_prior[["event_code"]]) & event_vocabulary==names(conditions_other[[i]])[j]][,.N]>0){
            #     persons_so_prior[grepl(paste0("^",paste(conditions_other[[i]][[j]][z])), persons_so_prior[["event_code"]]) & event_vocabulary==names(conditions_other[[i]])[j],filter:=1]
            #   }
            #   z<-z+1
            #   if(z>length(conditions_other[[i]][[j]])){
            #     break
            #   }
            # }
            
            if("filter" %in% names(persons_so_prior)){
              persons_so_prior[,filter:=NULL]
            }
            codes<-data.table(event_vocabulary=names(conditions_other[[i]])[j], event_code=conditions_other[[i]][[j]], filter=1)
            persons_so_prior<-merge.data.table(persons_so_prior,codes,by=c("event_vocabulary","event_code"),all.x = T,allow.cartesian = T)
            
            if("filter" %!in% names(persons_so_prior)){persons_so_prior[,filter:=0]}
            m<-1
            repeat{
              if(persons_so_prior[filter==1 & year==years_study_prior_so[m],.N]>0){
                saveRDS(data.table(persons_so_prior[filter==1 & year==years_study_prior_so[m],c("person_id","event_date","event_code","prior")], condition=names(conditions_other[i])), paste0(so_tmp,years_study_prior_so[m],"_", names(conditions_other[i]), "_",actual_tables$SURVEY_OBSERVATIONS[y], "_prior_other.rds"))
              }
              m<-m+1
              if(m >length(years_study_prior_so)){
                break
              }
            }
            persons_so_prior[,filter:=NULL]
          }
        }
      }
      #output to g_intermediate/tmp/SURVEY_OBSERVATIONS datasets splitted by condition, year, type of codes(exact match: other)
      }
    
    
    
    w<-w+1
    rm(df)
    rm(persons_so_prior)
  }
  
  ##############################################################################
  #number of subjects in the study population that have not had an event(so)
  ##############################################################################
  
  stdpop_not_so_files<-list.files(so_tmp, pattern = "stdpop_not_so")
  if (length(stdpop_not_so_files)>0){
    so_not_id<-readRDS(paste0(so_tmp, stdpop_not_so_files[1]))
    i<-2
    while(i <= length(stdpop_not_so_files)){
      a<-readRDS(paste0(so_tmp, stdpop_not_so_files[i]))
      so_not_id<-rbind(so_not_id, a)
      so_not_id<-so_not_id[duplicated(person_id)]
      i<-i+1
      rm(a)
    }
    
    for(i in 1:length(stdpop_not_so_files)){
      unlink(paste0(so_tmp,stdpop_not_so_files[i]))
    }
   
     study_population[person_id %in% so_not_id[,person_id], no_so_id:=1]
    
    if (so_not_id[,.N]>0){
      saveRDS(so_not_id, paste0(so_tmp, "so_not_ids_stdpop.rds"))
    }
    stdpop_not_so<-so_not_id[,.N]
    rm(so_not_id)
  } else {
    stdpop_not_so<-0
    study_population[,no_so_id:=1]
  }
  rm(stdpop_not_so_files)
  
  ##############################################################################
  #subjects included in the so_study population
  ##############################################################################
  stdpop_so_files<-list.files(so_tmp, pattern = "pers_included_so_")
  if (length(stdpop_so_files)>0){
    so_included_id<-readRDS(paste0(so_tmp, stdpop_so_files[1]))
    i<-2
    while(i <= length(stdpop_so_files)){
      a<-readRDS(paste0(so_tmp, stdpop_so_files[i]))
      so_included_id<-rbind(so_included_id, a)
      so_included_id<-so_included_id[!duplicated(person_id)]
      i<-i+1
      rm(a)
    }
    
    for(i in 1:length(stdpop_so_files)){
      unlink(paste0(so_tmp,stdpop_so_files[i]))
    }
    
    
    if (so_included_id[,.N]>0){
      saveRDS(so_included_id, paste0(so_tmp, "pers_so.rds"))
    }
    rm(so_included_id)
  }
  rm(stdpop_so_files)
  
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
  #number of records in the study population with event_date inside study period (flowchart 6)
  print("Get number of records for the study population(time criteria applied).")
  so_study_pop_obsper<-do.call(rbind,so_study_pop_obsper) 
  so_study_pop_obsper<-sum(so_study_pop_obsper)
  #number of records in the study population with no meaning (flowchart 7)
  print("Get number of records with no meaning.")
  so_stdpop_no_meaning<-do.call(rbind,so_stdpop_no_meaning) 
  so_stdpop_no_meaning<-sum(so_stdpop_no_meaning) 
  #Number of records with both code and vocabulary variables missing
  print("Get number of records with both code and vocabulary variables missing")
  so_code_vocabulary_miss<-do.call(rbind,so_code_vocabulary_miss)
  so_code_vocabulary_miss<-sum(so_code_vocabulary_miss)
  #Number of records with empty vocabulary when code is present
  print("Get number of records with empty vocabulary when code is present")
  so_code_pres_voc_miss<-do.call(rbind,so_code_pres_voc_miss)
  so_code_pres_voc_miss<-sum(so_code_pres_voc_miss)
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
                                                 so_code_vocabulary_miss,
                                                 so_code_pres_voc_miss,
                                                 so_not_vocabularies,
                                                 so_sex_not_specified,
                                                 so_study_population))
  
  rm(orig_no_rows_so,so_excluded_meanings,so_study_pop,so_date_miss,so_out_st_per,
     so_study_pop_obsper,so_stdpop_no_meaning,so_code_vocabulary_miss,so_code_pres_voc_miss,
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
  years_so<-suppressWarnings(do.call(rbind, years_so))
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
  
  ##############################################################################
  #tab20
  ##############################################################################
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
    tab20_so<-merge(so_study_population_my,empty_so_code.my, by=c("meaning","year"), all=T)
    tab20_so[is.na(no_empty_code),no_empty_code:=0]
  }
  if(tab20_so[is.na(meaning),.N]==1 & tab20_so[,.N]==1){tab20_so<-NULL}
  
  rm(empty_so_code.my)
  
  ######################
  #Combine populations
  #####################
  conditions_codelist<-names(conditions)
  ################################################################################
  #combine all person_id that had a prior condition by condition
  ###############################################################################
  conditions_prior<-c(list.files(so_tmp, "\\_prior_start.rds$"),list.files(so_tmp, "\\_prior_RCD.rds$"),list.files(so_tmp, "\\_prior_SNOMED.rds$"),list.files(so_tmp, "\\_prior_other.rds$"))
  if(length(conditions_prior)>0){
    files_prior<-vector(mode="list", length=length(conditions_codelist))
    names(files_prior)<-conditions_codelist
    for (i in 1:length(files_prior)){
      files_prior[[i]]<-conditions_prior[grepl(conditions_codelist[i], conditions_prior)]
    }
    files_prior<-Filter(length,files_prior) #all files are separated based on year and diagnosis
    
    #############################################################
    #load each element and combine in one by condition, keep only person_id and prior variables
    #############################################################
    for (i in 1:length(files_prior)){
      prior_so<-lapply(paste0(so_tmp,files_prior[[i]]), readRDS)
      prior_so<-do.call(rbind,prior_so)
      #keep only one person id
      prior_so<-prior_so[!duplicated(person_id)]
      prior_so<-prior_so[,c("person_id","prior","condition")]
      
      if(prior_so[,.N]>0){
        saveRDS(prior_so, paste0(so_tmp,names(files_prior)[i],"_so_prior.rds"))
      }
      rm(prior_so)
    }
    rm(files_prior)
    
    for(i in 1:length(conditions_prior)){
      unlink(paste0(so_tmp,conditions_prior[i]))
    }
  }
  rm(conditions_prior)
  
  ############################################################
  #Combine dataset by year and condition
  conditions_files<-c(list.files(so_tmp, "\\_start.rds$"),list.files(so_tmp, "\\_RCD.rds$"),list.files(so_tmp, "\\_SNOMED.rds$"),list.files(so_tmp, "\\_other.rds$"))
  if (length(conditions_files)>0){
    years_so_filter<-Filter(length,years_so_filter)
    years_so_filter<-suppressWarnings(do.call(rbind, years_so_filter))
    years_so_filter<-sort(unique(c(years_so_filter)))
    
    #create combination year_condition from years(years present in the study) and all names of conditions in the codelist
    filter_var<-as.data.table(expand.grid(years_so_filter,conditions_codelist))
    names(filter_var)<-c("year","diagnosis")
    filter_var[, comb:= paste0(year, "_", diagnosis,"_")]
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
    #export the data in populations named by so_year_condition_sex_population 
    #remove all files from tmp
    ############################################################################
    for (i in 1:length(files)){
      combined_diagnosis_so<-lapply(paste0(so_tmp,files[[i]]), readRDS)
      combined_diagnosis_so<-do.call(rbind,combined_diagnosis_so)
      combined_diagnosis_so[,code_nodot:=gsub("\\.","",event_code)]
      combined_diagnosis_so[!(event_vocabulary %in% c(conditions_snomed_codes, conditions_other_codes)),truncated_code:=substr(code_nodot,1,4)]
      combined_diagnosis_so[event_vocabulary %in% c(conditions_snomed_codes),truncated_code:=event_code]
      #remove duplicates between person_id,event_date,truncated_code, vocabulary and meaning
      combined_diagnosis_so[,combined:=paste(person_id,event_date,truncated_code,event_vocabulary,meaning, sep = "_")]
      combined_diagnosis_so<-combined_diagnosis_so[!duplicated(combined)]
      combined_diagnosis_so[,combined:=NULL]
      
      if (subpopulations_present=="Yes"){
        if(combined_diagnosis_so[,.N]>0){
          saveRDS(combined_diagnosis_so, paste0(diag_pop,subpopulations_names[s], "/", names(files)[i],"_so_diagnoses.rds"))
        }
      } else {
        if(combined_diagnosis_so[,.N]>0){
          saveRDS(combined_diagnosis_so, paste0(diag_pop,names(files)[i],"_so_diagnoses.rds"))
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
  study_population[,no_so_id:=1]
}
