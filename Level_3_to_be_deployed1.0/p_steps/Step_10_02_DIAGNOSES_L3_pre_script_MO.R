print("Analyse MEDICAL OBSERVATIONS table.")
############################################
#MEDICAL_OBSERVATIONS
###########################################
if(length(actual_tables$MEDICAL_OBSERVATIONS)>0){
  ###################################################
  #List for saving info
  ###################################################
  print("Creating lists to save the information.")
  orig_no_rows_mo<-list() #original number of records in the MEDICAL_OBSERVATIONS table
  #######################
  #pers_stdpop_not_mo
  mo_study_pop<-list() #number of records for the study population, no selection criteria for time applied
  event_date_miss<-list() #number of record with missing event_date
  years_mo<-list()
  years_mo_filter<-list()
  mo_not_vocabularies<-list() #number of records where event_vocabulary not of interest
  mo_code_vocabulary_miss<-list() #number of records with both mo code and mo record vocabulary missing
  mo_code_pres_voc_miss<-list() #number of records with missing vocabularies
  mo_sex_not_specified<-list()
  ######################
  mo_out_st_per<-list() #number of MEDICAL_OBSERVATIONS records outside the observation period(check is done on individual level)
  mo_study_pop_obsper<-list() #number of records in the study population inside study period
  ######################
  mo_stdpop_no_meaning<-list() #number of records in the study population with no meaning
  mo_excluded_meanings<-list() #number of recorded with excluded meanings
  meanings_mo<-list() #all meanings present
  #############################################################################
  #Table 20: Missingness of mo codes
  #############################################################################
  mo_study_population<-list() #number of records in the study population
  mo_study_population_meaning<-list() #number of records in the study population by meaning
  mo_study_population_my<-list() #number of records in the study population by meaning and year
  empty_mo_code.my<-list()#number of records with empty mo code in the study population by meaning and year
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
    df<-fread(paste(path_dir, actual_tables$MEDICAL_OBSERVATIONS[y], sep=""), stringsAsFactors = FALSE, colClasses = "character")
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
    df<-df[study_population,on=.(person_id)]#left join, keeps all people in the study population even if they didn't have an mo
    df<-df[,age_start_follow_up:=as.numeric(age_start_follow_up)]
    pers_stdpop_not_mo<-df[rowSums(is.na(df[,..colnames_mo]))==length(colnames_mo), ..std_names_mo] #subjects id present in the study population but that do not have an mo
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
    #number of records with event_date missing
    event_date_miss[[w]]<-df[is.na(year),.N]
    #remove records with both dates missing
    df<-df[!is.na(year)]
    #identify persons that have an mo one year before start_of_follow_up
    df[,date_dif:=start_follow_up-event_date][,filter:=fifelse(date_dif<=365 & date_dif>=1,1,0)]
    #save the persons who had a prior mo
    persons_mo_prior<-df[filter==1]
    setnames(persons_mo_prior,"filter","prior")
    persons_mo_prior[,date_dif:=NULL]
    df[,date_dif:=NULL]
    #create variable person_id with event_code in order to remove the same record for the same person for the same event
    if(persons_mo_prior[,.N]>0){
      persons_mo_prior[,combined_code:=paste(person_id, event_code, sep="_")]
      persons_mo_prior<-persons_mo_prior[!duplicated(combined_code)]
      persons_mo_prior[,combined_code:=NULL]
    }
    #remove records that are outside the obs_period for all subjects
    mo_out_st_per[[w]]<-df[event_date<start_follow_up | event_date>end_follow_up,.N] #number of records outside study population
    df[(event_date<start_follow_up | event_date>end_follow_up), obs_out:=1]
    df<-df[is.na(obs_out)] #remove records outside study period
    mo_study_pop_obsper[[w]]<-df[is.na(obs_out),.N] #number of records after removing records outside study period
    mo_stdpop_no_meaning[[w]]<-df[is.na(meaning) & is.na(obs_out),.N] #number of records with empty meaning
    df<-df[!is.na(meaning)] #remove records with empty meaning
    mo_code_vocabulary_miss[[w]]<-df[is.na(event_code) & is.na(event_vocabulary) & is.na(obs_out),.N]#number of records with both mo code and vocabulary missing
    df<-df[!is.na(event_code) | !is.na(event_vocabulary)]# remove records with both mo code and mo record vocabulary missing
    mo_code_pres_voc_miss[[w]]<-df[!is.na(event_code) & is.na(event_vocabulary) & is.na(obs_out),.N] #number of records where mo code present but vocabulary missing
    df<-df[!is.na(event_vocabulary)] #remove empty vocabularies
    mo_not_vocabularies[[w]]<-df[event_vocabulary %!in% vocabularies_list & is.na(obs_out),.N] #number of records where vocabularies doesn't match the codelist
    df<-df[is.na(event_vocabulary) | event_vocabulary %in% vocabularies_list] #remove records where vocabularies are not of interest
    mo_sex_not_specified[[w]]<-df[is.na(obs_out)][sex_at_instance_creation =="U" | sex_at_instance_creation == "O",.N] #number of records with unspecified sex
    df<-df[sex_at_instance_creation == "M" | sex_at_instance_creation == "F"]#remove unspecified sex
    #########
    meanings_mo[[w]]<-unique(na.omit(df[is.na(obs_out)][, meaning])) #will be used for description
    years_mo[[w]]<-unique(na.omit(df[is.na(obs_out)][, year])) #will be used for description
    years_mo_filter[[w]]<-unique(na.omit(df[, year])) #will be used for description
    male_population_mo[[w]]<-ifelse(df[is.na(obs_out)][sex_at_instance_creation=="M",.N]>0,1,0)
    female_population_mo[[w]]<-ifelse(df[is.na(obs_out)][sex_at_instance_creation=="F",.N]>0,1,0)
    females_childbearing_mo[[w]]<-ifelse(df[is.na(obs_out)][sex_at_instance_creation=="F" & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,.N]>0,1,0)
    ############################
    #Table 20
    ###########################
    mo_study_population[[w]]<-df[is.na(obs_out),.N] #number of records in the study population
    mo_study_population_meaning[[w]]<-df[is.na(obs_out),.N, by="meaning"] #number of records in the study population by meaning
    mo_study_population_my[[w]]<-df[is.na(obs_out),.N, by=.(meaning,year)] #number of records in the study population by meaning and year
    empty_mo_code.my[[w]]<-df[is.na(event_code) & is.na(obs_out), .N, by=.(meaning,year)] #number of records with missing mo code when date disp/presc is present
    #############################
   if("no_mo_id" %in% names(df)){
     df[,no_mo_id:=NULL]
   }
    ##################################################################
    #match codes based on coding system and code: algorithm start with, rcd, snomed
    #################################################################
    if(df[is.na(obs_out),.N]>0){
      years_study_mo<-sort(df[!duplicated(year), year])#years present in this table
      pers_included_mo<-df[!duplicated(person_id), c("person_id","birth_date","start_follow_up","end_follow_up","sex_at_instance_creation")]
      
      if(pers_included_mo[,.N]>0){
        saveRDS(pers_included_mo, paste0(mo_tmp, paste0("pers_included_mo_", actual_tables$MEDICAL_OBSERVATIONS[y], ".rds"))) #to give the results we will merge the files by keeping only person_id that are in all saved files
      }
      rm(pers_included_mo)
      
      print(paste0("Extracting data for conditions_to_start_with:",actual_tables$MEDICAL_OBSERVATIONS[y]))
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
              if(df[filter==1 & year==years_study_mo[m],.N]>0){
                if("code_no_dot" %in% names(df)){
                  df[,code_no_dot:=NULL]
                }
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
      #output to g_intermediate/tmp/MEDICAL_OBSERVATIONS datasets splitted by condition, year, type of codes(start with:ICD10,ICD10CM,ICPC,ICD9,ICD9CM)
      
      print(paste0("Extracting data for conditions_rcd:",actual_tables$MEDICAL_OBSERVATIONS[y]))
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
      #output to g_intermediate/tmp/MEDICAL_OBSERVATIONS datasets splitted by condition, year, type of codes(start with:Read codes)
      
      print(paste0("Extracting data for conditions_snomed:",actual_tables$MEDICAL_OBSERVATIONS[y]))
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
      #output to g_intermediate/tmp/MEDICAL_OBSERVATIONS datasets splitted by condition, year, type of codes(exact match: SNOMED)
      
      print(paste0("Extracting data for conditions_other:",actual_tables$MEDICAL_OBSERVATIONS[y]))
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
              if(df[filter==1 & year==years_study_mo[m],.N]>0){
                saveRDS(data.table(df[filter==1 & year==years_study_mo[m]], condition=names(conditions_other[i])), paste0(mo_tmp,years_study_mo[m],"_", names(conditions_other[i]), "_",actual_tables$MEDICAL_OBSERVATIONS[y], "_other.rds"))
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
      #output to g_intermediate/tmp/MEDICAL_OBSERVATIONS datasets splitted by condition, year, type of codes(exact match: other)
    }
    
    ##################################################################
    #match codes based on coding system and code: persons with prior mo
    #################################################################
    if(persons_mo_prior[,.N]>0){
      years_study_prior_mo<-sort(persons_mo_prior[!duplicated(year), year])#years present in this table
      
      print(paste0("Extracting data for prior events, conditions_to_start_with:",actual_tables$MEDICAL_OBSERVATIONS[y]))
      if(sum(persons_mo_prior[!duplicated(event_vocabulary), event_vocabulary] %in% conditions_to_start_with)>0){
        persons_mo_prior[,code_no_dot:=as.character(gsub("\\.","", event_code))]
        for (i in 1:length(conditions_start)){
          for(j in 1:length(conditions_start[[i]])){
            # z<-1
            # repeat{
            #   if(persons_mo_prior[grepl(paste0("^",paste(conditions_start[[i]][[j]][z])), persons_mo_prior[["event_code"]]) & event_vocabulary==names(conditions_start[[i]])[j]][,.N]>0){
            #     persons_mo_prior[grepl(paste0("^",paste(conditions_start[[i]][[j]][z])), persons_mo_prior[["event_code"]]) & event_vocabulary==names(conditions_start[[i]])[j],filter:=1]
            #   }
            #   z<-z+1
            #   if(z>length(conditions_start[[i]][[j]])){
            #     break
            #   }
            # }
            
            #remove dots before filtering
            codes_no_dot<-gsub("\\.","", conditions_start[[i]][[j]])
            
            pattern_to_search<-paste(paste0("^",codes_no_dot), collapse = "|")
            persons_mo_prior[grepl(pattern_to_search, persons_mo_prior[["code_no_dot"]]) & event_vocabulary==names(conditions_start[[i]])[j],filter:=1]
            rm(pattern_to_search,codes_no_dot)
            
            if("filter" %!in% names(persons_mo_prior)){persons_mo_prior[,filter:=0]}
            m<-1
            repeat{
              if(persons_mo_prior[filter==1 & year==years_study_prior_mo[m],.N]>0){
                if("code_no_dot" %in% names(persons_mo_prior)){
                  persons_mo_prior[,code_no_dot:=NULL]
                }
                saveRDS(data.table(persons_mo_prior[filter==1 & year==years_study_prior_mo[m],c("person_id","event_date","event_code","prior")], condition=names(conditions_start[i])), paste0(mo_tmp,years_study_prior_mo[m],"_", names(conditions_start[i]), "_",actual_tables$MEDICAL_OBSERVATIONS[y], "_prior_start.rds"))
              }
              m<-m+1
              if(m >length(years_study_prior_mo)){
                break
              }
            }
            persons_mo_prior[,filter:=NULL]
          }
        }
      }
      #output to g_intermediate/tmp/MEDICAL_OBSERVATIONS datasets splitted by condition, year, type of codes(start with:ICD10,ICD10CM,ICPC,ICD9,ICD9CM)
      
      print(paste0("Extracting data for prior events, conditions_rcd:",actual_tables$MEDICAL_OBSERVATIONS[y]))
      if(sum(persons_mo_prior[!duplicated(event_vocabulary), event_vocabulary] %in% conditions_rcd)>0){
        for (i in 1:length(conditions_read)){
          for(j in 1:length(conditions_read[[i]])){
            # z<-1
            # repeat{
            #   if(persons_mo_prior[grepl(paste0("^",paste(conditions_read[[i]][[j]][z])), persons_mo_prior[["event_code"]]) & event_vocabulary==names(conditions_read[[i]])[j]][,.N]>0){
            #     persons_mo_prior[grepl(paste0("^",paste(conditions_read[[i]][[j]][z])), persons_mo_prior[["event_code"]]) & event_vocabulary==names(conditions_read[[i]])[j],filter:=1]
            #   }
            #   z<-z+1
            #   if(z>length(conditions_read[[i]][[j]])){
            #     break
            #   }
            # }
            
            pattern_to_search<-paste(paste0("^",conditions_read[[i]][[j]]), collapse = "|")
            persons_mo_prior[grepl(pattern_to_search, persons_mo_prior[["event_code"]]) & event_vocabulary==names(conditions_read[[i]])[j],filter:=1]
            rm(pattern_to_search)
            
            if("filter" %!in% names(persons_mo_prior)){persons_mo_prior[,filter:=0]}
            m<-1
            repeat{
              if(persons_mo_prior[filter==1 & year==years_study_prior_mo[m],.N]>0){
                saveRDS(data.table(persons_mo_prior[filter==1 & year==years_study_prior_mo[m],c("person_id","event_date","event_code","prior")], condition=names(conditions_read[i])), paste0(mo_tmp,years_study_prior_mo[m],"_", names(conditions_read[i]), "_",actual_tables$MEDICAL_OBSERVATIONS[y], "_prior_RCD.rds"))
              }
              m<-m+1
              if(m >length(years_study_prior_mo)){
                break
              }
            }
            persons_mo_prior[,filter:=NULL]
          }
        }
      }
      #output to g_intermediate/tmp/MEDICAL_OBSERVATIONS datasets splitted by condition, year, type of codes(start with:Read codes)
      
      print(paste0("Extracting data for prior events, conditions_snomed:",actual_tables$MEDICAL_OBSERVATIONS[y]))
      if(sum(persons_mo_prior[!duplicated(event_vocabulary), event_vocabulary] %in% conditions_snomed_codes)>0){
        for (i in 1:length(conditions_snomed)){
          for(j in 1:length(conditions_snomed[[i]])){
            # z<-1
            # repeat{
            #   if(persons_mo_prior[grepl(paste0("^",paste(conditions_snomed[[i]][[j]][z])), persons_mo_prior[["event_code"]]) & event_vocabulary==names(conditions_snomed[[i]])[j]][,.N]>0){
            #     persons_mo_prior[grepl(paste0("^",paste(conditions_snomed[[i]][[j]][z])), persons_mo_prior[["event_code"]]) & event_vocabulary==names(conditions_snomed[[i]])[j],filter:=1]
            #   }
            #   z<-z+1
            #   if(z>length(conditions_snomed[[i]][[j]])){
            #     break
            #   }
            # }
            
            if("filter" %in% names(persons_mo_prior)){
              persons_mo_prior[,filter:=NULL]
            }
            codes<-data.table(event_vocabulary=names(conditions_snomed[[i]])[j], event_code=conditions_snomed[[i]][[j]], filter=1)
            persons_mo_prior<-merge.data.table(persons_mo_prior,codes,by=c("event_vocabulary","event_code"),all.x = T,allow.cartesian = T)
            
            if("filter" %!in% names(persons_mo_prior)){persons_mo_prior[,filter:=0]}
            m<-1
            repeat{
              if(persons_mo_prior[filter==1 & year==years_study_prior_mo[m],.N]>0){
                saveRDS(data.table(persons_mo_prior[filter==1 & year==years_study_prior_mo[m],c("person_id","event_date","event_code","prior")], condition=names(conditions_snomed[i])), paste0(mo_tmp,years_study_prior_mo[m],"_", names(conditions_snomed[i]), "_",actual_tables$MEDICAL_OBSERVATIONS[y], "_prior_SNOMED.rds"))
              }
              m<-m+1
              if(m >length(years_study_prior_mo)){
                break
              }
            }
            persons_mo_prior[,filter:=NULL]
          }
        }
      }
      #output to g_intermediate/tmp/MEDICAL_OBSERVATIONS datasets splitted by condition, year, type of codes(exact match: SNOMED)
   
      print(paste0("Extracting data for prior events, conditions_other:",actual_tables$MEDICAL_OBSERVATIONS[y]))
      if(sum(persons_mo_prior[!duplicated(event_vocabulary), event_vocabulary] %in% conditions_other_codes)>0){
        for (i in 1:length(conditions_other)){
          for(j in 1:length(conditions_other[[i]])){
            # z<-1
            # repeat{
            #   if(persons_mo_prior[grepl(paste0("^",paste(conditions_other[[i]][[j]][z])), persons_mo_prior[["event_code"]]) & event_vocabulary==names(conditions_other[[i]])[j]][,.N]>0){
            #     persons_mo_prior[grepl(paste0("^",paste(conditions_other[[i]][[j]][z])), persons_mo_prior[["event_code"]]) & event_vocabulary==names(conditions_other[[i]])[j],filter:=1]
            #   }
            #   z<-z+1
            #   if(z>length(conditions_other[[i]][[j]])){
            #     break
            #   }
            # }
            
            if("filter" %in% names(persons_mo_prior)){
              persons_mo_prior[,filter:=NULL]
            }
            codes<-data.table(event_vocabulary=names(conditions_other[[i]])[j], event_code=conditions_other[[i]][[j]], filter=1)
            persons_mo_prior<-merge.data.table(persons_mo_prior,codes,by=c("event_vocabulary","event_code"),all.x = T,allow.cartesian = T)
            
            if("filter" %!in% names(persons_mo_prior)){persons_mo_prior[,filter:=0]}
            m<-1
            repeat{
              if(persons_mo_prior[filter==1 & year==years_study_prior_mo[m],.N]>0){
                saveRDS(data.table(persons_mo_prior[filter==1 & year==years_study_prior_mo[m],c("person_id","event_date","event_code","prior")], condition=names(conditions_other[i])), paste0(mo_tmp,years_study_prior_mo[m],"_", names(conditions_other[i]), "_",actual_tables$MEDICAL_OBSERVATIONS[y], "_prior_other.rds"))
              }
              m<-m+1
              if(m >length(years_study_prior_mo)){
                break
              }
            }
            persons_mo_prior[,filter:=NULL]
          }
        }
      }
      #output to g_intermediate/tmp/MEDICAL_OBSERVATIONS datasets splitted by condition, year, type of codes(exact match: other)
       }
    
    
    
    w<-w+1
    rm(df)
    rm(persons_mo_prior)
  }
  
  
  ##############################################################################
  #number of subjects in the study population that have not had an event(mo)
  ##############################################################################
  
  stdpop_not_mo_files<-list.files(mo_tmp, pattern = "stdpop_not_mo")
  if (length(stdpop_not_mo_files)>0){
    mo_not_id<-readRDS(paste0(mo_tmp, stdpop_not_mo_files[1]))
    i<-2
    while(i <= length(stdpop_not_mo_files)){
      a<-readRDS(paste0(mo_tmp, stdpop_not_mo_files[i]))
      mo_not_id<-rbind(mo_not_id, a)
      mo_not_id<-mo_not_id[duplicated(person_id)]
      i<-i+1
      rm(a)
    }
    
    for(i in 1:length(stdpop_not_mo_files)){
      unlink(paste0(mo_tmp,stdpop_not_mo_files[i]))
    }
    
    study_population[person_id %in% mo_not_id[,person_id], no_mo_id:=1]
    
    if (mo_not_id[,.N]>0){
      saveRDS(mo_not_id, paste0(mo_tmp, "mo_not_ids_stdpop.rds"))
    }
    stdpop_not_mo<-mo_not_id[,.N]
    rm(mo_not_id)
  } else {
    stdpop_not_mo<-0
    study_population[,no_mo_id:=1]
  }
  rm(stdpop_not_mo_files)
  ##############################################################################
  #subjects included in the mo_study population
  ##############################################################################
  stdpop_mo_files<-list.files(mo_tmp, pattern = "pers_included_mo_")
  if (length(stdpop_mo_files)>0){
    mo_included_id<-readRDS(paste0(mo_tmp, stdpop_mo_files[1]))
    i<-2
    while(i <= length(stdpop_mo_files)){
      a<-readRDS(paste0(mo_tmp, stdpop_mo_files[i]))
      mo_included_id<-rbind(mo_included_id, a)
      mo_included_id<-mo_included_id[!duplicated(person_id)]
      i<-i+1
      rm(a)
    }
    
    for(i in 1:length(stdpop_mo_files)){
      unlink(paste0(mo_tmp,stdpop_mo_files[i]))
    }
    
    
    if (mo_included_id[,.N]>0){
      saveRDS(mo_included_id, paste0(mo_tmp, "pers_mo.rds"))
    }
    rm(mo_included_id)
  }
  rm(stdpop_mo_files)
  
  
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
  event_date_miss<-do.call(rbind,event_date_miss)
  event_date_miss<-sum(event_date_miss)
  #number of medicines records outside the observation period(check is done on individual level) (flowchart 5)
  print("Get number of records outside observation period.")
  mo_out_st_per<-do.call(rbind,mo_out_st_per) 
  mo_out_st_per<-sum(mo_out_st_per)
  #number of records in the study population with event_date inside study period (flowchart 6)
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
                                                  event_date_miss,
                                                  mo_out_st_per,
                                                  mo_study_pop_obsper,
                                                  mo_stdpop_no_meaning,
                                                  mo_code_vocabulary_miss,
                                                  mo_code_pres_voc_miss,
                                                  mo_not_vocabularies,
                                                  mo_sex_not_specified,
                                                  mo_study_population))
  
  rm(orig_no_rows_mo,mo_excluded_meanings,mo_study_pop,event_date_miss,mo_out_st_per,
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
  years_mo<-suppressWarnings(do.call(rbind, years_mo))
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
  
  ##############################################################################
  #tab20
  ##############################################################################
  mo_study_population_my<-do.call(rbind,mo_study_population_my)
  setnames(mo_study_population_my,"N","no_records")
  if(mo_study_population_my[,.N]>0){
    mo_study_population_my<-mo_study_population_my[,lapply(.SD,sum),by=c("meaning", "year"), .SDcols="no_records"]
  }
  empty_mo_code.my<-do.call(rbind,empty_mo_code.my)
  setnames(empty_mo_code.my,"N", "no_empty_code")
  if(empty_mo_code.my[,.N]>0){
    empty_mo_code.my<-empty_mo_code.my[,lapply(.SD,sum),by=c("meaning", "year"), .SDcols="no_empty_code"]
  }
  if(empty_mo_code.my[,.N]==0){
    tab20_mo<-data.table(mo_study_population_my, no_empty_code=0)
  } else {
    mo_study_population_my[,meaning:=as.character(meaning)][,year:=as.character(year)]
    empty_mo_code.my[,meaning:=as.character(meaning)][,year:=as.character(year)]
    tab20_mo<-merge(mo_study_population_my,empty_mo_code.my, by=c("meaning","year"), all=T)
    tab20_mo[is.na(no_empty_code),no_empty_code:=0]
  }
  if(tab20_mo[is.na(meaning),.N]==1 & tab20_mo[,.N]==1){tab20_mo<-NULL}
  
  rm(empty_mo_code.my)
  
  ######################
  #Combine populations
  #####################
  conditions_codelist<-names(conditions)
  ################################################################################
  #combine all person_id that had a prior condition by condition
  ###############################################################################
  conditions_prior<-c(list.files(mo_tmp, "\\_prior_start.rds$"),list.files(mo_tmp, "\\_prior_RCD.rds$"),list.files(mo_tmp, "\\_prior_SNOMED.rds$"),list.files(mo_tmp, "\\_prior_other.rds$"))
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
      prior_mo<-lapply(paste0(mo_tmp,files_prior[[i]]), readRDS)
      prior_mo<-do.call(rbind,prior_mo)
      #keep only one person id
      prior_mo<-prior_mo[!duplicated(person_id)]
      prior_mo<-prior_mo[,c("person_id","prior","condition")]
      
      if(prior_mo[,.N]>0){
        saveRDS(prior_mo, paste0(mo_tmp,names(files_prior)[i],"_mo_prior.rds"))
      }
      rm(prior_mo)
    }
    rm(files_prior)
    
    for(i in 1:length(conditions_prior)){
      unlink(paste0(mo_tmp,conditions_prior[i]))
    }
  }
  rm(conditions_prior)
  
  ############################################################
  #Combine dataset by year and condition
  conditions_files<-c(list.files(mo_tmp, "\\_start.rds$"),list.files(mo_tmp, "\\_RCD.rds$"),list.files(mo_tmp, "\\_SNOMED.rds$"),list.files(mo_tmp, "\\_other.rds$"))
  if (length(conditions_files)>0){
    years_mo_filter<-Filter(length,years_mo_filter)
    years_mo_filter<-suppressWarnings(do.call(rbind, years_mo_filter))
    years_mo_filter<-sort(unique(c(years_mo_filter)))
    
    #create combination year_condition from years(years present in the study) and all names of conditions in the codelist
    filter_var<-as.data.table(expand.grid(years_mo_filter,conditions_codelist))
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
    #export the data in populations named by mo_year_condition_sex_population 
    #remove all files from tmp
    ############################################################################
    for (i in 1:length(files)){
      combined_diagnosis_mo<-lapply(paste0(mo_tmp,files[[i]]), readRDS)
      combined_diagnosis_mo<-do.call(rbind,combined_diagnosis_mo)
      combined_diagnosis_mo[,code_nodot:=gsub("\\.","",event_code)]
      combined_diagnosis_mo[!(event_vocabulary %in% c(conditions_snomed_codes, conditions_other_codes)),truncated_code:=substr(code_nodot,1,4)]
      combined_diagnosis_mo[event_vocabulary %in% c(conditions_snomed_codes, conditions_other_codes),truncated_code:=event_code]
      #remove duplicates between person_id,event_date,truncated_code, vocabulary and meaning
      combined_diagnosis_mo[,combined:=paste(person_id,event_date,truncated_code,event_vocabulary,meaning, sep = "_")]
      combined_diagnosis_mo<-combined_diagnosis_mo[!duplicated(combined)]
      combined_diagnosis_mo[,combined:=NULL]
      
      if (subpopulations_present=="Yes"){
        if(combined_diagnosis_mo[,.N]>0){
          saveRDS(combined_diagnosis_mo, paste0(diag_pop,subpopulations_names[s], "/", names(files)[i],"_mo_diagnoses.rds"))
        }
      } else {
        if(combined_diagnosis_mo[,.N]>0){
          saveRDS(combined_diagnosis_mo, paste0(diag_pop,names(files)[i],"_mo_diagnoses.rds"))
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
  
  study_population[,no_mo_id:=1]
}
