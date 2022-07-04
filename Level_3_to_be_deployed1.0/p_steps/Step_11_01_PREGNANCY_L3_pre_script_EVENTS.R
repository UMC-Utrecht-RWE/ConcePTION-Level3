#Author: Vjola Hoxhaj Drs.
#email: v.hoxhaj@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/07/2021

print("Analyse EVENTS table for the PREGNANCY script.")
############################################
#EVENTS
############################################
if(length(actual_tables$EVENTS)>0){
  #####List for saving info####
  print("Creating lists to save the information.")
  ####Flowchart parameters######
  orig_no_rows_events_preg<-list() #original number of records in the EVENTS table
  events_excluded_meanings_preg<-list() #number of recorded with excluded meanings
  #nr_std number of records in the original study population
  events_study_pop_preg<-list() #number of records for the study population, no selection criteria for time applied
  events_date_miss_preg<-list() #number of record with missing pregnancy_code_date
  events_out_st_per_preg<-list() #number of EVENTS records outside the observation period(check is done on individual level)
  events_study_pop_obsper_preg<-list() #number of records in the study population inside study period
  events_stdpop_no_meaning_preg<-list() #number of records in the study population with no meaning
  events_code_vocabulary_miss_preg<-list() #number of records with both event code and event record vocabulary missing
  events_code_pres_voc_miss_preg<-list() #number of records with missing vocabularies
  events_not_vocabularies_preg<-list() #number of records where pregnancy_code_vocabulary not of interest
  empty_pregnancy_code_preg<-list()#number of records with empty event code in the study population 
  
  ####description parameters####
  meanings_events_preg<-list() #all meanings present
  years_events_preg<-list() #all years present
  #pers_stdpop_not_events_preg #number of people in the study population without a pregnancy
  ####Description of the data set parameters####
  events_study_population_preg<-list() #number of records in the study population
  events_study_pop_m_preg<-list() #number of records in the study population by meaning
  events_study_pop_my_preg<-list() #number of records in the study population by meaning and year
  ####Loop to filter data####
  w<-1
  for (y in 1:length(actual_tables$EVENTS)){
    ####Load the table and apply exclusions####
    df<-fread(paste(path_dir, actual_tables$EVENTS[y], sep=""), stringsAsFactors = FALSE, colClasses = "character")
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
    df<-df[!(meaning %in% meanings_exclude_events)]
    #merge with the study_population table(there is no missing data in this table)
    df[,person_id:=as.character(person_id)]
    study_population[,person_id:=as.character(person_id)]
    df<-df[study_population,on=.(person_id)]#left join, keeps all people in the study population even if they didn't have an event
    df<-df[sex_at_instance_creation == "F"]#keep only females
    pers_stdpop_not_events_preg<-df[rowSums(is.na(df[,..colnames_events]))==length(colnames_events), ..std_names_events] #subjects id present in the study population but that do not have an event
    pers_stdpop_not_events_preg<-pers_stdpop_not_events_preg[!duplicated(person_id)]
    df<-df[!rowSums(is.na(df[,..colnames_events]))==length(colnames_events)]
    if(pers_stdpop_not_events_preg[,.N]>0){
      saveRDS(pers_stdpop_not_events_preg, paste0(preg_ev_tmp, paste0("stdpop_not_events_preg_", actual_tables$EVENTS[y], ".rds"))) #to give the results we will merge the files by keeping only person_id that are in all saved files
    }
    rm(pers_stdpop_not_events_preg)
    events_study_pop_preg[[w]]<-df[,.N] #number of records for the study population, no selection criteria for time applied
    #transform into date variables
    df[,pregnancy_code_date:=as.IDate(pregnancy_code_date,"%Y%m%d")] #transform to date variables
    #create year variable
    df[,year:=year(pregnancy_code_date)]
    #number of records with pregnancy_code_date missing
    events_date_miss_preg[[w]]<-df[is.na(year),.N]
    #remove records with date missing
    df<-df[!is.na(year)]
    #remove records that are outside the obs_period for all subjects
    events_out_st_per_preg[[w]]<-df[pregnancy_code_date<start_follow_up_preg | pregnancy_code_date>end_follow_up_preg,.N] #number of records outside study population
    df[(pregnancy_code_date<start_follow_up_preg | pregnancy_code_date>end_follow_up_preg), obs_out:=1]
    df<-df[is.na(obs_out)] #remove records outside study period
    df[,obs_out:=NULL] 
    events_study_pop_obsper_preg[[w]]<-df[,.N] #number of records after removing records outside study period
    events_stdpop_no_meaning_preg[[w]]<-df[is.na(meaning),.N] #number of records with empty meaning
    df<-df[!is.na(meaning)] #remove records with empty meaning
    events_code_vocabulary_miss_preg[[w]]<-df[is.na(pregnancy_code) & is.na(pregnancy_code_vocabulary),.N]#number of records with both event code and vocabulary missing
    df[is.na(pregnancy_code) & is.na(pregnancy_code_vocabulary), remove:=1]#new 02.06.2022
    df<-df[is.na(remove)]# remove records with both event code and event record vocabulary missing #new 02.06.2022
    df[,remove:=NULL]#new 02.06.2022
    events_code_pres_voc_miss_preg[[w]]<-df[!is.na(pregnancy_code) & is.na(pregnancy_code_vocabulary),.N] #number of records where event code present but vocabulary missing
    df[!is.na(pregnancy_code) & is.na(pregnancy_code_vocabulary), remove:=1]#new 02.06.2022
    df<-df[is.na(remove)]#new 02.06.2022
    df[,remove:=NULL]#new 02.06.2022
    #df<-df[!is.na(pregnancy_code_vocabulary)] #remove empty vocabularies
    events_not_vocabularies_preg[[w]]<-df[pregnancy_code_vocabulary %!in% vocabularies_list_pregnancy,.N] #number of records where vocabularies doesn't match the codelist
    df<-df[pregnancy_code_vocabulary %in% vocabularies_list_pregnancy] #remove records where vocabularies are not of interest
    empty_pregnancy_code_preg[[w]]<-df[is.na(pregnancy_code),.N] #number of records with empty codes
    df<-df[!is.na(pregnancy_code)] #remove records with missing event code
    events_study_population_preg[[w]]<-df[,.N] #number of records in the study population(final)
    ####description####
    #meanings_events_preg[[w]]<-unique(na.omit(df[, meaning])) #will be used for description
    meanings_events_preg[[w]]<-df[!duplicated(meaning), "meaning"]
    #years_events_preg[[w]]<-unique(na.omit(df[, year])) #will be used for description
    years_events_preg[[w]]<-df[!duplicated(year), "year"] #will be used for description
    
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
      events_study_pop_m_preg[[w]]<-df[,.N, by="meaning"] #number of records in the study population by meaning
      events_study_pop_my_preg[[w]]<-df[,.N, by=.(meaning,year)] #number of records in the study population by meaning and year
     
     
      years_study_events_preg<-df[!duplicated(year), year]#years present in this table
      ####save all subjects id that had a recorded pregnancy####
      #will be used to add people having another diagnosis when calculating rates recurrent/first event
      pers_included_events<-df[!duplicated(person_id), c("person_id","birth_date","start_follow_up_preg","end_follow_up_preg","sex_at_instance_creation")]
      
      if(pers_included_events[,.N]>0){
        saveRDS(pers_included_events, paste0(preg_ev_tmp, paste0("pers_included_events_preg_", actual_tables$EVENTS[y], ".rds"))) #to give the results we will merge the files by keeping only person_id that are in all saved files
      }
      rm(pers_included_events)
      
      ####match codes based on coding system and code: algorithm start with####
      
      print(paste0("Extracting data for pregnancy_to_start_with:",actual_tables$EVENTS[y]))
      if(sum(df[!duplicated(pregnancy_code_vocabulary), pregnancy_code_vocabulary] %in% pregnancy_to_start_with)>0){
        if("filter" %in% names(df)){df[,filter:=NULL]}#new 01.06.2022
        if("code_no_dot" %in% names(df)){df[,code_no_dot:=NULL]}#new 01.06.2022
        if("condition" %in% names(df)){df[,condition:=NULL]}
        df[,code_no_dot:=as.character(gsub("\\.","", pregnancy_code))]
        for (i in 1:length(stage_pregnancy_start)){
          df[,condition:=names(stage_pregnancy_start)[i]]
          for(j in 1:length(stage_pregnancy_start[[i]])){
            # z<-1
            # repeat{
            #   if(df[grepl(paste0("^",paste(stage_pregnancy_start[[i]][[j]][z])), df[["event_code"]]) & pregnancy_code_vocabulary==names(conditions_start[[i]])[j]][,.N]>0){
            #     df[grepl(paste0("^",paste(stage_pregnancy_start[[i]][[j]][z])), df[["event_code"]]) & pregnancy_code_vocabulary==names(conditions_start[[i]])[j],filter:=T]
            #   }
            #   z<-z+1
            #   if(z>length(stage_pregnancy_start[[i]][[j]])){
            #     break
            #   }
            # }
            
            
            #remove dots before filtering
            codes_no_dot<-unique(gsub("\\.","", stage_pregnancy_start[[i]][[j]]))
            #remove wrong formatted codes
            codes_no_dot<-codes_no_dot[!str_detect(codes_no_dot,"\\+")]
            parent_codes<-unique(substr(codes_no_dot,1,3))
            codes_no_dot<-data.table(code=codes_no_dot)
            parent_codes<-data.table(code=parent_codes, presence=1)
            presence_check<-merge.data.table(codes_no_dot, parent_codes, all.x=T, by="code")
            #remove parent codes that are not present in the list of codes
            parent_codes<-parent_codes[code %in% presence_check[presence==1,code]]
            #remove all children codes for codes where parent code is present
            codes_no_dot[,nchar:=nchar(code)]
            for(code_index in 1:parent_codes[,.N]){
              codes_no_dot[substr(code,1,3)==parent_codes[code_index,code], filter:=T]
              codes_no_dot[filter==T & nchar>=4, remove:=1]
              #remove children code
              codes_no_dot<-codes_no_dot[is.na(remove)]
              codes_no_dot[,filter:=NULL][,remove:=NULL]
            }
            codes_no_dot<-codes_no_dot[,code]
            rm(parent_codes,presence_check)
            pattern_to_search<-paste(paste0("^",codes_no_dot), collapse = "|")
            
            df[,filter:=NA]#new 02.06.2022
            df[grepl(pattern_to_search, df[["code_no_dot"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_start[[i]])[j],filter:=T]
            rm(pattern_to_search,codes_no_dot)
            
            
            # if("filter" %!in% names(df)){df[,filter:=0]}
            # m<-1
            # repeat{
            #   if(df[filter==T & year==years_study_events_preg[m],.N]>0){
            #     if("code_no_dot" %in% names(df)){
            #       df[,code_no_dot:=NULL]
            #     }
            #     saveRDS(data.table(df[filter==T & year==years_study_events_preg[m]], condition=names(stage_pregnancy_start[i])), paste0(preg_ev_tmp,years_study_events_preg[m],"_", names(stage_pregnancy_start[i]), "_",actual_tables$EVENTS[y], "_start.rds"))
            #   }
            #   m<-m+1
            #   if(m >length(years_study_events_preg)){
            #     break
            #   }
            # }
            
            
            if(df[!is.na(filter),.N]>0){
              years_this_event<-sort(df[!is.na(filter)][!duplicated(year),year])
              for(year_ind in 1:length(years_this_event)){
                #col_rm<-c("code_no_dot")
                saveRDS(data.table(df[,c("person_id","pregnancy_code_date","meaning","sex_at_instance_creation","birth_date","start_follow_up_preg","end_follow_up_preg","year","condition","filter")][!is.na(filter) & year==years_this_event[year_ind]], condition=names(stage_pregnancy_start[i])), paste0(preg_ev_tmp,years_this_event[year_ind],"_", names(stage_pregnancy_start[i]), "_",actual_tables$EVENTS[y], "_start.rds"))
              }
            } else{
              years_this_event<-NULL 
            }
            rm(years_this_event)
            
            if("filter" %in% names(df)){df[,filter:=NULL]}
          }
        }
      }
      #output to g_intermediate/tmp/EVENTS datasets split by condition, year, type of codes(start with:ICD10,ICD10CM,ICPC,ICD9,ICD9CM)
      
      ####old to start with####
      # if(sum(df[!duplicated(pregnancy_code_vocabulary), pregnancy_code_vocabulary] %in% pregnancy_to_start_with)>0){
      #   for (i in 1:length(stage_pregnancy_start)){
      #     for(j in 1:length(stage_pregnancy_start[[i]])){
      #       z<-1
      #       repeat{
      #         if(df[grepl(paste0("^",paste(stage_pregnancy_start[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_start[[i]])[j]][,.N]>0){
      #           df[grepl(paste0("^",paste(stage_pregnancy_start[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_start[[i]])[j],filter_preg:=1]
      #         }
      #         z<-z+1
      #         if(z>length(stage_pregnancy_start[[i]][[j]])){
      #           break
      #         }
      #       }
      #       if("filter_preg" %!in% names(df)){df[,filter_preg:=0]}
      #       m<-1
      #       repeat{
      #         if(df[filter_preg==1 & year==years_study_events_preg[m],.N]>0){
      #           saveRDS(data.table(df[filter_preg==1 & year==years_study_events_preg[m]], condition=names(stage_pregnancy_start[i])), paste0(preg_ev_tmp,years_study_events_preg[m],"_", names(stage_pregnancy_start[i]), "_",actual_tables$EVENTS[y], "_start.rds"))
      #         }
      #         m<-m+1
      #         if(m >length(years_study_events_preg)){
      #           break
      #         }
      #       }
      #       df[,filter_preg:=NULL]
      #     }
      #   }
      # }
      # #output to g_intermediate/tmp/EVENTS datasets splitted by condition, year, type of codes(start with:ICD10,ICD10CM,ICPC,ICD9,ICD9CM)
      
      
      # if(sum(df[!duplicated(pregnancy_code_vocabulary), pregnancy_code_vocabulary] %in% pregnancy_rcd)>0){
      #   for (i in 1:length(stage_pregnancy_read)){
      #     for(j in 1:length(stage_pregnancy_read[[i]])){
      #       z<-1
      #       repeat{
      #         if(df[grepl(paste0("^",paste(stage_pregnancy_read[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_read[[i]])[j]][,.N]>0){
      #           df[grepl(paste0("^",paste(stage_pregnancy_read[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_read[[i]])[j],filter_preg:=1]
      #         }
      #         z<-z+1
      #         if(z>length(stage_pregnancy_read[[i]][[j]])){
      #           break
      #         }
      #       }
      #       if("filter_preg" %!in% names(df)){df[,filter_preg:=0]}
      #       m<-1
      #       repeat{
      #         if(df[filter_preg==1 & year==years_study_events_preg[m],.N]>0){
      #           saveRDS(data.table(df[filter_preg==1 & year==years_study_events_preg[m]], condition=names(conditions_read[i])), paste0(preg_ev_tmp,years_study_events_preg[m],"_", names(stage_pregnancy_read[i]), "_",actual_tables$EVENTS[y], "_RCD.rds"))
      #         }
      #         m<-m+1
      #         if(m >length(years_study_events_preg)){
      #           break
      #         }
      #       }
      #       df[,filter_preg:=NULL]
      #     }
      #   }
      # }
      # #output to g_intermediate/tmp/EVENTS datasets splitted by condition, year, type of codes(start with:Read codes)
      
      ####match codes based on coding system and code: algorithm (start with:Read codes)####
      print(paste0("Extracting data for pregnancy_rcd:",actual_tables$EVENTS[y]))
      if(sum(df[!duplicated(pregnancy_code_vocabulary), pregnancy_code_vocabulary] %in% pregnancy_rcd)>0){
        if("filter" %in% names(df)){df[,filter:=NULL]}#new 01.06.2022
        if("code_no_dot" %in% names(df)){df[,code_no_dot:=NULL]}#new 01.06.2022
        if("condition" %in% names(df)){df[,condition:=NULL]}
        df[,code_no_dot:=as.character(gsub("\\.","", pregnancy_code))]#new 01.06.2022
        
        for (i in 1:length(stage_pregnancy_read)){
          df[,condition:=names(stage_pregnancy_read)[i]]
          for(j in 1:length(stage_pregnancy_read[[i]])){
            
            pattern_to_search<-paste(paste0("^",stage_pregnancy_read[[i]][[j]]), collapse = "|")
            df[,filter:=NA]#new 01.06.2022
            df[grepl(pattern_to_search, df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_read[[i]])[j],filter:=T]
            rm(pattern_to_search)
            
            # if("filter" %!in% names(df)){df[,filter:=0]}
            #   m<-1
            #   repeat{
            #     if(df[filter==T & year==years_study_events_preg[m],.N]>0){
            #       saveRDS(data.table(df[filter==T & year==years_study_events_preg[m]], condition=names(stage_pregnancy_read[i])), paste0(preg_ev_tmp,years_study_events_preg[m],"_", names(stage_pregnancy_read[i]), "_",actual_tables$EVENTS[y], "_RCD.rds"))
            #     }
            #     m<-m+1
            #     if(m >length(years_study_events_preg)){
            #       break
            #     }
            #   }
            
            if(df[filter==T,.N]>0){
              years_this_event<-sort(df[filter==T][!duplicated(year),year])
              for(year_ind in 1:length(years_this_event)){
                #col_rm<-c("code_no_dot")
                saveRDS(data.table(df[,c("person_id","pregnancy_code_date","meaning","sex_at_instance_creation","birth_date","start_follow_up_preg","end_follow_up_preg","year","condition","filter")][filter==T & year==years_this_event[year_ind]], condition=names(stage_pregnancy_read[i])), paste0(preg_ev_tmp,years_this_event[year_ind],"_", names(stage_pregnancy_read[i]), "_",actual_tables$EVENTS[y], "_RCD.rds"))
              }
            } else {
              years_this_event<-NULL}# new 01.06.2022
            
            rm(years_this_event)
            
            if("filter" %in% names(df)){df[,filter:=NULL]}
          }
        }
      }
      #output to g_intermediate/tmp/EVENTS datasets splitted by condition, year, type of codes(start with:Read codes)
      
      ####match codes based on coding system and code: algorithm exact match SNOMED codes####
      print(paste0("Extracting data for pregnancy_snomed:",actual_tables$EVENTS[y]))
      if(sum(df[!duplicated(pregnancy_code_vocabulary), pregnancy_code_vocabulary] %in% pregnancy_snomed_codes)>0){
        if("filter" %in% names(df)){df[,filter:=NULL]}#new 01.06.2022
        if("code_no_dot" %in% names(df)){df[,code_no_dot:=NULL]}#new 01.06.2022
        if("condition" %in% names(df)){df[,condition:=NULL]}
        for (i in 1:length(stage_pregnancy_snomed)){
          df[,condition:=names(stage_pregnancy_snomed)[i]]
          for(j in 1:length(stage_pregnancy_snomed[[i]])){
            
            
            #  z<-1
            # repeat{
            #   if(df[grepl(paste0("^",paste(stage_pregnancy_snomed[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_snomed[[i]])[j]][,.N]>0){
            #     df[grepl(paste0("^",paste(stage_pregnancy_snomed[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_snomed[[i]])[j],filter_preg:=1]
            #   }
            #   z<-z+1
            #   if(z>length(stage_pregnancy_snomed[[i]][[j]])){
            #     break
            #   }
            # }
            # if("filter_preg" %!in% names(df)){df[,filter_preg:=0]}
            # m<-1
            # repeat{
            #   if(df[filter_preg==1 & year==years_study_events_preg[m],.N]>0){
            #     saveRDS(data.table(df[filter_preg==1 & year==years_study_events_preg[m]], condition=names(stage_pregnancy_snomed[i])), paste0(preg_ev_tmp,years_study_events_preg[m],"_", names(stage_pregnancy_snomed[i]), "_",actual_tables$EVENTS[y], "_SNOMED.rds"))
            #   }
            #   m<-m+1
            #   if(m >length(years_study_events_preg)){
            #     break
            #   }
            # }
            
            
            codes<-data.table(pregnancy_code_vocabulary=names(stage_pregnancy_snomed[[i]])[j], pregnancy_code=stage_pregnancy_snomed[[i]][[j]], filter=1)
            df<-merge.data.table(df,codes,by=c("pregnancy_code_vocabulary","pregnancy_code"),all.x = T,allow.cartesian = T)
            
            if(df[filter==T,.N]>0){
              years_this_event<-sort(df[filter==T][!duplicated(year),year])
              for(year_ind in 1:length(years_this_event)){
                saveRDS(data.table(df[,c("person_id","pregnancy_code_date","meaning","sex_at_instance_creation","birth_date","start_follow_up_preg","end_follow_up_preg","year","condition","filter")][filter==T & year==years_this_event[year_ind]], condition=names(stage_pregnancy_snomed[i])), paste0(preg_ev_tmp,years_this_event[year_ind],"_", names(stage_pregnancy_snomed[i]), "_",actual_tables$EVENTS[y], "_SNOMED.rds"))
              }
            } else {
              years_this_event<-NULL}# new 01.06.2022
            
            rm(years_this_event)
            if("filter" %in% names(df)){df[,filter:=NULL]}
            
          }
        }
        
        #output to g_intermediate/tmp/EVENTS datasets splitted by condition, year, type of codes(exact match: SNOMED)
      }
      
      ####match codes based on coding system and code: algorithm exact match other codes####
      print(paste0("Extracting data for pregnancy_other:",actual_tables$EVENTS[y]))
      if(sum(df[!duplicated(pregnancy_code_vocabulary), pregnancy_code_vocabulary] %in% pregnancy_other_codes)>0){
        if("filter" %in% names(df)){df[,filter:=NULL]}#new 01.06.2022
        if("code_no_dot" %in% names(df)){df[,code_no_dot:=NULL]}#new 01.06.2022
        if("condition" %in% names(df)){df[,condition:=NULL]}
        for (i in 1:length(stage_pregnancy_other)){
          df[,condition:=names(stage_pregnancy_other)[i]]
          for(j in 1:length(stage_pregnancy_other[[i]])){
            
            codes<-data.table(pregnancy_code_vocabulary=names(stage_pregnancy_other[[i]])[j], pregnancy_code=stage_pregnancy_other[[i]][[j]], filter=1)
            df<-merge.data.table(df,codes,by=c("pregnancy_code_vocabulary","pregnancy_code"),all.x = T,allow.cartesian = T)
            
            # z<-1
            # repeat{
            #   if(df[grepl(paste0("^",paste(stage_pregnancy_other[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_other[[i]])[j]][,.N]>0){
            #     df[grepl(paste0("^",paste(stage_pregnancy_other[[i]][[j]][z])), df[["pregnancy_code"]]) & pregnancy_code_vocabulary==names(stage_pregnancy_other[[i]])[j],filter:=T]
            #   }
            #   z<-z+1
            #   if(z>length(stage_pregnancy_other[[i]][[j]])){
            #     break
            #   }
            # }
            # if("filter" %!in% names(df)){df[,filter:=0]}
            # m<-1
            # repeat{
            #   if(df[filter==T & year==years_study_events[m],.N]>0){
            #     saveRDS(data.table(df[filter==T & year==years_study_events[m]], condition=names(stage_pregnancy_other[i])), paste0(events_tmp,years_study_events[m],"_", names(stage_pregnancy_other[i]), "_",actual_tables$EVENTS[y], "_other.rds"))
            #   }
            #   m<-m+1
            #   if(m >length(years_study_events)){
            #     break
            #   }
            # }
            
            if(df[filter==T,.N]>0){
              years_this_event<-sort(df[filter==T][!duplicated(year),year])
              for(year_ind in 1:length(years_this_event)){
                saveRDS(data.table(df[,c("person_id","pregnancy_code_date","meaning","sex_at_instance_creation","birth_date","start_follow_up_preg","end_follow_up_preg","year","condition","filter")][filter==T & year==years_this_event[year_ind]], condition=names(stage_pregnancy_other[i])), paste0(preg_ev_tmp,years_this_event[year_ind],"_", names(stage_pregnancy_other[i]), "_",actual_tables$EVENTS[y], "_other.rds"))
              }
            } else {
              years_this_event<-NULL}# new 01.06.2022
            
            rm(years_this_event)
            if("filter" %in% names(df)){df[,filter:=NULL]}
          }
        }
      }
      
    }
    ####repeat loop####
    w<-w+1
    rm(df)
  }
  
  ####number of subjects in the study population that have not had an event####
  stdpop_not_events_files_preg<-list.files(preg_ev_tmp, pattern = "stdpop_not_events_preg_")
  if (length(stdpop_not_events_files_preg)>0){
    events_not_id<-readRDS(paste0(preg_ev_tmp, stdpop_not_events_files_preg[1]))
    i<-2
    while(i <= length(stdpop_not_events_files_preg)){
      a<-readRDS(paste0(preg_ev_tmp, stdpop_not_events_files_preg[i]))
      events_not_id<-rbind(events_not_id, a)
      events_not_id<-events_not_id[duplicated(person_id)]
      i<-i+1
      rm(a)
    }
    
    for(i in 1:length(stdpop_not_events_files_preg)){
      unlink(paste0(preg_ev_tmp,stdpop_not_events_files_preg[i]))
    }
    
    study_population[person_id %in% events_not_id[,person_id], no_event_id:=1]
    
    if (events_not_id[,.N]>0){
      saveRDS(events_not_id, paste0(preg_ev_tmp, "events_not_ids_stdpop_preg.rds"))
    }
    stdpop_not_events_preg<-events_not_id[,.N]
    rm(events_not_id)
  } else {
    stdpop_not_events_preg<-0
  }
  rm(stdpop_not_events_files_preg)

  ####subjects included in the events_study population####
  stdpop_events_files_preg<-list.files(preg_ev_tmp, pattern = "pers_included_events_preg_")
  if (length(stdpop_events_files_preg)>0){
    events_included_id<-readRDS(paste0(preg_ev_tmp, stdpop_events_files_preg[1]))
    i<-2
    while(i <= length(stdpop_events_files_preg)){
      a<-readRDS(paste0(preg_ev_tmp, stdpop_events_files_preg[i]))
      events_included_id<-rbind(events_included_id, a)
      events_included_id<-events_included_id[!duplicated(person_id)]
      i<-i+1
      rm(a)
    }
    
    for(i in 1:length(stdpop_events_files_preg)){
      unlink(paste0(preg_ev_tmp,stdpop_events_files_preg[i]))
    }
    
    
    if (events_included_id[,.N]>0){
      saveRDS(events_included_id, paste0(preg_ev_tmp, "pers_events_preg.rds"))
    }
    rm(events_included_id)
  }
  rm(stdpop_events_files_preg)
  ####Flowchart####
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
  # events_excluded_males<-do.call(rbind,events_excluded_males)
  # events_excluded_males<-sum(events_excluded_males)
  #females outside age excluded(flowchart 5)
  # females_outside_age_events<-do.call(rbind,females_outside_age_events)
  # females_outside_age_events<-sum(females_outside_age_events)
  # #sex not specified(flowchart 6)
  # events_sex_not_specified_preg<-do.call(rbind, events_sex_not_specified_preg)
  # events_sex_not_specified_preg<-sum(events_sex_not_specified_preg)
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
  
  
  rm(orig_no_rows_events_preg, events_excluded_meanings_preg,
     events_study_pop_preg,events_date_miss_preg,events_out_st_per_preg,
     events_study_pop_obsper_preg,events_stdpop_no_meaning_preg,events_code_vocabulary_miss_preg,events_code_pres_voc_miss_preg,
     events_not_vocabularies_preg,empty_pregnancy_code_preg)  
  
  ####Description####
  print("Creating description of study population.")
  #meanings
  print("Get list of meanings.")
  meanings_events_preg<-Filter(length,meanings_events_preg)
  meanings_events_preg<-suppressWarnings(do.call(rbind,meanings_events_preg))
  meanings_events_preg<-meanings_events_preg[!duplicated(meaning),meaning]
  meanings_events_preg_des<-paste(meanings_events_preg, collapse = ", ")
  #study years
  years_events_preg<-Filter(length,years_events_preg)
  years_events_preg<-suppressWarnings(do.call(rbind, years_events_preg))
  years_events_preg<-sort(years_events_preg[!duplicated(year),year])
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
  study_population[,no_event_id:=1]
}





