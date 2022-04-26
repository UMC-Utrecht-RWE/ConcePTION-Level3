`%!in%` = Negate(`%in%`)
#Get EVENTS tables
actual_tables<-list()
actual_tables$EVENTS<-list.files(path_dir, pattern="^EVENTS")
start_study_date<-as.Date("19950101", "%Y%m%d")

#Get cdm_source file name
cdm_source_file<-list.files(path_dir, pattern="^CDM_SOURCE")
#Get DAP info and date createion fro CDM_SOURCE
CDM_SOURCE<-fread(paste0(path_dir, cdm_source_file))
date_creation<-CDM_SOURCE[["date_creation"]]
data_access_provider_name<-CDM_SOURCE[["data_access_provider_name"]]
data_source_name<-CDM_SOURCE[["data_source_name"]]
rm(CDM_SOURCE)

#METADATA
#Get metadata file name
metadata_file<-list.files(path_dir, pattern="^METADATA")
#Retrieve data from METADATA
METADATA<-fread(paste0(path_dir, metadata_file))
#Check if datasource has subpopulations
METADATA_subp<-METADATA[type_of_metadata %!in% c("presence_of_table", "presence_of_column", "list_of_values")]
rm(METADATA, metadata_file)
subpopulations_present<-ifelse(METADATA_subp[type_of_metadata=="subpopulations",.N]>0, "Yes", "No")
if(subpopulations_present=="Yes"){
subpopulations_names<-unlist(str_split(METADATA_subp[type_of_metadata=="subpopulations",values], pattern = " "))
}

populations_dir<-paste0(g_intermediate,"populations/")

#load study_population
study_population_dir<-list.files(paste0(g_intermediate,"populations/"), pattern="study_population")
study_population<-readRDS(paste0(g_intermediate, "populations/", study_population_dir))
nr_std<-study_population[,.N]
s<-1
#meanings to be excluded
if(subpopulations_present=="Yes"){
meanings_exclude<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="EVENTS" & other==subpopulations_names[s],values], pattern = " "))
} else {meanings_exclude<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="EVENTS",values], pattern = " "))
}

#EVENTS_tmp/EVENTS folder where all intermediary files are saved
if ("EVENTS" %in% list.files(tmp)){
  events_tmp<-paste(tmp, "EVENTS/", sep="")
  do.call(file.remove, list(list.files(events_tmp, full.names = T)))
}else{
  #Create the EVENTS folder in the output dir
  dir.create(paste(tmp, "EVENTS", sep=""))
  events_tmp<-paste(tmp, "EVENTS/", sep="")
}

#output folder for EVENTS report in g_output
if ("EVENTS" %in% list.files(output_dir)){
  events_dir<-paste(output_dir, "EVENTS/",sep="")
  do.call(file.remove, list(list.files(events_dir, full.names = T)))
  events_less<-paste(events_dir, "Masked/", sep="")
  do.call(file.remove, list(list.files(events_less, full.names = T)))
} else {
  #Create the EVENTS folder in the output dir
  dir.create(paste(output_dir, "EVENTS", sep=""))
  events_dir<-paste(output_dir, "EVENTS/", sep="")
  dir.create(paste(events_dir,"Masked", sep=""))
  events_less<-paste(events_dir, "Masked/", sep="")
}

#output folder to save EVENTS_study_population by sex, atc level 1 and year
if ("EVENTS" %in% list.files(populations_dir)){
  events_pop<-paste(populations_dir, "EVENTS/", sep="")
  do.call(file.remove, list(list.files(events_pop, full.names = T)))
} else {
  #Create the EVENTS folder in the output dir
  dir.create(paste(populations_dir, "EVENTS", sep=""))
  events_pop<-paste(populations_dir, "EVENTS/", sep="")
}


if (length(actual_tables$EVENTS)==0){
  print("There is no EVENTS table present in the working directory.")
} else {
  ###################################################
  #List for saving info
  ###################################################
  print("Creating lists to save the information.")
  orig_no_rows<-list() #original number of records in the EVENTS table
  #######################
  #pers_stdpop_not_vx
  events_study_pop<-list() #number of records for the study population, no selection criteria for time applied
  events_date_miss<-list() #number of record with missing start_date_record
  years<-list()
  ######################
  events_out_st_per<-list() #number of EVENTS records outside the observation period(check is done on individual level)
  events_study_pop_obsper<-list() #number of records in the study population with  inside study period
  ######################
  events_stdpop_no_meaning<-list() #number of records in the study population with no meaning
  events_excluded_meanings<-list() #number of recorded with excluded meanings
  meanings<-list() #all meanings present
  #############################################################################
  #Table 20: Missingness of event codes
  #############################################################################
  events_study_population<-list() #number of records in the study population
  events_study_population_meaning<-list() #number of records in the study population by meaning
  events_study_population_my<-list() #number of records in the study population by meaning and year
  empty_event_code.my<-list()#number of records with empty event code in the study population by meaning and year
  ##############################################################################
  male_population<-list() #save whether males are included
  female_population<-list() #save whether females are included
  ##############################
  events_of_interest<-list()
  ##############################
  events_study_population_meaning_f<-list() #number of records in females [12-55] years old by meaning
  events_study_population_f<-list() #number of records in females [12-55] years old
  ##############################
  females_childbearing<-list() #check if females of childbearing age are available
  ###############################
  #Table 12
  ##############################
  w<-1
  ###############################################
  #for_loop
  ##############################################
 
  
  for (y in 1:length(actual_tables$EVENTS)){
    #Load the table
    df<-fread(paste(path_dir, actual_tables$EVENTS[y], sep=""), stringsAsFactors = FALSE)
    df<-df[,c("person_id", "start_date_record", "event_code", "event_record_vocabulary", "meaning_of_event")]
    df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately
    
    colnames<-names(df)
    std_names<-names(study_population)
    colnames<-colnames[!colnames %in% "person_id"]
    #get the total number of records(a)
    orig_no_rows[[w]]<-df[,.N]
    #get the total number of records with excluded meanings
    events_excluded_meanings[[w]]<-df[meaning_of_event %in% meanings_exclude,.N]
    #remove all records for which the meaning is in excluded meanings
    df<-df[meaning_of_event %!in% meanings_exclude]
    
    #merge with the study_population table(there is no missing data in this table)
    df<-df[study_population,on=.(person_id)]#left join, keeps all people in the study population even if they didn't have an event
    df<-df[,age_fup:=as.numeric(age_fup)]
    pers_stdpop_not_events<-df[rowSums(is.na(df[,..colnames]))==length(colnames), ..std_names] #subjects id present in the study population but that do not have an event
    pers_stdpop_not_events<-pers_stdpop_not_events[!duplicated(person_id)]
    df<-df[!rowSums(is.na(df[,..colnames]))==length(colnames)]
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
    events_stdpop_no_meaning[[w]]<-df[is.na(meaning_of_event),.N] #number of records with empty meaning
    df<-df[!is.na(meaning_of_event)] #remove records with empty meaning
    #########
    if(df[,.N]>0){
    meanings[[w]]<-unique(df[["meaning_of_event"]])
    years[[w]]<-df[!duplicated(year),"year"]
    ############################
    #Table 20
    ###########################
    events_study_population[[w]]<-df[,.N] #number of records in the study population
    events_study_population_meaning[[w]]<-df[,.N, by="meaning_of_event"] #number of records in the study population by meaning
    events_study_population_my[[w]]<-df[,.N, by=.(meaning_of_event,year)] #number of records in the study population by meaning
    empty_event_code.my[[w]]<-df[is.na(event_code), .N, by=.(meaning_of_event,year)] #number of records with missing atc code when date disp/presc is present
    ##################################################################
    #match codes based on coding system and code: algorithm start with
    #################################################################
    years_study<-unique(na.omit(df[["year"]]))
    
    if(sum(unique(na.omit(df[["event_record_vocabulary"]])) %in% c("ICD10", "ICD10CM","ICD9","ICD9CM","ICPC"))>0){
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
            if(df[filter==1 & year==years_study[m],.N]>0){
              saveRDS(data.table(df[filter==1 & year==years_study[m]], condition=names(conditions_start[i])), paste0(events_tmp,years_study[m],"_", names(conditions_start[i]), "_",actual_tables$EVENTS[y], "_start.rds"))
            }
            m<-m+1
            if(m >length(years_study)){
              break
            }
          }
          df[,filter:=NULL]
        }
      }
    }
    #output to g_intermediate/tmp/EVENTS datasets splitted by condition, year, type of codes(start with:ICD10,ICD10CM,ICPC,ICD9,ICD9CM)
  
    if(sum(unique(na.omit(df[["event_record_vocabulary"]])) %in% c("RCD2"))>0){
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
            if(df[filter==1 & year==years_study[m],.N]>0){
              saveRDS(data.table(df[filter==1 & year==years_study[m]], condition=names(conditions_read[i])), paste0(events_tmp,years_study[m],"_", names(conditions_read[i]), "_",actual_tables$EVENTS[y], "_RCD.rds"))
            }
            m<-m+1
            if(m >length(years_study)){
              break
            }
          }
          df[,filter:=NULL]
        }
      }
    }
    
    #output to g_intermediate/tmp/EVENTS datasets splitted by condition, year, type of codes(start with:RCD2)
    ##################################################################
    #match codes based on coding system and code: algorithm exact match
    #################################################################
    if(sum(unique(na.omit(df[["event_record_vocabulary"]])) %in% c("SNOMEDCT_US"))>0){
      for (i in 1:length(conditions_snomed)){
        for(j in 1:length(conditions_snomed[[i]])){
          z<-1
          repeat{
            if(df[grepl(paste0("^",paste(conditions_snomed[[i]][[j]][z])), df[["event_code"]], fixed = T) & event_record_vocabulary==names(conditions_snomed[[i]])[j]][,.N]>0){
              df[grepl(paste0("^",paste(conditions_snomed[[i]][[j]][z])), df[["event_code"]],fixed = T) & event_record_vocabulary==names(conditions_snomed[[i]])[j],filter:=1]
            }
            z<-z+1
            if(z>length(conditions_snomed[[i]][[j]])){
              break
            }
          }
          if("filter" %!in% names(df)){df[,filter:=0]}
          m<-1
          repeat{
            if(df[filter==1 & year==years_study[m],.N]>0){
              saveRDS(data.table(df[filter==1 & year==years_study[m]], condition=names(conditions_snomed[i])), paste0(events_tmp,years_study[m],"_", names(conditions_snomed[i]), "_",actual_tables$EVENTS[y], "_SNOMED.rds"))
            }
            m<-m+1
            if(m >length(years_study)){
              break
            }
          }
          df[,filter:=NULL]
        }
      }
    }
    }
    ########
    w<-w+1
    rm(df)
    ########
  }
  years<-unlist(unique(c(do.call(rbind,years))))
  conditions_codelist<-names(conditions)
  
  ############################################################
  #Combine dataset by year and condition
  conditions_files<-c(list.files(events_tmp, "\\_start.rds$"),list.files(events_tmp, "\\_RCD.rds$"),list.files(events_tmp, "\\_SNOMED.rds$"))
  #create combination year_condition from years(years present in the study) and all names of conditions in the codelist
  filter_var<-as.data.table(expand.grid(years,conditions_codelist))
  names(filter_var)<-c("year","condition")
  filter_var[, comb:= paste0(year, "_", condition)]
  #Create list by conditions and years
  files<-vector(mode="list", length(unique(filter_var[["comb"]])))
  names(files)<-unique(filter_var[["comb"]])
  for (i in 1:length(files)){
    files[[i]]<-conditions_files[grepl(names(files)[i], conditions_files)]
  }
  files<-compact(files)#all files are separated based on year and condition
  
  ############################################################################
  #Load each list element by combining all files inside one element
  #perform the neccessary counts
  #export the data in populations named by events_year_condition_sex_population where sex==female
  #remove all files from tmp
  ############################################################################
  counts<-vector(mode="list", length(files))
  total<-vector(mode="list", length(files))
  for (i in 1:length(files)){
    combined_condition<-lapply(paste0(events_tmp,files[[i]]), readRDS)
    combined_condition<-do.call(rbind,combined_condition)
    combined_condition[,truncated_code:=substr(event_code,1,5)]
    counts[[i]]<-combined_condition[,.N,by=.(condition,event_record_vocabulary,meaning_of_event,truncated_code)]
    total[[i]]<-combined_condition[,.N,by=.(condition)]
  }
  rm(files)
  for(i in 1:length(conditions_files)){
    unlink(paste0(events_tmp,conditions_files[i]))
  }
  
  ######################################################
  #Tab: 21
  ######################################################
  counts<-do.call(rbind,counts)
  names(counts)<-c("UMLS_concept","vocabulary","meaning","code_truncated","count")
  counts<-counts[,lapply(.SD,sum), by=.(UMLS_concept,vocabulary,meaning,code_truncated),.SDcols="count"]
  total<-do.call(rbind,total)
  names(total)<-c("UMLS_concept","total")
  total<-total[,lapply(.SD,sum), by=.(UMLS_concept),.SDcols="total"]
  count<-merge(counts,total,by="UMLS_concept")
  
  
  }
  


