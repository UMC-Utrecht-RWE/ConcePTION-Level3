#Author: Vjola Hoxhaj Drs.
#email: v.hoxhaj@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/07/2021

#Changes: 29.10.2010
#Fix bug in calculating number of records(now the script allows for the same medication being prescribed to the same subject in the same date)
#person time expressed in days will now be expressed in years
#from rates of per 1000 pd now is per 100 py
#fix bug in calculating number of users, number of users is shown only for subjects having a prescribed/dispensed medicine
#added number of subjects in the rate tables

################################################
#VACCINES
################################################
if(length(actual_tables$VACCINES)>0){
  ###################################################
  #List for saving info
  ###################################################
  print("Creating lists to save the information.")
  orig_no_rows<-list() #original number of records in the VACCINES table
  vx_excluded_meanings<-list()#number of records with excluded meanings
  #######################
  #pers_stdpop_not_vx
  vx_study_pop<-list() #number of records for the study population, no selection criteria for time applied
  vx_date_miss<-list() #number of record with missing date dispensing/prescription
  years<-list()
  vx_sex_not_specified<-list() #number of records with unspecified sex
  ######################
  vx_out_st_per<-list() #number of VACCINES records outside the observation period(check is done on individual level)
  vx_study_pop_obsper<-list() #number of records in the study population with date dispensing/prescription inside study period
  ######################
  vx_stdpop_no_meaning<-list() #number of records in the study population with no meaning
  meanings<-list() #all meanings present
  #############################################################################
  vx_study_population<-list() #number of records in the study population
  vx_study_population_meaning<-list() #number of records in the study population by meaning
  vx_study_population_meaning_year<-list() #number of records in the study population by meaning and year
  male_population<-list() #save whether males are included
  female_population<-list() #save whether females are included
  #############################################################################
  empty_atc_code<-list() #number of records with empty atc codes when date disp/presc is present
  no_level1_atc<-list() #number of records with atc code up to level 1
  no_level2_atc<-list() #number of records with atc code up to level 2
  no_level3_atc<-list() #number of records with atc code up to level 3
  no_level4_atc<-list() #number of records with atc code up to level 4
  no_level5_atc<-list() #number of records with atc code up to level 5
  no_level6_atc<-list() #number of records with atc code up to level 6
  no_level7_atc<-list() #number of records with atc code up to level 7
  comp_atc<-list() #total number of records with complete atc code
  ##############################
  empty_atc_code_m<-list() #number of records with empty atc code by meaning
  empty_atc_code_m_y<-list() #number of records with empty atc code by meaning and year
  ##############################
  empty_atc_code_m_f<-list() #number of records with empty atc code by meaning in females in childebearing age
  empty_atc_code_m_y_f<-list() #number of records with empty atc code by meaning and year in females in childebearing age
  vx_study_population_meaning_f<-list() #number of records in females [12-55] years old by meaning
  vx_study_population_meaning_year_f<-list() #number of records in females [12-55] years old by meaning and year
  vx_study_population_f<-list() #number of records in females [12-55] years old
  ##############################
  females_childbearing<-list() #check if females of childbearing age are available
  w<-1
  ###############################################
  #for_loop
  ##############################################
  
  for (y in 1:length(actual_tables$VACCINES)){
    #Load the table
    df<-fread(paste(path_dir, actual_tables$VACCINES[y], sep=""), stringsAsFactors = FALSE)
    df<-df[,c("person_id", "vx_atc", "vx_admin_date", "vx_record_date", "meaning_of_vx_record")]
    df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately
    setnames(df, "meaning_of_vx_record", "meaning")
    colnames<-names(df)
    std_names<-names(study_population)
    colnames<-colnames[!colnames %in% "person_id"]
    #get the total number of records(a)
    orig_no_rows[[w]]<-df[,.N]
    #get the total number of records with excluded meanings
    vx_excluded_meanings[[w]]<-df[meaning %in% meanings_exclude_vx,.N]
    #remove all records for which the meaning is in excluded meanings
    df<-df[meaning %!in% meanings_exclude_vx]
    #merge with the study_population table(there is no missing data in this table)
    df[,person_id:=as.character(person_id)]
    study_population[,person_id:=as.character(person_id)]
    df<-df[study_population,on=.(person_id)]#left join, keeps all people in the study population even if they didn't have a prescription
    df<-df[,age_start_follow_up:=as.numeric(age_start_follow_up)]
    pers_stdpop_not_vx<-df[rowSums(is.na(df[,..colnames]))==length(colnames), ..std_names] #subjects id present in the study population but that do not have a dispensing/prescription
    pers_stdpop_not_vx<-pers_stdpop_not_vx[!duplicated(person_id)]
    df<-df[!rowSums(is.na(df[,..colnames]))==length(colnames)]
    if(pers_stdpop_not_vx[,.N]>0){
      saveRDS(pers_stdpop_not_vx, paste0(vaccines_tmp, paste0("stdpop_not_vx_", actual_tables$VACCINES[y], ".rds"))) #to give the results we will merge the files by keeping only person_id that are in all saved files
    }
    rm(pers_stdpop_not_vx)
    vx_study_pop[[w]]<-df[,.N] #number of records for the study population, no selection criteria for time applied
    
    #transform into date variables
    df[,vx_admin_date:=as.Date(vx_admin_date,"%Y%m%d")][,vx_record_date:=as.Date(vx_record_date,"%Y%m%d")] #transform to date variables
    #create year variable
    df[,year:=year(vx_record_date)][!is.na(vx_admin_date),year:=year(vx_admin_date)]
    df[,vaccines_date:=vx_record_date][!is.na(vx_admin_date),vaccines_date:=vx_admin_date]#date that will be used for person-years
    #remove vx_admin_date and vx_record_date 
    df[,vx_admin_date:=NULL][,vx_record_date:=NULL]
    #number of records with both date dispensing/prescription missing
    vx_date_miss[[w]]<-df[is.na(year),.N]
    #remove records with both dates missing
    df<-df[!is.na(year)]
    years[[w]]<-unique(na.omit(df[, year]))
    years_this_table<-unique(na.omit(df[, year]))
    #remove records that are outside the obs_period for all subjects
    vx_out_st_per[[w]]<-df[vaccines_date<start_follow_up | vaccines_date>end_follow_up,.N] #number of records outside study population
    df[(vaccines_date<start_follow_up | vaccines_date>end_follow_up), obs_out:=1]
    df<-df[is.na(obs_out)] #remove records outside study period
    df[,obs_out:=NULL] 
    vx_study_pop_obsper[[w]]<-df[,.N] #number of records after removing records outside study period
    vx_stdpop_no_meaning[[w]]<-df[is.na(meaning),.N] #number of records with empty meaning
    df<-df[!is.na(meaning)] #remove records with empty meaning
    vx_sex_not_specified[[w]]<-df[sex_at_instance_creation =="U" | sex_at_instance_creation == "O",.N] #number of records with unspecified sex
    df<-df[sex_at_instance_creation == "M" | sex_at_instance_creation == "F"]#remove unspecified sex
    
    #########
    meanings[[w]]<-unique(na.omit(df[, meaning]))
    ############################
    vx_study_population[[w]]<-df[,.N] #number of records in the study population
    vx_study_population_meaning[[w]]<-df[,.N, by="meaning"] #number of records in the study population by meaning
    vx_study_population_meaning_year[[w]]<-df[,.N, by=c("meaning","year")] #number of records in the study population by meaning and year
    ##############################
    
    #number of records with missing atc codes
    empty_atc_code[[w]]<-df[is.na(vx_atc), .N] #number of records with missing atc code when date disp/presc is present
    df[, atc_level:=nchar(vx_atc)] #create atc level variable showing which level is present in vx_atc
    no_level1_atc[[w]]<-df[atc_level==1, .N] #number of records with only first level of atc
    no_level2_atc[[w]]<-df[atc_level==2, .N] #number of records with only second level of atc
    no_level3_atc[[w]]<-df[atc_level==3, .N] #number of records with only third level of atc
    no_level4_atc[[w]]<-df[atc_level==4, .N] #number of records with only fourth level of atc
    no_level5_atc[[w]]<-df[atc_level==5, .N] #number of records with only fifth level of atc
    no_level6_atc[[w]]<-df[atc_level==6, .N] #number of records with only sixth level of atc
    no_level7_atc[[w]]<-df[atc_level==7, .N] #number of records with only seventh level of atc
    comp_atc[[w]]<-df[!is.na(vx_atc), .N]
    #p_incomplete_7: sum of records with atc 1-6/ total no of records with complete atc code(comp_atc)
    #p_incomplete_5: sum of records with atc 1-4/ total no of records with complete atc code(comp_atc)
    
    ##############################
    #save all person ids that are part of the vaccines_study_population
    ##############################
    if (df[,.N]>0){
      saveRDS(df[!duplicated(person_id),c("person_id","birth_date","start_follow_up","end_follow_up")], paste0(vaccines_tmp, paste0("vx_id_stdpop_", actual_tables$VACCINES[y], ".rds")))
    }
    ##############################
    #First section of the SAP
    ##############################
    #records in the raw table ==orig_no_rows
    #subjects in the study population but not in VACCINES == stdpop_not_vx
    #records for study_population (independent of time) == vx_study_pop
    #records for study_population (within study period) == vx_study_pop_obsper
    #records with missing atc code when date disp/presc is present == empty_atc_code
    #records with atc code up to level 1 ==no_level1_atc
    #records with atc code up to level 2 ==no_level2_atc
    #records with atc code up to level 3 ==no_level3_atc
    #records with atc code up to level 4 ==no_level4_atc
    #records with atc code up to level 5 ==no_level5_atc
    #records with atc code up to level 6 ==no_level6_atc
    #records with atc code up to level 7 ==no_level7_atc
    
    ##############################
    #Exclusions
    ##############################
    #start: orig_no_rows
    #sub2: vx_study_pop_obsper
    # number of records with empty meaning: vx_stdpop_no_meaning
    #sub3: vx_study_population
    ###############################
    #Table 10:
    ##############################
    #empty row
    #number of records with missing atc codes by meaning(denominator) 
    #empty_atc_code_m[[w]]<-df[is.na(vx_atc),.N, by="meaning"]
    #number of records with missing atc codes by meaning and year(numerator)
    empty_atc_code_m_y[[w]]<-df[is.na(vx_atc), .N, by=c("meaning", "year")]
    #total row
    #total records by meaning(numerator): vx_study_population_meaning
    
    #counts by meaning and year for atc trunacted to the first level
    Res.1<-m_year_atc(dt=df,
                      year_var = "year",
                      meaning_var = "meaning",
                      atc_var = "vx_atc",
                      level_num = 1) #export results to vaccines_tmp with name Res_1_name of original file
    saveRDS(Res.1$count, paste0(vaccines_tmp, paste0("Res.1_count_", actual_tables$VACCINES[y], ".rds"))) #allows to save data as list
    saveRDS(Res.1$total, paste0(vaccines_tmp, paste0("Res.1_total_", actual_tables$VACCINES[y], ".rds"))) #allows to save data as list
    rm(Res.1) 
    #################################
    #Table 11:
    #################################
    
    if(df[sex_at_instance_creation=="F" & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,.N]>0){
      #empty row
      #number of records with missing atc codes by meaning in females 12-55 years old
      #empty_atc_code_m_f[[w]]<-df[is.na(vx_atc) & sex_at_instance_creation=="F" & age_start_follow_up>=12 & age_start_follow_up<=55, .N, by="meaning"]
      #number of records with missing atc codes by meaning and year in females 12-55 years old(numerator)
      empty_atc_code_m_y_f[[w]]<-df[is.na(vx_atc) & sex_at_instance_creation=="F" & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg & !is.na(year), .N, by=c("meaning","year")]
      #total row
      #total records by meaning(numerator): vx_study_population_meaning_f
      vx_study_population_meaning_f[[w]]<-df[sex_at_instance_creation=="F" & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,.N, by="meaning"]
      vx_study_population_meaning_year_f[[w]]<-df[sex_at_instance_creation=="F" & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,.N, by=c("meaning","year")]
      
      #total records(denominator): vx_study_population_f
      #vx_study_population_f[[w]]<-df[sex_at_instance_creation=="F" & age_start_follow_up>=12 & age_start_follow_up<=55,.N]
      #counts by meaning and year for atc trunacted to the first level in females [12-55]
      Res.2<-m_year_atc(dt=df[sex_at_instance_creation=="F" & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg],
                        year_var = "year",
                        meaning_var = "meaning",
                        atc_var = "vx_atc",
                        level_num = 1) #export results to vaccines_tmp with name Res_2_name of original file
      saveRDS(Res.2$count, paste0(vaccines_tmp, paste0("Res.2_count_", actual_tables$VACCINES[y], ".rds")))
      saveRDS(Res.2$total, paste0(vaccines_tmp, paste0("Res.2_total_", actual_tables$VACCINES[y], ".rds")))
      rm(Res.2)
      females_childbearing[[w]]<-1
    } else {females_childbearing[[w]]<-0}
    
    ##################################
    #Table 12:Info
    ##################################
    male_population[[w]]<-ifelse(df[sex_at_instance_creation=="M",.N]>0,1,0)
    female_population[[w]]<-ifelse(df[sex_at_instance_creation=="F",.N]>0,1,0)
    ############################################
    #number of male users and median for male users:output of calculation dataset by atc level 1 and year
    ############################################
    df[,atc_code_1:=substr(vx_atc,1,1)]
    if(df[sex_at_instance_creation=="M",.N]>0){
      for (a in 1:length(LETTERS)){
        for (b in 1:length(years_this_table)){
          if(df[sex_at_instance_creation=="M" & atc_code_1==LETTERS[a] & year==years_this_table[b],.N]>0){
            if (subpopulations_present=="Yes"){
              saveRDS(df[sex_at_instance_creation=="M" & atc_code_1==LETTERS[a] & year==years_this_table[b], c("person_id", "year", "meaning", "vx_atc","vaccines_date","birth_date","start_follow_up","end_follow_up")], paste0(vaccines_pop,subpopulations_names[s], "/", paste0(LETTERS[a], "_", years_this_table[b], "_m_population_", actual_tables$VACCINES[y], ".rds")))
            } else {
              saveRDS(df[sex_at_instance_creation=="M" & atc_code_1==LETTERS[a] & year==years_this_table[b], c("person_id", "year", "meaning", "vx_atc","vaccines_date","birth_date","start_follow_up","end_follow_up")], paste0(vaccines_pop, paste0(LETTERS[a], "_", years_this_table[b], "_m_population_", actual_tables$VACCINES[y], ".rds")))
            }
          }
        }
      } 
    }
    
    
    ####################################################
    #number of female users and median for female users:output of calculation dataset by atc level 1 and year
    ####################################################
    if(df[sex_at_instance_creation=="F",.N]>0){
      for (a in 1:length(LETTERS)){
        for (b in 1:length(years_this_table)){
          if(df[sex_at_instance_creation=="F" & atc_code_1==LETTERS[a] & year==years_this_table[b],.N]>0){
            if (subpopulations_present=="Yes"){
              saveRDS(df[sex_at_instance_creation=="F" & atc_code_1==LETTERS[a] & year==years_this_table[b], c("person_id", "year", "meaning", "vx_atc","age_start_follow_up","vaccines_date","birth_date","start_follow_up","end_follow_up")], paste0(vaccines_pop,subpopulations_names[s], "/", paste0(LETTERS[a], "_", years_this_table[b], "_f_population_", actual_tables$VACCINES[y], ".rds")))
            } else {
              saveRDS(df[sex_at_instance_creation=="F" & atc_code_1==LETTERS[a] & year==years_this_table[b], c("person_id", "year", "meaning", "vx_atc","age_start_follow_up","vaccines_date","birth_date","start_follow_up","end_follow_up")], paste0(vaccines_pop, paste0(LETTERS[a], "_", years_this_table[b], "_f_population_", actual_tables$VACCINES[y], ".rds")))
              
            }
          }
        }
      } 
    }
    
    
    ########
    w<-w+1
    rm(df)
    ########
  }
  
  #################################################################################################
  #save all ids part of the vaccines_study_population
  #################################################################################################  
  #number of subjects in the study population that do not have a prescription/dispensing
  id_vx_files<-list.files(vaccines_tmp, pattern = "vx_id_stdpop")
  if (length(id_vx_files)>0){
    vx_id<-readRDS(paste0(vaccines_tmp, id_vx_files[1]))
    i<-2
    while(i <= length(id_vx_files)){
      a<-readRDS(paste0(vaccines_tmp, id_vx_files[i]))
      vx_id<-rbind(vx_id, a)
      vx_id<-vx_id[duplicated(person_id)]
      i<-i+1
      rm(a)
    }
    for(i in 1:length(id_vx_files)){
      unlink(paste0(vaccines_tmp,id_vx_files[i]))
    }
    rm(id_vx_files)
    
    if (vx_id[,.N]>0){
      saveRDS(vx_id, paste0(vaccines_tmp, "vx_ids_stdpop.rds"))
    }
    rm(vx_id)
  }
  
  #################################################################################################
  #Flowchart
  ################################################################################################
  print("Creating flowchart.")
  print("Get number of records in the original table.")
  #original number of records in the VACCINES table(flowchart 1)
  orig_no_rows<-do.call(rbind,orig_no_rows)
  orig_no_rows<-sum(orig_no_rows)
  #number of records with excluded meanings(flowchart 2)
  print("Get number of records with excluded meanings.")
  vx_excluded_meanings<-do.call(rbind, vx_excluded_meanings)
  vx_excluded_meanings<-sum(vx_excluded_meanings)
  #number of records for the study population, no selection criteria for time applied (flowchart 3)
  print("Get number of records for the study population (no time criteria applied).")
  vx_study_pop<-do.call(rbind,vx_study_pop)
  vx_study_pop<-sum(vx_study_pop)
  #number of records with both dates missing(flowchart 4)
  vx_date_miss<-do.call(rbind,vx_date_miss)
  vx_date_miss<-sum(vx_date_miss)
  #number of VACCINES records outside the observation period(check is done on individual level) (flowchart 5)
  print("Get number of records outside observation period.")
  vx_out_st_per<-do.call(rbind,vx_out_st_per) 
  vx_out_st_per<-sum(vx_out_st_per)
  #number of records in the study population with date dispensing/prescription inside study period (flowchart 6)
  print("Get number of records for the study population(time criteria applied).")
  vx_study_pop_obsper<-do.call(rbind,vx_study_pop_obsper) 
  vx_study_pop_obsper<-sum(vx_study_pop_obsper)
  #number of records in the study population with no meaning (flowchart 7)
  print("Get number of records with no meaning.")
  vx_stdpop_no_meaning<-do.call(rbind,vx_stdpop_no_meaning) 
  vx_stdpop_no_meaning<-sum(vx_stdpop_no_meaning) 
  #number of records with unspecified sex (flowchart 8)
  print("Get number of records with unspecified sex.")
  vx_sex_not_specified<-do.call(rbind,vx_sex_not_specified)
  vx_sex_not_specified<-sum(vx_sex_not_specified)
  #number of records in the study population (flowchart 9)
  print("Get number of records for study population.")
  vx_study_population<-do.call(rbind,vx_study_population) 
  vx_study_population<-sum(vx_study_population) 
  
  #Flowchart
  print("Create flowchart.")
  flowchart<-data.table(INDICATOR=c("Number of records in the original table(VACCINES)", 
                                    "Number of subjects in the original study population table",
                                    "Exclude:Number of records with excluded meanings",
                                    "Number of records for the study_population(no time criteria)",
                                    "Exclude: Number of records with both date record and date administration missing",
                                    "Exclude: Number of records with date record/administration outside study period",
                                    "Number of records for the study_population(time criteria applied)",
                                    "Exclude:Number of records with empty meaning",
                                    "Exclude: Number of records with unknown or other sex",
                                    "Number of records for study_population"), 
                        COUNT=c(orig_no_rows,
                                nr_std,
                                vx_excluded_meanings,
                                vx_study_pop,
                                vx_date_miss,
                                vx_out_st_per,
                                vx_study_pop_obsper,
                                vx_stdpop_no_meaning,
                                vx_sex_not_specified,
                                vx_study_population))
  
  rm(orig_no_rows,vx_excluded_meanings,vx_study_pop,vx_date_miss,vx_out_st_per,
     vx_study_pop_obsper,vx_stdpop_no_meaning,vx_sex_not_specified)
  
  print("Export flowchart to g_output.")
  if(subpopulations_present=="Yes"){
    write.csv(flowchart, paste0(vacc_dir,subpopulations_names[s], "/", subpopulations_names[s],"_vaccines_flowchart.csv"), row.names = F)
  } else {
    write.csv(flowchart, paste0(vacc_dir, "vaccines_flowchart.csv"), row.names = F)
  }
  
  #####Apply masking
  print("Masking results for flowchart.")
  flowchart[, COUNT:= as.character(COUNT)][as.numeric(COUNT) > 0 & as.numeric(COUNT) < 5, COUNT := "<5"]
  if(subpopulations_present=="Yes"){
    write.csv(flowchart, paste0(vacc_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_vaccines_flowchart_masked.csv"), row.names = F)
  } else {
    write.csv(flowchart, paste0(vacc_dir,"Masked/","vaccines_flowchart_masked.csv"), row.names = F)
  }
  
  rm(flowchart)
  ##################################################################################################
  #Description
  ##################################################################################################
  print("Creating description of study population.")
  #meanings
  print("Get list of meanings.")
  meanings<-Filter(length,meanings)
  meanings<-suppressWarnings(do.call(rbind,meanings))
  meanings<-unique(c(meanings))
  meanings_des<-paste(meanings, collapse = ", ")
  
  #study years
  years<-Filter(length,years)
  years<-suppressWarnings(do.call(rbind, years))
  years<-unique(c(years))
  years_des<-paste(sort(years), collapse=", ")
  #
  male_population<-do.call(rbind, male_population)
  male_population<-sum(male_population)
  female_population<-do.call(rbind, female_population)
  female_population<-sum(female_population)
  if(male_population>0 & female_population>0){sex_included<-c("Males, Females")}
  if(male_population==0 & female_population>0){sex_included<-c("Females")}
  if(male_population>0 & female_population==0){sex_included<-c("Males")}
  if(male_population==0 & female_population==0){sex_included<-c("None")}
  #original number of subjects in the study population
  #nr_std
  #number of subjects in the study population that do not have a prescription/dispensing
  stdpop_not_vx_files<-list.files(vaccines_tmp, pattern = "stdpop_not_vx")
  if (length(stdpop_not_vx_files)>0){
    stdpop_not_vx<-readRDS(paste0(vaccines_tmp, stdpop_not_vx_files[1]))
    i<-2
    while(i <= length(stdpop_not_vx_files)){
      a<-readRDS(paste0(vaccines_tmp, stdpop_not_vx_files[i]))
      stdpop_not_vx<-rbind(stdpop_not_vx, a)
      stdpop_not_vx<-stdpop_not_vx[duplicated(person_id)]
      i<-i+1
      rm(a)
    }
    
    if(stdpop_not_vx[,.N]>0){
      saveRDS(stdpop_not_vx, paste0(vaccines_tmp, "vx_id_no_rx.rds"))
    }
    
    stdpop_not_vx<-stdpop_not_vx[,.N]
    
    for(i in 1:length(stdpop_not_vx_files)){
      unlink(paste0(vaccines_tmp,stdpop_not_vx_files[i]))
    }
    rm(stdpop_not_vx_files)
    
  } else {stdpop_not_vx<-0}
  
  #number of records with empty atc codes 
  print("Get number of records with empty ATC codes.")
  empty_atc_code<-do.call(rbind,empty_atc_code) 
  empty_atc_code<-sum(empty_atc_code)  
  #number of records with atc code up to level 1
  no_level1_atc<-do.call(rbind,no_level1_atc) 
  no_level1_atc<-sum(no_level1_atc) 
  #number of records with atc code up to level 2
  no_level2_atc<-do.call(rbind,no_level2_atc) 
  no_level2_atc<-sum(no_level2_atc)  
  #number of records with atc code up to level 3
  no_level3_atc<-do.call(rbind,no_level3_atc) 
  no_level3_atc<-sum(no_level3_atc) 
  #number of records with atc code up to level 4
  no_level4_atc<-do.call(rbind,no_level4_atc) 
  no_level4_atc<-sum(no_level4_atc)  
  #number of records with atc code up to level 5
  no_level5_atc<-do.call(rbind,no_level5_atc) 
  no_level5_atc<-sum(no_level5_atc)  
  #number of records with atc code up to level 6
  no_level6_atc<-do.call(rbind,no_level6_atc) 
  no_level6_atc<-sum(no_level6_atc)  
  #number of records with atc code up to level 7
  no_level7_atc<-do.call(rbind,no_level7_atc) 
  no_level7_atc<-sum(no_level7_atc) 
  #total number of records with complete atc code
  comp_atc<-do.call(rbind,comp_atc) 
  comp_atc<-sum(comp_atc)  
  
  
  print("Create description.")
  description<-data.table(INDICATOR=c("List of meanings present",
                                      "Years included in the study period",
                                      "Sex included in the study population",
                                      "Number of subjects without dispensing/prescriptions in the study population",
                                      "Number of records with empty ATC codes when vx_admin_date/vx_record_date is present",
                                      "Number of records with complete ATC codes",
                                      "Number of records with complete ATC code up to level 1",
                                      "Number of records with complete ATC code up to level 2",
                                      "Number of records with complete ATC code up to level 3",
                                      "Number of records with complete ATC code up to level 4",
                                      "Number of records with complete ATC code up to level 5",
                                      "Number of records with complete ATC code up to level 6",
                                      "Number of records with complete ATC code up to level 7"), 
                          COUNT=c(meanings_des,
                                  years_des,
                                  sex_included,
                                  stdpop_not_vx,
                                  empty_atc_code,
                                  comp_atc,
                                  no_level1_atc,
                                  no_level2_atc,
                                  no_level3_atc,
                                  no_level4_atc,
                                  no_level5_atc,
                                  no_level6_atc,
                                  no_level7_atc))
  rm(meanings_des, years_des, sex_included, stdpop_not_vx,empty_atc_code,comp_atc,no_level1_atc,no_level2_atc,
     no_level3_atc,no_level4_atc,no_level5_atc,no_level6_atc,no_level7_atc)
  
  if(subpopulations_present=="Yes"){
    write.csv(description, paste0(vacc_dir,subpopulations_names[s], "/", subpopulations_names[s],"_vaccines_description.csv"), row.names = F)
  } else {
    write.csv(description, paste0(vacc_dir,"vaccines_description.csv"), row.names = F)
  }
  
  #Apply masking
  if(as.numeric(description[4, 2])<5 & as.numeric(description[4, 2])>0) {description[4, 2]<-"<5"}
  if(as.numeric(description[5, 2])<5 & as.numeric(description[5, 2])>0) {description[5, 2]<-"<5"} 
  if(as.numeric(description[6, 2])<5 & as.numeric(description[6, 2])>0) {description[6, 2]<-"<5"} 
  if(as.numeric(description[7, 2])<5 & as.numeric(description[7, 2])>0) {description[7, 2]<-"<5"} 
  if(as.numeric(description[8, 2])<5 & as.numeric(description[8, 2])>0) {description[8, 2]<-"<5"} 
  if(as.numeric(description[9, 2])<5 & as.numeric(description[9, 2])>0) {description[9, 2]<-"<5"} 
  if(as.numeric(description[10, 2])<5 & as.numeric(description[10, 2])>0) {description[10, 2]<-"<5"} 
  if(as.numeric(description[11, 2])<5 & as.numeric(description[11, 2])>0) {description[11, 2]<-"<5"} 
  if(as.numeric(description[12, 2])<5 & as.numeric(description[12, 2])>0) {description[12, 2]<-"<5"} 
  if(as.numeric(description[13, 2])<5 & as.numeric(description[13, 2])>0) {description[13, 2]<-"<5"} 
  
  if(subpopulations_present=="Yes"){
    write.csv(description, paste0(vacc_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_vaccines_description_masked.csv"), row.names = F)
  } else {
    write.csv(description, paste0(vacc_dir,"Masked/","vaccines_description_masked.csv"), row.names = F)
  }
  
  rm(description)
   #################################################################################################
  #Table 10: Number of vaccine records/administrations by ATC A level in the study population by year of dispensing/prescribing and by meaning
  #################################################################################################
  print("Creating Table 10:  Number of vaccine records/administrations by ATC A level in the study population by year of dispensing/prescribing and by meaning.")
  print("Get all variables.")
  #empty atc codes by meaning
  empty_atc_code_m_y<-do.call(rbind,empty_atc_code_m_y)
  empty_atc_code_m_y<-empty_atc_code_m_y[,lapply(.SD, sum), .SDcols="N", by=c("meaning", "year")]
  names(empty_atc_code_m_y)<-c("meaning","year", "count")
  empty_atc_code_m_y<-data.table(empty_atc_code_m_y,atc_code_1="empty")
  vx_study_population_meaning_year<-do.call(rbind,vx_study_population_meaning_year)
  vx_study_population_meaning_year<-vx_study_population_meaning_year[,lapply(.SD, sum), .SDcols="N", by=c("meaning", "year")]
  setnames(vx_study_population_meaning_year,"N", "total")
  
  
  #counts by meaning, year and atc level 1:load Res.1 
  Res.1_files<-list.files(vaccines_tmp,pattern="^Res.1_count")
  Res.1_tot<-list.files(vaccines_tmp,pattern="^Res.1_total")
  if (length(Res.1_files)>0){
    tab10<-lapply(paste0(vaccines_tmp,Res.1_files),readRDS)
    tab10<-do.call(rbind,tab10)
    tab10<-tab10[,lapply(.SD, sum), .SDcols="count", by=c("meaning", "year", "atc_code_1")]
  } else {
    tab10<-NULL 
  }
  if (length(Res.1_tot)>0){
    Res.1_total<-lapply(paste0(vaccines_tmp,Res.1_tot),readRDS)
    Res.1_total<-do.call(rbind,Res.1_total)
    Res.1_total<-Res.1_total[,lapply(.SD, sum), .SDcols="total", by=c("meaning", "year")]
  } else {
    Res.1_total<-NULL 
  }
  
  if(!is.null(tab10) | !is.null(Res.1_total)){
    tab10<-merge(tab10,Res.1_total, by=c("meaning","year"))
  }
  
  #remove all files in Res.1_files
  #remove files not needed from vaccines_tmp
  for(i in 1:length(Res.1_files)){
    unlink(paste0(vaccines_tmp,Res.1_files[i]))
  }
  for(i in 1:length(Res.1_tot)){
    unlink(paste0(vaccines_tmp,Res.1_tot[i]))
  }
  
  #get all combinations meaning and year from the table
  meanings_tab10<-tab10[!duplicated(meaning),meaning]
  years_tab10<-tab10[!duplicated(year),year]
  comb_tab10<-as.data.table(expand.grid(meanings_tab10,years_tab10))
  names(comb_tab10)<-c("meaning","year")
  empty_atc_code_m_y<-merge(empty_atc_code_m_y,comb_tab10, by=c("meaning","year"),all=T)
  empty_atc_code_m_y<-merge(empty_atc_code_m_y,vx_study_population_meaning_year,by=c("meaning","year"))
  empty_atc_code_m_y[is.na(count),count:=0][is.na(atc_code_1),atc_code_1:="empty"]
  rm(vx_study_population_meaning_year)
  print("Create table 10.")
  tab10<-data.table(rbind(tab10, empty_atc_code_m_y))
  setcolorder(tab10, c("meaning","year", "atc_code_1","count","total"))
  setorderv(tab10, c("meaning", "year", "atc_code_1"))
  rm(Res.1_files)
  
  print("Export table 10.")
  
  if(!is.null(tab10)){
    tab10<-data.table(tab10, data_access_provider= data_access_provider_name, data_source=data_source_name)
    if (subpopulations_present=="Yes"){
      fwrite(tab10, paste0(vacc_dir,subpopulations_names[s], "/", subpopulations_names[s],"_vaccines_my_atc_1.csv"), row.names = F)
    } else {
      fwrite(tab10, paste0(vacc_dir,"vaccines_my_atc_1.csv"), row.names = F)
    }
  }
  
  #Apply masking
  
  if(!is.null(tab10)){
    suppressWarnings(tab10[, count:= as.character(count)][as.numeric(count) > 0 & as.numeric(count) < 5, count := "<5"])
    suppressWarnings(tab10[, total:= as.character(total)][as.numeric(total) > 0 & as.numeric(total) < 5, total := "<5"])
    if(subpopulations_present=="Yes"){
      fwrite(tab10, paste0(vacc_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_vaccines_my_atc_1_masked.csv"), row.names = F)
    } else {
      fwrite(tab10, paste0(vacc_dir,"Masked/","vaccines_my_atc_1_masked.csv"), row.names = F)
    }
  }
  rm(tab10)
  #################################################################################################
  #Table 11: Number of vaccine records/administrations by ATC A level in the female study population of childbearing age 12-55 years (based on age at Start_study_fup) by year of dispensing/prescribing and by meaning
  #################################################################################################
  females_childbearing<-do.call(rbind,females_childbearing)
  females_childbearing<-sum(females_childbearing)
  if(females_childbearing>0){
    print("Creating Table 11:  Number of vaccine records/administrations by ATC A level in the female study population of childbearing age 12-55 years (based on age at Start_study_fup) by year of dispensing/prescribing and by meaning.")
    print("Get all variables.")
    
    #empty atc codes by meaning
    empty_atc_code_m_y_f<-do.call(rbind,empty_atc_code_m_y_f)
    empty_atc_code_m_y_f<-empty_atc_code_m_y_f[,lapply(.SD, sum), .SDcols="N", by=c("meaning", "year")]
    names(empty_atc_code_m_y_f)<-c("meaning","year", "count")
    empty_atc_code_m_y_f[,atc_code_1:="empty"]
    vx_study_population_meaning_year_f<-do.call(rbind,vx_study_population_meaning_year_f)
    vx_study_population_meaning_year_f<-vx_study_population_meaning_year_f[,lapply(.SD, sum), .SDcols="N", by=c("meaning", "year")]
    setnames(vx_study_population_meaning_year_f,"N", "total")
    
    #counts by meaning, year and atc level 1:load Res.2 
    Res.2_files<-list.files(vaccines_tmp,pattern="^Res.2_count")
    Res.2_tot<-list.files(vaccines_tmp,pattern="^Res.2_total")
    if (length(Res.2_files)>0){
      tab11<-lapply(paste0(vaccines_tmp,Res.2_files),readRDS)
      tab11<-do.call(rbind,tab11)
      tab11<-tab11[,lapply(.SD, sum), .SDcols="count", by=c("meaning", "year", "atc_code_1")]
    } else {
      tab11<-NULL 
    }
    if (length(Res.2_tot)>0){
      Res.2_total<-lapply(paste0(vaccines_tmp,Res.2_tot),readRDS)
      Res.2_total<-do.call(rbind,Res.2_total)
      Res.2_total<-Res.2_total[,lapply(.SD, sum), .SDcols="total", by=c("meaning", "year")]
    } else {
      Res.2_total<-NULL 
    }
    
    if(!is.null(tab11) | !is.null(Res.2_total)){
      tab11<-merge(tab11,Res.2_total, by=c("meaning","year"))
    }
    
    #remove all files in Res.2_files
    #remove files not needed from vaccines_tmp
    for(i in 1:length(Res.2_files)){
      unlink(paste0(vaccines_tmp,Res.2_files[i]))
    }
    for(i in 1:length(Res.2_tot)){
      unlink(paste0(vaccines_tmp,Res.2_tot[i]))
    }
    
    
    #get all combinations meaning and year from the table
    meanings_tab11<-tab11[!duplicated(meaning),meaning]
    years_tab11<-tab11[!duplicated(year),year]
    comb_tab11<-as.data.table(expand.grid(meanings_tab11,years_tab11))
    names(comb_tab11)<-c("meaning","year")
    empty_atc_code_m_y_f<-merge(empty_atc_code_m_y_f,comb_tab11, by=c("meaning","year"),all=T)
    empty_atc_code_m_y_f<-merge(empty_atc_code_m_y_f,vx_study_population_meaning_year_f,by=c("meaning","year"))
    empty_atc_code_m_y_f[is.na(count),count:=0][is.na(atc_code_1),atc_code_1:="empty"]
    rm(vx_study_population_meaning_year_f)
    print("Create table 10.")
    tab11<-data.table(rbind(tab11, empty_atc_code_m_y_f))
    setcolorder(tab11, c("meaning","year", "atc_code_1","count","total"))
    setorderv(tab11, c("meaning", "year", "atc_code_1"))
    rm(Res.2_files)
    print("Export table 11.")
    
  } else {
    tab11<-NULL
    print("Counts for females of childbearing age cannot be esstimated due to missingness of the data for this subpopulation.")
  }
  
  print("Export table 11.")
  
  if(!is.null(tab11)){
    tab11<-data.table(tab11, data_access_provider= data_access_provider_name, data_source=data_source_name)
    if(subpopulations_present=="Yes"){
      fwrite(tab11, paste0(vacc_dir,subpopulations_names[s], "/", subpopulations_names[s],"_vaccines_my_atc_1_f.csv"), row.names = F)
    } else {
      fwrite(tab11, paste0(vacc_dir,"vaccines_my_atc_1_f.csv"), row.names = F)
    }
  }
  
  #Apply masking
  if(!is.null(tab11)){
    suppressWarnings(tab11[, count:= as.character(count)][as.numeric(count) > 0 & as.numeric(count) < 5, count := "<5"])
    suppressWarnings(tab11[, total:= as.character(total)][as.numeric(total) > 0 & as.numeric(total) < 5, total := "<5"])
    if (subpopulations_present=="Yes"){
      fwrite(tab11, paste0(vacc_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_vaccines_my_atc_1_f_masked.csv"), row.names = F)
    } else {
      fwrite(tab11, paste0(vacc_dir,"Masked/", "vaccines_my_atc_1_f_masked.csv"), row.names = F)
    }
  }
  rm(tab11)
  ##################################
  #Table 12:
  ##################################
  print("Creating Table 12:  Number of vaccine records/administrations by ATC 1, 3 and 4 level in the study population by year of dispensing/prescribing and by meaning.")
  print("Get all variables.")
  ##############################
  #male users and median prescriptions for male users
  #############################
  if(male_population>0){
    if(subpopulations_present=="Yes"){
      median_males_files<-list.files(paste0(vaccines_pop, subpopulations_names[s], "/"),pattern="m_population")
    } else {
      median_males_files<-list.files(vaccines_pop,pattern="m_population") 
    }
    if(length(median_males_files)>0){
      #grab only the first letter which symoblized the ATC level 1
      letters_atc<-unique(sapply(median_males_files, function(x) substr(x,1,1))) 
      #create list for each letter
      list_median_males<-vector(mode="list", length=length(letters_atc))
      names(list_median_males)<-letters_atc
      for (i in 1:length(list_median_males)){
        list_median_males[[i]]<-median_males_files[substr(median_males_files,1,1)==names(list_median_males)[i]]
      }
      rm(letters_atc)
      
      vx_males_index<-1
      for (vx_males_index in 1:length(list_median_males)){
        if(subpopulations_present=="Yes"){
          median_male<-readRDS(paste0(vaccines_pop, subpopulations_names[s], "/", list_median_males[[vx_males_index]][1]))
        } else {
          median_male<-readRDS(paste0(vaccines_pop, list_median_males[[vx_males_index]][1]))
        }
        #count by person_id, atc code meaning and year(concatenate records with the same atc code for the same person)
        median_male<-median_male[,.(count=.N), by=c("person_id","meaning","year","vx_atc")]
        
        z<-2
        while (z <= length(list_median_males[[vx_males_index]])){
          if(subpopulations_present=="Yes"){
            a<-readRDS(paste0(vaccines_pop, subpopulations_names[s], "/", list_median_males[[vx_males_index]][[z]]))
          } else {
            a<-readRDS(paste0(vaccines_pop, list_median_males[[vx_males_index]][[z]]))
          }
          #concatenate data for the same person
          a<-a[,.(count=.N), by=c("person_id","meaning","year","vx_atc")]
          median_male<-rbind(median_male, a)
          #concatenate results for the same person
          median_male<-median_male[,lapply(.SD, sum), by=c("person_id","meaning","year","vx_atc"), .SDcols="count"]
          z<-z+1
          rm(a)
        }
        
        median_male<-median_male[,atc_level:=nchar(vx_atc)]
        #number of records, male users, median male users by atc_code_7 (table 13)
        if(median_male[atc_level==7,.N]>0){
          #number of records by meaning, year, and atc_code_7
          res.tab13.m_records.my<-median_male[atc_level==7,lapply(.SD, sum), by=.(vx_atc, meaning, year),.SDcols="count"]
          setnames(res.tab13.m_records.my,"count","no_records")
          setnames(res.tab13.m_records.my,"vx_atc","atc_code_7")
          res.tab13.m_records.my[,atc_code_3:=substr(atc_code_7,1,3)]
          setcolorder(res.tab13.m_records.my,c("meaning","year","atc_code_3","atc_code_7"))
          saveRDS(res.tab13.m_records.my,paste0(vaccines_tmp,names(list_median_males)[vx_males_index], "_tab13_m_rec_7.my_", vx_males_index, ".rds"))
          rm(res.tab13.m_records.my)
          #number of records by atc_code_7(irrespective of meaning and year)
          res.tab13.m_records.t<-median_male[atc_level==7,lapply(.SD,sum),by=.(person_id,vx_atc), .SDcols="count"][,lapply(.SD,sum),by=.(vx_atc), .SDcols="count"]
          res.tab13.m_records.t[,meaning:="All"][,year:="All"]
          setnames(res.tab13.m_records.t,"count","no_records")
          setnames(res.tab13.m_records.t,"vx_atc","atc_code_7")
          res.tab13.m_records.t[,atc_code_3:=substr(atc_code_7,1,3)]
          setcolorder(res.tab13.m_records.t,c("meaning","year","atc_code_3","atc_code_7"))
          saveRDS(res.tab13.m_records.t,paste0(vaccines_tmp,names(list_median_males)[vx_males_index], "_tab13_m_rec_7.t_", vx_males_index, ".rds"))
          rm(res.tab13.m_records.t)
          #number of male users by meaning, year and atc_code_7
          res.tab13.m_users.c_7<-median_male[atc_level==7,lapply(.SD,function(x) length(unique(na.omit(x)))), by=.(vx_atc, meaning, year),.SDcols="person_id"]
          setnames(res.tab13.m_users.c_7,"person_id","no_male_users")
          setnames(res.tab13.m_users.c_7,"vx_atc","atc_code_7")
          res.tab13.m_users.c_7[,atc_code_3:=substr(atc_code_7,1,3)]
          setcolorder(res.tab13.m_users.c_7,c("meaning","year","atc_code_3","atc_code_7"))
          saveRDS(res.tab13.m_users.c_7,paste0(vaccines_tmp,names(list_median_males)[vx_males_index], "_tab13_m_users_7.my_", vx_males_index, ".rds"))
          rm(res.tab13.m_users.c_7)
          #number of male users by atc_code_7
          res.tab13.m_users.t_7<-median_male[atc_level==7,lapply(.SD,sum),by=.(person_id,vx_atc), .SDcols="count"][,lapply(.SD,function(x) length(unique(na.omit(x)))), by=.(vx_atc),.SDcols="person_id"]
          setnames(res.tab13.m_users.t_7,"person_id","no_male_users")
          setnames(res.tab13.m_users.t_7,"vx_atc","atc_code_7")
          res.tab13.m_users.t_7[,meaning:="All"][,year:="All"]
          res.tab13.m_users.t_7[,atc_code_3:=substr(atc_code_7,1,3)]
          setcolorder(res.tab13.m_users.t_7,c("meaning","year","atc_code_3","atc_code_7"))
          saveRDS(res.tab13.m_users.t_7,paste0(vaccines_tmp,names(list_median_males)[vx_males_index], "_tab13_m_users_7.t_", vx_males_index, ".rds"))
          rm(res.tab13.m_users.t_7)
          #median prescription by meaning, year and atc_code_7
          res.tab13.m_users.med_7<-median_male[atc_level==7,lapply(.SD,median),.SDcols="count", by=.(meaning, year, vx_atc)]
          setnames(res.tab13.m_users.med_7,"count","median_rx_male_users")
          setnames(res.tab13.m_users.med_7,"vx_atc","atc_code_7")
          res.tab13.m_users.med_7[,atc_code_3:=substr(atc_code_7,1,3)]
          setcolorder(res.tab13.m_users.med_7,c("meaning","year","atc_code_3","atc_code_7"))
          saveRDS(res.tab13.m_users.med_7,paste0(vaccines_tmp,names(list_median_males)[vx_males_index],"_tab13_m_median_7.my_",vx_males_index, ".rds"))
          rm(res.tab13.m_users.med_7)
          #median prescriptions by atc_code_7
          res.tab13.m_users.med.t_7<-median_male[atc_level==7,lapply(.SD,sum),by=.(person_id,vx_atc), .SDcols="count"][,lapply(.SD,median),.SDcols="count", by=.(vx_atc)]
          setnames(res.tab13.m_users.med.t_7,"count","median_rx_male_users")
          setnames(res.tab13.m_users.med.t_7,"vx_atc","atc_code_7")
          res.tab13.m_users.med.t_7[,meaning:="All"][,year:="All"]
          res.tab13.m_users.med.t_7[,atc_code_3:=substr(atc_code_7,1,3)]
          setcolorder(res.tab13.m_users.med.t_7,c("meaning","year","atc_code_3","atc_code_7"))
          saveRDS(res.tab13.m_users.med.t_7,paste0(vaccines_tmp,names(list_median_males)[vx_males_index],"_tab13_m_median_7.t_",vx_males_index, ".rds"))
          rm(res.tab13.m_users.med.t_7)
          
          #mean prescription by meaning, year and atc_code_7
          res.tab13.m_users.mean_7<-median_male[atc_level==7,lapply(.SD,mean),.SDcols="count", by=.(meaning, year, vx_atc)]
          setnames(res.tab13.m_users.mean_7,"count","mean_rx_male_users")
          #round mean
          res.tab13.m_users.mean_7[,mean_rx_male_users:=round(mean_rx_male_users,2)]
          setnames(res.tab13.m_users.mean_7,"vx_atc","atc_code_7")
          res.tab13.m_users.mean_7[,atc_code_3:=substr(atc_code_7,1,3)]
          setcolorder(res.tab13.m_users.mean_7,c("meaning","year","atc_code_3","atc_code_7"))
          saveRDS(res.tab13.m_users.mean_7,paste0(vaccines_tmp,names(list_median_males)[vx_males_index],"_tab13_m_mean_7.my_",vx_males_index, ".rds"))
          rm(res.tab13.m_users.mean_7)
          #mean prescriptions by atc_code_7
          res.tab13.m_users.mean.t_7<-median_male[atc_level==7,lapply(.SD,sum),by=.(person_id,vx_atc), .SDcols="count"][,lapply(.SD,mean),.SDcols="count", by=.(vx_atc)]
          setnames(res.tab13.m_users.mean.t_7,"count","mean_rx_male_users")
          res.tab13.m_users.mean.t_7[,mean_rx_male_users:=round(mean_rx_male_users,2)]
          setnames(res.tab13.m_users.mean.t_7,"vx_atc","atc_code_7")
          res.tab13.m_users.mean.t_7[,meaning:="All"][,year:="All"]
          res.tab13.m_users.mean.t_7[,atc_code_3:=substr(atc_code_7,1,3)]
          setcolorder(res.tab13.m_users.mean.t_7,c("meaning","year","atc_code_3","atc_code_7"))
          saveRDS(res.tab13.m_users.mean.t_7,paste0(vaccines_tmp,names(list_median_males)[vx_males_index],"_tab13_m_mean_7.t_",vx_males_index, ".rds"))
          rm(res.tab13.m_users.mean.t_7)
        }
        
        #############
        #number of records, male users, median male users (table 12)
        if(median_male[atc_level==4|atc_level==5|atc_level==6|atc_level==7,.N]>0){
          median_male<-median_male[,atc_code_4:=substr(vx_atc,1,4)] #create atc_code_4
          #number of records by meaning, year, and atc_code_4
          res.tab12.m_records.my<-median_male[atc_level>=4,lapply(.SD, sum), by=.(atc_code_4, meaning, year),.SDcols="count"]
          setnames(res.tab12.m_records.my,"count","no_records")
          res.tab12.m_records.my[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          setcolorder(res.tab12.m_records.my,c("meaning","year","atc_code_1","atc_code_3","atc_code_4"))
          saveRDS(res.tab12.m_records.my,paste0(vaccines_tmp,names(list_median_males)[vx_males_index], "_tab12_m_rec_4.my_", vx_males_index, ".rds"))
          rm(res.tab12.m_records.my)
          #number of records by atc_code_4
          res.tab12.m_records.t<-median_male[atc_level>=4,lapply(.SD,sum),by=.(person_id,atc_code_4), .SDcols="count"][,lapply(.SD, sum), by=.(atc_code_4),.SDcols="count"]
          res.tab12.m_records.t[,meaning:="All"][,year:="All"]
          setnames(res.tab12.m_records.t,"count","no_records")
          res.tab12.m_records.t[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          setcolorder(res.tab12.m_records.t,c("meaning","year","atc_code_1","atc_code_3","atc_code_4"))
          saveRDS(res.tab12.m_records.t,paste0(vaccines_tmp,names(list_median_males)[vx_males_index], "_tab12_m_rec_4.t_", vx_males_index, ".rds"))
          rm(res.tab12.m_records.t)
          #number of male users by meaning, year and atc_code_4
          res.tab12.m_users.c_4<-median_male[atc_level>=4,lapply(.SD,function(x) length(unique(na.omit(x)))), by=.(atc_code_4, meaning, year),.SDcols="person_id"]
          setnames(res.tab12.m_users.c_4,"person_id","no_male_users")
          res.tab12.m_users.c_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          setcolorder(res.tab12.m_users.c_4,c("meaning","year","atc_code_1","atc_code_3","atc_code_4"))
          saveRDS(res.tab12.m_users.c_4,paste0(vaccines_tmp,names(list_median_males)[vx_males_index], "_tab12_m_users_4.my_", vx_males_index, ".rds"))
          rm(res.tab12.m_users.c_4)
          #number of male users by atc_code_4
          res.tab12.m_users.t_4<-median_male[atc_level>=4,lapply(.SD,sum),by=.(person_id,atc_code_4), .SDcols="count"][,lapply(.SD,function(x) length(unique(na.omit(x)))), by=.(atc_code_4),.SDcols="person_id"]
          setnames(res.tab12.m_users.t_4,"person_id","no_male_users")
          res.tab12.m_users.t_4[,meaning:="All"][,year:="All"]
          res.tab12.m_users.t_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          setcolorder(res.tab12.m_users.t_4,c("meaning","year","atc_code_1","atc_code_3","atc_code_4"))
          saveRDS(res.tab12.m_users.t_4,paste0(vaccines_tmp,names(list_median_males)[vx_males_index], "_tab12_m_users_4.t_", vx_males_index, ".rds"))
          rm(res.tab12.m_users.t_4)
          #median prescription by meaning, year and atc_code_4
          res.tab12.m_users.med_4<-median_male[atc_level>=4,lapply(.SD,median),.SDcols="count", by=.(meaning, year, atc_code_4)]
          setnames(res.tab12.m_users.med_4,"count","median_rx_male_users")
          res.tab12.m_users.med_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          setcolorder(res.tab12.m_users.med_4,c("meaning","year","atc_code_1","atc_code_3","atc_code_4"))
          saveRDS(res.tab12.m_users.med_4,paste0(vaccines_tmp,names(list_median_males)[vx_males_index],"_tab12_m_median_4.my_",vx_males_index, ".rds"))
          rm(res.tab12.m_users.med_4)
          #median prescriptions by atc_code_4
          res.tab12.m_users.med.t_4<-median_male[atc_level>=4,lapply(.SD,sum),by=.(person_id,atc_code_4), .SDcols="count"][,lapply(.SD,median),.SDcols="count", by=.(atc_code_4)]
          setnames(res.tab12.m_users.med.t_4,"count","median_rx_male_users")
          res.tab12.m_users.med.t_4[,meaning:="All"][,year:="All"]
          res.tab12.m_users.med.t_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          setcolorder(res.tab12.m_users.med.t_4,c("meaning","year","atc_code_1","atc_code_3","atc_code_4"))
          saveRDS(res.tab12.m_users.med.t_4,paste0(vaccines_tmp,names(list_median_males)[vx_males_index],"_tab12_m_median_4.t_",vx_males_index, ".rds"))
          rm(res.tab12.m_users.med.t_4)
          
          #mean prescription by meaning, year and atc_code_4
          res.tab12.m_users.mean_4<-median_male[atc_level>=4,lapply(.SD,mean),.SDcols="count", by=.(meaning, year, atc_code_4)]
          setnames(res.tab12.m_users.mean_4,"count","mean_rx_male_users")
          res.tab12.m_users.mean_4[,mean_rx_male_users:=round(mean_rx_male_users,2)]
          res.tab12.m_users.mean_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          setcolorder(res.tab12.m_users.mean_4,c("meaning","year","atc_code_1","atc_code_3","atc_code_4"))
          saveRDS(res.tab12.m_users.mean_4,paste0(vaccines_tmp,names(list_median_males)[vx_males_index],"_tab12_m_mean_4.my_",vx_males_index, ".rds"))
          rm(res.tab12.m_users.mean_4)
          #mean prescriptions by atc_code_4
          res.tab12.m_users.mean.t_4<-median_male[atc_level>=4,lapply(.SD,sum),by=.(person_id,atc_code_4), .SDcols="count"][,lapply(.SD,mean),.SDcols="count", by=.(atc_code_4)]
          setnames(res.tab12.m_users.mean.t_4,"count","mean_rx_male_users")
          res.tab12.m_users.mean.t_4[,mean_rx_male_users:=round(mean_rx_male_users,2)]
          res.tab12.m_users.mean.t_4[,meaning:="All"][,year:="All"]
          res.tab12.m_users.mean.t_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          setcolorder(res.tab12.m_users.mean.t_4,c("meaning","year","atc_code_1","atc_code_3","atc_code_4"))
          saveRDS(res.tab12.m_users.mean.t_4,paste0(vaccines_tmp,names(list_median_males)[vx_males_index],"_tab12_m_mean_4.t_",vx_males_index, ".rds"))
          rm(res.tab12.m_users.mean.t_4)
          median_male[,atc_code_4:=NULL]
        }
        
        ##number of records, male users, median male users by atc_3 (table 12)
        if(median_male[atc_level==3,.N]>0){
          median_male<-median_male[,atc_code_3:=substr(vx_atc,1,3)] #create atc_code_3
          #number of records by meaning, year, and atc_code_3
          res.tab12.m_records.my<-median_male[atc_level==3,lapply(.SD, sum), by=.(atc_code_3, meaning, year),.SDcols="count"]
          setnames(res.tab12.m_records.my,"count","no_records")
          res.tab12.m_records.my[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
          saveRDS(res.tab12.m_records.my,paste0(vaccines_tmp,names(list_median_males)[vx_males_index], "_tab12_m_rec_3.my_", vx_males_index, ".rds"))
          rm(res.tab12.m_records.my)
          #number of records by atc_code_3
          res.tab12.m_records.t<-median_male[atc_level==3,lapply(.SD,sum),by=.(person_id,atc_code_3), .SDcols="count"][,lapply(.SD, sum), by=.(atc_code_3),.SDcols="count"]
          res.tab12.m_records.t[,meaning:="All"][,year:="All"]
          setnames(res.tab12.m_records.t,"count","no_records")
          res.tab12.m_records.t[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
          saveRDS(res.tab12.m_records.t,paste0(vaccines_tmp,names(list_median_males)[vx_males_index], "_tab12_m_rec_3.t_", vx_males_index, ".rds"))
          rm(res.tab12.m_records.t)
          #number of male users by meaning, year and atc_code_3
          res.tab12.m_users.c_3<-median_male[atc_level==3,lapply(.SD,function(x) length(unique(na.omit(x)))), by=.(atc_code_3, meaning, year),.SDcols="person_id"]
          setnames(res.tab12.m_users.c_3,"person_id","no_male_users")
          res.tab12.m_users.c_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
          saveRDS(res.tab12.m_users.c_3,paste0(vaccines_tmp,names(list_median_males)[vx_males_index], "_tab12_m_users_3.my_", vx_males_index, ".rds"))
          rm(res.tab12.m_users.c_3)
          #number of male users by atc_code_3
          res.tab12.m_users.t_3<-median_male[atc_level==3,lapply(.SD,sum),by=.(person_id,atc_code_3), .SDcols="count"][,lapply(.SD,function(x) length(unique(na.omit(x)))), by=.(atc_code_3),.SDcols="person_id"]
          setnames(res.tab12.m_users.t_3,"person_id","no_male_users")
          res.tab12.m_users.t_3[,meaning:="All"][,year:="All"]
          res.tab12.m_users.t_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
          saveRDS(res.tab12.m_users.t_3,paste0(vaccines_tmp,names(list_median_males)[vx_males_index], "_tab12_m_users_3.t_", vx_males_index, ".rds"))
          rm(res.tab12.m_users.t_3)
          #median prescription by meaning, year and atc_code_3
          res.tab12.m_users.med_3<-median_male[atc_level==3,lapply(.SD,median),.SDcols="count", by=.(meaning, year, atc_code_3)]
          setnames(res.tab12.m_users.med_3,"count","median_rx_male_users")
          res.tab12.m_users.med_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
          saveRDS(res.tab12.m_users.med_3,paste0(vaccines_tmp,names(list_median_males)[vx_males_index],"_tab12_m_median_3.my_",vx_males_index, ".rds"))
          rm(res.tab12.m_users.med_3)
          #median prescriptions by atc_code_3
          res.tab12.m_users.med.t_3<-median_male[atc_level==3,lapply(.SD,sum),by=.(person_id,atc_code_3), .SDcols="count"][,lapply(.SD,median),.SDcols="count", by=.(atc_code_3)]
          setnames(res.tab12.m_users.med.t_3,"count","median_rx_male_users")
          res.tab12.m_users.med.t_3[,meaning:="All"][,year:="All"]
          res.tab12.m_users.med.t_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
          saveRDS(res.tab12.m_users.med.t_3,paste0(vaccines_tmp,names(list_median_males)[vx_males_index],"_tab12_m_median_3.t_",vx_males_index, ".rds"))
          rm(res.tab12.m_users.med.t_3,res.tab12.m)
          
          #mean prescription by meaning, year and atc_code_3
          res.tab12.m_users.mean_3<-median_male[atc_level==3,lapply(.SD,mean),.SDcols="count", by=.(meaning, year, atc_code_3)]
          setnames(res.tab12.m_users.mean_3,"count","mean_rx_male_users")
          res.tab12.m_users.mean_3[mean_rx_male_users:=round(mean_rx_male_users,2)]
          res.tab12.m_users.mean_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
          saveRDS(res.tab12.m_users.mean_3,paste0(vaccines_tmp,names(list_median_males)[vx_males_index],"_tab12_m_mean_3.my_",vx_males_index, ".rds"))
          rm(res.tab12.m_users.mean_3)
          #mean prescriptions by atc_code_3
          res.tab12.m_users.mean.t_3<-median_male[atc_level==3,lapply(.SD,sum),by=.(person_id,atc_code_3), .SDcols="count"][,lapply(.SD,mean),.SDcols="count", by=.(atc_code_3)]
          setnames(res.tab12.m_users.mean.t_3,"count","mean_rx_male_users")
          res.tab12.m_users.mean.t_3[,mean_rx_male_users:=round(mean_rx_male_users,2)]
          res.tab12.m_users.mean.t_3[,meaning:="All"][,year:="All"]
          res.tab12.m_users.mean.t_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
          saveRDS(res.tab12.m_users.mean.t_3,paste0(vaccines_tmp,names(list_median_males)[vx_males_index],"_tab12_m_mean_3.t_",vx_males_index, ".rds"))
          rm(res.tab12.m_users.mean.t_3,res.tab12.m)
          median_male[,atc_code_3:=NULL]
        }
        
        #number of records, male users, median male users by atc_3 (table 12)
        if(median_male[atc_level==1,.N]>0){
          median_male<-median_male[,atc_code_1:=substr(vx_atc,1,1)] #create atc_code_1
          #number of records by meaning, year, and atc_code_1
          res.tab12.m_records.my<-median_male[atc_level==1,lapply(.SD, sum), by=.(atc_code_1, meaning, year),.SDcols="count"]
          setnames(res.tab12.m_records.my,"count","no_records")
          res.tab12.m_records.my[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.m_records.my,paste0(vaccines_tmp,names(list_median_males)[vx_males_index], "_tab12_m_rec_1.my_", vx_males_index, ".rds"))
          rm(res.tab12.m_records.my)
          #number of records by atc_code_1
          res.tab12.m_records.t<-median_male[atc_level==1,lapply(.SD,sum),by=.(person_id,atc_code_1), .SDcols="count"][,lapply(.SD, sum), by=.(atc_code_1),.SDcols="count"]
          res.tab12.m_records.t[,meaning:="All"][,year:="All"]
          setnames(res.tab12.m_records.t,"count","no_records")
          res.tab12.m_records.t[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.m_records.t,paste0(vaccines_tmp,names(list_median_males)[vx_males_index], "_tab12_m_rec_1.t_", vx_males_index, ".rds"))
          rm(res.tab12.m_records.t)
          #number of male users by meaning, year and atc_code_1
          res.tab12.m_users.c_1<-median_male[atc_level==1,lapply(.SD,function(x) length(unique(na.omit(x)))), by=.(atc_code_1, meaning, year),.SDcols="person_id"]
          setnames(res.tab12.m_users.c_1,"person_id","no_male_users")
          res.tab12.m_users.c_1[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.m_users.c_1,paste0(vaccines_tmp,names(list_median_males)[vx_males_index], "_tab12_m_users_1.my_", vx_males_index, ".rds"))
          rm(res.tab12.m_users.c_1)
          #number of male users by atc_code_1
          res.tab12.m_users.t_1<-median_male[,lapply(.SD,sum),by=.(person_id,atc_code_1), .SDcols="count"][,lapply(.SD,function(x) length(unique(na.omit(x)))), by=.(atc_code_1),.SDcols="person_id"]
          setnames(res.tab12.m_users.t_1,"person_id","no_male_users")
          res.tab12.m_users.t_1[,meaning:="All"][,year:="All"]
          res.tab12.m_users.t_1[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.m_users.t_1,paste0(vaccines_tmp,names(list_median_males)[vx_males_index], "_tab12_m_users_1.t_", vx_males_index, ".rds"))
          rm(res.tab12.m_users.t_1)
          #median prescription by meaning, year and atc_code_1
          rres.tab12.m_users.med_1<-median_male[atc_level==1,lapply(.SD,median),.SDcols="count", by=.(meaning, year, atc_code_1)]
          setnames(rres.tab12.m_users.med_1,"count","median_rx_male_users")
          rres.tab12.m_users.med_1[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(rres.tab12.m_users.med_1,paste0(vaccines_tmp,names(list_median_males)[vx_males_index],"_tab12_m_median_1.my_",vx_males_index, ".rds"))
          rm(rres.tab12.m_users.med_1)
          #median prescriptions by atc_code_1
          res.tab12.m_users.med.t_1<-median_male[atc_level==1,lapply(.SD,sum),by=.(person_id,atc_code_1), .SDcols="count"][,lapply(.SD,median),.SDcols="count", by=.(atc_code_1)]
          setnames(res.tab12.m_users.med.t_1,"count","median_rx_male_users")
          res.tab12.m_users.med.t_1[,meaning:="All"][,year:="All"]
          res.tab12.m_users.med.t_1[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.m_users.med.t_1,paste0(vaccines_tmp,names(list_median_males)[vx_males_index],"_tab12_m_median_1.t_",vx_males_index, ".rds"))
          rm(res.tab12.m_users.med.t_1,res.tab12.m)
          
          #mean prescription by meaning, year and atc_code_1
          res.tab12.m_users.mean_1<-median_male[atc_level==1,lapply(.SD,mean),.SDcols="count", by=.(meaning, year, atc_code_1)]
          setnames(res.tab12.m_users.mean_1,"count","mean_rx_male_users")
          res.tab12.m_users.mean_1[,mean_rx_male_users:=round(mean_rx_male_users,2)]
          res.tab12.m_users.mean_1[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.m_users.mean_1,paste0(vaccines_tmp,names(list_median_males)[vx_males_index],"_tab12_m_mean_1.my_",vx_males_index, ".rds"))
          rm(res.tab12.m_users.mean_1)
          #mean prescriptions by atc_code_1
          res.tab12.m_users.mean.t_1<-median_male[atc_level==1,lapply(.SD,sum),by=.(person_id,atc_code_1), .SDcols="count"][,lapply(.SD,mean),.SDcols="count", by=.(atc_code_1)]
          setnames(res.tab12.m_users.mean.t_1,"count","mean_rx_male_users")
          res.tab12.m_users.mean.t_1[mean_rx_male_users:=round(mean_rx_male_users,2)]
          res.tab12.m_users.mean.t_1[,meaning:="All"][,year:="All"]
          res.tab12.m_users.mean.t_1[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.m_users.mean.t_1,paste0(vaccines_tmp,names(list_median_males)[vx_males_index],"_tab12_m_mean_1.t_",vx_males_index, ".rds"))
          rm(res.tab12.m_users.mean.t_1,res.tab12.m)
          median_male[,atc_code_1:=NULL]
        }
      }
      print("Calculating number of male users and number of median prescription/dispensings stratified by ATC code, meaning and year.")
      rm(median_male)
      ########
      #Info input
      #tab12_m_rec_4.my: number of male records by meaning, year, atc_code_4
      #tab12_m_rec_3.my: number of male records by meaning, year, atc_code_3
      #tab12_m_rec_1.my: number of male records by meaning, year, atc_code_1
      #tab12_m_rec_4.t: total number of male records by atc_code_4
      #tab12_m_rec_3.t: total number of male records by atc_code_3
      #tab12_m_rec_1.t: total number of male records by atc_code_1
      #tab12_m_users_4.my: number of male users by meaning, year, atc_code_4
      #tab12_m_users_3.my: number of male users by meaning, year, atc_code_3
      #tab12_m_users_1.my: number of male users by meaning, year, atc_code_1
      #tab12_m_users_4.t: number of male users by atc_code_4
      #tab12_m_users_3.t: number of male users by atc_code_3
      #tab12_m_users_1.t: number of male users by atc_code_1
      #tab12_m_median_4.my: number of median presc/disp for male users by meaning, year, atc_code_4
      #tab12_m_median_3.my: number of median presc/disp for male users by meaning, year, atc_code_3
      #tab12_m_median_1.my: number of median presc/disp for male users by meaning, year, atc_code_1
      #tab12_m_median_4: number of median presc/disp for male users by atc_code_4
      #tab12_m_median_3: number of median presc/disp for male users by atc_code_3
      #tab12_m_median_1: number of median presc/disp for male users by atc_code_1
      #########
      
      ##########
      #my
      ##########
      
      #combine results for male users records
      tab12_male_rec.my<-c(list.files(vaccines_tmp,pattern="tab12_m_rec_4.my"),list.files(vaccines_tmp,pattern="tab12_m_rec_3.my"),list.files(vaccines_tmp,pattern="tab12_m_rec_1.my"))
      m_records.my<-lapply(paste0(vaccines_tmp,tab12_male_rec.my), readRDS)
      m_records.my<-do.call(rbind,m_records.my)
      for(i in 1:length(tab12_male_rec.my)){
        unlink(paste0(vaccines_tmp,tab12_male_rec.my[i]))
      }
      saveRDS(m_records.my,paste0(vaccines_tmp,"m_records.my.rds"))
      rm(tab12_male_rec.my,m_records.my)
      #output: m_records.my
      
      #combine results for male users 
      tab12_male_users.my<-c(list.files(vaccines_tmp,pattern="tab12_m_users_4.my"),list.files(vaccines_tmp,pattern="tab12_m_users_3.my"),list.files(vaccines_tmp,pattern="tab12_m_users_1.my"))
      #load all files and rbind together
      m_users.my<-lapply(paste0(vaccines_tmp,tab12_male_users.my), readRDS)
      m_users.my<-do.call(rbind,m_users.my)
      for(i in 1:length(tab12_male_users.my)){
        unlink(paste0(vaccines_tmp,tab12_male_users.my[i]))
      }
      saveRDS(m_users.my,paste0(vaccines_tmp,"m_users.my.rds"))
      rm(tab12_male_users.my,m_users.my)
      #output: m_users.my
      
      #combine results for median male
      tab12_male_median.my<-c(list.files(vaccines_tmp,pattern="tab12_m_median_4.my"),list.files(vaccines_tmp,pattern="tab12_m_median_3.my"),list.files(vaccines_tmp,pattern="tab12_m_median_1.my"))
      #load all files and rbind together
      m_median.my<-lapply(paste0(vaccines_tmp,tab12_male_median.my), readRDS)
      m_median.my<-do.call(rbind,m_median.my)
      for(i in 1:length(tab12_male_median.my)){
        unlink(paste0(vaccines_tmp,tab12_male_median.my[i]))
      }
      saveRDS(m_median.my,paste0(vaccines_tmp,"m_median.my.rds"))
      rm(tab12_male_median.my,m_median.my)
      #output: m_median.my
      
      #combine results for mean male
      tab12_male_mean.my<-c(list.files(vaccines_tmp,pattern="tab12_m_mean_4.my"),list.files(vaccines_tmp,pattern="tab12_m_mean_3.my"),list.files(vaccines_tmp,pattern="tab12_m_mean_1.my"))
      #load all files and rbind together
      m_mean.my<-lapply(paste0(vaccines_tmp,tab12_male_mean.my), readRDS)
      m_mean.my<-do.call(rbind,m_mean.my)
      for(i in 1:length(tab12_male_mean.my)){
        unlink(paste0(vaccines_tmp,tab12_male_mean.my[i]))
      }
      saveRDS(m_mean.my,paste0(vaccines_tmp,"m_mean.my.rds"))
      rm(tab12_male_mean.my,m_mean.my)
      #output: m_mean.my
      
      print("Calculating total number of male users and number of median prescription/dispensings stratified by ATC code.")
      ########
      #total
      ########
      #combine results for male users records
      tab12_male_rec.t<-c(list.files(vaccines_tmp,pattern="_tab12_m_rec_4.t_"),list.files(vaccines_tmp,pattern="_tab12_m_rec_3.t_"),list.files(vaccines_tmp,pattern="_tab12_m_rec_1.t_"))
      m_records.t<-lapply(paste0(vaccines_tmp,tab12_male_rec.t), readRDS)
      m_records.t<-do.call(rbind,m_records.t)
      for(i in 1:length(tab12_male_rec.t)){
        unlink(paste0(vaccines_tmp,tab12_male_rec.t[i]))
      }
      saveRDS(m_records.t,paste0(vaccines_tmp,"m_records.t.rds"))
      rm(tab12_male_rec.t,m_records.t)
      #output: m_records.t
      
      #male users total 
      tab12_male_users.t<-c(list.files(vaccines_tmp,pattern="tab12_m_users_4.t"),list.files(vaccines_tmp,pattern="tab12_m_users_3.t"),list.files(vaccines_tmp,pattern="tab12_m_users_1.t"))
      #load all files and rbind together
      m_users.t<-lapply(paste0(vaccines_tmp,tab12_male_users.t), readRDS)
      m_users.t<-do.call(rbind,m_users.t)
      for(i in 1:length(tab12_male_users.t)){
        unlink(paste0(vaccines_tmp,tab12_male_users.t[i]))
      }
      saveRDS(m_users.t,paste0(vaccines_tmp,"m_users.t.rds"))
      rm(tab12_male_users.t,m_users.t)
      #output: m_users.t
      
      tab12_male_median.t<-c(list.files(vaccines_tmp,pattern="tab12_m_median_4.t"),list.files(vaccines_tmp,pattern="tab12_m_median_3.t"),list.files(vaccines_tmp,pattern="tab12_m_median_1.t"))
      #load all files and rbind together
      m_median.t<-lapply(paste0(vaccines_tmp,tab12_male_median.t), readRDS)
      m_median.t<-do.call(rbind,m_median.t)
      for(i in 1:length(tab12_male_median.t)){
        unlink(paste0(vaccines_tmp,tab12_male_median.t[i]))
      }
      saveRDS(m_median.t,paste0(vaccines_tmp,"m_median.t.rds"))
      rm(tab12_male_median.t,m_median.t)
      #output: m_median.t
      ########
      tab12_male_mean.t<-c(list.files(vaccines_tmp,pattern="tab12_m_mean_4.t"),list.files(vaccines_tmp,pattern="tab12_m_mean_3.t"),list.files(vaccines_tmp,pattern="tab12_m_mean_1.t"))
      #load all files and rbind together
      m_mean.t<-lapply(paste0(vaccines_tmp,tab12_male_mean.t), readRDS)
      m_mean.t<-do.call(rbind,m_mean.t)
      for(i in 1:length(tab12_male_mean.t)){
        unlink(paste0(vaccines_tmp,tab12_male_mean.t[i]))
      }
      saveRDS(m_mean.t,paste0(vaccines_tmp,"m_mean.t.rds"))
      rm(tab12_male_mean.t,m_mean.t)
      #output: m_mean.t
    }
  } else {
    tab12_males<-data.table(meaning="N/A", year="N/A", atc_code_1="N/A", atc_code_3="N/A", atc_code_4="N/A", no_male_users="N/A", median_rx_male_users="N/A")
  }
  ##############################
  #female users and median prescriptions for female users
  #############################
  if(female_population>0){
    if(subpopulations_present=="Yes"){
      median_females_files<-list.files(paste0(vaccines_pop, subpopulations_names[s], "/"),pattern="f_population")
    } else {
      median_females_files<-list.files(vaccines_pop, pattern="f_population")
    }
    if(length(median_females_files)>0){
      #grab only the first letter which symoblized the ATC level 1
      letters_atc<-unique(sapply(median_females_files, function(x) substr(x,1,1))) 
      #create list for each letter
      list_median_females<-vector(mode="list", length=length(letters_atc))
      names(list_median_females)<-letters_atc
      for (i in 1:length(list_median_females)){
        list_median_females[[i]]<-median_females_files[substr(median_females_files,1,1)==names(list_median_females)[i]]
      }
      rm(letters_atc)
      
      vx_females_index<-1
      for (vx_females_index in 1:length(list_median_females)){
        if (subpopulations_present=="Yes"){
          median_female<-readRDS(paste0(vaccines_pop, subpopulations_names[s], "/", list_median_females[[vx_females_index]][1]))
        } else {
          median_female<-readRDS(paste0(vaccines_pop, list_median_females[[vx_females_index]][1]))
          
        }
        
        #count by person_id, atc code meaning and year
        median_female<-median_female[,.(count=.N), by=c("person_id","meaning","year","vx_atc","age_start_follow_up")]
        
        z<-2
        while (z <= length(list_median_females[[vx_females_index]])){
          if(subpopulations_present=="Yes"){
            a<-readRDS(paste0(vaccines_pop, subpopulations_names[s], "/", list_median_females[[vx_females_index]][[z]]))
          } else {
            a<-readRDS(paste0(vaccines_pop, list_median_females[[vx_females_index]][[z]]))
          }
          a<-a[,.(count=.N), by=c("person_id","meaning","year","vx_atc","age_start_follow_up")]
          median_female<-rbind(median_female, a)
          median_female<-median_female[,lapply(.SD, sum), by=c("person_id","meaning","year","vx_atc","age_start_follow_up"), .SDcols="count"]
          z<-z+1
          rm(a)
        }
        
        median_female<-median_female[,atc_level:=nchar(vx_atc)]
        #number of records, female users, median female users by atc_code_7 (table 13)
        if(median_female[atc_level==7,.N]>0){
          #number of records by meaning, year, and atc_code_7
          res.tab13.f_records.my<-median_female[atc_level==7,lapply(.SD, sum), by=.(vx_atc, meaning, year),.SDcols="count"]
          setnames(res.tab13.f_records.my,"count","no_records")
          setnames(res.tab13.f_records.my,"vx_atc","atc_code_7")
          res.tab13.f_records.my[,atc_code_3:=substr(atc_code_7,1,3)]
          setcolorder(res.tab13.f_records.my, c("meaning","year","atc_code_3","atc_code_7"))
          saveRDS(res.tab13.f_records.my,paste0(vaccines_tmp,names(list_median_females)[vx_females_index], "_tab13_f_rec_7.my_", vx_females_index, ".rds"))
          rm(res.tab13.f_records.my)
          #number of records by atc_code_7
          res.tab13.f_records.t<-median_female[atc_level==7,lapply(.SD,sum),by=.(person_id,vx_atc), .SDcols="count"][,lapply(.SD, sum), by=.(vx_atc),.SDcols="count"]
          res.tab13.f_records.t[,meaning:="All"][,year:="All"]
          setnames(res.tab13.f_records.t,"count","no_records")
          setnames(res.tab13.f_records.t,"vx_atc","atc_code_7")
          res.tab13.f_records.t[,atc_code_3:=substr(atc_code_7,1,3)]
          setcolorder(res.tab13.f_records.t, c("meaning","year","atc_code_3","atc_code_7"))
          saveRDS(res.tab13.f_records.t,paste0(vaccines_tmp,names(list_median_females)[vx_females_index], "_tab13_f_rec_7.t_", vx_females_index, ".rds"))
          rm(res.tab13.f_records.t)
          #number of female users by meaning, year and atc_code_7
          res.tab13.f_users.c_7<-median_female[atc_level==7,lapply(.SD,function(x) length(unique(na.omit(x)))), by=.(vx_atc, meaning, year),.SDcols="person_id"]
          setnames(res.tab13.f_users.c_7,"person_id","no_female_users")
          setnames(res.tab13.f_users.c_7,"vx_atc","atc_code_7")
          res.tab13.f_users.c_7[,atc_code_3:=substr(atc_code_7,1,3)]
          setcolorder(res.tab13.f_users.c_7, c("meaning","year","atc_code_3","atc_code_7"))
          saveRDS(res.tab13.f_users.c_7,paste0(vaccines_tmp,names(list_median_females)[vx_females_index], "_tab13_f_users_7.my_", vx_females_index, ".rds"))
          rm(res.tab13.f_users.c_7)
          #number of female users by atc_code_7
          res.tab13.f_users.t_7<-median_female[atc_level==7,lapply(.SD,sum),by=.(person_id,vx_atc), .SDcols="count"][,lapply(.SD,function(x) length(unique(na.omit(x)))), by=.(vx_atc),.SDcols="person_id"]
          setnames(res.tab13.f_users.t_7,"person_id","no_female_users")
          setnames(res.tab13.f_users.t_7,"vx_atc","atc_code_7")
          res.tab13.f_users.t_7[,meaning:="All"][,year:="All"]
          res.tab13.f_users.t_7[,atc_code_3:=substr(atc_code_7,1,3)]
          setcolorder(res.tab13.f_users.t_7, c("meaning","year","atc_code_3","atc_code_7"))
          saveRDS(res.tab13.f_users.t_7,paste0(vaccines_tmp,names(list_median_females)[vx_females_index], "_tab13_f_users_7.t_", vx_females_index, ".rds"))
          rm(res.tab13.f_users.t_7)
          
          
          #median prescription by meaning, year and atc_code_7
          res.tab13.f_users.med_7<-median_female[atc_level==7,lapply(.SD,median),.SDcols="count", by=.(meaning, year, vx_atc)]
          setnames(res.tab13.f_users.med_7,"count","median_rx_female_users")
          setnames(res.tab13.f_users.med_7,"vx_atc","atc_code_7")
          res.tab13.f_users.med_7[,atc_code_3:=substr(atc_code_7,1,3)]
          setcolorder(res.tab13.f_users.med_7, c("meaning","year","atc_code_3","atc_code_7"))
          saveRDS(res.tab13.f_users.med_7,paste0(vaccines_tmp,names(list_median_females)[vx_females_index],"_tab13_f_median_7.my_",vx_females_index, ".rds"))
          rm(res.tab13.f_users.med_7)
          #median prescriptions by atc_code_7
          res.tab13.f_users.med.t_7<-median_female[atc_level==7,lapply(.SD,sum),by=.(person_id,vx_atc), .SDcols="count"][,lapply(.SD,median),.SDcols="count", by=.(vx_atc)]
          setnames(res.tab13.f_users.med.t_7,"count","median_rx_female_users")
          setnames(res.tab13.f_users.med.t_7,"vx_atc","atc_code_7")
          res.tab13.f_users.med.t_7[,meaning:="All"][,year:="All"]
          res.tab13.f_users.med.t_7[,atc_code_3:=substr(atc_code_7,1,3)]
          setcolorder(res.tab13.f_users.med.t_7, c("meaning","year","atc_code_3","atc_code_7"))
          saveRDS(res.tab13.f_users.med.t_7,paste0(vaccines_tmp,names(list_median_females)[vx_females_index],"_tab13_f_median_7.t_",vx_females_index, ".rds"))
          rm(res.tab13.f_users.med.t_7)
          
          #mean prescription by meaning, year and atc_code_7
          res.tab13.f_users.mean_7<-median_female[atc_level==7,lapply(.SD,mean),.SDcols="count", by=.(meaning, year, vx_atc)]
          setnames(res.tab13.f_users.mean_7,"count","mean_rx_female_users")
          res.tab13.f_users.mean_7[,mean_rx_female_users:=round(mean_rx_female_users,2)]
          setnames(res.tab13.f_users.mean_7,"vx_atc","atc_code_7")
          res.tab13.f_users.mean_7[,atc_code_3:=substr(atc_code_7,1,3)]
          setcolorder(res.tab13.f_users.mean_7, c("meaning","year","atc_code_3","atc_code_7"))
          saveRDS(res.tab13.f_users.mean_7,paste0(vaccines_tmp,names(list_median_females)[vx_females_index],"_tab13_f_mean_7.my_",vx_females_index, ".rds"))
          rm(res.tab13.f_users.mean_7)
          #mean prescriptions by atc_code_7
          res.tab13.f_users.mean.t_7<-median_female[atc_level==7,lapply(.SD,sum),by=.(person_id,vx_atc), .SDcols="count"][,lapply(.SD,mean),.SDcols="count", by=.(vx_atc)]
          setnames(res.tab13.f_users.mean.t_7,"count","mean_rx_female_users")
          res.tab13.f_users.mean.t_7[,mean_rx_female_users:=round(mean_rx_female_users,2)]
          setnames(res.tab13.f_users.mean.t_7,"vx_atc","atc_code_7")
          res.tab13.f_users.mean.t_7[,meaning:="All"][,year:="All"]
          res.tab13.f_users.mean.t_7[,atc_code_3:=substr(atc_code_7,1,3)]
          setcolorder(res.tab13.f_users.mean.t_7, c("meaning","year","atc_code_3","atc_code_7"))
          saveRDS(res.tab13.f_users.mean.t_7,paste0(vaccines_tmp,names(list_median_females)[vx_females_index],"_tab13_f_mean_7.t_",vx_females_index, ".rds"))
          rm(res.tab13.f_users.mean.t_7)
        }
        
        #number of records, female users, median female users by atc_code_7 (table 14) in females of childbearing age
        if(median_female[atc_level==7 & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,.N]>0){
          ##########users by meaning and year
          #number of records by meaning, year, and atc_code_7
          res.tab12.f_records.my<-median_female[atc_level==7 & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,lapply(.SD, sum), by=.(vx_atc, meaning, year),.SDcols="count"]
          setnames(res.tab12.f_records.my,"count","no_records")
          setnames(res.tab12.f_records.my,"vx_atc","atc_code_7")
          res.tab12.f_records.my[,atc_code_3:=substr(atc_code_7,1,3)]
          setcolorder(res.tab12.f_records.my, c("meaning","year","atc_code_3","atc_code_7"))
          saveRDS(res.tab12.f_records.my,paste0(vaccines_tmp,names(list_median_females)[vx_females_index], "_tab14_f_rec_7.my_", vx_females_index, ".rds"))
          rm(res.tab12.f_records.my)
          #number of records by atc_code_7
          res.tab12.f_records.t<-median_female[atc_level==7 & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,lapply(.SD,sum),by=.(person_id,vx_atc), .SDcols="count"][,lapply(.SD, sum), by=.(vx_atc),.SDcols="count"]
          res.tab12.f_records.t[,meaning:="All"][,year:="All"]
          setnames(res.tab12.f_records.t,"count","no_records")
          setnames(res.tab12.f_records.t,"vx_atc","atc_code_7")
          res.tab12.f_records.t[,atc_code_3:=substr(atc_code_7,1,3)]
          setcolorder(res.tab12.f_records.t, c("meaning","year","atc_code_3","atc_code_7"))
          saveRDS(res.tab12.f_records.t,paste0(vaccines_tmp,names(list_median_females)[vx_females_index], "_tab14_f_rec_7.t_", vx_females_index, ".rds"))
          rm(res.tab12.f_records.t)
          #number of female users by meaning, year and atc_code_7
          res.tab12.f_users.c_7<-median_female[atc_level==7 & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,lapply(.SD,function(x) length(unique(na.omit(x)))), by=.(vx_atc, meaning, year),.SDcols="person_id"]
          setnames(res.tab12.f_users.c_7,"person_id","no_female_users")
          setnames(res.tab12.f_users.c_7,"vx_atc","atc_code_7")
          res.tab12.f_users.c_7[,atc_code_3:=substr(atc_code_7,1,3)]
          setcolorder(res.tab12.f_users.c_7, c("meaning","year","atc_code_3","atc_code_7"))
          saveRDS(res.tab12.f_users.c_7,paste0(vaccines_tmp,names(list_median_females)[vx_females_index], "_tab14_f_users_7.my_", vx_females_index, ".rds"))
          rm(res.tab12.f_users.c_7)
          #number of female users by atc_code_7
          res.tab12.f_users.t_7<-median_female[atc_level==7 & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,lapply(.SD,sum),by=.(person_id,vx_atc), .SDcols="count"][,lapply(.SD,function(x) length(unique(na.omit(x)))), by=.(vx_atc),.SDcols="person_id"]
          setnames(res.tab12.f_users.t_7,"person_id","no_female_users")
          setnames(res.tab12.f_users.t_7,"vx_atc","atc_code_7")
          res.tab12.f_users.t_7[,meaning:="All"][,year:="All"]
          res.tab12.f_users.t_7[,atc_code_3:=substr(atc_code_7,1,3)]
          setcolorder(res.tab12.f_users.t_7, c("meaning","year","atc_code_3","atc_code_7"))
          saveRDS(res.tab12.f_users.t_7,paste0(vaccines_tmp,names(list_median_females)[vx_females_index], "_tab14_f_users_7.t_", vx_females_index, ".rds"))
          rm(res.tab12.f_users.t_7)
          #median prescription by meaning, year and atc_code_7
          res.tab12.f_users.med_7<-median_female[atc_level==7 & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,lapply(.SD,median),.SDcols="count", by=.(meaning, year, vx_atc)]
          setnames(res.tab12.f_users.med_7,"count","median_rx_female_users")
          setnames(res.tab12.f_users.med_7,"vx_atc","atc_code_7")
          res.tab12.f_users.med_7[,atc_code_3:=substr(atc_code_7,1,3)]
          setcolorder(res.tab12.f_users.med_7, c("meaning","year","atc_code_3","atc_code_7"))
          saveRDS(res.tab12.f_users.med_7,paste0(vaccines_tmp,names(list_median_females)[vx_females_index],"_tab14_f_median_7.my_",vx_females_index, ".rds"))
          rm(res.tab12.f_users.med_7)
          #median prescriptions by atc_code_7
          res.tab12.f_users.med.t_7<-median_female[atc_level==7 & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,lapply(.SD,sum),by=.(person_id,vx_atc), .SDcols="count"][,lapply(.SD,median),.SDcols="count", by=.(vx_atc)]
          setnames(res.tab12.f_users.med.t_7,"count","median_rx_female_users")
          setnames(res.tab12.f_users.med.t_7,"vx_atc","atc_code_7")
          res.tab12.f_users.med.t_7[,meaning:="All"][,year:="All"]
          res.tab12.f_users.med.t_7[,atc_code_3:=substr(atc_code_7,1,3)]
          setcolorder(res.tab12.f_users.med.t_7, c("meaning","year","atc_code_3","atc_code_7"))
          saveRDS(res.tab12.f_users.med.t_7,paste0(vaccines_tmp,names(list_median_females)[vx_females_index],"_tab14_f_median_7.t_",vx_females_index, ".rds"))
          rm(res.tab12.f_users.med.t_7)
          
          #mean prescription by meaning, year and atc_code_7
          res.tab12.f_users.mean_7<-median_female[atc_level==7 & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,lapply(.SD,mean),.SDcols="count", by=.(meaning, year, vx_atc)]
          setnames(res.tab12.f_users.mean_7,"count","mean_rx_female_users")
          res.tab12.f_users.mean_7[,mean_rx_female_users:=round(mean_rx_female_users,2)]
          setnames(res.tab12.f_users.mean_7,"vx_atc","atc_code_7")
          res.tab12.f_users.mean_7[,atc_code_3:=substr(atc_code_7,1,3)]
          setcolorder(res.tab12.f_users.mean_7, c("meaning","year","atc_code_3","atc_code_7"))
          saveRDS(res.tab12.f_users.mean_7,paste0(vaccines_tmp,names(list_median_females)[vx_females_index],"_tab14_f_mean_7.my_",vx_females_index, ".rds"))
          rm(res.tab12.f_users.mean_7)
          #mean prescriptions by atc_code_7
          res.tab12.f_users.mean.t_7<-median_female[atc_level==7 & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,lapply(.SD,sum),by=.(person_id,vx_atc), .SDcols="count"][,lapply(.SD,mean),.SDcols="count", by=.(vx_atc)]
          setnames(res.tab12.f_users.mean.t_7,"count","mean_rx_female_users")
          res.tab12.f_users.mean.t_7[,mean_rx_female_users:=round(mean_rx_female_users,2)]
          setnames(res.tab12.f_users.mean.t_7,"vx_atc","atc_code_7")
          res.tab12.f_users.mean.t_7[,meaning:="All"][,year:="All"]
          res.tab12.f_users.mean.t_7[,atc_code_3:=substr(atc_code_7,1,3)]
          setcolorder(res.tab12.f_users.mean.t_7, c("meaning","year","atc_code_3","atc_code_7"))
          saveRDS(res.tab12.f_users.mean.t_7,paste0(vaccines_tmp,names(list_median_females)[vx_females_index],"_tab14_f_mean_7.t_",vx_females_index, ".rds"))
          rm(res.tab12.f_users.mean.t_7)
        }
        
        #############
        #number of records, female users, median female users (table 12)
        if(median_female[atc_level>=4,.N]>0){
          median_female<-median_female[,atc_code_4:=substr(vx_atc,1,4)] #create atc_code_4
          #number of records by meaning, year, and atc_code_4
          res.tab12.f_records.my<-median_female[atc_level>=4,lapply(.SD, sum), by=.(atc_code_4, meaning, year),.SDcols="count"]
          setnames(res.tab12.f_records.my,"count","no_records")
          res.tab12.f_records.my[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          saveRDS(res.tab12.f_records.my,paste0(vaccines_tmp,names(list_median_females)[vx_females_index], "_tab12_f_rec_4.my_", vx_females_index, ".rds"))
          rm(res.tab12.f_records.my)
          #number of records by atc_code_4
          res.tab12.f_records.t<-median_female[atc_level>=4,lapply(.SD,sum),by=.(person_id,atc_code_4), .SDcols="count"][,lapply(.SD, sum), by=.(atc_code_4),.SDcols="count"]
          res.tab12.f_records.t[,meaning:="All"][,year:="All"]
          setnames(res.tab12.f_records.t,"count","no_records")
          res.tab12.f_records.t[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          saveRDS(res.tab12.f_records.t,paste0(vaccines_tmp,names(list_median_females)[vx_females_index], "_tab12_f_rec_4.t_", vx_females_index, ".rds"))
          rm(res.tab12.f_records.t)
          #number of female users by meaning, year and atc_code_4
          res.tab12.f_users.c_4<-median_female[atc_level>=4,lapply(.SD,function(x) length(unique(na.omit(x)))), by=.(atc_code_4, meaning, year),.SDcols="person_id"]
          setnames(res.tab12.f_users.c_4,"person_id","no_female_users")
          res.tab12.f_users.c_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          saveRDS(res.tab12.f_users.c_4,paste0(vaccines_tmp,names(list_median_females)[vx_females_index], "_tab12_f_users_4.my_", vx_females_index, ".rds"))
          rm(res.tab12.f_users.c_4)
          #number of female users by atc_code_4
          res.tab12.f_users.t_4<-median_female[atc_level>=4,lapply(.SD,sum),by=.(person_id,atc_code_4), .SDcols="count"][,lapply(.SD,function(x) length(unique(na.omit(x)))), by=.(atc_code_4),.SDcols="person_id"]
          setnames(res.tab12.f_users.t_4,"person_id","no_female_users")
          res.tab12.f_users.t_4[,meaning:="All"][,year:="All"]
          res.tab12.f_users.t_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          saveRDS(res.tab12.f_users.t_4,paste0(vaccines_tmp,names(list_median_females)[vx_females_index], "_tab12_f_users_4.t_", vx_females_index, ".rds"))
          rm(res.tab12.f_users.t_4)
          #median prescription by meaning, year and atc_code_4
          res.tab12.f_users.med_4<-median_female[atc_level>=4,lapply(.SD,median),.SDcols="count", by=.(meaning, year, atc_code_4)]
          setnames(res.tab12.f_users.med_4,"count","median_rx_female_users")
          res.tab12.f_users.med_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          saveRDS(res.tab12.f_users.med_4,paste0(vaccines_tmp,names(list_median_females)[vx_females_index],"_tab12_f_median_4.my_",vx_females_index, ".rds"))
          rm(res.tab12.f_users.med_4)
          #median prescriptions by atc_code_4
          res.tab12.f_users.med.t_4<-median_female[atc_level>=4,lapply(.SD,sum),by=.(person_id,atc_code_4), .SDcols="count"][,lapply(.SD,median),.SDcols="count", by=.(atc_code_4)]
          setnames(res.tab12.f_users.med.t_4,"count","median_rx_female_users")
          res.tab12.f_users.med.t_4[,meaning:="All"][,year:="All"]
          res.tab12.f_users.med.t_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          saveRDS(res.tab12.f_users.med.t_4,paste0(vaccines_tmp,names(list_median_females)[vx_females_index],"_tab12_f_median_4.t_",vx_females_index, ".rds"))
          rm(res.tab12.f_users.med.t_4)
          
          #mean prescription by meaning, year and atc_code_4
          res.tab12.f_users.mean_4<-median_female[atc_level>=4,lapply(.SD,mean),.SDcols="count", by=.(meaning, year, atc_code_4)]
          setnames(res.tab12.f_users.mean_4,"count","mean_rx_female_users")
          res.tab12.f_users.mean_4[,mean_rx_female_users:=round(mean_rx_female_users,2)]
          res.tab12.f_users.mean_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          saveRDS(res.tab12.f_users.mean_4,paste0(vaccines_tmp,names(list_median_females)[vx_females_index],"_tab12_f_mean_4.my_",vx_females_index, ".rds"))
          rm(res.tab12.f_users.mean_4)
          #mean prescriptions by atc_code_4
          res.tab12.f_users.mean.t_4<-median_female[atc_level>=4,lapply(.SD,sum),by=.(person_id,atc_code_4), .SDcols="count"][,lapply(.SD,mean),.SDcols="count", by=.(atc_code_4)]
          setnames(res.tab12.f_users.mean.t_4,"count","mean_rx_female_users")
          res.tab12.f_users.mean.t_4[,mean_rx_female_users:=round(mean_rx_female_users,2)]
          res.tab12.f_users.mean.t_4[,meaning:="All"][,year:="All"]
          res.tab12.f_users.mean.t_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          saveRDS(res.tab12.f_users.mean.t_4,paste0(vaccines_tmp,names(list_median_females)[vx_females_index],"_tab12_f_mean_4.t_",vx_females_index, ".rds"))
          rm(res.tab12.f_users.mean.t_4)
          median_female[,atc_code_4:=NULL]
        }
        
        ##number of records, female users, median female users by atc_3 (table 12)
        if(median_female[atc_level==3,.N]>0){
          median_female<-median_female[,atc_code_3:=substr(vx_atc,1,3)] #create atc_code_3
          #number of records by meaning, year, and atc_code_3
          res.tab12.f_records.my<-median_female[atc_level==3,lapply(.SD, sum), by=.(atc_code_3, meaning, year),.SDcols="count"]
          setnames(res.tab12.f_records.my,"count","no_records")
          res.tab12.f_records.my[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
          saveRDS(res.tab12.f_records.my,paste0(vaccines_tmp,names(list_median_females)[vx_females_index], "_tab12_f_rec_3.my_", vx_females_index, ".rds"))
          rm(res.tab12.f_records.my)
          #number of records by atc_code_3
          res.tab12.f_records.t<-median_female[atc_level==3,lapply(.SD,sum),by=.(person_id,atc_code_3), .SDcols="count"][,lapply(.SD, sum), by=.(atc_code_3),.SDcols="count"]
          res.tab12.f_records.t[,meaning:="All"][,year:="All"]
          setnames(res.tab12.f_records.t,"count","no_records")
          res.tab12.f_records.t[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
          saveRDS(res.tab12.f_records.t,paste0(vaccines_tmp,names(list_median_females)[vx_females_index], "_tab12_f_rec_3.t_", vx_females_index, ".rds"))
          rm(res.tab12.f_records.t)
          #number of female users by meaning, year and atc_code_3
          res.tab12.f_users.c_3<-median_female[atc_level==3,lapply(.SD,function(x) length(unique(na.omit(x)))), by=.(atc_code_3, meaning, year),.SDcols="person_id"]
          setnames(res.tab12.f_users.c_3,"person_id","no_female_users")
          res.tab12.f_users.c_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
          saveRDS(res.tab12.f_users.c_3,paste0(vaccines_tmp,names(list_median_females)[vx_females_index], "_tab12_f_users_3.my_", vx_females_index, ".rds"))
          rm(res.tab12.f_users.c_3)
          #number of female users by atc_code_3
          res.tab12.f_users.t_3<-median_female[atc_level==3,lapply(.SD,sum),by=.(person_id,atc_code_3), .SDcols="count"][,lapply(.SD,function(x) length(unique(na.omit(x)))), by=.(atc_code_3),.SDcols="person_id"]
          setnames(res.tab12.f_users.t_3,"person_id","no_female_users")
          res.tab12.f_users.t_3[,meaning:="All"][,year:="All"]
          res.tab12.f_users.t_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
          saveRDS(res.tab12.f_users.t_3,paste0(vaccines_tmp,names(list_median_females)[vx_females_index], "_tab12_f_users_3.t_", vx_females_index, ".rds"))
          rm(res.tab12.f_users.t_3)
          #median prescription by meaning, year and atc_code_3
          res.tab12.f_users.med_3<-median_female[atc_level==3,lapply(.SD,median),.SDcols="count", by=.(meaning, year, atc_code_3)]
          setnames(res.tab12.f_users.med_3,"count","median_rx_female_users")
          res.tab12.f_users.med_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
          saveRDS(res.tab12.f_users.med_3,paste0(vaccines_tmp,names(list_median_females)[vx_females_index],"_tab12_f_median_3.my_",vx_females_index, ".rds"))
          rm(res.tab12.f_users.med_3)
          #median prescriptions by atc_code_3
          res.tab12.f_users.med.t_3<-median_female[atc_level==3,lapply(.SD,sum),by=.(person_id,atc_code_3), .SDcols="count"][,lapply(.SD,median),.SDcols="count", by=.(atc_code_3)]
          setnames(res.tab12.f_users.med.t_3,"count","median_rx_female_users")
          res.tab12.f_users.med.t_3[,meaning:="All"][,year:="All"]
          res.tab12.f_users.med.t_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
          saveRDS(res.tab12.f_users.med.t_3,paste0(vaccines_tmp,names(list_median_females)[vx_females_index],"_tab12_f_median_3.t_",vx_females_index, ".rds"))
          rm(res.tab12.f_users.med.t_3)
          
          #mean prescription by meaning, year and atc_code_3
          res.tab12.f_users.mean_3<-median_female[atc_level==3,lapply(.SD,mean),.SDcols="count", by=.(meaning, year, atc_code_3)]
          setnames(res.tab12.f_users.mean_3,"count","mean_rx_female_users")
          res.tab12.f_users.mean_3[,mean_rx_female_users:=round(mean_rx_female_users,2)]
          res.tab12.f_users.mean_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
          saveRDS(res.tab12.f_users.mean_3,paste0(vaccines_tmp,names(list_median_females)[vx_females_index],"_tab12_f_mean_3.my_",vx_females_index, ".rds"))
          rm(res.tab12.f_users.mean_3)
          #mean prescriptions by atc_code_3
          res.tab12.f_users.mean.t_3<-median_female[atc_level==3,lapply(.SD,sum),by=.(person_id,atc_code_3), .SDcols="count"][,lapply(.SD,mean),.SDcols="count", by=.(atc_code_3)]
          setnames(res.tab12.f_users.mean.t_3,"count","mean_rx_female_users")
          res.tab12.f_users.mean.t_3[,mean_rx_female_users:=round(mean_rx_female_users,2)]
          res.tab12.f_users.mean.t_3[,meaning:="All"][,year:="All"]
          res.tab12.f_users.mean.t_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
          saveRDS(res.tab12.f_users.mean.t_3,paste0(vaccines_tmp,names(list_median_females)[vx_females_index],"_tab12_f_mean_3.t_",vx_females_index, ".rds"))
          rm(res.tab12.f_users.mean.t_3)
          median_female[,atc_code_3:=NULL]
        }
        
        #number of records, female users, median female users by atc_3 (table 12)
        if(median_female[atc_level==1,.N]>0){
          median_female<-median_female[,atc_code_1:=substr(vx_atc,1,1)] #create atc_code_1
          #number of records by meaning, year, and atc_code_1
          res.tab12.f_records.my<-median_female[atc_level==1,lapply(.SD, sum), by=.(atc_code_1, meaning, year),.SDcols="count"]
          setnames(res.tab12.f_records.my,"count","no_records")
          res.tab12.f_records.my[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.f_records.my,paste0(vaccines_tmp,names(list_median_females)[vx_females_index], "_tab12_f_rec_1.my_", vx_females_index, ".rds"))
          rm(res.tab12.f_records.my)
          #number of records by atc_code_1
          res.tab12.f_records.t<-median_female[atc_level==1,lapply(.SD,sum),by=.(person_id,atc_code_1), .SDcols="count"][,lapply(.SD, sum), by=.(atc_code_1),.SDcols="count"]
          res.tab12.f_records.t[,meaning:="All"][,year:="All"]
          setnames(res.tab12.f_records.t,"count","no_records")
          res.tab12.f_records.t[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.f_records.t,paste0(vaccines_tmp,names(list_median_females)[vx_females_index], "_tab12_f_rec_1.t_", vx_females_index, ".rds"))
          rm(res.tab12.f_records.t)
          #number of female users by meaning, year and atc_code_1
          res.tab12.f_users.c_1<-median_female[atc_level==1,lapply(.SD,function(x) length(unique(na.omit(x)))), by=.(atc_code_1, meaning, year),.SDcols="person_id"]
          setnames(res.tab12.f_users.c_1,"person_id","no_female_users")
          res.tab12.f_users.c_1[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.f_users.c_1,paste0(vaccines_tmp,names(list_median_females)[vx_females_index], "_tab12_f_users_1.my_", vx_females_index, ".rds"))
          rm(res.tab12.f_users.c_1)
          #number of female users by atc_code_1
          res.tab12.f_users.t_1<-median_female[atc_level==1,lapply(.SD,sum),by=.(person_id,atc_code_1), .SDcols="count"][,lapply(.SD,function(x) length(unique(na.omit(x)))), by=.(atc_code_1),.SDcols="person_id"]
          setnames(res.tab12.f_users.t_1,"person_id","no_female_users")
          res.tab12.f_users.t_1[,meaning:="All"][,year:="All"]
          res.tab12.f_users.t_1[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.f_users.t_1,paste0(vaccines_tmp,names(list_median_females)[vx_females_index], "_tab12_f_users_1.t_", vx_females_index, ".rds"))
          rm(res.tab12.f_users.t_1)
          #median prescription by meaning, year and atc_code_1
          res.tab12.f_users.med_1<-median_female[atc_level==1,lapply(.SD,median),.SDcols="count", by=.(meaning, year, atc_code_1)]
          setnames(res.tab12.f_users.med_1,"count","median_rx_female_users")
          res.tab12.f_users.med_1[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.f_users.med_1,paste0(vaccines_tmp,names(list_median_females)[vx_females_index],"_tab12_f_median_1.my_",vx_females_index, ".rds"))
          rm(res.tab12.f_users.med_1)
          #median prescriptions by atc_code_1
          res.tab12.f_users.med.t_1<-median_female[atc_level==1,lapply(.SD,sum),by=.(person_id,atc_code_1), .SDcols="count"][,lapply(.SD,median),.SDcols="count", by=.(atc_code_1)]
          setnames(res.tab12.f_users.med.t_1,"count","median_rx_female_users")
          res.tab12.f_users.med.t_1[,meaning:="All"][,year:="All"]
          res.tab12.f_users.med.t_1[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.f_users.med.t_1,paste0(vaccines_tmp,names(list_median_females)[vx_females_index],"_tab12_f_median_1.t_",vx_females_index, ".rds"))
          rm(res.tab12.f_users.med.t_1)
          
          #mean prescription by meaning, year and atc_code_1
          res.tab12.f_users.mean_1<-median_female[atc_level==1,lapply(.SD,mean),.SDcols="count", by=.(meaning, year, atc_code_1)]
          setnames(res.tab12.f_users.mean_1,"count","mean_rx_female_users")
          res.tab12.f_users.mean_1[,mean_rx_female_users:=round(mean_rx_female_users,2)]
          res.tab12.f_users.mean_1[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.f_users.mean_1,paste0(vaccines_tmp,names(list_median_females)[vx_females_index],"_tab12_f_mean_1.my_",vx_females_index, ".rds"))
          rm(res.tab12.f_users.mean_1)
          #mean prescriptions by atc_code_1
          res.tab12.f_users.mean.t_1<-median_female[atc_level==1,lapply(.SD,sum),by=.(person_id,atc_code_1), .SDcols="count"][,lapply(.SD,mean),.SDcols="count", by=.(atc_code_1)]
          setnames(res.tab12.f_users.mean.t_1,"count","mean_rx_female_users")
          res.tab12.f_users.mean.t_1[,mean_rx_female_users:=round(mean_rx_female_users,2)]
          res.tab12.f_users.mean.t_1[,meaning:="All"][,year:="All"]
          res.tab12.f_users.mean.t_1[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.f_users.mean.t_1,paste0(vaccines_tmp,names(list_median_females)[vx_females_index],"_tab12_f_mean_1.t_",vx_females_index, ".rds"))
          rm(res.tab12.f_users.mean.t_1)
          median_female[,atc_code_1:=NULL]
        }
      }
      rm(median_female)
      print("Calculating number of female users and number of median prescription/dispensings stratified by ATC code, meaning and year.")
      
      ########
      #Info input
      #tab12_f_rec_4.my: number of female records by meaning, year, atc_code_4
      #tab12_f_rec_3.my: number of female records by meaning, year, atc_code_3
      #tab12_f_rec_1.my: number of female records by meaning, year, atc_code_1
      #tab12_f_rec_4.t: total number of female records by atc_code_4
      #tab12_f_rec_3.t: total number of female records by atc_code_3
      #tab12_f_rec_1.t: total number of female records by atc_code_1
      #tab12_f_users_4.my: number of female users by meaning, year, atc_code_4
      #tab12_f_users_3.my: number of female users by meaning, year, atc_code_3
      #tab12_f_users_1.my: number of female users by meaning, year, atc_code_1
      #tab12_f_users_4.t: number of female users by atc_code_4
      #tab12_f_users_3.t: number of female users by atc_code_3
      #tab12_f_users_1.t: number of female users by atc_code_1
      #tab12_f_median_4.my: number of median presc/disp for female users by meaning, year, atc_code_4
      #tab12_f_median_3.my: number of median presc/disp for female users by meaning, year, atc_code_3
      #tab12_f_median_1.my: number of median presc/disp for female users by meaning, year, atc_code_1
      #tab12_f_median_4.t: number of median presc/disp for female users by atc_code_4
      #tab12_f_median_3.t: number of median presc/disp for female users by atc_code_3
      #tab12_f_median_1.t: number of median presc/disp for female users by atc_code_1
      #########
      
      ##########
      #my
      ##########
      
      #combine results for female users records
      tab12_female_rec.my<-c(list.files(vaccines_tmp,pattern="tab12_f_rec_4.my"),list.files(vaccines_tmp,pattern="tab12_f_rec_3.my"),list.files(vaccines_tmp,pattern="tab12_f_rec_1.my"))
      f_records.my<-lapply(paste0(vaccines_tmp,tab12_female_rec.my), readRDS)
      f_records.my<-do.call(rbind,f_records.my)
      for(i in 1:length(tab12_female_rec.my)){
        unlink(paste0(vaccines_tmp,tab12_female_rec.my[i]))
      }
      saveRDS(f_records.my,paste0(vaccines_tmp,"f_records.my.rds"))
      rm(tab12_female_rec.my,f_records.my)
      #output: f_records.my
      
      #combine results for female users 
      tab12_female_users.my<-c(list.files(vaccines_tmp,pattern="tab12_f_users_4.my"),list.files(vaccines_tmp,pattern="tab12_f_users_3.my"),list.files(vaccines_tmp,pattern="tab12_f_users_1.my"))
      #load all files and rbind together
      f_users.my<-lapply(paste0(vaccines_tmp,tab12_female_users.my), readRDS)
      f_users.my<-do.call(rbind,f_users.my)
      for(i in 1:length(tab12_female_users.my)){
        unlink(paste0(vaccines_tmp,tab12_female_users.my[i]))
      }
      saveRDS(f_users.my,paste0(vaccines_tmp,"f_users.my.rds"))
      rm(tab12_female_users.my,f_users.my)
      #output: f_users.my
      
      #median
      tab12_female_median.my<-c(list.files(vaccines_tmp,pattern="tab12_f_median_4.my"),list.files(vaccines_tmp,pattern="tab12_f_median_3.my"),list.files(vaccines_tmp,pattern="tab12_f_median_1.my"))
      f_median.my<-lapply(paste0(vaccines_tmp,tab12_female_median.my), readRDS)
      f_median.my<-do.call(rbind,f_median.my)
      for(i in 1:length(tab12_female_median.my)){
        unlink(paste0(vaccines_tmp,tab12_female_median.my[i]))
      }
      saveRDS(f_median.my,paste0(vaccines_tmp,"f_median.my.rds"))
      rm(tab12_female_median.my,f_median.my)
      #output: f_median.my
      
      #mean
      tab12_female_mean.my<-c(list.files(vaccines_tmp,pattern="tab12_f_mean_4.my"),list.files(vaccines_tmp,pattern="tab12_f_mean_3.my"),list.files(vaccines_tmp,pattern="tab12_f_mean_1.my"))
      f_mean.my<-lapply(paste0(vaccines_tmp,tab12_female_mean.my), readRDS)
      f_mean.my<-do.call(rbind,f_mean.my)
      for(i in 1:length(tab12_female_mean.my)){
        unlink(paste0(vaccines_tmp,tab12_female_mean.my[i]))
      }
      saveRDS(f_mean.my,paste0(vaccines_tmp,"f_mean.my.rds"))
      rm(tab12_female_mean.my,f_mean.my)
      #output: f_mean.my
      
      print("Calculating total number of female users and number of median prescription/dispensings stratified by ATC code.")
      ########
      #total
      ########
      #combine results for female users records
      tab12_female_rec.t<-c(list.files(vaccines_tmp,pattern="tab12_f_rec_4.t"),list.files(vaccines_tmp,pattern="tab12_f_rec_3.t"),list.files(vaccines_tmp,pattern="tab12_f_rec_1.t"))
      f_records.t<-lapply(paste0(vaccines_tmp,tab12_female_rec.t), readRDS)
      f_records.t<-do.call(rbind,f_records.t)
      for(i in 1:length(tab12_female_rec.t)){
        unlink(paste0(vaccines_tmp,tab12_female_rec.t[i]))
      }
      saveRDS(f_records.t,paste0(vaccines_tmp,"f_records.t.rds"))
      rm(tab12_female_rec.t,f_records.t)
      #output: f_records.t
      
      #female users total 
      tab12_female_users.t<-c(list.files(vaccines_tmp,pattern="tab12_f_users_4.t"),list.files(vaccines_tmp,pattern="tab12_f_users_3.t"),list.files(vaccines_tmp,pattern="tab12_f_users_1.t"))
      #load all files and rbind together
      f_users.t<-lapply(paste0(vaccines_tmp,tab12_female_users.t), readRDS)
      f_users.t<-do.call(rbind,f_users.t)
      for(i in 1:length(tab12_female_users.t)){
        unlink(paste0(vaccines_tmp,tab12_female_users.t[i]))
      }
      saveRDS(f_users.t,paste0(vaccines_tmp,"f_users.t.rds"))
      rm(tab12_female_users.t,f_users.t)
      #output: f_users.t
      
      #median
      tab12_female_median.t<-c(list.files(vaccines_tmp,pattern="tab12_f_median_4.t"),list.files(vaccines_tmp,pattern="tab12_f_median_3.t"),list.files(vaccines_tmp,pattern="tab12_f_median_1.t"))
      #load all files and rbind together
      f_median.t<-lapply(paste0(vaccines_tmp,tab12_female_median.t), readRDS)
      f_median.t<-do.call(rbind,f_median.t)
      for(i in 1:length(tab12_female_median.t)){
        unlink(paste0(vaccines_tmp,tab12_female_median.t[i]))
      }
      saveRDS(f_median.t,paste0(vaccines_tmp,"f_median.t.rds"))
      rm(tab12_female_median.t,f_median.t)
      #output:f_median.t
      
      #mean
      tab12_female_mean.t<-c(list.files(vaccines_tmp,pattern="tab12_f_mean_4.t"),list.files(vaccines_tmp,pattern="tab12_f_mean_3.t"),list.files(vaccines_tmp,pattern="tab12_f_mean_1.t"))
      #load all files and rbind together
      f_mean.t<-lapply(paste0(vaccines_tmp,tab12_female_mean.t), readRDS)
      f_mean.t<-do.call(rbind,f_mean.t)
      for(i in 1:length(tab12_female_mean.t)){
        unlink(paste0(vaccines_tmp,tab12_female_mean.t[i]))
      }
      saveRDS(f_mean.t,paste0(vaccines_tmp,"f_mean.t.rds"))
      rm(tab12_female_mean.t,f_mean.t)
      #output:f_mean.t
      ########
    } 
  } else {
    tab12_females<-data.table(meaning="N/A", year="N/A", atc_code_1="N/A", atc_code_3="N/A", atc_code_4="N/A", no_female_users="N/A", median_rx_female_users="N/A")
  }
  ########
  #Info input
  #f_records.my: number of female records by meaning, year, atc_code_4, atc_code_3, atc_code_1(combined)
  #f_records.t: total number of female records by atc_code_4, atc_code_3, atc_code_1(combined)
  #f_users.my: number of female users by meaning, year, atc_code_4, atc_code_3, atc_code_1(combined)
  #f_users.t: number of female users by atc_code_4, atc_code_3, atc_code_1(combined)
  #f_median.my: number of median presc/disp for female users by meaning, year, atc_code_4, atc_code_3, atc_code_1(combined)
  #f_median.t: number of median presc/disp for female users by atc_code_4, atc_code_3, atc_code_1(combined)
  
  #m_records.my:number of male records by meaning, year, atc_code_4, atc_code_3, atc_code_1(combined)
  #m_records.t: total number of male records by atc_code_4, atc_code_3, atc_code_1(combined)
  #m_users.my: number of male users by meaning, year, atc_code_4, atc_code_3, atc_code_1(combined)
  #m_users.t: number of male users by atc_code_4, atc_code_3, atc_code_1(combined)
  #m_median.my: number of median presc/disp for male users by meaning, year, atc_code_4, atc_code_3, atc_code_1(combined)
  #m_median.t: number of median presc/disp for male users by atc_code_4, atc_code_3, atc_code_1(combined)
  #########
  
  #Combine number of records by meaning, year and atc code
  #load files and merge if males and female available: no records sum between the two DT
  if(male_population>0 & female_population>0){
    tot_rec_m.my<-c(list.files(vaccines_tmp,pattern="m_records.my"))
    tot_rec_f.my<-c(list.files(vaccines_tmp,pattern="f_records.my"))
    tot_rec_m.my<-readRDS(paste0(vaccines_tmp,tot_rec_m.my))
    tot_rec_f.my<-readRDS(paste0(vaccines_tmp,tot_rec_f.my))
    tot_rec.my<-tot_rec_m.my
    rm(tot_rec_m.my)
    tot_rec.my<-rbind(tot_rec.my,tot_rec_f.my)
    tot_rec.my<-tot_rec.my[,lapply(.SD,sum), by=c("meaning","year","atc_code_1","atc_code_3","atc_code_4"), .SDcols="no_records"]
    tot_rec.my[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    unlink(paste0(vaccines_tmp,"m_records.my.rds"))
    unlink(paste0(vaccines_tmp,"f_records.my.rds"))
    rm(tot_rec_f.my)
    #tot_rec.my:combined number of records for males and female(sum) by meaning, year, atc_code_4, atc_code_3,atc_code_1
  }
  if(male_population==0 & female_population>0){
    tot_rec.my<-c(list.files(vaccines_tmp,pattern="f_records.my"))
    tot_rec.my<-readRDS(paste0(vaccines_tmp,tot_rec.my))
    unlink(paste0(vaccines_tmp,"f_records.my.rds"))
    #tot_rec_my:combined number of records for females by meaning, year, atc_code_4, atc_code_3,atc_code_1(males==0)
  }
  if(male_population>0 & female_population==0){
    tot_rec.my<-c(list.files(vaccines_tmp,pattern="m_records.my"))
    tot_rec.my<-readRDS(paste0(vaccines_tmp,tot_rec.my))
    unlink(paste0(vaccines_tmp,"m_records.my.rds"))
    #tot_rec_my:combined number of records for males by meaning, year, atc_code_4, atc_code_3,atc_code_1(females==0)
  }
  
  #Combine number of records by atc code
  if(male_population>0 & female_population>0){
    tot_rec_m.t<-c(list.files(vaccines_tmp,pattern="m_records.t"))
    tot_rec_f.t<-c(list.files(vaccines_tmp,pattern="f_records.t"))
    tot_rec_m.t<-readRDS(paste0(vaccines_tmp,tot_rec_m.t))
    tot_rec_f.t<-readRDS(paste0(vaccines_tmp,tot_rec_f.t))
    tot_rec.t<-tot_rec_m.t
    rm(tot_rec_m.t)
    tot_rec.t<-rbind(tot_rec.t,tot_rec_f.t)
    tot_rec.t<-tot_rec.t[,lapply(.SD,sum), by=c("meaning","year","atc_code_1","atc_code_3","atc_code_4"), .SDcols="no_records"]
    tot_rec.t[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    unlink(paste0(vaccines_tmp,"m_records.t.rds"))
    unlink(paste0(vaccines_tmp,"f_records.t.rds"))
    rm(tot_rec_f.t)
    #tot_rec.t:combined number of records for males and female(sum) by atc_code_4, atc_code_3,atc_code_1
  }
  if(male_population==0 & female_population>0){
    tot_rec.t<-c(list.files(vaccines_tmp,pattern="f_records.t"))
    tot_rec.t<-readRDS(paste0(vaccines_tmp,tot_rec.t))
    unlink(paste0(vaccines_tmp,"f_records.t.rds"))
    #tot_rec.t:combined number of records for female( by atc_code_4, atc_code_3,atc_code_1(males==0)
  }
  if(male_population>0 & female_population==0){
    tot_rec.t<-c(list.files(vaccines_tmp,pattern="m_records.t"))
    tot_rec.t<-readRDS(paste0(vaccines_tmp,tot_rec.t))
    unlink(paste0(vaccines_tmp,"m_records.t.rds"))
    #tot_rec.t:combined number of records for males by atc_code_4, atc_code_3,atc_code_1(females==0)
  }
  
  print("Combine all elements to create table 12.")
  if(female_population>0){
    tab12<-rbind(readRDS(paste0(vaccines_tmp,"f_users.my.rds")),readRDS(paste0(vaccines_tmp,"f_users.t.rds")))
    unlink(paste0(vaccines_tmp,"f_users.my.rds"))
    unlink(paste0(vaccines_tmp,"f_users.t.rds"))
    med.f<-rbind(readRDS(paste0(vaccines_tmp,"f_median.my.rds")),readRDS(paste0(vaccines_tmp,"f_median.t.rds")))
    unlink(paste0(vaccines_tmp,"f_median.my.rds"))
    unlink(paste0(vaccines_tmp,"f_median.t.rds"))
    mean.f<-rbind(readRDS(paste0(vaccines_tmp,"f_mean.my.rds")),readRDS(paste0(vaccines_tmp,"f_mean.t.rds")))
    unlink(paste0(vaccines_tmp,"f_mean.my.rds"))
    unlink(paste0(vaccines_tmp,"f_mean.t.rds"))
    #tab12: combined no_female_users(by meaning and year) no_female_users(total)
    #med.f combined median_rx_female_users(by_meaning and year) median_rx_female_users(total)
    #mean.f combined mean_rx_female_users(by_meaning and year) mean_rx_female_users(total)
  }
  
  if(male_population>0){
    males<-rbind(readRDS(paste0(vaccines_tmp,"m_users.my.rds")),readRDS(paste0(vaccines_tmp,"m_users.t.rds")))
    unlink(paste0(vaccines_tmp,"m_users.my.rds"))
    unlink(paste0(vaccines_tmp,"m_users.t.rds"))
    med.m<-rbind(readRDS(paste0(vaccines_tmp,"m_median.my.rds")),readRDS(paste0(vaccines_tmp,"m_median.t.rds")))
    unlink(paste0(vaccines_tmp,"m_median.my.rds"))
    unlink(paste0(vaccines_tmp,"m_median.t.rds"))
    mean.m<-rbind(readRDS(paste0(vaccines_tmp,"m_mean.my.rds")),readRDS(paste0(vaccines_tmp,"m_mean.t.rds")))
    unlink(paste0(vaccines_tmp,"m_mean.my.rds"))
    unlink(paste0(vaccines_tmp,"m_mean.t.rds"))
    #males: combined no_male_users(by meaning and year) no_male_users(total)
    #med.m combined median_rx_male_users(by_meaning and year) median_rx_male_users(total)
    #mean.m combined mean_rx_male_users(by_meaning and year) mean_rx_male_users(total)
  }
  
  #combine no_records(by meaning and year) with no_records(total)
  tot_rec.my<-rbind(tot_rec.my,tot_rec.t)
  tot_rec.my[,year:=as.character(year)]
  
  ########
  #Tab12: input
  ########
  #Depends on which population is available(males and females)
  #tot_rec.my no records by meaning, year and atc(both stratified and total, for total meaning and year equal "All")
  #tab12: combined no_female_users(by meaning and year) no_female_users(total)
  #med.f combined median_rx_female_users(by_meaning and year) median_rx_female_users(total)
  #males: combined no_male_users(by meaning and year) no_male_users(total)
  #med.m combined median_rx_male_users(by_meaning and year) median_rx_male_users(total)
  ########
  
  if(male_population>0 & female_population>0){
    tab12[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    males[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    tab12<-merge(tab12,males, by=c("meaning", "year", "atc_code_4", "atc_code_3", "atc_code_1"), all=T)
    rm(males)
    tab12[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    tot_rec.my[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    tab12<-merge(tab12,tot_rec.my, by=c("meaning", "year", "atc_code_4", "atc_code_3", "atc_code_1"), all=T)
    rm(tot_rec.my)
    tab12[is.na(no_female_users),no_female_users:=0][is.na(no_male_users),no_male_users:=0]
    tab12[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    med.m[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    tab12<-merge(tab12,med.m, by=c("meaning", "year", "atc_code_4", "atc_code_3", "atc_code_1"), all=T)
    rm(med.m)
    tab12[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    mean.m[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    tab12<-merge(tab12,mean.m, by=c("meaning", "year", "atc_code_4", "atc_code_3", "atc_code_1"), all=T)
    rm(mean.m)
    tab12[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    med.f[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    tab12<-merge(tab12,med.f, by=c("meaning", "year", "atc_code_4", "atc_code_3", "atc_code_1"), all=T)
    rm(med.f)
    tab12[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    mean.f[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    tab12<-merge(tab12,mean.f, by=c("meaning", "year", "atc_code_4", "atc_code_3", "atc_code_1"), all=T)
    rm(mean.f)
    tab12[is.na(median_rx_female_users),median_rx_female_users:=0][is.na(median_rx_male_users),median_rx_male_users:=0]
    setcolorder(tab12,c("meaning","year", "atc_code_4", "atc_code_3","atc_code_1","no_records","no_male_users","median_rx_male_users","no_female_users","median_rx_female_users"))
    setorderv(tab12,c("meaning","year","atc_code_4","atc_code_3","atc_code_1"))
  }
  if(male_population==0 & female_population>0){
    tab12[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    tot_rec.my[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    tab12<-merge(tab12,tot_rec.my, by=c("meaning", "year", "atc_code_4", "atc_code_3", "atc_code_1"), all=T)
    rm(tot_rec.my)
    tab12[is.na(no_female_users),no_female_users:=0]
    tab12[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    med.f[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    tab12<-merge(tab12,med.f, by=c("meaning", "year", "atc_code_4", "atc_code_3", "atc_code_1"), all=T)
    rm(med.f)
    tab12[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    mean.f[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    tab12<-merge(tab12,mean.f, by=c("meaning", "year", "atc_code_4", "atc_code_3", "atc_code_1"), all=T)
    rm(mean.f)
    tab12[is.na(median_rx_female_users),median_rx_female_users:=0]
    tab12[,no_male_users:=0][,median_rx_male_users:=0]
    setcolorder(tab12,c("meaning","year", "atc_code_4", "atc_code_3","atc_code_1","no_records","no_male_users","median_rx_male_users","no_female_users","median_rx_female_users"))
    setorderv(tab12,c("meaning","year","atc_code_4","atc_code_3","atc_code_1"))
  }
  if(male_population>0 & female_population==0){
    tab12<-males
    rm(males)
    tab12[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    tot_rec.my[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    tab12<-merge(tab12,tot_rec.my, by=c("meaning", "year", "atc_code_4", "atc_code_3", "atc_code_1"), all=T)
    rm(tot_rec.my)
    tab12[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    med.m[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    tab12<-merge(tab12,med.m, by=c("meaning", "year", "atc_code_4", "atc_code_3", "atc_code_1"), all=T)
    rm(med.m)
    tab12[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    mean.m[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_4:=as.character(atc_code_4)][,atc_code_3:=as.character(atc_code_3)][,atc_code_1:=as.character(atc_code_1)]
    tab12<-merge(tab12,mean.m, by=c("meaning", "year", "atc_code_4", "atc_code_3", "atc_code_1"), all=T)
    rm(mean.m)
    tab12[is.na(median_rx_male_users),median_rx_male_users:=0]
    tab12[,no_female_users:=0][,median_rx_female_users:=0]
    setcolorder(tab12,c("meaning","year", "atc_code_4", "atc_code_3","atc_code_1","no_records","no_male_users","median_rx_male_users","no_female_users","median_rx_female_users"))
    setorderv(tab12,c("meaning","year","atc_code_4","atc_code_3","atc_code_1"))
  }
  
  #######
  #output tab12 to vaccines_dir folder
  ######
  
  
  if(!is.null(tab12)){
    tab12<-data.table(tab12, data_access_provider= data_access_provider_name, data_source=data_source_name)
    if (subpopulations_present=="Yes"){
      fwrite(tab12, paste0(vacc_dir,subpopulations_names[s], "/", subpopulations_names[s],"_vaccines_my_atc_4.csv"), row.names = F)
    } else {
      fwrite(tab12, paste0(vacc_dir,"vaccines_my_atc_4.csv"), row.names = F)
    }
  }
  
  #Apply masking
  
  if(!is.null(tab12)){
    tab12[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
    tab12[, no_male_users:= as.character(no_male_users)][as.numeric(no_male_users) > 0 & as.numeric(no_male_users) < 5, no_male_users := "<5"]
    tab12[, no_female_users:= as.character(no_female_users)][as.numeric(no_female_users) > 0 & as.numeric(no_female_users) < 5, no_female_users := "<5"]
    
    if(subpopulations_present=="Yes"){
      fwrite(tab12, paste0(vacc_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_vaccines_my_atc_4_masked.csv"), row.names = F)
    } else {
      fwrite(tab12, paste0(vacc_dir,"Masked/", "vaccines_my_atc_4_masked.csv"), row.names = F)
    }
  }
  
  rm(tab12)
  #########################################
  #Table 13:Number of vaccine records/administrations by ATC 3 & 7 level in the study population by year of dispensing/prescribing and by meaning for each ATC class
  #########################################
  print("Creating Table 13:Number of vaccine records/administrations by ATC 3 & 7 level in the study population by year of dispensing/prescribing and by meaning for each ATC class.")
  print("Get all variables.")
  ########
  #Info input
  #tab13_m_rec_7.my: number of male records by meaning, year, atc_code_7
  #tab13_m_rec_7.t: total number of male records by atc_code_7
  #tab13_m_users_7.my: number of male users by meaning, year, atc_code_7
  #tab13_m_users_7.t: number of male users by atc_code_7
  #tab13_m_median_7.my: number of median presc/disp for male users by meaning, year, atc_code_7
  #tab13_m_median_7.t: number of median presc/disp for male users by atc_code_7
  
  #tab13_f_rec_7.my: number of female records by meaning, year, atc_code_7
  #tab13_f_rec_7.t: total number of female records by atc_code_7
  #tab13_f_users_7.my: number of female users by meaning, year, atc_code_7
  #tab13_f_users_7.t: number of female users by atc_code_7
  #tab13_f_median_7.my: number of median presc/disp for female users by meaning, year, atc_code_7
  #tab13_f_median_7.t: number of median presc/disp for female users by atc_code_7
  
  #To calculate number of records:
  #rec_my: number of male records + number of female records by meaning, year and atc_code_7
  #rec_t: number of male records + number of female records by atc_code_7
  #########
  print("Calculating number of male users and number of median prescription/dispensings stratified by ATC code level 7, meaning and year.")
  #######
  #males
  ######
  #######
  #my
  ######
  if(male_population>0){
    #combine results for male users records
    tab13_male_rec.my<-list.files(vaccines_tmp,pattern="tab13_m_rec_7.my")
    tab13.m_records.my<-lapply(paste0(vaccines_tmp,tab13_male_rec.my), readRDS)
    tab13.m_records.my<-do.call(rbind,tab13.m_records.my)
    for(i in 1:length(tab13_male_rec.my)){
      unlink(paste0(vaccines_tmp,tab13_male_rec.my[i]))
    }
    saveRDS(tab13.m_records.my,paste0(vaccines_tmp,"tab13.m_records.my.rds"))
    rm(tab13_male_rec.my,tab13.m_records.my)
    #output:tab13.m_records.my
    
    #combine results for male users 
    tab13_male_users.my<-list.files(vaccines_tmp,pattern="tab13_m_users_7.my")
    #load all files and rbind together
    tab13.m_users.my<-lapply(paste0(vaccines_tmp,tab13_male_users.my), readRDS)
    tab13.m_users.my<-do.call(rbind,tab13.m_users.my)
    for(i in 1:length(tab13_male_users.my)){
      unlink(paste0(vaccines_tmp,tab13_male_users.my[i]))
    }
    saveRDS(tab13.m_users.my,paste0(vaccines_tmp,"tab13.m_users.my.rds"))
    rm(tab13_male_users.my,tab13.m_users.my)
    #output:tab13.m_users.my
    
    #combine results for median presc/disp for male users
    tab13_male_median.my<-list.files(vaccines_tmp,pattern="tab13_m_median_7.my")
    tab13.m_median.my<-lapply(paste0(vaccines_tmp,tab13_male_median.my), readRDS)
    tab13.m_median.my<-do.call(rbind,tab13.m_median.my)
    for(i in 1:length(tab13_male_median.my)){
      unlink(paste0(vaccines_tmp,tab13_male_median.my[i]))
    }
    saveRDS(tab13.m_median.my,paste0(vaccines_tmp,"tab13.m_median.my.rds"))
    rm(tab13_male_median.my,tab13.m_median.my)
    #output: tab13.m_median.my
    
    #combine results for mean presc/disp for male users
    tab13_male_mean.my<-list.files(vaccines_tmp,pattern="tab13_m_mean_7.my")
    tab13.m_mean.my<-lapply(paste0(vaccines_tmp,tab13_male_mean.my), readRDS)
    tab13.m_mean.my<-do.call(rbind,tab13.m_mean.my)
    for(i in 1:length(tab13_male_mean.my)){
      unlink(paste0(vaccines_tmp,tab13_male_mean.my[i]))
    }
    saveRDS(tab13.m_mean.my,paste0(vaccines_tmp,"tab13.m_mean.my.rds"))
    rm(tab13_male_mean.my,tab13.m_mean.my)
    #output: tab13.m_mean.my
    
    ########
    #total
    ########
    #combine results for male users records
    tab13_male_rec.t<-list.files(vaccines_tmp,pattern="_tab13_m_rec_7.t_")
    tab13.m_records.t<-lapply(paste0(vaccines_tmp,tab13_male_rec.t), readRDS)
    tab13.m_records.t<-do.call(rbind,tab13.m_records.t)
    for(i in 1:length(tab13_male_rec.t)){
      unlink(paste0(vaccines_tmp,tab13_male_rec.t[i]))
    }
    saveRDS(tab13.m_records.t,paste0(vaccines_tmp,"tab13.m_records.t.rds"))
    rm(tab13_male_rec.t,tab13.m_records.t)
    #output:tab13.m_records.t
    
    #male users total 
    tab13_male_users.t<-list.files(vaccines_tmp,pattern="tab13_m_users_7.t")
    #load all files and rbind together
    tab13.m_users.t<-lapply(paste0(vaccines_tmp,tab13_male_users.t), readRDS)
    tab13.m_users.t<-do.call(rbind,tab13.m_users.t)
    for(i in 1:length(tab13_male_users.t)){
      unlink(paste0(vaccines_tmp,tab13_male_users.t[i]))
    }
    saveRDS(tab13.m_users.t,paste0(vaccines_tmp,"tab13.m_users.t.rds"))
    rm(tab13_male_users.t,tab13.m_users.t)
    #output: tab13.m_users.t
    
    tab13_male_median.t<-list.files(vaccines_tmp,pattern="tab13_m_median_7.t")
    #load all files and rbind together
    tab13.m_median.t<-lapply(paste0(vaccines_tmp,tab13_male_median.t), readRDS)
    tab13.m_median.t<-do.call(rbind,tab13.m_median.t)
    for(i in 1:length(tab13_male_median.t)){
      unlink(paste0(vaccines_tmp,tab13_male_median.t[i]))
    }
    saveRDS(tab13.m_median.t,paste0(vaccines_tmp,"tab13.m_median.t.rds"))
    rm(tab13_male_median.t,tab13.m_median.t)
    
    #mean total
    tab13_male_mean.t<-list.files(vaccines_tmp,pattern="tab13_m_mean_7.t")
    #load all files and rbind together
    tab13.m_mean.t<-lapply(paste0(vaccines_tmp,tab13_male_mean.t), readRDS)
    tab13.m_mean.t<-do.call(rbind,tab13.m_mean.t)
    for(i in 1:length(tab13_male_mean.t)){
      unlink(paste0(vaccines_tmp,tab13_male_mean.t[i]))
    }
    saveRDS(tab13.m_mean.t,paste0(vaccines_tmp,"tab13.m_mean.t.rds"))
    rm(tab13_male_mean.t,tab13.m_mean.t)
    #output:tab13.m_mean.t
  } 
  
  ########
  #females
  ########
  ########
  #my
  #######
  if(female_population>0){
    #combine results for female users records
    tab13_female_rec.my<-list.files(vaccines_tmp,pattern="tab13_f_rec_7.my")
    tab13.f_records.my<-lapply(paste0(vaccines_tmp,tab13_female_rec.my), readRDS)
    tab13.f_records.my<-do.call(rbind,tab13.f_records.my)
    for(i in 1:length(tab13_female_rec.my)){
      unlink(paste0(vaccines_tmp,tab13_female_rec.my[i]))
    }
    saveRDS(tab13.f_records.my,paste0(vaccines_tmp,"tab13.f_records.my.rds"))
    rm(tab13_female_rec.my,tab13.f_records.my)
    #output: tab13.f_records.my
    
    #combine results for female users 
    tab13_female_users.my<-list.files(vaccines_tmp,pattern="tab13_f_users_7.my")
    #load all files and rbind together
    tab13.f_users.my<-lapply(paste0(vaccines_tmp,tab13_female_users.my), readRDS)
    tab13.f_users.my<-do.call(rbind,tab13.f_users.my)
    for(i in 1:length(tab13_female_users.my)){
      unlink(paste0(vaccines_tmp,tab13_female_users.my[i]))
    }
    saveRDS(tab13.f_users.my,paste0(vaccines_tmp,"tab13.f_users.my.rds"))
    rm(tab13_female_users.my,tab13.f_users.my)
    #output: tab13.f_users.my
    
    #median female users
    tab13_female_median.my<-list.files(vaccines_tmp,pattern="tab13_f_median_7.my")
    tab13.f_median.my<-lapply(paste0(vaccines_tmp,tab13_female_median.my), readRDS)
    tab13.f_median.my<-do.call(rbind,tab13.f_median.my)
    for(i in 1:length(tab13_female_median.my)){
      unlink(paste0(vaccines_tmp,tab13_female_median.my[i]))
    }
    saveRDS(tab13.f_median.my,paste0(vaccines_tmp,"tab13.f_median.my.rds"))
    rm(tab13_female_median.my,tab13.f_median.my)
    #output: tab13.f_median.my
    
    #mean female users
    tab13_female_mean.my<-list.files(vaccines_tmp,pattern="tab13_f_mean_7.my")
    tab13.f_mean.my<-lapply(paste0(vaccines_tmp,tab13_female_mean.my), readRDS)
    tab13.f_mean.my<-do.call(rbind,tab13.f_mean.my)
    for(i in 1:length(tab13_female_mean.my)){
      unlink(paste0(vaccines_tmp,tab13_female_mean.my[i]))
    }
    saveRDS(tab13.f_mean.my,paste0(vaccines_tmp,"tab13.f_mean.my.rds"))
    rm(tab13_female_mean.my,tab13.f_mean.my)
    #output: tab13.f_mean.my
    
    #########
    #total
    #########
    #combine results for female users records
    tab13_female_rec.t<-list.files(vaccines_tmp,pattern="tab13_f_rec_7.t")
    tab13.f_records.t<-lapply(paste0(vaccines_tmp,tab13_female_rec.t), readRDS)
    tab13.f_records.t<-do.call(rbind,tab13.f_records.t)
    for(i in 1:length(tab13_female_rec.t)){
      unlink(paste0(vaccines_tmp,tab13_female_rec.t[i]))
    }
    saveRDS(tab13.f_records.t,paste0(vaccines_tmp,"tab13.f_records.t.rds"))
    rm(tab13_female_rec.t,tab13.f_records.t)
    
    #female users total 
    tab13_female_users.t<-list.files(vaccines_tmp,pattern="tab13_f_users_7.t")
    #load all files and rbind together
    tab13.f_users.t<-lapply(paste0(vaccines_tmp,tab13_female_users.t), readRDS)
    tab13.f_users.t<-do.call(rbind,tab13.f_users.t)
    for(i in 1:length(tab13_female_users.t)){
      unlink(paste0(vaccines_tmp,tab13_female_users.t[i]))
    }
    saveRDS(tab13.f_users.t,paste0(vaccines_tmp,"tab13.f_users.t.rds"))
    rm(tab13_female_users.t,tab13.f_users.t)
    #output: tab13.f_users.t
    
    #median
    tab13_female_median.t<-list.files(vaccines_tmp,pattern="tab13_f_median_7.t")
    #load all files and rbind together
    tab13.f_median.t<-lapply(paste0(vaccines_tmp,tab13_female_median.t), readRDS)
    tab13.f_median.t<-do.call(rbind,tab13.f_median.t)
    for(i in 1:length(tab13_female_median.t)){
      unlink(paste0(vaccines_tmp,tab13_female_median.t[i]))
    }
    saveRDS(tab13.f_median.t,paste0(vaccines_tmp,"tab13.f_median.t.rds"))
    rm(tab13_female_median.t,tab13.f_median.t)
    #output: tab13.f_median.t
    
    #mean
    tab13_female_mean.t<-list.files(vaccines_tmp,pattern="tab13_f_mean_7.t")
    #load all files and rbind together
    tab13.f_mean.t<-lapply(paste0(vaccines_tmp,tab13_female_mean.t), readRDS)
    tab13.f_mean.t<-do.call(rbind,tab13.f_mean.t)
    for(i in 1:length(tab13_female_mean.t)){
      unlink(paste0(vaccines_tmp,tab13_female_mean.t[i]))
    }
    saveRDS(tab13.f_mean.t,paste0(vaccines_tmp,"tab13.f_mean.t.rds"))
    rm(tab13_female_mean.t,tab13.f_mean.t)
    #output: tab13.f_mean.t
  }
  ########
  #Info input
  #tab13.f_records.my: number of female records by meaning, year, atc_code_7(combined)
  #tab13.f_records.t: total number of female records by atc_code_7(combined)
  #tab13.f_users.my: number of female users by meaning, year, atc_code_7(combined)
  #tab13.f_users.t: number of female users by atc_code_7(combined)
  #tab13.f_median.my: number of median presc/disp for female users by meaning, year, atc_code_7(combined)
  #tab13.f_median.t: number of median presc/disp for female users by atc_code_7(combined)
  
  #tab13.m_records.my:number of male records by meaning, year, atc_code_4, atc_code_3, atc_code_1(combined)
  #tab13.m_records.t: total number of male records by atc_code_4, atc_code_3, atc_code_1(combined)
  #tab13.m_users.my: number of male users by meaning, year, atc_code_4, atc_code_3, atc_code_1(combined)
  #tab13.m_users.t: number of male users by atc_code_4, atc_code_3, atc_code_1(combined)
  #tab13.m_median.my: number of median presc/disp for male users by meaning, year, atc_code_4, atc_code_3, atc_code_1(combined)
  #tab13.m_median.t: number of median presc/disp for male users by atc_code_4, atc_code_3, atc_code_1(combined)
  #########
  
  #Combine number of records by meaning, year and atc code
  #load files and merge if males and female available: no records sum between the two DT
  if(male_population>0 & female_population>0){
    tab13.tot_rec_m.my<-c(list.files(vaccines_tmp,pattern="tab13.m_records.my"))
    tab13.tot_rec_f.my<-c(list.files(vaccines_tmp,pattern="tab13.f_records.my"))
    tab13.tot_rec_m.my<-readRDS(paste0(vaccines_tmp,tab13.tot_rec_m.my))
    tab13.tot_rec_f.my<-readRDS(paste0(vaccines_tmp,tab13.tot_rec_f.my))
    tab13.tot_rec.my<-tab13.tot_rec_m.my
    rm(tab13.tot_rec_m.my)
    tab13.tot_rec.my<-rbind(tab13.tot_rec.my,tab13.tot_rec_f.my)
    tab13.tot_rec.my<-tab13.tot_rec.my[,lapply(.SD, sum), by=c("meaning","year","atc_code_3","atc_code_7"),.SDcols="no_records"]
    tab13.tot_rec.my[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    unlink(paste0(vaccines_tmp,"tab13.m_records.my.rds"))
    unlink(paste0(vaccines_tmp,"tab13.f_records.my.rds"))
    rm(tab13.tot_rec_f.my)
    #tab13.tot_rec.my:combined number of records for males and female(sum) by meaning, year, atc_code_7, atc_code_3
  }
  if(male_population==0 & female_population>0){
    tab13.tot_rec.my<-c(list.files(vaccines_tmp,pattern="tab13.f_records.my"))
    tab13.tot_rec.my<-readRDS(paste0(vaccines_tmp,tab13.tot_rec.my))
    unlink(paste0(vaccines_tmp,"tab13.f_records.my.rds"))
    #tab13.tot_rec.my:combined number of records for females by meaning, year, atc_code_7, atc_code_3(males==0)
  }
  if(male_population>0 & female_population==0){
    tab13.tot_rec.my<-c(list.files(vaccines_tmp,pattern="tab13.m_records.my"))
    tab13.tot_rec.my<-readRDS(paste0(vaccines_tmp,tab13.tot_rec.my))
    unlink(paste0(vaccines_tmp,"tab13.m_records.my.rds"))
    #tab13.tot_rec.my:combined number of records for males by meaning, year, atc_code_7, atc_code_3(females==0)
  }
  
  #Combine number of records by atc code
  if(male_population>0 & female_population>0){
    tab13.tot_rec_m.t<-c(list.files(vaccines_tmp,pattern="tab13.m_records.t"))
    tab13.tot_rec_f.t<-c(list.files(vaccines_tmp,pattern="tab13.f_records.t"))
    tab13.tot_rec_m.t<-readRDS(paste0(vaccines_tmp,tab13.tot_rec_m.t))
    tab13.tot_rec_f.t<-readRDS(paste0(vaccines_tmp,tab13.tot_rec_f.t))
    tab13.tot_rec.t<-tab13.tot_rec_m.t
    tab13.tot_rec.t<-rbind(tab13.tot_rec.t,tab13.tot_rec_f.t)
    tab13.tot_rec.t<-tab13.tot_rec.t[,lapply(.SD,sum), by=c("meaning","year","atc_code_3","atc_code_7"), .SDcols="no_records"]
    tab13.tot_rec.t[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    unlink(paste0(vaccines_tmp,"tab13.m_records.t.rds"))
    unlink(paste0(vaccines_tmp,"tab13.f_records.t.rds"))
    #tab13.tot_rec.t:combined number of records for males and female(sum) by atc_code_7, atc_code_3
  }
  if(male_population==0 & female_population>0){
    tab13.tot_rec.t<-c(list.files(vaccines_tmp,pattern="tab13.f_records.t"))
    tab13.tot_rec.t<-readRDS(paste0(vaccines_tmp,tab13.tot_rec.t))
    unlink(paste0(vaccines_tmp,"tab13.f_records.t.rds"))
    #tab13.tot_rec.t:combined number of records for female( by atc_code_7, atc_code_3(males==0)
  }
  if(male_population>0 & female_population==0){
    tab13.tot_rec.t<-c(list.files(vaccines_tmp,pattern="tab13.m_records.t"))
    tab13.tot_rec.t<-readRDS(paste0(vaccines_tmp,tab13.tot_rec.t))
    unlink(paste0(vaccines_tmp,"tab13.m_records.t.rds"))
    #tab13.tot_rec.t:combined number of records for males by atc_code_7, atc_code_3(females==0)
  }
  
  print("Combine all elements to create table 13.")
  if(female_population>0){
    tab13<-rbind(readRDS(paste0(vaccines_tmp,"tab13.f_users.my.rds")),readRDS(paste0(vaccines_tmp,"tab13.f_users.t.rds")))
    unlink(paste0(vaccines_tmp,"tab13.f_users.my.rds"))
    unlink(paste0(vaccines_tmp,"tab13.f_users.t.rds"))
    tab13.vacc.f<-rbind(readRDS(paste0(vaccines_tmp,"tab13.f_median.my.rds")),readRDS(paste0(vaccines_tmp,"tab13.f_median.t.rds")))
    unlink(paste0(vaccines_tmp,"tab13.f_median.my.rds"))
    unlink(paste0(vaccines_tmp,"tab13.f_median.t.rds"))
    tab13.mean.f<-rbind(readRDS(paste0(vaccines_tmp,"tab13.f_mean.my.rds")),readRDS(paste0(vaccines_tmp,"tab13.f_mean.t.rds")))
    unlink(paste0(vaccines_tmp,"tab13.f_mean.my.rds"))
    unlink(paste0(vaccines_tmp,"tab13.f_mean.t.rds"))
    #tab13: combined no_female_users(by meaning and year) no_female_users(total)
    #tab13.vacc.f combined median_rx_female_users(by_meaning and year) median_rx_female_users(total)
    #tab13.mean.f combined mean_rx_female_users(by_meaning and year) mean_rx_female_users(total)
  }
  
  if(male_population>0){
    tab13.males<-rbind(readRDS(paste0(vaccines_tmp,"tab13.m_users.my.rds")),readRDS(paste0(vaccines_tmp,"tab13.m_users.t.rds")))
    unlink(paste0(vaccines_tmp,"tab13.m_users.my.rds"))
    unlink(paste0(vaccines_tmp,"tab13.m_users.t.rds"))
    tab13.vacc.m<-rbind(readRDS(paste0(vaccines_tmp,"tab13.m_median.my.rds")),readRDS(paste0(vaccines_tmp,"tab13.m_median.t.rds")))
    unlink(paste0(vaccines_tmp,"tab13.m_median.my.rds"))
    unlink(paste0(vaccines_tmp,"tab13.m_median.t.rds"))
    tab13.mean.m<-rbind(readRDS(paste0(vaccines_tmp,"tab13.m_mean.my.rds")),readRDS(paste0(vaccines_tmp,"tab13.m_mean.t.rds")))
    unlink(paste0(vaccines_tmp,"tab13.m_mean.my.rds"))
    unlink(paste0(vaccines_tmp,"tab13.m_mean.t.rds"))
    #tab13.males: combined no_male_users(by meaning and year) no_male_users(total)
    #tab13.vacc.m combined median_rx_male_users(by_meaning and year) median_rx_male_users(total)
    #tab13.mean.m combined mean_rx_male_users(by_meaning and year) mean_rx_male_users(total)
  }
  
  #combine no_records(by meaning and year) with no_records(total)
  tab13.tot_rec.my<-rbind(tab13.tot_rec.my,tab13.tot_rec.t)
  tab13.tot_rec.my[,year:=as.character(year)]
  
  ########
  #Tab13: input
  ########
  #Depends on which population is available(males and females)
  #tab13.tot_rec.my no records by meaning, year and atc(both stratified and total, for total meaning and year equal "All")
  #tab13: combined no_female_users(by meaning and year) no_female_users(total)
  #tab13.vacc.f combined median_rx_female_users(by_meaning and year) median_rx_female_users(total)
  #tab13.males: combined no_male_users(by meaning and year) no_male_users(total)
  #tab13.vacc.m combined median_rx_male_users(by_meaning and year) median_rx_male_users(total)
  ########
  
  if(male_population>0 & female_population>0){
    tab13[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab13.males[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab13<-merge(tab13,tab13.males, by=c("meaning", "year", "atc_code_7", "atc_code_3"), all=T)
    rm(tab13.males)
    tab13[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab13.tot_rec.my[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab13<-merge(tab13,tab13.tot_rec.my, by=c("meaning", "year", "atc_code_7", "atc_code_3"), all=T)
    rm(tab13.tot_rec.my)
    tab13[is.na(no_female_users),no_female_users:=0][is.na(no_male_users),no_male_users:=0]
    tab13[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab13.vacc.m[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab13<-merge(tab13,tab13.vacc.m, by=c("meaning", "year", "atc_code_7", "atc_code_3"), all=T)
    rm(tab13.vacc.m)
    tab13[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab13.mean.m[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab13<-merge(tab13,tab13.mean.m, by=c("meaning", "year", "atc_code_7", "atc_code_3"), all=T)
    rm(tab13.mean.m)
    tab13[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab13.vacc.f[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab13<-merge(tab13,tab13.vacc.f, by=c("meaning", "year", "atc_code_7","atc_code_3"), all=T)
    rm(tab13.vacc.f)
    tab13[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab13.mean.f[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab13<-merge(tab13,tab13.mean.f, by=c("meaning", "year", "atc_code_7","atc_code_3"), all=T)
    rm(tab13.mean.f)
    tab13[is.na(median_rx_female_users),median_rx_female_users:=0][is.na(median_rx_male_users),median_rx_male_users:=0]
    setcolorder(tab13,c("meaning","year", "atc_code_7", "atc_code_3","no_records","no_male_users","median_rx_male_users","mean_rx_male_users","no_female_users","median_rx_female_users","mean_rx_female_users"))
    setorderv(tab13,c("meaning","year","atc_code_7","atc_code_3"))
  }
  if(male_population==0 & female_population>0){
    tab13[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab13.tot_rec.my[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab13<-merge(tab13,tab13.tot_rec.my, by=c("meaning", "year", "atc_code_7", "atc_code_3"), all=T)
    rm(tab13.tot_rec.my)
    tab13[is.na(no_female_users),no_female_users:=0]
    tab13[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab13.vacc.f[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab13<-merge(tab13,tab13.vacc.f, by=c("meaning", "year", "atc_code_7", "atc_code_3"), all=T)
    rm(tab13.vacc.f)
    tab13[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab13.mean.f[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab13<-merge(tab13,tab13.mean.f, by=c("meaning", "year", "atc_code_7", "atc_code_3"), all=T)
    rm(tab13.mean.f)
    tab13[is.na(median_rx_female_users),median_rx_female_users:=0]
    tab13[,no_male_users:=0][,median_rx_male_users:=0]
    setcolorder(tab13,c("meaning","year", "atc_code_7", "atc_code_3","no_records","no_male_users","median_rx_male_users","mean_rx_male_users", "no_female_users","median_rx_female_users", "mean_rx_female_users"))
    setorderv(tab13,c("meaning","year","atc_code_7","atc_code_3"))
  }
  if(male_population>0 & female_population==0){
    tab13<-tab13.males
    rm(tab13.males)
    tab13[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab13.tot_rec.my[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab13<-merge(tab13,tab13.tot_rec.my, by=c("meaning", "year", "atc_code_7", "atc_code_3"), all=T)
    rm(tab13.tot_rec.my)
    tab13[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab13.vacc.m[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab13<-merge(tab13,tab13.vacc.m, by=c("meaning", "year", "atc_code_7", "atc_code_3"), all=T)
    rm(tab13.vacc.m)
    tab13[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab13.mean.m[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab13<-merge(tab13,tab13.mean.m, by=c("meaning", "year", "atc_code_7", "atc_code_3"), all=T)
    rm(tab13.mean.m)
    tab13[is.na(median_rx_male_users),median_rx_male_users:=0]
    tab13[,no_female_users:=0][,median_rx_female_users:=0]
    setcolorder(tab13,c("meaning","year", "atc_code_7", "atc_code_3", "no_records","no_male_users","median_rx_male_users","mean_rx_male_users", "no_female_users","median_rx_female_users", "mean_rx_female_users"))
    setorderv(tab13,c("meaning","year","atc_code_7","atc_code_3"))
  }
  
  #######
  #output tab13 to vaccines_dir folder
  ######
  if(!is.null(tab13)){
    tab13<-data.table(tab13, data_access_provider= data_access_provider_name, data_source=data_source_name)
    if(subpopulations_present=="Yes"){
      fwrite(tab13, paste0(vacc_dir,subpopulations_names[s], "/", subpopulations_names[s],"_vaccines_my_atc_7.csv"), row.names = F)
    } else {
      fwrite(tab13, paste0(vacc_dir,"vaccines_my_atc_7.csv"), row.names = F)
    }
  }
  
  #Apply masking 
  if(!is.null(tab13)){
    tab13[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
    tab13[, no_male_users:= as.character(no_male_users)][as.numeric(no_male_users) > 0 & as.numeric(no_male_users) < 5, no_male_users := "<5"]
    tab13[, no_female_users:= as.character(no_female_users)][as.numeric(no_female_users) > 0 & as.numeric(no_female_users) < 5, no_female_users := "<5"]
    
    if(subpopulations_present=="Yes"){
      fwrite(tab13, paste0(vacc_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_vaccines_my_atc_7_masked.csv"), row.names = F)
    } else {
      fwrite(tab13, paste0(vacc_dir,"Masked/", "vaccines_my_atc_7_masked.csv"), row.names = F)
    }
  }
  
  rm(tab13)
  #########################################
  #Table 14:Number of vaccine records/administrations by ATC 3 & 7 level in the study population by year of dispensing/prescribing and by meaning for each ATC class
  #########################################
  print("Creating table 14: Number of vaccine records/administrations by ATC 3 & 7 level in the study population by year of dispensing/prescribing and by meaning for females of childbearing age (12-55 age_start_fup).")
  print("Get all variables.")
  if(females_childbearing>0){
    ########
    #Info input
    #tab14_f_rec_7.my: number of female records by meaning, year, atc_code_7(aged 12-55 years old at fup)
    #tab14_f_rec_7.t: total number of female records by atc_code_7(aged 12-55 years old at fup)
    #tab14_f_users_7.my: number of female users by meaning, year, atc_code_7(aged 12-55 years old at fup)
    #tab14_f_users_7.t: number of female users by atc_code_7(aged 12-55 years old at fup)
    #tab14_f_median_7.my: number of median presc/disp for female users by meaning, year, atc_code_7(aged 12-55 years old at fup)
    #tab14_f_median_7.t: number of median presc/disp for female users by atc_code_7(aged 12-55 years old at fup)
    #########
    
    ########
    #my
    #######
    
    #combine results for female users records
    tab14_female_rec.my<-list.files(vaccines_tmp,pattern="tab14_f_rec_7.my")
    tab14.f_records_my<-lapply(paste0(vaccines_tmp,tab14_female_rec.my), readRDS)
    tab14.f_records_my<-do.call(rbind,tab14.f_records_my)
    for(i in 1:length(tab14_female_rec.my)){
      unlink(paste0(vaccines_tmp,tab14_female_rec.my[i]))
    }
    saveRDS(tab14.f_records_my,paste0(vaccines_tmp,"tab14.f_records_my.rds"))
    rm(tab14_female_rec.my,tab14.f_records_my)
    #output: tab14.f_records_my
    
    #combine results for female users 
    tab14_female_users.my<-list.files(vaccines_tmp,pattern="tab14_f_users_7.my")
    #load all files and rbind together
    tab14.f_users_my<-lapply(paste0(vaccines_tmp,tab14_female_users.my), readRDS)
    tab14.f_users_my<-do.call(rbind,tab14.f_users_my)
    for(i in 1:length(tab14_female_users.my)){
      unlink(paste0(vaccines_tmp,tab14_female_users.my[i]))
    }
    saveRDS(tab14.f_users_my,paste0(vaccines_tmp,"tab14.f_users_my.rds"))
    rm(tab14_female_users.my,tab14.f_users_my)
    #output: tab14.f_users_my
    
    #median female users
    tab14_female_median.my<-list.files(vaccines_tmp,pattern="tab14_f_median_7.my")
    tab14.f_median_my<-lapply(paste0(vaccines_tmp,tab14_female_median.my), readRDS)
    tab14.f_median_my<-do.call(rbind,tab14.f_median_my)
    for(i in 1:length(tab14_female_median.my)){
      unlink(paste0(vaccines_tmp,tab14_female_median.my[i]))
    }
    saveRDS(tab14.f_median_my,paste0(vaccines_tmp,"tab14.f_median_my.rds"))
    rm(tab14_female_median.my,tab14.f_median_my)
    #output: tab14.f_median_my
    
    #mean female users
    tab14_female_mean.my<-list.files(vaccines_tmp,pattern="tab14_f_mean_7.my")
    tab14.f_mean_my<-lapply(paste0(vaccines_tmp,tab14_female_mean.my), readRDS)
    tab14.f_mean_my<-do.call(rbind,tab14.f_mean_my)
    for(i in 1:length(tab14_female_mean.my)){
      unlink(paste0(vaccines_tmp,tab14_female_mean.my[i]))
    }
    saveRDS(tab14.f_mean_my,paste0(vaccines_tmp,"tab14.f_mean_my.rds"))
    rm(tab14_female_mean.my,tab14.f_mean_my)
    #output: tab14.f_mean_my
    #########
    #total
    #########
    #combine results for female users records
    tab14_female_rec.t<-list.files(vaccines_tmp,pattern="tab14_f_rec_7.t")
    tab14.f_records_t<-lapply(paste0(vaccines_tmp,tab14_female_rec.t), readRDS)
    tab14.f_records_t<-do.call(rbind,tab14.f_records_t)
    for(i in 1:length(tab14_female_rec.t)){
      unlink(paste0(vaccines_tmp,tab14_female_rec.t[i]))
    }
    saveRDS(tab14.f_records_t,paste0(vaccines_tmp,"tab14.f_records_t.rds"))
    rm(tab14_female_rec.t,tab14.f_records_t)
    #output tab14.f_records_t
    
    #female users total 
    tab14_female_users.t<-list.files(vaccines_tmp,pattern="tab14_f_users_7.t")
    #load all files and rbind together
    tab14.f_users_t<-lapply(paste0(vaccines_tmp,tab14_female_users.t), readRDS)
    tab14.f_users_t<-do.call(rbind,tab14.f_users_t)
    for(i in 1:length(tab14_female_users.t)){
      unlink(paste0(vaccines_tmp,tab14_female_users.t[i]))
    }
    saveRDS(tab14.f_users_t,paste0(vaccines_tmp,"tab14.f_users_t.rds"))
    rm(tab14_female_users.t,tab14.f_users_t)
    #output: tab14.f_users_t
    
    #median
    tab14_female_median.t<-list.files(vaccines_tmp,pattern="tab14_f_median_7.t")
    #load all files and rbind together
    tab14.f_median_t<-lapply(paste0(vaccines_tmp,tab14_female_median.t), readRDS)
    tab14.f_median_t<-do.call(rbind,tab14.f_median_t)
    for(i in 1:length(tab14_female_median.t)){
      unlink(paste0(vaccines_tmp,tab14_female_median.t[i]))
    }
    saveRDS(tab14.f_median_t,paste0(vaccines_tmp,"tab14.f_median_t.rds"))
    rm(tab14_female_median.t,tab14.f_median_t)
    #output: tab14.f_median_t
    
    #mean
    tab14_female_mean.t<-list.files(vaccines_tmp,pattern="tab14_f_mean_7.t")
    #load all files and rbind together
    tab14.f_mean_t<-lapply(paste0(vaccines_tmp,tab14_female_mean.t), readRDS)
    tab14.f_mean_t<-do.call(rbind,tab14.f_mean_t)
    for(i in 1:length(tab14_female_mean.t)){
      unlink(paste0(vaccines_tmp,tab14_female_mean.t[i]))
    }
    saveRDS(tab14.f_mean_t,paste0(vaccines_tmp,"tab14.f_mean_t.rds"))
    rm(tab14_female_mean.t,tab14.f_mean_t)
    #output: tab14.f_mean_t
    
    ########
    #Info input
    #tab14.f_records_my: number of female records by meaning, year, atc_code_7(combined)(aged 12-55 years old at fup)
    #tab14.f_records_t: total number of female records by atc_code_7(combined)(aged 12-55 years old at fup)
    #tab14.f_users_my: number of female users by meaning, year, atc_code_7(combined)(aged 12-55 years old at fup)
    #tab14.f_users_t: number of female users by atc_code_7(combined)(aged 12-55 years old at fup)
    #tab14.f_median_my: number of median presc/disp for female users by meaning, year, atc_code_7(combined)(aged 12-55 years old at fup)
    #tab14.f_median_t: number of median presc/disp for female users by atc_code_7(combined)(aged 12-55 years old at fup)
    #########
    
    print("Combine all elements to create table 14.")
    tab14<-rbind(readRDS(paste0(vaccines_tmp,"tab14.f_records_my.rds")),readRDS(paste0(vaccines_tmp,"tab14.f_records_t.rds")))
    tab14[,year:=as.character(year)]
    unlink(paste0(vaccines_tmp,"tab14.f_records_my.rds"))
    unlink(paste0(vaccines_tmp,"tab14.f_records_t.rds"))
    users<-rbind(readRDS(paste0(vaccines_tmp,"tab14.f_users_my.rds")),readRDS(paste0(vaccines_tmp,"tab14.f_users_t.rds")))
    users[,year:=as.character(year)]
    unlink(paste0(vaccines_tmp,"tab14.f_users_my.rds"))
    unlink(paste0(vaccines_tmp,"tab14.f_users_t.rds"))
    median<-rbind(readRDS(paste0(vaccines_tmp,"tab14.f_median_my.rds")),readRDS(paste0(vaccines_tmp,"tab14.f_median_t.rds")))
    median[,year:=as.character(year)]
    unlink(paste0(vaccines_tmp,"tab14.f_median_my.rds"))
    unlink(paste0(vaccines_tmp,"tab14.f_median_t.rds"))
    mean<-rbind(readRDS(paste0(vaccines_tmp,"tab14.f_mean_my.rds")),readRDS(paste0(vaccines_tmp,"tab14.f_mean_t.rds")))
    mean[,year:=as.character(year)]
    unlink(paste0(vaccines_tmp,"tab14.f_mean_my.rds"))
    unlink(paste0(vaccines_tmp,"tab14.f_mean_t.rds"))
    tab14[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    users[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab14<-merge(tab14,users, by=c("meaning", "year", "atc_code_7", "atc_code_3"), all=T)
    rm(users)
    tab14[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    median[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab14<-merge(tab14,median, by=c("meaning", "year", "atc_code_7", "atc_code_3"), all=T)
    rm(median)
    tab14[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    mean[,meaning:=as.character(meaning)][,year:=as.character(year)][,atc_code_7:=as.character(atc_code_7)][,atc_code_3:=as.character(atc_code_3)]
    tab14<-merge(tab14,mean, by=c("meaning", "year", "atc_code_7", "atc_code_3"), all=T)
    rm(mean)
    setcolorder(tab14,c("meaning","year", "atc_code_7", "atc_code_3","no_records","no_female_users","median_rx_female_users","mean_rx_female_users"))
    setorderv(tab14,c("meaning","year","atc_code_7","atc_code_3"))
    
    #######
    #output tab14 to vaccines_dir folder
    ######
    
  } else {tab14<-NULL}
  
  
  if(!is.null(tab14)){
    tab14<-data.table(tab14, data_access_provider= data_access_provider_name, data_source=data_source_name)
    if (subpopulations_present=="Yes"){
      fwrite(tab14, paste0(vacc_dir,subpopulations_names[s], "/", subpopulations_names[s],"_vaccines_my_atc_7_f.csv"), row.names = F)
    } else {
      fwrite(tab14, paste0(vacc_dir,"vaccines_my_atc_7_f.csv"), row.names = F)
    }
  }
  
  #Apply masking 
  if(!is.null(tab14)){
    tab14[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
    tab14[, no_female_users:= as.character(no_female_users)][as.numeric(no_female_users) > 0 & as.numeric(no_female_users) < 5, no_female_users := "<5"]
    if(subpopulations_present=="Yes"){
      fwrite(tab14, paste0(vacc_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_vaccines_my_atc_7_f_masked.csv"), row.names = F)
    } else {
      fwrite(tab14, paste0(vacc_dir, "Masked/", "vaccines_my_atc_7_f_masked.csv"), row.names = F)
    }
  }
  
  rm(Res.1_total,Res.2_total)
  rm(comb_tab10,comb_tab11)
  rm(tab13.tot_rec.t, tab13.tot_rec_f.t, tab13.tot_rec_m.t)
  rm(tab14)
  rm(tot_rec.t)
  rm(empty_atc_code_m, empty_atc_code_m_f, empty_atc_code_m_y, empty_atc_code_m_y_f,
     meaning_info, vx_study_population_f, vx_study_population_meaning, vx_study_population_meaning_f, vx_study_population_meaning_info, presc_info,
     presc_unit_info, prescriber_info, years_this_table, disp_info, indication_info)
  ########################################################################################################
  #Rates in females of childbearing age by year and atc/ by year, atc and age band
  #######################################################################################################
  #calculate counts and person time for all females irrespective of age at start follow up
  #keep only data from 12 years old onwards
  #if a female is included in the data base from 0 years old she will not contribute to the person time from 0 t0 11 years old
  
  print("Calculating rates of medicine use.")
  if(subpopulations_present=="Yes"){
    vaccines_files<-list.files(paste0(vaccines_pop, subpopulations_names[s]), pattern = "f_population")
  } else {
    vaccines_files<-list.files(vaccines_pop, pattern = "f_population")
  }
  if(length(vaccines_files)>0){
    #creates filter year_ATC level
    files<-list()
    for (i in 1: length(vaccines_files)){
      files<-append(files,unique(list(unlist(str_split(vaccines_files[i],"_"))[2])))
    }
    files<-do.call(c,files)
    #remove duplicates 
    files<-files[!duplicated(files)]
    #create list with names year_condition
    vaccines_list<-vector(mode="list", length=length(files))
    names(vaccines_list)<-files
    rm(files)
    #separate all files into the right category
    #Change:substr(vaccines_files,1,6)/it removes the possibility to detect files that are named by year incorrectly
    #26.10.2021
    for (i in 1:length(vaccines_list)){
      vaccines_list[[i]]<-vaccines_files[str_detect(substr(vaccines_files,1,6),names(vaccines_list)[i])]
    }
    rm(vaccines_files)
    vaccines_files<-vaccines_list
    rm(vaccines_list)
  }
  
  print("Exporting files for calculation of number of records and users.")
  #records & users
  if(length(vaccines_files)>0){
    for (vx_files in 1: length(vaccines_files)){
      #load file
      if(subpopulations_present=="Yes"){
        vaccines<-lapply(paste0(vaccines_pop, subpopulations_names[s],"/",vaccines_files[[vx_files]]), readRDS)
      } else {
        vaccines<-lapply(paste0(vaccines_pop, vaccines_files[[vx_files]]), readRDS)
      }
      vaccines<-do.call(rbind,vaccines)
      #create char: number of characters for vx_atc
      vaccines[,char:=nchar(vx_atc)]
      #remove rows where atc code<3 characters
      vaccines<-vaccines[char>=3]
      #remove char
      vaccines[,char:=NULL]
      #create atc_code(truncted to the third level)
      vaccines[,truncated_atc_code:=substr(vx_atc,1,3)]
      if(vaccines[,.N]>0){
        outcomes_list<-unique(vaccines[["truncated_atc_code"]])
        
        print(names(vaccines_files)[vx_files])
        output<-CountPersonTime2(Dataset_events = vaccines[,.(person_id,truncated_atc_code, vaccines_date)],
                                 Dataset = unique(vaccines[,.(person_id, birth_date, start_follow_up,end_follow_up)]),
                                 Person_id = "person_id",
                                 Start_study_time =paste0(as.numeric(names(vaccines_files)[vx_files]),"0101"),#start only at the year of interest
                                 End_study_time = paste0(names(vaccines_files)[vx_files],"1231"),#end at the year of interest
                                 Start_date = "start_follow_up",
                                 End_date = "end_follow_up",
                                 Birth_date = "birth_date",
                                 Increment = "year",
                                 Unit_of_age = "year",
                                 include_remaning_ages = TRUE,
                                 Aggregate = F,
                                 Outcomes_rec = outcomes_list,
                                 Name_event = "truncated_atc_code",
                                 Date_event = "vaccines_date",
                                 Rec_period = rep(0, length(outcomes_list)),
                                 Age_bands = c(0,11,19,29,39,49,55),
                                 print = F, 
                                 check_overlap = F) #results will be used only for counts
        rm(vaccines)  
        #remove all data before 12 years old or after 55 years old
        output<-output[Ageband != paste0("0-", min_age_preg-1)]
        output<-output[Ageband != paste0(max_age_preg+1,"+")]
        
        #from wide to long(remove all person time)
        output[,colnames(output)[str_detect(colnames(output), "^Persontime")]]<-NULL
        output<-melt(output, id.vars=c("person_id", "Ageband","year"), measure.vars = colnames(output)[!colnames(output) %in% c("person_id", "Ageband", "year", "Persontime")], variable.name = "truncated_atc_code")        
        setnames(output, "value", "count_vaccines")
        setnames(output, "Ageband", "age_band")
        output[,truncated_atc_code:=substr(truncated_atc_code,1,3)]
        
        #will be used to count users
        saveRDS(output, paste0(vaccines_tmp, names(vaccines_files)[vx_files], "_rates_users_records.rds"))
        
        rm(output)
      }
    }
  }
  
  print("Exporting files for calculation of person time.")
  #person_time
  vx_files<-1
  if(length(vaccines_files)>0){
    person_ids<-c()
    for (vx_files in 1: length(vaccines_files)){
      print(names(vaccines_files)[vx_files])
      #load file
      if(subpopulations_present=="Yes"){
        vaccines<-lapply(paste0(vaccines_pop, subpopulations_names[s],"/",vaccines_files[[vx_files]]), readRDS)
      } else {
        vaccines<-lapply(paste0(vaccines_pop, vaccines_files[[vx_files]]), readRDS)
      }
      vaccines<-do.call(rbind,vaccines)
      vaccines<-vaccines[!duplicated(person_id)]
      #create char: number of characters for vx_atc
      vaccines[,char:=nchar(vx_atc)]
      #remove rows where atc code<3 characters
      vaccines<-vaccines[char>=3]
      #remove char
      vaccines[,char:=NULL]
      #remove person_id already calculated
      vaccines<-vaccines[!person_id %in% person_ids]
      if(vaccines[,.N]>0){
        person_ids<-c(person_ids,unique(vaccines[["person_id"]]))
        output<-CountPersonTime2(Dataset = unique(vaccines[,.(person_id, birth_date, start_follow_up,end_follow_up)]),
                                 Person_id = "person_id",
                                 Start_study_time =start_study_date2,
                                 End_study_time = end_study_date2,
                                 Start_date = "start_follow_up",
                                 End_date = "end_follow_up",
                                 Birth_date = "birth_date",
                                 Increment = "year",
                                 Unit_of_age = "year",
                                 include_remaning_ages = TRUE,
                                 Aggregate = F,
                                 Age_bands = c(0,11,19,29,39,49,55),
                                 print = F, 
                                 check_overlap = F)
        #remove all data before 12 years old or after 55
        output<-output[Ageband != paste0("0-", min_age_preg-1)]
        output<-output[Ageband != paste0(max_age_preg+1,"+")]
        
        #trasform from days into person-years
        output<-output[,Persontime:=round(Persontime/365.25,3)]
        no_subjects<-output[,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("year","Ageband"), .SDcols="person_id"]
        no_subjects_agg<-output[,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("year"), .SDcols="person_id"]
        output<-output[,lapply(.SD, sum), by=c("year","Ageband"), .SDcols="Persontime"]
        #results will be used only for py
        rm(vaccines)  
        saveRDS(output, paste0(vaccines_tmp, names(vaccines_files)[vx_files], "_rates_py.rds"))
        saveRDS(no_subjects_agg, paste0(vaccines_tmp, names(vaccines_files)[vx_files], "_subjects_agg.rds"))
        saveRDS(no_subjects, paste0(vaccines_tmp, names(vaccines_files)[vx_files], "_subjects.rds"))
        
        rm(output,no_subjects_agg,no_subjects)
      }
    }
  }
  
  ####################################
  #Calculate person time for all subjects that never had a prescription(are part of study population but not the medicicines_study_population)
  ####################################
  vx_no_rx_files<-list.files(vaccines_tmp, "vx_id_no_rx.rds")
  if(length(vx_no_rx_files)>0){
    pers_not_med<-readRDS(paste0(vaccines_tmp,"vx_id_no_rx.rds"))
    
    output<-CountPersonTime2(Dataset = unique(pers_not_med[,.(person_id, birth_date, start_follow_up,end_follow_up)]),
                             Person_id = "person_id",
                             Start_study_time =start_study_date2,
                             End_study_time = end_study_date2,
                             Start_date = "start_follow_up",
                             End_date = "end_follow_up",
                             Birth_date = "birth_date",
                             Increment = "year",
                             Unit_of_age = "year",
                             include_remaning_ages = TRUE,
                             Aggregate = F,
                             Age_bands = c(0,11,19,29,39,49,55),
                             print = F, 
                             check_overlap = F)
    #remove all data before 12 years old or after 55
    output<-output[Ageband != paste0("0-", min_age_preg-1)]
    output<-output[Ageband != paste0(max_age_preg+1,"+")]
    
    #trasform from days into person-years
    output<-output[,Persontime:=round(Persontime/365.25,3)]
    no_subjects<-output[,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("year","Ageband"), .SDcols="person_id"]
    no_subjects_agg<-output[,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("year"), .SDcols="person_id"]
    output<-output[,lapply(.SD, sum), by=c("year","Ageband"), .SDcols="Persontime"]
    #results will be used only for py
    rm(pers_not_med)  
    saveRDS(output, paste0(vaccines_tmp, "no_id_rates_py.rds"))
    saveRDS(no_subjects_agg, paste0(vaccines_tmp, "no_id_subjects_agg.rds"))
    saveRDS(no_subjects, paste0(vaccines_tmp, "no_id_subjects.rds"))
    
    rm(output,no_subjects_agg,no_subjects)
    
  }
  
  
  #####################################
  #combine results
  ####################################
  print("Calculating number of records and users.")
  records_files<-list.files(vaccines_tmp, "rates_users_records")  
  no_records<-readRDS(paste0(vaccines_tmp,records_files[[1]]))  
  no_records<-no_records[,lapply(.SD,sum), by=c("person_id","age_band","year","truncated_atc_code"), .SDcols="count_vaccines"]
  no_users<-no_records[count_vaccines!=0,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("age_band","year","truncated_atc_code"), .SDcols="person_id"]
  no_records_agg<-no_records[,lapply(.SD,sum), by=c("truncated_atc_code","year"), .SDcols="count_vaccines"]
  no_users_agg<-no_records[count_vaccines!=0,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("truncated_atc_code","year"), .SDcols="person_id"]
  
  rec_files<-2
  while(rec_files <= length(records_files)){
    no_records_2<-readRDS(paste0(vaccines_tmp,records_files[[rec_files]]))
    no_records_2<-no_records_2[,lapply(.SD,sum), by=c("person_id","age_band","year","truncated_atc_code"), .SDcols="count_vaccines"]
    no_users<-rbind(no_users,no_records_2[count_vaccines!=0,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("age_band","year","truncated_atc_code"), .SDcols="person_id"])
    no_records_agg<-rbind(no_records_agg,no_records_2[,lapply(.SD,sum), by=c("truncated_atc_code","year"), .SDcols="count_vaccines"])
    no_users_agg<-rbind(no_users_agg,no_records_2[count_vaccines!=0,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("truncated_atc_code","year"), .SDcols="person_id"])
    no_records<-rbind(no_records, no_records_2)
    no_records<-no_records[,lapply(.SD,sum), by=c("person_id","age_band","year","truncated_atc_code"), .SDcols="count_vaccines"]
    rm(no_records_2)
    rec_files<-rec_files+1
  }
  no_records<-no_records[,lapply(.SD,sum), by=c("truncated_atc_code","year","age_band"), .SDcols="count_vaccines"]
  no_users<-no_users[,lapply(.SD,sum), by=c("truncated_atc_code","year","age_band"), .SDcols="person_id"]
  setnames(no_users,"person_id","no_users")
  
  no_records_agg<-no_records_agg[,lapply(.SD,sum), by=c("year", "truncated_atc_code"), .SDcols="count_vaccines"]
  no_users_agg<-no_users_agg[,lapply(.SD,sum), by=c("year","truncated_atc_code"), .SDcols="person_id"]
  setnames(no_users_agg,"person_id","no_users")
  
  #NUMBER OF SUBJECTS by year
  subjects_agg_files<-list.files(vaccines_tmp, "subjects_agg")  
  no_subjects_agg<-lapply(paste0(vaccines_tmp,subjects_agg_files),readRDS)
  no_subjects_agg<-do.call(rbind,no_subjects_agg)
  names(no_subjects_agg)<-c("year","no_subjects")
  no_subjects_agg<-no_subjects_agg[,lapply(.SD,sum), by="year", .SDcols="no_subjects"]
  
  #delete files
  for (i in 1:length(subjects_agg_files)){
    file.remove(paste0(vaccines_tmp,subjects_agg_files[[i]]))
  }
  
  #NUMBER OF SUBJECTS by year and age band
  subjects_files<-list.files(vaccines_tmp, "subjects")  
  no_subjects<-lapply(paste0(vaccines_tmp,subjects_files),readRDS)
  no_subjects<-do.call(rbind,no_subjects)
  names(no_subjects)<-c("year","age_band", "no_subjects")
  no_subjects<-no_subjects[,lapply(.SD,sum), by=c("year","age_band"), .SDcols="no_subjects"]
  
  
  #delete files
  for (i in 1:length(subjects_files)){
    file.remove(paste0(vaccines_tmp,subjects_files[[i]]))
  }
  
  #put everything together
  ###################################################################
  #rates in females of child bearing age by year and atc code and age
  ####################################################################
  print("Combine number of records and users in one table.")
  no_records[,age_band:=as.character(age_band)][,year:=as.character(year)][,truncated_atc_code:=as.character(truncated_atc_code)]
  no_users[,age_band:=as.character(age_band)][,year:=as.character(year)][,truncated_atc_code:=as.character(truncated_atc_code)]
  no_subjects[,age_band:=as.character(age_band)][,year:=as.character(year)]
  no_records<-merge(no_records,no_users, by=c("age_band","year","truncated_atc_code"),all=T)
  rm(no_users)
  no_records<-merge(no_records,no_subjects, by=c("age_band","year"),all=T)
  no_records[is.na(no_users),no_users:=0][is.na(count_vaccines),count_vaccines:=0][is.na(truncated_atc_code),truncated_atc_code:="N/A"]
  rm(no_subjects)
  
  no_records_agg[,year:=as.character(year)][,truncated_atc_code:=as.character(truncated_atc_code)]
  no_users_agg[,year:=as.character(year)][,truncated_atc_code:=as.character(truncated_atc_code)]
  no_subjects_agg[,year:=as.character(year)]
  no_records_agg<-merge(no_records_agg,no_users_agg, by=c("truncated_atc_code","year"),all=T)
  rm(no_users_agg)
  no_records_agg<-merge(no_records_agg,no_subjects_agg, by=c("year"),all=T)
  rm(no_subjects_agg)
  no_records_agg[is.na(no_users),no_users:=0][is.na(count_vaccines),count_vaccines:=0][is.na(truncated_atc_code),truncated_atc_code:="N/A"]
  
  
  #delete files
  for (i in 1:length(records_files)){
    file.remove(paste0(vaccines_tmp,records_files[[i]]))
  }
  
  #########
  print("Calculating person time.")
  person_files<-list.files(vaccines_tmp, "rates_py")  
  person_years<-lapply(paste0(vaccines_tmp,person_files), readRDS)
  person_years<-do.call(rbind,person_years)
  person_years<-person_years[,lapply(.SD,sum), by=c("year","Ageband"), .SDcols="Persontime"]
  setnames(person_years, "Persontime", "person_years")
  setnames(person_years, "Ageband", "age_band")
  ##########
  
  #delete files
  for (i in 1:length(person_files)){
    file.remove(paste0(vaccines_tmp,person_files[[i]]))
  }
  
  #######################################################
  #rates by age, year and atc code
  #######################################################
  print("Create table 16: Rate of medicine use in females of child bearing age by year, age band and atc code.")
  no_records[,year:=as.character(year)][,age_band:=as.character(age_band)]
  person_years[,year:=as.character(year)][,age_band:=as.character(age_band)]
  no_records<-merge(no_records,person_years, by=c("year", "age_band"), all=T)
  no_records[,vaccines_per_100_py:=round(((count_vaccines/person_years)*100),2)]
  no_records[,users_per_100_py:=round(((no_users/person_years)*100),2)]
  no_records[,subjects_per_100_py:=round(((no_subjects/person_years)*100),2)]
  
  if(subpopulations_present=="Yes"){
    fwrite(no_records, paste0(vacc_dir, subpopulations_names[s], "/", subpopulations_names[s], "_vaccines_rates_year_age_atc.csv"), row.names = F)
  } else {
    fwrite(no_records, paste0(vacc_dir, "vaccines_rates_year_age_atc.csv"), row.names = F)
  }
  
  ################
  #Apply masking
  ################
  
  no_records[, count_vaccines:= as.character(count_vaccines)][as.numeric(count_vaccines) > 0 & as.numeric(count_vaccines) < 5, count_vaccines := "<5"]
  no_records[, no_users:= as.character(no_users)][as.numeric(no_users) > 0 & as.numeric(no_users) < 5, no_users := "<5"]
  no_records[, no_subjects:= as.character(no_subjects)][as.numeric(no_subjects) > 0 & as.numeric(no_subjects) < 5, no_subjects := "<5"]
  no_records[, person_years:= as.character(person_years)][as.numeric(person_years) > 0 & as.numeric(person_years) < 5, person_years := "<5"]
  no_records[, vaccines_per_100_py:= as.character(vaccines_per_100_py)][count_vaccines=="<5" | person_years=="<5", vaccines_per_100_py := "N/A"]
  no_records[, users_per_100_py:= as.character(users_per_100_py)][no_users=="<5" | person_years=="<5", users_per_100_py := "N/A"]
  no_records[, subjects_per_100_py:= as.character(subjects_per_100_py)][no_subjects=="<5" | person_years=="<5", subjects_per_100_py := "N/A"]
  
  if(subpopulations_present=="Yes"){
    fwrite(no_records, paste0(vacc_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_vaccines_rates_year_age_atc_masked.csv"), row.names = F)
  } else {
    fwrite(no_records, paste0(vacc_dir, "Masked/", "vaccines_rates_year_age_atc_masked.csv"), row.names = F)
  }
  rm(no_records)
  
  #######################################################
  #rates by year and atc code
  #######################################################
  print("Create table 15: Rate of medicine use in females of child bearing age by year and atc code.")
  #combine person years
  person_years<-person_years[,lapply(.SD,sum), by="year", .SDcols="person_years"]
  no_records_agg[,year:=as.character(year)]
  person_years[,year:=as.character(year)]
  no_records_agg<-merge(no_records_agg,person_years, by=c("year"), all=T)
  
  no_records_agg[,vaccines_per_100_py:=round(((count_vaccines/person_years)*100),2)]
  no_records_agg[,users_per_100_py:=round(((no_users/person_years)*100),2)]
  no_records_agg[,subjects_per_100_py:=round(((no_subjects/person_years)*100),2)]
  
  if(subpopulations_present=="Yes"){
    fwrite(no_records_agg, paste0(vacc_dir, subpopulations_names[s], "/", subpopulations_names[s], "_vaccines_rates_year_atc.csv"), row.names = F)
  } else {
    fwrite(no_records_agg, paste0(vacc_dir, "vaccines_rates_year_atc.csv"), row.names = F)
  }
  
  ################
  #Apply masking
  ################
  
  no_records_agg[, count_vaccines:= as.character(count_vaccines)][as.numeric(count_vaccines) > 0 & as.numeric(count_vaccines) < 5, count_vaccines := "<5"]
  no_records_agg[, no_users:= as.character(no_users)][as.numeric(no_users) > 0 & as.numeric(no_users) < 5, no_users := "<5"]
  no_records_agg[, no_subjects:= as.character(no_subjects)][as.numeric(no_subjects) > 0 & as.numeric(no_subjects) < 5, no_subjects := "<5"]
  no_records_agg[, person_years:= as.character(person_years)][as.numeric(person_years) > 0 & as.numeric(person_years) < 5, person_years := "<5"]
  no_records_agg[, vaccines_per_100_py:= as.character(vaccines_per_100_py)][count_vaccines=="<5" | person_years=="<5", vaccines_per_100_py := "N/A"]
  no_records_agg[, users_per_100_py:= as.character(users_per_100_py)][no_users=="<5" | person_years=="<5", users_per_100_py := "N/A"]
  no_records_agg[, subjects_per_100_py:= as.character(subjects_per_100_py)][no_subjects=="<5" | person_years=="<5", subjects_per_100_py := "N/A"]
  
  if(subpopulations_present=="Yes"){
    fwrite(no_records_agg, paste0(vacc_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_vaccines_rates_year_atc_masked.csv"), row.names = F)
  } else {
    fwrite(no_records_agg, paste0(vacc_dir, "Masked/", "vaccines_rates_year_atc_masked.csv"), row.names = F)
  }
  
  rm(no_records_agg) 
  
}






