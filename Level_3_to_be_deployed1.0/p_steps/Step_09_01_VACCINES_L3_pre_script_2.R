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
  vx_date_miss<-list() #number of record with missing date admin/record
  years<-list()
  vx_sex_not_specified<-list() #number of records with unspecified sex
  ######################
  vx_out_st_per<-list() #number of vaccines records outside the observation period(check is done on individual level)
  vx_study_pop_obsper<-list() #number of records in the study population with date admin/record inside study period
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
  #Tab 15
  ############################################################################
  no_type_m<-list() #number of records with missing vx_type but complete vx_atc by meaning
  no_dose_m<-list() #number of records with missing vx_dose but complete vx_atc by meaning
  no_manufacturer_num<-list() #number of records with empty vx_manufacturer but complete vx_atc by meaning
  no_type_t<-list() #total number of records with empty vx_type but complete vx_atc
  no_dose_t<-list()  #total number of records with empty vx_dose but complete vx_atc
  no_manufacturer_t<-list() #total number of records with empty vx_manufacturer
   ######################################################################################################################
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
    df<-fread(paste(path_dir, actual_tables$VACCINES[y], sep=""), stringsAsFactors = FALSE, colClasses = "character")
    df<-df[,c("person_id", "vx_atc", "vx_admin_date", "vx_record_date", "vx_type", "vx_dose", "vx_manufacturer", "meaning_of_vx_record")]
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
    pers_stdpop_not_vx<-df[rowSums(is.na(df[,..colnames]))==length(colnames), ..std_names] #subjects id present in the study population but that do not have a admin/record
    pers_stdpop_not_vx<-pers_stdpop_not_vx[!duplicated(person_id)]
    df<-df[!rowSums(is.na(df[,..colnames]))==length(colnames)]
    if(pers_stdpop_not_vx[,.N]>0){
      if("vaccines_rec" %in% names(pers_stdpop_not_vx)){
        pers_stdpop_not_vx[,vaccines_rec:=NULL]
      }
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
    #number of records with both date admin/record missing
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
    
    ############################
    #Table 15
    ############################
    #stratified by meaning
    no_type_m[[w]]<-df[is.na(vx_type) & !is.na(vx_atc), .N, by="meaning"]
    no_dose_m[[w]]<-df[is.na(vx_dose) & !is.na(vx_atc), .N, by="meaning"]
    no_manufacturer_num[[w]]<-df[is.na(vx_manufacturer) & !is.na(vx_atc), .N, by="meaning"]
    #total number of records
    no_type_t[[w]]<-df[is.na(vx_type) & !is.na(vx_atc), .N]
    no_dose_t[[w]]<-df[is.na(vx_dose) & !is.na(vx_atc), .N]
    no_manufacturer_t[[w]]<-df[is.na(vx_manufacturer) & !is.na(vx_atc), .N]
    #remove uneccessary columns
    df[,c("vx_type", "vx_dose", "vx_manufacturer"):=NULL]
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
    #save all person ids that are part of the medicines_study_population
    ##############################
    # if (df[,.N]>0){
    #   saveRDS(df[!duplicated(person_id),c("person_id","birth_date","start_follow_up","end_follow_up")], paste0(vaccines_tmp, paste0("med_id_stdpop_", actual_tables$VACCINES[y], ".rds")))
    # }
    ##############################
    #First section of the SAP
    ##############################
    #records in the raw table ==orig_no_rows
    #subjects in the study population but not in vaccines == stdpop_not_med
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
}