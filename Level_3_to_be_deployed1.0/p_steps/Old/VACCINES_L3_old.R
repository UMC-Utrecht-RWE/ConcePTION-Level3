library(data.table)
`%!in%` = Negate(`%in%`)
#females age
min_age_preg<-12
max_age_preg<-55
#Get VACCINES & VACCINES tables
actual_tables<-list()
actual_tables$VACCINES<-list.files(path_dir, pattern="^VACCINES")
start_study_date<-as.Date("19950101", "%Y%m%d")

#Get cdm_source file name
cdm_source_file<-list.files(path_dir, pattern="^CDM_SOURCE")
#Get DAP info and date createion fro CDM_SOURCE
CDM_SOURCE<-fread(paste0(path_dir, cdm_source_file))
date_creation<-CDM_SOURCE[["date_creation"]]
data_access_provider_name<-CDM_SOURCE[["data_access_provider_name"]]
data_source_name<-CDM_SOURCE[["data_source_name"]]
rm(CDM_SOURCE)

#load study_population
populations_dir<-paste0(g_intermediate,"populations/")
#load study_population
study_population_dir<-list.files(paste0(g_intermediate,"populations/"), pattern="study_population")
study_population<-readRDS(paste0(g_intermediate, "populations/", study_population_dir))
nr_std<-study_population[,.N]


#functions
#calculate the number of records for desired atc level by meaning and year
#counts are stratified by meaning,year and atc code
#total are stratified by meaning and year
m_year_atc<-function(dt, year_var, meaning_var, atc_var, level_num){
  dt[,meaning:=dt[[meaning_var]]]
  dt[,atc_sub:=substr(get(atc_var),1,level_num)]
  
  a.1<-dt[complete.cases(meaning) & complete.cases(year) & complete.cases(atc_sub), .N, by=.(year, meaning, atc_sub)]
  setnames(a.1, "N", "count")
  setnames(a.1, "atc_sub", paste0("atc_code_", level_num))
  a.2<-dt[complete.cases(meaning) & complete.cases(year) & complete.cases(atc_sub), .N, by=.(meaning, year)]
  setnames(a.2, "N", "total")
  
  results<-list("count"=a.1, "total"=a.2)
  return(results)
}

#medicines_vaccines_tmp/VACCINES folder where all intermediary files are saved
if ("VACCINES" %in% list.files(tmp)){
  vaccines_tmp<-paste(tmp, "VACCINES/", sep="")
  do.call(file.remove, list(list.files(vaccines_tmp, full.names = T)))
}else{
  #Create the VACCINES folder in the output dir
  dir.create(paste(tmp, "VACCINES", sep=""))
  vaccines_tmp<-paste(tmp, "VACCINES/", sep="")
}

#output folder for VACCINES report in g_output
if ("VACCINES" %in% list.files(output_dir)){
  vaccines_dir<-paste(output_dir, "VACCINES/",sep="")
  do.call(file.remove, list(list.files(vaccines_dir, full.names = T)))
  vaccines_less<-paste(vaccines_dir, "Masked/", sep="")
  do.call(file.remove, list(list.files(vaccines_less, full.names = T)))
} else {
  #Create the VACCINES folder in the output dir
  dir.create(paste(output_dir, "VACCINES", sep=""))
  vaccines_dir<-paste(output_dir, "VACCINES/", sep="")
  dir.create(paste(vaccines_dir,"Masked", sep=""))
  vaccines_less<-paste(vaccines_dir, "Masked/", sep="")
}

#output folder to save medicines_study_population by sex, atc level 1 and year
if ("VACCINES" %in% list.files(populations_dir)){
  vaccines_pop<-paste(populations_dir, "VACCINES/", sep="")
  do.call(file.remove, list(list.files(vaccines_pop, full.names = T)))
} else {
  #Create the EVENTS folder in the output dir
  dir.create(paste(populations_dir, "VACCINES", sep=""))
  vaccines_pop<-paste(populations_dir, "VACCINES/", sep="")
}


if (length(actual_tables$VACCINES)==0){
  print("There is no VACCINES table present in the working directory.")
} else {
  ###################################################
  #List for saving info
  ###################################################
  print("Creating lists to save the information.")
  orig_no_rows<-list() #original number of records in the VACCINES table
  #######################
  #pers_stdpop_not_vx
  vx_study_pop<-list() #number of records for the study population, no selection criteria for time applied
  vx_date_miss<-list() #number of record with missing date dispensing/prescription
  years<-list()
  ######################
  vx_out_st_per<-list() #number of VACCINES records outside the observation period(check is done on individual level)
  vx_study_pop_obsper<-list() #number of records in the study population with date dispensing/prescription inside study period
  ######################
  vx_stdpop_no_meaning<-list() #number of records in the study population with no meaning
  meanings<-list() #all meanings present
  #############################################################################
  vx_study_population<-list() #number of records in the study population
  vx_study_population_meaning<-list() #number of records in the study population by meaning
  male_population<-list() #save whether males are included
  female_population<-list() #save whether females are included
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
  vx_study_population_f<-list() #number of records in females [12-55] years old
  ##############################
  females_childbearing<-list() #check if females of childbearing age are available
  ###############################
  #Table 12
  ##############################
  w<-1
  ###############################################
  #for_loop
  ##############################################
  
  for (y in 1:length(actual_tables$VACCINES)){
    #Load the table
    df<-fread(paste(path_dir, actual_tables$VACCINES[y], sep=""), stringsAsFactors = FALSE)
    df<-df[,c("person_id", "vx_atc", "vx_admin_date", "vx_record_date", "meaning_of_vx_record")]
    df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately
    
    colnames<-names(df)
    std_names<-names(study_population)
    colnames<-colnames[!colnames %in% "person_id"]
    #get the total number of records(a)
    orig_no_rows[[w]]<-df[,.N]
    
    #merge with the study_population table(there is no missing data in this table)
    df<-df[study_population,on=.(person_id)]#left join, keeps all people in the study population even if they didn't have a prescription
    df<-df[,age_fup:=as.numeric(age_fup)]
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
    #number of records with both date dispensing/prescription missing
    vx_date_miss[[w]]<-df[is.na(year),.N]
    #remove records with both dates missing
    df<-df[!is.na(year)]
    years[[w]]<-unique(df[["year"]])
    #remove records that are outside the obs_period for all subjects
    vx_out_st_per[[w]]<-df[vx_admin_date<start_follow_up | vx_admin_date>exit_spell_category | vx_record_date<start_follow_up | vx_record_date>exit_spell_category,.N] #number of records outside study population
    df[(vx_admin_date<start_follow_up | vx_admin_date>exit_spell_category) | (vx_record_date<start_follow_up | vx_record_date>exit_spell_category), obs_out:=1]
    df<-df[is.na(obs_out)] #remove records outside study period
    df[,obs_out:=NULL] 
    vx_study_pop_obsper[[w]]<-df[,.N] #number of records after removing records outside study period
    vx_stdpop_no_meaning[[w]]<-df[is.na(meaning_of_vx_record),.N] #number of records with empty meaning
    df<-df[!is.na(meaning_of_vx_record)] #remove records with empty meaning
    #########
    meanings[[w]]<-unique(df[["meaning_of_vx_record"]])
    ############################
    vx_study_population[[w]]<-df[,.N] #number of records in the study population
    vx_study_population_meaning[[w]]<-df[,.N, by="meaning_of_vx_record"] #number of records in the study population by meaning
    ############################
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
    #empty_atc_code_m[[w]]<-df[is.na(vx_atc),.N, by="meaning_of_vx_record"]
    #number of records with missing atc codes by meaning and year(numerator)
    empty_atc_code_m_y[[w]]<-df[is.na(vx_atc), .N, by=c("meaning_of_vx_record", "year")]
    #total row
    #total records by meaning(numerator): vx_study_population_meaning
    
    #counts by meaning and year for atc trunacted to the first level
    Res.1<-m_year_atc(dt=df,
                      year_var = "year",
                      meaning_var = "meaning_of_vx_record",
                      atc_var = "vx_atc",
                      level_num = 1) #export results to vaccines_tmp with name Res_1_name of original file
    Res.1<-Res.1$count
    saveRDS(Res.1, paste0(vaccines_tmp, paste0("Res.1_", actual_tables$VACCINES[y], ".rds"))) #allows to save data as list
    rm(Res.1) 
    #################################
    #Table 11:
    #################################
    if(df[sex_at_instance_creation=="F" & age_fup>=12 & age_fup<=55,.N]>0){
      #empty row
      #number of records with missing atc codes by meaning in females 12-55 years old
      #empty_atc_code_m_f[[w]]<-df[is.na(vx_atc) & sex_at_instance_creation=="F" & age_fup>=12 & age_fup<=55, .N, by="meaning_of_vx_record"]
      #number of records with missing atc codes by meaning and year in females 12-55 years old(numerator)
      empty_atc_code_m_y_f[[w]]<-df[is.na(vx_atc) & sex_at_instance_creation=="F" & age_fup>=12 & age_fup<=55 & !is.na(year), .N, by=c("meaning_of_vx_record","year")]
      #total row
      #total records by meaning(numerator): vx_study_population_meaning_f
      vx_study_population_meaning_f[[w]]<-df[sex_at_instance_creation=="F" & age_fup>=12 & age_fup<=55,.N, by="meaning_of_vx_record"]
      #total records(denominator): vx_study_population_f
      #vx_study_population_f[[w]]<-df[sex_at_instance_creation=="F" & age_fup>=12 & age_fup<=55,.N]
      #counts by meaning and year for atc trunacted to the first level in females [12-55]
      Res.2<-m_year_atc(dt=df[sex_at_instance_creation=="F" & age_fup>=12 & age_fup<=55],
                        year_var = "year",
                        meaning_var = "meaning_of_vx_record",
                        atc_var = "vx_atc",
                        level_num = 1) #export results to vaccines_tmp with name Res_2_name of original file
      Res.2<-Res.2$count
      saveRDS(Res.2, paste0(vaccines_tmp, paste0("Res.2_", actual_tables$VACCINES[y], ".rds")))
      rm(Res.2)
      females_childbearing[[w]]<-1
    } else {females_childbearing[[w]]<-0}
    
    ##################################
    #Table 12:Info
    ##################################

    ############################################
    #number of male users and median for male users:output of calculation dataset by atc level 1 and year
    ############################################
    df[,atc_code_1:=substr(vx_atc,1,1)]
    if(df[sex_at_instance_creation=="M",.N]>0){
      years_this_table<-unique(do.call(rbind,years))
      male_population[[w]]<-1
      for (a in 1:length(LETTERS)){
        for (b in 1:length(years_this_table)){
          if(df[sex_at_instance_creation=="M" & atc_code_1==LETTERS[a] & year==years_this_table[b],.N]>0){
            saveRDS(df[sex_at_instance_creation=="M" & atc_code_1==LETTERS[a] & year==years_this_table[b], c("person_id", "year", "meaning_of_vx_record", "vx_atc")], paste0(vaccines_pop, paste0(LETTERS[a], "_", years_this_table[b], "_m_population_", actual_tables$VACCINES[y], ".rds")))
          }
        }
      } 
    }else {male_population[[w]]<-0}
    
    
    ####################################################
    #number of female users and median for female users:output of calculation dataset by atc level 1 and year
    ####################################################
    if(df[sex_at_instance_creation=="F",.N]>0){
      years_this_table<-unique(do.call(rbind,years))
      female_population[[w]]<-1
      for (a in 1:length(LETTERS)){
        for (b in 1:length(years_this_table)){
          if(df[sex_at_instance_creation=="F" & atc_code_1==LETTERS[a] & year==years_this_table[b],.N]>0){
            saveRDS(df[sex_at_instance_creation=="F" & atc_code_1==LETTERS[a] & year==years_this_table[b], c("person_id", "year", "meaning_of_vx_record", "vx_atc","age_fup")], paste0(vaccines_pop, paste0(LETTERS[a], "_", years_this_table[b], "_f_population_", actual_tables$VACCINES[y], ".rds")))
          }
        }
      } 
    }else {female_population[[w]]<-0}
    
    
    ########
    w<-w+1
    rm(df)
    ########
  }
  
  #################################################################################################
  #Flowchart
  ################################################################################################
  print("Creating flowchart.")
  print("Get number of records in the original table.")
  #original number of records in the VACCINES table(flowchart 1)
  orig_no_rows<-do.call(rbind,orig_no_rows)
  orig_no_rows<-sum(orig_no_rows)
  #number of records for the study population, no selection criteria for time applied (flowchart 2)
  print("Get number of records for the study population (no time criteria applied).")
  vx_study_pop<-do.call(rbind,vx_study_pop)
  vx_study_pop<-sum(vx_study_pop)
  #number of records with both dates missing
  vx_date_miss<-do.call(rbind,vx_date_miss)
  vx_date_miss<-sum(vx_date_miss)
  #number of VACCINES records outside the observation period(check is done on individual level) (flowchart 3)
  print("Get number of records outside observation period.")
  vx_out_st_per<-do.call(rbind,vx_out_st_per) 
  vx_out_st_per<-sum(vx_out_st_per)
  #number of records in the study population with date dispensing/prescription inside study period (flowchart 4)
  print("Get number of records for the study population(time criteria applied).")
  vx_study_pop_obsper<-do.call(rbind,vx_study_pop_obsper) 
  vx_study_pop_obsper<-sum(vx_study_pop_obsper)
  #number of records in the study population with no meaning (flowchart 5)
  print("Get number of records with no meaning.")
  vx_stdpop_no_meaning<-do.call(rbind,vx_stdpop_no_meaning) 
  vx_stdpop_no_meaning<-sum(vx_stdpop_no_meaning) 
  #number of records in the study population
  print("Get number of records for study population.")
  vx_study_population<-do.call(rbind,vx_study_population) 
  vx_study_population<-sum(vx_study_population) 
  
  #Flowchart
  print("Create flowchart.")
  flowchart<-data.table(indicator=c("Number of records in the original table", 
                                    "Number of records for the study_population(no time criteria)",
                                    "Exclude: Number of records with both date dispensing and date prescription missing",
                                    "Exclude: Number of records with date dispensing/prescription outside study period",
                                    "Number of records for the study_population(time criteria applied)",
                                    "Exclude:Number of records with empty meaning",
                                    "Number of records for study_population"), 
                        count=c(orig_no_rows,
                                vx_study_pop,
                                vx_date_miss,
                                vx_out_st_per,
                                vx_study_pop_obsper,
                                vx_stdpop_no_meaning,
                                vx_study_population))
  saveRDS(flowchart,paste0(vaccines_dir,"flowchart.rds"))
  ##################################################################################################
  #Description
  ##################################################################################################
  print("Creating description of study population.")
  #meanings
  print("Get list of meanings.")
  meanings<-do.call(rbind,meanings)
  meanings<-unique(c(meanings))
  meanings_des<-paste(meanings, collapse = ", ")
  #study years
  years<-do.call(rbind,years)
  years<-unique(c(years))
  years_des<-paste(sort(years), collapse=",")
  #
  male_population<-do.call(rbind, male_population)
  male_population<-sum(male_population)
  female_population<-do.call(rbind, female_population)
  female_population<-sum(female_population)
  if(male_population>0 & female_population>0){sex_included<-c("Males, Females")}
  if(male_population==0 & female_population>0){sex_included<-c("Females")}
  if(male_population>0 & female_population==0){sex_included<-c("Males")}
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
    stdpop_not_vx<-stdpop_not_vx[,.N]
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
  description<-data.table(indicator=c("List of meanings present",
                                      "Years included in the study period",
                                      "Sex included in the study population",
                                      "Number of subjects in the original study population table",
                                      "Number of subjects without dispensing/prescriptions in the study population",
                                      "Number of records with empty ATC codes when vx_admin_date/prescription is present",
                                      "Number of records with complete ATC codes",
                                      "Number of records with complete ATC code to level 1",
                                      "Number of records with complete ATC code to level 2",
                                      "Number of records with complete ATC code to level 3",
                                      "Number of records with complete ATC code to level 4",
                                      "Number of records with complete ATC code to level 5",
                                      "Number of records with complete ATC code to level 6",
                                      "Number of records with complete ATC code to level 7"), 
                          count=c(meanings_des,
                                  years_des,
                                  sex_included,
                                  nr_std,
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
  saveRDS(description,paste0(vaccines_dir,"description.rds")) 
  std_files<-list.files(vaccines_tmp, pattern="^stdpop_not_vx_")
  for(i in 1:length(std_files)){
    unlink(paste0(vaccines_tmp,std_files[i]))
  }
  rm(std_files)
  
  #################################################################################################
  #Table 10: Number of prescriptions/dispensings by ATC A level in the study population by year of dispensing/prescribing and by meaning_of_vx_record
  #################################################################################################
  print("Creating Table 10:  Number of prescriptions/dispensings by ATC A level in the study population by year of dispensing/prescribing and by meaning_of_vx_record.")
  print("Get all variables.")
  #empty atc codes by meaning
  empty_atc_code_m_y<-do.call(rbind,empty_atc_code_m_y)
  empty_atc_code_m_y<-empty_atc_code_m_y[,lapply(.SD, sum), .SDcols="N", by=c("meaning_of_vx_record", "year")]
  names(empty_atc_code_m_y)<-c("meaning","year", "count")
  empty_atc_code_m_y[,atc_code_1:="empty"]
  vx_study_population_meaning<-do.call(rbind,vx_study_population_meaning)
  vx_study_population_meaning_f<-do.call(rbind,vx_study_population_meaning_f)
  #counts by meaning, year and atc level 1:load Res.1 
  Res.1_files<-list.files(vaccines_tmp,pattern="^Res.1")
  
  if (length(Res.1_files)>0){
    Res.1<-readRDS(paste0(vaccines_tmp,Res.1_files[1]))
    i<-2
    while(i <= length(Res.1_files)){
      a<-readRDS(paste0(vaccines_tmp, Res.1_files[i]))
      Res.1<-rbind(Res.1, a)
      rm(a)
      Res.1<-Res.1[,lapply(.SD, sum), .SDcols="count", by=c("meaning", "year", "atc_code_1")]
      i<-i+1
    }
  } else {Res.1<-NULL}
  
  total<-data.table(vx_study_population_meaning, year="total", atc_code_1=NA)
  setnames(total,"meaning_of_vx_record","meaning")
  setnames(total,"N","count")
  Res.1<-rbind(Res.1,total)
  #remove all files in Res.1_files
  #remove files not needed from vaccines_tmp
  for(i in 1:length(Res.1_files)){
    unlink(paste0(vaccines_tmp,Res.1_files[i]))
  }
  
  print("Create table 10.")
  tab10<-data.table(rbind(Res.1, empty_atc_code_m_y))
  setcolorder(tab10, c("meaning","year", "atc_code_1","count"))
  setorderv(tab10, c("meaning", "year", "atc_code_1"))
  rm(Res.1,Res.1_files)
  
  print("Export table 10.")
  tab10<-data.table(DAP=data_access_provider_name,data_source=data_source_name,indicator="no_of_records", tab10)
  saveRDS(tab10, paste0(vaccines_dir, "tab10.rds"))
  rm(tab10)
  
  #################################################################################################
  #Table 11: Number of prescriptions/dispensings by ATC A level in the female study population of childbearing age 12-55 years (based on age at Start_study_fup) by year of dispensing/prescribing and by meaning_of_vx_record
  #################################################################################################
  females_childbearing<-do.call(rbind,females_childbearing)
  females_childbearing<-sum(females_childbearing)
  if(females_childbearing>0){
    print("Creating Table 11:  Number of prescriptions/dispensings by ATC A level in the female study population of childbearing age 12-55 years (based on age at Start_study_fup) by year of dispensing/prescribing and by meaning_of_vx_record.")
    print("Get all variables.")
    
    #empty atc codes by meaning
    empty_atc_code_m_y_f<-do.call(rbind,empty_atc_code_m_y_f)
    empty_atc_code_m_y_f<-empty_atc_code_m_y_f[,lapply(.SD, sum), .SDcols="N", by=c("meaning_of_vx_record", "year")]
    names(empty_atc_code_m_y_f)<-c("meaning","year", "count")
    empty_atc_code_m_y_f[,atc_code_1:="empty"]
    #counts by meaning, year and atc level 1:load Res.2 
    Res.2_files<-list.files(vaccines_tmp,pattern="^Res.2")
    if (length(Res.2_files)>0){
      Res.2<-readRDS(paste0(vaccines_tmp,Res.2_files[1]))
      i<-2
      while(i <= length(Res.2_files)){
        a<-readRDS(paste0(vaccines_tmp, Res.2_files[i]))
        Res.2<-rbind(Res.2,a)
        rm(a)
        Res.2<-Res.2[,lapply(.SD, sum), .SDcols="count", by=c("meaning", "year", "atc_code_1")]
        i<-i+1
      }
    } else {Res.2<-NULL}
    
    #remove all files in Res.2_files
    #remove files not needed from vaccines_tmp
    for(i in 1:length(Res.2_files)){
      unlink(paste0(vaccines_tmp,Res.2_files[i]))
    }
    
    tab11<-data.table(rbind(Res.2, empty_atc_code_m_y_f))
    setcolorder(tab11, c("meaning","year", "atc_code_1","count"))
    setorderv(tab11, c("meaning", "year", "atc_code_1"))
    rm(Res.2,Res.2_files)
    print("Export table 11.")
    
    tab11<-data.table(DAP=data_access_provider_name,data_source=data_source_name,indicator="no_of_records", tab11)
    saveRDS(tab11,paste0(vaccines_dir, "tab11.rds"))
    rm(tab11)
  } else {
    print("Counts for females of childbearing age cannot be esstimated due to missingness of the data for this subpopulation.")
  }
  ##################################
  #Table 12:
  ##################################
  print("Creating Table 12:  Number of prescriptions/dispensings by ATC 1, 3 and 4 level in the study population by year of dispensing/prescribing and by meaning_of_vx_record.")
  print("Get all variables.")
    ##############################
  #male users and median prescriptions for male users
  #############################
  if(male_population>0){
    median_males_files<-list.files(vaccines_pop,pattern="m_population")
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
      
      i<-1
      for (i in 1:length(list_median_males)){
        median_male<-readRDS(paste0(vaccines_pop, list_median_males[[i]][1]))
        setnames(median_male, "meaning_of_vx_record", "meaning")
        #count by person_id, atc code meaning and year
        median_male<-median_male[,.(count=.N), by=c("person_id","meaning","year","vx_atc")]
        
        z<-2
        while (z <= length(list_median_males[[i]])){
          a<-readRDS(paste0(vaccines_pop, list_median_males[[i]][[z]]))
          setnames(a, "meaning_of_vx_record", "meaning")
          a<-a[,.(count=.N), by=c("person_id","meaning","year","vx_atc")]
          median_male<-rbind(median_male, a)
          median_male<-median_male[,lapply(.SD, sum), by=c("person_id","meaning","year","vx_atc"), .SDcols="count"]
          z<-z+1
          rm(a)
        }
        
        median_male<-median_male[,atc_level:=nchar(vx_atc)]
        #number of records, male users, median male users by atc_code_7 (table 13)
        if(median_male[atc_level==7,.N]>0){
          res.tab13.m<-median_male[atc_level==7]
          setnames(res.tab13.m,"vx_atc", "atc_code_7")
          #number of records by meaning, year, and atc_code_7
          res.tab13.m_records.my<-res.tab13.m[,lapply(.SD, sum), by=.(atc_code_7, meaning, year),.SDcols="count"]
          setnames(res.tab13.m_records.my,"count","no_records")
          res.tab13.m_records.my[,atc_code_3:=substr(atc_code_7,1,3)]
          saveRDS(res.tab13.m_records.my,paste0(vaccines_tmp,names(list_median_males)[i], "_tab13_m_rec_7.my_", i, ".rds"))
          rm(res.tab13.m_records.my)
          #number of records by atc_code_7
          res.tab13.m_records.t<-res.tab13.m[,lapply(.SD,sum),by=.(person_id,atc_code_7), .SDcols="count"][,lapply(.SD, sum), by=.(atc_code_7),.SDcols="count"]
          res.tab13.m_records.t[,meaning:="All"][,year:="All"]
          setnames(res.tab13.m_records.t,"count","no_records")
          res.tab13.m_records.t[,atc_code_3:=substr(atc_code_7,1,3)]
          saveRDS(res.tab13.m_records.t,paste0(vaccines_tmp,names(list_median_males)[i], "_tab13_m_rec_7.t_", i, ".rds"))
          rm(res.tab13.m_records.t)
          #number of male users by meaning, year and atc_code_7
          res.tab13.m_users.c_7<-res.tab13.m[,lapply(.SD,function(x) length(unique(x))), by=.(atc_code_7, meaning, year),.SDcols="person_id"]
          setnames(res.tab13.m_users.c_7,"person_id","no_male_users")
          res.tab13.m_users.c_7[,atc_code_3:=substr(atc_code_7,1,3)]
          saveRDS(res.tab13.m_users.c_7,paste0(vaccines_tmp,names(list_median_males)[i], "_tab13_m_users_7.my_", i, ".rds"))
          rm(res.tab13.m_users.c_7)
          #number of male users by atc_code_7
          res.tab13.m_users.t_7<-res.tab13.m[,lapply(.SD,sum),by=.(person_id,atc_code_7), .SDcols="count"][,lapply(.SD,function(x) length(unique(x))), by=.(atc_code_7),.SDcols="person_id"]
          setnames(res.tab13.m_users.t_7,"person_id","no_male_users")
          res.tab13.m_users.t_7[,meaning:="All"][,year:="All"]
          res.tab13.m_users.t_7[,atc_code_3:=substr(atc_code_7,1,3)]
          saveRDS(res.tab13.m_users.t_7,paste0(vaccines_tmp,names(list_median_males)[i], "_tab13_m_users_7.t_", i, ".rds"))
          rm(res.tab13.m_users.t_7)
          #median prescription by meaning, year and atc_code_7
          res.tab13.m_users.vx_7<-res.tab13.m[,lapply(.SD,median),.SDcols="count", by=.(meaning, year, atc_code_7)]
          setnames(res.tab13.m_users.vx_7,"count","median_rx_male_users")
          res.tab13.m_users.vx_7[,atc_code_3:=substr(atc_code_7,1,3)]
          saveRDS(res.tab13.m_users.vx_7,paste0(vaccines_tmp,names(list_median_males)[i],"_tab13_m_median_7.my_",i, ".rds"))
          rm(res.tab13.m_users.vx_7)
          #median prescriptions by atc_code_7
          res.tab13.m_users.vx.t_7<-res.tab13.m[,lapply(.SD,sum),by=.(person_id,atc_code_7), .SDcols="count"][,lapply(.SD,median),.SDcols="count", by=.(atc_code_7)]
          setnames(res.tab13.m_users.vx.t_7,"count","median_rx_male_users")
          res.tab13.m_users.vx.t_7[,meaning:="All"][,year:="All"]
          res.tab13.m_users.vx.t_7[,atc_code_3:=substr(atc_code_7,1,3)]
          saveRDS(res.tab13.m_users.vx.t_7,paste0(vaccines_tmp,names(list_median_males)[i],"_tab13_m_median_7.t_",i, ".rds"))
          rm(res.tab13.m_users.vx.t_7,res.tab13.m)
        }
        
        #############
        #number of records, male users, median male users (table 12)
        if(median_male[atc_level==4|atc_level==5|atc_level==6|atc_level==7,.N]>0){
          median_male<-median_male[,atc_code_4:=substr(vx_atc,1,4)] #create atc_code_4
          #subset only atc codes of interest, aggregate data over atc_code_4
          res.tab12.m<-median_male[atc_level==4|atc_level==5|atc_level==6|atc_level==7][,vx_atc:=NULL][,lapply(.SD,sum), by=c("person_id", "meaning", "year", "atc_code_4","atc_level")]
          #number of records by meaning, year, and atc_code_4
          res.tab12.m_records.my<-res.tab12.m[,lapply(.SD, sum), by=.(atc_code_4, meaning, year),.SDcols="count"]
          setnames(res.tab12.m_records.my,"count","no_records")
          res.tab12.m_records.my[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          saveRDS(res.tab12.m_records.my,paste0(vaccines_tmp,names(list_median_males)[i], "_tab12_m_rec_4.my_", i, ".rds"))
          rm(res.tab12.m_records.my)
          #number of records by atc_code_4
          res.tab12.m_records.t<-res.tab12.m[,lapply(.SD,sum),by=.(person_id,atc_code_4), .SDcols="count"][,lapply(.SD, sum), by=.(atc_code_4),.SDcols="count"]
          res.tab12.m_records.t[,meaning:="All"][,year:="All"]
          setnames(res.tab12.m_records.t,"count","no_records")
          res.tab12.m_records.t[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          saveRDS(res.tab12.m_records.t,paste0(vaccines_tmp,names(list_median_males)[i], "_tab12_m_rec_4.t_", i, ".rds"))
          rm(res.tab12.m_records.t)
          #number of male users by meaning, year and atc_code_4
          res.tab12.m_users.c_4<-res.tab12.m[,lapply(.SD,function(x) length(unique(x))), by=.(atc_code_4, meaning, year),.SDcols="person_id"]
          setnames(res.tab12.m_users.c_4,"person_id","no_male_users")
          res.tab12.m_users.c_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          saveRDS(res.tab12.m_users.c_4,paste0(vaccines_tmp,names(list_median_males)[i], "_tab12_m_users_4.my_", i, ".rds"))
          rm(res.tab12.m_users.c_4)
          #number of male users by atc_code_4
          res.tab12.m_users.t_4<-res.tab12.m[,lapply(.SD,sum),by=.(person_id,atc_code_4), .SDcols="count"][,lapply(.SD,function(x) length(unique(x))), by=.(atc_code_4),.SDcols="person_id"]
          setnames(res.tab12.m_users.t_4,"person_id","no_male_users")
          res.tab12.m_users.t_4[,meaning:="All"][,year:="All"]
          res.tab12.m_users.t_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          saveRDS(res.tab12.m_users.t_4,paste0(vaccines_tmp,names(list_median_males)[i], "_tab12_m_users_4.t_", i, ".rds"))
          rm(res.tab12.m_users.t_4)
          #median prescription by meaning, year and atc_code_4
          res.tab12.m_users.vx_4<-res.tab12.m[,lapply(.SD,median),.SDcols="count", by=.(meaning, year, atc_code_4)]
          setnames(res.tab12.m_users.vx_4,"count","median_rx_male_users")
          res.tab12.m_users.vx_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          saveRDS(res.tab12.m_users.vx_4,paste0(vaccines_tmp,names(list_median_males)[i],"_tab12_m_median_4.my_",i, ".rds"))
          rm(res.tab12.m_users.vx_4)
          #median prescriptions by atc_code_4
          res.tab12.m_users.vx.t_4<-res.tab12.m[,lapply(.SD,sum),by=.(person_id,atc_code_4), .SDcols="count"][,lapply(.SD,median),.SDcols="count", by=.(atc_code_4)]
          setnames(res.tab12.m_users.vx.t_4,"count","median_rx_male_users")
          res.tab12.m_users.vx.t_4[,meaning:="All"][,year:="All"]
          res.tab12.m_users.vx.t_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          saveRDS(res.tab12.m_users.vx.t_4,paste0(vaccines_tmp,names(list_median_males)[i],"_tab12_m_median_4.t_",i, ".rds"))
          rm(res.tab12.m_users.vx.t_4,res.tab12.m)
          median_male[,atc_code_4:=NULL]
        }
        
        ##number of records, male users, median male users by atc_3 (table 12)
        if(median_male[atc_level==3,.N]>0){
          median_male<-median_male[,atc_code_3:=substr(vx_atc,1,3)] #create atc_code_3
          #subset only atc codes of interest, aggregate data over atc_code_3
          res.tab12.m<-median_male[atc_level==3][,vx_atc:=NULL][,lapply(.SD,sum), by=c("person_id", "meaning", "year", "atc_code_3","atc_level")]
          #number of records by meaning, year, and atc_code_3
          res.tab12.m_records.my<-res.tab12.m[,lapply(.SD, sum), by=.(atc_code_3, meaning, year),.SDcols="count"]
          setnames(res.tab12.m_records.my,"count","no_records")
          res.tab12.m_records.my[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
          saveRDS(res.tab12.m_records.my,paste0(vaccines_tmp,names(list_median_males)[i], "_tab12_m_rec_3.my_", i, ".rds"))
          rm(res.tab12.m_records.my)
          #number of records by atc_code_3
          res.tab12.m_records.t<-res.tab12.m[,lapply(.SD,sum),by=.(person_id,atc_code_3), .SDcols="count"][,lapply(.SD, sum), by=.(atc_code_3),.SDcols="count"]
          res.tab12.m_records.t[,meaning:="All"][,year:="All"]
          setnames(res.tab12.m_records.t,"count","no_records")
          res.tab12.m_records.t[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
          saveRDS(res.tab12.m_records.t,paste0(vaccines_tmp,names(list_median_males)[i], "_tab12_m_rec_3.t_", i, ".rds"))
          rm(res.tab12.m_records.t)
          #number of male users by meaning, year and atc_code_3
          res.tab12.m_users.c_3<-res.tab12.m[,lapply(.SD,function(x) length(unique(x))), by=.(atc_code_3, meaning, year),.SDcols="person_id"]
          setnames(res.tab12.m_users.c_3,"person_id","no_male_users")
          res.tab12.m_users.c_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
          saveRDS(res.tab12.m_users.c_3,paste0(vaccines_tmp,names(list_median_males)[i], "_tab12_m_users_3.my_", i, ".rds"))
          rm(res.tab12.m_users.c_3)
          #number of male users by atc_code_3
          res.tab12.m_users.t_3<-res.tab12.m[,lapply(.SD,sum),by=.(person_id,atc_code_3), .SDcols="count"][,lapply(.SD,function(x) length(unique(x))), by=.(atc_code_3),.SDcols="person_id"]
          setnames(res.tab12.m_users.t_3,"person_id","no_male_users")
          res.tab12.m_users.t_3[,meaning:="All"][,year:="All"]
          res.tab12.m_users.t_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
          saveRDS(res.tab12.m_users.t_3,paste0(vaccines_tmp,names(list_median_males)[i], "_tab12_m_users_3.t_", i, ".rds"))
          rm(res.tab12.m_users.t_3)
          #median prescription by meaning, year and atc_code_3
          res.tab12.m_users.vx_3<-res.tab12.m[,lapply(.SD,median),.SDcols="count", by=.(meaning, year, atc_code_3)]
          setnames(res.tab12.m_users.vx_3,"count","median_rx_male_users")
          res.tab12.m_users.vx_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
          saveRDS(res.tab12.m_users.vx_3,paste0(vaccines_tmp,names(list_median_males)[i],"_tab12_m_median_3.my_",i, ".rds"))
          rm(res.tab12.m_users.vx_3)
          #median prescriptions by atc_code_3
          res.tab12.m_users.vx.t_3<-res.tab12.m[,lapply(.SD,sum),by=.(person_id,atc_code_3), .SDcols="count"][,lapply(.SD,median),.SDcols="count", by=.(atc_code_3)]
          setnames(res.tab12.m_users.vx.t_3,"count","median_rx_male_users")
          res.tab12.m_users.vx.t_3[,meaning:="All"][,year:="All"]
          res.tab12.m_users.vx.t_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
          saveRDS(res.tab12.m_users.vx.t_3,paste0(vaccines_tmp,names(list_median_males)[i],"_tab12_m_median_3.t_",i, ".rds"))
          rm(res.tab12.m_users.vx.t_3,res.tab12.m)
          median_male[,atc_code_3:=NULL]
        }
        
        #number of records, male users, median male users by atc_3 (table 12)
        if(median_male[atc_level==1,.N]>0){
          median_male<-median_male[,atc_code_1:=substr(vx_atc,1,1)] #create atc_code_1
          #subset only atc codes of interest, aggregate data over atc_code_1
          res.tab12.m<-median_male[atc_level==1][,vx_atc:=NULL][,lapply(.SD,sum), by=c("person_id", "meaning", "year", "atc_code_1","atc_level")]
          #number of records by meaning, year, and atc_code_1
          res.tab12.m_records.my<-res.tab12.m[,lapply(.SD, sum), by=.(atc_code_1, meaning, year),.SDcols="count"]
          setnames(res.tab12.m_records.my,"count","no_records")
          res.tab12.m_records.my[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.m_records.my,paste0(vaccines_tmp,names(list_median_males)[i], "_tab12_m_rec_1.my_", i, ".rds"))
          rm(res.tab12.m_records.my)
          #number of records by atc_code_1
          res.tab12.m_records.t<-res.tab12.m[,lapply(.SD,sum),by=.(person_id,atc_code_1), .SDcols="count"][,lapply(.SD, sum), by=.(atc_code_1),.SDcols="count"]
          res.tab12.m_records.t[,meaning:="All"][,year:="All"]
          setnames(res.tab12.m_records.t,"count","no_records")
          res.tab12.m_records.t[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.m_records.t,paste0(vaccines_tmp,names(list_median_males)[i], "_tab12_m_rec_1.t_", i, ".rds"))
          rm(res.tab12.m_records.t)
          #number of male users by meaning, year and atc_code_1
          res.tab12.m_users.c_1<-res.tab12.m[,lapply(.SD,function(x) length(unique(x))), by=.(atc_code_1, meaning, year),.SDcols="person_id"]
          setnames(res.tab12.m_users.c_1,"person_id","no_male_users")
          res.tab12.m_users.c_1[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.m_users.c_1,paste0(vaccines_tmp,names(list_median_males)[i], "_tab12_m_users_1.my_", i, ".rds"))
          rm(res.tab12.m_users.c_1)
          #number of male users by atc_code_1
          res.tab12.m_users.t_1<-res.tab12.m[,lapply(.SD,sum),by=.(person_id,atc_code_1), .SDcols="count"][,lapply(.SD,function(x) length(unique(x))), by=.(atc_code_1),.SDcols="person_id"]
          setnames(res.tab12.m_users.t_1,"person_id","no_male_users")
          res.tab12.m_users.t_1[,meaning:="All"][,year:="All"]
          res.tab12.m_users.t_1[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.m_users.t_1,paste0(vaccines_tmp,names(list_median_males)[i], "_tab12_m_users_1.t_", i, ".rds"))
          rm(res.tab12.m_users.t_1)
          #median prescription by meaning, year and atc_code_1
          res.tab12.m_users.vx_1<-res.tab12.m[,lapply(.SD,median),.SDcols="count", by=.(meaning, year, atc_code_1)]
          setnames(res.tab12.m_users.vx_1,"count","median_rx_male_users")
          res.tab12.m_users.vx_1[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.m_users.vx_1,paste0(vaccines_tmp,names(list_median_males)[i],"_tab12_m_median_1.my_",i, ".rds"))
          rm(res.tab12.m_users.vx_1)
          #median prescriptions by atc_code_1
          res.tab12.m_users.vx.t_1<-res.tab12.m[,lapply(.SD,sum),by=.(person_id,atc_code_1), .SDcols="count"][,lapply(.SD,median),.SDcols="count", by=.(atc_code_1)]
          setnames(res.tab12.m_users.vx.t_1,"count","median_rx_male_users")
          res.tab12.m_users.vx.t_1[,meaning:="All"][,year:="All"]
          res.tab12.m_users.vx.t_1[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.m_users.vx.t_1,paste0(vaccines_tmp,names(list_median_males)[i],"_tab12_m_median_1.t_",i, ".rds"))
          rm(res.tab12.m_users.vx.t_1,res.tab12.m)
          median_male[,atc_code_1:=NULL]
        }
      }
      print("Calculating number of male users and number of median prescription/dispensings stratified by ATC code, meaning and year.")
      
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
    }
  } else {
    tab12_males<-data.table(meaning="N/A", year="N/A", atc_code_1="N/A", atc_code_3="N/A", atc_code_4="N/A", no_male_users="N/A", median_rx_male_users="N/A")
  }
  ##############################
  #female users and median prescriptions for female users
  #############################
  if(female_population>0){
    median_females_files<-list.files(vaccines_pop,pattern="f_population")
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
      
      i<-1
      for (i in 1:length(list_median_females)){
        median_female<-readRDS(paste0(vaccines_pop, list_median_females[[i]][1]))
        setnames(median_female, "meaning_of_vx_record", "meaning")
        #count by person_id, atc code meaning and year
        median_female<-median_female[,.(count=.N), by=c("person_id","meaning","year","vx_atc","age_fup")]
        
        z<-2
        while (z <= length(list_median_females[[i]])){
          a<-readRDS(paste0(vaccines_pop, list_median_females[[i]][[z]]))
          setnames(a, "meaning_of_vx_record", "meaning")
          a<-a[,.(count=.N), by=c("person_id","meaning","year","vx_atc","age_fup")]
          median_female<-rbind(median_female, a)
          median_female<-median_female[,lapply(.SD, sum), by=c("person_id","meaning","year","vx_atc","age_fup"), .SDcols="count"]
          z<-z+1
          rm(a)
        }
        
        median_female<-median_female[,atc_level:=nchar(vx_atc)]
        #number of records, female users, median female users by atc_code_7 (table 13)
        if(median_female[atc_level==7,.N]>0){
            res.tab13.f<-median_female[atc_level==7]
            setnames(res.tab13.f,"vx_atc", "atc_code_7")
            #number of records by meaning, year, and atc_code_7
            res.tab13.f_records.my<-res.tab13.f[,lapply(.SD, sum), by=.(atc_code_7, meaning, year),.SDcols="count"]
            setnames(res.tab13.f_records.my,"count","no_records")
            res.tab13.f_records.my[,atc_code_3:=substr(atc_code_7,1,3)]
            saveRDS(res.tab13.f_records.my,paste0(vaccines_tmp,names(list_median_females)[i], "_tab13_f_rec_7.my_", i, ".rds"))
            rm(res.tab13.f_records.my)
            #number of records by atc_code_7
            res.tab13.f_records.t<-res.tab13.f[,lapply(.SD,sum),by=.(person_id,atc_code_7), .SDcols="count"][,lapply(.SD, sum), by=.(atc_code_7),.SDcols="count"]
            res.tab13.f_records.t[,meaning:="All"][,year:="All"]
            setnames(res.tab13.f_records.t,"count","no_records")
            res.tab13.f_records.t[,atc_code_3:=substr(atc_code_7,1,3)]
            saveRDS(res.tab13.f_records.t,paste0(vaccines_tmp,names(list_median_females)[i], "_tab13_f_rec_7.t_", i, ".rds"))
            rm(res.tab13.f_records.t)
            #number of female users by meaning, year and atc_code_7
            res.tab13.f_users.c_7<-res.tab13.f[,lapply(.SD,function(x) length(unique(x))), by=.(atc_code_7, meaning, year),.SDcols="person_id"]
            setnames(res.tab13.f_users.c_7,"person_id","no_female_users")
            res.tab13.f_users.c_7[,atc_code_3:=substr(atc_code_7,1,3)]
            saveRDS(res.tab13.f_users.c_7,paste0(vaccines_tmp,names(list_median_females)[i], "_tab13_f_users_7.my_", i, ".rds"))
            rm(res.tab13.f_users.c_7)
            #number of female users by atc_code_7
            res.tab13.f_users.t_7<-res.tab13.f[,lapply(.SD,sum),by=.(person_id,atc_code_7), .SDcols="count"][,lapply(.SD,function(x) length(unique(x))), by=.(atc_code_7),.SDcols="person_id"]
            setnames(res.tab13.f_users.t_7,"person_id","no_female_users")
            res.tab13.f_users.t_7[,meaning:="All"][,year:="All"]
            res.tab13.f_users.t_7[,atc_code_3:=substr(atc_code_7,1,3)]
            saveRDS(res.tab13.f_users.t_7,paste0(vaccines_tmp,names(list_median_females)[i], "_tab13_f_users_7.t_", i, ".rds"))
            rm(res.tab13.f_users.t_7)
            #median prescription by meaning, year and atc_code_7
            res.tab13.f_users.vx_7<-res.tab13.f[,lapply(.SD,median),.SDcols="count", by=.(meaning, year, atc_code_7)]
            setnames(res.tab13.f_users.vx_7,"count","median_rx_female_users")
            res.tab13.f_users.vx_7[,atc_code_3:=substr(atc_code_7,1,3)]
            saveRDS(res.tab13.f_users.vx_7,paste0(vaccines_tmp,names(list_median_females)[i],"_tab13_f_median_7.my_",i, ".rds"))
            rm(res.tab13.f_users.vx_7)
            #median prescriptions by atc_code_7
            res.tab13.f_users.vx.t_7<-res.tab13.f[,lapply(.SD,sum),by=.(person_id,atc_code_7), .SDcols="count"][,lapply(.SD,median),.SDcols="count", by=.(atc_code_7)]
            setnames(res.tab13.f_users.vx.t_7,"count","median_rx_female_users")
            res.tab13.f_users.vx.t_7[,meaning:="All"][,year:="All"]
            res.tab13.f_users.vx.t_7[,atc_code_3:=substr(atc_code_7,1,3)]
            saveRDS(res.tab13.f_users.vx.t_7,paste0(vaccines_tmp,names(list_median_females)[i],"_tab13_f_median_7.t_",i, ".rds"))
            rm(res.tab13.f_users.vx.t_7,res.tab13.f)
          }
        
        #number of records, female users, median female users by atc_code_7 (table 14) in females of childbearing age
        if(median_female[atc_level==7 & age_fup>=12 & age_fup<=55,.N]>0){
          ##########users by meaning and year
          res.tab12.f<-median_female[atc_level==7 & age_fup>=12 & age_fup<=55]
          setnames(res.tab12.f,"vx_atc", "atc_code_7")
          #number of records by meaning, year, and atc_code_7
          res.tab12.f_records.my<-res.tab12.f[,lapply(.SD, sum), by=.(atc_code_7, meaning, year),.SDcols="count"]
          setnames(res.tab12.f_records.my,"count","no_records")
          res.tab12.f_records.my[,atc_code_3:=substr(atc_code_7,1,3)]
          saveRDS(res.tab12.f_records.my,paste0(vaccines_tmp,names(list_median_females)[i], "_tab14_f_rec_7.my_", i, ".rds"))
          rm(res.tab12.f_records.my)
          #number of records by atc_code_7
          res.tab12.f_records.t<-res.tab12.f[,lapply(.SD,sum),by=.(person_id,atc_code_7), .SDcols="count"][,lapply(.SD, sum), by=.(atc_code_7),.SDcols="count"]
          res.tab12.f_records.t[,meaning:="All"][,year:="All"]
          setnames(res.tab12.f_records.t,"count","no_records")
          res.tab12.f_records.t[,atc_code_3:=substr(atc_code_7,1,3)]
          saveRDS(res.tab12.f_records.t,paste0(vaccines_tmp,names(list_median_females)[i], "_tab14_f_rec_7.t_", i, ".rds"))
          rm(res.tab12.f_records.t)
          #number of female users by meaning, year and atc_code_7
          res.tab12.f_users.c_7<-res.tab12.f[,lapply(.SD,function(x) length(unique(x))), by=.(atc_code_7, meaning, year),.SDcols="person_id"]
          setnames(res.tab12.f_users.c_7,"person_id","no_female_users")
          res.tab12.f_users.c_7[,atc_code_3:=substr(atc_code_7,1,3)]
          saveRDS(res.tab12.f_users.c_7,paste0(vaccines_tmp,names(list_median_females)[i], "_tab14_f_users_7.my_", i, ".rds"))
          rm(res.tab12.f_users.c_7)
          #number of female users by atc_code_7
          res.tab12.f_users.t_7<-res.tab12.f[,lapply(.SD,sum),by=.(person_id,atc_code_7), .SDcols="count"][,lapply(.SD,function(x) length(unique(x))), by=.(atc_code_7),.SDcols="person_id"]
          setnames(res.tab12.f_users.t_7,"person_id","no_female_users")
          res.tab12.f_users.t_7[,meaning:="All"][,year:="All"]
          res.tab12.f_users.t_7[,atc_code_3:=substr(atc_code_7,1,3)]
          saveRDS(res.tab12.f_users.t_7,paste0(vaccines_tmp,names(list_median_females)[i], "_tab14_f_users_7.t_", i, ".rds"))
          rm(res.tab12.f_users.t_7)
          #median prescription by meaning, year and atc_code_7
          res.tab12.f_users.vx_7<-res.tab12.f[,lapply(.SD,median),.SDcols="count", by=.(meaning, year, atc_code_7)]
          setnames(res.tab12.f_users.vx_7,"count","median_rx_female_users")
          res.tab12.f_users.vx_7[,atc_code_3:=substr(atc_code_7,1,3)]
          saveRDS(res.tab12.f_users.vx_7,paste0(vaccines_tmp,names(list_median_females)[i],"_tab14_f_median_7.my_",i, ".rds"))
          rm(res.tab12.f_users.vx_7)
          #median prescriptions by atc_code_7
          res.tab12.f_users.vx.t_7<-res.tab12.f[,lapply(.SD,sum),by=.(person_id,atc_code_7), .SDcols="count"][,lapply(.SD,median),.SDcols="count", by=.(atc_code_7)]
          setnames(res.tab12.f_users.vx.t_7,"count","median_rx_female_users")
          res.tab12.f_users.vx.t_7[,meaning:="All"][,year:="All"]
          res.tab12.f_users.vx.t_7[,atc_code_3:=substr(atc_code_7,1,3)]
          saveRDS(res.tab12.f_users.vx.t_7,paste0(vaccines_tmp,names(list_median_females)[i],"_tab14_f_median_7.t_",i, ".rds"))
          rm(res.tab12.f_users.vx.t_7,res.tab12.f)
        }
        
        #############
        #number of records, female users, median female users (table 12)
        if(median_female[atc_level==4|atc_level==5|atc_level==6|atc_level==7,.N]>0){
          median_female<-median_female[,atc_code_4:=substr(vx_atc,1,4)] #create atc_code_4
          #subset only atc codes of interest, aggregate data over atc_code_4
          res.tab12.f<-median_female[atc_level==4|atc_level==5|atc_level==6|atc_level==7][,vx_atc:=NULL][,lapply(.SD,sum), by=c("person_id", "meaning", "year", "atc_code_4","age_fup","atc_level")]
          #number of records by meaning, year, and atc_code_4
          res.tab12.f_records.my<-res.tab12.f[,lapply(.SD, sum), by=.(atc_code_4, meaning, year),.SDcols="count"]
          setnames(res.tab12.f_records.my,"count","no_records")
          res.tab12.f_records.my[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          saveRDS(res.tab12.f_records.my,paste0(vaccines_tmp,names(list_median_females)[i], "_tab12_f_rec_4.my_", i, ".rds"))
          rm(res.tab12.f_records.my)
          #number of records by atc_code_4
          res.tab12.f_records.t<-res.tab12.f[,lapply(.SD,sum),by=.(person_id,atc_code_4), .SDcols="count"][,lapply(.SD, sum), by=.(atc_code_4),.SDcols="count"]
          res.tab12.f_records.t[,meaning:="All"][,year:="All"]
          setnames(res.tab12.f_records.t,"count","no_records")
          res.tab12.f_records.t[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          saveRDS(res.tab12.f_records.t,paste0(vaccines_tmp,names(list_median_females)[i], "_tab12_f_rec_4.t_", i, ".rds"))
          rm(res.tab12.f_records.t)
          #number of female users by meaning, year and atc_code_4
          res.tab12.f_users.c_4<-res.tab12.f[,lapply(.SD,function(x) length(unique(x))), by=.(atc_code_4, meaning, year),.SDcols="person_id"]
          setnames(res.tab12.f_users.c_4,"person_id","no_female_users")
          res.tab12.f_users.c_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          saveRDS(res.tab12.f_users.c_4,paste0(vaccines_tmp,names(list_median_females)[i], "_tab12_f_users_4.my_", i, ".rds"))
          rm(res.tab12.f_users.c_4)
          #number of female users by atc_code_4
          res.tab12.f_users.t_4<-res.tab12.f[,lapply(.SD,sum),by=.(person_id,atc_code_4), .SDcols="count"][,lapply(.SD,function(x) length(unique(x))), by=.(atc_code_4),.SDcols="person_id"]
          setnames(res.tab12.f_users.t_4,"person_id","no_female_users")
          res.tab12.f_users.t_4[,meaning:="All"][,year:="All"]
          res.tab12.f_users.t_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          saveRDS(res.tab12.f_users.t_4,paste0(vaccines_tmp,names(list_median_females)[i], "_tab12_f_users_4.t_", i, ".rds"))
          rm(res.tab12.f_users.t_4)
          #median prescription by meaning, year and atc_code_4
          res.tab12.f_users.vx_4<-res.tab12.f[,lapply(.SD,median),.SDcols="count", by=.(meaning, year, atc_code_4)]
          setnames(res.tab12.f_users.vx_4,"count","median_rx_female_users")
          res.tab12.f_users.vx_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          saveRDS(res.tab12.f_users.vx_4,paste0(vaccines_tmp,names(list_median_females)[i],"_tab12_f_median_4.my_",i, ".rds"))
          rm(res.tab12.f_users.vx_4)
          #median prescriptions by atc_code_4
          res.tab12.f_users.vx.t_4<-res.tab12.f[,lapply(.SD,sum),by=.(person_id,atc_code_4), .SDcols="count"][,lapply(.SD,median),.SDcols="count", by=.(atc_code_4)]
          setnames(res.tab12.f_users.vx.t_4,"count","median_rx_female_users")
          res.tab12.f_users.vx.t_4[,meaning:="All"][,year:="All"]
          res.tab12.f_users.vx.t_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
          saveRDS(res.tab12.f_users.vx.t_4,paste0(vaccines_tmp,names(list_median_females)[i],"_tab12_f_median_4.t_",i, ".rds"))
          rm(res.tab12.f_users.vx.t_4,res.tab12.f)
          median_female[,atc_code_4:=NULL]
        }
        
        ##number of records, female users, median female users by atc_3 (table 12)
        if(median_female[atc_level==3,.N]>0){
            median_female<-median_female[,atc_code_3:=substr(vx_atc,1,3)] #create atc_code_3
            #subset only atc codes of interest, aggregate data over atc_code_3
            res.tab12.f<-median_female[atc_level==3][,vx_atc:=NULL][,lapply(.SD,sum), by=c("person_id", "meaning", "year", "atc_code_3","age_fup","atc_level")]
            #number of records by meaning, year, and atc_code_3
            res.tab12.f_records.my<-res.tab12.f[,lapply(.SD, sum), by=.(atc_code_3, meaning, year),.SDcols="count"]
            setnames(res.tab12.f_records.my,"count","no_records")
            res.tab12.f_records.my[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
            saveRDS(res.tab12.f_records.my,paste0(vaccines_tmp,names(list_median_females)[i], "_tab12_f_rec_3.my_", i, ".rds"))
            rm(res.tab12.f_records.my)
            #number of records by atc_code_3
            res.tab12.f_records.t<-res.tab12.f[,lapply(.SD,sum),by=.(person_id,atc_code_3), .SDcols="count"][,lapply(.SD, sum), by=.(atc_code_3),.SDcols="count"]
            res.tab12.f_records.t[,meaning:="All"][,year:="All"]
            setnames(res.tab12.f_records.t,"count","no_records")
            res.tab12.f_records.t[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
            saveRDS(res.tab12.f_records.t,paste0(vaccines_tmp,names(list_median_females)[i], "_tab12_f_rec_3.t_", i, ".rds"))
            rm(res.tab12.f_records.t)
            #number of female users by meaning, year and atc_code_3
            res.tab12.f_users.c_3<-res.tab12.f[,lapply(.SD,function(x) length(unique(x))), by=.(atc_code_3, meaning, year),.SDcols="person_id"]
            setnames(res.tab12.f_users.c_3,"person_id","no_female_users")
            res.tab12.f_users.c_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
            saveRDS(res.tab12.f_users.c_3,paste0(vaccines_tmp,names(list_median_females)[i], "_tab12_f_users_3.my_", i, ".rds"))
            rm(res.tab12.f_users.c_3)
            #number of female users by atc_code_3
            res.tab12.f_users.t_3<-res.tab12.f[,lapply(.SD,sum),by=.(person_id,atc_code_3), .SDcols="count"][,lapply(.SD,function(x) length(unique(x))), by=.(atc_code_3),.SDcols="person_id"]
            setnames(res.tab12.f_users.t_3,"person_id","no_female_users")
            res.tab12.f_users.t_3[,meaning:="All"][,year:="All"]
            res.tab12.f_users.t_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
            saveRDS(res.tab12.f_users.t_3,paste0(vaccines_tmp,names(list_median_females)[i], "_tab12_f_users_3.t_", i, ".rds"))
            rm(res.tab12.f_users.t_3)
            #median prescription by meaning, year and atc_code_3
            res.tab12.f_users.vx_3<-res.tab12.f[,lapply(.SD,median),.SDcols="count", by=.(meaning, year, atc_code_3)]
            setnames(res.tab12.f_users.vx_3,"count","median_rx_female_users")
            res.tab12.f_users.vx_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
            saveRDS(res.tab12.f_users.vx_3,paste0(vaccines_tmp,names(list_median_females)[i],"_tab12_f_median_3.my_",i, ".rds"))
            rm(res.tab12.f_users.vx_3)
            #median prescriptions by atc_code_3
            res.tab12.f_users.vx.t_3<-res.tab12.f[,lapply(.SD,sum),by=.(person_id,atc_code_3), .SDcols="count"][,lapply(.SD,median),.SDcols="count", by=.(atc_code_3)]
            setnames(res.tab12.f_users.vx.t_3,"count","median_rx_female_users")
            res.tab12.f_users.vx.t_3[,meaning:="All"][,year:="All"]
            res.tab12.f_users.vx.t_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
            saveRDS(res.tab12.f_users.vx.t_3,paste0(vaccines_tmp,names(list_median_females)[i],"_tab12_f_median_3.t_",i, ".rds"))
            rm(res.tab12.f_users.vx.t_3,res.tab12.f)
            median_female[,atc_code_3:=NULL]
          }
        
        #number of records, female users, median female users by atc_3 (table 12)
        if(median_female[atc_level==1,.N]>0){
          median_female<-median_female[,atc_code_1:=substr(vx_atc,1,1)] #create atc_code_1
          #subset only atc codes of interest, aggregate data over atc_code_1
          res.tab12.f<-median_female[atc_level==1][,vx_atc:=NULL][,lapply(.SD,sum), by=c("person_id", "meaning", "year", "atc_code_1","age_fup","atc_level")]
          #number of records by meaning, year, and atc_code_1
          res.tab12.f_records.my<-res.tab12.f[,lapply(.SD, sum), by=.(atc_code_1, meaning, year),.SDcols="count"]
          setnames(res.tab12.f_records.my,"count","no_records")
          res.tab12.f_records.my[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.f_records.my,paste0(vaccines_tmp,names(list_median_females)[i], "_tab12_f_rec_1.my_", i, ".rds"))
          rm(res.tab12.f_records.my)
          #number of records by atc_code_1
          res.tab12.f_records.t<-res.tab12.f[,lapply(.SD,sum),by=.(person_id,atc_code_1), .SDcols="count"][,lapply(.SD, sum), by=.(atc_code_1),.SDcols="count"]
          res.tab12.f_records.t[,meaning:="All"][,year:="All"]
          setnames(res.tab12.f_records.t,"count","no_records")
          res.tab12.f_records.t[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.f_records.t,paste0(vaccines_tmp,names(list_median_females)[i], "_tab12_f_rec_1.t_", i, ".rds"))
          rm(res.tab12.f_records.t)
          #number of female users by meaning, year and atc_code_1
          res.tab12.f_users.c_1<-res.tab12.f[,lapply(.SD,function(x) length(unique(x))), by=.(atc_code_1, meaning, year),.SDcols="person_id"]
          setnames(res.tab12.f_users.c_1,"person_id","no_female_users")
          res.tab12.f_users.c_1[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.f_users.c_1,paste0(vaccines_tmp,names(list_median_females)[i], "_tab12_f_users_1.my_", i, ".rds"))
          rm(res.tab12.f_users.c_1)
          #number of female users by atc_code_1
          res.tab12.f_users.t_1<-res.tab12.f[,lapply(.SD,sum),by=.(person_id,atc_code_1), .SDcols="count"][,lapply(.SD,function(x) length(unique(x))), by=.(atc_code_1),.SDcols="person_id"]
          setnames(res.tab12.f_users.t_1,"person_id","no_female_users")
          res.tab12.f_users.t_1[,meaning:="All"][,year:="All"]
          res.tab12.f_users.t_1[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.f_users.t_1,paste0(vaccines_tmp,names(list_median_females)[i], "_tab12_f_users_1.t_", i, ".rds"))
          rm(res.tab12.f_users.t_1)
          #median prescription by meaning, year and atc_code_1
          res.tab12.f_users.vx_1<-res.tab12.f[,lapply(.SD,median),.SDcols="count", by=.(meaning, year, atc_code_1)]
          setnames(res.tab12.f_users.vx_1,"count","median_rx_female_users")
          res.tab12.f_users.vx_1[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.f_users.vx_1,paste0(vaccines_tmp,names(list_median_females)[i],"_tab12_f_median_1.my_",i, ".rds"))
          rm(res.tab12.f_users.vx_1)
          #median prescriptions by atc_code_1
          res.tab12.f_users.vx.t_1<-res.tab12.f[,lapply(.SD,sum),by=.(person_id,atc_code_1), .SDcols="count"][,lapply(.SD,median),.SDcols="count", by=.(atc_code_1)]
          setnames(res.tab12.f_users.vx.t_1,"count","median_rx_female_users")
          res.tab12.f_users.vx.t_1[,meaning:="All"][,year:="All"]
          res.tab12.f_users.vx.t_1[,atc_code_4:=NA][,atc_code_3:=NA]
          saveRDS(res.tab12.f_users.vx.t_1,paste0(vaccines_tmp,names(list_median_females)[i],"_tab12_f_median_1.t_",i, ".rds"))
          rm(res.tab12.f_users.vx.t_1,res.tab12.f)
          median_female[,atc_code_1:=NULL]
        }
         }
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
    tot_rec.my<-merge(tot_rec_f.my,tot_rec_m.my,by=c("meaning","year","atc_code_4","atc_code_3","atc_code_1"),all=T)
    tot_rec.my[is.na(no_records.x),no_records.x:=0][is.na(no_records.y),no_records.y:=0][,no_records:=no_records.x+no_records.y]
    tot_rec.my[,no_records.x:=NULL][,no_records.y:=NULL]
    unlink(paste0(vaccines_tmp,"m_records.my.rds"))
    unlink(paste0(vaccines_tmp,"f_records.my.rds"))
  rm(tot_rec_f.my,tot_rec_m.my)
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
    tot_rec.t<-merge(tot_rec_f.t,tot_rec_m.t,by=c("meaning","year","atc_code_4","atc_code_3","atc_code_1"),all=T)
    tot_rec.t[is.na(no_records.x),no_records.x:=0][is.na(no_records.y),no_records.y:=0][,no_records:=no_records.x+no_records.y]
    tot_rec.t[,no_records.x:=NULL][,no_records.y:=NULL]
    unlink(paste0(vaccines_tmp,"m_records.t.rds"))
    unlink(paste0(vaccines_tmp,"f_records.t.rds"))
    rm(tot_rec_f.t,tot_rec_m.t)
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
vx.f<-rbind(readRDS(paste0(vaccines_tmp,"f_median.my.rds")),readRDS(paste0(vaccines_tmp,"f_median.t.rds")))
unlink(paste0(vaccines_tmp,"f_median.my.rds"))
unlink(paste0(vaccines_tmp,"f_median.t.rds"))
#tab12: combined no_female_users(by meaning and year) no_female_users(total)
#vx.f combined median_rx_female_users(by_meaning and year) median_rx_female_users(total)
}

if(male_population>0){
males<-rbind(readRDS(paste0(vaccines_tmp,"m_users.my.rds")),readRDS(paste0(vaccines_tmp,"m_users.t.rds")))
unlink(paste0(vaccines_tmp,"m_users.my.rds"))
unlink(paste0(vaccines_tmp,"m_users.t.rds"))
vx.m<-rbind(readRDS(paste0(vaccines_tmp,"m_median.my.rds")),readRDS(paste0(vaccines_tmp,"m_median.t.rds")))
unlink(paste0(vaccines_tmp,"m_median.my.rds"))
unlink(paste0(vaccines_tmp,"m_median.t.rds"))
#males: combined no_male_users(by meaning and year) no_male_users(total)
#vx.m combined median_rx_male_users(by_meaning and year) median_rx_male_users(total)
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
#vx.f combined median_rx_female_users(by_meaning and year) median_rx_female_users(total)
#males: combined no_male_users(by meaning and year) no_male_users(total)
#vx.m combined median_rx_male_users(by_meaning and year) median_rx_male_users(total)
########

if(male_population>0 & female_population>0){
tab12<-merge(tab12,males, by=c("meaning", "year", "atc_code_4", "atc_code_3", "atc_code_1"), all=T)
rm(males)
tab12[,year:=as.character(year)]
tab12<-merge(tab12,tot_rec.my, by=c("meaning", "year", "atc_code_4", "atc_code_3", "atc_code_1"), all=T)
rm(tot_rec.my)
tab12[is.na(no_female_users),no_female_users:=0][is.na(no_male_users),no_male_users:=0]
tab12<-merge(tab12,vx.m, by=c("meaning", "year", "atc_code_4", "atc_code_3", "atc_code_1"), all=T)
rm(vx.m)
tab12<-merge(tab12,vx.f, by=c("meaning", "year", "atc_code_4", "atc_code_3", "atc_code_1"), all=T)
rm(vx.f)
tab12[is.na(median_rx_female_users),median_rx_female_users:=0][is.na(median_rx_male_users),median_rx_male_users:=0]
setcolorder(tab12,c("meaning","year", "atc_code_4", "atc_code_3","atc_code_1","no_records","no_male_users","median_rx_male_users","no_female_users","median_rx_female_users"))
setorderv(tab12,c("meaning","year","atc_code_4","atc_code_3","atc_code_1"))
saveRDS(tab12,paste0(vaccines_dir,"tab12.rds"))
rm(tab12)
}
if(male_population==0 & female_population>0){
  tab12[,year:=as.character(year)]
  tab12<-merge(tab12,tot_rec.my, by=c("meaning", "year", "atc_code_4", "atc_code_3", "atc_code_1"), all=T)
  rm(tot_rec.my)
  tab12[is.na(no_female_users),no_female_users:=0]
  tab12<-merge(tab12,vx.f, by=c("meaning", "year", "atc_code_4", "atc_code_3", "atc_code_1"), all=T)
  rm(vx.f)
  tab12[is.na(median_rx_female_users),median_rx_female_users:=0]
  tab12[,no_male_users:=0][,median_rx_male_users:=0]
  setcolorder(tab12,c("meaning","year", "atc_code_4", "atc_code_3","atc_code_1","no_records","no_male_users","median_rx_male_users","no_female_users","median_rx_female_users"))
  setorderv(tab12,c("meaning","year","atc_code_4","atc_code_3","atc_code_1"))
  saveRDS(tab12,paste0(vaccines_dir,"tab12.rds"))
  rm(tab12)
}
if(male_population>0 & female_population==0){
  tab12<-males
  rm(males)
  tab12[,year:=as.character(year)]
  tab12<-merge(tab12,tot_rec.my, by=c("meaning", "year", "atc_code_4", "atc_code_3", "atc_code_1"), all=T)
  rm(tot_rec.my)
  tab12<-merge(tab12,vx.m, by=c("meaning", "year", "atc_code_4", "atc_code_3", "atc_code_1"), all=T)
  rm(vx.m)
  tab12[is.na(median_rx_male_users),median_rx_male_users:=0]
  tab12[,no_female_users:=0][,median_rx_female_users:=0]
  setcolorder(tab12,c("meaning","year", "atc_code_4", "atc_code_3","atc_code_1","no_records","no_male_users","median_rx_male_users","no_female_users","median_rx_female_users"))
  setorderv(tab12,c("meaning","year","atc_code_4","atc_code_3","atc_code_1"))
  saveRDS(tab12,paste0(vaccines_dir,"tab12.rds"))
  rm(tab12)
}

#######
#output tab12 to vaccines_dir folder
######
#########################################
#Table 13:Number of prescriptions/dispensings by ATC 3 & 7 level in the study population by year of dispensing/prescribing and by meaning_of_vx_record for each ATC class
#########################################
print("Creating Table 13:Number of prescriptions/dispensings by ATC 3 & 7 level in the study population by year of dispensing/prescribing and by meaning_of_vx_record for each ATC class.")
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
#output:tab13.m_median.t
########
#females
########
########
#my
#######

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
  tab13.tot_rec.my<-merge(tab13.tot_rec_f.my,tab13.tot_rec_m.my,by=c("meaning","year","atc_code_7", "atc_code_3"),all=T)
  tab13.tot_rec.my[is.na(no_records.x),no_records.x:=0][is.na(no_records.y),no_records.y:=0][,no_records:=no_records.x+no_records.y]
  tab13.tot_rec.my[,no_records.x:=NULL][,no_records.y:=NULL]
  unlink(paste0(vaccines_tmp,"tab13.m_records.my.rds"))
  unlink(paste0(vaccines_tmp,"tab13.f_records.my.rds"))
  rm(tab13.tot_rec_f.my,tab13.tot_rec_m.my)
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
  tab13.tot_rec.t<-merge(tab13.tot_rec_f.t,tab13.tot_rec_m.t,by=c("meaning","year","atc_code_7","atc_code_3"),all=T)
  rm(tab13.tot_rec_f.t,tab13.tot_rec_m.t)
  tab13.tot_rec.t[is.na(no_records.x),no_records.x:=0][is.na(no_records.y),no_records.y:=0][,no_records:=no_records.x+no_records.y]
  tab13.tot_rec.t[,no_records.x:=NULL][,no_records.y:=NULL]
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
  tab13.vx.f<-rbind(readRDS(paste0(vaccines_tmp,"tab13.f_median.my.rds")),readRDS(paste0(vaccines_tmp,"tab13.f_median.t.rds")))
  unlink(paste0(vaccines_tmp,"tab13.f_median.my.rds"))
  unlink(paste0(vaccines_tmp,"tab13.f_median.t.rds"))
  #tab13: combined no_female_users(by meaning and year) no_female_users(total)
  #tab13.vx.f combined median_rx_female_users(by_meaning and year) median_rx_female_users(total)
}

if(male_population>0){
  tab13.males<-rbind(readRDS(paste0(vaccines_tmp,"tab13.m_users.my.rds")),readRDS(paste0(vaccines_tmp,"tab13.m_users.t.rds")))
  unlink(paste0(vaccines_tmp,"tab13.m_users.my.rds"))
  unlink(paste0(vaccines_tmp,"tab13.m_users.t.rds"))
  tab13.vx.m<-rbind(readRDS(paste0(vaccines_tmp,"tab13.m_median.my.rds")),readRDS(paste0(vaccines_tmp,"tab13.m_median.t.rds")))
  unlink(paste0(vaccines_tmp,"tab13.m_median.my.rds"))
  unlink(paste0(vaccines_tmp,"tab13.m_median.t.rds"))
  #tab13.males: combined no_male_users(by meaning and year) no_male_users(total)
  #tab13.vx.m combined median_rx_male_users(by_meaning and year) median_rx_male_users(total)
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
#tab13.vx.f combined median_rx_female_users(by_meaning and year) median_rx_female_users(total)
#tab13.males: combined no_male_users(by meaning and year) no_male_users(total)
#tab13.vx.m combined median_rx_male_users(by_meaning and year) median_rx_male_users(total)
########

if(male_population>0 & female_population>0){
  tab13<-merge(tab13,tab13.males, by=c("meaning", "year", "atc_code_7", "atc_code_3"), all=T)
  rm(tab13.males)
  tab13[,year:=as.character(year)]
  tab13<-merge(tab13,tab13.tot_rec.my, by=c("meaning", "year", "atc_code_7", "atc_code_3"), all=T)
  rm(tab13.tot_rec.my)
  tab13[is.na(no_female_users),no_female_users:=0][is.na(no_male_users),no_male_users:=0]
  tab13<-merge(tab13,tab13.vx.m, by=c("meaning", "year", "atc_code_7", "atc_code_3"), all=T)
  rm(tab13.vx.m)
  tab13<-merge(tab13,tab13.vx.f, by=c("meaning", "year", "atc_code_7","atc_code_3"), all=T)
  rm(tab13.vx.f)
  tab13[is.na(median_rx_female_users),median_rx_female_users:=0][is.na(median_rx_male_users),median_rx_male_users:=0]
  setcolorder(tab13,c("meaning","year", "atc_code_7", "atc_code_3","no_records","no_male_users","median_rx_male_users","no_female_users","median_rx_female_users"))
  setorderv(tab13,c("meaning","year","atc_code_7","atc_code_3"))
  saveRDS(tab13,paste0(vaccines_dir,"tab13.rds"))
  rm(tab13)
}
if(male_population==0 & female_population>0){
  tab13[,year:=as.character(year)]
  tab13<-merge(tab13,tab13.tot_rec.my, by=c("meaning", "year", "atc_code_7", "atc_code_3"), all=T)
  rm(tab13.tot_rec.my)
  tab13[is.na(no_female_users),no_female_users:=0]
  tab13<-merge(tab13,tab13.vx.f, by=c("meaning", "year", "atc_code_7", "atc_code_3"), all=T)
  rm(vx.f)
  tab13[is.na(median_rx_female_users),median_rx_female_users:=0]
  tab13[,no_male_users:=0][,median_rx_male_users:=0]
  setcolorder(tab13,c("meaning","year", "atc_code_7", "atc_code_3","no_records","no_male_users","median_rx_male_users","no_female_users","median_rx_female_users"))
  setorderv(tab13,c("meaning","year","atc_code_7","atc_code_3"))
  saveRDS(tab13,paste0(vaccines_dir,"tab13.rds"))
  rm(tab13)
}
if(male_population>0 & female_population==0){
  tab13<-tab13.males
  rm(tab13.males)
  tab13[,year:=as.character(year)]
  tab12<-merge(tab13,tab13.tot_rec.my, by=c("meaning", "year", "atc_code_7", "atc_code_3"), all=T)
  rm(tab13.tot_rec.my)
  tab13<-merge(tab13,tab13.vx.m, by=c("meaning", "year", "atc_code_7", "atc_code_3"), all=T)
  rm(tab13.vx.m)
  tab13[is.na(median_rx_male_users),median_rx_male_users:=0]
  tab13[,no_female_users:=0][,median_rx_female_users:=0]
  setcolorder(tab13,c("meaning","year", "atc_code_7", "atc_code_3", "no_records","no_male_users","median_rx_male_users","no_female_users","median_rx_female_users"))
  setorderv(tab13,c("meaning","year","atc_code_7","atc_code_3"))
  saveRDS(tab13,paste0(vaccines_dir,"tab13.rds"))
  rm(tab13)
}

#######
#output tab13 to vaccines_dir folder
######

#########################################
#Table 14:Number of prescriptions/dispensings by ATC 3 & 7 level in the study population by year of dispensing/prescribing and by meaning_of_vx_record for each ATC class
#########################################
print("Creating table 14: Number of prescriptions/dispensings by ATC 3 & 7 level in the study population by year of dispensing/prescribing and by meaning_of_vx_record for females of childbearing age (12-55 age_start_fup).")
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
  
  tab14<-merge(tab14,users, by=c("meaning", "year", "atc_code_7", "atc_code_3"), all=T)
  rm(users)
  tab14<-merge(tab14,median, by=c("meaning", "year", "atc_code_7", "atc_code_3"), all=T)
  rm(median)
  setcolorder(tab14,c("meaning","year", "atc_code_7", "atc_code_3","no_records","no_female_users","median_rx_female_users"))
  setorderv(tab14,c("meaning","year","atc_code_7","atc_code_3"))
  saveRDS(tab14,paste0(vaccines_dir,"tab14.rds"))
  rm(tab14)
  
  #######
  #output tab14 to vaccines_dir folder
  ######
  
}

}

#remove tmp/VACCINES directory
unlink(paste0(tmp, "VACCINES"), recursive = T)


