library(data.table)
`%!in%` = Negate(`%in%`)
#females age
min_age_preg<-12
max_age_preg<-55
#Get MEDICINES & VACCINES tables
actual_tables<-list()
actual_tables$MEDICINES<-list.files(path_dir, pattern="^MEDICINES")
actual_tables$VACCINES<-list.files(path_dir, pattern="^VACCINES")

#Get cdm_source file name
cdm_source_file<-list.files(path_dir, pattern="^CDM_SOURCE")
#Get DAP info and date createion fro CDM_SOURCE
CDM_SOURCE<-fread(paste0(path_dir, cdm_source_file))
date_creation<-CDM_SOURCE[["date_creation"]]
data_access_provider_name<-CDM_SOURCE[["data_access_provider_name"]]
data_source_name<-CDM_SOURCE[["data_source_name"]]
rm(CDM_SOURCE)

#load study_population
study_population<-fread(paste(g_intermediate, "study_population_3.csv", sep=""), stringsAsFactors = FALSE, colClasses = "character")

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

#calculate number of records, male users, female users
m_year_atc_sex<-function(dt, year_var, meaning_var, atc_var, level_num){
  dt[,meaning:=dt[[meaning_var]]]
  dt[,atc_sub:=substr(get(atc_var),1,level_num)]
  
  a.1<-dt[complete.cases(meaning) & complete.cases(year) & complete.cases(atc_sub), .N, by=.(year, meaning,atc_sub)]
  setnames(a.1, "N", "count")
  setnames(a.1, "atc_sub", paste0("atc_code_", level_num))
  
  a.2<-dt[complete.cases(meaning) & complete.cases(year) & complete.cases(atc_sub) & sex_at_instance_creation=="M", .N, by=.(year, meaning, atc_sub)]
  setnames(a.2, "N", "count")
  setnames(a.2, "atc_sub", paste0("atc_code_", level_num))
  
  a.3<-dt[complete.cases(meaning) & complete.cases(year) & complete.cases(atc_sub) & sex_at_instance_creation=="F", .N, by=.(year, meaning, atc_sub)]
  setnames(a.3, "N", "count")
  setnames(a.3, "atc_sub", paste0("atc_code_", level_num))

  results<-list("no_records"=a.1, "male_users"=a.2, "female_users"=a.3)
  return(results)
}

if (length(actual_tables$MEDICINES)==0){
  print("There is no MEDICINES table present in the working directory.")
} else {
  ###################################################
  #List for saving info
  ###################################################
  print("Creating lists to save the information.")
  meanings<-list() #all meanings present
  orig_no_rows<-list() #original number of records in the MEDICINES table
  miss_date_med<-list() #number of record with missing date dispensing/prescription
  med_study_pop<-list() #number of records for the study population, no selection criteria for time applied
  med_out_st_per<-list() #number of medicines records outside the observation period(check is done on individual level)
  med_study_pop_obsper<-list() #number of records in the study population with date dispensing/prescription inside study period
  med_stdpop_no_meaning<-list() #number of records in the study population with no meaning
  #############################
  med_study_population<-list() #number of records in the study population
  med_study_population_meaning<-list() #number of records in the study population by meaning
  #############################
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
  empty_atc_code_m_f<-list()
  empty_atc_code_m_y_f<-list() #number of records with empty atc code by meaning and year in females in childebearing age
  med_study_population_meaning_f<-list() #number of records in females [12-55] years old by meaning
  med_study_population_f<-list() #number of records in females [12-55] years old
  ##############################
  #Table 14 in the SAP
  no_indication_m<-list() #number of records with missing code_indication but complete code_indication_vocabulary by meaning
  no_prescriber_m<-list() #number of records with missing prescriber_speciality but complete prescriber_specilaity_vocabulary by meaning
  no_disp_num<-list() #number of records with empty disp_number_medicinal_product by meaning
  no_presc_quantity<-list() #number of records with empty presc_quantity_per_day by meaning
  no_presc_quantity_unit<-list() #number of records with empty presc_quantity_unit but complete presc_quantity_per_day by meaning
  no_indication_t<-list() #total number of records with empty indication_code but complete indication_code_vocabulary
  no_prescriber_t<-list()  #total number of records with empty prescriber_speciality but complete prescriber_speciality_vocabulary
  no_disp_num_t<-list() #total number of records with empty disp_number_medicinal_product
  no_presc_quantity_t<-list() #total number of records with empty presc_quantity_per_day
  no_presc_quantity_unit_t<-list() #total number of records with empty presc_quantity_unit but complete presc_quantity_per_day
  ###############################
  #Table 12
  ##############################
  male_users_m<-list() #number of male users by meaning
  female_users_m<-list() #number of female users by meaning
  no_records_4<-list() # total number of records atc_4
  no_records_3<-list() #total number of records atc_3
  no_records_1<-list() #total number of records atc_1
  male_users_4<-list() #number of male users by atc_4
  male_users_3<-list() #number of male users by atc_3
  male_users_1<-list() #number of male users by atc_1
  female_users_4<-list() #number of female users by atc_4
  female_users_3<-list() #number of female users by atc_3
  female_users_1<-list() #number of female users by atc_1
  median_males_4<-list() #median number of records in males stratified by atc_4
  median_males_3<-list() #median number of records in males stratified by atc_3
  median_males_1<-list() #median number of records in males stratified by atc_1
  median_females_4<-list() #median number of records in females stratified by atc_4
  median_females_3<-list() #median number of records in females stratified by atc_3
  median_females_1<-list() #median number of records in females stratified by atc_1
  #stratified by meaning and year
  no_records_my_4<-list() # total number of records atc_4
  no_records_my_3<-list() #total number of records atc_3
  no_records_my_1<-list() #total number of records atc_1
  male_users_my_4<-list() #number of male users by atc_4
  male_users_my_3<-list() #number of male users by atc_3
  male_users_my_1<-list() #number of male users by atc_1
  female_users_my_4<-list() #number of female users by atc_4
  female_users_my_3<-list() #number of female users by atc_3
  female_users_my_1<-list() #number of female users by atc_1
  median_males_my_4<-list() #median number of records in males stratified by atc_4
  median_males_my_3<-list() #median number of records in males stratified by atc_3
  median_males_my_1<-list() #median number of records in males stratified by atc_1
  median_females_my_4<-list() #median number of records in females stratified by atc_4
  median_females_my_3<-list() #median number of records in females stratified by atc_3
  median_females_my_1<-list() #median number of records in females stratified by atc_1
  ##############################################################################################
  #Table 13
  ##############################################################################################
  male_users_7_m<-list() #number of male users by meaning
  female_users_7_m<-list() #number of female users by meaning
  no_records_7<-list() # total number of records atc_7
  male_users_7<-list() #number of male users by atc_7
  female_users_7<-list() #number of female users by atc_7
  median_males_7<-list() #median number of records in males stratified by atc_7
  median_females_7<-list() #median number of records in females stratified by atc_7
  #stratified by meaning and year
  no_records_my_7<-list() # total number of records atc_7
  male_users_my_7<-list() #number of male users by atc_7
  female_users_my_7<-list() #number of female users by atc_7
  median_males_my_7<-list() #median number of records in males stratified by atc_7
  median_females_my_7<-list() #median number of records in females stratified by atc_7
  ###############################
  
  w<-1
  ###############################################
  #for_loop
  ##############################################

  for (i in 1:length(actual_tables$MEDICINES)){
    #Load the table
    df<-fread(paste(path_dir, actual_tables$MEDICINES[i], sep=""), stringsAsFactors = FALSE, colClasses = "character")
    df<-df[,c("person_id", "medicinal_product_atc_code", "date_dispensing", "date_prescription", "disp_number_medicinal_product", "presc_quantity_per_day", "presc_quantity_unit", "indication_code", "indication_code_vocabulary", "meaning_of_drug_record", "prescriber_speciality", "prescriber_speciality_vocabulary")]
    df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately
    colnames<-colnames[!colnames %in% "person_id"]
    #get the total number of records(a)
    orig_no_rows[[w]]<-df[,.N]

    #merge with the study_population table(there is no missing data in this table)
    df<-df[study_population,on=.(person_id)]#left join, keeps all people in the study population even if they didn't have a prescription
    pers_stdpop_not_med<-df[rowSums(is.na(df[,..colnames]))==length(colnames),person_id] #subjects id present in the study population but that do not have a dispensing/prescription
    saveRDS(pers_stdpop_not_med, paste0(tmp, paste0("stdpop_not_med_", actual_tables$MEDICINES[i], ".rds"))) #to give the results we will merge the files by keeping only person_id that are in all saved files
    
    med_study_pop[[w]]<-df[,.N] #number of records for the study population, no selection criteria for time applied
   
    
    #remove records that are outside the obs_period for all subjects
    df[,entry_spell_category:=as.IDate(entry_spell_category,"%Y%m%d")][,exit_spell_category:=as.IDate(exit_spell_category,"%Y%m%d")][,date_dispensing:=as.IDate(date_dispensing,"%Y%m%d")][,date_prescription:=as.IDate(date_prescription,"%Y%m%d")] #transform to date variables
    med_out_st_per[[w]]<-df[(date_dispensing<entry_spell_category | date_dispensing>exit_spell_category) | (date_prescription<entry_spell_category | date_prescription>exit_spell_category),.N] #number of records outside study population
    df[(date_dispensing<entry_spell_category | date_dispensing>exit_spell_category) | (date_prescription<entry_spell_category | date_prescription>exit_spell_category), obs_out:=1]
    df<-df[is.na(obs_out)] #remove records outside study period
    df[,obs_out:=NULL] 
    med_study_pop_obsper[[w]]<-df[,.N] #number of records after removing records outside study period
    med_stdpop_no_meaning[[w]]<-df[is.na(meaning_of_drug_record),.N] #number of records with empty meaning
    df<-df[!is.na(meaning_of_drug_record)] #remove records with empty meaning
    #########
    meanings[[w]]<-unique(df[["meaning_of_drug_record"]])
    ############################
    med_study_population[[w]]<-df[,.N] #number of records in the study population
    med_study_population_meaning[[w]]<-df[,.N, by="meaning_of_drug_record"] #number of records in the study population by meaning
    ############################
    #Table 14
    ############################
    #stratified by meaning
    no_indication_m[[w]]<-df[is.na(indication_code) & !is.na(indication_code_vocabulary), .N, by="meaning_of_drug_record"]
    no_prescriber_m[[w]]<-df[is.na(prescriber_speciality) & !is.na(prescriber_speciality_vocabulary), .N, by="meaning_of_drug_record"]
    no_disp_num[[w]]<-df[is.na(disp_number_medicinal_product), .N, by="meaning_of_drug_record"]
    no_presc_quantity[[w]]<-df[is.na(presc_quantity_per_day), .N, by="meaning_of_drug_record"]
    no_presc_quantity_unit[[w]]<-df[is.na(presc_quantity_unit) & !is.na(presc_quantity_per_day), .N, by="meaning_of_drug_record"]
    #total number of records
    no_indication_t[[w]]<-df[is.na(indication_code) & !is.na(indication_code_vocabulary), .N]
    no_prescriber_t[[w]]<-df[is.na(prescriber_speciality) & !is.na(prescriber_speciality_vocabulary), .N]
    no_disp_num_t[[w]]<-df[is.na(disp_number_medicinal_product), .N]
    no_presc_quantity_t[[w]]<-df[is.na(presc_quantity_per_day), .N]
    no_presc_quantity_unit_t[[w]]<-df[is.na(presc_quantity_unit) & !is.na(presc_quantity_per_day), .N]
     #remove uneccessary columns
    df[,c("indication_code", "indication_code_vocabulary", "prescriber_speciality", "prescriber_speciality_vocabulary","disp_number_medicinal_product", "presc_quantity_per_day", "presc_quantity_unit"):=NULL]
    ##############################
    
    #create year variable
    df[,year:=year(date_prescription)][!is.na(date_dispensing),year:=year(date_dispensing)]
    miss_date_med[[w]]<-df[is.na(year),.N]
    years<-unique(df[["year"]])
    #number of records with missing atc codes
    empty_atc_code[[w]]<-df[is.na(medicinal_product_atc_code) & !is.na(year), .N] #number of records with missing atc code when date disp/presc is present
    df[, atc_level:=nchar(medicinal_product_atc_code)] #create atc level variable showing which level is present in medicinal_product_atc_code
    no_level1_atc[[w]]<-df[atc_level==1, .N] #number of records with only first level of atc
    no_level2_atc[[w]]<-df[atc_level==2, .N] #number of records with only second level of atc
    no_level3_atc[[w]]<-df[atc_level==3, .N] #number of records with only third level of atc
    no_level4_atc[[w]]<-df[atc_level==4, .N] #number of records with only fourth level of atc
    no_level5_atc[[w]]<-df[atc_level==5, .N] #number of records with only fifth level of atc
    no_level6_atc[[w]]<-df[atc_level==6, .N] #number of records with only sixth level of atc
    no_level7_atc[[w]]<-df[atc_level==7, .N] #number of records with only seventh level of atc
    comp_atc[[w]]<-df[!is.na(medicinal_product_atc_code), .N]
    #p_incomplete_7: sum of records with atc 1-6/ total no of records with complete atc code(comp_atc)
    #p_incomplete_5: sum of records with atc 1-4/ total no of records with complete atc code(comp_atc)
    
    ##############################
    #First section of the SAP
    ##############################
    #records in the raw table ==orig_no_rows
    #records for study_population (independent of time) == med_study_pop
    #records for study_population (within study period) == med_study_pop_obsper
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
    #sub2: med_study_pop_obsper
    # number of records with empty meaning: med_stdpop_no_meaning
    #sub3: med_study_population
    ###############################
    #Table 10:
    ##############################
    #empty row
    #number of records with missing atc codes by meaning(denominator) 
    empty_atc_code_m<-df[is.na(medicinal_product_atc_code),.N, by="meaning_of_drug_record"]
    #number of records with missing atc codes by meaning and year(numerator)
    empty_atc_code_m_y[[w]]<-df[is.na(medicinal_product_atc_code) & !is.na(year), .N, by=c("meaning_of_drug_record", "year")]
    #total row
    #total records by meaning(numerator): med_study_population_meaning
    #total records(denominator): med_study_population
    #counts by meaning and year for atc trunacted to the first level
    Res.1<-m_year_atc(dt=df,
                      year_var = "year",
                      meaning_var = "meaning_of_drug_record",
                      atc_var = "medicinal_product_atc_code",
                      level_num = 1) #export results to tmp with name Res_1_name of original file
    saveRDS(Res.1, paste0(tmp, paste0("Res.1_", actual_tables$MEDICINES[i], ".rds"))) #allows to save data as list
    rm(Res.1)
    #################################
    #Table 11:
    #################################
    #empty row
    #number of records with missing atc codes by meaning in females 12-55 years old(denominator) 
    empty_atc_code_m_f[[w]]<-df[is.na(medicinal_product_atc_code) & sex_at_instance_creation=="F" & Age_start>=12 & Age_start<=55, .N, by="meaning_of_drug_record"]
    #number of records with missing atc codes by meaning and year in females 12-55 years old(numerator)
    empty_atc_code_m_y_f[[w]]<-df[is.na(medicinal_product_atc_code) & sex_at_instance_creation=="F" & Age_start>=12 & Age_start<=55 & !is.na(year), .N, by=c("meaning_of_drug_record","year")]
    #total row
    #total records by meaning(numerator): med_study_population_meaning_f
    med_study_population_meaning_f[[w]]<-df[sex_at_instance_creation=="F" & Age_start>=12 & Age_start<=55,.N, by="meaning_of_drug_record"]
    #total records(denominator): med_study_population_f
    med_study_population_f[[w]]<-df[sex_at_instance_creation=="F" & Age_start>=12 & Age_start<=55,.N]
    #counts by meaning and year for atc trunacted to the first level in females [12-55]
    Res.2<-m_year_atc(dt=df[sex_at_instance_creation=="F" & Age_start>=12 & Age_start<=55],
                      year_var = "year",
                      meaning_var = "meaning_of_drug_record",
                      atc_var = "medicinal_product_atc_code",
                      level_num = 1) #export results to tmp with name Res_2_name of original file
    saveRDS(Res.2, paste0(tmp, paste0("Res.2_", actual_tables$MEDICINES[i], ".rds")))
    rm(Res.2)
    ##################################
    #Table 12:Info
    ##################################
    #number of male users:male_users_m
    #number of female_users: female_users_m
    #number of total records where atc level 4,5,6,7:no_records_4
    #number of total records where atc level 3:no_records_3
    #number of total records where atc level 1:no_records_1
    #male_users stratified by atc 4,5,6,7: male_users_4
    #male_users stratified by atc 3:male_users_3
    #male_users stratified by atc 1:male_users_1
    #female_users stratified by atc 4,5,6,7: female_users_4
    #female_users stratified by atc 3:female_users_3
    #female_users stratified by atc 1:female_users_1
    #median users in males stratified by atc 4,5,6,7:median_males_4
    #median users in males stratified by atc 3:median_males_3
    #median users in males stratified by atc 1:median_males_1
    #median users in females stratified by atc 4,5,6,7:median_females_4
    #median users in females stratified by atc 3:median_females_3
    #median users in females stratified by atc 1:median_females_1
    #number of total records where atc level 4,5,6,7 by meaning and year:no_records_my_4
    #number of total records where atc level 3 by meaning and year:no_records__my_3
    #number of total records where atc level 1 by meaning and year:no_records_my_1
    #male_users stratified by atc 4,5,6,7 by meaning and year: male_users_my_4
    #male_users stratified by atc 3 by meaning and year:male_users_my_3
    #male_users stratified by atc 1 by meaning and year:male_users_my_1
    #female_users stratified by atc 4,5,6,7 by meaning and year: female_users_my_4
    #female_users stratified by atc 3 by meaning and year:female_users_my_3
    #female_users stratified by atc 1 by meaning and year:female_users_my_1
    #median users in males stratified by atc 4,5,6,7 by meaning and year:median_males_my_4
    #median users in males stratified by atc 3 by meaning and year:median_males_my_3
    #median users in males stratified by atc 1 by meaning and year:median_males_my_1
    #median users in females stratified by atc 4,5,6,7 by meaning and year:median_females_my_4
    #median users in females stratified by atc 3 by meaning and year:median_females_my_3
    #median users in females stratified by atc 1 by meaning and year:median_females_my_1
    ############  
    #number of male users
    male_users_m[[w]]<-df[sex_at_instance_creation=="M", .N, by="meaning_of_drug_record"]
    #number of female_users
    female_users_m[[w]]<-df[sex_at_instance_creation=="F", .N,by="meaning_of_drug_record"]
    #number of total records where atc level 4,5,6,7
    if(df[atc_level==4 | atc_level==5 | atc_level==6 |atc_level==7,.N]>0){
    no_records_4<-df[!is.na(year) &(atc_level==4 | atc_level==5 | atc_level==6 |atc_level==7), .N, by=substr(medicinal_product_atc_code,1,4)]
    names(no_records_4)<-c("atc_code_4", "count")
    no_records_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)][,meaning:="All"]
    for (a in 1:length(LETTERS)){
      if(no_records_4[atc_code_1==LETTERS[a],.N]>0)
        saveRDS(no_records_4[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_no_records_4_", actual_tables$MEDICINES[i], ".rds")))
    }
    rm(no_records_4)
  } else {no_records_4<-NULL}
    #number of total records where atc level 3
    if(df[atc_level==3,.N]>0){
    no_records_3<-df[!is.na(year) & atc_level==3, .N, by=substr(medicinal_product_atc_code,1,3)]
    names(no_records_3)<-c("atc_code_3", "count")
    no_records_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)][,meaning:="All"]
    for (a in 1:length(LETTERS)){
      if(no_records_3[atc_code_1==LETTERS[a],.N]>0)
        saveRDS(no_records_3[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_no_records_3_", actual_tables$MEDICINES[i], ".rds")))
    }
    rm(no_records_3)
  } else {no_records_3<-NULL}
    #number of total records where atc level 1
    if(df[atc_level==1,.N]>0){
    no_records_1<-df[!is.na(year) & atc_level==1, .N, by=substr(medicinal_product_atc_code,1,1)]
    names(no_records_1)<-c("atc_code_1", "count")
    no_records_1[,atc_code_3:=NA][,atc_code_4:=NA][meaning:="All"]
    for (a in 1:length(LETTERS)){
      if(no_records_1[atc_code_1==LETTERS[a],.N]>0)
        saveRDS(no_records_1[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_no_records_1_", actual_tables$MEDICINES[i], ".rds")))
    }
    rm(no_records_1)
} else {no_records_1<-NULL}
    
   #male_users stratified by atc 4,5,6,7
if(df[!is.na(year) & sex_at_instance_creation=="M" &(atc_level==4 | atc_level==5 | atc_level==6 |atc_level==7), .N]>0){
    male_users_4<-df[!is.na(year) & sex_at_instance_creation=="M" &(atc_level==4 | atc_level==5 | atc_level==6 |atc_level==7), .N, by=substr(medicinal_product_atc_code,1,4)]
    names(male_users_4)<-c("atc_code_4", "count")
    male_users_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)][,meaning:="All"]
    for (a in 1:length(LETTERS)){
      if(male_users_4[atc_code_1==LETTERS[a],.N]>0)
        saveRDS(male_users_4[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_m_users_4_", actual_tables$MEDICINES[i], ".rds")))
    }
    rm(male_users_4)
} else {male_users_4<-NULL}
    #male_users stratified by atc 3
if(df[!is.na(year) & sex_at_instance_creation=="M" & atc_level==3, .N]>0){
    male_users_3<-df[!is.na(year) & sex_at_instance_creation=="M" & atc_level==3, .N, by=substr(medicinal_product_atc_code,1,3)]
    names(male_users_3)<-c("atc_code_3", "count")
    male_users_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)][,meaning:="All"]
    for (a in 1:length(LETTERS)){
      if(male_users_3[atc_code_1==LETTERS[a],.N]>0)
        saveRDS(male_users_3[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_m_users_3_", actual_tables$MEDICINES[i], ".rds")))
    }
    rm(male_users_3)
} else {male_users_3<-NULL}
    #male_users by atc 1
if(df[!is.na(year) &sex_at_instance_creation=="M" & atc_level==1, .N]>0){
    male_users_1<-df[!is.na(year) &sex_at_instance_creation=="M" & atc_level==1, .N, by=substr(medicinal_product_atc_code,1,1)]
    names(male_users_1)<-c("atc_code_1", "count")
    male_users_1[,atc_code_3:=NA][,atc_code_4:=NA][meaning:="All"]
    for (a in 1:length(LETTERS)){
      if(male_users_1[atc_code_1==LETTERS[a],.N]>0)
        saveRDS(male_users_1[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_m_users_1_", actual_tables$MEDICINES[i], ".rds")))
    }
    rm(male_users_1)
    } else {male_users_1<-NULL}
    #female_users stratified by atc 4,5,6,7
if(df[!is.na(year) & sex_at_instance_creation=="F" &(atc_level==4 | atc_level==5 | atc_level==6 |atc_level==7), .N]>0){
    female_users_4<-df[!is.na(year) & sex_at_instance_creation=="F" &(atc_level==4 | atc_level==5 | atc_level==6 |atc_level==7), .N, by=substr(medicinal_product_atc_code,1,4)]
    names(female_users_4)<-c("atc_code_4", "count")
    female_users_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)][,meaning:="All"]
    for (a in 1:length(LETTERS)){
      if(female_users_4[atc_code_1==LETTERS[a],.N]>0)
        saveRDS(female_users_4[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_f_users_4_", actual_tables$MEDICINES[i], ".rds")))
    }
    rm(female_users_4)
} else {female_users_4<-NULL}
    #female_users stratified by atc 3
if(df[!is.na(year) & sex_at_instance_creation=="F" & atc_level==3, .N]>0){
    female_users_3<-df[!is.na(year) & sex_at_instance_creation=="F" & atc_level==3, .N, by=substr(medicinal_product_atc_code,1,3)]
    names(female_users_3)<-c("atc_code_3", "count")
    female_users_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)][,meaning:="All"]
    for (a in 1:length(LETTERS)){
      if(female_users_3[atc_code_1==LETTERS[a],.N]>0)
        saveRDS(female_users_3[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_f_users_3_", actual_tables$MEDICINES[i], ".rds")))
    }
    rm(female_users_3)
} else { female_users_3<-NULL}
    #female_users by atc 1
if(df[!is.na(year) & sex_at_instance_creation=="F" & atc_level==1, .N]>0){
    female_users_1<-df[!is.na(year) & sex_at_instance_creation=="F" & atc_level==1, .N, by=substr(medicinal_product_atc_code,1,1)]
    names(female_users_1)<-c("atc_code_1", "count")
    female_users_1[,atc_code_3:=NA][,atc_code_4:=NA][meaning:="All"]
    for (a in 1:length(LETTERS)){
      if(female_users_1[atc_code_1==LETTERS[a],.N]>0)
        saveRDS(female_users_1[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_f_users_1_", actual_tables$MEDICINES[i], ".rds")))
    }
    rm(female_users_1)
} else {female_users_1<-NULL}
    
    #median users in males
if(df[!is.na(year) & sex_at_instance_creation=="M" & (atc_level==4 |atc_level==5 |atc_level==6|atc_level==7), .N]>0){
    median_males_4<-df[!is.na(year) & sex_at_instance_creation=="M" & (atc_level==4 |atc_level==5 |atc_level==6|atc_level==7), .N, by=.(substr(medicinal_product_atc_code,1,4), person_id)]
    setnames(median_males_4, "N", "count")
    setnames(median_males_4, "substr", "atc_code_4")
    median_males_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]

    #save results splitted by atc_code_1
    for (a in 1:length(LETTERS)){
    if(median_males_4[atc_code_1==LETTERS[a],.N]>0)
    saveRDS(median_males_4[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_median_m_4_", actual_tables$MEDICINES[i], ".rds")))
    }
    rm(median_males_4)
} else {median_males_4<-NULL}  

if(df[!is.na(year) & sex_at_instance_creation=="M" & atc_level==3, .N]>0){
    median_males_3<-df[!is.na(year) & sex_at_instance_creation=="M" & atc_level==3, .N, by=.(substr(medicinal_product_atc_code,1,3), person_id)]
    setnames(median_males_3, "N", "count")
    setnames(median_males_3, "substr", "atc_code_3")
    median_males_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
    for (a in 1:length(LETTERS)){
      if(median_males_3[atc_code_1==LETTERS[a],.N]>0)
        saveRDS(median_males_3[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_median_m_3_", actual_tables$MEDICINES[i], ".rds")))
    }
    rm(median_males_3)
} else {median_males_3<-NULL} 

if(df[!is.na(year) & sex_at_instance_creation=="M" & atc_level==1, .N]>0){
    median_males_1<-df[!is.na(year) & sex_at_instance_creation=="M" & atc_level==1, .N, by=.(substr(medicinal_product_atc_code,1,1), person_id)]
    setnames(median_males_1, "N", "count")
    setnames(median_males_1, "substr", "atc_code_1")
    median_males_1[,atc_code_3:=NA][,atc_code_4:=NA]
    for (a in 1:length(LETTERS)){
    if(median_males_1[atc_code_1==LETTERS[a],.N]>0)
      saveRDS(median_males_1[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_median_m_1_", actual_tables$MEDICINES[i], ".rds")))
    }
  rm(median_males_1)
} else {median_males_1<-NULL}
  #median users in females
if(df[!is.na(year) & sex_at_instance_creation=="F" & (atc_level==4 |atc_level==5 |atc_level==6|atc_level==7), .N]>0){
  median_females_4<-df[!is.na(year) & sex_at_instance_creation=="F" & (atc_level==4 |atc_level==5 |atc_level==6|atc_level==7), .N, by=.(substr(medicinal_product_atc_code,1,4), person_id)]
  setnames(median_females_4, "N", "count")
  setnames(median_females_4, "substr", "atc_code_4")
  median_females_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
  
  #save results splitted by atc_code_1
  for (a in 1:length(LETTERS)){
    if(median_females_4[atc_code_1==LETTERS[a],.N]>0)
      saveRDS(median_females_4[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_median_f_4_", actual_tables$MEDICINES[i], ".rds")))
  }
  rm(median_females_4)
} else {median_females_4<-NULL}  

if(df[!is.na(year) & sex_at_instance_creation=="F" & atc_level==3, .N]>0){
  median_females_3<-df[!is.na(year) & sex_at_instance_creation=="F" & atc_level==3, .N, by=.(substr(medicinal_product_atc_code,1,3), person_id)]
  setnames(median_females_3, "N", "count")
  setnames(median_females_3, "substr", "atc_code_3")
  median_females_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
  for (a in 1:length(LETTERS)){
    if(median_females_3[atc_code_1==LETTERS[a],.N]>0)
      saveRDS(median_females_3[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_median_f_3_", actual_tables$MEDICINES[i], ".rds")))
  }
  rm(median_females_3)
} else {median_females_3<-NULL} 

if(df[!is.na(year) & sex_at_instance_creation=="F" & atc_level==1, .N]>0){
  median_females_1<-df[!is.na(year) & sex_at_instance_creation=="F" & atc_level==1, .N, by=.(substr(medicinal_product_atc_code,1,1), person_id)]
  setnames(median_females_1, "N", "count")
  setnames(median_females_1, "substr", "atc_code_1")
  median_females_1[,atc_code_3:=NA][,atc_code_4:=NA]
  for (a in 1:length(LETTERS)){
  if(median_females_1[atc_code_1==LETTERS[a],.N]>0)
    saveRDS(median_females_1[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_median_f_1_", actual_tables$MEDICINES[i], ".rds")))
}
rm(median_females_1)
} else {median_females_1<-NULL}

#number of total records where atc level 4,5,6,7
if(df[!is.na(year) &(atc_level==4 | atc_level==5 | atc_level==6 |atc_level==7), .N]>0){
no_records_my_4<-df[!is.na(year) &(atc_level==4 | atc_level==5 | atc_level==6 |atc_level==7), .N, by=.(meaning, year,substr(medicinal_product_atc_code,1,4))]
names(no_records_my_4)<-c("meaning", "year", "atc_code_4", "count")
no_records_my_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)][,meaning:="All"]
for (a in 1:length(LETTERS)){
  if(no_records_my_4[atc_code_1==LETTERS[a],.N]>0)
    saveRDS(no_records_my_4[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_no_recmy_4_", actual_tables$MEDICINES[i], ".rds")))
}
rm(no_records_my_4)} else {no_records_my_4<-NULL}
#number of total records where atc level 3
if(df[!is.na(year) & atc_level==3, .N]>0){
no_records_my_3<-df[!is.na(year) & atc_level==3, .N, by=.(meaning, year,substr(medicinal_product_atc_code,1,3))]
names(no_records_my_3)<-c("meaning", "year","atc_code_3", "count")
no_records_my_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
for (a in 1:length(LETTERS)){
  if(no_records_my_3[atc_code_1==LETTERS[a],.N]>0)
    saveRDS(no_records_my_3[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_no_recmy_3_", actual_tables$MEDICINES[i], ".rds")))
}
rm(no_records_my_3)} else {no_records_my_3<-NULL}
#number of total records where atc level 1
if(df[!is.na(year) & atc_level==1, .N]>0){
no_records_my_1<-df[!is.na(year) & atc_level==1, .N, by=.(meaning, year,substr(medicinal_product_atc_code,1,1))]
names(no_records_my_1)<-c("meaning", "year","atc_code_1", "count")
no_records_my_1[,atc_code_3:=NA][,atc_code_4:=NA]
for (a in 1:length(LETTERS)){
  if(no_records_my_1[atc_code_1==LETTERS[a],.N]>0)
    saveRDS(no_records_my_1[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_no_recmy_1_", actual_tables$MEDICINES[i], ".rds")))
}
rm(no_records_my_1)} else {no_records_my_1<-NULL}

#male_users stratified by atc 4,5,6,7
if(df[!is.na(year) & sex_at_instance_creation=="M" &(atc_level==4 | atc_level==5 | atc_level==6 |atc_level==7), .N]>0){
male_users_my_4<-df[!is.na(year) & sex_at_instance_creation=="M" &(atc_level==4 | atc_level==5 | atc_level==6 |atc_level==7), .N, by=.(meaning, year,substr(medicinal_product_atc_code,1,4))]
names(male_users_my_4)<-c("meaning", "year","atc_code_4", "count")
male_users_my_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
for (a in 1:length(LETTERS)){
  if(male_users_my_4[atc_code_1==LETTERS[a],.N]>0)
    saveRDS(male_users_my_4[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_m_u_my_4_", actual_tables$MEDICINES[i], ".rds")))
}
rm(male_users_my_4)} else {male_users_my_4<-NULL}
#male_users stratified by atc 3
if(df[!is.na(year) & sex_at_instance_creation=="M" & atc_level==3, .N]>0){
male_users_my_3<-df[!is.na(year) & sex_at_instance_creation=="M" & atc_level==3, .N, by=.(meaning, year,substr(medicinal_product_atc_code,1,3))]
names(male_users_my_3)<-c("meaning", "year", "atc_code_3", "count")
male_users_my_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
for (a in 1:length(LETTERS)){
  if(male_users_my_3[atc_code_1==LETTERS[a],.N]>0)
    saveRDS(male_users_my_3[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_m_u_my_3_", actual_tables$MEDICINES[i], ".rds")))
}
rm(male_users_my_3)} else {male_users_my_3<-NULL}
#male_users by atc 1
if(df[!is.na(year) &sex_at_instance_creation=="M" & atc_level==1, .N]>0){
male_users_my_1<-df[!is.na(year) &sex_at_instance_creation=="M" & atc_level==1, .N, by=.(meaning, year,substr(medicinal_product_atc_code,1,1))]
names(male_users_my_1)<-c("meaning", "year","atc_code_1", "count")
male_users_my_1[,atc_code_3:=NA][,atc_code_4:=NA]
for (a in 1:length(LETTERS)){
  if(male_users_my_1[atc_code_1==LETTERS[a],.N]>0)
    saveRDS(male_users_my_1[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_m_u_my_1_", actual_tables$MEDICINES[i], ".rds")))
}
rm(male_users_my_1)} else {male_users_my_1<-NULL}

#female_users stratified by atc 4,5,6,7
if(df[!is.na(year) & sex_at_instance_creation=="F" &(atc_level==4 | atc_level==5 | atc_level==6 |atc_level==7), .N]>0){
female_users_my_4<-df[!is.na(year) & sex_at_instance_creation=="F" &(atc_level==4 | atc_level==5 | atc_level==6 |atc_level==7), .N, by=.(meaning, year,substr(medicinal_product_atc_code,1,4))]
names(female_users_my_4)<-c("meaning", "year","atc_code_4", "count")
female_users_my_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)][,meaning:="All"]
for (a in 1:length(LETTERS)){
  if(female_users_my_4[atc_code_1==LETTERS[a],.N]>0)
    saveRDS(female_users_my_4[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_f_u_my_4_", actual_tables$MEDICINES[i], ".rds")))
}
rm(female_users_my_4)} else {female_users_my_4<-NULL}
#female_users stratified by atc 3
if(df[!is.na(year) & sex_at_instance_creation=="F" & atc_level==3, .N]>0){
female_users_my_3<-df[!is.na(year) & sex_at_instance_creation=="F" & atc_level==3, .N, by=.(meaning, year,substr(medicinal_product_atc_code,1,3))]
names(female_users_my_3)<-c("meaning", "year", "atc_code_3", "count")
female_users_my_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
for (a in 1:length(LETTERS)){
  if(female_users_my_3[atc_code_1==LETTERS[a],.N]>0)
    saveRDS(female_users_my_3[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_f_u_my_3_", actual_tables$MEDICINES[i], ".rds")))
}
rm(female_users_my_3)} else {female_users_my_3<-NULL}
#female_users by atc 1
if(df[!is.na(year) & sex_at_instance_creation=="F" & atc_level==1, .N]>0){
female_users_my_1<-df[!is.na(year) & sex_at_instance_creation=="F" & atc_level==1, .N, by=.(meaning, year,substr(medicinal_product_atc_code,1,1))]
names(female_users_my_1)<-c("meaning", "year", "atc_code_1", "count")
female_users_my_1[,atc_code_3:=NA][,atc_code_4:=NA]
for (a in 1:length(LETTERS)){
  if(female_users_my_1[atc_code_1==LETTERS[a],.N]>0)
    saveRDS(female_users_my_1[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_f_u_my_1_", actual_tables$MEDICINES[i], ".rds")))
}
rm(female_users_my_1)} else {female_users_my_1<-NULL}

    
    #median for males
if(df[!is.na(year) & sex_at_instance_creation=="M" & (atc_level==4 |atc_level==5 |atc_level==6|atc_level==7), .N]>0){
   median_males_my_4<-df[!is.na(year) & sex_at_instance_creation=="M" & (atc_level==4 |atc_level==5 |atc_level==6|atc_level==7), .N, by=.(meaning, year, substr(medicinal_product_atc_code,1,4), person_id)]
    setnames(median_males_my_4, "N", "count")
    setnames(median_males_my_4, "substr", "atc_code_4")
    median_males_my_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
    for (a in 1:length(LETTERS)){
      if(median_males_my_4[atc_code_1==LETTERS[a],.N]>0)
        saveRDS(median_males_my_4[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_median_mmy_4_", actual_tables$MEDICINES[i], ".rds")))
    }
    rm(median_males_my_4)} else {median_males_my_4<-NULL}
    
if(df[!is.na(year) & sex_at_instance_creation=="M" & atc_level==3, .N]>0){
    median_males_my_3<-df[!is.na(year) & sex_at_instance_creation=="M" & atc_level==3, .N, by=.(meaning, year, substr(medicinal_product_atc_code,1,3), person_id)]
    setnames(median_males_my_3, "N", "count")
    setnames(median_males_my_3, "substr", "atc_code_3")
    median_males_my_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
    for (a in 1:length(LETTERS)){
      if(median_males_my_3[atc_code_1==LETTERS[a],.N]>0)
        saveRDS(median_males_my_3[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_median_mmy_3_", actual_tables$MEDICINES[i], ".rds")))
    }
    rm(median_males_my_3)} else {median_males_my_3<-NULL}
    
if(df[!is.na(year) & sex_at_instance_creation=="M" & atc_level==1, .N]>0){
    median_males_my_1<-df[!is.na(year) & sex_at_instance_creation=="M" & atc_level==1, .N, by=.(meaning, year, substr(medicinal_product_atc_code,1,1), person_id)]
    setnames(median_males_my_1, "N", "count")
    setnames(median_males_my_1, "substr", "atc_code_1")
    median_males_my_1[,atc_code_3:=NA][,atc_code_4:=NA]
    for (a in 1:length(LETTERS)){
      if(median_males_my_1[atc_code_1==LETTERS[a],.N]>0)
        saveRDS(median_males_my_1[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_median_mmy_1_", actual_tables$MEDICINES[i], ".rds")))
    }
    rm(median_males_my_1)} else {median_males_my_1<-NULL}
    
    #median for females
if(df[!is.na(year) & sex_at_instance_creation=="F" & (atc_level==4 |atc_level==5 |atc_level==6|atc_level==7), .N]>0){
    median_females_my_4<-df[!is.na(year) & sex_at_instance_creation=="F" & (atc_level==4 |atc_level==5 |atc_level==6|atc_level==7), .N, by=.(meaning, year, substr(medicinal_product_atc_code,1,4), person_id)]
    setnames(median_females_my_4, "N", "count")
    setnames(median_females_my_4, "substr", "atc_code_4")
    median_females_my_4[,atc_code_3:=substr(atc_code_4,1,3)][,atc_code_1:=substr(atc_code_4,1,1)]
    for (a in 1:length(LETTERS)){
      if(median_females_my_4[atc_code_1==LETTERS[a],.N]>0)
        saveRDS(median_females_my_4[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_median_fmy_4_", actual_tables$MEDICINES[i], ".rds")))
    }
    rm(median_females_my_4)} else {median_females_my_4<-NULL}
    
if(df[!is.na(year) & sex_at_instance_creation=="F" & atc_level==3, .N]>0){
    median_females_my_3<-df[!is.na(year) & sex_at_instance_creation=="F" & atc_level==3, .N, by=.(meaning, year, substr(medicinal_product_atc_code,1,3), person_id)]
    setnames(median_females_my_3, "N", "count")
    setnames(median_females_my_3, "substr", "atc_code_3")
    median_females_my_3[,atc_code_4:=NA][,atc_code_1:=substr(atc_code_3,1,1)]
    for (a in 1:length(LETTERS)){
      if(median_females_my_3[atc_code_1==LETTERS[a],.N]>0)
        saveRDS(median_females_my_3[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_median_fmy_3_", actual_tables$MEDICINES[i], ".rds")))
    }
    rm(median_females_my_3)} else {median_females_my_3<-NULL}
    
if(df[!is.na(year) & sex_at_instance_creation=="F" & atc_level==1, .N]>0){
    median_females_my_1<-df[!is.na(year) & sex_at_instance_creation=="F" & atc_level==1, .N, by=.(meaning, year, substr(medicinal_product_atc_code,1,1), person_id)]
    setnames(median_females_my_1, "N", "count")
    setnames(median_females_my_1, "substr", "atc_code_1")
    median_females_my_1[,atc_code_3:=NA][,atc_code_4:=NA]
    for (a in 1:length(LETTERS)){
      if(median_females_my_1[atc_code_1==LETTERS[a],.N]>0)
        saveRDS(median_females_my_1[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_median_fmy_1_", actual_tables$MEDICINES[i], ".rds")))
    }
    rm(median_females_my_1)} else {median_females_my_1<-NULL}
    

    #############
    #Table 13
    #############
    #number of male users
    male_users_7_m[[w]]<-df[sex_at_instance_creation=="M" & atc_level==7, .N, by="meaning_of_drug_record"]
    #number of female_users
    female_users_7_m[[w]]<-df[sex_at_instance_creation=="F"& atc_level==7, .N,by="meaning_of_drug_record"]
    #number of total records where atc level 7
    if(df[atc_level==7,.N]>0){
      no_records_7<-df[!is.na(year) & atc_level==7, .N, by="medicinal_product_atc_code"]
      names(no_records_7)<-c("atc_code_7", "count")
      no_records_7[,atc_code_1:=substr(atc_code_7,1,1)][,meaning:="All"]
      for (a in 1:length(LETTERS)){
        if(no_records_7[atc_code_1==LETTERS[a],.N]>0)
          saveRDS(no_records_7[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_no_records_7_", actual_tables$MEDICINES[i], ".rds")))
      }
      rm(no_records_7)
    } else {no_records_7<-NULL}
    
    #male_users stratified by atc 7
    if(df[!is.na(year) & sex_at_instance_creation=="M" & atc_level==7, .N]>0){
      male_users_7<-df[!is.na(year) & sex_at_instance_creation=="M" & atc_level==7, .N, by="medicinal_product_atc_code"]
      names(male_users_7)<-c("atc_code_7", "count")
      male_users_7[,atc_code_1:=substr(atc_code_7,1,1)][,meaning:="All"]
      for (a in 1:length(LETTERS)){
        if(male_users_7[atc_code_1==LETTERS[a],.N]>0)
          saveRDS(male_users_7[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_m_users_7_", actual_tables$MEDICINES[i], ".rds")))
      }
      rm(male_users_7)
    } else {male_users_7<-NULL}
    
    #female_users stratified by atc 7
    if(df[!is.na(year) & sex_at_instance_creation=="F" & atc_level==7, .N]>0){
      female_users_7<-df[!is.na(year) & sex_at_instance_creation=="F" & atc_level==7, .N, by="medicinal_product_atc_code"]
      names(female_users_7)<-c("atc_code_7", "count")
      female_users_7[,atc_code_1:=substr(atc_code_7,1,1)][,meaning:="All"]
      for (a in 1:length(LETTERS)){
        if(female_users_7[atc_code_1==LETTERS[a],.N]>0)
          saveRDS(female_users_7[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_f_users_7_", actual_tables$MEDICINES[i], ".rds")))
      }
      rm(female_users_7)
    } else { female_users_7<-NULL}
    #median in males users by atc code 7
    if(df[!is.na(year) & sex_at_instance_creation=="M" & atc_level==7, .N]>0){
      median_males_7<-df[!is.na(year) & sex_at_instance_creation=="M" & atc_level==7, .N, by=.(medicinal_product_atc_code, person_id)]
      setnames(median_males_7, "N", "count")
      setnames(median_males_7, "medicinal_product_atc_code", "atc_code_7")
      median_males_7[,atc_code_1:=substr(atc_code_7,1,1)]
      for (a in 1:length(LETTERS)){
        if(median_males_7[atc_code_1==LETTERS[a],.N]>0)
          saveRDS(median_males_7[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_median_m_7_", actual_tables$MEDICINES[i], ".rds")))
      }
      rm(median_males_7)
    } else {median_males_7<-NULL} 
    
    #median in female users by atc code 7
    if(df[!is.na(year) & sex_at_instance_creation=="F" & atc_level==7, .N]>0){
      median_females_7<-df[!is.na(year) & sex_at_instance_creation=="F" & atc_level==7, .N, by=.(medicinal_product_atc_code, person_id)]
      setnames(median_females_7, "N", "count")
      setnames(median_females_7, "medicinal_product_atc_code", "atc_code_7")
      median_females_7[,atc_code_1:=substr(atc_code_7,1,1)]
      for (a in 1:length(LETTERS)){
        if(median_females_7[atc_code_1==LETTERS[a],.N]>0)
          saveRDS(median_females_7[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_median_f_7_", actual_tables$MEDICINES[i], ".rds")))
      }
      rm(median_females_7)
    } else {median_females_7<-NULL} 
    
    #number of total records where atc level 7
    if(df[!is.na(year) & atc_level==7, .N]>0){
      no_records_my_7<-df[!is.na(year) & atc_level==7, .N, by=.(meaning, year,medicinal_product_atc_code)]
      names(no_records_my_7)<-c("meaning", "year","atc_code_7", "count")
      no_records_my_7[,atc_code_1:=substr(atc_code_7,1,1)]
      for (a in 1:length(LETTERS)){
        if(no_records_my_7[atc_code_1==LETTERS[a],.N]>0)
          saveRDS(no_records_my_7[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_no_recmy_7_", actual_tables$MEDICINES[i], ".rds")))
      }
      rm(no_records_my_7)} else {no_records_my_7<-NULL}
    
    #male_users stratified by atc 7
    if(df[!is.na(year) & sex_at_instance_creation=="M" & atc_level==7, .N]>0){
      male_users_my_7<-df[!is.na(year) & sex_at_instance_creation=="M" & atc_level==7, .N, by=.(meaning, year,medicinal_product_atc_code)]
      names(male_users_my_7)<-c("meaning", "year", "atc_code_7", "count")
      male_users_my_7[,atc_code_1:=substr(atc_code_7,1,1)]
      for (a in 1:length(LETTERS)){
        if(male_users_my_7[atc_code_1==LETTERS[a],.N]>0)
          saveRDS(male_users_my_7[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_m_u_my_7_", actual_tables$MEDICINES[i], ".rds")))
      }
      rm(male_users_my_7)} else {male_users_my_7<-NULL}
    
    #female_users stratified by atc 7
    if(df[!is.na(year) & sex_at_instance_creation=="F" & atc_level==7, .N]>0){
      female_users_my_7<-df[!is.na(year) & sex_at_instance_creation=="F" & atc_level==7, .N, by=.(meaning, year,medicinal_product_atc_code)]
      names(female_users_my_7)<-c("meaning", "year", "atc_code_7", "count")
      female_users_my_7[,atc_code_1:=substr(atc_code_7,1,1)]
      for (a in 1:length(LETTERS)){
        if(female_users_my_7[atc_code_1==LETTERS[a],.N]>0)
          saveRDS(female_users_my_7[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_f_u_my_7_", actual_tables$MEDICINES[i], ".rds")))
      }
      rm(female_users_my_7)} else {female_users_my_7<-NULL}
    
    #median males my
    if(df[!is.na(year) & sex_at_instance_creation=="M" & atc_level==7, .N]>0){
      median_males_my_7<-df[!is.na(year) & sex_at_instance_creation=="M" & atc_level==7, .N, by=.(meaning, year, medicinal_product_atc_code, person_id)]
      setnames(median_males_my_7, "N", "count")
      setnames(median_males_my_7, "medicinal_product_atc_code", "atc_code_7")
      median_males_my_7[,atc_code_1:=substr(atc_code_7,1,1)]
      for (a in 1:length(LETTERS)){
        if(median_males_my_7[atc_code_1==LETTERS[a],.N]>0)
          saveRDS(median_males_my_7[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_median_mmy_7_", actual_tables$MEDICINES[i], ".rds")))
      }
      rm(median_males_my_7)} else {median_males_my_7<-NULL}
    
    #median females my
    if(df[!is.na(year) & sex_at_instance_creation=="F" & atc_level==7, .N]>0){
      median_females_my_7<-df[!is.na(year) & sex_at_instance_creation=="F" & atc_level==7, .N, by=.(meaning, year, medicinal_product_atc_code, person_id)]
      setnames(median_females_my_7, "N", "count")
      setnames(median_females_my_7, "medicinal_product_atc_code", "atc_code_7")
      median_females_my_7[,atc_code_1:=substr(atc_code_7,1,1)]
      for (a in 1:length(LETTERS)){
        if(median_females_my_7[atc_code_1==LETTERS[a],.N]>0)
          saveRDS(median_females_my_7[atc_code_1==LETTERS[a]], paste0(tmp, paste0(LETTERS[a], "_median_fmy_7_", actual_tables$MEDICINES[i], ".rds")))
      }
      rm(median_females_my_7)} else {median_females_my_7<-NULL}
    
    ##########
    rm(df)
    w<-w+1
    #########
  }
  rm(study_population)
  
#################################################################################################
#Flowchart
################################################################################################
  print("Creating flowchart.")
  print("Get number of records in the original table.")
  #original number of records in the MEDICINES table(flowchart 1)
  orig_no_rows<-do.call(rbind,orig_no_rows)
  orig_no_rows<-sum(orig_no_rows)
  #number of records for the study population, no selection criteria for time applied (flowchart 2)
  print("Get number of records for the study population (no time criteria applied).")
  med_study_pop<-do.call(rbind,med_study_pop)
  med_study_pop<-sum(med_study_pop)
  #number of medicines records outside the observation period(check is done on individual level) (flowchart 3)
  print("Get number of records outside observation period.")
  med_out_st_per<-do.call(rbind,med_out_st_per) 
  med_out_st_per<-sum(med_out_st_per)
  #number of records in the study population with date dispensing/prescription inside study period (flowchart 4)
  print("Get number of records for the study population(time criteria applied).")
  med_study_pop_obsper<-do.call(rbind,med_study_pop_obsper) 
  med_study_pop_obsper<-sum(med_study_pop_obsper)
  #number of records in the study population with no meaning (flowchart 5)
  print("Get number of records with no meaning.")
  med_stdpop_no_meaning<-do.call(rbind,med_stdpop_no_meaning) 
  med_stdpop_no_meaning<-sum(med_stdpop_no_meaning) 
  #number of records in the study population
  print("Get number of records for study population.")
  med_study_population<-do.call(rbind,med_study_population) 
  med_study_population<-sum(med_study_population) 
   
  #Flowchart
  print("Create flowchart.")
  flowchart<-data.table(indicator=c("Number of records in the original table", 
                                     "Number of records for the study_population(no time criteria)",
                                    "Exclude: Number of records with date dispensing/prescription outside study period",
                                    "Number of records for the study_population(time criteria applied)",
                                    "Exclude:Number of records with empty meaning",
                                    "Number of records for study_population"), 
                        count=c(orig_no_rows,
                                    med_study_pop,
                                    med_out_st_per,
                                    med_study_pop_obsper,
                                    med_stdpop_no_meaning,
                                    med_study_population))

##Number of subjects from the study population that have never had a prescription/dispensing during the study period

  no_disp_pres_files<-list.files(tmp, pattern = "stdpop_not_med") 
if (length(no_disp_pres_files)>1){
  a<-readRDS(paste0(tmp,no_disp_pres_files[1]))
  for (i in 2:length(no_disp_pres_files)){
  a<-unique(a, readRDS(paste0(tmp,no_disp_pres_files[i])))
  }
  no_disp_pres<-length(a)
  rm(a)
} 
if (length(no_disp_pres_files)==1){
  a<-readRDS(paste0(tmp,no_disp_pres_files[1]))
  no_disp_pres<-length(a)}
if (length(no_disp_pres_files)==0){
  no_disp_pres<-0
}
#remove files not needed from tmp
for(i in 1:length(no_disp_pres_files)){
unlink(paste0(tmp,no_disp_pres_files[i]))
}
rm(no_disp_pres_files)

##################################################################################################
#Description
##################################################################################################
print("Creating description of study population.")
#meanings
print("Get list of meanings.")
meanings<-do.call(rbind,meanings)
meanings<-unique(c(meanings))
meanings_des<-paste(meanings, collapse = ", ")
#records with missing dates
print("Get number of records with both dates missing.")
miss_date_med<-do.call(rbind,miss_date_med)
miss_date_med<-sum(miss_date_med)
#number of records with empty atc codes when date disp/presc is present
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
  "Number of records with missing date_dispensing/prescription", 
                                    "Number of records with empty ATC codes when date_dispensing/prescription is present",
                                    "Number of records with complete ATC codes",
                                    "Number of records with complete ATC code to level 1",
                                    "Number of records with complete ATC code to level 2",
                                    "Number of records with complete ATC code to level 3",
                                    "Number of records with complete ATC code to level 4",
                                    "Number of records with complete ATC code to level 5",
                                    "Number of records with complete ATC code to level 6",
                                    "Number of records with complete ATC code to level 7"), 
                        count=c(meanings_des,
                          miss_date_med,
                                empty_atc_code,
                                comp_atc,
                                no_level1_atc,
                                no_level2_atc,
                                no_level3_atc,
                                no_level4_atc,
                                no_level5_atc,
                                no_level6_atc,
                                no_level7_atc))
#############################################################################################
#Table 15: Number of prescriptions/dispensings with incomplete data
#############################################################################################
print("Creating Table 15: Number of prescriptions/dispensings with incomplete data.")
print("Get all variables.")
#stratified by meaning
no_indication_m<-do.call(rbind,no_indication_m)
no_indication_m<-no_indication_m[,lapply(.SD, sum), .SDcols="N", by="meaning_of_drug_record"]
names(no_indication_m)<-c("meaning", "count")
no_prescriber_m<-do.call(rbind,no_prescriber_m)
no_prescriber_m<-no_prescriber_m[,lapply(.SD, sum), .SDcols="N", by="meaning_of_drug_record"]
names(no_prescriber_m)<-c("meaning", "count")
no_disp_num<-do.call(rbind,no_disp_num)
no_disp_num<-no_disp_num[,lapply(.SD, sum), .SDcols="N", by="meaning_of_drug_record"]
names(no_disp_num)<-c("meaning", "count")
no_presc_quantity<-do.call(rbind,no_presc_quantity)
no_presc_quantity<-no_presc_quantity[,lapply(.SD, sum), .SDcols="N", by="meaning_of_drug_record"]
names(no_presc_quantity)<-c("meaning", "count")
no_presc_quantity_unit<-do.call(rbind,no_presc_quantity_unit)
no_presc_quantity_unit<-no_presc_quantity_unit[,lapply(.SD, sum), .SDcols="N", by="meaning_of_drug_record"]
names(no_presc_quantity_unit)<-c("meaning", "count")

#total number of records
no_indication_t<-do.call(rbind,no_indication_t)
no_indication_t<-sum(no_indication_t)
no_prescriber_t<-do.call(rbind,no_prescriber_t)
no_prescriber_t<-sum(no_prescriber_t)
no_disp_num_t<-do.call(rbind,no_disp_num_t)
no_disp_num_t<-sum(no_disp_num_t)
no_presc_quantity_t<-do.call(rbind,no_presc_quantity_t)
no_presc_quantity_t<-sum(no_presc_quantity_t)
no_presc_quantity_unit_t<-do.call(rbind,no_presc_quantity_unit_t)
no_presc_quantity_unit_t<-sum(no_presc_quantity_unit_t)

print("Combine counts.")
if(no_indication_m[,.N]==0){
indication_info<-data.table(meaning=meanings, count=0)
} else {
  if (length(meanings[meanings %!in% no_indication_m[["meaning"]]])>0){
  indication_info<-rbind(data.table(meaning=meanings[meanings %!in% no_indication_m[["meaning"]]], count=0),no_indication_m)
  } else {
    indication_info<-no_indication_m
  }
}
rm(no_indication_m)

if(no_prescriber_m[,.N]==0){
  prescriber_info<-data.table(meaning=meanings, count=0)
} else {
  if (length(meanings[meanings %!in% no_prescriber_m[["meaning"]]])>0){
  prescriber_info<-rbind(data.table(meaning=meanings[meanings %!in% no_prescriber_m[["meaning"]]], count=0),no_prescriber_m)
  } else {
    prescriber_info<-no_prescriber_m
  }
}
rm(no_prescriber_m)

if(no_disp_num[,.N]==0){
  disp_info<-data.table(meaning=meanings, count=0)
} else {
  if(length(meanings[meanings %!in% no_disp_num[["meaning"]]])>0){
  disp_info<-rbind(data.table(meaning=meanings[meanings %!in% no_disp_num[["meaning"]]], count=0),no_disp_num)
  } else {
    disp_info<-no_disp_num
  }
}
rm(no_disp_num)

if(no_presc_quantity[,.N]==0){
  presc_info<-data.table(meaning=meanings, count=0)
} else {
  if(length(meanings[meanings %!in% no_presc_quantity[["meaning"]]])>0){
    presc_info<-rbind(data.table(meaning=meanings[meanings %!in% no_presc_quantity[["meaning"]]], count=0),no_presc_quantity)
  } else {
    presc_info<-no_presc_quantity
  }
}
rm(no_presc_quantity)

if(no_presc_quantity_unit[,.N]==0){
  presc_unit_info<-data.table(meaning=meanings, count=0)
} else {
  if(length(meanings[meanings %!in% no_presc_quantity_unit[["meaning"]]])>0){
    presc_unit_info<-rbind(data.table(meaning=meanings[meanings %!in% no_presc_quantity_unit[["meaning"]]], count=0),no_presc_quantity_unit)
  } else {
    presc_unit_info<-no_presc_quantity_unit
  }
}
rm(no_presc_quantity_unit)

#number of records in the study population by meaning
med_study_population_meaning<-do.call(rbind,med_study_population_meaning)
med_study_population_meaning<-med_study_population_meaning[,lapply(.SD, sum), .SDcols="N", by="meaning_of_drug_record"]
names(med_study_population_meaning)<-c("meaning", "count")
if(med_study_population_meaning[,.N]==0){
  med_study_population_meaning_info<-data.table(meaning=meanings, count=0)
} else {
  if(length(meanings[meanings %!in% med_study_population_meaning[["meaning"]]])>0){
    med_study_population_meaning_info<-rbind(data.table(meaning=meanings[meanings %!in% med_study_population_meaning[["meaning"]]], count=0),med_study_population_meaning)
  } else {
    med_study_population_meaning_info<-med_study_population_meaning
  }
}
meaning_info<-data.table(meaning=meanings)

print("Create table 15.")
#combine all together
tab15<-merge(meaning_info,med_study_population_meaning_info,by="meaning")
setnames(tab15, "count", "records_study_population")    
tab15<-merge(tab15, indication_info, by="meaning")    
setnames(tab15, "count", "records_no_indication_code") 
tab15<-merge(tab15, prescriber_info, by="meaning")    
setnames(tab15, "count", "records_no_prescriber_speciality") 
tab15<-merge(tab15, disp_info, by="meaning")    
setnames(tab15, "count", "no_dispensed_quantity") 
tab15<-merge(tab15, presc_info, by="meaning")    
setnames(tab15, "count", "no_prescribed_quantity") 
tab15<-merge(tab15, presc_unit_info, by="meaning")    
setnames(tab15, "count", "no_prescribed_quantity_unit")
tab15<-rbind(tab15, data.table(meaning="All", 
                              records_study_population=med_study_population,
                              records_no_indication_code=no_indication_t,
                              records_no_prescriber_speciality=no_prescriber_t,
                              no_dispensed_quantity=no_disp_num_t,
                              no_prescribed_quantity=no_presc_quantity_t,
                              no_prescribed_quantity_unit=no_presc_quantity_unit_t))
rm(no_indication_t,no_prescriber_t,no_disp_num_t,no_presc_quantity_t,no_presc_quantity_unit_t)

print("Export table 15.")
tab15<-data.table(DAP=data_access_provider_name,data_source=data_source_name, tab15)
saveRDS(tab15,paste0(final, "tab15.rds"))

rm(tab15)

#################################################################################################
#Table 10: Number of prescriptions/dispensings by ATC A level in the study population by year of dispensing/prescribing and by meaning_of_drug_record
#################################################################################################
print("Creating Table 10:  Number of prescriptions/dispensings by ATC A level in the study population by year of dispensing/prescribing and by meaning_of_drug_record.")
print("Get all variables.")
#empty atc codes by meaning
empty_atc_code_m_y<-do.call(rbind,empty_atc_code_m_y)
empty_atc_code_m_y<-empty_atc_code_m_y[,lapply(.SD, sum), .SDcols="N", by=c("meaning_of_drug_record", "year")]
names(empty_atc_code_m_y)<-c("meaning","year", "count")
meaning_year<-expand.grid(meanings,years)
names(meaning_year)<-c("meaning", "year")

empty_atc_my_info<-data.table(merge(meaning_year,empty_atc_code_m_y, by=c("meaning", "year"), all=T))
empty_atc_my_info[is.na(count), count:=0]
empty_atc_my_info[,atc_code_1:="empty"]

#counts by meaning, year and atc level 1:load Res.1 
Res.1_files<-list.files(tmp,pattern="^Res.1")
if (length(Res.1_files)>1){
Res.1<-readRDS(paste0(tmp,Res.1_files[1]))
for (i in 2:length(Res.1_files)){
  a<-readRDS(paste0(tmp,Res.1_files[i]))
  Res.1$count<-rbind(Res.1$count,a$count)
  Res.1$total<-rbind(Res.1$total,a$total)
  rm(a)
  Res.1$count<-Res.1$count[,lapply(.SD, sum), .SDcols="count", by=c("meaning", "year", "atc_code_1")]
  Res.1$total<-Res.1$total[,lapply(.SD, sum), .SDcols="total", by=c("meaning", "year")]
}
}
if (length(Res.1_files)==1){
  Res.1<-readRDS(paste0(tmp,Res.1_files[1]))
}

#remove all files in Res.1_files
#remove files not needed from tmp
for(i in 1:length(Res.1_files)){
  unlink(paste0(tmp,Res.1_files[i]))
}

Res.1$total[,atc_code_1:="total"]
setnames(Res.1$total,"total", "count")
print("Create table 10.")
tab10<-data.table(rbind(Res.1$count, Res.1$total, empty_atc_my_info))
setorderv(tab10, c("meaning", "year", "atc_code_1"))
rm(Res.1,Res.1_files)

#remove empty if total not available
tmp_empty<-merge(tab10[atc_code_1=="empty",c("meaning", "atc_code_1", "year")],
           tab10[atc_code_1=="total",c("meaning", "atc_code_1", "year")], by=c("meaning", "year"), all=T)
names(tmp_empty)<-c("meaning", "year", "empty", "total")
tmp_empty<-tmp_empty[is.na(total), year]
tab10<-tab10[year %in% tmp_empty & atc_code_1=="empty", count:=NA]
tab10<-tab10[!is.na(count)]
rm(tmp_empty)
print("Export table 10.")
tab10<-data.table(DAP=data_access_provider_name,data_source=data_source_name,indicator="no_of_records", tab10)
saveRDS(tab10, paste0(final, "tab10.rds"))
rm(tab10)

#################################################################################################
#Table 11: Number of prescriptions/dispensings by ATC A level in the female study population of childbearing age 12-55 years (based on age at Start_study_fup) by year of dispensing/prescribing and by meaning_of_drug_record
#################################################################################################
print("Creating Table 11:  Number of prescriptions/dispensings by ATC A level in the female study population of childbearing age 12-55 years (based on age at Start_study_fup) by year of dispensing/prescribing and by meaning_of_drug_record.")
print("Get all variables.")

#empty atc codes by meaning
empty_atc_code_m_y_f<-do.call(rbind,empty_atc_code_m_y_f)
empty_atc_code_m_y_f<-empty_atc_code_m_y_f[,lapply(.SD, sum), .SDcols="N", by=c("meaning_of_drug_record", "year")]
names(empty_atc_code_m_y_f)<-c("meaning","year", "count")


empty_atc_myf_info<-data.table(merge(meaning_year,empty_atc_code_m_y_f, by=c("meaning", "year"), all=T))
empty_atc_myf_info[is.na(count), count:=0]
empty_atc_myf_info[,atc_code_1:="empty"]

#counts by meaning, year and atc level 1:load Res.1 
Res.2_files<-list.files(tmp,pattern="^Res.2")
if (length(Res.2_files)>1){
  Res.2<-readRDS(paste0(tmp,Res.2_files[1]))
  for (i in 2:length(Res.2_files)){
    a<-readRDS(paste0(tmp,Res.2_files[i]))
    Res.2$count<-rbind(Res.2$count,a$count)
    Res.2$total<-rbind(Res.2$total,a$total)
    rm(a)
    Res.2$count<-Res.2$count[,lapply(.SD, sum), .SDcols="count", by=c("meaning", "year", "atc_code_1")]
    Res.2$total<-Res.2$total[,lapply(.SD, sum), .SDcols="total", by=c("meaning", "year")]
  }
}
if (length(Res.2_files)==1){
  Res.2<-readRDS(paste0(tmp,Res.2_files[1]))
}

#remove all files in Res.1_files
#remove files not needed from tmp
for(i in 1:length(Res.2_files)){
  unlink(paste0(tmp,Res.2_files[i]))
}

Res.2$total[,atc_code_1:="total"]
setnames(Res.2$total,"total", "count")
print("Create table 11.")
tab11<-data.table(rbind(Res.2$count, Res.2$total, empty_atc_myf_info))
setorderv(tab11, c("meaning", "year", "atc_code_1"))
rm(Res.2,Res.2_files)
#remove empty if total not available
tmp_empty<-merge(tab11[atc_code_1=="empty",c("meaning", "atc_code_1", "year")],
tab11[atc_code_1=="total",c("meaning", "atc_code_1", "year")], by=c("meaning", "year"), all=T)
names(tmp_empty)<-c("meaning", "year", "empty", "total")
tmp_empty<-tmp_empty[is.na(total), year]
tab11<-tab11[year %in% tmp_empty & atc_code_1=="empty", count:=NA]
tab11<-tab11[!is.na(count)]
rm(tmp_empty)
print("Export table 11.")
tab11<-data.table(DAP=data_access_provider_name,data_source=data_source_name,indicator="no_of_records", tab11)
saveRDS(tab11,paste0(final, "tab11.rds"))
rm(tab11)
######################################################################################################
#Table 12:Number of prescriptions/dispensings by ATC 1,3 and 4level in the study population by year of dispensing/prescribing and by meaning_of_drug_record
######################################################################################################
print("Creating Table 12:  Number of prescriptions/dispensings by ATC 1, 3 and 4 level in the study population by year of dispensing/prescribing and by meaning_of_drug_record.")
print("Get all variables.")

#male and female users by meaning
male_users_m<-do.call(rbind,male_users_m)
if(male_users_m[,.N] !=0){
male_users_m<-male_users_m[,lapply(.SD,sum), by="meaning_of_drug_record"]
setnames(male_users_m, "meaning_of_drug_record", "meaning")
setnames(male_users_m, "N", "no_male_users")
male_users_m<-cbind(male_users_m, atc_code_1="none", atc_code_3="none", atc_code_4="none",no_records="N/A",median_rx_male_users="N/A",median_rx_female_users="N/A")
} else {male_users_m<-NULL}
female_users_m<-do.call(rbind,female_users_m)
if(female_users_m[,.N] !=0){
female_users_m<-female_users_m[,lapply(.SD,sum), by="meaning_of_drug_record"]
setnames(female_users_m, "meaning_of_drug_record", "meaning")
setnames(female_users_m, "N", "no_female_users")
female_users_m<-cbind(female_users_m, atc_code_1="none", atc_code_3="none", atc_code_4="none",no_records="N/A",median_rx_male_users="N/A",median_rx_female_users="N/A")
} else {female_users_m<-NULL}

population_m_tab12<-merge(male_users_m, female_users_m, by=c("meaning", "atc_code_1", "atc_code_3", "atc_code_4", "no_records", "median_rx_male_users", "median_rx_female_users"), all=T, use.names=F)
population_m_tab12[is.na(no_male_users),no_male_users:=0][is.na(no_female_users),no_female_users:=0]
population_m_tab12[,year:="all"]
###############
#no_of_records
###############
#atc_4
no_records_4_files<-list.files(tmp,pattern="no_records_4")
if(length(no_records_4_files)>0){
#grab only the first letter which symoblized the ATC level 1
letters_atc<-unique(sapply(no_records_4_files, function(x) substr(x,1,1))) 
#create list for each letter
list_records_4<-vector(mode="list", length=length(letters_atc))
names(list_records_4)<-letters_atc
for (i in 1:length(list_records_4)){
list_records_4[[i]]<-no_records_4_files[substr(no_records_4_files,1,1)==names(list_records_4)[i]]
}
rm(letters_atc)
#merge by atc code
for(i in 1 :length(list_records_4)){
  no_records_4<-c()
  if (length(list_records_4[[i]])>1){
    no_records_4<-readRDS(paste0(tmp,list_records_4[[i]][1]))
    for (z in 2:length(list_records_4[[i]])){
      a<-readRDS(paste0(tmp,list_records_4[[i]][z]))
      no_records_4<-rbind(no_records_4,a)
      no_records_4<-no_records_4[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning")]
  rm(a)
    }
  }
    if (length(list_records_4[[i]])==1){
      no_records_4<-readRDS(paste0(tmp,list_records_4[[i]][1]))
      no_records_4<-no_records_4[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning")]
    }
  saveRDS(no_records_4, paste0(tmp, names(list_records_4)[i], "_no_records_4.rds"))

}
rm(no_records_4)
#remove all files that match pattern letter+no_records_4
for(i in 1:length(no_records_4_files)){
  unlink(paste0(tmp,no_records_4_files[i]))
}
rm(list_records_4)
} else {no_records_4<-NULL}
rm(no_records_4_files)
#atc_3
no_records_3_files<-list.files(tmp,pattern="no_records_3")
if(length(no_records_3_files)>0){
#grab only the first letter which symoblized the ATC level 1
letters_atc<-unique(sapply(no_records_3_files, function(x) substr(x,1,1))) 
#create list for each letter
list_records_3<-vector(mode="list", length=length(letters_atc))
names(list_records_3)<-letters_atc
for (i in 1:length(list_records_3)){
  list_records_3[[i]]<-no_records_3_files[substr(no_records_3_files,1,1)==names(list_records_3)[i]]
}
rm(letters_atc)
#merge by atc code
for(i in 1 :length(list_records_3)){
  no_records_3<-c()
  if (length(list_records_3[[i]])>1){
    no_records_3<-readRDS(paste0(tmp,list_records_3[[i]][1]))
    for (z in 2:length(list_records_3[[i]])){
      a<-readRDS(paste0(tmp,list_records_3[[i]][z]))
      no_records_3<-rbind(no_records_3,a)
      no_records_3<-no_records_3[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning")]
      rm(a)
    }
  }
  if (length(list_records_3[[i]])==1){
    no_records_3<-readRDS(paste0(tmp,list_records_3[[i]][1]))
    no_records_3<-no_records_3[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning")]
  }
  saveRDS(no_records_3, paste0(tmp, names(list_records_3)[i], "_no_records_3.rds"))
}
rm(no_records_3)
#remove all files that match pattern letter+no_records_4
for(i in 1:length(no_records_3_files)){
  unlink(paste0(tmp,no_records_3_files[i]))
}
rm(list_records_3)
} else {no_records_3<-NULL}
rm(no_records_3_files)
#atc_1
no_records_1_files<-list.files(tmp,pattern="no_records_1")
if(length(no_records_1_files)>0){
#grab only the first letter which symoblized the ATC level 1
letters_atc<-unique(sapply(no_records_1_files, function(x) substr(x,1,1))) 
#create list for each letter
list_records_1<-vector(mode="list", length=length(letters_atc))
names(list_records_1)<-letters_atc
for (i in 1:length(list_records_1)){
  list_records_1[[i]]<-no_records_1_files[substr(no_records_1_files,1,1)==names(list_records_1)[i]]
}
rm(letters_atc)
#merge by atc code
for(i in 1 :length(list_records_1)){
  no_records_1<-c()
  if (length(list_records_1[[i]])>1){
    no_records_1<-readRDS(paste0(tmp,list_records_1[[i]][1]))
    for (z in 2:length(list_records_1[[i]])){
      a<-readRDS(paste0(tmp,list_records_1[[i]][z]))
      no_records_1<-rbind(no_records_1,a)
      no_records_1<-no_records_1[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning")]
      rm(a)
    }
  }
  if (length(list_records_1[[i]])==1){
    no_records_1<-readRDS(paste0(tmp,list_records_1[[i]][1]))
    no_records_1<-no_records_1[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning")]
  }
  saveRDS(no_records_1, paste0(tmp, names(list_records_1)[i], "_no_records_1.rds"))
}
rm(no_records_1)
#remove all files that match pattern letter+no_records_4
for(i in 1:length(no_records_1_files)){
  unlink(paste0(tmp,no_records_1_files[i]))
}
rm(list_records_1)
} else { no_records_1<-NULL}
rm(no_records_1_files)

#save atc code level 1 in one vector so we can filter the files by atc code
#load one by one by atc and merge them together
#after finishing with one atc, save that in tmp again with name atc_code+no_records_4
#do the same for atc_3

#do the same process for male,female, median users

###############
#male_users
###############
#atc_4
male_users_4_files<-list.files(tmp,pattern="m_users_4")
if(length(male_users_4_files)>0){
#grab only the first letter which symoblized the ATC level 1
letters_atc<-unique(sapply(male_users_4_files, function(x) substr(x,1,1))) 
#create list for each letter
list_males_4<-vector(mode="list", length=length(letters_atc))
names(list_males_4)<-letters_atc
for (i in 1:length(list_males_4)){
  list_males_4[[i]]<-male_users_4_files[substr(male_users_4_files,1,1)==names(list_males_4)[i]]
}
rm(letters_atc)
#merge by atc code
for(i in 1 :length(list_males_4)){
  male_users_4<-c()
  if (length(list_males_4[[i]])>1){
    male_users_4<-readRDS(paste0(tmp,list_males_4[[i]][1]))
    for (z in 2:length(list_males_4[[i]])){
      a<-readRDS(paste0(tmp,list_males_4[[i]][z]))
      male_users_4<-rbind(male_users_4,a)
      male_users_4<-male_users_4[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning")]
      rm(a)
    }
  }
  if (length(list_males_4[[i]])==1){
    male_users_4<-readRDS(paste0(tmp,list_males_4[[i]][1]))
    male_users_4<-male_users_4[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning")]
  }
  saveRDS(male_users_4, paste0(tmp, names(list_males_4)[i], "_m_users_4.rds"))
}

#remove all files that match pattern letter+male_users_4
for(i in 1:length(male_users_4_files)){
  unlink(paste0(tmp,male_users_4_files[i]))
}
rm(list_males_4)
} else {male_users_4<-NULL}
rm(male_users_4_files)
#atc_3
male_users_3_files<-list.files(tmp,pattern="m_users_3")
if(length(male_users_3_files)>0){
#grab only the first letter which symoblized the ATC level 1
letters_atc<-unique(sapply(male_users_3_files, function(x) substr(x,1,1))) 
#create list for each letter
list_males_3<-vector(mode="list", length=length(letters_atc))
names(list_males_3)<-letters_atc
for (i in 1:length(list_males_3)){
  list_males_3[[i]]<-male_users_3_files[substr(male_users_3_files,1,1)==names(list_males_3)[i]]
}
rm(letters_atc)
#merge by atc code
for(i in 1 :length(list_males_3)){
  male_users_3<-c()
  if (length(list_males_3[[i]])>1){
    male_users_3<-readRDS(paste0(tmp,list_males_3[[i]][1]))
    for (z in 2:length(list_males_3[[i]])){
      a<-readRDS(paste0(tmp,list_males_3[[i]][z]))
      male_users_3<-rbind(male_users_3,a)
      male_users_3<-male_users_3[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning")]
      rm(a)
    }
  }
  if (length(list_males_3[[i]])==1){
    male_users_3<-readRDS(paste0(tmp,list_males_3[[i]][1]))
    male_users_3<-male_users_3[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning")]
  }
  saveRDS(male_users_3, paste0(tmp, names(list_males_3)[i], "_m_users_3.rds"))
}

#remove all files that match pattern letter+male_users_3
for(i in 1:length(male_users_3_files)){
  unlink(paste0(tmp,male_users_3_files[i]))
}
rm(list_males_3)
} else {male_users_3<-NULL}
rm(male_users_3_files)
#atc_1
male_users_1_files<-list.files(tmp,pattern="m_users_1")
if(length(male_users_1_files)>0){
#grab only the first letter which symoblized the ATC level 1
letters_atc<-unique(sapply(male_users_1_files, function(x) substr(x,1,1))) 
#create list for each letter
list_males_1<-vector(mode="list", length=length(letters_atc))
names(list_males_1)<-letters_atc
for (i in 1:length(list_males_1)){
  list_males_1[[i]]<-male_users_1_files[substr(male_users_1_files,1,1)==names(list_males_1)[i]]
}
rm(letters_atc)
#merge by atc code
for(i in 1 :length(list_males_1)){
  male_users_1<-c()
  if (length(list_males_1[[i]])>1){
    male_users_1<-readRDS(paste0(tmp,list_males_1[[i]][1]))
    for (z in 2:length(list_males_1[[i]])){
      a<-readRDS(paste0(tmp,list_males_1[[i]][z]))
      male_users_1<-rbind(male_users_1,a)
      male_users_1<-male_users_1[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning")]
      rm(a)
    }
  }
  if (length(list_males_1[[i]])==1){
    male_users_1<-readRDS(paste0(tmp,list_males_1[[i]][1]))
    male_users_1<-male_users_1[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning")]
  }
  saveRDS(male_users_1, paste0(tmp, names(list_males_1)[i], "_m_users_1.rds"))
}

#remove all files that match pattern letter+male_users_3
for(i in 1:length(male_users_1_files)){
  unlink(paste0(tmp,male_users_1_files[i]))
}
rm(list_males_1)
} else {male_users_1<-NULL}
rm(male_users_1_files)
###############
#female_users
###############
#atc_4
female_users_4_files<-list.files(tmp,pattern="f_users_4")
if(length(female_users_4_files)>0){
#grab only the first letter which symoblized the ATC level 1
letters_atc<-unique(sapply(female_users_4_files, function(x) substr(x,1,1))) 
#create list for each letter
list_females_4<-vector(mode="list", length=length(letters_atc))
names(list_females_4)<-letters_atc
for (i in 1:length(list_females_4)){
  list_females_4[[i]]<-female_users_4_files[substr(female_users_4_files,1,1)==names(list_females_4)[i]]
}
rm(letters_atc)
#merge by atc code
for(i in 1 :length(list_females_4)){
  female_users_4<-c()
  if (length(list_females_4[[i]])>1){
    female_users_4<-readRDS(paste0(tmp,list_females_4[[i]][1]))
    for (z in 2:length(list_females_4[[i]])){
      a<-readRDS(paste0(tmp,list_females_4[[i]][z]))
      female_users_4<-rbind(female_users_4,a)
      female_users_4<-female_users_4[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning")]
      rm(a)
    }
  }
  if (length(list_females_4[[i]])==1){
    female_users_4<-readRDS(paste0(tmp,list_females_4[[i]][1]))
    female_users_4<-female_users_4[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning")]
  }
  saveRDS(female_users_4, paste0(tmp, names(list_females_4)[i], "_f_users_4.rds"))
}

#remove all files that match pattern letter+male_users_4
for(i in 1:length(female_users_4_files)){
  unlink(paste0(tmp,female_users_4_files[i]))
}
rm(list_females_4)
} else {female_users_4<-NULL}
rm(female_users_4_files)
#atc_3
female_users_3_files<-list.files(tmp,pattern="f_users_3")
if(length(female_users_3_files)>0){
#grab only the first letter which symoblized the ATC level 1
letters_atc<-unique(sapply(female_users_3_files, function(x) substr(x,1,1))) 
#create list for each letter
list_females_3<-vector(mode="list", length=length(letters_atc))
names(list_females_3)<-letters_atc
for (i in 1:length(list_females_3)){
  list_females_3[[i]]<-female_users_3_files[substr(female_users_3_files,1,1)==names(list_females_3)[i]]
}
rm(letters_atc)
#merge by atc code
for(i in 1 :length(list_females_3)){
  female_users_3<-c()
  if (length(list_females_3[[i]])>1){
    female_users_3<-readRDS(paste0(tmp,list_females_3[[i]][1]))
    for (z in 2:length(list_females_3[[i]])){
      a<-readRDS(paste0(tmp,list_females_3[[i]][z]))
      female_users_3<-rbind(female_users_3,a)
      female_users_3<-female_users_3[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning")]
      rm(a)
    }
  }
  if (length(list_females_3[[i]])==1){
    female_users_3<-readRDS(paste0(tmp,list_females_3[[i]][1]))
    female_users_3<-female_users_3[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning")]
  }
  saveRDS(female_users_3, paste0(tmp, names(list_females_3)[i], "_f_users_3.rds"))
}

#remove all files that match pattern letter+female_users_3
for(i in 1:length(female_users_3_files)){
  unlink(paste0(tmp,female_users_3_files[i]))
}
rm(list_females_3)
} else {female_users_3<-NULL}
rm(female_users_3_files)
#atc_1
female_users_1_files<-list.files(tmp,pattern="f_users_1")
if(length(female_users_1_files)>0){
#grab only the first letter which symoblized the ATC level 1
letters_atc<-unique(sapply(female_users_1_files, function(x) substr(x,1,1))) 
#create list for each letter
list_females_1<-vector(mode="list", length=length(letters_atc))
names(list_females_1)<-letters_atc
for (i in 1:length(list_females_1)){
  list_females_1[[i]]<-female_users_1_files[substr(female_users_1_files,1,1)==names(list_females_1)[i]]
}
rm(letters_atc)
#merge by atc code
for(i in 1 :length(list_females_1)){
  female_users_1<-c()
  if (length(list_females_1[[i]])>1){
    female_users_1<-readRDS(paste0(tmp,list_females_1[[i]][1]))
    for (z in 2:length(list_females_1[[i]])){
      a<-readRDS(paste0(tmp,list_females_1[[i]][z]))
      female_users_1<-rbind(female_users_1,a)
      female_users_1<-female_users_1[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning")]
      rm(a)
    }
  }
  if (length(list_females_1[[i]])==1){
    female_users_1<-readRDS(paste0(tmp,list_females_1[[i]][1]))
    female_users_1<-female_users_1[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning")]
  }
  saveRDS(female_users_1, paste0(tmp, names(list_females_1)[i], "_f_users_1.rds"))
}

#remove all files that match pattern letter+male_users_3
for(i in 1:length(female_users_1_files)){
  unlink(paste0(tmp,female_users_1_files[i]))
}
rm(list_females_1)
} else {female_users_1<-NULL}
rm(female_users_1_files)
###############
#no_of_records_my
###############
#atc_4
no_records_my_4_files<-list.files(tmp,pattern="no_recmy_4")
if(length(no_records_my_4_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(no_records_my_4_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_records_my_4<-vector(mode="list", length=length(letters_atc))
  names(list_records_my_4)<-letters_atc
  for (i in 1:length(list_records_my_4)){
    list_records_my_4[[i]]<-no_records_my_4_files[substr(no_records_my_4_files,1,1)==names(list_records_my_4)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_records_my_4)){
    no_records_my_4<-c()
    if (length(list_records_my_4[[i]])>1){
      no_records_my_4<-readRDS(paste0(tmp,list_records_my_4[[i]][1]))
      for (z in 2:length(list_records_my_4[[i]])){
        a<-readRDS(paste0(tmp,list_records_my_4[[i]][z]))
        no_records_my_4<-rbind(no_records_my_4,a)
        no_records_my_4<-no_records_my_4[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning", "year")]
        rm(a)
      }
    }
    if (length(list_records_my_4[[i]])==1){
      no_records_my_4<-readRDS(paste0(tmp,list_records_my_4[[i]][1]))
      no_records_my_4<-no_records_my_4[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning", "year")]
    }
    saveRDS(no_records_my_4, paste0(tmp, names(list_records_my_4)[i], "_no_recmy_4.rds"))
  }
  rm(no_records_my_4)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(no_records_my_4_files)){
    unlink(paste0(tmp,no_records_my_4_files[i]))
  }
  rm(list_records_my_4)
} else {no_records_my_4<-NULL}
rm(no_records_my_4_files)
#atc_3
no_records_my_3_files<-list.files(tmp,pattern="no_recmy_3")
if(length(no_records_my_3_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(no_records_my_3_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_records_my_3<-vector(mode="list", length=length(letters_atc))
  names(list_records_my_3)<-letters_atc
  for (i in 1:length(list_records_my_3)){
    list_records_my_3[[i]]<-no_records_my_3_files[substr(no_records_my_3_files,1,1)==names(list_records_my_3)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_records_my_3)){
    no_records_my_3<-c()
    if (length(list_records_my_3[[i]])>1){
      no_records_my_3<-readRDS(paste0(tmp,list_records_my_3[[i]][1]))
      for (z in 2:length(list_records_my_3[[i]])){
        a<-readRDS(paste0(tmp,list_records_my_3[[i]][z]))
        no_records_my_3<-rbind(no_records_my_3,a)
        no_records_my_3<-no_records_my_3[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning", "year")]
        rm(a)
      }
    }
    if (length(list_records_my_3[[i]])==1){
      no_records_my_3<-readRDS(paste0(tmp,list_records_my_3[[i]][1]))
      no_records_my_3<-no_records_my_3[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning", "year")]
    }
    saveRDS(no_records_my_3, paste0(tmp, names(list_records_my_3)[i], "_no_recmy_3.rds"))
  }
  rm(no_records_my_3)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(no_records_my_3_files)){
    unlink(paste0(tmp,no_records_my_3_files[i]))
  }
  rm(list_records_my_3)
} else {no_records_my_3<-NULL}
rm(no_records_my_3_files)
#atc_1
no_records_my_1_files<-list.files(tmp,pattern="no_recmy_1")
if(length(no_records_my_1_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(no_records_my_1_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_records_my_1<-vector(mode="list", length=length(letters_atc))
  names(list_records_my_1)<-letters_atc
  for (i in 1:length(list_records_my_1)){
    list_records_my_1[[i]]<-no_records_my_1_files[substr(no_records_my_1_files,1,1)==names(list_records_my_1)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_records_my_1)){
    no_records_my_1<-c()
    if (length(list_records_my_1[[i]])>1){
      no_records_my_1<-readRDS(paste0(tmp,list_records_my_1[[i]][1]))
      for (z in 2:length(list_records_my_1[[i]])){
        a<-readRDS(paste0(tmp,list_records_my_1[[i]][z]))
        no_records_my_1<-rbind(no_records_my_1,a)
        no_records_my_1<-no_records_my_1[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning", "year")]
        rm(a)
      }
    }
    if (length(list_records_my_1[[i]])==1){
      no_records_my_1<-readRDS(paste0(tmp,list_records_my_1[[i]][1]))
      no_records_my_1<-no_records_my_1[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning", "year")]
    }
    saveRDS(no_records_my_1, paste0(tmp, names(list_records_my_1)[i], "_no_recmy_1.rds"))
  }
  rm(no_records_my_1)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(no_records_my_1_files)){
    unlink(paste0(tmp,no_records_my_1_files[i]))
  }
  rm(list_records_my_1)
} else { no_records_my_1<-NULL}
rm(no_records_my_1_files)

#####################
#male_users_my
####################
#atc 4
male_users_my_4_files<-list.files(tmp,pattern="m_u_my_4")
if(length(male_users_my_4_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(male_users_my_4_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_male_users_my_4<-vector(mode="list", length=length(letters_atc))
  names(list_male_users_my_4)<-letters_atc
  for (i in 1:length(list_male_users_my_4)){
    list_male_users_my_4[[i]]<-male_users_my_4_files[substr(male_users_my_4_files,1,1)==names(list_male_users_my_4)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_male_users_my_4)){
    no_m_u_my_4<-c()
    if (length(list_male_users_my_4[[i]])>1){
      no_m_u_my_4<-readRDS(paste0(tmp,list_male_users_my_4[[i]][1]))
      for (z in 2:length(list_male_users_my_4[[i]])){
        a<-readRDS(paste0(tmp,list_male_users_my_4[[i]][z]))
        no_m_u_my_4<-rbind(no_m_u_my_4,a)
        no_m_u_my_4<-no_m_u_my_4[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning", "year")]
        rm(a)
      }
    }
    if (length(list_male_users_my_4[[i]])==1){
      no_m_u_my_4<-readRDS(paste0(tmp,list_male_users_my_4[[i]][1]))
      no_m_u_my_4<-no_m_u_my_4[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning", "year")]
    }
    saveRDS(no_m_u_my_4, paste0(tmp, names(list_male_users_my_4)[i], "_m_u_my_4.rds"))
  }
  rm(no_m_u_my_4)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(male_users_my_4_files)){
    unlink(paste0(tmp,male_users_my_4_files[i]))
  }
  rm(list_male_users_my_4)
} else {no_m_u_my_4<-NULL}
rm(male_users_my_4_files)
#atc 3
male_users_my_3_files<-list.files(tmp,pattern="m_u_my_3")
if(length(male_users_my_3_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(male_users_my_3_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_male_users_my_3<-vector(mode="list", length=length(letters_atc))
  names(list_male_users_my_3)<-letters_atc
  for (i in 1:length(list_male_users_my_3)){
    list_male_users_my_3[[i]]<-male_users_my_3_files[substr(male_users_my_3_files,1,1)==names(list_male_users_my_3)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_male_users_my_3)){
    no_m_u_my_3<-c()
    if (length(list_male_users_my_3[[i]])>1){
      no_m_u_my_3<-readRDS(paste0(tmp,list_male_users_my_3[[i]][1]))
      for (z in 2:length(list_male_users_my_3[[i]])){
        a<-readRDS(paste0(tmp,list_male_users_my_3[[i]][z]))
        no_m_u_my_3<-rbind(no_m_u_my_3,a)
        no_m_u_my_3<-no_m_u_my_4[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning", "year")]
        rm(a)
      }
    }
    if (length(list_male_users_my_3[[i]])==1){
      no_m_u_my_3<-readRDS(paste0(tmp,list_male_users_my_3[[i]][1]))
      no_m_u_my_3<-no_m_u_my_3[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning", "year")]
    }
    saveRDS(no_m_u_my_3, paste0(tmp, names(list_male_users_my_3)[i], "_m_u_my_3.rds"))
  }
  rm(no_m_u_my_3)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(male_users_my_3_files)){
    unlink(paste0(tmp,male_users_my_3_files[i]))
  }
  rm(list_male_users_my_3)
} else {no_m_u_my_3<-NULL}
rm(male_users_my_3_files)
#atc_1
male_users_my_1_files<-list.files(tmp,pattern="m_u_my_1")
if(length(male_users_my_1_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(male_users_my_1_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_male_users_my_1<-vector(mode="list", length=length(letters_atc))
  names(list_male_users_my_1)<-letters_atc
  for (i in 1:length(list_male_users_my_1)){
    list_male_users_my_1[[i]]<-male_users_my_1_files[substr(male_users_my_1_files,1,1)==names(list_male_users_my_1)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_male_users_my_1)){
    no_m_u_my_1<-c()
    if (length(list_male_users_my_1[[i]])>1){
      no_m_u_my_1<-readRDS(paste0(tmp,list_male_users_my_1[[i]][1]))
      for (z in 2:length(list_male_users_my_1[[i]])){
        a<-readRDS(paste0(tmp,list_male_users_my_1[[i]][z]))
        no_m_u_my_1<-rbind(no_m_u_my_1,a)
        no_m_u_my_1<-no_m_u_my_1[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning", "year")]
        rm(a)
      }
    }
    if (length(list_male_users_my_1[[i]])==1){
      no_m_u_my_1<-readRDS(paste0(tmp,list_male_users_my_1[[i]][1]))
      no_m_u_my_1<-no_m_u_my_1[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning", "year")]
    }
    saveRDS(no_m_u_my_1, paste0(tmp, names(list_male_users_my_1)[i], "_m_u_my_1.rds"))
  }
  rm(no_m_u_my_1)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(male_users_my_1_files)){
    unlink(paste0(tmp,male_users_my_1_files[i]))
  }
  rm(list_male_users_my_1)
} else {no_m_u_my_1<-NULL}
rm(male_users_my_1_files)
#####################
#female_users_my
####################
#atc 4
female_users_my_4_files<-list.files(tmp,pattern="f_u_my_4")
if(length(female_users_my_4_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(female_users_my_4_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_female_users_my_4<-vector(mode="list", length=length(letters_atc))
  names(list_female_users_my_4)<-letters_atc
  for (i in 1:length(list_female_users_my_4)){
    list_female_users_my_4[[i]]<-female_users_my_4_files[substr(female_users_my_4_files,1,1)==names(list_female_users_my_4)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_female_users_my_4)){
    no_f_u_my_4<-c()
    if (length(list_female_users_my_4[[i]])>1){
      no_f_u_my_4<-readRDS(paste0(tmp,list_female_users_my_4[[i]][1]))
      for (z in 2:length(list_female_users_my_4[[i]])){
        a<-readRDS(paste0(tmp,list_female_users_my_4[[i]][z]))
        no_f_u_my_4<-rbind(no_f_u_my_4,a)
        no_f_u_my_4<-no_f_u_my_4[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning", "year")]
        rm(a)
      }
    }
    if (length(list_female_users_my_4[[i]])==1){
      no_f_u_my_4<-readRDS(paste0(tmp,list_female_users_my_4[[i]][1]))
      no_f_u_my_4<-no_f_u_my_4[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning", "year")]
    }
    saveRDS(no_f_u_my_4, paste0(tmp, names(list_female_users_my_4)[i], "_f_u_my_4.rds"))
  }
  rm(no_f_u_my_4)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(female_users_my_4_files)){
    unlink(paste0(tmp,female_users_my_4_files[i]))
  }
  rm(list_female_users_my_4)
} else {no_f_u_my_4<-NULL}
rm(female_users_my_4_files)
#atc 3
female_users_my_3_files<-list.files(tmp,pattern="f_u_my_3")
if(length(female_users_my_3_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(female_users_my_3_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_female_users_my_3<-vector(mode="list", length=length(letters_atc))
  names(list_female_users_my_3)<-letters_atc
  for (i in 1:length(list_female_users_my_3)){
    list_female_users_my_3[[i]]<-female_users_my_3_files[substr(female_users_my_3_files,1,1)==names(list_female_users_my_3)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_female_users_my_3)){
    no_f_u_my_3<-c()
    if (length(list_female_users_my_3[[i]])>1){
      no_f_u_my_3<-readRDS(paste0(tmp,list_female_users_my_3[[i]][1]))
      for (z in 2:length(list_female_users_my_3[[i]])){
        a<-readRDS(paste0(tmp,list_female_users_my_3[[i]][z]))
        no_f_u_my_3<-rbind(no_f_u_my_3,a)
        no_f_u_my_3<-no_f_u_my_4[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning", "year")]
        rm(a)
      }
    }
    if (length(list_female_users_my_3[[i]])==1){
      no_f_u_my_3<-readRDS(paste0(tmp,list_female_users_my_3[[i]][1]))
      no_f_u_my_3<-no_f_u_my_3[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning", "year")]
    }
    saveRDS(no_f_u_my_3, paste0(tmp, names(list_female_users_my_3)[i], "_f_u_my_3.rds"))
  }
  rm(no_f_u_my_3)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(female_users_my_3_files)){
    unlink(paste0(tmp,female_users_my_3_files[i]))
  }
  rm(list_female_users_my_3)
} else {no_f_u_my_3<-NULL}
rm(female_users_my_3_files)
#atc_1
female_users_my_1_files<-list.files(tmp,pattern="f_u_my_1")
if(length(female_users_my_1_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(female_users_my_1_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_female_users_my_1<-vector(mode="list", length=length(letters_atc))
  names(list_female_users_my_1)<-letters_atc
  for (i in 1:length(list_female_users_my_1)){
    list_female_users_my_1[[i]]<-female_users_my_1_files[substr(female_users_my_1_files,1,1)==names(list_female_users_my_1)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_female_users_my_1)){
    no_f_u_my_1<-c()
    if (length(list_female_users_my_1[[i]])>1){
      no_f_u_my_1<-readRDS(paste0(tmp,list_female_users_my_1[[i]][1]))
      for (z in 2:length(list_female_users_my_1[[i]])){
        a<-readRDS(paste0(tmp,list_female_users_my_1[[i]][z]))
        no_f_u_my_1<-rbind(no_f_u_my_1,a)
        no_f_u_my_1<-no_f_u_my_1[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning", "year")]
        rm(a)
      }
    }
    if (length(list_female_users_my_1[[i]])==1){
      no_f_u_my_1<-readRDS(paste0(tmp,list_female_users_my_1[[i]][1]))
      no_f_u_my_1<-no_f_u_my_1[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "meaning", "year")]
    }
    saveRDS(no_f_u_my_1, paste0(tmp, names(list_female_users_my_1)[i], "_f_u_my_1.rds"))
  }
  rm(no_f_u_my_1)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(female_users_my_1_files)){
    unlink(paste0(tmp,female_users_my_1_files[i]))
  }
  rm(list_female_users_my_1)
} else {no_f_u_my_1<-NULL}
rm(female_users_my_1_files)
#save atc code level 1 in one vector so we can filter the files by atc code
#load one by one by atc and merge them together
#after finishing with one atc, save that in tmp again with name atc_code+no_records_4
#do the same for atc_3

#do the same process for male,female, median users

#####################
#median_males
####################
#atc 4
median_males_4_files<-list.files(tmp,pattern="median_m_4")
if(length(median_males_4_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(median_males_4_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_median_males_4<-vector(mode="list", length=length(letters_atc))
  names(list_median_males_4)<-letters_atc
  for (i in 1:length(list_median_males_4)){
    list_median_males_4[[i]]<-median_males_4_files[substr(median_males_4_files,1,1)==names(list_median_males_4)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_median_males_4)){
    med_m_4<-c()
    if (length(list_median_males_4[[i]])>1){
      med_m_4<-readRDS(paste0(tmp,list_median_males_4[[i]][1]))
      for (z in 2:length(list_median_males_4[[i]])){
        a<-readRDS(paste0(tmp,list_median_males_4[[i]][z]))
        med_m_4<-rbind(med_m_4,a)
        med_m_4<-med_m_4[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "person_id")]
        rm(a)
      }
      med_m_4[,meaning:="All"][,person_id:=NULL]
      med_m_4<-med_m_4[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning, atc_code_4,atc_code_3,atc_code_1)]
      setnames(med_m_4, "count", "median")
      }
    if (length(list_median_males_4[[i]])==1){
      med_m_4<-readRDS(paste0(tmp,list_median_males_4[[i]][1]))
      med_m_4<-med_m_4[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "person_id")]
      med_m_4[,meaning:="All"][,person_id:=NULL]
      med_m_4<-med_m_4[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning, atc_code_4,atc_code_3,atc_code_1)]
      setnames(med_m_4, "count", "median")
       }
    saveRDS(med_m_4, paste0(tmp, names(list_median_males_4)[i], "_median_m_4.rds"))
  }
  rm(med_m_4)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(median_males_4_files)){
    unlink(paste0(tmp,median_males_4_files[i]))
  }
  rm(list_median_males_4)
} else {med_m_4<-NULL}
rm(median_males_4_files)
#atc 3
median_males_3_files<-list.files(tmp,pattern="median_m_3")
if(length(median_males_3_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(median_males_3_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_median_males_3<-vector(mode="list", length=length(letters_atc))
  names(list_median_males_3)<-letters_atc
  for (i in 1:length(list_median_males_3)){
    list_median_males_3[[i]]<-median_males_3_files[substr(median_males_3_files,1,1)==names(list_median_males_3)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_median_males_3)){
    med_m_3<-c()
    if (length(list_median_males_3[[i]])>1){
      med_m_3<-readRDS(paste0(tmp,list_median_males_3[[i]][1]))
      for (z in 2:length(list_median_males_3[[i]])){
        a<-readRDS(paste0(tmp,list_median_males_3[[i]][z]))
        med_m_3<-rbind(med_m_3,a)
        med_m_3<-med_m_3[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "person_id")]
        rm(a)
      }
      med_m_3[,meaning:="All"][,person_id:=NULL]
      med_m_3<-med_m_3[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning, atc_code_4,atc_code_3,atc_code_1)]
      setnames(med_m_3, "count", "median")
    }
    if (length(list_median_males_3[[i]])==1){
      med_m_3<-readRDS(paste0(tmp,list_median_males_3[[i]][1]))
      med_m_3<-med_m_3[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "person_id")]
      med_m_3[,meaning:="All"][,person_id:=NULL]
      med_m_3<-med_m_3[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning, atc_code_4,atc_code_3,atc_code_1)]
      setnames(med_m_3, "count", "median")
    }
    saveRDS(med_m_3, paste0(tmp, names(list_median_males_3)[i], "_median_m_3.rds"))
  }
  rm(med_m_3)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(median_males_3_files)){
    unlink(paste0(tmp,median_males_3_files[i]))
  }
  rm(list_median_males_3)
} else {med_m_3<-NULL}
rm(median_males_3_files)
#atc 1
median_males_1_files<-list.files(tmp,pattern="median_m_1")
if(length(median_males_1_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(median_males_1_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_median_males_1<-vector(mode="list", length=length(letters_atc))
  names(list_median_males_1)<-letters_atc
  for (i in 1:length(list_median_males_1)){
    list_median_males_1[[i]]<-median_males_1_files[substr(median_males_1_files,1,1)==names(list_median_males_1)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_median_males_1)){
    med_m_1<-c()
    if (length(list_median_males_1[[i]])>1){
      med_m_1<-readRDS(paste0(tmp,list_median_males_1[[i]][1]))
      for (z in 2:length(list_median_males_1[[i]])){
        a<-readRDS(paste0(tmp,list_median_males_1[[i]][z]))
        med_m_1<-rbind(med_m_1,a)
        med_m_1<-med_m_1[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "person_id")]
        rm(a)
      }
      med_m_1[,meaning:="All"][,person_id:=NULL]
      med_m_1<-med_m_1[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning, atc_code_4,atc_code_3,atc_code_1)]
      setnames(med_m_1, "count", "median")
    }
    if (length(list_median_males_1[[i]])==1){
      med_m_1<-readRDS(paste0(tmp,list_median_males_1[[i]][1]))
      med_m_1<-med_m_1[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "person_id")]
      med_m_1[,meaning:="All"][,person_id:=NULL]
      med_m_1<-med_m_1[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning, atc_code_4,atc_code_3,atc_code_1)]
      setnames(med_m_1, "count", "median")
    }
    saveRDS(med_m_1, paste0(tmp, names(list_median_males_1)[i], "_median_m_1.rds"))
  }
  rm(med_m_1)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(median_males_1_files)){
    unlink(paste0(tmp,median_males_1_files[i]))
  }
  rm(list_median_males_1)
} else {med_m_1<-NULL}
rm(median_males_1_files)
#####################
#median_females
####################
#atc 4
median_females_4_files<-list.files(tmp,pattern="median_f_4")
if(length(median_females_4_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(median_females_4_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_median_females_4<-vector(mode="list", length=length(letters_atc))
  names(list_median_females_4)<-letters_atc
  for (i in 1:length(list_median_females_4)){
    list_median_females_4[[i]]<-median_females_4_files[substr(median_females_4_files,1,1)==names(list_median_females_4)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_median_females_4)){
    med_f_4<-c()
    if (length(list_median_females_4[[i]])>1){
      med_f_4<-readRDS(paste0(tmp,list_median_females_4[[i]][1]))
      for (z in 2:length(list_median_females_4[[i]])){
        a<-readRDS(paste0(tmp,list_median_females_4[[i]][z]))
        med_f_4<-rbind(med_f_4,a)
        med_f_4<-med_f_4[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "person_id")]
        rm(a)
      }
      med_f_4[,meaning:="All"][,person_id:=NULL]
      med_f_4<-med_f_4[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning, atc_code_4,atc_code_3,atc_code_1)]
      setnames(med_f_4, "count", "median")
    }
    if (length(list_median_females_4[[i]])==1){
      med_f_4<-readRDS(paste0(tmp,list_median_females_4[[i]][1]))
      med_f_4<-med_f_4[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "person_id")]
      med_f_4[,meaning:="All"][,person_id:=NULL]
      med_f_4<-med_f_4[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning, atc_code_4,atc_code_3,atc_code_1)]
      setnames(med_f_4, "count", "median")
    }
    saveRDS(med_f_4, paste0(tmp, names(list_median_females_4)[i], "_median_f_4.rds"))
  }
  rm(med_f_4)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(median_females_4_files)){
    unlink(paste0(tmp,median_females_4_files[i]))
  }
  rm(list_median_females_4)
} else {med_f_4<-NULL}
rm(median_females_4_files)
#atc 3
median_females_3_files<-list.files(tmp,pattern="median_f_3")
if(length(median_females_3_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(median_females_3_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_median_females_3<-vector(mode="list", length=length(letters_atc))
  names(list_median_females_3)<-letters_atc
  for (i in 1:length(list_median_females_3)){
    list_median_females_3[[i]]<-median_females_3_files[substr(median_females_3_files,1,1)==names(list_median_females_3)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_median_females_3)){
    med_f_3<-c()
    if (length(list_median_females_3[[i]])>1){
      med_f_3<-readRDS(paste0(tmp,list_median_females_3[[i]][1]))
      for (z in 2:length(list_median_females_3[[i]])){
        a<-readRDS(paste0(tmp,list_median_females_3[[i]][z]))
        med_f_3<-rbind(med_f_3,a)
        med_f_3<-med_f_3[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "person_id")]
        rm(a)
      }
      med_f_3[,meaning:="All"][,person_id:=NULL]
      med_f_3<-med_f_3[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning, atc_code_4,atc_code_3,atc_code_1)]
      setnames(med_f_3, "count", "median")
    }
    if (length(list_median_females_3[[i]])==1){
      med_f_3<-readRDS(paste0(tmp,list_median_females_3[[i]][1]))
      med_f_3<-med_f_3[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "person_id")]
      med_f_3[,meaning:="All"][,person_id:=NULL]
      med_f_3<-med_f_3[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning, atc_code_4,atc_code_3,atc_code_1)]
      setnames(med_f_3, "count", "median")
    }
    saveRDS(med_f_3, paste0(tmp, names(list_median_females_3)[i], "_median_f_3.rds"))
  }
  rm(med_f_3)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(median_females_3_files)){
    unlink(paste0(tmp,median_females_3_files[i]))
  }
  rm(list_median_females_3)
} else {med_f_3<-NULL}
rm(median_females_3_files)
#atc 1
median_females_1_files<-list.files(tmp,pattern="median_f_1")
if(length(median_females_1_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(median_females_1_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_median_females_1<-vector(mode="list", length=length(letters_atc))
  names(list_median_females_1)<-letters_atc
  for (i in 1:length(list_median_females_1)){
    list_median_females_1[[i]]<-median_females_1_files[substr(median_females_1_files,1,1)==names(list_median_females_1)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_median_females_1)){
    med_f_1<-c()
    if (length(list_median_females_1[[i]])>1){
      med_f_1<-readRDS(paste0(tmp,list_median_females_1[[i]][1]))
      for (z in 2:length(list_median_females_1[[i]])){
        a<-readRDS(paste0(tmp,list_median_females_1[[i]][z]))
        med_f_1<-rbind(med_f_1,a)
        med_f_1<-med_f_1[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "person_id")]
        rm(a)
      }
      med_f_1[,meaning:="All"][,person_id:=NULL]
      med_f_1<-med_f_1[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning, atc_code_4,atc_code_3,atc_code_1)]
      setnames(med_f_1, "count", "median")
    }
    if (length(list_median_females_1[[i]])==1){
      med_f_1<-readRDS(paste0(tmp,list_median_females_1[[i]][1]))
      med_f_1<-med_f_1[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "person_id")]
      med_f_1[,meaning:="All"][,person_id:=NULL]
      med_f_1<-med_f_1[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning, atc_code_4,atc_code_3,atc_code_1)]
      setnames(med_f_1, "count", "median")
    }
    saveRDS(med_f_1, paste0(tmp, names(list_median_females_1)[i], "_median_f_1.rds"))
  }
  rm(med_f_1)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(median_females_1_files)){
    unlink(paste0(tmp,median_females_1_files[i]))
  }
  rm(list_median_females_1)
} else {med_f_1<-NULL}
rm(median_females_1_files)

#####################
#median_males_my
####################
#atc 4
median_mmy_4_files<-list.files(tmp,pattern="median_mmy_4")
if(length(median_mmy_4_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(median_mmy_4_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_median_mmy_4<-vector(mode="list", length=length(letters_atc))
  names(list_median_mmy_4)<-letters_atc
  for (i in 1:length(list_median_mmy_4)){
    list_median_mmy_4[[i]]<-median_mmy_4_files[substr(median_mmy_4_files,1,1)==names(list_median_mmy_4)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_median_mmy_4)){
    med_mmy_4<-c()
    if (length(list_median_mmy_4[[i]])>1){
      med_mmy_4<-readRDS(paste0(tmp,list_median_mmy_4[[i]][1]))
      for (z in 2:length(list_median_mmy_4[[i]])){
        a<-readRDS(paste0(tmp,list_median_mmy_4[[i]][z]))
        med_mmy_4<-rbind(med_mmy_4,a)
        med_mmy_4<-med_mmy_4[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "person_id", "meaning", "year")]
        rm(a)
      }
      med_mmy_4[,meaning:="All"][,person_id:=NULL]
      med_mmy_4<-med_mmy_4[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning,year, atc_code_4,atc_code_3,atc_code_1)]
      setnames(med_mmy_4, "count", "median")
    }
    if (length(list_median_mmy_4[[i]])==1){
      med_mmy_4<-readRDS(paste0(tmp,list_median_mmy_4[[i]][1]))
      med_mmy_4<-med_mmy_4[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "person_id", "meaning", "year")]
      med_mmy_4[,meaning:="All"][,person_id:=NULL]
      med_mmy_4<-med_mmy_4[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning,year, atc_code_4,atc_code_3,atc_code_1)]
      setnames(med_mmy_4, "count", "median")
    }
    saveRDS(med_mmy_4, paste0(tmp, names(list_median_mmy_4)[i], "_med_mmy_4.rds"))
  }
  rm(med_mmy_4)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(median_mmy_4_files)){
    unlink(paste0(tmp,median_mmy_4_files[i]))
  }
  rm(list_median_mmy_4)
} else {med_mmy_4<-NULL}
rm(median_mmy_4_files)

#atc 3
median_mmy_3_files<-list.files(tmp,pattern="median_mmy_3")
if(length(median_mmy_3_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(median_mmy_3_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_median_mmy_3<-vector(mode="list", length=length(letters_atc))
  names(list_median_mmy_3)<-letters_atc
  for (i in 1:length(list_median_mmy_3)){
    list_median_mmy_3[[i]]<-median_mmy_3_files[substr(median_mmy_3_files,1,1)==names(list_median_mmy_3)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_median_mmy_3)){
    med_mmy_3<-c()
    if (length(list_median_mmy_3[[i]])>1){
      med_mmy_3<-readRDS(paste0(tmp,list_median_mmy_3[[i]][1]))
      for (z in 2:length(list_median_mmy_3[[i]])){
        a<-readRDS(paste0(tmp,list_median_mmy_3[[i]][z]))
        med_mmy_3<-rbind(med_mmy_3,a)
        med_mmy_3<-med_mmy_3[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "person_id", "meaning", "year")]
        rm(a)
      }
      med_mmy_3[,meaning:="All"][,person_id:=NULL]
      med_mmy_3<-med_mmy_3[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning,year, atc_code_4,atc_code_3,atc_code_1)]
      setnames(med_mmy_3, "count", "median")
    }
    if (length(list_median_mmy_3[[i]])==1){
      med_mmy_3<-readRDS(paste0(tmp,list_median_mmy_3[[i]][1]))
      med_mmy_3<-med_mmy_3[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "person_id", "meaning", "year")]
      med_mmy_3[,meaning:="All"][,person_id:=NULL]
      med_mmy_3<-med_mmy_3[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning,year, atc_code_4,atc_code_3,atc_code_1)]
      setnames(med_mmy_3, "count", "median")
    }
    saveRDS(med_mmy_3, paste0(tmp, names(list_median_mmy_3)[i], "_med_mmy_3.rds"))
  }
  rm(med_mmy_3)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(median_mmy_3_files)){
    unlink(paste0(tmp,median_mmy_3_files[i]))
  }
  rm(list_median_mmy_3)
} else {med_mmy_3<-NULL}
rm(median_mmy_3_files)

#atc 1
median_mmy_1_files<-list.files(tmp,pattern="median_mmy_1")
if(length(median_mmy_1_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(median_mmy_1_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_median_mmy_1<-vector(mode="list", length=length(letters_atc))
  names(list_median_mmy_1)<-letters_atc
  for (i in 1:length(list_median_mmy_1)){
    list_median_mmy_1[[i]]<-median_mmy_1_files[substr(median_mmy_1_files,1,1)==names(list_median_mmy_1)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_median_mmy_1)){
    med_mmy_1<-c()
    if (length(list_median_mmy_1[[i]])>1){
      med_mmy_1<-readRDS(paste0(tmp,list_median_mmy_1[[i]][1]))
      for (z in 2:length(list_median_mmy_1[[i]])){
        a<-readRDS(paste0(tmp,list_median_mmy_1[[i]][z]))
        med_mmy_1<-rbind(med_mmy_1,a)
        med_mmy_1<-med_mmy_1[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "person_id", "meaning", "year")]
        rm(a)
      }
      med_mmy_1[,meaning:="All"][,person_id:=NULL]
      med_mmy_1<-med_mmy_1[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning,year, atc_code_4,atc_code_3,atc_code_1)]
      setnames(med_mmy_1, "count", "median")
    }
    if (length(list_median_mmy_1[[i]])==1){
      med_mmy_1<-readRDS(paste0(tmp,list_median_mmy_1[[i]][1]))
      med_mmy_1<-med_mmy_1[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "person_id", "meaning", "year")]
      med_mmy_1[,meaning:="All"][,person_id:=NULL]
      med_mmy_1<-med_mmy_1[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning,year, atc_code_4,atc_code_3,atc_code_1)]
      setnames(med_mmy_1, "count", "median")
    }
    saveRDS(med_mmy_1, paste0(tmp, names(list_median_mmy_1)[i], "_med_mmy_1.rds"))
  }
  rm(med_mmy_1)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(median_mmy_1_files)){
    unlink(paste0(tmp,median_mmy_1_files[i]))
  }
  rm(list_median_mmy_1)
} else {med_mmy_1<-NULL}
rm(median_mmy_1_files)

#####################
#median_females_my
####################
#atc 4
median_fmy_4_files<-list.files(tmp,pattern="median_fmy_4")
if(length(median_fmy_4_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(median_fmy_4_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_median_fmy_4<-vector(mode="list", length=length(letters_atc))
  names(list_median_fmy_4)<-letters_atc
  for (i in 1:length(list_median_fmy_4)){
    list_median_fmy_4[[i]]<-median_fmy_4_files[substr(median_fmy_4_files,1,1)==names(list_median_fmy_4)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_median_fmy_4)){
    med_fmy_4<-c()
    if (length(list_median_fmy_4[[i]])>1){
      med_fmy_4<-readRDS(paste0(tmp,list_median_fmy_4[[i]][1]))
      for (z in 2:length(list_median_fmy_4[[i]])){
        a<-readRDS(paste0(tmp,list_median_fmy_4[[i]][z]))
        med_fmy_4<-rbind(med_fmy_4,a)
        med_fmy_4<-med_fmy_4[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "person_id", "meaning", "year")]
        rm(a)
      }
      med_fmy_4[,meaning:="All"][,person_id:=NULL]
      med_fmy_4<-med_fmy_4[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning,year, atc_code_4,atc_code_3,atc_code_1)]
      setnames(med_fmy_4, "count", "median")
    }
    if (length(list_median_fmy_4[[i]])==1){
      med_fmy_4<-readRDS(paste0(tmp,list_median_fmy_4[[i]][1]))
      med_fmy_4<-med_fmy_4[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "person_id", "meaning", "year")]
      med_fmy_4[,meaning:="All"][,person_id:=NULL]
      med_fmy_4<-med_fmy_4[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning,year, atc_code_4,atc_code_3,atc_code_1)]
      setnames(med_fmy_4, "count", "median")
    }
    saveRDS(med_fmy_4, paste0(tmp, names(list_median_fmy_4)[i], "_med_fmy_4.rds"))
  }
  rm(med_fmy_4)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(median_fmy_4_files)){
    unlink(paste0(tmp,median_fmy_4_files[i]))
  }
  rm(list_median_fmy_4)
} else {med_fmy_4<-NULL}
rm(median_fmy_4_files)

#atc 3
median_fmy_3_files<-list.files(tmp,pattern="median_fmy_3")
if(length(median_fmy_3_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(median_fmy_3_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_median_fmy_3<-vector(mode="list", length=length(letters_atc))
  names(list_median_fmy_3)<-letters_atc
  for (i in 1:length(list_median_fmy_3)){
    list_median_fmy_3[[i]]<-median_fmy_3_files[substr(median_fmy_3_files,1,1)==names(list_median_fmy_3)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_median_fmy_3)){
    med_fmy_3<-c()
    if (length(list_median_fmy_3[[i]])>1){
      med_fmy_3<-readRDS(paste0(tmp,list_median_fmy_3[[i]][1]))
      for (z in 2:length(list_median_fmy_3[[i]])){
        a<-readRDS(paste0(tmp,list_median_fmy_3[[i]][z]))
        med_fmy_3<-rbind(med_fmy_3,a)
        med_fmy_3<-med_fmy_3[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "person_id", "meaning", "year")]
        rm(a)
      }
      med_fmy_3[,meaning:="All"][,person_id:=NULL]
      med_fmy_3<-med_fmy_3[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning,year, atc_code_4,atc_code_3,atc_code_1)]
      setnames(med_fmy_3, "count", "median")
    }
    if (length(list_median_fmy_3[[i]])==1){
      med_fmy_3<-readRDS(paste0(tmp,list_median_fmy_3[[i]][1]))
      med_fmy_3<-med_fmy_3[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "person_id", "meaning", "year")]
      med_fmy_3[,meaning:="All"][,person_id:=NULL]
      med_fmy_3<-med_fmy_3[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning,year, atc_code_4,atc_code_3,atc_code_1)]
      setnames(med_fmy_3, "count", "median")
    }
    saveRDS(med_fmy_3, paste0(tmp, names(list_median_fmy_3)[i], "_med_fmy_3.rds"))
  }
  rm(med_fmy_3)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(median_fmy_3_files)){
    unlink(paste0(tmp,median_fmy_3_files[i]))
  }
  rm(list_median_fmy_3)
} else {med_fmy_4<-NULL}
rm(median_fmy_3_files)

#atc 1
median_fmy_1_files<-list.files(tmp,pattern="median_fmy_1")
if(length(median_fmy_1_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(median_fmy_1_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_median_fmy_1<-vector(mode="list", length=length(letters_atc))
  names(list_median_fmy_1)<-letters_atc
  for (i in 1:length(list_median_fmy_1)){
    list_median_fmy_1[[i]]<-median_fmy_1_files[substr(median_fmy_1_files,1,1)==names(list_median_fmy_1)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_median_fmy_1)){
    med_fmy_1<-c()
    if (length(list_median_fmy_1[[i]])>1){
      med_fmy_1<-readRDS(paste0(tmp,list_median_fmy_1[[i]][1]))
      for (z in 2:length(list_median_fmy_1[[i]])){
        a<-readRDS(paste0(tmp,list_median_fmy_1[[i]][z]))
        med_fmy_1<-rbind(med_fmy_1,a)
        med_fmy_1<-med_fmy_1[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "person_id", "meaning", "year")]
        rm(a)
      }
      med_fmy_1[,meaning:="All"][,person_id:=NULL]
      med_fmy_1<-med_fmy_1[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning,year, atc_code_4,atc_code_3,atc_code_1)]
      setnames(med_fmy_1, "count", "median")
    }
    if (length(list_median_fmy_1[[i]])==1){
      med_fmy_1<-readRDS(paste0(tmp,list_median_fmy_1[[i]][1]))
      med_fmy_1<-med_fmy_1[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_4", "atc_code_3", "atc_code_1", "person_id", "meaning", "year")]
      med_fmy_1[,meaning:="All"][,person_id:=NULL]
      med_fmy_1<-med_fmy_1[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning,year, atc_code_4,atc_code_3,atc_code_1)]
      setnames(med_fmy_1, "count", "median")
    }
    saveRDS(med_fmy_1, paste0(tmp, names(list_median_fmy_1)[i], "_med_fmy_1.rds"))
  }
  rm(med_fmy_1)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(median_fmy_1_files)){
    unlink(paste0(tmp,median_fmy_1_files[i]))
  }
  rm(list_median_fmy_1)
} else {med_fmy_4<-NULL}
rm(median_fmy_1_files)


####################
#output files to tmp
####################
#all meanings and years
#no_records:no_records_4,no_records_3,no_records_1
#male_users:m_users_4,m_users_3,m_users_1
#female users:f_users_4,f_users_3,f_users_1
#median male users:median_m_4,median_m_3,median_m_1
#median female users:median_f_4,median_f_3,median_f_1
#stratified by meaning and year
#no_records_my:no_recmy_4,no_recmy_3,no_recmy_1
#male_users_my:m_u_my_4,m_u_my_3,m_u_my_1
#female_users_my:f_u_my_4,f_u_my_3,f_u_my_1
#median male_users_my:med_mmy_4,med_mmy_3,med_mmy_1
#median female_users_my:med_fmy_4,med_fmy_3,med_fmy_1
####################
#Combine in one table
#####################
print("Retrieve information from the temporary folder and merge all together.")
records_total_files<-c(list.files(tmp, pattern = "no_records_4"),list.files(tmp, pattern = "no_records_3"),list.files(tmp, pattern = "no_records_1"))
male_users_total_files<-c(list.files(tmp,pattern = "m_users_4"),list.files(tmp,pattern = "m_users_3"),list.files(tmp,pattern = "m_users_1"))
female_users_total_files<-c(list.files(tmp,pattern = "f_users_4"),list.files(tmp,pattern = "f_users_3"),list.files(tmp,pattern = "f_users_1"))
median_male_users_total_files<-c(list.files(tmp,pattern = "median_m_4"),list.files(tmp,pattern = "median_m_3"),list.files(tmp,pattern = "median_m_1"))
median_female_users_total_files<-c(list.files(tmp,pattern = "median_f_4"),list.files(tmp,pattern = "median_f_3"),list.files(tmp,pattern = "median_f_1"))
records_my_files<-c(list.files(tmp,pattern = "no_recmy_4"),list.files(tmp,pattern = "no_recmy_3"),list.files(tmp,pattern = "no_recmy_1"))
male_users_my_files<-c(list.files(tmp,pattern = "m_u_my_4"),list.files(tmp,pattern = "m_u_my_3"),list.files(tmp,pattern = "m_u_my_1"))
female_users_my_files<-c(list.files(tmp,pattern = "f_u_my_4"),list.files(tmp,pattern = "f_u_my_3"),list.files(tmp,pattern = "f_u_my_1"))
median_male_users_my_files<-c(list.files(tmp,pattern = "med_mmy_4"),list.files(tmp,pattern = "med_mmy_3"),list.files(tmp,pattern = "med_mmy_1"))
median_female_users_my_files<-c(list.files(tmp,pattern = "med_fmy_4"),list.files(tmp,pattern = "med_fmy_3"),list.files(tmp,pattern = "med_fmy_1"))

records_total<-lapply(paste0(tmp,records_total_files), readRDS)
records_total<-do.call(rbind,records_total)
if (records_total[,.N]>0){
records_total[,year:="All"]
setnames(records_total, "count", "no_records")
for(i in 1:length(records_total_files)){
  unlink(paste0(tmp,records_total_files[i]))
}
} else {records_total<-NULL}
rm(records_total_files)
male_users_total<-lapply(paste0(tmp,male_users_total_files), readRDS)
male_users_total<-do.call(rbind,male_users_total)
if (male_users_total[,.N]>0){
male_users_total[,year:="All"]
setnames(male_users_total, "count","no_male_users")
for(i in 1:length(male_users_total_files)){
  unlink(paste0(tmp,male_users_total_files[i]))
}
}else {male_users_total<-NULL}
rm(male_users_total_files)
female_users_total<-lapply(paste0(tmp,female_users_total_files), readRDS)
female_users_total<-do.call(rbind,female_users_total)
if (female_users_total[,.N]>0){
female_users_total[,year:="All"]
setnames(female_users_total, "count","no_female_users")
for(i in 1:length(female_users_total_files)){
  unlink(paste0(tmp,female_users_total_files[i]))
}
} else {
  female_users_total<-NULL
}
rm(female_users_total_files)
median_male_users_total<-lapply(paste0(tmp,median_male_users_total_files), readRDS)
median_male_users_total<-do.call(rbind,median_male_users_total)
if (median_male_users_total[,.N]>0){
median_male_users_total[,year:="All"]
setnames(median_male_users_total, "median","median_rx_male_users")
for(i in 1:length(median_male_users_total_files)){
  unlink(paste0(tmp,median_male_users_total_files[i]))
}
} else {median_male_users_total<-NULL}
rm(median_male_users_total_files)
median_female_users_total<-lapply(paste0(tmp,median_female_users_total_files), readRDS)
median_female_users_total<-do.call(rbind,median_female_users_total)
if (median_female_users_total[,.N]>0){
median_female_users_total[,year:="All"]
setnames(median_female_users_total, "median","median_rx_female_users")
for(i in 1:length(median_female_users_total_files)){
  unlink(paste0(tmp,median_female_users_total_files[i]))
}
} else {median_female_users_total<-NULL}
rm(median_female_users_total_files)
records_my<-lapply(paste0(tmp,records_my_files), readRDS)
records_my<-do.call(rbind,records_my)
if (records_my[,.N]>0){
setnames(records_my, "count", "no_records")
for(i in 1:length(records_my_files)){
  unlink(paste0(tmp,records_my_files[i]))
}
} else {records_my<-NULL}
rm(records_my_files)
male_users_my<-lapply(paste0(tmp,male_users_my_files), readRDS)
male_users_my<-do.call(rbind,male_users_my)
if (male_users_my[,.N]>0){
setnames(male_users_my, "count","no_male_users")
for(i in 1:length(male_users_my_files)){
  unlink(paste0(tmp,male_users_my_files[i]))
}
} else {male_users_my<-NULL}
rm(male_users_my_files)
female_users_my<-lapply(paste0(tmp,female_users_my_files), readRDS)
female_users_my<-do.call(rbind,female_users_my)
if (female_users_my[,.N]>0){
setnames(female_users_my, "count","no_female_users")
for(i in 1:length(female_users_my_files)){
  unlink(paste0(tmp,female_users_my_files[i]))
}
} else {female_users_my<-NULL}
rm(female_users_my_files)
median_male_users_my<-lapply(paste0(tmp,median_male_users_my_files), readRDS)
median_male_users_my<-do.call(rbind,median_male_users_my)
if (median_male_users_my[,.N]>0){
setnames(median_male_users_my, "median","median_rx_male_users")
for(i in 1:length(median_male_users_my_files)){
  unlink(paste0(tmp,median_male_users_my_files[i]))
}
} else {median_male_users_my<-NULL}
rm(median_male_users_my_files)
median_female_users_my<-lapply(paste0(tmp,median_female_users_my_files), readRDS)
median_female_users_my<-do.call(rbind,median_female_users_my)
if (median_female_users_my[,.N]>0){
setnames(median_female_users_my, "median","median_rx_female_users")
for(i in 1:length(median_female_users_my_files)){
  unlink(paste0(tmp,median_female_users_my_files[i]))
}
} else {median_female_users_my<-NULL}
rm(median_female_users_my_files)

totals<-merge(records_total,male_users_total,by=c("meaning", "year","atc_code_4", "atc_code_3","atc_code_1"),all=T)
rm(records_total,male_users_total)
totals<-merge(totals,female_users_total,by=c("meaning", "year","atc_code_4", "atc_code_3","atc_code_1"),all=T)
rm(female_users_total)
totals<-merge(totals,median_male_users_total,by=c("meaning", "year","atc_code_4", "atc_code_3","atc_code_1"),all=T)
rm(median_male_users_total)
totals<-merge(totals,median_female_users_total,by=c("meaning", "year","atc_code_4", "atc_code_3","atc_code_1"),all=T)
rm(median_female_users_total)

print("Create table 12.")
tab12<-merge(records_my,male_users_my,by=c("meaning", "year","atc_code_4", "atc_code_3","atc_code_1"),all=T)
rm(records_my,male_users_my)
tab12<-merge(tab12,female_users_my,by=c("meaning", "year","atc_code_4", "atc_code_3","atc_code_1"),all=T)
rm(female_users_my)
tab12<-merge(tab12,median_male_users_my,by=c("meaning", "year","atc_code_4", "atc_code_3","atc_code_1"),all=T)
rm(median_male_users_my)
tab12<-merge(tab12,median_female_users_my,by=c("meaning", "year","atc_code_4", "atc_code_3","atc_code_1"),all=T)
rm(median_female_users_my)

tab12<-rbind(tab12,totals)
rm(totals)
tab12<-rbind(population_m_tab12,tab12)
tab12[is.na(no_records), no_records:=0][is.na(no_male_users), no_male_users:=0][is.na(no_female_users), no_female_users:=0][is.na(median_rx_male_users), median_rx_male_users:=0][is.na(median_rx_female_users), median_rx_female_users:=0]
setcolorder(tab12,c("meaning", "year", "atc_code_1", "atc_code_3", "atc_code_4", "no_records", "no_male_users", "median_rx_male_users", "no_female_users", "median_rx_female_users"))
setorderv(tab12,c("meaning", "year", "atc_code_1", "atc_code_3", "atc_code_4"))
##############
#export tab12
#############
print("Export table 12.")
saveRDS(tab12,paste0(final,"tab12.rds"))
rm(tab12)
#############
########################################################################################################
#Table 13: Number of prescriptions/dispensings by ATC 7 level in the study population by year of dispensing/prescribing and by meaning_of_drug_record for each ATC class
#######################################################################################################
print("Creating Table 13:  Number of prescriptions/dispensings by ATC 7 level in the study population by year of dispensing/prescribing and by meaning_of_drug_record for each ATC class.")
print("Get all variables.")

#male and female users by meaning
male_users_7_m<-do.call(rbind,male_users_7_m)
if(male_users_7_m[,.N] !=0){
  male_users_7_m<-male_users_7_m[,lapply(.SD,sum), by="meaning_of_drug_record"]
  setnames(male_users_7_m, "meaning_of_drug_record", "meaning")
  setnames(male_users_7_m, "N", "no_male_users")
  male_users_7_m<-cbind(male_users_7_m, atc_code_1="none", atc_code_7="none",no_records="N/A",median_rx_male_users="N/A",median_rx_female_users="N/A")
} else {male_users_7_m<-NULL}
female_users_7_m<-do.call(rbind,female_users_7_m)
if(female_users_7_m[,.N] !=0){
  female_users_7_m<-female_users_7_m[,lapply(.SD,sum), by="meaning_of_drug_record"]
  setnames(female_users_7_m, "meaning_of_drug_record", "meaning")
  setnames(female_users_7_m, "N", "no_female_users")
  female_users_7_m<-cbind(female_users_7_m, atc_code_1="none",atc_code_7="none",no_records="N/A",median_rx_male_users="N/A",median_rx_female_users="N/A")
} else {female_users_7_m<-NULL}

population_m_tab13<-merge(male_users_7_m, female_users_7_m, by=c("meaning", "atc_code_1", "atc_code_7","no_records", "median_rx_male_users", "median_rx_female_users"), all=T, use.names=F)
population_m_tab13[is.na(no_male_users),no_male_users:=0][is.na(no_female_users),no_female_users:=0]
population_m_tab13[,year:="all"]

###############
#no_of_records
###############
#atc_4
no_records_7_files<-list.files(tmp,pattern="no_records_7")
if(length(no_records_7_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(no_records_7_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_records_7<-vector(mode="list", length=length(letters_atc))
  names(list_records_7)<-letters_atc
  for (i in 1:length(list_records_7)){
    list_records_7[[i]]<-no_records_7_files[substr(no_records_7_files,1,1)==names(list_records_7)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_records_7)){
    no_records_7<-c()
    if (length(list_records_7[[i]])>1){
      no_records_7<-readRDS(paste0(tmp,list_records_7[[i]][1]))
      for (z in 2:length(list_records_7[[i]])){
        a<-readRDS(paste0(tmp,list_records_7[[i]][z]))
        no_records_7<-rbind(no_records_7,a)
        no_records_7<-no_records_7[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_7", "atc_code_1", "meaning")]
        rm(a)
      }
    }
    if (length(list_records_7[[i]])==1){
      no_records_7<-readRDS(paste0(tmp,list_records_7[[i]][1]))
      no_records_7<-no_records_7[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_7", "atc_code_1", "meaning")]
    }
    saveRDS(no_records_7, paste0(tmp, names(list_records_7)[i], "_nr_rec_7.rds"))
    
  }
  rm(no_records_7)
  #remove all files that match pattern letter+no_records_7
  for(i in 1:length(no_records_7_files)){
    unlink(paste0(tmp,no_records_7_files[i]))
  }
  rm(list_records_7)
} else {no_records_7<-NULL}
rm(no_records_7_files)
###############
#male_users
###############
#atc_4
male_users_7_files<-list.files(tmp,pattern="m_users_7")
if(length(male_users_7_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(male_users_7_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_males_7<-vector(mode="list", length=length(letters_atc))
  names(list_males_7)<-letters_atc
  for (i in 1:length(list_males_7)){
    list_males_7[[i]]<-male_users_7_files[substr(male_users_7_files,1,1)==names(list_males_7)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_males_7)){
    male_users_7<-c()
    if (length(list_males_7[[i]])>1){
      male_users_7<-readRDS(paste0(tmp,list_males_7[[i]][1]))
      for (z in 2:length(list_males_7[[i]])){
        a<-readRDS(paste0(tmp,list_males_7[[i]][z]))
        male_users_7<-rbind(male_users_7,a)
        male_users_7<-male_users_7[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_7", "atc_code_1", "meaning")]
        rm(a)
      }
    }
    if (length(list_males_7[[i]])==1){
      male_users_7<-readRDS(paste0(tmp,list_males_7[[i]][1]))
      male_users_7<-male_users_7[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_7", "atc_code_1", "meaning")]
    }
    saveRDS(male_users_7, paste0(tmp, names(list_males_7)[i], "_mu_7.rds"))
  }
  
  #remove all files that match pattern letter+male_users_4
  for(i in 1:length(male_users_7_files)){
    unlink(paste0(tmp,male_users_7_files[i]))
  }
  rm(list_males_7)
} else {male_users_7<-NULL}
rm(male_users_7_files)
###############
#female_users
###############
#atc_4
female_users_7_files<-list.files(tmp,pattern="f_users_7")
if(length(female_users_7_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(female_users_7_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_females_7<-vector(mode="list", length=length(letters_atc))
  names(list_females_7)<-letters_atc
  for (i in 1:length(list_females_7)){
    list_females_7[[i]]<-female_users_7_files[substr(female_users_7_files,1,1)==names(list_females_7)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_females_7)){
    female_users_7<-c()
    if (length(list_females_7[[i]])>1){
      female_users_7<-readRDS(paste0(tmp,list_females_7[[i]][1]))
      for (z in 2:length(list_females_7[[i]])){
        a<-readRDS(paste0(tmp,list_females_7[[i]][z]))
        female_users_7<-rbind(female_users_7,a)
        female_users_7<-female_users_7[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_7", "atc_code_1", "meaning")]
        rm(a)
      }
    }
    if (length(list_females_7[[i]])==1){
      female_users_7<-readRDS(paste0(tmp,list_females_7[[i]][1]))
      female_users_7<-female_users_7[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_7", "atc_code_1", "meaning")]
    }
    saveRDS(female_users_7, paste0(tmp, names(list_females_7)[i], "_fu_7.rds"))
  }
  
  #remove all files that match pattern letter+male_users_4
  for(i in 1:length(female_users_7_files)){
    unlink(paste0(tmp,female_users_7_files[i]))
  }
  rm(list_females_7)
} else {female_users_7<-NULL}
rm(female_users_7_files)



###############
#no_of_records_my
###############
#atc_7
no_records_my_7_files<-list.files(tmp,pattern="no_recmy_7")
if(length(no_records_my_7_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(no_records_my_7_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_records_my_7<-vector(mode="list", length=length(letters_atc))
  names(list_records_my_7)<-letters_atc
  for (i in 1:length(list_records_my_7)){
    list_records_my_7[[i]]<-no_records_my_7_files[substr(no_records_my_7_files,1,1)==names(list_records_my_7)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_records_my_7)){
    no_records_my_7<-c()
    if (length(list_records_my_7[[i]])>1){
      no_records_my_7<-readRDS(paste0(tmp,list_records_my_7[[i]][1]))
      for (z in 2:length(list_records_my_7[[i]])){
        a<-readRDS(paste0(tmp,list_records_my_7[[i]][z]))
        no_records_my_7<-rbind(no_records_my_7,a)
        no_records_my_7<-no_records_my_7[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_7", "atc_code_1", "meaning", "year")]
        rm(a)
      }
    }
    if (length(list_records_my_7[[i]])==1){
      no_records_my_7<-readRDS(paste0(tmp,list_records_my_7[[i]][1]))
      no_records_my_7<-no_records_my_7[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_7", "atc_code_1", "meaning", "year")]
    }
    saveRDS(no_records_my_7, paste0(tmp, names(list_records_my_7)[i], "_no_rcmy_7.rds"))
  }
  rm(no_records_my_7)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(no_records_my_7_files)){
    unlink(paste0(tmp,no_records_my_7_files[i]))
  }
  rm(list_records_my_7)
} else {no_records_my_7<-NULL}
rm(no_records_my_7_files)

#####################
#male_users_my
####################
#atc 7
male_users_my_7_files<-list.files(tmp,pattern="m_u_my_7")
if(length(male_users_my_7_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(male_users_my_7_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_male_users_my_7<-vector(mode="list", length=length(letters_atc))
  names(list_male_users_my_7)<-letters_atc
  for (i in 1:length(list_male_users_my_7)){
    list_male_users_my_7[[i]]<-male_users_my_7_files[substr(male_users_my_7_files,1,1)==names(list_male_users_my_7)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_male_users_my_7)){
    no_m_u_my_7<-c()
    if (length(list_male_users_my_7[[i]])>1){
      no_m_u_my_7<-readRDS(paste0(tmp,list_male_users_my_7[[i]][1]))
      for (z in 2:length(list_male_users_my_7[[i]])){
        a<-readRDS(paste0(tmp,list_male_users_my_7[[i]][z]))
        no_m_u_my_7<-rbind(no_m_u_my_7,a)
        no_m_u_my_7<-no_m_u_my_7[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_7", "atc_code_1", "meaning", "year")]
        rm(a)
      }
    }
    if (length(list_male_users_my_7[[i]])==1){
      no_m_u_my_7<-readRDS(paste0(tmp,list_male_users_my_7[[i]][1]))
      no_m_u_my_7<-no_m_u_my_7[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_7", "atc_code_1", "meaning", "year")]
    }
    saveRDS(no_m_u_my_7, paste0(tmp, names(list_male_users_my_7)[i], "_mumy_7.rds"))
  }
  rm(no_m_u_my_7)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(male_users_my_7_files)){
    unlink(paste0(tmp,male_users_my_7_files[i]))
  }
  rm(list_male_users_my_7)
} else {no_m_u_my_7<-NULL}
rm(male_users_my_7_files)

#####################
#female_users_my
####################
#atc 7
female_users_my_7_files<-list.files(tmp,pattern="f_u_my_7")
if(length(female_users_my_7_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(female_users_my_7_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_female_users_my_7<-vector(mode="list", length=length(letters_atc))
  names(list_female_users_my_7)<-letters_atc
  for (i in 1:length(list_female_users_my_7)){
    list_female_users_my_7[[i]]<-female_users_my_7_files[substr(female_users_my_7_files,1,1)==names(list_female_users_my_7)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_female_users_my_7)){
    no_f_u_my_7<-c()
    if (length(list_female_users_my_7[[i]])>1){
      no_f_u_my_7<-readRDS(paste0(tmp,list_female_users_my_7[[i]][1]))
      for (z in 2:length(list_female_users_my_7[[i]])){
        a<-readRDS(paste0(tmp,list_female_users_my_7[[i]][z]))
        no_f_u_my_7<-rbind(no_f_u_my_7,a)
        no_f_u_my_7<-no_f_u_my_7[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_7", "atc_code_1", "meaning", "year")]
        rm(a)
      }
    }
    if (length(list_female_users_my_7[[i]])==1){
      no_f_u_my_7<-readRDS(paste0(tmp,list_female_users_my_7[[i]][1]))
      no_f_u_my_7<-no_f_u_my_7[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_7", "atc_code_1", "meaning", "year")]
    }
    saveRDS(no_f_u_my_7, paste0(tmp, names(list_female_users_my_7)[i], "_fumy_7.rds"))
  }
  rm(no_f_u_my_7)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(female_users_my_7_files)){
    unlink(paste0(tmp,female_users_my_7_files[i]))
  }
  rm(list_female_users_my_7)
} else {no_f_u_my_7<-NULL}
rm(female_users_my_7_files)

#####################
#median_males
####################
#atc 4
median_males_7_files<-list.files(tmp,pattern="median_m_7")
if(length(median_males_7_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(median_males_7_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_median_males_7<-vector(mode="list", length=length(letters_atc))
  names(list_median_males_7)<-letters_atc
  for (i in 1:length(list_median_males_7)){
    list_median_males_7[[i]]<-median_males_7_files[substr(median_males_7_files,1,1)==names(list_median_males_7)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_median_males_7)){
    med_m_7<-c()
    if (length(list_median_males_7[[i]])>1){
      med_m_7<-readRDS(paste0(tmp,list_median_males_7[[i]][1]))
      for (z in 2:length(list_median_males_7[[i]])){
        a<-readRDS(paste0(tmp,list_median_males_7[[i]][z]))
        med_m_7<-rbind(med_m_7,a)
        med_m_7<-med_m_7[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_7", "atc_code_1", "person_id")]
        rm(a)
      }
      med_m_7[,meaning:="All"][,person_id:=NULL]
      med_m_7<-med_m_7[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning, atc_code_7,atc_code_1)]
      setnames(med_m_7, "count", "median")
    }
    if (length(list_median_males_7[[i]])==1){
      med_m_7<-readRDS(paste0(tmp,list_median_males_7[[i]][1]))
      med_m_7<-med_m_7[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_7", "atc_code_1", "person_id")]
      med_m_7[,meaning:="All"][,person_id:=NULL]
      med_m_7<-med_m_7[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning, atc_code_7,atc_code_1)]
      setnames(med_m_7, "count", "median")
    }
    saveRDS(med_m_7, paste0(tmp, names(list_median_males_7)[i], "_med_m_7.rds"))
  }
  rm(med_m_7)
  #remove all files that match pattern letter+no_records_7
  for(i in 1:length(median_males_7_files)){
    unlink(paste0(tmp,median_males_7_files[i]))
  }
  rm(list_median_males_7)
} else {med_m_7<-NULL}
rm(median_males_7_files)

#####################
#median_females
####################
#atc 4
median_females_7_files<-list.files(tmp,pattern="median_f_7")
if(length(median_females_7_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(median_females_7_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_median_females_7<-vector(mode="list", length=length(letters_atc))
  names(list_median_females_7)<-letters_atc
  for (i in 1:length(list_median_females_7)){
    list_median_females_7[[i]]<-median_females_7_files[substr(median_females_7_files,1,1)==names(list_median_females_7)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_median_females_7)){
    med_f_7<-c()
    if (length(list_median_females_7[[i]])>1){
      med_f_7<-readRDS(paste0(tmp,list_median_females_7[[i]][1]))
      for (z in 2:length(list_median_females_7[[i]])){
        a<-readRDS(paste0(tmp,list_median_females_7[[i]][z]))
        med_f_7<-rbind(med_f_7,a)
        med_f_7<-med_f_7[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_7", "atc_code_1", "person_id")]
        rm(a)
      }
      med_f_7[,meaning:="All"][,person_id:=NULL]
      med_f_7<-med_f_7[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning, atc_code_7,atc_code_1)]
      setnames(med_f_7, "count", "median")
    }
    if (length(list_median_females_7[[i]])==1){
      med_f_7<-readRDS(paste0(tmp,list_median_females_7[[i]][1]))
      med_f_7<-med_f_7[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_7", "atc_code_1", "person_id")]
      med_f_7[,meaning:="All"][,person_id:=NULL]
      med_f_7<-med_f_7[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning, atc_code_7,atc_code_1)]
      setnames(med_f_7, "count", "median")
    }
    saveRDS(med_f_7, paste0(tmp, names(list_median_females_7)[i], "_med_f_7.rds"))
  }
  rm(med_f_7)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(median_females_7_files)){
    unlink(paste0(tmp,median_females_7_files[i]))
  }
  rm(list_median_females_7)
} else {med_f_7<-NULL}
rm(median_females_7_files)

#####################
#median_males_my
####################
#atc 7
median_mmy_7_files<-list.files(tmp,pattern="median_mmy_7")
if(length(median_mmy_7_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(median_mmy_7_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_median_mmy_7<-vector(mode="list", length=length(letters_atc))
  names(list_median_mmy_7)<-letters_atc
  for (i in 1:length(list_median_mmy_7)){
    list_median_mmy_7[[i]]<-median_mmy_7_files[substr(median_mmy_7_files,1,1)==names(list_median_mmy_7)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_median_mmy_7)){
    med_mmy_7<-c()
    if (length(list_median_mmy_7[[i]])>1){
      med_mmy_7<-readRDS(paste0(tmp,list_median_mmy_7[[i]][1]))
      for (z in 2:length(list_median_mmy_7[[i]])){
        a<-readRDS(paste0(tmp,list_median_mmy_7[[i]][z]))
        med_mmy_7<-rbind(med_mmy_7,a)
        med_mmy_7<-med_mmy_7[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_7", "atc_code_1", "person_id", "meaning", "year")]
        rm(a)
      }
      med_mmy_7[,meaning:="All"][,person_id:=NULL]
      med_mmy_7<-med_mmy_7[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning,year, atc_code_7,atc_code_1)]
      setnames(med_mmy_7, "count", "median")
    }
    if (length(list_median_mmy_7[[i]])==1){
      med_mmy_7<-readRDS(paste0(tmp,list_median_mmy_7[[i]][1]))
      med_mmy_7<-med_mmy_7[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_7", "atc_code_1", "person_id", "meaning", "year")]
      med_mmy_7[,meaning:="All"][,person_id:=NULL]
      med_mmy_7<-med_mmy_7[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning,year, atc_code_7,atc_code_1)]
      setnames(med_mmy_7, "count", "median")
    }
    saveRDS(med_mmy_7, paste0(tmp, names(list_median_mmy_7)[i], "_medmmy_7.rds"))
  }
  rm(med_mmy_7)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(median_mmy_7_files)){
    unlink(paste0(tmp,median_mmy_7_files[i]))
  }
  rm(list_median_mmy_7)
} else {med_mmy_7<-NULL}
rm(median_mmy_7_files)

#####################
#median_females_my
####################
#atc 4
median_fmy_7_files<-list.files(tmp,pattern="median_fmy_7")
if(length(median_fmy_7_files)>0){
  #grab only the first letter which symoblized the ATC level 1
  letters_atc<-unique(sapply(median_fmy_7_files, function(x) substr(x,1,1))) 
  #create list for each letter
  list_median_fmy_7<-vector(mode="list", length=length(letters_atc))
  names(list_median_fmy_7)<-letters_atc
  for (i in 1:length(list_median_fmy_7)){
    list_median_fmy_7[[i]]<-median_fmy_7_files[substr(median_fmy_7_files,1,1)==names(list_median_fmy_7)[i]]
  }
  rm(letters_atc)
  #merge by atc code
  for(i in 1 :length(list_median_fmy_7)){
    med_fmy_7<-c()
    if (length(list_median_fmy_7[[i]])>1){
      med_fmy_7<-readRDS(paste0(tmp,list_median_fmy_7[[i]][1]))
      for (z in 2:length(list_median_fmy_7[[i]])){
        a<-readRDS(paste0(tmp,list_median_fmy_7[[i]][z]))
        med_fmy_7<-rbind(med_fmy_7,a)
        med_fmy_7<-med_fmy_7[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_7", "atc_code_1", "person_id", "meaning", "year")]
        rm(a)
      }
      med_fmy_7[,meaning:="All"][,person_id:=NULL]
      med_fmy_7<-med_fmy_7[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning,year, atc_code_7,atc_code_1)]
      setnames(med_fmy_7, "count", "median")
    }
    if (length(list_median_fmy_7[[i]])==1){
      med_fmy_7<-readRDS(paste0(tmp,list_median_fmy_7[[i]][1]))
      med_fmy_7<-med_fmy_7[,lapply(.SD,sum), .SDcols="count", by=c("atc_code_7", "atc_code_1", "person_id", "meaning", "year")]
      med_fmy_7[,meaning:="All"][,person_id:=NULL]
      med_fmy_7<-med_fmy_7[, lapply(.SD, median), .SDcols=c("count"), by=.(meaning,year, atc_code_7,atc_code_1)]
      setnames(med_fmy_7, "count", "median")
    }
    saveRDS(med_fmy_7, paste0(tmp, names(list_median_fmy_7)[i], "_medfmy_7.rds"))
  }
  rm(med_fmy_7)
  #remove all files that match pattern letter+no_records_4
  for(i in 1:length(median_fmy_7_files)){
    unlink(paste0(tmp,median_fmy_7_files[i]))
  }
  rm(list_median_fmy_7)
} else {med_fmy_7<-NULL}
rm(median_fmy_7_files)

####################
#output files to tmp
####################
#all meanings and years
#no_records:nr_rec_7
#male_users:mu_7
#female users:fu_7
#median male users:med_m_7
#median female users:med_f_7
#stratified by meaning and year
#no_records_my:no_rcmy_7
#male_users_my:mumy_7
#female_users_my:fumy_7
#median male_users_my:medmmy_7
#median female_users_my:medfmy_7

####################
#Combine in one table
#####################
print("Retrieve information from the temporary folder and merge all together.")
records_total_files_7<-list.files(tmp, pattern = "nr_rec_7")
male_users_total_files_7<-list.files(tmp,pattern = "mu_7")
female_users_total_files_7<-list.files(tmp,pattern = "fu_7")
median_male_users_total_files_7<-list.files(tmp,pattern = "med_m_7")
median_female_users_total_files_7<-list.files(tmp,pattern = "med_f_7")
records_my_files_7<-list.files(tmp,pattern = "no_rcmy_7")
male_users_my_files_7<-list.files(tmp,pattern = "mumy_7")
female_users_my_files_7<-list.files(tmp,pattern = "fumy_7")
median_male_users_my_files_7<-list.files(tmp,pattern = "medmmy")
median_female_users_my_files_7<-list.files(tmp,pattern = "medfmy")

records_total_7<-lapply(paste0(tmp,records_total_files_7), readRDS)
records_total_7<-do.call(rbind,records_total_7)
if (records_total_7[,.N]>0){
  records_total_7[,year:="All"]
  setnames(records_total_7, "count", "no_records")
  for(i in 1:length(records_total_files_7)){
    unlink(paste0(tmp,records_total_files_7[i]))
  }
} else {records_total_7<-NULL}
rm(records_total_files_7)
male_users_total_7<-lapply(paste0(tmp,male_users_total_files_7), readRDS)
male_users_total_7<-do.call(rbind,male_users_total_7)
if (male_users_total_7[,.N]>0){
  male_users_total_7[,year:="All"]
  setnames(male_users_total_7, "count","no_male_users")
  for(i in 1:length(male_users_total_files_7)){
    unlink(paste0(tmp,male_users_total_files_7[i]))
  }
}else {male_users_total_7<-NULL}
rm(male_users_total_files_7)
female_users_total_7<-lapply(paste0(tmp,female_users_total_files_7), readRDS)
female_users_total_7<-do.call(rbind,female_users_total_7)
if (female_users_total_7[,.N]>0){
  female_users_total_7[,year:="All"]
  setnames(female_users_total_7, "count","no_female_users")
  for(i in 1:length(female_users_total_files_7)){
    unlink(paste0(tmp,female_users_total_files_7[i]))
  }
} else {
  female_users_total_7<-NULL
}
rm(female_users_total_files_7)
median_male_users_total_7<-lapply(paste0(tmp,median_male_users_total_files_7), readRDS)
median_male_users_total_7<-do.call(rbind,median_male_users_total_7)
if (median_male_users_total_7[,.N]>0){
  median_male_users_total_7[,year:="All"]
  setnames(median_male_users_total_7, "median","median_rx_male_users")
  for(i in 1:length(median_male_users_total_files_7)){
    unlink(paste0(tmp,median_male_users_total_files_7[i]))
  }
} else {median_male_users_total_7<-NULL}
rm(median_male_users_total_files_7)
median_female_users_total_7<-lapply(paste0(tmp,median_female_users_total_files_7), readRDS)
median_female_users_total_7<-do.call(rbind,median_female_users_total_7)
if (median_female_users_total_7[,.N]>0){
  median_female_users_total_7[,year:="All"]
  setnames(median_female_users_total_7, "median","median_rx_female_users")
  for(i in 1:length(median_female_users_total_files_7)){
    unlink(paste0(tmp,median_female_users_total_files_7[i]))
  }
} else {median_female_users_total_7<-NULL}
rm(median_female_users_total_files_7)
records_my_7<-lapply(paste0(tmp,records_my_files_7), readRDS)
records_my_7<-do.call(rbind,records_my_7)
if (records_my_7[,.N]>0){
  setnames(records_my_7, "count", "no_records")
  for(i in 1:length(records_my_files_7)){
    unlink(paste0(tmp,records_my_files_7[i]))
  }
} else {records_my_7<-NULL}
rm(records_my_files_7)
male_users_my_7<-lapply(paste0(tmp,male_users_my_files_7), readRDS)
male_users_my_7<-do.call(rbind,male_users_my_7)
if (male_users_my_7[,.N]>0){
  setnames(male_users_my_7, "count","no_male_users")
  for(i in 1:length(male_users_my_files_7)){
    unlink(paste0(tmp,male_users_my_files_7[i]))
  }
} else {male_users_my_7<-NULL}
rm(male_users_my_files_7)
female_users_my_7<-lapply(paste0(tmp,female_users_my_files_7), readRDS)
female_users_my_7<-do.call(rbind,female_users_my_7)
if (female_users_my_7[,.N]>0){
  setnames(female_users_my_7, "count","no_female_users")
  for(i in 1:length(female_users_my_files_7)){
    unlink(paste0(tmp,female_users_my_files_7[i]))
  }
} else {female_users_my_7<-NULL}
rm(female_users_my_files_7)
median_male_users_my_7<-lapply(paste0(tmp,median_male_users_my_files_7), readRDS)
median_male_users_my_7<-do.call(rbind,median_male_users_my_7)
if (median_male_users_my_7[,.N]>0){
  setnames(median_male_users_my_7, "median","median_rx_male_users")
  for(i in 1:length(median_male_users_my_files_7)){
    unlink(paste0(tmp,median_male_users_my_files_7[i]))
  }
} else {median_male_users_my_7<-NULL}
rm(median_male_users_my_files_7)
median_female_users_my_7<-lapply(paste0(tmp,median_female_users_my_files_7), readRDS)
median_female_users_my_7<-do.call(rbind,median_female_users_my_7)
if (median_female_users_my_7[,.N]>0){
  setnames(median_female_users_my_7, "median","median_rx_female_users")
  for(i in 1:length(median_female_users_my_files_7)){
    unlink(paste0(tmp,median_female_users_my_files_7[i]))
  }
} else {median_female_users_my_7<-NULL}
rm(median_female_users_my_files_7)

totals_7<-merge(records_total_7,male_users_total_7,by=c("meaning", "year","atc_code_7", "atc_code_1"),all=T)
rm(records_total_7,male_users_total_7)
totals_7<-merge(totals_7,female_users_total_7,by=c("meaning", "year","atc_code_7", "atc_code_1"),all=T)
rm(female_users_total_7)
totals_7<-merge(totals_7,median_male_users_total_7,by=c("meaning", "year","atc_code_7", "atc_code_1"),all=T)
rm(median_male_users_total_7)
totals_7<-merge(totals_7,median_female_users_total_7,by=c("meaning", "year","atc_code_7", "atc_code_1"),all=T)
rm(median_female_users_total_7)

print("Create table 13.")
tab13<-merge(records_my_7,male_users_my_7,by=c("meaning", "year","atc_code_7", "atc_code_1"),all=T)
rm(records_my_7,male_users_my_7)
tab13<-merge(tab13,female_users_my_7,by=c("meaning", "year","atc_code_7", "atc_code_1"),all=T)
rm(female_users_my_7)
tab13<-merge(tab13,median_male_users_my_7,by=c("meaning", "year","atc_code_7", "atc_code_1"),all=T)
rm(median_male_users_my_7)
tab13<-merge(tab13,median_female_users_my_7,by=c("meaning", "year","atc_code_7", "atc_code_1"),all=T)
rm(median_female_users_my_7)

tab13<-rbind(tab13,totals_7)
rm(totals_7)
tab13<-rbind(population_m_tab13,tab13)
tab13[is.na(no_records), no_records:=0][is.na(no_male_users), no_male_users:=0][is.na(no_female_users), no_female_users:=0][is.na(median_rx_male_users), median_rx_male_users:=0][is.na(median_rx_female_users), median_rx_female_users:=0][,atc_code_3:=substr(atc_code_7,1,3)][,atc_code_1:=NULL]
tab13[atc_code_3=="non",atc_code_3:="none"]
setcolorder(tab13,c("meaning", "year", "atc_code_3", "atc_code_7", "no_records", "no_male_users", "median_rx_male_users", "no_female_users", "median_rx_female_users"))
setorderv(tab13,c("meaning", "year", "atc_code_3", "atc_code_7"))

##############
#export tab13
#############
print("Export table 13.")
saveRDS(tab13,paste0(final,"tab13.rds"))
rm(tab13)
#############

}




