if(length(actual_tables$VACCINES)>0){
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
  #number of vaccines records outside the observation period(check is done on individual level) (flowchart 5)
  print("Get number of records outside observation period.")
  vx_out_st_per<-do.call(rbind,vx_out_st_per) 
  vx_out_st_per<-sum(vx_out_st_per)
  #number of records in the study population with date admin/record inside study period (flowchart 6)
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
                                    "Exclude: Number of records with both date administration and date record missing",
                                    "Exclude: Number of records with date admin/record outside study period",
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
  #number of subjects in the study population that do not have a record/administration
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
    
    study_population[person_id %in% stdpop_not_vx[,person_id],no_vaccines:=1]
    
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
                                      "Number of subjects without administration/prescriptions in the study population",
                                      "Number of records with empty ATC codes when date_admin/record is present",
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
  #############################################################################################
  #Table 15: Number of administrations/record with incomplete data
  #############################################################################################
  print("Creating Table 15: Number of administrations/record with incomplete data.")
  print("Get all variables.")
  #stratified by meaning
  no_type_m<-do.call(rbind,no_type_m)
  no_type_m<-no_type_m[,lapply(.SD, sum), .SDcols="N", by="meaning"]
  names(no_type_m)<-c("meaning", "count")
  no_dose_m<-do.call(rbind,no_dose_m)
  no_dose_m<-no_dose_m[,lapply(.SD, sum), .SDcols="N", by="meaning"]
  names(no_dose_m)<-c("meaning", "count")
  no_manufacturer_num<-do.call(rbind,no_manufacturer_num)
  no_manufacturer_num<-no_manufacturer_num[,lapply(.SD, sum), .SDcols="N", by="meaning"]
  names(no_manufacturer_num)<-c("meaning", "count")

  #total number of records
  no_type_t<-do.call(rbind,no_type_t)
  no_type_t<-sum(no_type_t)
  no_dose_t<-do.call(rbind,no_dose_t)
  no_dose_t<-sum(no_dose_t)
  no_manufacturer_t<-do.call(rbind,no_manufacturer_t)
  no_manufacturer_t<-sum(no_manufacturer_t)

  print("Combine counts.")
  if(no_type_m[,.N]==0){
    type_info<-data.table(meaning=meanings, count=0)
  } else {
    if (length(meanings[meanings %!in% no_type_m[["meaning"]]])>0){
      type_info<-rbind(data.table(meaning=meanings[meanings %!in% no_type_m[["meaning"]]], count=0),no_type_m)
    } else {
      type_info<-no_type_m
    }
  }
  rm(no_type_m)
  
  if(no_dose_m[,.N]==0){
    dose_info<-data.table(meaning=meanings, count=0)
  } else {
    if (length(meanings[meanings %!in% no_dose_m[["meaning"]]])>0){
      dose_info<-rbind(data.table(meaning=meanings[meanings %!in% no_dose_m[["meaning"]]], count=0),no_dose_m)
    } else {
      dose_info<-no_dose_m
    }
  }
  rm(no_dose_m)
  
  if(no_manufacturer_num[,.N]==0){
    manufacturer_info<-data.table(meaning=meanings, count=0)
  } else {
    if(length(meanings[meanings %!in% no_manufacturer_num[["meaning"]]])>0){
      manufacturer_info<-rbind(data.table(meaning=meanings[meanings %!in% no_manufacturer_num[["meaning"]]], count=0),no_manufacturer_num)
    } else {
      manufacturer_info<-no_manufacturer_num
    }
  }
  rm(no_manufacturer_num)
  

  #number of records in the study population by meaning
  vx_study_population_meaning<-do.call(rbind,vx_study_population_meaning)
  vx_study_population_meaning<-vx_study_population_meaning[,lapply(.SD, sum), .SDcols="N", by="meaning"]
  names(vx_study_population_meaning)<-c("meaning", "count")
  if(vx_study_population_meaning[,.N]==0){
    vx_study_population_meaning_info<-data.table(meaning=meanings, count=0)
  } else {
    if(length(meanings[meanings %!in% vx_study_population_meaning[["meaning"]]])>0){
      vx_study_population_meaning_info<-rbind(data.table(meaning=meanings[meanings %!in% vx_study_population_meaning[["meaning"]]], count=0),vx_study_population_meaning)
    } else {
      vx_study_population_meaning_info<-vx_study_population_meaning
    }
  }
  meaning_info<-data.table(meaning=meanings)
  
  print("Create table 15.")
  #combine all together
  meaning_info[,meaning:=as.character(meaning)]
  vx_study_population_meaning_info[,meaning:=as.character(meaning)]
  tab15<-as.data.table(merge(meaning_info,vx_study_population_meaning_info,by="meaning"))
  setnames(tab15, "count", "records_study_population")   
  tab15[,meaning:=as.character(meaning)]
  type_info[,meaning:=as.character(meaning)]
  tab15<-as.data.table(merge(tab15, type_info, by="meaning")) 
  setnames(tab15, "count", "records_no_vx_type") 
  tab15[,meaning:=as.character(meaning)]
  dose_info[,meaning:=as.character(meaning)]
  tab15<-as.data.table(merge(tab15, dose_info, by="meaning"))    
  setnames(tab15, "count", "records_no_vx_dose") 
  tab15[,meaning:=as.character(meaning)]
  manufacturer_info[,meaning:=as.character(meaning)]
  tab15<-as.data.table(merge(tab15, manufacturer_info, by="meaning"))    
  setnames(tab15, "count", "records_no_vx_manufacturer") 
  tab15<-rbind(tab15, data.table(meaning="All", 
                                 records_study_population=vx_study_population,
                                 records_no_vx_type=no_type_t,
                                 records_no_vx_dose=no_dose_t,
                                 records_no_vx_manufacturer=no_manufacturer_t))
  rm(no_type_t,no_dose_t,no_manufacturer_t)
  
  print("Export table 15.")
  
  if(!is.null(tab15)){
    tab15<-data.table(tab15, data_access_provider= data_access_provider_name, data_source=data_source_name)
    if(subpopulations_present=="Yes"){
      write.csv(tab15, paste0(vacc_dir,subpopulations_names[s], "/", subpopulations_names[s],"_vaccines_completeness.csv"), row.names = F)
    } else {
      write.csv(tab15, paste0(vacc_dir,"vaccines_completeness.csv"), row.names = F)
    }
  }
  
  #Apply masking
  if(!is.null(tab15)){
    tab15[, records_study_population:= as.character(records_study_population)][as.numeric(records_study_population) > 0 & as.numeric(records_study_population) < 5, records_study_population := "<5"]
    tab15[, records_no_vx_type:= as.character(records_no_vx_type)][as.numeric(records_no_vx_type) > 0 & as.numeric(records_no_vx_type) < 5, records_no_vx_type := "<5"]
    tab15[, records_no_vx_dose:= as.character(records_no_vx_dose)][as.numeric(records_no_vx_dose) > 0 & as.numeric(records_no_vx_dose) < 5, records_no_vx_dose := "<5"]
    tab15[, records_no_vx_manufacturer:= as.character(records_no_vx_manufacturer)][as.numeric(records_no_vx_manufacturer) > 0 & as.numeric(records_no_vx_manufacturer) < 5, records_no_vx_manufacturer := "<5"]

    if (subpopulations_present=="Yes"){
      write.csv(tab15, paste0(vacc_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_vaccines_completeness_masked.csv"), row.names = F)
    } else {
      write.csv(tab15, paste0(vacc_dir,"Masked/","vaccines_completeness_masked.csv"), row.names = F)
    }
  }
  rm(tab15)
  
  #################################################################################################
  #Table 10: Number of administrations/record by ATC A level in the study population by year of administration/record and by meaning
  #################################################################################################
  print("Creating Table 10:  Number of administrations/record by ATC A level in the study population by year of administration/record and by meaning.")
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
    tab10<-as.data.table(merge(tab10,Res.1_total, by=c("meaning","year")))
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
  empty_atc_code_m_y<-as.data.table(merge(empty_atc_code_m_y,comb_tab10, by=c("meaning","year"),all=T))
  empty_atc_code_m_y<-as.data.table(merge(empty_atc_code_m_y,vx_study_population_meaning_year,by=c("meaning","year")))
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
  #Table 11: Number of administrations/record by ATC A level in the female study population of childbearing age 12-55 years (based on age at Start_study_fup) by year of administration/record and by meaning
  #################################################################################################
  females_childbearing<-do.call(rbind,females_childbearing)
  females_childbearing<-sum(females_childbearing)
  if(females_childbearing>0){
    print("Creating Table 11:  Number of administrations/record by ATC A level in the female study population of childbearing age 12-55 years (based on age at Start_study_fup) by year of administration/record and by meaning.")
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
      tab11<-as.data.table(merge(tab11,Res.2_total, by=c("meaning","year")))
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
    empty_atc_code_m_y_f<-as.data.table(merge(empty_atc_code_m_y_f,comb_tab11, by=c("meaning","year"),all=T))
    empty_atc_code_m_y_f<-as.data.table(merge(empty_atc_code_m_y_f,vx_study_population_meaning_year_f,by=c("meaning","year")))
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
  #Table 12(atc>=4) and table 13(atc==7)
  ##################################
  print("Creating Table 12:  Number of administrations/record by ATC 1, 3 and 4 level in the study population by year of administration/record and by meaning.")
  print("Creating Table 13:  Number of administrations/record by ATC 7 level in the study population by year of administration/record and by meaning.")
  ##############################
  #Counts for females
  ##############################
  #Create list of files 
  
  if(female_population>0){
    #Load list of files
    if(subpopulations_present=="Yes"){
      vaccines_files_females<-list.files(paste0(vaccines_pop, subpopulations_names[s]), pattern = "f_population")
    } else {
      vaccines_files_females<-list.files(vaccines_pop, pattern = "f_population")
    }
    
    #Separate data in list by atc letter and year
    if(length(vaccines_files_females)>0){
      #creates filter year_ATC level
      files<-list()
      for (i in 1: length(vaccines_files_females)){
        files<-append(files,unique(list(paste(unlist(str_split(vaccines_files_females[i],"_"))[1],
                                              unlist(str_split(vaccines_files_females[i],"_"))[2],sep="_"))))
      }
      files<-do.call(c,files)
      #remove duplicates 
      files<-files[!duplicated(files)]
      #create list with names year_condition
      vaccines_list<-vector(mode="list", length=length(files))
      names(vaccines_list)<-files
      rm(files)
      #separate all files into the right category
      #Change:substr(vaccines_files_females,1,6)/it removes the possibility to detect files that are named by year incorrectly
      #26.10.2021
      for (i in 1:length(vaccines_list)){
        vaccines_list[[i]]<-vaccines_files_females[str_detect(substr(vaccines_files_females,1,6),names(vaccines_list)[i])]
      }
      rm(vaccines_files_females)
      vaccines_files_females<-vaccines_list
      rm(vaccines_list)
    }
    
    #Load file one by one
    #Calculate counts for atc===7
    #Separate in database with atc code up to 1, 3 and higher
    age_band_tab<-create_age_band(agebands_rates)
    for (vx_files in 1: length(vaccines_files_females)){
      print(paste0("Creating counts for females ",names(vaccines_files_females)[vx_files]))
      #load file
      if(subpopulations_present=="Yes"){
        vaccines<-lapply(paste0(vaccines_pop, subpopulations_names[s],"/",vaccines_files_females[[vx_files]]), readRDS)
      } else {
        vaccines<-lapply(paste0(vaccines_pop, vaccines_files_females[[vx_files]]), readRDS)
      }
      vaccines<-as.data.table(do.call(rbind,vaccines))
      #create char: number of characters for vx_atc
      vaccines[,char:=nchar(vx_atc)]
      
      ###########
      #Create age_bands, count records and users for rates
      ###########
      #Calculate age at date of disp/presc
      vaccines[,vaccines_date:=as.IDate(vaccines_date,"%Y%m%d")][,birth_date:=as.IDate(birth_date,"%Y%m%d")]
      vaccines[,age:=floor((vaccines_date-birth_date)/365.25)]
      #assign age_bands
      for (i in 1:age_band_tab[,.N]){
        vaccines[age>=age_band_tab[i,min] & age<=age_band_tab[i,max], age_band:=age_band_tab[i,age_band]]
      }
      #create truncated atc code
      vaccines[char==3,truncated_atc_code:=substr(vx_atc,1,3)]
      vaccines[char>=4,truncated_atc_code:=substr(vx_atc,1,4)]
      
      #counts by "sex","year","age_band"
      no_users_yas<-vaccines[!is.na(truncated_atc_code),lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("year","age_band","truncated_atc_code"), .SDcols="person_id"]
      no_users_yas[,sex:="F"]
      setnames(no_users_yas,"person_id","no_users")
      
      no_records_yas<-vaccines[!is.na(truncated_atc_code),.N, by=c("year","age_band","truncated_atc_code")]
      no_records_yas[,sex:="F"]
      setnames(no_records_yas,"N","no_records")
      
      no_records_yas<-as.data.table(merge(no_records_yas,no_users_yas),by=c("year","age_band","truncated_atc_code"),all=T)
      rm(no_users_yas)
      saveRDS(no_records_yas,paste0(vaccines_tmp, vx_files,"f_counts_yas.rds"))
      
      #counts by "sex","year"
      no_users_ys<-vaccines[!is.na(truncated_atc_code),lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("year","truncated_atc_code"), .SDcols="person_id"]
      no_users_ys[,sex:="F"]
      setnames(no_users_ys,"person_id","no_users")
      
      no_records_ys<-vaccines[!is.na(truncated_atc_code),.N, by=c("year","truncated_atc_code")]
      no_records_ys[,sex:="F"]
      setnames(no_records_ys,"N","no_records")
      
      no_records_ys<-as.data.table(merge(no_records_ys,no_users_ys),by=c("year","truncated_atc_code"),all=T)
      rm(no_users_ys)
      saveRDS(no_records_ys,paste0(vaccines_tmp, vx_files,"f_counts_ys.rds"))
      
      vaccines[,age_band:=NULL][,truncated_atc_code:=NULL][,age:=NULL]
      
      
      #########
      #Counts for atc==1 only
      ########
      vaccines_1<-vaccines[char==1]
      if(vaccines_1[,.N]>0){
        vaccines<-vaccines[char>1]
        #create atc_code(truncted to the first level)
        vaccines_1[,truncated_atc_code:=substr(vx_atc,1,1)]
        #combine records per person
        vaccines_1<-vaccines_1[,.(count=.N), by=c("person_id","meaning","year","truncated_atc_code")]
        #no_records by meaning, year,sex
        no_records_1<-vaccines_1[,lapply(.SD, sum), by=c("meaning","year", "truncated_atc_code"), .SDcols="count"]
        setnames(no_records_1,"count","no_records")
        #no_users by meaning,year,sex
        no_users_1<-vaccines_1[,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("meaning","year","truncated_atc_code"), .SDcols="person_id"]
        setnames(no_users_1,"person_id","no_users")
        #median record
        median_1<-vaccines_1[,lapply(.SD,median),.SDcols="count", by=.(meaning, year, truncated_atc_code)]
        setnames(median_1,"count","median_records")
        #median record
        mean_1<-vaccines_1[,lapply(.SD,mean),.SDcols="count", by=.(meaning, year, truncated_atc_code)]
        setnames(mean_1,"count","mean_records")
        #Combine
        no_records_1<-as.data.table(merge(no_records_1,no_users_1, by=c("meaning","year", "truncated_atc_code"), all=T))
        rm(no_users_1)
        no_records_1<-as.data.table(merge(no_records_1,median_1, by=c("meaning","year", "truncated_atc_code"), all=T))
        rm(median_1)
        no_records_1<-as.data.table(merge(no_records_1,mean_1, by=c("meaning","year", "truncated_atc_code"), all=T))
        rm(mean_1)
        no_records_1[,atc_3:="N/A"][,atc_4:="N/A"]
        setnames(no_records_1,"truncated_atc_code","atc_1")
      } else{
        no_records_1<-NULL
      }
      rm(vaccines_1)
      
      #########
      #Counts for atc==3 only
      ########
      vaccines_3<-vaccines[char==3]
      if(vaccines_3[,.N]>0){
        vaccines<-vaccines[char>3]
        #create atc_code(truncted to the first level)
        vaccines_3[,truncated_atc_code:=substr(vx_atc,1,3)]
        #combine records per person
        vaccines_3<-vaccines_3[,.(count=.N), by=c("person_id","meaning","year","truncated_atc_code")]
        #no_records by meaning, year,sex
        no_records_3<-vaccines_3[,lapply(.SD, sum), by=c("meaning","year", "truncated_atc_code"), .SDcols="count"]
        setnames(no_records_3,"count","no_records")
        #no_users by meaning,year,sex
        no_users_3<-vaccines_3[,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("meaning","year","truncated_atc_code"), .SDcols="person_id"]
        setnames(no_users_3,"person_id","no_users")
        #median record
        median_3<-vaccines_3[,lapply(.SD,median),.SDcols="count", by=.(meaning, year, truncated_atc_code)]
        setnames(median_3,"count","median_records")
        #median record
        mean_3<-vaccines_3[,lapply(.SD,mean),.SDcols="count", by=.(meaning, year, truncated_atc_code)]
        setnames(mean_3,"count","mean_records")
        #Combine
        no_records_3<-as.data.table(merge(no_records_3,no_users_3, by=c("meaning","year", "truncated_atc_code"), all=T))
        rm(no_users_3)
        no_records_3<-as.data.table(merge(no_records_3,median_3, by=c("meaning","year", "truncated_atc_code"), all=T))
        rm(median_3)
        no_records_3<-as.data.table(merge(no_records_3,mean_3, by=c("meaning","year", "truncated_atc_code"), all=T))
        rm(mean_3)
        no_records_3[,atc_1:=substr(truncated_atc_code,1,1)][,atc_4:="N/A"]
        setnames(no_records_3,"truncated_atc_code","atc_3")
      } else {no_records_3<-NULL}
      rm(vaccines_3)
      
      if(vaccines[,.N]>0){
        #combine records per person
        vaccines<-vaccines[,.(count=.N), by=c("person_id","meaning","year","vx_atc","char")]
      }
      
      #########
      #Counts for atc==7
      ########
      if(vaccines[char==7,.N]>0){
        #no_records by meaning, year,sex
        no_records_7<-vaccines[char==7,lapply(.SD, sum), by=c("meaning","year", "vx_atc"), .SDcols="count"]
        setnames(no_records_7,"count","no_records")
        #no_users by meaning,year,sex
        no_users_7<-vaccines[char==7,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("meaning","year","vx_atc"), .SDcols="person_id"]
        setnames(no_users_7,"person_id","no_users")
        #median record
        median_7<-vaccines[char==7,lapply(.SD,median),.SDcols="count", by=.(meaning, year, vx_atc)]
        setnames(median_7,"count","median_records")
        #median record
        mean_7<-vaccines[char==7,lapply(.SD,mean),.SDcols="count", by=.(meaning, year, vx_atc)]
        setnames(mean_7,"count","mean_records")
        #Combine
        no_records_7<-as.data.table(merge(no_records_7,no_users_7, by=c("meaning","year", "vx_atc"), all=T))
        rm(no_users_7)
        no_records_7<-as.data.table(merge(no_records_7,median_7, by=c("meaning","year", "vx_atc"), all=T))
        rm(median_7)
        no_records_7<-as.data.table(merge(no_records_7,mean_7, by=c("meaning","year", "vx_atc"), all=T))
        rm(mean_7)
        setnames(no_records_7,"vx_atc","atc_7")
      } else {
        no_records_7<-NULL
      }
      
      #########
      #Counts for atc>=4
      ########
      if(vaccines[,.N]>0){
        #create atc_code(truncted to the first level)
        vaccines[,truncated_atc_code:=substr(vx_atc,1,4)]
        #combine records per person
        vaccines<-vaccines[,lapply(.SD,sum), by=c("person_id","meaning","year","truncated_atc_code"), .SDcols="count"]
        #no_records by meaning, year,sex
        no_records_4<-vaccines[,lapply(.SD, sum), by=c("meaning","year", "truncated_atc_code"), .SDcols="count"]
        setnames(no_records_4,"count","no_records")
        #no_users by meaning,year,sex
        no_users_4<-vaccines[,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("meaning","year","truncated_atc_code"), .SDcols="person_id"]
        setnames(no_users_4,"person_id","no_users")
        #median record
        median_4<-vaccines[,lapply(.SD,median),.SDcols="count", by=.(meaning, year, truncated_atc_code)]
        setnames(median_4,"count","median_records")
        #median record
        mean_4<-vaccines[,lapply(.SD,mean),.SDcols="count", by=.(meaning, year, truncated_atc_code)]
        setnames(mean_4,"count","mean_records")
        #Combine
        no_records_4<-as.data.table(merge(no_records_4,no_users_4, by=c("meaning","year", "truncated_atc_code"), all=T))
        rm(no_users_4)
        no_records_4<-as.data.table(merge(no_records_4,median_4, by=c("meaning","year", "truncated_atc_code"), all=T))
        rm(median_4)
        no_records_4<-as.data.table(merge(no_records_4,mean_4, by=c("meaning","year", "truncated_atc_code"), all=T))
        rm(mean_4)
        no_records_4[,atc_1:=substr(truncated_atc_code,1,1)][,atc_3:=substr(truncated_atc_code,1,3)]
        setnames(no_records_4,"truncated_atc_code","atc_4")
      } else {
        no_records_4<-NULL
      } 
      
      rm(vaccines)
      
      #Combine results
      tab12<-rbind(no_records_1,no_records_3,no_records_4)
      saveRDS(tab12, paste0(vaccines_tmp, vx_files, "f_tab12.rds"))
      rm(tab12)
      
      saveRDS(no_records_7, paste0(vaccines_tmp, vx_files, "f_tab13.rds"))
      rm(no_records_7)
    }
    
    #Combine results for tab12
    tab12_females_list<-list.files(vaccines_tmp, "f_tab12")
    if(length(tab12_females_list)>0){
      tab12_females<-lapply(paste0(vaccines_tmp,tab12_females_list),readRDS)
      tab12_females<-as.data.table(do.call(rbind,tab12_females))
      tab12_females[,mean_records:=round(mean_records,2)]
      tab12_females[,sex:="F"]
      for (i in 1:length(tab12_females_list)){
        file.remove(paste0(vaccines_tmp, tab12_females_list[i]))
      }
      
      saveRDS(tab12_females, paste0(vaccines_tmp, "f_tab12.rds"))
    }
    rm(tab12_females_list)
    
    #Combine results for tab13
    tab13_females_list<-list.files(vaccines_tmp, "f_tab13")
    if(length(tab13_females_list)>0){
      tab13_females<-lapply(paste0(vaccines_tmp,tab13_females_list),readRDS)
      tab13_females<-as.data.table(do.call(rbind,tab13_females))
      tab13_females[,mean_records:=round(mean_records,2)]
      tab13_females[,sex:="F"]
      for (i in 1:length(tab13_females_list)){
        file.remove(paste0(vaccines_tmp, tab13_females_list[i]))
      }
      
      saveRDS(tab13_females, paste0(vaccines_tmp, "f_tab13.rds"))
    }
    rm(tab13_females_list)
    
    #combine results for counts yas
    counts_yas_list<-list.files(vaccines_tmp, "f_counts_yas")
    if(length(counts_yas_list)>0){
      counts_yas<-lapply(paste0(vaccines_tmp,counts_yas_list),readRDS)
      counts_yas<-as.data.table(do.call(rbind,counts_yas))
      
      for (i in 1:length(counts_yas_list)){
        file.remove(paste0(vaccines_tmp, counts_yas_list[i]))
      }
      
      saveRDS(counts_yas, paste0(vaccines_tmp, "f_counts_rates_yas.rds"))
    }
    rm(counts_yas_list)
    
    #combine results for counts ys
    counts_ys_list<-list.files(vaccines_tmp, "f_counts_ys")
    if(length(counts_ys_list)>0){
      counts_ys<-lapply(paste0(vaccines_tmp,counts_ys_list),readRDS)
      counts_ys<-as.data.table(do.call(rbind,counts_ys))
      
      for (i in 1:length(counts_ys_list)){
        file.remove(paste0(vaccines_tmp, counts_ys_list[i]))
      }
      
      saveRDS(counts_ys, paste0(vaccines_tmp, "f_counts_rates_ys.rds"))
    }
    rm(counts_ys_list)
  }
  
  if(male_population>0){
    #Load list of files
    if(subpopulations_present=="Yes"){
      vaccines_files_males<-list.files(paste0(vaccines_pop, subpopulations_names[s]), pattern = "m_population")
    } else {
      vaccines_files_males<-list.files(vaccines_pop, pattern = "m_population")
    }
    
    #Separate data in list by atc letter and year
    if(length(vaccines_files_males)>0){
      #creates filter year_ATC level
      files<-list()
      for (i in 1: length(vaccines_files_males)){
        files<-append(files,unique(list(paste(unlist(str_split(vaccines_files_males[i],"_"))[1],
                                              unlist(str_split(vaccines_files_males[i],"_"))[2],sep="_"))))
      }
      files<-do.call(c,files)
      #remove duplicates 
      files<-files[!duplicated(files)]
      #create list with names year_condition
      vaccines_list<-vector(mode="list", length=length(files))
      names(vaccines_list)<-files
      rm(files)
      #separate all files into the right category
      #Change:substr(vaccines_files_males,1,6)/it removes the possibility to detect files that are named by year incorrectly
      #26.10.2021
      for (i in 1:length(vaccines_list)){
        vaccines_list[[i]]<-vaccines_files_males[str_detect(substr(vaccines_files_males,1,6),names(vaccines_list)[i])]
      }
      rm(vaccines_files_males)
      vaccines_files_males<-vaccines_list
      rm(vaccines_list)
    }
    
    #Load file one by one
    #Calculate counts for atc===7
    #Separate in database with atc code up to 1, 3 and higher
    age_band_tab<-create_age_band(agebands_rates)
    for (vx_files in 1: length(vaccines_files_males)){
      print(paste0("Creating counts for males ",names(vaccines_files_males)[vx_files]))
      #load file
      if(subpopulations_present=="Yes"){
        vaccines<-lapply(paste0(vaccines_pop, subpopulations_names[s],"/",vaccines_files_males[[vx_files]]), readRDS)
      } else {
        vaccines<-lapply(paste0(vaccines_pop, vaccines_files_males[[vx_files]]), readRDS)
      }
      vaccines<-as.data.table(do.call(rbind,vaccines))
      #create char: number of characters for vx_atc
      vaccines[,char:=nchar(vx_atc)]
      
      ###########
      #Create age_bands, count records and users for rates
      ###########
      #Calculate age at date of disp/presc
      vaccines[,vaccines_date:=as.IDate(vaccines_date,"%Y%m%d")][,birth_date:=as.IDate(birth_date,"%Y%m%d")]
      vaccines[,age:=floor((vaccines_date-birth_date)/365.25)]
      #assign age_bands
      for (i in 1:age_band_tab[,.N]){
        vaccines[age>=age_band_tab[i,min] & age<=age_band_tab[i,max], age_band:=age_band_tab[i,age_band]]
      }
      #create truncated atc code
      vaccines[char==3,truncated_atc_code:=substr(vx_atc,1,3)]
      vaccines[char>=4,truncated_atc_code:=substr(vx_atc,1,4)]
      
      #counts by "sex","year","age_band"
      no_users_yas<-vaccines[!is.na(truncated_atc_code),lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("year","age_band","truncated_atc_code"), .SDcols="person_id"]
      no_users_yas[,sex:="M"]
      setnames(no_users_yas,"person_id","no_users")
      
      no_records_yas<-vaccines[!is.na(truncated_atc_code),.N, by=c("year","age_band","truncated_atc_code")]
      no_records_yas[,sex:="M"]
      setnames(no_records_yas,"N","no_records")
      
      no_records_yas<-as.data.table(merge(no_records_yas,no_users_yas),by=c("year","age_band","truncated_atc_code"),all=T)
      rm(no_users_yas)
      saveRDS(no_records_yas,paste0(vaccines_tmp, vx_files,"m_counts_yas.rds"))
      
      #counts by "sex","year"
      no_users_ys<-vaccines[!is.na(truncated_atc_code),lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("year","truncated_atc_code"), .SDcols="person_id"]
      no_users_ys[,sex:="M"]
      setnames(no_users_ys,"person_id","no_users")
      
      no_records_ys<-vaccines[!is.na(truncated_atc_code),.N, by=c("year","truncated_atc_code")]
      no_records_ys[,sex:="M"]
      setnames(no_records_ys,"N","no_records")
      
      no_records_ys<-as.data.table(merge(no_records_ys,no_users_ys),by=c("year","truncated_atc_code"),all=T)
      rm(no_users_ys)
      saveRDS(no_records_ys,paste0(vaccines_tmp, vx_files,"m_counts_ys.rds"))
      
      vaccines[,age_band:=NULL][,truncated_atc_code:=NULL][,age:=NULL]
      
      
      #########
      #Counts for atc==1 only
      ########
      vaccines_1<-vaccines[char==1]
      if(vaccines_1[,.N]>0){
        vaccines<-vaccines[char>1]
        #create atc_code(truncted to the first level)
        vaccines_1[,truncated_atc_code:=substr(vx_atc,1,1)]
        #combine records per person
        vaccines_1<-vaccines_1[,.(count=.N), by=c("person_id","meaning","year","truncated_atc_code")]
        #no_records by meaning, year,sex
        no_records_1<-vaccines_1[,lapply(.SD, sum), by=c("meaning","year", "truncated_atc_code"), .SDcols="count"]
        setnames(no_records_1,"count","no_records")
        #no_users by meaning,year,sex
        no_users_1<-vaccines_1[,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("meaning","year","truncated_atc_code"), .SDcols="person_id"]
        setnames(no_users_1,"person_id","no_users")
        #median record
        median_1<-vaccines_1[,lapply(.SD,median),.SDcols="count", by=.(meaning, year, truncated_atc_code)]
        setnames(median_1,"count","median_records")
        #median record
        mean_1<-vaccines_1[,lapply(.SD,mean),.SDcols="count", by=.(meaning, year, truncated_atc_code)]
        setnames(mean_1,"count","mean_records")
        #Combine
        no_records_1<-as.data.table(merge(no_records_1,no_users_1, by=c("meaning","year", "truncated_atc_code"), all=T))
        rm(no_users_1)
        no_records_1<-as.data.table(merge(no_records_1,median_1, by=c("meaning","year", "truncated_atc_code"), all=T))
        rm(median_1)
        no_records_1<-as.data.table(merge(no_records_1,mean_1, by=c("meaning","year", "truncated_atc_code"), all=T))
        rm(mean_1)
        no_records_1[,atc_3:="N/A"][,atc_4:="N/A"]
        setnames(no_records_1,"truncated_atc_code","atc_1")
      } else{
        no_records_1<-NULL
      }
      rm(vaccines_1)
      
      #########
      #Counts for atc==3 only
      ########
      vaccines_3<-vaccines[char==3]
      if(vaccines_3[,.N]>0){
        vaccines<-vaccines[char>3]
        #create atc_code(truncted to the first level)
        vaccines_3[,truncated_atc_code:=substr(vx_atc,1,3)]
        #combine records per person
        vaccines_3<-vaccines_3[,.(count=.N), by=c("person_id","meaning","year","truncated_atc_code")]
        #no_records by meaning, year,sex
        no_records_3<-vaccines_3[,lapply(.SD, sum), by=c("meaning","year", "truncated_atc_code"), .SDcols="count"]
        setnames(no_records_3,"count","no_records")
        #no_users by meaning,year,sex
        no_users_3<-vaccines_3[,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("meaning","year","truncated_atc_code"), .SDcols="person_id"]
        setnames(no_users_3,"person_id","no_users")
        #median record
        median_3<-vaccines_3[,lapply(.SD,median),.SDcols="count", by=.(meaning, year, truncated_atc_code)]
        setnames(median_3,"count","median_records")
        #median record
        mean_3<-vaccines_3[,lapply(.SD,mean),.SDcols="count", by=.(meaning, year, truncated_atc_code)]
        setnames(mean_3,"count","mean_records")
        #Combine
        no_records_3<-as.data.table(merge(no_records_3,no_users_3, by=c("meaning","year", "truncated_atc_code"), all=T))
        rm(no_users_3)
        no_records_3<-as.data.table(merge(no_records_3,median_3, by=c("meaning","year", "truncated_atc_code"), all=T))
        rm(median_3)
        no_records_3<-as.data.table(merge(no_records_3,mean_3, by=c("meaning","year", "truncated_atc_code"), all=T))
        rm(mean_3)
        no_records_3[,atc_1:=substr(truncated_atc_code,1,1)][,atc_4:="N/A"]
        setnames(no_records_3,"truncated_atc_code","atc_3")
      } else {no_records_3<-NULL}
      rm(vaccines_3)
      
      if(vaccines[,.N]>0){
        #combine records per person
        vaccines<-vaccines[,.(count=.N), by=c("person_id","meaning","year","vx_atc","char")]
      }
      
      #########
      #Counts for atc==7
      ########
      if(vaccines[char==7,.N]>0){
        #no_records by meaning, year,sex
        no_records_7<-vaccines[char==7,lapply(.SD, sum), by=c("meaning","year", "vx_atc"), .SDcols="count"]
        setnames(no_records_7,"count","no_records")
        #no_users by meaning,year,sex
        no_users_7<-vaccines[char==7,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("meaning","year","vx_atc"), .SDcols="person_id"]
        setnames(no_users_7,"person_id","no_users")
        #median record
        median_7<-vaccines[char==7,lapply(.SD,median),.SDcols="count", by=.(meaning, year, vx_atc)]
        setnames(median_7,"count","median_records")
        #median record
        mean_7<-vaccines[char==7,lapply(.SD,mean),.SDcols="count", by=.(meaning, year, vx_atc)]
        setnames(mean_7,"count","mean_records")
        #Combine
        no_records_7<-as.data.table(merge(no_records_7,no_users_7, by=c("meaning","year", "vx_atc"), all=T))
        rm(no_users_7)
        no_records_7<-as.data.table(merge(no_records_7,median_7, by=c("meaning","year", "vx_atc"), all=T))
        rm(median_7)
        no_records_7<-as.data.table(merge(no_records_7,mean_7, by=c("meaning","year", "vx_atc"), all=T))
        rm(mean_7)
        setnames(no_records_7,"vx_atc","atc_7")
      } else {
        no_records_7<-NULL
      }
      
      #########
      #Counts for atc>=4
      ########
      if(vaccines[,.N]>0){
        #create atc_code(truncted to the first level)
        vaccines[,truncated_atc_code:=substr(vx_atc,1,4)]
        #combine records per person
        vaccines<-vaccines[,lapply(.SD,sum), by=c("person_id","meaning","year","truncated_atc_code"), .SDcols="count"]
        #no_records by meaning, year,sex
        no_records_4<-vaccines[,lapply(.SD, sum), by=c("meaning","year", "truncated_atc_code"), .SDcols="count"]
        setnames(no_records_4,"count","no_records")
        #no_users by meaning,year,sex
        no_users_4<-vaccines[,lapply(.SD, function(x) length(unique(na.omit(x)))), by=c("meaning","year","truncated_atc_code"), .SDcols="person_id"]
        setnames(no_users_4,"person_id","no_users")
        #median record
        median_4<-vaccines[,lapply(.SD,median),.SDcols="count", by=.(meaning, year, truncated_atc_code)]
        setnames(median_4,"count","median_records")
        #median record
        mean_4<-vaccines[,lapply(.SD,mean),.SDcols="count", by=.(meaning, year, truncated_atc_code)]
        setnames(mean_4,"count","mean_records")
        #Combine
        no_records_4<-as.data.table(merge(no_records_4,no_users_4, by=c("meaning","year", "truncated_atc_code"), all=T))
        rm(no_users_4)
        no_records_4<-as.data.table(merge(no_records_4,median_4, by=c("meaning","year", "truncated_atc_code"), all=T))
        rm(median_4)
        no_records_4<-as.data.table(merge(no_records_4,mean_4, by=c("meaning","year", "truncated_atc_code"), all=T))
        rm(mean_4)
        no_records_4[,atc_1:=substr(truncated_atc_code,1,1)][,atc_3:=substr(truncated_atc_code,1,3)]
        setnames(no_records_4,"truncated_atc_code","atc_4")
      } else {
        no_records_4<-NULL
      } 
      
      rm(vaccines)
      
      #Combine results
      tab12<-rbind(no_records_1,no_records_3,no_records_4)
      saveRDS(tab12, paste0(vaccines_tmp, vx_files, "m_tab12.rds"))
      rm(tab12)
      
      saveRDS(no_records_7, paste0(vaccines_tmp, vx_files, "m_tab13.rds"))
      rm(no_records_7)
      
    }
    
    #Combine results for tab12
    tab12_males_list<-list.files(vaccines_tmp, "m_tab12")
    if(length(tab12_males_list)>0){
      tab12_males<-lapply(paste0(vaccines_tmp,tab12_males_list),readRDS)
      tab12_males<-as.data.table(do.call(rbind,tab12_males))
      tab12_males[,mean_records:=round(mean_records,2)]
      tab12_males[,sex:="M"]
      for (i in 1:length(tab12_males_list)){
        file.remove(paste0(vaccines_tmp, tab12_males_list[i]))
      }
      
      saveRDS(tab12_males, paste0(vaccines_tmp, "m_tab12.rds"))
    }
    rm(tab12_males_list)
    
    #Combine results for tab13
    tab13_males_list<-list.files(vaccines_tmp, "m_tab13")
    if(length(tab13_males_list)>0){
      tab13_males<-lapply(paste0(vaccines_tmp,tab13_males_list),readRDS)
      tab13_males<-as.data.table(do.call(rbind,tab13_males))
      tab13_males[,mean_records:=round(mean_records,2)]
      tab13_males[,sex:="M"]
      for (i in 1:length(tab13_males_list)){
        file.remove(paste0(vaccines_tmp, tab13_males_list[i]))
      }
      
      saveRDS(tab13_males, paste0(vaccines_tmp, "m_tab13.rds"))
    }
    rm(tab13_males_list)
    
    #combine results for counts yas
    counts_yas_list<-list.files(vaccines_tmp, "m_counts_yas")
    if(length(counts_yas_list)>0){
      counts_yas<-lapply(paste0(vaccines_tmp,counts_yas_list),readRDS)
      counts_yas<-as.data.table(do.call(rbind,counts_yas))
      
      for (i in 1:length(counts_yas_list)){
        file.remove(paste0(vaccines_tmp, counts_yas_list[i]))
      }
      
      saveRDS(counts_yas, paste0(vaccines_tmp, "m_counts_rates_yas.rds"))
    }
    rm(counts_yas_list)
    
    #combine results for counts ys
    counts_ys_list<-list.files(vaccines_tmp, "m_counts_ys")
    if(length(counts_ys_list)>0){
      counts_ys<-lapply(paste0(vaccines_tmp,counts_ys_list),readRDS)
      counts_ys<-as.data.table(do.call(rbind,counts_ys))
      
      for (i in 1:length(counts_ys_list)){
        file.remove(paste0(vaccines_tmp, counts_ys_list[i]))
      }
      
      saveRDS(counts_ys, paste0(vaccines_tmp, "m_counts_rates_ys.rds"))
    }
    rm(counts_ys_list)
    
  }
  
  #create tab12
  tab12_f<-list.files(vaccines_tmp,"tab12")
  if(length(tab12_f)>0){
    tab12<-lapply(paste0(vaccines_tmp,tab12_f), readRDS)
    tab12<-as.data.table(do.call(rbind,tab12))
    setcolorder(tab12,c("sex", "year", "atc_4", "no_records", "no_users","median_records","mean_records","meaning","atc_1","atc_3"))
    
    for (i in 1:length(tab12_f)){
      file.remove(paste0(vaccines_tmp, tab12_f[i]))
    }
    
  } else {
    tab12<-NULL
  }
  rm(tab12_f)
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
    tab12[, no_users:= as.character(no_users)][as.numeric(no_users) > 0 & as.numeric(no_users) < 5, no_users := "<5"]
    
    if(subpopulations_present=="Yes"){
      fwrite(tab12, paste0(vacc_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_vaccines_my_atc_4_masked.csv"), row.names = F)
    } else {
      fwrite(tab12, paste0(vacc_dir,"Masked/", "vaccines_my_atc_4_masked.csv"), row.names = F)
    }
  }
  
  rm(tab12)
  
  
  #create tab13
  tab13_f<-list.files(vaccines_tmp,"tab13")
  if(length(tab13_f)>0){
    tab13<-lapply(paste0(vaccines_tmp,tab13_f), readRDS)
    tab13<-as.data.table(do.call(rbind,tab13))
    setcolorder(tab13,c("sex", "year", "atc_7", "no_records", "no_users","median_records","mean_records","meaning"))
    
    for (i in 1:length(tab13_f)){
      file.remove(paste0(vaccines_tmp, tab13_f[i]))
    }
  } else {
    tab13<-NULL
  }
  rm(tab13_f)
  
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
    tab13[, no_users:= as.character(no_users)][as.numeric(no_users) > 0 & as.numeric(no_users) < 5, no_users := "<5"]
    
    if(subpopulations_present=="Yes"){
      fwrite(tab13, paste0(vacc_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_vaccines_my_atc_7_masked.csv"), row.names = F)
    } else {
      fwrite(tab13, paste0(vacc_dir,"Masked/", "vaccines_my_atc_7_masked.csv"), row.names = F)
    }
  }
  
  rm(tab13)
  
  
}
