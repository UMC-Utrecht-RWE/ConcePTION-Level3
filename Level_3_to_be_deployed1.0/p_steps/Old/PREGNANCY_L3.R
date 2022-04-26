###############################################################################################
source(paste0(pre_dir, "create_conceptsets.R")) #for diagnoses
###############################################################################################
source(paste0(pre_dir,"info.R"))
########################################################
#Create output folders
########################################################
#########################
#Pregnancy
#########################
if (subpopulations_present=="No"){
  #output folder for PREGNANCY report in g_output
  if ("PREGNANCY" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"PREGNANCY/"), recursive = T)#delete folder
    dir.create(paste(output_dir, "PREGNANCY", sep=""))
    preg_dir<-paste(output_dir, "PREGNANCY/", sep="")
    dir.create(paste(preg_dir,"Masked", sep=""))
    preg_less<-paste(preg_dir, "Masked/", sep="")
  } else {
    #Create the PREGNANCY folder in the output dir
    dir.create(paste(output_dir, "PREGNANCY", sep=""))
    preg_dir<-paste(output_dir, "PREGNANCY/", sep="")
    dir.create(paste(preg_dir,"Masked", sep=""))
    preg_less<-paste(preg_dir, "Masked/", sep="")
  }
  
  #output diagnoses_study_population in g_intermediate/populations/pregnancy
  if ("PREGNANCY" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"PREGNANCY/"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "PREGNANCY", sep=""))
    preg_pop<-paste(populations_dir, "PREGNANCY/", sep="")
  } else {
    #Create the PREGNANCY folder in the output dir
    dir.create(paste(populations_dir, "PREGNANCY", sep=""))
    preg_pop<-paste(populations_dir, "PREGNANCY/", sep="")
  }
  
  #PREGNANCY_tmp/PREGNANCY_EV folder where all intermediary files are saved
  if ("PREGNANCY_EV" %in% list.files(tmp)){
    preg_ev_tmp<-paste(tmp, "PREGNANCY_EV/", sep="")
    do.call(file.remove, list(list.files(preg_ev_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the PREGNANCY_EVENTS folder in the output dir
    dir.create(paste(tmp, "PREGNANCY_EV", sep=""))
    preg_ev_tmp<-paste(tmp, "PREGNANCY_EV/", sep="")
  }
  
  #PREGNANCY_M_tmp/PREGNANCY_M folder where all intermediary files are saved
  if ("PREGNANCY_M" %in% list.files(tmp)){
    preg_m_tmp<-paste(tmp, "PREGNANCY_M/", sep="")
    do.call(file.remove, list(list.files(preg_m_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the MO folder in the output dir
    dir.create(paste(tmp, "PREGNANCY_M", sep=""))
    preg_m_tmp<-paste(tmp, "PREGNANCY_M/", sep="")
  }
  
  #SO_tmp/SO folder where all intermediary files are saved
  if ("PREGNANCY_S" %in% list.files(tmp)){
    preg_s_tmp<-paste(tmp, "PREGNANCY_S/", sep="")
    do.call(file.remove, list(list.files(preg_s_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the SO folder in the output dir
    dir.create(paste(tmp, "PREGNANCY_S", sep=""))
    preg_s_tmp<-paste(tmp, "PREGNANCY_S/", sep="")
  }
  
} else {
  #output folder for PREGNANCY report in g_output
  if ("PREGNANCY" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"PREGNANCY/"), recursive = T)#delete folder
    dir.create(paste(output_dir, "PREGNANCY", sep=""))
    preg_dir<-paste(output_dir, "PREGNANCY/", sep="")
    
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(preg_dir, subpopulations_names[i],"/Masked"))
    }
  } else {
    #Create the PREGNANCY folder in the output dir
    dir.create(paste(output_dir, "PREGNANCY", sep=""))
    preg_dir<-paste(output_dir, "PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(preg_dir, subpopulations_names[i],"/Masked"))
    }
  }
  
  #output diagnoses_study_population in g_intermediate/populations/diagnoses
  if ("PREGNANCY" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"PREGNANCY/"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "PREGNANCY", sep=""))
    preg_pop<-paste(populations_dir, "PREGNANCY/",sep="")
    
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(preg_pop, subpopulations_names[i]))
    }
  } else {
    #Create the PREGNANCY folder in the output dir
    dir.create(paste(populations_dir, "PREGNANCY", sep=""))
    preg_pop<-paste(populations_dir, "PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(preg_pop, subpopulations_names[i]))
    }
  }
  
  #PREGNANCY_EV_tmp/PREGNANCY_EV folder where all intermediary files are saved
  if ("PREGNANCY_EV" %in% list.files(tmp)){
    preg_ev_tmp<-paste(tmp, "PREGNANCY_EV/", sep="")
    do.call(file.remove, list(list.files(preg_ev_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the PREGNANCY_EV folder in the output dir
    dir.create(paste(tmp, "PREGNANCY_EV", sep=""))
    preg_ev_tmp<-paste(tmp, "PREGNANCY_EV/", sep="")
  }
  
  #PREGNANCY_M_tmp/PREGNANCY_M folder where all intermediary files are saved
  if ("PREGNANCY_M" %in% list.files(tmp)){
    preg_m_tmp<-paste(tmp, "PREGNANCY_M/", sep="")
    do.call(file.remove, list(list.files(preg_m_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the PREGNANCY_M folder in the output dir
    dir.create(paste(tmp, "PREGNANCY_M", sep=""))
    preg_m_tmp<-paste(tmp, "PREGNANCY_M/", sep="")
  }
  
  #PREGNANCY_S_tmp/PREGNANCY_S folder where all intermediary files are saved
  if ("PREGNANCY_S" %in% list.files(tmp)){
    preg_s_tmp<-paste(tmp, "PREGNANCY_S/", sep="")
    do.call(file.remove, list(list.files(preg_s_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the PREGNANCY_S folder in the output dir
    dir.create(paste(tmp, "PREGNANCY_S", sep=""))
    preg_s_tmp<-paste(tmp, "PREGNANCY_S/", sep="")
  }
  
}

####################################################################################################
#Main script
####################################################################################################
if (subpopulations_present=="Yes"){
  for (s in 1: length(subpopulations_names)){
    study_sub_population<-study_population_dir[grepl(study_population_dir, pattern=paste0(subpopulations_names[s],"_study_population"), fixed=T)]
    study_sub_population<-study_sub_population[grepl(study_sub_population, pattern=paste0("^", subpopulations_names[s]))]
    study_population<-readRDS(paste0(g_intermediate, "populations/", study_sub_population))
    study_population<-study_population[sex_at_instance_creation=="F" & age_fup>=12 & age_fup<=55]
    nr_std<-study_population[,.N]
    #MEANINGS TO BE EXCLUDED
    meanings_exclude_events<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="EVENTS" & other==subpopulations_names[s],values], pattern = " "))
    meanings_exclude_mo<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="MEDICAL_OBSERVATIONS" & other==subpopulations_names[s],values], pattern = " "))
    meanings_exclude_so<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="SURVEY_OBSERVATIONS" & other==subpopulations_names[s],values], pattern = " "))
    
    source(paste0(pre_dir, "PREGNANCY_L3_pre_script.R"))
    
    tab17_preg<-data.table(tab17_preg, data_access_provider= data_access_provider_name, data_source=data_source_name)
    tab19_preg<-data.table(tab19_preg, data_access_provider= data_access_provider_name, data_source=data_source_name)
    tab19_preg[, percentage_min_1_code_fup:= as.character(percentage_min_1_code_fup)][no_min_1_code_fup == 0 & no_min_1_code_fup==0, percentage_min_1_code_fup := "N/A"]
    
    
    write.csv(flowchart_preg, paste0(preg_dir,subpopulations_names[s], "/", subpopulations_names[s],"_flowchart_pregnancy.csv"), row.names = F)
    write.csv(description_preg, paste0(preg_dir,subpopulations_names[s], "/", subpopulations_names[s],"_description_pregnancy.csv"), row.names = F)
    
    if(!is.null(tab17_preg)){
      write.csv(tab17_preg, paste0(preg_dir,subpopulations_names[s], "/", subpopulations_names[s],"_tab17_pregnancy.csv"), row.names = F)
    }
    if(!is.null(tab19_preg)){
      write.csv(tab19_preg, paste0(preg_dir,subpopulations_names[s], "/", subpopulations_names[s],"_tab19_pregnancy.csv"), row.names = F)
    }
    
    flowchart_preg[, EVENTS:= as.character(EVENTS)][as.numeric(EVENTS) > 0 & as.numeric(EVENTS) < 5, EVENTS := "<5"]
    flowchart_preg[, MEDICAL_OBSERVATIONS:= as.character(MEDICAL_OBSERVATIONS)][as.numeric(MEDICAL_OBSERVATIONS) > 0 & as.numeric(MEDICAL_OBSERVATIONS) < 5, MEDICAL_OBSERVATIONS := "<5"]
    flowchart_preg[, SURVEY_OBSERVATIONS:= as.character(SURVEY_OBSERVATIONS)][as.numeric(SURVEY_OBSERVATIONS) > 0 & as.numeric(SURVEY_OBSERVATIONS) < 5, SURVEY_OBSERVATIONS := "<5"]
    write.csv(flowchart_preg, paste0(preg_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_flowchart_pregnancy_masked.csv"), row.names = F)
    
    if(description_preg[4, 2]<5 & description_preg[4, 2]>0) {description_preg[4, 2]<-"<5"}
    if(description_preg[4, 3]<5 & description_preg[4, 3]>0) {description_preg[4, 3]<-"<5"} 
    if(description_preg[4, 4]<5 & description_preg[4, 4]>0) {description_preg[4, 4]<-"<5"} 
    write.csv(description_preg, paste0(preg_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_description_pregnancy_masked.csv"), row.names = F)
    
    if(!is.null(tab17_preg)){
      tab17_preg[, person_years:= as.character(person_years)][as.numeric(person_years) > 0 & as.numeric(person_years) < 5, person_years := "<5"]
      tab17_preg[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
      tab17_preg[, rate_min_1_code_per_100_py:= as.character(rate_min_1_code_per_100_py)][no_records == "<5", rate_min_1_code_per_100_py := "N/A"]
      write.csv(tab17_preg, paste0(preg_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_tab17_pregnancy_masked.csv"), row.names = F)
      
    }
    
    if(!is.null(tab19_preg)){
      tab19_preg[, no_min_1_code:= as.character(no_min_1_code)][as.numeric(no_min_1_code) > 0 & as.numeric(no_min_1_code) < 5, no_min_1_code := "<5"]
      tab19_preg[, no_min_1_code_fup:= as.character(no_min_1_code_fup)][as.numeric(no_min_1_code_fup) > 0 & as.numeric(no_min_1_code_fup) < 5, no_min_1_code_fup := "<5"]
      tab19_preg[, min_1_code_fup_py:= as.character(min_1_code_fup_py)][as.numeric(min_1_code_fup_py) > 0 & as.numeric(min_1_code_fup_py) < 5, min_1_code_fup_py := "<5"]
      tab19_preg[, percentage_min_1_code_fup:= as.character(percentage_min_1_code_fup)][no_min_1_code_fup == "<5", percentage_min_1_code_fup := "N/A"]
      write.csv(tab19_preg, paste0(preg_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_tab19_pregnancy_masked.csv"), row.names = F)
    }
    
    
    do.call(file.remove, list(list.files(preg_ev_tmp, full.names = T)))
    do.call(file.remove, list(list.files(preg_m_tmp, full.names = T)))
    do.call(file.remove, list(list.files(preg_s_tmp, full.names = T)))
    
    rm(study_population, nr_std, flowchart, description, tab17_preg, tab19_preg)
  }
} else {
  study_population_dir<-study_population_dir[grepl(study_population_dir, pattern="study_population", fixed=T)]
  study_population<-readRDS(paste0(g_intermediate, "populations/", study_population_dir))
  study_population<-study_population[sex_at_instance_creation=="F" & age_fup>=12 & age_fup<=55]
   nr_std<-study_population[,.N]
  #MEANINGS TO BE EXCLUDED
  meanings_exclude_events<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="EVENTS",values], pattern = " "))
  meanings_exclude_mo<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="MEDICAL_OBSERVATIONS", values], pattern = " "))
  meanings_exclude_so<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="SURVEY_OBSERVATIONS", values], pattern = " "))
  
  source(paste0(pre_dir, "PREGNANCY_L3_pre_script.R"))
  
  
  tab17_preg<-data.table(tab17_preg, data_access_provider= data_access_provider_name, data_source=data_source_name)
  tab19_preg<-data.table(tab19_preg, data_access_provider= data_access_provider_name, data_source=data_source_name)
  tab19_preg[, percentage_min_1_code_fup:= as.character(percentage_min_1_code_fup)][no_min_1_code_fup == 0 & no_min_1_code_fup==0, percentage_min_1_code_fup := "N/A"]
  
  #######################################################
  write.csv(flowchart_preg, paste0(preg_dir, "flowchart_pregnancy.csv"), row.names = F)
  write.csv(description, paste0(preg_dir,"description_pregnancy.csv"), row.names = F)
  if(!is.null(tab17_preg)){
    write.csv(tab17_preg, paste0(preg_dir,"tab17_pregnancy.csv"), row.names = F)
  }
  if(!is.null(tab19_preg)){
    write.csv(tab19_preg, paste0(preg_dir,"tab19_pregnancy.csv"), row.names = F)
  }
  ########################################################
  #create masked files
  #######################################################
  flowchart[, EVENTS:= as.character(EVENTS)][as.numeric(EVENTS) > 0 & as.numeric(EVENTS) < 5, EVENTS := "<5"]
  flowchart[, MEDICAL_OBSERVATIONS:= as.character(MEDICAL_OBSERVATIONS)][as.numeric(MEDICAL_OBSERVATIONS) > 0 & as.numeric(MEDICAL_OBSERVATIONS) < 5, MEDICAL_OBSERVATIONS := "<5"]
  flowchart[, SURVEY_OBSERVATIONS:= as.character(SURVEY_OBSERVATIONS)][as.numeric(SURVEY_OBSERVATIONS) > 0 & as.numeric(SURVEY_OBSERVATIONS) < 5, SURVEY_OBSERVATIONS := "<5"]
  write.csv(flowchart, paste0(preg_dir,"Masked/", "flowchart_pregnancy_masked.csv"), row.names = F)
  
  if(description[4, 2]<5 & description[4, 2]>0) {description[4, 2]<-"<5"}
  if(description[4, 3]<5 & description[4, 3]>0) {description[4, 3]<-"<5"} 
  if(description[4, 4]<5 & description[4, 4]>0) {description[4, 4]<-"<5"} 
  write.csv(description, paste0(preg_dir,"Masked/", "description_pregnancy_masked.csv"), row.names = F)
  
  if(!is.null(tab17_preg)){
    tab17_preg[, person_years:= as.character(person_years)][as.numeric(person_years) > 0 & as.numeric(person_years) < 5, person_years := "<5"]
    tab17_preg[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
    tab17_preg[, rate_min_1_code_per_100_py:= as.character(rate_min_1_code_per_100_py)][no_records == "<5", rate_min_1_code_per_100_py := "N/A"]
    write.csv(tab17_preg, paste0(preg_dir,"Masked/", "tab17_pregnancy_masked.csv"), row.names = F)
  }
  
  if(!is.null(tab19_preg)){
    tab19_preg[, no_min_1_code:= as.character(no_min_1_code)][as.numeric(no_min_1_code) > 0 & as.numeric(no_min_1_code) < 5, no_min_1_code := "<5"]
    tab19_preg[, no_min_1_code_fup:= as.character(no_min_1_code_fup)][as.numeric(no_min_1_code_fup) > 0 & as.numeric(no_min_1_code_fup) < 5, no_min_1_code_fup := "<5"]
    tab19_preg[, min_1_code_fup_py:= as.character(min_1_code_fup_py)][as.numeric(min_1_code_fup_py) > 0 & as.numeric(min_1_code_fup_py) < 5, min_1_code_fup_py := "<5"]
    tab19_preg[, percentage_min_1_code_fup:= as.character(percentage_min_1_code_fup)][no_min_1_code_fup == "<5", percentage_min_1_code_fup := "N/A"]
    write.csv(tab19_preg, paste0(preg_dir,"Masked/", "tab19_pregnancy_masked.csv"), row.names = F)
  }
  
  do.call(file.remove, list(list.files(preg_ev_tmp, full.names = T)))
  do.call(file.remove, list(list.files(preg_m_tmp, full.names = T)))
  do.call(file.remove, list(list.files(preg_s_tmp, full.names = T)))
  
  rm(study_population, nr_std, flowchart_preg, description_preg, tab17_preg, tab19_preg)
  
}


#Delete folders events, so, mo from tmp
unlink(paste0(tmp,"PREGNANCY_EV"), recursive = T)
unlink(paste0(tmp,"PREGNANCY_MO"), recursive = T)
unlink(paste0(tmp,"PREGNANCY_SO"), recursive = T)


keep_environment<-c("g_intermediate", "StudyName", "data_access_provider_name","path_dir","projectFolder","subpopulations_names",
                    "METADATA_subp", "actual_tables", "data_source_name", "dir_base", "max_age_preg", "min_age_preg", "populations_dir",
                    "subpopulations_present", "date_creation", "output_dir", "pre_dir", "start_study_date","med_dir","medicines_tmp","medicines_pop",
                    "vacc_dir", "vaccines_tmp", "vaccines_pop", "Rmd_MEDICINES", "Rmd_VACCINES", "tmp", "s", "preg_dir", "events_tmp","mo_tmp","so_tmp", "diag_pop","Rmd_DIAGNOSES",
                    "preg_dir", "preg_ev_tmp","preg_m_tmp","preg_s_tmp", "preg_pop","Rmd_PREGNANCY")
list_rm<-ls()[ls() %!in% keep_environment]
rm(list = list_rm)
rm(list_rm)
`%!in%` = Negate(`%in%`)


