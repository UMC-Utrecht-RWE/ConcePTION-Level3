#Author: Vjola Hoxhaj Drs.
#email: v.hoxhaj@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/07/2021

##############################################################################################
source(paste0(pre_dir, "DAP_info.R"))
source(paste0(pre_dir, "info.R"))
source(paste0(pre_dir,"date_parameters.R"))
source(paste0(pre_dir,"/functions/create_age_band.R"))
###############################################################################################
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

########################################################
#Create output folders
########################################################
if (subpopulations_present=="No"){
  #output folder for MEDICINES report in g_output
  if ("MEDICINES" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"MEDICINES"), recursive = T)#delete folder
    dir.create(paste(output_dir, "MEDICINES", sep=""))
    med_dir<-paste(output_dir, "MEDICINES/", sep="")
    dir.create(paste(med_dir,"Masked", sep=""))
    med_less<-paste(med_dir, "Masked/", sep="")
  } else {
    #Create the MEDICINES folder in the output dir
    dir.create(paste(output_dir, "MEDICINES", sep=""))
    med_dir<-paste(output_dir, "MEDICINES/", sep="")
    dir.create(paste(med_dir,"Masked", sep=""))
    med_less<-paste(med_dir, "Masked/", sep="")
  }
  
  #output medicines_study_population in g_intermediate/populations/medicines
  if ("MEDICINES" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"MEDICINES"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "MEDICINES", sep=""))
    medicines_pop<-paste(populations_dir, "MEDICINES/", sep="")
  } else {
    #Create the MEDICINES folder in the output dir
    dir.create(paste(populations_dir, "MEDICINES", sep=""))
    medicines_pop<-paste(populations_dir, "MEDICINES/", sep="")
  }
  
  #MEDICINES_tmp/MEDICINES folder where all intermediary files are saved
  if ("MEDICINES" %in% list.files(tmp)){
      unlink(paste0(tmp,"MEDICINES"), recursive = T)#delete folder
    dir.create(paste(tmp, "MEDICINES", sep=""))
    medicines_tmp<-paste(tmp, "MEDICINES/", sep="")
  }else{
    #Create the MEDICINES folder in the output dir
    dir.create(paste(tmp, "MEDICINES", sep=""))
    medicines_tmp<-paste(tmp, "MEDICINES/", sep="")
  }
  
} else {
  #output folder for MEDICINES report in g_output
  if ("MEDICINES" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"MEDICINES"), recursive = T)#delete folder
    dir.create(paste(output_dir, "MEDICINES", sep=""))
    med_dir<-paste(output_dir, "MEDICINES/", sep="")
    
    do.call(file.remove, list(list.files(med_dir, full.names = T)))
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(med_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(med_dir, subpopulations_names[i],"/Masked"))
    }
  } else {
    #Create the MEDICINES folder in the output dir
    dir.create(paste(output_dir, "MEDICINES", sep=""))
    med_dir<-paste(output_dir, "MEDICINES/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(med_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(med_dir, subpopulations_names[i],"/Masked"))
    }
  }
  
  #output medicines_study_population in g_intermediate/populations/diagnoses
  if ("MEDICINES" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"MEDICINES"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "MEDICINES", sep=""))
    medicines_pop<-paste(populations_dir, "MEDICINES/",sep="")
    do.call(file.remove, list(list.files(medicines_pop, full.names = T)))
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(medicines_pop, subpopulations_names[i]))
    }
  } else {
    #Create the MEDICINES folder in the output dir
    dir.create(paste(populations_dir, "MEDICINES", sep=""))
    medicines_pop<-paste(populations_dir, "MEDICINES/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(medicines_pop, subpopulations_names[i]))
    }
  }
  
  #MEDICINES_tmp/MEDICINES folder where all intermediary files are saved
  if ("MEDICINES" %in% list.files(tmp)){
    unlink(paste0(tmp,"MEDICINES"), recursive = T)#delete folder
    dir.create(paste(tmp, "MEDICINES", sep=""))
    medicines_tmp<-paste(tmp, "MEDICINES/", sep="")
  }else{
    #Create the MEDICINES folder in the output dir
    dir.create(paste(tmp, "MEDICINES", sep=""))
    medicines_tmp<-paste(tmp, "MEDICINES/", sep="")
  }
  
}
########################################################
if(length(actual_tables$MEDICINES)>0){
####################################################################################################
#Main script
####################################################################################################
if (subpopulations_present=="Yes"){
  for (s in 1: length(subpopulations_names)){
    study_sub_population<-study_population_dir[grepl(study_population_dir, pattern=paste0(subpopulations_names[s],"_study_population"), fixed=T)]
    study_sub_population<-study_sub_population[grepl(study_sub_population, pattern=paste0("^", subpopulations_names[s]))]
    study_population<-readRDS(paste0(g_intermediate, "populations/", study_sub_population))[,c("person_id","sex_at_instance_creation","birth_date","end_follow_up","start_follow_up","age_start_follow_up")]
    study_population<-study_population[,person_id:=as.character(person_id)]
    study_population<-study_population[sex_at_instance_creation %in% c("F","M")]
    study_population[,medicines_rec:=0]
    nr_std<-study_population[,.N]
    #MEANINGS TO BE EXCLUDED
    meanings_exclude_med<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="MEDICINES" & other==subpopulations_names[s],values], pattern = " "))
    
    source(paste0(pre_dir, "Step_08_01_MEDICINES_L3_pre_script_2.R"))
    source(paste0(pre_dir, "Step_08_02_MEDICINES_L3_counts_new.R"))
    source(paste0(pre_dir, "Step_08_03_MEDICINES_L3_rates_new.R"))
    
    #clean medicines_tmp
    for(i in 1:length(list.files(medicines_tmp))){
      unlink(paste0(medicines_tmp,list.files(medicines_tmp)[i]))
    }
    
    rm(nr_std)
  }
} else {
  study_population_dir<-study_population_dir[grepl(study_population_dir, pattern="ALL_study_population", fixed=T)]
  study_population<-readRDS(paste0(g_intermediate, "populations/", study_population_dir))[,c("person_id","sex_at_instance_creation","birth_date","end_follow_up","start_follow_up","age_start_follow_up")]
  study_population<-study_population[,person_id:=as.character(person_id)]
  study_population<-study_population[sex_at_instance_creation %in% c("F","M")]
  nr_std<-study_population[,.N]
  study_population[,medicines_rec:=0]
  
  #MEANINGS TO BE EXCLUDED
  meanings_exclude_med<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="MEDICINES",values], pattern = " "))

  source(paste0(pre_dir, "Step_08_01_MEDICINES_L3_pre_script_2.R"))
  source(paste0(pre_dir, "Step_08_02_MEDICINES_L3_counts_new.R"))
  source(paste0(pre_dir, "Step_08_03_MEDICINES_L3_rates_new.R"))

  do.call(file.remove, list(list.files(medicines_tmp, full.names = T)))
  
  rm(nr_std)
  
}
}

#Delete folders events, so, mo from tmp
unlink(paste0(tmp,"MEDICINES"), recursive = T)


keep_environment<-c("StudyName", "data_access_provider_name", "data_source_name", "subpopulations_names", "subpopulations_present", "subpopulations","SUBP",
                    "Age_max","Age_min", "min_age_preg","max_age_preg",
                    "start_study_date","end_study_date", "lookback_period", "intv","recommended_end_date", "date_creation","start_study_date2","end_study_date2",
                    "METADATA_subp", "actual_tables", "tmp", "s", "meanings_birth_registry","CountPersonTime2",
                    "diagnoses","pregnancies","diagnoses_pregnancy_med_vacc","diagnoses_pregnancy_med","diagnoses_pregnancy_vacc","pregnancy_only_med_vacc","pregnancy_only_med","pregnancy_only_vacc",
                    "dir_base", "populations_dir", "output_dir", "pre_dir", "study_population_dir", "g_intermediate", "path_dir","projectFolder","path","path_output",
                    "med_dir","medicines_tmp","medicines_pop", "Rmd_MEDICINES",
                    "vacc_dir", "vaccines_tmp", "vaccines_pop","Rmd_VACCINES",
                    "diag_dir", "events_tmp","mo_tmp","so_tmp","diag_tmp", "diag_pop","Rmd_DIAGNOSES", "conditions","diagnoses","duplicated_event_dates","remove_subj",
                    "preg_dir","preg_ev_tmp","preg_m_tmp","preg_s_tmp","preg_si_tmp","preg_tmp", "preg_pop","Rmd_PREGNANCY","pregnancies",
                    "poi_dir","ev_preg_dir","med_preg_dir","vacc_preg_dir","ev_med_preg_dir","ev_vacc_preg_dir","ev_med_dir","ev_vacc_dir","ev_preg_pop","med_preg_pop","vacc_preg_pop","ev_med_preg_pop","ev_vacc_preg_pop","ev_med_pop","ev_vacc_pop","poi_tmp","Rmd_POI")
list_rm<-ls()[ls() %!in% keep_environment]
rm(list = list_rm)
rm(list_rm)
`%!in%` = Negate(`%in%`)








