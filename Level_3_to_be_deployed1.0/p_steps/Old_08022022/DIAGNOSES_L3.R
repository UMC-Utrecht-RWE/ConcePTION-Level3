#Author: Vjola Hoxhaj Drs.
#email: v.hoxhaj@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/07/2021

###############################################################################################
source(paste0(pre_dir, "DAP_info.R")) 
source(paste0(pre_dir, "info.R")) 
source(paste0(pre_dir, "create_conceptsets.R")) #for diagnoses
########################################################
#Create output folders
########################################################
#########################
#Diagnoses
#########################
if (subpopulations_present=="No"){
  #output folder for DIAGNOSES report in g_output
  if ("DIAGNOSES" %in% list.files(output_dir)){
   unlink(paste0(output_dir,"DIAGNOSES"), recursive = T)#delete folder
    dir.create(paste(output_dir, "DIAGNOSES", sep=""))
    diag_dir<-paste(output_dir, "DIAGNOSES/", sep="")
    dir.create(paste(diag_dir,"Masked", sep=""))
    diag_less<-paste(diag_dir, "Masked/", sep="")
  } else {
    #Create the DIAGNOSES folder in the output dir
    dir.create(paste(output_dir, "DIAGNOSES", sep=""))
    diag_dir<-paste(output_dir, "DIAGNOSES/", sep="")
    dir.create(paste(diag_dir,"Masked", sep=""))
    diag_less<-paste(diag_dir, "Masked/", sep="")
  }
  
  #output diagnoses_study_population in g_intermediate/populations/diagnoses
  if ("DIAGNOSES" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"DIAGNOSES"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "DIAGNOSES", sep=""))
    diag_pop<-paste(populations_dir, "DIAGNOSES/", sep="")
  } else {
    #Create the DIAGNOSES folder in the output dir
    dir.create(paste(populations_dir, "DIAGNOSES", sep=""))
    diag_pop<-paste(populations_dir, "DIAGNOSES/", sep="")
  }
  
  #EVENTS_tmp/EVENTS folder where all intermediary files are saved
  if ("EVENTS" %in% list.files(tmp)){
    events_tmp<-paste(tmp, "EVENTS/", sep="")
    do.call(file.remove, list(list.files(events_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the EVENTS folder in the output dir
    dir.create(paste(tmp, "EVENTS", sep=""))
    events_tmp<-paste(tmp, "EVENTS/", sep="")
  }
  
  #MO_tmp/MO folder where all intermediary files are saved
  if ("MO" %in% list.files(tmp)){
    mo_tmp<-paste(tmp, "MO/", sep="")
    do.call(file.remove, list(list.files(mo_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the MO folder in the output dir
    dir.create(paste(tmp, "MO", sep=""))
    mo_tmp<-paste(tmp, "MO/", sep="")
  }
  
  #SO_tmp/SO folder where all intermediary files are saved
  if ("SO" %in% list.files(tmp)){
    so_tmp<-paste(tmp, "SO/", sep="")
    do.call(file.remove, list(list.files(so_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the SO folder in the output dir
    dir.create(paste(tmp, "SO", sep=""))
    so_tmp<-paste(tmp, "SO/", sep="")
  }
  
  #DIAGNOSES_tmp/DIAGNOSES folder where all intermediary files are saved
  if ("DIAGNOSES" %in% list.files(tmp)){
    diag_tmp<-paste(tmp, "DIAGNOSES/", sep="")
    do.call(file.remove, list(list.files(diag_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the DIAGNOSES folder in the output dir
    dir.create(paste(tmp, "DIAGNOSES", sep=""))
    diag_tmp<-paste(tmp, "DIAGNOSES/", sep="")
  }
  
} else {
  #output folder for DIAGNOSES report in g_output
  if ("DIAGNOSES" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"DIAGNOSES"), recursive = T)#delete folder
    dir.create(paste(output_dir, "DIAGNOSES", sep=""))
    diag_dir<-paste(output_dir, "DIAGNOSES/", sep="")
    
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(diag_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(diag_dir, subpopulations_names[i],"/Masked"))
    }
  } else {
    #Create the DIAGNOSES folder in the output dir
    dir.create(paste(output_dir, "DIAGNOSES", sep=""))
    diag_dir<-paste(output_dir, "DIAGNOSES/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(diag_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(diag_dir, subpopulations_names[i],"/Masked"))
    }
  }
  
  #output diagnoses_study_population in g_intermediate/populations/diagnoses
  if ("DIAGNOSES" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"DIAGNOSES"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "DIAGNOSES", sep=""))
    diag_pop<-paste(populations_dir, "DIAGNOSES/",sep="")

    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(diag_pop, subpopulations_names[i]))
    }
  } else {
    #Create the DIAGNOSES folder in the output dir
    dir.create(paste(populations_dir, "DIAGNOSES", sep=""))
    diag_pop<-paste(populations_dir, "DIAGNOSES/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(diag_pop, subpopulations_names[i]))
    }
  }
  
  #EVENTS_tmp/EVENTS folder where all intermediary files are saved
  if ("EVENTS" %in% list.files(tmp)){
    events_tmp<-paste(tmp, "EVENTS/", sep="")
    do.call(file.remove, list(list.files(events_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the EVENTS folder in the output dir
    dir.create(paste(tmp, "EVENTS", sep=""))
    events_tmp<-paste(tmp, "EVENTS/", sep="")
  }
  
  #MO_tmp/MO folder where all intermediary files are saved
  if ("MO" %in% list.files(tmp)){
    mo_tmp<-paste(tmp, "MO/", sep="")
    do.call(file.remove, list(list.files(mo_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the MO folder in the output dir
    dir.create(paste(tmp, "MO", sep=""))
    mo_tmp<-paste(tmp, "MO/", sep="")
  }
  
  #SO_tmp/SO folder where all intermediary files are saved
  if ("SO" %in% list.files(tmp)){
    so_tmp<-paste(tmp, "SO/", sep="")
    do.call(file.remove, list(list.files(so_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the SO folder in the output dir
    dir.create(paste(tmp, "SO", sep=""))
    so_tmp<-paste(tmp, "SO/", sep="")
  }
  
  #DIAGNOSES_tmp/DIAGNOSES folder where all intermediary files are saved
  if ("DIAGNOSES" %in% list.files(tmp)){
    diag_tmp<-paste(tmp, "DIAGNOSES/", sep="")
    do.call(file.remove, list(list.files(diag_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the DIAGNOSES folder in the output dir
    dir.create(paste(tmp, "DIAGNOSES", sep=""))
    diag_tmp<-paste(tmp, "DIAGNOSES/", sep="")
  }
  
}

###############################################################

if(sum(length(actual_tables$EVENTS),length(actual_tables$MEDICAL_OBSERVATIONS),length(actual_tables$SURVEY_OBSERVATIONS))>0){
####################################################################################################
#Main script
####################################################################################################
if (subpopulations_present=="Yes"){
for (s in 1: length(subpopulations_names)){
  study_sub_population<-study_population_dir[grepl(study_population_dir, pattern=paste0(subpopulations_names[s],"_study_population"), fixed=T)]
  study_sub_population<-study_sub_population[grepl(study_sub_population, pattern=paste0("^", subpopulations_names[s]))]
  study_population<-readRDS(paste0(g_intermediate, "populations/", study_sub_population))[,c("person_id","sex_at_instance_creation","birth_date","end_follow_up","start_follow_up","age_start_follow_up")]
  nr_std<-study_population[,.N]

    #MEANINGS TO BE EXCLUDED
  meanings_exclude_events<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="EVENTS" & other==subpopulations_names[s],values], pattern = " "))
  meanings_exclude_mo<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="MEDICAL_OBSERVATIONS" & other==subpopulations_names[s],values], pattern = " "))
  meanings_exclude_so<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="SURVEY_OBSERVATIONS" & other==subpopulations_names[s],values], pattern = " "))

  source(paste0(pre_dir, "DIAGNOSES_L3_pre_script_2.R"))
  
  do.call(file.remove, list(list.files(events_tmp, full.names = T)))
  do.call(file.remove, list(list.files(mo_tmp, full.names = T)))
  do.call(file.remove, list(list.files(so_tmp, full.names = T)))
  do.call(file.remove, list(list.files(diag_tmp, full.names = T)))

  rm(study_population, nr_std)
  }
} else {
  study_population_dir<-study_population_dir[grepl(study_population_dir, pattern="ALL_study_population", fixed=T)]
  study_population<-readRDS(paste0(g_intermediate, "populations/", study_population_dir))[,c("person_id","sex_at_instance_creation","birth_date","end_follow_up","start_follow_up","age_start_follow_up")]
  nr_std<-study_population[,.N]

  #MEANINGS TO BE EXCLUDED
  meanings_exclude_events<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="EVENTS",values], pattern = " "))
  meanings_exclude_mo<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="MEDICAL_OBSERVATIONS", values], pattern = " "))
  meanings_exclude_so<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="SURVEY_OBSERVATIONS", values], pattern = " "))
  
  source(paste0(pre_dir, "DIAGNOSES_L3_pre_script_2.R"))
 
  do.call(file.remove, list(list.files(events_tmp, full.names = T)))
  do.call(file.remove, list(list.files(mo_tmp, full.names = T)))
  do.call(file.remove, list(list.files(so_tmp, full.names = T)))
  do.call(file.remove, list(list.files(diag_tmp, full.names = T)))
  
  rm(study_population, nr_std)
  
}
}

#Delete folders events, so, mo from tmp
unlink(paste0(tmp,"EVENTS"), recursive = T)
unlink(paste0(tmp,"MO"), recursive = T)
unlink(paste0(tmp,"SO"), recursive = T)
unlink(paste0(tmp,"DIAGNOSES"), recursive = T)


keep_environment<-c("StudyName", "data_access_provider_name", "data_source_name", "subpopulations_names", "subpopulations_present", "subpopulations","SUBP",
                    "Age_max","Age_min", "min_age_preg","max_age_preg", 
                    "start_study_date","end_study_date", "lookback_period", "intv","recommended_end_date", "date_creation","start_study_date2","end_study_date2",
                    "METADATA_subp", "actual_tables", "tmp", "s", "meanings_birth_registry", "CountPersonTime2",
                    "diagnoses","pregnancies","diagnoses_pregnancy_med_vacc","diagnoses_pregnancy_med","diagnoses_pregnancy_vacc","pregnancy_only_med_vacc","pregnancy_only_med","pregnancy_only_vacc",
                    "dir_base", "populations_dir", "output_dir", "pre_dir", "study_population_dir", "g_intermediate", "path_dir","projectFolder","path","output_path",
                    "med_dir","medicines_tmp","medicines_pop", "Rmd_MEDICINES",
                    "vacc_dir", "vaccines_tmp", "vaccines_pop","Rmd_VACCINES",
                    "diag_dir", "events_tmp","mo_tmp","so_tmp","diag_tmp", "diag_pop","Rmd_DIAGNOSES", "conditions","diagnoses")


list_rm<-ls()[ls() %!in% keep_environment]
rm(list = list_rm)
rm(list_rm)
`%!in%` = Negate(`%in%`)


