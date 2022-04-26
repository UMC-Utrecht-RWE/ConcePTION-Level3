#Author: Vjola Hoxhaj Drs.
#email: v.hoxhaj@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/07/2021

`%!in%` = Negate(`%in%`)
#Get EVENTS, MO, SO, MEDICINES, VACCINES tables
actual_tables<-list()
actual_tables$EVENTS<-list.files(path_dir, pattern="^EVENTS")
actual_tables$MEDICAL_OBSERVATIONS<-list.files(path_dir, pattern="^MEDICAL_OBSERVATIONS")
actual_tables$SURVEY_OBSERVATIONS<-list.files(path_dir, pattern="^SURVEY_OBSERVATIONS")
actual_tables$MEDICINES<-list.files(path_dir, pattern="^MEDICINES")
actual_tables$VACCINES<-list.files(path_dir, pattern="^VACCINES")
actual_tables$SURVEY_ID<-list.files(path_dir, pattern="^SURVEY_ID")
actual_tables$EUROCAT<-list.files(path_dir, pattern="^EUROCAT")

if(sum(length(actual_tables$EVENTS), length(actual_tables$MEDICAL_OBSERVATIONS), length(actual_tables$SURVEY_OBSERVATIONS))==0){
  #no diagnoses can be retrieved
  diagnoses<-FALSE
  diagnoses_pregnancy_med_vacc<-FALSE
  diagnoses_pregnancy_vacc<-FALSE
  diagnoses_pregnancy_med<-FALSE
  if(length(actual_tables$SURVEY_ID)>0){
    #pregnancies can be retrieved
    pregnancies<-TRUE
    if(length(actual_tables$MEDICINES)>0){
      if(length(actual_tables$VACCINES)>0){
    pregnancy_only_med_vacc<-TRUE
    pregnancy_only_med<-TRUE
    pregnancy_only_vacc<-TRUE
      } else {
        pregnancy_only_med_vacc<-FALSE
        pregnancy_only_med<-TRUE 
        pregnancy_only_vacc<-FALSE
      }
    } else {
      if(length(actual_tables$VACCINES)>0){
        pregnancy_only_med_vacc<-FALSE
        pregnancy_only_vacc<-TRUE
        pregnancy_only_med<-FALSE
      } else {
        pregnancy_only_med_vacc<-FALSE
        pregnancy_only_vacc<-FALSE
        pregnancy_only_med<-FALSE 
      }
  }
  } else {
    pregnancies<-FALSE
  }
} else {
  diagnoses<-TRUE
  pregnancies<-TRUE
  pregnancy_only_med_vacc<-FALSE
  pregnancy_only_vacc<-FALSE
  pregnancy_only_med<-FALSE 
  
  if(length(actual_tables$MEDICINES)>0){
    if(length(actual_tables$VACCINES)>0){
    diagnoses_pregnancy_med_vacc<-TRUE
    diagnoses_pregnancy_med<-TRUE 
    diagnoses_pregnancy_vacc<-TRUE 
    } else {
      diagnoses_pregnancy_med_vacc<-FALSE
      diagnoses_pregnancy_med<-TRUE 
      diagnoses_pregnancy_vacc<-FALSE 
    }
  } else {
    if(length(actual_tables$VACCINES)>0){
      diagnoses_pregnancy_med_vacc<-FALSE
      diagnoses_pregnancy_vacc<-TRUE
      diagnoses_pregnancy_med<-FALSE
     } else {
      diagnoses_pregnancy_med_vacc<-FALSE
      diagnoses_pregnancy_vacc<-FALSE
      diagnoses_pregnancy_med<-FALSE
     }
  }
}

#pregnancies: if TRUE pregnancies can be retrieved
#diagnoses: if TRUE diagnoses can be retrieved
#diagnoses_pregnancy_med_vacc: if TRUE both medicines and vaccine exposure can be estimated
#diagnoses_pregnancy_vacc: if TRUE vaccine exposure can be estimated
#diagnoses_pregnancy_med: if TRUE medicine exposure can be estimated
#pregnancy_only_med_vacc: if TRUE only medicine and vaccine exposure in pregnancy can be estimated
#pregnancy_only_med: if TRUE only medicine exposure in pregnancy can be estimated
#pregnancy_only_vacc: if TRUE only vaccine exposure in pregnancy can be estimated

######################################
#METADATA(PRESENCE OF SUBPOPULATIONS)
######################################
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


#######################################################
#study population directory
#######################################################
#load study_population
study_population_dir<-list.files(paste0(g_intermediate,"populations/"), pattern="study_population")




