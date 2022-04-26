#Author: Roel Elbers Drs.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021


############################################################################################
#Get cdm_source file name
cdm_source_file<-list.files(path_dir, pattern="^CDM_SOURCE")
#Get DAP info and date createion fro CDM_SOURCE
CDM_SOURCE<-fread(paste0(path_dir, cdm_source_file))
date_creation<-CDM_SOURCE[,date_creation]
data_access_provider_name<-CDM_SOURCE[,data_access_provider_name]
data_source_name<-CDM_SOURCE[,data_source_name]
recommended_end_date <- as.IDate(as.character(CDM_SOURCE$recommended_end_date),"%Y%m%d")
rm(CDM_SOURCE, cdm_source_file)
############################################################################################

#########################################################
#date transformations
#######################################################
start_study_date <- as.IDate(start_study_date,"%Y%m%d")
end_study_date <- as.IDate(end_study_date,"%Y%m%d")
date_creation<-as.IDate(end_study_date,"%Y%m%d")
intv <- as.IDate(c(start_study_date, end_study_date))
##########################################################
print("Check date creation is after end study date")
end_study_date <- min(end_study_date,date_creation,recommended_end_date,na.rm = T)

start_study_date2 <- paste0(year(start_study_date),sprintf("%02d",month(start_study_date)),sprintf("%02d",day(start_study_date)))
end_study_date2 <- paste0(year(end_study_date),sprintf("%02d",month(end_study_date)),sprintf("%02d",day(end_study_date)))

#Set to IDate format


SelectionCriteria <- list(
  
  No_observation_time = expression(!is.na(num_spell) & !is.na(op_start_date) & !is.na(op_end_date)),
  No_op_start_date = expression(!is.na(op_start_date)),
  No_year_of_birth = expression(!is.na(year_of_birth)),
  No_year_of_death = expression(!(is.na(year_of_death) & (!is.na(day_of_death) | !is.na(month_of_death)))),
  OP_START_DATE_before_OP_END_DATE = expression(op_start_date < op_end_date),
  Study_Period_and_spell_overlap  = expression(op_start_date %between% intv| op_end_date %between% intv | (op_start_date  < start_study_date & op_end_date > end_study_date)),
  Spells_less_then_lookback_period = expression(op_end_date - op_start_date > lookback_period),
  Remaning_time_to_end_study_date_less_then_lookback_period = expression(end_study_date - op_start_date > lookback_period),
  #Remaning_time_to_end_study_date_less_then_lookback_period = expression(end_study_date - date_min > lookback_period),
  Remaning_time_to_date_max_less_then_lookback_period = expression(date_max - op_start_date > lookback_period),
  Age_min_to_end_of_study_above_0 = expression(end_study_date - date_min > 0),
  Start_of_study_Age_max_to_above_0 = expression(date_max - start_study_date > 0)
  
)
#Age_filter_spells = expression(age_op_start_date < Age_max & age_op_end_date > Age_min)
#Age_start_study = expression(age_start_study < Age_max & age_start_study >= Age_min),




#Check subpopulations from metadata
#Get metadata file name
metadata_file<-list.files(path_dir, pattern="^METADATA")
#Retrieve data from METADATA
METADATA<-fread(paste0(path_dir, metadata_file))

if(any(METADATA[["type_of_metadata"]]== "subpopulations")){
  
  if(!(METADATA[type_of_metadata == "subpopulations", values] == "" | is.na(METADATA[type_of_metadata == "subpopulations", values]))){
    SUBP <-T
    METADATA_subp<-METADATA[!type_of_metadata %in% c("presence_of_table", "presence_of_column", "list_of_values")]  
    
    #Get meanings_sets
    op_meaning_list_set<-METADATA[type_of_metadata=="op_meanings_list_per_set", c("other", "values")]
    names(op_meaning_list_set)<-c("op_meaning_sets", "op_meanings_list_per_set")
    
    #Get subpopulation_meanings
    subpopulation_meanings<-METADATA[type_of_metadata=="op_meaning_sets", c("other", "values")]
    names(subpopulation_meanings)<-c("subpopulations", "meaning_sets")
    
    library("stringr")
    subpopulations<-unlist(str_split(METADATA_subp[type_of_metadata=="subpopulations",values], pattern = " "))
    gc()
  } else SUBP <- FALSE
} else SUBP <- FALSE

rm(METADATA)
gc()

#Parameters for end analyses
Analyse_dates <- c("start_follow_up","end_follow_up","birth_date")
