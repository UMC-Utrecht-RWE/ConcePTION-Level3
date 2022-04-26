
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


#Set parameters basic parameters
start_study_date <- "19950101"
end_study_date <- "20211231"

#########################################################
#date transformations
#######################################################
start_study_date <- as.IDate(start_study_date,"%Y%m%d")
end_study_date <- as.IDate(end_study_date,"%Y%m%d")
date_creation<-as.IDate(as.character(date_creation),"%Y%m%d")
print("Check date creation is after end study date")
end_study_date <- min(end_study_date,date_creation,recommended_end_date,na.rm = T)

start_study_date2 <- paste0(year(start_study_date),sprintf("%02d",month(start_study_date)),sprintf("%02d",day(start_study_date)))
end_study_date2 <- paste0(year(end_study_date),sprintf("%02d",month(end_study_date)),sprintf("%02d",day(end_study_date)))

if(end_study_date < as.IDate(paste0(year(end_study_date),"1231"), "%Y%m%d")){
  end_study_rates<-paste0(year(end_study_date)-1, "1231")
} else {
  end_study_rates<-end_study_date2 
}

##########################################################

agebands_rates<-c(0,11,19,29,39,49,55)
agebands_rates_pregnancy<-c(12,29,39,49)
