rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

########################################################################################################################
#Specify all meanings that refer to birth registry in the SURVEY_ID table(if applicable) for identifying pregnancy
########################################################################################################################
#example meanings_birth_registry<-c("birth_registry", "birth_registry_meanings")
#keep in mind this records will be classified as end_of_pregnancy so no spontaneous abortion registry should be included
meanings_birth_registry<-c("birth_registry")
########################################################################################################################
#Specify all variables of interest to generate the Lifestyle report
########################################################################################################################
#Variables of interest:Smoking, Folic acid use, Alcohol abuse, BMI, SES
#1.Identify the CDM table you used to save the information about the variables of interest.
#2.Identify the original name of the variable of interest.
#3.Use the information above to complete the list below.
#4.CDM_table:name of the CDM table where you saved the information.
#5.CDM_column: name of the CDM column where you saved the information about the name of the variable of interest.
#6.value: name of the original variable.
#7.c.voc: name of the CDM column where you saved the vocabulary representing the variable of interest.If no vocabulary fill NULL
#8.v.voc: the vocabulary used for the variable of interest.If no vocabulary fill NULL
#9.v.date: name of the CDM column which saves the date of recording.
#10.If you don't have information about a variable then delete that section and use Lifestyle <- list()

#example BMI(saved in MEDICAL_OBSERVATIONS, original name: body_mass_index, unit:kg/m2)
#  BMI = list(
#    CDM_table = "MEDICAL_OBSERVATIONS",
#    CDM_column = "mo_source_column",
#    value = "body_mass_index",
#    c.voc = "mo_record_vocabulary",
#    v.voc = "ICD9",
#    v.date = "mo_date"
#  )

#Smoking = list(
#  CDM_table = "SURVEY_OBSERVATIONS",
#  CDM_column = "so_source_column",
#  value = c("SMOKING","SMOKESEV"),
#  c.voc = NULL,
#  v.voc = NULL,
#  v.date = "so_date"
#)

Lifestyle <- list()

# Lifestyle <- list(
#   Smoking = list(
#     CDM_table = "",
#     CDM_column = "",
#     value = "",
#     c.voc = "",
#     v.voc = "",
#     v.date = ""
#   ),
#   Folic_acid = list(
#     CDM_table = "",
#     CDM_column = "",
#     value = "",
#     c.voc = "",
#     v.voc = "",
#     v.date = ""
#   ),
#   Alcohol = list(
#     CDM_table = "",
#     CDM_column = "",
#     value = "",
#     c.voc = "",
#     v.voc = "",
#     v.date = ""
#   ),
#   BMI = list(
#     CDM_table = "",
#     CDM_column = "",
#     value = c(""),
#     c.voc = "" ,
#     v.voc = "",
#     v.date = ""
#   ),
#   SES = list(
#     CDM_table = "",
#     CDM_column = "",
#     value = c(""),
#     c.voc = "" ,
#     v.voc = "",
#     v.date = ""
#   )
# )
###############################################
source("packages.R")
source("99_path.R")
source(paste0(pre_dir, "info.R"))
source(paste0(pre_dir,"study_parameters.R"))
setwd(projectFolder)

#################################################
#Study_source_population
#################################################
system.time(source(paste0(pre_dir,"study_source_population_script.R")))

#Create report
for(i in readRDS(paste0(std_pop_tmp,"SCHEME_06.rds"))[["subpopulations"]]){
  
  if(SUBP) {
    report_dir1 <- paste0(std_source_pop_dir,i)
    report_dir2 <- paste0(std_source_pop_dir,i,"/Masked")
    
  }else{
    report_dir1 <- substr(std_source_pop_dir,1,nchar(std_source_pop_dir)-1)
    report_dir2 <- paste0(std_source_pop_dir,"Masked")
  }
  
  rmarkdown::render(paste0(pre_dir,"Report_01_StudyPopulation.Rmd"),
                    output_file = paste0(report_dir1,"/","Report_01_Study_population_",i,".html"),
                    output_dir = report_dir1
  )
  
  rmarkdown::render(paste0(pre_dir,"Report_02_Dates.Rmd"),
                    output_file = paste0(report_dir1,"/","Report_02_Dates_",i,".html"),
                    output_dir = report_dir1
  )
  
  rmarkdown::render(paste0(pre_dir,"Report_03_VisitsLifestyle.Rmd"),
                    output_file = paste0(report_dir1,"/","Report_03_VisitsLifestyle_",i,".html"),
                    output_dir = report_dir1
  )
  
  
  rm(report_dir1,report_dir2)
}
source(paste0(pre_dir,"save_environment.R"))
####################################################
#Medicine exposure
####################################################
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source("packages.R")
source("99_path.R")
load(paste0(g_intermediate,"environment.RData"))
system.time(source(paste0(pre_dir,"Step_08_00_MEDICINES_L3.R")))

if(length(actual_tables$MEDICINES)>0){
  if(subpopulations_present=="No"){
    system.time(render(paste0(pre_dir,"/Report_08_MEDICINES_Overview_Completeness_L3.Rmd"), output_dir = paste0(output_dir,"MEDICINES/"), output_file = "MEDICINES_Overview_Completeness_L3.html")) 
    system.time(render(paste0(pre_dir,"/Report_08_MEDICINES_Counts_L3.Rmd"), output_dir = paste0(output_dir,"MEDICINES/"), output_file = "MEDICINES_Counts_L3.html")) 
    system.time(render(paste0(pre_dir,"/Report_08_MEDICINES_Rates_L3.Rmd"), output_dir = paste0(output_dir,"MEDICINES/"), output_file = "MEDICINES_Rates_L3.html")) 
    
  } else {
    for (a in 1: length(subpopulations_names)){
      system.time(render(paste0(pre_dir,"/Report_08_MEDICINES_Overview_Completeness_L3.Rmd"), output_dir = paste0(output_dir,"MEDICINES/"), output_file = paste0(subpopulations_names[a],"_MEDICINES_Overview_Completeness_L3.html")))  
    }
    for (a in 1: length(subpopulations_names)){
      system.time(render(paste0(pre_dir,"/Report_08_MEDICINES_Counts_L3.Rmd"), output_dir = paste0(output_dir,"MEDICINES/"), output_file = paste0(subpopulations_names[a],"_MEDICINES_Counts_L3.html")))  
    }
    for (a in 1: length(subpopulations_names)){
      system.time(render(paste0(pre_dir,"/Report_08_MEDICINES_Rates_L3.Rmd"), output_dir = paste0(output_dir,"MEDICINES/"), output_file = paste0(subpopulations_names[a],"_MEDICINES_Rates_L3.html")))  
    }
  }
}
source(paste0(pre_dir,"save_environment.R"))
##################################################
#Vaccine exposure
##################################################
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source("packages.R")
source("99_path.R")
load(paste0(g_intermediate,"environment.RData"))

system.time(source(paste0(pre_dir,"Step_09_00_VACCINES_L3.R")))

if(length(actual_tables$VACCINES)>0){
  if(subpopulations_present=="No"){
    system.time(render(paste0(pre_dir,"/Report_09_VACCINES_Overview_Completeness_L3.Rmd"), output_dir = paste0(output_dir,"VACCINES/"), output_file = "VACCINES_Overview_Completeness_L3.html")) 
    system.time(render(paste0(pre_dir,"/Report_09_VACCINES_Counts_L3.Rmd"), output_dir = paste0(output_dir,"VACCINES/"), output_file = "VACCINES_Counts_L3.html")) 
    system.time(render(paste0(pre_dir,"/Report_09_VACCINES_Rates_L3.Rmd"), output_dir = paste0(output_dir,"VACCINES/"), output_file = "VACCINES_Rates_L3.html")) 
    
  } else {
    for (a in 1: length(subpopulations_names)){
      system.time(render(paste0(pre_dir,"/Report_09_VACCINES_Overview_Completeness_L3.Rmd"), output_dir = paste0(output_dir,"VACCINES/"), output_file = paste0(subpopulations_names[a],"_VACCINES_Overview_Completeness_L3.html")))  
    }
    for (a in 1: length(subpopulations_names)){
      system.time(render(paste0(pre_dir,"/Report_09_VACCINES_Counts_L3.Rmd"), output_dir = paste0(output_dir,"VACCINES/"), output_file = paste0(subpopulations_names[a],"_VACCINES_Counts_L3.html")))  
    }
    for (a in 1: length(subpopulations_names)){
      system.time(render(paste0(pre_dir,"/Report_09_VACCINES_Rates_L3.Rmd"), output_dir = paste0(output_dir,"VACCINES/"), output_file = paste0(subpopulations_names[a],"_VACCINES_Rates_L3.html")))  
    }
  }
}
source(paste0(pre_dir,"save_environment.R"))
#################################################
#Diagnoses
#################################################
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source("packages.R")
source("99_path.R")
study_name_codelist<-NULL
recurrent_event_analysis<-"Yes"
load(paste0(g_intermediate,"environment.RData"))
system.time(source(paste0(pre_dir,"Step_10_00_DIAGNOSES_L3.R")))


if(sum(length(actual_tables$EVENTS),length(actual_tables$MEDICAL_OBSERVATIONS),length(actual_tables$SURVEY_OBSERVATIONS))>0){
  if(subpopulations_present=="No"){
    system.time(render(paste0(pre_dir,"/Report_10_DIAGNOSES_Overview_Completeness_L3.Rmd"), output_dir = paste0(output_dir,"DIAGNOSES/"), output_file = "DIAGNOSES_Overview_Completeness_L3.html")) 
    system.time(render(paste0(pre_dir,"/Report_10_DIAGNOSES_Counts_L3.Rmd"), output_dir = paste0(output_dir,"DIAGNOSES/"), output_file = "DIAGNOSES_Counts_L3.html")) 
    system.time(render(paste0(pre_dir,"/Report_10_DIAGNOSES_Rates_L3.Rmd"), output_dir = paste0(output_dir,"DIAGNOSES/"), output_file = "DIAGNOSES_Rates_L3.html")) 
    
  } else {
    for (a in 1: length(subpopulations_names)){
      system.time(render(paste0(pre_dir,"/Report_10_DIAGNOSES_Overview_Completeness_L3.Rmd"), output_dir = paste0(output_dir,"DIAGNOSES/"), output_file = paste0(subpopulations_names[a],"_DIAGNOSES_Overview_Completeness_L3.html")))  
    }
    for (a in 1: length(subpopulations_names)){
      system.time(render(paste0(pre_dir,"/Report_10_DIAGNOSES_Counts_L3.Rmd"), output_dir = paste0(output_dir,"DIAGNOSES/"), output_file = paste0(subpopulations_names[a],"_DIAGNOSES_Counts_L3.html")))  
    }
    for (a in 1: length(subpopulations_names)){
      system.time(render(paste0(pre_dir,"/Report_10_DIAGNOSES_Rates_L3.Rmd"), output_dir = paste0(output_dir,"DIAGNOSES/"), output_file = paste0(subpopulations_names[a],"_DIAGNOSES_Rates_L3.html")))  
    }
  }
}

source(paste0(pre_dir,"save_environment.R"))
#################################################
#Pregnancy
#################################################
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source("packages.R")
source("99_path.R")
load(paste0(g_intermediate,"environment.RData"))
Rmd_PREGNANCY<-paste0(pre_dir,"/PREGNANCY_L3.Rmd")
source(paste0(pre_dir,"PREGNANCY_L3.R"))

if(sum(length(actual_tables$EVENTS),length(actual_tables$MEDICAL_OBSERVATIONS),length(actual_tables$SURVEY_OBSERVATIONS), length(actual_tables$SURVEY_ID))>0){
  if(subpopulations_present=="No"){
    system.time(render(Rmd_PREGNANCY, output_dir = paste0(output_dir,"PREGNANCY/"), output_file = "PREGNANCY_L3.html")) 
  } else {
    for (a in 1: length(subpopulations_names)){
      system.time(render(Rmd_PREGNANCY, output_dir = paste0(output_dir,"PREGNANCY/"), output_file = paste0(subpopulations_names[a],"_PREGNANCY_L3.html")))  
    }
  }
}
source(paste0(pre_dir,"save_environment.R"))
#################################################
#Populations of interest
#################################################
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source("packages.R")
source("99_path.R")
load(paste0(g_intermediate,"environment.RData"))
Rmd_POI<-paste0(pre_dir,"/POI_L3.Rmd")
system.time(source(paste0(pre_dir,"POI_L3.R")))

if(subpopulations_present=="No"){
  system.time(render(Rmd_POI, output_dir = paste0(output_dir,"POI/"), output_file = "POI_L3.html")) 
} else {
  for (a in 1: length(subpopulations_names)){
    system.time(render(Rmd_POI, output_dir = paste0(output_dir,"POI/"), output_file = paste0(subpopulations_names[a],"_POI_L3.html")))  
  }
}
source(paste0(pre_dir,"save_environment.R"))

#################################################
#EUROCAT INDICATORS
#################################################
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source("packages.R")
source("99_path.R")
load(paste0(g_intermediate,"environment.RData"))
Rmd_EUROCAT<-paste0(pre_dir,"/EUROCAT_DQI_L3.Rmd")
system.time(source(paste0(pre_dir,"eurocat_dqi.R")))

if(subpopulations_present=="No"){
  system.time(render(Rmd_EUROCAT, output_dir = paste0(output_dir,"EUROCAT/"), output_file = "EUROCAT_DQI_L3.html")) 
} else {
  for (a in 1: length(subpopulations_names)){
    system.time(render(Rmd_EUROCAT, output_dir = paste0(output_dir,"EUROCAT/"), output_file = paste0(subpopulations_names[a],"_EUROCAT_DQI_L3.html")))  
  }
}
source(paste0(pre_dir,"save_environment.R"))

####################################################
#Create ForDashboard folder
####################################################
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

source("packages.R")
source(paste0(pre_dir,"for_dashboard.R"))


