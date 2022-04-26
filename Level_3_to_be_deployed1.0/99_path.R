#Directory
setwd('..') #in Data Characterisation
setwd('..') #in ConcePTION
dir_base<-getwd()
# set the name of the study
StudyName <- "ConcePTION"
path_dir<-paste0(dir_base,"/CDMInstances/",StudyName,"/")
path<-path_dir

#Set the path to where you want your report to be saved(make sure that the output folder already exists)
output_dir<-paste0(projectFolder,"/g_output/")
path_output<-output_dir

pre_dir<-paste0(projectFolder,"/p_steps/")

g_intermediate<-paste0(projectFolder,"/g_intermediate/")
tmp<-paste0(g_intermediate,"tmp/")
populations_dir<-paste0(g_intermediate,"populations/")

eurocat_dir<-paste0(output_dir,"EUROCAT/")