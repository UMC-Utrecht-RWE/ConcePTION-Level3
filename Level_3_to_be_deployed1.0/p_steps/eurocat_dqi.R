#Author: Ema Alsina/Vjola Hoxhaj Drs.
#email: palsinaaer@gmail.com/v.hoxhaj@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 23/08/2021

###############################################################################################
source(paste0(pre_dir, "DAP_info.R")) 
source(paste0(pre_dir, "info.R")) 

###################################################
#EUROCAT DQI
###################################################

##################################################
#Create output folders
##################################################
  #output folder for EUROCAT report in g_output
  if ("EUROCAT" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"EUROCAT"), recursive = TRUE)#delete folder
    dir.create(paste(output_dir, "EUROCAT", sep=""))
    eurocat_dir<-paste(output_dir, "EUROCAT/", sep="")
    dir.create(paste(eurocat_dir,"Masked", sep=""))
    eurocat_less<-paste(eurocat_dir, "Masked/", sep="")
  } else {
    #Create the EUROCAT folder in the output dir
    dir.create(paste(output_dir, "EUROCAT", sep=""))
    eurocat_dir<-paste(output_dir, "EUROCAT/", sep="")
    dir.create(paste(eurocat_dir,"Masked", sep=""))
    eurocat_less<-paste(eurocat_dir, "Masked/", sep="")
  }
  
  #EUROCAT_tmp/EUROCAT folder where all intermediary files are saved
  if ("EUROCAT" %in% list.files(tmp)){
    eurocat_tmp<-paste(tmp, "EUROCAT/", sep="")
    do.call(file.remove, list(list.files(eurocat_tmp, full.names = TRUE)))#clean folder completely
  }else{
    #Create the EUROCAT folder in the output dir
    dir.create(paste(tmp, "EUROCAT", sep=""))
    eurocat_tmp<-paste(tmp, "EUROCAT/", sep="")
  }
  
###################################################

if(length(actual_tables$EUROCAT)>0){
  ####################################################################################################
  #Main script
  ####################################################################################################
  
  source(paste0(pre_dir, "Step1_FLOWCHART.R"))
  source(paste0(pre_dir,"Step2_ascert_accuracy.R"))
  source(paste0(pre_dir,"step3_complete.R"))
  
  do.call(file.remove, list(list.files(eurocat_tmp, full.names = T)))
  
}

#Delete folders events, so, mo from tmp
unlink(paste0(tmp,"EUROCAT"), recursive = T)

