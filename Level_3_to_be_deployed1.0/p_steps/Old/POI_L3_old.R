#Author: Vjola Hoxhaj Drs.
#email: v.hoxhaj@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/07/2021



#Start with conditions
#get number in the original table
#get number of women of child bearing age(12-55 year old)
#load pregnancy stage file
#merge
#keep only women with at least 365 years of follow up
#report number
#analysis to be done by year and condition + pregnancy stage

########################################################
#Create output folders
########################################################
if (subpopulations_present=="No"){
  
  if ("POI" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"POI"), recursive = T)#delete folder
    dir.create(paste(output_dir, "POI", sep=""))
    poi_dir<-paste(output_dir, "POI/", sep="")
    
    #EVENTS_PREGNANCY
    dir.create(paste(poi_dir, "EVENTS_PREGNANCY", sep=""))
    ev_preg_dir<-paste(poi_dir, "EVENTS_PREGNANCY/", sep="")
    dir.create(paste(ev_preg_dir,"Masked", sep=""))
    
    #MEDICINES_PREGNANCY
    dir.create(paste(poi_dir, "MEDICINES_PREGNANCY", sep=""))
    med_preg_dir<-paste(poi_dir, "MEDICINES_PREGNANCY/", sep="")
    dir.create(paste(med_preg_dir,"Masked", sep=""))
    
    #VACCINES_PREGNANCY
    dir.create(paste(poi_dir, "VACCINES_PREGNANCY", sep=""))
    vacc_preg_dir<-paste(poi_dir, "VACCINES_PREGNANCY/", sep="")
    dir.create(paste(vacc_preg_dir,"Masked", sep=""))
    
    #EVENTS_MEDICINES_PREGNANCY
    dir.create(paste(poi_dir, "EVENTS_MEDICINES_PREGNANCY", sep=""))
    ev_med_preg_dir<-paste(poi_dir, "EVENTS_MEDICINES_PREGNANCY/", sep="")
    dir.create(paste(ev_med_preg_dir,"Masked", sep=""))
    
    #EVENTS_VACCINES_PREGNANCY
    dir.create(paste(poi_dir, "EVENTS_VACCINES_PREGNANCY", sep=""))
    ev_vacc_preg_dir<-paste(poi_dir, "EVENTS_VACCINES_PREGNANCY/", sep="")
    dir.create(paste(ev_vacc_preg_dir,"Masked", sep=""))
    
    #EVENTS_MEDICINES
    dir.create(paste(poi_dir, "EVENTS_MEDICINES", sep=""))
    ev_med_dir<-paste(poi_dir, "EVENTS_MEDICINES/", sep="")
    dir.create(paste(ev_med_dir,"Masked", sep=""))
    
    #EVENTS_VACCINES
    dir.create(paste(poi_dir, "EVENTS_VACCINES", sep=""))
    ev_vacc_dir<-paste(poi_dir, "EVENTS_VACCINES/", sep="")
    dir.create(paste(ev_vacc_dir,"Masked", sep=""))
    
   
  } else {
    #Create the  folder in the output dir
    dir.create(paste(output_dir, "POI", sep=""))
    poi_dir<-paste(output_dir, "POI/", sep="")
    
    #EVENTS_PREGNANCY
    dir.create(paste(poi_dir, "EVENTS_PREGNANCY", sep=""))
    ev_preg_dir<-paste(poi_dir, "EVENTS_PREGNANCY/", sep="")
    dir.create(paste(ev_preg_dir,"Masked", sep=""))
    
    #MEDICINES_PREGNANCY
    dir.create(paste(poi_dir, "MEDICINES_PREGNANCY", sep=""))
    med_preg_dir<-paste(poi_dir, "MEDICINES_PREGNANCY/", sep="")
    dir.create(paste(med_preg_dir,"Masked", sep=""))
    
    #VACCINES_PREGNANCY
    dir.create(paste(poi_dir, "VACCINES_PREGNANCY", sep=""))
    vacc_preg_dir<-paste(poi_dir, "VACCINES_PREGNANCY/", sep="")
    dir.create(paste(vacc_preg_dir,"Masked", sep=""))
    
    #EVENTS_MEDICINES_PREGNANCY
    dir.create(paste(poi_dir, "EVENTS_MEDICINES_PREGNANCY", sep=""))
    ev_med_preg_dir<-paste(poi_dir, "EVENTS_MEDICINES_PREGNANCY/", sep="")
    dir.create(paste(ev_med_preg_dir,"Masked", sep=""))
    
    #EVENTS_VACCINES_PREGNANCY
    dir.create(paste(poi_dir, "EVENTS_VACCINES_PREGNANCY", sep=""))
    ev_vacc_preg_dir<-paste(poi_dir, "EVENTS_VACCINES_PREGNANCY/", sep="")
    dir.create(paste(ev_vacc_preg_dir,"Masked", sep=""))
    
    #EVENTS_MEDICINES
    dir.create(paste(poi_dir, "EVENTS_MEDICINES", sep=""))
    ev_med_dir<-paste(poi_dir, "EVENTS_MEDICINES/", sep="")
    dir.create(paste(ev_med_dir,"Masked", sep=""))
    
    #EVENTS_VACCINES
    dir.create(paste(poi_dir, "EVENTS_VACCINES", sep=""))
    ev_vacc_dir<-paste(poi_dir, "EVENTS_VACCINES/", sep="")
    dir.create(paste(ev_vacc_dir,"Masked", sep=""))
   
  }
  
  #########################################################################
  #output ev_preg_pop_study_population in g_intermediate/populations/ev_preg_pop
  if ("EVENTS_PREGNANCY" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"EVENTS_PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "EVENTS_PREGNANCY", sep=""))
    ev_preg_pop<-paste(populations_dir, "EVENTS_PREGNANCY/", sep="")
  } else {
    #Create the EVENTS_PREGNANCY folder in the output dir
    dir.create(paste(populations_dir, "EVENTS_PREGNANCY", sep=""))
    ev_preg_pop<-paste(populations_dir, "EVENTS_PREGNANCY/", sep="")
  }
  
  #output med_preg_dir_study_population in g_intermediate/populations/med_preg_dir
  if ("MEDICINES_PREGNANCY" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"MEDICINES_PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "MEDICINES_PREGNANCY", sep=""))
    med_preg_pop<-paste(populations_dir, "MEDICINES_PREGNANCY/", sep="")
  } else {
    #Create the EVENTS_PREGNANCY folder in the output dir
    dir.create(paste(populations_dir, "MEDICINES_PREGNANCY", sep=""))
    med_preg_pop<-paste(populations_dir, "MEDICINES_PREGNANCY/", sep="")
  }
  
  #output vacc_preg_pop_study_population in g_intermediate/populations/vacc_preg_pop
  if ("VACCINES_PREGNANCY" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"VACCINES_PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "VACCINES_PREGNANCY", sep=""))
    vacc_preg_pop<-paste(populations_dir, "VACCINES_PREGNANCY/", sep="")
  } else {
    #Create the EVENTS_PREGNANCY folder in the output dir
    dir.create(paste(populations_dir, "VACCINES_PREGNANCY", sep=""))
    vacc_preg_pop<-paste(populations_dir, "VACCINES_PREGNANCY/", sep="")
  }
  
  if ("EVENTS_MEDICINES_PREGNANCY" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"EVENTS_MEDICINES_PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "EVENTS_MEDICINES_PREGNANCY", sep=""))
    ev_med_preg_pop<-paste(populations_dir, "EVENTS_MEDICINES_PREGNANCY/", sep="")
  } else {
    #Create the  folder in the output dir
    dir.create(paste(populations_dir, "EVENTS_MEDICINES_PREGNANCY", sep=""))
    ev_med_preg_pop<-paste(populations_dir, "EVENTS_MEDICINES_PREGNANCY/", sep="")
  }
  
  if ("EVENTS_VACCINES_PREGNANCY" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"EVENTS_VACCINES_PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "EVENTS_VACCINES_PREGNANCY", sep=""))
    ev_vacc_preg_pop<-paste(populations_dir, "EVENTS_VACCINES_PREGNANCY/", sep="")
  } else {
    #Create the  folder in the output dir
    dir.create(paste(populations_dir, "EVENTS_VACCINES_PREGNANCY", sep=""))
    ev_vacc_preg_pop<-paste(populations_dir, "EVENTS_VACCINES_PREGNANCY/", sep="")
  }
  
  if ("EVENTS_MEDICINES" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"EVENTS_MEDICINES"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "EVENTS_MEDICINES", sep=""))
    ev_med_pop<-paste(populations_dir, "EVENTS_MEDICINES/", sep="")
  } else {
    #Create the  folder in the output dir
    dir.create(paste(populations_dir, "EVENTS_MEDICINES", sep=""))
    ev_med_pop<-paste(populations_dir, "EVENTS_MEDICINES/", sep="")
  }
  
  if ("EVENTS_VACCINES" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"EVENTS_VACCINES"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "EVENTS_VACCINES", sep=""))
    ev_vacc_pop<-paste(populations_dir, "EVENTS_VACCINES/", sep="")
  } else {
    #Create the  folder in the output dir
    dir.create(paste(populations_dir, "EVENTS_VACCINES", sep=""))
    ev_vacc_pop<-paste(populations_dir, "EVENTS_VACCINES/", sep="")
  }
  
  #POI_tmp/POI folder where all intermediary files are saved
  if ("POI" %in% list.files(tmp)){
    unlink(paste0(tmp,"POI"), recursive = T)#delete folder
    dir.create(paste(tmp, "POI", sep=""))
    poi_tmp<-paste(tmp, "POI/", sep="")
  }else{
    #Create the POI folder in the output dir
    dir.create(paste(tmp, "POI", sep=""))
    poi_tmp<-paste(tmp, "POI/", sep="")
  }
} else {
  
  if ("POI" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"POI"), recursive = T)#delete folder
    dir.create(paste(output_dir, "POI", sep=""))
    poi_dir<-paste(output_dir, "POI/", sep="")
    
    #EVENTS_PREGNANCY
    dir.create(paste(poi_dir, "EVENTS_PREGNANCY", sep=""))
    ev_preg_dir<-paste(poi_dir, "EVENTS_PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_preg_dir, subpopulations_names[i],"/Masked"))
    }
    
    #MEDICINES_PREGNANCY
    dir.create(paste(poi_dir, "MEDICINES_PREGNANCY", sep=""))
    med_preg_dir<-paste(poi_dir, "MEDICINES_PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(med_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(med_preg_dir, subpopulations_names[i],"/Masked"))
    }
    
    
    #VACCINES_PREGNANCY
    dir.create(paste(poi_dir, "VACCINES_PREGNANCY", sep=""))
    vacc_preg_dir<-paste(poi_dir, "VACCINES_PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(vacc_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(vacc_preg_dir, subpopulations_names[i],"/Masked"))
    }
    
    #EVENTS_MEDICINES_PREGNANCY
    dir.create(paste(poi_dir, "EVENTS_MEDICINES_PREGNANCY", sep=""))
    ev_med_preg_dir<-paste(poi_dir, "EVENTS_MEDICINES_PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_preg_dir, subpopulations_names[i],"/Masked"))
    }
    
    
    #EVENTS_VACCINES_PREGNANCY
    dir.create(paste(poi_dir, "EVENTS_VACCINES_PREGNANCY", sep=""))
    ev_vacc_preg_dir<-paste(poi_dir, "EVENTS_VACCINES_PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_vacc_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_vacc_preg_dir, subpopulations_names[i],"/Masked"))
    }
    
    
    #EVENTS_MEDICINES
    dir.create(paste(poi_dir, "EVENTS_MEDICINES", sep=""))
    ev_med_dir<-paste(poi_dir, "EVENTS_MEDICINES/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_dir, subpopulations_names[i],"/Masked"))
    }
    
    #EVENTS_VACCINES
    dir.create(paste(poi_dir, "EVENTS_VACCINES", sep=""))
    ev_vacc_dir<-paste(poi_dir, "EVENTS_VACCINES/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_vacc_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_vacc_dir, subpopulations_names[i],"/Masked"))
    }
    
    
  } else {
    #Create the  folder in the output dir
    dir.create(paste(output_dir, "POI", sep=""))
    poi_dir<-paste(output_dir, "POI/", sep="")
    
    #EVENTS_PREGNANCY
    dir.create(paste(poi_dir, "EVENTS_PREGNANCY", sep=""))
    ev_preg_dir<-paste(poi_dir, "EVENTS_PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_preg_dir, subpopulations_names[i],"/Masked"))
    }
    
    #MEDICINES_PREGNANCY
    dir.create(paste(poi_dir, "MEDICINES_PREGNANCY", sep=""))
    med_preg_dir<-paste(poi_dir, "MEDICINES_PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(med_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(med_preg_dir, subpopulations_names[i],"/Masked"))
    }
    
    
    #VACCINES_PREGNANCY
    dir.create(paste(poi_dir, "VACCINES_PREGNANCY", sep=""))
    vacc_preg_dir<-paste(poi_dir, "VACCINES_PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(vacc_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(vacc_preg_dir, subpopulations_names[i],"/Masked"))
    }
    
    #EVENTS_MEDICINES_PREGNANCY
    dir.create(paste(poi_dir, "EVENTS_MEDICINES_PREGNANCY", sep=""))
    ev_med_preg_dir<-paste(poi_dir, "EVENTS_MEDICINES_PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_preg_dir, subpopulations_names[i],"/Masked"))
    }
    
    
    #EVENTS_VACCINES_PREGNANCY
    dir.create(paste(poi_dir, "EVENTS_VACCINES_PREGNANCY", sep=""))
    ev_vacc_preg_dir<-paste(poi_dir, "EVENTS_VACCINES_PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_vacc_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_vacc_preg_dir, subpopulations_names[i],"/Masked"))
    }
    
    
    #EVENTS_MEDICINES
    dir.create(paste(poi_dir, "EVENTS_MEDICINES", sep=""))
    ev_med_dir<-paste(poi_dir, "EVENTS_MEDICINES/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_dir, subpopulations_names[i],"/Masked"))
    }
    
    #EVENTS_VACCINES
    dir.create(paste(poi_dir, "EVENTS_VACCINES", sep=""))
    ev_vacc_dir<-paste(poi_dir, "EVENTS_VACCINES/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_vacc_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_vacc_dir, subpopulations_names[i],"/Masked"))
    }
    
  }
  
  
  ###################################################################
  if ("EVENTS_PREGNANCY" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"EVENTS_PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "EVENTS_PREGNANCY", sep=""))
    ev_preg_pop<-paste(populations_dir, "EVENTS_PREGNANCY/",sep="")
    do.call(file.remove, list(list.files(ev_preg_pop, full.names = T)))
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_preg_pop, subpopulations_names[i]))
    }
  } else {
    #Create the EVENTS_PREGNANCY folder in the output dir
    dir.create(paste(populations_dir, "EVENTS_PREGNANCY", sep=""))
    ev_preg_pop<-paste(populations_dir, "EVENTS_PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_preg_pop, subpopulations_names[i]))
    }
  }
  
  if ("MEDICINES_PREGNANCY" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"MEDICINES_PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "MEDICINES_PREGNANCY", sep=""))
    med_preg_pop<-paste(populations_dir, "MEDICINES_PREGNANCY/",sep="")
    do.call(file.remove, list(list.files(med_preg_pop, full.names = T)))
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(med_preg_pop, subpopulations_names[i]))
    }
  } else {
    #Create the MEDICINES_PREGNANCY folder in the output dir
    dir.create(paste(populations_dir, "MEDICINES_PREGNANCY", sep=""))
    med_preg_pop<-paste(populations_dir, "MEDICINES_PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(med_preg_pop, subpopulations_names[i]))
    }
  }
  
  if ("VACCINES_PREGNANCY" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"VACCINES_PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "VACCINES_PREGNANCY", sep=""))
    vacc_preg_pop<-paste(populations_dir, "VACCINES_PREGNANCY/",sep="")
    do.call(file.remove, list(list.files(vacc_preg_pop, full.names = T)))
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(vacc_preg_pop, subpopulations_names[i]))
    }
  } else {
    #Create the VACCINES_PREGNANCY folder in the output dir
    dir.create(paste(populations_dir, "VACCINES_PREGNANCY", sep=""))
    vacc_preg_pop<-paste(populations_dir, "VACCINES_PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(vacc_preg_pop, subpopulations_names[i]))
    }
  }
  
  if ("EVENTS_MEDICINES_PREGNANCY" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"EVENTS_MEDICINES_PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "EVENTS_MEDICINES_PREGNANCY", sep=""))
    ev_med_preg_pop<-paste(populations_dir, "EVENTS_MEDICINES_PREGNANCY/",sep="")
    do.call(file.remove, list(list.files(ev_med_preg_pop, full.names = T)))
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_preg_pop, subpopulations_names[i]))
    }
  } else {
    #Create the EVENTS_MEDICINES_PREGNANCY folder in the output dir
    dir.create(paste(populations_dir, "EVENTS_MEDICINES_PREGNANCY", sep=""))
    ev_med_preg_pop<-paste(populations_dir, "EVENTS_MEDICINES_PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_preg_pop, subpopulations_names[i]))
    }
  }

  if ("EVENTS_VACCINES_PREGNANCY" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"EVENTS_VACCINES_PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "EVENTS_VACCINES_PREGNANCY", sep=""))
    ev_vacc_preg_pop<-paste(populations_dir, "EVENTS_VACCINES_PREGNANCY/",sep="")
    do.call(file.remove, list(list.files(ev_vacc_preg_pop, full.names = T)))
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_vacc_preg_pop, subpopulations_names[i]))
    }
  } else {
    #Create the EVENTS_VACCINES_PREGNANCY folder in the output dir
    dir.create(paste(populations_dir, "EVENTS_VACCINES_PREGNANCY", sep=""))
    ev_vacc_preg_pop<-paste(populations_dir, "EVENTS_VACCINES_PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_vacc_preg_pop, subpopulations_names[i]))
    }
  }

  if ("EVENTS_MEDICINES" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"EVENTS_MEDICINES"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "EVENTS_MEDICINES", sep=""))
    ev_med_pop<-paste(populations_dir, "EVENTS_MEDICINES/",sep="")
    do.call(file.remove, list(list.files(ev_med_pop, full.names = T)))
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_pop, subpopulations_names[i]))
    }
  } else {
    #Create the EVENTS_MEDICINES folder in the output dir
    dir.create(paste(populations_dir, "EVENTS_MEDICINES", sep=""))
    ev_med_pop<-paste(populations_dir, "EVENTS_MEDICINES/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_pop, subpopulations_names[i]))
    }
  }
  
  if ("EVENTS_VACCINES" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"EVENTS_VACCINES"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "EVENTS_VACCINES", sep=""))
    ev_vacc_pop<-paste(populations_dir, "EVENTS_VACCINES/",sep="")
    do.call(file.remove, list(list.files(ev_vacc_pop, full.names = T)))
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_vacc_pop, subpopulations_names[i]))
    }
  } else {
    #Create the EVENTS_VACCINES folder in the output dir
    dir.create(paste(populations_dir, "EVENTS_VACCINES", sep=""))
    ev_vacc_pop<-paste(populations_dir, "EVENTS_VACCINES/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_vacc_pop, subpopulations_names[i]))
    }
  }
  
  
  #POI_tmp/POI folder where all intermediary files are saved
  if ("POI" %in% list.files(tmp)){
    unlink(paste0(tmp,"POI"), recursive = T)#delete folder
    dir.create(paste(tmp, "POI", sep=""))
    poi_tmp<-paste(tmp, "POI/", sep="")
  }else{
    #Create the POI folder in the output dir
    dir.create(paste(tmp, "POI", sep=""))
    poi_tmp<-paste(tmp, "POI/", sep="")
  }
}
#######################################################
recurrent_events<-c("Breast cancer", "Gestational diabetes")
chronic_conditions<-c("ADHD", "Depression", "Epilepsy", "Migraine", "Multiple sclerosis", "Rheumatoid arthritis", "SLE")

if(sum(diagnoses,pregnancies)>0){
if (subpopulations_present=="Yes"){
  for (s in 1: length(subpopulations_names)){
    #get all files in the folder g_intermediate/populations/pregnancy
    pregnancy_files<-list.files(paste0(preg_pop, subpopulations_names[s]))
    #get all files in the folder g_intermediate/populations/diagnoses
    if(diagnoses==TRUE){
    conditions_files_chronic<-c()
    if(length(list.files(paste0(diag_pop, subpopulations_names[s]), chronic_conditions[i]))>0){
    for (i in 1:length(chronic_conditions)){
      conditions_files_chronic<-c(conditions_files_chronic,list.files(paste0(diag_pop, subpopulations_names[s]), chronic_conditions[i]))
    }
    }
    conditions_files_recurrent<-c()
    for (i in 1:length(recurrent_events)){
      if(length(list.files(paste0(diag_pop, subpopulations_names[s]), recurrent_events[i]))>0){
      conditions_files_recurrent<-c(conditions_files_recurrent,list.files(paste0(diag_pop, subpopulations_names[s]), recurrent_events[i]))
    }
    }
    } else {
      conditions_files_chronic<-c()
      conditions_files_recurrent<-c()
    }
    #get all files in the folder g_intermediate/populations/medicines
    if(sum(diagnoses_pregnancy_med, pregnancy_only_med)>0){
    medicines_files<-list.files(paste0(medicines_pop, subpopulations_names[s]), pattern = "f_population")
    } else { 
      medicines_files<-c()
      }
    #get all files in the folder g_intermediate/populations/vaccines
    if(sum(diagnoses_pregnancy_vacc, pregnancy_only_vacc)>0){
    vaccines_files<-list.files(paste0(vaccines_pop, subpopulations_names[s]), pattern = "f_population")
    } else {
      vaccines_files<-c()
    }
    
    #################################################################################
    #Combine DIAGNOSES file by year and condition
    ################################################################################
    #################################################################################
    #Combine diagnoses_df file by year and condition
    ################################################################################
    if (length(conditions_files_chronic)>0){
      #creates filter year_condition
      files<-list()
      for (i in 1: length(conditions_files_chronic)){
        files<-append(files,list(paste(unlist(str_split(conditions_files_chronic[i],"_"))[2], collapse = "_")))
      }
      files<-do.call(c,files)
      #remove duplicates 
      files<-files[!duplicated(files)]
      #create list with names year_condition
      conditions_chronic<-vector(mode="list", length=length(files))
      names(conditions_chronic)<-files
      rm(files)
      #separate all files into the right category
      for (i in 1:length(conditions_chronic)){
        conditions_chronic[[i]]<-conditions_files_chronic[str_detect(conditions_files_chronic, names(conditions_chronic)[i])]
      }
      rm(conditions_files_chronic)
      conditions_files_chronic<-conditions_chronic
      rm(conditions_chronic)
    }
    
    if (length(conditions_files_recurrent)>0){
      #creates filter year_condition
      files<-list()
      for (i in 1: length(conditions_files_recurrent)){
        files<-append(files,list(paste(unlist(str_split(conditions_files_recurrent[i],"_"))[1:2], collapse = "_")))
      }
      files<-do.call(c,files)
      #remove duplicates 
      files<-files[!duplicated(files)]
      #create list with names year_condition
      conditions_recurrent<-vector(mode="list", length=length(files))
      names(conditions_recurrent)<-files
      rm(files)
      #separate all files into the right category
      for (i in 1:length(conditions_recurrent)){
        conditions_recurrent[[i]]<-conditions_files_recurrent[str_detect(conditions_files_recurrent, paste0("^", names(conditions_recurrent)[i]))]
      }
      rm(conditions_files_recurrent)
      conditions_files_recurrent<-conditions_recurrent
      rm(conditions_recurrent)
    }
    ######################################################################################
    #OUTPUT FOR diagnoses_df: CONDITIONS
    ######################################################################################
    #################################################################################
    #Combine MEDICINES file by ATC level 1 and year
    ################################################################################
    if(length(medicines_files)>0){
      #creates filter year_ATC level
      files<-list()
      for (i in 1: length(medicines_files)){
        files<-append(files,list(paste(unlist(str_split(medicines_files[i],"_"))[1:2], collapse = "_")))
      }
      files<-do.call(c,files)
      #remove duplicates 
      files<-files[!duplicated(files)]
      #create list with names year_condition
      medicines_list<-vector(mode="list", length=length(files))
      names(medicines_list)<-files
      rm(files)
      #separate all files into the right category
      for (i in 1:length(medicines_list)){
        medicines_list[[i]]<-medicines_files[str_detect(medicines_files, paste0("^", names(medicines_list)[i]))]
      }
      rm(medicines_files)
      medicines_files<-medicines_list
      rm(medicines_list)
    }
    ######################################################################################
    #OUTPUT FOR MEDICINES: MEDICINES_LIST
    ######################################################################################
    #################################################################################
    #Combine VACCINES file by ATC level 1 and year
    ################################################################################
    if(length(vaccines_files)>0){
      #creates filter year_ATC level
      files<-list()
      for (i in 1: length(vaccines_files)){
        files<-append(files,list(paste(unlist(str_split(vaccines_files[i],"_"))[1:2], collapse = "_")))
      }
      files<-do.call(c,files)
      #remove duplicates 
      files<-files[!duplicated(files)]
      #create list with names year_condition
      vaccines_list<-vector(mode="list", length=length(files))
      names(vaccines_list)<-files
      rm(files)
      #separate all files into the right category
      for (i in 1:length(vaccines_list)){
        vaccines_list[[i]]<-vaccines_files[str_detect(vaccines_files, paste0("^", names(vaccines_list)[i]))]
      }
      rm(vaccines_files)
      vaccines_files<-vaccines_list
      rm(vaccines_list)
    }
    ######################################################################################
    #OUTPUT FOR VACCINES: VACCINES_LIST
    ######################################################################################
    
    ######################################################################################
    #Create prevalence databases in poi_tmp for chronic conditions
    #####################################################################################
    if(length(conditions_files_chronic)>0){
      for (cond_chr_ind in 1:length(conditions_files_chronic)){
          if (length(conditions_files_chronic[[cond_chr_ind]])>1){
            diagnoses_df<-readRDS(paste0(diag_pop, subpopulations_names[s],"/",conditions_files_chronic[[cond_chr_ind]][1]))
            #create min date for each person and keep only that record
            diagnoses_df[, min_date:=min(event_date), by="person_id"]
            #keep only the earliest record for each person
            diagnoses_df<-diagnoses_df[event_date==min_date]
            diagnoses_df[,min_date:=NULL]
            l<-2
            while (l<=length(conditions_files_chronic[[cond_chr_ind]])){
              diagnoses_df<-rbind(diagnoses_df,readRDS(paste0(diag_pop, subpopulations_names[s],"/",conditions_files_chronic[[cond_chr_ind]][l])))
              diagnoses_df[, min_date:=min(event_date), by="person_id"]
              #keep only the earliest record for each person
              diagnoses_df<-diagnoses_df[event_date==min_date]
              diagnoses_df[,min_date:=NULL]
              l<-l+1
            }
          } else {
            diagnoses_df<-readRDS(paste0(diag_pop, subpopulations_names[s],"/",conditions_files_chronic[[cond_chr_ind]])) #combine all files for one pregnancy stage
            diagnoses_df[, min_date:=min(event_date), by="person_id"]
            #keep only the earliest record for each person
            diagnoses_df<-diagnoses_df[event_date==min_date]
            diagnoses_df[,min_date:=NULL]
            
            }

      diagnoses_df<-diagnoses_df[sex_at_instance_creation=="F" & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,c("person_id","condition", "truncated_code","event_vocabulary","birth_date","end_follow_up","start_follow_up","age_start_follow_up","year", "event_date")]
      #get min year present 
      if(diagnoses_df[,.N]>0){
        min_year<-as.IDate(paste0(min(diagnoses_df[!duplicated(year),year]),12,31), "%Y%m%d")#first year of having an event in the database
        conditions_present<-diagnoses_df[!duplicated(condition),condition]
        for (l in min(diagnoses_df[!duplicated(year),year]):year(date_creation)){
          for (k in 1: length(conditions_present)){
            #create filter variable if event_date before end of year then filter:=1
            saveRDS(diagnoses_df[year<=l & year(start_follow_up)<=l & year(end_follow_up)>=l & condition==conditions_present[k]], paste0(poi_tmp,l,"_",conditions_present[k], "_prevalence.rds"))
          }
        }
      }
      }
    }
      rm(diagnoses_df)
      
      conditions_files_chronic<-c()
      for (i in 1:length(chronic_conditions)){
        conditions_files_chronic<-c(conditions_files_chronic,list.files(poi_tmp, chronic_conditions[i]))
      }
      conditions_files_chronic<-conditions_files_chronic[str_detect(conditions_files_chronic, "_prevalence.rds")]
      
      if (length(conditions_files_chronic)>0){
        #creates filter year_condition
        files<-list()
        for (i in 1: length(conditions_files_chronic)){
          files<-append(files,list(paste(unlist(str_split(conditions_files_chronic[i],"_"))[1:2], collapse = "_")))
        }
        files<-do.call(c,files)
        #remove duplicates 
        files<-files[!duplicated(files)]
        #create list with names year_condition
        conditions_chronic<-vector(mode="list", length=length(files))
        names(conditions_chronic)<-files
        rm(files)
        #separate all files into the right category
        for (i in 1:length(conditions_chronic)){
          conditions_chronic[[i]]<-conditions_files_chronic[str_detect(conditions_files_chronic, names(conditions_chronic)[i])]
        }
        rm(conditions_files_chronic)
        conditions_files_chronic<-conditions_chronic
        rm(conditions_chronic)
      }
    
    ###############################
    #run script
    ##############################
    
    source(paste0(pre_dir,"POI_L3_pre_script.R"))
}
} else {
  ###########################
  #No subpopulations
  ############################
  #get all files in the folder g_intermediate/populations/pregnancy
  pregnancy_files<-list.files(preg_pop)
  #get all files in the folder g_intermediate/populations/diagnoses_df
  if(diagnoses==T){
    conditions_files_chronic<-c()
    for (i in 1:length(chronic_conditions)){
      if(length(list.files(diag_pop,chronic_conditions[i]))>0){
      conditions_files_chronic<-c(conditions_files_chronic,list.files(diag_pop,chronic_conditions[i]))
      }
    }
    
    conditions_files_recurrent<-c()
    for (i in 1:length(recurrent_events)){
      if(length(list.files(diag_pop,recurrent_events[i]))>0){
      conditions_files_recurrent<-c(conditions_files_recurrent,list.files(diag_pop, recurrent_events[i]))
      }
    }
  } else {
    conditions_files_chronic<-c()
    conditions_files_recurrent<-c()
  }
  #get all files in the folder g_intermediate/populations/medicines
  if(sum(diagnoses_pregnancy_med, pregnancy_only_med)>0){
    medicines_files<-list.files(medicines_pop, pattern = "f_population")
  } else{medicines_files<-c()}
  #get all files in the folder g_intermediate/populations/vaccines
  if(sum(diagnoses_pregnancy_vacc, pregnancy_only_vacc)>0){
    vaccines_files<-list.files(vaccines_pop, pattern = "f_population")
  } else {
    vaccines_files<-c()
  }
  
  #################################################################################
  #Combine diagnoses_df file by year and condition
  ################################################################################
  if (length(conditions_files_chronic)>0){
    #creates filter year_condition
    files<-list()
    for (i in 1: length(conditions_files_chronic)){
      files<-append(files,list(paste(unlist(str_split(conditions_files_chronic[i],"_"))[2], collapse = "_")))
    }
    files<-do.call(c,files)
    #remove duplicates 
    files<-files[!duplicated(files)]
    #create list with names year_condition
    conditions_chronic<-vector(mode="list", length=length(files))
    names(conditions_chronic)<-files
    rm(files)
    #separate all files into the right category
    for (i in 1:length(conditions_chronic)){
      conditions_chronic[[i]]<-conditions_files_chronic[str_detect(conditions_files_chronic, names(conditions_chronic)[i])]
    }
    rm(conditions_files_chronic)
    conditions_files_chronic<-conditions_chronic
    rm(conditions_chronic)
  }
  
  if (length(conditions_files_recurrent)>0){
    #creates filter year_condition
    files<-list()
    for (i in 1: length(conditions_files_recurrent)){
      files<-append(files,list(paste(unlist(str_split(conditions_files_recurrent[i],"_"))[1:2], collapse = "_")))
    }
    files<-do.call(c,files)
    #remove duplicates 
    files<-files[!duplicated(files)]
    #create list with names year_condition
    conditions_recurrent<-vector(mode="list", length=length(files))
    names(conditions_recurrent)<-files
    rm(files)
    #separate all files into the right category
    for (i in 1:length(conditions_recurrent)){
      conditions_recurrent[[i]]<-conditions_files_recurrent[str_detect(conditions_files_recurrent, paste0("^", names(conditions_recurrent)[i]))]
    }
    rm(conditions_files_recurrent)
    conditions_files_recurrent<-conditions_recurrent
    rm(conditions_recurrent)
  }
  ######################################################################################
  #OUTPUT FOR diagnoses_df: CONDITIONS
  ######################################################################################
  #################################################################################
  #Combine MEDICINES file by ATC level 1 and year
  ################################################################################
  #creates filter year_ATC level
  if(length(medicines_files)>0){
    files<-list()
    for (i in 1: length(medicines_files)){
      files<-append(files,list(paste(unlist(str_split(medicines_files[i],"_"))[1:2], collapse = "_")))
    }
    files<-do.call(c,files)
    #remove duplicates 
    files<-files[!duplicated(files)]
    #create list with names year_condition
    medicines_list<-vector(mode="list", length=length(files))
    names(medicines_list)<-files
    rm(files)
    #separate all files into the right category
    for (i in 1:length(medicines_list)){
      medicines_list[[i]]<-medicines_files[str_detect(medicines_files, paste0("^", names(medicines_list)[i]))]
    }
    rm(medicines_files)
    medicines_files<-medicines_list
    rm(medicines_list)
  }
  ######################################################################################
  #OUTPUT FOR MEDICINES: MEDICINES_LIST
  ######################################################################################
  #################################################################################
  #Combine VACCINES file by ATC level 1 and year
  ################################################################################
  #creates filter year_ATC level
  if(length(vaccines_files)>0){
    files<-list()
    for (i in 1: length(vaccines_files)){
      files<-append(files,list(paste(unlist(str_split(vaccines_files[i],"_"))[1:2], collapse = "_")))
    }
    files<-do.call(c,files)
    #remove duplicates 
    files<-files[!duplicated(files)]
    #create list with names year_condition
    vaccines_list<-vector(mode="list", length=length(files))
    names(vaccines_list)<-files
    rm(files)
    #separate all files into the right category
    for (i in 1:length(vaccines_list)){
      vaccines_list[[i]]<-vaccines_files[str_detect(vaccines_files, paste0("^", names(vaccines_list)[i]))]
    }
    rm(vaccines_files)
    vaccines_files<-vaccines_list
    rm(vaccines_list)
  }
  ######################################################################################
  #OUTPUT FOR VACCINES: VACCINES_LIST
  ######################################################################################
  
  ######################################################################################
  #Create prevalence databases in poi_tmp for chronic conditions
  #####################################################################################
  if(length(conditions_files_chronic)>0){
    for (cond_chr_ind in 1:length(conditions_files_chronic)){
        if (length(conditions_files_chronic[[cond_chr_ind]])>1){
          diagnoses_df<-readRDS(paste0(diag_pop,conditions_files_chronic[[cond_chr_ind]][1]))
          #create min date for each person and keep only that record
          diagnoses_df[, min_date:=min(event_date), by="person_id"]
          #keep only the earliest record for each person
          diagnoses_df<-diagnoses_df[event_date==min_date]
          diagnoses_df[,min_date:=NULL]
          l<-2
          while (l<=length(conditions_files_chronic[[cond_chr_ind]])){
            diagnoses_df<-rbind(diagnoses_df,readRDS(paste0(diag_pop,conditions_files_chronic[[cond_chr_ind]][l])))
            diagnoses_df[, min_date:=min(event_date), by="person_id"]
            #keep only the earliest record for each person
            diagnoses_df<-diagnoses_df[event_date==min_date]
            diagnoses_df[,min_date:=NULL]
            l<-l+1
          } 
        } else {
          diagnoses_df<-readRDS(paste0(diag_pop,conditions_files_chronic[[cond_chr_ind]][1]))
          diagnoses_df[, min_date:=min(event_date), by="person_id"]
          #keep only the earliest record for each person
          diagnoses_df<-diagnoses_df[event_date==min_date]
          diagnoses_df[,min_date:=NULL]
        }
      diagnoses_df<-diagnoses_df[sex_at_instance_creation=="F" & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,c("person_id","condition", "truncated_code","event_vocabulary","birth_date","end_follow_up","start_follow_up","age_start_follow_up","year", "event_date")]
      
    if(diagnoses_df[,.N]>0){
     #get min year present 
    min_year<-as.IDate(paste0(min(diagnoses_df[!duplicated(year),year]),12,31), "%Y%m%d")#first year of having an event in the database
    conditions_present<-diagnoses_df[!duplicated(condition),condition]
    for (year_ind in min(diagnoses_df[!duplicated(year),year]):year(date_creation)){
      for (con_pres_ind in 1: length(conditions_present)){
        #create filter variable if event_date before end of year then filter:=1
        saveRDS(diagnoses_df[year<=year_ind & year(end_follow_up)>=year_ind & condition==conditions_present[con_pres_ind]], paste0(poi_tmp,year_ind,"_",conditions_present[con_pres_ind], "_prevalence.rds"))
      }
    }
    }
    }
  }
    rm(diagnoses_df)
    
    conditions_files_chronic<-c()
    for (i in 1:length(chronic_conditions)){
      conditions_files_chronic<-c(conditions_files_chronic,list.files(poi_tmp, chronic_conditions[i]))
    }
    conditions_files_chronic<-conditions_files_chronic[str_detect(conditions_files_chronic, "_prevalence.rds")]
    
    if (length(conditions_files_chronic)>0){
      #creates filter year_condition
      files<-list()
      for (i in 1: length(conditions_files_chronic)){
        files<-append(files,list(paste(unlist(str_split(conditions_files_chronic[i],"_"))[1:2], collapse = "_")))
      }
      files<-do.call(c,files)
      #remove duplicates 
      files<-files[!duplicated(files)]
      #create list with names year_condition
      conditions_chronic<-vector(mode="list", length=length(files))
      names(conditions_chronic)<-files
      rm(files)
      #separate all files into the right category
      for (i in 1:length(conditions_chronic)){
        conditions_chronic[[i]]<-conditions_files_chronic[str_detect(conditions_files_chronic, names(conditions_chronic)[i])]
      }
      rm(conditions_files_chronic)
      conditions_files_chronic<-conditions_chronic
      rm(conditions_chronic)
    }

  ##############################
  #run script
  ##############################
  
  source(paste0(pre_dir,"POI_L3_pre_script.R"))
  
}
}
#Delete folders poi from tmp
unlink(paste0(tmp,"POI"), recursive = T)



