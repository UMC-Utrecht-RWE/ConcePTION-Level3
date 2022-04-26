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
  #output folder for MEDICINES report in g_output
  if ("EVENTS_PREGNANCY" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"EVENTS_PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(output_dir, "EVENTS_PREGNANCY", sep=""))
    ev_preg_dir<-paste(output_dir, "EVENTS_PREGNANCY/", sep="")
    dir.create(paste(ev_preg_dir,"Masked", sep=""))
  } else {
    #Create the  folder in the output dir
    dir.create(paste(output_dir, "EVENTS_PREGNANCY", sep=""))
    ev_preg_dir<-paste(output_dir, "EVENTS_PREGNANCY/", sep="")
    dir.create(paste(ev_preg_dir,"Masked", sep=""))
  }

  if ("MEDICINES_PREGNANCY" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"MEDICINES_PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(output_dir, "MEDICINES_PREGNANCY", sep=""))
    med_preg_dir<-paste(output_dir, "MEDICINES_PREGNANCY/", sep="")
    dir.create(paste(med_preg_dir,"Masked", sep=""))
  } else {
    #Create the  folder in the output dir
    dir.create(paste(output_dir, "MEDICINES_PREGNANCY", sep=""))
    med_preg_dir<-paste(output_dir, "MEDICINES_PREGNANCY/", sep="")
    dir.create(paste(med_preg_dir,"Masked", sep=""))
  }
  
  if ("VACCINES_PREGNANCY" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"VACCINES_PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(output_dir, "VACCINES_PREGNANCY", sep=""))
    vacc_preg_dir<-paste(output_dir, "VACCINES_PREGNANCY/", sep="")
    dir.create(paste(vacc_preg_dir,"Masked", sep=""))
  } else {
    #Create the  folder in the output dir
    dir.create(paste(output_dir, "VACCINES_PREGNANCY", sep=""))
    vacc_preg_dir<-paste(output_dir, "VACCINES_PREGNANCY/", sep="")
    dir.create(paste(vacc_preg_dir,"Masked", sep=""))
  }
  
  if ("EVENTS_MEDICINES_PREGNANCY" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"EVENTS_MEDICINES_PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(output_dir, "EVENTS_MEDICINES_PREGNANCY", sep=""))
    ev_med_preg_dir<-paste(output_dir, "EVENTS_MEDICINES_PREGNANCY/", sep="")
    dir.create(paste(ev_med_preg_dir,"Masked", sep=""))
  } else {
    #Create the  folder in the output dir
    dir.create(paste(output_dir, "EVENTS_MEDICINES_PREGNANCY", sep=""))
    ev_med_preg_dir<-paste(output_dir, "EVENTS_MEDICINES_PREGNANCY/", sep="")
    dir.create(paste(ev_med_preg_dir,"Masked", sep=""))
  }
  
  if ("EVENTS_VACCINES_PREGNANCY" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"EVENTS_VACCINES_PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(output_dir, "EVENTS_VACCINES_PREGNANCY", sep=""))
    ev_vacc_preg_dir<-paste(output_dir, "EVENTS_VACCINES_PREGNANCY/", sep="")
    dir.create(paste(ev_vacc_preg_dir,"Masked", sep=""))
  } else {
    #Create the  folder in the output dir
    dir.create(paste(output_dir, "EVENTS_VACCINES_PREGNANCY", sep=""))
    ev_vacc_preg_dir<-paste(output_dir, "EVENTS_VACCINES_PREGNANCY/", sep="")
    dir.create(paste(ev_vacc_preg_dir,"Masked", sep=""))
  }
  
  if ("EVENTS_MEDICINES" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"EVENTS_MEDICINES"), recursive = T)#delete folder
    dir.create(paste(output_dir, "EVENTS_MEDICINES", sep=""))
    ev_med_dir<-paste(output_dir, "EVENTS_MEDICINES/", sep="")
    dir.create(paste(ev_med_dir,"Masked", sep=""))
  } else {
    #Create the  folder in the output dir
    dir.create(paste(output_dir, "EVENTS_MEDICINES", sep=""))
    ev_med_dir<-paste(output_dir, "EVENTS_MEDICINES/", sep="")
    dir.create(paste(ev_med_dir,"Masked", sep=""))
  }
  
  if ("EVENTS_VACCINES" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"EVENTS_VACCINES"), recursive = T)#delete folder
    dir.create(paste(output_dir, "EVENTS_VACCINES", sep=""))
    ev_vacc_dir<-paste(output_dir, "EVENTS_VACCINES/", sep="")
    dir.create(paste(ev_vacc_dir,"Masked", sep=""))
  } else {
    #Create the  folder in the output dir
    dir.create(paste(output_dir, "EVENTS_VACCINES", sep=""))
    ev_vacc_dir<-paste(output_dir, "EVENTS_VACCINES/", sep="")
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
  #output folder for MEDICINES report in g_output
  if ("EVENTS_PREGNANCY" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"EVENTS_PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(output_dir, "EVENTS_PREGNANCY", sep=""))
    ev_preg_dir<-paste(output_dir, "EVENTS_PREGNANCY/", sep="")
    
    do.call(file.remove, list(list.files(ev_preg_dir, full.names = T)))
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_preg_dir, subpopulations_names[i],"/Masked"))
    }
    
  } else {
    #Create the EVENTS_PREGNANCY folder in the output dir
    dir.create(paste(output_dir, "EVENTS_PREGNANCY", sep=""))
    ev_preg_dir<-paste(output_dir, "EVENTS_PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_preg_dir, subpopulations_names[i],"/Masked"))
    }
  }
  
  #output folder for MEDICINES_PREGNANCY report in g_output
  if ("MEDICINES_PREGNANCY" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"MEDICINES_PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(output_dir, "MEDICINES_PREGNANCY", sep=""))
    med_preg_dir<-paste(output_dir, "MEDICINES_PREGNANCY/", sep="")
    
    do.call(file.remove, list(list.files(med_preg_dir, full.names = T)))
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(med_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(med_preg_dir, subpopulations_names[i],"/Masked"))
    }
    
  } else {
    #Create the MEDICINES_PREGNANCY folder in the output dir
    dir.create(paste(output_dir, "MEDICINES_PREGNANCY", sep=""))
    med_preg_dir<-paste(output_dir, "MEDICINES_PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(med_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(med_preg_dir, subpopulations_names[i],"/Masked"))
    }
  }
  
  if ("VACCINES_PREGNANCY" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"VACCINES_PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(output_dir, "VACCINES_PREGNANCY", sep=""))
    vacc_preg_dir<-paste(output_dir, "VACCINES_PREGNANCY/", sep="")
    
    do.call(file.remove, list(list.files(vacc_preg_dir, full.names = T)))
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(vacc_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(vacc_preg_dir, subpopulations_names[i],"/Masked"))
    }
    
  } else {
    #Create the VACCINES_PREGNANCY folder in the output dir
    dir.create(paste(output_dir, "VACCINES_PREGNANCY", sep=""))
    vacc_preg_dir<-paste(output_dir, "VACCINES_PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(vacc_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(vacc_preg_dir, subpopulations_names[i],"/Masked"))
    }
  }
  
  if ("EVENTS_MEDICINES_PREGNANCY" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"EVENTS_MEDICINES_PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(output_dir, "EVENTS_MEDICINES_PREGNANCY", sep=""))
    ev_med_preg_dir<-paste(output_dir, "EVENTS_MEDICINES_PREGNANCY/", sep="")
    
    do.call(file.remove, list(list.files(ev_med_preg_dir, full.names = T)))
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_preg_dir, subpopulations_names[i],"/Masked"))
    }
    
  } else {
    #Create the EVENTS_MEDICINES_PREGNANCY folder in the output dir
    dir.create(paste(output_dir, "EVENTS_MEDICINES_PREGNANCY", sep=""))
    ev_med_preg_dir<-paste(output_dir, "EVENTS_MEDICINES_PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_preg_dir, subpopulations_names[i],"/Masked"))
    }
  }
  
  if ("EVENTS_VACCINES_PREGNANCY" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"EVENTS_VACCINES_PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(output_dir, "EVENTS_VACCINES_PREGNANCY", sep=""))
    ev_vacc_preg_dir<-paste(output_dir, "EVENTS_VACCINES_PREGNANCY/", sep="")
    
    do.call(file.remove, list(list.files(ev_vacc_preg_dir, full.names = T)))
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_vacc_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_vacc_preg_dir, subpopulations_names[i],"/Masked"))
    }
    
  } else {
    #Create the EVENTS_VACCINES_PREGNANCY folder in the output dir
    dir.create(paste(output_dir, "EVENTS_VACCINES_PREGNANCY", sep=""))
    ev_vacc_preg_dir<-paste(output_dir, "EVENTS_VACCINES_PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_vacc_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_vacc_preg_dir, subpopulations_names[i],"/Masked"))
    }
  }
  
  if ("EVENTS_MEDICINES" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"EVENTS_MEDICINES"), recursive = T)#delete folder
    dir.create(paste(output_dir, "EVENTS_MEDICINES", sep=""))
    ev_med_dir<-paste(output_dir, "EVENTS_MEDICINES/", sep="")
    
    do.call(file.remove, list(list.files(ev_med_dir, full.names = T)))
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_dir, subpopulations_names[i],"/Masked"))
    }
    
  } else {
    #Create the EVENTS_MEDICINES folder in the output dir
    dir.create(paste(output_dir, "EVENTS_MEDICINES", sep=""))
    ev_med_dir<-paste(output_dir, "EVENTS_MEDICINES/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_dir, subpopulations_names[i],"/Masked"))
    }
  }
  
  if ("EVENTS_VACCINES" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"EVENTS_VACCINES"), recursive = T)#delete folder
    dir.create(paste(output_dir, "EVENTS_VACCINES", sep=""))
    ev_vacc_dir<-paste(output_dir, "EVENTS_VACCINES/", sep="")
    
    do.call(file.remove, list(list.files(ev_vacc_dir, full.names = T)))
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_vacc_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_vacc_dir, subpopulations_names[i],"/Masked"))
    }
    
  } else {
    #Create the EVENTS_VACCINES folder in the output dir
    dir.create(paste(output_dir, "EVENTS_VACCINES", sep=""))
    ev_vacc_dir<-paste(output_dir, "EVENTS_VACCINES/", sep="")
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


if (subpopulations_present=="Yes"){
  for (s in 1: length(subpopulations_names)){
    #get all files in the folder g_intermediate/populations/pregnancy
    pregnancy_files<-list.files(paste0(preg_pop, subpopulations_names[s]))
    #get all files in the folder g_intermediate/populations/diagnoses
    conditions_files<-list.files(paste0(diag_pop, subpopulations_names[s]))
    #get all files in the folder g_intermediate/populations/medicines
    medicines_files<-list.files(paste0(medicines_pop, subpopulations_names[s]), pattern = "f_population")
    #get all files in the folder g_intermediate/populations/vaccines
    vaccines_files<-list.files(paste0(vaccines_pop, subpopulations_names[s]), pattern = "f_population")
    
    #################################################################################
    #Combine DIAGNOSES file by year and condition
    ################################################################################
    #creates filter year_condition
    files<-list()
    for (i in 1: length(conditions_files)){
      files<-append(files,list(paste(unlist(str_split(conditions_files[i],"_"))[1:2], collapse = "_")))
    }
    files<-do.call(c,files)
    #remove duplicates 
    files<-files[!duplicated(files)]
    #create list with names year_condition
    conditions<-vector(mode="list", length=length(files))
    names(conditions)<-files
    rm(files)
    #separate all files into the right category
    for (i in 1:length(conditions)){
      conditions[[i]]<-conditions_files[str_detect(conditions_files, paste0("^", names(conditions)[i]))]
    }
    rm(conditions_files)
    conditions_files<-conditions
    rm(conditions)
    ######################################################################################
    #OUTPUT FOR DIAGNOSES: CONDITIONS
    ######################################################################################
    #################################################################################
    #Combine MEDICINES file by ATC level 1 and year
    ################################################################################
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
    ######################################################################################
    #OUTPUT FOR MEDICINES: MEDICINES_LIST
    ######################################################################################
    #################################################################################
    #Combine VACCINES file by ATC level 1 and year
    ################################################################################
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
    ######################################################################################
    #OUTPUT FOR VACCINES: VACCINES_LIST
    ######################################################################################
    
    
    
    
    
    
    
    
    
    source(paste0(pre_dir,"POI_pre_script.R"))
    
    
  }
} else {
  pregnancy_files<-list.files(preg_pop)
  
  conditions_files<-list.files(paste0(diag_pop, subpopulations_names[s]))
  
  #creates filter year_condition
  files<-list()
  for (i in 1: length(conditions_files)){
    files<-append(files,list(paste(unlist(str_split(conditions_files[i],"_"))[1:2], collapse = "_")))
  }
  
  files<-do.call(c,files)
  #remove duplicates 
  files<-files[!duplicated(files)]
  #create list with names year_condition
  conditions<-vector(mode="list", length=length(files))
  names(conditions)<-files
  rm(files)
  #separate all files into the right category
  for (i in 1:length(conditions)){
    conditions[[i]]<-conditions_files[str_detect(conditions_files, paste0("^", names(conditions)[i]))]
  }
  rm(conditions_files)
}


