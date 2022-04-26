#Copy the output to the ForDashboard folder

#Check if ForDashboard folder exists in the g_output, if so delete and recreate
if("ForDashoard" %in% list.files(output_dir)){
  unlink(paste0(output_dir,"ForDashboard"), recursive = T)#delete folder
  dir.create(paste(output_dir, "ForDashboard", sep=""))
  dashboard_dir<-paste(output_dir, "ForDashboard/",sep="")
} else {
  dir.create(paste(output_dir, "ForDashboard", sep=""))
  dashboard_dir<-paste(output_dir, "ForDashboard/",sep="")
}


#Create main folder ALL(no subpopulations) and a new folder for each subpopulation if available
if (subpopulations_present=="No"){
  dir.create(paste(dashboard_dir, "ALL", sep=""))
} else {
  for (i in 1:length(subpopulations_names)){
    dir.create(paste0(dashboard_dir, subpopulations_names[i]))
  }
}

#Output 1:Study source population
if (subpopulations_present=="No"){
  file.copy(file.path(paste0(output_dir,"STUDY_SOURCE_POPULATION/Masked/"),list.files(paste0(output_dir,"STUDY_SOURCE_POPULATION/Masked/"))), 
            paste0(dashboard_dir,"ALL/"))
  file.copy(file.path(paste0(output_dir,"STUDY_SOURCE_POPULATION/Report_01_Study_population_ALL.html")), 
            paste0(dashboard_dir,"ALL/"))
  file.copy(file.path(paste0(output_dir,"STUDY_SOURCE_POPULATION/Report_02_Dates_ALL.html")), 
            paste0(dashboard_dir,"ALL/"))
  file.copy(file.path(paste0(output_dir,"STUDY_SOURCE_POPULATION/Report_03_VisitsLifestyle_ALL.html")), 
            paste0(dashboard_dir,"ALL/"))
  
} else {
  for (subp_names in 1:length(subpopulations_names)){
    file.copy(file.path(paste0(output_dir,"STUDY_SOURCE_POPULATION/", subpopulations_names[subp_names], "/Masked/"),list.files(paste0(output_dir,"STUDY_SOURCE_POPULATION/", subpopulations_names[subp_names], "/Masked/"))), 
              paste0(dashboard_dir, subpopulations_names[subp_names],"/"))
    file.copy(file.path(paste0(output_dir,"STUDY_SOURCE_POPULATION/", subpopulations_names[subp_names],"/Report_01_Study_population_",subpopulations_names[subp_names],".html")), 
              paste0(dashboard_dir,subpopulations_names[subp_names],"/"))
    file.copy(file.path(paste0(output_dir,"STUDY_SOURCE_POPULATION/", subpopulations_names[subp_names],"/Report_02_Dates_",subpopulations_names[subp_names],".html")), 
              paste0(dashboard_dir,subpopulations_names[subp_names],"/"))
    file.copy(file.path(paste0(output_dir,"STUDY_SOURCE_POPULATION/", subpopulations_names[subp_names],"/Report_03_VisitsLifestyle_",subpopulations_names[subp_names],".html")), 
              paste0(dashboard_dir,subpopulations_names[subp_names],"/"))
    file.copy(file.path(paste0(output_dir,"STUDY_SOURCE_POPULATION/", "R_01_01_POPTREE.csv")), 
              paste0(dashboard_dir,subpopulations_names[subp_names],"/"))
  }
}

#Output 2:MEDICINES
if (subpopulations_present=="No"){
file.copy(file.path(paste0(med_dir,"Masked/"),list.files(paste0(med_dir,"Masked/"))), 
                    paste0(dashboard_dir,"ALL/"))
  file.copy(file.path(paste0(med_dir,"MEDICINES_L3.html")), 
            paste0(dashboard_dir,"ALL/"))
  
} else {
  for (subp_names in 1:length(subpopulations_names)){
    file.copy(file.path(paste0(med_dir, subpopulations_names[subp_names], "/Masked/"),list.files(paste0(med_dir,  subpopulations_names[subp_names], "/Masked/"))), 
              paste0(dashboard_dir, subpopulations_names[subp_names],"/"))
    file.copy(file.path(paste0(med_dir,subpopulations_names[subp_names],"_MEDICINES_L3.html")), 
              paste0(dashboard_dir,subpopulations_names[subp_names],"/"))
  }

}

#Output 3:VACCINES
if (subpopulations_present=="No"){
  file.copy(file.path(paste0(vacc_dir,"Masked/"),list.files(paste0(vacc_dir,"Masked/"))), 
            paste0(dashboard_dir,"ALL/"))
  file.copy(file.path(paste0(vacc_dir,"VACCINES_L3.html")), 
            paste0(dashboard_dir,"ALL/"))
} else {
  for (subp_names in 1:length(subpopulations_names)){
    file.copy(file.path(paste0(vacc_dir, subpopulations_names[subp_names], "/Masked/"),list.files(paste0(vacc_dir,  subpopulations_names[subp_names], "/Masked/"))), 
              paste0(dashboard_dir, subpopulations_names[subp_names],"/"))
    file.copy(file.path(paste0(vacc_dir,subpopulations_names[subp_names],"_VACCINES_L3.html")), 
              paste0(dashboard_dir,subpopulations_names[subp_names],"/"))
  }
}

#Output 4:DIAGNOSES
if (subpopulations_present=="No"){
  file.copy(file.path(paste0(diag_dir,"Masked/"),list.files(paste0(diag_dir,"Masked/"))), 
            paste0(dashboard_dir,"ALL/"))
  file.copy(file.path(paste0(diag_dir,"DIAGNOSES_L3.html")), 
            paste0(dashboard_dir,"ALL/"))
} else {
  for (subp_names in 1:length(subpopulations_names)){
    file.copy(file.path(paste0(diag_dir, subpopulations_names[subp_names], "/Masked/"),list.files(paste0(diag_dir,  subpopulations_names[subp_names], "/Masked/"))), 
              paste0(dashboard_dir, subpopulations_names[subp_names],"/"))
    file.copy(file.path(paste0(diag_dir,subpopulations_names[subp_names],"_DIAGNOSES_L3.html")), 
              paste0(dashboard_dir,subpopulations_names[subp_names],"/"))
  }
}

#Output 5: PREGNANCY
if (subpopulations_present=="No"){
  file.copy(file.path(paste0(preg_dir,"Masked/"),list.files(paste0(preg_dir,"Masked/"))), 
            paste0(dashboard_dir,"ALL/"))
  file.copy(file.path(paste0(preg_dir,"PREGNANCY_L3.html")), 
            paste0(dashboard_dir,"ALL/"))
} else {
  for (subp_names in 1:length(subpopulations_names)){
    file.copy(file.path(paste0(preg_dir, subpopulations_names[subp_names], "/Masked/"),list.files(paste0(preg_dir,  subpopulations_names[subp_names], "/Masked/"))), 
              paste0(dashboard_dir, subpopulations_names[subp_names],"/"))
    file.copy(file.path(paste0(preg_dir,subpopulations_names[subp_names],"_PREGNANCY_L3.html")), 
              paste0(dashboard_dir,subpopulations_names[subp_names],"/"))
  }
}

#Output 6: POI
if (subpopulations_present=="No"){
  file.copy(file.path(paste0(ev_med_dir,"Masked/"),list.files(paste0(ev_med_dir,"Masked/"))), 
            paste0(dashboard_dir,"ALL/"))
  file.copy(file.path(paste0(ev_med_preg_dir,"Masked/"),list.files(paste0(ev_med_preg_dir,"Masked/"))), 
            paste0(dashboard_dir,"ALL/"))
  file.copy(file.path(paste0(ev_preg_dir,"Masked/"),list.files(paste0(ev_preg_dir,"Masked/"))), 
            paste0(dashboard_dir,"ALL/"))
  file.copy(file.path(paste0(ev_vacc_dir,"Masked/"),list.files(paste0(ev_vacc_dir,"Masked/"))), 
            paste0(dashboard_dir,"ALL/"))
  file.copy(file.path(paste0(ev_vacc_preg_dir,"Masked/"),list.files(paste0(ev_vacc_preg_dir,"Masked/"))), 
            paste0(dashboard_dir,"ALL/"))
  file.copy(file.path(paste0(med_preg_dir,"Masked/"),list.files(paste0(med_preg_dir,"Masked/"))), 
            paste0(dashboard_dir,"ALL/"))
  file.copy(file.path(paste0(vacc_preg_dir,"Masked/"),list.files(paste0(vacc_preg_dir,"Masked/"))), 
            paste0(dashboard_dir,"ALL/"))
  file.copy(file.path(paste0(poi_dir,"POI_L3.html")), 
            paste0(dashboard_dir,"ALL/"))
} else {
  for (subp_names in 1:length(subpopulations_names)){
    file.copy(file.path(paste0(ev_med_dir, subpopulations_names[subp_names], "/Masked/"),list.files(paste0(ev_med_dir,  subpopulations_names[subp_names], "/Masked/"))), 
              paste0(dashboard_dir, subpopulations_names[subp_names],"/"))
    file.copy(file.path(paste0(ev_med_preg_dir, subpopulations_names[subp_names], "/Masked/"),list.files(paste0(ev_med_preg_dir,  subpopulations_names[subp_names], "/Masked/"))), 
              paste0(dashboard_dir, subpopulations_names[subp_names],"/")) 
    file.copy(file.path(paste0(ev_preg_dir, subpopulations_names[subp_names], "/Masked/"),list.files(paste0(ev_preg_dir,  subpopulations_names[subp_names], "/Masked/"))), 
              paste0(dashboard_dir, subpopulations_names[subp_names],"/"))   
    file.copy(file.path(paste0(ev_vacc_dir, subpopulations_names[subp_names], "/Masked/"),list.files(paste0(ev_vacc_dir,  subpopulations_names[subp_names], "/Masked/"))), 
              paste0(dashboard_dir, subpopulations_names[subp_names],"/"))   
    file.copy(file.path(paste0(ev_vacc_preg_dir, subpopulations_names[subp_names], "/Masked/"),list.files(paste0(ev_vacc_preg_dir,  subpopulations_names[subp_names], "/Masked/"))), 
              paste0(dashboard_dir, subpopulations_names[subp_names],"/")) 
    file.copy(file.path(paste0(med_preg_dir, subpopulations_names[subp_names], "/Masked/"),list.files(paste0(med_preg_dir,  subpopulations_names[subp_names], "/Masked/"))), 
              paste0(dashboard_dir, subpopulations_names[subp_names],"/"))
    file.copy(file.path(paste0(vacc_preg_dir, subpopulations_names[subp_names], "/Masked/"),list.files(paste0(vacc_preg_dir,  subpopulations_names[subp_names], "/Masked/"))), 
              paste0(dashboard_dir, subpopulations_names[subp_names],"/"))
    file.copy(file.path(paste0(poi_dir,subpopulations_names[subp_names],"_PREGNANCY_L3.html")), 
              paste0(dashboard_dir,subpopulations_names[subp_names],"/"))
  }
  
}

#Output 7: EUROCAT
if (subpopulations_present=="No"){
file.copy(file.path(paste0(eurocat_dir,"Masked/"),list.files(paste0(eurocat_dir,"Masked/"))), 
          paste0(dashboard_dir,"ALL/"))
file.copy(file.path(paste0(eurocat_dir,"EUROCAT_DQI_L3.html")), 
          paste0(dashboard_dir,"ALL/"))
}else{
  for (subp_names in 1:length(subpopulations_names)){
    file.copy(file.path(paste0(eurocat_dir,"Masked/"),list.files(paste0(eurocat_dir,"Masked/"))), 
              paste0(dashboard_dir,subpopulations_names[subp_names],"/"))
    file.copy(file.path(paste0(eurocat_dir,"EUROCAT_DQI_L3.html")), 
              paste0(dashboard_dir,subpopulations_names[subp_names],"/"))
  }
}

#Output 8: Subpopulations names
if (subpopulations_present=="Yes"){
  for (subp_names in 1:length(subpopulations_names)){
    file.copy(file.path(paste0(output_dir,"Info/subpopulations_names.csv"),list.files(paste0(output_dir,"Info/subpopulations_names.csv"))), 
              paste0(dashboard_dir,subpopulations_names[subp_names],"/"))
  }
}