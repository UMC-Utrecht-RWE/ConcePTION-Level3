#Author: Vjola Hoxhaj Drs.
#email: v.hoxhaj@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/07/2021

`%!in%` = Negate(`%in%`)
#load codelist
if(!is.null(study_name_codelist)){
  codelist_directory<-list.files(pre_dir,paste0("^", study_name_codelist))
  codelist<-fread(paste0(pre_dir,codelist_directory))
  names(codelist)<-c("Condition","Coding system", "Code", "Code name", "Concept", "Concept name","Tags","System", "Event abreviation", "Type", "Code range")
  codelist<-codelist[,c("Condition","Coding system", "Code", "Code name", "Concept", "Concept name","Tags", "Code range")]
  codelist[,Condition:=gsub("-","_",Condition)]
} else {
  codelist<-fread(paste0(pre_dir,"Data_characterisation_EVENTS_codelist.csv"))
}
#select only necessary columns
codelist<-codelist[,c("Condition", "Coding system", "Code")]
setnames(codelist,"Coding system", "Coding_system")

conditions_vocabularies<-codelist[!duplicated(Coding_system),Coding_system]
conditions_to_start_with<-c(conditions_vocabularies[str_detect(conditions_vocabularies, "^ICD")], 
                            conditions_vocabularies[str_detect(conditions_vocabularies, "^ICPC")], 
                            conditions_vocabularies[str_detect(conditions_vocabularies, "^MTHICD")])
conditions_rcd<-conditions_vocabularies[str_detect(conditions_vocabularies, "^RCD")]
conditions_snomed_codes<-conditions_vocabularies[str_detect(conditions_vocabularies, "^SNOMED")]

#remove dots for read codes
codelist<-codelist[Coding_system %in% conditions_rcd, Code:=str_replace_all(Code,"[.]","")]
while (codelist[Coding_system %in% conditions_rcd & str_detect(Code,"[.]"),.N]>0){
  codelist<-codelist[Coding_system %in% conditions_rcd, Code:=str_replace_all(Code,"[.]","")]
}

#Create variable dot_present
codelist[,dot_present:=str_detect(codelist[,Code],"\\.")]
#Create variable code_no_dot by removing dot from all codes
codelist[,code_no_dot:=gsub("\\.","",codelist[,Code])]
vocabularies_list<-codelist[!duplicated(Coding_system), Coding_system]
#put all information in a list
conditions<-vector(mode="list", length=length(unique(na.omit(codelist[,Condition]))))
names(conditions)<-unique(na.omit(codelist[,Condition]))
for (i in 1:length(conditions)){
  vocabularies<-vector(mode="list", length=length(unique(na.omit(codelist[,Coding_system]))))
  names(vocabularies)<-unique(na.omit(codelist[,Coding_system]))
  for (j in 1:length(vocabularies)){
    vocabularies[[j]]<-codelist[Condition==names(conditions)[i] & Coding_system==names(vocabularies)[j], Code]
  }
  conditions[[i]]<-list.append(conditions[[i]],vocabularies)
  rm(vocabularies)
}

#remove empty vocabularies
conditions<-lapply(conditions, function(x) Filter(length, x))
conditions<-Filter(function(k) length(k)>0, conditions)
#################################################################################################################
#Rule: start with
#Coding system: ICD9, ICD9CM, ICD10, ICD10CM, ICPC
#################################################################################################################
#vocabularies that will be filtered with start with
#vocabularies that will be filtered with start with
conditions_start<-list()
for(i in 1:length(conditions)){
  conditions_start[[i]]<-conditions[[i]][names(conditions[[i]]) %in% conditions_to_start_with]
}

names(conditions_start)<-names(conditions)

for(i in 1:length(conditions_start)){
  lapply(conditions_start[[i]], function(x) x[names(x) %in% c("Code")])
}
conditions_start<-lapply(conditions_start, function(x) Filter(length, x))
conditions_start<-Filter(function(k) length(k)>0, conditions_start)

################################################################################################################
#Rule:Remove dot, start with
#Coding system: Read codes v2
###############################################################################################################
conditions_read<-list()

for(i in 1:length(conditions)){
  conditions_read[[i]]<-conditions[[i]][names(conditions[[i]]) %in% conditions_rcd]
}

names(conditions_read)<-names(conditions)
conditions_read<-lapply(conditions_read, function(x) Filter(length, x))
conditions_read<-Filter(function(k) length(k)>0, conditions_read)
################################################################################################################
#Rule: match exactly
#Coding system: SNOMEDCT_US
#################################################################################################################
#SNOMED codes
conditions_snomed<-list()
for(i in 1:length(conditions)){
  conditions_snomed[[i]]<-conditions[[i]][names(conditions[[i]]) %in% conditions_snomed_codes]
}
names(conditions_snomed)<-names(conditions)

conditions_snomed<-lapply(conditions_snomed, function(x) Filter(length, x))
conditions_snomed<-Filter(function(k) length(k)>0, conditions_snomed)
################################################################################################################
#output folder for Info report in g_output
if ("Info" %in% list.files(output_dir)){
  info_dir<-paste(output_dir, "Info/",sep="")
  do.call(file.remove, list(list.files(info_dir, full.names = T)))
} else {
  #Create the Info folder in the output dir
  dir.create(paste(output_dir, "Info", sep=""))
  info_dir<-paste(output_dir, "Info/", sep="")
}

codelist[,comb:=paste(Condition, Coding_system,"_")]
codelist[,dot_present:=NULL][,code_no_dot:=NULL]
codelist[,Codes:=paste0(Code, collapse = ", "), by="comb"]
codelist<-codelist[!duplicated(comb)]
codelist[,Code:=NULL][,comb:=NULL]
setnames(codelist,"Condition","event_definition")

write.csv(codelist, paste0(info_dir, "data_characterisation_codelist.csv"), row.names = F)
rm(codelist)
#########################################################################################
#Pregnancy codelist
#########################################################################################
#load codelist
codelist_pregnancy<-fread(paste0(pre_dir,"Data_characterisation_pregnancy_matcho_algorithm.csv"))
#remove "/" if present
codelist_pregnancy[,vocabulary:=gsub("/","",vocabulary)]
#Create variable dot_present
codelist_pregnancy[,dot_present:=str_detect(codelist_pregnancy[,code],"[.]")]

vocabularies_list_pregnancy<-codelist_pregnancy[!duplicated(vocabulary), vocabulary]

pregnancy_to_start_with<-c(vocabularies_list_pregnancy[str_detect(vocabularies_list_pregnancy, "^ICD")], 
                           vocabularies_list_pregnancy[str_detect(vocabularies_list_pregnancy, "^ICPC")], 
                           vocabularies_list_pregnancy[str_detect(vocabularies_list_pregnancy, "^MTHICD")])
pregnancy_rcd<-vocabularies_list_pregnancy[str_detect(vocabularies_list_pregnancy, "^RCD")]
pregnancy_snomed_codes<-vocabularies_list_pregnancy[str_detect(vocabularies_list_pregnancy, "^SNOMED")]

#Create variable code_no_dot by removing dot from all codes
codelist_pregnancy<-codelist_pregnancy[vocabulary %in% conditions_rcd, code:=str_replace_all(code,"[.]","")]
while (codelist_pregnancy[vocabulary %in% conditions_rcd & str_detect(code,"[.]"),.N]>0){
  codelist_pregnancy<-codelist_pregnancy[vocabulary %in% conditions_rcd, code:=str_replace_all(code,"[.]","")]
}

#put all information in a list
stage_pregnancy<-vector(mode="list", length=length(unique(na.omit(codelist_pregnancy[,pregnancy_filter]))))
names(stage_pregnancy)<-unique(na.omit(codelist_pregnancy[,pregnancy_filter]))
for (i in 1:length(stage_pregnancy)){
  vocabularies_pregnancy<-vector(mode="list", length=length(unique(na.omit(codelist_pregnancy[,vocabulary]))))
  names(vocabularies_pregnancy)<-unique(na.omit(codelist_pregnancy[,vocabulary]))
  for (j in 1:length(vocabularies_pregnancy)){
    vocabularies_pregnancy[[j]]<-codelist_pregnancy[pregnancy_filter==names(stage_pregnancy)[i] & vocabulary==names(vocabularies_pregnancy)[j], code]
  }
  stage_pregnancy[[i]]<-list.append(stage_pregnancy[[i]],vocabularies_pregnancy)
  rm(vocabularies_pregnancy)
}

#remove empty vocabularies
stage_pregnancy<-lapply(stage_pregnancy, function(x) Filter(length, x))
stage_pregnancy<-Filter(function(k) length(k)>0, stage_pregnancy)
#################################################################################################################
#Rule: start with
#Coding system: ICD9CM, ICD10CM, ICPC2P
#################################################################################################################
#vocabularies that will be filtered with start with
stage_pregnancy_start<-list()
for(i in 1:length(stage_pregnancy)){
  stage_pregnancy_start[[i]]<-stage_pregnancy[[i]][names(stage_pregnancy[[i]]) %in% pregnancy_to_start_with]
}

names(stage_pregnancy_start)<-names(stage_pregnancy)

for(i in 1:length(stage_pregnancy_start)){
  lapply(stage_pregnancy_start[[i]], function(x) x[names(x) %in% c("Code")])
}
stage_pregnancy_start<-lapply(stage_pregnancy_start, function(x) Filter(length, x))
################################################################################################################
#Rule:Remove dot, start with
#Coding system: RCD, RCD2
###############################################################################################################
stage_pregnancy_read<-list()
for(i in 1:length(stage_pregnancy)){
  stage_pregnancy_read[[i]]<-stage_pregnancy[[i]][names(stage_pregnancy[[i]]) %in% pregnancy_rcd]
}
names(stage_pregnancy_read)<-names(stage_pregnancy)

for(i in 1:length(stage_pregnancy_read)){
  lapply(stage_pregnancy_read[[i]], function(x) x[names(x) %in% c("Code")])
}
stage_pregnancy_read<-lapply(stage_pregnancy_read, function(x) Filter(length, x))

################################################################################################################
#Rule: match exactly
#Coding system: SNOMEDCT_US
#################################################################################################################
#SNOMED codes
stage_pregnancy_snomed<-list()
for(i in 1:length(stage_pregnancy)){
  stage_pregnancy_snomed[[i]]<-stage_pregnancy[[i]][names(stage_pregnancy[[i]]) %in% pregnancy_snomed_codes]
}
names(stage_pregnancy_snomed)<-names(stage_pregnancy)

################################################################################################################

codelist_pregnancy[,dot_present:=NULL][,code:=NULL]
setnames(codelist_pregnancy, "original_code", "code")

write.csv(codelist_pregnancy, paste0(info_dir, "data_characterisation_codelist_pregnancy.csv"), row.names = F)
rm(codelist_pregnancy)


if(subpopulations_present=="Yes"){
  subpop_names_export<-data.table(DAP=data_access_provider_name, data_source=data_source_name, subpopulations_names=subpopulations_names)
  write.csv(subpop_names_export, paste0(info_dir, "subpopulations_names.csv"), row.names = F)
}