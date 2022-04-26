#Author: Vjola Hoxhaj Drs.
#email: v.hoxhaj@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/07/2021

`%!in%` = Negate(`%in%`)
#load codelist
if(!is.null(study_name_codelist)){
  codelist_directory<-list.files(pre_dir,paste0("^", study_name_codelist))
  codelist<-fread(paste0(pre_dir,codelist_directory), colClasses = "character")
  codelist[,event_definition:=gsub("-","_",event_definition)]
  } else {
  codelist<-fread(paste0(pre_dir,"Data_characterisation_EVENTS_codelist.csv"), colClasses = "character")
}

#select only necessary columns
codelist<-codelist[,c("event_definition", "coding_system", "code")]
codelist<-codelist[,coding_system:=gsub("/","",coding_system)]
#remove duplicates
codelist[,comb:=paste(event_definition,coding_system,code,sep="^")]
codelist<-codelist[!duplicated(comb)]
codelist[,comb:=NULL]


conditions_vocabularies<-codelist[!duplicated(coding_system),coding_system]
conditions_to_start_with<-c(conditions_vocabularies[str_detect(conditions_vocabularies, "^ICD")], 
                            conditions_vocabularies[str_detect(conditions_vocabularies, "^ICPC")], 
                            conditions_vocabularies[str_detect(conditions_vocabularies, "^MTHICD")])
conditions_rcd<-conditions_vocabularies[str_detect(conditions_vocabularies, "^RCD")]
conditions_snomed_codes<-conditions_vocabularies[str_detect(conditions_vocabularies, "^SNOMED")]
conditions_other_codes<-conditions_vocabularies[!(conditions_vocabularies %in% c(conditions_to_start_with,conditions_rcd,conditions_snomed_codes))]

#remove dots for read codes
codelist<-codelist[coding_system %in% conditions_rcd, code:=str_replace_all(code,"[.]","")]
while (codelist[coding_system %in% conditions_rcd & str_detect(code,"[.]"),.N]>0){
  codelist<-codelist[coding_system %in% conditions_rcd, code:=str_replace_all(code,"[.]","")]
}
#Create variable dot_present
codelist[,dot_present:=str_detect(codelist[,code],"\\.")]
#Create variable code_no_dot by removing dot from all codes
codelist[,code_no_dot:=gsub("\\.","",codelist[,code])]
vocabularies_list<-codelist[!duplicated(coding_system), coding_system]
#put all information in a list
conditions<-vector(mode="list", length=length(unique(na.omit(codelist[,event_definition]))))
names(conditions)<-unique(na.omit(codelist[,event_definition]))
for (i in 1:length(conditions)){
  vocabularies<-vector(mode="list", length=length(unique(na.omit(codelist[,coding_system]))))
  names(vocabularies)<-unique(na.omit(codelist[,coding_system]))
  for (j in 1:length(vocabularies)){
    vocabularies[[j]]<-codelist[event_definition==names(conditions)[i] & coding_system==names(vocabularies)[j], code]
  }
  conditions[[i]]<-list.append(conditions[[i]],vocabularies)
  rm(vocabularies)
}

#remove empty vocabularies
conditions<-lapply(conditions, function(x) Filter(length, x))

#################################################################################################################
#Rule: start with
#Coding system: ICD9, ICD9CM, ICD10, ICD10CM, ICPC
#################################################################################################################
#vocabularies that will be filtered with start with
conditions_start<-list()
for(i in 1:length(conditions)){
  conditions_start[[i]]<-conditions[[i]][names(conditions[[i]]) %in% conditions_to_start_with]
}

names(conditions_start)<-names(conditions)

for(i in 1:length(conditions_start)){
  lapply(conditions_start[[i]], function(x) x[names(x) %in% c("code")])
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
#Rule: match exactly
#Coding system: other codes
#################################################################################################################
#other codes
conditions_other<-list()
for(i in 1:length(conditions)){
  conditions_other[[i]]<-conditions[[i]][names(conditions[[i]]) %in% conditions_other_codes]
}
names(conditions_other)<-names(conditions)

conditions_other<-lapply(conditions_other, function(x) Filter(length, x))
conditions_other<-Filter(function(k) length(k)>0, conditions_other)
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

codelist[,comb:=paste(event_definition, coding_system,"_")]
codelist[,dot_present:=NULL][,code_no_dot:=NULL]
codelist[,codes:=paste0(code, collapse = ", "), by="comb"]
codelist<-codelist[!duplicated(comb)]
codelist[,code:=NULL][,comb:=NULL]
codelist<-codelist[order(event_definition,coding_system)]

write.csv(codelist, paste0(info_dir, "data_characterisation_codelist.csv"), row.names = F)
rm(codelist)
#########################################################################################
#Pregnancy codelist
#########################################################################################
#load codelist
codelist_pregnancy<-fread(paste0(pre_dir,"Data_characterisation_pregnancy_matcho_algorithm.csv"), colClasses = "character")
#remove "/" if present
codelist_pregnancy[,coding_system:=gsub("/","",coding_system)]
#Create variable dot_present
codelist_pregnancy[,dot_present:=str_detect(codelist_pregnancy[,code],"[.]")]
#remove duplicates
codelist_pregnancy[,comb:=paste(event_definition,coding_system,code,sep="^")]
codelist_pregnancy<-codelist_pregnancy[!duplicated(comb)]
codelist_pregnancy[,comb:=NULL]


vocabularies_list_pregnancy<-codelist_pregnancy[!duplicated(coding_system), coding_system]

pregnancy_to_start_with<-c(vocabularies_list_pregnancy[str_detect(vocabularies_list_pregnancy, "^ICD")], 
                           vocabularies_list_pregnancy[str_detect(vocabularies_list_pregnancy, "^ICPC")], 
                           vocabularies_list_pregnancy[str_detect(vocabularies_list_pregnancy, "^MTHICD")])
pregnancy_rcd<-vocabularies_list_pregnancy[str_detect(vocabularies_list_pregnancy, "^RCD")]
pregnancy_snomed_codes<-vocabularies_list_pregnancy[str_detect(vocabularies_list_pregnancy, "^SNOMED")]
pregnancy_other_codes<-vocabularies_list_pregnancy[!(vocabularies_list_pregnancy %in% c(pregnancy_to_start_with,pregnancy_rcd,pregnancy_snomed_codes))]

#Create variable code_no_dot by removing dot from all codes
#remove dots for read codes

codelist_pregnancy<-codelist_pregnancy[coding_system %in% conditions_rcd, code:=str_replace_all(code,"[.]","")]
while (codelist_pregnancy[coding_system %in% conditions_rcd & str_detect(code,"[.]"),.N]>0){
  codelist_pregnancy<-codelist_pregnancy[coding_system %in% conditions_rcd, code:=str_replace_all(code,"[.]","")]
}

#put all information in a list
stage_pregnancy<-vector(mode="list", length=length(unique(na.omit(codelist_pregnancy[,event_definition]))))
names(stage_pregnancy)<-unique(na.omit(codelist_pregnancy[,event_definition]))
for (i in 1:length(stage_pregnancy)){
  vocabularies_pregnancy<-vector(mode="list", length=length(unique(na.omit(codelist_pregnancy[,coding_system]))))
  names(vocabularies_pregnancy)<-unique(na.omit(codelist_pregnancy[,coding_system]))
  for (j in 1:length(vocabularies_pregnancy)){
    vocabularies_pregnancy[[j]]<-codelist_pregnancy[event_definition==names(stage_pregnancy)[i] & coding_system==names(vocabularies_pregnancy)[j], code]
  }
  stage_pregnancy[[i]]<-list.append(stage_pregnancy[[i]],vocabularies_pregnancy)
  rm(vocabularies_pregnancy)
}

#remove empty vocabularies
stage_pregnancy<-lapply(stage_pregnancy, function(x) Filter(length, x))

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
  lapply(stage_pregnancy_start[[i]], function(x) x[names(x) %in% c("code")])
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
  lapply(stage_pregnancy_read[[i]], function(x) x[names(x) %in% c("code")])
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
stage_pregnancy_snomed<-lapply(stage_pregnancy_snomed, function(x) Filter(length, x))
################################################################################################################
#Rule: match exactly
#Coding system: other codes
#################################################################################################################
#other codes
stage_pregnancy_other<-list()
for(i in 1:length(stage_pregnancy)){
  stage_pregnancy_other[[i]]<-stage_pregnancy[[i]][names(stage_pregnancy[[i]]) %in% pregnancy_other_codes]
}
names(stage_pregnancy_other)<-names(stage_pregnancy)
stage_pregnancy_other<-lapply(stage_pregnancy_other, function(x) Filter(length, x))
################################################################################################################

codelist_pregnancy[,dot_present:=NULL][,code:=NULL]
setnames(codelist_pregnancy, "code_original", "code")

write.csv(codelist_pregnancy, paste0(info_dir, "data_characterisation_codelist_pregnancy.csv"), row.names = F)
rm(codelist_pregnancy)


if(subpopulations_present=="Yes"){
  subpop_names_export<-data.table(DAP=data_access_provider_name, data_source=data_source_name, subpopulations_names=subpopulations_names)
  write.csv(subpop_names_export, paste0(info_dir, "subpopulations_names.csv"), row.names = F)
}