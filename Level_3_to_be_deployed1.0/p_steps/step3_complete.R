#completeness of data section#

#SETUP#

library(data.table)
library(tidyverse)
#need stringr for item 21


mydt<-data.table::fread (file=(paste0(path, "mydt_flowchart.csv")))

#completeness
# Number of core variables 90% complete (out of chosen 11)
# Variables are: sex, number of babies/fetuses delivered (nbrbaby), number of malformed in 
# multiple set (nbrmalf), type of birth (type), birth weight (weight), length of gestation in 
# completed weeks (gestlength), survival beyond one week of age (survival), when discovered 
# (whendisc), if prenatally diagnosed, gestational age at discovery in completed weeks 
# (agedisc), age of mother at delivery (agemo), civil registration status (civreg)

cases<-length(unique(mydt$numloc))

comp_vars_1<- c("sex","nbrbaby", "nbrmalf", "type","weight","gestlength","survival","whendisc","agedisc","agemo", "civreg")


# 9== "not known" - IS THIS THE SAME AS MISSING?
#3 == INTERMEDIATE

unk_sex<-nrow(mydt[mydt$sex==9,])

# 9== "not known" - IS THIS THE SAME AS MISSING?
# barplot(table(mydt$nbrbaby),main="nbrbaby")
# barplot(table(is.na(mydt$nbrbaby)),main="nbrbaby, NA")
unk_nbrbaby<-nrow(mydt[mydt$nbrbaby==9,])

# 9== "not known" - IS THIS THE SAME AS MISSING?
# barplot(table(mydt$nbrmalf),main="nbrmalf")
# barplot(table(is.na(mydt$nbrmalf)),main="nbrmalf, NA")
unk_nbrmalf<- nrow(mydt[mydt$nbrmalf==9,])

# 9== "not known" - IS THIS THE SAME AS MISSING?
# barplot(table(mydt$type),main="type")
# barplot(table(is.na(mydt$type)),main="type, NA")
unk_type<-nrow(mydt[mydt$type==9,])

# 9999== "not known" - IS THIS THE SAME AS MISSING?
# barplot(table(mydt$weight),main="weight")
# barplot(table(is.na(mydt$weight)),main="weight, NA")
unk_weight<-nrow(mydt[mydt$weight==9999,])

# 99== "not known" - IS THIS THE SAME AS MISSING?
# barplot(table(mydt$gestlength),main="gestlength")
# barplot(table(is.na(mydt$gestlength)),main="gestlength, NA")
unk_gestlength<-nrow(mydt[mydt$gestlength==99,])

# 9== "not known" - IS THIS THE SAME AS MISSING?
# barplot(table(mydt$survival),main="survival")
# barplot(table(is.na(mydt$survival)),main="survival, NA")
unk_survival<-nrow(mydt[mydt$survival==9,])

# 9== "not known" - IS THIS THE SAME AS MISSING?
# barplot(table(mydt$whendisc),main="whendisc")
# barplot(table(is.na(mydt$whendisc)),main="whendisc, NA")
unk_whendisc<-nrow(mydt[mydt$whendisc==9,])

# 99== "not known" - IS THIS THE SAME AS MISSING?
# barplot(table(mydt$agedisc),main="agedisc")
# barplot(table(is.na(mydt$agedisc)),main="agedisc, NA")
unk_agedisc<-nrow(mydt[mydt$agedisc==99,])

# 99== "not known" - IS THIS THE SAME AS MISSING?
# barplot(table(mydt$agemo),main="agemo")
# barplot(table(is.na(mydt$agemo)),main="agemo, NA")
unk_agemo<-nrow(mydt[mydt$agemo==99,])

# 9== "not known" - IS THIS THE SAME AS MISSING?
# barplot(table(mydt$civreg),main="civreg")
# barplot(table(is.na(mydt$civreg)),main="civreg, NA")
unk_civreg<-nrow(mydt[mydt$civreg==9,])

comp_data_1<-c((unk_sex), (unk_nbrbaby), (unk_nbrmalf),
               (unk_type), (unk_weight), (unk_gestlength),
               (unk_survival), (unk_whendisc), (unk_agedisc),
               (unk_agemo), (unk_civreg))
comp_perc_1<-comp_data_1/cases
perc90<-comp_perc_1<0.1

comp90_tab<-cbind(comp_vars_1, comp_data_1, comp_perc_1, perc90)
colnames(comp90_tab)<-c("variable","number missing", "percent missing", "completeness>90%")
write.csv(comp90_tab, paste0(path_output,"EUROCAT/comp90.csv"), row.names = F )

# 17.	Number of non-core variables 80% complete(out of chosen 26) 
#date of death (death-date)
#if survival= dead--> should have death date
#transform into date format (is.date or as.date)
# condition at discovery (condisc), 
#karyotype of infant/fetus (karyo), 
#post mortem examination (pm),
#if didn't survive
# date of birth of mother (datemo),
#mother's residence code (residmo),
# total number of previous pregnancies (totpreg),
#mother's occupation at time of conception (occupmo),
# assisted conception (assconcept),
#illness before pregnancy (illbef),
#illness during pregnancy (illdur1),
# drugs1, consanguinity (consang),
#previous malformed siblings notified to EUROCAT (prevsib), 
# sibling ID number notified to Central Registry (sib1), 
#siblings with anomalies (sibanom), 
# mother's family with anomalies (moanom), 
#father's family with anomalies (faanom), 
# first postitive prenatal test (firstpre), 
#first surgical procedure for malformation (surgery),
# folic acid supplementation (folic), 
#maternal education (matedu), 
#socioeconomic status of mother (socm),
# socioeconomic status of father (socf),
#migrant status (migrant),
# aetiological classification of malformation (aetiology). 

# Day, month, year 99= Died, not known day or month 44 =Died, not known year
#(Do not use 99 for   "not known" year   of death, as this   will be read as died   in 1999, day and   month not known.)
#222222= Known to be alive at 1 year 333333= Not known if alive or dead at 1 year
# table(mydt$death_date)
# table(is.na(mydt$death_date))

unk_death_date<-nrow(mydt[mydt$death_date==333333,])

# 9  = Not known
# table(mydt$condisc)
# table(is.na(mydt$condisc))
unk_condisc<-nrow(mydt[mydt$condisc==9,])

# 9  = Not known
# table(mydt$karyo)
# table(is.na(mydt$karyo))
unk_karyo<-nrow(mydt[mydt$karyo==9,])

# 9  = Not known
# table(mydt$pm)
# table(is.na(mydt$pm))
unk_pm<-nrow(mydt[mydt$pm==9,])

# Day, month, year 99 = Not known day or month,  44 = Not known year
# table(mydt$datemo)
# table(is.na(mydt$datemo))

unk_datemo<-mydt$datemo[mydt$datemo==99|mydt$datemo==44]


# table(mydt$residmo)
# table(is.na(mydt$residmo))
# table (mydt$residmo)
#99999 code unlike the others --> missing?
unk_residmo<-nrow(mydt[mydt$residmo==99999,])

# 99  = Not known
# table(mydt$totpreg)
# table(is.na(mydt$totpreg))
unk_totpreg<-nrow(mydt[mydt$totpreg==99,])

# 9999  = Not known
# table(mydt$occupmo)
# table(is.na(mydt$occupmo))
unk_occupmo<-nrow(mydt[mydt$occupmo==9999,])

# 9  = Not known
# table(mydt$assconcept)
# table(is.na(mydt$assconcept))
unk_assconcept<-nrow(mydt[mydt$assconcept==9,])

# 9  = Not known
# table(mydt$illbef1)
# table(is.na(mydt$illbef1))
unk_illbef1<-nrow(mydt[mydt$illbef1==9,])

# 9  = Not known
# table(mydt$illdur1)
# table(is.na(mydt$illdur1))
unk_illdur1<-nrow(mydt[mydt$illdur1==9,])

# 9  = Not known
# table(mydt$consang)
# table(is.na(mydt$consang))
unk_consang<-nrow(mydt[mydt$consang==9,])

# 9  = Not known
# table(mydt$prevsib)
# table(is.na(mydt$prevsib))
unk_prevsib<-nrow(mydt[mydt$prevsib==9,])

# is.na
# table(mydt$sib1)
# table(is.na(mydt$sib1))
unk_sib1<-nrow(mydt[is.na(mydt$sib1),])

# 9  = Not known
# table(mydt$sibanom)
# table(is.na(mydt$sibanom))
unk_sibanom<-nrow(mydt[mydt$sibanom==9,])

# 9  = Not known
# table(mydt$moanom)
# table(is.na(mydt$moanom))
unk_moanom<-nrow(mydt[mydt$moanom==9,])

# 9  = Not known
# table(mydt$faanom)
# table(is.na(mydt$faanom))
unk_faanom<-nrow(mydt[mydt$faanom==9,])

# 9  = Not known
# table(mydt$firstpre)
# table(is.na(mydt$firstpre))
unk_firstpre<-nrow(mydt[mydt$firstpre==9,])

# 9  = Not known
# table(mydt$surgery)
# table(is.na(mydt$surgery))
unk_surgery<-nrow(mydt[mydt$surgery==9,])

# 9  = Not known
# table(mydt$folic_g14)
# table(is.na(mydt$folic_g14))
unk_folic<-nrow(mydt[mydt$folic_g14==9,])

# 9  = Not known
# table(mydt$matedu)
# table(is.na(mydt$matedu))
unk_matedu<-nrow(mydt[mydt$matedu==9,])

# 9  = Not known
# table(mydt$socm)
# table(is.na(mydt$socm))
unk_socm<-nrow(mydt[mydt$socm==9,])

# 9  = Not known
# table(mydt$socf)
# table(is.na(mydt$socf))
unk_socf<-nrow(mydt[mydt$socf==9,])

# 9  = Not known
# table(mydt$migrant)
# table(is.na(mydt$migrant))
unk_migrant<-nrow(mydt[mydt$migrant==9,])

comp_vars_2<-c("death_date", "condisc", "karyo", "pm", "datemo", "residmo", "totpreg", "occumo", "assconcept", "illbef1", "illdur1",
               "consang","prevsib","sib1","sibanom", "moanom","faanom","firstpre","surgery","folic", "matedu", "socm", "socf", "migrant")
comp_data_2<-c((unk_death_date), (unk_condisc), (unk_karyo),
               (unk_pm), (unk_datemo), (unk_residmo),
               (unk_totpreg), (unk_occupmo), (unk_assconcept),
               (unk_illbef1), (unk_illdur1),(unk_consang), (unk_prevsib), (unk_sib1),
               (unk_sibanom), (unk_moanom), (unk_faanom),
               (unk_firstpre), (unk_surgery), (unk_folic), (unk_matedu), (unk_socm),
               (unk_socf), (unk_migrant))
comp_perc_2<-comp_data_2/cases
comp80<-(comp_perc_2<0.2)
comp80_tab<-cbind(comp_vars_2, comp_data_2, comp_perc_2, comp80)
colnames(comp80_tab)<- c("variable","number missing", "percent missing", "completeness>80%")
write.csv(comp80_tab, paste0(path_output,"EUROCAT/comp80.csv"), row.names = F )

# 18.
name_comp_18<- "% TOPFA with civil registration known"

comp18_prop<-(nrow(mydt[mydt$type==4,])-nrow(mydt[(mydt$type==4&mydt$civreg==3),]))/nrow(mydt[mydt$type==4,])
comp18<-round((comp18_prop*100),2)
#100% civreg==3==noregistration

# 19.
name_comp_19<- "% live births with one week survival known"
comp19_prop<-(((nrow(mydt[mydt$type==1,])-nrow(mydt[mydt$type==1 & mydt$survival==9,]))/nrow(mydt[mydt$type==1,])))

comp19<-round((comp19_prop*100),2)
#1 cases unknown

# 20.	
name_comp_20<- "Medication exposure recorded using 7 digit ATC codes Yes or No"
#drugs1,2,3,4,5 and extradrugs
#what's the question? is the code 7 digits? is the code missing? 
drug_codes<- c((mydt$drugs1), (mydt$drugs2), (mydt$drugs3), (mydt$drugs4), (mydt$drugs5), (mydt$extra_drugs))
comp_20_tab<- table(nchar(drug_codes)==7)
comp20<-paste0(comp_20_tab[1], "=FALSE, ", comp_20_tab[2], "=TRUE")

# check code length distribution
(table(nchar(drug_codes)))

# 21
name_comp_21<-	"% of ATC codes with 7 digits and in correct format"
# https://www.whocc.no/atc/structure_and_principles/
# char-num-num-char-char-num-num
comp_21_list<- vector()
for (i in 1:length(drug_codes)){
  myex<-drug_codes[i]
  myex.list<-str_extract_all(myex, boundary("character"))
  if(((stringr::str_length(myex))==7)==T
     &all(is.na(as.numeric(myex.list[[1]]))==c(T,F,F,T,T,F,F))==T){comp_21_list[i]<-1}
  else {comp_21_list[i]<-0}}
comp21<-round(((table(comp_21_list)[2])/comp_20_tab[2]*100),2)
# warning is expected: the algorythm uses the NA generated by as.numeric 

# 22.	
name_comp_22<- "% genetic syndromes + microdeletions with syndrome text complete"
#sydrome text== sp_syndrome?
ind_5<-c("Q4471", "Q6190", "Q7484", "Q751", "Q754", "Q7581", "Q87", "Q936", "D821", "75581", "75601", "75604", "7598", "27910")
comp_22_tab<-table((rbind(mydt[mydt$syndrome%in%ind_5,],mydt[mydt$malfo1%in%ind_5,], mydt[mydt$malfo2%in%ind_5,],
                          mydt[mydt$malfo3%in%ind_5,], mydt[mydt$malfo4%in%ind_5,], mydt[mydt$malfo5%in%ind_5,], 
                          mydt[mydt$malfo6%in%ind_5,], mydt[mydt$malfo7%in%ind_5,], mydt[mydt$malfo8%in%ind_5,] ))$sp_syndrome=="")

comp22<- round(((comp_22_tab[2]/sum(comp_22_tab))*100),2)
#7 cases no text of 70 total

# 23.
name_comp_23<- "% malformation with 1 text complete" 
#sp_malfo1
comp_23_tab<- table(mydt$sp_malfo1=="")

comp23<- round(((comp_23_tab[2]/sum(comp_23_tab))*100),2)
#929 cases- but logic check for if text is necessary?

# 24.	
name_comp_24<- "Number of unresolved data edits (excluding free text fields)" 
comp24<- "not applicable"


names_comp_18_24<-c(name_comp_18, name_comp_19, name_comp_20, name_comp_21, name_comp_22, name_comp_23, name_comp_24)

perc_comp_18_24<-c(comp18, comp19, comp20, comp21, comp22, comp23, comp24)

comp_18_24<-cbind(names_comp_18_24, perc_comp_18_24)

colnames(comp_18_24)<-c("Indicator", "% or number complete")
write.csv(comp_18_24, paste0(path_output,"EUROCAT/comp18_24.csv"), row.names = F )
