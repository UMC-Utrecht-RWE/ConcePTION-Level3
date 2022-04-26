#DQI ascertainment (items 0-7)

#SETUP#

mydt<-data.table::fread (file=(paste0(eurocat_tmp, "mydt_flowchart.csv")))

# colnames(mydt)
#what syndrome codes are in the data?
#EUROCAT codes aren't only in syndrome, but also the malfo variables... more complicated 


all_codes<-unique(c(mydt$syndrome, mydt$malfo1, mydt$malfo2, mydt$malfo3, mydt$malfo4, mydt$malfo5, mydt$malfo6, mydt$malfo7,mydt$malfo8))



####################################
#define each of the EUROCAT indicators


name_ind_0<-"number of cases"
#pregnancies or babies/fetuses? Discussed with Carlos, decided pregnancies
ind_0<-"length(unique(mydt$numloc))"

# Total congenital anomaly prevalence D215, D821, D1810, P350, P351, P371, 74, 75, 27910, 2281, 76076, 76280, 7710, 7711, 77121
name_ind_1<-"Total congenital anomaly prevalence"
ind_1<-c("D215", "D821", "D1810", "P350", "P351", "P371", "74", "75", "27910", "2281", "76076", "76280", "7710", "7711", "77121")

#what syndome codes==ancephalus? PG 92 & 101 EUROCAT
name_ind_2<-"Prevalence of anencephalus"
ind_2<-c("Q00", "740", "Q891")

#what syndome codes==cardiac defect? PG 98 & 106 EUROCAT
name_ind_3<-"Prevalence of severe cardiac defects"
ind_3<-c("Q202","Q203","Q204", "Q205", "Q206", "Q211", "Q213", "Q226", "Q234","Q248","Q249", "Q250", "Q26","745", "746", "7470","7471", "7472","7473","7474")

#Includes codes for corpus callosum anomalies (Q040), cataract (Q120), coarctation of aorta (Q251), Hirschprung's disease (Q431) and craniosynostosis (Q750).
name_ind_4<- "Prevalence of selected postnatal diagnosis" 
ind_4<-c("Q040", "Q120", "Q251", "Q431", "Q750")

# Prevalence of genetic syndromes and microdeletions, page 96 table EUROCAT
name_ind_5<- "Prevalence genetic syndromes and microdeletions"
ind_5<-c("Q4471", "Q6190", "Q7484", "Q751", "Q754", "Q7581", "Q87", "Q936", "D821", "75581", "75601", "75604", "7598", "27910")

#fetal death is mydt$type= 2 or 3 or 4
#1= Live birth, 2 = Stillbirth, 3 = Spontaneous abortion, 4 = TOPFA, 9 = Not known
# name_ind_6<-"Prevalence of malformed fetal deaths"
#definition of "malformed"? Having any diagnoses in "syndrome"? 
# ind_6<-fetaldeath[(fetaldeath$syndrome!=""),]
#OR number malformed>1 ?
#check with Vjola- inconsistency between malformation definition
#slightly different results
name_ind_6<-"Prevalence of malformed fetal deaths"

mydt$fetaldeath[mydt$type==1]<-0
mydt$fetaldeath[mydt$type==2]<-1
mydt$fetaldeath[mydt$type==3]<-1
mydt$fetaldeath[mydt$type==4]<-1
mydt$fetaldeath[mydt$type==9]<-NA

table(mydt$fetaldeath)
ind_6<-"mydt$nbrmalf, mydt$type"

name_ind_7<-"Down Syndrome"
#EUROCAT pg96 says codes== Q90 and 7580 ---BUT the sp_syndrome variable shows Q909 coded as Down Syndrom and Trisomy 21 (Valencia coding error?)
  #SOLUTION# Vjola 31.5 - use all codes starting with Q90x
    # 3.5 Detailed Congenital Anomaly Coding Guideline pg 103

ind_7<-c(all_codes[(startsWith(all_codes, "Q90"))], "7580")
#further calculations for Down 
# Down syndrome: Observed/Expected ratio by maternal age
# This calculates the ratio of Observed to Expected Down Syndrome cases. Observed (O) is the 
# number of livebirth (LB) + fetal death (FD) ???20 weeks gestational age + the number of TOPFA 
# corrected for probability of fetal survival to 20 weeks. 
# The calculation is:
#   O = LB + FD + (TOPFA corrected to 20 weeks gestational age) 
# Expected (E) is based on EUROCAT average 5 year maternal age-specific estimates (LB +FD + TOPFA corrected to 20 weeks)
# for the time period of analysis applied to the maternal age 
# profile of each registry birth population.

# this method (for loop finding the indicator codes anywhere in the data) creates a binary variable for each indicator, so the dataset can be subset
# and analyzed for any of them (and overlaps) without writing new files or storing data.frames in R 

# BUT IT IS PRETTY SLOW- FOR THIS RELETIVELY SMALL DATASET, IT TAKES 2:45 MINUTES
# BUT ALSO, IT ONLY NEEDS TO BE RUN ONCE PER DATASET

#cases defined as pregnancies
prev_ind_0<-length(unique(mydt$numloc))

#cases defined as individual fetuses/babies--> 1-5 singletons to quintuplets, but 6, 7, 8 and 9 are unspecified.
# table(mydt$nbrbaby)

for(i in 1:nrow(mydt)){
  if(any(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")]%in%ind_1)==T) {mydt$ind_1[i]<-1} else {mydt$ind_1[i]<-0}}
prev_ind_1<-table(mydt$ind_1)[2]

for(i in 1:nrow(mydt)){
  if(any(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")]%in%ind_2)==T) {mydt$ind_2[i]<-1} else {mydt$ind_2[i]<-0}}
prev_ind_2<-table(mydt$ind_2)[2]

for(i in 1:nrow(mydt)){
  if(any(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")]%in%ind_3)==T) {mydt$ind_3[i]<-1} else {mydt$ind_3[i]<-0}}
prev_ind_3<-table(mydt$ind_3)[2]

for(i in 1:nrow(mydt)){
  if(any(mydt[i,]%in%ind_4)==T) {mydt$ind_4[i]<-1} else {mydt$ind_4[i]<-0}}
prev_ind_4<-table(mydt$ind_4)[2]

for(i in 1:nrow(mydt)){
  if(any(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")]%in%ind_5)==T) {mydt$ind_5[i]<-1} else {mydt$ind_5[i]<-0}}
prev_ind_5<-table(mydt$ind_5)[2]

mydt$ind_6<-0
mydt$ind_6[(mydt$fetaldeath==1)&(mydt$nbrmalf>=1)]<-1
prev_ind_6<-table(mydt$ind_6)[2]

for(i in 1:nrow(mydt)){
  if(any(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")]%in%ind_7)==T) {mydt$ind_7[i]<-1} else {mydt$ind_7[i]<-0}}
prev_ind_7<-table(mydt$ind_7)[2]

# export indicator data

names_asc<-c(name_ind_0, name_ind_1, name_ind_2, name_ind_3, name_ind_4, name_ind_5, name_ind_6, name_ind_7)
raw_asc<-c((prev_ind_0),(prev_ind_1), (prev_ind_2), (prev_ind_3), (prev_ind_4), (prev_ind_5), (prev_ind_6), (prev_ind_7))
raw_asc_prev<-round((raw_asc/prev_ind_0),4)
ci<-list() 
  for (i in 1:length(raw_asc)) {
    ci[[i]] <- binom.test(x = raw_asc[i], n = prev_ind_0, p=(raw_asc[i]/prev_ind_0) )
  }
ci<-sapply(ci, function(x) unclass(x$conf.int))
ci<-round(ci, 4)

ci_asc<-vector()
for(i in 1:ncol(ci)){
  ci_asc[i]<- paste0(ci[1,i],"-", ci[2,i])
}



asc_tab<-cbind(names_asc,raw_asc, raw_asc_prev, ci_asc)
colnames(asc_tab)<-c("Indicator", "Total", "Prevalence", "95% CI")

write.csv(asc_tab,paste0(eurocat_dir,"ascertainment.csv"), row.names = F) 


# maternal age and birth type total dataset 

mydt$agemocat<-(cut(mydt$agemo,c(0,19, 24, 29, 34, 39, 44,100)))

# 1= Live birth, 2 = Stillbirth, 3 = Spontaneous abortion, 4 = TOPFA, 9 = Not known
type_table<-(table(mydt$agemocat, mydt$type))
names_type<-c("Live Birth", "Stillbirth", "spontaneos abortion", "TOPFA", "unknown")
type_df<- cbind(type_table[,1], type_table[,2], names_type)
colnames(type_df)<-c("Maternal Age", "# of Type of Birth", "Birth Type")

down_table<-(table(mydt$agemocat, mydt$ind_7))
down_perc<-round(((down_table[,2]/prev_ind_7)*100),4)
age_type_down<-cbind(type_table, down_table[,2],down_perc)
colnames(age_type_down)<-c("Live Birth", "Still Birth", "Spontaneous Abortion", "TOPFA", "# of Downs Cases", "% of total Down's Cases")
write.csv(age_type_down, paste0(eurocat_dir,"birth_type_downsynd_by_agemo.csv"), row.names = F )



#  DQI_accuracy
#items 8-15


#INDICATOR SELECTION


name_ind_8<-"% potential multiples according to the flowchart variable"
#VJOLA 31/5 FLOWCHART --> a.	Section 3.4 Step 22 EUROCAT
# ind_8<-



name_ind_9<- "% fetal death with  post-mortem"
# only results known? or both known and unknown?f
# % fetal deaths with post-mortem examination carried out
# 1 = Performed, results known 2 = Performed, results not known, 3 = Not performed, 4 = Macerated fetus, 9 = Not known
mydt$postmortemYN[mydt$pm==1]<-1
mydt$postmortemYN[mydt$pm==2]<-1
mydt$postmortemYN[mydt$pm==3]<-0
mydt$postmortemYN[mydt$pm==4]<-0
mydt$postmortemYN[mydt$pm==9]<-0

table(mydt$postmortemYN)

name_ind_10<- "% TOPFA with postmortem"
# % TOPFA (GA ??? 15 weeks) with post-mortem examination carried out
# " mydt$type==4 & mydt$pm=1|2"


name_ind_11<- "% chromosomal with karyotype (excluding trisomy 13, 18, 21)"
# 1= Performed, result known, 2 = Performed results not known, 3 = Not performed, 4 = Probe test performed, 8 = Failed, 9 = Not known
# %chromosomal cases (except trisomy 13 (q907), 18 (q913) and 21(q909)) with karyotype text 
table(mydt$karyo)
# (mydt$sp_karyo)
#definition of chromosomal cases from EUROCAT page 96 
cases_karyo<-mydt[(mydt$sp_karyo!=""),]
table(cases_karyo$syndrome)
ind_11<-c(all_codes[(startsWith(all_codes, "Q90"))], all_codes[(startsWith(all_codes, "Q91"))], all_codes[(startsWith(all_codes, "Q92"))],
          all_codes[(startsWith(all_codes, "Q93"))],all_codes[(startsWith(all_codes, "Q96"))],all_codes[(startsWith(all_codes, "Q96"))],
          all_codes[(startsWith(all_codes, "Q97"))],all_codes[(startsWith(all_codes, "Q98"))],all_codes[(startsWith(all_codes, "Q99"))])

name_ind_12<- "% Non-chromosomal potential multiple cases with known karyotype" 
#NEED flowchart

name_ind_13<-"Prevalence of selected exact 4-digit Q-BPA codes Selected Q-BPA codes"  
ind_13<-c("Q0000", "Q0020", "Q0400", "Q0435", "Q2110", "Q2121", "Q2510", "Q2511", "Q2620", "Q3380", "Q3911", "Q4420", "Q6141", "Q6420", "Q7131", "Q8980")  


name_ind_14<- "Prevalence of selected unspecified Q codes Selected unspecified codes" 
#= Q049, Q059, Q249, Q339, Q439, Q549, Q639, Q749, Q799, Q899, Q999 
ind_14<-c("Q049", "Q059", "Q249", "Q339", "Q439", "Q549", "Q639", "Q749", "Q799", "Q899", "Q999" )

name_ind_15<-	"% livebirths with ASD, VSD, hydronephrosis, hypospadia or club foot with known data on surgery"
ind_15<-c("Q210", "7454", "al21", "Q211", "7455", "Q2111", "al22", "Q620", "75320", all_codes[(startsWith(all_codes, "Q54"))], "75260", "Q660", "75450") 
#1 = Performed (or expected) in the first year of life2 = Performed (or expected) after the first year of life 3 = Prenatal surgery
#4 = No surgery required5= Too severe for surgery6 = Died before surgery 9 = Not known

#%in% function delivers T or F if the value of syndrome or malfo variables matches any value in ind_x
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/match.html

prev_ind_8<-nrow(mydt[mydt$flowchart=="M"])

prev_ind_9<-nrow(mydt[(mydt$fetaldeath==1)&(mydt$postmortemYN==1)])

for(i in 1:nrow(mydt)){
  if((mydt$type[i]==4 & mydt$pm[i]==(1|2))==T) {mydt$ind_10[i]<-1} else {mydt$ind_10[i]<-0}}
prev_ind_10<-prev_ind_0-table(mydt$ind_10)[1]

for(i in 1:nrow(mydt)){
  if(any(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")]%in%ind_11)==T) {mydt$ind_11[i]<-1} else {mydt$ind_11[i]<-0}}
prev_ind_11<-table(mydt$ind_11)[2]

prev_ind_12<-nrow(mydt[mydt$flowchart=="M"& (mydt$karyo==1)])

for(i in 1:nrow(mydt)){
  if(any(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")]%in%ind_13)==T) {mydt$ind_13[i]<-1} else {mydt$ind_13[i]<-0}}
prev_ind_13<-table(mydt$ind_13)[2]

for(i in 1:nrow(mydt)){
  if(any(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")]%in%ind_14)==T) {mydt$ind_14[i]<-1} else {mydt$ind_14[i]<-0}}
prev_ind_14<-table(mydt$ind_14)[2]

for(i in 1:nrow(mydt)){
  if(any(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")]%in%ind_15)==T) {mydt$ind_15[i]<-1} else {mydt$ind_15[i]<-0}}
prev_ind_15<-table(mydt$ind_15)[2]


# export accuracy tables

names_acc<-c(name_ind_8, name_ind_9, name_ind_10, name_ind_11, name_ind_12, name_ind_13, name_ind_14, name_ind_15)

totals_acc<-c((prev_ind_8), (prev_ind_9), (prev_ind_10), (prev_ind_11), (prev_ind_12), (prev_ind_13), (prev_ind_14), (prev_ind_15))

ind_8_perc<-prev_ind_8/prev_ind_0
ind_9_perc<-prev_ind_9/(sum(mydt$fetaldeath))
ind_10_perc<-prev_ind_10/nrow(mydt[mydt$type==4])
ind_11_perc<- nrow(mydt[(mydt$ind_11==1)&(mydt$karyo==1)])/prev_ind_11
ind_12_perc<-prev_ind_12/prev_ind_8
ind_13_perc<-prev_ind_13/prev_ind_0
ind_14_perc<-prev_ind_14/prev_ind_0
ind_15_perc<-perc_LB_ASD<-prev_ind_15/nrow(mydt[mydt$type==1])

perc_acc<-c(ind_8_perc, ind_9_perc, ind_10_perc, ind_11_perc, ind_12_perc, ind_13_perc, ind_14_perc, ind_15_perc)
perc_acc<-(round(perc_acc, 4)*100)
acc<-cbind(names_acc, totals_acc, perc_acc)
colnames(acc)<-c("DQI name", "Total", "Percentage")
write.csv(acc, paste0(eurocat_dir,"accuracy.csv") , row.names = F)


save.image(file="ascert_acc.RData")
