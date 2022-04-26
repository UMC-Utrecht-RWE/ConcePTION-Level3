# DQI FLOWCHART

#finished 2.7.21 and only took 4 minutes to run on sample dataset!!!!

#SETUP#
if (length(actual_tables$EUROCAT)>1){
  mydt<-lapply(paste0(path_dir,actual_tables$EUROCAT),fread)
  mydt<-do.call(rbind,mydt)
} else {
  mydt<-fread (file=(paste0(path_dir, actual_tables$EUROCAT)))
}

all_codes<-unique(c(mydt$syndrome, mydt$malfo1, mydt$malfo2, mydt$malfo3, mydt$malfo4, mydt$malfo5, mydt$malfo6, mydt$malfo7,mydt$malfo8))

mydt$flowchart<-vector(length=nrow(mydt))

OG_length<- nrow(mydt)


# Step 1
# Exclude all cases with a chromosomal code
# ICD/BPA9: all 759 cases except 7584 (typo? should be 7594? - not in dataset in any case)
# Q90-Q93 excluding Q936.
# Q96-Q99, . Transfer to group C

step_1.1<-all_codes[(startsWith(all_codes, ("Q9")))]
step_1.2<-(step_1.1[step_1.1!="Q936"])  
step_1.3<-step_1.2[(startsWith(step_1.2, ("Q94"))==F)]
step_1.4<-step_1.2[(startsWith(step_1.2, ("Q95"))==F)]
step_1.5<-all_codes[(startsWith(all_codes, ("759")))]
step_1.6<-step_1.5[step_1.5!=7594]
step_1.7<-c(step_1.4, step_1.6)

#fixed bug equal should be with ==(23 Aug 2021:Vjola)
for(i in 1:nrow(mydt)){
  if(any(mydt[i,]%in%step_1.7)==T){mydt$flowchart[i]<-"C"}
}

mydt_C<-mydt[(mydt$flowchart=="C"),]
mydt<-mydt[(mydt$flowchart!="C"),]


if(((nrow(mydt)+nrow(mydt_C))!=OG_length)==T){print("LENGTH ERROR")}

# Step 2
# Exclude all cases with genetic syndrome codes, skeletal dysplasia and congenital skin disorder codes
# ICD/BPA9: 7598, 27910, 7571, 7573, 75581 (Larsen syndrome), 75601 (Crouzon), 75604 
# (Mandibulofacial dysostosis)
    # table(mydt%in%(c(75601,7598,27910,7571,7573,75581, 75601, 75604)))
    #codes not present in any column
# No ICD9 subgroup for skeletal dysplasia
# Q87 Excluding Q8703, Q8704, Q8706, Q8708, Q8724, Q8726, 
# Q936, D821, 
# Q77, Q7800, Q782-788, Q7402
# Q80-Q82
# Q4471 Alagille syndrome, Q6190 Meckel-Gruber, Q7484 Larsen syndrome
# Q751 Crouzon /craniofacial dysostosis, Q754 Mandibulofacial dysostosis (Treacher Collin)
# Q7581Frontonasal dysplasia . Transfer to group B

step_2.1<-c(all_codes[(startsWith(all_codes, ("Q87")))], "Q936", "D821", all_codes[(startsWith(all_codes, ("Q77")))],
            "Q7800", "Q936", "D821", all_codes[(startsWith(all_codes, ("Q78")))], "Q7402", all_codes[(startsWith(all_codes, ("Q80")))],
            all_codes[(startsWith(all_codes, ("Q81")))],all_codes[(startsWith(all_codes, ("Q82")))], "Q4471", "Q6190","Q7484","Q7581")

step_2.2<-vector()
ex_2.1<-c("Q8703", "Q8704", "Q8706", "Q8708", "Q8724", "Q8726", "Q936", "D821","Q781","Q789" )
for(i in 1:length(step_2.1)){
  if(any(step_2.1[i]%in%ex_2.1)==F) {step_2.2[i]<-step_2.1[i]}
}

step_2.3<-step_2.2[is.na(step_2.2)==F]

step_2.4<-c(7598, 27910, 7571, 7573, 75581,75601,75604)

step_2.5<-c(step_2.3, step_2.4)

#fixed bug from =T to ==T(23 Aug 2021)
for(i in 1:nrow(mydt)){
  if(any(mydt[i,]%in%step_2.5)==T) {mydt$flowchart[i]<-"B"}
}

mydt_B<-mydt[(mydt$flowchart=="B"),]
mydt<-mydt[(mydt$flowchart!="B"),]

#fixed bug from =T to ==T(23 Aug 2021)
if(((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B))!=OG_length)==T){print("LENGTH ERROR")}

# Step 3
# Exclude all cases with a code for teratogenic syndrome code
# No ICD9 subgroup for teratogenic syndromes, only the codes for infection 7710, 7711, 77121 and 
# fetal alcohol syndrome 76076
# Q86, P350, P351, P371 . Transfer to group T


step_3.1<-all_codes[(startsWith(all_codes, ("Q86")))]

step_3.2<-c(7710, 7711, 77121,76076)
step_3.3<-c("P350", "P351", "P371")
step_3.4<-c(step_3.1, step_3.2, step_3.3)

#fixed bug from =T to ==T(23 Aug 2021)
for(i in 1:nrow(mydt)){
  if(any(mydt[i,]%in%step_3.4)==T) {mydt$flowchart[i]<-"T"}
}


mydt_T<-mydt[(mydt$flowchart=="T"),]
mydt<-mydt[(mydt$flowchart!="T"),]

#fixed bug from =T to ==T(23 Aug 2021)
if(((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T))!=OG_length)==T){print("LENGTH ERROR")}

# Step 4 --> moved to end to avoid misclassification of single codes following Maria's feedback


# Step 5
# Exclude all cases with ONLY NTD codes
# ICD/BPA9: 740-741, 7420
# Q00-Q01, Q05 . Transfer to group N

step_5.1<-c(all_codes[(startsWith(all_codes, ("Q00")))],all_codes[(startsWith(all_codes, ("Q01")))],
            all_codes[(startsWith(all_codes, ("Q05")))])
step_5.2<-c(740, 741, 7420)
step_5.3<-c(step_5.1, step_5.2)

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  #fixed bug from =T to ==T(23 Aug 2021)
  if(all(myvars%in%step_5.3)==T) 
   {mydt$flowchart[i]<-"N"}
}

mydt_N<-mydt[(mydt$flowchart=="N"),]
mydt<-mydt[(mydt$flowchart!="N"),]

#fixed bug from =T to ==T(23 Aug 2021)
if(((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N))!=OG_length)==T){print("LENGTH ERROR")}


# Step 6
# Exclude all cases with codes ONLY in cardiac chapter
# ICD/BPA9: 745-746, 7470-7474
# Q20-Q26 . 
# Transfer to group A


step_6.1<-c(all_codes[(startsWith(all_codes, ("Q20")))],all_codes[(startsWith(all_codes, ("Q21")))],all_codes[(startsWith(all_codes, ("Q22")))],
            all_codes[(startsWith(all_codes, ("Q23")))],all_codes[(startsWith(all_codes, ("Q24")))], all_codes[(startsWith(all_codes, ("Q25")))],
            all_codes[(startsWith(all_codes, ("Q26")))])

step_6.2<- c((7450:7460),(7470:7474))
step_6.3<-c(step_6.1, step_6.2)

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars%in%step_6.3)==T) 
    {mydt$flowchart[i]<-"A"}
}

mydt_A<-mydt[(mydt$flowchart=="A"),]
mydt<-mydt[(mydt$flowchart!="A"),]

#fixed bug from =T to ==T(23 Aug 2021)
if(((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A))!=OG_length)==T){print("LENGTH ERROR")}


# Step 7
# Exclude all cases with codes ONLY in renal chapter
# ICD/BPA9: 753, 756.72, 752.61
# Q60 - Q64, Q794 . Transfer to group R

step_7.1<-c(all_codes[(startsWith(all_codes, ("Q60")))],all_codes[(startsWith(all_codes, ("Q61")))],all_codes[(startsWith(all_codes, ("Q66")))],
            all_codes[(startsWith(all_codes, ("Q63")))],all_codes[(startsWith(all_codes, ("Q64")))], "Q794")
step_7.2<-c((7530:7539), (756.72), (752.61))
step_7.3<-c(step_7.1, step_7.2)

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  #fixed bug from =T to ==T(23 Aug 2021)
  if(all(myvars%in%step_7.3)==T) 
  {mydt$flowchart[i]<-"R"}
}
mydt_R<-mydt[(mydt$flowchart=="R"),]
mydt<-mydt[(mydt$flowchart!="R"),]

#fixed bug from =T to ==T(23 Aug 2021)
if(((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R))!=OG_length)==T){print("LENGTH ERROR")}

# Step 8

# Exclude all cases with only one code within Q chapter

# Include known local coding variations/errors

# ICD/BPA9: 740-759 (transfer to groups like ICD10 - see codes for group N, A, and R earlier)

# If Q00-Q01, Q05 . Transfer to group N
# If Q20-Q26 . Transfer to group A
# If Q60-Q64, Q794 . Transfer to group R


for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(length(myvars)==1){mydt$flowchart[i]<-"single"}
}

#fixed bug from mydt_flowchart to mydt$flowchart(23 Aug 2021:Vjola)
step8.1<-table(mydt$flowchart)
single_malf<-nrow(mydt[mydt$flowchart=="single",])
####################################
#Do you want to save this number somewhere?
#######################################

# SORTING SINGLES

step_8.N<-step_5.3

step_8.A<-step_6.3

step_8.R<-step_7.3



for (i in 1:nrow(mydt[mydt$flowchart=="single",])){
  if(any(mydt[mydt$flowchart=="single",][i,]%in%step_8.N)==T){mydt[mydt$flowchart=="single",]$flowchart[i]<-"N"}
  if(any(mydt[mydt$flowchart=="single",][i,]%in%step_8.A)==T){mydt[mydt$flowchart=="single",]$flowchart[i]<-"A"}
  if(any(mydt[mydt$flowchart=="single",][i,]%in%step_8.R)==T){mydt[mydt$flowchart=="single",]$flowchart[i]<-"R"}
}

step8.sort<-table(mydt$flowchart)

# if previous NAR steps worked, these cases will already be sorted and removed

mydt_N<-rbind(mydt_N, mydt[(mydt$flowchart=="N"),])
mydt<-mydt[(mydt$flowchart!="N"),]

mydt_A<-rbind(mydt_A, mydt[(mydt$flowchart=="A"),])
mydt<-mydt[(mydt$flowchart!="A"),]

mydt_R<-rbind(mydt_R, mydt[(mydt$flowchart=="R"),])
mydt<-mydt[(mydt$flowchart!="R"),]

#all single malformations which were NOT sorted to N, A, R --> I

mydt[(mydt$flowchart=="single"),]$flowchart<-"I"
mydt_I<-mydt[(mydt$flowchart=="I"),]
mydt<-mydt[(mydt$flowchart!="I"),]


if(((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I))!=OG_length)==T){print("LENGTH ERROR")}

#STEP 8.CLEFT

# (for our own purpose, transfer also cases with ONLY cleft codes - chapter Q35-Q37)

# If only one other Q-code or D1810 or D215/icd9 2281 . Transfer to group I
##### this logically belongs to a separate step#####

step_8.cleft<-c(all_codes[(startsWith(all_codes, ("Q35")))],all_codes[(startsWith(all_codes, ("Q036")))], 
                    all_codes[(startsWith(all_codes, ("Q37")))], "D1810", "D215",2281)

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  #fixed bug from =T to ==T(23 Aug 2021)
  if(((any(myvars%in%step_8.cleft)) & length(myvars<=2)==T)|(all(myvars%in%step_8.cleft)==T)){mydt$flowchart[i]<-"cleft"}
}

step8.cleft<-table(mydt$flowchart)
mydt_cleft<-mydt[(mydt$flowchart=="cleft"),]
mydt_cleft$flowchart<-"I"
mydt_I<- rbind(mydt_I,mydt_cleft)
mydt<-mydt[(mydt$flowchart!="cleft"),]


#removed extra parantheses
#fixed bug from =T to ==T(23 Aug 2021)
if(((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I))!=OG_length)==T){print("LENGTH ERROR")}

# Step 9
# Exclude all cases with codes ONLY in eye chapter
# ICD/BPA9: 743
# Q10-Q15 . Transfer to group I

step_9.1<-c(all_codes[(startsWith(all_codes, ("Q10")))],all_codes[(startsWith(all_codes, ("Q11")))],
            all_codes[(startsWith(all_codes, ("Q12")))],all_codes[(startsWith(all_codes, ("Q13")))],
            all_codes[(startsWith(all_codes, ("Q14")))],all_codes[(startsWith(all_codes, ("Q15")))], 743)

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  #fixed bug from =T to ==T(23 Aug 2021)
  if(all(myvars%in%step_9.1)==T) 
  {mydt$flowchart[i]<-"I"}
}

mydt_I<- rbind(mydt_I,mydt[(mydt$flowchart=="I"),])
mydt<-mydt[(mydt$flowchart!="I"),]

if(((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I))!=OG_length)==T){print("LENGTH ERROR")}


# Step 10
# Exclude all cases with codes ONLY with limb reduction defects
# ICD/BPA9: 7552-7554
# Q71-Q73 . Transfer to group I

step_10.1<-c(all_codes[(startsWith(all_codes, ("Q71")))],all_codes[(startsWith(all_codes, ("Q72")))],
             all_codes[(startsWith(all_codes, ("Q73")))], (7552:7554))

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars%in%step_10.1)=="I") 
  {mydt$flowchart[i]<-"I"}
}


mydt_I<- rbind(mydt_I, mydt[(mydt$flowchart=="I"),])
mydt<-mydt[(mydt$flowchart!="I"),]


if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I))!=OG_length){print("LENGTH ERROR")}


# Step 11
# Exclude all cases with codes ONLY for hypospadias
# ICD/BPA: 75260
# Q54 . Transfer to group I

step_11.1<-c(all_codes[(startsWith(all_codes, ("Q54")))], 75260)

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  #fixed bug from =T to ==T(23 Aug 2021)
  if(all(myvars%in%step_11.1)==T) 
  {mydt$flowchart[i]<-"I"}
}

mydt_I<- rbind(mydt_I, mydt[(mydt$flowchart=="I"),])
mydt<-mydt[(mydt$flowchart!="I"),]

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I))!=OG_length){print("LENGTH ERROR")}

# Step 12
# Exclude all cases with codes ONLY for polydactyly
# ICD/BPA9: 7550
# Q69 . Transfer to group I

step_12.1<-c(all_codes[(startsWith(all_codes, ("Q69")))], 7550)

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars%in%step_12.1)==T) 
  {mydt$flowchart[i]<-"I"}
}

mydt_I<- rbind(mydt_I, mydt[(mydt$flowchart=="I"),])
mydt<-mydt[(mydt$flowchart!="I"),]

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I))!=OG_length){print("LENGTH ERROR")}

# Step 13
# Exclude all cases with codes only for reduction defects of brain
# ICD/BPA9: 7422
# Q04 . Transfer to group I

step_13.1<-c(all_codes[(startsWith(all_codes, ("Q04")))], 7422)

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  #fixed bug from =T to ==T(23 Aug 2021)
  if(all(myvars%in%step_13.1)==T) 
  {mydt$flowchart[i]<-"I"}
}

mydt_I<- rbind(mydt_I, mydt[(mydt$flowchart=="I"),])
mydt<-mydt[(mydt$flowchart!="I"),]

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I))!=OG_length){print("LENGTH ERROR")}


# Step 14
# Exclude all cases with codes only for hip anomalies 
# ICD/BPA9: 75430
# Q65 . Transfer to group I

step_14.1<-c(all_codes[(startsWith(all_codes, ("Q65")))], 75430)

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars%in%step_14.1)==T) 
  {mydt$flowchart[i]<-"I"}
}

mydt_I<- rbind(mydt_I, mydt[(mydt$flowchart=="I"),])
mydt<-mydt[(mydt$flowchart!="I"),]

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I))!=OG_length){print("LENGTH ERROR")}


# Step 15
# Exclude all cases with codes only for syndactyly
# ICD/BPA9: 7551
# Q70 . Transfer to group I

step_15.1<-c(all_codes[(startsWith(all_codes, ("Q70")))], 7551)

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars%in%step_15.1)==T) 
  {mydt$flowchart[i]<-"I"}
}

mydt_I<- rbind(mydt_I, mydt[(mydt$flowchart=="I"),])
mydt<-mydt[(mydt$flowchart!="I"),]

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I))!=OG_length){print("LENGTH ERROR")}


# Step 16
# Exclude all cases with codes only for syndactyly + polydactyly
# ICD/BPA9: 7550 and 7551
# Q69 and Q70 . Transfer to group I

step_16.1<-c(all_codes[(startsWith(all_codes, ("Q69")))],all_codes[(startsWith(all_codes, ("Q70")))], 7550, 7551)

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars%in%step_16.1)==T) 
  {mydt$flowchart[i]<-"I"}
}

mydt_I<- rbind(mydt_I, mydt[(mydt$flowchart=="I"),])
mydt<-mydt[(mydt$flowchart!="I"),]

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I))!=OG_length){print("LENGTH ERROR")}


# Step 17
# Exclude all cases with codes only for small intestinal atresia
# ICD/BPA9: 75111, 75112, 75119
# Q41 . Transfer to group I

step_17.1<-c(all_codes[(startsWith(all_codes, ("Q41")))], 75111, 75112, 75119)

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  #fixed bug from =T to ==T(23 Aug 2021)
  if(all(myvars%in%step_17.1)==T) 
  {mydt$flowchart[i]<-"I"}
}

mydt_I<- rbind(mydt_I, mydt[(mydt$flowchart=="I"),])
mydt<-mydt[(mydt$flowchart!="I"),]

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I))!=OG_length){print("LENGTH ERROR")}


# Step 18
# Exclude all cases with codes only for facial clefts
# ICD/BPA9 7490, 7491, 7492
# Q35, Q36, Q37 . Transfer to group I

step_18.1<-c(all_codes[(startsWith(all_codes, ("Q35")))],all_codes[(startsWith(all_codes, ("Q36")))],
             all_codes[(startsWith(all_codes, ("Q37")))],7490, 7491, 7492 )

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars%in%step_18.1)==T) 
  {mydt$flowchart[i]<-"I"}
}

mydt_I<- rbind(mydt_I, mydt[(mydt$flowchart=="I"),])
mydt<-mydt[(mydt$flowchart!="I"),]

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I))!=OG_length){print("LENGTH ERROR")}


# Step 19
# Exclude all cases with the code for balanced chromosomal rearrangements (Q95 or 7584)
# and only one other Q-code

step_19.balance<-c(all_codes[(startsWith(all_codes, ("Q95")))])


flowchart19.1<-vector(length=nrow(mydt))

for(i in 1:nrow(mydt)){
  #fixed bug from =T to ==T(23 Aug 2021)
  if(any(mydt[i,]%in%step_19.balance)==T){flowchart19.1[i]<-"balance"}
  if(any(mydt[i,]%in%7584)==T){flowchart19.1[i]<-"balance7584"}
}

step19.balance<-table(mydt$flowchart)

for (i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if((length(myvars)<=2)&flowchart19.1[i]=="balance")
  {mydt$flowchart[i]<-"balance2"}
}

for (i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if((length(myvars)<=2)&flowchart19.1[i]=="balance7584")
  {mydt$flowchart[i]<-"balance2_7584"}
}

mydt_balance2<- mydt[(mydt$flowchart=="balance2"),]
mydt<-mydt[(mydt$flowchart!="balance2"),]

mydt_balance2_7584<- mydt[(mydt$flowchart=="balance2_7584"),]
mydt<-mydt[(mydt$flowchart!="balance2_7584"),]


#in this case, one row is "balance_I" and its second code is Q220, which belongs to the A group

# ICD/BPA9: 7584 (transfer to groups like ICD10 - see codes for group N, A, and R earlier)

# Q95
# If (Q00-Q01, Q05) and Q95 . Transfer to group N
# If Q20-Q26 and Q95 . Transfer to group A
# If (Q60-Q64, Q794) and Q95 . Transfer to group R
# If only one other Q-code and Q95 . Transfer to group I


#fixed bug from =T to ==T(23 Aug 2021)
for (i in 1:nrow(mydt_balance2)){
  if(any(mydt_balance2[i,]%in%step_8.N)==T) {mydt_balance2$flowchart[i]<-"N"}
  if(nrow(mydt_balance2[mydt_balance2$flowchart=="balance2"])<1){break}
  if(any(mydt_balance2[i,]%in%step_8.A)==T) {mydt_balance2$flowchart[i]<-"A"}
  if(nrow(mydt_balance2[mydt_balance2$flowchart=="balance2"])<1){break}
  if(any(mydt_balance2[i,]%in%step_8.R)==T) {mydt_balance2$flowchart[i]<-"R"}
  if(nrow(mydt_balance2[mydt_balance2$flowchart=="balance2"])<1){break}
  if((mydt_balance2$flowchart[1]=="balance2")==T){mydt_balance2$flowchart[i]<-"I"}
}

mydt_N<-rbind(mydt_N, mydt_balance2[(mydt_balance2$flowchart=="N"),])

mydt_A<-rbind(mydt_A, mydt_balance2[(mydt_balance2$flowchart=="A"),])

mydt_R<-rbind(mydt_R, mydt_balance2[(mydt_balance2$flowchart=="R"),])


#fixed bug from =T to ==T(23 Aug 2021)
for (i in 1:nrow(mydt_balance2_7584)){
  if(any(mydt_balance2_7584[i,]%in%step_8.N)==T) {mydt_balance2_7584$flowchart[i]<-"N"}
  if(nrow(mydt_balance2_7584[mydt_balance2_7584$flowchart=="balance2_7584"])<1){break}
  if(any(mydt_balance2_7584[i,]%in%step_8.A)==T) {mydt_balance2_7584$flowchart[i]<-"A"}
  if(nrow(mydt_balance2_7584[mydt_balance2_7584$flowchart=="balance2_7584"])<1){break}
  if(any(mydt_balance2_7584[i,]%in%step_8.R)==T) {mydt_balance2_7584$flowchart[i]<-"R"}
  if(nrow(mydt_balance2_7584[mydt_balance2_7584$flowchart=="balance2_7584"])<1){break}
  if((mydt_balance2_7584$flowchart[1]=="balance2_7584")==T){mydt_balance2_7584$flowchart[i]<-"I"}
}

mydt_N<-rbind(mydt_N, mydt_balance2_7584[(mydt_balance2_7584$flowchart=="N"),])

mydt_A<-rbind(mydt_A, mydt_balance2_7584[(mydt_balance2_7584$flowchart=="A"),])

mydt_R<-rbind(mydt_R, mydt_balance2_7584[(mydt_balance2_7584$flowchart=="R"),])

mydt_I<-rbind(mydt_I, mydt_balance2[(mydt_balance2$flowchart=="I"),])

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I))!=OG_length){print("LENGTH ERROR")}


# Step 20
# Exclude all cases with ONLY outside Q chapter codes (without Q-codes)
# ICD/BPA9: outside 740-759
# 2281 and 76280 accepted as outside malformation chapter codes
# Not beginning with Q
# D1810 and D215 accepted as outside Q-code . Transfer to group O


# MARIA
# EUROCAT include a few codes that are outside the ICD9 codes 740-759 and outside the ICD19 Q chapter.
# I have attached the EUROCAT coding guide to help you. Step 20 is telling you that the ICD9 codes 2281 and 77280 
# and the equivalent ICD10 codes (D1810 and D215) are accepted as VALID EUROCAT codes and are dealt with in Step 8.

step_20.1<-c(all_codes[!(startsWith(all_codes, ("Q")))])
step_20.2<-step_20.1[step_20.1%in%c(2281, 76280,"D1810", "D215")==F]


for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  #fixed bug from =T to ==T(23 Aug 2021)
  if(all(myvars%in%step_20.2)==T) 
  {mydt$flowchart[i]<-"O"}
}

mydt_O<- mydt[(mydt$flowchart=="O"),]
mydt<-mydt[(mydt$flowchart!="O"),]

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I)+nrow(mydt_O))!=OG_length){print("LENGTH ERROR")}


# Step 21
# Exclude all known sequences or combinations of anomalies without other anomaly codes
# (NB: Anyone of these codes may be used more than once - disregard duplicate codes)

# STEP21.1

# Spina bifida - talipes - hydrocephalus:
#   ICD/BPA9 741 coded with 7545 and/or 7423
# Q05 coded with Q66 and/or Q03 . Transfer to group N

# step A: does it have 741?
step_21.1a<-741
flowchart21.1a<-vector(length=nrow(mydt))
for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  #fixed bug from =T to ==T(23 Aug 2021)
  if(any(myvars%in%step_21.1a)==T) 
  {flow21.1a[i]<-741}
}
table(flowchart21.1a)#do you want to save this?

# Step B: does it also have 7545 or 423?
step_21.1b<-c(step_21.1a, 7545,7423)

##Can you have a look at this flowchart21.1a seems like a TRUE/FALSE dataframe
##Is this a mistake: step_21.2a cannot be found
for (i in 1:nrow(mydt[flowchart21.1a==741,])){
  myvars<-as.matrix((mydt[flowchart21.1a==741,])[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  #fixed bug from =T to ==T(23 Aug 2021)
  if(all(myvars[i]%in%step_21.1a)==T){mydt[flowchart21.1a==741,]$flowchart[i]<-"N"}
  }

# step C: does it have codes starting with Q05?

step_21.1c<-all_codes[(startsWith(all_codes, ("Q05")))]
flowchart21.1c<-vector(length=nrow(mydt))

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(any(myvars%in%step_21.1c)==T) 
  {flowchart21.1c[i]<-"Q05"}
}

table(flowchart21.1c)#do you want to save it?

# step D: does it also have codes with Q66 or Q03?

step_21.1d<-c(step_21.1c, all_codes[(startsWith(all_codes, ("Q66")))], all_codes[(startsWith(all_codes, ("Q03")))])

for (i in 1:nrow(mydt[flowchart21.1c=="Q05",])){
  myvars<-as.matrix((mydt[flowchart21.1c=="Q05",])[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars[i]%in%step_21.1d)==T) {mydt[flowchart21.1c=="Q05",]$flowchart[i]<-"N"}
  }


# View(mydt[(flowchart21.1c=="Q05"),c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8","flowchart")])
#OK

mydt_N<-rbind(mydt_N, mydt[(mydt$flowchart=="N"),])
mydt<-mydt[(mydt$flowchart!="N"),]

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I)+nrow(mydt_O))!=OG_length){print("LENGTH ERROR")}

# step 21.2

# Bilateral renal aplasia/dysplasia - lung hypoplasia - talipes:
#   ICD/BPA9 75300 coded with 74851 and/or 7545 
# Q601/Q606 coded with Q336 and/or Q66 . Transfer to group R


step_21.2a<-75300
flowchart21.2a<-vector(length=nrow(mydt))
for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(any(myvars%in%step_21.2a)==T) 
  {flow21.2a[i]<-75300}
}
table(flowchart21.2a)

# Step B: does it also have 74851, 7545?
step_21.2b<-c(step_21.2a, 74851, 7545)

for (i in 1:nrow(mydt[flowchart21.2a==75300,])){
  myvars<-as.matrix((mydt[flowchart21.2a==75300,])[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars[i]%in%step_21.2b)==T) {mydt[flowchart21.2a==75300,]$flowchart[i]<-"R"}}

# step C: does it have codes Q601 or Q606

step_21.2c<-c("Q601", "Q606")
flowchart21.2c<-vector(length=nrow(mydt))

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(any(myvars%in%step_21.2c)==T) 
  {flowchart21.2c[i]<-"Q60"}
}

table(flowchart21.2c)

# step D: does it also have codes with Q66 or Q336?

step_21.2d<-c(step_21.2c, all_codes[(startsWith(all_codes, ("Q66")))], "Q336")

##Error
#fixed #Object 'flowchart22.1c' not found amongst centre, numloc, birth_date, sex, nbrbaby and 95 more
for (i in 1:nrow(mydt[flowchart21.2c=="Q60",])){
  myvars<-as.matrix((mydt[flowchart21.2c=="Q60",])[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars[i]%in%step_21.2d)==T) {mydt[flowchart21.2c=="Q60",]$flowchart[i]<-"R"}}

mydt_R<-rbind(mydt_R, mydt[(mydt$flowchart=="R"),])
mydt<-mydt[(mydt$flowchart!="R"),]

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I)+nrow(mydt_O))!=OG_length){print("LENGTH ERROR")}


# Omphalocele/gastroschisis - malrotation of gut - small intestinal atresia
# ICD/BPA9 75670/75671 coded with 7514 and/or 7511
# Q792/Q793 coded with Q433 and/or Q41 . Transfer to group I


# step A: does the row contain 75670 OR 75671?

step_21.3a<-c(75670,75671)
flowchart21.3a<-vector(length=nrow(mydt))
for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(any(myvars%in%step_21.3a)==T) 
  {flow21.3a[i]<-1}
}
table(flowchart21.3a)

# Step B: if so, does it also have 74851, 7545?
step_21.3b<-c(7514, 7511, step_21.3a)

for (i in 1:nrow(mydt[flowchart21.3a==1,])){
  myvars<-as.matrix((mydt[flowchart21.3a==1,])[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars[i]%in%step_21.3b)==T) {mydt[flowchart21.3a==1,]$flowchart[i]<-"I"}}

# step C: does it have codes Q601 or Q606

step_21.3c<-c("Q601", "Q606")
flowchart21.3c<-vector(length=nrow(mydt))

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(any(myvars%in%step_21.3c)==T) 
  {flowchart21.3c[i]<-1}
}

table(flowchart21.3c)

# step D: does it also have codes with Q66 or Q336?

step_21.3d<-c(step_21.3c, all_codes[(startsWith(all_codes, ("Q66")))], "Q336")


for (i in 1:nrow(mydt[flowchart21.3c==1,])){
  myvars<-as.matrix((mydt[flowchart21.3c==1,])[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars[i]%in%step_21.3d)==T) {mydt[flowchart21.3c==1,]$flowchart[i]<-"I"}}

mydt_I<-rbind(mydt_I, mydt[(mydt$flowchart=="I"),])
mydt<-mydt[(mydt$flowchart!="I"),]

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I)+nrow(mydt_O))!=OG_length){print("LENGTH ERROR")}


# Anal atresia - rectovaginal fistula
# ICD/BPA9 7512 coded with 75242
# Q42 coded with Q522 . Transfer to group I


# step A: does the row contain 7512?

step_21.4a<-7512
flowchart21.4a<-vector(length=nrow(mydt))
for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(any(myvars%in%step_21.4a)==T) 
  {flow21.4a[i]<-1}
}
table(flowchart21.4a)

# Step B: if so, does it also have 75242?
step_21.4b<-c(75242, step_21.4a)

for (i in 1:nrow(mydt[flowchart21.4a==1,])){
  myvars<-as.matrix((mydt[flowchart21.4a==1,])[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars[i]%in%step_21.4b)==T) {mydt[flowchart21.4a==1,]$flowchart[i]<-"I"}}

# step C: does it have codes Q42X

step_21.4c<-all_codes[(startsWith(all_codes, ("Q42")))]
flowchart21.4c<-vector(length=nrow(mydt))

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(any(myvars%in%step_21.4c)==T) 
  {flowchart21.4c[i]<-1}
}

table(flowchart21.4c)

# step D: does it also have codes with Q522?

step_21.4d<-c(step_21.4c, "Q522")


for (i in 1:nrow(mydt[flowchart21.4c==1,])){
  myvars<-as.matrix((mydt[flowchart21.4c==1,])[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars[i]%in%step_21.4d)==T) {mydt[flowchart21.4c==1,]$flowchart[i]<-"I"}}

mydt_I<-rbind(mydt_I, mydt[(mydt$flowchart=="I"),])
mydt<-mydt[(mydt$flowchart!="I"),]

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I)+nrow(mydt_O))!=OG_length){print("LENGTH ERROR")}


# Diaphragmatic hernia - lung hypoplasia
# ICD/BPA9 75661 coded with 74851
# Q790 coded with Q336 . Transfer to group I

# step A: does the row contain 75670 OR 75671?

step_21.5a<-c(75661)
flowchart21.5a<-vector(length=nrow(mydt))
for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(any(myvars%in%step_21.5a)==T) 
  {flow21.5a[i]<-1}
}
table(flowchart21.5a)

# Step B: if so, does it also have 74851?
step_21.5b<-c(74851, step_21.5a)

for (i in 1:nrow(mydt[flowchart21.5a==1,])){
  myvars<-as.matrix((mydt[flowchart21.5a==1,])[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars[i]%in%step_21.5b)==T) {mydt[flowchart21.5a==1,]$flowchart[i]<-"I"}}

# step C: does it have codes Q790

step_21.5c<-c("Q790")
flowchart21.5c<-vector(length=nrow(mydt))

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(any(myvars%in%step_21.5c)==T) 
  {flowchart21.5c[i]<-1}
}

table(flowchart21.5c)

# step D: does it also have code Q336?

step_21.5d<-c(step_21.5c, "Q336")


for (i in 1:nrow(mydt[flowchart21.5c==1,])){
  myvars<-as.matrix((mydt[flowchart21.5c==1,])[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars[i]%in%step_21.5d)==T) {mydt[flowchart21.5c==1,]$flowchart[i]<-"I"}}

mydt_I<-rbind(mydt_I, mydt[(mydt$flowchart=="I"),])
mydt<-mydt[(mydt$flowchart!="I"),]

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I)+nrow(mydt_O))!=OG_length){print("LENGTH ERROR")}


# Anencephalus - adrenal hypoplasia
# ICD/BPA9 740 coded with 75911
# Q00 coded with Q891 . Transfer to group N

# step A: does the row contain 740?

step_21.6a<-c(740)
flowchart21.6a<-vector(length=nrow(mydt))
for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(any(myvars%in%step_21.6a)==T) 
  {flow21.6a[i]<-1}
}
table(flowchart21.6a)

# Step B: if so, does it also have 75911?
step_21.6b<-c(75911, step_21.6a)

for (i in 1:nrow(mydt[flowchart21.6a==1,])){
  myvars<-as.matrix((mydt[flowchart21.6a==1,])[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars[i]%in%step_21.6b)==T) {mydt[flowchart21.6a==1,]$flowchart[i]<-"N"}}

# step C: does it have codes starting Q00?

step_21.6c<-c(all_codes[(startsWith(all_codes, ("Q00")))])
flowchart21.6c<-vector(length=nrow(mydt))

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(any(myvars%in%step_21.6c)==T) 
  {flowchart21.6c[i]<-1}
}

table(flowchart21.6c)

# step D: does it also have codes with Q891?

step_21.6d<-c(step_21.6c, "Q891")


for (i in 1:nrow(mydt[flowchart21.6c==1,])){
  myvars<-as.matrix((mydt[flowchart21.6c==1,])[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars[i]%in%step_21.6d)==T) {mydt[flowchart21.6c==1,]$flowchart[i]<-"N"}}

mydt_N<-rbind(mydt_N, mydt[(mydt$flowchart=="N"),])
mydt<-mydt[(mydt$flowchart!="N"),]

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I)+nrow(mydt_O))!=OG_length){print("LENGTH ERROR")}

# Unspecified hydrocephalus - reduction defect of brain
# ICD/BPA9 74239 coded with 7422
# Q039 coded with Q04 . Transfer to group I

# step A: does the row contain 74239

step_21.7a<-c(74239)
flowchart21.7a<-vector(length=nrow(mydt))
for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(any(myvars%in%step_21.7a)==T) 
  {flow21.7a[i]<-1}
}
table(flowchart21.7a)

# Step B: if so, does it also have 7422?
step_21.7b<-c(7422, step_21.7a)

for (i in 1:nrow(mydt[flowchart21.7a==1,])){
  myvars<-as.matrix((mydt[flowchart21.7a==1,])[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars[i]%in%step_21.7b)==T) {mydt[flowchart21.7a==1,]$flowchart[i]<-"I"}}

# step C: does it have codes Q039

step_21.7c<-c("Q039")
flowchart21.7c<-vector(length=nrow(mydt))

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(any(myvars%in%step_21.7c)==T) 
  {flowchart21.7c[i]<-1}
}

table(flowchart21.7c)

# step D: does it also have codes with Q04?

step_21.7d<-c(step_21.7c, all_codes[(startsWith(all_codes, ("Q04")))])


for (i in 1:nrow(mydt[flowchart21.7c==1,])){
  myvars<-as.matrix((mydt[flowchart21.7c==1,])[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars[i]%in%step_21.7d)==T) {mydt[flowchart21.7c==1,]$flowchart[i]<-"I"}}

mydt_I<-rbind(mydt_I, mydt[(mydt$flowchart=="I"),])
mydt<-mydt[(mydt$flowchart!="I"),]

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I)+nrow(mydt_O))!=OG_length){print("LENGTH ERROR")}

#########################################################################3
# Unspecified hydrocephalus - Arnold-Chiari
# no ICD/BPA9 code for Arnold-Chiari
##########################################################################
# Q039 coded with Q070 . 
# Transfer to group I



# step C: does it have codes Q039

step_21.8c<-c("Q039")
flowchart21.8c<-vector(length=nrow(mydt))

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(any(myvars%in%step_21.8c)==T) 
  {flowchart21.8c[i]<-1}
}

table(flowchart21.8c)

# step D: does it also have codes with Q070?

step_21.8d<-c(step_21.8c, "Q070")


for (i in 1:nrow(mydt[flowchart21.8c==1,])){
  myvars<-as.matrix((mydt[flowchart21.8c==1,])[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars[i]%in%step_21.8d)==T) {mydt[flowchart21.8c==1,]$flowchart[i]<-"I"}}

mydt_I<-rbind(mydt_I, mydt[(mydt$flowchart=="I"),])
mydt<-mydt[(mydt$flowchart!="I"),]

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I)+nrow(mydt_O))!=OG_length){print("LENGTH ERROR")}


########################################################33
# NTD - Arnold Chiari 
# no ICD/BPA9 code for Arnold-Chiari
# https://icd.codes/icd10cm/Q0701
#########################################################
# Q01 or Q05 coded with Q070 . 
# Transfer to group N


# step C: does it have codes Q01 or Q05

step_21.9c<-c(all_codes[(startsWith(all_codes, ("Q01")))], all_codes[(startsWith(all_codes, ("Q05")))])
flowchart21.9c<-vector(length=nrow(mydt))

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(any(myvars%in%step_21.9c)==T) 
  {flowchart21.9c[i]<-1}
}

table(flowchart21.9c)

# step D: does it also have codes with Q070?

step_21.9d<-c(step_21.9c, "Q070")


for (i in 1:nrow(mydt[flowchart21.9c==1,])){
  myvars<-as.matrix((mydt[flowchart21.9c==1,])[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars[i]%in%step_21.9d)==T) {mydt[flowchart21.9c==1,]$flowchart[i]<-"N"}}

mydt_N<-rbind(mydt_N, mydt[(mydt$flowchart=="N"),])
mydt<-mydt[(mydt$flowchart!="N"),]

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I)+nrow(mydt_O))!=OG_length){print("LENGTH ERROR")}


##########################################
#########################################
# Amniotic band sequence
# ICD/BPA9: 76280
# All cases with the code Q7980 .
# Transfer to group I
################################################
# step A: does the row contain 75670 OR 75671?

step_21.10a<-c(76280)
# flowchart21.10a<-vector(length=nrow(mydt))
for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(any(myvars%in%step_21.10a)==T) 
  {mydt$flowchart[i]<-"I"}
}
# table(flowchart21.10a)

# # Step B: if so, does it also have 74851, 7545?
# step_21.10b<-c(step_21.10a)
# 
# for (i in 1:nrow(mydt[flowchart21.10a==1,])){
#   myvars<-as.matrix((mydt[flowchart21.10a==1,])[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
#   myvars<-myvars[myvars!=""]
#   if(all(myvars[i]%in%step_21.10b)=T) {mydt[flowchart21.10a==1,]$flowchart[i]<-"I"}}

# step C: does it have codes Q601 or Q606

step_21.10c<-c("Q7980")
# flowchart21.10c<-vector(length=nrow(mydt))

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(any(myvars%in%step_21.10c)==T) 
  {mydt$flowchart[i]<-"I"}
}

# table(flowchart21.10c)

# step D: does it also have codes with Q66 or Q336?

# step_21.10d<-c(step_21.10c, all_codes[(startsWith(all_codes, ("Q66")))], "Q336")
# 
# 
# for (i in 1:nrow(mydt[flowchart21.10c==1,])){
#   myvars<-as.matrix((mydt[flowchart21.10c==1,])[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
#   myvars<-myvars[myvars!=""]
#   if(all(myvars[i]%in%step_21.10d)=T) {mydt[flowchart21.10c==1,]$flowchart[i]<-"I"}}

mydt_I<-rbind(mydt_I, mydt[(mydt$flowchart=="I"),])
mydt<-mydt[(mydt$flowchart!="I"),]

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I)+nrow(mydt_O))!=OG_length){print("LENGTH ERROR")}


###########################################3
# Caudal dysplasia sequence
# no ICD/BPA9 code for caudal dysplasia sequence
##############################################3
# All cases with the code Q8980 . 
# Transfer to group I


step_21.11c<-c("Q8980")
flowchart21.11c<-vector(length=nrow(mydt))

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(any(myvars%in%step_21.11c)==T) 
  {mydt$flowchart[i]<-"I"}
}


mydt_I<-rbind(mydt_I, mydt[(mydt$flowchart=="I"),])
mydt<-mydt[(mydt$flowchart!="I"),]

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I)+nrow(mydt_O))!=OG_length){print("LENGTH ERROR")}


# Sirenomelia sequence
# ICD/BPA9: All cases coded with 759844
# All cases coded with Q8724 . Transfer to group I

# step A: does the row contain 759844

step_21.12a<-c(7759844)
flowchart21.12a<-vector(length=nrow(mydt))
for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(any(myvars%in%step_21.12a)==T) 
  {mydt$flowchart[i]<-"I"}
}
# table(flowchart21.12a)



# step C: does it have codes Q601 or Q606

step_21.12c<-c("Q8724")


for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(any(myvars%in%step_21.12c)==T) 
  {mydt$flowchart[i]<-"I"}
}


mydt_I<-rbind(mydt_I, mydt[(mydt$flowchart=="I"),])
mydt<-mydt[(mydt$flowchart!="I"),]

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I)+nrow(mydt_O))!=OG_length){print("LENGTH ERROR")}


# Cyclops sequence
# ICD/BPA9: All cases coded with 759801
# All cases coded with Q8703 . Transfer to group I

# step A: does the row contain 75670 OR 75671?

step_21.13a<-c(759801)
flowchart21.13a<-vector(length=nrow(mydt))
for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(any(myvars%in%step_21.13a)==T) 
  {mydt$flowchart[i]<-"I"}
}
# table(flowchart21.13a)



# step C: does it have codes Q601 or Q606

step_21.13c<-c("Q8703")


for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(any(myvars%in%step_21.13c)==T) 
  {mydt$flowchart[i]<-"I"}
}


mydt_I<-rbind(mydt_I, mydt[(mydt$flowchart=="I"),])
mydt<-mydt[(mydt$flowchart!="I"),]

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I)+nrow(mydt_O))!=OG_length){print("LENGTH ERROR")}

#-----------------------------------------#

# Pierre Robin sequence 
# ICD/BPA9: All cases coded with 756030 as only code or with 7490, 7491, 7492

# All cases coded with Q8708 as only code or with Q35-Q37 . 
# Transfer to group I

# step A: does the row contain 756030?

step_21.14a<-c(756030)
flowchart21.14a<-vector(length=nrow(mydt))
for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(any(myvars%in%step_21.14a)==T) 
  {flow21.14a[i]<-1}
}
table(flowchart21.14a)


# Step B: if so, does it also have 7490, 7491, 7492?
step_21.14b<-c(7490, 7491, 7492, step_21.14a)

for (i in 1:nrow(mydt[flowchart21.14a==1,])){
  myvars<-as.matrix((mydt[flowchart21.14a==1,])[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars[i]%in%step_21.14b)==T) {mydt[flowchart21.14a==1,]$flowchart[i]<-"I"}}

# step C: does it have codes Q8708

step_21.14c<-c("Q8708")
flowchart21.14c<-vector(length=nrow(mydt))

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(any(myvars%in%step_21.14c)==T) 
  {flowchart21.14c[i]<-1}
}

table(flowchart21.14c)

# step D: does it also have codes with Q35-Q37?

step_21.14d<-c(step_21.14c, all_codes[(startsWith(all_codes, ("Q35")))], all_codes[(startsWith(all_codes, ("Q37")))])


for (i in 1:nrow(mydt[flowchart21.14c==1,])){
  myvars<-as.matrix((mydt[flowchart21.14c==1,])[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars[i]%in%step_21.14d)==T) {mydt[flowchart21.14c==1,]$flowchart[i]<-"I"}}

mydt_I<-rbind(mydt_I, mydt[(mydt$flowchart=="I"),])
mydt<-mydt[(mydt$flowchart!="I"),]

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_I)+nrow(mydt_O))!=OG_length){print("LENGTH ERROR")}

# Holoprosencephaly - median cleft lip
# ICD/BPA9: All cases coded with 74226 and 74912
# All cases coded with Q042 and Q361 . Transfer to group I
# step A: does the combo match?

step_21.15a<-c(74226,74912)

for(i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars%in%step_21.15a)==T) 
  {mydt$flowchart[i]<-"I"}
}


# step D: does it also have codes with Q66 or Q336?

step_21.15d<-c( "Q042", "Q361")


for (i in 1:nrow(mydt)){
  myvars<-as.matrix(mydt[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  if(all(myvars[i]%in%step_21.3d)==T) {mydt$flowchart[i]<-"I"}}

mydt_I<-rbind(mydt_I, mydt[(mydt$flowchart=="I"),])
mydt<-mydt[(mydt$flowchart!="I"),]

if((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_O)+nrow(mydt_I))!=OG_length){print("LENGTH ERROR")}


# Step 4 --> moved to end to avoid misclassification of single codes following Maria's feedback
# Exclude all cases with a heterogenous syndrome code
# ICD9: 756110, 75680, 759611, 3568 (not exactly the same as ICD10)
# Q761, Q7982, Q8581, Q8706 . Transfer to group M

step_4.1<- c("Q761", "Q7982", "Q8581", "Q8706")
step_4.2<-c(756110, 75680, 759611, 3568)
step_4.3<-c(step_4.1, step_4.2)
for(i in 1:nrow(mydt)){
  if(any(mydt[i,]%in%step_4.3)==T) {mydt$flowchart[i]<-"M"}
}

mydt_M<-mydt[(mydt$flowchart=="M"),]
mydt<-mydt[(mydt$flowchart!="M"),]

#everything left after flowchart classification should be group M


if((((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_O)+nrow(mydt_I)+nrow(mydt_M)))!= OG_length)==T){print("LENGTH ERROR")}


# Step 22
# The remaining cases are group M: potential multiple anomalies. Manual evaluation of all remaining 
# cases before final inclusion into multiple anomaly group - or inclusion in one of the other groups.


mydt$flowchart<-"M"
mydt_M<-rbind(mydt_M, mydt[mydt$flowchart=="M"])
mydt<-mydt[(mydt$flowchart!="M"),]

if((((nrow(mydt)+nrow(mydt_C)+nrow(mydt_B)+nrow(mydt_T)+nrow(mydt_N)+nrow(mydt_A)+nrow(mydt_R)+nrow(mydt_O)+nrow(mydt_I)+nrow(mydt_M)))!= OG_length)==T){print("LENGTH ERROR")}



# Notes:
#   Then need to output group M cases as individual case lists with text description of anomalies as well 
# as codes plus variables: ID no, registry, year of birth, type of birth, twin, GA, BW, karyotype 
# (including written text), postmortem examination, when discovered
# For the website review of potential multiple cases, a subgroup for "poorly specified cases" has to be 
# added (could go to group X
# 


#logical checks for group M
check_length<-vector(length=nrow(mydt_M))
for (i in 1:nrow(mydt_M)){
  myvars<-as.matrix(mydt_M[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  check_length[i]<-(length(myvars))
  }
all(check_length>1)


#VJOLA and MARIA advise on further checks 


mydt_flowchart<-rbind(mydt, mydt_A, mydt_B, mydt_C, mydt_I, mydt_M, mydt_N, 
                      mydt_O, mydt_R, mydt_T)

if(nrow(mydt_flowchart)==OG_length){print("Length Check OK")}

check_length<-vector(length=nrow(mydt_flowchart))
for (i in 1:nrow(mydt_flowchart)){
  myvars<-as.matrix(mydt_flowchart[i,c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", "malfo5", "malfo6", "malfo7","malfo8")])
  myvars<-myvars[myvars!=""]
  check_length[i]<-(length(myvars))
}

table(check_length[mydt_flowchart$flowchart=="A"])
table(check_length[mydt_flowchart$flowchart=="B"])
table(check_length[mydt_flowchart$flowchart=="C"])
table(check_length[mydt_flowchart$flowchart=="T"])
table(check_length[mydt_flowchart$flowchart=="O"])
table(check_length[mydt_flowchart$flowchart=="M"])
table(check_length[mydt_flowchart$flowchart=="N"])
table(check_length[mydt_flowchart$flowchart=="R"])
table(check_length[mydt_flowchart$flowchart=="I"])


#Vjola 21/6 Q codes or ICD10, different datasets will use different systems
#-->add IDC9 numbers to case selection

barplot(table(mydt_flowchart$flowchart), col=rainbow(9), main='flowchart classification', cex.main=2)


#group checks
A<-(as.matrix(mydt_flowchart[mydt_flowchart$flowchart=="A",c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", 
                               "malfo5", "malfo6", "malfo7","malfo8","flowchart")]))
table(as.vector(A))


B<-(as.matrix(mydt_flowchart[mydt_flowchart$flowchart=="B",c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", 
                                         "malfo5", "malfo6", "malfo7","malfo8","flowchart")]))
table(as.vector(B))


C<-(as.matrix(mydt_flowchart[mydt_flowchart$flowchart=="C",c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", 
                                         "malfo5", "malfo6", "malfo7","malfo8","flowchart")]))
table(as.vector(C))


I<-(as.matrix(mydt_flowchart[mydt_flowchart$flowchart=="I",c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", 
                                         "malfo5", "malfo6", "malfo7","malfo8","flowchart")]))
table(as.vector(I))


M<-(as.matrix(mydt_flowchart[mydt_flowchart$flowchart=="M",c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", 
                                         "malfo5", "malfo6", "malfo7","malfo8","flowchart")]))
table(as.vector(M))


N<-(as.matrix(mydt_flowchart[mydt_flowchart$flowchart=="N",c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", 
                                         "malfo5", "malfo6", "malfo7","malfo8","flowchart")]))
table(as.vector(N))


O<-(as.matrix(mydt_flowchart[mydt_flowchart$flowchart=="O",c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4",                                          "malfo5", "malfo6", "malfo7","malfo8","flowchart")]))

table(as.vector(O))


R<-(as.matrix(mydt_flowchart[mydt_flowchart$flowchart=="R",c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", 
                                        "malfo5", "malfo6", "malfo7","malfo8","flowchart")]))
table(as.vector(R))


T<-(as.matrix(mydt_flowchart[mydt_flowchart$flowchart=="T",c("syndrome", "malfo1", "malfo2", "malfo3", "malfo4", 
                                         "malfo5", "malfo6", "malfo7","malfo8","flowchart")]))
table(as.vector(T))



mydt_flowchart<-unique(mydt_flowchart)

write.csv(mydt_flowchart, paste0(path, "mydt_flowchart.csv"))

mydt_files<-ls(pattern="mydt")
rm(list=mydt_files)
