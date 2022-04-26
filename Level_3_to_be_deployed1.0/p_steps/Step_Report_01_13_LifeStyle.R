#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021


#Lifestyle <- list()


if(SUBP) SCHEME_0113 <- subpopulation_meanings[, 
                                               ':=' 
                                               (
                                                 file_in = paste0(subpopulations,"_study_population.rds"), 
                                                 
                                                 file_out = paste0(subpopulations,"_R_01_13_LIFESTYLE.csv"),
                                                 folder_in = populations_dir, 
                                                 folder_out = paste0(std_source_pop_dir,subpopulations,"/"))
                                               
]


if(!SUBP) SCHEME_0113 <- data.frame(subpopulations = c("ALL"), 
                                    
                                    file_in = paste0("ALL","_study_population.rds"), 
                                    
                                    file_out = paste0("ALL","_R_01_13_LIFESTYLE.csv"),
                                    folder_in = populations_dir, 
                                    folder_out = std_source_pop_dir)


if(length(Lifestyle) > 0){

print('Import and append relevant files')

#Collect all the CDM tables that are needed to extract lifestyle factors from  
TABLES <- vector()
for(i in 1:length(Lifestyle)){
  TABLES[i] <- Lifestyle[[i]]["CDM_table"]
}

#import and append all relevant CDM tables 
TABLES <- unique(TABLES)
for(i in TABLES) {
  assign(eval(i),IMPORT_PATTERN(pat = i, dir = path_dir))
  assign(eval(paste0("L.",i)),get(i)[0])
}

#Select all relevant rows form the tables. May be that this can be done more efficient by constructing 1 expression and then do in once instead of by a loop
for(i in names(Lifestyle)){
  
  if(!is.null(Lifestyle[[i]][["c.voc"]])) temp <- copy(get(Lifestyle[[i]][["CDM_table"]]))[get(Lifestyle[[i]][["CDM_column"]]) %in% eval(Lifestyle[[i]][["value"]]) & get(Lifestyle[[i]][["c.voc"]]) %in% eval(Lifestyle[[i]][["v.voc"]]),]
  if(is.null(Lifestyle[[i]][["c.voc"]]))  temp <- copy(get(Lifestyle[[i]][["CDM_table"]]))[get(Lifestyle[[i]][["CDM_column"]]) %in% eval(Lifestyle[[i]][["value"]]) ,]
  
  assign(eval(paste0("L.",Lifestyle[[i]][["CDM_table"]])),rbindlist(list(get(paste0("L.",Lifestyle[[i]][["CDM_table"]])),temp), fill = T, use.names=T))
  rm(temp)
  gc()
  
}

#Delete imported tables. These are replaced by a relavant subset
for(i in TABLES) rm(list = i)
gc()

TABLES <- paste0("L.",TABLES)


Agebands <- CreateBands(seq(from = 12 , to = 55, by = 10))


#Merge needed variables (Agebands/sex) to lifestyle factors and construct table with counts. This may be done with a join instead of a loop
for(i in 1:nrow(SCHEME_0113)){
  
  STUDY_POPULATION <- readRDS(file = paste0(SCHEME_0113[["folder_in"]][i],SCHEME_0113[["file_in"]][i]))[, .(person_id, sex_at_instance_creation, birth_date, start_follow_up, end_follow_up,Year_op, age_start_follow_up,start_follow_up_year)]
  STUDY_POPULATION <- STUDY_POPULATION[sex_at_instance_creation == "F",]
  STUDY_POPULATION <- merge(x = STUDY_POPULATION, y = Agebands, by.x = "age_start_follow_up",by.y = "INT")
  COUNT2 <- STUDY_POPULATION[,.(NoU2 = uniqueN(person_id)), keyby = list(Year_op,band,Order)]
  
  COUNT1 <- data.table(col = as.character(), Year_op = as.numeric(),band = as.character(), NoU = as.numeric(), Order = as.numeric())
  
  
  for(j in names(Lifestyle)){ 
    table <- paste0("L.",Lifestyle[[j]][["CDM_table"]])
    date.v <- Lifestyle[[j]][["v.date"]]
    col <- Lifestyle[[j]][["CDM_column"]]
    
    if(!is.null(Lifestyle[[j]][["c.voc"]])) temp <- copy(get(table))[get(Lifestyle[[j]][["CDM_column"]]) %in% eval(Lifestyle[[j]][["value"]]) & get(Lifestyle[[j]][["c.voc"]]) %in% eval(Lifestyle[[j]][["v.voc"]]),]
    if(is.null(Lifestyle[[j]][["c.voc"]]))  temp <- copy(get(table))[get(Lifestyle[[j]][["CDM_column"]]) %in% eval(Lifestyle[[j]][["value"]]) ,]
    
    temp <- merge(STUDY_POPULATION, temp, by = "person_id", allow.cartesian = T)
    temp <- temp[, eval(date.v) := as.IDate(as.character(get(date.v)),"%Y%m%d")][get(date.v) %between% list(start_follow_up, end_follow_up), ]
    setnames(temp,col,"col")
    
    if(nrow(temp) > 0){
    temp[["col"]] <- j  
    COUNT <- temp[,.(NoU = uniqueN(person_id)), keyby = list(Year_op,band,col,Order)]
    COUNT1 <- rbindlist(list(COUNT1,COUNT), fill = T, use.names = T)
    rm(COUNT)
    }
    
    rm(table,date.v,col,temp)
    gc()
  
  }
  
  
  
  if(nrow(COUNT1) > 0){COUNT3 <- merge(x = COUNT1, y = COUNT2, by = c("band","Year_op", "Order"), allow.cartesian = T)[, per := round(NoU/NoU2*100,1)]}else{
    COUNT3 <- data.table(col = as.character(),Year_op = as.character(),band = as.character(),NoU = as.character(),NoU2 = as.character(),per = as.character(),Order = as.character())
    
  }
  
  
  setorder(COUNT3,col, Year_op, Order )
  old <- c("col","Year_op","band","NoU","NoU2","per","Order")
  new <- c("Factor","Year op_start_date","Ageband start follow up","Women with at least 1 record in study period","Woman in category","%","Order" )
  setnames(COUNT3,old,new)
  setcolorder(COUNT3,new)
  fwrite(COUNT3, file = paste0(SCHEME_0113[["folder_out"]][i],SCHEME_0113[["file_out"]][i]), sep = ";")
  
  rm(old,new,COUNT1,COUNT2,COUNT3)
  gc()
}

for(i in TABLES){
  rm(list = i)
}  
 
rm(SCHEME_0113, Agebands, STUDY_POPULATION,TABLES) 


gc()

}else{
  COUNT3 <- data.table(col = as.character(),Year_op = as.character(),band = as.character(),NoU = as.character(),NoU2 = as.character(),per = as.character(),Order = as.character())
  old <- c("col","Year_op","band","NoU","NoU2","per","Order")
  new <- c("Factor","Year op_start_date","Ageband start follow up","Women with at least 1 record in study period","Woman in category","%","Order" )
  setnames(COUNT3,old,new)
  setcolorder(COUNT3,new)
  for(i in 1:nrow(SCHEME_0113)){
  fwrite(COUNT3, file = paste0(SCHEME_0113[["folder_out"]][i],SCHEME_0113[["file_out"]][i]), sep = ";")
  }
}


