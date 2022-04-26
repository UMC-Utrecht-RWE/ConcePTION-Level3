
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021



#Create variables
if(SUBP) {
  
  SCHEME_05 <- copy(subpopulation_meanings)
  SCHEME_05 <- SCHEME_05[, ':=' (file_in = paste0(subpopulations,"_source_population.rds"), file_out = paste0(subpopulations,"_source_population.rds"),folder_in = std_pop_tmp, folder_out = std_pop_tmp) ]
  SCHEME_05 <- rbind(data.frame(subpopulations = c("ALL"),meaning_sets = "ALL",file_in = "ALL_source_population.rds", file_out = "ALL_source_population.rds",folder_in = std_pop_tmp, folder_out = std_pop_tmp),SCHEME_05)  

}

if(!SUBP) SCHEME_05 <- data.frame(subpopulations = c("ALL"),file_in = "ALL_source_population.rds", file_out = "ALL_source_population.rds",folder_in = std_pop_tmp, folder_out = std_pop_tmp)

# SCHEME_05$nrows <- as.integer(NA)
# SCHEME_05$ncols <- as.integer(NA)
# SCHEME_05$ncolsneeded <- 23

for(i in 1:nrow(SCHEME_05)){
  
  print(paste0("Read Source population table for population ",SCHEME_05[["subpopulations"]][i]," from intermediate"))
  SOURCE_POPULATION <- readRDS(paste0(SCHEME_05[["folder_in"]][i],SCHEME_05[["file_in"]][i]))
  
  #Create new Columns here
  ###################################################################################
  
  print("Add personyears and year start observation period with op_start_date and op_end_date")
  #SOURCE_POPULATION <- SOURCE_POPULATION[,PY_OP := round((op_end_date - op_start_date)/365.25,2) ]
  SOURCE_POPULATION <- SOURCE_POPULATION[,PY_OP := round((min(op_end_date, date_creation,recommended_end_date) - op_start_date)/365.25,2), by = row.names(SOURCE_POPULATION) ]
  
  SOURCE_POPULATION <- SOURCE_POPULATION[,Year_op := year(op_start_date)]
  
  
  ###
  
  ###################################################################################
  
  print(paste0("Write Source population table for population ",SCHEME_05[["subpopulations"]][i]," to intermediate"))
  #SCHEME_05[i,"nrows"] <- nrow(SOURCE_POPULATION)
  #SCHEME_05[i,"ncols"] <- ncol(SOURCE_POPULATION) 
  saveRDS(SOURCE_POPULATION,file = paste0(SCHEME_05[["folder_out"]][i],SCHEME_05[["file_out"]][i]))
  
  rm(SOURCE_POPULATION)
  gc()
}

saveRDS(SCHEME_05,file = paste0(std_pop_tmp,"SCHEME_05.rds"))

rm(SCHEME_05)
gc()

