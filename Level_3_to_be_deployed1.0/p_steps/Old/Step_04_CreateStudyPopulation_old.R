
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021


if(SUBP){
  SCHEME_04 <- copy(subpopulation_meanings)
  SCHEME_04 <- SCHEME_04[, ':=' (file_in = paste0(subpopulations,"_source_population.rds"), file_out = paste0(subpopulations,"_study_population.rds"), folder_out = "populations") ]

}

if(!SUBP) SCHEME_04 <- data.frame(subpopulations = c("ALL"),file_in = "ALL_source_population.rds", file_out = "ALL_study_population.rds",folder_out = "populations")

SCHEME_04$nrows <- as.integer(NA)
SCHEME_04$ncols <- as.integer(NA)



SCHEME_04$ncolsneeded <- 23

FlowChartSourcetoStudy <- list()
for(i in 1:nrow(SCHEME_04)){
    SOURCE <- readRDS(paste0(std_pop_tmp, SCHEME_04[["file_in"]][i]))
    
    print('Exclude patients according to SelectionCriteria specified in to_run file')
    
    
    for (j in 1:length(SelectionCriteria)){
      
      before <- nrow(SOURCE)
      SOURCE <- SOURCE[eval(SelectionCriteria[[j]]),]
      after <- nrow(SOURCE)
      
      FlowChartSourcetoStudy[[paste0(names(SelectionCriteria[j]),"_",SCHEME_04[["subpopulations"]][i])]]$step <- "04_CreateStudyPopulation"
      FlowChartSourcetoStudy[[paste0(names(SelectionCriteria[j]),"_",SCHEME_04[["subpopulations"]][i])]]$population <- SCHEME_04[["subpopulations"]][i]
      FlowChartSourcetoStudy[[paste0(names(SelectionCriteria[j]),"_",SCHEME_04[["subpopulations"]][i])]]$before <- before
      FlowChartSourcetoStudy[[paste0(names(SelectionCriteria[j]),"_",SCHEME_04[["subpopulations"]][i])]]$after <- after
      
      rm(before,after)
      gc()
    } 
    
    FlowChart3 <- list()
    
    print(paste0("Set start_follow up date and end follow_up_date ",SCHEME_04[["subpopulations"]][i]))
    STUDY_POPULATION <- SOURCE[,start_follow_up := max(start_study_date,op_start_date+lookback_period,date_min),by = list(row.names(SOURCE))]
    STUDY_POPULATION <- STUDY_POPULATION[,end_follow_up := min(end_study_date,op_end_date,date_creation,recommended_end_date,date_max),by = list(row.names(SOURCE))]
    
    rm(SOURCE)
    gc()
    
    before <- nrow(STUDY_POPULATION)
    STUDY_POPULATION <- STUDY_POPULATION[start_follow_up < end_follow_up ,]
    STUDY_POPULATION <- STUDY_POPULATION[(start_follow_up - op_start_date) >= lookback_period ,]
    after <- nrow(STUDY_POPULATION)
    
    FlowChart3[[paste0("End_look_back_period_after_end_follow_up_",SCHEME_04[["subpopulations"]][i])]]$step <- "04_CreateStudyPopulation"
    FlowChart3[[paste0("End_look_back_period_after_end_follow_up_",SCHEME_04[["subpopulations"]][i])]]$population <- SCHEME_04[["subpopulations"]][i]
    FlowChart3[[paste0("End_look_back_period_after_end_follow_up_",SCHEME_04[["subpopulations"]][i])]]$before <- before
    FlowChart3[[paste0("End_look_back_period_after_end_follow_up_",SCHEME_04[["subpopulations"]][i])]]$after <- after
    
    rm(before,after)
    gc()
    
    print(paste0("Calculate age at start and end follow up ",SCHEME_04[["subpopulations"]][i]))
    STUDY_POPULATION <- STUDY_POPULATION[, ':=' 
                                         (age_start_follow_up = floor(time_length(interval(birth_date, start_follow_up),"year")),
                                           age_end_follow_up = floor(time_length(interval(birth_date, end_follow_up),"year")) 
                                         )
    ]
    
    STUDY_POPULATION <- STUDY_POPULATION[, Population := SCHEME_04[["subpopulations"]][i]]
    SCHEME_04[i,"nrows"] <- nrow(STUDY_POPULATION)
    SCHEME_04[i,"ncols"] <- ncol(STUDY_POPULATION)
    
    saveRDS(STUDY_POPULATION,file = paste0(populations_dir,SCHEME_04[["file_out"]][i]))
    rm(STUDY_POPULATION)
    gc()
}

saveRDS(FlowChart3,file = paste0(std_pop_tmp,"FlowChart3.rds"))
saveRDS(FlowChartSourcetoStudy,file = paste0(std_pop_tmp,"FlowChartSourcetoStudy.rds"))
saveRDS(SCHEME_04,file = paste0(std_pop_tmp,"SCHEME_04.rds"))
rm(FlowChart3,FlowChartSourcetoStudy,SCHEME_04)

gc()





















