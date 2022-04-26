
if(SUBP) SCHEME_01_0405 <- subpopulation_meanings[, 
                                               ':=' 
                                               (
                                                   file_in = paste0(subpopulations,"_study_population.rds"), 
                                                   file_out = paste0(subpopulations,"_R_01_03_STUDYPOP.csv"), 
                                                   folder_in = "/g_intermediate/populations/", 
                                                   folder_out = "/g_output/output2/")
                                                   
]


if(!SUBP) SCHEME_01_0405 <- data.frame(subpopulations = c("ALL"), 
                                    
                                    file_in = paste0("ALL_study_population.rds"), 
                                    file_out = paste0("ALL_R_01_03_STUDYPOP.csv"), 
                                    folder_in = "/g_intermediate/populations/", 
                                    folder_out = "/g_output/output2/")




for(i in 1:nrow(SCHEME_01_0405)){

                    STUDY_POPULATION <- readRDS(file = paste0(thisdir,SCHEME_01_0405[["folder_in"]][i],SCHEME_01_0405[["file_in"]][i]))
                    
                    
                  
                    #saveRDS(COUNT_T, file = paste0(thisdir,"/g_intermediate/InDatabaseDuration.rds"))
                    fwrite(COUNT, file = paste0(thisdir,SCHEME_01_0405[["folder_out"]][i],SCHEME_01_0405[["file_out"]][i]), sep = ";")
                    
                    rm(COUNT,COUNT_T,order,new)
                    gc()




}

saveRDS(SCHEME_01_0405, file = paste0(thisdir,"/g_intermediate/tmp2/SCHEME_01_0405.rds"))
rm(SCHEME_01_0405)





