#list of chronic and recurrent events analysis
recurrent_events_list<-c("Depression", "Elective abortion",
                         "Gestational diabetes","Multiple gestation"," Preeclampsia", "Spontaneous abortion",
                         "TOPFA", "Breast cancer")
#time lag for recurrent events analysis
time_lag<-data.table(condition=c("Depression", "Elective abortion",
                                 "Gestational diabetes","Multiple gestation"," Preeclampsia", "Spontaneous abortion",
                                 "TOPFA", "Breast cancer"),
                     time_lag=c(3*30, 8*7, 23*7, 23*7, 8*7, 8*7, 8*7,5*365), time_remove=c(3*30, 8*7, 23*7, 23*7, 8*7, 8*7, 8*7,0))
