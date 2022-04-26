


CountPersonTime<-function(Dataset_events = NULL, Dataset, Person_id, Start_study_time, End_study_time, Start_date, End_date, Birth_date = NULL,Rec_events = F,Rec_period = NULL, Strata = NULL,Outcomes = NULL, Name_event = NULL, Date_event = NULL, Age_bands = NULL, Unit_of_age = "year" , Increment = "year", include_remaning_ages = T, Aggregate = T){
  
  print("Version 12.3")
  # Check if demanded R packages are installed, install if not,  and activate
  ################################################################################################################################
  print("Check packages data.table and lubridate")
  if (!require("data.table")) install.packages("data.table")
  library(data.table)
  
  if (!require("lubridate")) install.packages("lubridate")
  library(lubridate)
  ################################################################################################################################
  
  #Set character input for study dates to date format
  ################################################################################################################################
  print("Assign date format to Start_study_time and End_study_time")
  Start_study_time<-as.IDate(as.character(Start_study_time),"%Y%m%d")
  End_study_time<-as.IDate(as.character(End_study_time),"%Y%m%d")
  ################################################################################################################################
  
  #
  ################################################################################################################################
  end_date_new <- as.Date(ifelse(End_study_time <= Sys.Date(), Sys.Date(), End_study_time + 1),origin = "1970-01-01")
  ################################################################################################################################
  
  #check if study start and stop dates are valid
  ################################################################################################################################
  
  print("Check if Start_study_time and End_study_time are valid")
  if(!sum(Start_study_time==seq.Date(as.Date("19000101","%Y%m%d"),Sys.Date(),by = Increment))==1){
    
    if(Increment == "year"){stop("Change the start date to the first of january. Wrong study start date can produce invalid results.")}
    if(Increment == "month"){stop("Change the start date to the first of month. Wrong study start date can produce invalid results.")}
    if(Increment == "week"){stop("Change the start date to a monday. Wrong study start date can produce invalid results.")}
    
  }
  
  if(!sum(End_study_time==seq.Date(as.Date("19000101","%Y%m%d"),end_date_new ,by = Increment)-1)==1){
    
    if(Increment == "year"){stop("Change the end date to the 31th of december. Wrong study start date can produce invalid results.")}
    if(Increment == "month"){stop("Change the end date to the last day of the month. Wrong study start date can produce invalid results.")}
    if(Increment == "week"){stop("Change the end date to a sunday. Wrong study start date can produce invalid results.")}
    
  }
  
  gc()
  
  ################################################################################################################################
  
  date_cols<-c(Start_date,End_date,Birth_date)
  
  # Reduce memory size using integers
  Dataset[, c(date_cols) := lapply(.SD, as.IDate), .SDcols = date_cols]
  #Dataset[, c(Strata) := lapply(.SD, as.integer), .SDcols = Strata]
  gc()
  
  
  #Check if start, end and birth date are all filled. If end date is not filled it will be replaced by the and study date
  ################################################################################################################################
  

  print("Check if date columns in input data are valid and in correct order")
  if(sum(is.na(Dataset[,.(get(Start_date))]))>0){stop("Empty start dates")}
  if(!is.null(Age_bands)){if(sum(is.na(Dataset[,.(get(Birth_date))]))>0){stop("Empty birth dates")}}
  if(sum(is.na(Dataset[,.(get(End_date))]))>0){print(paste0(sum(is.na(Dataset[,.(get(End_date))]))," empty end dates will be filled with the end study date. This may cause overlapping intervals"))}
  Dataset[is.na(get(End_date)),eval(End_date) := End_study_time]
  
  gc()
  
  #Check the order of dates
  ################################################################################################################################
  wrong_End_date<-nrow(Dataset[get(Start_date)>get(End_date),])
  if (wrong_End_date>0){warning(paste0(wrong_End_date," end date(s) prior to start date"))}
  wrong_Start_date<-nrow(Dataset[get(Start_date)>Sys.Date(),])
  if (wrong_Start_date>0){warning(paste0(wrong_Start_date," start date(s) in future"))}
  
  if(!is.null(Age_bands)){
    wrong_Birth_date<-nrow(Dataset[get(Start_date)<get(Birth_date),])
    if (wrong_Birth_date>0){warning(paste0(wrong_Start_date," start date(s) before birth date"))}}
  ################################################################################################################################
  
  #Check if birthdays are unique
  ################################################################################################################################
  #if(!is.null(Age_bands)){
   # if(nrow(Dataset[,uniqueN(get(Birth_date)), by = Person_id][V1>1,])!=0){stop("Persons with several birth dates") }
  #}
  ################################################################################################################################
  
  #Check if the subjects have overlap in the time intervals (within strata???), defined by end-start date.
  ################################################################################################################################
  
  print("Check if observation periods do not have overlap")
  test_overlap<-Dataset[!is.na(get(End_date))&!is.na(get(End_date))&get(End_date)>get(Start_date),][,.(get(Person_id), as.integer(get(Start_date)), as.integer(get(End_date)))]
  setkey(test_overlap,V1,V2,V3)
  test_overlap2<-as.data.table(foverlaps(test_overlap, test_overlap, type="any", which=TRUE))
  test_overlap2<-test_overlap2[xid!=yid,]
  test_overlap[,id:=as.integer(rownames(test_overlap))]
  overlap_subjects<-unlist(unique(test_overlap2[test_overlap, on = .(xid = id), nomatch=NULL][,.(V1)]))
  
  if(length(overlap_subjects) > 0){
    warning("Subjects have overlapping person time: ")
    warning(paste0(overlap_subjects," "))
  }
  
  rm(test_overlap,test_overlap2,overlap_subjects)
  gc()
  ################################################################################################################################
  
    
  #Select relevant data
  ################################################################################################################################
  intv <- as.IDate(c(Start_study_time, End_study_time))
  Dataset <- Dataset[get(Start_date) %between% intv|get(End_date) %between% intv|(get(Start_date) < Start_study_time & get(End_date) > End_study_time)] 
  Dataset[get(Start_date) < Start_study_time,eval(Start_date):=Start_study_time]
  Dataset[get(End_date) > End_study_time,eval(End_date) := End_study_time]
  
  start <- Start_study_time
  end <- End_study_time
  Time_Increment <- as.IDate(seq.Date(start, end, Increment))
  ################################################################################################################################
  
  #Enlarge table by time increment. If agebands, then calculate tha ages at the start and at the end of every new created time interval
  ################################################################################################################################
  
  
  print(paste0("Transform input date to a dataset per ", Increment, ". This step increases the size of the file with respect to the choosen increment" ))
  
  Table_Temp <- copy(Dataset[0])
  gc()
  for(i in 1:length(Time_Increment)){
    
    first_day <- Time_Increment[i]
    last_day <- as.IDate(seq.Date(Time_Increment[i], by = Increment, length = 2) - 1)
    last_day <- last_day[2]
    
    Temp <- copy(Dataset)
    Temp[,eval(Increment) := Time_Increment[i]]
    Temp <- Temp[!((get(Start_date) < first_day & get(End_date) < first_day) | (get(Start_date) > last_day & get(End_date) > last_day)),]
    Temp[get(Start_date) <= first_day & get(End_date) >= first_day,eval(Start_date) := first_day ]
    Temp[get(End_date) >= last_day & get(Start_date) <= last_day, eval(End_date) := last_day]
    gc()
    
    Table_Temp <- rbindlist(list(Table_Temp, Temp), fill = TRUE)
    rm(Temp)
    
    gc()
  }
  
  rm(Dataset)
  gc()
  
  Dataset <- Table_Temp
  rm(Table_Temp)
  gc()
  ################################################################################################################################
  
  #Determine the ages at the beginning and end of all observation periods. Output is a starting point for calculation and splitting of
  # age bands
  ################################################################################################################################
  if(!is.null(Age_bands)){
    
    print(paste0("Calculate ages at the start and end of every observation period by ", Increment))
    
    if (nrow(Dataset) > 0){
      
      Dataset[, age_start := floor(time_length(interval(get(Birth_date), get(Start_date)), Unit_of_age)) ]
      Dataset[, age_end := floor(time_length(interval(get(Birth_date), get(End_date)), Unit_of_age)) ]
      
    } else{
      Dataset[,age_start := NA]
      Dataset[,age_end := NA]
    }   
    
  }
  ################################################################################################################################
  
  #Calculate agebands in 2 steps ((1)split/recalculate start/end ages and (assign row to ageband) )
  ################################################################################################################################
  if(!is.null(Age_bands)){
    
    
    #Split time interval that are switching from ageband. Those rows are doubled and start end end dates are changed
    if(Increment != "day" ){for(j in Age_bands){
      
      print(paste0("Spliting rows that fall within several agebands for age ", j))
      if(j!= 0){
        
        if(j == Age_bands[1]){j <- j-1}
        
        temp <- as.data.table(Dataset[age_end>j & age_start <= j,])
        
        if(nrow(temp) > 0){
          temp[age_end > j & age_start <= j,change_date := as.IDate(add_with_rollback(get(Birth_date), period(j+1,units = Unit_of_age), roll_to_first = T, preserve_hms = T))]
          Dataset[age_end > j & age_start <= j,change_date := as.IDate(add_with_rollback(get(Birth_date), period(j+1,units = Unit_of_age), roll_to_first = T, preserve_hms = T))]
          Dataset[change_date > get(Start_date) & age_end > j & age_start <= j, eval(End_date) := as.IDate(change_date-1)]
          temp[change_date <= get(End_date),eval(Start_date) := as.IDate(change_date)]
          #Dataset<-rbind(Dataset,temp)
          Dataset <- rbindlist(list(Dataset,temp), fill = TRUE)
        }
        
        rm(temp)
        gc()
        
        #Recalculate start end end age for rows that are doubled
        Dataset[age_end > j & age_start <= j, ':=' (  
          age_start = floor(time_length(interval(get(Birth_date), get(Start_date)), Unit_of_age)),
          age_end = floor(time_length(interval(get(Birth_date), get(End_date)), Unit_of_age))
        )]
        
        gc()
      }
      
    }}
    
    #assign age bands 
    
    print("Assign agebands")
    for (k in 1:length(Age_bands)){
      
      #if (k==1){Dataset[age_start %between% c(0,Age_bands[k]) & age_end %between% c(0,Age_bands[k]),Ageband := paste0("0-",Age_bands[k])]}
      if (k == 1){Dataset <- Dataset[age_end >= Age_bands[k],]}
      
      if (k == 2){Dataset[age_start %between% c(Age_bands[k-1],Age_bands[k]) & age_end %between% c(Age_bands[k-1],Age_bands[k]),Ageband := paste0(Age_bands[k-1],"-",Age_bands[k])]}
      if (k > 2 & k < length(Age_bands)+1){Dataset[age_start %between% c(Age_bands[k-1]+1,Age_bands[k]) & age_end %between% c(Age_bands[k-1]+1,Age_bands[k]),Ageband := paste0(Age_bands[k-1]+1,"-",Age_bands[k])]}
      
      if (k == length(Age_bands) & include_remaning_ages == T){Dataset[age_start >= Age_bands[k]+1,Ageband := paste0(Age_bands[k]+1,"+")]}
      if (k == length(Age_bands) & include_remaning_ages == F){Dataset <- Dataset[age_start <= Age_bands[k],]}  
      
    }
    Age_band_coln<-"Ageband"
    
  } else Age_band_coln<-"Ageband"<-NULL
  
  
  #NEW CODE V11 If recurrent events is true. This is a whole different approach compared to the situation where only the first
  # event is used. When joining multiple doubling will occur. I choose to do not do this and choose a method only allowing joins 
  # that have unique combinations
  ################################################################################################################################
  
  #Combine two input datasets to one dataset if only the first occurrence of an event is evaluated or exclude events that are following 
  #the previous event within a time period. 
  ################################################################################################################################
  if (!is.null(Dataset_events)) {
    
    Dataset_events <- copy(Dataset_events)
    
  #NEW CODE FROM V11
  
  if(nrow(Dataset_events) > 0){
    Dataset_events <- Dataset_events[get(Name_event) %in% Outcomes,]
    
    # set date to integer to spare RAM
    Dataset_events[, c(Date_event) := lapply(.SD, as.IDate), .SDcols = Date_event]
    
    colls_outcomes <- Outcomes[Outcomes %in% Dataset_events[,get(Name_event)]]
    setorderv(Dataset_events,c(Person_id,Name_event,Date_event))
    Dataset_events[,Recurrent := cumsum(!is.na(get(Date_event))),by=c(Person_id,Name_event)]
    
    if(Rec_events){
      print("If Rec_events = T then determine the censoring periods")
      it=1
      if(!length(Rec_period)==length(Outcomes)) stop("the vectors Outcomes and rec period have different lengths")
      #events_rec_list <- list()
      events_rec_list <- copy(Dataset_events[0])
      
      for (i in 1:length(Outcomes)){
        
        events_rec  <- copy(Dataset_events)[get(Name_event) == Outcomes[i],]
        
        while(nrow(events_rec) > 0){ 
          
          events_rec <- events_rec[,D := shift(get(Date_event)),by = c(Person_id,Name_event) ]
          events_rec[,dif := get(Date_event)-D]
          events_rec[is.na(dif), dif := 0 ][,dif := as.numeric(dif)]
          events_rec[,cumdif := cumsum(dif),by = c(Person_id,Name_event)]
          
          events_rec_list <- rbindlist(list(events_rec_list,events_rec[ cumdif <= Rec_period[i],][,.SD[1], c(Person_id,Name_event)][,Iteration := it]),fill=T)
          
          #events_rec_list[[it]] <- events_rec[ cumdif <= Rec_period[i],][,.SD[1], c(Person_id,Name_event)][,Iteration := it]
          events_rec <- events_rec[cumdif > Rec_period[i],]
          
          lapply(c("dif","cumdif","D"), function(x){events_rec <-events_rec[,eval(x) := NULL]})
          
          it=it+1
          gc()
        }
        rm(events_rec)
        gc()
      }
      
      #Dataset_events <- do.call(rbind,events_rec_list)
      Dataset_events <- events_rec_list
      rm(events_rec_list)
      gc()
      
      for(i in 1:length(Rec_period)){
        
        
        Dataset_events<- Dataset_events[dif!=0 & get(Name_event) == Outcomes[i] & dif < Rec_period[i],Delete:=T][is.na(Delete),]
        Dataset_events <- Dataset_events[get(Name_event) == Outcomes[i], ":=" (RecSTDT=get(Date_event),RecENDT=get(Date_event)+Rec_period[i])]  
        gc()
        
      }
      
      lapply(c("dif","cumdif","D","Iteration"), function(x){Dataset_events <-Dataset_events[,eval(x) := NULL]})
      
      Dataset_events<-dcast(Dataset_events, get(Person_id) + Recurrent ~ get(Name_event), value.var = c(Date_event,"RecSTDT","RecENDT"))
      setcolorder(Dataset_events,neworder = c('Person_id','Recurrent',paste0(Date_event,"_",colls_outcomes),paste0("RecSTDT_",colls_outcomes),paste0("RecENDT_",colls_outcomes)))
      colnames(Dataset_events) <- c('Person_id','Recurrent',colls_outcomes,paste0("RecSTDT_",colls_outcomes),paste0("RecENDT_",colls_outcomes))
      
      
    } else {
      
      print("If Rec_events = F then selecting only the first event")
      Dataset_events <- Dataset_events[Recurrent==1,]
      Dataset_events<-dcast(Dataset_events, get(Person_id) + Recurrent ~ get(Name_event), value.var = eval(Date_event))
      setcolorder(Dataset_events,neworder = c('Person_id','Recurrent',colls_outcomes))
      
      setkeyv(Dataset,Person_id)
      setkey(Dataset_events,Person_id)
      Dataset<-Dataset_events[Dataset,][,Recurrent := NULL]
      setnames(Dataset, "Person_id", eval(Person_id))
      
    }
    
    
    invisible(lapply(Outcomes, function(x) if (!x %in% colnames(Dataset)) Dataset <- Dataset[, eval(x) := as.IDate(NA, format = "%d%m%Y")])) 
  } else{
    invisible(lapply(Outcomes, function(x) Dataset <- Dataset[, eval(x) := as.IDate(NA, format = "%d%m%Y")]))
  }
  
  ################################################################################################################################
  
  if(Rec_events){
  
    REC <- sort(unique(Dataset_events[,Recurrent]))
    setnames(Dataset,eval(Person_id) ,"Person_id" )
    p <- as.data.table(expand.grid(REC,colls_outcomes))
    colnames(p) <-c("REC","OUTC")
    
    check_combinations <- list()
    
    for(i in colls_outcomes){
      DT_temp <- copy(Dataset_events)
      check_combinations[[i]] <- unique(DT_temp[,  ':=' (Event = i,sum = sum(!is.na(get(i)))),by = Recurrent ][,c("Event","sum","Recurrent")])
      rm(DT_temp)
      gc()
    }
    
    check_combinations <- do.call(rbind,check_combinations)
    
    setkey(p,REC,OUTC)
    setkey(check_combinations,Recurrent,Event)
    p <- as.data.frame(p[check_combinations,][sum>0,])
    
    Dataset[,row :=row.names(Dataset)]
    
    #Calculate for every observation period the amount of persontime that needs to subtracted for every event
    ################################################################################################################################
    for(i in 1:nrow(p)){
      
      print(paste0("Calculate days to subtract from persontime for ",p[i,"OUTC"]," for recurrent event ",p[i,"REC"]))
      ST<-paste0("RecSTDT_",p[i,"OUTC"])
      EN<-paste0("RecENDT_",p[i,"OUTC"])
      SUB<- paste0("SUBTRCUM_",p[i,"OUTC"])  
      
      Dataset<-copy(Dataset)
      temp_events <- copy(Dataset_events)[Recurrent == p[i,"REC"],]
      setkey(Dataset,Person_id)
      setkey(temp_events,Person_id)
      Dataset<-temp_events[Dataset,]
      
      if(nrow(Dataset[ !((get(ST) < get(Start_date) & get(EN) < get(Start_date)) | (get(ST) > get(End_date) & get(EN) > get(End_date))),]) > 0){
        
        
        Dataset <- Dataset[!((get(ST) < get(Start_date) & get(EN) < get(Start_date)) | (get(ST) > get(End_date) & get(EN) > get(End_date))) ,
                           ':='
                           (start_date2 = max(get(Start_date),get(ST),na.rm=T),
                             end_date2 = min(get(End_date)+1,get(EN),na.rm=T)
                           ), 
                           
                           by=row]                                       
        
        Dataset <- Dataset[,SUBTR := (as.numeric(end_date2)-as.numeric(start_date2)),by=row]
        if(!any(colnames(Dataset)==SUB)) Dataset <- Dataset[,eval(SUB) := 0]
        Dataset <- Dataset[!is.na(SUBTR),eval(SUB) := SUBTR+get(SUB)]
        del <- c("SUBTR","start_date2","end_date2","Recurrent",colls_outcomes,paste0("RecSTDT_",colls_outcomes),paste0("RecENDT_",colls_outcomes))                 
        
        lapply(del, function(x){Dataset <-Dataset[,eval(x) := NULL]})
      } else {
        
        del <- c("Recurrent",colls_outcomes,paste0("RecSTDT_",colls_outcomes),paste0("RecENDT_",colls_outcomes))                 
        lapply(del, function(x){Dataset <- Dataset[,eval(x):=NULL]})
        
      }
     rm(ST,EN,SUB,temp_events,del)
     gc()
    }
    
    
    #Check if all outcomes are present. If not add column with value 0
    
    #if(length(Outcomes[!Outcomes %in% unique(p[["OUTC"]])]) > 0)lapply(paste0("SUBTRCUM_",Outcomes[!Outcomes %in% unique(p[["OUTC"]])]), function(x){Dataset <-Dataset[,eval(x) := 0]})
    SUBTRCUM_MISSING <- Outcomes[!paste0("SUBTRCUM_",Outcomes) %in% unique(colnames(Dataset))] 
    if(length(SUBTRCUM_MISSING) > 0)lapply(paste0("SUBTRCUM_",SUBTRCUM_MISSING), function(x){Dataset <-Dataset[,eval(x) := 0]})
    
    
    ################################################################################################################################
    
    #Calculate the number of events per observation period
    ################################################################################################################################
    k <- c("Person_id",Start_date,End_date)
    
    for(i in colls_outcomes){
      
      print(paste0("Count the number of events per subject per ",Increment," for ", i))
      Dataset_temp <- copy(Dataset)[,c("Person_id",Start_date,End_date),with=F]
      Event_temp <- copy(Dataset_events)[!is.na(get(i)),c("Person_id",i),with=F]
      setkey(Dataset_temp,Person_id)
      setkey(Event_temp,Person_id)
      Dataset_temp<-Dataset_temp[Event_temp,][between(get(i),get(Start_date),get(End_date),NAbounds = F),]
      Dataset_temp<-Dataset_temp[,paste0(i,"_b") := sum(!is.na(get(i))),by = c("Person_id",Start_date,End_date)]
      Dataset_temp <- unique(Dataset_temp[,eval(i) := NULL])
      
      setkeyv(Dataset_temp,k)
      setkeyv(Dataset,k)
      Dataset <- Dataset_temp[Dataset,][is.na(get(paste0(i, "_b"))), eval(paste0(i, "_b")) := 0]
      
      rm(Event_temp,Dataset_temp)
      gc()
    }  
    #if(length(Outcomes[!Outcomes %in% unique(p[["OUTC"]])]) > 0)lapply(paste0(Outcomes[!Outcomes %in% unique(p[["OUTC"]])],"_b"), function(x){Dataset <-Dataset[,eval(x) := 0]})
    
    B_MISSING <- Outcomes[!paste0(Outcomes, "_b") %in% unique(colnames(Dataset))]
    if(length(B_MISSING) > 0)lapply(paste0(B_MISSING,"_b"), function(x){Dataset <-Dataset[,eval(x) := 0]})
    
    
    ################################################################################################################################   
  }
  }
  
  ################################################################################################################################
  
  
  #Calculate persontimes
  ################################################################################################################################
  
  print("Calculate persontimes")
  
  if(!is.null(Dataset_events)){
  
  rm(Dataset_events)  
  gc()    
    
  Outcomes_b <- paste0(Outcomes, "_b")
  Persontime_Outcomes <- paste0("Persontime_", Outcomes)
  #sort_order <- c(eval(Person_id), eval(Start_date))
  sort_order <- c(eval(Person_id), eval(Start_date),Age_band_coln,eval(Strata))
  coln <- c(eval(Person_id), eval(Strata), Age_band_coln, eval(Increment), "Persontime", eval(Persontime_Outcomes), eval(Outcomes_b))
  Dataset[,Persontime := .(get(End_date)-get(Start_date) + 1)]
        if(!Rec_events){
          lapply(Outcomes,function(x)Dataset[,paste0(eval(x),"_b") := fifelse(!is.na(get(x)) & get(x) %between% list(get(Start_date),get(End_date)),1,0)])
          lapply(Outcomes, function(x) Dataset[,paste0("Persontime_",x) := fifelse(!is.na(get(x)) & get(x) < get(Start_date), 0, Persontime)])
          #lapply(Outcomes,function(x)Dataset[get(x) %between% list(get(Start_date),get(End_date)),`:=`(paste0(eval(x),"_b")= 1, paste0("Persontime_",x) = .(get(x)-get(Start_date)+1))])
          lapply(Outcomes,function(x)Dataset[get(x) %between% list(get(Start_date),get(End_date)),paste0("Persontime_",x) := .(get(x)-get(Start_date)+1)])
          }
        
        
        if(Rec_events){
          setnames(Dataset,"Person_id",eval(Person_id))
          lapply(Outcomes,function(x)Dataset[,paste0("Persontime_",x) := .(get(End_date) - get(Start_date) + 1 - get(paste0("SUBTRCUM_",x)))])
        }
        
  } else {
    
    rm(Dataset_events)  
    gc()
    Dataset[,Persontime := .(get(End_date)-get(Start_date) + 1)]
    sort_order <- c(eval(Person_id), eval(Start_date),Age_band_coln,eval(Strata))
    coln <- c(eval(Person_id), eval(Strata), Age_band_coln, eval(Increment),"Persontime")
    
  }
          
  

  
  

  ################################################################################################################################
  
  #Create output table
  ################################################################################################################################
  
  print("Create output table")
  if(Increment=="month"){Dataset[,eval(Increment) :=substr(get(Increment),1,7)]}
  if(Increment=="year"){Dataset[,eval(Increment) :=substr(get(Increment),1,4)]}
  
  setorderv(Dataset, sort_order)
  Dataset <- Dataset[,coln,with=FALSE]
  
  
  if (Aggregate == T) {
    if (!is.null(Age_bands)) Dataset <- Dataset[, lapply(.SD, sum), .SDcols=c("Persontime",paste0("Persontime_",Outcomes),paste0(Outcomes,"_b")), by  = c(Strata, Increment, "Ageband")]
    if (is.null(Age_bands)) Dataset <- Dataset[, lapply(.SD, sum), .SDcols=c("Persontime",paste0("Persontime_",Outcomes),paste0(Outcomes,"_b")), by  = c(Strata, Increment)]
    
  }
  
  return(Dataset)
  ################################################################################################################################
  rm(Dataset)
  gc()
  
  
  
}






