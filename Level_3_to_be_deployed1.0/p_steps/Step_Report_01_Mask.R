



#i="PC"

for(i in readRDS(paste0(std_pop_tmp,"SCHEME_06.rds"))[["subpopulations"]]){

if(SUBP) {
  report_dir1 <- paste0(std_source_pop_dir,i)
  report_dir2 <- paste0(std_source_pop_dir,i,"/Masked")
  
  }else{
  
    
  report_dir1 <- substr(std_source_pop_dir,1,nchar(std_source_pop_dir)-1)
  report_dir2 <- paste0(std_source_pop_dir,"Masked")
}


list1 <- c("No. source","PY source","No. study","PY study","Total No","No Female","No Male","PY Male","PY Female","Total","No","No. of visits","No. of persons with at least one visit","Women with at least 1 record in study period","Woman in category",as.character(c(1:12)),as.character(c(1900:2200)))
list2 <- c("% Male","% Female","%","Visit rate, No. of visits/1000 PY","PY")

vlist <- rep(5,length(list1)) 
names(vlist) <- list1

do.call(file.remove, list(list.files(report_dir2, full.names = TRUE)))
DRE_Treshold(
  Inputfolder = report_dir1,
  Outputfolder = report_dir2,
  Delimiter = ";",
  Varlist = vlist,
  NAlist = list2
  
)

c("No. source","PY source","No. study","PY study")





}

rm(report_dir1,report_dir2,list1,list2)