
create_age_band<-function(x){
  a<-x
  a<-c(a[1],a[2:length(a)]+1)
  b<-c(x[2:length(x)],120)
  data<-data.table(min=a,max=b)
  data[,age_band:=paste(min,max,sep="-")]
  data[data[,.N], age_band:=paste(min,"+",sep="")]
  return(data)
}

