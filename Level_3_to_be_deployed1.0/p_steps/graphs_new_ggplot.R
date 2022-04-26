
rm(list = ls(all=TRUE))

library(data.table)
i = "ALL"
report_dir1 <- "C:/Level-3-checks/Data characterisation/Level_3_to_be_deployed/g_output/STUDY_SOURCE_POPULATION"


TEMP <- fread(paste0(report_dir1,"/",i,"_R_01_02_CompareToSource.csv"), sep = ";")
colnames(TEMP) <- c("year","Ageband","Order","NbSource","PYSource","SpellsSource","NbStudy","PYStudy","SpellsStudy")
TEMP <- TEMP[year != 9999,]
setorder(TEMP,Order)
years <- c(min(TEMP[,year]):max(TEMP[,year]))
stack <- unique(TEMP[["Ageband"]]) 
colors <- colorRamps::matlab.like(length(stack))


TEMP1 <- INPUTMATRIX(
  d = TEMP,
  value = "PYSource",
  type = "none",
  var = "year",
  var.v= years,
  cat = "Ageband" ,
  cat.v = stack , 
  per = F,
  perdir = "col",
  mask = F,
  output = "long"
  
  
)

library(ggplot2)

aa <- ggplot(TEMP, aes(fill = Ageband,x = year, y = PYSource)) +
  #geom_bar(position="stack",stat = "identity") +
  geom_line(aes(color = Ageband)) +
  geom_point(aes(color = Ageband)) +
  #facet_wrap(~ meaning, ncol=2, scales = "fixed") +
  xlab("Year") +
  ylab("PY")+
  theme_classic() +
  ggtitle("Source") +
    scale_fill_manual(values = rainbow(12)) +
    theme(plot.margin=unit(c(2,2,2,2),"cm")) +
    theme(axis.text.x = element_text(margin = margin(t = 0))) +
    theme(legend.title = element_text(size = 7),legend.text = element_text(size = 7))





library(plotly)
ggplotly(aa)



TEMP <- fread(paste0(report_dir1,"/",i,"_R_01_12_VISITS.csv"), sep = ";") 


if(nrow(TEMP) > 0) {
  
  bands <- unique(TEMP[["Age"]])
  colors <- colorRamps::matlab.like(length(bands))
  meanings <- unique(TEMP[["Visit meaning"]])
  years <- c(min(TEMP[["Calendar year" ]]):max(TEMP[["Calendar year" ]]))
  
  for(k in 1:length(meanings)){
    
    TEMP1 <- TEMP[get("Visit meaning") == unique(TEMP[["Visit meaning"]])[k],]
    
    TEMP1 <- INPUTMATRIX(
      
      d = TEMP1,
      value = "Visit rate, No. of visits/1000 PY",
      type = "none",
      var = "Calendar year",
      var.v = years,
      cat = "Age",
      cat.v = bands,
      per = F,
      output = "long"
      
      
    )
    
    TEMP1[, Meaning := meanings[k]]
    
    if(k == 1) TEMP2 <- TEMP1[0]
    TEMP2 <- rbind(TEMP2,TEMP1)
    rm(TEMP1)
    
}
}

setnames(TEMP2,c("Age","Calendar year","Visit rate, No. of visits/1000 PY"),c("Ageband","Year","Rate"))
TEMP2[,Year := as.numeric(Year)]

aa <- ggplot(TEMP2, aes(fill = Ageband,x = Year, y = Rate)) +
  #geom_bar(position="stack",stat = "identity") +
  geom_line(aes(color = Ageband)) +
  geom_point(aes(color = Ageband)) +
  facet_wrap(~ Meaning, ncol=2, scales = "fixed") +
  xlab("Year") +
  ylab("PY")+
  theme_classic() +
  #ggtitle("Source") +
  scale_fill_manual(values = colors) +
  theme(plot.margin=unit(c(2,2,2,2),"cm")) +
  theme(axis.text.x = element_text(margin = margin(t = 0))) +
  theme(legend.title = element_text(size = 7),legend.text = element_text(size = 7)) 
  #scale_x_continuous(breaks = x.thicks, labels = x.labels)





library(plotly)
ggplotly(aa)











theme(
  axis.title.y = element_text(vjust = 0,margin = margin(t = 0, r = 0, b = 0, l = 0))) +



TEMP2 <- INPUTMATRIX(
  d = TEMP,
  value = "PYStudy",
  type = "none",
  var = "year",
  var.v= years,
  cat = "Ageband" ,
  cat.v = stack , 
  per = F,
  perdir = "col",
  mask = F
  
)

TEMP3 <- INPUTMATRIX(
  d = TEMP,
  value = "PYSource",
  type = "none",
  var = "year",
  var.v= years,
  cat = "Ageband" ,
  cat.v = stack , 
  per = T,
  perdir = "col",
  mask = F
  
)

TEMP2 <- INPUTMATRIX(
  d = TEMP,
  value = "PYStudy",
  type = "none",
  var = "year",
  var.v= years,
  cat = "Ageband" ,
  cat.v = stack , 
  per = T,
  perdir = "col",
  mask = F
  
)

TEMP2 <- INPUTMATRIX(
  d = COUNT2,
  value = "diff_per",
  type = "none",
  var = "year",
  var.v = years,
  cat = "Ageband",
  cat.v = stack,
  per = F,
  #output = "long",
  mask = F
)

TEMP3 <- INPUTMATRIX(
  d = COUNT2,
  value = "diff_abs",
  type = "none",
  var = "year",
  var.v = years,
  cat = "Ageband",
  cat.v = stack,
  per = F,
  #output = "long",
  mask = F
)





























TEMP <- fread(paste0(report_dir1,"/",i,"_R_01_05_STUDYPOPPY2.csv"))

if(nrow(TEMP) > 0) {
  
  TEMP <- TEMP[, ':=' (Year = as.numeric(Year), Month = as.numeric(Month)) ]
  setorderv(TEMP,c("Year","Month"))
  TEMP <- TEMP[, X := paste0(Year," - ",Month)  ]
  TEMP1 <- melt(TEMP,id.vars = "X", measure.vars = c("PY Male","PY Female"),variable.name = "Gender",value.name = "PY")[,":=" (X = as.character(X), Gender = as.character(Gender),PY = as.numeric(PY))]
  
  d = TEMP1
  value = "PY"
  type = "none"
  var = "X"
  var.v = paste0(sort(rep(min(TEMP[["Year"]]):max(TEMP[["Year"]]),12))," - ", rep(1:12,max(TEMP[["Year"]])-min(TEMP[["Year"]])))
  cat = "Gender"
  cat.v = c("PY Male","PY Female")
  per = F
  
  perdir = "row"
  mask = T
  type = "base"
  
  INPUTMATRIX  <- function(d,value,type,var,var.v,cat = NULL,cat.v = NULL, per = F, perdir = "row", mask = T, output = "matrix" ){
    
    d <- copy(d)
    
    if(is.null(cat)){
      d[,cat := "ALL"]
      cat <- "cat"
      cat.v <- "ALL"
    }
    
    if(!class(d[[cat]]) == "character") d[,eval(cat) := as.character(get(cat)) ]
    if(!class(d[[var]]) == "character") d[,eval(var) := as.character(get(var)) ]
    
    if(type == "sum")graph <- aggregate(d[[value]],list(d[[var]],d[[cat]] ), FUN = sum,na.rm = TRUE)
    if(type == "count"){graph <- aggregate(!is.na(d[[value]]),list(d[[var]],d[[cat]] ), FUN = sum,na.rm = TRUE)}
    if(type == "none"){
      tmp.v <- c(var,cat,value)
      #graph <- d[,.(get(var),get(cat),get(value))]
      graph <- d[,..tmp.v]
      rm(tmp.v)
    }
    colnames(graph) <- c("Var","Cat","Val")
  
    graph <- graph[!is.na(graph[["Val"]]),]
    
    
    temp <- matrix(NA, ncol = length(var.v),nrow = length(cat.v))
    colnames(temp) <- as.character(var.v)
    row.names(temp) <- as.character(cat.v)
    
    
    for (i in as.character(cat.v)){
      
      for(j in as.character(var.v)){temp[i,j] <- ifelse(any(graph$Cat == i & graph$Var == j),graph$Val[graph$Cat == i & graph$Var == j] ,0)}
      
    }
    
    if(per){
      if(perdir == "row") temp <- round((temp/rowSums(temp))*100,1)
      if(perdir == "col") temp <- round(prop.table(temp, 2)*100,1)
      
      
    } 
    
  
    if(mask & !per){
      temp[temp > 0 & temp < 5] <- 5
      
    }
    
    if(output == "long"){
      temp <- as.data.table(temp, keep.rownames = cat)
      temp <- melt(temp,id.vars = cat ,measure.vars =  colnames(temp)[!colnames(temp) %in% cat], variable.name = var, value.name = value )
      #temp[, ':=' (eval(cat) = as.character(get(cat)), eval(var) = as.character(get(var)), eval(value) = as.numeric(get(value)))]  
      temp[, eval(cat) := as.character(get(cat))]  
      temp[, eval(var) := as.character(get(var))] 
      temp[, eval(value) := as.numeric(get(value))] 

    }
    
    
    
    return(temp)
    rm(d,graph,temp)
    gc()
  }
  
  
  
  
  
  
  
  
  TEMP1 <- INPUTMATRIX(
    
    d = TEMP1,
    value = "PY",
    type = "none",
    var = "X",
    var.v = paste0(sort(rep(min(TEMP[["Year"]]):max(TEMP[["Year"]]),12))," - ", rep(1:12,max(TEMP[["Year"]])-min(TEMP[["Year"]]))),
    cat = "Gender",
    cat.v = c("PY Male","PY Female"),
    per = F,
    output = "long"
    
    
  )
  
  
  
  
  
  
  
  x.thicks = unique(TEMP1[["X"]])[substr(unique(TEMP1[["X"]]),8,9) == "1"]
  x.labels = substr(x.thicks,1,4)
  
  
  ggplotly(ggplot(TEMP1, aes(x = X, y = PY, group = Gender)) +
             geom_line(aes(color = Gender)) +
             geom_point(aes(color = Gender)) + 
             #facet_wrap(~ meaning, ncol=2, scales = "fixed") +
             ggtitle("Test") + 
             xlab("Year") +
             ylab("PY")+
             theme_classic() +
             guides(shape = guide_legend(override.aes = list(size = 0.3))) +
             theme(text=element_text(size = 10),
                   axis.text.x = element_text(angle = 90, hjust = 1),
                   axis.title.x = element_text(colour = "#76b82a"),
                   axis.title.y = element_text(colour = "#76b82a"),
                   plot.title = element_text(colour = "#76b82a"),
                   strip.text.y = element_text(angle = 0),
                   legend.title = element_text(size = 7), 
                   legend.text = element_text(size = 7)) +
             scale_x_discrete(breaks = x.thicks, labels = x.labels)
           
           ) 
  
  
  
  ggplotly(plot(0:1,0:1))
  
  
  
  