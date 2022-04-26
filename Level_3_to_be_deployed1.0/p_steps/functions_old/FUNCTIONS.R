
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

CreateBands <- function(bands){

  bands_list <- list()

      for (k in 1:length(bands)){
        if(!k== length(bands)){
          bands_list[["band"]][k] <- paste0(bands[k],"-",bands[k+1]-1)
          bands_list[["ST"]][k] <- bands[k]
          bands_list[["END"]][k] <- bands[k+1]-1
        }
      }

  bands <- as.data.frame(cbind("band" = bands_list[["band"]],"ST" = bands_list[["ST"]],"END" = bands_list[["END"]]))
  
  new <- list()

      for(i in 1:nrow(bands)){
        if(length(strsplit(bands[i,"band"],"-")[[1]])==2) {
          ST <- as.integer(strsplit(bands[i,"band"],"-")[[1]][1])
          EN <- as.integer(strsplit(bands[i,"band"],"-")[[1]][2])
          vec <- c(ST:EN)
          new[[i]] <-cbind(bands[i,][rep(seq_len(nrow(bands[i,])), each = length(vec)),],"new" = vec)
          
        } else new[[i]] <- bands[i,]
        
      }

  bands <- do.call(rbind,new)
  bands <- as.data.table(bands)[,INT := as.numeric(new)][,.(band,INT)]
  
  setorder(bands,INT)
  ORDER <- as.data.table(cbind("band" = unique(bands[,band]),"Order" = seq(from = 1, to = length(unique(bands[,band])),by = 1)))[,Order := as.integer(Order)]
  bands <- merge(x= bands, y= ORDER, by = "band", all.x = T) 
  setorder(bands,Order)

}

IMPORT_PATTERN <- function(pat,dir){
  obs_files<-list.files(dir, pattern=pat)
  if(length(obs_files) > 0){
  temp <- list()
  for(i in 1:length(obs_files)){
    TEMP <- fread(paste0(dir,"/",obs_files[i]), stringsAsFactors = F)
    
    
    if(i == 1) {FILE <- TEMP}
    if(i > 1) {FILE <- rbindlist(list(TEMP,FILE),fill = T, use.names = T)}
    rm(TEMP)
    gc()
    
    
  }
  }else FILE <- NULL 
    
  return(FILE)
  rm(FILE,obs_files)
  gc()
}



SeparateRanges <- function(codesheet,code_col,separator,vars){
  
  new <- list()
  
  for(i in 1:nrow(codesheet)){
    if(length(strsplit(codesheet[i,code_col],separator)[[1]])==2) {
      
      ST <- strsplit(codesheet[i,code_col],"-")[[1]][1]
      EN <- strsplit(codesheet[i,code_col],"-")[[1]][2]
      
      EN_char <- gsub("^([[:alpha:]]*).*$","\\1",EN)
      ST_char <- gsub("^([[:alpha:]]*).*$","\\1",ST)
      if(EN_char != ST_char) warning("Chars differ")
      
      EN_num <- floor(as.numeric(gsub("[^[:digit:].]", "\\1",EN)))
      ST_num <- floor(as.numeric(gsub("[^[:digit:].]", "\\1",ST)))
      
      vec <- paste0(ST_char,formatC(c(ST_num:EN_num), width = 2, format = "d", flag = "0"))
      
      
      
      
      new[[i]] <-cbind(codesheet[i,][rep(seq_len(nrow(codesheet[i,])), each = length(vec)),],"Code_new" = vec)
      
      
      
      
    } else new[[i]] <- codesheet[i,]
    
  }
  
  file <- do.call(rbind,new)
  
}


INPUTMATRIX  <- function(d,value,type,var,var.v,cat = NULL,cat.v = NULL, per = F, perdir = "row"){
  
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
  #graph <- graph[!is.na(Val),]
  graph <- graph[!is.na(graph[["Val"]]),]
  #rm(d)
  #gc()
  
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
  
  return(temp)
  rm(d,graph)
  gc()
}


Line_plot <- function(MATRIX,title,x.l,y.l,color){
  
  plot(NULL,type="l",xlab = x.l,cex.lab = 0.8,lwd=3,xlim = c(1,length(colnames(MATRIX))), ylim = c(1,max(MATRIX)), main= title, axes = F, mgp=c(4, 0, 0) )
  
  
  axis(1,at=1:length(colnames(MATRIX)),labels = colnames(MATRIX), las=2,cex.axis =0.6)
  t <- format(seq(from = 0,to = ceiling(max(MATRIX)),length.out = 10),digits = 0)
  axis(2, at = t , labels = as.character(t),cex.axis =0.6 )
  mtext(y.l, side=2, line=2, cex=0.8,las=0, col="black",)
  #mtext(x.l, side=1, line=2, cex=0.8,las=0, col="black",)
  
  
  #temp <-MATRIX[,which(colSums(temp)>0),drop=F]
  for(i in 1:nrow(MATRIX)){
    #if(max(temp[i,]) > 0){
    lines(MATRIX[i,],lwd=2.3,type = 'l', col = color)
    #}
  }
  
}




POP_TREE <- function(m, xlabel = 'Percentage', offset = 1, linewidth = 15, cols = c('pink', 'lightblue'), x.axis.steps = 2, cex.percentage = 0.7){
  xlimits <- c(-max(m) - offset + (0.1*-max(m)), max(m) + offset + (0.1*max(m)))    
  ylimits <- c(1, ncol(m))
  x.lab <- seq(0, ceiling(max(xlimits)), x.axis.steps)
  x.at <- seq(offset, ceiling(max(xlimits) + offset), x.axis.steps)
  plot(NULL, xlim = xlimits, ylim = ylimits, main = '', xlab = xlabel, axes = F, ylab = '')
  abline(v = c(-x.at, x.at), col = 'lightgrey', lty = 'dotted')
  segments(x0 = -offset, y0 = 1:ncol(m), x1 = -1 * m[1,] - offset, lwd = linewidth, lend = 1, col = cols[1])
  segments(x0 = offset, y0 = 1:ncol(m), x1 = m[2,] + offset, lwd = linewidth, lend = 1, col = cols[2])
  text(colnames(m), x = 0, y = 1:ncol(m), cex = 0.8)
  text(as.character(round(m[1,], 1)), x = -m[1,] - offset, y = 1:ncol(m), pos = 2, cex = cex.percentage)
  text(as.character(round(m[2,], 1)), x = m[2,] + offset, y = 1:ncol(m), pos = 4, cex = cex.percentage)    
  axis(1, x.lab, at = x.at, cex.axis = 0.8)
  axis(1, rev(x.lab), at = -rev(x.at), cex.axis = 0.8)
  #legend('topright', legend = row.names(m), ncol = 1, cex = 0.8, fill = cols, bty = 'n')
  mtext(rownames(m)[1], at = -offset, side = 3, adj = 1, line = 1)
  mtext(rownames(m)[2], at = offset, side = 3, adj = 0, line = 1)
}






Line_plot2 <- function(MATRIX, title, x.l, y.l, y.axis = F, color = NULL, leg = T, y.labels = NULL, y.thicks = NULL, y.las = 2){
  
  
  #if(is.null(color)) color <- rainbow(nrow(MATRIX))
  if(is.null(color)) color <- c(2:(nrow(MATRIX)+1))
  plot(NULL,type="l",xlab = x.l,ylab = y.l,cex.lab = 0.8,lwd=3,xlim = c(1,length(colnames(MATRIX))), ylim = c(0,max(MATRIX)), main= title, axes = F)
  
  #X-axis
  if(!y.axis) {axis(1,at=1:length(colnames(MATRIX)),labels = colnames(MATRIX), las=2,cex.axis =0.6)}else{
  axis(1,at=y.thicks,labels = y.labels, las = y.las,cex.axis = 0.6)}
      
  if(ceiling(max(MATRIX)) < 10) t <- format(seq(from = 0,to = ceiling(max(MATRIX)),length.out = 11),digits = 1)
  if(ceiling(max(MATRIX)) >= 10)t <- format(seq(from = 0,to = ceiling(max(MATRIX)),length.out = 11),digits = 0)
  #axis(2, at = t , labels = as.character(t),cex.axis =0.6 )
  
  #axis(2, at = t , labels = as.character(t),cex.axis =0.6 )
  axis(2, cex.axis =0.6 )
  
  #mtext(y.l, side=2, line=2, cex=0.8,las=0, col="black")
  #mtext(x.l, side=1, line=2, cex=0.8,las=0, col="black")
  
  
  #temp <-MATRIX[,which(colSums(temp)>0),drop=F]
  for(i in 1:nrow(MATRIX)){
    #if(max(temp[i,]) > 0){
    lines(MATRIX[i,],lwd=2.3,type = 'l',col = color[i])
    #}
  }
  if(leg)legend("right",legend =  rownames(MATRIX), col = color, cex = 0.5,pch=10, box.col = "white",inset = c(-0.1, 0))
  
}









