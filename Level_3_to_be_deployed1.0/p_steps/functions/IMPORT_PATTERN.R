
#' Aim
#'
#'This function is ment to import groups of csv's and apply some basic formatting (Date, NA,leading/tailing spaces, all character) and selection/subsetting.  
#'The function appends the csv's to 1 file.
#'By default all columns are set to character format and from all columns leading and trailing spaces are removed and if "" cells are set to NA 
#'
#' @section ??
#'
#' @author
#' Roel Elbers
#'
#' @docType package
#' @name IMPORT_PATTERN
#' @keywords ??
#' @import data.table

NULL


#' @param pat A string that serves as a search term for the resembling files in a folder
#' @param dir The path of the folder you want to search in for your resembling csv's.
#' @param colls A vector of strings to indicate which column you want to import. This is optional.
#' @param colls.new A vector of strings to indicate which indicated the new column names. This is optional and the vector needs to have the same length as colls.
#' @param exprss A data.table expression filled within experssion(). With this exression you can subset the rows that are needed.
#' @param date.colls vector of strings to indicate which variables are dates with 'YYYYMMDD' format. These columns are set to date format.
#' @param append If FALSE it is checked that only 1 file is found with the pat in dir.If not the function is stopped. Default is TRUE, meaning no check.

#' @return A data.table data frame with an age band (bands/string), ages that are within the band (INT/integer), order for sorting (integer/Order) and an ageband with leading 0 (string/band0)

#' @export



IMPORT_PATTERN <- function(pat,dir, colls = NULL, colls.new = NULL, exprss = NULL, date.colls = NULL, append = T){
  
  #Find all files that contain the pat 
  obs_files<-list.files(dir, pattern = pat)
  
  #If more than 1 file is found while append is F then stop the function
  if(!append & length(obs_files) > 1){stop("Several files meet pattern while append is FALSE")}
  
  #Import every file and append loop wise. If no files found return NUL
  if(length(obs_files) > 0){
    
    for(i in 1:length(obs_files)){
      
      #Import csv
      if(!is.null(colls)){TEMP <- fread(paste0(dir,"/",obs_files[i]), stringsAsFactors = F, select = colls, na.strings = c("", NA))}else{
        TEMP <- fread(paste0(dir,"/",obs_files[i]), stringsAsFactors = F, na.strings = c("" , NA), colClasses=c("character"))
      }
      
      #if(!is.null(colls)){ TEMP <- TEMP[, ..colls] }
      
      #Rename
      if(!is.null(colls) & !is.null(colls.new)){setnames(TEMP, eval(colls),eval(colls.new))}
      
      
      #Set all columns to character, removed because added collcalsses in fread
      #invisible(lapply(colnames(TEMP), function (x) if (class(TEMP[[x]]) != "character") TEMP[, eval(x) := as.character(get(x)) ]))
      
      #Add correction for spaces to prevent misinterpretation of NA's and avoiding leading and tailing spaced. 
      invisible(lapply(colnames(TEMP), function(x) TEMP <- TEMP[, eval(x) := trimws(get(x), "b")]))
      TEMP[TEMP == ""] <- NA
      
      
      #Set specified columns in data.colls to date format
      if(!is.null(date.colls)){lapply(date.colls, function (x)  TEMP[, eval(x) := as.Date(get(x),"%Y%m%d") ])}
      
      #Apply the subsetting with the specified exprss
      if(!is.null(exprss)){
        
        rem1 <- nrow(TEMP) 
        TEMP <- TEMP[eval(exprss),]
        rem2 <- nrow(TEMP)
        print(paste0(rem2," selected rows from ",rem1," rows after evaluation of exprss"))
      }
      
      #Append the new file to the files imported prior
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
