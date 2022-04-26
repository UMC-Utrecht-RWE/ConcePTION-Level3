#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021


print('Import and append persons files')
persons_files<-list.files(path_dir, pattern="^PERSONS")

for (i in 1:length(persons_files)){
  PERSONS <- lapply(paste0(path_dir,persons_files[[i]]),fread)
}
PERSONS<-do.call(rbind,PERSONS)

print('Remove abbundant variables')
lapply(c("race","country_of_birth","quality"), function (x) PERSONS <- PERSONS[,eval(x) := NULL])

c("race","country_of_birth","quality")
dates_persons <- c("year_of_birth", "month_of_birth","day_of_birth","year_of_death", "month_of_death","day_of_death")

print('Check if date variables are integer, if not set to integer')
PERSONS$month_of_birth <- as.character(PERSONS$month_of_birth)
invisible(lapply(dates_persons, function (x) if (class(PERSONS[[x]]) != "integer") PERSONS[, eval(x) := as.integer(get(x)) ]))

print('Inpute birth and death day and month')
PERSONS[is.na(day_of_birth) & is.na(month_of_birth) & !is.na(year_of_birth), ':=' (day_of_birth = 1, month_of_birth = 6, inputed_birth_day = T,inputed_birth_month = T)]
PERSONS[is.na(day_of_birth) & !is.na(year_of_birth), ':=' (day_of_birth = 16, inputed_birth_day = T)]
PERSONS[is.na(month_of_birth) & !is.na(year_of_birth), ':=' (month_of_birth = 6, inputed_birth_month = T)]

PERSONS[is.na(day_of_death) & is.na(month_of_death) & !is.na(year_of_death), ':=' (day_of_death = 1, month_of_death = 6, inputed_death_day = T,inputed_death_month = T)]
PERSONS[is.na(day_of_death) & !is.na(year_of_death), ':=' (day_of_death = 16, inputed_death_day = T)]
PERSONS[is.na(month_of_death) & !is.na(year_of_death), ':=' (month_of_death = 6, inputed_death_month = T)]

INPUTED <- PERSONS[inputed_birth_day == T |inputed_birth_month == T|inputed_death_day == T| inputed_death_month == T ,.(person_id,inputed_birth_day,inputed_birth_month,inputed_death_day,inputed_death_month)]
saveRDS(INPUTED,file = paste0(std_pop_tmp,"INPUTED.rds"))
rm(INPUTED)
gc()

lapply(c("inputed_birth_day","inputed_birth_month","inputed_death_day","inputed_death_month"), function (x) PERSONS <- PERSONS[,eval(x) := NULL])

print('Create birth and death dates')
PERSONS[!is.na(day_of_birth) & !is.na(month_of_birth) & !is.na(year_of_birth),birth_date := as.IDate(paste0(year_of_birth, sprintf("%02d",month_of_birth),sprintf("%02d",day_of_birth)),"%Y%m%d")]
PERSONS[!is.na(day_of_death) & !is.na(month_of_death) & !is.na(year_of_death),death_date := as.IDate(paste0(year_of_death, sprintf("%02d",month_of_death),sprintf("%02d",day_of_death)),"%Y%m%d")]
PERSONS <- PERSONS[,age_start_study := floor(time_length(interval(birth_date, start_study_date),"year"))]

print('Delete abundant columns and tables')  
#lapply(c("day_of_death","month_of_death","year_of_death"), function (x) PERSONS <- PERSONS[,eval(x) := NULL])

saveRDS(PERSONS,file = paste0(std_pop_tmp,"PERSONS.rds"))
if(any(duplicated(PERSONS[["person_id"]]))) stop("Duplicates in person table") 
rm(PERSONS,dates_persons)
gc()
