
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021


SOURCE_POPULATION <- readRDS(file = paste0(std_pop_tmp,"ALL_source_population.rds"))[age_start_study >= 0 & sex_at_instance_creation %in% c("F","M"),.(person_id,sex_at_instance_creation,age_start_study)]

Agebands <- CreateBands(seq(from = 0, to = 125, by = 5))

SOURCE_POPULATION <- merge(x = SOURCE_POPULATION, y = Agebands,by.x = "age_start_study", by.y = "INT", all.x = F, all.y = F)
setorder(SOURCE_POPULATION,Order)

TEMP <- INPUTMATRIX(
  d = SOURCE_POPULATION,
  value = "person_id",
  type = "count",
  var = "band",
  var.v = unique(SOURCE_POPULATION[["band"]]),
  cat = "sex_at_instance_creation",
  cat.v = c("F","M"),
  per = T
)

saveRDS(TEMP, file = paste0(std_source_pop_dir,"R_01_01_POPTREE.rds"))
fwrite(as.data.table(TEMP, keep.rownames = T), file = paste0(std_source_pop_dir,"R_01_01_POPTREE.csv"),sep = ";" )

rm(TEMP,Agebands,SOURCE_POPULATION)







