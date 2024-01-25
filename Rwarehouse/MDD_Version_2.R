library(haven)
library(tidyverse)

#  Read dataset:
d = read_sav("2021 Gallup World Poll data for public release.sav") 

# Rename DQQ variables
d$DQQ1 <- d$WP22295
d$DQQ2 <- d$WP22296
d$DQQ3 <- d$WP22297
d$DQQ4 <- d$WP22298
d$DQQ5 <- d$WP22299
d$DQQ6.1 <- d$WP22300
d$DQQ6.2 <- d$WP22301
d$DQQ7.1 <- d$WP22302
d$DQQ7.2 <- d$WP22303
d$DQQ7.3 <- d$WP22304
d$DQQ8 <- d$WP22305
d$DQQ9 <- d$WP22306
d$DQQ10.1 <- d$WP22307
d$DQQ10.2 <- d$WP22308
d$DQQ10.3 <- d$WP22309
d$DQQ11 <- d$WP22310
d$DQQ12 <- d$WP22311
d$DQQ13 <- d$WP22312
d$DQQ13_IND <- d$WP22312_IND
d$DQQ14 <- d$WP22313
d$DQQ15 <- d$WP22314
d$DQQ16 <- d$WP22315
d$DQQ16_IND <- d$WP22315_IND
d$DQQ17 <- d$WP22316
d$DQQ17_IND1 <- d$WP22316_IND
d$DQQ17_IND2 <- d$WP22316_IND1
d$DQQ18 <- d$WP22317
d$DQQ18_IND1 <- d$WP22317_IND
d$DQQ18_IND2 <- d$WP22317_IND1
d$DQQ19 <- d$WP22318
d$DQQ19_IND <- d$WP22318_IND
d$DQQ20 <- d$WP22319
d$DQQ20_IND <- d$WP22319_IND
d$DQQ20_ISR1 <- d$WP22319_ISR
d$DQQ20_ISR2 <- d$WP22319_ISR1
d$DQQ21 <- d$WP22320
d$DQQ22 <- d$WP22321
d$DQQ23 <- d$WP22322
d$DQQ24 <- d$WP22323
d$DQQ25 <- d$WP22324
d$DQQ26 <- d$WP22325
d$DQQ27 <- d$WP22326
d$DQQ28 <- d$WP22327
d$DQQ29 <- d$WP22328

d <- d[ , c("STRATA", "PSU", "CaseID", "Weight", "FieldDate", "Country", "Gender", 
            "Age", "Education", "IncomeQuintiles", "DEGURBA_F2F", "DEGURBA_PHONE", 
            "Urbanicity", "DQQ1", "DQQ2", "DQQ3", "DQQ4", "DQQ5", "DQQ6.1", "DQQ6.2", 
            "DQQ7.1", "DQQ7.2", "DQQ7.3", "DQQ8", "DQQ9", "DQQ10.1", "DQQ10.2", "DQQ11", 
            "DQQ12", "DQQ13", "DQQ13_IND", "DQQ14", "DQQ15", "DQQ16", "DQQ16_IND", "DQQ17", 
            "DQQ17_IND1", "DQQ17_IND2", "DQQ18", "DQQ18_IND1", "DQQ18_IND2", "DQQ19", "DQQ19_IND", 
            "DQQ20", "DQQ20_IND", "DQQ20_ISR1", "DQQ20_ISR2", "DQQ21", "DQQ22", "DQQ23", "DQQ24", 
            "DQQ25", "DQQ26", "DQQ27", "DQQ28", "DQQ29")]

start.time <- Sys.time()

d$MDD <- (rowSums(d[c("DQQ1","DQQ2", "DQQ3")] == 1, na.rm=TRUE) > 0) + 
         (rowSums(d[c("DQQ14","DQQ15", "DQQ25")] == 1, na.rm=TRUE) > 0) +
         (rowSums(d[c("DQQ16","DQQ17", "DQQ18", "DQQ19","DQQ20")] == 1, na.rm=TRUE) > 0) +
         (rowSums(d[c("DQQ6.1","DQQ6.2")] == 1, na.rm=TRUE) > 0) +
         (rowSums(d[c("DQQ5","DQQ8")] == 1, na.rm=TRUE) > 0) +
         (rowSums(d[c("DQQ7.1","DQQ7.2", "DQQ7.3")] == 1, na.rm=TRUE) > 0) +
         (rowSums(d[c("DQQ9","DQQ10.1", "DQQ10.2")] == 1, na.rm=TRUE) > 0) 

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken # 0.1159301 secs

write_csv(d, "MDD_Version_2.csv")



