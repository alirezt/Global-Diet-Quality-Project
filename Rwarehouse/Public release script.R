library(haven)
library(tidyverse)

#  Read dataset:
d = read_sav("2021 Gallup World Poll data for public release.sav") 
View(d)
names(d)


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

# Add DQQ10.3 if data includes that variable

d <- d[ , c("STRATA", "PSU", "CaseID", "Weight", "FieldDate", "Country", "Gender", 
            "Age", "Education", "IncomeQuintiles", "DEGURBA_F2F", "DEGURBA_PHONE", 
            "Urbanicity", "DQQ1", "DQQ2", "DQQ3", "DQQ4", "DQQ5", "DQQ6.1", "DQQ6.2", 
            "DQQ7.1", "DQQ7.2", "DQQ7.3", "DQQ8", "DQQ9", "DQQ10.1", "DQQ10.2", "DQQ11", 
            "DQQ12", "DQQ13", "DQQ13_IND", "DQQ14", "DQQ15", "DQQ16", "DQQ16_IND", "DQQ17", 
            "DQQ17_IND1", "DQQ17_IND2", "DQQ18", "DQQ18_IND1", "DQQ18_IND2", "DQQ19", "DQQ19_IND", 
            "DQQ20", "DQQ20_IND", "DQQ20_ISR1", "DQQ20_ISR2", "DQQ21", "DQQ22", "DQQ23", "DQQ24", 
            "DQQ25", "DQQ26", "DQQ27", "DQQ28", "DQQ29")]

write_csv(d, "DQQ2021_NoNA.csv")

# Setting NA values in DQQ variables to 13
d_noNA <- d

for (i in 1:nrow(d_noNA)){
  for (j in 14:ncol(d_noNA)){
    if (is.na(d_noNA[i,j])){
      d_noNA[i,j] <- 13
    }
  }
}

MDD <- rep(0, nrow(d_noNA))
d_noNA <- cbind(d_noNA, MDD)
names(d_noNA)
d_noNA <- d_noNA %>% select(-one_of("MDD1", "MDD5", "MDD4", "MDD7","MDD8","MDD9", "MDD10"))
write_csv(d_noNA, "DQQ2021_NoNA.csv")

# measuring MDD variable

for (i in 1:nrow(d_noNA)){
  # first: Grains, white roots and tubers, and plantains
  if ((d_noNA$DQQ1[i] == 1) == TRUE | (d_noNA$DQQ2[i] == 1) == TRUE | (d_noNA$DQQ3[i] == 1) == TRUE){
    d_noNA$MDD[i] <- 1+(d_noNA$MDD[i])
  } else {
    d_noNA$MDD[i] <- 0+(d_noNA$MDD[i])
  }
  # fourth: dairy
  if ((d_noNA$DQQ14[i] == 1) == TRUE | (d_noNA$DQQ15[i] == 1) == TRUE | (d_noNA$DQQ25[i] == 1) == TRUE) {
    d_noNA$MDD[i] <- 1+(d_noNA$MDD[i])
  } else {
    d_noNA$MDD[i] <- 0+(d_noNA$MDD[i])
  }
  # fifth: Meat, poultry and fish
  if ((d_noNA$DQQ16[i] == 1) == TRUE | (d_noNA$DQQ17[i] == 1) == TRUE | (d_noNA$DQQ18[i] == 1) == TRUE | (d_noNA$DQQ19[i] == 1) == TRUE | (d_noNA$DQQ20[i] == 1) == TRUE) {
    d_noNA$MDD[i] <- 1+(d_noNA$MDD[i])
  } else {
    d_noNA$MDD[i] <- 0+(d_noNA$MDD[i])
  }
  # seventh: Dark green leafy vegetables
  if ((d_noNA$DQQ6.1[i] == 1) == TRUE | (d_noNA$DQQ6.2[i] == 1) == TRUE ) {
    d_noNA$MDD[i] <- 1+(d_noNA$MDD[i])
  } else {
    d_noNA$MDD[i] <- 0+(d_noNA$MDD[i])
  }
  # eighth: Other vitamin A-rich fruits and vegetables
  if ((d_noNA$DQQ5[i] == 1) == TRUE | (d_noNA$DQQ8[i] == 1) == TRUE ) {
    d_noNA$MDD[i] <- 1+(d_noNA$MDD[i])
  } else {
    d_noNA$MDD[i] <- 0+(d_noNA$MDD[i])
  }
  # ninth: Other vegetables
  if ((d_noNA$DQQ7.1[i] == 1) == TRUE | (d_noNA$DQQ7.2[i] == 1) == TRUE | (d_noNA$DQQ7.3[i] == 1) == TRUE ) {
    d_noNA$MDD[i] <- 1+(d_noNA$MDD[i])
  } else {
    d_noNA$MDD[i] <- 0+(d_noNA$MDD[i])
  }
  # tenth: Other fruits 
  if ((d_noNA$DQQ9[i] == 1) == TRUE | (d_noNA$DQQ10.1[i] == 1) == TRUE | (d_noNA$DQQ10.2[i] == 1) == TRUE ) {
    d_noNA$MDD[i] <- 1+(d_noNA$MDD[i])
  } else {
    d_noNA$MDD[i] <- 0+(d_noNA$MDD[i])
  }
}

write_csv(d_noNA, "DQQ2021_NoNAtest.csv")





