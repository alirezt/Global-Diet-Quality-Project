# DQQ variables ----

## 1. Adding required libraries ----
library(haven)
library(tidyverse)

## 2. Data preparation ----
### 2.1 setting working directory n----
#getwd()
#setwd() 
d = read_sav("2021 Gallup World Poll data for public release.sav") 
write.csv(d, "inputData.csv")

### 2.2 Rename DQQ variables ----
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

##  3. DQQ variables computation  ----
###  1. MDD-W and FGDS  ----
####  1.1 FGDS  ----
d$FGDS <- (rowSums(d[c("DQQ1","DQQ2", "DQQ3")] == 1, na.rm=TRUE) > 0) + 
          (rowSums(d[c("DQQ4")] == 1, na.rm=TRUE) > 0) +
          (rowSums(d[c("DQQ21")] == 1, na.rm=TRUE) > 0) +
          (rowSums(d[c("DQQ14","DQQ15", "DQQ25")] == 1, na.rm=TRUE) > 0) +
          (rowSums(d[c("DQQ16","DQQ17", "DQQ18", "DQQ19","DQQ20")] == 1, na.rm=TRUE) > 0) +
          (rowSums(d[c("DQQ13")] == 1, na.rm=TRUE) > 0) +
          (rowSums(d[c("DQQ6.1","DQQ6.2")] == 1, na.rm=TRUE) > 0) +
          (rowSums(d[c("DQQ5","DQQ8")] == 1, na.rm=TRUE) > 0) +
          (rowSums(d[c("DQQ7.1","DQQ7.2", "DQQ7.3")] == 1, na.rm=TRUE) > 0) +
          (rowSums(d[c("DQQ9","DQQ10.1", "DQQ10.2")] == 1, na.rm=TRUE) > 0) 

####  1.2 MDD-W  ----
d$MDD_W <- ifelse(((rowSums(d[c("DQQ1","DQQ2", "DQQ3")] == 1, na.rm=TRUE) > 0) + 
                     (rowSums(d[c("DQQ4")] == 1, na.rm=TRUE) > 0) +
                     (rowSums(d[c("DQQ21")] == 1, na.rm=TRUE) > 0) +
                     (rowSums(d[c("DQQ14","DQQ15", "DQQ25")] == 1, na.rm=TRUE) > 0) +
                     (rowSums(d[c("DQQ16","DQQ17", "DQQ18", "DQQ19","DQQ20")] == 1, na.rm=TRUE) > 0) +
                     (rowSums(d[c("DQQ13")] == 1, na.rm=TRUE) > 0) +
                     (rowSums(d[c("DQQ6.1","DQQ6.2")] == 1, na.rm=TRUE) > 0) +
                     (rowSums(d[c("DQQ5","DQQ8")] == 1, na.rm=TRUE) > 0) +
                     (rowSums(d[c("DQQ7.1","DQQ7.2", "DQQ7.3")] == 1, na.rm=TRUE) > 0) +
                     (rowSums(d[c("DQQ9","DQQ10.1", "DQQ10.2")] == 1, na.rm=TRUE) > 0)) >= 5 & d$Gender == 2 &  d$Age >= 15 & d$Age <= 49, 1, 
                 
                 ifelse(((rowSums(d[c("DQQ1","DQQ2", "DQQ3")] == 1, na.rm=TRUE) > 0) + 
                           (rowSums(d[c("DQQ4")] == 1, na.rm=TRUE) > 0) +
                           (rowSums(d[c("DQQ21")] == 1, na.rm=TRUE) > 0) +
                           (rowSums(d[c("DQQ14","DQQ15", "DQQ25")] == 1, na.rm=TRUE) > 0) +
                           (rowSums(d[c("DQQ16","DQQ17", "DQQ18", "DQQ19","DQQ20")] == 1, na.rm=TRUE) > 0) +
                           (rowSums(d[c("DQQ13")] == 1, na.rm=TRUE) > 0) +
                           (rowSums(d[c("DQQ6.1","DQQ6.2")] == 1, na.rm=TRUE) > 0) +
                           (rowSums(d[c("DQQ5","DQQ8")] == 1, na.rm=TRUE) > 0) +
                           (rowSums(d[c("DQQ7.1","DQQ7.2", "DQQ7.3")] == 1, na.rm=TRUE) > 0) +
                           (rowSums(d[c("DQQ9","DQQ10.1", "DQQ10.2")] == 1, na.rm=TRUE) > 0)) < 5 & d$Gender == 2 &  d$Age >= 15 & d$Age <= 49, 0, NA))                                                                


### 2. All-5  ----
d$All_5 <- ifelse(((rowSums(d[c("DQQ1","DQQ2", "DQQ3")] == 1, na.rm=TRUE) > 0) + 
                    (rowSums(d[c("DQQ5", "DQQ6.1", "DQQ6.2", "DQQ7.1", "DQQ7.2", "DQQ7.3")] == 1, na.rm=TRUE) > 0) +
                    (rowSums(d[c("DQQ8","DQQ9", "DQQ10.1", "DQQ10.2")] == 1, na.rm=TRUE) > 0) +
                    (rowSums(d[c("DQQ4","DQQ21")] == 1, na.rm=TRUE) > 0) +
                    (rowSums(d[c("DQQ13","DQQ14", "DQQ15", "DQQ17", "DQQ18", "DQQ19", "DQQ20", "DQQ25")] == 1, na.rm=TRUE) > 0)) >= 5, 1, 0)

#### 2.a.	At least one vegetable  ----
d$All_5_a <- ifelse((rowSums(d[c("DQQ5", "DQQ6.1", "DQQ6.2", "DQQ7.1", "DQQ7.2", "DQQ7.3")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)
#### 2.b.	At least one fruit  ----
d$All_5_b <- ifelse((rowSums(d[c("DQQ8","DQQ9", "DQQ10.1", "DQQ10.2")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)
#### 2.c. At least one pulse, nut or seed  ----
d$All_5_c <- ifelse((rowSums(d[c("DQQ4","DQQ21")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)
#### 2.d. d.	At least one animal-source food (ASF)  ----
d$All_5_d <- ifelse((rowSums(d[c("DQQ13","DQQ14", "DQQ15", "DQQ17", "DQQ18", "DQQ19", "DQQ20", "DQQ25")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)
#### 2.e. At least starchy staple  ----
d$All_5_e <- ifelse((rowSums(d[c("DQQ1","DQQ2", "DQQ3")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 3. NCD-Protect score  ----
d$NCD_P <- (rowSums(d[c("DQQ2")] == 1, na.rm=TRUE) > 0) + 
           (rowSums(d[c("DQQ4")] == 1, na.rm=TRUE) > 0) +
           (rowSums(d[c("DQQ21")] == 1, na.rm=TRUE) > 0) +
           (rowSums(d[c("DQQ5")] == 1, na.rm=TRUE) > 0) +
           (rowSums(d[c("DQQ6.1","DQQ6.2")] == 1, na.rm=TRUE) > 0) +
           (rowSums(d[c("DQQ7.1","DQQ7.2", "DQQ7.3")] == 1, na.rm=TRUE) > 0) +
           (rowSums(d[c("DQQ8")] == 1, na.rm=TRUE) > 0)+
           (rowSums(d[c("DQQ9")] == 1, na.rm=TRUE) > 0)+
           (rowSums(d[c("DQQ10.1", "DQQ10.2")] == 1, na.rm=TRUE) > 0)

### 4. NCD-Risk score  ----
d$NCD_R <- (rowSums(d[c("DQQ28")] == 1, na.rm=TRUE) > 0) + 
           (rowSums(d[c("DQQ11")] == 1, na.rm=TRUE) > 0) +
           (rowSums(d[c("DQQ12")] == 1, na.rm=TRUE) > 0) +
           (rowSums(d[c("DQQ16")] == 1, na.rm=TRUE) > 0) +
           (rowSums(d[c("DQQ16")] == 1, na.rm=TRUE) > 0) +
           (rowSums(d[c("DQQ17","DQQ18")] == 1, na.rm=TRUE) > 0) +
           (rowSums(d[c("DQQ24")] == 1, na.rm=TRUE) > 0) +
           (rowSums(d[c("DQQ23", "DQQ29")] == 1, na.rm=TRUE) > 0)+
           (rowSums(d[c("DQQ22")] == 1, na.rm=TRUE) > 0)

### 5. GDR score  ----
d$GDR <- (rowSums(d[c("DQQ2")] == 1, na.rm=TRUE) > 0) + 
         (rowSums(d[c("DQQ4")] == 1, na.rm=TRUE) > 0) +
         (rowSums(d[c("DQQ21")] == 1, na.rm=TRUE) > 0) +
         (rowSums(d[c("DQQ5")] == 1, na.rm=TRUE) > 0) +
         (rowSums(d[c("DQQ6.1","DQQ6.2")] == 1, na.rm=TRUE) > 0) +
         (rowSums(d[c("DQQ7.1","DQQ7.2", "DQQ7.3")] == 1, na.rm=TRUE) > 0) +
         (rowSums(d[c("DQQ8")] == 1, na.rm=TRUE) > 0)+
         (rowSums(d[c("DQQ8")] == 1, na.rm=TRUE) > 0)+
         (rowSums(d[c("DQQ9")] == 1, na.rm=TRUE) > 0)+
         (rowSums(d[c("DQQ10.1", "DQQ10.2")] == 1, na.rm=TRUE) > 0)-
         (rowSums(d[c("DQQ28")] == 1, na.rm=TRUE) > 0) - 
         (rowSums(d[c("DQQ11")] == 1, na.rm=TRUE) > 0) -
         (rowSums(d[c("DQQ12")] == 1, na.rm=TRUE) > 0) -
         (rowSums(d[c("DQQ16")] == 1, na.rm=TRUE) > 0) -
         (rowSums(d[c("DQQ16")] == 1, na.rm=TRUE) > 0) -
         (rowSums(d[c("DQQ17","DQQ18")] == 1, na.rm=TRUE) > 0) -
         (rowSums(d[c("DQQ24")] == 1, na.rm=TRUE) > 0) -
         (rowSums(d[c("DQQ23", "DQQ29")] == 1, na.rm=TRUE) > 0)-
         (rowSums(d[c("DQQ22")] == 1, na.rm=TRUE) > 0)+ 9*TRUE

### 6. DQQ score  ----

### 7. Zero vegetable or fruit   ----
d$zeroVeg <- ifelse((rowSums(d[c("DQQ5","DQQ6.1","DQQ6.2", "DQQ7.1", "DQQ7.2", "DQQ7.3", "DQQ8", "DQQ9", "DQQ10.1", "DQQ10.2")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 8. ASF consumption  ----
d$ASF <- ifelse((rowSums(d[c("DQQ13","DQQ14","DQQ15", "DQQ17", "DQQ18", "DQQ19", "DQQ20", "DQQ25")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 9. Sweet beverage consumption   ----
d$SweetBev <- ifelse((rowSums(d[c("DQQ26","DQQ27","DQQ28")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

#### 9.a. Sugar-sweetened soft drink consumption  ----
d$SugarDrink <- ifelse((rowSums(d[c("DQQ28")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 10. Sweet foods consumption   ----
d$SweetFood <- ifelse((rowSums(d[c("DQQ11", "DQQ12")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 11. Salty or fried snack consumption   ----
d$SaltyFood <- ifelse((rowSums(d[c("DQQ22","DQQ23","DQQ24")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 12. Whole grain consumption   ----
d$WholeGrain <- ifelse((rowSums(d[c("DQQ2")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 13. Pulse consumption   ----
d$Pulse <- ifelse((rowSums(d[c("DQQ4")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 14. Nuts and seeds consumption   ----
d$Nuts <- ifelse((rowSums(d[c("DQQ21")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 15. Processed meat consumption   ----
d$ProMeat <- ifelse((rowSums(d[c("DQQ16")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 16. WHO-FV score   ----
d$WHO_FV <- ifelse(((rowSums(d[c("DQQ5")] == 1, na.rm=TRUE) > 0) + 
                    (rowSums(d[c("DQQ6.1","DQQ6.2")] == 1, na.rm=TRUE) > 0) +
                    (rowSums(d[c("DQQ7.1","DQQ7.2", "DQQ7.3")] == 1, na.rm=TRUE) > 0) +
                    (rowSums(d[c("DQQ8")] == 1, na.rm=TRUE) > 0)+
                    (rowSums(d[c("DQQ9")] == 1, na.rm=TRUE) > 0)+
                    (rowSums(d[c("DQQ10.1", "DQQ10.2")] == 1, na.rm=TRUE) > 0)) >= 3, 1, 0)

### 17. WHO-Fiber score  ----
d$WHO_Fiber <- ifelse(((rowSums(d[c("DQQ5")] == 1, na.rm=TRUE) > 0) + 
                       (rowSums(d[c("DQQ6.1","DQQ6.2")] == 1, na.rm=TRUE) > 0) +
                       (rowSums(d[c("DQQ7.1","DQQ7.2", "DQQ7.3")] == 1, na.rm=TRUE) > 0) +
                       (rowSums(d[c("DQQ8")] == 1, na.rm=TRUE) > 0)+
                       (rowSums(d[c("DQQ9")] == 1, na.rm=TRUE) > 0)+
                       (rowSums(d[c("DQQ4")] == 1, na.rm=TRUE) > 0)+
                       (rowSums(d[c("DQQ4")] == 1, na.rm=TRUE) > 0)+
                       (rowSums(d[c("DQQ21")] == 1, na.rm=TRUE) > 0)+
                       (rowSums(d[c("DQQ2")] == 1, na.rm=TRUE) > 0)+
                       (rowSums(d[c("DQQ10.1", "DQQ10.2")] == 1, na.rm=TRUE) > 0)) >= 4, 1, 0)

### 18. WHO-Sugar score  ----
d$WHO_Sugar <- ifelse(((rowSums(d[c("DQQ28")] == 1, na.rm=TRUE) > 0) +
                       (rowSums(d[c("DQQ28")] == 1, na.rm=TRUE) > 0) +
                       (rowSums(d[c("DQQ27")] == 1, na.rm=TRUE) > 0) +
                       (rowSums(d[c("DQQ26")] == 1, na.rm=TRUE) > 0) +
                       (rowSums(d[c("DQQ11")] == 1, na.rm=TRUE) > 0)+
                       (rowSums(d[c("DQQ12")] == 1, na.rm=TRUE) > 0)) >= 2, 1, 0)

### 19. WHO-Saturated fat score  ----
d$WHO_SatFat <- ifelse(((rowSums(d[c("DQQ12")] == 1, na.rm=TRUE) > 0) +
                         (rowSums(d[c("DQQ14", "DQQ15")] == 1, na.rm=TRUE) > 0) +
                         (rowSums(d[c("DQQ25")] == 1, na.rm=TRUE) > 0) +
                         (rowSums(d[c("DQQ16")] == 1, na.rm=TRUE) > 0) +
                         (rowSums(d[c("DQQ17", "DQQ18")] == 1, na.rm=TRUE) > 0)-
                         (rowSums(d[c("DQQ20")] == 1, na.rm=TRUE) > 0)-
                         (rowSums(d[c("DQQ19")] == 1, na.rm=TRUE) > 0)+
                         (rowSums(d[c("DQQ29")] == 1, na.rm=TRUE) > 0)) >= 2, 1, 0)

### 20. Percent consuming each food group   ----
#### 20.a. at least one vegetable or fruit   ----
d$Veg <- ifelse((rowSums(d[c("DQQ5","DQQ6.1","DQQ6.2", "DQQ7.1", "DQQ7.2", "DQQ7.3", "DQQ8", "DQQ9", "DQQ10.1","DQQ10.2")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

#### 20.b. ultra-processed salty snacks, instant noodles, or fast food  ----
d$Junk <- ifelse((rowSums(d[c("DQQ22","DQQ23","DQQ29")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

## 4. CSV Export ----
write_csv(d, "DQQ_Variables_Main.csv")


## 5. Web variables ----

d2 <- d %>% 
  group_by(Country) %>%
  summarise(
    MDD_pc   = round(mean(MDD_W == 1, na.rm = TRUE)*100, digits = 2), #MDD-W
    FGDS_avg = round(mean(FGDS, na.rm = TRUE), digits = 2), #Food group diversity score
    All5_pc  = round(prop.table(table(All_5))["1"]*100, digits = 2), #All-5
    All5e_pc = round(prop.table(table(All_5_e))["1"]*100, digits = 2), #At least one starchy staple food
    All5a_pc = round(prop.table(table(All_5_a))["1"]*100, digits = 2), #At least one vegetable
    All5b_pc = round(prop.table(table(All_5_b))["1"]*100, digits = 2), #At least one fruit
    All5c_pc = round(prop.table(table(All_5_c))["1"]*100, digits = 2), #At least one pulse, nut, or seed
    All5d_pc = round(prop.table(table(All_5_d))["1"]*100, digits = 2), #At least one animal-source food
    zeroVeg_pc = round(prop.table(table(zeroVeg))["1"]*100, digits = 2), #Zero vegetable or fruit consumption
    sugarDrink_pc = round(prop.table(table(SugarDrink))["1"]*100, digits = 2), #Sugar-sweetened soft drink consumption
    All5e_pc = round(prop.table(table(All_5_e))["1"]*100, digits = 2), #At least one starchy staple food
    whGrain_pc = round(prop.table(table(DQQ1))["1"]*100, digits = 2), #Whole grains
    grains_pc = round(prop.table(table(DQQ2))["1"]*100, digits = 2), #Foods made from grains
    roots_pc = round(prop.table(table(DQQ3))["1"]*100, digits = 2), #White roots or tubers
    All5c_pc = round(prop.table(table(All_5_c))["1"]*100, digits = 2), #At least one pulse, nut, or seed
    pulse_pc = round(prop.table(table(DQQ4))["1"]*100, digits = 2), #Pulses
    nuts_pc = round(prop.table(table(DQQ21))["1"]*100, digits = 2), #Nuts or seeds
    All5a_pc = round(prop.table(table(All_5_a))["1"]*100, digits = 2), #At least one vegetable
    AVeg_pc = round(mean(DQQ5 == 1, na.rm = TRUE)*100, digits = 2), #Vitamin A-rich orange vegetables
    DVeg_pc = round(mean(rowSums(across(DQQ6.1:DQQ6.2) == 1, na.rm=TRUE) >= 1)*100, digits = 2), #Dark green leafy vegetables 
    OVeg_pc = round(mean(rowSums(across(DQQ7.1:DQQ7.3) == 1, na.rm=TRUE) >= 1)*100, digits = 2), #Other vegetables
    All5b_pc = round(prop.table(table(All_5_b))["1"]*100, digits = 2), #At least one fruit
    AFruit_pc = round(mean(DQQ8 == 1, na.rm = TRUE)*100, digits = 2), #Vitamin A-rich fruits
    CFruit_pc = round(mean(DQQ9 == 1, na.rm = TRUE)*100, digits = 2), #Citrus
    OFruit_pc = round(mean(rowSums(across(DQQ10.1:DQQ10.2) == 1, na.rm=TRUE) >= 1)*100, digits = 2), #Other fruits
    Dairy_pc = round(mean(rowSums(across(c(DQQ14, DQQ15, DQQ25)) == 1, na.rm=TRUE) >= 1)*100, digits = 2), #Dairy
    Milk_pc = round(mean(DQQ25 == 1, na.rm = TRUE)*100, digits = 2), #Milk
    Yog_pc = round(mean(DQQ15 == 1, na.rm = TRUE)*100, digits = 2), #Yogurt
    Cheese_pc = round(mean(DQQ14 == 1, na.rm = TRUE)*100, digits = 2), #Cheese
    Egg_pc = round(mean(DQQ13 == 1, na.rm = TRUE)*100, digits = 2), #Eggs
    Animal_pc = round(mean(rowSums(across(c(DQQ20, DQQ19, DQQ17, DQQ18)) == 1, na.rm=TRUE) >= 1)*100, digits = 2), #Meat, poultry, or fish
    Fish_pc = round(mean(DQQ20 == 1, na.rm = TRUE)*100, digits = 2), #Fish or seafood
    Poultry_pc = round(mean(DQQ19 == 1, na.rm = TRUE)*100, digits = 2), #Poultry
    URmeat_pc = round(mean(rowSums(across(c(DQQ17, DQQ18)) == 1, na.rm=TRUE) >= 1)*100, digits = 2), #Unprocessed red meat
    URmeatr_pc = round(mean(DQQ17 == 1, na.rm = TRUE)*100, digits = 2), #Unprocessed red meat ruminants
    URmeatnr_pc = round(mean(DQQ18 == 1, na.rm = TRUE)*100, digits = 2), #Unprocessed red meat non-ruminants
    PRmeat_pc = round(mean(DQQ16 == 1, na.rm = TRUE)*100, digits = 2), #Processed meats
    FFSalty_pc = round(mean(rowSums(across(c(DQQ22, DQQ23, DQQ24, DQQ29)) == 1, na.rm=TRUE) >= 1)*100, digits = 2), #Salty snacks, instant noodles, or fast food
    Salty_pc = round(mean(rowSums(across(DQQ22:DQQ24) == 1, na.rm=TRUE) >= 1)*100, digits = 2), #Salty or fried snacks
    USalty_pc = round(mean(DQQ22 == 1, na.rm = TRUE)*100, digits = 2), #Packaged ultra-processed salty snacks
    Noodle_pc = round(mean(DQQ23 == 1, na.rm = TRUE)*100, digits = 2), #Instant noodles
    Fastf_pc = round(mean(DQQ29 == 1, na.rm = TRUE)*100, digits = 2), #Fast food
    DFried_pc = round(mean(DQQ24 == 1, na.rm = TRUE)*100, digits = 2), #Deep fried foods
    Sweet_pc = round(mean(rowSums(across(DQQ11:DQQ12) == 1, na.rm=TRUE) >= 1)*100, digits = 2), #Sweet foods
    BSweet_pc = round(mean(DQQ11 == 1, na.rm = TRUE)*100, digits = 2), #Baked / grain-based sweets
    OSweet_pc = round(mean(DQQ12 == 1, na.rm = TRUE)*100, digits = 2), #Other sweets
    Sdrink =  round(mean(rowSums(across(DQQ26:DQQ28) == 1, na.rm=TRUE) >= 1)*100, digits = 2), #Sugar-sweetened beverages
    Softd_pc = round(mean(DQQ28 == 1, na.rm = TRUE)*100, digits = 2), # Sugar-sweetened soft drink consumption
    Tead_pc = round(mean(DQQ26 == 1, na.rm = TRUE)*100, digits = 2), # Sweet tea, coffee, or cocoa
    Fruitd_pc = round(mean(DQQ27 == 1, na.rm = TRUE)*100, digits = 2), #Fruit juice and fruit drinks
  )

write_csv(d2, "DQQ_WebVariables.csv")

## 6. Visualization ----
### 6.1 Data preparation ----
d3 <- d2 %>% 
  column_to_rownames(var="Country") %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column (var = "variables") %>%
  pivot_longer(!variables, names_to = "country", values_to = "values")

### 6.2 ggplot and dplyr for All countries ----
new_levels <- d3$variables %>% unique() %>% rev()
d3 <- d3 %>% mutate(variables = factor(variables, levels = new_levels))

gplot <- d3 %>% ggplot( aes(x=variables, y=values))+
  geom_bar(stat="identity", fill="#69b3a2", width=0.6) +
  coord_flip() +
  #theme_ipsum() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  ylim(0,100) +
  ylab("values") +
  xlab("") +
  facet_wrap(~country, ncol=21)
ggsave(filename = "Web variables.png", width = 35, height = 20, device='png', dpi=800)

### 6.3 Function for plotting a single country ----

d3 %>% 
  filter(country == "Nigeria") %>%
  ggplot(aes(x = variables, y = values)) +
  geom_segment(aes(x=variables, xend=variables, y=0, yend=values), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0,100,10), expand = c(0,1,0.02,0))+
  theme_light() +
  coord_flip() +
  labs(title = paste("DQQ Web variables for", "Nigeria"))+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks = element_line(size = 2)
  )


ctryplot <- function(country_name){
    d3 %>% 
      filter(country == country_name) %>%
      ggplot(aes(x = variables, y = values)) +
      geom_segment(aes(x=variables, xend=variables, y=0, yend=values), color="skyblue") +
      geom_point( color="blue", size=4, alpha=0.6) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0,100,10), expand = c(0,1,0.02,0)) +
      theme_light() +
      coord_flip() +
      labs(title = paste("Web variables for", country_name))+
      theme(
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks = element_line(size = 2)
    )
  }

ctryplot(country_name = "Nigeria")
ggsave(filename = "Nigeria Web DQQ plot.png", width = 7, height = 10, device='png', dpi=700)

### 6.4 Plotting a selection of countries ----
cowplot::plot_grid(
  ctryplot("Benin"),
  ctryplot("China"),
  ctryplot("United States"),
  ctryplot("Nigeria"),
  nrow = 1,
  ncol = 4,
  labels = paste0()
  )
ggsave(filename = "Selection of countries.png", width = 15, height = 10, device='png', dpi=700)

## 7. DQQ Indicator Calculator (https://www.dietquality.org/calculator) ----
### 7.1 Generating csv input file for uploading into website ---- 
d4 <- d %>% group_by(Country) %>%
  summarise(
    Respondent_ID = CaseID,
    Gender_Female_1_Male_0 = Gender,
    Locality_Urban_1_Rural_0 = Urbanicity,
    Age_years = Age,
    "01_Foods_made_from_grains" = DQQ1,
    "02_Whole_grains" = DQQ2,
    "03_White_roots_or_tubers" = DQQ3,
    "04_Pulses" = DQQ4,
    "05_Vitamin_A-rich_orange_vegetables" = DQQ5,
    "06_Dark_green_leafy_vegetables" = ifelse(rowSums(across(DQQ6.1:DQQ6.2) == 1, na.rm = TRUE) > 0, 1, 0),
    "07_Other_vegetables" = ifelse(rowSums(across(DQQ7.1:DQQ7.3) == 1, na.rm = TRUE) > 0, 1, 0), 
    "08_Vitamin_A-rich_fruits" = DQQ8,
    "09_Citrus" = DQQ9,
    "10_Other_fruits" = ifelse(rowSums(across(DQQ10.1:DQQ10.2) == 1, na.rm = TRUE) > 0, 1, 0),
    "11_Baked_or_grain-based_sweets" = DQQ11,
    "12_Other_sweets" = DQQ12,
    "13_Eggs" = DQQ13,
    "14_Cheese" = DQQ14,
    "15_Yogurt" = DQQ15,
    "16_Processed_meats" = DQQ16,
    "17_Unprocessed_red_meat_ruminant" = DQQ17,
    "18_Unprocessed_red_meat_non-ruminant" = DQQ18,
    "19_Poultry" = DQQ19,
    "20_Fish_or_seafood" = DQQ20,
    "21_Nuts_or_seeds" = DQQ21,
    "22_Packaged_ultra-processed_salty_snacks" = DQQ22,
    "23_Instant_noodles" = DQQ23,
    "24_Deep_fried_foods" = DQQ24,
    "25_Milk" = DQQ25,
    "26_Sweet tea_coffee_or_cocoa" = DQQ26,
    "27_Fruit_juice_or_fruit_drinks" = DQQ27,
    "28_Sugar-sweetened_soft_drinks" = DQQ28,
    "29_Fast_food" = DQQ29,
  )

### 7.2 Sample csv for uploading into the website ---- 
d4USA <- d4 %>% filter(Country == "United States")
write.csv(d4USA, row.names = FALSE, "DQQ_USA.csv")

d4Turkey <- d4 %>% filter(Country == "Turkey")
write.csv(d4Turkey, row.names = FALSE, "DQQ_Turkey.csv")

d4Nigeria <- d4 %>% filter(Country == "Nigeria")
write.csv(d4Nigeria, row.names = FALSE, "DQQ_Nigeria.csv")

d4China <- d4 %>% filter(Country == "China")
write.csv(d4China, row.names = FALSE, "DQQ_China.csv")

d4India <- d4 %>% filter(Country == "India")
write.csv(d4India, row.names = FALSE, "DQQ_India.csv")


### 7.3 Checking the indicators generated by web bar chart ----
d5 <- d %>% 
      group_by(Country) %>%
      summarise(
        n = n(),
        "GDR Score"   = mean(GDR, na.rm = TRUE),
        "NCD-Protect" = mean(NCD_P, na.rm = TRUE),
        "NCD-Risk"    = mean(NCD_R, na.rm = TRUE),
        "All-5"       = round(mean(All_5 == 1, na.rm = TRUE)*100, digits = 2),
        "All-5.a"     = round(mean(All_5_a == 1, na.rm = TRUE)*100, digits = 2),
        "All-5.b"     = round(mean(All_5_b == 1, na.rm = TRUE)*100, digits = 2),
        "All-5.c"     = round(mean(All_5_c == 1, na.rm = TRUE)*100, digits = 2),
        "All-5.d"     = round(mean(All_5_d == 1, na.rm = TRUE)*100, digits = 2),
        "MDD-w"       = round(mean(MDD_W == 1, na.rm = TRUE)*100, digits = 2),
        "FGDS"        = round(mean(FGDS, na.rm = TRUE), digits = 2),
        "Zero Veg Fr" = 100-round(mean(zeroVeg == 1, na.rm = TRUE)*100, digits = 2),
        "One Veg Fr"  = round(mean(zeroVeg == 1, na.rm = TRUE)*100, digits = 2),
        "Pulse"       = round(mean(Pulse == 1, na.rm = TRUE)*100, digits = 2),
        "Nuts"        = round(mean(Nuts == 1, na.rm = TRUE)*100, digits = 2),
        "Whole Grain" = round(mean(WholeGrain == 1, na.rm = TRUE)*100, digits = 2),
        "Proc. Meat"  = round(mean(ProMeat == 1, na.rm = TRUE)*100, digits = 2),
        "Salty Fried Snaks"    = round(mean(SaltyFood == 1, na.rm = TRUE)*100, digits = 2),
        "Deep Fried"  = round(mean(DQQ24 == 1, na.rm = TRUE)*100, digits = 2),
        "Sweet Food"  = round(mean(SweetFood == 1, na.rm = TRUE)*100, digits = 2),
        "Soft Drink"  = round(mean(DQQ28 == 1, na.rm = TRUE)*100, digits = 2),
        )

d5USA<- d5 %>% filter(Country == "United States")
write.csv(d5USA, row.names = FALSE, "DQQ_USA_BarChart.csv")

d5Turkey<- d5 %>% filter(Country == "Turkey")
write.csv(d5Turkey, row.names = FALSE, "DQQ_Turkey_BarChart.csv")

d5Nigeria<- d5 %>% filter(Country == "Nigeria")
write.csv(d5Nigeria, row.names = FALSE, "DQQ_Nigeria_BarChart.csv")

d5China<- d5 %>% filter(Country == "China")
write.csv(d5China, row.names = FALSE, "DQQ_China_BarChart.csv")

d5India<- d5 %>% filter(Country == "India")
write.csv(d5India, row.names = FALSE, "DQQ_India_BarChart.csv")



