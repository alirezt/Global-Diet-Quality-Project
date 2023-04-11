# DQQ variables ----

## 1. Adding required libraries ----
library(haven)
library(tidyverse)

## 2. Data preparation ----
### 2.1 setting working directory n----
d = read_sav(file = "Input/DQQ2022/Diet_Quality_032023_INTERNAL.sav")
glimpse(d)
names(d)
write.csv(d, "GallupInputData.csv")

d <- d[ , c("STRATA", "PSU", "CaseID", "Weight", "FieldDate", "Country", "Gender", 
            "Age", "Education", "IncomeQuintiles", "REG2_GLOBAL", "COUNTRY_ISO3",
            "Urbanicity", "DQQ1", "DQQ2", "DQQ3", "DQQ4", "DQQ5", "DQQ6_1", "DQQ6_2", 
            "DQQ7_1", "DQQ7_2", "DQQ7_3", "DQQ8", "DQQ9", "DQQ10_1", "DQQ10_2", "DQQ11", 
            "DQQ12", "DQQ13", "DQQ13_IND", "DQQ14", "DQQ15", "DQQ16", "DQQ16_IND", "DQQ17", 
            "DQQ17_IND1", "DQQ17_IND2", "DQQ18", "DQQ18_IND1", "DQQ18_IND2", "DQQ19", "DQQ19_IND", 
            "DQQ20", "DQQ20_IND", "DQQ20_ISR1", "DQQ20_ISR2", "DQQ21", "DQQ22", "DQQ23", "DQQ24", 
            "DQQ25", "DQQ26", "DQQ27", "DQQ28", "DQQ29")]


##  3. DQQ variables computation  ----
###  1. MDD-W and FGDS  ----
####  1.1 FGDS  ----
d$fgds <- (rowSums(d[c("DQQ1","DQQ2", "DQQ3")] == 1, na.rm=TRUE) > 0) + 
          (rowSums(d[c("DQQ4")] == 1, na.rm=TRUE) > 0) +
          (rowSums(d[c("DQQ21")] == 1, na.rm=TRUE) > 0) +
          (rowSums(d[c("DQQ14","DQQ15", "DQQ25")] == 1, na.rm=TRUE) > 0) +
          (rowSums(d[c("DQQ16","DQQ17", "DQQ18", "DQQ19","DQQ20")] == 1, na.rm=TRUE) > 0) +
          (rowSums(d[c("DQQ13")] == 1, na.rm=TRUE) > 0) +
          (rowSums(d[c("DQQ6_1","DQQ6_2")] == 1, na.rm=TRUE) > 0) +
          (rowSums(d[c("DQQ5","DQQ8")] == 1, na.rm=TRUE) > 0) +
          (rowSums(d[c("DQQ7_1","DQQ7_2", "DQQ7_3")] == 1, na.rm=TRUE) > 0) +
          (rowSums(d[c("DQQ9","DQQ10_1", "DQQ10_2")] == 1, na.rm=TRUE) > 0) 

####  1.2 MDD-W  ----
d$mddw <- ifelse(((rowSums(d[c("DQQ1","DQQ2", "DQQ3")] == 1, na.rm=TRUE) > 0) + 
                     (rowSums(d[c("DQQ4")] == 1, na.rm=TRUE) > 0) +
                     (rowSums(d[c("DQQ21")] == 1, na.rm=TRUE) > 0) +
                     (rowSums(d[c("DQQ14","DQQ15", "DQQ25")] == 1, na.rm=TRUE) > 0) +
                     (rowSums(d[c("DQQ16","DQQ17", "DQQ18", "DQQ19","DQQ20")] == 1, na.rm=TRUE) > 0) +
                     (rowSums(d[c("DQQ13")] == 1, na.rm=TRUE) > 0) +
                     (rowSums(d[c("DQQ6_1","DQQ6_2")] == 1, na.rm=TRUE) > 0) +
                     (rowSums(d[c("DQQ5","DQQ8")] == 1, na.rm=TRUE) > 0) +
                     (rowSums(d[c("DQQ7_1","DQQ7_2", "DQQ7_3")] == 1, na.rm=TRUE) > 0) +
                     (rowSums(d[c("DQQ9","DQQ10_1", "DQQ10_2")] == 1, na.rm=TRUE) > 0)) >= 5 & d$Gender == 2 &  d$Age >= 15 & d$Age <= 49, 1, 
                 
                 ifelse(((rowSums(d[c("DQQ1","DQQ2", "DQQ3")] == 1, na.rm=TRUE) > 0) + 
                           (rowSums(d[c("DQQ4")] == 1, na.rm=TRUE) > 0) +
                           (rowSums(d[c("DQQ21")] == 1, na.rm=TRUE) > 0) +
                           (rowSums(d[c("DQQ14","DQQ15", "DQQ25")] == 1, na.rm=TRUE) > 0) +
                           (rowSums(d[c("DQQ16","DQQ17", "DQQ18", "DQQ19","DQQ20")] == 1, na.rm=TRUE) > 0) +
                           (rowSums(d[c("DQQ13")] == 1, na.rm=TRUE) > 0) +
                           (rowSums(d[c("DQQ6_1","DQQ6_2")] == 1, na.rm=TRUE) > 0) +
                           (rowSums(d[c("DQQ5","DQQ8")] == 1, na.rm=TRUE) > 0) +
                           (rowSums(d[c("DQQ7_1","DQQ7_2", "DQQ7_3")] == 1, na.rm=TRUE) > 0) +
                           (rowSums(d[c("DQQ9","DQQ10_1", "DQQ10_2")] == 1, na.rm=TRUE) > 0)) < 5 & d$Gender == 2 &  d$Age >= 15 & d$Age <= 49, 0, NA))                                                                


### 2. All-5  ----
d$all5 <- ifelse(((rowSums(d[c("DQQ1","DQQ2", "DQQ3")] == 1, na.rm=TRUE) > 0) + 
                    (rowSums(d[c("DQQ5", "DQQ6_1", "DQQ6_2", "DQQ7_1", "DQQ7_2", "DQQ7_3")] == 1, na.rm=TRUE) > 0) +
                    (rowSums(d[c("DQQ8","DQQ9", "DQQ10_1", "DQQ10_2")] == 1, na.rm=TRUE) > 0) +
                    (rowSums(d[c("DQQ4","DQQ21")] == 1, na.rm=TRUE) > 0) +
                    (rowSums(d[c("DQQ13","DQQ14", "DQQ15", "DQQ17", "DQQ18", "DQQ19", "DQQ20", "DQQ25")] == 1, na.rm=TRUE) > 0)) >= 5, 1, 0)

#### 2.a.	At least one vegetable  ----
d$all5a <- ifelse((rowSums(d[c("DQQ5", "DQQ6_1", "DQQ6_2", "DQQ7_1", "DQQ7_2", "DQQ7_3")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)
#### 2.b.	At least one fruit  ----
d$all5b <- ifelse((rowSums(d[c("DQQ8","DQQ9", "DQQ10_1", "DQQ10_2")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)
#### 2.c. At least one pulse, nut or seed  ----
d$all5c <- ifelse((rowSums(d[c("DQQ4","DQQ21")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)
#### 2.d. d.	At least one animal-source food (ASF)  ----
d$all5d <- ifelse((rowSums(d[c("DQQ13","DQQ14", "DQQ15", "DQQ17", "DQQ18", "DQQ19", "DQQ20", "DQQ25")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)
#### 2.e. At least starchy staple  ----
d$all5e <- ifelse((rowSums(d[c("DQQ1","DQQ2", "DQQ3")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 3. NCD-Protect score  ----
d$ncdp <- (rowSums(d[c("DQQ2")] == 1, na.rm=TRUE) > 0) + 
           (rowSums(d[c("DQQ4")] == 1, na.rm=TRUE) > 0) +
           (rowSums(d[c("DQQ21")] == 1, na.rm=TRUE) > 0) +
           (rowSums(d[c("DQQ5")] == 1, na.rm=TRUE) > 0) +
           (rowSums(d[c("DQQ6_1","DQQ6_2")] == 1, na.rm=TRUE) > 0) +
           (rowSums(d[c("DQQ7_1","DQQ7_2", "DQQ7_3")] == 1, na.rm=TRUE) > 0) +
           (rowSums(d[c("DQQ8")] == 1, na.rm=TRUE) > 0)+
           (rowSums(d[c("DQQ9")] == 1, na.rm=TRUE) > 0)+
           (rowSums(d[c("DQQ10_1", "DQQ10_2")] == 1, na.rm=TRUE) > 0)

### 4. NCD-Risk score  ----
d$ncdr <- (rowSums(d[c("DQQ28")] == 1, na.rm=TRUE) > 0) + 
           (rowSums(d[c("DQQ11")] == 1, na.rm=TRUE) > 0) +
           (rowSums(d[c("DQQ12")] == 1, na.rm=TRUE) > 0) +
           (rowSums(d[c("DQQ16")] == 1, na.rm=TRUE) > 0) +
           (rowSums(d[c("DQQ16")] == 1, na.rm=TRUE) > 0) +
           (rowSums(d[c("DQQ17","DQQ18")] == 1, na.rm=TRUE) > 0) +
           (rowSums(d[c("DQQ24")] == 1, na.rm=TRUE) > 0) +
           (rowSums(d[c("DQQ23", "DQQ29")] == 1, na.rm=TRUE) > 0)+
           (rowSums(d[c("DQQ22")] == 1, na.rm=TRUE) > 0)

### 5. GDR score  ----
d$gdr <- (rowSums(d[c("DQQ2")] == 1, na.rm=TRUE) > 0) + 
         (rowSums(d[c("DQQ4")] == 1, na.rm=TRUE) > 0) +
         (rowSums(d[c("DQQ21")] == 1, na.rm=TRUE) > 0) +
         (rowSums(d[c("DQQ5")] == 1, na.rm=TRUE) > 0) +
         (rowSums(d[c("DQQ6_1","DQQ6_2")] == 1, na.rm=TRUE) > 0) +
         (rowSums(d[c("DQQ7_1","DQQ7_2", "DQQ7_3")] == 1, na.rm=TRUE) > 0) +
         (rowSums(d[c("DQQ8")] == 1, na.rm=TRUE) > 0)+
         (rowSums(d[c("DQQ8")] == 1, na.rm=TRUE) > 0)+
         (rowSums(d[c("DQQ9")] == 1, na.rm=TRUE) > 0)+
         (rowSums(d[c("DQQ10_1", "DQQ10_2")] == 1, na.rm=TRUE) > 0)-
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
d$zvegfr <- ifelse((rowSums(d[c("DQQ5","DQQ6_1","DQQ6_2", "DQQ7_1", "DQQ7_2", "DQQ7_3", "DQQ8", "DQQ9", "DQQ10_1", "DQQ10_2")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 8. ASF consumption  ----
d$asf <- ifelse((rowSums(d[c("DQQ13","DQQ14","DQQ15", "DQQ17", "DQQ18", "DQQ19", "DQQ20", "DQQ25")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 9. Sweet beverage consumption   ----
d$swtbev <- ifelse((rowSums(d[c("DQQ26","DQQ27","DQQ28")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

#### 9.a. Sugar-sweetened soft drink consumption  ----
d$sugdr <- ifelse((rowSums(d[c("DQQ28")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 10. Sweet foods consumption   ----
d$swtfd <- ifelse((rowSums(d[c("DQQ11", "DQQ12")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 11. Salty or fried snack consumption   ----
d$safd <- ifelse((rowSums(d[c("DQQ22","DQQ23","DQQ24")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 12. Whole grain consumption   ----
d$wgrn <- ifelse((rowSums(d[c("DQQ2")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 13. Pulse consumption   ----
d$pls <- ifelse((rowSums(d[c("DQQ4")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 14. Nuts and seeds consumption   ----
d$nut <- ifelse((rowSums(d[c("DQQ21")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 15. Processed meat consumption   ----
d$pmeat <- ifelse((rowSums(d[c("DQQ16")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 16. WHO-FV score   ----
d$whofv <- ifelse(((rowSums(d[c("DQQ5")] == 1, na.rm=TRUE) > 0) + 
                    (rowSums(d[c("DQQ6_1","DQQ6_2")] == 1, na.rm=TRUE) > 0) +
                    (rowSums(d[c("DQQ7_1","DQQ7_2", "DQQ7_3")] == 1, na.rm=TRUE) > 0) +
                    (rowSums(d[c("DQQ8")] == 1, na.rm=TRUE) > 0)+
                    (rowSums(d[c("DQQ9")] == 1, na.rm=TRUE) > 0)+
                    (rowSums(d[c("DQQ10_1", "DQQ10_2")] == 1, na.rm=TRUE) > 0)) >= 3, 1, 0)

### 17. WHO-Fiber score  ----
d$whofib <- ifelse(((rowSums(d[c("DQQ5")] == 1, na.rm=TRUE) > 0) + 
                       (rowSums(d[c("DQQ6_1","DQQ6_2")] == 1, na.rm=TRUE) > 0) +
                       (rowSums(d[c("DQQ7_1","DQQ7_2", "DQQ7_3")] == 1, na.rm=TRUE) > 0) +
                       (rowSums(d[c("DQQ8")] == 1, na.rm=TRUE) > 0)+
                       (rowSums(d[c("DQQ9")] == 1, na.rm=TRUE) > 0)+
                       (rowSums(d[c("DQQ4")] == 1, na.rm=TRUE) > 0)+
                       (rowSums(d[c("DQQ4")] == 1, na.rm=TRUE) > 0)+
                       (rowSums(d[c("DQQ21")] == 1, na.rm=TRUE) > 0)+
                       (rowSums(d[c("DQQ2")] == 1, na.rm=TRUE) > 0)+
                       (rowSums(d[c("DQQ10_1", "DQQ10_2")] == 1, na.rm=TRUE) > 0)) >= 4, 1, 0)

### 18. WHO-Sugar score  ----
d$whosug <- ifelse(((rowSums(d[c("DQQ28")] == 1, na.rm=TRUE) > 0) +
                       (rowSums(d[c("DQQ28")] == 1, na.rm=TRUE) > 0) +
                       (rowSums(d[c("DQQ27")] == 1, na.rm=TRUE) > 0) +
                       (rowSums(d[c("DQQ26")] == 1, na.rm=TRUE) > 0) +
                       (rowSums(d[c("DQQ11")] == 1, na.rm=TRUE) > 0)+
                       (rowSums(d[c("DQQ12")] == 1, na.rm=TRUE) > 0)) >= 2, 1, 0)

### 19. WHO-Saturated fat score  ----
d$whosfat <- ifelse(((rowSums(d[c("DQQ12")] == 1, na.rm=TRUE) > 0) +
                         (rowSums(d[c("DQQ14", "DQQ15")] == 1, na.rm=TRUE) > 0) +
                         (rowSums(d[c("DQQ25")] == 1, na.rm=TRUE) > 0) +
                         (rowSums(d[c("DQQ16")] == 1, na.rm=TRUE) > 0) +
                         (rowSums(d[c("DQQ17", "DQQ18")] == 1, na.rm=TRUE) > 0)-
                         (rowSums(d[c("DQQ20")] == 1, na.rm=TRUE) > 0)-
                         (rowSums(d[c("DQQ19")] == 1, na.rm=TRUE) > 0)+
                         (rowSums(d[c("DQQ29")] == 1, na.rm=TRUE) > 0)) >= 2, 1, 0)

### 20. Percent consuming each food group   ----
#### 20.a. at least one vegetable or fruit   ----
d$vegfr <- ifelse((rowSums(d[c("DQQ5","DQQ6_1","DQQ6_2", "DQQ7_1", "DQQ7_2", "DQQ7_3", "DQQ8", "DQQ9", "DQQ10_1","DQQ10_2")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

#### 20.b. ultra-processed salty snacks, instant noodles, or fast food  ----
d$snf <- ifelse((rowSums(d[c("DQQ22","DQQ23","DQQ29")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### Some complementary indicators ----
#### 21. Dairy ----
d$dairy <- ifelse((rowSums(d[c("DQQ14","DQQ15","DQQ25")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

#### 22. Dark green leafy vegetables ----
d$dveg <- ifelse((rowSums(d[c("DQQ6_1","DQQ6_2")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

#### 23. Animal-based food ----
d$anml <- ifelse((rowSums(d[c("DQQ17","DQQ18", "DQQ19", "DQQ20")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

#### 24. Other fruits ----
d$ofr <- ifelse((rowSums(d[c("DQQ10_1","DQQ10_2")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

#### 25. Other vegetables ----
d$oveg <- ifelse((rowSums(d[c("DQQ7_1","DQQ7_2", "DQQ7_3")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

#### 26. Salty snacks, instant noodles, or fast food (including deep fried) ----
d$snfd <- ifelse((rowSums(d[c("DQQ22","DQQ23", "DQQ24", "DQQ29")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

#### 27. Unprocessed red meat ----
d$umeat <- ifelse((rowSums(d[c("DQQ17","DQQ18")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)


## 4. CSV Export ----
write_csv(d, "CSV/dqqMain.csv")


## 5.Countries web variables ----
# Indicators generated in reverse bar plots in https://www.dietquality.org/countries

dpc <- d %>% 
  group_by(Country) %>%
  summarise(
    mddw_pc  = round(mean(mddw == 1, na.rm = TRUE)*100, digits = 2), #MDD-W
    fgds_avg = round(mean(fgds, na.rm = TRUE), digits = 2), #Food group diversity score
    all5_pc  = round(prop.table(table(all5))["1"]*100, digits = 2), #All-5
    all5e_pc = round(prop.table(table(all5e))["1"]*100, digits = 2), #At least one starchy staple food
    all5a_pc = round(prop.table(table(all5a))["1"]*100, digits = 2), #At least one vegetable
    all5b_pc = round(prop.table(table(all5b))["1"]*100, digits = 2), #At least one fruit
    all5c_pc = round(prop.table(table(all5c))["1"]*100, digits = 2), #At least one pulse, nut, or seed
    all5d_pc = round(prop.table(table(all5d))["1"]*100, digits = 2), #At least one animal-source food
    zvegfr_pc= round(prop.table(table(zvegfr))["1"]*100, digits = 2), #Zero vegetable or fruit consumption
    sugdr_pc = round(prop.table(table(DQQ28))["1"]*100, digits = 2), #Sugar-sweetened soft drink consumption
    all5e_pc = round(prop.table(table(all5e))["1"]*100, digits = 2), #At least one starchy staple food
    wgrn_pc  = round(prop.table(table(DQQ2))["1"]*100, digits = 2), #Whole grains
    grn_pc   = round(prop.table(table(DQQ1))["1"]*100, digits = 2), #Foods made from grains
    root_pc  = round(prop.table(table(DQQ3))["1"]*100, digits = 2), #White roots or tubers
    all5c_pc = round(prop.table(table(all5c))["1"]*100, digits = 2), #At least one pulse, nut, or seed
    pls_pc   = round(prop.table(table(DQQ4))["1"]*100, digits = 2), #Pulses
    nut_pc   = round(prop.table(table(DQQ21))["1"]*100, digits = 2), #Nuts or seeds
    all5a_pc = round(prop.table(table(all5a))["1"]*100, digits = 2), #At least one vegetable
    aveg_pc  = round(mean(DQQ5 == 1, na.rm = TRUE)*100, digits = 2), #Vitamin A-rich orange vegetables
    dveg_pc  = round(mean(rowSums(across(DQQ6_1:DQQ6_2) == 1, na.rm=TRUE) >= 1)*100, digits = 2), #Dark green leafy vegetables 
    oveg_pc  = round(mean(rowSums(across(DQQ7_1:DQQ7_3) == 1, na.rm=TRUE) >= 1)*100, digits = 2), #Other vegetables
    all5b_pc = round(prop.table(table(all5b))["1"]*100, digits = 2), #At least one fruit
    aFr_pc   = round(mean(DQQ8 == 1, na.rm = TRUE)*100, digits = 2), #Vitamin A-rich fruits
    ctr_pc   = round(mean(DQQ9 == 1, na.rm = TRUE)*100, digits = 2), #Citrus
    ofr_pc   = round(mean(rowSums(across(DQQ10_1:DQQ10_2) == 1, na.rm=TRUE) >= 1)*100, digits = 2), #Other fruits
    dairy_pc = round(mean(rowSums(across(c(DQQ14, DQQ15, DQQ25)) == 1, na.rm=TRUE) >= 1)*100, digits = 2), #Dairy
    milk_pc  = round(mean(DQQ25 == 1, na.rm = TRUE)*100, digits = 2), #Milk
    yog_pc   = round(mean(DQQ15 == 1, na.rm = TRUE)*100, digits = 2), #Yogurt
    chse_pc  = round(mean(DQQ14 == 1, na.rm = TRUE)*100, digits = 2), #Cheese
    egg_pc   = round(mean(DQQ13 == 1, na.rm = TRUE)*100, digits = 2), #Eggs
    animal_pc= round(mean(rowSums(across(c(DQQ20, DQQ19, DQQ17, DQQ18)) == 1, na.rm=TRUE) >= 1)*100, digits = 2), #Meat, poultry, or fish
    fish_pc  = round(mean(DQQ20 == 1, na.rm = TRUE)*100, digits = 2), #Fish or seafood
    pltry_pc = round(mean(DQQ19 == 1, na.rm = TRUE)*100, digits = 2), #Poultry
    umeat_pc = round(mean(rowSums(across(c(DQQ17, DQQ18)) == 1, na.rm=TRUE) >= 1)*100, digits = 2), #Unprocessed red meat
    umeatr_pc= round(mean(DQQ17 == 1, na.rm = TRUE)*100, digits = 2), #Unprocessed red meat ruminants
    umeatnr_pc = round(mean(DQQ18 == 1, na.rm = TRUE)*100, digits = 2), #Unprocessed red meat non-ruminants
    pmeat_pc = round(mean(DQQ16 == 1, na.rm = TRUE)*100, digits = 2), #Processed meats
    snfd_pc  = round(mean(rowSums(across(c(DQQ22, DQQ23, DQQ24, DQQ29)) == 1, na.rm=TRUE) >= 1)*100, digits = 2), #Salty snacks, instant noodles, or fast food
    snf_pc   = round(mean(rowSums(across(DQQ22:DQQ24) == 1, na.rm=TRUE) >= 1)*100, digits = 2), #Salty or fried snacks
    ssnk_pc  = round(mean(DQQ22 == 1, na.rm = TRUE)*100, digits = 2), #Packaged ultra-processed salty snacks
    nodl_pc  = round(mean(DQQ23 == 1, na.rm = TRUE)*100, digits = 2), #Instant noodles
    ffd_pc   = round(mean(DQQ29 == 1, na.rm = TRUE)*100, digits = 2), #Fast food
    dfd_pc   = round(mean(DQQ24 == 1, na.rm = TRUE)*100, digits = 2), #Deep fried foods
    swtfd_pc = round(mean(rowSums(across(DQQ11:DQQ12) == 1, na.rm=TRUE) >= 1)*100, digits = 2), #Sweet foods
    swtgrn_pc= round(mean(DQQ11 == 1, na.rm = TRUE)*100, digits = 2), #Baked / grain-based sweets
    oswt_pc  = round(mean(DQQ12 == 1, na.rm = TRUE)*100, digits = 2), #Other sweets
    swtbev_pc=  round(mean(rowSums(across(DQQ26:DQQ28) == 1, na.rm=TRUE) >= 1)*100, digits = 2), #Sugar-sweetened beverages
    softdr_pc= round(mean(DQQ28 == 1, na.rm = TRUE)*100, digits = 2), # Sugar-sweetened soft drink consumption
    tcc_pc   = round(mean(DQQ26 == 1, na.rm = TRUE)*100, digits = 2), # Sweet tea, coffee, or cocoa
    frdr_pc  = round(mean(DQQ27 == 1, na.rm = TRUE)*100, digits = 2), #Fruit juice and fruit drinks
  )

write_csv(dpc, "CSV/Country WebDQQ.csv")

## 6. Visualization for Country WebDQQ----
### 6.1 Pivot-longer version of 'dpc' data frame ----
dpcpivot <- dpc %>% 
  column_to_rownames(var="Country") %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column (var = "variables") %>%
  pivot_longer(!variables, names_to = "country", values_to = "values")

### 6.2 All-countries graph ----
new_levels <- dpcpivot$variables %>% unique() %>% rev()
dpcpivot <- dpcpivot %>% mutate(variables = factor(variables, levels = new_levels))

allgplot <- dpcpivot %>% ggplot( aes(x=variables, y=values))+
  geom_bar(stat="identity", fill="#69b3a2", width=0.6) +
  coord_flip() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  ylim(0,100) +
  ylab("values") +
  xlab("") +
  facet_wrap(~country, ncol=21)

ggsave(filename = "Graphs/All countries WebDQQ.png", width = 35, height = 20, device='png', dpi=800)

### 6.3 Individual-country graphs ----
# make a function to accept 'country name' as an input for graphing
ctrygplot <- function(country_name){
  dpcpivot %>% 
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

ctrygplot(country_name = "Nigeria")
ggsave(filename = "Graphs/Nigeria WebDQQ.png", width = 7, height = 10, device='png', dpi=700)

### 6.4 Graph for a selection of countries ----
cowplot::plot_grid(
  ctrygplot("Benin"),
  ctrygplot("China"),
  ctrygplot("United States"),
  ctrygplot("Nigeria"),
  nrow = 1,
  ncol = 4,
  labels = paste0()
  )
ggsave(filename = "Graphs/Selected countries WebDQQ.png", width = 15, height = 10, device='png', dpi=700)

## 7. DQQ Indicator Calculator (https://www.dietquality.org/calculator) ----
### 7.1 Generating csv input file for uploading into website ---- 
dpivot <- d %>% group_by(Country) %>%
  reframe(
    "Respondent_ID" = CaseID,
    "Gender_Female_1_Male_0" = Gender,
    "Locality_Urban_1_Rural_0" = Urbanicity,
    "Age_years" = Age,
    "01_Foods_made_from_grains" = DQQ1,
    "02_Whole_grains" = DQQ2,
    "03_White_roots_or_tubers" = DQQ3,
    "04_Pulses" = DQQ4,
    "05_Vitamin_A-rich_orange_vegetables" = DQQ5,
    "06_Dark_green_leafy_vegetables" = ifelse(rowSums(across(DQQ6_1:DQQ6_2) == 1, na.rm = TRUE) > 0, 1, 0),
    "07_Other_vegetables" = ifelse(rowSums(across(DQQ7_1:DQQ7_3) == 1, na.rm = TRUE) > 0, 1, 0), 
    "08_Vitamin_A-rich_fruits" = DQQ8,
    "09_Citrus" = DQQ9,
    "10_Other_fruits" = ifelse(rowSums(across(DQQ10_1:DQQ10_2) == 1, na.rm = TRUE) > 0, 1, 0),
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

### 7.2 Sample csv files for uploading into the website ---- 
dqqUSA <- dpivot %>% filter(Country == "United States")
write.csv(dqqUSA, row.names = FALSE, "CSV/DQQ_USA.csv")

dqqTurkey <- dpivot %>% filter(Country == "Turkey")
write.csv(dqqTurkey, row.names = FALSE, "CSV/DQQ_Turkey.csv")

dqqNigeria <- dpivot %>% filter(Country == "Nigeria")
write.csv(dqqNigeria, row.names = FALSE, "CSV/DQQ_Nigeria.csv")

dqqChina <- dpivot %>% filter(Country == "China")
write.csv(dqqChina, row.names = FALSE, "CSV/DQQ_China.csv")

dqqIndia <- dpivot %>% filter(Country == "India")
write.csv(dqqIndia, row.names = FALSE, "CSV/DQQ_India.csv")


### 7.3 Checking the indicators generated by web bar chart ----
dpivotR <- d %>% 
  group_by(Country) %>%
  reframe(
    n = n(),
    "GDR Score"   = mean(gdr, na.rm = TRUE),
    "NCD-Protect" = mean(ncdp, na.rm = TRUE),
    "NCD-Risk"    = mean(ncdr, na.rm = TRUE),
    "All-5"       = round(mean(all5 == 1, na.rm = TRUE)*100, digits = 2),
    "All-5.a"     = round(mean(all5a == 1, na.rm = TRUE)*100, digits = 2),
    "All-5.b"     = round(mean(all5b == 1, na.rm = TRUE)*100, digits = 2),
    "All-5.c"     = round(mean(all5c == 1, na.rm = TRUE)*100, digits = 2),
    "All-5.d"     = round(mean(all5d == 1, na.rm = TRUE)*100, digits = 2),
    "MDD-w"       = round(mean(mddw == 1, na.rm = TRUE)*100, digits = 2),
    "FGDS"        = round(mean(fgds, na.rm = TRUE), digits = 2),
    "Zero Veg Fr" = 100-round(mean(zvegfr == 1, na.rm = TRUE)*100, digits = 2),
    "One Veg Fr"  = round(mean(zvegfr == 1, na.rm = TRUE)*100, digits = 2),
    "Pulse"       = round(mean(pls == 1, na.rm = TRUE)*100, digits = 2),
    "Nuts"        = round(mean(nut == 1, na.rm = TRUE)*100, digits = 2),
    "Whole Grain" = round(mean(wgrn == 1, na.rm = TRUE)*100, digits = 2),
    "Proc. Meat"  = round(mean(pmeat == 1, na.rm = TRUE)*100, digits = 2),
    "Salty Fried Snaks" = round(mean(safd == 1, na.rm = TRUE)*100, digits = 2),
    "Deep Fried"  = round(mean(DQQ24 == 1, na.rm = TRUE)*100, digits = 2),
    "Sweet Food"  = round(mean(swtfd == 1, na.rm = TRUE)*100, digits = 2),
    "Soft Drink"  = round(mean(DQQ28 == 1, na.rm = TRUE)*100, digits = 2),
  )

dqqRUSA<- dpivotR %>% filter(Country == "United States")
write.csv(dqqRUSA, row.names = FALSE, "CSV/DQQ USA BarChart.csv")

dqqRTurkey<- dpivotR %>% filter(Country == "Turkey")
write.csv(dqqRTurkey, row.names = FALSE, "DQQ Turkey BarChart.csv")

dqqRNigeria<- dpivotR %>% filter(Country == "Nigeria")
write.csv(dqqRNigeria, row.names = FALSE, "CSV/DQQ Nigeria BarChart.csv")

dqqRChina<- dpivotR %>% filter(Country == "China")
write.csv(dqqRChina, row.names = FALSE, "CSV/DQQ China BarChart.csv")

dqqRIndia<- dpivotR %>% filter(Country == "India")
write.csv(dqqRIndia, row.names = FALSE, "CSV/DQQ India BarChart.csv")

# 8. Subgroup data set ----
## 8.1 Some preparation ----
attr(d$REG2_GLOBAL, "labels")
attributes(d$REG2_GLOBAL)

dsub <- d %>%
  group_by(Country, REG2_GLOBAL, COUNTRY_ISO3) %>%
  reframe(n= n())

REG2_GLOBAL <- fct_collapse(as.factor(dsub$REG2_GLOBAL), "EU" = "1", "FSU" = "2", "AS" = "3", "AM" = "4", "MENA" = "5", "SSA" = "6")
dsub$REG2_GLOBAL <- REG2_GLOBAL

attributes(dsub$Country) <- NULL
attributes(dsub$COUNTRY_ISO3) <- NULL
attributes(dsub$REG2_GLOBAL) <- NULL


cntryNames <- dsub$Country
cntryISO3 <- dsub$COUNTRY_ISO3
cntryRegion <- dsub$REG2_GLOBAL
cntryIncome <- c("L", "UM", "UM", "UM", "LM", "L", "LM", "L", "LM", "LM", "L", "H", "UM", "UM", "UM", "LM", "UM", "LM", "H", "LM",  
                 "LM", "LM", "H", "UM", "UM", "LM", "LM", "LM", "UM", "L", "L", "UM", "LM", "L", "L", "LM","L", "LM",
                 "L", "LM", "LM", "UM", "LM", "L", "UM", "UM", "L", "L", "LM", "L", "L", "H", "LM", "LM", "L", "LM")

## 8.2 Adding the new columns to the 'dqqMain' data frame ----
newcols <- data.frame(Country = cntryNames, ISO3 = cntryISO3, Region = cntryRegion, "Income classification" = cntryIncome)
dgroup <- left_join(d, newcols, by= "Country")
write.csv(dgroup, "Output/CSV/dgroup.csv")

## 8.3 Functions for upper/lower confidence in proportional sampling ----
### 8.3.1 Upper confidence interval ----
upconf <- function(x){
  mean(x == 1, na.rm = TRUE)*100 + 
    qnorm(0.975) * sqrt(((mean(x == 1, na.rm = TRUE)*100)*
                           (100-(mean(x == 1, na.rm = TRUE)*100)))/length(na.omit(x)))
}

### 8.3.2 Lower confidence interval ----
lowconf <- function(x){
  mean(x == 1, na.rm = TRUE)*100 - 
    qnorm(0.975) * sqrt(((mean(x == 1, na.rm = TRUE)*100)*
                           (100-(mean(x == 1, na.rm = TRUE)*100)))/length(na.omit(x)))
}

### 8.4 Labels and names of the Urbanicity and Gender ----
Gender <- fct_collapse(as.factor(dgroup$Gender), "Male" = "1", "Female" = "2")
dgroup$Gender <- Gender

Urbanicity <- fct_collapse(as.factor(dgroup$Urbanicity), "Rural" = "1", "Rural" = "2", "Urban" = "3", "DK" = "4", "Refused" = "5", "Urban" = "6")
dgroup$Urbanicity <- Urbanicity
dgroup <- dgroup %>% filter(Urbanicity != "DK" & Urbanicity != "Refused")


#### 8.5 Indicators long names ----
longNames <- c(all5 = "All-5", 
               all5a = "At least one vegetable",
               all5b = "At least one fruit",
               all5c = "At least one pulse, nut or seed",
               all5d = "At least one animal-source food",
               all5e = "At least starchy staple",
               fgds = "Food Group Diversity Score",
               ncdp = "NCD-Protect",
               ncdr = "NCD-Risk",
               gdr = "GDR score",
               DQQ11 = "Baked or grain-based sweets",
               DQQ14 = "Cheese",
               DQQ9 = "Citrus",
               dveg = "Dark green leafy vegetables",
               DQQ24 = "Deep fried foods",
               DQQ13 = "Eggs",
               DQQ29 = "Fast food", 
               DQQ20 = "Fish or seafood",
               DQQ1 = "Foods made from grains",
               DQQ27 = "Fruit juice and fruit drinks",
               DQQ23 = "Instant noodles",
               mddw = "MDD-W",
               anml = "Meat, poultry, or fish",
               DQQ25  = "Milk",
               DQQ21 = "Nuts or seeds",
               ofr = "Other fruits",
               DQQ12 = "Other sweets",
               oveg = "Other vegetables",
               DQQ22 = "Packaged ultra-processed salty snacks",
               DQQ19 = "Poultry",
               DQQ16 = "Processed meats",
               DQQ4  = "Pulses",
               safd = "Salty or fried snacks",
               snfd = "Salty snacks, instant noodles, or fast food",
               swtbev = "Sugar-sweetened beverages",
               DQQ28 = "Sugar-sweetened soft drink consumption",
               swtfd = "Sweet foods",
               DQQ26  = "Sweet tea, coffee, or cocoa",
               umeat = "Unprocessed red meat",
               DQQ18 = "Unprocessed red meat (non-ruminants)",
               DQQ17 = "Unprocessed red meat (ruminants)",
               DQQ8 = "Vitamin A-rich fruits",
               DQQ5 = "Vitamin A-rich orange vegetables",
               DDQ3 = "White roots or tubers",
               DQQ2  = "Whole grains",
               DQQ15  = "Yogurt",
               zvegfr= "Zero vegetable or fruit consumption"
)

## 8.6 Constructing the 'subgroup' pivot-long version ----
dgroup_pivot <- dgroup %>% 
  
  # 1. Copy and paste 'All' category as a 'Female + Male'
  mutate(Gender = "All") %>%
  bind_rows(., dgroup) %>%
  
  # 2. Making the data frame in a pivot-longer format
  pivot_longer(c(Gender, Urbanicity), values_to = "Subgroup") %>%
  pivot_longer(c(
    all5, all5a, all5b, all5c, all5d, all5e, fgds, ncdp, ncdr, gdr,
    DQQ11, DQQ14, DQQ9, dveg, DQQ24, DQQ13, DQQ29, DQQ20, DQQ1, DQQ27, DQQ3, mddw,
    anml, DQQ25, DQQ21, ofr, DQQ12, oveg, DQQ22, DQQ19, DQQ16, DQQ4, safd, snfd,
    swtbev,DQQ28, swtfd, DQQ26, umeat, DQQ18, DQQ17, DQQ8, DQQ5, DQQ3, DQQ2, DQQ15, zvegfr
  ), names_to = "Indicators", values_to = "Values") %>%
  
  # 3. Grouping based on desired columns and summary statistics
  group_by(Income.classification, Region, Country, ISO3, Subgroup, Indicators) %>%
  
  reframe(
    Prevalence = if (Indicators == "fgds" || Indicators == "ncdp" || Indicators == "ncdr" || Indicators == "gdr") {
      round(mean(Values, na.rm = TRUE), digits = 2) 
    }
    else {
      round(mean(Values == 1, na.rm = TRUE)*100, digits = 2)
    },
    
    "Lower confidence interval" = if (Indicators == "fgds" || Indicators == "ncdp" || Indicators == "ncdr" || Indicators == "gdr") {
      round(mean(Values, na.rm = TRUE) - qnorm(0.975)*sd(Values, na.rm = TRUE)/sqrt(length(na.omit(Values))), digits = 2)
    } 
    else {
      round(lowconf(Values), digits = 2)
    }
    ,
    "Upper confidence interval" = if (Indicators == "fgds" || Indicators == "ncdp" || Indicators == "ncdr" || Indicators == "gdr") {
      round(mean(Values, na.rm = TRUE) + qnorm(0.975)*sd(Values, na.rm = TRUE)/sqrt(length(na.omit(Values))), digits = 2)
    } 
    else {
      round(upconf(Values), digits = 2)  
    }
  )
  
  # 4. replacing long names format
dgroup_pivot <- dgroup_pivot %>%
  mutate(
    Indicators = as.character(longNames[dgroup_pivot$Indicators])
  )

write.csv(dgroup_pivot, "Output/CSV/dqq Subgroups.csv", row.names = FALSE)

























