DQQ Indicators
================

``` r
# DQQ Indicators ----

## 1. Adding the required libraries ----
library(haven)
library(tidyverse)

## 2. Data preparation ----
d = read_sav(file = "Input/DQQ2022/Diet_Quality_032023_INTERNAL.sav")

d <- d[ , c("STRATA", "PSU", "CaseID", "Weight", "FieldDate", "Country", "Gender", 
            "Age", "Education", "IncomeQuintiles", "REG2_GLOBAL", "COUNTRY_ISO3",
            "Urbanicity", "DQQ1", "DQQ2", "DQQ3", "DQQ4", "DQQ5", "DQQ6_1", "DQQ6_2", 
            "DQQ7_1", "DQQ7_2", "DQQ7_3", "DQQ8", "DQQ9", "DQQ10_1", "DQQ10_2", "DQQ11", 
            "DQQ12", "DQQ13", "DQQ13_IND", "DQQ14", "DQQ15", "DQQ16", "DQQ16_IND", "DQQ17", 
            "DQQ17_IND1", "DQQ17_IND2", "DQQ18", "DQQ18_IND1", "DQQ18_IND2", "DQQ19", "DQQ19_IND", 
            "DQQ20", "DQQ20_IND", "DQQ20_ISR1", "DQQ20_ISR2", "DQQ21", "DQQ22", "DQQ23", "DQQ24", 
            "DQQ25", "DQQ26", "DQQ27", "DQQ28", "DQQ29")]

write.csv(d, "Output/CSV/GallupInputData.csv")

##  3. DQQ-based Indicators  ----
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

#### 2.a.   At least one vegetable  ----
d$all5a <- ifelse((rowSums(d[c("DQQ5", "DQQ6_1", "DQQ6_2", "DQQ7_1", "DQQ7_2", "DQQ7_3")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)
#### 2.b.   At least one fruit  ----
d$all5b <- ifelse((rowSums(d[c("DQQ8","DQQ9", "DQQ10_1", "DQQ10_2")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)
#### 2.c. At least one pulse, nut or seed  ----
d$all5c <- ifelse((rowSums(d[c("DQQ4","DQQ21")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)
#### 2.d. d.    At least one animal-source food (ASF)  ----
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
### 21. Dairy ----
d$dairy <- ifelse((rowSums(d[c("DQQ14","DQQ15","DQQ25")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 22. Dark green leafy vegetables ----
d$dveg <- ifelse((rowSums(d[c("DQQ6_1","DQQ6_2")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 23. Animal-based food ----
d$anml <- ifelse((rowSums(d[c("DQQ17","DQQ18", "DQQ19", "DQQ20")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 24. Other fruits ----
d$ofr <- ifelse((rowSums(d[c("DQQ10_1","DQQ10_2")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 25. Other vegetables ----
d$oveg <- ifelse((rowSums(d[c("DQQ7_1","DQQ7_2", "DQQ7_3")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 26. Salty snacks, instant noodles, or fast food (including deep fried) ----
d$snfd <- ifelse((rowSums(d[c("DQQ22","DQQ23", "DQQ24", "DQQ29")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 27. Unprocessed red meat ----
d$umeat <- ifelse((rowSums(d[c("DQQ17","DQQ18")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

write_csv(d, "Output/CSV/dqqMain.csv")

# 4. Subgroup data set ----
## 4.1 data preparation ----
dsub <- d %>%
  group_by(Country, REG2_GLOBAL, COUNTRY_ISO3) %>%
  reframe(n= n())

REG2_GLOBAL <- fct_collapse(as.factor(dsub$REG2_GLOBAL), "EU" = "1", "FSU" = "2", "AS" = "3", "AM" = "4", "MENA" = "5", "SSA" = "6")
dsub$REG2_GLOBAL <- REG2_GLOBAL

attributes(dsub$Country) <- NULL
attributes(dsub$COUNTRY_ISO3) <- NULL

cntryNames <- dsub$Country
cntryISO3 <- dsub$COUNTRY_ISO3
cntryRegion <- dsub$REG2_GLOBAL
cntryIncome <- c("L", "UM", "UM", "UM", "LM", "L", "LM", "L", "LM", "LM", "L", "H", "UM", "UM", "UM", "LM", "UM", "LM", "H", "LM",  
                 "LM", "LM", "H", "UM", "UM", "LM", "LM", "LM", "UM", "L", "L", "UM", "LM", "L", "L", "LM","L", "LM",
                 "L", "LM", "LM", "UM", "LM", "L", "UM", "UM", "L", "L", "LM", "L", "L", "H", "LM", "LM", "L", "LM")

## 4.2 New columns ----
newcols <- data.frame(Country = cntryNames, ISO3 = cntryISO3, Region = cntryRegion, "Income classification" = cntryIncome)
dgroup <- left_join(d, newcols, by= "Country")
write.csv(dgroup, "Output/CSV/dgroup.csv")

## 4.3 Functions for upper/lower confidence (proportional sampling) ----
### 4.3.1 Upper confidence interval ----
upconf <- function(x){
  mean(x == 1, na.rm = TRUE)*100 + 
    qnorm(0.975) * sqrt(((mean(x == 1, na.rm = TRUE)*100)*
                           (100-(mean(x == 1, na.rm = TRUE)*100)))/length(na.omit(x)))
}

### 4.3.2 Lower confidence interval ----
lowconf <- function(x){
  mean(x == 1, na.rm = TRUE)*100 - 
    qnorm(0.975) * sqrt(((mean(x == 1, na.rm = TRUE)*100)*
                           (100-(mean(x == 1, na.rm = TRUE)*100)))/length(na.omit(x)))
}

### 4.4 Seeting labels for 'Urbanicity' and 'Gender' ----
Gender <- fct_collapse(as.factor(dgroup$Gender), "Male" = "1", "Female" = "2")
dgroup$Gender <- Gender

Urbanicity <- fct_collapse(as.factor(dgroup$Urbanicity), "Rural" = "1", "Rural" = "2", "Urban" = "3", "DK" = "4", "Refused" = "5", "Urban" = "6")
dgroup$Urbanicity <- Urbanicity
dgroup <- dgroup %>% filter(Urbanicity != "DK" & Urbanicity != "Refused") # 84 obs. were filtered out


#### 4.5 Indicators long names ----
longNames <- c(all5 = "All-5", 
               all5a = "At least one vegetable",
               all5b = "At least one fruit",
               all5c = "At least one pulse, nut, or seed",
               all5d = "At least one animal-source food",
               all5e = "At least one starchy staple",
               fgds = "Food group diversity score",
               ncdp = "NCD-Protect",
               ncdr = "NCD-Risk",
               gdr = "GDR score",
               DQQ11 = "Baked or grain-based sweets",
               DQQ14 = "Cheese",
               DQQ9 = "Citrus",
               dairy = "Dairy",
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
               DQQ3 = "White roots or tubers",
               DQQ2  = "Whole grains",
               DQQ15  = "Yogurt",
               zvegfr= "Zero vegetable or fruit consumption"
)

dqqNames <- c(DQQ11 = "DQQ11",
               DQQ14 = "DQQ14",
               DQQ9 = "DQQ9",
               dveg = "DQQ6",
               DQQ24 = "DQQ24",
               DQQ13 = "DQQ13",
               DQQ29 = "DQQ29", 
               DQQ20 = "DQQ20",
               DQQ1 = "DQQ1",
               DQQ27 = "DQQ27",
               DQQ23 = "DQQ23",
               DQQ25  = "DQQ25",
               DQQ21 = "DQQ21",
               ofr = "DQQ10",
               DQQ12 = "DQQ12",
               oveg = "DQQ7",
               DQQ22 = "DQQ22",
               DQQ19 = "DQQ19",
               DQQ16 = "DQQ16",
               DQQ4  = "DQQ4",
               DQQ28 = "DQQ28",
               DQQ26  = "DQQ26",
               DQQ18 = "DQQ18",
               DQQ17 = "DQQ17",
               DQQ8 = "DQQ8",
               DQQ5 = "DQQ5",
               DQQ3 = "DQQ3",
               DQQ2  = "DQQ2",
               DQQ15  = "DQQ15"
)

## 4.6 Constructing the 'subgroup' pivot-long version ----
dgroup_pivot <- dgroup %>% 
  
  # 1. Copy and paste 'All' category as a 'Female + Male'
  mutate(Gender = "All") %>%
  bind_rows(., dgroup) %>%
  
  # 2. Making the data frame in a pivot-longer format
  pivot_longer(c(Gender, Urbanicity), values_to = "Subgroup") %>%
  pivot_longer(c(
    all5, all5a, all5b, all5c, all5d, all5e, fgds, ncdp, ncdr, gdr,
    DQQ11, DQQ14, DQQ9, dairy, dveg, DQQ24, DQQ13, DQQ29, DQQ20, DQQ1, DQQ27, DQQ23, mddw,
    anml, DQQ25, DQQ21, ofr, DQQ12, oveg, DQQ22, DQQ19, DQQ16, DQQ4, safd, snfd,
    swtbev,DQQ28, swtfd, DQQ26, umeat, DQQ18, DQQ17, DQQ8, DQQ5, DQQ3, DQQ2, DQQ15, zvegfr
  ), names_to = "Indicators", values_to = "Values") %>%
  
  # 3. Grouping based on desired columns and summary statistics
  group_by("Income classification" = Income.classification, Region, Country, ISO3, Subgroup, Indicators) %>%
  
  reframe(
    "Mean/Prevalence" = if (Indicators == "fgds" || Indicators == "ncdp" || Indicators == "ncdr" || Indicators == "gdr") {
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
    "DQQ Names" = as.character(dqqNames[dgroup_pivot$Indicators]),
    Indicators = as.character(longNames[dgroup_pivot$Indicators])
  ) %>%
  relocate("DQQ Names", .after = Indicators)

write.csv(dgroup_pivot, "Output/CSV/dqq Subgroups.csv", row.names = FALSE)
```
