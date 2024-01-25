# 1. Loading required libraries ----
library(haven)
library(expss)
library(tidyverse)
library(foreign)
library(survey)
library(plotrix)
library(labelled)
library(ggiraph)
library(patchwork)
library(gapminder)

# 2. Data preparation ----
d= read.spss("Data/Input/Sav/2022/Diet_Quality_032023_INTERNAL.sav", use.value.labels=F, to.data.frame=TRUE, stringsAsFactors=FALSE)

d <- d[ , c("EMP_2010", "EMP_WORK_HOURS", "WP1223", "WP132", "WP1325", "WP16", "WP18", "WP19472", "WP2319", "WP93", "WP9811", 
            "STRATA", "FieldDate", "YEAR_WAVE", "PSU", "CaseID", "WPID", "Weight", "FieldDate", "Country", "Gender", 
            "Age", "Education", "IncomeQuintiles", "REG2_GLOBAL", "COUNTRY_ISO3",
            "Urbanicity", "WP7572", "DEGURBA_2021_F2F", "DEGURBA_2021_PHONE", "DEGURBA_2022", 
            "DQQ1", "DQQ2", "DQQ3", "DQQ4", "DQQ5", "DQQ6_1", "DQQ6_2", 
            "DQQ7_1", "DQQ7_2", "DQQ7_3", "DQQ8", "DQQ9", "DQQ10_1", "DQQ10_2", "DQQ10_3", "DQQ11", 
            "DQQ12", "DQQ13", "DQQ13_IND", "DQQ14", "DQQ15", "DQQ16", "DQQ16_IND", "DQQ17", 
            "DQQ17_IND1", "DQQ17_IND2", "DQQ18", "DQQ18_IND1", "DQQ18_IND2", "DQQ19", "DQQ19_IND", 
            "DQQ20", "DQQ20_IND", "DQQ20_ISR1", "DQQ20_ISR2", "DQQ21", "DQQ22", "DQQ23", "DQQ24", 
            "DQQ25", "DQQ26", "DQQ27", "DQQ28", "DQQ29")]

## 2.1 Correcting date format ----
# Converting spss date format to 4-digit year format
d$FieldDate <- format(as.Date(d$FieldDate/86400, origin = "1582-10-14"), "%Y")


## 2.2 Replacing India and Israel DQQ ----
# Adding DQQ for India and Israel to main global DQQ 
d$DQQ13 <- ifelse(is.na(d$DQQ13_IND), d$DQQ13, d$DQQ13_IND)
d$DQQ16 <- ifelse(is.na(d$DQQ16_IND), d$DQQ16, d$DQQ16_IND)
d$DQQ17 <- ifelse(is.na(d$DQQ17_IND1), d$DQQ17, d$DQQ17_IND1)
d$DQQ17 <- ifelse(is.na(d$DQQ17_IND2), d$DQQ17, d$DQQ17_IND2)
d$DQQ18 <- ifelse(is.na(d$DQQ18_IND1), d$DQQ18, d$DQQ18_IND1)
d$DQQ18 <- ifelse(is.na(d$DQQ18_IND2), d$DQQ18, d$DQQ18_IND2)
d$DQQ19 <- ifelse(is.na(d$DQQ19_IND), d$DQQ19, d$DQQ19_IND)
d$DQQ20 <- ifelse(is.na(d$DQQ20_IND), d$DQQ20, d$DQQ20_IND)
d$DQQ20 <- ifelse(is.na(d$DQQ20_ISR1), d$DQQ20, d$DQQ20_ISR1)
d$DQQ20 <- ifelse(is.na(d$DQQ20_ISR2), d$DQQ20, d$DQQ20_ISR2)

# Replacing DK and Refused answers with NA
d <- d %>%
  mutate(across(.cols = starts_with("DQQ"), .fns = ~ replace(.x, .x == 9, NA)),
         across(.cols = starts_with("DQQ"), .fns = ~ replace(.x, .x == 8, 0)))

# 3. DQQ-based Indicators  ----
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
  (rowSums(d[c("DQQ9","DQQ10_1", "DQQ10_2", "DQQ10_3")] == 1, na.rm=TRUE) > 0) 

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
                    (rowSums(d[c("DQQ9","DQQ10_1", "DQQ10_2", "DQQ10_3")] == 1, na.rm=TRUE) > 0)) >= 5 & d$Gender == 2 &  d$Age >= 15 & d$Age <= 49, 1, 
                 
                 ifelse(((rowSums(d[c("DQQ1","DQQ2", "DQQ3")] == 1, na.rm=TRUE) > 0) + 
                           (rowSums(d[c("DQQ4")] == 1, na.rm=TRUE) > 0) +
                           (rowSums(d[c("DQQ21")] == 1, na.rm=TRUE) > 0) +
                           (rowSums(d[c("DQQ14","DQQ15", "DQQ25")] == 1, na.rm=TRUE) > 0) +
                           (rowSums(d[c("DQQ16","DQQ17", "DQQ18", "DQQ19","DQQ20")] == 1, na.rm=TRUE) > 0) +
                           (rowSums(d[c("DQQ13")] == 1, na.rm=TRUE) > 0) +
                           (rowSums(d[c("DQQ6_1","DQQ6_2")] == 1, na.rm=TRUE) > 0) +
                           (rowSums(d[c("DQQ5","DQQ8")] == 1, na.rm=TRUE) > 0) +
                           (rowSums(d[c("DQQ7_1","DQQ7_2", "DQQ7_3")] == 1, na.rm=TRUE) > 0) +
                           (rowSums(d[c("DQQ9","DQQ10_1", "DQQ10_2", "DQQ10_3")] == 1, na.rm=TRUE) > 0)) < 5 & d$Gender == 2 &  d$Age >= 15 & d$Age <= 49, 0, NA))                                                                


### 2. All-5  ----
d$all5 <- ifelse(((rowSums(d[c("DQQ1","DQQ2", "DQQ3")] == 1, na.rm=TRUE) > 0) + 
                    (rowSums(d[c("DQQ5", "DQQ6_1", "DQQ6_2", "DQQ7_1", "DQQ7_2", "DQQ7_3")] == 1, na.rm=TRUE) > 0) +
                    (rowSums(d[c("DQQ8","DQQ9", "DQQ10_1", "DQQ10_2", "DQQ10_3")] == 1, na.rm=TRUE) > 0) +
                    (rowSums(d[c("DQQ4","DQQ21")] == 1, na.rm=TRUE) > 0) +
                    (rowSums(d[c("DQQ13", "DQQ14", "DQQ15", "DQQ16", "DQQ17", "DQQ18", "DQQ19", "DQQ20", "DQQ25")] == 1, na.rm=TRUE) > 0)) == 5, 1, 0)

#### 2.a.	At least one vegetable  ----
d$all5a <- ifelse((rowSums(d[c("DQQ5", "DQQ6_1", "DQQ6_2", "DQQ7_1", "DQQ7_2", "DQQ7_3")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)
#### 2.b.	At least one fruit  ----
d$all5b <- ifelse((rowSums(d[c("DQQ8","DQQ9", "DQQ10_1", "DQQ10_2", "DQQ10_3")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)
#### 2.c. At least one pulse, nut or seed  ----
d$all5c <- ifelse((rowSums(d[c("DQQ4","DQQ21")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)
#### 2.d. At least one animal-source food (ASF)  ----
d$all5d <- ifelse((rowSums(d[c("DQQ13", "DQQ14", "DQQ15", "DQQ16", "DQQ17", "DQQ18", "DQQ19", "DQQ20", "DQQ25")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)
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
  (rowSums(d[c("DQQ10_1", "DQQ10_2", "DQQ10_3")] == 1, na.rm=TRUE) > 0)

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
  (rowSums(d[c("DQQ10_1", "DQQ10_2", "DQQ10_3")] == 1, na.rm=TRUE) > 0)-
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
d$zvegfr <- ifelse((rowSums(d[c("DQQ5","DQQ6_1","DQQ6_2", "DQQ7_1", "DQQ7_2", "DQQ7_3", "DQQ8", "DQQ9", "DQQ10_1", "DQQ10_2", "DQQ10_3")] == 1, na.rm=TRUE) > 0) == TRUE, 0, 1)

### 8. ASF consumption  ----
d$asf <- ifelse((rowSums(d[c("DQQ13", "DQQ14", "DQQ15", "DQQ17", "DQQ18", "DQQ19", "DQQ20", "DQQ25")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

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
                     (rowSums(d[c("DQQ10_1", "DQQ10_2", "DQQ10_3")] == 1, na.rm=TRUE) > 0)) >= 3, 1, 0)

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
                      (rowSums(d[c("DQQ10_1", "DQQ10_2", "DQQ10_3")] == 1, na.rm=TRUE) > 0)) >= 4, 1, 0)

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
d$vegfr <- ifelse((rowSums(d[c("DQQ5","DQQ6_1","DQQ6_2", "DQQ7_1", "DQQ7_2", "DQQ7_3", "DQQ8", "DQQ9", "DQQ10_1", "DQQ10_2", "DQQ10_3")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

#### 20.b. Salty snacks, instant noodles, or fast food  ----
d$snf <- ifelse((rowSums(d[c("DQQ22","DQQ23","DQQ29")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### Some complementary indicators ----
### 21. Dairy ----
d$dairy <- ifelse((rowSums(d[c("DQQ14","DQQ15","DQQ25")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 22. Dark green leafy vegetables ----
d$dveg <- ifelse((rowSums(d[c("DQQ6_1","DQQ6_2")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 23. Meat, poultry, or fish ----
d$anml <- ifelse((rowSums(d[c("DQQ16", "DQQ17","DQQ18", "DQQ19", "DQQ20")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 24. Other fruits ----
d$ofr <- ifelse((rowSums(d[c("DQQ10_1","DQQ10_2", "DQQ10_3")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 25. Other vegetables ----
d$oveg <- ifelse((rowSums(d[c("DQQ7_1","DQQ7_2", "DQQ7_3")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 26. Salty snacks, instant noodles, or fast food (including deep fried) ----
d$snfd <- ifelse((rowSums(d[c("DQQ22","DQQ23", "DQQ24", "DQQ29")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)

### 27. Unprocessed red meat ----
d$umeat <- ifelse((rowSums(d[c("DQQ17","DQQ18")] == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)
glimpse(d)

# 4. Data preparation (new columns and re-encoding factor variables) ----
## 4.1 New columns ----
# Removing extra space in countries names
d$Country <- str_trim(d$Country, side = "right")

# Add ISO, Income, and Region
d$Region <- ifelse(d$Country == "Egypt" | d$Country == "Israel" | d$Country == "Jordan" | d$Country == "Lebanon" | d$Country == "Morocco" | d$Country == "Palestine" | d$Country == "Yemen" | d$Country == "Tunisia", "MENA",
                   ifelse(d$Country == "Greece" | d$Country == "Kazakhstan" | d$Country == "Russia" | d$Country == "Tajikistan" | d$Country == "Turkey" | d$Country == "Albania" | d$Country == "Armenia" | d$Country == "Azerbaijan" | d$Country == "Kyrgyzstan" | d$Country == "Uzbekistan", "ECA",                                                                   
                          ifelse(d$Country == "Bangladesh" | d$Country == "Cambodia" | d$Country == "China" | d$Country == "India" | d$Country == "Indonesia" | d$Country == "Laos" | d$Country == "Nepal" | d$Country == "Pakistan" | d$Country == "Philippines" | d$Country == "Sri Lanka" | d$Country == "Vietnam" | d$Country == "Afghanistan", "AP",                             
                                 ifelse(d$Country == "Bolivia" | d$Country == "United States" | d$Country == "Chile" | d$Country == "Colombia" | d$Country == "Ecuador" | d$Country == "Mexico" | d$Country == "Nicaragua" | d$Country == "Honduras", "AM", "SSA"))))


dsub <- d %>%
  group_by(Country, COUNTRY_ISO3) %>%
  reframe(n= n())

# Adding ISO3, Income and Region to main data frame
#attributes(dsub$Country) <- NULL
#attributes(dsub$COUNTRY_ISO3) <- NULL
cntryNames <- dsub$Country
cntryISO3 <- dsub$COUNTRY_ISO3
cntryIncome <- c("L", "UM", "UM", "UM", "LM", "LM", "LM", "L", "LM", "LM", "L", "H", "UM", "UM", "UM", "LM", "UM", "LM", "H", "LM",  
                 "LM", "LM", "H", "UM", "UM", "LM", "LM", "LM", "UM", "L", "L", "UM", "LM", "L", "L", "LM","L", "LM",
                 "LM", "LM", "LM", "UM", "LM", "L", "UM", "LM", "LM", "LM", "LM", "UM", "L", "H", "LM", "LM", "L", "LM")

newcols <- data.frame(Country = cntryNames, ISO3 = cntryISO3, "Income classification" = cntryIncome)
d <- left_join(d, newcols, by= "Country")


## 4.2 Encoding Gender and Residence ----
# Checking values labels and attributes
fct_count(as.factor(d$Gender)); fct_count(as.factor(d$DEGURBA_2021_F2F)); fct_count(as.factor(d$DEGURBA_2022))
attributes(d$Gender); attributes(d$DEGURBA_2021_F2F); attributes(d$DEGURBA_2022)

# Gender
Gender <- fct_collapse(as.factor(d$Gender), "Male" = "1", "Female" = "2")
d$Gender <- Gender

# Residence 
d$Residence <- ifelse(d$DEGURBA_2021_F2F == 2 | d$DEGURBA_2021_F2F == 3, "Urban",
                      ifelse(d$DEGURBA_2021_F2F == 1, "Rural", NA)) # remaining NAs: 36,559

d$Residence <- ifelse(is.na(d$Residence),
                      ifelse(d$DEGURBA_2022 == 2 | d$DEGURBA_2022 == 3, "Urban",
                             ifelse(d$DEGURBA_2022 == 1, "Rural", NA)), d$Residence) # remaining NAs 19,571

d$Residence <- ifelse(is.na(d$Residence),
                      ifelse(d$Urbanicity == 1 | d$Urbanicity == 2, "Rural",
                             ifelse(d$Urbanicity == 3 | d$Urbanicity == 6, "Urban", NA)), d$Residence) # remaining NAs 74 

sum(is.na(d$Residence))
glimpse(d)
# Saving data 
write_csv(d, "Data/Output/CSV/dAnalysis.csv")
