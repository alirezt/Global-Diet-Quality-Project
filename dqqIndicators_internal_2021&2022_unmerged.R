#########   DQQ-based indicators (un-merged version)   #########
##############           (Internal format)        ##############

# 1. Loading required libraries ----
library(haven)
library(expss)
library(tidyverse)
library(foreign)
library(survey)
library(plotrix)
library(labelled)

# 2. Data preparation ----
d= read.spss("Data/Input/Sav/2022/Diet_Quality_032023_INTERNAL.sav", use.value.labels=F, to.data.frame=TRUE, stringsAsFactors=FALSE)
glimpse(d)
d <- d[ , c("STRATA", "PSU", "CaseID", "Weight", 
            "FieldDate", "YEAR_WAVE" , 
            "Country", "Gender", "Age", "Education", "IncomeQuintiles", "COUNTRY_ISO3",
            "Urbanicity", "DEGURBA_2021_F2F", "DEGURBA_2022", 
            "DQQ1", "DQQ2", "DQQ3", "DQQ4", "DQQ5", "DQQ6_1", "DQQ6_2", 
            "DQQ7_1", "DQQ7_2", "DQQ7_3", "DQQ8", "DQQ9", "DQQ10_1", "DQQ10_2", "DQQ10_3", "DQQ11", 
            "DQQ12", "DQQ13", "DQQ13_IND", "DQQ14", "DQQ15", "DQQ16", "DQQ16_IND", "DQQ17", 
            "DQQ17_IND1", "DQQ17_IND2", "DQQ18", "DQQ18_IND1", "DQQ18_IND2", "DQQ19", "DQQ19_IND", 
            "DQQ20", "DQQ20_IND", "DQQ20_ISR1", "DQQ20_ISR2", "DQQ21", "DQQ22", "DQQ23", "DQQ24", 
            "DQQ25", "DQQ26", "DQQ27", "DQQ28", "DQQ29")]

## 2.1 Correcting date format in "FieldDate" column ----
# 2.1 is not necessary to run. for date we use "YEAR_WAVE". 
# Converting spss date format to 4-digit year format
d$FieldDate <- format(as.Date(d$FieldDate/86400, origin = "1582-10-14"), "%Y")
Clist23 <- d %>% filter(FieldDate == 2023) %>% group_by(Country) %>% summarise(n = n()) # 2 countries
Clist22 <- d %>% filter(FieldDate == 2022) %>% group_by(Country) %>% summarise(n = n()) # 18 
Clist21 <- d %>% filter(FieldDate == 2021) %>% group_by(Country) %>% summarise(n = n())  # 38 
intersect(Clist22$Country, Clist21$Country) # "Sierra Leone" and "United States" in both years 
intersect(Clist23$Country, Clist22$Country); intersect(Clist23$Country, Clist21$Country) # no intersection

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

#  3. DQQ-based Indicators  ----
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
glimpse(d)

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

sum(is.na(d$Residence)) # 74
write_csv(d, "Data/Output/CSV/d_internal_complete.csv")

# 5. Complex Survey design ----
result1 <- setNames(data.frame(matrix(ncol = 14, nrow = 1)), c("Income_group", "Region", "Country", "ISO3", "Subgroup", "Variable", "Year", "Mean_Prevalence", "LCI", "UCI", "Difference", "Diff_LCI", "Diff_UCI", "Diff_p"))
result2 <- setNames(data.frame(matrix(ncol = 14, nrow = 1)), c("Income_group", "Region", "Country", "ISO3", "Subgroup", "Variable", "Year", "Mean_Prevalence", "LCI", "UCI", "Difference", "Diff_LCI", "Diff_UCI", "Diff_p"))

## 5.1 Main loop ----
options(survey.lonely.psu="adjust")
for (j in unique(d$YEAR_WAVE)){
  year <- j
  dd <- d[d$YEAR_WAVE == year,]
  
  for(k in unique(dd$Country)){
    curcountry <- k
    curdat <- dd[dd$Country == curcountry, ]
    # Overall
    # Create weighted object
    ifelse(is.na(unique(curdat$PSU)), d_w <- svydesign(ids = ~CaseID, strata = ~STRATA, nest = FALSE, weights = ~Weight, data =  curdat), 
           d_w <- svydesign(ids = ~PSU, strata = ~STRATA, nest = TRUE, weights = ~Weight, data =  curdat))
    
    # By Sex
    # Male
    
    d_m <- curdat[curdat$Gender == "Male", ]
    
    # Create weighted object
    ifelse(is.na(unique(d_m$PSU)), d_m_w <- svydesign(ids = ~CaseID, strata = ~STRATA, nest = FALSE, weights = ~Weight, data = d_m), 
           d_m_w <- svydesign(ids = ~PSU, strata = ~STRATA, nest = TRUE, weights = ~Weight, data = d_m))
    
    
    # Female
    d_f <- curdat[curdat$Gender == "Female", ]
    
    # Create weighted object
    ifelse(is.na(unique(d_f$PSU)), d_f_w <- svydesign(ids = ~CaseID, strata = ~STRATA, nest = FALSE, weights = ~Weight, data = d_f), 
           d_f_w <- svydesign(ids = ~PSU, strata = ~STRATA, nest = TRUE, weights = ~Weight, data = d_f))
    
    
    # By Residence
    # Urban
    #d_u <- curdat[curdat$Residence == "Urban", ]
    d_u <- curdat[which(curdat$Residence == "Urban"), ]
    
    # Create weighted object
    ifelse(is.na(unique(d_u$PSU)), d_u_w <- svydesign(ids = ~CaseID, strata = ~STRATA, nest = FALSE, weights = ~Weight, data = d_u), 
           d_u_w <- svydesign(ids = ~PSU, strata = ~STRATA, nest = TRUE, weights = ~Weight, data = d_u))
    
    
    # subset the dataset for rural subgroup;
    
    d_r <- curdat[which(curdat$Residence == "Rural"), ]
    
    #Create weighted object for rural subrgoup
    ifelse(is.na(unique(d_r$PSU)), d_r_w <- svydesign(ids = ~CaseID, strata = ~STRATA, nest = FALSE, weights = ~Weight, data = d_r), 
           d_r_w <- svydesign(ids = ~PSU, strata = ~STRATA, nest = TRUE, weights = ~Weight, data = d_r))
    
    
    for(i in c("fgds", "ncdp", "ncdr", "gdr")){
      x <- svymean(~curdat[, i], d_w, na.rm = TRUE, method = "as", df=degf(d_w))
      x_LCI <- confint(x)[1]
      x_UCI <- confint(x)[2]
      result1 <- rbind(result1, c(unique(as.character(curdat$Income.classification[curdat$Country == curcountry])), 
                                  unique(as.character(curdat$Region[curdat$Country == curcountry])), 
                                  curcountry, 
                                  unique(as.character(curdat$ISO3[curdat$Country == curcountry])),
                                  "All", 
                                  i,
                                  year,
                                  round(x, digits = 2), 
                                  round(x_LCI, digits = 2), 
                                  round(x_UCI, digits = 2), NA, NA, NA, NA))
      #  # Male
      x <- svymean(~d_m[, i],  d_m_w, na.rm = TRUE, method = "as", df=degf(d_m_w))
      x_LCI <- confint(x)[1]
      x_UCI <- confint(x)[2]
      varname <- as.name(i)
      t <- eval(bquote(svyttest(.(varname)~Gender, d_w)))
      result1 <- rbind(result1, c(unique(as.character(curdat$Income.classification[curdat$Country == curcountry])), 
                                  unique(as.character(curdat$Region[curdat$Country == curcountry])), 
                                  curcountry, 
                                  unique(as.character(curdat$ISO3[curdat$Country == curcountry])),
                                  "Male", 
                                  i, 
                                  year,
                                  round(x, digits = 2), 
                                  round(x_LCI, digits = 2), 
                                  round(x_UCI, digits = 2), 
                                  as.numeric(round(t$estimate, digits = 2)), 
                                  as.numeric(round(t$conf.int[1], digits = 2)), 
                                  as.numeric(round(t$conf.int[2], digits = 2)), 
                                  as.numeric(t$p.value)))
      
      #  # Female
      x <- svymean(~d_f[, i],  d_f_w, na.rm = TRUE, method = "as", df=degf(d_f_w))
      x_LCI <- confint(x)[1]
      x_UCI <- confint(x)[2]
      result1 <- rbind(result1, c(unique(as.character(curdat$Income.classification[curdat$Country == curcountry])), 
                                  unique(as.character(curdat$Region[curdat$Country == curcountry])), 
                                  curcountry, 
                                  unique(as.character(curdat$ISO3[curdat$Country == curcountry])),
                                  "Female", 
                                  i, 
                                  year,
                                  round(x, digits = 2), 
                                  round(x_LCI, digits = 2), 
                                  round(x_UCI, digits = 2), 
                                  as.numeric(round(t$estimate, digits = 2)), 
                                  as.numeric(round(t$conf.int[1], digits = 2)), 
                                  as.numeric(round(t$conf.int[2], digits = 2)), 
                                  as.numeric(t$p.value)))
      
      #  # Urban
      x <- svymean(~d_u[, i],  d_u_w, na.rm = TRUE, method = "as", df=degf(d_u_w))
      x_LCI <- confint(x)[1]
      x_UCI <- confint(x)[2]
      varname <- as.name(i)
      t <- eval(bquote(svyttest(.(varname)~Residence, d_w)))
      result1 <- rbind(result1, c(unique(as.character(curdat$Income.classification[curdat$Country == curcountry])), 
                                  unique(as.character(curdat$Region[curdat$Country == curcountry])), 
                                  curcountry, 
                                  unique(as.character(curdat$ISO3[curdat$Country == curcountry])),
                                  "Urban", 
                                  i, 
                                  year,
                                  round(x, digits = 2), 
                                  round(x_LCI, digits = 2), 
                                  round(x_UCI, digits = 2), 
                                  as.numeric(round(t$estimate, digits = 2)), 
                                  as.numeric(round(t$conf.int[1], digits = 2)), 
                                  as.numeric(round(t$conf.int[2], digits = 2)), 
                                  as.numeric(t$p.value)))
      
      #  # Rural
      x <- svymean(~d_r[, i],  d_r_w, na.rm = TRUE, method = "as", df=degf(d_r_w))
      x_LCI <- confint(x)[1]
      x_UCI <- confint(x)[2]
      result1 <- rbind(result1, c(unique(as.character(curdat$Income.classification[curdat$Country == curcountry])), 
                                  unique(as.character(curdat$Region[curdat$Country == curcountry])), 
                                  curcountry, 
                                  unique(as.character(curdat$ISO3[curdat$Country == curcountry])),
                                  "Rural", 
                                  i,
                                  year,
                                  round(x, digits = 2), 
                                  round(x_LCI, digits = 2), 
                                  round(x_UCI, digits = 2), 
                                  as.numeric(round(t$estimate, digits = 2)), 
                                  as.numeric(round(t$conf.int[1], digits = 2)), 
                                  as.numeric(round(t$conf.int[2], digits = 2)), 
                                  as.numeric(t$p.value)))
      
    }
    #prevalence
    for(i in c("all5", "all5a", "all5b", "all5c", "all5d", "all5e", "DQQ11", "DQQ14", "DQQ9", "dairy", "dveg", "DQQ24", "DQQ13", 
               "DQQ29", "DQQ20", "DQQ1", "DQQ27", "DQQ23", "mddw", "anml", "DQQ25", "DQQ21", "ofr", "DQQ12", "oveg", "DQQ22", 
               "DQQ19", "DQQ16", "DQQ4", "safd", "snf", "swtbev", "DQQ28", "swtfd", "DQQ26", "umeat", "DQQ18", "DQQ17",
               "DQQ8", "DQQ5", "DQQ3", "DQQ2", "DQQ15", "zvegfr")){ ## 44 proportional indicators + 4 score variables 
      # All
      x <- svyciprop(~I(curdat[, i] == 1), d_w, na.rm = TRUE, method = "as", df=degf(d_w))
      x_LCI <- attributes(x)$ci[1]
      x_UCI <- attributes(x)$ci[2]
      result2 <- rbind(result2, c(unique(as.character(curdat$Income.classification[curdat$Country == curcountry])), 
                                  unique(as.character(curdat$Region[curdat$Country == curcountry])), 
                                  curcountry, 
                                  unique(as.character(curdat$ISO3[curdat$Country == curcountry])),
                                  "All", 
                                  i, 
                                  year,
                                  round(x, digits = 2), 
                                  round(x_LCI, digits = 2), 
                                  round(x_UCI, digits = 2), NA, NA, NA, NA))
      # Male and Female
      x <- svyciprop(~I(d_m[, i] == 1), d_m_w, na.rm = TRUE, method = "as", df=degf(d_m_w))
      x_LCI <- attributes(x)$ci[1]
      x_UCI <- attributes(x)$ci[2]
      
      y <- svyciprop(~I(d_f[, i] == 1), d_f_w, na.rm = TRUE, method = "as", df=degf(d_f_w))
      y_LCI <- attributes(y)$ci[1]
      y_UCI <- attributes(y)$ci[2]
      
      varname <- as.name(i)
      ifelse(x == 0 & y == 0, t <- NA, t <- eval(bquote(svychisq(~.(varname)+Gender,d_w))))
      
      result2 <- rbind(result2, c(unique(as.character(curdat$Income.classification[curdat$Country == curcountry])), 
                                  unique(as.character(curdat$Region[curdat$Country == curcountry])), 
                                  curcountry, 
                                  unique(as.character(curdat$ISO3[curdat$Country == curcountry])),
                                  "Male", 
                                  i, 
                                  year,
                                  round(x, digits = 4), 
                                  round(x_LCI, digits = 4), 
                                  round(x_UCI, digits = 4), 
                                  round(x-y, digits = 4), 
                                  NA, 
                                  NA, 
                                  ifelse(is.na(t), NA, as.numeric(t$p.value))))
      
      result2 <- rbind(result2, c(unique(as.character(curdat$Income.classification[curdat$Country == curcountry])), 
                                  unique(as.character(curdat$Region[curdat$Country == curcountry])), 
                                  curcountry, 
                                  unique(as.character(curdat$ISO3[curdat$Country == curcountry])),
                                  "Female", 
                                  i, 
                                  year,
                                  round(y, digits = 4), 
                                  round(y_LCI, digits = 4), 
                                  round(y_UCI, digits = 4), 
                                  round(x-y, digits = 4), 
                                  NA, 
                                  NA, 
                                  ifelse(is.na(t), NA, as.numeric(t$p.value))))
      
      # Rural and Urban
      x <- svyciprop(~I(d_u[, i] == 1), d_u_w, na.rm = TRUE, method = "as", df=degf(d_u_w))
      x_LCI <- attributes(x)$ci[1]
      x_UCI <- attributes(x)$ci[2]
      
      y <- svyciprop(~I(d_r[, i] == 1), d_r_w, na.rm = TRUE, method = "as", df=degf(d_r_w))
      y_LCI <- attributes(y)$ci[1]
      y_UCI <- attributes(y)$ci[2]
      
      varname <- as.name(i)
      ifelse(x == 0 & y == 0, t <- NA, t <- eval(bquote(svychisq(~.(varname)+Residence,d_w))))
      
      result2 <- rbind(result2, c(unique(as.character(curdat$Income.classification[curdat$Country == curcountry])), 
                                  unique(as.character(curdat$Region[curdat$Country == curcountry])), 
                                  curcountry, 
                                  unique(as.character(curdat$ISO3[curdat$Country == curcountry])),
                                  "Urban", 
                                  i,
                                  year,
                                  round(x, digits = 4), 
                                  round(x_LCI, digits = 4), 
                                  round(x_UCI, digits = 4), 
                                  round(x-y, digits = 4), 
                                  NA, 
                                  NA, 
                                  ifelse(is.na(t), NA, as.numeric(t$p.value))))
      
      result2 <- rbind(result2, c(unique(as.character(curdat$Income.classification[curdat$Country == curcountry])), 
                                  unique(as.character(curdat$Region[curdat$Country == curcountry])), 
                                  curcountry, 
                                  unique(as.character(curdat$ISO3[curdat$Country == curcountry])),
                                  "Rural", 
                                  i, 
                                  year,
                                  round(y, digits = 4), 
                                  round(y_LCI, digits = 4), 
                                  round(y_UCI, digits = 4), 
                                  round(x-y, digits = 4), 
                                  NA, 
                                  NA, 
                                  ifelse(is.na(t), NA, as.numeric(t$p.value))))
      
    }
  }
}


## 5.2 Saving the results ----
resultlist1 <- c()
resultlist2 <- c()
result1 <- result1[-1,]
resultlist1 <- c(resultlist1,result1)
result2 <- result2[-1,]
resultlist2 <- c(resultlist2,result2)
resultlist1 <- data.frame(resultlist1)      
resultlist2  <- data.frame(resultlist2)
colnames(resultlist1) <- colnames(resultlist2)
results <- rbind(resultlist1, resultlist2) 

## 5.3 Replacing the names ----
# Long Names format
longNames <- c(all5 = "All-5", 
               all5a = "At least one vegetable",
               all5b = "At least one fruit",
               all5c = "At least one pulse, nut, or seed",
               all5d = "At least one animal-source food",
               all5e = "At least one starchy staple food",
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
               snf = "Salty snacks, instant noodles, or fast food",
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

# DQQ names
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

## 5.4 Cleaning and exporting ----
results$Mean_Prevalence <- as.numeric(results$Mean_Prevalence)
results$LCI <- as.numeric(results$LCI)
results$UCI <- as.numeric(results$UCI)

resultst <- results %>%
  mutate(
    Mean_Prevalence = ifelse(Variable == "fgds" | Variable == "ncdp" | Variable == "ncdr" | Variable == "gdr",
                             Mean_Prevalence, round(Mean_Prevalence*100, digits = 2)),
    LCI = ifelse(Variable == "fgds" | Variable == "ncdp" | Variable == "ncdr" | Variable == "gdr",
                 LCI, round(LCI*100, digits = 2)),
    UCI = ifelse(Variable == "fgds" | Variable == "ncdp" | Variable == "ncdr" | Variable == "gdr",
                 UCI, round(UCI*100, digits = 2)),
    "DQQ Names" = as.character(dqqNames[results$Variable]),
    Variable = as.character(longNames[results$Variable]),
    Country = case_when(Year == "2021" & ISO3 == "USA" ~ "United States 2021", .default = Country),
    Country = case_when(Year == "2021" & ISO3 == "SLE" ~ "Sierra Leone 2021", .default = Country),
    Country = case_when(Year == "2021" & ISO3 == "MWI" ~ "Malawi 2021", .default = Country),
    Country = case_when(Year == "2022" & ISO3 == "USA" ~ "United States 2022", .default = Country),
    Country = case_when(Year == "2022" & ISO3 == "SLE" ~ "Sierra Leone 2022", .default = Country),
    Country = case_when(Year == "2022" & ISO3 == "MWI" ~ "Malawi 2022", .default = Country)
  ) %>%
  relocate("DQQ Names", .after = Variable) %>%
  rename("Income classification" = Income_group, Indicator = Variable, "Mean/Prevalence" = Mean_Prevalence,
         "Lower confidence interval" = LCI, "Upper confidence interval" = UCI)

head(results)
glimpse(results)
write_csv(resultst, "Data/Output/CSV/results_internal_unmerged.csv")

# End ----
