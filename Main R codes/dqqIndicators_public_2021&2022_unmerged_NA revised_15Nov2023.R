#########   DQQ-based indicators (un-merged version)   #########
##############          (Public format)        ##############

# 1. Loading required libraries ----
library(haven)
library(expss)
library(tidyverse)
library(foreign)
library(survey)
library(plotrix)
library(labelled)

# 2. Data preparation ----
## 2.1 Loading and variable-selection ----
  # Paste your SAV file in your working directory
d= read.spss(paste(getwd(), "DQQ GWP 2021–2022 Microdata.sav", sep = "/"), 
             use.value.labels=F, to.data.frame=TRUE, stringsAsFactors=FALSE)

d <- d[ , c("STRATA", "PSU", "CaseID", "Weight", 
            "YEAR",
            "Country", "Gender", "Age", "Education", "IncomeQuintiles", "COUNTRY_ISO3",
            "Urbanicity", "DEGURBA_2021_F2F", "DEGURBA_2022", "DEGURBA_2021_PHONE", 
            "DQQ1", "DQQ2", "DQQ3", "DQQ4", "DQQ5", "DQQ6_1", "DQQ6_2", 
            "DQQ7_1", "DQQ7_2", "DQQ7_3", "DQQ8", "DQQ9", "DQQ10_1", "DQQ10_2", "DQQ10_3", "DQQ11", 
            "DQQ12", "DQQ13", "DQQ13_IND", "DQQ14", "DQQ15", "DQQ16", "DQQ16_IND", "DQQ17", 
            "DQQ17_IND1", "DQQ17_IND2", "DQQ18", "DQQ18_IND1", "DQQ18_IND2", "DQQ19", "DQQ19_IND", 
            "DQQ20", "DQQ20_IND", "DQQ20_ISR1", "DQQ20_ISR2", "DQQ21", "DQQ22", "DQQ23", "DQQ24", 
            "DQQ25", "DQQ26", "DQQ27", "DQQ28", "DQQ29")]

# Removing extra space in countries names
d$Country <- str_trim(d$Country, side = "right")

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

## 2.3 Replacing initial NAs with 0 ----
d %<>%
  mutate(
    across(c(DQQ6_2, DQQ7_2, DQQ7_3, DQQ10_2, DQQ10_3, DQQ14, DQQ18, DQQ23), .fns = ~ replace(.x, is.na(.x), 0)))

## 2.4 DK and Refused answers ----
d <- d %>%
  mutate(across(.cols = starts_with("DQQ"), .fns = ~ replace(.x, .x == 9, NA)),
         across(.cols = starts_with("DQQ"), .fns = ~ replace(.x, .x == 8, 0)),
         across(.cols = starts_with("DQQ"), .fns = ~ replace(.x, .x == 2, 0)))

#  3. DQQ-based Indicators  ----
###  3.1. MDD-W and FGDS  ----
  #FGDS  
d %<>% 
  mutate(
    fgds = 
      case_when(DQQ1 == 0 & DQQ2 == 0 & DQQ3 == 0 ~ 0L, DQQ1 == 1 | DQQ2 == 1 | DQQ3 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ4 == 0 ~ 0L, DQQ4 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ21 == 0 ~ 0L, DQQ21 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ14 == 0 & DQQ15 == 0 & DQQ25 == 0 ~ 0L, DQQ14 == 1 | DQQ15 == 1 | DQQ25 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ16 == 0 & DQQ17 == 0 & DQQ18 == 0 & DQQ19 == 0 & DQQ20 == 0 ~ 0L, DQQ16 == 1 | DQQ17 == 1 | DQQ18 == 1 | DQQ19 == 1 | DQQ20 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ13 == 0 ~ 0L, DQQ13 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ6_1 == 0 & DQQ6_2 == 0 ~ 0L, DQQ6_1 == 1 | DQQ6_2 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ5 == 0 & DQQ8 == 0 ~ 0L, DQQ5 == 1 | DQQ8 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ7_1 == 0 & DQQ7_2 == 0 & DQQ7_3 == 0 ~ 0L, DQQ7_1 == 1 | DQQ7_2 == 1 | DQQ7_3 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ9 == 0 & DQQ10_1 == 0 & DQQ10_2 == 0 & DQQ10_3 == 0 ~ 0L, DQQ9 == 1 | DQQ10_1 == 1 | DQQ10_2 == 1 | DQQ10_3 == 1 ~ 1L, TRUE ~ NA)
  )

  #MDD-W 
d$mddw <- ifelse((case_when(d$DQQ1 == 0 & d$DQQ2 == 0 & d$DQQ3 == 0 ~ 0L, d$DQQ1 == 1 | d$DQQ2 == 1 | d$DQQ3 == 1 ~ 1L, TRUE ~ NA) + 
                    case_when(d$DQQ4 == 0 ~ 0L, d$DQQ4 == 1 ~ 1L, TRUE ~ NA) + 
                    case_when(d$DQQ21 == 0 ~ 0L, d$DQQ21 == 1 ~ 1L, TRUE ~ NA) + 
                    case_when(d$DQQ14 == 0 & d$DQQ15 == 0 & d$DQQ25 == 0 ~ 0L, d$DQQ14 == 1 | d$DQQ15 == 1 | d$DQQ25 == 1 ~ 1L, TRUE ~ NA) + 
                    case_when(d$DQQ16 == 0 & d$DQQ17 == 0 & d$DQQ18 == 0 & d$DQQ19 == 0 & d$DQQ20 == 0 ~ 0L, d$DQQ16 == 1 | d$DQQ17 == 1 | d$DQQ18 == 1 | d$DQQ19 == 1 | d$DQQ20 == 1 ~ 1L, TRUE ~ NA) + 
                    case_when(d$DQQ13 == 0 ~ 0L, d$DQQ13 == 1 ~ 1L, TRUE ~ NA) + 
                    case_when(d$DQQ6_1 == 0 & d$DQQ6_2 == 0 ~ 0L, d$DQQ6_1 == 1 | d$DQQ6_2 == 1 ~ 1L, TRUE ~ NA) + 
                    case_when(d$DQQ5 == 0 & d$DQQ8 == 0 ~ 0L, d$DQQ5 == 1 | d$DQQ8 == 1 ~ 1L, TRUE ~ NA) + 
                    case_when(d$DQQ7_1 == 0 & d$DQQ7_2 == 0 & d$DQQ7_3 == 0 ~ 0L, d$DQQ7_1 == 1 | d$DQQ7_2 == 1 | d$DQQ7_3 == 1 ~ 1L, TRUE ~ NA) + 
                    case_when(d$DQQ9 == 0 & d$DQQ10_1 == 0 & d$DQQ10_2 == 0 & d$DQQ10_3 == 0 ~ 0L, d$DQQ9 == 1 | d$DQQ10_1 == 1 | d$DQQ10_2 == 1 | d$DQQ10_3 == 1 ~ 1L, TRUE ~ NA)) >= 5 & d$Gender == 2 &  d$Age >= 15 & d$Age <= 49, 1, 
                 
                 ifelse((case_when(d$DQQ1 == 0 & d$DQQ2 == 0 & d$DQQ3 == 0 ~ 0L, d$DQQ1 == 1 | d$DQQ2 == 1 | d$DQQ3 == 1 ~ 1L, TRUE ~ NA) + 
                           case_when(d$DQQ4 == 0 ~ 0L, d$DQQ4 == 1 ~ 1L, TRUE ~ NA) + 
                           case_when(d$DQQ21 == 0 ~ 0L, d$DQQ21 == 1 ~ 1L, TRUE ~ NA) + 
                           case_when(d$DQQ14 == 0 & d$DQQ15 == 0 & d$DQQ25 == 0 ~ 0L, d$DQQ14 == 1 | d$DQQ15 == 1 | d$DQQ25 == 1 ~ 1L, TRUE ~ NA) + 
                           case_when(d$DQQ16 == 0 & d$DQQ17 == 0 & d$DQQ18 == 0 & d$DQQ19 == 0 & d$DQQ20 == 0 ~ 0L, d$DQQ16 == 1 | d$DQQ17 == 1 | d$DQQ18 == 1 | d$DQQ19 == 1 | d$DQQ20 == 1 ~ 1L, TRUE ~ NA) + 
                           case_when(d$DQQ13 == 0 ~ 0L, d$DQQ13 == 1 ~ 1L, TRUE ~ NA) + 
                           case_when(d$DQQ6_1 == 0 & d$DQQ6_2 == 0 ~ 0L, d$DQQ6_1 == 1 | d$DQQ6_2 == 1 ~ 1L, TRUE ~ NA) + 
                           case_when(d$DQQ5 == 0 & d$DQQ8 == 0 ~ 0L, d$DQQ5 == 1 | d$DQQ8 == 1 ~ 1L, TRUE ~ NA) + 
                           case_when(d$DQQ7_1 == 0 & d$DQQ7_2 == 0 & d$DQQ7_3 == 0 ~ 0L, d$DQQ7_1 == 1 | d$DQQ7_2 == 1 | d$DQQ7_3 == 1 ~ 1L, TRUE ~ NA) + 
                           case_when(d$DQQ9 == 0 & d$DQQ10_1 == 0 & d$DQQ10_2 == 0 & d$DQQ10_3 == 0 ~ 0L, d$DQQ9 == 1 | d$DQQ10_1 == 1 | d$DQQ10_2 == 1 | d$DQQ10_3 == 1 ~ 1L, TRUE ~ NA)) < 5 & d$Gender == 2 &  d$Age >= 15 & d$Age <= 49, 0, NA)) 


### 3.2. All-5  ----
d$all5 <- ifelse((
  case_when(d$DQQ1 == 0 & d$DQQ2 == 0 & d$DQQ3 == 0 ~ 0L, d$DQQ1 == 1 | d$DQQ2 == 1 | d$DQQ3 == 1 ~ 1L, TRUE ~ NA) +
    case_when(d$DQQ5 == 0 & d$DQQ6_1 == 0 & d$DQQ6_2 == 0 & d$DQQ7_1 == 0 & d$DQQ7_2 == 0 & d$DQQ7_3 == 0 ~ 0L, 
              d$DQQ5 == 1 | d$DQQ6_1 == 1 | d$DQQ6_2 == 1 | d$DQQ7_1 == 1 | d$DQQ7_2 == 1 | d$DQQ7_3 == 1 ~ 1L, TRUE ~ NA) + 
    case_when(d$DQQ8 == 0 & d$DQQ9 == 0 & d$DQQ10_1 == 0 & d$DQQ10_2 == 0 & d$DQQ10_3 == 0 ~ 0L, 
              d$DQQ8 == 1 | d$DQQ9 == 1 | d$DQQ10_1 == 1 | d$DQQ10_2 == 1 | d$DQQ10_3 == 1 ~ 1L, TRUE ~ NA) + 
    case_when(d$DQQ4 == 0 & d$DQQ21 == 0 ~ 0L, d$DQQ4 == 1 | d$DQQ21 == 1 ~ 1L, TRUE ~ NA) +
    case_when(d$DQQ13 == 0 & d$DQQ14 == 0 & d$DQQ15 == 0 & d$DQQ16 == 0 & d$DQQ17 == 0 & d$DQQ18 == 0 & d$DQQ19 == 0 & d$DQQ20 == 0 & d$DQQ25 == 0 ~ 0L, 
              d$DQQ13 == 1 | d$DQQ14 == 1 | d$DQQ15 == 1 | d$DQQ16 == 1 | d$DQQ17 == 1 | d$DQQ18 == 1 | d$DQQ19 == 1 | d$DQQ20 == 1 | d$DQQ25 == 1 ~ 1L, TRUE ~ NA)) == 5, 1, 0)

#### 3.2.a.	At least one vegetable  ----
d %<>% mutate(
  all5a = case_when(DQQ5 == 0 & DQQ6_1 == 0 & DQQ6_2 == 0 & DQQ7_1 == 0 & DQQ7_2 == 0 & DQQ7_3 == 0 ~ 0L, 
                    DQQ5 == 1 | DQQ6_1 == 1 | DQQ6_2 == 1 | DQQ7_1 == 1 | DQQ7_2 == 1 | DQQ7_3 == 1 ~ 1L, TRUE ~ NA)
)

#### 3.2.b.	At least one fruit  ----
d %<>% mutate(
  all5b = case_when(DQQ8 == 0 & DQQ9 == 0 & DQQ10_1 == 0 & DQQ10_2 == 0 & DQQ10_3 == 0 ~ 0L, 
                    DQQ8 == 1 | DQQ9 == 1 | DQQ10_1 == 1 | DQQ10_2 == 1 | DQQ10_3 == 1 ~ 1L, TRUE ~ NA)
)

#### 3.2.c. At least one pulse, nut or seed  ----
d %<>% mutate(
  all5c = case_when(DQQ4 == 0 & DQQ21 == 0 ~ 0L, DQQ4 == 1 | DQQ21 == 1 ~ 1L, TRUE ~ NA)
)

#### 3.2.d. At least one animal-source food (ASF)  ----
d %<>% mutate(
  all5d = case_when(DQQ13 == 0 & DQQ14 == 0 & DQQ15 == 0 & DQQ16 == 0 & DQQ17 == 0 & DQQ18 == 0 & DQQ19 == 0 & DQQ20 == 0 & DQQ25 == 0 ~ 0L, 
                    DQQ13 == 1 | DQQ14 == 1 | DQQ15 == 1 | DQQ16 == 1 | DQQ17 == 1 | DQQ18 == 1 | DQQ19 == 1 | DQQ20 == 1 | DQQ25 == 1 ~ 1L, TRUE ~ NA)
)

#### 3.2.e. At least starchy staple  ----
d %<>% mutate(
  all5e = case_when(DQQ1 == 0 & DQQ2 == 0 & DQQ3 == 0 ~ 0L, DQQ1 == 1 | DQQ2 == 1 | DQQ3 == 1 ~ 1L, TRUE ~ NA)
)

### 3.3. NCD-Protect score  ----
d %<>%
  mutate(
    ncdp = 
      case_when(DQQ2 == 0 ~ 0L, DQQ2 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ4 == 0 ~ 0L, DQQ4 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ21 == 0 ~ 0L, DQQ21 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ5 == 0 ~ 0L, DQQ5 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ6_1 == 0 & DQQ6_2 == 0 ~ 0L, DQQ6_1 == 1 | DQQ6_2 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ7_1 == 0 & DQQ7_2 == 0 & DQQ7_3 == 0 ~ 0L, DQQ7_1 == 1 | DQQ7_2 == 1 | DQQ7_3 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ8 == 0 ~ 0L, DQQ8 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ9 == 0 ~ 0L, DQQ9 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ10_1 == 0 & DQQ10_2 == 0 & DQQ10_3 == 0 ~ 0L, DQQ10_1 == 1 | DQQ10_2 == 1 | DQQ10_3 == 1 ~ 1L, TRUE ~ NA)
  )

### 3.4. NCD-Risk score  ----
d %<>%
  mutate(
    ncdr = 
      case_when(DQQ28 == 0 ~ 0L, DQQ28 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ11 == 0 ~ 0L, DQQ11 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ12 == 0 ~ 0L, DQQ12 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ16 == 0 ~ 0L, DQQ16 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ16 == 0 ~ 0L, DQQ16 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ17 == 0 & DQQ18 == 0 ~ 0L, DQQ17 == 1 | DQQ18 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ24 == 0 ~ 0L, DQQ24 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ23 == 0 & DQQ29 == 0 ~ 0L, DQQ23 == 1 | DQQ29 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ22 == 0 ~ 0L, DQQ22 == 1 ~ 1L, TRUE ~ NA) 
  )

### 3.5. GDR score  ----
d %<>%
  mutate(
    gdr = 
      case_when(DQQ2 == 0 ~ 0L, DQQ2 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ4 == 0 ~ 0L, DQQ4 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ21 == 0 ~ 0L, DQQ21 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ5 == 0 ~ 0L, DQQ5 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ6_1 == 0 & DQQ6_2 == 0 ~ 0L, DQQ6_1 == 1 | DQQ6_2 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ7_1 == 0 & DQQ7_2 == 0 & DQQ7_3 == 0 ~ 0L, DQQ7_1 == 1 | DQQ7_2 == 1 | DQQ7_3 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ8 == 0 ~ 0L, DQQ8 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ9 == 0 ~ 0L, DQQ9 == 1 ~ 1L, TRUE ~ NA) + 
      case_when(DQQ10_1 == 0 & DQQ10_2 == 0 & DQQ10_3 == 0 ~ 0L, DQQ10_1 == 1 | DQQ10_2 == 1 | DQQ10_3 == 1 ~ 1L, TRUE ~ NA) -
      case_when(DQQ28 == 0 ~ 0L, DQQ28 == 1 ~ 1L, TRUE ~ NA) - 
      case_when(DQQ11 == 0 ~ 0L, DQQ11 == 1 ~ 1L, TRUE ~ NA) - 
      case_when(DQQ12 == 0 ~ 0L, DQQ12 == 1 ~ 1L, TRUE ~ NA) - 
      case_when(DQQ16 == 0 ~ 0L, DQQ16 == 1 ~ 1L, TRUE ~ NA) - 
      case_when(DQQ16 == 0 ~ 0L, DQQ16 == 1 ~ 1L, TRUE ~ NA) - 
      case_when(DQQ17 == 0 & DQQ18 == 0 ~ 0L, DQQ17 == 1 | DQQ18 == 1 ~ 1L, TRUE ~ NA) - 
      case_when(DQQ24 == 0 ~ 0L, DQQ24 == 1 ~ 1L, TRUE ~ NA) - 
      case_when(DQQ23 == 0 & DQQ29 == 0 ~ 0L, DQQ23 == 1 | DQQ29 == 1 ~ 1L, TRUE ~ NA) - 
      case_when(DQQ22 == 0 ~ 0L, DQQ22 == 1 ~ 1L, TRUE ~ NA) + 
      9
  )

### 3.6. DQQ score  ----
  #Not applicable for this stage

### 3.7. Zero vegetable or fruit   ----
d %<>% mutate(
  zvegfr = case_when(DQQ5 == 0 & DQQ6_1 == 0 & DQQ6_2 == 0 & DQQ7_1 == 0 & DQQ7_2 == 0 & DQQ7_3 == 0 & DQQ8 == 0 & DQQ9 == 0 & DQQ10_1 == 0 & DQQ10_2 == 0 & DQQ10_3 == 0 ~ 1L, 
                     DQQ5 == 1 | DQQ6_1 == 1 | DQQ6_2 == 1 | DQQ7_1 == 1 | DQQ7_2 == 1 | DQQ7_3 == 1 | DQQ8 == 1 | DQQ9 == 1 | DQQ10_1 == 1 | DQQ10_2 == 1 | DQQ10_3 == 1 ~ 0L, TRUE ~ NA)
)

### 3.8. ASF consumption  ----
d %<>% mutate(
  asf = case_when(DQQ13 == 0 & DQQ14 == 0 & DQQ15 == 0 & DQQ17 == 0 & DQQ18 == 0 & DQQ19 == 0 & DQQ20 == 0 & DQQ25 == 0 ~ 0L, 
                  DQQ13 == 1 | DQQ14 == 1 | DQQ15 == 1 | DQQ17 == 1 | DQQ18 == 1 | DQQ19 == 1 | DQQ20 == 1 | DQQ25 == 1 ~ 1L, TRUE ~ NA)
)

### 3.9. Sweet beverage consumption   ----
d %<>% mutate(
  swtbev = case_when(DQQ26 == 0 & DQQ27 == 0 & DQQ28 == 0 ~ 0L, 
                     DQQ26 == 1 | DQQ27 == 1 | DQQ28 == 1 ~ 1L, TRUE ~ NA)
)

#### 3.9.a. Sugar-sweetened soft drink consumption  ----
d %<>% mutate(
  sugdr = case_when(DQQ28 == 0 ~ 0L, DQQ28 == 1 ~ 1L, TRUE ~ NA)
)

### 3.10. Sweet foods consumption   ----
d %<>% mutate(
  swtfd = case_when(DQQ11 == 0 & DQQ12 == 0 ~ 0L, 
                    DQQ11 == 1 | DQQ12 == 1 ~ 1L, TRUE ~ NA) 
)

### 3.11. Salty or fried snack consumption   ----
d %<>% mutate(
  safd = case_when(DQQ22 == 0 & DQQ23 == 0 & DQQ24 == 0 ~ 0L, 
                   DQQ22 == 1 | DQQ23 == 1 | DQQ24 == 1 ~ 1L, TRUE ~ NA)
)

### 3.12. Whole grain consumption   ----
d %<>% mutate(
  wgrn = case_when(DQQ2 == 0 ~ 0L, DQQ2 == 1 ~ 1L, TRUE ~ NA)
)

### 3.13. Pulse consumption   ----
d %<>% mutate(
  pls = case_when(DQQ4 == 0 ~ 0L, DQQ4 == 1 ~ 1L, TRUE ~ NA)
)

### 3.14. Nuts and seeds consumption   ----
d %<>% mutate(
  nut = case_when(DQQ21 == 0 ~ 0L, DQQ21 == 1 ~ 1L, TRUE ~ NA)
)

### 3.15. Processed meat consumption   ----
d %<>% mutate(
  pmeat = case_when(DQQ16 == 0 ~ 0L, DQQ16 == 1 ~ 1L, TRUE ~ NA)
)

### 3.16. Percent consuming each food group   ----
#### 3.16.a. at least one vegetable or fruit   ----
d %<>% mutate(
  vegfr = case_when(DQQ5 == 0 & DQQ6_1 == 0 & DQQ6_2 == 0 & DQQ7_1 == 0 & DQQ7_2 == 0 & DQQ7_3 == 0 & DQQ8 == 0 & DQQ9 == 0 & DQQ10_1 == 0 & DQQ10_2 == 0 & DQQ10_3 == 0 ~ 0L, 
                    DQQ5 == 1 | DQQ6_1 == 1 | DQQ6_2 == 1 | DQQ7_1 == 1 | DQQ7_2 == 1 | DQQ7_3 == 1 | DQQ8 == 1 | DQQ9 == 1 | DQQ10_1 == 1 | DQQ10_2 == 1 | DQQ10_3 == 1 ~ 1L, TRUE ~ NA)
)

#### 3.16.b. Salty snacks, instant noodles, or fast food  ----
d %<>% mutate(
  snf = case_when(DQQ22 == 0 & DQQ23 == 0 & DQQ29 == 0 ~ 0L, 
                  DQQ22 == 1 | DQQ23 == 1 | DQQ29 == 1 ~ 1L, TRUE ~ NA)
)

### Some complementary indicators ----
### 3.17. Dairy ----
d %<>% mutate(
  dairy = case_when(DQQ14 == 0 & DQQ15 == 0 & DQQ25 == 0 ~ 0L, 
                    DQQ14 == 1 | DQQ15 == 1 | DQQ25 == 1 ~ 1L, TRUE ~ NA)
)

### 3.18. Dark green leafy vegetables ----
d %<>% mutate(
  dveg = case_when(DQQ6_1 == 0 & DQQ6_2 == 0 ~ 0L, 
                   DQQ6_1 == 1 | DQQ6_2 == 1 ~ 1L, TRUE ~ NA)
)

### 3.19. Meat, poultry, or fish ----
d %<>% mutate(
  anml = case_when(DQQ16 == 0 & DQQ17 == 0 & DQQ18 == 0 & DQQ19 == 0 & DQQ20 == 0 ~ 0L, 
                   DQQ16 == 1 | DQQ17 == 1 | DQQ18 == 1 | DQQ19 == 1 | DQQ20 == 1 ~ 1L, TRUE ~ NA)
)

### 3.20. Other fruits ----
d %<>% mutate(
  ofr = case_when(DQQ10_1 == 0 & DQQ10_2 == 0 & DQQ10_3 == 0 ~ 0L, 
                  DQQ10_1 == 1 | DQQ10_2 == 1 | DQQ10_3 == 1 ~ 1L, TRUE ~ NA) 
)

### 3.21. Other vegetables ----
d %<>% mutate(
  oveg = case_when(DQQ7_1 == 0 & DQQ7_2 == 0 & DQQ7_3 == 0 ~ 0L, 
                   DQQ7_1 == 1 | DQQ7_2 == 1 | DQQ7_3 == 1 ~ 1L, TRUE ~ NA)
)

### 3.22. Salty snacks, instant noodles, or fast food (including deep fried) ----
d %<>% mutate(
  snfd = case_when(DQQ22 == 0 & DQQ23 == 0 & DQQ24 == 0 & DQQ29 == 0 ~ 0L, 
                   DQQ22 == 1 | DQQ23 == 1 | DQQ24 == 1 | DQQ29 == 0 ~ 1L, TRUE ~ NA)
)

### 3.23 Unprocessed red meat ----
d %<>% mutate(
  umeat = case_when(DQQ17 == 0 & DQQ18 == 0 ~ 0L, 
                    DQQ17 == 1 | DQQ18 == 1 ~ 1L, TRUE ~ NA) 
)

# 4. New columns and re-encoding factor variables ----
## 4.1 New columns ----
# Add ISO, Income, and Region
d$Region <- ifelse(d$Country == "Egypt" | d$Country == "Israel" | d$Country == "Jordan" | d$Country == "Lebanon" | d$Country == "Morocco" | d$Country == "Palestine" | d$Country == "Yemen" | d$Country == "Tunisia", "Middle East and North Africa",
                   ifelse(d$Country == "Greece" | d$Country == "Kazakhstan" | d$Country == "Russia" | d$Country == "Tajikistan" | d$Country == "Turkey" | d$Country == "Albania" | d$Country == "Armenia" | d$Country == "Azerbaijan" | d$Country == "Kyrgyzstan" | d$Country == "Uzbekistan", "Europe and Central Asia",                                                                   
                          ifelse(d$Country == "Bangladesh" | d$Country == "Cambodia" | d$Country == "China" | d$Country == "India" | d$Country == "Indonesia" | d$Country == "Laos" | d$Country == "Nepal" | d$Country == "Pakistan" | d$Country == "Philippines" | d$Country == "Sri Lanka" | d$Country == "Vietnam" | d$Country == "Afghanistan", "Asia Pacific",                             
                                 ifelse(d$Country == "Bolivia" | d$Country == "United States" | d$Country == "Chile" | d$Country == "Colombia" | d$Country == "Ecuador" | d$Country == "Mexico" | d$Country == "Nicaragua" | d$Country == "Honduras", "Americas", "Sub-Saharan Africa"))))


dsub <- d %>%
  group_by(Country, COUNTRY_ISO3) %>%
  reframe(n= n())

# Adding ISO3, Income and Region to main data frame
#attributes(dsub$Country) <- NULL
#attributes(dsub$COUNTRY_ISO3) <- NULL
cntryNames <- dsub$Country
cntryISO3 <- dsub$COUNTRY_ISO3
cntryIncome <- c("Low", "Upper middle", "Upper middle", "Upper middle", "Lower middle", "Lower middle", "Lower middle", "Low", "Lower middle", "Lower middle", "Low", "Highe", "Upper middle", "Upper middle", "Upper middle", "Lower middle", "Upper middle", "Lower middle", "Highe", "Lower middle",  
                 "Lower middle", "Lower middle", "Highe", "Upper middle", "Upper middle", "Lower middle", "Lower middle", "Lower middle", "Upper middle", "Low", "Low", "Upper middle", "Lower middle", "Low", "Low", "Lower middle","Low", "Lower middle",
                 "Lower middle", "Lower middle", "Lower middle", "Upper middle", "Lower middle", "Low", "Upper middle", "Lower middle", "Lower middle", "Lower middle", "Lower middle", "Upper middle", "Low", "Highe", "Lower middle", "Lower middle", "Low", "Lower middle")

newcols <- data.frame(Country = cntryNames, ISO3 = cntryISO3, "Income classification" = cntryIncome)
d <- left_join(d, newcols, by= "Country")

## 4.2 Encoding Gender and Residence ----
# Gender
Gender <- fct_collapse(as.factor(d$Gender), "Male" = "1", "Female" = "2")
d$Gender <- Gender

# Residence
d$Residence <- ifelse(d$DEGURBA_2021_F2F == 2 | d$DEGURBA_2021_F2F == 3, "Urban",
                      ifelse(d$DEGURBA_2021_F2F == 1, "Rural", NA)) 

d$Residence <- ifelse(is.na(d$Residence),
                      ifelse(d$DEGURBA_2022 == 2 | d$DEGURBA_2022 == 3, "Urban",
                             ifelse(d$DEGURBA_2022 == 1, "Rural", NA)), d$Residence) 

d$Residence <- ifelse(is.na(d$Residence),
                      ifelse(d$DEGURBA_2021_PHONE == 2 | d$DEGURBA_2021_PHONE == 3, "Urban",
                             ifelse(d$DEGURBA_2021_PHONE == 1, "Rural", NA)), d$Residence) 

d$Residence <- ifelse(is.na(d$Residence),
                      ifelse(d$Urbanicity == 1 | d$Urbanicity == 2, "Rural",
                             ifelse(d$Urbanicity == 3 | d$Urbanicity == 6, "Urban", NA)), d$Residence) 

# 5. Complex Survey design ----
result1 <- setNames(data.frame(matrix(ncol = 10, nrow = 1)), c("World_bank_income_group", "Region", "Country", "ISO3", "Subgroup", "Variable", "Year", "Mean_prevalence", "Lower_95_CI", "Upper_95_CI"))
result2 <- setNames(data.frame(matrix(ncol = 10, nrow = 1)), c("World_bank_income_group", "Region", "Country", "ISO3", "Subgroup", "Variable", "Year", "Mean_prevalence", "Lower_95_CI", "Upper_95_CI"))

## 5.1 Main loop ----
options(survey.lonely.psu="adjust")
for (j in unique(d$YEAR)){
  year <- j
  dd <- d[d$YEAR == year,]
  
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
                                  round(x_UCI, digits = 2)))
      
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
                                  round(x_UCI, digits = 2)))
      
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
                                  round(x_UCI, digits = 2)))
      
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
                                  round(x_UCI, digits = 2)))
      
    }
    #prevalence
    for(i in c("all5", "all5a", "all5b", "all5c", "all5d", "all5e", "DQQ11", "DQQ14", "DQQ9", "dairy", "dveg", "DQQ24", "DQQ13", 
               "DQQ29", "DQQ20", "DQQ1", "DQQ27", "DQQ23", "mddw", "anml", "DQQ25", "DQQ21", "ofr", "DQQ12", "oveg", "DQQ22", 
               "DQQ19", "DQQ16", "DQQ4", "safd", "snf", "swtbev", "DQQ28", "swtfd", "DQQ26", "umeat", "DQQ18", "DQQ17",
               "DQQ8", "DQQ5", "DQQ3", "DQQ2", "DQQ15", "zvegfr")){ 
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
                                  round(x*100, digits = 2), 
                                  round(x_LCI*100, digits = 2), 
                                  round(x_UCI*100, digits = 2), NA, NA, NA, NA))
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
                                  round(x*100, digits = 2), 
                                  round(x_LCI*100, digits = 2), 
                                  round(x_UCI*100, digits = 2)))
      
      result2 <- rbind(result2, c(unique(as.character(curdat$Income.classification[curdat$Country == curcountry])), 
                                  unique(as.character(curdat$Region[curdat$Country == curcountry])), 
                                  curcountry, 
                                  unique(as.character(curdat$ISO3[curdat$Country == curcountry])),
                                  "Female", 
                                  i, 
                                  year,
                                  round(y*100, digits = 2), 
                                  round(y_LCI*100, digits = 2), 
                                  round(y_UCI*100, digits = 2), 
                                  round((x-y)*100, digits = 2)))
      
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
                                  round(x*100, digits = 2), 
                                  round(x_LCI*100, digits = 2), 
                                  round(x_UCI*100, digits = 2)))
      
      result2 <- rbind(result2, c(unique(as.character(curdat$Income.classification[curdat$Country == curcountry])), 
                                  unique(as.character(curdat$Region[curdat$Country == curcountry])), 
                                  curcountry, 
                                  unique(as.character(curdat$ISO3[curdat$Country == curcountry])),
                                  "Rural", 
                                  i, 
                                  year,
                                  round(y*100, digits = 2), 
                                  round(y_LCI*100, digits = 2), 
                                  round(y_UCI*100, digits = 2)))
      
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
               fgds = "Dietary diversity score",
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
               swtbev = "Sweet beverages",
               DQQ28 = "Soft drink consumption",
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
results$Mean_prevalence <- as.numeric(results$Mean_prevalence)
results$Lower_95_CI <- as.numeric(results$Lower_95_CI)
results$Upper_95_CI <- as.numeric(results$Upper_95_CI)

results <- results %>%
  mutate(
    DQQ_question = as.character(dqqNames[results$Variable]),
    Variable = as.character(longNames[results$Variable]),
    Mean_prevalence = case_when(Subgroup == "Male" & Variable == "MDD-W" ~ NA, .default = Mean_prevalence)
  ) %>%
  relocate(DQQ_question, .after = Variable) %>%
  rename(Indicator = Variable)

write_csv(results, "Data/Output/CSV/results_public_unmerged_NA_Revised_15Nov.csv")

# End ----

