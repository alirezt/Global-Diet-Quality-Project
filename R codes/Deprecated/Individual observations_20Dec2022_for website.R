# Creation of Diet Quality Indicators
# GAIN Gallup Poll Project

# setup #
rm(list = ls())

#install.packages("expss") 
library(haven)
library(expss)
library(tidyverse)

# This package is to do basic data management
# for setting variable labels 

# set the working directory;
setwd("~/Google Drive/GAIN/Gallup/Analysis")

#  Read dataset:
df0 = read_sav("data/Diet_Quality_2021_042722.sav") 

# code to subset the data per country;

 #df0 <- df0 %>%

 #filter(COUNTRYNEW == " ###")

#  creating sex variable

df0$WP1219 <- as.character(df0$WP1219)
df0$WP1219[df0$WP1219 == 1] <- "Male"
df0$WP1219[df0$WP1219 == 2] <- "Female"
df0$Sex <- df0$WP1219

# Create food security variable
df0$FS <- ifelse(df0$WP40 == 1, "No",
                 ifelse(df0$WP40 == 2, "Yes", NA))

# Create wellbeing variable (good life if on 5th rung or higher on a 10 step ladder)
df0$GL <- ifelse(df0$WP16 == 10 | df0$WP16 == 9 | df0$WP16 == 8 | df0$WP16 == 7 | df0$WP16 == 6 |df0$WP16 == 5, "Yes", ifelse(df0$WP16 == 4 | df0$WP16 == 3 | df0$WP16 == 2 | df0$WP16 == 1 | df0$WP16 == 0, "No", NA))

#  creating age variable 

df0$age <- as.numeric(df0$WP1220)
df0$age <- ifelse(df0$age == 100, NA, df0$age)

df0$age_cat <- ifelse(df0$age >= 25, 1, 0)

class(df0$age)
table(df0$age_cat)

# assigning levels for each age category

df0$age_cat <- factor(df0$age_cat,
                      levels = c(0,1),
                      labels = c("15-24 Years", ">= 25 years"))

# Subset socio-demographic and economic characteristics (non diet variables);

df1 <- df0 %>%
  select(c("WP5889", "age", "age_cat", "COUNTRY_ISO3", "EMP_2010", "EMP_FTEMP","EMP_FTEMP_POP","EMP_LFPR", "EMP_UNDER","EMP_UNEMP", "WP5", "WAVE", "WPID" ,
           "WPID_RANDOM","WGT", "FIELD_DATE","COUNTRY_ISO2", "INCOME_7", "INDEX_CB", "INDEX_CE", "INDEX_CMA","INDEX_CMU","INDEX_FL", "INDEX_FS",
           "INDEX_LE", "INDEX_LO","INDEX_NI","INDEX_OT","INDEX_ST","INDEX_SU", "INDEX_TH","REG_GLOBAL","REG2_GLOBAL", "REGION_CAM", "REGION_KEN",
           "REGION2_CAM", "REGION2_KEN",   "WP10200",  "WP10202",   "WP10268",  "WP108",  "WP109", "WP110",   "WP111", "WP112", "WP113","WP117","WP119",
           "WP12",  "WP1220", "WP1223", "WP1230", "WP1230RECODED" , "WP1233",     "WP1233RECODED",  "WP13156",
           "WP137",  "WP138", "WP139",  "WP14",  "WP141" , "WP144","WP15862","WP16", "WP16056","WP16590","WP17625", "WP17626", "WP18", "WP19544", "WP2319", "WP30",
           "WP31", "WP3117", "WP4","WP40", "EMP_WORK_HOURS", "HHSIZE"   ,      "HHWEIGHT2"   ,   "INCOME_1"   ,    "INCOME_2" ,      "INCOME_3", "WP43", "WP4657","WP5AA",
           "WP7824" ,  "WP91", "WP92", "WP93", "WP94" , "WP95", "WP97", "WP9811" , "YEAR_CALENDAR" , "YEAR_WAVE",
           "INCOME_4" ,      "INCOME_5", "WP12258"   ,     "WP12258A"   ,    "WP12259")) %>%
  rename(id = "WP5889")



# Select diet variables;


df2 <- df0[, colnames(df0) %in% c("WP5889", "COUNTRYNEW",	"WP22295", "WP22296", "WP22297", "WP22298",  "WP22299",
                                  "WP22300","WP22301", "WP22302",  "WP22303",  "WP22304",  "WP22305",  "WP22306",
                                  "WP22307",  "WP22308",  "WP22309",  "WP22310", "WP22311", "WP22312",  "WP22312_IND", "WP22313",
                                  "WP22314",   "WP22315", "WP22315_IND", "WP22316", "WP22316_IND", "WP22316_IND1", "WP22317", "WP22317_IND", "WP22317_IND1", "WP22318", "WP22318_IND", "WP22319", "WP22319_IND", "WP22319_ISR", "WP22319_ISR1",  "WP22320",
                                  "WP22321",    "WP22322",   "WP22323", "WP22324",  "WP22325",  "WP22326",    "WP22327",
                                  "WP22328",    "Sex" , "FS", "GL", "age", "WP7572", "DEGURBA_F2F", "WP14")]



# Create a lookup table that can used as a data dictionary;

column_lookup <- data.frame(
  old = c("WP5889", "COUNTRYNEW",	"WP22295", "WP22296", "WP22297", "WP22298", "WP22299",
          "WP22300", "WP22301", "WP22302",  "WP22303", "WP22304",  "WP22305",  "WP22306",
          "WP22307",  "WP22308",  "WP22309",  "WP22310", "WP22311", "WP22312",    "WP22312_IND", "WP22313",
          "WP22314",   "WP22315", "WP22315_IND", "WP22316", "WP22316_IND", "WP22316_IND1", "WP22317", "WP22317_IND", "WP22317_IND1", "WP22318", "WP22318_IND", "WP22319", "WP22319_IND", "WP22319_ISR", "WP22319_ISR1",   "WP22320",
          "WP22321",    "WP22322",   "WP22323", "WP22324",  "WP22325",  "WP22326",    "WP22327",
          "WP22328",    "Sex" , "FS", "GL", "age", "WP7572", "DEGURBA_F2F", "WP14"),
  
  new = c("id", "COUNTRYNEW", "DQ1","DQ2","DQ3","DQ4","DQ5", "DQ6a","DQ6b","DQ7a","DQ7b","DQ7c", "DQ8",
          "DQ9", "DQ10a", "DQ10b", "DQ10c", "DQ11", "DQ12", "DQ13", "DQ13_IND", "DQ14", "DQ15",
          "DQ16", "DQ16_IND", "DQ17", "DQ17_INDa", "DQ17_INDb", "DQ18", "DQ18_INDa", "DQ18_INDb","DQ19", "DQ19_IND", "DQ20", "DQ20_IND", "DQ20_ISRa", "DQ20_ISRb", "DQ21","DQ22","DQ23","DQ24","DQ25",
          "DQ26", "DQ27", "DQ28", "DQ29",  "Sex" , "FS", "GL", "age", "WP7572", "DEGURBA_F2F", "WP14"))

# create a function to replace the old variables from a questions with new variables names created 

lookup_rename <- function(df, column_lookup) {
  df2 <- df
  names(df2) = column_lookup$new[match(names(df), column_lookup$old)]
  df2
}

# provide the dataset name and the variables names to replace the old variable 
# names with the new desired variable names

df2 <- lookup_rename(df2, column_lookup)


 # variable names of the dataset;

 allcols <- c("id", "COUNTRYNEW", "DQ1","DQ2","DQ3","DQ4","DQ5", "DQ6a","DQ6b","DQ7a","DQ7b","DQ7c", "DQ8",
              "DQ9", "DQ10a", "DQ10b", "DQ10c", "DQ11", "DQ12", "DQ13", "DQ13_IND", "DQ14", "DQ15",
              "DQ16", "DQ16_IND", "DQ17", "DQ17_INDa", "DQ17_INDb", "DQ18", "DQ18_INDa", "DQ18_INDb","DQ19", "DQ19_IND", "DQ20", "DQ20_IND", "DQ20_ISRa", "DQ20_ISRb", "DQ21","DQ22","DQ23","DQ24","DQ25",
              "DQ26", "DQ27", "DQ28", "DQ29",  "Sex" , "FS", "GL", "age", "WP7572", "DEGURBA_F2F", "WP14")

 # Identify missing variables. For those diet variables missed, assign NA 
 # Some countries missed some of the diet variables.
 
 missingcols <- allcols[!allcols %in% colnames(df2)]

if(length(missingcols) >0){
 for(i in 1:length(missingcols)){
   df2[,missingcols[i]] <- NA
 }
}

 falseifNA <- function(x){
   ifelse(is.na(x), FALSE, x)
 }
 
 # This ifelse2 function is used to create diet indicators;
 # This function ignores those diet variables which are missed,
 # and create the indicators using the available variables 
 
 ifelse2 <- function(x, a, b){
   ifelse(falseifNA(x), a, b)
 }
 
 # Create NAs for "Don't Know" or "Refused" responses
 df2$DQ1 <- ifelse(df2$DQ1 == 1, 1,
                   ifelse(df2$DQ1 == 2, 0, NA))
 df2$DQ2 <- ifelse(df2$DQ2 == 1, 1,
                   ifelse(df2$DQ2 == 2, 0, NA))
 df2$DQ3 <- ifelse(df2$DQ3 == 1, 1,
                   ifelse(df2$DQ3 == 2, 0, NA))
 df2$DQ4 <- ifelse(df2$DQ4 == 1, 1,
                   ifelse(df2$DQ4 == 2, 0, NA))
 df2$DQ5 <- ifelse(df2$DQ5 == 1, 1,
                   ifelse(df2$DQ5 == 2, 0, NA))
 df2$DQ6a <- ifelse(df2$DQ6a == 1, 1,
                    ifelse(df2$DQ6a == 2, 0, NA))
 df2$DQ6b <- ifelse(df2$DQ6b == 1, 1,
                    ifelse(df2$DQ6b == 2, 0, NA))
 df2$DQ7a <- ifelse(df2$DQ7a == 1, 1,
                    ifelse(df2$DQ7a == 2, 0, NA))
 df2$DQ7b <- ifelse(df2$DQ7b == 1, 1,
                    ifelse(df2$DQ7b == 2, 0, NA))
 df2$DQ7c <- ifelse(df2$DQ7c == 1, 1,
                    ifelse(df2$DQ7c == 2, 0, NA))
 df2$DQ8 <- ifelse(df2$DQ8 == 1, 1,
                   ifelse(df2$DQ8 == 2, 0, NA))
 df2$DQ9 <- ifelse(df2$DQ9 == 1, 1,
                   ifelse(df2$DQ9 == 2, 0, NA))
 df2$DQ10a <- ifelse(df2$DQ10a == 1, 1,
                     ifelse(df2$DQ10a == 2, 0, NA))
 df2$DQ10b <- ifelse(df2$DQ10b == 1, 1,
                     ifelse(df2$DQ10b == 2, 0, NA))
 df2$DQ10c <- ifelse(df2$DQ10c == 1, 1,
                     ifelse(df2$DQ10c == 2, 0, NA))
 df2$DQ11 <- ifelse(df2$DQ11 == 1, 1,
                    ifelse(df2$DQ11 == 2, 0, NA))
 df2$DQ12 <- ifelse(df2$DQ12 == 1, 1,
                    ifelse(df2$DQ12 == 2, 0, NA))
 df2$DQ13 <- ifelse(df2$DQ13 == 1, 1,
                    ifelse(df2$DQ13 == 2, 0, NA))
 df2$DQ13_IND <- ifelse(df2$DQ13_IND == 1, 1,
                        ifelse(df2$DQ13_IND == 2, 0, NA))
 df2$DQ13 <- ifelse(is.na(df2$DQ13_IND), df2$DQ13, df2$DQ13_IND)
 df2$DQ14 <- ifelse(df2$DQ14 == 1, 1,
                    ifelse(df2$DQ14 == 2, 0, NA))
 df2$DQ15 <- ifelse(df2$DQ15 == 1, 1,
                    ifelse(df2$DQ15 == 2, 0, NA))
 df2$DQ16 <- ifelse(df2$DQ16 == 1, 1,
                    ifelse(df2$DQ16 == 2, 0, NA))
 df2$DQ16_IND <- ifelse(df2$DQ16_IND == 1, 1,
                        ifelse(df2$DQ16_IND == 2, 0, NA))
 df2$DQ16 <- ifelse(is.na(df2$DQ16_IND), df2$DQ16, df2$DQ16_IND)
 df2$DQ17 <- ifelse(df2$DQ17 == 1, 1,
                    ifelse(df2$DQ17 == 2, 0, NA))
 df2$DQ17_INDa <- ifelse(df2$DQ17_INDa == 1, 1,
                         ifelse(df2$DQ17_INDa == 2, 0, NA))
 df2$DQ17_INDb <- ifelse(df2$DQ17_INDb == 1, 1,
                         ifelse(df2$DQ17_INDb == 2, 0, NA))
 df2$DQ17 <- ifelse(is.na(df2$DQ17_INDa), df2$DQ17, df2$DQ17_INDa)
 df2$DQ17 <- ifelse(is.na(df2$DQ17_INDb), df2$DQ17, df2$DQ17_INDb)
 df2$DQ18 <- ifelse(df2$DQ18 == 1, 1,
                    ifelse(df2$DQ18 == 2, 0, NA))
 df2$DQ18_INDa <- ifelse(df2$DQ18_INDa == 1, 1, 0)
 df2$DQ18_INDb <- ifelse(df2$DQ18_INDb == 1, 1, 0)
 df2$DQ19 <- ifelse(df2$DQ19 == 1, 1,
                    ifelse(df2$DQ19 == 2, 0, NA))
 df2$DQ19_IND <- ifelse(df2$DQ19_IND == 1, 1, 0)
 df2$DQ20 <- ifelse(df2$DQ20 == 1, 1,
                    ifelse(df2$DQ20 == 2, 0, NA))
 df2$DQ20_IND <- ifelse(df2$DQ20_IND == 1, 1, 0)
 df2$DQ20_ISRa <- ifelse(df2$DQ20_ISRa == 1, 1, 0)
 df2$DQ20_ISRb <- ifelse(df2$DQ20_ISRb == 1, 1, 0)
 df2$DQ21 <- ifelse(df2$DQ21 == 1, 1,
                    ifelse(df2$DQ21 == 2, 0, NA))
 df2$DQ22 <- ifelse(df2$DQ22 == 1, 1,
                    ifelse(df2$DQ22 == 2, 0, NA))
 df2$DQ23 <- ifelse(df2$DQ23 == 1, 1,
                    ifelse(df2$DQ23 == 2, 0, NA))
 df2$DQ24 <- ifelse(df2$DQ24 == 1, 1,
                    ifelse(df2$DQ24 == 2, 0, NA))
 df2$DQ25 <- ifelse(df2$DQ25 == 1, 1,
                    ifelse(df2$DQ25 == 2, 0, NA))
 df2$DQ26 <- ifelse(df2$DQ26 == 1, 1,
                    ifelse(df2$DQ26 == 2, 0, NA))
 df2$DQ27 <- ifelse(df2$DQ27 == 1, 1,
                    ifelse(df2$DQ27 == 2, 0, NA))
 df2$DQ28 <- ifelse(df2$DQ28 == 1, 1,
                    ifelse(df2$DQ28 == 2, 0, NA))
 df2$DQ29 <- ifelse(df2$DQ29 == 1, 1,
                    ifelse(df2$DQ29 == 2, 0, NA))
 
 
#********************************##
#**** Diet Quality Indicators***###
#*******************************##


# create a function to create a diet indicators;


diet_quality_indicators <- function(df) {
  
  #Country_name <- rlang::parse_expr(quo_name(enquo(Country_name)))

#****************
# 1. MDD-W 
#****************

# Subscores 
  
# White roots and tubers
 
  df$WRT = df$DQ3

   # Grains;
  
  df$Grains <- ifelse(df$DQ1 ==1 | df$DQ2 ==1, 1,
                       ifelse(is.na(df$DQ1) | is.na(df$DQ2), NA, 0))
  

# 1. Staple foods maded from Grains, whole grains,  white roots and tubers

df$GWRTP <- ifelse(df$DQ1 ==1 | df$DQ2 ==1 | df$DQ3 ==1, 1,
                    ifelse(is.na(df$DQ1) | is.na(df$DQ2) | is.na(df$DQ3), NA, 0))
          
# 2. Pulses (beans, peas and lentils):  DQ4

df$pulses = df$DQ4

# 3. Nuts and seeds: DQ21 

df$nutseeds = df$DQ21

# 4. Dairy	

df$dairy = ifelse2(df$DQ14 == 1 | df$DQ15 == 1| df$DQ25 == 1, 1, 0)

 # 5. Meat, poultry and fish	

df$MPF = ifelse2(df$DQ16 == 1 | df$DQ16_IND == 1 | df$DQ17 == 1	| df$DQ17_INDa == 1 | df$DQ17_INDb == 1 | df$DQ18 == 1 | df$DQ18_INDa == 1 | df$DQ18_INDb == 1 | df$DQ19 == 1 | df$DQ19_IND == 1	| df$DQ20
== 1 | df$DQ20_IND == 1 | df$DQ20_ISRa == 1 | df$DQ20_ISRb == 1, 1, 0)	 

# 6. Eggs
df$eggs = df$DQ13

# 7. Dark green leafy vegetables	

df$dglv = ifelse2(df$DQ6a == 1 |  df$DQ6b ==1, 1, 0) 

# 8. Other vitamin A-rich fruits and vegetables

df$vafv = ifelse2(df$DQ5 ==1	| df$DQ8 ==1, 1, 0)	

# 9. Other vegetables

df$otherveg = ifelse2(df$DQ7a ==1 | df$DQ7b == 1 | df$DQ7c == 1, 1, 0)

# 10. Other fruits 	
df$otherfruit_MDD = ifelse2(df$DQ9 ==1 | df$DQ10a == 1 | df$DQ10b ==1 | df$DQ10c ==1, 1, 0 )	 


# total FGDS 
df$FGDS = df$GWRTP + df$pulses +df$nutseeds + df$dairy + df$MPF + df$eggs + df$dglv + df$vafv + df$otherveg + df$otherfruit_MDD

# Creating minimum dietary diversity for women 15 to 49 years. A score of 5 or more indicates higher likehood of adequate  micronutrient intake  

df$MDD_W = ifelse(df$FGDS > 4 & df$Sex == "Female" & df$age < 50, 1,
                   ifelse(df$FGDS < 5 & df$Sex == "Female" & df$age < 50, 0, NA))

#**************
# 10.	F&V score (provisional; cutoff not globally validated)  
#**************

#******A score of 3 or more indicates likelihood of consuming at 
#*****least 400g fruits and vegetables, which is a global dietary recommendation(Herforth et al. 2020)

# Vitamin A-rich orange veg

df$vaveg = ifelse2(df$DQ5 ==1, 1, 0)

# Dark green leafy vegetables: use df$dglv

# Other vegetables: use df$otherveg

# Vitamin A-rich fruits		

df$vafruit = ifelse2(df$DQ8 ==1, 1, 0)

# Citrus		
df$citrus = ifelse2(df$DQ9 ==1, 1, 0)


# Other fruits 		

df$otherfruit_fv_fib = ifelse2(df$DQ10a ==1 |df$DQ10b ==1 | df$DQ10c == 1, 1, 0)


# Total FV score 

df$FV_score = df$vaveg + df$dglv + df$otherveg + df$vafruit + df$citrus + df$otherfruit_fv_fib

#*****************
# 11. Fiber score (provisional; cutoff not globally validated)

#A score of 4 or more indicates likelihood of consuming at least 25g fiber, which is a
#global dietary recommendation. (Herforth et al. 2020)
# Whole grains

df$wholegrain = ifelse2(df$DQ2 ==1, 1, 0)

# Legumes	
df$legume_fiber = ifelse2(df$DQ4 ==1, 2, 0)

# Nuts and Seeds: use df$nutseeds

# Vitamin A-rich orange veg: use df$vaveg

# Dark green leafy vegetables: use df$dglv

# Other vegetables: use df$otherveg

# Vitamin A-rich fruits: df$vafruit

# Citrus: use df$citrus

# Other fruits: use df$otherfruit_fv_fib

# Total fiber score 
df$fiber_score = df$wholegrain + 
  df$legume_fiber + 
  df$nutseeds +
  df$vaveg + 
  df$dglv + 
  df$otherveg + 
  df$vafruit +
  df$citrus +
  df$otherfruit_fv_fib

#***************
# 12.Sugar score   
#***************

# Soft drinks (sodas)	
df$soda_Sugarscore = ifelse2(df$DQ28 == 1, 2, 0)

# Fruit drinks and fruit juice	
df$fruitdrinks = ifelse2(df$DQ27 ==1, 1, 0)

# Sweetened coffee, tea, and milk drinks	
df$coffeetea = ifelse2(df$DQ26 ==1, 1, 0)

# Baked grain-based sweets	
df$grainsweets = ifelse2(df$DQ11 ==1, 1, 0)

# Other sweets	
df$othersweets = ifelse2(df$DQ12 ==1, 1, 0)

# Sweet foods
df$sweetfoods <- ifelse2(df$DQ11 == 1 | df$DQ12 == 1, 1, 0)

# Total Sugar Score
df$sugar_score = df$soda_Sugarscore  + df$fruitdrinks +  df$coffeetea + df$grainsweets + df$othersweets


#******************************
# 13. Saturated fat score  
#******************************

# Processed meat	
df$processedmeat = ifelse2(df$DQ16 ==1 | df$DQ16_IND ==1, 1, 0)

# No processed meat
df$noprocessedmeat = ifelse2(df$DQ16 ==1 | df$DQ16_IND ==1, 0, 1)

# Unprocessed red meat	
df$redmeat = ifelse2(df$DQ17 ==1 | df$DQ17_INDa ==1 | df$DQ17_INDb ==1 | df$DQ18 ==1 | df$DQ18_INDa ==1 | df$DQ18_INDb ==1, 1, 0)

# Unprocessed ruminant meat binary score for bar graph;
df$unprocessed_ruminant <- ifelse2(df$DQ17 ==1 | df$DQ17_INDa ==1 | df$DQ17_INDb ==1, 1, 0)

# Unprocessed non-ruminant meat binary score for bar graph;
df$unprocessed_nonruminant <- ifelse2(df$DQ18==1 | df$DQ18_INDa ==1 | df$DQ18_INDb ==1, 1, 0)

# Fast food	
df$fastfood = ifelse2(df$DQ29==1, 1, 0)


# Cheese and yogurt	
df$cheeseyogurt = ifelse2(df$DQ14 ==1 | df$DQ15 ==1, 1, 0)


# Creating cheese as a binary variable;
df$cheese = ifelse2(df$DQ14 ==1, 1, 0)

# Creating yogurt  as a binary variable;
df$yogurt = ifelse2(df$DQ15 ==1, 1, 0)


# Milk	
df$milk = ifelse2(df$DQ25 ==1, 1, 0)

# Other sweets: use g$othersweets

# Fish for saturated fat score 

df$fish_SAF = ifelse2(df$DQ20 ==1 | df$DQ20_IND ==1 | df$DQ20_ISRa ==1 | df$DQ20_ISRb ==1, -1, 0)

# Poultry	for saturated fat score 

df$poultry_SAF = ifelse2(df$DQ19 ==1 | df$DQ19_IND ==1, -1, 0)


# Fish and seafood to estimate the prevalence of fish and seafood consumption;
df$fishseafood = ifelse2(df$DQ20 ==1 | df$DQ20_IND ==1 | df$DQ20_ISRa ==1 | df$DQ20_ISRb ==1, 1, 0)


# Poultry variable for to estimate the prevalence of poultry consumption;

df$poultry_score = ifelse2(df$DQ19 == 1 | df$DQ19_IND == 1, 1, 0)


# Total Saturated Fat Score
df$satfat_score = df$processedmeat + 
  df$redmeat + 
  df$fastfood + 
  df$cheeseyogurt +
  df$milk + 
  df$othersweets + 
  df$fish_SAF + 
  df$poultry_SAF


#*************
# 7.  Legumes :
#*************

df$legume_score = ifelse2(df$DQ4 ==1, 1, 0)


#****************
# 8.Nuts & seeds 
#****************

df$nutseeds_score = df$nutseeds


#*****************
# 6. Whole grains 
#*****************

df$wholegrain_score = df$wholegrain


#******************
# 9.Processed meat 
#******************

df$processedmeat_score = df$processedmeat


##### Composite indicators:#####


#*********************
# 3.  GDR- Healthy component:
#*****GDR_H (Fruits, Legumes, Vegetables, Orange fruits & veg, Un-refined grains, Seeds & nuts)
#*********************

# Whole grains
df$wholegrain_GDR_H = ifelse2(df$DQ2 ==1, 1, 0)

# Legumes: use df$legume_score

# Nuts and seeds: use df$nutseeds

# vitamin A-rich orange veg: use df$vaveg

# Dark green leafy vegetables: use df$dglv

# Other vegetables: use df$otherveg

# Vitamin A-rich fruits: use df$vafruit

# Citrus: use df$citrus

# Total GDR_H Score
df$GDR_H_score = df$wholegrain_GDR_H + 
  df$legume_score +
  df$nutseeds + 
  df$vaveg +
  df$dglv + 
  df$otherveg + 
  df$vafruit + 
  df$citrus + 
  df$otherfruit_fv_fib
                   

#**************
# 4. GDR_Limit (Foods to Avoid or limit)
#**************

# Soft drinks (sodas)
#  This indicator will be used also for indicator 21. 

df$soda_GDR_L  = ifelse2(df$DQ28 ==1, 1, 0)
### ****************
### 21.Consumed soda, energy drinks, or sports drinks 

df$soda = df$soda_GDR_L

# other Sweets 
df$sweets_GDR_L = ifelse2(df$DQ12 ==1, 1, 0)

# use df$grainsweets  for Baked / grain-based sweets

#df$grainsweets = ifelse(df$DQ11 ==1, 1, 0)

# Processed meat	
df$processedmeat_GDR_L = ifelse2(df$DQ16 ==1 | df$DQ16_IND ==1, 2, 0)

#*****Unprocessed red meat: use g$redmeat

# Deep fried food
df$friedfood = ifelse2(df$DQ24 ==1, 1, 0)

##**************************************
#### 24. Consumed fast food or instant noodles

# Fast food & Instant noodles	
df$ff_instantnoodles = ifelse2(df$DQ23 ==1 | df$DQ29 ==1, 1, 0)

df$instantnoodles = ifelse2(df$DQ23 ==1, 1, 0)

# Packaged ultra-processed salty snacks
df$saltysnacks <- ifelse2(df$DQ22 == 1, 1, 0)

####*****************************
#### XX. Consumed salty snacks, instant noodles, or fast food (Salty UPF);

df$Salty_UPF <- ifelse2 (df$saltysnacks ==1 | df$instantnoodles == 1 | df$fastfood == 1, 1, 0)

# Salty or fried snack
df$SaltyFriedSnack <- ifelse2 (df$saltysnacks ==1 | df$instantnoodles == 1 | df$friedfood == 1, 1, 0)

# Total GDR_L Score: 

df$GDR_L_score = df$soda_GDR_L + 
              df$sweets_GDR_L +
              df$processedmeat_GDR_L + 
              df$redmeat + 
              df$friedfood + 
              df$ff_instantnoodles +
              df$grainsweets +
              df$saltysnacks

# C. Global dietary recommendations (GDR) score
df$GDR_score = df$GDR_H_score - df$GDR_L_score + 9

#*****************************
## Other indicators (binary)**
#******************************

df$StarchyStaple <- ifelse2(df$GWRTP == 1, 1, 0) 

### 18.At least one legume, nut or seed

df$PulseNutSeed <- ifelse2(df$pulses == 1 | df$nutseeds == 1, 1, 0)


df$NoPulseNutSeed <- ifelse2(df$pulses == 0 & df$nutseeds == 0, 1, 0)

###********************
#### 16.At least one vegetable

df$OneVegetable <- ifelse2(df$dglv == 1 | df$vaveg == 1 | df$otherveg == 1, 1, 0)

###************************
##  17. At least one fruit

df$OneFruit <- ifelse2(df$vafruit == 1 | df$otherfruit_MDD == 1, 1, 0)

###************************
## At least one fruit or vegetable
df$ForV <- ifelse2(df$OneFruit == 1 | df$OneVegetable == 1, 1, 0)

###**********************************
####   20. Zero fruits or vegetables

df$NoFV <- ifelse2(df$OneFruit == 0 & df$OneVegetable == 0, 1, 0)


df$OneProtein <- ifelse2(df$eggs == 1 | df$dairy == 1 | df$DQ17 == 1	| df$DQ17_INDa == 1 | df$DQ17_INDb == 1 | df$DQ18 == 1 | df$DQ18_INDa == 1 | df$DQ18_INDb == 1 | df$DQ19 == 1 | df$DQ19_IND == 1	| df$DQ20
                         == 1 | df$DQ20_IND == 1 | df$DQ20_ISRa == 1 | df$DQ20_ISRb == 1
                        | df$PulseNutSeed == 1, 1, 0)


df$no_Protein <- ifelse2(df$OneProtein ==0, 1, 0)

  #***************************************
### 19.Consumed animal-source food (ASF)


df$ASF <- ifelse2(df$DQ17 == 1	| df$DQ17_INDa == 1 | df$DQ17_INDb == 1 | df$DQ18 == 1 | df$DQ18_INDa == 1 | df$DQ18_INDb == 1 | df$DQ19 == 1 | df$DQ19_IND == 1 | df$eggs == 1 | df$dairy == 1 | df$fishseafood  == 1, 1, 0) 

df$no_ASF <- ifelse2(df$ASF == 0, 1, 0)



df$FBDG_3 <- ifelse2(df$OneVegetable == 1 & df$OneFruit == 1 & df$OneProtein == 1, 1, 0)

df$All5 <- ifelse2(df$OneVegetable == 1 & df$OneFruit == 1 & df$PulseNutSeed  == 1 & df$StarchyStaple == 1 & df$ASF ==1, 1, 0)

df$All5_score <- df$OneVegetable + df$OneFruit +  df$PulseNutSeed + df$StarchyStaple + df$ASF 

#### 22. Consumed any sugar-sweetened beverage (SSB)


df$SSB <- ifelse2(df$DQ26 == 1 | df$DQ27 == 1 | df$DQ28 == 1, 1, 0)

# Create F&V indicator (FV ?? 400)
df$FV_400 <- ifelse2(df$FV_score > 2, 1, 0)

# Create Fiber indicator (Fib ?? 25)
df$Fib_25 <- ifelse2(df$fiber_score > 3, 1, 0)

# Create Sugar indicator (Sug > 10% of energy)
df$Sug_10 <- ifelse2(df$sugar_score > 1, 1, 0)

# Create sug <10% energy indicator
# Create Sugar indicator (Sug > 10% of energy)
df$LessSug_10 <- ifelse2(df$sugar_score > 1, 0, 1)

# Create Saturated fat indicator (SF > 10% of energy)
df$SF_10 <- ifelse2(df$satfat_score > 1, 1, 0)

# Create Global dietary recommendations indicator (GDR) are not found in the recent analytical guide 

# Create DQQ score
df$DQQ_score <- df$GDR_score + df$All5 + df$FGDS

df$Residence <- ifelse(df$DEGURBA_F2F == 1 | df$DEGURBA_F2F == 2, "Urban",
                       ifelse(df$DEGURBA_F2F == 3, "Rural", NA))
df$Residence <- ifelse(is.na(df$Residence),
                       ifelse(df$WP7572 == 1, "Urban",
                       ifelse(df$WP7572 == 2, "Rural", NA)), df$Residence)
df$Residence <- ifelse(is.na(df$Residence),
                       ifelse(df$WP14 == 1 | df$WP14 == 2, "Rural",
                              ifelse(df$WP14 == 3 | df$WP14 == 6, "Urban", NA)), df$Residence)


# List the output of the function;

 var <- list(WRT = df$WRT, Grains = df$Grains, GWRTP = df$GWRTP, pulses = df$pulses,  nutseeds=df$nutseeds, 
           dairy = df$dairy, MPF = df$MPF, eggs = df$eggs, dglv = df$dglv, vafv = df$vafv,
          otherveg = df$otherveg, otherfruit_fv_fib = df$otherfruit_fv_fib, otherfruit_MDD = df$otherfruit_MDD, FGDS = df$FGDS, MDD_W = df$MDD_W,
          vaveg = df$vaveg, vafruit = df$vafruit, citrus = df$citrus, FV_score = df$FV_score, wholegrain = df$wholegrain, fiber_score = df$fiber_score,
           fruitdrinks  = df$fruitdrinks, coffeetea = df$coffeetea, grainsweets = df$grainsweets,
           othersweets = df$othersweets, sweetfoods = df$sweetfoods, sugar_score = df$sugar_score, processedmeat = df$processedmeat, noprocessedmeat = df$noprocessedmeat, redmeat = df$redmeat, unprocessed_ruminant = df$unprocessed_ruminant,
           unprocessed_nonruminant = df$unprocessed_nonruminant, fastfood = df$fastfood, cheeseyogurt = df$cheeseyogurt, cheese  = df$cheese, yogurt = df$yogurt, milk = df$milk, 	
           fishseafood  = df$fishseafood,  poultry_score  = df$poultry_score, satfat_score = df$satfat_score, legume_score = df$legume_score, nutseeds_score = df$nutseeds_score,
             processedmeat_score = df$processedmeat_score,  GDR_H_score = df$GDR_H_score,
            soda_GDR_L = df$soda_GDR_L, soda = df$soda, sweets_GDR_L = df$sweets_GDR_L, processedmeat_GDR_L = df$processedmeat_GDR_L, friedfood = df$friedfood, ff_instantnoodles = df$ff_instantnoodles,
            instantnoodles = df$instantnoodles, Salty_UPF = df$Salty_UPF, saltysnacks = df$saltysnacks, GDR_L_score = df$GDR_L_score, GDR_score = df$GDR_score, StarchyStaple = df$StarchyStaple,
            PulseNutSeed  = df$PulseNutSeed, NoPulseNutSeed = df$NoPulseNutSeed, OneVegetable = df$OneVegetable, OneFruit = df$OneFruit, ForV = df$ForV, NoFV = df$NoFV, OneProtein = df$OneProtein, ASF  = df$ASF, FBDG_3 = df$FBDG_3, All5 = df$All5,  All5_score = df$All5_score,
          no_Protein = df$no_Protein, no_ASF = df$no_ASF,  SSB  = df$SSB,  FV_400 = df$FV_400, Fib_25 = df$Fib_25, Sug_10 = df$Sug_10, LessSug_10 = df$LessSug_10, SF_10 = df$SF_10, DQQ_score = df$DQQ_score, SaltyFriedSnack = df$SaltyFriedSnack, DQ1 = df$DQ1, DQ2 = df$DQ2, DQ3 = df$DQ3, DQ4 = df$DQ4, DQ5 = df$DQ5, DQ6a = df$DQ6a, DQ6b = df$DQ6b, DQ7a = df$DQ7a, DQ7b = df$DQ7b, DQ7c = df$DQ7c, DQ8 = df$DQ8, DQ9 = df$DQ9, DQ10a = df$DQ10a, DQ10b = df$DQ10b, DQ10c = df$DQ10c, DQ11 = df$DQ11, DQ12 = df$DQ12, DQ13 = df$DQ13, DQ13_IND = df$DQ13_IND, DQ14 = df$DQ14, DQ15 = df$DQ15, DQ16 = df$DQ16, DQ16_IND = df$DQ16_IND, DQ17 = df$DQ17, DQ17_INDa = df$DQ17_INDa, DQ17_INDb = df$DQ17_INDb, DQ18 = df$DQ18, DQ18_INDa = df$DQ18_INDa, DQ18_INDb = df$DQ18_INDb, DQ19 = df$DQ19, DQ19_IND = df$DQ19_IND, DQ20 = df$DQ20, DQ20_IND = df$DQ20_IND, DQ20_ISRa = df$DQ20_ISRa, DQ20_ISRb = df$DQ20_ISRb, DQ21 = df$DQ21, DQ22 = df$DQ22, DQ23 = df$DQ23, DQ24 = df$DQ24, DQ25 = df$DQ25, DQ26 = df$DQ26, DQ27 = df$DQ27, DQ28 = df$DQ28, DQ29 = df$DQ29, Sex = df$Sex, FS = df$FS, GL = df$GL, Residence = df$Residence, COUNTRYNEW = df$COUNTRYNEW)
 
 return(var)

}


# Run the function to create the diet quality indicators and create a dataframe 


df_INDi <- diet_quality_indicators(df2)

# create a data frame;

df_indi <- data.frame(df_INDi)

#**********************************************************##
#********* Merge, add labels, and save data       **** ###
#*********************************************************###

# add variable labels #

Indicators  = apply_labels(df_indi, 
                       WRT = "White roots or tubers",
                       GWRTP = "Grains, white roots and tubers, and plantains",
                       Grains = "Foods made from grains",             
                       pulses = "Pulses",
                       nutseeds = "Nuts or seeds",          
                       dairy = "Dairy", 
                       unprocessed_ruminant = "Unprocessed red meat (ruminants)",
                       unprocessed_nonruminant = "Unprocessed red meat (non-ruminants)",
                       cheese = "Cheese",
                       yogurt = "Yogurt",
                       MPF = "Meat, poultry, or fish",              
                       eggs = "Eggs",              
                       dglv = "Dark green leafy vegetables",              
                       vafv = "Other vitamin A-rich fruits and vegetables",              
                       otherveg = "Other vegetables", 
                       otherfruit_fv_fib = "Other fruits",
                       otherfruit_MDD = "Other fruit including citrus (for MDD-W)",  
                       FGDS = "Food group diversity score",
                       MDD_W = "MDD-W",
                       vaveg = "Vitamin A-rich orange vegetables",             
                       vafruit = "Vitamin A-rich fruits",            
                       citrus = "Citrus",
                       fishseafood  = "Fish or seafood",
                       poultry_score  = "Poultry",
                       FV_score = "Fruit and Vegetable Score (0-6)",
                       wholegrain = "Whole grains",
                       fiber_score = "Fiber score (0-10)",
                       soda = "Sugar-sweetened soft drink consumption",             
                       fruitdrinks = "Fruit juice and fruit drinks",       
                       coffeetea =  "Sweet tea, coffee, or cocoa",        
                       grainsweets = "Baked or grain-based sweets",     
                       othersweets = "Other sweets",
                       sweetfoods = "Sweet foods",
                       sugar_score = "Sugar Score (0-6)",
                       processedmeat = "Processed meats",
                       noprocessedmeat = "No processed meat",
                       redmeat = "Unprocessed red meat",            
                       fastfood = "Fast food",          
                       cheeseyogurt = "Cheese and yogurt",       
                       milk = "Milk",               
                       satfat_score = "Saturated Fat Score (-2-6)",
                       legume_score = "Pulses",
                       nutseeds_score = "Nuts and Seeds Score (0-1)",
                       processedmeat_score = "Processed Meat Score (0-1)",
                       GDR_H_score = "NCD-Protect",
                       instantnoodles = "Instant noodles",
                       friedfood = "Deep fried foods",           
                       ff_instantnoodles = "Fast food and instant noodles",
                       saltysnacks = "Packaged ultra-processed salty snacks", 
                       Salty_UPF = "Salty snacks, instant noodles, or fast food",
                       GDR_L_score = "NCD-Risk",
                       GDR_score = "GDR score",
                       StarchyStaple = "At least one starchy staple food",
                       PulseNutSeed = "At least one pulse, nut, or seed",
                       NoPulseNutSeed = "Zero pulses, nuts, or seeds",
                       OneVegetable = "At least one vegetable",
                       OneFruit = "At least one fruit",
                       ForV = "At least one fruit or vegetable",
                       NoFV = "Zero vegetable or fruit consumption",
                       OneProtein = "At least one protein",
                       ASF = "At least one animal-source food",
                       FBDG_3 = "At least one vegetable, fruit, and protein",
                       All5 = "All-5",
                       All5_score = "All-5 Score (0-5)",
                       SSB = "Sugar-sweetened beverages",
                       FV_400 = "At least 400 g of fruits and vegetables",
                       Fib_25 = "At least 25 g of fiber",
                       Sug_10 = "More than 10% of energy from free sugars",
                       LessSug_10 = "Less than 10% of energy from free sugars",
                       SF_10 = "More than 10% of energy from saturated fats",
                       no_Protein  = "Zero protein-rich foods",
                       no_ASF = "Zero animal-source foods",
                       DQQ_score = "DQQ Score (0-33)",
                       SaltyFriedSnack = "Salty or fried snacks",
                       DQ1 = "Ate Staple Foods Made From Grains Yesterday",
                       DQ2 = "Ate Whole Grains Yesterday",
                       DQ3 = "Ate White Roots/Tubers Yesterday", 
                       DQ4 = "Ate Legumes Yesterday", 
                       DQ5 = "Ate Vitamin A-Rich Orange Vegetables Yesterday", 
                       DQ6a = "Ate Dark Green Leafy Vegetables (First List) Yesterday", 
                       DQ6b = "Ate Dark Green Leafy Vegetables (Second List) Yesterday", 
                       DQ7a = "Ate Other Types of Vegetables (First List) Yesterday", 
                       DQ7b = "Ate Other Types of Vegetables (Second List) Yesterday", 
                       DQ7c = "Ate Other Types of Vegetables (Third List) Yesterday", 
                       DQ8 = "Ate Vitamin A-Rich Fruits Yesterday", 
                       DQ9 = "Ate Citrus Fruits Yesterday", 
                       DQ10a = "Ate Other Types of Fruits (First List) Yesterday", 
                       DQ10b = "Ate Other Types of Fruits (Second List) Yesterday", 
                       DQ10c = "Ate Other Types of Fruits (Third List) Yesterday", 
                       DQ11 = "Ate Baked Sweets Yesterday", 
                       DQ12 = "Ate Other Types of Sweets Yesterday", 
                       DQ13 = "Ate Eggs Yesterday",
                       DQ13_IND = "Ate Eggs Yesterday (India)",
                       DQ14 = "Ate Cheese Yesterday", 
                       DQ15 = "Ate Yogurt Yesterday", 
                       DQ16 = "Ate Processed Meat Yesterday",
                       DQ16_IND = "Ate Processed Meat Yesterday (India)", 
                       DQ17 = "Ate Unprocessed Red Meat-Ruminant Yesterday", 
                       DQ17_INDa = "Ate Unprocessed Red Meat-Ruminant Yesterday (India a)", 
                       DQ17_INDb = "Ate Unprocessed Red Meat-Ruminant Yesterday (India b)", 
                       DQ18 = "Ate Unprocessed Red Meat-Non-Ruminant Yesterday", 
                       DQ18_INDa = "Ate Unprocessed Red Meat-Non-Ruminant Yesterday (India a)", 
                       DQ18_INDb = "Ate Unprocessed Red Meat-Non-Ruminant Yesterday (India b)", 
                       DQ19 = "Ate Poultry Yesterday", 
                       DQ19_IND = "Ate Poultry Yesterday (India)", 
                       DQ20 = "Ate Fish and Seafood Yesterday", 
                       DQ20_IND = "Ate Fish and Seafood Yesterday (India)", 
                       DQ20_ISRa = "Ate Fish and Seafood Yesterday (Israel a)", 
                       DQ20_ISRb = "Ate Fish and Seafood Yesterday (Israel b)", 
                       DQ21 = "Ate Nuts and Seeds Yesterday", 
                       DQ22 = "Ate Ultra-Processed Packaged Salty Snacks Yesterday", 
                       DQ23 = "Ate Instant Noodles Yesterday", 
                       DQ24 = "Ate Deep Fried Foods Yesterday", 
                       DQ25 = "Drank Fluid Milk Yesterday", 
                       DQ26 = "Drank Sweetened Tea/Coffee/Milk Drinks Yesterday", 
                       DQ27 = "Drank Fruit Juice Yesterday", 
                       DQ28 = "Drank SSBs/Sodas Yesterday", 
                       DQ29 = "Got Food From Fast Food Place Yesterday",
                       Residence = "Residence",
                       Sex = "Sex",
                       FS = "Food security",
                       GL = "Good life",
                       COUNTRYNEW = "COUNTRYNEW") # I keep  COUNTRYNEW as it is because there is another variable named Country 
                    

# merge back with original data set that contain socio-demographic and economic variables

df.done <- cbind(df1, Indicators) 
         
  
# export dataset 

write_csv(df.done, "data/Individual observations.csv")

write_sav(df.done, "data/Individual observations.sav")



