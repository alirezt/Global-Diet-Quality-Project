library(haven)
library(expss)
library(tidyverse)
library(foreign)
library(survey)
library(plotrix)
library(labelled)

# 1. Check for 2022 entries in 2021 survey ----
d_gal2021= read.spss("Data/Input/Sav/2021/2021 Gallup World Poll data for public release.sav", use.value.labels=F, to.data.frame=TRUE, stringsAsFactors=FALSE)
glimpse(d_gal2021) #46,646 
d_gal2021$FieldDate <- format(as.Date(d_gal2021$FieldDate/86400, origin = "1582-10-14"), "%Y")

d_gal2021 %>%
  filter(FieldDate == "2022") %>%
  group_by(Country)%>%
  summarise(n = n()) # except for Bangladesh, Colombia and Mexico, all entries belong to year 2021. 

# 2. select 46,646 entries from 2022 data ----
# 'd' has already constructed in train.R
d_complete <- read_csv("Data/Output/CSV/d_complete.csv")
d21 <- data.frame(d_complete %>% filter(FieldDate == "2021" | Country %in% c("Bangladesh", "Colombia", "Mexico"))) # 46,646
dim(d21) # 46,646 x 100

result1 <- setNames(data.frame(matrix(ncol = 14, nrow = 1)), c("Income_group", "Region", "Country", "ISO3", "Subgroup", "Variable", "Variable_label", "Mean_Prevalence", "LCI", "UCI", "Difference", "Diff_LCI", "Diff_UCI", "Diff_p"))
result2 <- setNames(data.frame(matrix(ncol = 14, nrow = 1)), c("Income_group", "Region", "Country", "ISO3", "Subgroup", "Variable", "Variable_label", "Mean_Prevalence", "LCI", "UCI", "Difference", "Diff_LCI", "Diff_UCI", "Diff_p"))
options(survey.lonely.psu="adjust")

# 3. Complex Survey design ----
for(k in unique(d21$Country)){
  curcountry <- k
  curdat <- d21[d21$Country == curcountry, ]
  # Overall
  # Create weighted object
  ifelse(is.na(unique(curdat$PSU)), d_w <- svydesign(ids = ~WPID, strata = ~STRATA, nest = FALSE, weights = ~Weight, data =  curdat), 
         d_w <- svydesign(ids = ~PSU, strata = ~STRATA, nest = TRUE, weights = ~Weight, data =  curdat))
  
  # By Sex
  # Male
  
  d_m <- curdat[curdat$Gender == "Male", ]
  
  # Create weighted object
  ifelse(is.na(unique(d_m$PSU)), d_m_w <- svydesign(ids = ~WPID, strata = ~STRATA, nest = FALSE, weights = ~Weight, data = d_m), 
         d_m_w <- svydesign(ids = ~PSU, strata = ~STRATA, nest = TRUE, weights = ~Weight, data = d_m))
  
  
  # Female
  d_f <- curdat[curdat$Gender == "Female", ]
  
  # Create weighted object
  ifelse(is.na(unique(d_f$PSU)), d_f_w <- svydesign(ids = ~WPID, strata = ~STRATA, nest = FALSE, weights = ~Weight, data = d_f), 
         d_f_w <- svydesign(ids = ~PSU, strata = ~STRATA, nest = TRUE, weights = ~Weight, data = d_f))
  
  
  # By Residence
  # Urban
  #d_u <- curdat[curdat$Residence == "Urban", ]
  d_u <- curdat[which(curdat$Residence == "Urban"), ]
  
  # Create weighted object
  ifelse(is.na(unique(d_u$PSU)), d_u_w <- svydesign(ids = ~WPID, strata = ~STRATA, nest = FALSE, weights = ~Weight, data = d_u), 
         d_u_w <- svydesign(ids = ~PSU, strata = ~STRATA, nest = TRUE, weights = ~Weight, data = d_u))
  
  
  # subset the dataset for rural subgroup;
  
  d_r <- curdat[which(curdat$Residence == "Rural"), ]
  
  #Create weighted object for rural subrgoup
  ifelse(is.na(unique(d_r$PSU)), d_r_w <- svydesign(ids = ~WPID, strata = ~STRATA, nest = FALSE, weights = ~Weight, data = d_r), 
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
                                attributes(d21)$names[i], 
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
                                attributes(d21)$names[i], 
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
                                attributes(d21)$names[i], 
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
                                attributes(d21)$names[i], 
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
                                attributes(d21)$names[i], 
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
                                attributes(d21)$names[i], 
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
                                attributes(d21)$names[i], 
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
                                attributes(d21)$names[i], 
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
                                attributes(d21)$names[i], 
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
                                attributes(d21)$names[i], 
                                round(y, digits = 4), 
                                round(y_LCI, digits = 4), 
                                round(y_UCI, digits = 4), 
                                round(x-y, digits = 4), 
                                NA, 
                                NA, 
                                ifelse(is.na(t), NA, as.numeric(t$p.value))))
    
  }
}

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

# 4. Replacing names ----

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

# 5. Cleaning and exporting ----
results$Mean_Prevalence <- as.numeric(results$Mean_Prevalence)
results$LCI <- as.numeric(results$LCI)
results$UCI <- as.numeric(results$UCI)

results <- results %>%
  select(!c(Variable_label, Difference, Diff_LCI, Diff_UCI, Diff_p)) %>%
  mutate(
    Mean_Prevalence = ifelse(Variable == "fgds" | Variable == "ncdp" | Variable == "ncdr" | Variable == "gdr",
                             Mean_Prevalence, round(Mean_Prevalence*100, digits = 2)),
    LCI = ifelse(Variable == "fgds" | Variable == "ncdp" | Variable == "ncdr" | Variable == "gdr",
                 LCI, round(LCI*100, digits = 2)),
    UCI = ifelse(Variable == "fgds" | Variable == "ncdp" | Variable == "ncdr" | Variable == "gdr",
                 UCI, round(UCI*100, digits = 2)),
    "DQQ Names" = as.character(dqqNames[results$Variable]),
    Variable = as.character(longNames[results$Variable])
  ) %>%
  relocate("DQQ Names", .after = Variable) %>%
  rename("Income classification" = Income_group, Indicator = Variable, "Mean/Prevalence" = Mean_Prevalence,
         "Lower confidence interval" = LCI, "Upper confidence interval" = UCI)

head(results)
glimpse(results)
write_csv(results, "Data/Output/CSV/results2021.csv")

# End ----
