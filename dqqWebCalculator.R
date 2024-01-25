# Load the libraries
library(tidyverse)
library(janitor)

# Import the data
 # Paste your file path in your working directory 
example <- read_csv("Data/raw/dqq-example.csv", lazy = FALSE)

# Main function
dqqWebCalculator <- function(data) {
  
  foodGroup <- function(data) {
    data <- data %>% 
      rename(
        CaseID = "Respondent_ID", 
        Gender = "Gender_Female_1_Male_0", 
        Urbanicity = "Locality_Urban_1_Rural_0", 
        Age = "Age_years",
        DQQ1 = "01_Foods_made_from_grains", 
        DQQ2 = "02_Whole_grains", 
        DQQ3 = "03_White_roots_or_tubers", 
        DQQ4 = "04_Pulses", 
        DQQ5 = "05_Vitamin_A-rich_orange_vegetables", 
        DQQ6 = "06_Dark_green_leafy_vegetables", 
        DQQ7 = "07_Other_vegetables",  
        DQQ8 = "08_Vitamin_A-rich_fruits", 
        DQQ9 = "09_Citrus", 
        DQQ10 = "10_Other_fruits", 
        DQQ11 = "11_Baked_or_grain-based_sweets", 
        DQQ12 = "12_Other_sweets", 
        DQQ13 = "13_Eggs", 
        DQQ14 = "14_Cheese", 
        DQQ15 = "15_Yogurt", 
        DQQ16 = "16_Processed_meats", 
        DQQ17 = "17_Unprocessed_red_meat_ruminant", 
        DQQ18 = "18_Unprocessed_red_meat_non-ruminant", 
        DQQ19 = "19_Poultry", 
        DQQ20 = "20_Fish_or_seafood", 
        DQQ21 = "21_Nuts_or_seeds", 
        DQQ22 = "22_Packaged_ultra-processed_salty_snacks", 
        DQQ23 = "23_Instant_noodles", 
        DQQ24 = "24_Deep_fried_foods", 
        DQQ25 = "25_Milk", 
        DQQ26 = "26_Sweet tea_coffee_or_cocoa", 
        DQQ27 = "27_Fruit_juice_or_fruit_drinks", 
        DQQ28 = "28_Soft_drinks", 
        DQQ29 = "29_Fast_food" 
      ) %>%
      mutate(
        ncdp = ((rowSums(pick("DQQ2") == 1, na.rm=TRUE) > 0) + 
                  (rowSums(pick("DQQ4") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ21") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ5") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ6") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ7") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ8") == 1, na.rm=TRUE) > 0)+
                  (rowSums(pick("DQQ9") == 1, na.rm=TRUE) > 0)+
                  (rowSums(pick("DQQ10") == 1, na.rm=TRUE) > 0)),
        
        ncdr = ((rowSums(pick("DQQ28") == 1, na.rm=TRUE) > 0) + 
                  (rowSums(pick("DQQ11") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ12") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ16") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ16") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ17","DQQ18") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ24") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ23", "DQQ29") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ22") == 1, na.rm=TRUE) > 0)),
        
        gdr = (ncdp - ncdr + 9), 
        
        all5a = ifelse((rowSums(pick("DQQ5", "DQQ6", "DQQ7") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5b = ifelse((rowSums(pick("DQQ8","DQQ9", "DQQ10") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5c = ifelse((rowSums(pick("DQQ4","DQQ21") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5d = ifelse((rowSums(pick("DQQ13", "DQQ14", "DQQ15", "DQQ16", "DQQ17", "DQQ18", "DQQ19", "DQQ20", "DQQ25") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5e = ifelse((rowSums(pick("DQQ1","DQQ2", "DQQ3") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5 = ifelse((all5a + all5b + all5c + all5d + all5e) == 5, 1, 0),
        
        fgds = ((rowSums(pick("DQQ1","DQQ2", "DQQ3") == 1, na.rm=TRUE) > 0) + 
                  (rowSums(pick("DQQ4") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ21") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ14","DQQ15", "DQQ25") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ16","DQQ17", "DQQ18", "DQQ19","DQQ20") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ13") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ6") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ5","DQQ8") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ7") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ9","DQQ10") == 1, na.rm=TRUE) > 0)),
        
        mddw = ifelse((fgds >= 5 & Gender == 1 &  Age >= 15 & Age <= 49), 1, 
                      ifelse((fgds < 5 & Gender == 1 &  Age >= 15 & Age <= 49), 0, NA)),
        zvegfr = ifelse((rowSums(pick("DQQ5","DQQ6", "DQQ7", "DQQ8", "DQQ9", "DQQ10") == 1, 
                                 na.rm=TRUE) > 0) == TRUE, 0, 1),
        vegfr = ifelse((rowSums(pick("DQQ5","DQQ6", "DQQ7", "DQQ8", "DQQ9", "DQQ10") == 1, 
                                na.rm=TRUE) > 0) == TRUE, 1, 0),
        safd = ifelse((rowSums(pick("DQQ22","DQQ23","DQQ24") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        swtfd = ifelse((rowSums(pick("DQQ11", "DQQ12") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        # extra 
        swtbev = ifelse((rowSums(pick("DQQ26","DQQ27","DQQ28") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        snf = ifelse((rowSums(pick("DQQ22","DQQ23","DQQ29") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        dairy = ifelse((rowSums(pick("DQQ14","DQQ15","DQQ25") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        dveg = ifelse((rowSums(pick("DQQ6") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        anml = ifelse((rowSums(pick("DQQ16", "DQQ17","DQQ18", "DQQ19", "DQQ20") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        ofr = ifelse((rowSums(pick("DQQ10") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        oveg = ifelse((rowSums(pick("DQQ7") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        umeat = ifelse((rowSums(pick("DQQ17","DQQ18") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5e = ifelse((rowSums(pick("DQQ1","DQQ2", "DQQ3") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)
      )
  }
  dataComplete <- foodGroup(data)
  
  Gender <- fct_collapse(as.factor(dataComplete$Gender), "Female" = "1", "Male" = "0")
  dataComplete$Gender <- Gender
  
  Urbanicity <- fct_collapse(as.factor(dataComplete$Urbanicity), "Urban" = "1", "Rural" = "0")
  dataComplete$Urbanicity <- Urbanicity
  
  dat <- dataComplete %>% 
    #pivot_longer(cols = DQQ1:swtfd, names_to = "Indicator", values_to = "Value") %>%
    pivot_longer(cols = c("Gender", "Urbanicity"), names_to = "Sub", values_to = "Subgroup") %>%
    select(!Sub) %>%
    group_by(Subgroup) %>%
    reframe(
      "GDR score" = round(mean(gdr, na.rm = TRUE), digits = 2),
      "NCD-Protect" = round(mean(ncdp, na.rm = TRUE), digits = 2),
      "NCD-Risk" = round(mean(ncdr, na.rm = TRUE), digits = 2),
      "All-5" = round(mean(all5 == 1, na.rm = TRUE)*100, digits = 2),
      "At least one vegetable" = round(mean(all5a == 1, na.rm = TRUE)*100, digits = 2),
      "At least one fruit" = round(mean(all5b == 1, na.rm = TRUE)*100, digits = 2),
      "At least one pulse, nut, or seed" = round(mean(all5c == 1, na.rm = TRUE)*100, digits = 2),
      "At least one animal-source food" = round(mean(all5d == 1, na.rm = TRUE)*100, digits = 2),
      "At least one starchy staple food" = round(mean(all5e == 1, na.rm = TRUE)*100, digits = 2),
      "MDD-W" = round(mean(mddw == 1, na.rm = TRUE)*100, digits = 2),
      "Dietary diversity score" = round(mean(fgds, na.rm = TRUE), digits = 2),
      "Zero vegetable or fruit consumption" = round(mean(zvegfr == 1, na.rm = TRUE)*100, digits = 2),
      "At least one vegetable or fruit" = round(mean(vegfr == 1, na.rm = TRUE)*100, digits = 2),
      "Pulse consumption" = round(mean(DQQ4 == 1, na.rm = TRUE)*100, digits = 2),
      "Nuts or seeds consumption" = round(mean(DQQ21 == 1, na.rm = TRUE)*100, digits = 2),
      "Whole grain consumption" = round(mean(DQQ2 == 1, na.rm = TRUE)*100, digits = 2),
      "Processed meat consumption" = round(mean(DQQ16 == 1, na.rm = TRUE)*100, digits = 2),
      "Salty snacks, instant noodles, or fast food" = round(mean(snf == 1, na.rm = TRUE)*100, digits = 2),
      "Salty or fried snack consumption" = round(mean(safd == 1, na.rm = TRUE)*100, digits = 2),
      "Deep fried foods" = round(mean(DQQ24 == 1, na.rm = TRUE)*100, digits = 2),
      "Sweet foods consumption" = round(mean(swtfd == 1, na.rm = TRUE)*100, digits = 2),
      "Sweet beverages" = round(mean(swtbev == 1, na.rm = TRUE)*100, digits = 2),
      "Soft drink consumption" = round(mean(DQQ28 == 1, na.rm = TRUE)*100, digits = 2),
      "01 Foods made from grains" = round(mean(DQQ1 == 1, na.rm = TRUE)*100, digits = 2),
      "02 Whole grains" = round(mean(DQQ2 == 1, na.rm = TRUE)*100, digits = 2),
      "03 White roots or tubers" = round(mean(DQQ3 == 1, na.rm = TRUE)*100, digits = 2),
      "04 Pulses" = round(mean(DQQ4 == 1, na.rm = TRUE)*100, digits = 2),
      "05 Vitamin A-rich orange vegetables" = round(mean(DQQ5 == 1, na.rm = TRUE)*100, digits = 2),
      "06 Dark green leafy vegetables" = round(mean(dveg == 1, na.rm = TRUE)*100, digits = 2),
      "07 Other vegetables" = round(mean(oveg == 1, na.rm = TRUE)*100, digits = 2),
      "08 Vitamin A-rich fruits" = round(mean(DQQ8 == 1, na.rm = TRUE)*100, digits = 2),
      "09 Citrus" = round(mean(DQQ9 == 1, na.rm = TRUE)*100, digits = 2),
      "10 Other fruits" = round(mean(ofr == 1, na.rm = TRUE)*100, digits = 2),
      "11 Baked or grain-based sweets" = round(mean(DQQ11 == 1, na.rm = TRUE)*100, digits = 2),
      "12 Other sweets"= round(mean(DQQ12 == 1, na.rm = TRUE)*100, digits = 2),
      "13 Eggs" = round(mean(DQQ13 == 1, na.rm = TRUE)*100, digits = 2),
      "14 Cheese" = round(mean(DQQ14 == 1, na.rm = TRUE)*100, digits = 2),
      "15 Yogurt" = round(mean(DQQ15 == 1, na.rm = TRUE)*100, digits = 2),
      "16 Processed meats" = round(mean(DQQ16 == 1, na.rm = TRUE)*100, digits = 2),
      "17 Unprocessed red meat (ruminant)" = round(mean(DQQ17 == 1, na.rm = TRUE)*100, digits = 2),
      "18 Unprocessed red meat (non-ruminant)" = round(mean(DQQ18 == 1, na.rm = TRUE)*100, digits = 2),
      "19 Poultry" = round(mean(DQQ19 == 1, na.rm = TRUE)*100, digits = 2),
      "20 Fish or seafood" = round(mean(DQQ20 == 1, na.rm = TRUE)*100, digits = 2),
      "21 Nuts or seeds" = round(mean(DQQ21 == 1, na.rm = TRUE)*100, digits = 2),
      "22 Packaged ultra-processed salty snacks" = round(mean(DQQ22 == 1, na.rm = TRUE)*100, digits = 2),
      "23 Instant noodles" = round(mean(DQQ23 == 1, na.rm = TRUE)*100, digits = 2),
      "24 Deep fried foods" = round(mean(DQQ24 == 1, na.rm = TRUE)*100, digits = 2),
      "25 Fluid milk" = round(mean(DQQ25 == 1, na.rm = TRUE)*100, digits = 2),
      "26 Sweet tea, coffee, or cocoa" = round(mean(DQQ26 == 1, na.rm = TRUE)*100, digits = 2),
      "27 Fruit juice and fruit drinks" = round(mean(DQQ27 == 1, na.rm = TRUE)*100, digits = 2),
      "28 Soft drinks" = round(mean(DQQ28 == 1, na.rm = TRUE)*100, digits = 2),
      "29 Fast food" = round(mean(DQQ29 == 1, na.rm = TRUE)*100, digits = 2),
      "Meat, poultry, or fish" = round(mean(anml == 1, na.rm = TRUE)*100, digits = 2),
      "Unprocessed red meat" = round(mean(umeat == 1, na.rm = TRUE)*100, digits = 2),
      "Dairy" = round(mean(dairy == 1, na.rm = TRUE)*100, digits = 2),
      )
  
  datAll <- dataComplete %>% 
    group_by(Country) %>%
    reframe(
      "GDR score" = round(mean(gdr, na.rm = TRUE), digits = 2),
      "NCD-Protect" = round(mean(ncdp, na.rm = TRUE), digits = 2),
      "NCD-Risk" = round(mean(ncdr, na.rm = TRUE), digits = 2),
      "All-5" = round(mean(all5 == 1, na.rm = TRUE)*100, digits = 2),
      "At least one vegetable" = round(mean(all5a == 1, na.rm = TRUE)*100, digits = 2),
      "At least one fruit" = round(mean(all5b == 1, na.rm = TRUE)*100, digits = 2),
      "At least one pulse, nut, or seed" = round(mean(all5c == 1, na.rm = TRUE)*100, digits = 2),
      "At least one animal-source food" = round(mean(all5d == 1, na.rm = TRUE)*100, digits = 2),
      "At least one starchy staple food" = round(mean(all5e == 1, na.rm = TRUE)*100, digits = 2),
      "MDD-W" = round(mean(mddw == 1, na.rm = TRUE)*100, digits = 2),
      "Dietary diversity score" = round(mean(fgds, na.rm = TRUE), digits = 2),
      "Zero vegetable or fruit consumption" = round(mean(zvegfr == 1, na.rm = TRUE)*100, digits = 2),
      "At least one vegetable or fruit" = round(mean(vegfr == 1, na.rm = TRUE)*100, digits = 2),
      "Pulse consumption" = round(mean(DQQ4 == 1, na.rm = TRUE)*100, digits = 2),
      "Nuts or seeds consumption" = round(mean(DQQ21 == 1, na.rm = TRUE)*100, digits = 2),
      "Whole grain consumption" = round(mean(DQQ2 == 1, na.rm = TRUE)*100, digits = 2),
      "Processed meat consumption" = round(mean(DQQ16 == 1, na.rm = TRUE)*100, digits = 2),
      "Salty snacks, instant noodles, or fast food" = round(mean(snf == 1, na.rm = TRUE)*100, digits = 2),
      "Salty or fried snack consumption" = round(mean(safd == 1, na.rm = TRUE)*100, digits = 2),
      "Deep fried foods" = round(mean(DQQ24 == 1, na.rm = TRUE)*100, digits = 2),
      "Sweet foods consumption" = round(mean(swtfd == 1, na.rm = TRUE)*100, digits = 2),
      "Sweet beverages" = round(mean(swtbev == 1, na.rm = TRUE)*100, digits = 2),
      "Soft drink consumption" = round(mean(DQQ28 == 1, na.rm = TRUE)*100, digits = 2),
      "01 Foods made from grains" = round(mean(DQQ1 == 1, na.rm = TRUE)*100, digits = 2),
      "02 Whole grains" = round(mean(DQQ2 == 1, na.rm = TRUE)*100, digits = 2),
      "03 White roots or tubers" = round(mean(DQQ3 == 1, na.rm = TRUE)*100, digits = 2),
      "04 Pulses" = round(mean(DQQ4 == 1, na.rm = TRUE)*100, digits = 2),
      "05 Vitamin A-rich orange vegetables" = round(mean(DQQ5 == 1, na.rm = TRUE)*100, digits = 2),
      "06 Dark green leafy vegetables" = round(mean(dveg == 1, na.rm = TRUE)*100, digits = 2),
      "07 Other vegetables" = round(mean(oveg == 1, na.rm = TRUE)*100, digits = 2),
      "08 Vitamin A-rich fruits" = round(mean(DQQ8 == 1, na.rm = TRUE)*100, digits = 2),
      "09 Citrus" = round(mean(DQQ9 == 1, na.rm = TRUE)*100, digits = 2),
      "10 Other fruits" = round(mean(ofr == 1, na.rm = TRUE)*100, digits = 2),
      "11 Baked or grain-based sweets" = round(mean(DQQ11 == 1, na.rm = TRUE)*100, digits = 2),
      "12 Other sweets"= round(mean(DQQ12 == 1, na.rm = TRUE)*100, digits = 2),
      "13 Eggs" = round(mean(DQQ13 == 1, na.rm = TRUE)*100, digits = 2),
      "14 Cheese" = round(mean(DQQ14 == 1, na.rm = TRUE)*100, digits = 2),
      "15 Yogurt" = round(mean(DQQ15 == 1, na.rm = TRUE)*100, digits = 2),
      "16 Processed meats" = round(mean(DQQ16 == 1, na.rm = TRUE)*100, digits = 2),
      "17 Unprocessed red meat (ruminant)" = round(mean(DQQ17 == 1, na.rm = TRUE)*100, digits = 2),
      "18 Unprocessed red meat (non-ruminant)" = round(mean(DQQ18 == 1, na.rm = TRUE)*100, digits = 2),
      "19 Poultry" = round(mean(DQQ19 == 1, na.rm = TRUE)*100, digits = 2),
      "20 Fish or seafood" = round(mean(DQQ20 == 1, na.rm = TRUE)*100, digits = 2),
      "21 Nuts or seeds" = round(mean(DQQ21 == 1, na.rm = TRUE)*100, digits = 2),
      "22 Packaged ultra-processed salty snacks" = round(mean(DQQ22 == 1, na.rm = TRUE)*100, digits = 2),
      "23 Instant noodles" = round(mean(DQQ23 == 1, na.rm = TRUE)*100, digits = 2),
      "24 Deep fried foods" = round(mean(DQQ24 == 1, na.rm = TRUE)*100, digits = 2),
      "25 Fluid milk" = round(mean(DQQ25 == 1, na.rm = TRUE)*100, digits = 2),
      "26 Sweet tea, coffee, or cocoa" = round(mean(DQQ26 == 1, na.rm = TRUE)*100, digits = 2),
      "27 Fruit juice and fruit drinks" = round(mean(DQQ27 == 1, na.rm = TRUE)*100, digits = 2),
      "28 Soft drinks" = round(mean(DQQ28 == 1, na.rm = TRUE)*100, digits = 2),
      "29 Fast food" = round(mean(DQQ29 == 1, na.rm = TRUE)*100, digits = 2),
      "Meat, poultry, or fish" = round(mean(anml == 1, na.rm = TRUE)*100, digits = 2),
      "Unprocessed red meat" = round(mean(umeat == 1, na.rm = TRUE)*100, digits = 2),
      "Dairy" = round(mean(dairy == 1, na.rm = TRUE)*100, digits = 2)) %>%
    rename(Subgroup = Country) %>%
    mutate(Subgroup = "All") 
  
  dqqResults <- bind_rows(datAll, dat) %>%
    t  %>% 
    as.data.frame %>% 
    row_to_names(row_number = 1) %>%
    rownames_to_column(var = "Indicator")
  
  
  write_csv(dqqResults, "dqq-results.csv")
}

# Save csv output in local directory 
dqqWebCalculator(example)






