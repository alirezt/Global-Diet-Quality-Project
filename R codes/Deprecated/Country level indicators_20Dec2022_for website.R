library(foreign)
library(survey)
library(plotrix)
library(tidyverse)
options(survey.lonely.psu="adjust")
setwd("~/Google Drive/GAIN/Gallup/Analysis")

d <- read.spss("data/Individual observations.sav", use.value.labels=F, to.data.frame=TRUE, stringsAsFactors=FALSE)

# Remove white space at end of country name strings
d$COUNTRYNEW <- str_trim(d$COUNTRYNEW, side = "right")

# d <- d %>%
#   
#   filter(d$COUNTRYNEW == "Ghana")
# 
# d <- d %>%
#   
#   filter(d$age_cat == "15-24 Years")

d$Residence <- ifelse(d$Residence == "Urban", "Urban",
                       ifelse(d$Residence == "Rural", "Rural", NA))

d$FS <- ifelse(d$FS == "Yes", "Yes",
                      ifelse(d$FS == "No ", "No", NA))

d$GL <- ifelse(d$GL == "Yes", "Yes",
               ifelse(d$GL == "No ", "No", NA))

# Create welbeing variable
d$WB <- ifelse(d$WP16 == 98 | d$WP16 == 99, NA, d$WP16)

d$Education <- ifelse(d$WP3117 == 1, "<9 years", 
                      ifelse(d$WP3117 == 2, "9-15 years",
                             ifelse(d$WP3117 == 3, ">15 years", NA)))

# Drop obs with NA strata
#d <- d[!is.na(d$WP12258A), ]

#d <- d[d$age_cat == "15-24 Years", ]



# Add World Bank Income Group and Region
d$WBIG <- ifelse(d$COUNTRYNEW == "United States" | d$COUNTRYNEW == "Israel" | d$COUNTRYNEW == "Chile" | d$COUNTRYNEW == "Greece", "H",
                 ifelse(d$COUNTRYNEW == "Mexico" | d$COUNTRYNEW == "Jordan"| d$COUNTRYNEW == "China" | d$COUNTRYNEW == "Kazakhstan" | d$COUNTRYNEW == "Ecuador" | d$COUNTRYNEW == "Gabon" | d$COUNTRYNEW == "South Africa" | d$COUNTRYNEW == "Sri Lanka" | d$COUNTRYNEW == "Lebanon" | d$COUNTRYNEW == "Mexico" | d$COUNTRYNEW == "Russia" | d$COUNTRYNEW == "Colombia", "UM",
                        ifelse(d$COUNTRYNEW == "Ghana" | d$COUNTRYNEW == "Indonesia" | d$COUNTRYNEW == "Bangladesh" | d$COUNTRYNEW == "India" | d$COUNTRYNEW == "Vietnam" | d$COUNTRYNEW == "Cambodia" | d$COUNTRYNEW == "Laos" | d$COUNTRYNEW == "Bolivia" | d$COUNTRYNEW == "Nicaragua" | d$COUNTRYNEW == "Egypt" | d$COUNTRYNEW == "Nigeria" | d$COUNTRYNEW == "Kenya" | d$COUNTRYNEW == "Ghana" | d$COUNTRYNEW == "Senegal" | d$COUNTRYNEW == "Cameroon" | d$COUNTRYNEW == "Morocco" | d$COUNTRYNEW == "Philippines", "LM", "L")))

# Add region (Anna's grouping)
d$Region <- ifelse(d$COUNTRYNEW == "Egypt" | d$COUNTRYNEW == "Israel" | d$COUNTRYNEW == "Jordan" | d$COUNTRYNEW == "Lebanon" | d$COUNTRYNEW == "Morocco", "MENA",
                   ifelse(d$COUNTRYNEW == "Greece" | d$COUNTRYNEW == "Kazakhstan" | d$COUNTRYNEW == "Russia" | d$COUNTRYNEW == "Tajikistan" | d$COUNTRYNEW == "Turkey", "ECA",
                          ifelse(d$COUNTRYNEW == "Bangladesh" | d$COUNTRYNEW == "Cambodia" | d$COUNTRYNEW == "China" | d$COUNTRYNEW == "India" | d$COUNTRYNEW == "Indonesia" | d$COUNTRYNEW == "Laos" | d$COUNTRYNEW == "Nepal" | d$COUNTRYNEW == "Pakistan" | d$COUNTRYNEW == "Philippines" | d$COUNTRYNEW == "Sri Lanka" | d$COUNTRYNEW == "Vietnam", "AP",
                                 ifelse(d$COUNTRYNEW == "Bolivia" | d$COUNTRYNEW == "United States" | d$COUNTRYNEW == "Chile" | d$COUNTRYNEW == "Colombia" | d$COUNTRYNEW == "Ecuador" | d$COUNTRYNEW == "Mexico" | d$COUNTRYNEW == "Nicaragua", "AM", "SSA"))))

# Exctract attributes
a <- attributes(d)

a <- data.frame(cbind(a$names, a$variable.labels))

names(a) <- c("Variable", "Variable Label")

rownames(a) <- 1:230

write.csv(a, "data/GWP variables.csv", row.names=FALSE)

#################################################################

# Individual food groups
#? the for variable sequence in the for loop should be replace with the varialble names. 

# for(i in 182:212){
#     d[, i] <- ifelse(d[, i] == "Yes", 1,
#                      ifelse(d[, i] == "No", 0, NA))
# }


##########################
# Mean indicators
# # Create empty table

resultlist1 <- c()
resultlist2 <- c()

# Result1: This is a result template for mean score 

result1 <- setNames(data.frame(matrix(ncol = 14, nrow = 1)), c("Income_group", "Region", "Country", "ISO3", "Subgroup", "Variable", "Variable_label", "Mean_Prevalence", "LCI", "UCI", "Difference", "Diff_LCI", "Diff_UCI", "Diff_p"))


# Result1: This is a result template for proportions 

result2 <- setNames(data.frame(matrix(ncol = 14, nrow = 1)), c("Income_group", "Region", "Country", "ISO3", "Subgroup", "Variable", "Variable_label", "Mean_Prevalence", "LCI", "UCI", "Difference", "Diff_LCI", "Diff_UCI", "Diff_p"))


for(k in unique(d$COUNTRYNEW)){
  curcountry <- k
  
  
   curdat <- d[d$COUNTRYNEW == curcountry, ]
    
    # Overall
    # Create weighted object
    ifelse(is.na(unique(curdat$WP12259)), d_w <- svydesign(ids = ~WPID, strata = ~WP12258A, nest = FALSE, weights = ~WGT, data =  curdat), 
           d_w <- svydesign(ids = ~WP12259, strata = ~WP12258A, nest = TRUE, weights = ~WGT, data =  curdat))
    
    # By Sex
    # Male
    
    d_m <- curdat[curdat$Sex == "Male  ", ]
    
    # Create weighted object
    ifelse(is.na(unique(d_m$WP12259)), d_m_w <- svydesign(ids = ~WPID, strata = ~WP12258A, nest = FALSE, weights = ~WGT, data =  d_m), 
           d_m_w <- svydesign(ids = ~WP12259, strata = ~WP12258A, nest = TRUE, weights = ~WGT, data =  d_m))

    
    # Female
    d_f <- curdat[curdat$Sex == 'Female', ]
    
    # Create weighted object
    ifelse(is.na(unique(d_f$WP12259)), d_f_w <- svydesign(ids = ~WPID, strata = ~WP12258A, nest = FALSE, weights = ~WGT, data =  d_f), 
           d_f_w <- svydesign(ids = ~WP12259, strata = ~WP12258A, nest = TRUE, weights = ~WGT, data =  d_f))
    

    # By Residence
    # Urban
    #d_u <- curdat[curdat$Residence == "Urban", ]
    d_u <- curdat[ which(curdat$Residence == "Urban"), ]
    
    # Create weighted object
    ifelse(is.na(unique(d_u$WP12259)), d_u_w <- svydesign(ids = ~WPID, strata = ~WP12258A, nest = FALSE, weights = ~WGT, data =  d_u), 
           d_u_w <- svydesign(ids = ~WP12259, strata = ~WP12258A, nest = TRUE, weights = ~WGT, data =  d_u))
    

    # subset the dataset for rural subgroup;
    
    d_r <- curdat[ which(curdat$Residence == "Rural"), ]
    
       #Create weighted object for rural subrgoup
    ifelse(is.na(unique(d_r$WP12259)), d_r_w <- svydesign(ids = ~WPID, strata = ~WP12258A, nest = FALSE, weights = ~WGT, data =  d_r), 
           d_r_w <- svydesign(ids = ~WP12259, strata = ~WP12258A, nest = TRUE, weights = ~WGT, data =  d_r))

    
    # By Food insecurity
    # Food secure
    
    d_s <- curdat[ which(curdat$FS == "Yes"), ]
    
    # Create weighted object
    ifelse(is.na(unique(d_s$WP12259)), d_s_w <- svydesign(ids = ~WPID, strata = ~WP12258A, nest = FALSE, weights = ~WGT, data =  d_s), 
           d_s_w <- svydesign(ids = ~WP12259, strata = ~WP12258A, nest = TRUE, weights = ~WGT, data =  d_s))
    
    
    # Food insecure
    d_i <- curdat[ which(curdat$FS == "No"), ]
    
    # Create weighted object
    ifelse(is.na(unique(d_i$WP12259)), d_i_w <- svydesign(ids = ~WPID, strata = ~WP12258A, nest = FALSE, weights = ~WGT, data =  d_i), 
           d_i_w <- svydesign(ids = ~WP12259, strata = ~WP12258A, nest = TRUE, weights = ~WGT, data =  d_i))
    
    
    # Good life
    
    d_GL <- curdat[ which(curdat$GL == "Yes"), ]
    
    # Create weighted object
    ifelse(is.na(unique(d_GL$WP12259)), d_GL_w <- svydesign(ids = ~WPID, strata = ~WP12258A, nest = FALSE, weights = ~WGT, data =  d_GL), 
           d_GL_w <- svydesign(ids = ~WP12259, strata = ~WP12258A, nest = TRUE, weights = ~WGT, data =  d_GL))
    
    
    # Bad life
    d_BL <- curdat[ which(curdat$GL == "No"), ]
    
    # Create weighted object
    ifelse(is.na(unique(d_BL$WP12259)), d_BL_w <- svydesign(ids = ~WPID, strata = ~WP12258A, nest = FALSE, weights = ~WGT, data =  d_BL), 
           d_BL_w <- svydesign(ids = ~WP12259, strata = ~WP12258A, nest = TRUE, weights = ~WGT, data =  d_BL))
    
    
    # By Education level
    # Primary or less
    d_PE <- curdat[ which(curdat$Education == "<9 years"), ]
    
    # Create weighted object
    ifelse(is.na(unique(d_PE$WP12259)), d_PE_w <- svydesign(ids = ~WPID, strata = ~WP12258A, nest = FALSE, weights = ~WGT, data =  d_PE), 
           d_PE_w <- svydesign(ids = ~WP12259, strata = ~WP12258A, nest = TRUE, weights = ~WGT, data =  d_PE))
    
    # Secondary and up to 3 years of college
    d_SE <- curdat[ which(curdat$Education == "9-15 years"), ]
    
    # Create weighted object
    ifelse(is.na(unique(d_SE$WP12259)), d_SE_w <- svydesign(ids = ~WPID, strata = ~WP12258A, nest = FALSE, weights = ~WGT, data =  d_SE), 
           d_SE_w <- svydesign(ids = ~WP12259, strata = ~WP12258A, nest = TRUE, weights = ~WGT, data =  d_SE))
    
    # College or at least 4 years of post-secondary
    d_CE <- curdat[ which(curdat$Education == ">15 years"), ]
    
    # Create weighted object
    ifelse(is.na(unique(d_CE$WP12259)), d_CE_w <- svydesign(ids = ~WPID, strata = ~WP12258A, nest = FALSE, weights = ~WGT, data =  d_CE), 
           d_CE_w <- svydesign(ids = ~WP12259, strata = ~WP12258A, nest = TRUE, weights = ~WGT, data =  d_CE))
    
    for(i in c("FGDS", "GDR_H_score", "GDR_L_score", "GDR_score", "All5_score", "DQQ_score", "WB")){
       
      # all
         x <- svymean(~curdat[, i], d_w, na.rm = TRUE, method = "as", df=degf(d_w))
        x_LCI <- confint(x)[1]
        x_UCI <- confint(x)[2]
        result1 <- rbind(result1, c(unique(as.character(curdat$WBIG[curdat$COUNTRYNEW == curcountry])), unique(as.character(curdat$Region[curdat$COUNTRYNEW == curcountry])), curcountry, unique(as.character(curdat$COUNTRY_ISO3[curdat$COUNTRYNEW == curcountry])), "All", i, attributes(d)$variable.labels[i], round(x, digits = 2), round(x_LCI, digits = 2), round(x_UCI, digits = 2), NA, NA, NA, NA))
       
        #  # Male
        x <- svymean(~d_m[, i],  d_m_w, na.rm = TRUE, method = "as", df=degf(d_m_w))
             x_LCI <- confint(x)[1]
             x_UCI <- confint(x)[2]
             varname <- as.name(i)
             t <- eval(bquote(svyttest(.(varname)~Sex,d_w)))
            result1 <- rbind(result1, c(unique(as.character(curdat$WBIG[curdat$COUNTRYNEW == curcountry])), unique(as.character(curdat$Region[curdat$COUNTRYNEW == curcountry])), curcountry, unique(as.character(curdat$COUNTRY_ISO3[curdat$COUNTRYNEW == curcountry])), "Male", i, attributes(d)$variable.labels[i], round(x, digits = 2), round(x_LCI, digits = 2), round(x_UCI, digits = 2), as.numeric(round(t$estimate, digits = 2)), as.numeric(round(t$conf.int[1], digits = 2)), as.numeric(round(t$conf.int[2], digits = 2)), as.numeric(t$p.value)))
        
        # Female
        x <- svymean(~d_f[, i],  d_f_w, na.rm = TRUE, method = "as", df=degf(d_f_w))
              x_LCI <- confint(x)[1]
              x_UCI <- confint(x)[2]
              result1 <- rbind(result1, c(unique(as.character(curdat$WBIG[curdat$COUNTRYNEW == curcountry])), unique(as.character(curdat$Region[curdat$COUNTRYNEW == curcountry])), curcountry, unique(as.character(curdat$COUNTRY_ISO3[curdat$COUNTRYNEW == curcountry])), "Female", i, attributes(d)$variable.labels[i], round(x, digits = 2), round(x_LCI, digits = 2), round(x_UCI, digits = 2), as.numeric(round(t$estimate, digits = 2)), as.numeric(round(t$conf.int[1], digits = 2)), as.numeric(round(t$conf.int[2], digits = 2)), as.numeric(t$p.value)))
        
        # Urban
         x <- svymean(~d_u[, i], d_u_w, na.rm = TRUE, method = "as", df=degf(d_u_w))
            x_LCI <- confint(x)[1]
            x_UCI <- confint(x)[2]
            varname <- as.name(i)
            t <- eval(bquote(svyttest(.(varname)~Residence,d_w)))
            result1 <- rbind(result1, c(unique(as.character(curdat$WBIG[curdat$COUNTRYNEW == curcountry])), unique(as.character(curdat$Region[curdat$COUNTRYNEW == curcountry])), curcountry, unique(as.character(curdat$COUNTRY_ISO3[curdat$COUNTRYNEW == curcountry])), "Urban", i, attributes(d)$variable.labels[i], round(x, digits = 2), round(x_LCI, digits = 2), round(x_UCI, digits = 2), as.numeric(round(t$estimate, digits = 2)), as.numeric(round(t$conf.int[1], digits = 2)), as.numeric(round(t$conf.int[2], digits = 2)), as.numeric(t$p.value)))
    
        # rural
         x <- svymean(~d_r[, i], d_r_w, na.rm = TRUE, method = "as", df=degf(d_r_w))
                x_LCI <- confint(x)[1]
                x_UCI <- confint(x)[2]
                result1 <- rbind(result1, c(unique(as.character(curdat$WBIG[curdat$COUNTRYNEW == curcountry])), unique(as.character(curdat$Region[curdat$COUNTRYNEW == curcountry])), curcountry, unique(as.character(curdat$COUNTRY_ISO3[curdat$COUNTRYNEW == curcountry])), "Rural", i, attributes(d)$variable.labels[i], round(x, digits = 2), round(x_LCI, digits = 2), round(x_UCI, digits = 2), as.numeric(round(t$estimate, digits = 2)), as.numeric(round(t$conf.int[1], digits = 2)), as.numeric(round(t$conf.int[2], digits = 2)), as.numeric(t$p.value)))

    
    # Food secure
    x <- svymean(~d_s[, i], d_s_w, na.rm = TRUE, method = "as", df=degf(d_s_w))
    x_LCI <- confint(x)[1]
    x_UCI <- confint(x)[2]
    varname <- as.name(i)
    t <- eval(bquote(svyttest(.(varname)~FS,d_w)))
    result1 <- rbind(result1, c(unique(as.character(curdat$WBIG[curdat$COUNTRYNEW == curcountry])), unique(as.character(curdat$Region[curdat$COUNTRYNEW == curcountry])), curcountry, unique(as.character(curdat$COUNTRY_ISO3[curdat$COUNTRYNEW == curcountry])), "Food secure", i, attributes(d)$variable.labels[i], round(x, digits = 2), round(x_LCI, digits = 2), round(x_UCI, digits = 2), as.numeric(round(t$estimate, digits = 2)), as.numeric(round(t$conf.int[1], digits = 2)), as.numeric(round(t$conf.int[2], digits = 2)), as.numeric(t$p.value)))


# Food insecure
x <- svymean(~d_i[, i], d_i_w, na.rm = TRUE, method = "as", df=degf(d_i_w))
x_LCI <- confint(x)[1]
x_UCI <- confint(x)[2]
result1 <- rbind(result1, c(unique(as.character(curdat$WBIG[curdat$COUNTRYNEW == curcountry])), unique(as.character(curdat$Region[curdat$COUNTRYNEW == curcountry])), curcountry, unique(as.character(curdat$COUNTRY_ISO3[curdat$COUNTRYNEW == curcountry])), "Food insecure", i, attributes(d)$variable.labels[i], round(x, digits = 2), round(x_LCI, digits = 2), round(x_UCI, digits = 2), as.numeric(round(t$estimate, digits = 2)), as.numeric(round(t$conf.int[1], digits = 2)), as.numeric(round(t$conf.int[2], digits = 2)), as.numeric(t$p.value)))


# Good life
x <- svymean(~d_GL[, i], d_GL_w, na.rm = TRUE, method = "as", df=degf(d_GL_w))
x_LCI <- confint(x)[1]
x_UCI <- confint(x)[2]
varname <- as.name(i)
t <- eval(bquote(svyttest(.(varname)~GL,d_w)))
result1 <- rbind(result1, c(unique(as.character(curdat$WBIG[curdat$COUNTRYNEW == curcountry])), unique(as.character(curdat$Region[curdat$COUNTRYNEW == curcountry])), curcountry, unique(as.character(curdat$COUNTRY_ISO3[curdat$COUNTRYNEW == curcountry])), "Good life", i, attributes(d)$variable.labels[i], round(x, digits = 2), round(x_LCI, digits = 2), round(x_UCI, digits = 2), as.numeric(round(t$estimate, digits = 2)), as.numeric(round(t$conf.int[1], digits = 2)), as.numeric(round(t$conf.int[2], digits = 2)), as.numeric(t$p.value)))


# Bad life
x <- svymean(~d_BL[, i], d_BL_w, na.rm = TRUE, method = "as", df=degf(d_BL_w))
x_LCI <- confint(x)[1]
x_UCI <- confint(x)[2]
result1 <- rbind(result1, c(unique(as.character(curdat$WBIG[curdat$COUNTRYNEW == curcountry])), unique(as.character(curdat$Region[curdat$COUNTRYNEW == curcountry])), curcountry, unique(as.character(curdat$COUNTRY_ISO3[curdat$COUNTRYNEW == curcountry])), "Bad life", i, attributes(d)$variable.labels[i], round(x, digits = 2), round(x_LCI, digits = 2), round(x_UCI, digits = 2), as.numeric(round(t$estimate, digits = 2)), as.numeric(round(t$conf.int[1], digits = 2)), as.numeric(round(t$conf.int[2], digits = 2)), as.numeric(t$p.value)))


# Primary education or less
x <- svymean(~d_PE[, i], d_PE_w, na.rm = TRUE, method = "as", df=degf(d_PE_w))
x_LCI <- confint(x)[1]
x_UCI <- confint(x)[2]
varname <- as.name(i)
m <- svyglm(bquote(.(varname)~Education), d_w)
f <- regTermTest(m, "Education")
result1 <- rbind(result1, c(unique(as.character(curdat$WBIG[curdat$COUNTRYNEW == curcountry])), unique(as.character(curdat$Region[curdat$COUNTRYNEW == curcountry])), curcountry, unique(as.character(curdat$COUNTRY_ISO3[curdat$COUNTRYNEW == curcountry])), "<9 years of education", i, attributes(d)$variable.labels[i], round(x, digits = 2), round(x_LCI, digits = 2), round(x_UCI, digits = 2), NA, NA, NA, as.numeric(f$p)))

# Secondary education and up to 3 years of college
x <- svymean(~d_SE[, i], d_SE_w, na.rm = TRUE, method = "as", df=degf(d_SE_w))
x_LCI <- confint(x)[1]
x_UCI <- confint(x)[2]
result1 <- rbind(result1, c(unique(as.character(curdat$WBIG[curdat$COUNTRYNEW == curcountry])), unique(as.character(curdat$Region[curdat$COUNTRYNEW == curcountry])), curcountry, unique(as.character(curdat$COUNTRY_ISO3[curdat$COUNTRYNEW == curcountry])), "9-15 years of education", i, attributes(d)$variable.labels[i], round(x, digits = 2), round(x_LCI, digits = 2), round(x_UCI, digits = 2), NA, NA, NA, as.numeric(f$p)))

# College degree or at least 4 years of college education
x <- svymean(~d_CE[, i], d_CE_w, na.rm = TRUE, method = "as", df=degf(d_CE_w))
x_LCI <- confint(x)[1]
x_UCI <- confint(x)[2]
result1 <- rbind(result1, c(unique(as.character(curdat$WBIG[curdat$COUNTRYNEW == curcountry])), unique(as.character(curdat$Region[curdat$COUNTRYNEW == curcountry])), curcountry, unique(as.character(curdat$COUNTRY_ISO3[curdat$COUNTRYNEW == curcountry])), ">15 years of education", i, attributes(d)$variable.labels[i], round(x, digits = 2), round(x_LCI, digits = 2), round(x_UCI, digits = 2), NA, NA, NA, as.numeric(f$p)))

}
   
 ######################
        # Prevalence Indicators
        # Overall

        for(i in c("FV_400", "Fib_25",  "nutseeds_score", "SF_10", "Sug_10", "LessSug_10", "otherfruit_fv_fib", "otherfruit_MDD", "OneVegetable", "OneFruit", "ForV", "NoFV", "OneProtein", "All5",
                   "ASF", "SSB", "dairy", "saltysnacks", "Salty_UPF",
                   "Grains", "WRT",  "GWRTP", "PulseNutSeed",   "NoPulseNutSeed",  "nutseeds","dairy", "MPF", "eggs", "dglv", "vafv", "otherveg","soda",
                   "vaveg", "vafruit", "citrus","wholegrain", "legume_score", "fruitdrinks", "coffeetea",
                   "grainsweets", "othersweets", "sweetfoods", "processedmeat", "noprocessedmeat", "redmeat", "fastfood", "cheeseyogurt","milk", "fishseafood", "poultry_score",
                   "instantnoodles", "friedfood", "cheese", "yogurt", "unprocessed_ruminant", "unprocessed_nonruminant", "MDD_W", "no_Protein", "no_ASF", 
                   "ff_instantnoodles", "StarchyStaple", "SaltyFriedSnack", "DQ1","DQ2","DQ3","DQ4","DQ5", "DQ6a","DQ6b","DQ7a","DQ7b","DQ7c", "DQ8",
                   "DQ9", "DQ10a", "DQ10b", "DQ10c","DQ11", "DQ12", "DQ13", "DQ14", "DQ15",
                   "DQ16", "DQ16_IND", "DQ17", "DQ18", "DQ19", "DQ20", "DQ21","DQ22","DQ23","DQ24","DQ25",
                   "DQ26", "DQ27", "DQ28", "DQ29")){

            #all

            x <- svyciprop(~curdat[, i], d_w, na.rm = TRUE, method = "as", df=degf(d_w))
            x_LCI <- attributes(x)$ci[1]
            x_UCI <- attributes(x)$ci[2]
            result2 <- rbind(result2, c(unique(as.character(curdat$WBIG[curdat$COUNTRYNEW == curcountry])), unique(as.character(curdat$Region[curdat$COUNTRYNEW == curcountry])), curcountry, unique(as.character(curdat$COUNTRY_ISO3[curdat$COUNTRYNEW == curcountry])), "All", i, attributes(d)$variable.labels[i], round(x, digits = 4), round(x_LCI, digits = 4), round(x_UCI, digits = 4), NA, NA, NA, NA))

             # Male;

            x <- svyciprop(~d_m[, i], d_m_w, na.rm = TRUE, method = "as", df=degf(d_m_w))
            x_LCI <- attributes(x)$ci[1]
            x_UCI <- attributes(x)$ci[2]
            
            y <- svyciprop(~d_f[, i], d_f_w, na.rm = TRUE, method = "as", df=degf(d_f_w))
            y_LCI <- attributes(y)$ci[1]
            y_UCI <- attributes(y)$ci[2]
            varname <- as.name(i)
            ifelse(x == 0 & y == 0, t <- NA, t <- eval(bquote(svychisq(~.(varname)+Sex,d_w))))
            result2 <- rbind(result2, c(unique(as.character(curdat$WBIG[curdat$COUNTRYNEW == curcountry])), unique(as.character(curdat$Region[curdat$COUNTRYNEW == curcountry])), curcountry, unique(as.character(curdat$COUNTRY_ISO3[curdat$COUNTRYNEW == curcountry])), "Male", i, attributes(d)$variable.labels[i], round(x, digits = 4), round(x_LCI, digits = 4), round(x_UCI, digits = 4), round(x-y, digits = 4), NA, NA, ifelse(is.na(t), NA, as.numeric(t$p.value))))

            # Female;

            
            result2 <- rbind(result2, c(unique(as.character(curdat$WBIG[curdat$COUNTRYNEW == curcountry])), unique(as.character(curdat$Region[curdat$COUNTRYNEW == curcountry])), curcountry, unique(as.character(curdat$COUNTRY_ISO3[curdat$COUNTRYNEW == curcountry])), "Female", i, attributes(d)$variable.labels[i], round(y, digits = 4), round(y_LCI, digits = 4), round(y_UCI, digits = 4), round(x-y, digits = 4), NA, NA, ifelse(is.na(t), NA, as.numeric(t$p.value))))


            # Urban;

            x <- svyciprop(~d_u[, i], d_u_w, na.rm = TRUE, method = "as", df=degf(d_u_w))
            x_LCI <- attributes(x)$ci[1]
            x_UCI <- attributes(x)$ci[2]
            
            y <- svyciprop(~d_r[, i], d_r_w, na.rm = TRUE, method = "as", df=degf(d_r_w))
            y_LCI <- attributes(y)$ci[1]
            y_UCI <- attributes(y)$ci[2]
            varname <- as.name(i)
            ifelse(x == 0 & y == 0, t <- NA, t <- eval(bquote(svychisq(~.(varname)+Residence,d_w))))
            result2 <- rbind(result2, c(unique(as.character(curdat$WBIG[curdat$COUNTRYNEW == curcountry])), unique(as.character(curdat$Region[curdat$COUNTRYNEW == curcountry])), curcountry, unique(as.character(curdat$COUNTRY_ISO3[curdat$COUNTRYNEW == curcountry])), "Urban", i, attributes(d)$variable.labels[i], round(x, digits = 4), round(x_LCI, digits = 4), round(x_UCI, digits = 4), round(x-y, digits = 4), NA, NA, ifelse(is.na(t), NA, as.numeric(t$p.value))))

            # Rural;

            result2 <- rbind(result2, c(unique(as.character(curdat$WBIG[curdat$COUNTRYNEW == curcountry])), unique(as.character(curdat$Region[curdat$COUNTRYNEW == curcountry])), curcountry, unique(as.character(curdat$COUNTRY_ISO3[curdat$COUNTRYNEW == curcountry])), "Rural", i, attributes(d)$variable.labels[i], round(y, digits = 4), round(y_LCI, digits = 4), round(y_UCI, digits = 4), round(x-y, digits = 4), NA, NA, ifelse(is.na(t), NA, as.numeric(t$p.value))))
            
            
            
            
            # Food secure
            
            x <- svyciprop(~d_s[, i], d_s_w, na.rm = TRUE, method = "as", df=degf(d_s_w))
            x_LCI <- attributes(x)$ci[1]
            x_UCI <- attributes(x)$ci[2]
            
            y <- svyciprop(~d_i[, i], d_i_w, na.rm = TRUE, method = "as", df=degf(d_i_w))
            y_LCI <- attributes(y)$ci[1]
            y_UCI <- attributes(y)$ci[2]
            varname <- as.name(i)
            ifelse(x == 0 & y == 0, t <- NA, t <- eval(bquote(svychisq(~.(varname)+FS,d_w))))
            result2 <- rbind(result2, c(unique(as.character(curdat$WBIG[curdat$COUNTRYNEW == curcountry])), unique(as.character(curdat$Region[curdat$COUNTRYNEW == curcountry])), curcountry, unique(as.character(curdat$COUNTRY_ISO3[curdat$COUNTRYNEW == curcountry])), "Food secure", i, attributes(d)$variable.labels[i], round(x, digits = 4), round(x_LCI, digits = 4), round(x_UCI, digits = 4), round(x-y, digits = 4), NA, NA, ifelse(is.na(t), NA, as.numeric(t$p.value))))
            
            # Food insecure
            result2 <- rbind(result2, c(unique(as.character(curdat$WBIG[curdat$COUNTRYNEW == curcountry])), unique(as.character(curdat$Region[curdat$COUNTRYNEW == curcountry])), curcountry, unique(as.character(curdat$COUNTRY_ISO3[curdat$COUNTRYNEW == curcountry])), "Food insecure", i, attributes(d)$variable.labels[i], round(y, digits = 4), round(y_LCI, digits = 4), round(y_UCI, digits = 4), round(x-y, digits = 4), NA, NA, ifelse(is.na(t), NA, as.numeric(t$p.value))))

            
            
            # Good life
            
            x <- svyciprop(~d_GL[, i], d_GL_w, na.rm = TRUE, method = "as", df=degf(d_GL_w))
            x_LCI <- attributes(x)$ci[1]
            x_UCI <- attributes(x)$ci[2]
            
            y <- svyciprop(~d_BL[, i], d_BL_w, na.rm = TRUE, method = "as", df=degf(d_BL_w))
            y_LCI <- attributes(y)$ci[1]
            y_UCI <- attributes(y)$ci[2]
            varname <- as.name(i)
            ifelse(x == 0 & y == 0, t <- NA, t <- eval(bquote(svychisq(~.(varname)+GL,d_w))))
            result2 <- rbind(result2, c(unique(as.character(curdat$WBIG[curdat$COUNTRYNEW == curcountry])), unique(as.character(curdat$Region[curdat$COUNTRYNEW == curcountry])), curcountry, unique(as.character(curdat$COUNTRY_ISO3[curdat$COUNTRYNEW == curcountry])), "Good life", i, attributes(d)$variable.labels[i], round(x, digits = 4), round(x_LCI, digits = 4), round(x_UCI, digits = 4), round(x-y, digits = 4), NA, NA, ifelse(is.na(t), NA, as.numeric(t$p.value))))
            
            # Bad life
            result2 <- rbind(result2, c(unique(as.character(curdat$WBIG[curdat$COUNTRYNEW == curcountry])), unique(as.character(curdat$Region[curdat$COUNTRYNEW == curcountry])), curcountry, unique(as.character(curdat$COUNTRY_ISO3[curdat$COUNTRYNEW == curcountry])), "Bad life", i, attributes(d)$variable.labels[i], round(y, digits = 4), round(y_LCI, digits = 4), round(y_UCI, digits = 4), round(x-y, digits = 4), NA, NA, ifelse(is.na(t), NA, as.numeric(t$p.value))))
            
            
            # Prmiary education or less
            x <- svyciprop(~d_PE[, i], d_PE_w, na.rm = TRUE, method = "as", df=degf(d_PE_w))
            x_LCI <- attributes(x)$ci[1]
            x_UCI <- attributes(x)$ci[2]
            
            y <- svyciprop(~d_SE[, i], d_SE_w, na.rm = TRUE, method = "as", df=degf(d_SE_w))
            y_LCI <- attributes(y)$ci[1]
            y_UCI <- attributes(y)$ci[2]
            
            z <- svyciprop(~d_CE[, i], d_CE_w, na.rm = TRUE, method = "as", df=degf(d_CE_w))
            z_LCI <- attributes(z)$ci[1]
            z_UCI <- attributes(z)$ci[2]
            varname <- as.name(i)
            ifelse(x == 0 & y == 0 & z == 0, t <- NA, t <- eval(bquote(svychisq(~.(varname)+Education,d_w))))
            result2 <- rbind(result2, c(unique(as.character(curdat$WBIG[curdat$COUNTRYNEW == curcountry])), unique(as.character(curdat$Region[curdat$COUNTRYNEW == curcountry])), curcountry, unique(as.character(curdat$COUNTRY_ISO3[curdat$COUNTRYNEW == curcountry])), "<9 years of education", i, attributes(d)$variable.labels[i], round(x, digits = 4), round(x_LCI, digits = 4), round(x_UCI, digits = 4), NA, NA, NA, ifelse(is.na(t), NA, as.numeric(t$p.value))))
            
            # Secondary education and up to 3 years of college
            result2 <- rbind(result2, c(unique(as.character(curdat$WBIG[curdat$COUNTRYNEW == curcountry])), unique(as.character(curdat$Region[curdat$COUNTRYNEW == curcountry])), curcountry, unique(as.character(curdat$COUNTRY_ISO3[curdat$COUNTRYNEW == curcountry])), "9-15 years of education", i, attributes(d)$variable.labels[i], round(y, digits = 4), round(y_LCI, digits = 4), round(y_UCI, digits = 4), NA, NA, NA, ifelse(is.na(t), NA, as.numeric(t$p.value))))
            
            # College degree or at least 4 years of college
            result2 <- rbind(result2, c(unique(as.character(curdat$WBIG[curdat$COUNTRYNEW == curcountry])), unique(as.character(curdat$Region[curdat$COUNTRYNEW == curcountry])), curcountry, unique(as.character(curdat$COUNTRY_ISO3[curdat$COUNTRYNEW == curcountry])), ">15 years of education", i, attributes(d)$variable.labels[i], round(z, digits = 4), round(z_LCI, digits = 4), round(z_UCI, digits = 4), NA, NA, NA, ifelse(is.na(t), NA, as.numeric(t$p.value))))
            
        }

}
   
   result1 <- result1[-1,]
  resultlist1 <- c(resultlist1,result1)

    result2 <- result2[-1,]
   resultlist2 <- c(resultlist2,result2)

#allresults2 <- do.call(rbind.data.frame,resultlist2)

resultlist1 <- data.frame(resultlist1)      
resultlist2  <- data.frame(resultlist2)



############ ******FInal result for export******
# This function helps to Make the columns of two dataframes similar

colnames(resultlist1) <- colnames(resultlist2)

results <- rbind(resultlist1, resultlist2) 

write.csv(results, "data/Country level indicators.csv", row.names=FALSE) # add the countryname to save the indicator results

