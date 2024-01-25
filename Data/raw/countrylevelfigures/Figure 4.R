
rm(list = ls())

# font_import(paths = "C:\\Users\\fc-lu-u1\\AppData\\Local\\Microsoft\\Windows\\Fonts")
# windowsFonts(Whitney = windowsFont("Whitney"))
# #windowsFonts(Catamaran = windowsFonts("Catamaran-VariableFont_wght"))
# windowsFonts(Whitneybook = windowsFont("whitney-book"))
# windowsFonts(whisemibold = windowsFont("whitney-semibold"))
# windowsFonts(Alternates = windowsFont("TopicAlternates"))
# windowsFonts(TopicAlternatesBold = windowsFont("TopicAlternates-Bold"))
# 
# windowsFonts(FreightSansLight = windowsFont("Freight Sans Light"))
# windowsFonts(FreightSansLightsc = windowsFont("Freight Sans Light SC"))
# 
# 
# extrafont::loadfonts() 
# 
# loadfonts(device="win")

quartzFonts(whitney = c("Whitney Book", "Whitney Black", "Whitney Book Oblique", "Whitney Black Oblique"))
par(family = "whitney")


library(latex2exp)
library(extrafont)
library(haven)
library(tidyverse)
library(ggplot2)
library(expss)
library(extrafont)
library(rlang)
library(Cairo)
##########


setwd("~/Google Drive/GAIN/Gallup/Analysis")

df <- read_csv("data/Country level indicators.csv")


df$Subgroup[df$Subgroup == "All"]    <- 1
df$Subgroup[df$Subgroup == "Female"] <- 2
df$Subgroup[df$Subgroup == "Male"]   <- 3
df$Subgroup[df$Subgroup == "Urban"]  <- 4
df$Subgroup[df$Subgroup == "Rural"]  <- 5


df$Subgroup <- factor(df$Subgroup,
                      levels = c(1, 2, 3, 4, 5),
                      labels = c("All", "Female", "Male", "Urban", "Rural"),  ordered = TRUE)


# Dataset for the final function;


df1 <- df 

# Graphic function



plot_individual <-  function(df, Country_name){
  Country_name <- rlang::parse_expr(quo_name(enquo(Country_name)))
    df %>%
    filter(Country == Country_name & Subgroup == "All" &  Variable_label %in% c("Grains", "Whole grains", "White roots and tubers", "Pulses",  "Vitamin A-rich orange vegetables",
                                "Dark green leafy vegetables",  "Other vegetables", "Vitamin A-rich fruits", "Citrus", "Other fruit",
                                "Baked grain-based sweets", "Other sweets including toffee, chocolate or ice cream", "Eggs",  "Cheese", "Yogurt", "Processed meat",
                                "Unprocessed ruminant meat", "Unprocessed non-ruminant meat",  "Poultry", "Fish and seafood", "Nuts and seeds", "Packaged ultra-processed salty snacks",
                                "Instant noodles", "Deep fried food", "Milk", "Soft drinks (soda)", "Fruit drinks and fruit juice", "Sweetened coffee, tea, and milk drinks", "Fast food")) %>%
             rename(mean = "Mean_Prevalence") %>%
             mutate(mean = round(mean*100, digits = 0), LCI_L = LCI*100, UCI_U = UCI*100) %>%
                   #Food_order = fct_reorder(Variable_label, mean)) %>%
            ggplot(aes(x = fct_reorder(Variable_label, mean), y = mean)) +
            geom_bar(stat = "identity", fill = "#043B5C", width = .9, 
                      position=position_dodge(0.88)) +
           geom_text(aes(label = paste(mean, "%", sep = ""), y = mean + 1.7),color="#043B5C",  family = "Whitney semibold", fontface = "bold", vjust = 0.48, hjust = 0, size = 7.5, position = position_dodge(-0.84)) +
          
           scale_x_discrete(breaks = c("Grains", "Whole grains", "White roots and tubers", "Pulses",  "Vitamin A-rich orange vegetables",
                                       "Dark green leafy vegetables",  "Other vegetables", "Vitamin A-rich fruits", "Citrus", "Other fruit",
                                       "Baked grain-based sweets", "Other sweets including toffee, chocolate or ice cream", "Eggs",  "Cheese", "Yogurt", "Processed meat",
                                       "Unprocessed ruminant meat", "Unprocessed non-ruminant meat",  "Poultry", "Fish and seafood", "Nuts and seeds", "Packaged ultra-processed salty snacks",
                                       "Instant noodles", "Deep fried food", "Milk", "Soft drinks (soda)", "Fruit drinks and fruit juice", "Sweetened coffee, tea, and milk drinks", "Fast food"),
                              
                                         labels = c("Grains" = "Foods made from grains",  "Whole grains"  = "Whole grains", "White roots and tubers" = "White roots, tubers, and plantains", "Pulses" = "Pulses (beans, peas, lentils)",  "Vitamin A-rich orange vegetables" = "Vitamin A\u002Drich orange vegetables",
                                         "Dark green leafy vegetables"= "Dark green leafy vegetables",  "Other vegetables" = "Other vegetables", "Vitamin A-rich fruits"= "Vitamin A\u002Drich fruits", "Citrus" = "Citrus",  "Other fruit" = "Other fruits",
                                         "Baked grain-based sweets" = "Baked or grain\u002Dbased sweets", "Other sweets including toffee, chocolate or ice cream" = "Other sweets", "Eggs" = "Eggs",  "Cheese" = "Cheese", "Yogurt" = "Yogurt", "Processed meat" = "Processed meat",
                                         "Unprocessed ruminant meat" = "Unprocessed red meat (ruminant)",  "Unprocessed non-ruminant meat" ="Unprocessed red meat (non\u002Druminant)",  "Poultry" = "Poultry", "Fish and seafood" = "Fish and seafood", "Nuts and seeds" = "Nuts and seeds", 
                                         "Packaged ultra-processed salty snacks" = "Packaged ultra\u002Dprocessed salty snacks", "Instant noodles" = "Instant noodles", "Deep fried food" = "Deep fried foods", "Milk" = "Milk", "Soft drinks (soda)" = "Sweetened soft drinks",
                                         "Fruit drinks and fruit juice" = "Fruit juice and fruit drinks", "Sweetened coffee, tea, and milk drinks" = "Sweet tea, coffee, and cocoa", "Fast food" = "Fast food"), expand = c(.0345,.0345)) +
         scale_y_continuous(breaks = c(120),
                                limits = c(0, 110),
                                labels = function(x) paste0(x, "%"), expand = c(0,0)) +
        xlab (" ") +
             ylab (" ") + 
             #scale_x_discrete("Subgroup", labels = c("All" = "all", "Female"= "Female", 
             #"Male"= "Male", "Rural"= "Rural",
             #"Urban"= "Urban")) +
             #
             coord_flip(clip = "off") + 
             
             theme_bw() + 
    theme(#text = element_text(size = 8, color = "red", family = "Whitney"),
      panel.border = element_blank(),
      axis.line.x =  element_blank(),
      axis.line.y =  element_line(size = 1, linetype = "solid", color = "black"),
      axis.text.y = element_text(family = "Whitney", face = "plain", color = "Black", size = 22, hjust =  1, vjust = 0.5, margin=margin(-8,8,0,0)),
      axis.text.x = element_text(family = "Whitney", color = "black", size = 20),
      axis.ticks.y.left = element_line(colour = "green"),
      axis.ticks.length=unit(0, "cm"),
      axis.title.x = element_text(size = 10, color = "black", family = "Whitney"),
      # axis.title.y = element_text(margin = unit(c(8, 8, 8, 8), "mm")),
      axis.text = element_text(size = 10, color = "black",family = "Whitney"),
      #panel.aborder = element_rect(colour = "black", fill=NA, size = 0.5),
      # legend.background = element_rect(fill = "white", linetype="solid", 
      #colour ="black", size = 0.2),
      legend.key = element_rect(colour = NA, fill=NA, size= 7),
      legend.text = element_text(size = 10, color = "black", family = "Whitney"),
      legend.margin=margin(t= -1, r= 2, b= 2, l= 2),
      legend.title =  element_blank(), 
      legend.key.height = unit(0.03, "npc"),
      legend.key.width = unit(0.05, "npc"),
      #legend.key.size = unit(5, 'lines'),
      # legend.position = "bottom",
      # legend.direction="horizonatl",
      legend.position = c(0.85, 0.90),  # right corner
      panel.grid.major.x = element_line(color = "black", size = 0.00000001, linetype = "solid"), 
      panel.grid.minor.y =  element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x =  element_blank())+
      theme(plot.margin=unit(c(1, 1.3,.5,.5),"cm"))  
  
}

  
plot_individual  <- plot_individual(df = df1,  Country_name = "`India`") 
  
  plot_individual 
  
  # Save the graphs; 
  
  ggsave(file = paste0("graphs/India_Figure 4.pdf"), 
         plot_individual, width = 25,
         height = 25, units='cm',
         device = cairo_pdf)
  