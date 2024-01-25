# 1. Load libraries ----
library(latex2exp)
library(haven)
library(tidyverse)
library(expss)
library(extrafont)
library(rlang)
library(Cairo)
library(cowplot)
library(ggnewscale)
library(ggtext)
library(grid)
library(gridExtra)


# 2. Setting the font for Mac----
quartzFonts(whitney = c("Whitney Book", "Whitney Black", "Whitney Book Oblique", "Whitney Black Oblique"))
par(family = "whitney")

# 3. Data import and preparation ----
setwd("/Users/tybeal/Google Drive/GAIN/Gallup/Analysis")
df <- read_csv("data/results_public_unmerged.csv")
glimpse(df)

# create the subgroup variable as factor variable 
graph4 <- function(Country_name, year){
  g4 <- df %>%
    mutate(mean = round(`Mean/Prevalence`, digits = 0)) %>%
    filter(Subgroup == "All" & !is.na(`DQQ Names`) & Country == Country_name & Year == year) %>%
    ggplot(aes(x = fct_reorder(Indicator, mean), y = mean)) +
    geom_bar(stat = "identity", fill = "#043B5C", width = .9, position = position_dodge(0.2)) +
    geom_text(aes(label = paste(mean, "%", sep = ""), y = mean + 1.7), color="#043B5C",  
              family = "Whitney semibold", vjust = 0.48, hjust = 0, size = 2.5, 
              position = position_dodge(-0.84)) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(breaks = c(120),
                       limits = c(0, 110),
                       labels = function(x) paste0(x, "%"), expand = c(0,0)) +
    xlab (" ") + 
    ylab (" ") + 
    coord_flip(clip = "off") + 
    theme_bw() + 
    theme(
      panel.border = element_blank(),
      axis.line.y =  element_line(size = .5, linetype = "solid", color = "black"),
      axis.text.y = element_text(family = "whitney", color = "Black", size = 8, 
                                 hjust =  1, vjust = 0.5, margin=margin(-8,8,0,0)),
      axis.ticks.y.left = element_line(colour = "green"),
      axis.ticks.length=unit(0, "cm"),
      panel.grid.major.x = element_line(color = "black", size = 0.00000001, linetype = "solid"), 
      panel.grid.minor.y =  element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x =  element_blank(),
      aspect.ratio = 2.7,
      plot.margin = margin(t = 10, r = 0, b = 0, l = 0, unit = "pt")
    )
  g4
}

Brief <- graph4(Country_name = "United States 2021", year = 2021)
ggsave(filename = "graphs/United States 2021 Graph 4 (Mac).pdf", 
       height = 12, width = 10, units = "cm", plot = Brief, device = cairo_pdf)

showtext_auto(FALSE)



