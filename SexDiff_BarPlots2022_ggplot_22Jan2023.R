# Loading libraries ----
library(survey)
library(stringr)
library(plotrix)
library(dplyr)
library(showtext)
library(ggplot2)
library(hrbrthemes)
library(forcats)
library(ggtext)


# Data Preparation ----
d <- read.csv("Data/Output/CSV/results_internal_unmerged_NA_Revised_24Nov.csv")
d <- d[order(d$Subgroup), ]
d$SexDiff <- rep(dim(as.matrix(d[d$Subgroup == "Male",]["Difference"])), 5)
d$SexDiffPerc <- rep(as.matrix(d[d$Subgroup == "Male",]["Difference"]/d[d$Subgroup == "Male",]["Mean_Prevalence"]), 5)
d <- d[order(d$SexDiffPerc, decreasing = T), ]


d_brief <- d %>%
  mutate(fvar = ifelse(Diff_LCI > 0 & Diff_UCI > 0, "Men are higher", 
                       ifelse(Diff_LCI < 0 & Diff_UCI < 0, "Women are higher", "Not significant")),
         nvar = Difference/Mean_Prevalence,
         jvar = ifelse(nvar > 0, -0.45, 1.1),
         ISO3 = case_when(Year == "2021" & ISO3 == "USA" ~ "USA2021", .default = Country),
         ISO3 = case_when(Year == "2021" & ISO3 == "SLE" ~ "SLE2021", .default = Country),
         ISO3 = case_when(Year == "2021" & ISO3 == "MWI" ~ "MWI2021", .default = Country),
         ISO3 = case_when(Year == "2022" & ISO3 == "USA" ~ "USA2022", .default = Country),
         ISO3 = case_when(Year == "2022" & ISO3 == "SLE" ~ "SLE2022", .default = Country),
         ISO3 = case_when(Year == "2022" & ISO3 == "MWI" ~ "MWI2022", .default = Country)
         ) %>%
  filter(Indicator == "Dietary diversity score" & Subgroup == "Male" & 
           !(ISO3 %in% c("USA2021", "SLE2021", "MWI2021")))

# Construction of the graph ----
brief <- d_brief %>% ggplot(aes(x = fct_reorder(Country, nvar, .desc = T), 
                                y = nvar,
                                fill = fvar)) +
  geom_col(width = 0.8) +
  
  scale_fill_manual(name = "Legend", 
                    
                    values = c("Men are higher" = "#5b3b71", 
                               "Women are higher" = "#9a4054", 
                               "Not significant" = "#b7c1bd"),
                    
                    labels = c("Higher DDS for men<br>than for Men",
                               "No significant<br>difference",
                               "Higher DDS for women<br>than for Women"
                               )) +
  
  geom_text(data = d_brief |> filter(fvar != "Not significant"),
            aes(label = paste0(abs(round(Difference*100/Mean_Prevalence, 0)), " ", "ppt"), hjust = jvar,
            color = fvar, fontface = "bold", family = "Whitney"), size = 2.8) +
  
  scale_color_manual(values = c("Men are higher" = "#5b3b71", 
                               "Women are higher" = "#9a4054", 
                               "Not significant" = "#b7c1bd"), guide = "none") +
  
  
  coord_cartesian(clip = "off") +
  coord_flip() +
  scale_y_continuous(#limits = c(-max(max(d_brief$nvar), abs(min(d_brief$nvar))), max(max(d_brief$nvar), abs(min(d_brief$nvar)))),
                     limits = c(-0.25, 0.25),
                     labels = function(x) paste0(abs(x*100), "%"),
                     breaks = seq(-0.25, 0.25, by = 0.05),
                     expand = expansion(add = c(0, 0))
                     ) +
  
  labs( x= "", 
        y = "Percentage difference<br>of Dietary Diversity Score") +
  theme(
    panel.background = element_rect(fill = "#edf6f9"),
    plot.background = element_rect(fill = "#edf6f9"),
    panel.border = element_blank(),
    axis.title.x = element_markdown(family = "Whitney", color = 'black', size = 8, face = "bold"),
    axis.line.x = element_line(color = "black"),
    axis.text.y = element_text(family = "Whitney", color = "black", size = 8, lineheight = 1, margin = margin(r = 0.2, unit = "cm")),
    axis.ticks.y.left = element_blank(),
    aspect.ratio = 1.55,
    legend.text = element_markdown(size = 8, family = "Whitney", margin = margin(t = 1)),
    legend.key = element_rect(colour = NA, fill = NA, linewidth = 4),
    legend.background = element_rect(fill = "#edf6f9"),
    #legend.key.height = unit(0.015, "npc"),
    #legend.key.width = unit(0.035, "npc"),
    legend.key.height = unit(0.01, "npc"),  
    legend.key.width = unit(0.02, "npc"),
    legend.margin = margin(t = 1.5, r = 1, b = 1.5, l = 1),
    legend.title = element_blank(),
    legend.position = c(0.75, 0.92),
    legend.spacing.y  = unit(8, "pt"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = unit(c(0.2, .5, 0.2, 0), "cm")
  ) +
  guides(fill = guide_legend(byrow = TRUE))



brief

ggsave(file = "Data/Output/Graphs/DDS_Sex_Diff_Perc_ggplot40.pdf", 
       width = 12.5,  height = 16, units ='cm', device = cairo_pdf, 
       brief)


