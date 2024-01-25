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
# Read data from CSV file
data_file <- "Data/Output/CSV/results_internal_unmerged_NA_Revised_24Nov.csv"
data <- read.csv(data_file)

# Ensure data is sorted
data <- data[order(data$Subgroup), ]

# Create new columns for sex difference
data$SexDiff <- rep(dim(as.matrix(data[data$Subgroup == "Male",]["Difference"])), 5)
data$SexDiffPerc <- rep(as.matrix(data[data$Subgroup == "Male",]["Difference"]/data[data$Subgroup == "Male",]["Mean_Prevalence"]), 5)
data <- data[order(data$SexDiffPerc, decreasing = TRUE), ]

# Filter and mutate data for the brief plot
data_brief <- data %>%
  filter(Indicator == "Dietary diversity score" & Subgroup == "Male" & 
           !(ISO3 %in% c("USA2021", "SLE2021", "MWI2021"))) %>%
  mutate(
    fvar = ifelse(Diff_LCI > 0 & Diff_UCI > 0, "Men are higher", 
                  ifelse(Diff_LCI < 0 & Diff_UCI < 0, "Women are higher", "Not significant")),
    nvar = Difference/Mean_Prevalence,
    jvar = ifelse(nvar > 0, -0.45, 1.1),
    ISO3 = case_when(
      Year == "2021" & ISO3 %in% c("USA", "SLE", "MWI") ~ paste0(ISO3, "2021"),
      Year == "2022" & ISO3 %in% c("USA", "SLE", "MWI") ~ paste0(ISO3, "2022"),
      TRUE ~ Country
    )
  )

# Construction of the graph ----
brief_plot <- data_brief %>%
  ggplot(aes(x = fct_reorder(Country, nvar, .desc = TRUE), 
             y = nvar,
             fill = fvar)) +
  geom_col(width = 0.8) +
  scale_fill_manual(
    name = "Legend", 
    values = c("Men are higher" = "#5b3b71", 
               "Women are higher" = "#9a4054", 
               "Not significant" = "#b7c1bd"),
    labels = c("Higher DDS for men<br>than for Men",
               "No significant<br>difference",
               "Higher DDS for women<br>than for Women")
  ) +
  geom_text(data = data_brief %>%
              filter(fvar != "Not significant"),
            aes(label = paste0(abs(round(Difference*100/Mean_Prevalence, 0)), " ", "ppt"), hjust = jvar,
                color = fvar, fontface = "bold", family = "Whitney"), size = 2.8) +
  scale_color_manual(values = c("Men are higher" = "#5b3b71", 
                                "Women are higher" = "#9a4054", 
                                "Not significant" = "#b7c1bd"), guide = "none") +
  coord_cartesian(clip = "off") +
  coord_flip() +
  scale_y_continuous(
    limits = c(-0.25, 0.25),
    labels = function(x) paste0(abs(x*100), "%"),
    breaks = seq(-0.25, 0.25, by = 0.05),
    expand = expansion(add = c(0, 0))
  ) +
  labs(x = "", y = "Percentage difference<br>of Dietary Diversity Score") +
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
    legend.key.height = unit(0.01, "npc"),  
    legend.key.width = unit(0.02, "npc"),
    legend.margin = margin(t = 1.5, r = 1, b = 1.5, l = 1),
    legend.title = element_blank(),
    legend.position = c(0.75, 0.92),
    legend.spacing.y = unit(8, "pt"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = unit(c(0.2, .5, 0.2, 0), "cm")
  ) +
  guides(fill = guide_legend(byrow = TRUE))

# Display the plot
brief_plot

# Save the plot to a PDF file
ggsave(file = "Data/Output/Graphs/DDS_Sex_Diff_Perc_ggplot40.pdf", 
       width = 12.5,  height = 16, units ='cm', device = cairo_pdf)
