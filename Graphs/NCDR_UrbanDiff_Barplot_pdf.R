# 1. Loading libraries ----
library(survey)
library(stringr)
library(plotrix)
library(dplyr)
library(showtext)
library(ggplot2)
library(hrbrthemes)
library(forcats)
library(ggtext)

# 2. Data Preparation ----
# Read data from CSV file
data_file <- "DQQ_GWP_2021-2022_2023_Internal_yearly-pooled_15April2024.csv"
data <- read.csv(data_file)

# Ensure data is sorted
data <- data[order(data$Subgroup), ]

# Create new columns for Urban difference
data$UrbanDiff <- rep(dim(as.matrix(data[data$Subgroup == "Urban",]["Difference"])), 5)
data$UrbanDiffPerc <- rep(as.matrix(data[data$Subgroup == "Urban",]["Difference"]/data[data$Subgroup == "Urban",]["Mean"]), 5)
data <- data[order(data$UrbanDiffPerc, decreasing = TRUE), ]

# Filter and mutate data for the brief plot
data_brief <- dat_agg %>%
  filter(Indicator == "NCD-Risk" & Subgroup == "Urban" &
           !(Country %in% c("Israel", "Palestine"))) %>%
  mutate(
    fvar = ifelse(Difference > 0 & Diff_p < 0.05, "Urban higher", 
                  ifelse(Difference < 0 & Diff_p < 0.05, 
                         "Rural higher", "No significant difference")),
    nvar = Difference/Mean_prevalence,
    jvar = ifelse(nvar > 0, -0.1, 1.1),
  )
summary(data_brief$nvar)

# 3. Construction of the graph ----
data_brief %>%
  ggplot(aes(x = fct_reorder(Country, nvar, .desc = TRUE), 
             y = nvar,
             fill = fvar)) +
  geom_col(width = 0.92) +
  scale_fill_manual(
    name = "Legend", 
    values = c("Urban higher" = "#2b5374", 
               "Rural higher" = "#4783aa", 
               "No significant difference" = "#b7c1bd"),
    limits = c(
      "Rural higher",
      "No significant difference",
      "Urban higher"
    )
  ) +
  geom_text(data = data_brief %>%
              filter(fvar != "No significant difference"),
            aes(label = paste0(abs(round(Difference*100/Mean_prevalence, 0)), " ", "%"), hjust = jvar,
                color = fvar, fontface = "bold", family = "Whitney"), size = 6) +
  scale_color_manual(values = c("Urban higher" = "#2b5374", 
                                "Rural higher" = "#4783aa", 
                                "No significant difference" = "#b7c1bd"), guide = "none") +
  coord_flip() +
  scale_y_continuous(
    limits = c(-0.4, 0.8),
    labels = function(x) paste0(abs(x*100), "%"),
    breaks = seq(-0.4, 0.8, by = 0.1),
    expand = expansion(add = c(0, 0))
  ) +
  labs(x = "", y = "Percentage difference in mean score<br>NCD-Risk") +
  theme(
    panel.background = element_rect(fill = "#edf6f9"),
    plot.background = element_rect(fill = "#edf6f9"),
    panel.border = element_blank(),
    axis.title.x = ggtext::element_textbox_simple(family = "Whitney", 
                                                  color = 'black', 
                                                  size = 24, 
                                                  face = "bold",
                                                  halign = 0.5,
                                                  lineheight = 0.35,
                                                  margin = margin(t = 0.5, unit = "cm")),
    axis.line.x = element_line(color = "black", linewidth = 0.8),
    axis.ticks.x = element_line(color = "black", linewidth = 0.8),
    axis.text.x = element_text(family = "Whitney",
                               size = 16,
                               margin = margin(t = 0.1, unit = "cm")),
    axis.text.y = element_text(family = "Whitney", 
                               color = "black", size = 20, 
                               lineheight = 1, 
                               margin = margin(r = 0.2, unit = "cm")),
    axis.ticks.y.left = element_blank(),
    aspect.ratio = 2.4,
    legend.text = ggtext::element_textbox_simple(size = 24, 
                                                 family = "Whitney", 
                                                 margin = margin(t = 1, l = 5),
                                                 width = grid::unit(10, "cm"),
                                                 hjust = 0
    ),
    legend.box.margin = margin(l = -1.5, unit = "cm"),
    legend.background = element_rect(fill = "#edf6f9"),
    legend.key.width = unit(0.68, "cm"), 
    legend.margin = margin(t = 1.5, r = 1, b = 1.5, l = 5),
    legend.title = element_blank(),
    legend.position = c(0.75, 0.92),
    legend.key.spacing.y = unit(0.25, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = unit(c(0.2, 1, 0.2, 0), "cm")
  ) +
  guides(fill = guide_legend(byrow = T))

# Save the plot to a PDF file
ggsave(file = "Data/Output/Graphs/Cam/Final PKG/Urban Diff Maps/Full countries/GWP_DQQ_2021-2022-2023_NCDR-Barchart_Rural-Urban.pdf", dpi = 96,
       width = 12,  height = 22.7, units ='in', device = cairo_pdf)


