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
data_file <- "DQQ_GWP_2021-2022_Internal_17Feb2024.csv"
data <- read.csv(data_file)

# Ensure data is sorted
data <- data[order(data$Subgroup), ]

# Create new columns for sex difference
data$UrbanDiff <- rep(dim(as.matrix(data[data$Subgroup == "Urban",]["Difference"])), 5)
data$UrbanDiffPerc <- rep(as.matrix(data[data$Subgroup == "Urban",]["Difference"]/data[data$Subgroup == "Urban",]["Mean_prevalence"]), 5)
data <- data[order(data$UrbanDiffPerc, decreasing = TRUE), ]

dat_agg <- data %>%
  group_by(Country, Indicator, Subgroup) %>%
  mutate(Mean_prevalence = mean(Mean_prevalence)) %>%
  distinct(Country, Indicator, Subgroup, .keep_all = T)

# Filter and mutate data for the brief plot
data_brief <- dat_agg %>%
  filter(Indicator == "MDD-W" & 
         Subgroup == "Urban" &
         !(Country %in% c("Israel", "Palestine"))   
           ) %>%
  mutate(
    fvar = ifelse(Difference > 0 & Diff_p < 0.05, "Urban higher", 
                  ifelse(Difference < 0 & Diff_p < 0.05, 
                         "Rural higher", "No significant difference")),
    jvar = ifelse(Difference > 0, -0.15, 1.1),
  )

# Construction of the graph ----
data_brief %>%
  ggplot(aes(x = fct_reorder(Country, Difference, .desc = TRUE), 
             y = Difference,
             fill = fvar)) +
  geom_col(width = 0.92) +
  scale_fill_manual(
    name = "Legend", 
    values = c("Urban higher" = "#2b5374",
               "No significant difference" = "#b7c1bd",
               "Rural higher" = "#4783aa" 
               ),
    breaks = c(
      "Rural higher",
      "No significant difference",
      "Urban higher"
    ),
    limits = c(
      "Rural higher",
      "No significant difference",
      "Urban higher"
    )
  ) +
  geom_text(data = data_brief %>%
              filter(fvar != "No significant difference"),
            aes(label = paste0(abs(round(Difference, 0)), " ", "ppt"), hjust = jvar,
                color = fvar, fontface = "bold", family = "Whitney"), size = 6) +
  scale_color_manual(values = c("Urban higher" = "#2b5374", 
                                "Rural higher" = "#4783aa", 
                                "No significant difference" = "#b7c1bd"), guide = "none") +
  coord_flip() +
  scale_y_continuous(
    limits = c(-15, 30),
    labels = function(x) paste0(abs(x)),
    breaks = seq(-15, 30, by = 5),
    expand = expansion(add = c(0, 0))
  ) +
  labs(x = "", y = "Percentage point difference in prevalence<br>MDD-W") +
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
    aspect.ratio = 1.8,
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
ggsave(file = "Data/Output/Graphs/pdf/MDD-W_urban-rural_GWP2021-2022.pdf", dpi = 96,
       width = 12,  height = 18.75, units ='in', device = cairo_pdf)


