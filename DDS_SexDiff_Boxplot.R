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
data$SexDiff <- rep(dim(as.matrix(data[data$Subgroup == "Male",]["Difference"])), 5)
data$SexDiffPerc <- rep(as.matrix(data[data$Subgroup == "Male",]["Difference"]/data[data$Subgroup == "Male",]["Mean_prevalence"]), 5)
data <- data[order(data$SexDiffPerc, decreasing = TRUE), ]

dat_agg <- data %>%
  group_by(Country, Indicator, Subgroup) %>%
  mutate(Mean_prevalence = mean(Mean_prevalence)) %>%
  distinct(Country, Indicator, Subgroup, .keep_all = T)

# Filter and mutate data for the brief plot
data_brief <- dat_agg %>%
  filter(Indicator == "Dietary diversity score" & Subgroup == "Male") %>%
  mutate(
    fvar = ifelse(Diff_LCI > 0 & Diff_UCI > 0, "Men are higher", 
                  ifelse(Diff_LCI < 0 & Diff_UCI < 0, "Women are higher", "Not significant")),
    nvar = Difference/Mean_prevalence,
    jvar = ifelse(nvar > 0, -0.45, 1.1),
  )

# Construction of the graph ----
brief_plot

guide_squarekey <- function(...) {
  # Constructor just prepends a different class
  x <- guide_legend(...)
  class(x) <- c("squarekey", class(x))
  x
}

guide_gengrob.squarekey <- function(guide, theme) {
  # Make default legend
  legend <- NextMethod()
  
  # Find the key grobs
  is_key <- startsWith(legend$layout$name, "key-")
  is_key <- is_key & !endsWith(legend$layout$name, "-bg")
  
  # Extract the width of the key column
  key_col <- unique(legend$layout$l[is_key])
  keywidth <- convertUnit(legend$widths[2], "mm", valueOnly = TRUE)
  
  # Set the height of every key to the key width
  legend$grobs[is_key] <- lapply(legend$grobs[is_key], function(key) {
    key$height <- unit(keywidth - 0.5, "mm") # I think 0.5mm is default offset
    key
  })
  legend
}

data_brief %>%
  ggplot(aes(x = fct_reorder(Country, nvar, .desc = TRUE), 
             y = nvar,
             fill = fvar)) +
  geom_col(width = 0.95) +
  scale_fill_manual(
    name = "Legend", 
    #guide = "squarekey",
    values = c("Men are higher" = "#5b3b71", 
               "Women are higher" = "#9a4054", 
               "Not significant" = "#b7c1bd"),
    labels = c("Higher for men",
               "No significant difference",
               "Higher for women")
  ) +
  geom_text(data = data_brief %>%
              filter(fvar != "Not significant"),
            aes(label = paste0(abs(round(Difference*100/Mean_prevalence, 0)), " ", "ppt"), hjust = jvar,
                color = fvar, fontface = "bold", family = "Whitney"), size = 8) +
  scale_color_manual(values = c("Men are higher" = "#5b3b71", 
                                "Women are higher" = "#9a4054", 
                                "Not significant" = "#b7c1bd"), guide = "none") +
  coord_flip() +
  scale_y_continuous(
    limits = c(-0.25, 0.25),
    labels = function(x) paste0(abs(x*100), "%"),
    breaks = seq(-0.25, 0.25, by = 0.05),
    expand = expansion(add = c(0, 0))
  ) +
  labs(x = "", y = "Percentage difference between means<br>of men and women scores of DDS") +
  theme(
    panel.background = element_rect(fill = "#edf6f9"),
    plot.background = element_rect(fill = "#edf6f9"),
    panel.border = element_blank(),
    axis.title.x = ggtext::element_textbox_simple(family = "Whitney", 
                                            color = 'black', 
                                            size = 28, 
                                            face = "bold",
                                            halign = 0.5,
                                            lineheight = 0.35,
                                            margin = margin(t = 0.25, unit = "cm")),
    axis.line.x = element_line(color = "black"),
    axis.text.x = element_text(family = "Whitney",
                                  size = 20),
    axis.text.y = element_text(family = "Whitney", 
                               color = "black", size = 25, 
                               lineheight = 1, 
                               margin = margin(r = 0.2, unit = "cm")),
    axis.ticks.y.left = element_blank(),
    aspect.ratio = 2,
    legend.text = ggtext::element_textbox_simple(size = 28, 
                                           family = "Whitney", 
                                           margin = margin(t = 1, l = 0),
                                           width = grid::unit(3.5, "cm"),
                                           lineheight = 0.4,
                                           hjust = 0
                                           ),
    legend.box.margin = margin(l = 0.3, unit = "cm"),
    legend.background = element_rect(fill = "#edf6f9"),
    legend.key.height = unit(0.35, "cm"),  
    legend.key.width = unit(0.32, "cm"),
    legend.margin = margin(t = 1.5, r = 1, b = 1.5, l = 1),
    legend.title = element_blank(),
    legend.position = c(0.75, 0.92),
    legend.spacing.y = unit(3, "pt"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = unit(c(0.2, .5, 0.2, 0), "cm")
  ) +
  guides(fill = guide_legend(byrow = TRUE))

# Save the plot to a PDF file
ggsave(file = "Data/Output/Graphs/test.pdf", dpi = 96,
       width = 4.5,  height = 7.5, units ='in', device = cairo_pdf)


