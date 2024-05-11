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
library(showtext)
library(grid)
library(rlang)

# Setting the font ----
font_paths("font/whitney-2")
font_add("Whitney", 
         regular = "whitneybook.otf", 
         bold = "whitneybold.otf",
         italic = "whitneybookitalic.otf", 
         bolditalic = "whitneysemibold.otf")

showtext_auto()
par(family = "Whitney")

# Data Preparation ----
# Read data from CSV file
data_file <- "DQQ_GWP_2021-2022_2023_Internal_yearly-pooled_15April2024.csv"
dat <- read.csv(data_file)

# Ensure data is sorted
dat <- dat[order(dat$Subgroup), ]

# Filter and mutate data for the brief plot
dat_softd <- dat %>%
  filter(Indicator == "Soft drinks (soda, energy drinks, sports drinks)" & Subgroup == "Male") %>%
  
  # this is for coercing legend to have "women are higher category" 
  add_row(
    Difference = -10,
    Country = "",
    Diff_p = 0.0001
  ) %>%
  
  mutate(
    fvar = ifelse(
      Difference > 0 & Diff_p < 0.05, 
      "Men are higher", 
      ifelse(
        Difference < 0 & Diff_p < 0.05, 
        "Women are higher", 
        "Not significant"
      )
    ),
    jvar = ifelse(Difference > 0, -0.1, 1.1)
  )
summary(dat_softd$Difference)

draw_square <- function(data, params, size) {
  if (is.null(data$size)) {
    data$size <- 0.5
  }
  lwd <- min(data$size, min(size) / 4)
  grid::rectGrob(
    width = unit(1, "snpc") - unit(lwd, "mm"),
    height = unit(1, "snpc") - unit(lwd, "mm"),
    gp = gpar(
      col = data$colour %||% NA,
      fill = alpha(data$fill %||% "grey20", data$alpha),
      lty = data$linetype %||% 1,
      lwd = lwd * .pt,
      linejoin = params$linejoin %||% "mitre",
      lineend = if (identical(params$linejoin, "round")) "round" else "square"
    )
  )
}
# Construction of the graph ----
dat_softd %>%
  ggplot(aes(x = fct_reorder(Country, Difference, .desc = TRUE), 
             y = Difference,
             fill = fvar)) +
  geom_col(key_glyph = draw_square, width = 0.92) +
  scale_fill_manual(
    name = "Legend", 
    values = c(
      "Men are higher" = "#5b3b71", 
      "Women are higher" = "#9a4054", 
      "Not significant" = "#b7c1bd"
    ),
    breaks = c(
      "Men are higher",
      "Women are higher",
      "Not significant"
    ),
    limits = c(
      "Men are higher",
      "Women are higher",
      "Not significant"
    ),
    labels = c(
      "Higher soft drink consumption for men than women",
      "Higher soft drink consumption for women than men",
      paste(str_pad("No significant difference", 50, side = "right"), "")
    ), drop = T
  ) +
  geom_text(
    data = dat_softd %>% filter(fvar != "Not significant"),
    aes(
      label = paste0(round(Difference, digits = 0), " ", "ppt"), 
      hjust = jvar,
      color = fvar, 
      fontface = "bold", 
      family = "Whitney"
    ), 
    size = 6
  ) +
  scale_color_manual(
    values = c(
      "Men are higher" = "#5b3b71", 
      "Women are higher" = "#9a4054", 
      "Not significant" = "#b7c1bd"
    ), 
    guide = "none"
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, 30),
    labels = function(x) paste0(abs(x)),
    breaks = seq(0, 30, by = 5),
    expand = expansion(add = c(0, 0))
  ) +
  labs(
    x = "", 
    y = "Percentage point (ppt) difference in prevalence<br>of soft drink consumption"
  ) +
  theme(
    panel.background = element_rect(fill = "#edf6f9"),
    plot.background = element_rect(fill = "#edf6f9"),
    panel.border = element_blank(),
    axis.title.x = ggtext::element_textbox_simple(family = "Whitney", 
                                                  color = 'black', 
                                                  size = 26, 
                                                  face = "bold",
                                                  halign = 0.5,
                                                  lineheight = 1.3,
                                                  margin = margin(t = 0.5, unit = "cm")),
    axis.line.x = element_line(color = "black", linewidth = 0.8),
    axis.ticks.x = element_line(color = "black", linewidth = 0.8),
    axis.ticks.length.x = unit(0.5, "cm"),
    axis.text.x = element_text(family = "Whitney",
                               size = 16,
                               margin = margin(t = 0.1, unit = "cm")),
    axis.text.y = element_text(family = "Whitney", 
                               color = "black", size = 20, 
                               lineheight = 1, 
                               margin = margin(r = 0.2, unit = "cm")),
    axis.ticks.y.left = element_blank(),
    aspect.ratio = 2.6,
    legend.text = ggtext::element_textbox_simple(size = 22, 
                                                 family = "Whitney", 
                                                 margin = margin(t = 0, l = 5),
                                                 width = grid::unit(10, "cm"),
                                                 height = grid::unit(2, "cm"),
                                                 valign = -0.8
    ),
    legend.box.margin = margin(l = -1.5, unit = "cm"),
    legend.background = element_rect(fill = "#edf6f9"),
    legend.key.size = unit(0.68, "cm"), 
    legend.margin = margin(t = 1.5, r = 1, b = 1.5, l = 5),
    legend.title = element_blank(),
    legend.position = c(0.70, 0.85),
    legend.key.spacing.y = unit(0.25, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = unit(c(0, 1, 0.2, 0), "cm")
  ) +
  guides(fill = guide_legend(byrow = T))

# Save the plot to a PDF file
ggsave(file = "GWP_DQQ_2021-2022-2023_Soft-drink_Gender-difference.pdf", dpi = 96,
       width = 12,  height = 24.8, units ='in', device = cairo_pdf)
