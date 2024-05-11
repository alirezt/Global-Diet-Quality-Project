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
library(showtext)

# 2. Setting the font for Mac----
quartzFonts(whitney = c("Whitney Book", "Whitney Black", "Whitney Book Oblique", "Whitney Black Oblique"))
par(family = "whitney")

# 3. Data import and preparation ----
df <- read_csv("Data/Output/CSV/results_public_unmerged.csv")
glimpse(df)

# create the subgroup variable as factor variable 
df1 <- df %>%
  mutate(across(.cols = Subgroup, .fns = ~ parse_factor(.x, levels = c("Urban", "Rural", "Female", "Male", "All"), ordered = TRUE)),
         mean = round(`Mean/Prevalence`, digits = 0)) %>%
  filter(Subgroup %in% c("Urban", "Rural", "Female", "Male"))
glimpse(df1)

# create a data for the vertical lines on graph 1;
# required because it complains for fill, which I cannot specify again for geom_segment.
linesdata <- data.frame(
  xvals = c(1.5, 2.5, 3.5, 4.5),
  Subgroup=NA   
)

# Color code
colorPanel <- c("#073A5B", "#187DA2",  "#8E214A", "#42225C")
#colorPanel <- c("#42235e", "#7d103d", "#007da5", "#003b5d")
names(colorPanel) <- c("Urban", "Rural", "Female", "Male")
labels <- paste0("<span style='color:", colorPanel, ";'>", names(colorPanel), "</span>")
names(labels) <- names(colorPanel)


# 4. Function for main bar chart graph ----
figure2 <- function(df, dfline, Country_name, year){
  df %>%
    filter(Country == Country_name & Year == year & Indicator %in% c("At least one fruit", 
                                                      "At least one vegetable", 
                                                      "Pulses", "Nuts or seeds", 
                                                      "Whole grains")) %>%
    ggplot(aes(x = Indicator, y = mean, fill = Subgroup)) +
    geom_col(position = position_dodge(c(-0.9, -1.1, -1.1, -0.9)), width = 0.81) +
    scale_fill_manual(values = colorPanel, breaks = c("Urban", "Rural"), 
                      labels = labels[c("Urban", "Rural")], guide = guide_legend(order = 1, byrow = TRUE)) +
    new_scale_fill() +
    geom_col(aes(fill = Subgroup), position = position_dodge(c(-0.9, -1.1, -1.1, -0.9)), width = 0.81) +
    scale_fill_manual(values = colorPanel, breaks = c("Female", "Male"), 
                      labels = labels[c("Female", "Male")], guide = guide_legend(order = 1, byrow = TRUE)) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80), 
                       labels = function(x) paste0(x, "%"), limits = c(0, 100), expand = c(0, 0)) +
    scale_x_discrete(limits = rev(c("At least one fruit", "At least one vegetable", "Pulses", 
                                    "Nuts or seeds", "Whole grains")),
                     labels = c("At least one fruit" = "Fruits ", "At least one vegetable" = "Vegetables ", 
                                "Whole grains" = "Whole grains ", "Pulses" = "Pulses ", "Nuts or seeds" = "Nuts \nor Seeds "), 
                     expand = c(.11,.11)) +
    geom_text(aes(label = paste0(mean,"%"), y = mean - 1), color = "white", size = 4.5, 
              vjust = rep(c(0.5, 0.7, 0.5, 0.3), 5), hjust = 1, fontface = "bold", 
              family = "Whitney semibold", position = position_dodge(-0.9), alpha = .9, show.legend = FALSE) + 
    coord_flip(clip="off") +
    geom_segment(aes(x=0.5613, xend=5.4387, y = 0, yend = 0), linewidth = .5) +
    geom_segment(data= linesdata, y=-48.3, yend= 100, aes(x=xvals, xend=xvals), show.legend = FALSE, linewidth = 0.15) +
    labs(x = "", y = "") +
    ggtitle("FIGURE 2. Dietary factors protective of NCDs") +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.title = element_text(family = "whitney", face = "bold", color = '#187DA2', size = 15, hjust = 1.36),
      panel.border = element_blank(),
      axis.text.y = element_text(family = "whitney", color = "black", size = 15, lineheight = 1),
      axis.text.x = element_blank(),
      axis.ticks.length = unit(0, "cm"),
      legend.text = element_markdown(size = 12, family = "whitney", margin = margin(t = -2)),
      legend.key = element_rect(colour = NA, fill = NA, linewidth = 4),
      legend.key.height = unit(0.023, "npc"),
      legend.key.width = unit(0.023, "npc"),
      legend.margin = margin(t = 1.5, r = 1, b = 1.5, l = 1),
      legend.title = element_blank(),
      legend.position = c(0.88, 0.115),
      legend.spacing.y  = unit(3, "pt"),
      plot.margin = unit(c(0.5, .5, 0, 0), "cm"),
      aspect.ratio = 1.55,
    )
}
figure2 <- figure2(df = df1, dfline = linesdata, Country_name = "Sierra Leone 2021", year = 2021)

# 5. Function for NCD-Positive graph  ----
ncdpGraph <- function(Country_name, year){
  d <- df %>%
    select(Country, Year, Subgroup, Indicator, mean_pre = `Mean/Prevalence`) %>%
    mutate(stack = "mean") %>%
    filter(Country == Country_name & Year == year & Indicator %in% c("NCD-Protect"))
  
  
  ncdp_score <- df %>%
    select(Country, Year, Subgroup, Indicator, mean_pre = `Mean/Prevalence`) %>%
    mutate(stack = "mean") %>%
    filter(Country == Country_name & Year == year & Indicator %in% c("NCD-Protect") & Subgroup %in% c("All")) 
  
  ncdp_max <- data.frame("Country" = Country_name,
                         "Year" = year,
                         "Subgroup"= "All", 
                         "Indicator" = "NCD-Protect",
                         "mean_pre" = 9 - ncdp_score$mean_pre,
                         "stack" = "max")
  
  ncdp_bind <- rbind(ncdp_score, ncdp_max)
  colorPanel1 = c("#D1DFCC", "#488135")
  
  ggplot(ncdp_bind, aes(x = Indicator, y = mean_pre, fill = stack)) + 
    geom_bar(stat = "identity", position = position_stack(vjust = 0.5), width = 3.4) +
    coord_flip(clip = "off") +
    scale_fill_manual(values =  colorPanel1) + 
    scale_x_discrete(expand = c(.1, .1)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x= "", y = "") +  
    annotate("text", x = 1.1, y = d$mean_pre[1] + .15, label = round(d$mean_pre[1], digits = 1), 
             family = "whitney semibold", color = "#488135", fontface = "bold",  size = 4.5, hjust = 0) +
    annotate("text",  x = 4, y = 0, label = "Global dietary recommendations-healthy (#/9)\n", 
             size = 5, family =  "whitney semibold", fontface = "bold", hjust = 0, lineheight = .95) +
    annotate("text", x = -2, y = 0, label = paste("\nUrban:", round(d$mean_pre[4], digits = 1), " ", "Rural:", round(d$mean_pre[5], digits = 1), " ", "Female:", round(d$mean_pre[3], digits = 1), " ", "Male:", round(d$mean_pre[2], digits = 1), sep = " "), 
             size = 4, family =  "whitneybook", hjust = 0, lineheight = .95, fontface = "italic") + 
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.text.y = element_blank(),
          axis.line.y =  element_blank(),
          axis.text.x = element_blank(),
          axis.line.x =  element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          aspect.ratio = 0.07,
          plot.margin = unit(c(0, .5, 0, 0), "cm"),
          legend.position = "none",
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.y =  element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x =  element_blank())
  
}
NCDP <- ncdpGraph("Sierra Leone 2021", year = 2021)

# 6. Combined graphs ----
Brief <- plot_grid(figure2, NCDP, rel_heights = c(1, 0.1), nrow = 2, ncol = 1)
ggsave(file = "Data/Output/Graphs/Sierra Leone 2021 Graph 2.pdf", 
       width = 12.5,  height = 15, units ='cm', device = cairo_pdf, 
       Brief)