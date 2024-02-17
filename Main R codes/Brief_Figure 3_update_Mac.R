# 1. Load libraries ----
library(latex2exp)
library(extrafont)
library(haven)
library(tidyverse)
library(ggplot2)
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
df <- read_csv("Data/Output/CSV/results_internal_unmerged.csv")

df1 <- df %>%
  mutate(across(.cols = Subgroup, .fns = ~ parse_factor(.x, levels = c("Urban", "Rural", "Female", "Male", "All"), ordered = TRUE)),
         mean = round(`Mean/Prevalence`, digits = 0)) %>%
  filter(Subgroup %in% c("Urban", "Rural", "Female", "Male"))
glimpse(df)

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
figure3 <- function(df, dfline, Country_name, year){
  df %>%
    filter(Country == Country_name & Year == year & Indicator %in% c("Sugar-sweetened soft drink consumption",
                                                      "Sweet foods",
                                                      "Deep fried foods",
                                                      "Salty snacks, instant noodles, or fast food",
                                                      "Processed meats")) %>%
    ggplot(aes(x = Indicator, y = mean, fill = Subgroup)) +
    geom_col(position = position_dodge(c(-0.9, -1.1, -1.1, -0.9)), width = 0.81) +
    geom_text(aes(label = paste0(mean,"%"), y = mean + 1.9, color = Subgroup), 
              size = 4.5, vjust = rep(c(0.5, 0.7, 0.5, 0.3), 5), 
              hjust = 0, 
              family = "Whitney semibold",   
              position = position_dodge(-.9), 
              show.legend = FALSE) +
    scale_fill_manual(values = colorPanel, breaks = c("Urban", "Rural"), labels = labels[c("Urban", "Rural")],
                      aesthetics = c("color", "fill"),
                      guide = guide_legend(order = 1, byrow = TRUE)) +
    new_scale_fill() +
    new_scale_color() +
    geom_col(aes(fill = Subgroup), position = position_dodge(c(-0.9, -1.1, -1.1, -0.9)), width = 0.81) +
    scale_fill_manual(values = colorPanel, breaks = c("Female", "Male"), labels = labels[c("Female", "Male")],
                      aesthetics = c("color", "fill"),
                      guide = guide_legend(order = 2, byrow = TRUE)) +
    scale_y_continuous(
      breaks = c(0, 20, 40, 60, 80), expand = c(0, 0),
      limits = c(0, 100),
      labels = function(x) paste0(x, "%")) +
    scale_x_discrete(limits = c("Sugar-sweetened soft drink consumption",
                                "Sweet foods",
                                "Deep fried foods",
                                "Salty snacks, instant noodles, or fast food",
                                "Processed meats"),
                     
                     labels = c("Processed meats" = "Processed \nmeats ",
                                "Deep fried foods" = "Deep fried foods ",
                                "Salty snacks, instant noodles, or fast food"= "Salty snacks, \ninstant noodles, \n or fast food ", 
                                "Sweet foods" = "Sweet foods ",
                                "Sugar-sweetened soft drink consumption" = "Soft drinks "), expand = c(.11,.11)) + 
    coord_flip(clip="off") +
    geom_segment(aes(x=0.5613, xend=5.4387, y = 0, yend = 0), size = .5) +
    geom_segment(
      data= linesdata, y=-48.3, yend= 100,
      aes(x=xvals, xend=xvals), show.legend = FALSE, size = 0.15) +  
    labs(x= " ", y = " ") + 
    ggtitle("FIGURE 3. Dietary risk factors for NCDs") + 
    theme(
      panel.background = element_rect(fill = "white"),
      plot.title = element_text(family = "whitney", face = "bold", color = '#187DA2', size = 15, hjust = 2.86),
      axis.text.y = element_text(family = "whitney", color = "black", size = 15, lineheight = 1),
      axis.text.x = element_blank(),
      plot.margin = unit(c(0, .5, 0.2, 0), "cm"),
      aspect.ratio = 1.55,
      axis.ticks.length = unit(0, "cm"),
      legend.text = element_markdown(size = 12, family = "whitney", margin = margin(t = -2)),
      legend.key.height = unit(0.023, "npc"),
      legend.key.width = unit(0.023, "npc"),
      legend.margin = margin(t = 1.5, r = 1, b = 1.5, l = 1),
      legend.title = element_blank(),
      legend.position = c(0.88, 0.9),
      legend.spacing.y  = unit(3, "pt")
    )
}

figure3 <- figure3(df = df1, dfline = linesdata ,  Country_name = "United States 2021", year = 2021)

# 5. Function for NCD-Risk graph  ----
ncdrGraph <- function(Country_name, year){
  d <- df %>%
    rename(mean_pre = `Mean/Prevalence`) %>%
    select(Country, Year, Subgroup, Indicator, mean_pre) %>%
    mutate(stack = "mean") %>%
    filter(Country == Country_name & Year == year & Indicator %in% c("NCD-Risk"))
  
  
  ncdrAll_scoreL <- df %>%
    rename(mean_pre = `Mean/Prevalence`) %>%
    select(Country, Year, Subgroup, Indicator, mean_pre) %>%
    mutate(stack = "mean") %>%
    filter(Country == Country_name & Year == year & Indicator %in% c("NCD-Risk")) %>%
    filter(Subgroup %in% c("All"))
  
  ncdrMax <- data.frame("Country" = Country_name,
                        "Year" = year,
                        "Subgroup"= "All", 
                        "Indicator" = "NCD-Risk",
                        "mean_pre" = 9 - ncdrAll_scoreL$mean_pre,
                        "stack" = "max")
  
  ncdr_bind <- rbind(ncdrAll_scoreL, ncdrMax)
  #colorPanel = c("#D1DFCC", "#488135")
  colorPanel1 = c("#F0CDC9", "#C23626") # red for risk
  
  ggplot(ncdr_bind, aes(x = Indicator, y = mean_pre, fill = stack)) + 
    geom_bar(stat = "identity", position = position_stack(vjust = 0.5), width = 3.4) +
    coord_flip(clip = "off") +
    scale_fill_manual(values =  colorPanel1) + 
    scale_x_discrete(expand = c(.1, .1)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x= "", y = "") +  
    annotate("text", x = 1.1, y = d$mean_pre[1] + .15, 
             label = round(d$mean_pre[1], digits = 1), family = "whitney semibold", color = "#C23626", 
             fontface = "bold",  size = 4.5, hjust = 0) +
    annotate("text",  x = 4, y = 0, label = "NCD-Risk (#/9)\n", 
             size = 5, family =  "whitney semibold", fontface = "bold", hjust = 0, lineheight = .95) +
    annotate("text", x = -2, y = 0, size = 4, family =  "whitneybook", hjust = 0, lineheight = .95, fontface = "italic",
             label = paste("\nUrban:", round(d$mean_pre[4], digits = 1), " ", "Rural:", round(d$mean_pre[5], digits = 1), " ", "Female:", 
                           round(d$mean_pre[3], digits = 1), " ", "Male:", round(d$mean_pre[2], digits = 1), sep = " ")) + 
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
NCDR <- ncdrGraph("United States 2021", year = 2021)

# 6. Combined Graphs ----
Brief <- plot_grid(figure3, NCDR, rel_heights = c(1, 0.1), nrow = 2, ncol = 1)
ggsave(file = "Data/Output/Graphs/United States 2021 Graph 3.pdf", 
       width = 12.5,  height = 15, units ='cm', device = cairo_pdf, 
       Brief)


