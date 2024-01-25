library(showtext)
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)

# 1. Data preparation ----
df <- read_csv("Data/Output/CSV/results_internal_unmerged_NA_Revised_24Nov.csv")

# long names don't work
df <- df %>% mutate(
  Region = case_when(Region == "Americas" ~ "AM",
                     Region == "Asia Pacific" ~ "AP",
                     Region == "Europe and Central Asia" ~ "ECA",
                     Region == "Middle East and North Africa" ~ "MENA",
                     Region == "Sub-Saharan Africa" ~ "SSA",
                     .default = Region)
)

# Two-years countries 
df <- df %>% mutate(
  Country = case_when(Year == 2021 & ISO3 == "USA" ~ "United States 2021",
                      Year == 2022 & ISO3 == "USA" ~ "United States 2022",
                      Year == 2021 & ISO3 == "SLE" ~ "Sierra Leone 2021",
                      Year == 2022 & ISO3 == "SLE" ~ "Sierra Leone 2022",
                      .default = Country
                      )
)



# 2. Make the base plot ----

## 2.1 Color palate ----  
color_palette <- c("#5e5766", "#c8875e", "#6f9894", "#e6b123", "#b95547")
names(color_palette) <- unique(df$Region)

## 2.2 Plot ----
df %>%
  mutate(across(.cols = Region, .fns = ~readr::parse_factor(.x, 
                                                            levels = c("AM", "AP", "ECA", "MENA", "SSA"), 
                                                            ordered = TRUE))) %>%
  filter(Indicator == "MDD-W", Subgroup == "All") %>%
  ggplot(aes(x = factor(Country, levels = unique(Country[order(Region, Mean_Prevalence)]), ordered = TRUE), 
             y = Mean_Prevalence, fill = str_wrap(Region, width = 10))) +
  geom_bar(stat="identity", position = "dodge", width = 0.83) +
  scale_fill_manual(values = color_palette, 
                    breaks = c("SSA", "MENA", "ECA", "AP", "AM"), 
                    labels = c("Sub-Saharan Africa", "Middle East and North Africa", "Europe and Central Asia", "Asia Pacific", "Americas")) +
  geom_errorbar(
    aes(x = Country, ymin = `Lower_95_CI`, ymax = `Upper_95_CI`),
    width = 0.4, linewidth = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(
    breaks = c(0, 20, 40, 60, 80, 100), expand = c(0, 0),
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")) +
  #coord_flip() +
  labs(
    x = "",
    y = "<span>**Share of women who achieve<br>minimum dietary diversity (MDD-W)**</span>",
  ) +
  theme(
    panel.background = element_rect(fill = "#edf6f8"),
    plot.background = element_rect(fill = "#edf6f8"),
    #plot.caption = element_markdown(family = "Whitney", size = 8, hjust = 0),
    legend.text = element_text(family = "Whitney", size = 8),
    axis.text = element_text(family = "Whitney", size = 8),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
    axis.ticks.x = element_blank(),
    axis.title.x = element_markdown(family = "Whitney", size = 10, margin = margin(t = 10)),
    axis.title.y = element_markdown(family = "Whitney", size = 8),
    axis.ticks.length.y = unit(0, "cm"),
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.2),
    legend.title = element_blank(),
    legend.key.height = unit(0.015, "npc"),
    legend.key.width = unit(0.025, "npc"),
    #legend.position = c(0.83, 0.75),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.spacing.y = unit(4, 'pt'),
    legend.background = element_rect(fill = "#edf6f8"),
    plot.margin = unit(c(0.7, .5, 0.7, 0.2), units = "cm"), #trlb
    #aspect.ratio = 2,
    aspect.ratio = 0.44,
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y =  element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x =  element_blank()
  ) +
  guides(fill = guide_legend(byrow = TRUE))

ggsave(file = "Data/Output/Graphs/Fig5_Windows_3.pdf", 
       width = 19.5,  height = 12, units ='cm', device = cairo_pdf)  



# 3. Loop for all indicators ----
for (i in unique(df$Indicator)[5:48]) {
  ind <- i
  d <- df[df$Indicator == ind, ]
  maxi <- max(d$Mean_Prevalence)
  mini <- min(d$Mean_Prevalence)
  
  d %>%
    mutate(across(.cols = Region, .fns = ~readr::parse_factor(.x, 
                                                              levels = c("AM", "AP", "ECA", "MENA", "SSA"), 
                                                              ordered = TRUE))) %>%
    filter(Subgroup == "All") %>%
    ggplot(aes(x = factor(Country, levels = unique(Country[order(Region, Mean_Prevalence)]), ordered = TRUE), 
               y = Mean_Prevalence, fill = str_wrap(Region, width = 10))) +
    geom_bar(stat = "identity", position = "dodge", width = 0.83) +
    scale_fill_manual(values = color_palette, 
                      breaks = c("SSA", "MENA", "ECA", "AP", "AM"), 
                      labels = c("Sub-Saharan Africa", "Middle East and North Africa", "Europe and Central Asia", "Asia Pacific", "Americas")) +
    geom_errorbar(
      aes(x = Country, ymin = `Lower_95_CI`, ymax = `Upper_95_CI`),
      width = 0.4, linewidth = 0.2,
      position = position_dodge(width = 0.9)
    ) +
    scale_y_continuous(
      
      # for scores
      #breaks = c(0, 1, 2, 3, 4), 
      #expand = c(0, 0),
      #limits = c(0, 4),
      
      # for percentages
      breaks = c(0, 20, 40, 60, 80, 100), 
      expand = c(0, 0),
      limits = c(0, 100),
      labels = function(x) paste0(x, "%")
      
      ) +
    #coord_flip() +
    labs(
      x = "",
      y = ind,
      #y = "<span>**Share of women who achieve<br>minimum dietary diversity (MDD-W)**</span>",
    ) +
    theme(
      panel.background = element_rect(fill = "#edf6f8"),
      plot.background = element_rect(fill = "#edf6f8"),
      legend.text = element_text(family = "Whitney", size = 8),
      axis.text = element_text(family = "Whitney", size = 8),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
      axis.ticks.x = element_blank(),
      axis.title.x = element_markdown(family = "Whitney", size = 10, margin = margin(t = 10)),
      axis.title.y = element_markdown(family = "Whitney", size = 8),
      axis.ticks.length.y = unit(0, "cm"),
      axis.line.x.bottom = element_line(color = "black", linewidth = 0.2),
      legend.title = element_blank(),
      legend.key.height = unit(0.015, "npc"),
      legend.key.width = unit(0.025, "npc"),
      #legend.position = c(0.83, 0.75), # for inside legend
      legend.direction = "horizontal",
      legend.position = "bottom",
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
      legend.spacing.y = unit(4, 'pt'),
      legend.background = element_rect(fill = "#edf6f8"),
      plot.margin = unit(c(0.7, .5, 0.7, 0.2), units = "cm"), #trlb
      #aspect.ratio = 2, # for vertical version 
      aspect.ratio = 0.44,
      panel.grid.major.x = element_blank(), 
      panel.grid.minor.y =  element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x =  element_blank()
    ) +
    guides(fill = guide_legend(byrow = TRUE))
  
  ggsave(file = paste("Data/Output/Graphs/Graph5_update/Legend bottom/", "G5", "_", ind, ".pdf", sep = ""), 
         width = 18,  height = 12, units ='cm', device = cairo_pdf)  
}











