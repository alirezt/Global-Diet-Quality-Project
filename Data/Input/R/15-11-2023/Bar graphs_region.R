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


## Font for Mac ----
# quartzFonts(whitney = c("Whitney Book", "Whitney Black", "Whitney Book Oblique", "Whitney Black Oblique"))
# par(family = "whitney")
quartzFonts(avenir = c("Avenir Book", "Avenir Black", "Avenir Book Oblique", "Avenir Black Oblique"))
par(family = "avenir")

## Data preparation ----

setwd("/Users/tybeal/Library/CloudStorage/GoogleDrive-tbeal@ucdavis.edu/My Drive/GAIN/Diet Quality/Analysis")
d <- read_csv("data/results_internal_merged.csv")
glimpse(d)
color_palette <- c("#5e5766", "#c8875e", "#6f9894", "#e6b123", "#b95547")
names(color_palette) <- unique(d$Region)

###############################################################################
# MDD-W region-wise grouped bar plots ----

## Plot ----
d %>%
  mutate(across(.cols = Region, 
                .fns = ~readr::parse_factor(.x, levels = c("AM", "AP", "ECA", "MENA", "SSA"), 
                                                            ordered = TRUE))) %>%
  filter(Indicator == "MDD-W", Subgroup == "All") %>%
  ggplot(aes(x = factor(Country, levels = unique(Country[order(Region, `Mean_Prevalence`)]), ordered = TRUE), 
             y = `Mean_Prevalence`, fill = str_wrap(Region, width = 10))) +
  geom_bar(stat="identity", position = "dodge", width = 0.91) +
  scale_fill_manual(values = color_palette, 
                    breaks = rev(c("SSA", "MENA", "ECA", "AP", "AM")), 
                    labels = rev(c("Sub-Saharan Africa", "Middle East and North Africa", 
                               "Europe and Central Asia", "Asia Pacific", "Americas"))) +
  geom_errorbar(
    aes(x = Country, ymin = `Lower confidence interval`, ymax = `Upper confidence interval`),
    width = 0.4, linewidth = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(
    breaks = c(0, 20, 40, 60, 80, 100), expand = c(0, 0),
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(
    x = "",
    y = "<span>**Share of women who achieve<br>minimum dietary diversity (MDD-W)**</span>",
    ) +
  theme(
    panel.background = element_rect(fill = "#edf6f8"),
    plot.background = element_rect(fill = "#edf6f8"),
    legend.text = element_text(family = "avenir", size = 8),
    axis.text = element_text(family = "avenir", size = 8),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
    axis.title.x = element_markdown(family = "avenir", size = 10, margin = margin(t = 10)),
    axis.title.y = element_markdown(family = "avenir", size = 8),
    axis.ticks.length.x = unit(0, "cm"),
    axis.ticks = element_line(size = 0.25),
    axis.line.y.left = element_line(color = "black", linewidth = 0.25),
    legend.title = element_blank(),
    legend.key.height = unit(0.015, "npc"),
    legend.key.width = unit(0.015, "npc"),
    legend.position = c(0.9, 0.9),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.spacing.y = unit(4, 'pt'),
    legend.background = element_rect(fill = "transparent"),
    plot.margin = unit(c(0.5, .5, 0, 0.4), units = "cm"),
    aspect.ratio = .503,
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y =  element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x =  element_blank()
  ) +
  guides(fill = guide_legend(byrow = TRUE)) # necessary for legend.spacing.y

## Save the plot ----
ggsave(file = "graphs/MDD-W_2021-2022.pdf", 
       width = 22,  height = 12.5, units ='cm', device = cairo_pdf)
  
###############################################################################
# All-5 region-wise grouped bar plots ----

## Plot ----
d %>%
  mutate(across(.cols = Region, 
                .fns = ~readr::parse_factor(.x, levels = c("AM", "AP", "ECA", "MENA", "SSA"), 
                                            ordered = TRUE))) %>%
  filter(Indicator == "All-5", Subgroup == "All") %>%
  ggplot(aes(x = factor(Country, levels = unique(Country[order(Region, `Mean_Prevalence`)]), ordered = TRUE), 
             y = `Mean_Prevalence`, fill = str_wrap(Region, width = 10))) +
  geom_bar(stat="identity", position = "dodge", width = 0.91) +
  scale_fill_manual(values = color_palette, 
                    breaks = c("SSA", "MENA", "ECA", "AP", "AM"), 
                    labels = c("Sub-Saharan Africa", "Middle East and North Africa", 
                               "Europe and Central Asia", "Asia Pacific", "Americas")) +
  geom_errorbar(
    aes(x = Country, ymin = `Lower confidence interval`, ymax = `Upper confidence interval`),
    width = 0.4, linewidth = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70), expand = c(0, 0),
    limits = c(0, 70),
    labels = function(x) paste0(x, "%")) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(
    x = "",
    y = "<span>**Share of adults who consume<br>all five recommended food groups (All-5)**</span>",
  ) +
  theme(
    panel.background = element_rect(fill = "#edf6f8"),
    plot.background = element_rect(fill = "#edf6f8"),
    legend.text = element_text(family = "avenir", size = 8),
    axis.text = element_text(family = "avenir", size = 8),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
    axis.title.x = element_markdown(family = "avenir", size = 10, margin = margin(t = 10)),
    axis.title.y = element_markdown(family = "avenir", size = 8),
    axis.ticks.length.x = unit(0, "cm"),
    axis.ticks = element_line(size = 0.25),
    axis.line.y.left = element_line(color = "black", linewidth = 0.25),
    legend.title = element_blank(),
    legend.key.height = unit(0.015, "npc"),
    legend.key.width = unit(0.015, "npc"),
    legend.position = c(0.9, 0.9),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.spacing.y = unit(4, 'pt'),
    legend.background = element_rect(fill = "transparent"),
    plot.margin = unit(c(0.5, .5, 0, 0.4), units = "cm"),
    aspect.ratio = .503,
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y =  element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x =  element_blank()
  ) +
  guides(fill = guide_legend(byrow = TRUE)) # necessary for legend.spacing.y

## Save the plot ----
ggsave(file = "graphs/All-5_2021-2022.pdf", 
       width = 22,  height = 12.5, units ='cm', device = cairo_pdf)

###############################################################################
# Zero vegetable or fruit consumption region-wise grouped bar plots ----

## Plot ----
d %>%
  mutate(across(.cols = Region, 
                .fns = ~readr::parse_factor(.x, levels = c("AM", "AP", "ECA", "MENA", "SSA"), 
                                            ordered = TRUE))) %>%
  filter(Indicator == "Zero vegetable or fruit consumption", Subgroup == "All") %>%
  ggplot(aes(x = factor(Country, levels = unique(Country[order(Region, `Mean_Prevalence`)]), ordered = TRUE), 
             y = `Mean_Prevalence`, fill = str_wrap(Region, width = 10))) +
  geom_bar(stat="identity", position = "dodge", width = 0.91) +
  scale_fill_manual(values = color_palette, 
                    breaks = c("SSA", "MENA", "ECA", "AP", "AM"), 
                    labels = c("Sub-Saharan Africa", "Middle East and North Africa", 
                               "Europe and Central Asia", "Asia Pacific", "Americas")) +
  geom_errorbar(
    aes(x = Country, ymin = `Lower confidence interval`, ymax = `Upper confidence interval`),
    width = 0.4, linewidth = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(
    breaks = c(0, 5, 10, 15, 20, 25, 30, 35), expand = c(0, 0),
    limits = c(0, 35),
    labels = function(x) paste0(x, "%")) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(
    x = "",
    y = "<span>**Share of adults who consume<br>zero vegetables or fruits**</span>",
  ) +
  theme(
    panel.background = element_rect(fill = "#edf6f8"),
    plot.background = element_rect(fill = "#edf6f8"),
    legend.text = element_text(family = "avenir", size = 8),
    axis.text = element_text(family = "avenir", size = 8),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
    axis.title.x = element_markdown(family = "avenir", size = 10, margin = margin(t = 10)),
    axis.title.y = element_markdown(family = "avenir", size = 8),
    axis.ticks.length.x = unit(0, "cm"),
    axis.ticks = element_line(size = 0.25),
    axis.line.y.left = element_line(color = "black", linewidth = 0.25),
    legend.title = element_blank(),
    legend.key.height = unit(0.015, "npc"),
    legend.key.width = unit(0.015, "npc"),
    legend.position = c(0.14, 0.9),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.spacing.y = unit(4, 'pt'),
    legend.background = element_rect(fill = "transparent"),
    plot.margin = unit(c(0.5, .5, 0, 0.4), units = "cm"),
    aspect.ratio = .503,
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y =  element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x =  element_blank()
  ) +
  guides(fill = guide_legend(byrow = TRUE)) # necessary for legend.spacing.y

## Save the plot ----
ggsave(file = "graphs/ZFV_2021-2022.pdf", 
       width = 22,  height = 12.5, units ='cm', device = cairo_pdf)

###############################################################################
# ASF region-wise grouped bar plots ----

## Plot ----
d %>%
  mutate(across(.cols = Region, 
                .fns = ~readr::parse_factor(.x, levels = c("AM", "AP", "ECA", "MENA", "SSA"), 
                                            ordered = TRUE))) %>%
  filter(Indicator == "At least one animal-source food", Subgroup == "All") %>%
  ggplot(aes(x = factor(Country, levels = unique(Country[order(Region, `Mean_Prevalence`)]), ordered = TRUE), 
             y = `Mean_Prevalence`, fill = str_wrap(Region, width = 10))) +
  geom_bar(stat="identity", position = "dodge", width = 0.91) +
  scale_fill_manual(values = color_palette, 
                    breaks = c("SSA", "MENA", "ECA", "AP", "AM"), 
                    labels = c("Sub-Saharan Africa", "Middle East and North Africa", 
                               "Europe and Central Asia", "Asia Pacific", "Americas")) +
  geom_errorbar(
    aes(x = Country, ymin = `Lower confidence interval`, ymax = `Upper confidence interval`),
    width = 0.4, linewidth = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(
    breaks = c(0, 20, 40, 60, 80, 100), expand = c(0, 0),
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(
    x = "",
    y = "<span>**Share of adults who consumed<br>at least one animal-source food**</span>",
  ) +
  theme(
    panel.background = element_rect(fill = "#edf6f8"),
    plot.background = element_rect(fill = "#edf6f8"),
    legend.text = element_text(family = "avenir", size = 8),
    axis.text = element_text(family = "avenir", size = 8),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
    axis.title.x = element_markdown(family = "avenir", size = 10, margin = margin(t = 10)),
    axis.title.y = element_markdown(family = "avenir", size = 8),
    axis.ticks.length.x = unit(0, "cm"),
    axis.ticks = element_line(size = 0.25),
    axis.line.y.left = element_line(color = "black", linewidth = 0.25),
    legend.title = element_blank(),
    legend.key.height = unit(0.015, "npc"),
    legend.key.width = unit(0.015, "npc"),
    legend.position = c(0.8, 0.9),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.spacing.y = unit(4, 'pt'),
    legend.background = element_rect(fill = "transparent"),
    plot.margin = unit(c(0.5, .5, 0, 0.4), units = "cm"),
    aspect.ratio = .503,
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y =  element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x =  element_blank()
  ) +
  guides(fill = guide_legend(byrow = TRUE)) # necessary for legend.spacing.y

## Save the plot ----
ggsave(file = "graphs/ASF_2021-2022.pdf", 
       width = 22,  height = 12.5, units ='cm', device = cairo_pdf)

###############################################################################
# PNS region-wise grouped bar plots ----

## Plot ----
d %>%
  mutate(across(.cols = Region, 
                .fns = ~readr::parse_factor(.x, levels = c("AM", "AP", "ECA", "MENA", "SSA"), 
                                            ordered = TRUE))) %>%
  filter(Indicator == "At least one pulse, nut, or seed", Subgroup == "All") %>%
  ggplot(aes(x = factor(Country, levels = unique(Country[order(Region, `Mean_Prevalence`)]), ordered = TRUE), 
             y = `Mean_Prevalence`, fill = str_wrap(Region, width = 10))) +
  geom_bar(stat="identity", position = "dodge", width = 0.91) +
  scale_fill_manual(values = color_palette, 
                    breaks = c("SSA", "MENA", "ECA", "AP", "AM"), 
                    labels = c("Sub-Saharan Africa", "Middle East and North Africa", 
                               "Europe and Central Asia", "Asia Pacific", "Americas")) +
  geom_errorbar(
    aes(x = Country, ymin = `Lower confidence interval`, ymax = `Upper confidence interval`),
    width = 0.4, linewidth = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(
    breaks = c(0, 20, 40, 60, 80, 100), expand = c(0, 0),
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(
    x = "",
    y = "<span>**Share of adults who consumed<br>at least one legume, nut, or seed**</span>",
  ) +
  theme(
    panel.background = element_rect(fill = "#edf6f8"),
    plot.background = element_rect(fill = "#edf6f8"),
    legend.text = element_text(family = "avenir", size = 8),
    axis.text = element_text(family = "avenir", size = 8),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
    axis.title.x = element_markdown(family = "avenir", size = 10, margin = margin(t = 10)),
    axis.title.y = element_markdown(family = "avenir", size = 8),
    axis.ticks.length.x = unit(0, "cm"),
    axis.ticks = element_line(size = 0.25),
    axis.line.y.left = element_line(color = "black", linewidth = 0.25),
    legend.title = element_blank(),
    legend.key.height = unit(0.015, "npc"),
    legend.key.width = unit(0.015, "npc"),
    legend.position = c(0.9, 0.9),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.spacing.y = unit(4, 'pt'),
    legend.background = element_rect(fill = "transparent"),
    plot.margin = unit(c(0.5, .5, 0, 0.4), units = "cm"),
    aspect.ratio = .503,
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y =  element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x =  element_blank()
  ) +
  guides(fill = guide_legend(byrow = TRUE)) # necessary for legend.spacing.y

## Save the plot ----
ggsave(file = "graphs/PNS_2021-2022.pdf", 
       width = 22,  height = 12.5, units ='cm', device = cairo_pdf)

###############################################################################
# Processed meat region-wise grouped bar plots ----

## Plot ----
d %>%
  mutate(across(.cols = Region, 
                .fns = ~readr::parse_factor(.x, levels = c("AM", "AP", "ECA", "MENA", "SSA"), 
                                            ordered = TRUE))) %>%
  filter(Indicator == "Processed meats", Subgroup == "All") %>%
  ggplot(aes(x = factor(Country, levels = unique(Country[order(Region, `Mean_Prevalence`)]), ordered = TRUE), 
             y = `Mean_Prevalence`, fill = str_wrap(Region, width = 10))) +
  geom_bar(stat="identity", position = "dodge", width = 0.91) +
  scale_fill_manual(values = rev(color_palette), 
                    breaks = rev(c("SSA", "MENA", "ECA", "AP", "AM")), 
                    labels = rev(c("Sub-Saharan Africa", "Middle East and North Africa", 
                                   "Europe and Central Asia", "Asia Pacific", "Americas"))) +
  geom_errorbar(
    aes(x = Country, ymin = `Lower confidence interval`, ymax = `Upper confidence interval`),
    width = 0.4, linewidth = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(
    breaks = c(0, 10, 20, 30, 40, 50, 60), expand = c(0, 0),
    limits = c(0, 60),
    labels = function(x) paste0(x, "%")) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(
    x = "",
    y = "<span>**Share of adults who consumed<br>processed meat in the prior 24 hours**</span>",
  ) +
  theme(
    panel.background = element_rect(fill = "#edf6f8"),
    plot.background = element_rect(fill = "#edf6f8"),
    legend.text = element_text(family = "avenir", size = 8),
    axis.text = element_text(family = "avenir", size = 8),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
    axis.title.x = element_markdown(family = "avenir", size = 10, margin = margin(t = 10)),
    axis.title.y = element_markdown(family = "avenir", size = 8),
    axis.ticks.length.x = unit(0, "cm"),
    axis.ticks = element_line(size = 0.25),
    axis.line.y.left = element_line(color = "black", linewidth = 0.25),
    legend.title = element_blank(),
    legend.key.height = unit(0.015, "npc"),
    legend.key.width = unit(0.015, "npc"),
    legend.position = c(0.9, 0.9),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.spacing.y = unit(4, 'pt'),
    legend.background = element_rect(fill = "transparent"),
    plot.margin = unit(c(0.5, .5, 0, 0.4), units = "cm"),
    aspect.ratio = .503,
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y =  element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x =  element_blank()
  ) +
  guides(fill = guide_legend(byrow = TRUE)) # necessary for legend.spacing.y

## Save the plot ----
ggsave(file = "graphs/Processed meat_2021-2022.pdf", 
       width = 22,  height = 12.5, units ='cm', device = cairo_pdf)

###############################################################################
# DGLV region-wise grouped bar plots ----

## Plot ----
d %>%
  mutate(across(.cols = Region, 
                .fns = ~readr::parse_factor(.x, levels = c("AM", "AP", "ECA", "MENA", "SSA"), 
                                            ordered = TRUE))) %>%
  filter(Indicator == "Dark green leafy vegetables", Subgroup == "All") %>%
  ggplot(aes(x = factor(Country, levels = unique(Country[order(Region, `Mean_Prevalence`)]), ordered = TRUE), 
             y = `Mean_Prevalence`, fill = str_wrap(Region, width = 10))) +
  geom_bar(stat="identity", position = "dodge", width = 0.91) +
  scale_fill_manual(values = color_palette, 
                    breaks = rev(c("SSA", "MENA", "ECA", "AP", "AM")), 
                    labels = rev(c("Sub-Saharan Africa", "Middle East and North Africa", 
                               "Europe and Central Asia", "Asia Pacific", "Americas"))) +
  geom_errorbar(
    aes(x = Country, ymin = `Lower confidence interval`, ymax = `Upper confidence interval`),
    width = 0.4, linewidth = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90), expand = c(0, 0),
    limits = c(0, 90),
    labels = function(x) paste0(x, "%")) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(
    x = "",
    y = "<span>**Share of adults who consume<br>dark green leafy vegetables**</span>",
  ) +
  theme(
    panel.background = element_rect(fill = "#edf6f8"),
    plot.background = element_rect(fill = "#edf6f8"),
    legend.text = element_text(family = "avenir", size = 8),
    axis.text = element_text(family = "avenir", size = 8),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
    axis.title.x = element_markdown(family = "avenir", size = 10, margin = margin(t = 10)),
    axis.title.y = element_markdown(family = "avenir", size = 8),
    axis.ticks.length.x = unit(0, "cm"),
    axis.ticks = element_line(size = 0.25),
    axis.line.y.left = element_line(color = "black", linewidth = 0.25),
    legend.title = element_blank(),
    legend.key.height = unit(0.015, "npc"),
    legend.key.width = unit(0.015, "npc"),
    legend.position = c(0.15, 0.9),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.spacing.y = unit(4, 'pt'),
    legend.background = element_rect(fill = "transparent"),
    plot.margin = unit(c(0.5, .5, 0, 0.4), units = "cm"),
    aspect.ratio = .503,
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y =  element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x =  element_blank()
  ) +
  guides(fill = guide_legend(byrow = TRUE)) # necessary for legend.spacing.y

## Save the plot ----
ggsave(file = "graphs/DGLV_2021-2022.pdf", 
       width = 22,  height = 12.5, units ='cm', device = cairo_pdf)


###############################################################################
# Sweet foods region-wise grouped bar plots ----

## Plot ----
d %>%
  mutate(across(.cols = Region, 
                .fns = ~readr::parse_factor(.x, levels = c("AM", "AP", "ECA", "MENA", "SSA"), 
                                            ordered = TRUE))) %>%
  filter(Indicator == "Sweet foods", Subgroup == "All") %>%
  ggplot(aes(x = factor(Country, levels = unique(Country[order(Region, `Mean_Prevalence`)]), ordered = TRUE), 
             y = `Mean_Prevalence`, fill = str_wrap(Region, width = 10))) +
  geom_bar(stat="identity", position = "dodge", width = 0.91) +
  scale_fill_manual(values = rev(color_palette), 
                    breaks = rev(c("SSA", "MENA", "ECA", "AP", "AM")), 
                    labels = rev(c("Sub-Saharan Africa", "Middle East and North Africa", 
                               "Europe and Central Asia", "Asia Pacific", "Americas"))) +
  geom_errorbar(
    aes(x = Country, ymin = `Lower confidence interval`, ymax = `Upper confidence interval`),
    width = 0.4, linewidth = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90), expand = c(0, 0),
    limits = c(0, 90),
    labels = function(x) paste0(x, "%")) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(
    x = "",
    y = "<span>**Share of adults who consumed<br>sweet foods in the prior 24 hours**</span>",
  ) +
  theme(
    panel.background = element_rect(fill = "#edf6f8"),
    plot.background = element_rect(fill = "#edf6f8"),
    legend.text = element_text(family = "avenir", size = 8),
    axis.text = element_text(family = "avenir", size = 8),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
    axis.title.x = element_markdown(family = "avenir", size = 10, margin = margin(t = 10)),
    axis.title.y = element_markdown(family = "avenir", size = 8),
    axis.ticks.length.x = unit(0, "cm"),
    axis.ticks = element_line(size = 0.25),
    axis.line.y.left = element_line(color = "black", linewidth = 0.25),
    legend.title = element_blank(),
    legend.key.height = unit(0.015, "npc"),
    legend.key.width = unit(0.015, "npc"),
    legend.position = c(0.9, 0.9),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.spacing.y = unit(4, 'pt'),
    legend.background = element_rect(fill = "transparent"),
    plot.margin = unit(c(0.5, .5, 0, 0.4), units = "cm"),
    aspect.ratio = .503,
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y =  element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x =  element_blank()
  ) +
  guides(fill = guide_legend(byrow = TRUE)) # necessary for legend.spacing.y

## Save the plot ----
ggsave(file = "graphs/Sweet foods_2021-2022.pdf", 
       width = 22,  height = 12.5, units ='cm', device = cairo_pdf)



###############################################################################
# Packaged ultra-processed salty snacks region-wise grouped bar plots ----

## Plot ----
d %>%
  mutate(across(.cols = Region, 
                .fns = ~readr::parse_factor(.x, levels = c("AM", "AP", "ECA", "MENA", "SSA"), 
                                            ordered = TRUE))) %>%
  filter(Indicator == "Packaged ultra-processed salty snacks", Subgroup == "All") %>%
  ggplot(aes(x = factor(Country, levels = unique(Country[order(Region, `Mean_Prevalence`)]), ordered = TRUE), 
             y = `Mean_Prevalence`, fill = str_wrap(Region, width = 10))) +
  geom_bar(stat="identity", position = "dodge", width = 0.91) +
  scale_fill_manual(values = rev(color_palette), 
                    breaks = rev(c("SSA", "MENA", "ECA", "AP", "AM")), 
                    labels = rev(c("Sub-Saharan Africa", "Middle East and North Africa", 
                                   "Europe and Central Asia", "Asia Pacific", "Americas"))) +
  geom_errorbar(
    aes(x = Country, ymin = `Lower confidence interval`, ymax = `Upper confidence interval`),
    width = 0.4, linewidth = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(
    breaks = c(0, 10, 20, 30, 40, 50), expand = c(0, 0),
    limits = c(0, 50),
    labels = function(x) paste0(x, "%")) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(
    x = "",
    y = "<span>**Share of adults who consumed<br>packaged ultra-processed salty snacks in the prior 24 hours**</span>",
  ) +
  theme(
    panel.background = element_rect(fill = "#edf6f8"),
    plot.background = element_rect(fill = "#edf6f8"),
    legend.text = element_text(family = "avenir", size = 8),
    axis.text = element_text(family = "avenir", size = 8),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
    axis.title.x = element_markdown(family = "avenir", size = 10, margin = margin(t = 10)),
    axis.title.y = element_markdown(family = "avenir", size = 8),
    axis.ticks.length.x = unit(0, "cm"),
    axis.ticks = element_line(size = 0.25),
    axis.line.y.left = element_line(color = "black", linewidth = 0.25),
    legend.title = element_blank(),
    legend.key.height = unit(0.015, "npc"),
    legend.key.width = unit(0.015, "npc"),
    legend.position = c(0.9, 0.9),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.spacing.y = unit(4, 'pt'),
    legend.background = element_rect(fill = "transparent"),
    plot.margin = unit(c(0.5, .5, 0, 0.4), units = "cm"),
    aspect.ratio = .503,
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y =  element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x =  element_blank()
  ) +
  guides(fill = guide_legend(byrow = TRUE)) # necessary for legend.spacing.y

## Save the plot ----
ggsave(file = "graphs/Packaged ultra-processed salty snacks_2021-2022.pdf", 
       width = 22,  height = 12.5, units ='cm', device = cairo_pdf)


###############################################################################
# Sugar-sweetened soft drink consumption region-wise grouped bar plots ----

## Plot ----
d %>%
  mutate(across(.cols = Region, 
                .fns = ~readr::parse_factor(.x, levels = c("AM", "AP", "ECA", "MENA", "SSA"), 
                                            ordered = TRUE))) %>%
  filter(Indicator == "Sugar-sweetened soft drink consumption", Subgroup == "All") %>%
  ggplot(aes(x = factor(Country, levels = unique(Country[order(Region, `Mean_Prevalence`)]), ordered = TRUE), 
             y = `Mean_Prevalence`, fill = str_wrap(Region, width = 10))) +
  geom_bar(stat="identity", position = "dodge", width = 0.91) +
  scale_fill_manual(values = rev(color_palette), 
                    breaks = rev(c("SSA", "MENA", "ECA", "AP", "AM")), 
                    labels = rev(c("Sub-Saharan Africa", "Middle East and North Africa", 
                                   "Europe and Central Asia", "Asia Pacific", "Americas"))) +
  geom_errorbar(
    aes(x = Country, ymin = `Lower confidence interval`, ymax = `Upper confidence interval`),
    width = 0.4, linewidth = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70), expand = c(0, 0),
    limits = c(0, 70),
    labels = function(x) paste0(x, "%")) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(
    x = "",
    y = "<span>**Share of adults who consumed<br>sugar-sweetened soft drinks in the prior 24 hours**</span>",
  ) +
  theme(
    panel.background = element_rect(fill = "#edf6f8"),
    plot.background = element_rect(fill = "#edf6f8"),
    legend.text = element_text(family = "avenir", size = 8),
    axis.text = element_text(family = "avenir", size = 8),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
    axis.title.x = element_markdown(family = "avenir", size = 10, margin = margin(t = 10)),
    axis.title.y = element_markdown(family = "avenir", size = 8),
    axis.ticks.length.x = unit(0, "cm"),
    axis.ticks = element_line(size = 0.25),
    axis.line.y.left = element_line(color = "black", linewidth = 0.25),
    legend.title = element_blank(),
    legend.key.height = unit(0.015, "npc"),
    legend.key.width = unit(0.015, "npc"),
    legend.position = c(0.85, 0.9),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.spacing.y = unit(4, 'pt'),
    legend.background = element_rect(fill = "transparent"),
    plot.margin = unit(c(0.5, .5, 0, 0.4), units = "cm"),
    aspect.ratio = .503,
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y =  element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x =  element_blank()
  ) +
  guides(fill = guide_legend(byrow = TRUE)) # necessary for legend.spacing.y

## Save the plot ----
ggsave(file = "graphs/Sugar-sweetened soft drinks_2021-2022.pdf", 
       width = 22,  height = 12.5, units ='cm', device = cairo_pdf)



###############################################################################
# Fast food region-wise grouped bar plots ----

## Plot ----
d %>%
  mutate(across(.cols = Region, 
                .fns = ~readr::parse_factor(.x, levels = c("AM", "AP", "ECA", "MENA", "SSA"), 
                                            ordered = TRUE))) %>%
  filter(Indicator == "Fast food", Subgroup == "All") %>%
  ggplot(aes(x = factor(Country, levels = unique(Country[order(Region, `Mean_Prevalence`)]), ordered = TRUE), 
             y = `Mean_Prevalence`, fill = str_wrap(Region, width = 10))) +
  geom_bar(stat="identity", position = "dodge", width = 0.91) +
  scale_fill_manual(values = rev(color_palette), 
                    breaks = rev(c("SSA", "MENA", "ECA", "AP", "AM")), 
                    labels = rev(c("Sub-Saharan Africa", "Middle East and North Africa", 
                                   "Europe and Central Asia", "Asia Pacific", "Americas"))) +
  geom_errorbar(
    aes(x = Country, ymin = `Lower confidence interval`, ymax = `Upper confidence interval`),
    width = 0.4, linewidth = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(
    breaks = c(0, 10, 20, 30), expand = c(0, 0),
    limits = c(0, 30),
    labels = function(x) paste0(x, "%")) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(
    x = "",
    y = "<span>**Share of adults who consumed<br>fast food in the prior 24 hours**</span>",
  ) +
  theme(
    panel.background = element_rect(fill = "#edf6f8"),
    plot.background = element_rect(fill = "#edf6f8"),
    legend.text = element_text(family = "avenir", size = 8),
    axis.text = element_text(family = "avenir", size = 8),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
    axis.title.x = element_markdown(family = "avenir", size = 10, margin = margin(t = 10)),
    axis.title.y = element_markdown(family = "avenir", size = 8),
    axis.ticks.length.x = unit(0, "cm"),
    axis.ticks = element_line(size = 0.25),
    axis.line.y.left = element_line(color = "black", linewidth = 0.25),
    legend.title = element_blank(),
    legend.key.height = unit(0.015, "npc"),
    legend.key.width = unit(0.015, "npc"),
    legend.position = c(0.9, 0.9),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.spacing.y = unit(4, 'pt'),
    legend.background = element_rect(fill = "transparent"),
    plot.margin = unit(c(0.5, .5, 0, 0.4), units = "cm"),
    aspect.ratio = .503,
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y =  element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x =  element_blank()
  ) +
  guides(fill = guide_legend(byrow = TRUE)) # necessary for legend.spacing.y

## Save the plot ----
ggsave(file = "graphs/Fast food_2021-2022.pdf", 
       width = 22,  height = 12.5, units ='cm', device = cairo_pdf)




###############################################################################
# Instant noodles region-wise grouped bar plots ----

## Plot ----
d %>%
  mutate(across(.cols = Region, 
                .fns = ~readr::parse_factor(.x, levels = c("AM", "AP", "ECA", "MENA", "SSA"), 
                                            ordered = TRUE))) %>%
  filter(Indicator == "Instant noodles", Subgroup == "All") %>%
  ggplot(aes(x = factor(Country, levels = unique(Country[order(Region, `Mean_Prevalence`)]), ordered = TRUE), 
             y = `Mean_Prevalence`, fill = str_wrap(Region, width = 10))) +
  geom_bar(stat="identity", position = "dodge", width = 0.91) +
  scale_fill_manual(values = rev(color_palette), 
                    breaks = rev(c("SSA", "MENA", "ECA", "AP", "AM")), 
                    labels = rev(c("Sub-Saharan Africa", "Middle East and North Africa", 
                                   "Europe and Central Asia", "Asia Pacific", "Americas"))) +
  geom_errorbar(
    aes(x = Country, ymin = `Lower confidence interval`, ymax = `Upper confidence interval`),
    width = 0.4, linewidth = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(
    breaks = c(0, 10, 20, 30, 40, 50), expand = c(0, 0),
    limits = c(0, 50),
    labels = function(x) paste0(x, "%")) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(
    x = "",
    y = "<span>**Share of adults who consumed<br>instant noodles in the prior 24 hours**</span>",
  ) +
  theme(
    panel.background = element_rect(fill = "#edf6f8"),
    plot.background = element_rect(fill = "#edf6f8"),
    legend.text = element_text(family = "avenir", size = 8),
    axis.text = element_text(family = "avenir", size = 8),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
    axis.title.x = element_markdown(family = "avenir", size = 10, margin = margin(t = 10)),
    axis.title.y = element_markdown(family = "avenir", size = 8),
    axis.ticks.length.x = unit(0, "cm"),
    axis.ticks = element_line(size = 0.25),
    axis.line.y.left = element_line(color = "black", linewidth = 0.25),
    legend.title = element_blank(),
    legend.key.height = unit(0.015, "npc"),
    legend.key.width = unit(0.015, "npc"),
    legend.position = c(0.9, 0.9),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.spacing.y = unit(4, 'pt'),
    legend.background = element_rect(fill = "transparent"),
    plot.margin = unit(c(0.5, .5, 0, 0.4), units = "cm"),
    aspect.ratio = .503,
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y =  element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x =  element_blank()
  ) +
  guides(fill = guide_legend(byrow = TRUE)) # necessary for legend.spacing.y

## Save the plot ----
ggsave(file = "graphs/Instant noodles_2021-2022.pdf", 
       width = 22,  height = 12.5, units ='cm', device = cairo_pdf)



###############################################################################
# NCD-Protect region-wise grouped bar plots ----

## Plot ----
d %>%
  mutate(across(.cols = Region, 
                .fns = ~readr::parse_factor(.x, levels = c("AM", "AP", "ECA", "MENA", "SSA"), 
                                            ordered = TRUE))) %>%
  filter(Indicator == "NCD-Protect", Subgroup == "All") %>%
  ggplot(aes(x = factor(Country, levels = unique(Country[order(Region, `Mean_Prevalence`)]), ordered = TRUE), 
             y = `Mean_Prevalence`, fill = str_wrap(Region, width = 10))) +
  geom_bar(stat="identity", position = "dodge", width = 0.91) +
  scale_fill_manual(values = rev(color_palette), 
                    breaks = rev(c("SSA", "MENA", "ECA", "AP", "AM")), 
                    labels = rev(c("Sub-Saharan Africa", "Middle East and North Africa", 
                                   "Europe and Central Asia", "Asia Pacific", "Americas"))) +
  geom_errorbar(
    aes(x = Country, ymin = `Lower confidence interval`, ymax = `Upper confidence interval`),
    width = 0.4, linewidth = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(
    breaks = c(0, 1, 2, 3, 4, 5, 6), expand = c(0, 0),
    limits = c(0, 6),
    labels = function(x) paste0(x)) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(
    x = "",
    y = "<span>**NCD-Protect score**</span>",
  ) +
  theme(
    panel.background = element_rect(fill = "#edf6f8"),
    plot.background = element_rect(fill = "#edf6f8"),
    legend.text = element_text(family = "avenir", size = 8),
    axis.text = element_text(family = "avenir", size = 8),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
    axis.title.x = element_markdown(family = "avenir", size = 10, margin = margin(t = 10)),
    axis.title.y = element_markdown(family = "avenir", size = 8, margin = margin(r = 10)),
    axis.ticks.length.x = unit(0, "cm"),
    axis.ticks = element_line(size = 0.25),
    axis.line.y.left = element_line(color = "black", linewidth = 0.25),
    legend.title = element_blank(),
    legend.key.height = unit(0.015, "npc"),
    legend.key.width = unit(0.015, "npc"),
    legend.position = c(0.9, 0.9),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.spacing.y = unit(4, 'pt'),
    legend.background = element_rect(fill = "transparent"),
    plot.margin = unit(c(0.5, .5, 0, 0.4), units = "cm"),
    aspect.ratio = .503,
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y =  element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x =  element_blank()
  ) +
  guides(fill = guide_legend(byrow = TRUE)) # necessary for legend.spacing.y

## Save the plot ----
ggsave(file = "graphs/NCD-Protect_2021-2022.pdf", 
       width = 22,  height = 12.5, units ='cm', device = cairo_pdf)



###############################################################################
# NCD-Risk region-wise grouped bar plots ----

## Plot ----
d %>%
  mutate(across(.cols = Region, 
                .fns = ~readr::parse_factor(.x, levels = c("AM", "AP", "ECA", "MENA", "SSA"), 
                                            ordered = TRUE))) %>%
  filter(Indicator == "NCD-Risk", Subgroup == "All") %>%
  ggplot(aes(x = factor(Country, levels = unique(Country[order(Region, `Mean_Prevalence`)]), ordered = TRUE), 
             y = `Mean_Prevalence`, fill = str_wrap(Region, width = 10))) +
  geom_bar(stat="identity", position = "dodge", width = 0.91) +
  scale_fill_manual(values = rev(color_palette), 
                    breaks = rev(c("SSA", "MENA", "ECA", "AP", "AM")), 
                    labels = rev(c("Sub-Saharan Africa", "Middle East and North Africa", 
                                   "Europe and Central Asia", "Asia Pacific", "Americas"))) +
  geom_errorbar(
    aes(x = Country, ymin = `Lower confidence interval`, ymax = `Upper confidence interval`),
    width = 0.4, linewidth = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(
    breaks = c(0, 1, 2, 3, 4, 5), expand = c(0, 0),
    limits = c(0, 5),
    labels = function(x) paste0(x)) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(
    x = "",
    y = "<span>**NCD-Risk score**</span>",
  ) +
  theme(
    panel.background = element_rect(fill = "#edf6f8"),
    plot.background = element_rect(fill = "#edf6f8"),
    legend.text = element_text(family = "avenir", size = 8),
    axis.text = element_text(family = "avenir", size = 8),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
    axis.title.x = element_markdown(family = "avenir", size = 10, margin = margin(t = 10)),
    axis.title.y = element_markdown(family = "avenir", size = 8, margin = margin(r = 10)),
    axis.ticks.length.x = unit(0, "cm"),
    axis.ticks = element_line(size = 0.25),
    axis.line.y.left = element_line(color = "black", linewidth = 0.25),
    legend.title = element_blank(),
    legend.key.height = unit(0.015, "npc"),
    legend.key.width = unit(0.015, "npc"),
    legend.position = c(0.9, 0.9),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.spacing.y = unit(4, 'pt'),
    legend.background = element_rect(fill = "transparent"),
    plot.margin = unit(c(0.5, .5, 0, 0.4), units = "cm"),
    aspect.ratio = .503,
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y =  element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x =  element_blank()
  ) +
  guides(fill = guide_legend(byrow = TRUE)) # necessary for legend.spacing.y

## Save the plot ----
ggsave(file = "graphs/NCD-Risk_2021-2022.pdf", 
       width = 22,  height = 12.5, units ='cm', device = cairo_pdf)