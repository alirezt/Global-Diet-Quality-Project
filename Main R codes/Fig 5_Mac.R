# Fig 5: MDD-W region-wise grouped bar plots ----

## Font for Mac ----
quartzFonts(whitney = c("Whitney Book", "Whitney Black", "Whitney Book Oblique", 
                        "Whitney Black Oblique"))
par(family = "whitney")

## Data preparation ----
df <- read_csv("Data/Output/CSV/results_internal_merged.csv")
glimpse(df)
color_palette <- c("#5e5766", "#c8875e", "#6f9894", "#e6b123", "#b95547")
names(color_palette) <- unique(df$Region)

## Plot ----
df %>%
  mutate(across(.cols = Region, 
                .fns = ~readr::parse_factor(.x, levels = c("AM", "AP", "ECA", "MENA", "SSA"), 
                                                            ordered = TRUE))) %>%
  filter(Indicator == "MDD-W", Subgroup == "All") %>%
  ggplot(aes(x = factor(Country, levels = unique(Country[order(Region, `Mean/Prevalence`)]), ordered = TRUE), 
             y = `Mean/Prevalence`, fill = str_wrap(Region, width = 10))) +
  geom_bar(stat="identity", position = "dodge", width = 0.83) +
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
  coord_flip() +
  labs(
    x = "",
    y = "<span>**Share of women who achieve<br>minimum dietary diversity (MDD-W)**</span>",
    ) +
  theme(
    panel.background = element_rect(fill = "#edf6f8"),
    plot.background = element_rect(fill = "#edf6f8"),
    legend.text = element_text(family = "whitney", size = 8),
    axis.text = element_text(family = "whitney", size = 8),
    axis.title.x = element_markdown(family = "whitney", size = 10, margin = margin(t = 10)),
    axis.title.y = element_markdown(family = "whitney", size = 8),
    axis.ticks.length.y = unit(0, "cm"),
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.2),
    legend.title = element_blank(),
    legend.key.height = unit(0.015, "npc"),
    legend.key.width = unit(0.025, "npc"),
    legend.position = c(0.83, 0.75),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.spacing.y = unit(4, 'pt'),
    legend.background = element_rect(fill = "#edf6f8"),
    plot.margin = unit(c(0.2, .5, 0.5, 0.2), units = "cm"),
    aspect.ratio = 2,
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y =  element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x =  element_blank()
  ) +
  guides(fill = guide_legend(byrow = TRUE)) # necessary for legend.spacing.y

## Save the plot ----
ggsave(file = "Data/Output/Graphs/Fig5_Mac.pdf", 
       width = 12.5,  height = 22, units ='cm', device = cairo_pdf)  
  
