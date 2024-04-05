data_file <- "DQQ_GWP_2021-2022_2023_Internal_03April2024.csv"
dat <- read.csv(data_file)

dat %>%
  group_by(Country) %>%
  summarise(n = n()) %>%
  filter(n != 240) %>%
  print(n = 300) 

# A tibble: 4 × 2
#Country           n
#<chr>         <int>
#1 Afghanistan     480
#2 Malawi          480
#3 Sierra Leone    720
#4 United States   720

dat %>%
  filter(Country == "Sierra Leone" & Unit == "Score") %>%
  ggplot(aes(x = Year , y = Mean_prevalence)) +
  geom_line(aes(color = Subgroup)) +
  facet_wrap(~Indicator, ncol = 4) +
  scale_x_continuous(
    breaks = c(2021, 2022, 2023)
  ) +
  scale_y_continuous(
    #breaks = c(0, 20, 40, 60, 80, 100), 
    expand = c(0.1, 0.1)
  ) +
  labs(
    title = "Yearly fluctuations of DQQ Scores",
    subtitle = "Estimated DQQ food group scores for **Sierra Leone** across multiple years",
    x = "Year", 
    y = "DQQ Percentage"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "#f8f9fa"),
    panel.border = element_blank(),
    plot.title = element_text(
      family = "Whitney", 
      size = 24, 
      face = "bold",
      margin = margin(t = 0.5, unit = "cm")
    ),
    plot.subtitle = ggtext::element_markdown(
      family = "Whitney", 
      size = 14, 
      face = "italic",
      margin = margin(t = 0.2, b = 0.2, unit = "cm")
    ),
    axis.title.x = element_text(
      family = "Whitney", 
      size = 20, 
      face = "bold",
      margin = margin(t = 0.5, unit = "cm")
    ),
    axis.title.y = element_text(
      family = "Whitney", 
      size = 20, 
      face = "bold",
      margin = margin(r = 0.5, unit = "cm")
    ),
    axis.line.x = element_line(
      color = "black", 
      linewidth = 0.4
    ),
    axis.text.x = element_text(
      family = "Whitney",
      size = 12,
      angle = 45,
      hjust = 1,
      margin = margin(t = 0.1, unit = "cm")
    ),
    axis.text.y = element_text(
      family = "Whitney", 
      color = "black", size = 12, 
      lineheight = 1, 
      margin = margin(r = 0.2, unit = "cm")
    ),
    axis.ticks= element_blank(),
    legend.position = "bottom",
    legend.text = element_text(
      family = "Whitney", 
      face = "bold", 
      size = 12
    ),
    legend.title = element_text(
      family = "Whitney", 
      face = "bold", 
      size = 14
    ),
    legend.background = element_rect(
      fill = "#f8f9fa"
    ),
    strip.background = element_rect(
      fill = "#f8f9fa", 
      colour = "#dad7cd"
    ),
    strip.text = element_text(
      family = "Whitney",
      face = "bold",
      size = 10
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(
      linetype = 1, 
      linewidth = 0.2, 
      color = "#e5e5e5"
    ),
    panel.grid.minor.y= element_blank(),
    plot.margin = unit(c(0.2, 1, 0.2, 1), "cm"),
  )
