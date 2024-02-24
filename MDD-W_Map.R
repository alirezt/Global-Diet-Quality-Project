dat <- read_csv("DQQ_GWP_2021-2022_Internal_17Feb2024.csv")
shp <- rnaturalearth::ne_countries(returnclass = "sf", scale = 110)

names(dat)
glimpse(dat)
unique(dat$Indicator)

dat_agg <- dat %>%
  group_by(Country, Indicator, Subgroup) %>%
  mutate(Mean_prevalence = mean(Mean_prevalence)) %>%
  distinct(Country, Indicator, Subgroup, .keep_all = T)

dat_agg %>%
  filter(Subgroup == "All") %>%
  group_by(Indicator) %>%
  summarise(min_value = min(Mean_prevalence),
            max_value = max(Mean_prevalence)) %>%
  print(n = 100)

dat_mddw <-  dat_agg %>%
  filter(Indicator == "MDD-W" & Subgroup == "All") %>%
  right_join(shp[ , c("geometry", "iso_a3")], by = join_by(ISO3 == iso_a3)) %>%
  filter(ISO3 != "ATA") %>%
  st_as_sf()

cols <- hcl.colors(
  n = 10,
  palette = "GnBu",
  rev = T
)

ni <- classInt::classIntervals(
  dat_mddw$Mean_prevalence,
  n = 6,
  style = "equal"
)$brks

brk <- ni |>
  append(
    max(dat_mddw$Mean_prevalence)
  ) |>
  head(-1)

breaks <- c(
  min(dat_mddw$Mean_prevalence), brk
) |>
  tail(-1)


dat_mddw %>%
  ggplot() +
  geom_sf(aes(fill = Mean_prevalence), color = "white") +
  scale_fill_gradientn(name = "MDD-W",
                       colors = cols,
                       breaks = breaks,
                       labels = round(breaks, 0),
                       limits = c(
                         min(dat_mddw$Mean_prevalence),
                         max(dat_mddw$Mean_prevalence)
                       ),
                       #colors = RColorBrewer::brewer.pal(8, "BuPu"), 
                       na.value = "#D3D3D3", trans = "identity") + 
  
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                               title.position = "top", title.hjust = 0, title.vjust = -3.5, label.vjust = 4.5, 
                               barwidth = 8, barheight = 0.5, direction = "horizontal")) +
  labs(x = NULL,
       y = NULL,
       title = "MDD-W",
       subtitle = "MDD-W values in subcategory <span style='color:#00008b;'>**All**</span> 
       across the world for year 2022",
       caption = "<span style = 'font-size:25pt;'>MDD-W is expressed as a binary score (1/0), 
       and is achieved when <span style = 'color:#00008b;'>**more than 5 out of 10 specific food groups**</span> 
       are consumed by an individual over the course of a day. 
       This indicator is only validated for women age 15-49 years in 
       low- and middle-income countries</span>."
       
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Whitney", face = "bold", color = "black"),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#f5f3f4", color = NA),
    panel.background = element_rect(fill = "#f5f3f4", color = NA),
    legend.background = element_rect(fill = "#f5f3f4", color = NA),
    legend.box.margin = margin(b = 0, t = -0.4, unit = "cm"),
    legend.position = c(0.82, -0.2),
    plot.margin = unit(c(0, 0.35, 0, 0.35), "cm"),
    panel.border = element_blank(),
    panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
    legend.title = element_text(size = 20, 
                                face = "bold",
                                color = "#4f504b", 
                                family = "Whitney"),
    legend.text = element_text(size = 20, 
                               hjust = 0.4, 
                               face = "bold", 
                               color = "#4f504b", 
                               family = "Whitney"),
    plot.title = element_text(size = 40, 
                              hjust = 0.028, 
                              color = "#4f504b", 
                              family = "Whitney", 
                              face = "bold"),
    plot.subtitle = ggtext::element_markdown(size = 25, 
                                             hjust = -0.31, 
                                             color = "#4f504b", 
                                             face = "italic", 
                                             family = "Whitney",
                                             margin = margin(
                                               b = 0.2, 
                                               t = -0.1, 
                                               l = 2, 
                                               unit = "cm"), 
                                             debug = F),
    plot.caption = ggtext::element_textbox_simple(size = 18, 
                                                  hjust = 0, 
                                                  color = "#4f504b", 
                                                  face = "italic", 
                                                  family = "Whitney",
                                                  lineheight = 0.55,
                                                  padding = margin(0,0,0,0.5, unit = "cm"),
                                                  #fill = "black",
                                                  width = 0.55,
                                                  margin = margin(0.5, 0, 0, 0, unit = "cm"))
    
  )
