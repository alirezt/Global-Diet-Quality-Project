# 1. Loading libraries ----
library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(rlang)
library(stringr)
library(ggpattern)
library(showtext)
library(sysfonts)
library(colorspace)
library(classInt)
library(rlang)
library(RColorBrewer)

# 2. Data Preparation ----
dat <- read_csv("DQQ_GWP_2021-2022_2023_Public_10April2024.csv")
shp <- rnaturalearth::ne_countries(returnclass = "sf", scale = 110)

dat_mddw <-  dat %>%
  filter(Indicator == "MDD-W" & Subgroup == "All") %>%
  right_join(shp[ , c("geometry", "iso_a3")], by = join_by(ISO3 == iso_a3)) %>%
  filter(ISO3 != "ATA") %>%
  st_as_sf()

# Retrieve summary statistics  
summary(dat_mddw$Mean)

# 3. Font setting ----
font_paths("font/whitney-2")
font_add("Whitney", 
         regular = "whitneybook.otf", 
         bold = "whitneybold.otf",
         italic = "whitneybookitalic.otf", 
         bolditalic = "whitneysemibold.otf")
showtext_auto()
par(family = "Whitney")


# 4. Drawing maps ----
## 4.1. N-color gradient ----
cols <- hcl.colors(
  n = 10,
  palette = "GnBu",
  rev = T
)

ni <- classInt::classIntervals(
  dat_mddw$Mean,
  n = 6,
  style = "equal"
)$brks

brk <- ni |>
  append(
    max(dat_mddw$Mean)
  ) |>
  head(-1)

breaks <- c(
  min(dat_mddw$Mean), brk
) |>
  tail(-1)

dat_mddw %>%
  ggplot() +
  geom_sf(aes(fill = Mean), color = "white") +
  scale_fill_gradientn(name = "MDD-W",
                       colors = cols,
                       breaks = breaks,
                       labels = round(breaks, 0),
                       limits = c(
                         min(dat_mddw$Mean),
                         max(dat_mddw$Mean)
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

## 4.2. Two color gradient ----
dat_mddw %>%
  ggplot() +
  geom_sf(
    aes(fill = Mean), 
    color = "gray", 
    linewidth = 0.1
  ) +
  geom_sf_pattern(data = dat_mddw %>% filter(is.na(Mean)),
    aes(fill = Mean),
    color = "gray",
    linewidth = 0.1,
    pattern_fill = "white",
    pattern_size = 0.02,
    pattern_spacing = 0.005,
    pattern_colour = "white",
    pattern_density = 0.5
  ) + 
  scale_fill_gradient(
    name = paste(str_pad("MDD-W", 68, "right"), "%"),
    low = "#fffffe", 
    high = "#275578", 
    breaks = c(
      min(
        dat_mddw$Mean, 
        na.rm = T
      ), 
      max(
        dat_mddw$Mean, 
        na.rm = T
      )
    ), 
    labels = as_function(~ paste0(round(.x, 0), "%"))
  ) + 
  guides(fill = guide_colorbar(
    ticks.colour = NA, 
    frame.colour = "gray", 
    title.position = "top", 
    label.hjust = c(0, 1), 
    barwidth = 10, 
    barheight = 0.4, 
    direction = "horizontal")
  ) +
  theme(
    text = element_text(family = "Whitney", face = "bold", color = "black"),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    legend.background = element_rect(fill = "#ffffff", color = "gray", linewidth = 0.35),
    legend.margin = margin(r = 0.2, l = 0.2, t = 0.1, b = 0.1, unit = "cm"),
    legend.position = c(0.88, -0.1),
    legend.title = element_text(size = 8, face = "bold", family = "Whitney", margin = margin(b= 1.2, unit = "mm")),
    legend.text = element_text(size = 6, face = "plain", family = "Whitney", margin = margin(t = 1.2, unit = "mm")), 
    legend.frame = element_rect(linewidth = 0.3),
    plot.margin = unit(c(0, 0.35, 0, 0.35), "cm")
  )

## 4.3. Diverging color gradient ----
### 4.3.1 with scale_fill_gradient2 ----
dat_mddw %>%
  ggplot() +
  geom_sf(
    aes(fill = Mean), 
    # set aesthetics to fixed values
    color = "gray", 
    linewidth = 0.1
  ) + 
  scale_fill_gradient2(
    name = paste(str_pad("MDD-W", 68, "right"), "%"),
    low = "#d7404e", 
    mid = "#feffbd",
    high = "#30759b",
    na.value="white",
    midpoint = 60,
    breaks = c(
      min(
        dat_mddw$Mean, 
        na.rm = T
      ), 
      max(
        dat_mddw$Mean, 
        na.rm = T
      )
    ), 
    labels = as_function(~ paste0(round(.x, 0), "%"))
  ) + 
  guides(fill = guide_colorbar(
    ticks.colour = NA, 
    frame.colour = "gray", 
    title.position = "top", 
    label.hjust = c(0, 1), 
    barwidth = 10, 
    barheight = 0.4, 
    direction = "horizontal")
  ) +
  theme(
    text = element_text(family = "Whitney", face = "bold", color = "black"),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    legend.background = element_rect(fill = "#ffffff", color = "gray", linewidth = 0.35),
    legend.margin = margin(r = 0.2, l = 0.2, t = 0.1, b = 0.1, unit = "cm"),
    legend.position = c(0.88, -0.1),
    legend.title = element_text(size = 8, face = "bold", family = "Whitney", margin = margin(b= 1.2, unit = "mm")),
    legend.text = element_text(size = 6, face = "plain", family = "Whitney", margin = margin(t = 1.2, unit = "mm")), 
    legend.frame = element_rect(linewidth = 0.3),
    plot.margin = unit(c(0, 0.35, 0, 0.35), "cm")
  )

### 4.3.2 with distiller ---- 
dat_mddw %>%
  ggplot() +
  geom_sf(
    aes(fill = Mean), 
    # set aesthetics to fixed values
    color = "gray", 
    linewidth = 0.1
  ) + 
  scale_fill_distiller(
    name = "MDD-W",
    palette = "Spectral", 
    direction = 1,
    breaks = classIntervals(
      dat_mddw$Mean[!is.na(dat_mddw$Mean)], 
      n = 5, 
      style = "equal"
    )$brks,
    labels = as_function(~ paste0(round(.x, digits = 0), "%")),
    na.value = "white"
  ) +
  guides(fill = guide_colorbar(
    ticks.colour = "gray", 
    frame.colour = "gray", 
    title.position = "top", 
    label.hjust = c(0, 0.5, 0.5, 0.5, 0.5, 1), 
    barwidth = 12, 
    barheight = 1.2, 
    direction = "horizontal")
  ) +
  theme(
    text = element_text(family = "Whitney", color = "black"),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    legend.background = element_rect(fill = "#ffffff", color = "gray", linewidth = 0.35),
    legend.margin = margin(r = 0.2, l = 0.2, t = 0.1, b = 0.1, unit = "cm"),
    legend.position = c(0.5, -0.1),
    legend.title = element_text(size = 10, face = "bold", margin = margin(b= 1.2, unit = "mm")),
    legend.text = element_text(size = 8, face = "plain", margin = margin(t = 1.2, unit = "mm")), 
    legend.frame = element_rect(linewidth = 0.3),
    plot.margin = unit(c(0, 0.35, 0, 0.35), "cm")
  )

## 4.4 colorRampPalette in n-colour gradient ---- 
dat_mddw %>%
  ggplot() +
  geom_sf(
    aes(fill = Mean), 
    # set aesthetics to fixed values
    color = "#7f7f7f", 
    linewidth = 0.1
  ) + 
  scale_fill_gradientn(
    name = "MDD-W",
    colors = colorRampPalette(brewer.pal(9, "Spectral"))(134),
    breaks = classIntervals(
      dat_mddw$Mean[!is.na(dat_mddw$Mean)], 
      n = 5, 
      style = "equal"
    )$brks,
    labels = as_function(~ paste0(round(.x, digits = 0), "%")),
    na.value = "#e5e5e5"
  ) +
  guides(fill = guide_colorbar(
    ticks.colour = "black", 
    #frame.colour = "", 
    title.position = "top", 
    label.hjust = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5), 
    barwidth = 12, 
    barheight = 1.2, 
    direction = "horizontal")
  ) +
  theme(
    text = element_text(family = "Whitney", color = "black"),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    legend.background = element_rect(fill = "#ffffff"),
    legend.margin = margin(r = 0.2, l = 0.2, t = 0.1, b = 0.1, unit = "cm"),
    legend.position = c(0.5, -0.1),
    legend.title = element_text(size = 10, face = "bold", margin = margin(b= 1.2, unit = "mm")),
    legend.text = element_text(size = 8, face = "plain", margin = margin(t = 1.2, unit = "mm")), 
    plot.margin = unit(c(0, 0.35, 0, 0.35), "cm")
  )

# Paste your desired file path
ggsave(file = "GWP_DQQ_2021-2022-2023_MDD-W-Map_6.pdf", dpi = 300,
       width = 12,  height = 8, units ='in', device = cairo_pdf)

