library(tidyverse)
library(showtext)
library(ggiraph)
library(patchwork)
library(shiny)
library(gt)
showtext_auto() # necessary 

# Load analysis format of DQQ data ----
d <- read_csv("Data/Output/CSV/dAnalysis.csv")
dgr <- read_csv("Data/Output/CSV/results_public_merged.csv")

# 1. Data Visualizations ----
## 1.1 Correlation ----
scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

dgrWide <- dgr %>%
  filter(Subgroup == "All") %>%
  select(!c(`Lower confidence interval`, `Upper confidence interval`, `DQQ Names`)) %>%
  group_by(`Income classification`, Region, Country, ISO3, Subgroup) %>% # such a tricky point
  pivot_wider(names_from = Indicator, values_from = `Mean/Prevalence`) %>%
  rename(
    all5 = "All-5", 
    all5a = "At least one vegetable",
    all5b = "At least one fruit",
    all5c = "At least one pulse, nut, or seed",
    all5d = "At least one animal-source food",
    all5e = "At least one starchy staple food",
    fgds = "Food group diversity score",
    ncdp = "NCD-Protect",
    ncdr = "NCD-Risk",
    gdr = "GDR score",
    DQQ11 = "Baked or grain-based sweets",
    DQQ14 = "Cheese",
    DQQ9 = "Citrus",
    dairy = "Dairy",
    dveg = "Dark green leafy vegetables",
    DQQ24 = "Deep fried foods",
    DQQ13 = "Eggs",
    DQQ29 = "Fast food", 
    DQQ20 = "Fish or seafood",
    DQQ1 = "Foods made from grains",
    DQQ27 = "Fruit juice and fruit drinks",
    DQQ23 = "Instant noodles",
    mddw = "MDD-W",
    anml = "Meat, poultry, or fish",
    DQQ25  = "Milk",
    DQQ21 = "Nuts or seeds",
    ofr = "Other fruits",
    DQQ12 = "Other sweets",
    oveg = "Other vegetables",
    DQQ22 = "Packaged ultra-processed salty snacks",
    DQQ19 = "Poultry",
    DQQ16 = "Processed meats",
    DQQ4  = "Pulses",
    safd = "Salty or fried snacks",
    snf = "Salty snacks, instant noodles, or fast food",
    swtbev = "Sugar-sweetened beverages",
    DQQ28 = "Sugar-sweetened soft drink consumption",
    swtfd = "Sweet foods",
    DQQ26  = "Sweet tea, coffee, or cocoa",
    umeat = "Unprocessed red meat",
    DQQ18 = "Unprocessed red meat (non-ruminants)",
    DQQ17 = "Unprocessed red meat (ruminants)",
    DQQ8 = "Vitamin A-rich fruits",
    DQQ5 = "Vitamin A-rich orange vegetables",
    DQQ3 = "White roots or tubers",
    DQQ2  = "Whole grains",
    DQQ15  = "Yogurt",
    zvegfr= "Zero vegetable or fruit consumption") #%>% not sure why this not worked
  # mutate(across(where(is.numeric), ~ scale2(.x, na.rm = TRUE)))

dgrWideScale <- data.frame(dgrWide[, c(1:5)], scale(dgrWide[, -c(1:5)]))
mean(dgrWideScale$fgds); sd(dgrWideScale$fgds)

### Three ways for correlogram ----
#### 1. ggcorr ----
library(GGally)
ggcorr(dgrWideScale, 
       nbreaks = 8,
       low = "#ad7d2a",
       mid = "white",
       high = "#375248",
       geom = "circle",
       label = TRUE,
       min_size = 1,
       max_size = 6,
       label_color = "#F1F1F2",
       legend.size = 25,
       label_alpha = TRUE, hjust = 0.6, size = 10) +
  labs(title = "DQQ Correlogram", subtitle = "Chart of correlation statistics for DQQ food groups", caption = "one") +
  theme(plot.title = element_text(size = 120, face = "bold"),
        plot.subtitle = element_text(size = 90), 
        plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm"),
        panel.background = element_rect(fill = "#FEFCF7"))

ggsave(filename = "Correlogram_1.jpeg", path = "Data/Output/Graphs", width = 35, height = 45, units = "cm", dpi = 320)

#### 2. corrplot ----
library(corrplot)
dCor <- cor(dgrWideScale[, -c(1:5)])

jpeg(filename = "Data/Output/Graphs/Correlogram_2.jpeg", width = 1200, height = 1400, units = "px")
corrplot(dCor, method = "square", 
         order = "hclust",
         hclust.method = "ward.D2",
         addrect = 2,
         rect.col = 3,            
         rect.lwd = 3,
         type = "upper", 
         #title = "DQQ Correlogram",
         mar = c(0,0.5,0.5,0.5),
         diag = TRUE,
         #col = colorRampPalette(c("purple", "dark green"))(200),
         tl.col = "black", tl.srt = 90, tl.cex = 0.8)
dev.off()



#### 3. ggcorrmat ----
install.packages("ggcorrplot")
install.packages("ggstatsplot")
library(ggstatsplot)
library(ggcorrplot)
str(dgrWideScale)
ggcorrmat(data = dgrWideScale,
          title = "DQQ Indicators Correlogram",
          subtitle = "Correlation coefficients between DQQ-based indicators for 2021+2022 data",
          ggcorrplot.args = list(outline.color = "black", 
                                 hc.order = TRUE,
                                 pch.cex = 3,
                                 tl.srt = 90,
                                 lab_size = 6),
          colors = c("#ffa600", "white", "#003f5c")) +
  ggplot2::theme(
      plot.title = element_text(size = 120, face = "bold"),
      plot.subtitle = element_text(size = 80), 
      plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm"),
      panel.background = element_rect(fill = "#FEFCF7"), 
      legend.text = element_text(size = 60),
      legend.title = element_text(size = 80),
      legend.key.height = unit(2, 'cm'),
      legend.key.width = unit(1, 'cm'),
      plot.caption = element_blank(),
      axis.text.x = element_text(size = 50),
      axis.text.y = element_text(size = 50)
    
  )

ggsave(filename = "Correlogram_3.jpeg", path = "Data/Output/Graphs", width = 35, height = 45, units = "cm", dpi = 320)

# 3-1 grouped correlation plots
grouped_ggcorrmat(data = dgrWideScale,
                  cor.vars = c(fgds, gdr, all5), grouping.var = Region,
                  plotgrid.args = list(nrow = 1),
                  colors = c("#ffa600", "white", "#003f5c"),
                  annotation.args = list(
                    tag_levels = "a",
                    title = "Correlogram for each Region",
                    caption = "Region-wise correlation coefficients between DQQ-based indicators")) +
  ggplot2::theme(
    plot.title = element_text(size = 120, face = "bold"),
    plot.subtitle = element_text(size = 80), 
    plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm"),
    panel.background = element_rect(fill = "#FEFCF7"), 
    legend.text = element_text(size = 60),
    legend.title = element_text(size = 80),
    legend.key.height = unit(2, 'cm'),
    legend.key.width = unit(1, 'cm'),
    plot.caption = element_blank(),
    axis.text.x = element_text(size = 50),
    axis.text.y = element_text(size = 50)
  ) # not good
ggsave(filename = "Correlogram_4.jpeg", path = "Data/Output/Graphs", width = 35, height = 45, units = "cm", dpi = 320)



#### 4. ggcorrplot ----
ggcorrplot(
  dCor,
  hc.order = TRUE,
  type = "lower",
  outline.color = "white",
  ggtheme = ggplot2::theme_gray,
  colors = c("#6D9EC1", "white", "#E46726")
)

# now lets do some scatter plots based on correlated indicators
# swtbev vs swtfd
# swtbev vs ncdr but not with swtfd
# mddw vs zvegfr
# umeat vs zvegfr
# mddw vs DQQ8
# DQQ15 vs DQQ20

grf4 <- dgrWide %>%
  ggplot(aes(x = DQQ20, y = DQQ15, color = Region)) +
  geom_point_interactive(
    aes(data_id = Region, tooltip = Country), size = 3,
    hover_nearest = TRUE) +
  #scale_colour_gradient(low = "#999999", high = "orange") +
  theme_minimal()

girafe(
  ggobj = grf4,
  options = list(
    opts_hover(css = ''), ## CSS code of line we're hovering over
    opts_hover_inv(css = "opacity:0.1;"), ## CSS code of all other lines
    opts_sizing(rescale = FALSE) ## Fixes sizes to dimensions below
  ),
  height_svg = 6,
  width_svg = 9
)

## Bar plots for sierra Leone for 2 years: 2021 and 2022 ----
d %>%
  filter(ISO3 == "USA") %>%
  select(starts_with("DQQ"), YEAR_WAVE) %>%
  pivot_longer(cols = starts_with("DQQ"), 
               names_to = "DQQ", 
               values_to = "Value",
               names_transform = list(DQQ = ~readr::parse_factor(.x, ordered = TRUE))) %>%
  filter(Value == 1) %>%
  ggplot(aes(DQQ, fill = as.factor(YEAR_WAVE))) +
  #ggplot(aes(fct_relevel(DQQ, sort), fill = as.factor(YEAR_WAVE))) +
  geom_bar(position = "dodge") +
  labs(
    y = "Yes counts",
    title = "DQQ YES Answer Counts",
    subtitle = "Counts of 'Yes' answers of DQQ indicators for Sierra Leone",
    fill = "Year",
       ) +
  scale_y_continuous(expand = expansion(add = c(0, 200))) +
  scale_fill_manual(values = c("#ffa600", "#003f5c")) +
  theme(
    panel.background = element_rect(fill = "#F3F5F4", colour = "#F3F5F4"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 40, face = "bold"),
    plot.title.position = "panel",
    plot.subtitle = element_text(size = 30, vjust = 2),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.margin = unit(c(0.5, .5, 0, 0.5), "cm"),
    axis.ticks.length = unit(0, "cm"),
    legend.background = element_rect(fill = "#F3F5F4", colour = "#F3F5F4"),
    legend.text = element_text(size = 30, face = "bold", colour = "#68756E"),
    legend.title = element_text(size = 30, face = "bold", colour = "#68756E"),
    legend.position = c(0.97, .97),
    legend.justification = c("right", "top"),
    #legend.margin = margin(1, 1, 1, 1),
    legend.spacing.y = unit(0.2, 'cm'),
    legend.spacing.x = unit(0.2, 'cm'),
    axis.text.y = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 40, face = "bold"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(face="bold", size = 20, angle=90, hjust = 0.1, vjust = 0.5)
  ) # very good

ggsave(filename = "SLE.jpeg", path = "Data/Output/Graphs", width = 20, height = 10, units = "cm", dpi = 320)

## Bar plots for USA for 2 years: 2021 and 2022 ----
# set_mediocre_all() not for now
names(d)

d %>%
  filter(ISO3 == "USA") %>%
  select(starts_with("DQQ"), YEAR_WAVE) %>%
  pivot_longer(cols = starts_with("DQQ"), 
               names_to = "DQQ", 
               values_to = "Value",
               names_transform = list(DQQ = ~readr::parse_factor(.x, ordered = TRUE))) %>%
  filter(Value == 1) %>%
  ggplot(aes(DQQ, fill = as.factor(YEAR_WAVE))) +
  geom_bar(position = "dodge") +
  labs(
    y = "Yes counts",
    title = "DQQ YES Answer Counts",
    subtitle = "Counts of 'Yes' answers of DQQ indicators for United States",
    fill = "Year",
  ) +
  scale_y_continuous(expand = expansion(add = c(0, 200))) +
  scale_fill_manual(values = c("#BAABB6", "#2B1725")) +
  theme(
    panel.background = element_rect(fill = "#F3F5F4", colour = "#F3F5F4"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 40, face = "bold"),
    plot.title.position = "panel",
    plot.subtitle = element_text(size = 30, vjust = 2),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.margin = unit(c(0.5, .5, 0, 0.5), "cm"),
    axis.ticks.length = unit(0, "cm"),
    legend.background = element_rect(fill = "#F3F5F4", colour = "#F3F5F4"),
    legend.text = element_text(size = 30, face = "bold", colour = "#68756E"),
    legend.title = element_text(size = 30, face = "bold", colour = "#68756E"),
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    #legend.margin = margin(1, 1, 1, 1),
    legend.spacing.y = unit(0.2, 'cm'),
    legend.spacing.x = unit(0.2, 'cm'),
    axis.text.y = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 40, face = "bold"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(face="bold", size = 20, angle=90, hjust = 0.1, vjust = 0.5)
  ) # very good

ggsave(filename = "USA.jpeg", path = "Data/Output/Graphs", width = 20, height = 10, units = "cm", dpi = 320)

## Stacked bar plots of FGDS for income ---- 

g1 <- d %>% 
  select(EMP_2010, fgds, IncomeQuintiles, Age, Education, ISO3, Gender) %>%
  ggplot(aes(x = fgds, fill = as.factor(IncomeQuintiles)))+
  geom_bar() +
  scale_fill_manual(values = c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600"),
                    labels=c('Poorest 20%', 'Second 20%', "Middle 20%", "Fourth 20%", "Richest 20%"))+
  scale_x_binned()+
  labs(title = "Food Group Diversity Score Distribution", 
       subtitle = "Stacked bar plots of FGDS score across income groups",
       fill = "Per Capita Income Quintiles") +
  xlab(label = "FGDS Score") +
  ylab(label = "Counts") +
  facet_wrap(vars(as.factor(ISO3))) +
    theme(
      panel.background = element_rect(fill = "#F3F5F4", colour = "#F3F5F4"),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 50, face = "bold"),
      plot.subtitle = element_text(size = 30, vjust = 2),
      axis.text.y = element_text(size = 25, face = "bold"),
      axis.title.y = element_text(size = 40, face = "bold", margin = margin(r = 15)),
      axis.text.x = element_text(size = 25, face = "bold"),
      axis.title.x = element_text(size = 40, face = "bold", margin = margin(t = 15)),
      legend.text = element_text(size = 25, face = "bold", colour = "#68756E"),
      legend.title = element_text(size = 25, face = "bold", colour = "#68756E"),
      plot.margin = unit(c(0.5, .5, 0, 0.5), "cm"),
      strip.background = element_rect(fill = "#899C92", colour = "#4F5F56"),
      strip.text = element_text(size = 20, face = "bold"),
      strip.text.x = element_text(size = 20, face = "bold", colour = "white")
    )
g1 # good
ggsave(filename = "FGDS_facet_Income.jpeg", path = "Data/Output/Graphs", width = 30, height = 20, units = "cm", dpi = 320)

## Stacked bar plots of FGDS for Gender ---- 
g2 <- d %>% 
  select(EMP_2010, fgds, ISO3, Gender) %>%
  ggplot(aes(x = fgds, fill = as.factor(Gender)))+
  geom_bar() +
  scale_fill_manual(values = c("#1f3459", "#b72936"))+
  scale_x_binned()+
  labs(title = "Food Group Diversity Score Distribution", 
       subtitle = "Stacked bar plots of FGDS score for Male and Female",
       fill = "Gender") +
  xlab(label = "FGDS Score") +
  ylab(label = "Counts") +
  facet_wrap(vars(as.factor(ISO3))) +
  theme(
    panel.background = element_rect(fill = "#F3F5F4", colour = "#F3F5F4"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 50, face = "bold"),
    plot.subtitle = element_text(size = 30, vjust = 2),
    axis.text.y = element_text(size = 25, face = "bold"),
    axis.title.y = element_text(size = 40, face = "bold", margin = margin(r = 15)),
    axis.text.x = element_text(size = 25, face = "bold"),
    axis.title.x = element_text(size = 40, face = "bold", margin = margin(t = 15)),
    legend.text = element_text(size = 25, face = "bold", colour = "#68756E"),
    legend.title = element_text(size = 25, face = "bold", colour = "#68756E"),
    plot.margin = unit(c(0.5, .5, 0, 0.5), "cm"),
    strip.background = element_rect(fill = "#afdddb", colour = "#5c98b0"),
    strip.text = element_text(size = 20, face = "bold"),
    strip.text.x = element_text(size = 20, face = "bold", colour = "#1e3559")
  )
g2 # good
ggsave(filename = "FGDS_facet_Gender.jpeg", path = "Data/Output/Graphs", width = 30, height = 20, units = "cm", dpi = 320)

## girafe graphs ----
### first ----
color_palette <- thematic::okabe_ito(7) # a vector of color codes (qualitative colorscale)
cSelect <- c("IDN", "MEX", "TUN", "BFA", "NER", "AFG", "USA")
names(color_palette) <- cSelect

d %>%
  filter(ISO3 %in% cSelect) %>%
  ggplot(aes(as.factor(REG2_GLOBAL), fgds, col = ISO3)) +
  geom_point(position = ggforce::position_auto(), alpha = 0.1, size = 3) +
  #facet_wrap(vars(ISO3)) +
  scale_color_manual(values = color_palette) # not good

## Try girafe with Shiny ----
### first (constructing the girafe objects) ----
#### graph 1 ----

#color_palette <- thematic::okabe_ito(5)
color_palette <- c("#5e5766", "#c8875e", "#6f9894", "#e6b123", "#b95547")
names(color_palette) <- unique(d$Region)

grf1 <- d %>%
  select(Region, fgds, Country) %>%
  mutate(Region1 = forcats::fct_reorder(Region, fgds)) %>%
  ggplot(aes(x = fgds, y = Region1, fill = Region, data_id = Region)) +
  geom_boxplot_interactive(position = position_nudge(y = 0.25), width = 0.5) +
  scale_fill_manual(values = color_palette) +
  scale_color_manual(values = color_palette) +
  labs(title = "FGDS Score for Each Region" , 
       subtitle = "Food Group Diversity 0-10", 
       x = "FGDS", 
       y = "Region") +
  theme(
    panel.background = element_rect(fill = "#F3F5F4", colour = "#F3F5F4"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    axis.text.x = element_text(size = 8, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    plot.subtitle = element_text(size = 12, vjust = 2),
    text = element_text(color = 'grey20'),
    legend.position = 'none',
    plot.title.position = 'plot',
    #legend.text = element_text(size = 10, face = "bold", colour = "#68756E"),
    #legend.title = element_text(size = 10, face = "bold", colour = "#68756E"),
    plot.margin = unit(c(0.5, .5, 0, 0.5), "cm")
  ) 
grf1


girafe(
  ggobj = grf1,
  options = list(
    opts_hover(css = ''), ## CSS code of line we're hovering over
    opts_hover_inv(css = "opacity:0.1;"), ## CSS code of all other lines
    opts_sizing(rescale = FALSE) ## Fixes sizes to dimensions below
  ),
  height_svg = 10,
  width_svg = 9
)

#### graph 2 ----
grf2 <- d %>%
  select(Region, fgds, Country) %>%
  mutate(Country = forcats::fct_reorder(Country, fgds)) %>%
  ggplot(aes(x = fgds, y = Country, fill = Region, data_id = Region)) +
  geom_boxplot_interactive(position = position_nudge(y = 0.25), width = 0.5) +
  scale_fill_manual(values = color_palette) +
  scale_color_manual(values = color_palette) +
  labs(title = "FGDS Score for Each Country" , 
       subtitle = "Food Group Diversity 0-10", 
       x = "FGDS", 
       y = "Country") +
  theme(panel.background = element_rect(fill = "#F3F5F4", colour = "#F3F5F4"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 8, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
        axis.text.x = element_text(size = 8, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
        plot.subtitle = element_text(size = 12, vjust = 2),
        text = element_text(color = 'grey20'),
        legend.key.height = unit(1.6, 'cm'),
        legend.key.width = unit(1.6, 'cm'),
        plot.title.position = 'plot',
        legend.text = element_text(size = 10, face = "bold", colour = "#68756E"),
        legend.title = element_text(size = 10, face = "bold", colour = "#68756E"),
        plot.margin = unit(c(0.5, .5, 0, 0.5), "cm")
  ) 

girafe(
  ggobj = grf2,
  options = list(
    opts_hover(css = ''), ## CSS code of line we're hovering over
    opts_hover_inv(css = "opacity:0.1;"), ## CSS code of all other lines
    opts_sizing(rescale = FALSE) ## Fixes sizes to dimensions below
  ),
  height_svg = 10,
  width_svg = 9
)

#### graph 1 + 2 graph with patchwork ----
girafe(
  ggobj = grf1 + plot_spacer() + grf2 + plot_layout(widths = c(0.47, 0.06, 0.47)),
  options = list(
    opts_hover(css = ''),
    opts_hover_inv(css = "opacity:0.1;"), 
    opts_sizing(rescale = FALSE)
  ),
  height_svg = 10,
  width_svg = 15
)
ggsave(filename = "test.jpeg", path = "Data/Output/Graphs", width = 20, height = 15, units = "cm", dpi = 320)


### add shiny to the game ----
# test something first
dim(d)
dat <- d %>%
  mutate(Gender = "All") %>%
  bind_rows(d) %>%
  mutate(Residence = ifelse(Gender == "All", "UR", Residence)) %>%
  pivot_longer(cols = c(Residence, Gender), names_to = "Sub names", values_to = "Subgroup") %>%
  select(!"Sub names") %>%
  filter(Subgroup != "UR")

#### ui ----
ui <- fluidPage(
  theme = bslib::bs_theme(
    # Colors (background, foreground, primary)
    bg = 'white', 
    fg = '#06436e', 
    primary = colorspace::lighten('#06436e', 0.3),
    
    # Fonts (Use multiple in case a font cannot be displayed)
    base_font = c('Source Sans Pro',  'Lato', 'Merriweather', 'Roboto Regular', 'Cabin Regular'),
    heading_font = c('Oleo Script', 'Prata', 'Roboto', 'Playfair Display', 'Montserrat'),
    font_scale = 1.25
  ),
  h1('DQQ Indicators!'),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      selectInput(
        'subgroup',
        'Select a subgroup for calculation?',
        choices = unique(dat$Subgroup)
      ),
      gt_output('gt_output')
    ),
    mainPanel = mainPanel(
      girafeOutput('girafe_output', height = 600)
    )
  )
)

#### server ----
server <- function(input, output, session) {
  
  dat_subgroup <- reactive({dat |> filter(Subgroup == input$subgroup)})
  
  output$gt_output <- render_gt({
    req(input$girafe_output_selected)
    
    selected_rows <- parse_number(input$girafe_output_selected)
    
    dat_subgroup() %>% 
      slice(selected_rows) %>% 
      select(Country, Region, fgds) %>% 
      arrange(desc(fgds)) %>% 
      group_by(Region) %>% 
      gt() %>% 
      opt_stylize(style = 5) %>% 
      cols_align(columns = -fgds, align = 'left') %>% 
      cols_label(Region = 'Region', Country = 'Country', fgds = 'FGDS Score') %>% 
      tab_header(
        title = 'Selected Countries'
      ) %>% 
      tab_style(
        locations = cells_row_groups(),
        style = list(
          cell_fill(color = 'grey20'),
          cell_text(color = 'white')
        )
      )
  })

  output$girafe_output <- renderGirafe({
    
    box_plot <- dat_subgroup() %>%
      select(Region, fgds, Country, Subgroup) %>%
      mutate(Region1 = fct_reorder(Region, fgds)) %>%
      ggplot(aes(x = fgds, y = Region1, fill = Region, data_id = Region)) +
      geom_boxplot_interactive(position = position_nudge(y = 0.25), width = 0.5) +
      scale_fill_manual(values = color_palette) +
      scale_color_manual(values = color_palette) +
      labs(title = "FGDS Score for Each Region" , 
           subtitle = "Food Group Diversity 0-10", 
           x = "FGDS", 
           y = "Region") +
      theme_minimal(base_size = 10) +
      theme(
        text = element_text(
          color = 'grey20'
        ),
        legend.position = 'none',
        panel.grid.minor = element_blank(),
        plot.title.position = 'plot'
      ) 
    
    girafe(
      ggobj = box_plot,
      options = list(
        opts_hover(css = ''),
        opts_sizing(rescale = TRUE),
        opts_hover_inv(css = "opacity:0.1;")
      ),
      height_svg = 12,
      width_svg = 25
    )
    
  })
  
}
shinyApp(ui, server)

### second try () ----
#### graph 3 ----
grf3 <- d %>%
  filter(ISO3 == "SLE") %>%
  select(starts_with("DQQ"), YEAR_WAVE) %>%
  pivot_longer(cols = starts_with("DQQ"), 
               names_to = "DQQ", 
               values_to = "Value",
               names_transform = list(DQQ = ~readr::parse_factor(.x, ordered = TRUE))) %>%
  filter(Value == 1) %>%
  ggplot(aes(DQQ, fill = as.factor(YEAR_WAVE))) +
  #ggplot(aes(fct_relevel(DQQ, sort), fill = as.factor(YEAR_WAVE))) +
  geom_bar_interactive(aes(data_id = YEAR_WAVE, tooltip = DQQ), position = "dodge") +
  labs(
    y = "Yes counts",
    title = "DQQ YES Answer Counts",
    subtitle = "Counts of 'Yes' answers of DQQ indicators for Sierra Leone",
    fill = "Year",
  ) +
  scale_y_continuous(expand = expansion(add = c(0, 200))) +
  scale_fill_manual(values = c("#ffa600", "#003f5c")) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.margin = unit(c(0, .5, 0, 0.5), "cm"),
    axis.ticks.length = unit(0, "cm"),
    #aspect.ratio = 0.5,
    legend.position = c(0.97, .97),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(face="bold", size = 12, angle=90, hjust = 0.1, vjust = 0.5)
  )

girafe(
  ggobj = grf3,
  options = list(
    opts_hover(css = ''), ## CSS code of line we're hovering over
    opts_hover_inv(css = "opacity:0.1;"), ## CSS code of all other lines
    opts_sizing(rescale = FALSE) ## Fixes sizes to dimensions below
  ),
  height_svg = 6,
  width_svg = 9
) 

# End of visualization part
# 2. Data Analysis ----
# EMP_2010: Employment Status with 6 levels
# EMP_WORK_HOURS: Total Number of Hours Work Per Week with 5 levels and 98 as no answer
# WP1223: Marital status with 8 levels, 6 and 7 DK and refused 

dat0 <- d %>%
  select(EMP_2010, WP1223, Age, Education, gdr)

dim(dat0)
sum(is.na(dat0)) # 7000
dat0 <- na.omit(dat0)  
dim(dat0) #60163     

dat0 <- dat0 %>%
  filter(!(WP1223 %in% c(6, 7) | Education %in% c(4, 5)))

dat0$EMP_2010 <- as.factor(dat0$EMP_2010)
dat0$WP1223 <- as.factor(dat0$WP1223)
dat0$Education <- as.factor(dat0$Education)
glimpse(dat0)


library(leaps)  
regfit.full <- regsubsets(gdr ~ ., data = dat0, nvmax = 4)
summary(regfit.full)  




  

