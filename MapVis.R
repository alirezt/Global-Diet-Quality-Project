library(shiny)
library(readxl)
library(tidyverse)
library(sf)
library(viridis)
library(showtext)
library(rnaturalearth)
library(magrittr)
library(RColorBrewer)
library(ggthemes)
library(cowplot)
library(scales)
library(maps)

# 1. sf plot ----
## 1. Trying dqqMap() function on sample data ---- 
### Data preparation ----
dqData <- read_excel("dqq-shiny1/data/Country level indicators_for website.xlsx")
head(dqData)
source("dqq-shiny1/dqqMap.R")
dqqMap(data = dqData, var = "MDD-W", group = "All", 
       style = "pretty", plt = c("#D1FBD4","#579C97", "#0E3F5C"))

datat <-  dqData %>%
  filter(Indicator == "All-5" & Subgroup == "All")

dqq_worldt <- rnaturalearth::ne_countries(returnclass = "sf", scale = 110) %>% 
  left_join(datat, by = join_by(iso_a3 == ISO3)) %>%
  mutate(Mean_prevalence = Mean_prevalence*100)

## 2. sf plotting with increasing complexity ----
plot(st_geometry(dqq_worldt))
plot(st_geometry(dqq_worldt), col = sf.colors(12, categorical = TRUE), border = 'grey', 
     axes = TRUE)
plot(dqq_worldt, max.plot = 4)
plot(dqq_worldt["Mean_prevalence"])
plot(dqq_worldt["Mean_prevalence"],
     key.pos = 4, 
     axes = FALSE,
     #pal = hcl.colors(10, "Dark Mint", rev = FALSE),
     #nbreaks = 5,
     breaks = "pretty",
     key.width = lcm(1.3), key.length = 0.75
     )

## 3. Trying with cutting the mean vector into 4 groups ----
dqq_worldt$meanF <- cut(dqq_worldt$Mean_prevalence, breaks = 20)
plot(dqq_worldt[-which(dqq_worldt$sovereignt == "Antarctica"), ]["meanF"], 
     key.pos = 4, 
     #pal = hcl.colors(20, "Dark Mint", rev = TRUE),
     pal = brewer.pal(20,"RdYlBu"), # it has a limit in numbers: 
     key.width = lcm(4.5)
     ) 

# How to surpass the limits in number of colors 
## ** point ** ----
plot(dqq_worldt[-which(dqq_worldt$sovereignt == "Antarctica"), ]["meanF"], 
     key.pos = 4, 
     pal = colorRampPalette(brewer.pal(20,"RdYlBu")),
     key.width = lcm(4.5)
)

# 2. Try sf with ggplot ----
dat <- read_csv("C:/Users/Niloofarfr/OneDrive - McGill University/R Projects/DQQ/Data/Output/CSV/results_public_merged.csv")
shp <- rnaturalearth::ne_countries(returnclass = "sf", scale = 110)

dat1 <-  dat %>%
  filter(Indicator == "All-5" & Subgroup == "All") %>%
  right_join(shp[ , c("geometry", "iso_a3")], by = join_by(ISO3 == iso_a3)) %>%
  # right join because we want the geometries of all countries.
  filter(ISO3 != "ATA") %>%
  st_as_sf()

## 1.scale_fill_viridis_c ----
dat1 %>%
  ggplot() +
  geom_sf(aes(fill = Mean_prevalence), color = "black") +
  #geom_sf_text(aes(label = ISO3), size = 2)
  #geom_sf_label(aes(label = ISO3), size = 1.5) +
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "white") +
  labs(title = "All-5", 
       subtitle = "Global variations of All-5 indicator", 
       caption = "Mean: sd: ")

## 2.scale_fill_viridis and more complexity ---- 
names(dat1); class(dat1)

## ** point ** ----
quantiles <- quantile(x = dat1$Mean_prevalence, 
                      probs = seq(0, 1, 0.2), # numeric vector of probabilities 
                      na.rm = TRUE) %>% as.vector()

labels <- imap_chr(quantiles, function(x, idx){
  return(paste0(round(quantiles[idx], 0), "%", " – ", round(quantiles[idx + 1], 0), "%"))
  })

labels <- labels[1:length(labels) - 1]

dat1 %<>%
  mutate(mean_quantiles = cut(Mean_prevalence,
                              breaks = quantiles,
                              labels = labels,
                              include.lowest = T))

### try discrete ----
dat1 %>%
  ggplot() +
  geom_sf(aes(fill = `mean_quantiles`), color = "black", size = 0.5) +
  scale_fill_viridis(
    option = "A",
    #na.translate = TRUE,
    #na.value = "white",
    name = "All-5 ranges",
    alpha = 0.9, 
    begin = 0.1, 
    end = 0.9,
    discrete = T, 
    direction = -1, # this change the order of color in map
    ) +
  guides(fill = guide_legend(direction = "horizontal",
                             label.hjust = 0.5,
                             nrow = 1,
                             byrow = T,
                             reverse = F,
                             label.position = "bottom",
                             keyheight = unit(1.75, units = "mm"),
                             keywidth = unit(16, units = "mm"))) +
  labs(x = NULL,
       y = NULL,
       title = "Global variation of All-5",
       subtitle = "Percentage ranges of All-5 indicators across the globe",
       caption = "Map CC-BY-SA; Code:github.com/alirezt") +
  
  theme_minimal() +
  theme(
    text = element_text(family = "Whitney", face = "bold", color = "black"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.background = element_rect(fill = "#f5f3f4", color = NA),
    panel.background = element_rect(fill = "#f5f3f4", color = NA),
    legend.background = element_rect(fill = "#f5f3f4", color = NA),
    legend.position = "bottom",
    plot.margin = unit(c(.5, .5, .2, .5), "cm"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12, hjust = 0, face = "bold", color = "#4f504b", family = "Whitney"),
    plot.title = element_text(size = 20, hjust = 0.5, color = "#4f504b", family = "Whitney"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#4f504b", 
                                 margin = margin(b = 0.2, t = -0.1, l = 2, unit = "cm"), debug = F),
    plot.caption = element_text(size = 10, hjust = .5, 
                                margin = margin(t = 0.2, b = 0, unit = "cm"), color = "#939184")
)
### try continuous ----
dat1 %>%
  ggplot() +
  geom_sf(aes(fill = Mean_prevalence), color = "black", size = 0.5) +
  scale_fill_viridis(
    option = "turbo",
    #na.translate = TRUE,
    na.value = "white",
    name = "All-5 ranges",
    alpha = 0.9, 
    #begin = 0.1, 
    #end = 0.9,
    discrete = F, 
    direction = -1, # this change the order of color in map
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = "top",
      #reverse = T 
    ))

dat1 %>%
  ggplot() +
  geom_sf(aes(fill = Mean_prevalence), color = "black", size = 0.5) +
  scale_fill_gradientn(name="% inadequate",
                       #labels=scales::percent, #lim=c(0,1),
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                       na.value="grey90") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                               title.position = "top", title.hjust = 0.5))


# 3. Shiny with ggplot and sf ----
dat2 <-  dat %>%
  right_join(shp[ , c("geometry", "iso_a3")], by = join_by(ISO3 == iso_a3)) %>%
  filter(ISO3 != "ATA") %>%
  st_as_sf()


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  plotOutput("map", width = "100%", height = "110%"),
  tableOutput("table"),
  absolutePanel(bottom = 10, left = 10, draggable = TRUE,
                selectInput(selected = "All", label = "Select a subgroup: ", inputId = "subgroup", choices = unique(dat2$Subgroup)),
                selectInput(selected = "Fish or seafood", label = "Select an indicator", inputId = "var", choices = unique(dat2$Indicator)),
                sliderInput(inputId = "quant", label = "Select the number of quantiles", value = 3, min = 3, max = 5)
  )
)

server <- function(input, output, session){
  
  dat3 <- reactive({
    dat2 %>% 
      filter(Subgroup == input$subgroup & Indicator == input$var)
  })
  
  quantiles <- reactive({
    dat3() %>%
      pull(var = Mean_prevalence) %>%
      quantile(probs = seq(0, 1, length.out = input$quant +1), na.rm = TRUE) %>% 
      as.vector()
  })
  
  labels <- reactive({
    imap_chr(quantiles(), function(x, idx){
      return(paste0(round(quantiles()[idx], 0), "%", " – ", round(quantiles()[idx + 1], 0), "%"))
    })
  })
  
  dat4 <- reactive({
    mylabels <- labels()[1:length(labels()) - 1]
    dat3() %>%
      mutate(mean_quantiles = cut(Mean_prevalence,
                                  breaks = quantiles(),
                                  labels = mylabels,
                                  include.lowest = T))
  })
  
  output$map <- renderPlot({
    global = map_data('world')
    global <- global[global$region != "Antarctica", ]
    dat4() %>%
      ggplot() +
      geom_polygon(data = global, aes(x=long, y=lat, group = group), fill='NA', color='black', size=0.2) +
      geom_sf(aes(fill = `mean_quantiles`), color = "black", size = 0.5) +
      scale_fill_viridis(
        option = "A",
        alpha = 0.9, 
        begin = 0.1, 
        end = 0.9,
        discrete = T, 
        direction = -1, 
        ) +
      guides(fill = guide_legend(direction = "horizontal",
                                 label.hjust = 0.5,
                                 nrow = 1,
                                 byrow = T,
                                 reverse = F,
                                 label.position = "bottom",
                                 keyheight = unit(1.75, units = "mm"),
                                 keywidth = unit(16, units = "mm"))) +
      labs(x = NULL,
           y = NULL,
           title = paste("Global map of", input$var, sep = " "),
           subtitle = paste("Global variation of", input$var, "indicator", "in", input$subgroup, "sub-class", sep = " "),
           caption = "Map CC-BY-SA; Code:github.com/alirezt") +
      
      theme_minimal() +
      theme(
        text = element_text(family = "Whitney", face = "bold", color = "black"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "#f5f3f4", color = NA),
        panel.background = element_rect(fill = "#f5f3f4", color = NA),
        legend.background = element_rect(fill = "#f5f3f4", color = NA),
        legend.position = "bottom",
        plot.margin = unit(c(.5, .5, .2, .5), "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, hjust = 0, face = "bold", color = "#4f504b", family = "Whitney"),
        plot.title = element_text(size = 20, hjust = 0.5, color = "#4f504b", family = "Whitney"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#4f504b", 
                                     margin = margin(b = 0.2, t = -0.1, l = 2, unit = "cm"), debug = F),
        plot.caption = element_text(size = 10, hjust = .5, 
                                    margin = margin(t = 0.2, b = 0, unit = "cm"), color = "#939184")
      )
  }) 
  
}

shinyApp(ui, server)
 

# 4. Difference Maps ----
dat <- read_csv("DQQ_GWP_2021-2022_Internal_17Feb2024.csv")
names(dat)
  
## 4.1 data preparation ----
shp <- rnaturalearth::ne_countries(returnclass = "sf", scale = 110)

# Example: Soft Drink for Male
dat_dif <-  dat %>%
  filter(Indicator == "Soft drink consumption" & Diff_p < 0.05 & Subgroup == "Male") %>%
  right_join(shp[ , c("geometry", "iso_a3")], by = join_by(ISO3 == iso_a3)) %>%
  filter(ISO3 != "ATA") %>%
  st_as_sf()
 
## 4.2 Quantile for discrete version  ----
quantiles <- quantile(x = dat_dif$Difference, 
                      probs = seq(0, 1, 0.2),
                      na.rm = TRUE) %>% as.vector()

labels <- imap_chr(quantiles, function(x, idx){
  return(paste0(round(quantiles[idx], 2), 
                " – ", round(quantiles[idx + 1], 2)))
})
# or
labels <- imap_chr(quantiles, \(x, idx) paste0(round(quantiles[idx], 2), " – ", round(quantiles[idx + 1], 2)))
labels <- labels[1:length(labels) - 1]

dat_dif %<>%
  mutate(diff_fm_quantiles = cut(Difference,
                              breaks = quantiles, # number of breaks = labels+1
                              labels = labels,
                              include.lowest = T))

### maps ----
dat_dif %>%
  ggplot() +
  geom_sf(aes(fill = diff_fm_quantiles), color = "black", size = 0.5) +
  scale_fill_viridis(
    option = "A",
    na.translate = TRUE,
    na.value = "white",
    name = "All-5 ranges",
    alpha = 0.9, 
    begin = 0.1, 
    end = 0.9,
    discrete = T, 
    direction = -1, # this change the order of color in map
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = "top",
      reverse = T 
    )) +
  guides(fill = guide_legend(direction = "horizontal",
                             label.hjust = 0.5,
                             nrow = 1,
                             byrow = T,
                             reverse = F,
                             label.position = "bottom",
                             keyheight = unit(1.75, units = "mm"),
                             keywidth = unit(16, units = "mm"))) +
  labs(x = NULL,
       y = NULL,
       title = "Soft Drink Consumption Gender Differences Map",
       subtitle = "Global distribution of differences between male and female for Sfot Drink for year 2021 and 2022",
       caption = "Map CC-BY-SA; Code:github.com/alirezt") +
  
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
    legend.position="bottom",
    plot.margin = unit(c(.5, .5, .2, .5), "cm"),
    panel.border = element_blank(),
    panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8, hjust = 0, face = "bold", color = "#4f504b", family = "Whitney"),
    plot.title = element_text(size = 16, hjust = 0.5, color = "#4f504b", family = "Whitney"),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "#4f504b", 
                                 margin = margin(b = 0.2, t = -0.1, l = 2, unit = "cm"), debug = F),
    plot.caption = element_text(size = 10, hjust = .5, 
                                margin = margin(t = 0.2, b = 0, unit = "cm"), color = "#939184")
  )

ggsave(filename = "plotdir/FGDS gender differences.png",
       width=8.5, height=4.5, units="in", dpi=200)

## 4.3 Continuous ----
dat_dif %>%
  ggplot() +
  geom_sf(aes(fill = Difference), color = "black", size = 2) +
  scale_fill_gradientn(name="Male minus Female",
                       #labels=scales::percent, #lim=c(0,1),
                       colors = RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                       na.value = "grey90", trans = "identity") +
  
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                               title.position = "top", title.hjust = 0.5, 
                               barwidth = 8, barheight = 0.5)) +
  labs(x = NULL,
       y = NULL,
       title = "Soda Consumption Urban/Rural Differences Map",
       #title = "Soda Consumption Male/Female Differences Map", 
       subtitle = "Global distribution of differences between urban and rural soda Consumption",
       #subtitle = "Global distribution of differences between male and female soda Consumption",
       caption = "Map CC-BY-SA; Code:github.com/alirezt") +
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
    legend.position="bottom",
    plot.margin = unit(c(.5, .5, .2, .5), "cm"),
    panel.border = element_blank(),
    panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
    legend.title = element_text(size = 12, face = "bold", color = "#4f504b", family = "Whitney"),
    legend.text = element_text(size = 12, hjust = 0.4, face = "bold", color = "#4f504b", family = "Whitney"),
    plot.title = element_text(size = 16, hjust = 0.5, color = "#4f504b", family = "Whitney"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#4f504b", 
                                 margin = margin(b = 0.2, t = -0.1, l = 2, unit = "cm"), debug = F),
    plot.caption = element_text(size = 10, hjust = .5, 
                                margin = margin(t = 0.2, b = 0, unit = "cm"), color = "#939184")
  )

ggsave(filename = "plotdir/Soda urban-rural differences.png",
       width=8.5, height=4.5, units="in", dpi=200)
  
  
### 4.3.1 with facet ----
#### 4.3.1.1 data preparation ----
# Differences for urban and rural are same, so we can use either of them (Subgroup %in% c("Urban))
dat_dif_all <- dat %>%
  filter(Indicator %in% c("Food group diversity score", "NCD-Risk", "NCD-Protect") & Subgroup %in% c("Male")) %>%
  right_join(shp[ , c("geometry", "iso_a3")], by = join_by(ISO3 == iso_a3)) %>%
  filter(ISO3 != "ATA") %>%
  st_as_sf()

dat_dif <- dat %>%
  filter(Indicator %in% c("Food group diversity score", "NCD-Risk", "NCD-Protect") & Subgroup %in% c("Male")) %>%
  right_join(shp[ , c("geometry", "iso_a3")], by = join_by(ISO3 == iso_a3)) %>%
  filter(ISO3 != "ATA") %>%
  st_as_sf()

baseMap <- shp %>% filter(iso_a3 != "ATA")

#### 4.3.1.2 facet look up table for labels ----
sub_labs <- c(
  Male = "Male minus Female"
)

ind_labs <- c(
  "Sugar-sweetened soft drink consumption" = "Soda",
  "At least one animal-source food" = "Animal-Source Food"
)

ind_labs1 <- c(
  "NCD-Risk" = "NCD-Risk",
  "NCD-Protect" = "NCD-Protect",
  "Food group diversity score" = "DDS"
  
)

#### 4.3.1.3 setting color palette ----
lowCol = colorRampPalette(c("#3288BD", "#FFFFBF"))
highCol = colorRampPalette(c("#FFFFBF", "#D53E4F"))


#### maps ----
facetg <- dat_dif %>%
  filter(!is.na(Difference)) %>%
  ggplot() +
  geom_sf(data = baseMap, fill = "transparent") +
  geom_sf(aes(fill = Difference), size = 0.1) +
  #geom_sf(data = subset(dat_dif_all, Diff_p >= 0.05), fill = "#C4C4C4") +
  facet_grid(rows = vars(Indicator), cols = vars(Subgroup), #Subgroup ~ Indicator, 
             labeller = labeller(Indicator = ind_labs1, Subgroup = label_value)) +
  
  scale_fill_gradientn(name= paste("Men are higher", "Women are higher", sep = "                                                                                         "),
                       colors = RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                       #values = rescale(c(-1.1, -0.82, -0.55, -0.275, 0, 0.336, 0.504, 0.672, 0.84), from = c(-1.1,0.84)),
                       #values = rescale(c(-6.3, -4.725, -3.15, -1.575, 0, 5.075, 10.15, 15.225, 20.3), from = c(-6.3,20.3)),
                       #values = rescale(c(seq(-6.3, 0, length.out = 5), seq(5.075, 20.3, length.out = 4)), from = c(-6.3,20.3)),
                       values = rescale(c(seq(min(dat_dif$Difference, na.rm = T), 0, length.out = 5), 
                                          seq(max(dat_dif$Difference, na.rm = T)/4, max(dat_dif$Difference, na.rm = T), length.out = 4)), 
                                        from = c(min(dat_dif$Difference, na.rm = T), max(dat_dif$Difference, na.rm = T))),
                       trans = "identity"
                       #colors = c(lowCol(6), highCol(20)),
  ) +
  
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                               title.position = "top", title.hjust = 0, 
                               barwidth = 15, barheight = 0.6)) +
  labs(x = NULL,
       y = NULL,
       title = "Map of Differences",
       subtitle = "Difference between means of male and female scores",
       caption = "Map CC-BY-SA; Code:github.com/alirezt") +
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
    legend.position="bottom",
    plot.margin = unit(c(.5, .5, .2, .5), "cm"),
    panel.border = element_blank(),
    strip.text.y = element_text(size = 40, face = "bold", color = "#4f504b", family = "Whitney"),
    strip.text.x = element_blank(),
    legend.title = element_text(size = 22, face = "bold", color = "#4f504b", family = "Whitney"),
    legend.text = element_text(size = 22, hjust = 0.4, face = "bold", color = "#4f504b", family = "Whitney"),
    plot.title = element_text(size = 50, hjust = 0.5, color = "#4f504b", family = "Whitney"),
    plot.subtitle = element_text(size = 40, hjust = 0.5, color = "#4f504b", 
                                 margin = margin(b = 0.5, t = -0.3, l = 2, unit = "cm"), debug = F),
    plot.caption = element_text(size = 18, hjust = .5, 
                                margin = margin(t = 0.2, b = 0, unit = "cm"), color = "#939184")
  ) 
facetg
ggsave(filename = "plotdir/Male_Female Diff Map for DDS_NCDP_NCDR (no significance)_1.png",
       width=6, height=8.5, units="in", dpi=320)

# 5. Function for differences map and histogram ----

maphist <- function(dat){
  mapmain <- dat %>%
    filter(!is.na(Difference)) %>%
    ggplot() +
    geom_sf(data = baseMap, fill = "white") +
    geom_sf(data = subset(dat_dif_all, Diff_p > 0.05), fill = "#C4C4C4") +
    geom_sf(aes(fill = Difference*100), color = "black", size = 2) +
    scale_fill_gradientn(name="Male minus Female or Urban minus Rural",
                         colors = RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                         values = rescale(c(-6.3, -4.725, -3.15, -1.575, 0, 5.075, 10.15, 15.225, 20.3), 
                                          from = c(-6.3,20.3)),
    ) +
    
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                                 title.position = "top", title.hjust = 0.5, 
                                 barwidth = 8, barheight = 0.5)) +
    labs(x = NULL,
         y = NULL,
         title = "Map of Differences",
         subtitle = "Difference between means of urban and rural, and male and female scores",
         caption = "Map CC-BY-SA; Code:github.com/alirezt") +
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
      legend.position="bottom",
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      legend.justification = "center",
      legend.box.just = "center",
      strip.text = element_text(size = 22, face = "bold", color = "#4f504b", family = "Whitney"),
      legend.title = element_text(size = 22, face = "bold", color = "#4f504b", family = "Whitney"),
      legend.text = element_text(size = 22, hjust = 0.4, face = "bold", color = "#4f504b", family = "Whitney"),
      plot.title = element_text(size = 30, hjust = 0.5, color = "#4f504b", family = "Whitney"),
      plot.subtitle = element_text(size = 24, hjust = 0.5, color = "#4f504b", 
                                   margin = margin(b = 0.2, t = -0.1, l = 2, unit = "cm"), debug = F),
      plot.caption = element_text(size = 18, hjust = .5, 
                                  margin = margin(t = 0.2, b = 0, unit = "cm"), color = "#939184")
    )
  
  histpatch <- dat %>%
    ggplot() +
    geom_histogram(aes(Difference), binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))
  
  ggdraw(mapmain) +
    draw_plot(histpatch + theme(legend.position="none") + theme_cowplot(12), .04, .25, .25, .2)
    
} 

maphist(dat = dat_dif)

ggsave(filename = "plotdir/test19.png",
       width=12, height=6, units="in", dpi=320)

# 6. Histograms and counts for NCDR and NCDP ----
names(dat)
dat <- data.frame(dat)
dat %>%
  filter(Year == 2021) %>%
  select(c("ISO3", "Subgroup", "Indicator", "Mean_prevalence")) %>%
  pivot_wider(names_from = Subgroup, values_from = Mean_prevalence) %>%
  mutate(M_minus_F = Male - Female) %>%
  filter(Indicator == "NCD-Risk") %>%
  ggplot(aes(M_minus_F)) +
  geom_histogram()

dat %>%
  filter(Year == 2022) %>%
  select(c("ISO3", "Subgroup", "Indicator", "Mean_prevalence")) %>%
  pivot_wider(names_from = Subgroup, values_from = Mean_prevalence) %>%
  mutate(M_minus_F = Male - Female) %>%
  filter(Indicator == "NCD-Protect") %>%
  count(sign(M_minus_F))

# NCD-Risk: 43 positive, 12 negative and one exactly zero
# NCD-Protect: 27 positive, 28 negative 

# 7. Three good examples for scale_fill_gradientn ----
## 7.1 ----
log_both <- function(x){ifelse(x == 0, 0, log(abs(x)) * sign(x))}
exp_both <- function(x){exp(abs(x)) * sign(x)} # this is the inverse of log_both

log_both_trans <- 
  function(){
    trans_new(name = 'log_both', 
              transform = log_both,
              inverse = exp_both)
  }

df <- tibble(y = (-10:10),
             x = (y^4)*sign(y))

ggplot(df) +
  #no transformation
  geom_point(aes(factor(x), y = 1, fill = x), shape = 21, size = 10) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       guide = guide_colorbar(order = 1)) +
  #transformed
  geom_point(aes(factor(x), y = - 1, color = x), size  = 10) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red",
                        trans = "log_both",
                        breaks = c(-10000, -100, 0, 100, 10000), # desired breaks on transformed scale
                        guide = guide_colorbar(order = 2)) +
  ylim(-2, 2) +
  labs(colour = "transformed", fill = "default", x = "", y = "")

## 7.2 ----
colour_breaks <- c(10, 20, 30)
colours <- c("darkblue", "lightblue", "yellow")

ggplot(mpg, aes(displ, hwy, colour = cty)) +
  geom_point() +
  scale_colour_gradientn(
    limits  = range(mpg$cty),
    colours = colours[c(1, seq_along(colours), length(colours))],
    values  = c(0, scales::rescale(colour_breaks, from = range(mpg$cty)), 1),
  )

## 7.3 ----
plot.demo <- function(title.width = 20,
                      title.position = "top",
                      legend.direction = "vertical"){
  ggplot(iris, 
         aes(x=Sepal.Length, y=Sepal.Width, color=Petal.Width)) + 
    geom_point(size = 3) +
    scale_color_distiller(palette = "YlGn",
                          name = stringr::str_wrap("Long legend heading should be centered",
                                                   width = title.width), 
                          guide = guide_colourbar(title.position = title.position),
                          direction = -1) +
    theme(legend.title.align = 0.5,
          legend.direction = legend.direction)
}

cowplot::plot_grid(plot.demo(),
                   plot.demo(title.position = "left"),
                   plot.demo(title.position = "bottom"),
                   plot.demo(title.width = 10, title.position = "right"),
                   plot.demo(title.width = 50, legend.direction = "horizontal"),
                   plot.demo(title.width = 10, legend.direction = "horizontal"),
                   ncol = 2)






