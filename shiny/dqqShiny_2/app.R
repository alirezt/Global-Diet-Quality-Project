library(readr)
library(shiny)
library(sf)
library(dplyr)
library(viridis)
library(showtext)
library(ggplot2)
library(purrr)
library(rnaturalearth)
library(maps)


font_paths("www")
font_add("Whitney", regular = "whitneybook.otf", 
           bold = "whitneybold.otf",
           italic = "whitneybookitalic.otf", 
           bolditalic = "whitneysemibold.otf")
  
#font_add_google("Fraunces", "Fraunces")
#font_add_google("Commissioner", "Commissioner")
showtext_auto()
par(family = "Whitney")

dat <- read_csv("data/results_internal_merged.csv")
shp <- ne_countries(returnclass = "sf", scale = 110)

dat2 <-  dat %>%
  right_join(shp[ , c("geometry", "iso_a3")], by = join_by(ISO3 == iso_a3)) %>%
  filter(ISO3 != "ATA") %>%
  st_as_sf()


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  plotOutput("map", width = "100%", height = "110%"),
  tableOutput("table"),
  absolutePanel(bottom = 10, left = 10, draggable = TRUE, width = "250px", height = "250px",
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
      pull(var = `Mean/Prevalence`) %>%
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
      mutate(mean_quantiles = cut(`Mean/Prevalence`,
                                  breaks = quantiles(),
                                  labels = mylabels,
                                  include.lowest = T))
  })
  
  output$map <- renderPlot({
    global <- map_data('world')
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
                                 keyheight = unit(4, units = "mm"),
                                 keywidth = unit(30, units = "mm"))) +
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

 




