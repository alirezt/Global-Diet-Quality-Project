library(shiny)
library(tidyverse)
library(ggiraph)
library(showtext)
library(gt)
library(gtExtras)


dgr <- read_csv("data/results_public_merged.csv")
color_palette <- c("#5e5766", "#c8875e", "#6f9894", "#e6b123", "#b95547")
names(color_palette) <- unique(dgr$Region)
font_add_google("Fraunces", "Fraunces")
font_add_google("Commissioner", "Commissioner")
showtext_auto()

# Shiny and girafe training
# crime data
ui <- fluidPage(
  h1("DQQ Scatter plot and Correlation"),
  
  fluidRow(
    column(width = 3,
           h4("Filter data"),
           selectInput(selected = "All", label = "Select a subgroup: ", inputId = "subgroup", choices = unique(dgr$Subgroup)),
           selectInput(selected = "Fish or seafood", label = "Select an indicator for x axis: ", inputId = "xvar", choices = unique(dgr$Indicator)),
           selectInput(selected = "Yogurt", label = "Select an indicator for y axis: ", inputId = "yvar",  choices = unique(dgr$Indicator))
    ),
    column(width = 6,
           h4("Select Countries: "),
           actionButton("reset", label = "Reset selection"),
           ggiraph::girafeOutput("plot")
    ),
    column(width = 3,
           h4("Hovering countries"),
           verbatimTextOutput("console"),
           h4("Selected countries"),
           tableOutput("datatab")
    )
  )
)

server <- function(input, output, session) {
  
  dgrSub <- reactive({
    # data preparation 
    dgr %>%
      filter(Subgroup == input$subgroup) %>%
      select(!c(`Lower confidence interval`, `Upper confidence interval`, `DQQ Names`)) %>%
      mutate(`Income classification` = parse_factor(`Income classification`, levels = c("L", "LM", "UM", "H"), ordered = T)) %>%
      group_by(`Income classification`, Region, Country, ISO3, Subgroup) %>% 
      pivot_wider(names_from = Indicator, values_from = `Mean/Prevalence`) %>%
      # interactive ggplot 
      ggplot(aes(x = .data[[input$xvar]], y = .data[[input$yvar]])) +
      geom_point_interactive(aes(fill = Region, 
                                 size = `Income classification`, 
                                 data_id = Country, 
                                 tooltip = paste0("Country = ", Country, 
                                                  "\n", input$xvar, " = ", .data[[input$xvar]], # use .data to access to data-variable of dgr
                                                  "\n", input$yvar, " = ", .data[[input$yvar]])),
                             stroke = NA, alpha = 0.95, shape = 21, hover_nearest = TRUE) +
      guides(fill = guide_legend(override.aes = list(size = 6, color = "white"), keyheight = unit(0.7, units = "cm"), keywidth = unit(0.5, units = "cm")),
             size = guide_legend(keyheight = unit(0.7, units = "cm"), keywidth = unit(0.5, units = "cm"))) +
      geom_smooth_interactive(color = "#afb9b5", 
                              fill = "#afb9b5", 
                              se = TRUE, 
                              #data_id="smooth", 
                              span = 10)+
      scale_fill_manual(values = color_palette) + # factors as names, colors as values 
      labs(title = "Scatter plots of DQQ food groups") +
      theme_minimal() +
      theme(
        text = element_text(family = "Commissioner", colour = "#2F4F4F", face = "bold"),
        plot.title.position = "plot",
        plot.title = element_text(family = "Fraunces", size = 24, colour = "#2F4F4F",
                                  margin = unit(c(0.5, 0, 0.5, 0), "cm")),
        axis.text = element_text(size = 10, vjust = 0.3, colour = "#2F4F4F"),
        axis.title.y = element_text(size = 12, vjust = 2, colour = "#2F4F4F"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#F0F5F5", colour = "#F0F5F5"),
        panel.background = element_rect(fill = "#F0F5F5", colour = "#F0F5F5"),
        legend.key.height = unit(0.023, "npc"),
        legend.key.width = unit(0.023, "npc")
      )
  })
  
  output$plot <- renderGirafe({
    girafe(ggobj = dgrSub(),
           width_svg = 7, height_svg = 6,
           options = list( # a list of options for girafe rendering
             opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;", reactive = TRUE), # how to render hovered graphics
             opts_selection(type = "multiple", css = "fill:#FF3333;stroke:black;") # how to render selected graphics 
           ))
  })
  
  # connected to type = "multiple" argument in opts_selection
  selected_state <- reactive({
    input$plot_selected 
  })
  
  # connected to reactive = TRUE argument opts_hover
  output$console <- renderPrint({
    input$plot_hovered 
  })
  
  # girafeid_set (name of the input id of the reactive output value + _set)
  observeEvent(input$reset, {
    session$sendCustomMessage(type = 'plot_set', message = character(0))
  })
  
  output$datatab <- render_gt({
    out <- dgr[dgr$Country %in% selected_state(), ]
    out <- out %>%
      filter(Indicator %in% c(input$xvar, input$yvar)) %>%
      select(Country, Subgroup, Indicator, `Mean/Prevalence`)
      
    
    if(nrow(out) < 1 ) return(NULL)
    row.names(out) <- NULL
    out%>% 
      gt() %>% 
      gt_theme_nytimes() %>% 
      tab_header(title = "DQQ values for selected countries") %>%
      gt_plt_bar_pct(column = `Mean/Prevalence`, scaled = FALSE, fill = "blue", background = "lightblue") 
      #cols_align("center", contains("scale")) %>%
      #cols_width(4 ~ px(125), 5 ~ px(125))
  })
}

shinyApp(ui, server)
