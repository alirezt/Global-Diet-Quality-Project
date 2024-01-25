linesdata <- data.frame(
  xvals = c(1.5, 2.5, 3.5, 4.5),
  Subgroup=NA   # required because it complains for fill, which I cannot specify again for geom_segment.
)

dietary_ad0 <- function(df, dfline, Country_name) {
    Country_name <- rlang::parse_expr(quo_name(enquo(Country_name)))
    df %>%
    filter(Country == "Country_name" &
             Indicator %in% c("MDD-W",
                              "All-5",
                              "At least one vegetable",
                              "At least one pulse, nut, or seed",
                              "At least one animal-source food")) %>%
      
      ggplot(aes(x = Indicator, y = mean, fill = Subgroup)) +
      
      geom_col(position = position_dodge(c(-0.9, -1.1, -1.1, -0.9)), width = 0.81) +
      
      ## It's recommended to use a named vector for values
      # breaks to control the appearance of the legend.
      scale_fill_manual(values = colorPanel, 
                        breaks = c("Urban", "Rural"), 
                        labels = labels[c("Urban", "Rural")],
                        aesthetics = c("color", "fill"),
                        guide = guide_legend(order = 1, byrow = TRUE)) +
      
      new_scale_fill() +
      new_scale_color() +
      
      geom_col(aes(fill = Subgroup), 
               position = position_dodge(c(-0.9, -1.1, -1.1, -0.9)), 
               width = 0.81) +
      
      scale_fill_manual(values = colorPanel, 
                        breaks = c("Female", "Male"), 
                        labels = labels[c("Female", "Male")], 
                        aesthetics = c("color", "fill"),
                        guide = guide_legend(order = 2, byrow = TRUE)) +
      
      scale_y_continuous(
        breaks = c(0, 20, 40, 60, 80), # where the ticks appear
        expand = c(0, 0),
        limits = c(0, 100),
        labels = function(x) paste0(x, "%") # choose your own labels
      )+
      
      scale_x_discrete(limits = c("At least one vegetable",
                                  "At least one pulse, nut, or seed",
                                  "At least one animal-source food",
                                  "All-5",
                                  "MDD-W"),
                       
                       labels = c("MDD-W"  = "Minimum \n dietary diversity \nfor women ",
                                  "All-5" = "Consumed all 5 \nrecommended \nfood groups ", 
                                  "At least one vegetable" = "At least \n one fruit or \n vegetable ",
                                  "At least one pulse, nut, or seed" = "Pulses, nuts, \n or seeds " ,
                                  "At least one animal-source food" = "Animal\u002D \nsource foods "), 
                       expand = c(.11,.11)) +
      
      geom_text(aes(label = paste0(mean,"%"), y = mean - 1), 
                color = "white", 
                size = 4.5, 
                vjust = rep(c(0.5, 0.7, 0.5, 0.3), 5), 
                hjust = 1, 
                facefont = "bold", 
                family = "Whitney semibold",   
                position = position_dodge(-.9), 
                alpha = .9, 
                show.legend = FALSE) +
      
      coord_flip(clip="off") + 
      
      geom_segment(aes(x=0.5613, xend=5.4387, y = 0, yend = 0), size = .5) +
      
      geom_segment(
        data= linesdata, y=-48.3, yend= 100,
        aes(x=xvals, xend=xvals), show.legend = FALSE, size = 0.15) +  
      
      # scale_fill_manual(values = colorPanel) + 
      labs( x= " ", 
            y = " ") + 
      ggtitle("FIGURE 1. Dietary adequacy") + 
      #geom_text(aes(label = "FIGURE 1. Dietary adequacy", y = 0, x = 5.75), hjust = .68, family = "Whitney", facefont = "bold", color = '#007da5', size = 5.25)+
      geom_text(aes(label = "Not applicable", y = 0, x = 4.63), 
                hjust = -.05, 
                family = "Whitney", 
                facefont = "plain", 
                color = '#42225C', 
                size = 4.5) +
      theme(
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(family = "whitney", 
                                  face = "bold", 
                                  color = '#187DA2', 
                                  size = 15, 
                                  hjust = -2.751),
        
        panel.border = element_blank(),
        axis.text.y = element_text(family = "whitney", 
                                   color = "black", 
                                   size = 15, 
                                   lineheight = 1),
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        plot.margin = unit(c(0, .5, 0, 0), "cm"),
        aspect.ratio = 1.55,
        axis.ticks.y.left = element_line(colour = "green"),
        axis.ticks.length = unit(0, "cm"),
        axis.title.x = element_text(size = 8, 
                                    color = "black", 
                                    family = "Whitney"),
        axis.text = element_text(size = 8, 
                                 color = "black", 
                                 family = "Whitney"),
        legend.text = element_markdown(size = 12, 
                                       family = "whitney",
                                       margin = margin(t = -2)),
        legend.key = element_rect(colour = NA, 
                                  fill = NA, 
                                  size = 4),
        legend.key.height = unit(0.023, "npc"),
        legend.key.width = unit(0.023, "npc"),
        legend.margin = margin(t = 1.5, r = 1, b = 1.5, l = 1),
        legend.title = element_blank(),
        ## Second = 0.70
        legend.position = c(0.88, 0.7),
        legend.spacing.y  = unit(3, "pt"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank()
      )
}

dietary_ad0(df = df1, dfline = linesdata ,  Country_name = "Jordan")
q

# 2- try some examples ----
library(tidyverse)

if(.Platform$OS.type == "windows") {
  
  ft.url = "https://www.cufonfonts.com/download/font/whitney-2"
  dir.create(file.path(getwd(), "font"))
  #dest = paste(getwd(), "font", basename(ft.url), sep = "/")
  download.file("https://www.cufonfonts.com/download/font/whitney-2", 
                "font/whitney-2.zip", mode = "wb")
  unzip(file.path("font/whitney-2.zip"), exdir = paste("font", basename(ft.url), sep = "/"))
  
  font_add("Whitney", 
           regular = "whitneybook.otf", 
           bold = "whitneybold.otf",
           italic = "whitneybookitalic.otf", 
           bolditalic = "WhitneySemiboldItalic.otf")
  
  showtext_auto()
} else {
  
  quartzFonts(whitney = c("Whitney Book", 
                          "Whitney Black", 
                          "Whitney Book Oblique", 
                          "Whitney Black Oblique"))
  
  par(family = "whitney")
}

iris %>%
  ggplot(aes(x = Species, y = Sepal.Length)) +
  geom_col() + 
  ggtitle("Graph Example One") +
  theme(text = element_text(family = "Whitney", 
                            face = "bold", 
                            color = '#187DA2', 
                            size = 18))

showtext_auto()
font_files()
font_paths()
font_paths("font/whitney-2")

font_add("Whitney", 
         regular = "whitneybook.otf", 
         bold = "whitneybold.otf",
         italic = "whitneybookitalic.otf", 
         bolditalic = "WhitneySemiboldItalic.otf")

font_add("Kanit", 
         regular = "wqy-microhei/Kanit/Kanit-Regular.ttf", 
         bold = "wqy-microhei/Kanit/Kanit-Bold.ttf")

font_families()
font_add_google("Montserrat", "Montserrat")
font_add_google("Roboto", "Roboto")
font_add_google("Kanit", "Kanit")
font_add("Palatino", "pala.ttf")
font_families()

a <- 
  ggplot(mtcars, aes(x = wt, y = mpg)) + 
  geom_point() +
  ggtitle("Fuel Efficiency of 32 Cars") +
  xlab("Weight (x1000 lb)") + ylab("Miles per Gallon") +
  theme(text = element_text(size = 30, family = "Kanit", face = "bold")) +
  annotate("text", 4, 30, label = 'Palatino Linotype',
           family = "Whitney", size = 10) +
  annotate("text", 1, 11, label = 'Roboto', hjust = 0,
           family = myFont2, size = 10)

## On-screen device
print(a)
showtext_auto(FALSE)

# Figure 2 Train ----
gg <- df1 %>%
  filter(Country == "Jordan" & Indicator %in% c("At least one fruit", "At least one vegetable", "Pulses", "Nuts or seeds", "Whole grains")) %>%
  ggplot(aes(x = Indicator, y = mean, fill = Subgroup)) +
  geom_col(position = position_dodge(c(-0.9, -1.1, -1.1, -0.9)), width = 0.81) +
  scale_fill_manual(values = colorPanel, breaks = c("Urban", "Rural"), labels = labels[c("Urban", "Rural")], guide = guide_legend(order = 1, byrow = TRUE)) +
  new_scale_fill() +
  geom_col(aes(fill = Subgroup), position = position_dodge(c(-0.9, -1.1, -1.1, -0.9)), width = 0.81) +
  scale_fill_manual(values = colorPanel, breaks = c("Female", "Male"), labels = labels[c("Female", "Male")], guide = guide_legend(order = 1, byrow = TRUE)) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80), labels = function(x) paste0(x, "%"), limits = c(0, 100), expand = c(0, 0)) +
  scale_x_discrete(limits = rev(c("At least one fruit", "At least one vegetable", "Pulses", "Nuts or seeds", "Whole grains")),
                   labels = c("At least one fruit" = "Fruits ", "At least one vegetable" = "Vegetables ", "Whole grains" = "Whole grains ", "Pulses" = "Pulses ", "Nuts or seeds" = "Nuts \nor Seeds "), expand = c(.11,.11)) +
  geom_text(aes(label = paste0(mean,"%"), y = mean - 1), 
            color = "white", 
            size = 4.5, 
            vjust = rep(c(0.5, 0.7, 0.5, 0.3), 5), 
            hjust = 1, 
            fontface = "bold", 
            family = "Whitney",   
            position = position_dodge(-0.9), 
            alpha = .9, 
            show.legend = FALSE) +
  coord_flip(clip="off") +
  geom_segment(aes(x=0.5613, xend=5.4387, y = 0, yend = 0), linewidth = .5) +
  geom_segment(data= linesdata, y=-48.3, yend= 100, aes(x=xvals, xend=xvals), show.legend = FALSE, linewidth = 0.15) +
  labs(x= "", y = "") +
  ggtitle("FIGURE 2. Dietary factors protective of NCDs g G") +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(family = "Whitney", face = "bold", color = '#187DA2', size = 15, hjust = 1.44),
    panel.border = element_blank(),
    axis.text.y = element_text(family = "Whitney", color = "black", size = 15, lineheight = 1),
    axis.text.x = element_blank(),
    #axis.line.x = element_blank(),
    #axis.line.y = element_blank(),
    plot.margin = unit(c(0, .5, 0, 0), "cm"),
    aspect.ratio = 1.55,
    #axis.ticks.y.left = element_line(colour = "green"),
    axis.ticks.length = unit(0, "cm"),
    #axis.title.x = element_text(size = 8, color = "black", family = "Whitney"),
    #axis.text = element_text(size = 8, color = "black", family = "Whitney"),
    legend.text = element_markdown(size = 12, family = "Whitney", margin = margin(t = -2)),
    legend.key = element_rect(colour = NA, fill = NA, size = 4),
    legend.key.height = unit(0.023, "npc"),
    legend.key.width = unit(0.023, "npc"),
    legend.margin = margin(t = 1.5, r = 1, b = 1.5, l = 1),
    legend.title = element_blank(),
    # Bottom y = 0.115, Top = 0.90
    legend.position = c(0.88, 0.115),
    legend.spacing.y  = unit(3, "pt"),
    #panel.grid.major.x = element_blank(),
    #panel.grid.minor.y = element_blank(),
    #panel.grid.major.y = element_blank(),
    #panel.grid.minor.x = element_blank()
  )


           