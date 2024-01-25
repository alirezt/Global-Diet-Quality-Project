quartzFonts(whitney = c("Whitney Book", "Whitney Black", "Whitney Book Oblique", "Whitney Black Oblique"))
par(family = "whitney")

rm(list = ls())

library(latex2exp)
library(extrafont)
library(haven)
library(tidyverse)
library(ggplot2)
library(expss)
library(extrafont)
library(rlang)
library(Cairo)
library(cowplot)
library(ggnewscale)
library(ggtext)
library(grid)
library(gridExtra)


##########


setwd("~/Google Drive/GAIN/Gallup/Analysis")

df <- read_csv("data/Country level indicators.csv")


df$Subgroup[df$Subgroup == "Urban"]  <- 1
df$Subgroup[df$Subgroup == "Rural"]  <- 2
df$Subgroup[df$Subgroup == "Female"] <- 3
df$Subgroup[df$Subgroup == "Male"]   <- 4
df$Subgroup[df$Subgroup == "All"]    <- 5


# create the subgroup variable as factor variable 

df$Subgroup <- factor(df$Subgroup,
                      levels = c(1, 2, 3, 4, 5),
                      labels = c("Urban", "Rural", "Female", "Male", "All"))


# Multiply the proportion by 100%  and the mean values by 1;


df1  <- df %>%
  
  rename(mean_pre = "Mean_Prevalence") %>%
  filter (Subgroup %in% c("Urban", "Rural", "Female", "Male")) %>%
  
  mutate(mean = case_when(mean_pre  < 1.00 ~ mean_pre*100,
                          mean_pre  > 1.00 ~ mean_pre*1)) %>%
  mutate(LCI1 = case_when(LCI < 1.00 ~  LCI*100,
                          LCI  > 1.00 ~  LCI*1)) %>%
  mutate(UCI1 = case_when(UCI  < 1.00 ~  UCI*100,
                          UCI  > 1.00 ~ UCI*1)) %>%
  mutate(mean = round(mean, digits = 0)) 


###############*********######################
############### Graph 2 #######################
###############*********#######################

# create a data for the vertical lines on graph 1;

linesdata <- data.frame(
  xvals = c(1.5, 2.5, 3.5, 4.5),
  Subgroup=NA   # required because it complains for fill, which I cannot specify again for geom_segment.
)



# Color code
colorPanel <- c("#073A5B", "#187DA2",  "#8E214A", "#42225C")
names(colorPanel) <- c("Urban", "Rural", "Female", "Male")

#colorPanel <- c("#42235e", "#7d103d", "#007da5", "#003b5d")


labels <- paste0("<span style='color:", colorPanel, ";'>", names(colorPanel), "</span>")
names(labels) <- names(colorPanel)


# bar chart

figure2 <- function(df,dfline, Country_name){
  Country_name <- rlang::parse_expr(quo_name(enquo(Country_name)))
  df %>%
    filter(Country == Country_name &
             Variable_label %in% c("At least one fruit", "At least one vegetable", "Pulses", "Nuts and seeds", "Whole grains")) %>%
    ggplot(aes(x = Variable_label, y = mean, fill = Subgroup)) +
    geom_col(position = position_dodge(c(-0.9, -1.1, -1.1, -0.9)), width = 0.81) +
    scale_fill_manual(values = colorPanel, breaks = c("Urban", "Rural"), labels = labels[c("Urban", "Rural")],
                      aesthetics = c("color", "fill"),
                      guide = guide_legend(order = 1, byrow = TRUE)) +
    new_scale_fill() +
    new_scale_color() +
    geom_col(aes(fill = Subgroup), position = position_dodge(c(-0.9, -1.1, -1.1, -0.9)), width = 0.81) +
    
    scale_fill_manual(values = colorPanel, breaks = c("Female", "Male"), labels = labels[c("Female", "Male")], 
                      aesthetics = c("color", "fill"),
                      guide = guide_legend(order = 2, byrow = TRUE)) +
    scale_y_continuous(
      breaks = c(0, 20, 40, 60, 80), expand = c(0, 0),
      limits = c(0, 100),
      labels = function(x) paste0(x, "%")
    ) +
    scale_x_discrete(limits = rev(c("At least one fruit", "At least one vegetable", "Pulses", "Nuts and seeds", "Whole grains")),
                     
                     labels = c("At least one fruit" = "Fruits ",
                                "At least one vegetable" = "Vegetables ",
                                "Whole grains" = "      Whole grains ",
                                "Pulses" = "Pulses ",
                                "Nuts and seeds" = "Nuts \nor Seeds "), expand = c(.11,.11)) +
    geom_text(aes(label = paste0(mean,"%"), y = mean - 1), color = "white", 
              size = 4.5, vjust = rep(c(0.5, 0.7, 0.5, 0.3), 5), hjust = 1, facefont = "bold", family = "Whitney semibold",   position = position_dodge(-.9), alpha = .9, show.legend = FALSE) +
    coord_flip(clip="off") + 
    geom_segment(aes(x=0.5613, xend=5.4387, y = 0, yend = 0), size = .5) +
    geom_segment(
      data= linesdata, y=-48.3, yend= 100,
      aes(x=xvals, xend=xvals), show.legend = FALSE, size = 0.15) +  
    # scale_fill_manual(values = colorPanel) + 
    labs( x= " ", 
          y = " ") + 
    ggtitle("FIGURE 2. Dietary factors protective of NCDs") + 
    #geom_text(aes(label = "FIGURE 1. Dietary adequacy", y = 0, x = 5.75), hjust = .68, family = "Whitney", facefont = "bold", color = '#007da5', size = 5.25)+
    # geom_text(aes(label = "Not applicable", y = 0, x = 4.63), hjust = -.05, family = "Whitney", facefont = "plain", color = '#42225C', size = 4.5)+
    theme(
      panel.background = element_rect(fill = "white"),
      plot.title = element_text(family = "whitney", face = "bold", color = '#187DA2', size = 15, hjust = 1.44),
      panel.border = element_blank(),
      axis.text.y = element_text(family = "whitney", color = "black", size = 15, lineheight = 1),
      axis.text.x = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      plot.margin = unit(c(0, .5, 0, 0), "cm"),
      aspect.ratio = 1.55,
      axis.ticks.y.left = element_line(colour = "green"),
      axis.ticks.length = unit(0, "cm"),
      axis.title.x = element_text(size = 8, color = "black", family = "Whitney"),
      axis.text = element_text(size = 8, color = "black", family = "Whitney"),
      legend.text = element_markdown(size = 12, family = "whitney", margin = margin(t = -2)),
      legend.key = element_rect(colour = NA, fill = NA, size = 4),
      legend.key.height = unit(0.023, "npc"),
      legend.key.width = unit(0.023, "npc"),
      legend.margin = margin(t = 1.5, r = 1, b = 1.5, l = 1),
      legend.title = element_blank(),
      # Bottom y = 0.115, Top = 0.90
      legend.position = c(0.88, 0.115),
      legend.spacing.y  = unit(3, "pt"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  
}


figure2 <- figure2(df = df1, dfline = linesdata ,  Country_name = "'Mozambique'")


#  graph 1.1 : Food group diversity score

# data preparation for graph 1.1

d <- df %>%
  rename(mean_pre = "Mean_Prevalence") %>%
  select("Country", "Subgroup", "Variable_label", "mean_pre") %>%
  mutate(stack = "mean") %>%
  filter(Country == "Mozambique" &
           Variable_label %in% c("GDR-Healthy Score (0-9)"))
  #filter(Subgroup %in% c("All"))

GDRGDR_scoreL <- df %>%
  rename(mean_pre = "Mean_Prevalence") %>%
  select("Country", "Subgroup", "Variable_label", "mean_pre") %>%
  mutate(stack = "mean") %>%
  filter(Country == "Mozambique" &
           Variable_label %in% c("GDR-Healthy Score (0-9)")) %>%
  filter(Subgroup %in% c("All"))

GDR_max <- data.frame("Country" = c("Mozambique"),
                      "Subgroup"= c("All"), 
                      "Variable_label" = c("GDR-Healthy Score (0-9)"),
                      "mean_pre" = 9 - GDRGDR_scoreL$mean_pre,
                      "stack" = c("max"))

# Food group diversity score;

GDR_healthy <- rbind(GDRGDR_scoreL, GDR_max)


colorPanel = c("#D1DFCC", "#488135")


GDRH <- ggplot(GDR_healthy, aes(x = Variable_label, y = mean_pre, fill = stack)) + 
  geom_bar(stat = "identity", position = position_stack(vjust = 0.5), width = 3.4) +
  coord_flip(clip = "off") +
  
  scale_fill_manual(values =  colorPanel) + 
  scale_x_discrete(expand = c(.1, .1)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs( x= " ",
        y = " ") +  
  annotate("text", x = 1.1, y = d$mean_pre[1] + .15, label = round(d$mean_pre[1], digits = 1),  family = "whitney semibold",  color = "#488135", fontface = "bold", size = 4.5, hjust = 0) +
  annotate("text", x = 4, y = 0, label = "Global dietary recommendations-healthy (#/9)\n", size = 5, family =  "whitney semibold", fontface = "bold", hjust = 0, lineheight = .95) +
  annotate("text", x = -2, y = 0, label = paste("\nUrban:", round(d$mean_pre[4], digits = 1), " ", "Rural:", round(d$mean_pre[5], digits = 1), " ", "Female:", round(d$mean_pre[3], digits = 1), " ", "Male:", round(d$mean_pre[2], digits = 1), sep = " "), size = 4, family =  "whitneybook", hjust = 0, lineheight = .95, fontface = "italic") + 
  
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y =  element_blank(),
        axis.text.x = element_blank(),
        
       # plot.margin = rep(unit(0,"null"),4),
        
        axis.line.x =  element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        aspect.ratio = 0.07,
        plot.margin = unit(c(0, .5, 0, 0), "cm"),
        legend.position = "none",
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.y =  element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x =  element_blank())



Brief <- plot_grid(figure2, GDRH, rel_heights = c(1, 0.1), nrow = 2, ncol = 1)

theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))


ggsave(file = "graphs/Mozambique_Figure 2.pdf", width = 12.5,  height = 15, units ='cm', device = cairo_pdf, 
       Brief)