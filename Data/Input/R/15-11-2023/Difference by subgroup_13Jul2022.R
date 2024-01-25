install.packages("hrbrthemes")

library(survey)
library(stringr)
library(plotrix)
library(dplyr)
library(showtext)
library(ggplot2)
library(hrbrthemes)
library(forcats)
library(ggtext)

d <- read.csv("Data/Output/CSV/results_internal_unmerged_NA_Revised_24Nov.csv")
glimpse(d)
# Remove white space at end of country name strings
d$Country <- str_trim(d$Country, side = "right")



########################################################################
# By sex

d <- d[order(d$Subgroup), ]
d$SexDiff <- rep(dim(as.matrix(d[d$Subgroup == "Male",]["Difference"])), 5)
d$SexDiffPerc <- rep(as.matrix(d[d$Subgroup == "Male",]["Difference"]/d[d$Subgroup == "Male",]["Mean_Prevalence"]), 5)
d <- d[order(d$SexDiffPerc, decreasing = T), ]


d %>%
  filter(Subgroup == "Male") %>%
  ggplot(aes(x = Difference)) +
  geom_histogram(binwidth = 1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 0.01") +
  xlim(-20,20)+
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )


d %>%
  ggplot(aes(x = x)) +
  geom_histogram(data = . %>% filter(Subgroup == "Urban"), aes(x = Difference, y = ..density..), fill = "#69b3a2", binwidth = 1) +
  geom_label( aes(x = 4.5, y = 0.25, label="Urban minus Rural"), color = "#69b3a2") +
  geom_histogram(data = . %>% filter(Subgroup == "Male"), aes(x = Difference, y = -..density..), fill = "#404080", binwidth = 1) +
  geom_label( aes(x = 4.5, y = -0.25, label="Male minus Female"), color = "#404080") +
  xlim(-20,20) +
  theme_ipsum() +
  xlab("Difference percentage")
  

# Font setting

if(.Platform$OS.type == "windows") {
  
  ft.url = "https://www.cufonfonts.com/download/font/whitney-2"
  dir.create(file.path(getwd(), "font"))
  download.file("https://www.cufonfonts.com/download/font/whitney-2", 
                "font/whitney-2.zip", mode = "wb")
  unzip(file.path("font/whitney-2.zip"), exdir = paste("font", basename(ft.url), sep = "/"))
  font_paths("font/whitney-2")
  font_add("Whitney", 
           regular = "whitneybook.otf", 
           bold = "whitneybold.otf",
           italic = "whitneybookitalic.otf", 
           bolditalic = "whitneysemibold.otf")
  
  showtext_auto()
  par(family = "Whitney")
  
} 


brief <- d %>%
  filter(Indicator == "Dietary diversity score" & Subgroup == "Male") %>%
  ggplot(aes(x = fct_reorder(Country, Difference/Mean_Prevalence, .desc = T), y = Difference/Mean_Prevalence,
             fill = ifelse(Diff_LCI > 0 & Diff_UCI > 0, "Men are higher", 
                           ifelse(Diff_LCI < 0 & Diff_UCI < 0, "Women are higher", "Not significant")))) +
  geom_col(width = 0.8) +
  
  geom_text(data = d %>% filter(Diff_LCI > 0 & Diff_UCI > 0),
            aes(label = paste0(round(Difference*100/Mean_Prevalence, 0),"%")), position = position_dodge(-.9)) +
  
  scale_fill_manual(name = "Legend", 
                    values = c("Men are higher" = "#5b3b71", 
                               "Women are higher" = "#9a4054", 
                               "Not significant" = "#b7c1bd")) +
  
  scale_y_continuous(limits = c(-0.2, 0.2),
                     labels = function(x) paste0(x*100, "%")) +
  coord_flip() +
  labs( x= "", 
        y = "Percentage difference<br>of Dietary Diversity Score") + 
  #ggtitle("Percentage Point Differences") +
  theme(
    panel.background = element_rect(fill = "#edf6f9"),
    plot.background = element_rect(fill = "#edf6f9"),
    panel.border = element_blank(),
    axis.title.x = element_markdown(family = "Whitney", color = 'black', size = 8),
    axis.line.x = element_line(color = "black"),
    axis.text.y = element_text(family = "Whitney", color = "black", size = 8, lineheight = 1),
    axis.ticks.y.left = element_blank(),
    aspect.ratio = 1.55,
    legend.text = element_markdown(size = 8, family = "Whitney", margin = margin(t = 1)),
    legend.key = element_rect(colour = NA, fill = NA, linewidth = 4),
    legend.background = element_rect(fill = "#edf6f9"),
    legend.key.height = unit(0.023, "npc"),
    legend.key.width = unit(0.028, "npc"),
    legend.margin = margin(t = 1.5, r = 1, b = 1.5, l = 1),
    legend.title = element_blank(),
    legend.position = c(0.75, 0.94),
    legend.spacing.y  = unit(3, "pt"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = unit(c(0, .5, 0, 0), "cm")
  )

brief
  
ggsave(file = "Data/Output/Graphs/All-5_Sex_Diff_Perc_ggplot24.pdf", 
       width = 12.5,  height = 16, units ='cm', device = cairo_pdf, 
       brief)

# Dietary diversity score (percentage difference in means among women and men)
cairo_pdf("Data/Output/Graphs/FGDS_Sex_Diff_Perc.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,3))

barplot(as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Male",]["Mean_Prevalence"]), names.arg = as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.1, .1), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Male",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Male",]["Diff_UCI"]) > 0, "#f1a340", ifelse(
    as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Male",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Male",]["Diff_UCI"]) < 0, "#998ec3", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Male",]["Difference"] >= 0, as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Male",]["Mean_Prevalence"]), as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Male",]["Mean_Prevalence"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Male",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Male",]["Diff_UCI"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Male",]["Mean_Prevalence"])), digits = 0), "%", sep = ""), ifelse(
    as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Male",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Male",]["Diff_UCI"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Male",]["Mean_Prevalence"])), digits = 0), "%", sep = ""), NA)), col = ifelse(as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Male",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Male",]["Diff_UCI"]) > 0, "#f1a340", ifelse(
        as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Male",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Male",]["Diff_UCI"]) < 0, "#998ec3", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Male",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.1, -.05, 0, .05, .1), lab = paste0(c(10, 5, 0, 5, 10) , "%"), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage difference between means", cex = 2, xpd =T)
legend(.0033, 46, inset = 0, fill = rev(c("#b7c1bd", "#f1a340", "#998ec3")),
       legend = c("Higher for women", "Higher for men", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()


# All-5 (percentage difference in means among women and men)
cairo_pdf("graphs/All-5 score_Sex_Diff_Perc.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,3))

barplot(as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "All5_score" & d$Subgroup == "Male",]["Mean_Prevalence"]), names.arg = as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.06, .06), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Male",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Male",]["Diff_UCI"]) > 0, "#f1a340", ifelse(
    as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Male",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Male",]["Diff_UCI"]) < 0, "#998ec3", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "All5_score" & d$Subgroup == "Male",]["Difference"] >= 0, as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "All5_score" & d$Subgroup == "Male",]["Mean_Prevalence"]), as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "All5_score" & d$Subgroup == "Male",]["Mean_Prevalence"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Male",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Male",]["Diff_UCI"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "All5_score" & d$Subgroup == "Male",]["Mean_Prevalence"])), digits = 0), "%", sep = ""), ifelse(
    as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Male",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Male",]["Diff_UCI"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "All5_score" & d$Subgroup == "Male",]["Mean_Prevalence"])), digits = 0), "%", sep = ""), NA)), col = ifelse(as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Male",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Male",]["Diff_UCI"]) > 0, "#f1a340", ifelse(
        as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Male",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Male",]["Diff_UCI"]) < 0, "#998ec3", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "All5_score" & d$Subgroup == "Male",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.06, -.04, -.02, 0, .02, .04, .06), lab = paste0(c(6, 4, 2, 0, 2, 4, 6) , "%"), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage difference between means", cex = 2, xpd =T)
legend(.002, 46, inset = 0, fill = rev(c("#b7c1bd", "#f1a340", "#998ec3")),
       legend = c("Higher for women", "Higher for men", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()


# GDR-H (percentage difference in means among women and men)
cairo_pdf("graphs/GDR-H score_Sex_Diff_Perc.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,3))

barplot(as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "GDR_H_score" & d$Subgroup == "Male",]["Mean_Prevalence"]), names.arg = as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.15, .15), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Male",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Male",]["Diff_UCI"]) > 0, "#f1a340", ifelse(
    as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Male",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Male",]["Diff_UCI"]) < 0, "#998ec3", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Male",]["Difference"] >= 0, as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "GDR_H_score" & d$Subgroup == "Male",]["Mean_Prevalence"]), as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "GDR_H_score" & d$Subgroup == "Male",]["Mean_Prevalence"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Male",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Male",]["Diff_UCI"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "GDR_H_score" & d$Subgroup == "Male",]["Mean_Prevalence"])), digits = 0), "%", sep = ""), ifelse(
    as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Male",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Male",]["Diff_UCI"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "GDR_H_score" & d$Subgroup == "Male",]["Mean_Prevalence"])), digits = 0), "%", sep = ""), NA)), col = ifelse(as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Male",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Male",]["Diff_UCI"]) > 0, "#f1a340", ifelse(
    as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Male",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Male",]["Diff_UCI"]) < 0, "#998ec3", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Male",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.15, -.1, -.05, 0, .05, .1, .15), lab = paste0(c(15, 10, 5, 0, 5, 10, 15) , "%"), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage difference between means", cex = 2, xpd =T)
legend(.005, 46, inset = 0, fill = rev(c("#b7c1bd", "#f1a340", "#998ec3")),
       legend = c("Higher for women", "Higher for men", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()



# GDR-L (percentage difference in means among women and men)
cairo_pdf("graphs/GDR-L score_Sex_Diff_Perc.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,3))

barplot(as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "GDR_L_score" & d$Subgroup == "Male",]["Mean_Prevalence"]), names.arg = as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.35, .35), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Male",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Male",]["Diff_UCI"]) > 0, "#f1a340", ifelse(
    as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Male",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Male",]["Diff_UCI"]) < 0, "#998ec3", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Male",]["Difference"] >= 0, as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "GDR_L_score" & d$Subgroup == "Male",]["Mean_Prevalence"]), as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "GDR_L_score" & d$Subgroup == "Male",]["Mean_Prevalence"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Male",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Male",]["Diff_UCI"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "GDR_L_score" & d$Subgroup == "Male",]["Mean_Prevalence"])), digits = 0), "%", sep = ""), ifelse(
    as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Male",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Male",]["Diff_UCI"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "GDR_L_score" & d$Subgroup == "Male",]["Mean_Prevalence"])), digits = 0), "%", sep = ""), NA)), col = ifelse(as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Male",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Male",]["Diff_UCI"]) > 0, "#f1a340", ifelse(
        as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Male",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Male",]["Diff_UCI"]) < 0, "#998ec3", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Male",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.35, -.30, -.25, -.20, -.15, -.1, -.05, 0, .05, .1, .15, .20, .25, .30, .35), lab = paste0(c(35, 30, 25, 20, 15, 10, 5, 0, 5, 10, 15, 20, 25, 30, 35) , "%"), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage difference between means", cex = 2, xpd =T)
legend(.0115, 46, inset = 0, fill = rev(c("#b7c1bd", "#f1a340", "#998ec3")),
       legend = c("Higher for women", "Higher for men", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()



# GDR (percentage difference in means among women and men)
cairo_pdf("graphs/GDR score_Sex_Diff_Perc.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,3))

barplot(as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "GDR_score" & d$Subgroup == "Male",]["Mean_Prevalence"]), names.arg = as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.15, .15), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Male",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Male",]["Diff_UCI"]) > 0, "#f1a340", ifelse(
    as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Male",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Male",]["Diff_UCI"]) < 0, "#998ec3", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "GDR_score" & d$Subgroup == "Male",]["Difference"] >= 0, as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "GDR_score" & d$Subgroup == "Male",]["Mean_Prevalence"]), as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "GDR_score" & d$Subgroup == "Male",]["Mean_Prevalence"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Male",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Male",]["Diff_UCI"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "GDR_score" & d$Subgroup == "Male",]["Mean_Prevalence"])), digits = 0), "%", sep = ""), ifelse(
    as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Male",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Male",]["Diff_UCI"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Male",]["Difference"]/d[d$Indicator == "GDR_score" & d$Subgroup == "Male",]["Mean_Prevalence"])), digits = 0), "%", sep = ""), NA)), col = ifelse(as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Male",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Male",]["Diff_UCI"]) > 0, "#f1a340", ifelse(
        as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Male",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Male",]["Diff_UCI"]) < 0, "#998ec3", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "GDR_score" & d$Subgroup == "Male",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.15, -.1, -.05, 0, .05, .1, .15), lab = paste0(c(15, 10, 5, 0, 5, 10, 15) , "%"), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage difference between means", cex = 2, xpd =T)
legend(.005, 46, inset = 0, fill = rev(c("#b7c1bd", "#f1a340", "#998ec3")),
       legend = c("Higher for women", "Higher for men", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()


d <- d[order(d$Subgroup), ]
d$SexDiff <- rep(as.matrix(d[d$Subgroup == "Male",]["Difference"]), 9)
d <- d[order(d$SexDiff, decreasing = T), ]

# All-5 (percentage difference in means among women and men)
cairo_pdf("graphs/All-5_Sex_Diff_PPT.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,3))

barplot(as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Male",]["Difference"]), names.arg = as.matrix(d[d$Indicator == "All5" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.15, .15), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Male",]["Difference"]) > 0, "#f1a340", ifelse(
    as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Male",]["Difference"]) < 0, "#998ec3", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "All5" & d$Subgroup == "Male",]["Difference"] >= 0, as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Male",]["Difference"]), as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Male",]["Difference"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Male",]["Difference"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Male",]["Difference"])), digits = 0), "ppt", sep = " "), ifelse(
    as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Male",]["Difference"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Male",]["Difference"])), digits = 0), "ppt", sep = " "), NA)), col = ifelse(as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Male",]["Difference"]) > 0, "#f1a340", ifelse(
        as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Male",]["Difference"]) < 0, "#998ec3", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "All5" & d$Subgroup == "Male",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.15, -.1, -.05, 0, .05, .1, .15), lab = paste0(c(15, 10, 5, 0, 5, 10, 15) , ""), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage point (ppt) difference in prevalence", cex = 2, xpd =T)
legend(.005, 46, inset = 0, fill = rev(c("#b7c1bd", "#f1a340", "#998ec3")),
       legend = c("Higher for women", "Higher for men", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()




# ASF (percentage difference in means among women and men)
cairo_pdf("graphs/ASF_Sex_Diff_PPT.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,3))

barplot(as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Male",]["Difference"]), names.arg = as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.1, .1), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Male",]["Difference"]) > 0, "#f1a340", ifelse(
    as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Male",]["Difference"]) < 0, "#998ec3", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "ASF" & d$Subgroup == "Male",]["Difference"] >= 0, as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Male",]["Difference"]), as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Male",]["Difference"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Male",]["Difference"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Male",]["Difference"])), digits = 0), "ppt", sep = " "), ifelse(
    as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Male",]["Difference"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Male",]["Difference"])), digits = 0), "ppt", sep = " "), NA)), col = ifelse(as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Male",]["Difference"]) > 0, "#f1a340", ifelse(
        as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Male",]["Difference"]) < 0, "#998ec3", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "ASF" & d$Subgroup == "Male",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.15, -.1, -.05, 0, .05, .1, .15), lab = paste0(c(15, 10, 5, 0, 5, 10, 15) , ""), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage point (ppt) difference in prevalence", cex = 2, xpd =T)
legend(.005, 46, inset = 0, fill = rev(c("#b7c1bd", "#f1a340", "#998ec3")),
       legend = c("Higher for women", "Higher for men", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()



# Pulses, nuts, or seeds (percentage difference in means among women and men)
cairo_pdf("graphs/PNS_Sex_Diff_PPT.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,5))

barplot(as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Male",]["Difference"]), names.arg = as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.15, .15), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Male",]["Difference"]) > 0, "#f1a340", ifelse(
    as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Male",]["Difference"]) < 0, "#998ec3", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Male",]["Difference"] >= 0, as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Male",]["Difference"]), as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Male",]["Difference"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Male",]["Difference"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Male",]["Difference"])), digits = 0), "ppt", sep = " "), ifelse(
    as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Male",]["Difference"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Male",]["Difference"])), digits = 0), "ppt", sep = " "), NA)), col = ifelse(as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Male",]["Difference"]) > 0, "#f1a340", ifelse(
        as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Male",]["Difference"]) < 0, "#998ec3", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Male",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.15, -.1, -.05, 0, .05, .1, .15), lab = paste0(c(15, 10, 5, 0, 5, 10, 15) , ""), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage point (ppt) difference in prevalence", cex = 2, xpd =T)
legend(.005, 46, inset = 0, fill = rev(c("#b7c1bd", "#f1a340", "#998ec3")),
       legend = c("Higher for women", "Higher for men", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()



# At least one fruit or vegetable (percentage difference in means among women and men)
cairo_pdf("graphs/Atleast1ForV_Sex_Diff_PPT.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,3))

barplot(as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Male",]["Difference"]), names.arg = as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.1, .1), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Male",]["Difference"]) > 0, "#f1a340", ifelse(
    as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Male",]["Difference"]) < 0, "#998ec3", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "ForV" & d$Subgroup == "Male",]["Difference"] >= 0, as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Male",]["Difference"]), as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Male",]["Difference"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Male",]["Difference"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Male",]["Difference"])), digits = 0), "ppt", sep = " "), ifelse(
    as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Male",]["Difference"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Male",]["Difference"])), digits = 0), "ppt", sep = " "), NA)), col = ifelse(as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Male",]["Difference"]) > 0, "#f1a340", ifelse(
        as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Male",]["Difference"]) < 0, "#998ec3", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "ForV" & d$Subgroup == "Male",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.15, -.1, -.05, 0, .05, .1, .15), lab = paste0(c(15, 10, 5, 0, 5, 10, 15) , ""), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage point (ppt) difference in prevalence", cex = 2, xpd =T)
legend(.005, 46, inset = 0, fill = rev(c("#b7c1bd", "#f1a340", "#998ec3")),
       legend = c("Higher for women", "Higher for men", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()



# Whole grains (percentage difference in means among women and men)
cairo_pdf("graphs/Whole grains_Sex_Diff_PPT.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,3))

barplot(as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Male",]["Difference"]), names.arg = as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.15, .15), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Male",]["Difference"]) > 0, "#f1a340", ifelse(
    as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Male",]["Difference"]) < 0, "#998ec3", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "wholegrain" & d$Subgroup == "Male",]["Difference"] >= 0, as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Male",]["Difference"]), as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Male",]["Difference"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Male",]["Difference"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Male",]["Difference"])), digits = 0), "ppt", sep = " "), ifelse(
    as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Male",]["Difference"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Male",]["Difference"])), digits = 0), "ppt", sep = " "), NA)), col = ifelse(as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Male",]["Difference"]) > 0, "#f1a340", ifelse(
        as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Male",]["Difference"]) < 0, "#998ec3", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "wholegrain" & d$Subgroup == "Male",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.15, -.1, -.05, 0, .05, .1, .15), lab = paste0(c(15, 10, 5, 0, 5, 10, 15) , ""), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage point (ppt) difference in prevalence", cex = 2, xpd =T)
legend(.005, 46, inset = 0, fill = rev(c("#b7c1bd", "#f1a340", "#998ec3")),
       legend = c("Higher for women", "Higher for men", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()


# Processed meat (percentage difference in means among women and men)
cairo_pdf("graphs/Processed meat_Sex_Diff_PPT.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,5))

barplot(as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Male",]["Difference"]), names.arg = as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.2, .2), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Male",]["Difference"]) > 0, "#f1a340", ifelse(
    as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Male",]["Difference"]) < 0, "#998ec3", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "processedmeat" & d$Subgroup == "Male",]["Difference"] >= 0, as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Male",]["Difference"]), as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Male",]["Difference"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Male",]["Difference"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Male",]["Difference"])), digits = 0), "ppt", sep = " "), ifelse(
    as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Male",]["Difference"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Male",]["Difference"])), digits = 0), "ppt", sep = " "), NA)), col = ifelse(as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Male",]["Difference"]) > 0, "#f1a340", ifelse(
        as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Male",]["Difference"]) < 0, "#998ec3", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "processedmeat" & d$Subgroup == "Male",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.20, -.15, -.1, -.05, 0, .05, .1, .15, .20), lab = paste0(c(20, 15, 10, 5, 0, 5, 10, 15, 20) , ""), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage point (ppt) difference in prevalence", cex = 2, xpd =T)
legend(.005, 46, inset = 0, fill = rev(c("#b7c1bd", "#f1a340", "#998ec3")),
       legend = c("Higher for women", "Higher for men", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()


# Salty snacks, instant noodles, or fast food (percentage difference in means among women and men)
cairo_pdf("graphs/Salty UPF_Sex_Diff_PPT.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,3))

barplot(as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Male",]["Difference"]), names.arg = as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.2, .2), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Male",]["Difference"]) > 0, "#f1a340", ifelse(
    as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Male",]["Difference"]) < 0, "#998ec3", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Male",]["Difference"] >= 0, as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Male",]["Difference"]), as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Male",]["Difference"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Male",]["Difference"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Male",]["Difference"])), digits = 0), "ppt", sep = " "), ifelse(
    as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Male",]["Difference"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Male",]["Difference"])), digits = 0), "ppt", sep = " "), NA)), col = ifelse(as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Male",]["Difference"]) > 0, "#f1a340", ifelse(
        as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Male",]["Difference"]) < 0, "#998ec3", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Male",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.20, -.15, -.1, -.05, 0, .05, .1, .15, .20), lab = paste0(c(20, 15, 10, 5, 0, 5, 10, 15, 20) , ""), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage point (ppt) difference in prevalence", cex = 2, xpd =T)
legend(.005, 46, inset = 0, fill = rev(c("#b7c1bd", "#f1a340", "#998ec3")),
       legend = c("Higher for women", "Higher for men", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()


# Deep fried foods (percentage difference in means among women and men)
cairo_pdf("graphs/Deep fried_Sex_Diff_PPT.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,8))

barplot(as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Male",]["Difference"]), names.arg = as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.15, .15), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Male",]["Difference"]) > 0, "#f1a340", ifelse(
    as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Male",]["Difference"]) < 0, "#998ec3", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "friedfood" & d$Subgroup == "Male",]["Difference"] >= 0, as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Male",]["Difference"]), as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Male",]["Difference"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Male",]["Difference"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Male",]["Difference"])), digits = 0), "ppt", sep = " "), ifelse(
    as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Male",]["Difference"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Male",]["Difference"])), digits = 0), "ppt", sep = " "), NA)), col = ifelse(as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Male",]["Difference"]) > 0, "#f1a340", ifelse(
        as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Male",]["Difference"]) < 0, "#998ec3", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "friedfood" & d$Subgroup == "Male",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.20, -.15, -.1, -.05, 0, .05, .1, .15, .20), lab = paste0(c(20, 15, 10, 5, 0, 5, 10, 15, 20) , ""), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage point (ppt) difference in prevalence", cex = 2, xpd =T)
legend(.005, 46, inset = 0, fill = rev(c("#b7c1bd", "#f1a340", "#998ec3")),
       legend = c("Higher for women", "Higher for men", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()


# Sweet foods (percentage difference in means among women and men)
cairo_pdf("graphs/Sweet foods_Sex_Diff_PPT.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,3))

barplot(as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Male",]["Difference"]), names.arg = as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.2, .2), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Male",]["Difference"]) > 0, "#f1a340", ifelse(
    as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Male",]["Difference"]) < 0, "#998ec3", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "sweetfoods" & d$Subgroup == "Male",]["Difference"] >= 0, as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Male",]["Difference"]), as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Male",]["Difference"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Male",]["Difference"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Male",]["Difference"])), digits = 0), "ppt", sep = " "), ifelse(
    as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Male",]["Difference"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Male",]["Difference"])), digits = 0), "ppt", sep = " "), NA)), col = ifelse(as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Male",]["Difference"]) > 0, "#f1a340", ifelse(
        as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Male",]["Difference"]) < 0, "#998ec3", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "sweetfoods" & d$Subgroup == "Male",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.20, -.15, -.1, -.05, 0, .05, .1, .15, .20), lab = paste0(c(20, 15, 10, 5, 0, 5, 10, 15, 20) , ""), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage point (ppt) difference in prevalence", cex = 2, xpd =T)
legend(.005, 46, inset = 0, fill = rev(c("#b7c1bd", "#f1a340", "#998ec3")),
       legend = c("Higher for women", "Higher for men", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()


# Soft drinks (percentage difference in means among women and men)
cairo_pdf("graphs/Soft drinks_Sex_Diff_PPT.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,6))

barplot(as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Male",]["Difference"]), names.arg = as.matrix(d[d$Indicator == "soda" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.2, .2), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Male",]["Difference"]) > 0, "#f1a340", ifelse(
    as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Male",]["Difference"]) < 0, "#998ec3", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "soda" & d$Subgroup == "Male",]["Difference"] >= 0, as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Male",]["Difference"]), as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Male",]["Difference"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Male",]["Difference"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Male",]["Difference"])), digits = 0), "ppt", sep = " "), ifelse(
    as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Male",]["Difference"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Male",]["Difference"])), digits = 0), "ppt", sep = " "), NA)), col = ifelse(as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Male",]["Difference"]) > 0, "#f1a340", ifelse(
        as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Male",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Male",]["Difference"]) < 0, "#998ec3", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "soda" & d$Subgroup == "Male",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.20, -.15, -.1, -.05, 0, .05, .1, .15, .20), lab = paste0(c(20, 15, 10, 5, 0, 5, 10, 15, 20) , ""), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage point (ppt) difference in prevalence", cex = 2, xpd =T)
legend(.02, 46, inset = 0, fill = rev(c("#b7c1bd", "#f1a340", "#998ec3")),
       legend = c("Higher for women", "Higher for men", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()


########################################################################
# By residence
d <- d[order(d$Subgroup), ]
d$ResDiff <- rep(as.matrix(d[d$Subgroup == "Urban",]["Difference"]), 9)
d$ResDiffPerc <- rep(as.matrix(d[d$Subgroup == "Urban",]["Difference"]/d[d$Subgroup == "Urban",]["Mean_Prevalence"]), 9)
d <- d[order(d$ResDiffPerc, decreasing = T), ]

# Dietary diversity score (percentage difference in means between urban and rural)
cairo_pdf("graphs/FGDS_Res_Diff_Perc.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,3))

barplot(as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Urban",]["Mean_Prevalence"]), names.arg = as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.15, .15), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Urban",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Urban",]["Diff_UCI"]) > 0, "#67a9cf", ifelse(
    as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Urban",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Urban",]["Diff_UCI"]) < 0, "#ef8a62", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Urban",]["Difference"] >= 0, as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Urban",]["Mean_Prevalence"]), as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Urban",]["Mean_Prevalence"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Urban",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Urban",]["Diff_UCI"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Urban",]["Mean_Prevalence"])), digits = 0), "%", sep = ""), ifelse(
    as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Urban",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Urban",]["Diff_UCI"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Urban",]["Mean_Prevalence"])), digits = 0), "%", sep = ""), NA)), col = ifelse(as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Urban",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Urban",]["Diff_UCI"]) > 0, "#67a9cf", ifelse(
        as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Urban",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Urban",]["Diff_UCI"]) < 0, "#ef8a62", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Urban",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.15, -.1, -.05, 0, .05, .1, .15), lab = paste0(c(15, 10, 5, 0, 5, 10, 15) , "%"), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage difference between means", cex = 2, xpd =T)
legend(.0033, 46, inset = 0, fill = rev(c("#b7c1bd", "#67a9cf", "#ef8a62")),
       legend = c("Rural higher", "Urban higher", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()


# All-5 (percentage difference in means between urban and rural)
cairo_pdf("graphs/All-5 score_Res_Diff_Perc.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,3))

barplot(as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "All5_score" & d$Subgroup == "Urban",]["Mean_Prevalence"]), names.arg = as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.15, .15), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Urban",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Urban",]["Diff_UCI"]) > 0, "#67a9cf", ifelse(
    as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Urban",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Urban",]["Diff_UCI"]) < 0, "#ef8a62", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "All5_score" & d$Subgroup == "Urban",]["Difference"] >= 0, as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "All5_score" & d$Subgroup == "Urban",]["Mean_Prevalence"]), as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "All5_score" & d$Subgroup == "Urban",]["Mean_Prevalence"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Urban",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Urban",]["Diff_UCI"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "All5_score" & d$Subgroup == "Urban",]["Mean_Prevalence"])), digits = 0), "%", sep = ""), ifelse(
    as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Urban",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Urban",]["Diff_UCI"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "All5_score" & d$Subgroup == "Urban",]["Mean_Prevalence"])), digits = 0), "%", sep = ""), NA)), col = ifelse(as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Urban",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Urban",]["Diff_UCI"]) > 0, "#67a9cf", ifelse(
        as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Urban",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "All5_score" & d$Subgroup == "Urban",]["Diff_UCI"]) < 0, "#ef8a62", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "All5_score" & d$Subgroup == "Urban",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.15, -.10, -.05, 0, .05, .10, .15), lab = paste0(c(15, 10, 5, 0, 5, 10, 15) , "%"), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage difference between means", cex = 2, xpd =T)
legend(.002, 46, inset = 0, fill = rev(c("#b7c1bd", "#67a9cf", "#ef8a62")),
       legend = c("Rural higher", "Urban higher", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()


# GDR-H (percentage difference in means between urban and rural)
cairo_pdf("graphs/GDR-H score_Res_Diff_Perc.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,3))

barplot(as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "GDR_H_score" & d$Subgroup == "Urban",]["Mean_Prevalence"]), names.arg = as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.25, .25), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Urban",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Urban",]["Diff_UCI"]) > 0, "#67a9cf", ifelse(
    as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Urban",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Urban",]["Diff_UCI"]) < 0, "#ef8a62", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Urban",]["Difference"] >= 0, as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "GDR_H_score" & d$Subgroup == "Urban",]["Mean_Prevalence"]), as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "GDR_H_score" & d$Subgroup == "Urban",]["Mean_Prevalence"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Urban",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Urban",]["Diff_UCI"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "GDR_H_score" & d$Subgroup == "Urban",]["Mean_Prevalence"])), digits = 0), "%", sep = ""), ifelse(
    as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Urban",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Urban",]["Diff_UCI"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "GDR_H_score" & d$Subgroup == "Urban",]["Mean_Prevalence"])), digits = 0), "%", sep = ""), NA)), col = ifelse(as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Urban",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Urban",]["Diff_UCI"]) > 0, "#67a9cf", ifelse(
        as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Urban",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Urban",]["Diff_UCI"]) < 0, "#ef8a62", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "GDR_H_score" & d$Subgroup == "Urban",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.25, 0, .25), lab = paste0(c(25, 0, 25) , "%"), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage difference between means", cex = 2, xpd =T)
legend(.005, 46, inset = 0, fill = rev(c("#b7c1bd", "#67a9cf", "#ef8a62")),
       legend = c("Rural higher", "Urban higher", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()



# GDR-L (percentage difference in means between urban and rural)
cairo_pdf("graphs/GDR-L score_Res_Diff_Perc.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,3))

barplot(as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "GDR_L_score" & d$Subgroup == "Urban",]["Mean_Prevalence"]), names.arg = as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.5, .5), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Urban",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Urban",]["Diff_UCI"]) > 0, "#67a9cf", ifelse(
    as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Urban",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Urban",]["Diff_UCI"]) < 0, "#ef8a62", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Urban",]["Difference"] >= 0, as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "GDR_L_score" & d$Subgroup == "Urban",]["Mean_Prevalence"]), as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "GDR_L_score" & d$Subgroup == "Urban",]["Mean_Prevalence"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Urban",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Urban",]["Diff_UCI"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "GDR_L_score" & d$Subgroup == "Urban",]["Mean_Prevalence"])), digits = 0), "%", sep = ""), ifelse(
    as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Urban",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Urban",]["Diff_UCI"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "GDR_L_score" & d$Subgroup == "Urban",]["Mean_Prevalence"])), digits = 0), "%", sep = ""), NA)), col = ifelse(as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Urban",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Urban",]["Diff_UCI"]) > 0, "#67a9cf", ifelse(
        as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Urban",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Urban",]["Diff_UCI"]) < 0, "#ef8a62", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "GDR_L_score" & d$Subgroup == "Urban",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.5, -.25, 0, .25, .5), lab = paste0(c(50, 25, 0, 25, 50) , "%"), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage difference between means", cex = 2, xpd =T)
legend(.035, 46, inset = 0, fill = rev(c("#b7c1bd", "#67a9cf", "#ef8a62")),
       legend = c("Rural higher", "Urban higher", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()



# GDR (percentage difference in means between urban and rural)
cairo_pdf("graphs/GDR score_Res_Diff_Perc.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,3))

barplot(as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "GDR_score" & d$Subgroup == "Urban",]["Mean_Prevalence"]), names.arg = as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.1, .1), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Urban",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Urban",]["Diff_UCI"]) > 0, "#67a9cf", ifelse(
    as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Urban",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Urban",]["Diff_UCI"]) < 0, "#ef8a62", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "GDR_score" & d$Subgroup == "Urban",]["Difference"] >= 0, as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "GDR_score" & d$Subgroup == "Urban",]["Mean_Prevalence"]), as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "GDR_score" & d$Subgroup == "Urban",]["Mean_Prevalence"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Urban",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Urban",]["Diff_UCI"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "GDR_score" & d$Subgroup == "Urban",]["Mean_Prevalence"])), digits = 0), "%", sep = ""), ifelse(
    as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Urban",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Urban",]["Diff_UCI"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Urban",]["Difference"]/d[d$Indicator == "GDR_score" & d$Subgroup == "Urban",]["Mean_Prevalence"])), digits = 0), "%", sep = ""), NA)), col = ifelse(as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Urban",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Urban",]["Diff_UCI"]) > 0, "#67a9cf", ifelse(
        as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Urban",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "GDR_score" & d$Subgroup == "Urban",]["Diff_UCI"]) < 0, "#ef8a62", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "GDR_score" & d$Subgroup == "Urban",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.15, -.1, -.05, 0, .05, .1, .15), lab = paste0(c(15, 10, 5, 0, 5, 10, 15) , "%"), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage difference between means", cex = 2, xpd =T)
legend(.005, 46, inset = 0, fill = rev(c("#b7c1bd", "#67a9cf", "#ef8a62")),
       legend = c("Rural higher", "Urban higher", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()


d <- d[order(d$Subgroup), ]
d$ResDiff <- rep(as.matrix(d[d$Subgroup == "Urban",]["Difference"]), 9)
d <- d[order(d$ResDiff, decreasing = T), ]

# All-5 (percentage difference in means between urban and rural)
cairo_pdf("graphs/All-5_Res_Diff_PPT.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,3))

barplot(as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Urban",]["Difference"]), names.arg = as.matrix(d[d$Indicator == "All5" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.2, .2), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Urban",]["Difference"]) > 0, "#67a9cf", ifelse(
    as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Urban",]["Difference"]) < 0, "#ef8a62", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "All5" & d$Subgroup == "Urban",]["Difference"] >= 0, as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Urban",]["Difference"]), as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Urban",]["Difference"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Urban",]["Difference"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Urban",]["Difference"])), digits = 0), "ppt", sep = " "), ifelse(
    as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Urban",]["Difference"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Urban",]["Difference"])), digits = 0), "ppt", sep = " "), NA)), col = ifelse(as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Urban",]["Difference"]) > 0, "#67a9cf", ifelse(
        as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "All5" & d$Subgroup == "Urban",]["Difference"]) < 0, "#ef8a62", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "All5" & d$Subgroup == "Urban",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.20, -.15, -.1, -.05, 0, .05, .1, .15, .20), lab = paste0(c(20, 15, 10, 5, 0, 5, 10, 15, 20) , ""), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage point (ppt) difference in prevalence", cex = 2, xpd =T)
legend(.005, 46, inset = 0, fill = rev(c("#b7c1bd", "#67a9cf", "#ef8a62")),
       legend = c("Rural higher", "Urban higher", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()


# MDD-W (percentage difference in means between urban and rural)
d <- read.csv("data/Country level indicators.csv")
# Remove white space at end of country name strings
d$Country <- str_trim(d$Country, side = "right")
d <- d[d$Country != "United States" & d$Country != "Greece" & d$Country != "Israel" & d$Country != "Chile", ]
d <- d[order(d$Subgroup), ]
d$ResDiff <- rep(as.matrix(d[d$Subgroup == "Urban",]["Difference"]), 9)
d$ResDiffPerc <- rep(as.matrix(d[d$Subgroup == "Urban",]["Difference"]/d[d$Subgroup == "Urban",]["Mean_Prevalence"]), 9)
d <- d[order(d$ResDiff, decreasing = T), ]

cairo_pdf("graphs/MDD-W_Res_Diff_PPT.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,6))

barplot(as.matrix(d[d$Indicator == "MDD_W" & d$Subgroup == "Urban",]["Difference"]), names.arg = as.matrix(d[d$Indicator == "MDD_W" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.2, .2), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "MDD_W" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "MDD_W" & d$Subgroup == "Urban",]["Difference"]) > 0, "#67a9cf", ifelse(
    as.matrix(d[d$Indicator == "MDD_W" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "MDD_W" & d$Subgroup == "Urban",]["Difference"]) < 0, "#ef8a62", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "MDD_W" & d$Subgroup == "Urban",]["Difference"] >= 0, as.matrix(d[d$Indicator == "MDD_W" & d$Subgroup == "Urban",]["Difference"]), as.matrix(d[d$Indicator == "MDD_W" & d$Subgroup == "Urban",]["Difference"])), seq(.6, 41, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "MDD_W" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "MDD_W" & d$Subgroup == "Urban",]["Difference"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "MDD_W" & d$Subgroup == "Urban",]["Difference"])), digits = 0), "ppt", sep = " "), ifelse(
    as.matrix(d[d$Indicator == "MDD_W" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "MDD_W" & d$Subgroup == "Urban",]["Difference"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "MDD_W" & d$Subgroup == "Urban",]["Difference"])), digits = 0), "ppt", sep = " "), NA)), col = ifelse(as.matrix(d[d$Indicator == "MDD_W" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "MDD_W" & d$Subgroup == "Urban",]["Difference"]) > 0, "#67a9cf", ifelse(
        as.matrix(d[d$Indicator == "MDD_W" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "MDD_W" & d$Subgroup == "Urban",]["Difference"]) < 0, "#ef8a62", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "MDD_W" & d$Subgroup == "Urban",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 41)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.20, -.15, -.1, -.05, 0, .05, .1, .15, .20), lab = paste0(c(20, 15, 10, 5, 0, 5, 10, 15, 20) , ""), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage point (ppt) difference in prevalence", cex = 2, xpd =T)
legend(.01, 41, inset = 0, fill = rev(c("#b7c1bd", "#67a9cf", "#ef8a62")),
       legend = c("Rural higher", "Urban higher", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()


d <- read.csv("data/Country level indicators.csv")
# Remove white space at end of country name strings
d$Country <- str_trim(d$Country, side = "right")
d <- d[order(d$Subgroup), ]
d$ResDiff <- rep(as.matrix(d[d$Subgroup == "Urban",]["Difference"]), 9)
d$ResDiffPerc <- rep(as.matrix(d[d$Subgroup == "Urban",]["Difference"]/d[d$Subgroup == "Urban",]["Mean_Prevalence"]), 9)
d <- d[order(d$ResDiff, decreasing = T), ]

# ASF (percentage difference in means between urban and rural)
cairo_pdf("graphs/ASF_Res_Diff_PPT.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,6))

barplot(as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Urban",]["Difference"]), names.arg = as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.2, .2), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Urban",]["Difference"]) > 0, "#67a9cf", ifelse(
    as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Urban",]["Difference"]) < 0, "#ef8a62", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "ASF" & d$Subgroup == "Urban",]["Difference"] >= 0, as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Urban",]["Difference"]), as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Urban",]["Difference"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Urban",]["Difference"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Urban",]["Difference"])), digits = 0), "ppt", sep = " "), ifelse(
    as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Urban",]["Difference"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Urban",]["Difference"])), digits = 0), "ppt", sep = " "), NA)), col = ifelse(as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Urban",]["Difference"]) > 0, "#67a9cf", ifelse(
        as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "ASF" & d$Subgroup == "Urban",]["Difference"]) < 0, "#ef8a62", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "ASF" & d$Subgroup == "Urban",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.20, -.15, -.1, -.05, 0, .05, .1, .15, .20), lab = paste0(c(20, 15, 10, 5, 0, 5, 10, 15, 20) , ""), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage point (ppt) difference in prevalence", cex = 2, xpd =T)
legend(.01, 46, inset = 0, fill = rev(c("#b7c1bd", "#67a9cf", "#ef8a62")),
       legend = c("Rural higher", "Urban higher", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()



# Pulses, nuts, or seeds (percentage difference in means between urban and rural)
cairo_pdf("graphs/PNS_Res_Diff_PPT.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,3))

barplot(as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Urban",]["Difference"]), names.arg = as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.2, .2), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Urban",]["Difference"]) > 0, "#67a9cf", ifelse(
    as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Urban",]["Difference"]) < 0, "#ef8a62", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Urban",]["Difference"] >= 0, as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Urban",]["Difference"]), as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Urban",]["Difference"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Urban",]["Difference"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Urban",]["Difference"])), digits = 0), "ppt", sep = " "), ifelse(
    as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Urban",]["Difference"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Urban",]["Difference"])), digits = 0), "ppt", sep = " "), NA)), col = ifelse(as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Urban",]["Difference"]) > 0, "#67a9cf", ifelse(
        as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Urban",]["Difference"]) < 0, "#ef8a62", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "PulseNutSeed" & d$Subgroup == "Urban",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.20, -.15, -.1, -.05, 0, .05, .1, .15, .20), lab = paste0(c(20, 15, 10, 5, 0, 5, 10, 15, 20) , ""), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage point (ppt) difference in prevalence", cex = 2, xpd =T)
legend(.005, 46, inset = 0, fill = rev(c("#b7c1bd", "#67a9cf", "#ef8a62")),
       legend = c("Rural higher", "Urban higher", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()



# At least one fruit or vegetable (percentage difference in means between urban and rural)
cairo_pdf("graphs/Atleast1ForV_Res_Diff_PPT.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,4))

barplot(as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Urban",]["Difference"]), names.arg = as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.1, .1), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Urban",]["Difference"]) > 0, "#67a9cf", ifelse(
    as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Urban",]["Difference"]) < 0, "#ef8a62", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "ForV" & d$Subgroup == "Urban",]["Difference"] >= 0, as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Urban",]["Difference"]), as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Urban",]["Difference"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Urban",]["Difference"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Urban",]["Difference"])), digits = 0), "ppt", sep = " "), ifelse(
    as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Urban",]["Difference"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Urban",]["Difference"])), digits = 0), "ppt", sep = " "), NA)), col = ifelse(as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Urban",]["Difference"]) > 0, "#67a9cf", ifelse(
        as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "ForV" & d$Subgroup == "Urban",]["Difference"]) < 0, "#ef8a62", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "ForV" & d$Subgroup == "Urban",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.15, -.1, -.05, 0, .05, .1, .15), lab = paste0(c(15, 10, 5, 0, 5, 10, 15) , ""), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage point (ppt) difference in prevalence", cex = 2, xpd =T)
legend(.005, 46, inset = 0, fill = rev(c("#b7c1bd", "#67a9cf", "#ef8a62")),
       legend = c("Rural higher", "Urban higher", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()



# Whole grains (percentage difference in means between urban and rural)
cairo_pdf("graphs/Whole grains_Res_Diff_PPT.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,3))

barplot(as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Urban",]["Difference"]), names.arg = as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.35, .35), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Urban",]["Difference"]) > 0, "#67a9cf", ifelse(
    as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Urban",]["Difference"]) < 0, "#ef8a62", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "wholegrain" & d$Subgroup == "Urban",]["Difference"] >= 0, as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Urban",]["Difference"]), as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Urban",]["Difference"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Urban",]["Difference"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Urban",]["Difference"])), digits = 0), "ppt", sep = " "), ifelse(
    as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Urban",]["Difference"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Urban",]["Difference"])), digits = 0), "ppt", sep = " "), NA)), col = ifelse(as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Urban",]["Difference"]) > 0, "#67a9cf", ifelse(
        as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "wholegrain" & d$Subgroup == "Urban",]["Difference"]) < 0, "#ef8a62", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "wholegrain" & d$Subgroup == "Urban",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.35, 0, .35), lab = paste0(c(35, 0, 35) , ""), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage point (ppt) difference in prevalence", cex = 2, xpd =T)
legend(.005, 46, inset = 0, fill = rev(c("#b7c1bd", "#67a9cf", "#ef8a62")),
       legend = c("Rural higher", "Urban higher", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()


# Processed meat (percentage difference in means between urban and rural)
cairo_pdf("graphs/Processed meat_Res_Diff_PPT.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,3))

barplot(as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Urban",]["Difference"]), names.arg = as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.2, .2), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Urban",]["Difference"]) > 0, "#67a9cf", ifelse(
    as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Urban",]["Difference"]) < 0, "#ef8a62", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "processedmeat" & d$Subgroup == "Urban",]["Difference"] >= 0, as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Urban",]["Difference"]), as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Urban",]["Difference"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Urban",]["Difference"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Urban",]["Difference"])), digits = 0), "ppt", sep = " "), ifelse(
    as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Urban",]["Difference"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Urban",]["Difference"])), digits = 0), "ppt", sep = " "), NA)), col = ifelse(as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Urban",]["Difference"]) > 0, "#67a9cf", ifelse(
        as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "processedmeat" & d$Subgroup == "Urban",]["Difference"]) < 0, "#ef8a62", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "processedmeat" & d$Subgroup == "Urban",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.20, -.15, -.1, -.05, 0, .05, .1, .15, .20), lab = paste0(c(20, 15, 10, 5, 0, 5, 10, 15, 20) , ""), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage point (ppt) difference in prevalence", cex = 2, xpd =T)
legend(.01, 46, inset = 0, fill = rev(c("#b7c1bd", "#67a9cf", "#ef8a62")),
       legend = c("Rural higher", "Urban higher", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()


# Salty snacks, instant noodles, or fast food (percentage difference in means between urban and rural)
cairo_pdf("graphs/Salty UPF_Res_Diff_PPT.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,6))

barplot(as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Urban",]["Difference"]), names.arg = as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.2, .2), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Urban",]["Difference"]) > 0, "#67a9cf", ifelse(
    as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Urban",]["Difference"]) < 0, "#ef8a62", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Urban",]["Difference"] >= 0, as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Urban",]["Difference"]), as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Urban",]["Difference"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Urban",]["Difference"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Urban",]["Difference"])), digits = 0), "ppt", sep = " "), ifelse(
    as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Urban",]["Difference"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Urban",]["Difference"])), digits = 0), "ppt", sep = " "), NA)), col = ifelse(as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Urban",]["Difference"]) > 0, "#67a9cf", ifelse(
        as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Urban",]["Difference"]) < 0, "#ef8a62", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "Salty_UPF" & d$Subgroup == "Urban",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.20, -.15, -.1, -.05, 0, .05, .1, .15, .20), lab = paste0(c(20, 15, 10, 5, 0, 5, 10, 15, 20) , ""), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage point (ppt) difference in prevalence", cex = 2, xpd =T)
legend(.01, 46, inset = 0, fill = rev(c("#b7c1bd", "#67a9cf", "#ef8a62")),
       legend = c("Rural higher", "Urban higher", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()


# Deep fried foods (percentage difference in means between urban and rural)
cairo_pdf("graphs/Deep fried_Res_Diff_PPT.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,5))

barplot(as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Urban",]["Difference"]), names.arg = as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.2, .2), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Urban",]["Difference"]) > 0, "#67a9cf", ifelse(
    as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Urban",]["Difference"]) < 0, "#ef8a62", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "friedfood" & d$Subgroup == "Urban",]["Difference"] >= 0, as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Urban",]["Difference"]), as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Urban",]["Difference"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Urban",]["Difference"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Urban",]["Difference"])), digits = 0), "ppt", sep = " "), ifelse(
    as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Urban",]["Difference"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Urban",]["Difference"])), digits = 0), "ppt", sep = " "), NA)), col = ifelse(as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Urban",]["Difference"]) > 0, "#67a9cf", ifelse(
        as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "friedfood" & d$Subgroup == "Urban",]["Difference"]) < 0, "#ef8a62", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "friedfood" & d$Subgroup == "Urban",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.20, -.15, -.1, -.05, 0, .05, .1, .15, .20), lab = paste0(c(20, 15, 10, 5, 0, 5, 10, 15, 20) , ""), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage point (ppt) difference in prevalence", cex = 2, xpd =T)
legend(.01, 46, inset = 0, fill = rev(c("#b7c1bd", "#67a9cf", "#ef8a62")),
       legend = c("Rural higher", "Urban higher", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()


# Sweet foods (percentage difference in means between urban and rural)
cairo_pdf("graphs/Sweet foods_Res_Diff_PPT.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,7))

barplot(as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Urban",]["Difference"]), names.arg = as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.15, .15), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Urban",]["Difference"]) > 0, "#67a9cf", ifelse(
    as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Urban",]["Difference"]) < 0, "#ef8a62", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "sweetfoods" & d$Subgroup == "Urban",]["Difference"] >= 0, as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Urban",]["Difference"]), as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Urban",]["Difference"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Urban",]["Difference"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Urban",]["Difference"])), digits = 0), "ppt", sep = " "), ifelse(
    as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Urban",]["Difference"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Urban",]["Difference"])), digits = 0), "ppt", sep = " "), NA)), col = ifelse(as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Urban",]["Difference"]) > 0, "#67a9cf", ifelse(
        as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "sweetfoods" & d$Subgroup == "Urban",]["Difference"]) < 0, "#ef8a62", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "sweetfoods" & d$Subgroup == "Urban",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.20, -.15, -.1, -.05, 0, .05, .1, .15, .20), lab = paste0(c(20, 15, 10, 5, 0, 5, 10, 15, 20) , ""), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage point (ppt) difference in prevalence", cex = 2, xpd =T)
legend(.01, 46, inset = 0, fill = rev(c("#b7c1bd", "#67a9cf", "#ef8a62")),
       legend = c("Rural higher", "Urban higher", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()


# Soft drinks (percentage difference in means between urban and rural)
cairo_pdf("graphs/Soft drinks_Res_Diff_PPT.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,8))

barplot(as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Urban",]["Difference"]), names.arg = as.matrix(d[d$Indicator == "soda" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.2, .2), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Urban",]["Difference"]) > 0, "#67a9cf", ifelse(
    as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Urban",]["Difference"]) < 0, "#ef8a62", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "soda" & d$Subgroup == "Urban",]["Difference"] >= 0, as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Urban",]["Difference"]), as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Urban",]["Difference"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Urban",]["Difference"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Urban",]["Difference"])), digits = 0), "ppt", sep = " "), ifelse(
    as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Urban",]["Difference"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Urban",]["Difference"])), digits = 0), "ppt", sep = " "), NA)), col = ifelse(as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Urban",]["Difference"]) > 0, "#67a9cf", ifelse(
        as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Urban",]["Diff_p"]) < 0.05 & as.matrix(d[d$Indicator == "soda" & d$Subgroup == "Urban",]["Difference"]) < 0, "#ef8a62", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "soda" & d$Subgroup == "Urban",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.20, -.15, -.1, -.05, 0, .05, .1, .15, .20), lab = paste0(c(20, 15, 10, 5, 0, 5, 10, 15, 20) , ""), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage point (ppt) difference in prevalence", cex = 2, xpd =T)
legend(.02, 46, inset = 0, fill = rev(c("#b7c1bd", "#67a9cf", "#ef8a62")),
       legend = c("Rural higher", "Urban higher", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()


########################################################################
# Food security

d <- d[order(d$Subgroup), ]
d$FSDiff <- rep(as.matrix(d[d$Subgroup == "Food secure",]["Difference"]), 9)
d$FSDiffPerc <- rep(as.matrix(d[d$Subgroup == "Food secure",]["Difference"]/d[d$Subgroup == "Food secure",]["Mean_Prevalence"]), 9)
d <- d[order(d$FSDiffPerc, decreasing = T), ]

# Dietary diversity score (percentage difference in means among women and men)
cairo_pdf("graphs/FGDS_FS_Diff_Perc.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,5))

barplot(as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Food secure",]["Difference"]/d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Food secure",]["Mean_Prevalence"]), names.arg = as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.25, .25), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Food secure",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Food secure",]["Diff_UCI"]) > 0, "#f1a340", ifelse(
    as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Food secure",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Food secure",]["Diff_UCI"]) < 0, "#998ec3", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Food secure",]["Difference"] >= 0, as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Food secure",]["Difference"]/d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Food secure",]["Mean_Prevalence"]), as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Food secure",]["Difference"]/d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Food secure",]["Mean_Prevalence"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Food secure",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Food secure",]["Diff_UCI"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Food secure",]["Difference"]/d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Food secure",]["Mean_Prevalence"])), digits = 0), "%", sep = ""), ifelse(
    as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Food secure",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Food secure",]["Diff_UCI"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Food secure",]["Difference"]/d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Food secure",]["Mean_Prevalence"])), digits = 0), "%", sep = ""), NA)), col = ifelse(as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Food secure",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Food secure",]["Diff_UCI"]) > 0, "#f1a340", ifelse(
        as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Food secure",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Food secure",]["Diff_UCI"]) < 0, "#998ec3", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Food secure",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.25, 0, .25), lab = paste0(c(25, 0, 25) , "%"), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage difference between means", cex = 2, xpd =T)
legend(.0033, 46, inset = 0, fill = rev(c("#b7c1bd", "#f1a340", "#998ec3")),
       legend = c("Higher for food insecure", "Higher for food secure", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()




########################################################################
# Wellbeing

d <- d[order(d$Subgroup), ]
d$FSDiff <- rep(as.matrix(d[d$Subgroup == "Good life",]["Difference"]), 9)
d$FSDiffPerc <- rep(as.matrix(d[d$Subgroup == "Good life",]["Difference"]/d[d$Subgroup == "Good life",]["Mean_Prevalence"]), 9)
d <- d[order(d$FSDiffPerc, decreasing = T), ]

# Dietary diversity score (percentage difference in means among women and men)
cairo_pdf("graphs/FGDS_GL_Diff_Perc.pdf", width = 12, height = 16)
par(mar = c(5.5,13,0,5))

barplot(as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Good life",]["Difference"]/d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Good life",]["Mean_Prevalence"]), names.arg = as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "All",]["Country"]), horiz = T, beside = T, las = 2, axes = F, xlim = c(-.2, .2), cex.names = 2, col = ifelse(as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Good life",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Good life",]["Diff_UCI"]) > 0, "#f1a340", ifelse(
    as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Good life",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Good life",]["Diff_UCI"]) < 0, "#998ec3", "#b7c1bd")), xlab = "", cex.lab = 2, cex.axis = 3, space = .1, border = NA)

text(ifelse(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Good life",]["Difference"] >= 0, as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Good life",]["Difference"]/d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Good life",]["Mean_Prevalence"]), as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Good life",]["Difference"]/d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Good life",]["Mean_Prevalence"])), seq(.6, 44.6, 1.1) - .1,  ifelse(as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Good life",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Good life",]["Diff_UCI"]) > 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Good life",]["Difference"]/d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Good life",]["Mean_Prevalence"])), digits = 0), "%", sep = ""), ifelse(
    as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Good life",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Good life",]["Diff_UCI"]) < 0, paste(round(100 * abs(as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Good life",]["Difference"]/d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Good life",]["Mean_Prevalence"])), digits = 0), "%", sep = ""), NA)), col = ifelse(as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Good life",]["Diff_LCI"]) > 0 & as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Good life",]["Diff_UCI"]) > 0, "#f1a340", ifelse(
        as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Good life",]["Diff_LCI"]) < 0 & as.matrix(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Good life",]["Diff_UCI"]) < 0, "#998ec3", "#b7c1bd")), cex = 2, pos = ifelse(d[d$Indicator == "Dietary diversity score" & d$Subgroup == "Good life",]["Difference"] >= 0, 4, 2), xpd = T, font = 2)

clip(-1, 1, 0, 45.1)
abline(v = 0, lty = 1, lwd = 3)
axis(1, pos = .1, at = c(-.25, 0, .25), lab = paste0(c(20, 0, 20) , "%"), cex.axis = 2, padj = 1, lwd = 3)
text(0, -3.5, "Percentage difference between means", cex = 2, xpd =T)
legend(-.21, 46, inset = 0, fill = rev(c("#b7c1bd", "#f1a340", "#998ec3")),
       legend = c("Higher for\nlower wellbeing", "Higher for\nbetter wellbeing", "Insignificantly different"),
       box.col = "transparent", ncol = 1, border = NA, cex = 2, y.intersp = 1.25, xpd = T)
dev.off()