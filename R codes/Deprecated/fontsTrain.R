# 1----
font_paths()    
font_files()    
font_add("Palatino", "pala.ttf")
font_families()
showtext_auto() 
windows()

myFont1 <- "Montserrat"
myFont2 <- "Roboto"
myFont3 <- "Palatino"

library(ggplot2)

a <- ggplot(mtcars, aes(x = wt, y = mpg)) + 
  geom_point() +
  ggtitle("Fuel Efficiency of 32 Cars") +
  xlab("Weight (x1000 lb)") + ylab("Miles per Gallon") +
  theme(text = element_text(size = 16, family = myFont1)) +
  annotate("text", 4, 30, label = 'Palatino Linotype',
           family = myFont3, size = 10) +
  annotate("text", 1, 11, label = 'Roboto', hjust = 0,
           family = myFont2, size = 10) 

## On-screen device
print(a)
# 2----
library(showtext)
## Loading Google fonts (https://fonts.google.com/)
font_add_google("Gochi Hand", "gochi")
font_add_google("Schoolbell", "bell")
font_add_google("Covered By Your Grace", "grace")
font_add_google("Rock Salt", "rock")
font_add_google("Merriweather", "merri")

## Automatically use showtext to render text for future devices
showtext_auto()

## Tell showtext the resolution of the device,
## only needed for bitmap graphics. Default is 96
showtext_opts(dpi = 96)

set.seed(123)
x = rnorm(10)
y = 1 + x + rnorm(10, sd = 0.2)
y[1] = 5
mod = lm(y ~ x)

op = par(cex.lab = 2, cex.axis = 1.5, cex.main = 2)
plot(x, y, pch = 16, col = "steelblue",
     xlab = "X variable", ylab = "Y variable", family = "merri")
grid()
title("Draw Plots Before You Fit A Regression", family = "bell")
text(-0.5, 4.5, "This is the outlier", cex = 2, col = "steelblue",
     family = "grace")
abline(coef(mod))
abline(1, 1, col = "red")
par(family = "rock")
text(1, 1, expression(paste("True model: ", y == x + 1)),
     cex = 1.5, col = "red", srt = 20)
text(0, 2, expression(paste("OLS: ", hat(y) == 0.79 * x + 1.49)),
     cex = 1.5, srt = 15)
legend("topright", legend = c("Truth", "OLS"), col = c("red", "black"), lty = 1)

par(op)



# 3----
library(showtext)
## Add fonts that are available on Windows
font_paths()
head(font_files())
font_files()$family
font_add("heiti", "simhei.ttf")
font_add("constan", "constan.ttf", italic = "constani.ttf")

library(ggplot2)
p = ggplot(NULL, aes(x = 1, y = 1)) + ylim(0.8, 1.2) +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank()) +
  annotate("text", 1, 1.1, family = "heiti", size = 15,
           label = "\u4F60\u597D\uFF0C\u4E16\u754C") +
  annotate("text", 1, 0.9, label = 'Chinese for "Hello, world!"',
           family = "constan", fontface = "italic", size = 12)

## Automatically use showtext for new devices
showtext_auto()

## On-screen device
x11()
print(p)
dev.off()

## PDF device
pdf("showtext-example-3.pdf", 7, 4)
print(p)
dev.off()

## PNG device
ggsave("showtext-example-4.png", width = 7, height = 4, dpi = 96)

## Turn off if no longer needed
showtext_auto(FALSE)
