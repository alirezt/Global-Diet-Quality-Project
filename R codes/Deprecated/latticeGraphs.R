## 5. Visualization ----
library(lattice)

# Univariate distribution and multipanel conditioning 
histogram(~ FGDS | factor(Country), data = d)
histogram(FGDS ~ Weight | factor(Country), data = d)


# Kernel density plots
densityplot(FGDS ~ Weight | factor(Country), data = d, plot.points = FALSE, ref = TRUE)
densityplot(~ Weight | factor(FGDS), data = d, plot.points = FALSE, ref = TRUE)



# Kernel with superposition
densityplot(~ FGDS, data = d, groups = Country, plot.points = FALSE, ref = TRUE, auto.key = list(column = 5))
densityplot(~ Weight, data = d, groups = FGDS, plot.points = FALSE, ref = TRUE, auto.key = list(column = 5))


# Normal Q-Q plots
qqmath(~ FGDS | factor(Country), data = d, f.value = ppoints(100))
qqmath(~ FGDS | Gender, data = d, groups = Country, aspect = "xy", f.value = ppoints(100), auto.key = list(space = 'right'),
       xlab = "Standard Normal Quantiles",
       ylab = "FGDS Score")

# Two-sample Q-Q plots for gender
qq(Gender ~ FGDS | factor(Country), data = d, f.value = ppoints(10000), aspect = 1)

# Tukey mean-difference plot
#first creating a trellis object
QQ = qq(Gender ~ FGDS | factor(Country), data = d, f.value = ppoints(10000), strip = strip.custom(style = 5))
#then performing tmd function on trellis object
tmd(QQ)

# strip plot or univariate scatter plot
stripplot(FGDS ~ Age | factor(Country), data = d, jitter.data = TRUE, alpha = 0.6,
          xlab = "Age",
          ylab = "FGDS Score")

stripplot(FGDS ~ Age | factor(Education), data = d, jitter.data = TRUE, alpha = 0.6,
          xlab = "Age",
          ylab = "FGDS Score")

stripplot(FGDS ~ Age | factor(IncomeQuintiles), data = d, jitter.data = TRUE, alpha = 0.6,
          xlab = "Age",
          ylab = "FGDS Score")

stripplot(FGDS ~ Weight | factor(Urbanicity), data = d, jitter.data = TRUE, alpha = 0.6,
          xlab = "Weight",
          ylab = "FGDS Score")

stripplot(FGDS ~ Weight | factor(Country), data = d, jitter.data = TRUE, alpha = 0.6,
          xlab = "Weight",
          ylab = "FGDS Score")

#not so good
stripplot(sqrt(abs(residuals(lm(FGDS ~ Gender+Age+Education+Urbanicity)))) ~ Age,
          data = d, groups = Gender, jitter.data = TRUE, 
          auto.key = list(points = TRUE, Lines = TRUE, columns = 2),
          type = c("p", "a"), fun = median)

#box-and-whisker plot
bwplot(factor(FGDS) ~ log(Weight), data = d, xlab = "Log Weight", ylab = "FGDS Score")
bwplot(factor(FGDS) ~ Weight, data = d, xlab = "Weight", ylab = "FGDS Score")
bwplot(factor(FGDS) ~ Weight, panel = panel.violin, box.ratio = 3, data = d)


bwplot(factor(Country) ~ FGDS | Gender, data = d, xlab = "FGDS Score", strip = strip.custom(strip.names=TRUE, strip.levels=TRUE))
bwplot(factor(Urbanicity) ~ FGDS | Gender, data = d, xlab = "FGDS Score", ylab = "Urbanicity", strip = strip.custom(strip.names=TRUE, strip.levels=TRUE))
bwplot(factor(Urbanicity) ~ FGDS | Gender, data = d, xlab = "FGDS Score", ylab = "Urbanicity", strip = strip.custom(strip.names=TRUE, strip.levels=TRUE), panel = panel.violin, box.ratio = 3)
bwplot(factor(Education) ~ FGDS | Gender, data = d)
bwplot(factor(Gender) ~ FGDS | Urbanicity, data = d)
bwplot(factor(Gender) ~ FGDS | IncomeQuintiles, data = d)
bwplot(factor(FGDS) ~ Weight | IncomeQuintiles, data = d)
bwplot(factor(Weight) ~ FGDS | Gender, data = d)

bwplot(Gender ~ Weight | FGDS, data = d, varwidth = TRUE, layout = c(8, 1), ylab = "Gender")

#dot plot
#not useful 
dotplot(d$MDD_W ~ d$Weight | d$Country, data = d)

dotplot(FGDS ~ Weight | Education,
        layout = c(1,5), aspect = 0.7, origin = 0, type = c("P", "h"), data = d)

dotplot(FGDS ~ Weight | Education,
        type = "o", auto.key = list(lines = TRUE, space = "right"), data = d)

dotplot(FGDS ~ Weight, type = "o", auto.key = list(lines = TRUE, space = "right"), data = d)
