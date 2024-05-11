mahdf1 <- dgroup_pivot %>%
  filter(Subgroup == "All") %>%
  select(c(Country, Subgroup, Indicators, `Mean/Prevalence`)) %>%
  pivot_wider(names_from = Indicators, values_from = `Mean/Prevalence`) %>%
  select(!c(Subgroup, fgds, ncdp, ncdr, gdr)) %>%
  drop_na()

glimpse(mahdf1)

x <- data.frame(mahdf1 %>% select(!Country))
glimpse(x)

#measuring distance
cm <- colMeans(x, na.rm = TRUE)
S <- var(x, na.rm = TRUE)
mahalanobis(x = x, center = cm, cov = S)

# plotting generalized distance
layout(matrix(1:1, ncol=1))
par(mar = c(5, 6, 4, 2))

plot(qc <- qchisq((1:nrow(x) - 1/2) / nrow(x), df = 44), sd <- sort(d),
     xlab = "Chi-square Quantile",
     ylab = "Ordered distances", xpd = TRUE, bg = "gray", pch = 19)

oups <- which(rank(sd) > nrow(x) - 29)
text(qc[oups], sd[oups]-0.75, names(oups),pos = 1, offset = 1,
     col = "red", cex = 0.7, srt = "90") # for outliers

# Univariate Probability plots ####
layout(matrix(1:15, ncol=5, byrow = TRUE))
layout.show(15)
par(mar = c(4, 3, 3, 1))

sapply(colnames(x), function (i) {
  qqnorm(x[[i]], main = i)
  qqline(x[[i]])
})