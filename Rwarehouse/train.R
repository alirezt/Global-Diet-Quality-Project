# 1 ----
set.seed(1)
var1 <- sample(c(1:3, NA), 10, replace = TRUE)
var2 <- sample(c(1:3, NA), 10, replace = TRUE)
var3 <- sample(c(1:3, NA), 10, replace = TRUE)
var4 <- sample(c(1:3, NA), 10, replace = TRUE)
var5 <- sample(c(1:3, NA), 10, replace = TRUE)

df <- data.frame(var1, var2, var3, var4, var5)

rowSums(df[c("var1", "var2")] == 1)

# note two characteristics of True and False in R
TRUE + TRUE + TRUE # 3
TRUE > 0 # True 
9*TRUE +TRUE

# 2 ----
df <- tribble(
  ~category,   ~x,  ~y,  ~z, ~id,
  'a',      4,   6,   8, 'abc',
  'a',      7,   3,   0, 'def',
  'a',      7,   9,   0, 'abc',
  'b',      2,   8,   8, 'gh',
  'b',      5,   1,   8, 'ij',
  'b',      8,   0,   1, 'kl',
  'c',      2,   1,   1, 'mn',
  'c',      3,   8,   0, 'mn',
  'c',      1,   9,   1, 'mn',
)
str(df)

df %>%
  group_by(category) %>%
  summarise(rowSums(across(c(x,z)) == 1))
# using rowsums to count the number of times a condition happens.
# like number of times value of 1 appeared across multiple columns. 

# 3 ----
library(dplyr)
library(tidyverse)
install.packages(glue)
library(glue)

set.seed(1)
ch   <- sample(LETTERS[1:5], size = 20, replace = TRUE)
cls  <- sample(c("CLASS ONE", "CLASS TWO"), size = 20, replace = TRUE)
var1 <- sample(1:20, size = 20, replace = TRUE)
var2 <- sample(20:40, size = 20, replace = TRUE)
var3 <- sample(40:60, size = 20, replace = TRUE)
var4 <- sample(c(0,1), size = 20, replace = TRUE)

df <- data.frame(ch, cls, var1, var2, var3, var4, stringsAsFactors = TRUE)

funcA <- function(col1, col2) {
  
  select1 <- df %>% 
    summarise(
      prop = prop.table(table(col1))["1"]*100,
      avg = mean(col2)
    )
  
  out <- list(
    prop = paste0("The percentage of value 1 in ", as.list(match.call())[[2]], " is: ", unique(select1$prop), "%"),
    percent = paste0("The mean value of ", as.list(match.call())[[3]], " is: ", unique(select1$avg))
  )
  
  glue::glue_collapse(out, sep = "\n\n")
  
}

funcA(var4, var3)
#> The percentage of value 1 in var4 is: 0.5
#> 
#> The mean value of var3 is: 51.1


prop.table(table(df$var3))["60"]
table(df$var4)


















# 4 ----

df <- tribble(
  ~id, ~x, 
  'A',0,
  'A',1,
  'A',1,
  'B',0,
  'B',0,
  'B',1,
  'C',1,
  'C',0,
  'C',0,
  'C',1,
)


df %>%
  group_by(id) %>%
  summarise(
    result = round(prop.table(table(x))["1"]*100, digits = 2)
    )

df %>%
  group_by(id) %>%
  summarise(
    result = mean(x == 1)*100
  )


df <- tribble(
  ~id, ~x, ~y, ~z,
  'A',0,0,0,
  'A',1,0,0,
  'A',1,1,0,
  'B',0,0,0,
  'B',0,0,0,
  'B',1,0,0,
  'C',1,0,0,
  'C',0,0,0,
  'C',0,0,0,
  'C',1,0,0,
)

# one way
df %>%
  group_by(id) %>%
  summarise(
    result = rowSums(across(c(y,z))) >= 1
  )

(TRUE*2+FALSE)/3

df %>%
  group_by(id) %>%
  summarise(
    result = mean(rowSums(across(c(x,z))) >= 1)
  )

library(dplyr) # version >= 1.1.0
df %>% 
  reframe(result = mean(if_any(x:z)), .by = id)

df %>%
  group_by(id) %>%
  summarise(result = round(mean(x|y|z), digits = 2))

0|0



df <- tribble(
  ~id, ~x, ~y, ~z, ~m,
  'A',0,8,1,1,
  'A',1,6,1,0,
  'A',1,1,0,2,
  'B',1,1,1,3,
  'B',4,5,7,4,
  'B',1,0,0,5,
  'C',1,5,0,6,
  'C',3,3,8,7,
  'C',5,4,8,8,
  'C',2,0,1,9,
)

df %>% 
  group_by(id) %>%
  summarise(
    avg = mean(rowSums(across(x:z) == 1) >= 1)
    )

df %>%
  group_by(id) %>%
  summarise(
    result = round(mean(rowSums(across(c(x, m)) == 1, na.rm=TRUE) >= 1)*100, digits = 2),
  )

df %>%
  group_by(id) %>%
  summarise(
    result = round(prop.table(table(y))["1"]*100, digits = 2),
  )

# 5 ----

# Libraries
library(ggplot2)

# Create data
data <- data.frame(
  ch=LETTERS[1:10],
  a=abs(rnorm(10)),
  c=abs(rnorm(10)),
  d=abs(rnorm(10))
)

# Horizontal version
ggplot(data, aes(x=ch, y=a)) +
  geom_segment( aes(x=ch, xend=ch, y=0, yend=a), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )



df <- tribble(
  ~id, ~var1, ~var2, ~var3,
  'Country A',8.5,1.2,6.3,
  'Country B',2.5,7.6,9.1,
  'Country C',6.8,4.5,6.7,
  'Country D',8,7.5,7.6,
  'Country F',5.5,6.3,7.2,
)

df <- tribble(
  ~id, ~var1, ~var2, ~var3,
  'Country A',8.5,1.2,6.3,
)

ggplot(df, )

set.seed(1)
data <-as.data.frame(matrix( sample( 2:20 , 40 , replace=T) , ncol=10))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding", "data-viz" , "french" , "physic", "statistic", "sport" )
data <-rbind(rep(20,10) , rep(0,10) , data)
rownames(data) <- c("-", "--", "John", "Angli", "Baptiste", "Alfred")

view(data)

library(tidyverse)

data <- data %>% 
  slice(3:6) %>% 
  t()  %>%
  as.data.frame() %>%
  add_rownames() %>%
  mutate(rowname=factor(rowname, rowname)) %>% 
  pivot_longer(cols = -1, values_to = 'mark')


data %>% ggplot( aes(x=rowname, y=mark)) +
  geom_bar(stat="identity", fill="#69b3a2", width=0.6) +
  coord_flip() +
  #theme_ipsum() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  ylim(0,20) +
  ylab("mark") +
  xlab("") +
  facet_wrap(~name, ncol=4)
















df <- tribble(
  ~variables, ~cities, ~values,
  'var1', "C1", 2.5,
  'var1', "C2", 3.5,
  'var1', "C3", 4.9,
  'var2', "C1", 8.5,
  'var2', "C2", 7.3,
  'var2', "C3", 6.1,
  'var3', "C1", 3.1,
  'var3', "C2", 5.2,
  'var3', "C3", 7.8,
  
)

df %>% 
  filter(cities == "C2") %>%
  ggplot(aes(x = variables, y = values)) +
  geom_segment(aes(x=variables, xend=variables, y=0, yend=values), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  coord_flip()



lolly <- function(city_name){
  df %>% 
    filter(cities == city_name) %>%
    ggplot(aes(x = variables, y = values)) +
    geom_segment(aes(x=variables, xend=variables, y=0, yend=values), color="skyblue") +
    geom_point( color="blue", size=4, alpha=0.6) +
    coord_flip()
}

lolly("C1")

cowplot::plot_grid(
  lolly("C1"),
  lolly("C2"),
  lolly("C3"),
  nrow = 3,
  labels = paste0("City = C",1:3))














library(tidyverse)
df <- tribble(
  ~variables, ~names, ~values,
  'vard', "D", 2.5,
  'vard', "D", 3.5,
  'vard', "D", 4.9,
  'vara', "A", 2.5,
  'vara', "A", 3.5,
  'vara', "A", 4.9,
  'varc', "C", 8.5,
  'varc', "C", 7.3,
  'varc', "C", 6.1,
  'varb', "B", 3.1,
  'varb', "B", 5.2,
  'varb', "B", 7.8,
  
)
df %>% ggplot( aes(x=variables, y=values)) + coord_flip()

new_levels <- df$variables %>% unique() %>% rev()

df <- df %>% mutate(variables = factor(variables, levels = new_levels))
df %>% ggplot(aes(x=variables, y=values)) + coord_flip()

# 6 ----

df <- tribble(
  ~Gender, ~Age, ~x, ~y, ~z,
  1, 15, 1, 1, 0, 
  1, 14, 1, 1, 1, 
  2, 12, 0, 0, 1, 
  1, 18, 1, 0, 0, 
  2, 19, 0, 0, 0, 
  2, 22, 1, 0, 1,
  1, 11, 0, 1, 1, 
  1, 13, 1, 1, 1, 
  2, 10, 1, 0, 1,
  1, 12, NA,0, 1 
)


df$varA <- ifelse(df$Gender == 1 &                        
                    df$Age >= 10 & df$Age <= 14 &           
                    rowSums(df[c("x", "y", "z")] == 1) > 1, 
                  1, 0)

# NA remove = True
df %>%
  group_by(Gender) %>%
  summarise(
    var = mean(x == 1, na.rm = TRUE) 
  )

# 7 ----
df1 <- tribble(
  ~Country, ~id, ~var1, ~var2, ~ var3,
  "Bangladesh", "1", 2.5, 3, 1.5,
  "Bangladesh", "2", 4.5, 4.3, 2.7,
  "Laos", "1", 2.7, 3.2, 6.5,
  "Laos", "2", 3.5, 5.1, 8.2,
  "Ghana", "1", 8.5, 5, 7.5,
  "Ghana", "2", 4, 6.7, 1.3,
  "China", "1", 4.3, 6.1, 2.5,
  "China", "2", 6.2, 2.8, 6.8,
)

df2 <- tribble(
  ~Country, ~ISO3, ~Region, ~Income,
  "Bangladesh", "BGD", "AP", "LM",
  "Laos", "LAO", "AP", "LM", 
  "Ghana", "GHA", "SSA", "LM",
  "China", "CHN", "AP", "UM", 
)

df3 <- tribble(
  ~Country, ~id, ~var1, ~var2, ~ var3, ~ISO3, ~Region, ~Income,
  "Bangladesh", "1", 2.5, 3, 1.5, "BGD", "AP", "LM",
  "Bangladesh", "2", 4.5, 4.3, 2.7, "BGD", "AP", "LM",
  "Laos", "1", 2.7, 3.2, 6.5, "LAO", "AP", "LM", 
  "Laos", "2", 3.5, 5.1, 8.2, "LAO", "AP", "LM", 
  "Ghana", "1", 8.5, 5, 7.5, "GHA", "SSA", "LM",
  "Ghana", "2", 4, 6.7, 1.3, "GHA", "SSA", "LM",
  "China", "1", 4.3, 6.1, 2.5, "CHN", "AP", "UM",
  "China", "2", 6.2, 2.8, 6.8, "CHN", "AP", "UM",
)




for (i in 1:nrow(df1)){
    if (df1$Country[i] == "Bangladesh"){
      df1$ISO[i] = "BGD"
    }
  if (df1$Country[i] == "Laos"){
    df1$ISO[i] = "LAO"
  }
  if (df1$Country[i] == "Ghana"){
    df1$ISO[i] = "GHA"
  }
  if (df1$Country[i] == "China"){
    df1$ISO[i] = "CHN"
  }
}



left_join(df1, df2, by = "Country")



df1 <- tribble(
  ~Country, ~Gender, ~var1, ~var2, ~ var3, ~Income,
  "Bangladesh", "F", 2.5, 3, 1.5, "LM",
  "Bangladesh", "M", 4.5, 4.3, 2.7, "LM",
  "Laos", "F", 2.7, 3.2, 6.5, "LM", 
  "Laos", "M", 3.5, 5.1, 8.2, "LM", 
  "Ghana", "F", 8.5, 5, 7.5, "LM",
  "Ghana", "M", 4, 6.7, 1.3, "LM",
  "China", "F", 4.3, 6.1, 2.5, "UM",
  "China", "M", 6.2, 2.8, 6.8, "UM",
)

df1 %>% 
  group_by(Country, subgroup = var1 + var2) %>%
  summarise()

df1 %>% 
  group_by(Country, subgroup = Gender + Income) %>%
  summarise()

df2 <- tribble(
  ~Country, ~subgroup, 
  "Bangladesh", "F", 
  "Bangladesh", "M", 
  "Laos", "F",  
  "Laos", "M", 
  "Ghana", "F", 
  "Ghana", "M", 
  "China", "F", 
  "China", "M",
  "Bangladesh", "LM", 
  "Bangladesh", "LM", 
  "Laos", "LM",  
  "Laos", "LM", 
  "Ghana", "LM", 
  "Ghana", "LM", 
  "China", "UM", 
  "China", "UM",
)

df1 %>% reframe(subgroup = c("Gender", "Income"), .by = Country)
qt()




# 8 ----

df <- tribble(
  ~"name", ~"region", ~x, ~y, ~z,
  #"A", "reg1", 0, 1, 1,
  #"A", "reg1", 1, 1, NA,
  "B", "reg1", 1, 0, 4,
  "C", "reg2", 1, 0, 2,
  "B", "reg2", 0, NA, 0,
  "C", "reg1", NA, 0, 5,
  "C", "reg1", 0, 1, 2,
  "B", "reg1", NA, 1, 3,
  "B", "reg2", 1, NA, NA,
  "A", "reg2", 1, 1, 1,
  "A", "reg2", 0, 1, 4,
  "A", "reg2", 1, 1, 2,
  #"A", "reg1", 0, 1, 3,
)

4/2

df %>%
  group_by(name, region) %>%
  summarise(
    meanX = mean(x == 1, na.rm = TRUE)*100,
    nX = n(),
    Xlower_ci = mean(x == 1)*100 - qt(1- 0.05/2, (n() - 1))*sd(x == 1)/sqrt(n()),
    Xupper_ci = mean(x == 1)*100 + qt(1- 0.05/2, (n() - 1))*sd(x == 1)/sqrt(n()),
    meanY = mean(y == 1, na.rm = TRUE)*100,
    nY = n(),
    Ylower_ci = mean(y == 1)*100 - qt(1- 0.05/2, (n() - 1))*sd(y == 1)/sqrt(n()),
    Yupper_ci = mean(y == 1)*100 + qt(1- 0.05/2, (n() - 1))*sd(y == 1)/sqrt(n()),
    meanZ = mean(z, na.rm = TRUE),
    nZ = n(),
    Zlower_ci = mean(z) - qt(1- 0.05/2, (n() - 1))*sd(z)/sqrt(n()),
    Zupper_ci = mean(z) + qt(1- 0.05/2, (n() - 1))*sd(z)/sqrt(n()),
  )


df1 <- tribble(
  ~"name", ~"region", ~"Indicator", ~"mean/prevalence", ~"Upper interval", ~"Lower interval",
  "A", "reg1", "x", "33", "-", "-",
  "A", "reg1", "z", "2", "14.7", "-10.7",
)


z <- c(1, NA, 3); length(na.omit(z))
mean(z, na.rm = TRUE) - qt(1- 0.05/2, (length(na.omit(z)) - 1))*sd(z, na.rm = TRUE)/sqrt(length(na.omit(z)))
mean(z, na.rm = TRUE) + qt(1- 0.05/2, (length(na.omit(z)) - 1))*sd(z, na.rm = TRUE)/sqrt(length(na.omit(z)))



x <- c(FALSE, TRUE, FALSE); length(na.omit(x))
mean(x, na.rm = TRUE)*100 - qt(1- 0.05/2, (length(na.omit(x)) - 1))*sd(x, na.rm = TRUE)*100/sqrt(length(na.omit(x)))
mean(x, na.rm = TRUE)*100 + qt(1- 0.05/2, (length(na.omit(x)) - 1))*sd(x, na.rm = TRUE)*100/sqrt(length(na.omit(x)))
















# 9----

foo <- mtcars[,c("mpg","vs")]; names(foo) <- c("x","y") ## Working example data
mod <- glm(y ~ x, data = foo, family = binomial)
preddata <- with(foo, data.frame(x = seq(min(x), max(x), length = 100)))
preds <- predict(mod, newdata = preddata, type = "link", se.fit = TRUE)



critval <- 1.96 ## approx 95% CI
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit

qnorm(0.975)


fit2 <- mod$family$linkinv(fit)
upr2 <- mod$family$linkinv(upr)
lwr2 <- mod$family$linkinv(lwr)

preddata$lwr <- lwr2 
preddata$upr <- upr2 
ggplot(data=foo, mapping=aes(x=x,y=y)) + geom_point() +         
  stat_smooth(method="glm", method.args=list(family=binomial)) + 
  geom_line(data=preddata, mapping=aes(x=x, y=upr), col="red") + 
  geom_line(data=preddata, mapping=aes(x=x, y=lwr), col="red") 



# number of independent samples
n_samples <- 1000000
# sample size to use for each sample
sample_size <- 80
# probability a plant will be purple
purple_prob <- 0.4

# repeat the sampling/estimation procedure many times
raw_samples <- rbinom(n = n_samples, size = sample_size, prob = purple_prob)
# convert results to %
percent_samples <- 100 * raw_samples / sample_size
sd(percent_samples)

survey <- read.csv(file = "C:/Users/Apple/Downloads/2022-sample-data.csv")
laptop = table(survey$Laptop.Type, useNA = "no") / nrow(survey)
stderr = sqrt(laptop * (1 - laptop) / nrow(survey))

moe = 1.96 * stderr

paste0("Estimated ", names(laptop), " users = ", 
       round(100 * (laptop - moe)), "% to ", 
       round(100 * (laptop + moe)), "%")


df <- tribble(
  ~Gender, ~Age, ~x, ~y, ~z,
  1, 15, 1, 1, 0, 
  1, 14, 1, 1, 1, 
  2, 12, 0, 0, 1, 
  1, 18, 1, 0, 0, 
  2, 19, 0, 0, 0, 
  2, 22, 1, 0, 1,
  1, 11, 0, 1, 1, 
  1, 13, 1, 1, 1, 
  2, 10, 1, 0, 1,
  1, 12, NA,0, 1 
)


mean(df$x == 1, na.rm = TRUE)*100 + 
  1.96 * sqrt(((mean(df$x == 1, na.rm = TRUE)*100)*
                 (100-(mean(df$x == 1, na.rm = TRUE)*100)))/length(na.omit(df$x)))

upconf <- function(x){
  mean(x == 1, na.rm = TRUE)*100 + 
    1.96 * sqrt(((mean(x == 1, na.rm = TRUE)*100)*
    (100-(mean(x == 1, na.rm = TRUE)*100)))/length(na.omit(x)))
}

upconf(df$x)



x = c(2,3,6,8,7,1,2,6,4,5, NA, NA)

qnorm(0.975)*sd(x, na.rm = TRUE)/sqrt(length(na.omit(x)))




df <- tribble(
  ~"name", ~"region", ~x, ~y, ~z,
  #"A", "reg1", 0, 1, 1,
  #"A", "reg1", 1, 1, NA,
  "B", "reg1", 1, 0, 4,
  "C", "reg2", 1, 0, 2,
  "B", "reg2", 0, NA, 0,
  "C", "reg1", NA, 0, 5,
  "C", "reg1", 0, 1, 2,
  "B", "reg1", NA, 1, 3,
  "B", "reg2", 1, NA, NA,
  "A", "reg2", 1, 1, 1,
  "A", "reg2", 0, 1, 4,
  "A", "reg2", 1, 1, 2,
  #"A", "reg1", 0, 1, 3,
)

df$region

# first create some fake data that approximates your situation
set.seed(6933)

fruit_words <- c("apple", "orange", "banana", "pappels", "orong", "bernaner")

dat <- data.frame(fruit = sample(fruit_words, size=10, replace=TRUE), 
                  stringsAsFactors=FALSE)

fruit_lkup <- c(apple="appl", orange="orng", banana="bnna", 
                pappels="appl", orong="orng", bernaner="bnna")



dat$fruit <- as.character(fruit_lkup[dat$fruit])



fct_count(gss_cat$partyid)

partyid2 <- fct_collapse(gss_cat$partyid,
                         missing = c("No answer", "Don't know"),
                         other = "Other party",
                         rep = c("Strong republican", "Not str republican"),
                         ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                         dem = c("Not str democrat", "Strong democrat")
)
fct_count(partyid2)


gss_cat1 <- gss_cat
gss_cat1$partyid <- partyid2





df <- tribble(
  ~Country, ~Gender, ~var,
  "Bangladesh", "F", 2.5,
  "Bangladesh", "M", 4.5,
  "Bangladesh", "M", 4.1,
  "US", "F", 1.7,
  "US", "F", 2.7,
  "US", "M", 3.5,
)
  
df %>% 
  group_by(Country, Gender) %>%
  reframe(
    n = n(),
    meanVar = mean(var))

df %>% 
  group_by(Country) %>%
  reframe(
    n = n(),
    meanVar = mean(var))

df %>% 
  mutate(Gender = "All") %>% 
  bind_rows(., df) %>% 
  group_by(Country, Gender) %>% 
  summarise(n = n(),
            meanVar = mean(var))



# 10----

ls1 <- c(1,0,1,1,0,NA)
mean(ls1 == 1, na.rm = TRUE)

df <- tribble(
  ~Country, ~Gender, ~Condition, ~var1, ~var2, 
  "Bangladesh", "Male", "Rural", 1, 1,
  "Bangladesh", "Female", "Urban", 0, 1,
  "Bangladesh", "Male", "Rural", 1, 0,
  "Bangladesh", "Male", "Urban", 0, 4.2,
  "Laos", "Female", "Rural", 0, 1.5,  
  "Laos", "Female", "Rural", 0, 6.7,
  "Laos", "Female", "Rural", 0, 5,
  "Laos", "Male", "Urban", 1, 7.8,
  "Laos", "Male", "Urban", 1, 0.8,
  "China", "Male", "Urban", 0, 1.9, 
  "China", "Male", "Urban", 1, 6.7,
  "China", "Male", "Urban", 1, 8.5,
  "China", "Female","Urban", 1, 6,
  "China", "Female", "Urban", 1, 3.2,
  "China", "Female", "Rural", 0, 7.1
)

df %>%
  mutate(Gender = "All") %>%
  bind_rows(., df) %>%
  pivot_longer(c(Gender, Urbanicity), values_to = "subgroup") %>%
  pivot_longer(c(var1, var2), names_to = "Variables", values_to = "Values") %>%
  group_by(Country, subgroup, Variables)%>%
  reframe(
    n= n(),
    "Mean/Prevalence" = if (Variables == "var2") {
      round(mean(Values, na.rm = TRUE), digits = 2)
    }
    else {
      round(mean(Values == 1, na.rm = TRUE)*100, digits = 2)
    }
  )%>%
  print(n = 50)

df1<-reshape2::melt(df, id.vars=c("Country", "var1","var2"), measure.vars=c("Gender", "Condition"))
df1

library(tidyverse)
for (i in length(df$var1)){
  df[,ind[1]] <- NA
}
ind <- c("var3", "var4")
#or
for (i in length(df$var1)){
  df[,"var3"] <- NA
}


df <- data.frame(
  x= sample(x = c(1,0, NA), size = 10, replace = TRUE),
  y= sample(x = c(1,0, NA), size = 10, replace = TRUE)
)

rowSums(df, na.rm = TRUE) # when na.rm = TRUE: 1+NA = 1, NA+NA = 0

# 11----
data(api)
view(apiclus1)
dclus1<-svydesign(id=~dnum, fpc=~fpc, data=apiclus1)

svyciprop(~I(ell==0), dclus1, method="li")
svyciprop(~I(ell==0), dclus1, method="lo")
svyciprop(~I(ell==0), dclus1, method="as")
svyciprop(~I(ell==0), dclus1, method="be")
svyciprop(~I(ell==0), dclus1, method="me")
svyciprop(~I(ell==0), dclus1, method="xl")

## reproduces Stata svy: mean
svyciprop(~I(ell==0), dclus1, method="me", df=degf(dclus1))
## reproduces Stata svy: prop
svyciprop(~I(ell==0), dclus1, method="lo", df=degf(dclus1))


rclus1<-as.svrepdesign(dclus1)
svyciprop(~I(emer==0), rclus1, method="li")
svyciprop(~I(emer==0), rclus1, method="lo")
svyciprop(~I(emer==0), rclus1, method="as")
svyciprop(~I(emer==0), rclus1, method="be")
svyciprop(~I(emer==0), rclus1, method="me")

str(I(c(1,2,3)))

# 12 ----

dat <- tribble(
  ~Country, ~Gender, ~Indicator, ~value,  
  "Bangladesh", "Male", "A", 3.7,
  "Bangladesh", "Female", "A", 2.6,
  "Bangladesh", "Male", "B", 6.8,
  "Bangladesh", "Female", "B", 4.1,
  "China", "Male", "A", 7.6,
  "China", "Female", "A", 3.9,
  "China", "Male", "B", 1.5,
  "China", "Female", "B", 2.9,
  "Laos", "Male", "A", 7.6,
  "Laos", "Female", "A", 5.1,
  "Laos", "Male", "B", 3.8,
  "Laos", "Female", "B", 2.8,
)

dat %>%
  pivot_wider(names_from = c(Indicator) , values_from = value)

dat %>%
  reshape(idvar = Indicator, )

dat2019 <- tribble(
  ~Country, ~Gender, ~Indicator, ~value,  
  "Bangladesh", "Male", "A", 3.6,
  "Bangladesh", "Female", "A", 6.8,
  "Bangladesh", "Male", "B", 9.2,
  "Bangladesh", "Female", "B", 1.5,
  "China", "Male", "A", 8.5,
  "Chiina", "Female", "A", 3.9,
  "China", "Male", "B", 4.6,
  "China", "Female", "B", 5.3,
)

library(dplyr)
left_join(dat2020, dat2019, by = names(dat2020)[1:3]) %>% 
  mutate(Diff = value.x - value.y, value = value.x, .keep = "unused")

# 13 ----

library(leaps)

set.seed(123)
dat <- data.frame(
  x = as.factor(sample(c(1,2,3), size = 10, TRUE)),
  y = as.factor(sample(c(4,5,6,7), size = 10, TRUE)),
  z = rnorm(10),
  res = rnorm(10)
)
glimpse(dat)

model.full <- regsubsets(res ~ ., data = dat, nvmax = 3)
summary(model.full)
coef(model.full, 3)


# 14 for creating data sets----
id = paste0("id", 1:10)
data = expand.grid(list(
  variable = c("2000", "2005", "2010", "2015"),
  id = id
))

groups = sample(LETTERS[1:3], size = length(id), replace = TRUE)
data$group = groups[match(data$id, id)]
data$value = runif(n = nrow(data))
data$tooltip = paste0('line ', data$id )
data$onclick = paste0("alert(\"", data$id, "\")")

cols = c("orange", "orange1", "orange2", "navajowhite4", "navy")
dataset2 <- data.frame(x = rep(1:20, 5),
                       y = rnorm(100, 5, .2) + rep(1:5, each=20),
                       z = rep(1:20, 5),
                       grp = factor(rep(1:5, each=20)),
                       color = factor(rep(1:5, each=20)),
                       label = rep(paste0( "id ", 1:5 ), each=20),
                       onclick = paste0(
                         "alert(\"",
                         sample(letters, 100, replace = TRUE),
                         "\")" )
)

# 15 ----

set.seed(1)
dat <- data.frame(
  x = sample(c(0,1), size = 5, replace = TRUE),
  y = sample(c(0,1), size = 5, replace = TRUE)
)
 # option 1
dat %>%
  rename(newX = x) %>%
  mutate(z = rowSums(pick(newX, y) == 1))



dat <- dat %>%
  rename(newX = x)

 # option 2
dat %>% 
  mutate(z = rowSums(dat[c("newX", "y")] == 1))


# 16 ----
x1 <- letters[1:4]
x2 <- sample(1:4)

cbind(x1, x2)
rbind(x1, x2)


# 17 ----
g1 <- mpg %>%
  mutate(drv = (fct_reorder(drv, hwy))) %>%
  ggplot(aes(x = hwy, y = drv, fill = drv, data_id = drv)) +
  geom_boxplot_interactive() +
  theme(legend.position = 'none')

g2 <- mpg %>%
  mutate(class = (fct_reorder(class , hwy))) %>%
  ggplot(aes(x = hwy, y = class , fill = drv, data_id = as.factor(drv))) +
  geom_boxplot_interactive() +
  theme(legend.position = 'none')
  

girafe(
  ggobj = g1 + plot_spacer() + g2 + plot_layout(widths = c(0.45, 0.1, 0.45)),
  options = list(
    opts_hover(css = ''),
    opts_hover_inv(css = "opacity:0.1;"), 
    opts_sizing(rescale = FALSE)
  ),
  height_svg = 5,
  width_svg = 9
)

# 18 ----

set.seed(1111)

df <- data.frame(
  item = as.numeric(sample(1:20)),
  clust = as.numeric(sample(1:3, 20, replace = TRUE))
)

df %>%
  arrange(clust, item) %>%
  group_by(clust) %>% 
  mutate(id =row_number()) %>%
  pivot_wider(names_from = clust, values_from = item, names_prefix = "Cluster_") %>%
  select(-id)




