# Stack Overflow Queries ----

## Bar plots conditional fill ----

library(dplyr)
library(ggplot2)
library(forcats)
x <- LETTERS[1:11]
y <- c(9, 4, 6, 8, 5, 7, -9, -4, -6, -8, -5)
z <- seq(-1, 1, by = 0.2)

dat <- data.frame(x, y, z)

dat %>%
  filter(x != "B") %>%
  ggplot(aes(fct_reorder(x, y, .desc = T), y, 
             fill = ifelse(z > 0.7, "up", 
                           ifelse(z < -0.6, "down", "stable")))) +
  geom_col() +
  scale_fill_manual("Color", 
                    values = c("up" = "#5b3b71", 
                               "down" = "#9a4054", 
                               "stable" = "#b7c1bd")) +
  geom_text(data = dat %>% filter() ,aes(label = paste0(y,"%")), 
            hjust = c(-1,-1,-1,-1,-1,1,1,1,1,1)) +
  coord_flip() +
  theme_void()


x <- LETTERS[1:11]
y <- c(9, 4, 6, 8, 5, 7, -9, -4, -6, -8, -5)
z <- seq(-1, 1, by = 0.2)

dat <- data.frame(x, y, z) |> 
  mutate(fill_col = ifelse(z > 0.7, "up", 
                           ifelse(z < -0.6, "down", "stable")),
         just_text = ifelse(z > 0, 1, -0.2)) |> 
  filter(x != "B")

dat %>%
  ggplot(aes(fct_reorder(x, y, .desc = T), y, 
             fill = fill_col)) +
  geom_col() +
    scale_fill_manual("Color", 
                    values = c("up" = "#5b3b71", 
                               "down" = "#9a4054", 
                               "stable" = "#b7c1bd")) +
  geom_text(
    data = dat |> filter(fill_col != "stable"),
    aes(label = paste0(abs(y),"ppt"), hjust = just_text)) +
  coord_flip() +
  theme_void()


## Bar plots label padding ----
x <- LETTERS[1:11]
y <- c(9, 4, 6, 8, 5, 7, -9, -4, -6, -8, -5)
z <- seq(-1, 1, by = 0.2)

dat <- data.frame(x, y, z) |> 
  mutate(just_text = ifelse(z > 0, 1, -0.2))

dat %>%
  ggplot(aes(fct_reorder(x, y, .desc = T), y)) +
  geom_col() +
  geom_text(
    aes(label = paste0(abs(y), " ", "abcdef"), 
        hjust = just_text)) +
  coord_flip() +
  theme(
  axis.text.y = element_text(size = 8, 
                             lineheight = 1, 
                             margin = margin(r = 1, unit = "cm")))


## Labels extend to axis limit ----
x <- LETTERS[1:11]
y <- c(9, 4, 6, 8, 5, 7, -12, -4, -6, -8, -5)
z <- seq(-1, 1, by = 0.2)

dat <- data.frame(x, y, z) |> 
  mutate(just_text = ifelse(z > 0, 1, -0.2))

dat %>%
  ggplot(aes(fct_reorder(x, y, .desc = T), y)) +
  geom_col() +
  geom_text(aes(label = paste0(abs(y), " ", "abcdef"), hjust = just_text)) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "%"),
    #limits = c(-15,15),
    expand = expansion(add = c(5, 5))
  ) +
  theme(
        axis.ticks.y.left = element_blank(),
        axis.line.x = element_line(color = "black"))



abs(seq(-25, 25, by = 5))
seq(stats::rnorm(20)) # effectively 'along'
seq(1, 9, by = 2)     # matches 'end'
seq(1, 9, by = pi)    # stays below 'end'
seq(1, 6, by = 3)
seq(1.575, 5.125, by = 0.05)
seq(17)

demo_continuous(c(0, 10))
demo_continuous(c(0, 10), breaks = breaks_extended(3))
demo_continuous(c(0, 10), breaks = breaks_extended(10))

















