# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(visdat)
library(ggridges)
library(png)
library(ggtext)
library(extrafont)

# Data --------------------------------------------------------------------

tuesdata <- tt_load("2020-07-28")

tuesdata

penguins <- tuesdata$penguins


penguins <- penguins %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  drop_na()



# EDA ---------------------------------------------------------------------

font_import(pattern = "frosty.TTF", prompt = FALSE)

img <- readPNG("img/penguins.png")

n <- 150

snowflakes <- tibble(
  x = runif(n, 2300, 6700),
  y = runif(n, 1, 4.4),
  size = runif(n, 1, 10)
)

ggplot(data = penguins, aes(x = body_mass_g, y = species, fill = species, color = species)) +
  geom_point(data = snowflakes,
             aes(x, y, size = size),
             shape = 42,
             color = "white",
             inherit.aes = FALSE,
             alpha = 0.8) +
  geom_density_ridges() +
  scale_size(range = c(0, 10)) +
  scale_fill_manual(values = c("#F07430", "#C55BCC", "#2D7174")) +
  scale_color_manual(values = c("#F07430", "#C55BCC", "#2D7174")) +
  scale_x_continuous(breaks = c(3000, 4000, 5000, 6000), labels = paste0(3:6, "kg")) +
  annotate(
    geom = "text",
    x = 5050,
    y = 3.5,
    label = "Gentoo",
    color = "white",
    family = "Frosty"
  ) +
  annotate(
    geom = "text",
    x = 3650,
    y = 2.8,
    label = "Chinstrap",
    color = "white",
    family = "Frosty"
  ) +
  annotate(
    geom = "text",
    x = 3650,
    y = 1.6,
    label = "Adelie",
    color = "white",
    family = "Frosty"
  ) +
  annotation_custom(
    grob = grid::rasterGrob(img),
    xmin = 6000,
    xmax = 6800,
    ymin = 0.8,
    ymax = 1.8
  ) +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.6))) +
  labs(title = "Penguin Body Mass (<span style = 'color:#F07430'>Adelie</span>, 
       <span style = 'color:#C55BCC'>Chinstrap</span>, 
       <span style = 'color:#2D7174'>Gentoo</span>)",
       x = NULL,
       caption = "Dataviz: @henrywangnl <br>Artwork: @allison_horst") +
  theme(axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "grey50"),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_markdown(hjust = 0.5,
                                      padding = margin(t = 13, b = 10),
                                      size = 22,
                                      color = "white"),
        plot.background = element_rect(fill = "black"),
        text = element_text(family = "Frosty"),
        plot.caption = element_markdown(color = "white",
                                        padding = margin(r = 18)),
        axis.text.x = element_text(color = "white"))
