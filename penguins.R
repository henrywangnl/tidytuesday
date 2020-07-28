# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(visdat)
library(ggridges)
library(png)
library(ggtext)

# Data --------------------------------------------------------------------

tuesdata <- tt_load("2020-07-28")

tuesdata

penguins <- tuesdata$penguins


penguins <- penguins %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  drop_na()



# EDA ---------------------------------------------------------------------

img <- readPNG("img/penguins.png")


ggplot(data = penguins, aes(x = body_mass_g, y = species, fill = species, color = species)) +
  geom_density_ridges() +
  scale_fill_manual(values = c("#F07430", "#C55BCC", "#2D7174")) +
  scale_color_manual(values = c("#F07430", "#C55BCC", "#2D7174")) +
  annotate(
    geom = "text",
    x = 5050,
    y = 3.5,
    label = "Gentoo",
    color = "white"
  ) +
  annotate(
    geom = "text",
    x = 3650,
    y = 2.8,
    label = "Chinstrap",
    color = "white"
  ) +
  annotate(
    geom = "text",
    x = 3650,
    y = 1.6,
    label = "Adelie",
    color = "white"
  ) +
  annotation_custom(
    grob = grid::rasterGrob(img),
    xmin = 5500,
    xmax = 6500,
    ymin = 1.9,
    ymax = 2.8
  ) +
  annotate(
    geom = "curve",
    x = 5600,
    xend = 4500,
    y = 2.7,
    yend = 2.5,
    curvature = 0.2,
    size = 0.5,
    color = "#C55BCC",
    arrow = arrow(length = unit(0.1, "inches"))
  ) +
  annotate(
    geom = "curve",
    x = 6000,
    xend = 6000,
    y = 2.8,
    yend = 2.95,
    curvature = 0,
    size = 0.5,
    color = "#2D7174",
    arrow = arrow(length = unit(0.1, "inches"))
  ) +
  annotate(
    geom = "curve",
    x = 6300,
    xend = 5000,
    y = 1.9,
    yend = 1.2,
    curvature = -0.2,
    size = 0.5,
    color = "#F07430",
    arrow = arrow(length = unit(0.1, "inches"))
  ) +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.6))) +
  labs(title = "Penguin Body Mass in grams by species (<span style = 'color:#F07430'>Adelie</span>, 
       <span style = 'color:#C55BCC'>Chinstrap</span>, 
       <span style = 'color:#2D7174'>Gentoo</span>)",
       x = NULL,
       caption = "Source: TidyTuesday Week31, 2020 <br>Dataviz: @henrywangnl") +
  theme(axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "grey50"),
        legend.position = "none",
        panel.background = element_blank(),
        plot.title = element_markdown(hjust = 0.5,
                                      padding = margin(t = 10, b = 10),
                                      size = 16),
        plot.caption = element_markdown(padding = margin(t = 10)))
