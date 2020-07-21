
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(ggcharts)

# Data --------------------------------------------------------------------

tuesdata <- tt_load("2020-07-07")

coffee_rating <- tuesdata$coffee_ratings



# Coffee ratings by country -----------------------------------------------

coffee_rating %>% 
  select(total_cup_points, country = country_of_origin, aroma:cupper_points) %>% 
  mutate(total_cup_points = ifelse(total_cup_points < 10, NA, total_cup_points)) %>% 
  add_count(country) %>% 
  filter(n > 10) %>% 
  ggplot(aes(total_cup_points, fct_reorder(country, total_cup_points, na.rm = TRUE))) +
  geom_boxplot(color = "#F4C95D", fill = "#6A6061", size = 0.2, outlier.size = 0.8) +
  scale_x_continuous(breaks = seq(60, 90, 5)) +
  labs(x = "Total Cup Points",
       y = NULL,
       title = "Coffee Ratings by Country",
       subtitle = "<i style = 'font-size:10pt'>Countries with samples less than 10 are excluded</i>",
       caption = "Source: TidyTuesday Week28, 2020 <br> Dataviz: @henrywangnl") +
  theme(plot.background = element_rect(fill = "#483D3F"),
        plot.title = element_text(color = "#C4BBAF"),
        plot.subtitle = element_markdown(color = "#C4BBAF"),
        plot.caption = element_markdown(color = "#C4BBAF"),
        axis.text = element_text(color = "#C4BBAF"),
        axis.title = element_text(color = "#C4BBAF"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "grey"),
        panel.background = element_rect(fill = "#483D3F"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", size = 0.5, color = "#6A6061"))

