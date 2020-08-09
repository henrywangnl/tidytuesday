# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(rnaturalearth)
library(sf)

# Data --------------------------------------------------------------------

tuesdata <- tt_load("2020-08-04")

tuesdata

energy_types <- tuesdata$energy_types
country_totals <- tuesdata$country_totals


# EDA ---------------------------------------------------------------------

europe_map_data <- ne_countries(continent = "europe", 
             returnclass = "sf") %>% 
  select(name_long, geometry)

net_production_2018 <- country_totals %>% 
  filter(type == "Total net production") %>% 
  mutate(country_name = if_else(country == "UK", "United Kingdom", country_name)) %>% 
  select(country_name, total = `2018`)

net_production_top <- net_production_2018 %>% 
  left_join(europe_map_data, by = c("country_name" = "name_long")) %>% 
  st_as_sf() %>% 
  arrange(desc(total)) %>% 
  slice(1:11)
  
net_production_2018 %>% 
  left_join(europe_map_data, by = c("country_name" = "name_long")) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = total)) +
  stat_sf_coordinates(data = net_production_top,
                      alpha = 0.5) +
  geom_sf_label(data = net_production_top,
                aes(label = country_name),
                label.size = 0,
                size = 3,
                alpha = 0,
                nudge_y = c(1,1,1,1,1,1,1,1,1,1.3,0.75),
                nudge_x = c(0,0,0,0,0,0,0,0,0,0,-3)) +
  scale_fill_steps2(
    high = "darkred",
    labels = scales::comma,
    name = "GWh"
  ) +
  theme_void() +
  xlim(c(-10, 40)) +
  ylim(c(35, 85)) +
  theme_void() +
  labs(subtitle = "Total net production") +
  theme(plot.subtitle = element_text(colour = "darkred",
                                     margin = margin(20, 0, 0, 0),
                                     size = rel(1.5)),
        legend.position = c(0.2, 0.75))


