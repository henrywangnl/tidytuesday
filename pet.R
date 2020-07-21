# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(ggthemes)

# Data --------------------------------------------------------------------

tuesdata <- tt_load("2020-07-21")

animal_outcomes <- tuesdata$animal_outcomes
animal_complaints <- tuesdata$animal_complaints
brisbane_complaints <- tuesdata$brisbane_complaints


# Animal Types over time ------------------------------------------------------------

skimr::skim(animal_outcomes)

outcomes <- animal_outcomes %>% 
  drop_na() %>% 
  rename(type = animal_type,
         total = Total)

p1 <- outcomes %>% 
  count(year, type, wt = total) %>% 
  ggplot(aes(year, n, color = type)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1999, 2018, 1)) +
  labs(y = "counts",
       title = "Animal Types Over Time from 1999 to 2018",
       subtitle = "Cats and dogs remain the top two, while wildlife sees a gradual increase since 2012",
       color = NULL) +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(hjust = 0, vjust = 1, angle = -30)) +
  guides(color = guide_legend(nrow = 1))


# Dogs vs Cats outcomes -------------------------------------------------------

p2 <- outcomes %>% 
  select(year, type, outcome, total) %>% 
  filter(type %in% c("Cats", "Dogs")) %>% 
  add_count(year, type, wt = total) %>% 
  mutate(prop = total / n) %>% 
  filter(outcome %in% c("Reclaimed", "Rehomed")) %>% 
  ggplot(aes(year, prop, color = outcome)) +
  scale_x_continuous(breaks = seq(1999, 2018, 2)) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_line() +
  facet_wrap(~type) +
  labs(title = "Cats vs. Dogs by Outcomes",
       subtitle = "Cats are easy to be rehomed while dogs are more likely to be reclaimed",
       color = NULL) +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(hjust = 0, vjust = 1, angle = -30))

# Dogs vs Cats complaints -------------------------------------------------

complaints <- animal_complaints %>% 
  select(1:3) %>% 
  setNames(c("animal", "type", "date"))

  
p3 <- complaints %>% 
  count(animal, type) %>% 
  add_count(animal, wt = n) %>% 
  mutate(prop = n / nn) %>% 
  arrange(desc(type)) %>% 
  group_by(animal) %>% 
  mutate(posi = ifelse(!is.na(lag(prop)), cumsum(prop) - prop / 2, prop / 2)) %>% 
  ggplot(aes(animal, prop, fill = type)) +
  geom_col() + 
  geom_text(aes(y = posi, label = type)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Cats vs. Dogs by Complaints",
       subtitle = "Not surprising...Noise has nothing to do with cats") +
  theme(legend.position = "none")


