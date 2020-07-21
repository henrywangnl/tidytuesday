library(tidyverse)
library(skimr)
library(lubridate)
library(zoo) # Get Month_Year from date

gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')

glimpse(gdpr_violations)

View(gdpr_violations)

skim(gdpr_violations)

gdpr <- gdpr_violations %>% 
  mutate(date = mdy(date)) %>% 
  mutate(month_year = as.yearmon(date, "%Y%m%d")) %>% 
  mutate_at(vars(picture, name, authority, article_violated, type), factor) %>% 
  rename(country = name)

glimpse(gdpr)

skim(gdpr)

gdpr <- gdpr %>% 
  filter(date > "2018/05/25")

glimpse(gdpr)

# Data exploration


gdpr %>% 
  count(country, sort = TRUE, wt = price, name = "total_price")

gdpr %>% 
  count(price > 50000)

## fines by months
gdpr %>% 
  ggplot(aes(month_year)) +
  geom_bar() +
  scale_x_yearmon(n = 10)

# fines by country
gdpr %>% 
  ggplot(aes(country)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = -30,
                                   hjust = 0,
                                   vjust = 1))


gdpr %>% 
  mutate(country = fct_lump(country, 8, w = price)) %>% 
  count(country, wt = price, name = "total_price") %>% 
  mutate(country = fct_reorder(country, total_price)) %>% 
  ggplot(aes(total_price, country)) +
  geom_col()


gdpr %>% 
  mutate(country = fct_lump_n(country, 8, w = price)) %>% 
  distinct(country)

gdpr_lump <- gdpr %>% 
  mutate(country = fct_lump_n(country, 8, w = price))


gdpr_ft <- gdpr %>% 
    count(country, wt = price, name = "total_price")

glimpse(gdpr_ft)

levels(gdpr_ft$country)

gdpr_ft_reo <- gdpr_ft %>% 
  mutate(country = fct_reorder(country, total_price))

levels(gdpr_ft_reo$country)  

gdpr_ft_reo %>% 
  ggplot(aes(total_price, country)) +
  geom_col()

  
  
ggplot(gdpr, aes(type)) +
  geom_bar()

gdpr %>% 
  distinct(type)

gdpr %>% 
  group_by(type) %>% 
  summarise(n = n(), total_price = sum(price)) %>% 
  top_n(3, total_price) %>% 
  ggplot(aes(type, total_price)) +
  geom_col() +
  ylab("Total Price") +
  scale_y_continuous(labels = scales::label_dollar()) +
  theme(axis.text.x = element_text(angle = -30,
                                   hjust = 0,
                                   vjust = 1))



gdpr %>% 
  mutate(month = floor_date(date, "month")) %>% 
  count(month, name = "num") %>% 
  ggplot(aes(month, num)) +
  geom_col()


gdpr %>% 
  count(month = floor_date(date, "month"),
        country = fct_lump_n(country, 4, w = price),
        wt = price, name = "total_price", sort = TRUE) %>% 
  ggplot(aes(month, total_price, fill = country)) +
  geom_col() +
  scale_y_continuous(labels = scales::dollar_format())


x <- gdpr %>% 
  count(month = floor_date(date, "month"),
        country = fct_lump_n(country, 4, w = price),
        wt = price, name = "total_price", sort = TRUE) 

levels(x$country)

gdpr %>% 
  count(month = floor_date(date, "month"),
        country = fct_lump_n(country, 4, w = price),
        wt = price, name = "total_price", sort = TRUE) %>% 
  count(country, wt = total_price)

y <- gdpr %>% 
  count(month = floor_date(date, "month"),
        country = fct_lump_n(country, 4, w = price),
        wt = price, name = "total_price", sort = TRUE) %>% 
  mutate(country = fct_reorder(country, -total_price, sum))

levels(y$country)  


# test --------------------------------------------------------------------

gdpr %>% 
  select(price) %>%
  mutate(price = scales::dollar(price))






