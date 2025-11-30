#install packages 
install.packages("sf")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggplot")

library(sf)
library(tidyverse)
library(tidyverse)
library(ggplot2)

#import data
cars_2015 <- read_csv("Data/2015_Car_use.csv") %>%
  rename(Metric = 1) %>%      # first column = Metric
  mutate(Year = 2015)
cars_2020 <- read_csv("Data/2020_Car_use.csv") %>%
  rename(Metric = 1) %>%      # first column = Metric
  mutate(Year = 2020)

cars_all <- bind_rows(cars_2015, cars_2020) %>%
  pivot_longer(cols = c(USA, Japan),
               names_to = "Country",
               values_to = "Value")

# quick check
cars_all

#bar charr
cars_all %>%
  filter(Metric == "passanger cars in use") %>%
  ggplot(aes(x = factor(Year), y = Value, fill = Country)) +
  geom_col(position = "dodge") +
  labs(
    x = "Year",
    y = "Passenger cars in use",
    fill = "Country",
    title = "Passenger cars in use: USA vs Japan"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

#all three metrics, faceted
cars_all %>%
  ggplot(aes(x = factor(Year), y = Value, fill = Country)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(
    x = "Year",
    y = NULL,
    fill = "Country",
    title = "Car use indicators: USA vs Japan (2015 vs 2020)"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()










