#install pckages 
install.packages("sf")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggplot")
install.packages("tidyverse")

library(sf)
library(ggplot2)
library(dplyr)
library(ggplot)
library(tidyverse)

km_trans <- read.csv("Data/km.csv")


#clean data set
km_trans_clean <- km_trans %>%
  rename(
    Country    = X,
    km_traveled_million = total.kilometers.traveled..vehicles.
  )

#plot in millions of km 
library(scales) 

km_trans_clean %>%
  ggplot(aes(x = Country, y = km_traveled_million, fill = Country)) +
  geom_col() +
  labs(
    title = "Total Kilometers Traveled by Vehicle, Japan vs USA (2017)",
    x = "Country",
    y = "Vehicle-kilometers (millions)",
    caption = "Each unit on the y-axis = 1 million vehicle-km"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()



#compare km and per car
cars_2020_use <- cars_all %>%
  filter(Year == 2020,
         Metric == "passanger cars in use") %>%
  select(Country, cars_in_use = Value)

car_usage <- cars_2020_use %>%
  left_join(km_trans_clean, by = "Country") %>%
  mutate(km_per_100_cars_million = km_traveled_million / cars_in_use * 100)

car_usage

#km traveled per passanger car
car_usage %>%
  ggplot(aes(x = Country, y = km_per_100_cars_million, fill = Country)) +
  geom_col() +
  labs(
    title = "Average Vehicle-km Per 100 Passenger Cars",
    x = "Country",
    y = "Vehicle-km per 100 cars (millions)",
    caption = "km data in millions of vehicle-km; car counts from 2020"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()


